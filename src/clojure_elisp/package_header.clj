(ns clojure-elisp.package-header
  "Emacs library headers for the files of a package.

   A package is described once, by a map with the keys of `PackageMap`. It comes
   from the ns attr-map,

   (ns my.pkg
     \"One-line summary.

      Commentary paragraphs.\"
     {:elisp/package {:author  \"Jane Doe <jane@example.org>\"
                      :url     \"https://example.org/my-pkg\"
                      :version \"0.1.0\"
                      :package-requires [[emacs \"28.1\"]]
                      :keywords [\"convenience\"]
                      :license \"GPL-3.0-or-later\"}})

   and/or from the project descriptor (clel.edn `:package`), which lets a
   multi-file package state author, URL, license, version and requirements
   ONCE. `project-packages` works out which file is the package's MAIN file
   and gives every file of the package its effective map.

   The main file (the one named after the package) gets the full header:
   Version, URL, Keywords and Package-Requires, which always names `emacs` and
   the runtime at `version/minimum-runtime-version`. Every other file gets the
   header package-lint and melpazoid require of a SECONDARY file: summary,
   Copyright/Author, SPDX-License-Identifier and a Commentary section, and no
   Package-Requires, which package-lint rejects outside the main file.

   In every file the ns docstring's first line is the summary and the rest the
   Commentary, unless `:commentary` is given."
  (:require [clojure.string :as str]
            [clojure-elisp.version :as version]))

(def runtime-package
  "Package that provides the runtime every compiled file requires."
  version/runtime-feature)

(def default-emacs-version
  "Emacs version the runtime itself requires."
  "28.1")

(def PackageMap
  "A package description, from `:elisp/package` or clel.edn `:package`.
   `:name` is the package name, i.e. the main file's name without .el; a
   file whose own name differs from it is a secondary file of the package.
   `:assisted-by` names the coding assistants, as AGENT:MODEL, that MELPA
   asks a package to credit under its Author line."
  [:map {:closed false}
   [:name {:optional true} [:or :string :symbol]]
   [:author {:optional true} [:or :string [:sequential :string]]]
   [:assisted-by {:optional true} [:or :string [:sequential :string]]]
   [:maintainer {:optional true} [:or :string [:sequential :string]]]
   [:url {:optional true} :string]
   [:version {:optional true} :string]
   [:package-requires {:optional true}
    [:sequential [:tuple [:or :symbol :keyword :string] [:or :string :int :double]]]]
   [:keywords {:optional true} [:sequential :string]]
   [:license {:optional true} :string]
   [:copyright {:optional true} :string]
   [:commentary {:optional true} :string]])

(def package-wide-keys
  "Keys that describe the whole package. `:commentary` is not one: it belongs
   to the file whose ns declares it."
  [:name :author :assisted-by :maintainer :url :version :package-requires
   :keywords :license :copyright])

(defn- version-parts [v]
  (mapv #(or (parse-long %) 0) (str/split (str v) #"\.")))

(defn- version< [a b]
  (let [pa (version-parts a)
        pb (version-parts b)
        n  (max (count pa) (count pb))
        pad #(into % (repeat (- n (count %)) 0))]
    (neg? (compare (pad pa) (pad pb)))))

(defn package-requires
  "Declared [[pkg \"version\"] ...] with emacs and the runtime added when absent.
   Throws when the runtime is declared older than compiled code needs."
  [declared]
  (let [pairs (mapv (fn [[p v]] [(symbol (name p)) (str v)]) declared)
        found (into {} pairs)
        rt    (get found runtime-package)]
    (when (and rt (version< rt version/minimum-runtime-version))
      (throw (ex-info (str "Package-Requires declares " runtime-package " " rt
                           ", but compiled code needs "
                           version/minimum-runtime-version)
                      {:declared rt :needs version/minimum-runtime-version})))
    (cond->> pairs
      (not (contains? found 'emacs))
      (into [['emacs default-emacs-version]])

      (not rt)
      (#(conj % [runtime-package version/minimum-runtime-version])))))

(defn- render-requires [pairs]
  (str "(" (str/join " " (map (fn [[p v]] (str "(" p " \"" v "\")")) pairs)) ")"))

(defn- dedent
  "Strip the indentation shared by all non-blank lines."
  [lines]
  (let [indents (->> lines
                     (remove str/blank?)
                     (map #(count (re-find #"^ *" %))))
        n       (if (seq indents) (apply min indents) 0)]
    (map #(if (str/blank? %) "" (subs % (min n (count %)))) lines)))

(defn split-doc
  "[summary commentary-lines] from an ns docstring."
  [doc]
  (let [[head & more] (str/split-lines (or doc ""))
        summary (-> (str/trim (or head "")) (str/replace #"\.$" ""))
        body    (->> (dedent more)
                     (drop-while str/blank?)
                     reverse
                     (drop-while str/blank?)
                     reverse)]
    [summary (vec body)]))

(defn- comment-lines [lines]
  (map #(if (str/blank? %) ";;" (str ";; " %)) lines))

(defn- people-lines [label people]
  (let [people (if (string? people) [people] people)
        pad    (apply str (repeat (+ 3 (count label)) \space))]
    (map-indexed (fn [i p] (if (zero? i) (str ";; " label ": " p) (str ";;" pad p)))
                 people)))

;; ============================================================================
;; Main and secondary files
;; ============================================================================

(defn main-file?
  "True when elisp-name is the package's main file: the package has no
   `:name`, or `:name` is elisp-name."
  [elisp-name pkg]
  (let [n (:name pkg)]
    (or (nil? n) (= (str n) elisp-name))))

(defn- header-lines
  "Header text from the file name, ns docstring and package map, up to and
   including the ;;; Code: line. `fields` are the metadata lines."
  [elisp-name doc pkg fields]
  (let [[summary doc-body] (split-doc doc)
        commentary         (if-let [c (:commentary pkg)]
                             (vec (dedent (str/split-lines c)))
                             doc-body)]
    (str/join
     "\n"
     (concat
      [(str ";;; " elisp-name ".el --- " summary "  -*- lexical-binding: t; -*-")
       ""]
      (when-let [c (:copyright pkg)] [(str ";; Copyright (C) " c) ""])
      fields
      [""
       ";; This file is not part of GNU Emacs."
       ";; Generated by ClojureElisp: edit the .cljel source, not this file."
       ""
       ";;; Commentary:"
       ""]
      (comment-lines (if (seq commentary) commentary [summary]))
      [""
       ";;; Code:"
       ""]))))

(defn- people-fields
  "Author, one Assisted-by line per assistant (right under Author, as MELPA's
   CONTRIBUTING asks), then Maintainer."
  [{:keys [author assisted-by maintainer]}]
  (concat (when author (people-lines "Author" author))
          (map #(str ";; Assisted-by: " %)
               (if (string? assisted-by) [assisted-by] assisted-by))
          (when maintainer (people-lines "Maintainer" maintainer))))

(defn- field [label v]
  (when v [(str ";; " label ": " v)]))

(defn- render-main
  [elisp-name doc {:keys [url version keywords license] :as pkg}]
  (header-lines elisp-name doc pkg
                (concat (people-fields pkg)
                        (field "URL" url)
                        (field "Version" version)
                        [(str ";; Package-Requires: "
                              (render-requires (package-requires (:package-requires pkg))))]
                        (when (seq keywords) [(str ";; Keywords: " (str/join ", " keywords))])
                        (field "SPDX-License-Identifier" license))))

(defn- render-secondary
  "No URL, Version, Keywords or Package-Requires: those describe the package,
   and package-lint reports Package-Requires outside the main file as an
   error. Author and license are per file, melpazoid checks every file."
  [elisp-name doc {:keys [license] :as pkg}]
  (header-lines elisp-name doc pkg
                (concat (people-fields pkg)
                        (field "SPDX-License-Identifier" license))))

(defn render
  "Header text from the file name, ns docstring and package map, up to and
   including the ;;; Code: line: the main file's when elisp-name is the
   package's main file, a secondary file's otherwise."
  [elisp-name doc pkg]
  (if (main-file? elisp-name pkg)
    (render-main elisp-name doc pkg)
    (render-secondary elisp-name doc pkg)))

;; ============================================================================
;; A project's files
;; ============================================================================

(defn- member?
  "True when the file elisp-name belongs to package package-name: MELPA wants
   every file named package-name or package-name-*, and a file whose own
   :elisp/package names another package belongs to that one."
  [package-name elisp-name own]
  (and (or (= elisp-name package-name)
           (str/starts-with? elisp-name (str package-name "-")))
       (or (nil? (:name own)) (= (str (:name own)) package-name))))

(defn package-name
  "The name of the package a project's files make up, or nil when neither the
   project nor any file declares one. In order: the project's `:name`; a
   file's own `:name`; the file whose own map carries `:package-requires`
   (only the main file has them); else the shortest file name, which for
   tod, tod-sun, tod-util is the package tod."
  [project-pkg name->own]
  (let [declared (sort-by key (filter (comp some? val) name->own))]
    (when (or project-pkg (seq declared))
      (or (some-> (:name project-pkg) str)
          (some (fn [[_ own]] (some-> (:name own) str)) declared)
          (some (fn [[n own]] (when (contains? own :package-requires) n)) declared)
          (first (sort-by (juxt count identity)
                          (map key (if project-pkg name->own declared))))))))

(defn project-packages
  "Effective package map of every file of a project that belongs to its
   package, as {elisp-name pkg}; files outside it are absent.

   project-pkg is clel.edn's `:package` (or nil); name->own maps each file's
   elisp name to its ns's own `:elisp/package` (or nil). A file's map is the
   package-wide keys of the project map and the main file's own map, then its
   own map, with `:name` set to the package name, which is what makes every
   file but the main one a secondary file."
  [project-pkg name->own]
  (when-let [pname (package-name project-pkg name->own)]
    (let [package-wide (select-keys (merge project-pkg (get name->own pname))
                                    package-wide-keys)]
      (into {}
            (for [[elisp-name own] name->own
                  :when (member? pname elisp-name own)]
              [elisp-name (merge package-wide own {:name pname})])))))
