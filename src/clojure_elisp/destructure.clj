(ns clojure-elisp.destructure
  "Destructuring expansion for ClojureElisp.

   Pure functions that expand destructuring patterns (vector and map)
   into flat sequences of simple [symbol init-form] bindings.
   Also handles function parameter processing with & rest args.")

;; ============================================================================
;; Pattern Detection
;; ============================================================================

(defn destructure-pattern?
  "Returns true if pattern requires destructuring (is a vector or map)."
  [pattern]
  (or (vector? pattern) (map? pattern)))

;; ============================================================================
;; Vector Destructuring
;; ============================================================================

(declare expand-destructuring)

(defn- expand-vector-destructuring
  "Expand vector destructuring pattern into simple bindings.
   Returns a vector of [symbol init-form] pairs.

   Examples:
   - [a b] with coll -> [[a (nth coll 0)] [b (nth coll 1)]]
   - [a & rest] -> [[a (first coll)] [rest (rest coll)]]
   - [_ x] -> [[x (nth coll 1)]]  (ignores _)
   - [:as all] -> [[all coll]]"
  [pattern coll-sym]
  (loop [items      (seq pattern)
         idx        0
         bindings   []
         as-binding nil]
    (cond
      ;; Done processing items
      (empty? items)
      (if as-binding
        (conj bindings as-binding)
        bindings)

      ;; Handle :as keyword
      (= :as (first items))
      (let [as-sym (second items)]
        (recur (drop 2 items) idx bindings [as-sym coll-sym]))

      ;; Handle & rest
      (= '& (first items))
      (let [rest-sym     (second items)
            remaining    (drop 2 items)
            ;; Elisp `nthcdr` is (nthcdr N LIST) — coll SECOND — and returns
            ;; the remaining sublist, exactly Clojure rest-destructuring. The
            ;; old (nthrest coll idx) emitted a void function AND had the args
            ;; reversed.
            rest-binding (when (and rest-sym (not= rest-sym '_))
                           [rest-sym (list 'nthcdr idx coll-sym)])]
        (recur remaining idx
               (if rest-binding (conj bindings rest-binding) bindings)
               as-binding))

      ;; Handle _ (ignore binding)
      (= '_ (first items))
      (recur (rest items) (inc idx) bindings as-binding)

      ;; Handle nested destructuring
      (destructure-pattern? (first items))
      (let [nested-pattern  (first items)
            temp-sym        (gensym "vec__")
            nested-bindings (expand-destructuring nested-pattern temp-sym)]
        (recur (rest items)
               (inc idx)
               (into (conj bindings [temp-sym (list 'nth coll-sym idx)])
                     nested-bindings)
               as-binding))

      ;; Simple symbol binding
      :else
      (let [sym (first items)]
        (recur (rest items)
               (inc idx)
               (conj bindings [sym (list 'nth coll-sym idx)])
               as-binding)))))

;; ============================================================================
;; Map Destructuring
;; ============================================================================

(defn- expand-map-destructuring
  "Expand map destructuring pattern into simple bindings.
   Returns a vector of [symbol init-form] pairs.

   Examples:
   - {:keys [x y]} with m -> [[x (get m :x)] [y (get m :y)]]
   - {:strs [x y]} -> [[x (get m \"x\")] [y (get m \"y\")]]
   - {a :a b :b} -> [[a (get m :a)] [b (get m :b)]]
   - {:keys [x] :or {x 0}} -> [[x (get m :x 0)]]
   - {:keys [x] :as all} -> [[x (get m :x)] [all m]]"
  [pattern map-sym]
  (let [as-sym            (:as pattern)
        or-map            (:or pattern)
        lookup
        (fn [k sym]
          ;; `contains?`, not `(get or-map sym)`: a declared default of nil or
          ;; false is a real default, and reading it as "no default" drops it.
          (if (contains? or-map sym)
            (list 'get map-sym k (get or-map sym))
            (list 'get map-sym k)))
        keys-syms         (:keys pattern)
        strs-syms         (:strs pattern)
        syms-syms         (:syms pattern)
        ;; Remove special keys to get explicit bindings
        explicit-bindings (dissoc pattern :as :or :keys :strs :syms)]
    (cond-> []
      ;; Handle :keys [x y] -> bind x to (get m :x)
      keys-syms
      (into (for [sym keys-syms]
              [sym (lookup (keyword (name sym)) sym)]))

      ;; Handle :strs [x y] -> bind x to (get m "x")
      strs-syms
      (into (for [sym strs-syms]
              [sym (lookup (name sym) sym)]))

      ;; Handle :syms [x y] -> bind x to (get m 'x)
      syms-syms
      (into (for [sym syms-syms]
              [sym (lookup (list 'quote sym) sym)]))

      ;; Handle explicit bindings {a :a b :b}
      (seq explicit-bindings)
      (into (for [[sym k] explicit-bindings
                  :when   (not= sym '_)]
              (if (destructure-pattern? sym)
                ;; Nested destructuring
                [(gensym "map__") (lookup k sym)]
                ;; Simple binding
                [sym (lookup k sym)])))

      ;; Handle :as binding
      as-sym
      (conj [as-sym map-sym]))))

;; ============================================================================
;; Unified Expansion
;; ============================================================================

(defn expand-destructuring
  "Expand a destructuring pattern into simple bindings.
   Takes a pattern and a value form, returns a vector of [symbol init-form] pairs.

   For simple symbols, returns [[sym value]].
   For vectors/maps, returns the expanded bindings with gensyms for temp values."
  [pattern value]
  (cond
    ;; Simple symbol - no destructuring needed
    (symbol? pattern)
    [[pattern value]]

    ;; Vector destructuring
    (vector? pattern)
    (let [coll-sym (gensym "vec__")]
      (into [[coll-sym value]]
            (expand-vector-destructuring pattern coll-sym)))

    ;; Map destructuring
    (map? pattern)
    (let [map-sym (gensym "map__")]
      (into [[map-sym value]]
            (expand-map-destructuring pattern map-sym)))

    :else
    (throw (ex-info (str "Invalid binding pattern: " pattern)
                    {:pattern pattern}))))

;; ============================================================================
;; Binding Expansion
;; ============================================================================

(defn- paren-pair-bindings?
  "Detect [(var val) (var2 val2)] style bindings (common in elisp ports).
   Returns true if all elements are 2-element lists starting with a symbol."
  [bindings]
  (and (seq bindings)
       (every? #(and (seq? %) (= 2 (count %)) (symbol? (first %)))
               bindings)))

(defn expand-bindings
  "Expand a let binding vector, handling destructuring.
   Returns a flat vector suitable for a simple let form.
   Auto-flattens [(var val) (var2 val2)] into [var val var2 val2]."
  [bindings]
  (let [bindings (if (paren-pair-bindings? bindings)
                   (vec (mapcat identity bindings))
                   bindings)]
    (->> (partition 2 bindings)
         (mapcat (fn [[pattern init]]
                   (expand-destructuring pattern init)))
         vec)))

;; ============================================================================
;; Function Parameter Processing
;; ============================================================================

(defn- extract-rest-param
  "Extract rest parameter from params vector.
   Returns [regular-params rest-sym] where rest-sym is nil if no & rest."
  [params]
  (let [amp-idx (.indexOf (vec params) '&)]
    (if (neg? amp-idx)
      [params nil]
      [(subvec (vec params) 0 amp-idx)
       (nth params (inc amp-idx))])))

(defn process-fn-params
  "Process function parameters, handling destructuring and rest args.
   Returns a map with:
   - :simple-params - vector of simple symbols for the Elisp function signature
   - :rest-param - the rest parameter symbol (or nil)
   - :destructure-bindings - vector of [pattern gensym] pairs needing expansion
   - :all-locals - set of all local symbols that will be bound"
  [params]
  (let [[regular-params rest-sym]                                        (extract-rest-param params)
        ;; Process regular params
        regular-result
        (reduce (fn [acc param]
                  (if (destructure-pattern? param)
                    ;; Destructuring param - use gensym
                    (let [gsym (gensym "p__")]
                      (-> acc
                          (update :simple-params conj gsym)
                          (update :destructure-bindings conj [param gsym])))
                    ;; Simple param
                    (update acc :simple-params conj param)))
                {:simple-params []
                 :destructure-bindings []}
                regular-params)

        ;; Process rest param if present
        rest-result
        (if rest-sym
          (if (destructure-pattern? rest-sym)
            ;; Rest param with destructuring
            (let [gsym (gensym "rest__")]
              (-> regular-result
                  (assoc :rest-param gsym)
                  (update :destructure-bindings conj [rest-sym gsym])))
            ;; Simple rest param
            (assoc regular-result :rest-param rest-sym))
          regular-result)

        ;; Calculate all locals from destructure bindings
        all-destructure-locals
        (->> (:destructure-bindings rest-result)
             (mapcat (fn [[pattern gsym]]
                       (map first (expand-destructuring pattern gsym))))
             set)]
    (assoc rest-result
           :all-locals (into (set (:simple-params rest-result))
                             (if (:rest-param rest-result)
                               (conj all-destructure-locals (:rest-param rest-result))
                               all-destructure-locals)))))
