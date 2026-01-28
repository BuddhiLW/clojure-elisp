package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/BuddhiLW/bonzai"
)

var CompileCmd = &bonzai.Cmd{
	Name:    "compile",
	Alias:   "c",
	Short:   "compile cljel files to elisp",
	Usage:   "<file.cljel|dir/> [-o output]",
	MinArgs: 1,
	MaxArgs: 3,
	Mcp: &bonzai.McpMeta{
		Desc: "Compile ClojureElisp (.cljel) files to Emacs Lisp (.el)",
		Params: []bonzai.McpParam{
			{Name: "input", Desc: "Input .cljel file or directory path", Type: "string", Required: true},
			{Name: "output", Desc: "Output .el file or directory path", Type: "string"},
		},
	},
	Do: func(x *bonzai.Cmd, args ...string) error {
		input := args[0]
		output := parseOutputFlag(args[1:])

		info, err := os.Stat(input)
		if err != nil {
			return fmt.Errorf("cannot access %s: %w", input, err)
		}

		if info.IsDir() {
			return compileDir(input, output)
		}
		return compileFile(input, output)
	},
}

func parseOutputFlag(args []string) string {
	for i := 0; i < len(args); i++ {
		if args[i] == "-o" && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

func compileFile(input, output string) error {
	if output == "" {
		output = strings.TrimSuffix(input, filepath.Ext(input)) + ".el"
	}

	absInput, err := filepath.Abs(input)
	if err != nil {
		return fmt.Errorf("resolving input path: %w", err)
	}
	absOutput, err := filepath.Abs(output)
	if err != nil {
		return fmt.Errorf("resolving output path: %w", err)
	}

	if err := os.MkdirAll(filepath.Dir(absOutput), 0755); err != nil {
		return fmt.Errorf("creating output directory: %w", err)
	}

	expr := fmt.Sprintf(
		`(require '[clojure-elisp.core :as clel]) (let [r (clel/compile-file "%s" "%s")] (println (str "Compiled " (:input r) " -> " (:output r) " (" (:size r) " chars)")))`,
		escapeCljString(absInput), escapeCljString(absOutput),
	)

	return runClojure(expr)
}

func compileDir(input, output string) error {
	if output == "" {
		output = input
	}

	var files []string
	err := filepath.Walk(input, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && strings.HasSuffix(path, ".cljel") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("scanning directory: %w", err)
	}

	if len(files) == 0 {
		return fmt.Errorf("no .cljel files found in %s", input)
	}

	for _, f := range files {
		rel, _ := filepath.Rel(input, f)
		outFile := filepath.Join(output, strings.TrimSuffix(rel, ".cljel")+".el")
		if err := compileFile(f, outFile); err != nil {
			return err
		}
	}
	return nil
}

func escapeCljString(s string) string {
	s = strings.ReplaceAll(s, `\`, `\\`)
	s = strings.ReplaceAll(s, `"`, `\"`)
	return s
}

func findProjectRoot() (string, error) {
	if home := os.Getenv("CLEL_HOME"); home != "" {
		if _, err := os.Stat(filepath.Join(home, "deps.edn")); err == nil {
			return home, nil
		}
		return "", fmt.Errorf("CLEL_HOME=%s does not contain deps.edn", home)
	}

	dir, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("getting working directory: %w", err)
	}

	for {
		if _, err := os.Stat(filepath.Join(dir, "deps.edn")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}

	return "", fmt.Errorf("cannot find project root (no deps.edn found); set CLEL_HOME env var")
}

func runClojure(expr string) error {
	root, err := findProjectRoot()
	if err != nil {
		return err
	}

	clj, err := exec.LookPath("clojure")
	if err != nil {
		return fmt.Errorf("clojure CLI not found on PATH: %w", err)
	}

	cmd := exec.Command(clj, "-M", "-e", expr)
	cmd.Dir = root
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
