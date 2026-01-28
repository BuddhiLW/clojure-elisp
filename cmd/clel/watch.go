package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/BuddhiLW/bonzai"
	"github.com/fsnotify/fsnotify"
)

var WatchCmd = &bonzai.Cmd{
	Name:    "watch",
	Alias:   "w",
	Short:   "watch and recompile on changes",
	Usage:   "<dir/> [-o outdir/]",
	MinArgs: 1,
	MaxArgs: 3,
	Mcp: &bonzai.McpMeta{
		Desc:      "Watch directory for .cljel changes and recompile automatically",
		Streaming: true,
		Params: []bonzai.McpParam{
			{Name: "dir", Desc: "Directory to watch for .cljel file changes", Type: "string", Required: true},
			{Name: "output", Desc: "Output directory for compiled .el files", Type: "string"},
		},
	},
	Do: func(x *bonzai.Cmd, args ...string) error {
		dir := args[0]
		output := parseOutputFlag(args[1:])
		return watchDir(dir, output)
	},
}

func watchDir(dir, output string) error {
	if output == "" {
		output = dir
	}

	absDir, err := filepath.Abs(dir)
	if err != nil {
		return fmt.Errorf("resolving directory: %w", err)
	}

	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		return fmt.Errorf("creating watcher: %w", err)
	}
	defer watcher.Close()

	// Watch directory tree
	err = filepath.Walk(absDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return watcher.Add(path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("watching %s: %w", absDir, err)
	}

	fmt.Printf("Watching %s for .cljel changes (ctrl-c to stop)\n", absDir)

	// Initial compilation
	if err := compileDir(absDir, output); err != nil {
		fmt.Fprintf(os.Stderr, "Initial compile error: %v\n", err)
	}

	var debounce *time.Timer

	for {
		select {
		case event, ok := <-watcher.Events:
			if !ok {
				return nil
			}
			if !strings.HasSuffix(event.Name, ".cljel") {
				continue
			}
			if event.Op&(fsnotify.Write|fsnotify.Create) == 0 {
				continue
			}

			if debounce != nil {
				debounce.Stop()
			}
			changed := event.Name
			debounce = time.AfterFunc(100*time.Millisecond, func() {
				rel, _ := filepath.Rel(absDir, changed)
				outFile := filepath.Join(output, strings.TrimSuffix(rel, ".cljel")+".el")
				fmt.Printf("[%s] Compiling %s\n", time.Now().Format("15:04:05"), rel)
				if err := compileFile(changed, outFile); err != nil {
					fmt.Fprintf(os.Stderr, "  error: %v\n", err)
				}
			})

		case err, ok := <-watcher.Errors:
			if !ok {
				return nil
			}
			fmt.Fprintf(os.Stderr, "Watch error: %v\n", err)
		}
	}
}
