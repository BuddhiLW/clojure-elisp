package main

import (
	"fmt"

	"github.com/BuddhiLW/bonzai"
)

const version = "0.1.0"

var Root = &bonzai.Cmd{
	Name:  "clel",
	Short: "clojure-elisp compiler cli",
	Vers:  version,
	Long: `ClojureElisp compiler — write Clojure, emit Emacs Lisp.

Commands:
  compile  Compile .cljel files to .el
  watch    Watch directory and recompile on changes
  mcp      Start MCP server on stdio
  version  Print version`,

	Do: func(x *bonzai.Cmd, args ...string) error {
		fmt.Printf("%s %s — %s\n\n%s\n", x.Name, x.Vers, x.Short, x.Long)
		return nil
	},
}

var VersionCmd = &bonzai.Cmd{
	Name:   "version",
	Alias:  "v",
	Short:  "print version information",
	NoArgs: true,
	Do: func(x *bonzai.Cmd, args ...string) error {
		fmt.Println("clel " + version)
		return nil
	},
}

func init() {
	Root.Cmds = []*bonzai.Cmd{
		CompileCmd,
		WatchCmd,
		McpCmd,
		VersionCmd,
	}
}

func main() {
	Root.Exec()
}
