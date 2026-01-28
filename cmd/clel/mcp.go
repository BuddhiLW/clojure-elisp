package main

import (
	"github.com/BuddhiLW/bonzai"
	mcpsrv "github.com/BuddhiLW/bonzai/mcp"
	"github.com/mark3labs/mcp-go/server"
)

var McpCmd = &bonzai.Cmd{
	Name:   "mcp",
	Short:  "start mcp server on stdio",
	NoArgs: true,
	Do: func(x *bonzai.Cmd, args ...string) error {
		s := mcpsrv.NewServer(Root, mcpsrv.OnlyTagged(), mcpsrv.WithVersion(Root.Vers))
		return server.ServeStdio(s)
	},
}
