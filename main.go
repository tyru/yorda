package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/haya14busa/go-vimlparser"
	"github.com/haya14busa/go-vimlparser/ast"
)

func main() {
	os.Exit(yordaMain())
}

func usage() {
	fmt.Println(`Usage: yorda [SCRIPTS...]`)
}

func yordaMain() int {
	if len(os.Args) < 2 {
		usage()
		return 0
	}
	fset, err := parseArgs(os.Args[1:])
	if err != nil {
		fmt.Fprintln(os.Stderr, strerror(err))
		return 1
	}
	analyzer := newAnalyzer(fset)
	err = analyzer.run()
	if err != nil {
		fmt.Fprintln(os.Stderr, strerror(err))
		return 10
	}
	// TODO query dumped prolog code to prolog processor
	fset.iterate(func(f *analyFile) bool {
		r := newConverter(f).toReader(f.node)
		_, err = io.Copy(os.Stdout, r)
		fmt.Println()
		return err == nil
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, strerror(err))
		return 20
	}
	return 0
}

func parseArgs(args []string) (*fileSet, error) {
	fset := newFileSet(64)
	var lasterr error

	for i := range args {
		filepath.Walk(args[i], func(path string, info os.FileInfo, err error) error {
			if err != nil {
				lasterr = err
				return nil
			}
			parseVimFile(path, func(name string, size int64, file *ast.File, err error) {
				if err != nil {
					lasterr = err
				} else {
					fset.addFile(name, size, file)
				}
			})
			return nil
		})
		if lasterr != nil {
			return nil, lasterr
		}
	}
	if lasterr != nil {
		return nil, lasterr
	}
	return fset, nil
}

func parseVimFile(name string, callback func(string, int64, *ast.File, error)) {
	r, err := os.Open(name)
	if err != nil {
		callback("", 0, nil, err)
		return
	}
	fi, err := r.Stat()
	if err != nil {
		callback("", 0, nil, err)
		return
	}
	buf := bufio.NewReaderSize(r, int(fi.Size())+1)
	file, err := vimlparser.ParseFile(buf, name, nil)
	r.Close()
	if err != nil {
		callback("", 0, nil, err)
		return
	}
	callback(name, fi.Size(), file, nil)
}

func strerror(err error) string {
	if os.Getenv("YORDA_DEBUG") != "" {
		return fmt.Sprintf("%+v", err) // with stacktrace
	}
	return err.Error()
}
