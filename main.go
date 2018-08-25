package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/haya14busa/go-vimlparser"
)

func main() {
	os.Exit(vainMain())
}

func vainMain() int {
	filenames, err := walkFiles(os.Args[1:])
	if err != nil {
		fmt.Fprintf(os.Stderr, err.Error())
		return 1
	}
	fset := newFileSet(len(filenames))
	for _, name := range filenames {
		r, err := os.Open(name)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %+v\n", name, err)
			return 2
		}
		fi, err := r.Stat()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %+v\n", name, err)
			return 3
		}
		file, err := vimlparser.ParseFile(r, name, nil)
		r.Close()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %+v\n", name, err)
			return 4
		}
		err = fset.AddFile(name, fi.Size(), file)
		if err != nil { // it must not occur (duplicate files)
			fmt.Fprintf(os.Stderr, "fatal: %+v\n", err)
			return 5
		}
	}
	err = analyze(fset)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%+v\n", err)
		return 6
	}
	return 0
}

func walkFiles(args []string) ([]string, error) {
	files := make([]string, 0, 32)
	for i := range args {
		if err := filepath.Walk(args[i], func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			files = append(files, path)
			return nil
		}); err != nil {
			return nil, err
		}
	}
	return files, nil
}
