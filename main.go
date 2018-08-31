package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/haya14busa/go-vimlparser"
)

func main() {
	os.Exit(yordaMain())
}

func yordaMain() int {
	filenames, err := walkFiles(os.Args[1:])
	if err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		return 1
	}
	fset := newFileSet(len(filenames))
	for _, name := range filenames {
		r, err := os.Open(name)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", name, err.Error())
			return 2
		}
		fi, err := r.Stat()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", name, err.Error())
			return 3
		}
		file, err := vimlparser.ParseFile(r, name, nil)
		r.Close()
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %s\n", name, err.Error())
			return 4
		}
		fset.addFile(name, fi.Size(), file)
	}
	analyzer := newAnalyzer(fset)
	err = analyzer.run()
	if err != nil {
		fmt.Fprintln(os.Stderr, strerror(err))
		return 10
	}
	// TODO query dumped prolog code to prolog processor
	fset.Iterate(func(f *analyFile) bool {
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

func strerror(err error) string {
	if os.Getenv("YORDA_DEBUG") != "" {
		return fmt.Sprintf("%+v", err) // with stacktrace
	}
	return err.Error()
}
