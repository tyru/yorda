package main

import "github.com/haya14busa/go-vimlparser/ast"

type fileSet struct {
	files []analyFile
	last  *analyFile
	base  int64
}

func newFileSet(cap int) *fileSet {
	return &fileSet{
		files: make([]analyFile, 0, cap),
		base:  1, // 0 == NoPos
	}
}

// addFile adds given file to this file set.
// addFile returns non-nil error when given filename is already in the file set.
func (s *fileSet) addFile(filename string, size int64, node *ast.File) {
	base := s.base + size + 1 // +1 because EOF also has a position
	f := analyFile{s.base, size, filename, node, nil}
	s.files = append(s.files, f)
	s.base = base
}

// file returns *analyFile by filename.
func (s *fileSet) file(pos ast.Pos) *analyFile {
	if s.last != nil && s.last.name == pos.Filename {
		return s.last
	}
	if pos.Filename == "" {
		return nil
	}
	for i := range s.files {
		if s.files[i].name == pos.Filename {
			s.last = &s.files[i]
			return &s.files[i]
		}
	}
	return nil
}

func (s *fileSet) iterate(f func(*analyFile) bool) {
	for i := range s.files {
		if !f(&s.files[i]) {
			break
		}
	}
}

func (s *fileSet) size() int {
	return len(s.files)
}

type analyFile struct {
	base int64
	size int64
	name string
	node *ast.File
	info *fileNodeInfo
}

type setPos int64 // noPos == 0

func (f *analyFile) toSetPos() setPos {
	return setPos(f.base + f.size)
}
