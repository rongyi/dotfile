package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

var (
	noteFile string
)

const (
	noteSep string = `==========`
)

func argParse() {
	flag.StringVar(&noteFile, "note", "note.txt", "specify kindle note file")
	flag.Parse()
}

// group note by book and order it by page

func main() {
	argParse()

	b, err := ioutil.ReadFile(noteFile)
	if err != nil {
		log.Fatalf("read file err: %v", err)
	}
	nss, err := parse(string(b))
	if err != nil {
		log.Fatal(err)
	}
	group := make(map[string][]string)
	for _, ns := range nss {
		group[ns.BookName] = append(group[ns.BookName], ns.Content)
	}

	for bookName, contents := range group {
		fmt.Println(noteSep)
		fmt.Printf(">> %s\n", bookName)
		fmt.Println("")
		fmt.Println(strings.Join(contents, "\n\n"))
	}
}

type NoteSection struct {
	BookName string
	Place    string
	Content  string
}

func parse(raw string) ([]NoteSection, error) {
	total := len(raw)
	spIndex := strings.Index(raw, noteSep)
	ret := []NoteSection{}

	for spIndex != -1 {
		cur := NoteSection{}
		secStr := raw[:spIndex]
		lines := strings.Split(secStr, "\n")
		cur.BookName = lines[0]
		cur.Place = lines[1]
		cur.Content = lines[3]
		// may be a book mark
		if cur.Content != "" {
			ret = append(ret, cur)
		}

		// update
		next := spIndex + len(noteSep) + 1 // skip the whole line with end '\n'
		if next >= total {
			break
		}
		raw = raw[next:] // skip the total line contain '\n'
		spIndex = strings.Index(raw, noteSep)
	}

	return ret, nil
}
