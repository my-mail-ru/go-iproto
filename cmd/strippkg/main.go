package main

import (
	"fmt"
	"log"
	"os"

	"github.com/my-mail-ru/go-iproto/iprotogen"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "usage: %s <path-to-package>\n", os.Args[0])
		os.Exit(1)
	}

	oldPkgDir := os.Args[1]

	_, newPkgDir, err := iprotogen.StripPackage(oldPkgDir, true)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(newPkgDir)
}
