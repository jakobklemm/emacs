package main

import (
	"fmt"
	"encoding/hex"
	"path/filepath"
	"crypto/md5"
	"io/ioutil"
	"github.com/thanhpk/randstr"
	"os/user"
	"log"
	"os"
	"time"
)

var (
	// Relative to home directory.
	log_file = "/documents/archive/hoth.log"
)

func main() {
	mode, file := arguments()
	if mode == "md5" {
		hash := hash(file)
		move(mode, file, hash)
	} else {
		uuid := uid(file)
		move(mode, file, uuid)
	}
}

func arguments() (string, string) {
	length := len(os.Args)
	if length == 3 {
		return os.Args[1], os.Args[2]
	} else {
		log.Fatalf("Arguments erronr!")
		return "error", "fatal"
	}
}

func move(directory, local, name string) {
	user, err := user.Current()
    if err != nil {
        log.Fatalf(err.Error())
	}
	homeDirectory := user.HomeDir
	archive := homeDirectory + "/documents/files/" + directory + "/"
	rel := event(local, name, directory)
	e := os.Rename(local, archive + name)
    if e != nil {
        log.Fatal(e)
    } else {
		status := save(rel)
		if status {
			fmt.Println("file:" + "~/documents/files/" + directory + "/" + name)

		} else {
			fmt.Println("ERROR][ERROR")
		}
	}
}

func hash(file string) string{
	data, err := ioutil.ReadFile(file)
    if err != nil {
        fmt.Println("File reading error", err)
        return "Error"
    }
	content := string(data)
	hash := md5.Sum([]byte(content))
	pass := hex.EncodeToString(hash[:])
	ftype := filepath.Ext(file)
	return pass + ftype
}

func uid(file string) string {
	token := randstr.Hex(16)
	ftype := filepath.Ext(file)
	return token + ftype
}

func event(filename, id, mode  string) string {
	now := time.Now().Format(time.ANSIC)
	res := now + "-" + filename + "-" + id + "-" + mode +"\n"
	return res
}

func save(event string) bool {
	user, err := user.Current()
    if err != nil {
		return false
	}
	homeDirectory := user.HomeDir
	path := homeDirectory + log_file
	f, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY, 0600)
	if err != nil {
		return false
	}
	defer f.Close()
	if _, err = f.WriteString(event); err != nil {
		return false
	} else {
		return true
	}
}
