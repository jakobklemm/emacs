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
		log.Fatalf("Arguments error!")
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
	fmt.Println("file:" + "~/documents/files/" + directory + "/" + name)
	os.Rename(local, archive + name)
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
