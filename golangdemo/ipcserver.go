package main

/*
#cgo LDFLAGS: -L${SRCDIR}/../loaddlldemo -ldynsimpleipc

#include "simpleipc.h"

extern void callback(char* s);
*/
import "C"

import (
	"unsafe"
	"fmt"
	"time"
	"bufio"
	"os"
//	"syscall"
//	"log"
)

const (
	OPT_NO_THREAD = 0
	OPT_THREAD = 1
)

const SERVER_ID = "123"

var reader *bufio.Reader

// sleep for number of seconds
func sleepSec(sec time.Duration) {
	time.Sleep(sec * time.Second)
}

// sleep for number of milliseconds
func sleepMillisec(millisec time.Duration) {
	time.Sleep(millisec * time.Millisecond)
}

func Example1() {
	C.sIpcCreateServerTest()
	C.sIpcStartServer()
	// sleepMillisec(100)
	C.sIpcFreeServer()
}

var recvdstop bool = false

//export callback
func callback(s *C.char) {
	gostr := C.GoString(s)  // convert to golang string
	if gostr == "stop" {
		recvdstop = true
	}
	fmt.Println("Msg recvd: ", gostr)
}

func Example2() {
	cSERVER_ID := C.CString(SERVER_ID)  // convert to c string
	C.sIpcCreateServer(cSERVER_ID, OPT_NO_THREAD)
	C.sIpcStartServer()
	fmt.Println("IPC Server running")
    fmt.Println("Checking for messages...")
	for {
        C.sIpcExecOnMsg(10,10,(C.TCallbackString)(unsafe.Pointer(C.callback)),nil,nil,nil,nil)
        if recvdstop {
			break
		}
	}
	C.sIpcFreeServer()
}


func main() {
	reader = bufio.NewReader(os.Stdin)
	fmt.Println("Go program started")
	Example1()
	Example2()
	fmt.Println("Go program done")
}