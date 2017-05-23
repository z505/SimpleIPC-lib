/* SimpleIPC demo server in go that uses the simple ipc dll
   Run the fpc client program to send a message to this go app
   
   Copyright 2017, Z505 Software
   License: BSD/MIT  */

package main

/*
#cgo LDFLAGS: -L${SRCDIR}/../loaddlldemo -ldynsimpleipc

#include "simpleipc.h"

// Go code linked up to these C names
extern void CallbackString(char* s);
extern void CallbackInt32(int i);
// note: long vs int, vs int32.. fix this inconsistency
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

var recvdstop bool = false

//export CallbackString
func CallbackString(s *C.char) {
	gostr := C.GoString(s)  // convert to golang string
	if gostr == "stop" {
		recvdstop = true
	}
	fmt.Println("Msg recvd (string): ", gostr)
}

//export CallbackInt32
func CallbackInt32(i int32) {
	fmt.Println("Msg recvd (int32):  ", i)
}

func Example1() {
	C.sIpcCreateServerTest()
	C.sIpcStartServer()
	// sleepMillisec(100)
	C.sIpcFreeServer()
}

func Example2() {
	cSERVER_ID := C.CString(SERVER_ID)  // convert to c string
	C.sIpcCreateServer(cSERVER_ID, OPT_NO_THREAD)
	C.sIpcStartServer()
	fmt.Println("IPC Server running")
    fmt.Println("Checking for messages...")
	for {
		// setup a callback when a msg is received
        C.sIpcExecOnMsg(10,10,
			(C.TCallbackString)(unsafe.Pointer(C.CallbackString)),  // one for a string message
			(C.TCallbackInt32)(unsafe.Pointer(C.CallbackInt32)), // one for an integer message
			nil,
			nil,
			nil)
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