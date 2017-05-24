/* SimpleIPC demo server in go that uses the simple ipc dll
   Run the fpc client program to send a message to this go app

   Copyright 2017, Z505 Software
   License: BSD/MIT  */

package main

/*
#cgo LDFLAGS: -L${SRCDIR} -ldynsimpleipc

#include "simpleipc.h"

// Go code linked up to these C names
extern void CallbackString(char* s);
extern void CallbackInt32(int i);
extern void CallbackXY(int x, int y);
extern void CallbackIntStr(int x, char* s);
extern void CallbackInts(int x1, int x2, int x3, int x4);
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

//export CallbackIntStr
func CallbackIntStr(i int32, s *C.char) {
	gostr := C.GoString(s)  // convert to golang string
	fmt.Println("Msg recvd (int and string): int:", i, "string:", gostr)
}

//export CallbackXY
func CallbackXY(x int32, y int32,) {
	fmt.Println("Msg recvd (two ints): ",x, "and", y)
}

//export CallbackInts
func CallbackInts(x1 int32, x2 int32, x3 int32, x4 int32) {
	fmt.Println("Msg recvd (multiple ints): ",x1, "and", x2, "and", x3, "and", x4)
}

func Example1() {
	C.sIpcCreateServer()
	C.sIpcStartServerTest()
	// sleepMillisec(100)
	C.sIpcFreeServer()
}

func Example2() {
	cSERVER_ID := C.CString(SERVER_ID)  // convert to c string
	C.sIpcCreateServer()
	C.sIpcStartServer(cSERVER_ID, OPT_NO_THREAD)
	fmt.Println("IPC Server running")
    fmt.Println("Checking for messages...")
	for {
		// setup a callback when a msg is received
        C.sIpcExecOnMsg(10,10,
			(C.TCallbackString)(unsafe.Pointer(C.CallbackString)),  // one for a string message
			(C.TCallbackInt32)(unsafe.Pointer(C.CallbackInt32)), // one for an integer message
            (C.TCallbackXY)(unsafe.Pointer(C.CallbackXY)),
            (C.TCallbackInts)(unsafe.Pointer(C.CallbackInts)),
			(C.TCallbackIntStr)(unsafe.Pointer(C.CallbackIntStr)))
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