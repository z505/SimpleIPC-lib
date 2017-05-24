/* Header file for importing SimpleIPC dll
   Note: golang/fpc int32 is a C long, but on 64bit long can be int64, so use
         int in C

   Copyright 2017, Z505 Software
   License: BSD/MIT

*/

/*
  TCallbackString = procedure(s: pchar); cdecl;
  TCallbackInt32 = procedure(x: int32); cdecl;
  TCallbackXY = procedure(x,y: int32); cdecl;
  TCallbackInts = procedure(x1,x2,x3,x4: int32); cdecl;
  TCallbackIntStr = procedure(i: int32; s: pchar); cdecl;
*/

typedef void (__cdecl *TCallbackString)(char*);
typedef void (__cdecl *TCallbackInt32)(int);
typedef void (__cdecl *TCallbackXY)(int, int);
typedef void (__cdecl *TCallbackInts)(int, int, int, int);
typedef void (__cdecl *TCallbackIntStr)(int, char*);

extern int __cdecl
sIpcCreateServer();

extern int __cdecl
sIpcStartServerTest();

extern int __cdecl
sIpcFreeServer();

extern int __cdecl
sIpcStartServer(char* servID, int threaded);

extern int __cdecl
sIpcPeekMsg(int timeout, int readopt);

extern int __cdecl
sIpcExecOnMsg(int peektime, int sleeptime,
  TCallbackString cbString,
  TCallbackInt32 cbInt32,
  TCallbackXY cbXY,
  TCallbackInts cbInts,
  TCallbackIntStr cbIntStr);

// optional todo:

//sIpcExecOnString(peektime: int32; sleeptime: int32; cb: TCallbackString): int32;

//sIpcExecOnInt32(peektime: int32; sleeptime: int32; cb: TCallbackInt32): int32;

//sIpcExecOnInts(peektime: int32; sleeptime: int32; cb: TCallbackX4): int32;

//sIpcExecOnXY(peektime: int32; sleeptime: int32; cb: TCallbackXY): int32;

//sIpcExecOnIntStr(peektime: int32; sleeptime: int32; cb: TCallbackIntStr): int32;

