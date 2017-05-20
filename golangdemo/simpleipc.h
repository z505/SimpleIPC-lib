/* Header file for importing SimpleIPC dll
   Note: golang/fpc int32 is a C long */
/*
  TCallbackString = procedure(s: pchar); cdecl;
  TCallbackInt32 = procedure(x: int32); cdecl;
  TCallbackXY = procedure(x,y: int32); cdecl;
  TCallbackX4 = procedure(x1,x2,x3,x4: int32); cdecl;
  TCallbackIntStr = procedure(i: int32; s: pchar); cdecl;
*/

typedef void (__cdecl *TCallbackString)(char*);
typedef void (__cdecl *TCallbackInt32)(long);
typedef void (__cdecl *TCallbackXY)(long, long);
typedef void (__cdecl *TCallbackX4)(long, long, long, long);
typedef void (__cdecl *TCallbackIntStr)(long, char*);

extern long __cdecl
sIpcCreateServer(char* ServerID, long threaded);

extern void __cdecl
sIpcCreateServerTest();

extern long __cdecl
sIpcFreeServer();

extern long __cdecl
sIpcStartServer();

extern long __cdecl
sIpcPeekMsg(long timeout, long readopt);

extern long __cdecl
sIpcExecOnMsg(long peektime, long sleeptime,
  TCallbackString cbString,
  TCallbackInt32 cbInt32,
  TCallbackXY cbXY,
  TCallbackX4 cbX4,
  TCallbackIntStr cbIntStr);

//sIpcExecOnString(peektime: int32; sleeptime: int32; cb: TCallbackString): int32;

//sIpcExecOnInt32(peektime: int32; sleeptime: int32; cb: TCallbackInt32): int32;

//sIpcExecOnX4(peektime: int32; sleeptime: int32; cb: TCallbackX4): int32;

//sIpcExecOnXY(peektime: int32; sleeptime: int32; cb: TCallbackXY): int32;

//sIpcExecOnIntStr(peektime: int32; sleeptime: int32; cb: TCallbackIntStr): int32;

