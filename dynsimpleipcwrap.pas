unit dynsimpleipcwrap;

{$mode objfpc}{$H+}

interface

{$i libdeclare.inc}

const dll = 'dynsimpleipc.dll';
// todo: bsd/linux dso name

{ DLL functions (and needs to be a C Header for GoLang/C/other): }

  {server functions...}
function sIpcCreateServer: int32; cdecl;
external dll;
function sIpcFreeServer: int32; cdecl;
external dll;
function sIpcStartServer(servID: pchar; threaded: int32): int32; cdecl;
external dll;
function sIpcStartServerTest: int32; cdecl;
external dll;
function sIpcPeekMsg(timeout: int32; readopt: int32): int32; cdecl;
external dll;
function sIpcExecOnMsg(peektime: int32; sleeptime: int32;
  cbString: TCallbackString;
  cbInt32: TCallbackInt32;
  cbXY: TCallbackXY;
  cbInts: TCallbackInts;
  cbIntStr: TCallbackIntStr
  ): int32; cdecl;
external dll;
function sIpcExecOnString(peektime: int32; sleeptime: int32; cb: TCallbackString): int32; cdecl;
external dll;
function sIpcExecOnInt32(peektime: int32; sleeptime: int32; cb: TCallbackInt32): int32; cdecl;
external dll;
function sIpcExecOnX4(peektime: int32; sleeptime: int32; cb: TCallbackX4): int32; cdecl;
external dll;
function sIpcExecOnXY(peektime: int32; sleeptime: int32; cb: TCallbackXY): int32; cdecl;
external dll;
function sIpcExecOnIntStr(peektime: int32; sleeptime: int32; cb: TCallbackIntStr): int32; cdecl;
external dll;

{  client functions }
function sIpcCreateClient(ServerID: pchar): int32; cdecl;
external dll;
function sIpcFreeClient: int32; cdecl;
external dll;
function sIpcStartClient(servID: pchar): int32; cdecl;
external dll;
function sIpcSendStringMsg(s: pchar): int32; cdecl;
external dll;
function sIpcSendXYMsg(x: int32; y: int32): int32; cdecl;
external dll;
function sIpcSendInt32Msg(i: int32): int32; cdecl;
external dll;
function sIpcSendIntStrMsg(i: int32; s: pchar): int32; cdecl;
external dll;
function sIpcSendIntsMsg(x1: int32; x2: int32; x3: int32; x4: int32): int32; cdecl;
external dll;

implementation

end.

