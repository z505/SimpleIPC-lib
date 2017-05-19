unit dynsimpleipcwrap;

{$mode objfpc}{$H+}

interface

{$i libdeclare.inc}

const dll = 'dynsimpleipc.dll';
// todo: bsd/linux dso name

// DLL functions (and needs to be a C Header for golang/C):

function sIpcCreateIpcServer(ServerID: pchar; threaded: int32): int32; cdecl;
external dll;
procedure sIpcCreateIpcServerTest; cdecl;
external dll;
function sIpcFreeIpcServer: int32; cdecl;
external dll;
function sIpcStartIpcServer: int32; cdecl;
external dll;
function sIpcPeekMsg(timeout: int32; readopt: int32): int32; cdecl;
external dll;
function sIpcExecOnMsg(peektime: int32; sleeptime: int32;
  cbString: TCallbackString;
  cbInt32: TCallbackInt32;
  cbXY: TCallbackXY;
  cbX4: TCallbackX4;
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


implementation

end.

