{
  IPC functions for creating and running ipc client

  Todo:
    - custom message type? integer above a fixed number like mtCustom (integer over 100)
    - Google protocol buffers optional

  Copyright 2017, Z505 Software
  License: BSD/MIT

  Todo:
    -could SetLastErrors whenever there is an exception in DLL, and pass on
     detailed error information into a function that sets a global variable
     similar to GetLastError}


unit sIpcClient;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SimpleIPC, SysUtils;

{$I libdeclare.inc}

procedure DefaultStatusLn(s: string);

var StatusLn: procedure (s: string) = @DefaultStatusLn;

implementation

var IpcCli: TSimpleIPCClient;

procedure DefaultStatusLn(s: string);
begin
  writeln(':: ' + s);
end;

// write a status, "yes" if boolean true, "no" otherwise
procedure StatusYesLn(b: boolean; s: string);
begin
  if b then StatusLn(s +'yes') else StatusLn(s +'no');
end;

{ returns errors:
   0: success
   1: ipc client already created, can't create until freed first }
function sIpcCreateClient(): int32; cdecl;
begin
  if assigned(IpcCli) then exit(1);
  IpcCli := TSimpleIPCClient.Create(nil);
  result := 0;
end; exports sIpcCreateClient;

{ returns errors:
    0: success
    1: ipc client not assigned (is nil), can't free variable that doesn't exist }
function sIpcFreeClient: int32; cdecl;
begin
  if not assigned(IpcCli) then exit(1);
  IpcCli.free; IpcCli := nil;
  result := 0;
end; exports sIpcFreeClient;

{ returns errors:
    0: success
    1: IpcCli not assigned (is nil), not created
    2: error connecting to server }
function sIpcStartClient(servID: pchar): int32; cdecl;
begin
  if not assigned(IpcCli) then exit(1);

  try
    IpcCli.ServerID := servID;
    IpcCli.Connect;
  except
    exit(2);
  end;
  result := 0;
end; exports sIpcStartClient;

{ General function to send any msg type
  Returns errors:
    0: success
    1: ipc client not assigned (is nil), it is not created
    2: ipc client not active
    3: ipc server is not running }
function SendMsg(msgType: int32; x1: int32; x2: int32; x3: int32; x4: int32; s: pchar): int32;
begin
  if not assigned(IpcCli) then exit(1);
  if not IpcCli.Active then exit(2);
  if not IpcCli.ServerRunning then exit(3);

  case msgType of
    mtUnknown: { not implemented } ;
    mtString: IpcCli.SendStringMessage(msgType, s);
    mtInt32:  IpcCli.SendStringMessage(msgType, inttostr(x1));
    mtXY:     IpcCli.SendStringMessage(msgType, inttostr(x1)+';'+inttostr(x2));
    mtInts:   IpcCli.SendStringMessage(msgType, inttostr(x1)+';'+inttostr(x2)+';'+inttostr(x3)+';'+inttostr(x4));
    mtIntStr: IpcCli.SendStringMessage(msgType, inttostr(x1)+';'+s);
  end;
  result := 0;
end;

function sIpcSendStringMsg(s: pchar): int32; cdecl;
begin
  result := SendMsg(mtString,0,0,0,0,s);
end; exports sIpcSendStringMsg;

function sIpcSendXYMsg(x: int32; y: int32): int32; cdecl;
begin
  result := SendMsg(mtXY,x,y,0,0,nil);
end; exports sIpcSendXYMsg;

function sIpcSendInt32Msg(i: int32): int32; cdecl;
begin
  result := SendMsg(mtInt32,i,0,0,0,nil);
end; exports sIpcSendInt32Msg;

function sIpcSendIntStrMsg(i: int32; s: pchar): int32; cdecl;
begin
  result := SendMsg(mtInt32,i,0,0,0,s);
end; exports sIpcSendIntStrMsg;

function sIpcSendIntsMsg(x1: int32; x2: int32; x3: int32; x4: int32): int32; cdecl;
begin
  result := SendMsg(mtInt32,x1,x2,x3,x4,nil);
end; exports sIpcSendIntsMsg;

end.

