{ Server delphi example for Simple IPC DLL (inter process communication) }

program ipcserv;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  DynSimpleIpcWrap;

const
  SERVER_ID = '12345';

var
  RecvdStop: boolean = false;

procedure CallbackInts(x1: int32; x2: int32; x3: int32; x4: int32); cdecl;
begin
  // stop the server on four zeros msg
  if (x1=0) and (x2=0) and (x3=0) and (x4=0) then begin
    writeln('stopping server');
    RecvdStop := true;
  end else begin
	  writeln('Msg recvd (multiple ints): ',x1, ', ', x2, ', ', x3, ', and ', x4);
  end;
end;

procedure StartServer;
begin
	sIpcCreateServer;
	sIpcStartServer(SERVER_ID, OPT_NO_THREAD);
	writeln('IPC Server running');
  writeln('Checking for messages...');
	repeat
		// setup a callback when four integers are received
    sIpcExecOnInts(10, 10, CallbackInts);
  until RecvdStop = true;

	sIpcFreeServer;
end;

begin
  StartServer;

  writeln('Program finished.');
  readln;
end.
