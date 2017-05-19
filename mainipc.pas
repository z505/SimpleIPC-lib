{

  Todo:
    -could SetLastErrors whenever there is an exception in DLL, and pass on
     detailed error information into a function that sets a global variable}


unit mainipc;

{$mode objfpc}{$H+}

// Disable this normally, will make program bigger if including verification tests
{...$DEFINE TESTINGON}

interface

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SimpleIPC, SysUtils;

{$I libdeclare.inc}

const TEST_SERVER_ID = '12345test'; // server id for default testing

type
  TIpc = Class(TObject)
    Srv : TSimpleIPCServer;
    Threaded: Boolean;
    DoStop: Boolean;
    procedure MessageQueued(Sender: TObject);
    procedure Start;
    procedure PrintMessage;
  end;

var
  ipc: TIpc;

procedure DefaultStatusLn(s: string);

var StatusLn: procedure (s: string) = @DefaultStatusLn;

{$ifdef TESTINGON}
  procedure ParseIntTests;
  procedure ParseIntStrTests;
{$endif}

implementation

procedure DefaultStatusLn(s: string);
begin
  writeln(':: ' + s);
end;

// write a status, "yes" if boolean true, "no" otherwise
procedure StatusYesLn(b: boolean; s: string);
begin
  if b then StatusLn(s +'yes') else StatusLn(s +'no');
end;

procedure TIpc.PrintMessage;
var
  s: string;
begin
  s := srv.StringMessage;
  //check message type, todo: how to put this in DLL?
  // if srv.msgtype = SOMETHING then
  // ...
  StatusLn('Received message: '+S);
  DoStop := DoStop or (s='stop');
end;

// todo: callback mechanism for threads?
// this function is currently not implemented
procedure TIpc.MessageQueued(Sender: TObject);
begin
  srv.ReadMessage;
  // PrintMessage;
end;

procedure TIpc.Start;
begin
  srv.StartServer(self.Threaded);
end;

{ Parameters:
  timeout: a good default to use is 10
  readopt: MSG_READ causes a readmessage to occur, MSG_NO_READ does not
  returns errors:
  0: success
  1: error when peeking for message
  2: exception when peeking for message
  3: ipc variable doesn't exist (is nil), server probably not created
  4: ipc server doesn't exist (is nil), server probably not created
  5: ipc server not active
}
function sIpcPeekMsg(timeout: int32; readopt: int32): int32; cdecl;
var peekrslt: boolean;
begin
  if not assigned(ipc) then exit(3);
  if not assigned(ipc.srv) then exit(4);
  if not ipc.srv.active then exit(5);
  try
    case readopt of
      MSG_READ: peekrslt := ipc.srv.PeekMessage(timeout, true);
      MSG_NO_READ: peekrslt := ipc.srv.PeekMessage(timeout, false);
    end;
  except
    exit(2);
  end;
  if peekrslt then result := 0 else result := 1;
end; exports sIpcPeekMsg;

(*
{
  // NOT implemented yet
  Try reading a message as a string
  Memory must be allocated for pchar buffer by caller.. make sure length of
  buffer is correct

  Returns
    1: error if reading message
    2: if msg available is not a string
    3: ipc variable does not exist (is nil), server probably not created
    4: ipc server does not exist (is nil), server probably not created
    5: ipc server not active
}
function sIpcReadMsgAsString(buf: pchar): int32; cdecl;
begin
  result := 0;
  if not assigned(ipc) then exit(3);
  if not assigned(ipc.srv) then exit(4);
  if not ipc.srv.active exit(5);

  try
    ipc.srv.ReadMessage;
    // allocate memory for pchar by caller.. make sure length of buffer is correct
    if ipc.srv.MsgType = mtString then begin
      // TODO: copy into buffer here
      // is this function really needed? or is the callback mechanism good
      // enough since callback doesn't require memory allocation and this
      // function does?

      // copy( buf
    end else begin
      result := 2;
    end;
  except
    exit(1);
  end;
end; exports sIpcReadMsgAsString;
*)

{ Converts string to 4 integers, separated by ;  ...i.e. 3;35;1;9833
  No trailing semi colon at very end
  todo: could just send binary instead of string, fixed length padding, might
  be more efficient (how much?)
}
function parseInts(s: string; out val1, val2, val3, val4: integer): boolean;
var i, delimcnt: integer;
    tmp: string = '';
    success: boolean;
begin
  result := true;
  delimcnt := 1;
  i := 0;
  val1 := 0; val2 := 0;  val3 := 0; val4 := 0;
  while i < length(s) do begin
    inc(i);
    if s[i] = ';' then begin
      inc(delimcnt);
      tmp := '';
      continue;
    end else begin
      tmp := tmp + s[i];
    end;
    case delimcnt of
      1: begin
           success := trystrtoint(tmp, val1);
           if not success then exit(false);
         end;
      2: begin
           success := trystrtoint(tmp, val2);
           if not success then exit(false);
         end;
      3: begin
           success := trystrtoint(tmp, val3);
           if not success then exit(false);
         end;
      4: begin
           success := trystrtoint(tmp, val4);
           if not success then exit(false);
         end;
    end;
  end;
end;

{ Returns false if couldn't parse integer and string combination, delimited by
  semicolon i.e. 7632;somestring of text. TODO: could just do binary, fixed
  length padding, if faster }
function parseIntStr(inputstr: string; out outI: integer; var outS: string): boolean;
var i: integer;
    tmp: string = '';
    FirstSemiColFound, success: boolean;
begin
  result := false;
  FirstSemiColFound := false;
  outI := 0;
  outS := '';
  i := 0;
  while i < length(inputstr) do begin
    inc(i);
    if not FirstSemiColFound then begin
      if inputstr[i] = ';' then begin
        FirstSemiColFound := true;
        // end of integer reached
        success := TryStrToInt(tmp, outI);
        if not success then exit;
        tmp := '';
        continue;
      end;
      tmp := tmp + inputstr[i];
    end else begin
      tmp := tmp + inputstr[i];
    end;
    if (FirstSemiColFound) and (i = length(inputstr)) then begin
      outS := tmp;
    end;
  end;
  result := true;
end;

// x,y 2 integers, wraps around 4 integer function
function parse2Ints(s: string; out val1, val2: integer): boolean;
var dummy1, dummy2: integer;
begin
  result := parseInts(s, val1, val2, dummy1, dummy2);
end;

{ Checks for incoming message, and executes a callback function if there was a
  string message. Call this to check messages frequently in app. If the ipc
  server was threaded (in the DLL) then it checks for synchronization.
  Parameters
    peektime: amount of timeout peek call, good default to use is 10
    sleeptime: amount of time to sleep to rest cpu, 0 or < 1 means no sleep function will be called, good default to use is 10
  Returns errors:
  0: success
  1: problem converting message value(s) to an integer
  2: error when peeking for message
  3: callback parameter invalid (nil)
  4: this function does not work in OPT_THREADED mode
  5: ipc variable is not created (it is nil)
  6: ipc server is not created (it is nil)
}
function sIpcExecOnMsg(peektime: int32; sleeptime: int32;
  cbString: TCallbackString;
  cbInt32: TCallbackInt32;
  cbXY: TCallbackXY;
  cbX4: TCallbackX4;
  cbIntStr: TCallbackIntStr
  ): int32; cdecl;

  procedure DoSleep;
  begin if sleeptime > 0 then sleep(sleeptime);
  end;

var
  x,y,x1,x2,x3,x4: integer;
  recvdstring: string;
  recvdint32: int32;
  success: boolean;
  tmpint: integer;
  tmpstr: string;
begin
  result := 0;
  if not assigned(ipc) then exit(6);
  if not assigned(ipc.srv) then exit(5);
  if ipc.Threaded then exit(4);

  try
    if ipc.srv.PeekMessage(peektime, true) then begin
      recvdstring := ipc.srv.StringMessage;
      case ipc.srv.MsgType of
        mtString: begin
                    if cbString <> nil then begin
                      cbString(pchar(recvdstring));
                    end else begin
                      exit(3);
                    end;
                  end;
        mtInt32:  begin
                    success := TryStrToInt(recvdstring, recvdint32);
                    if not success then exit(1);
                    if cbInt32 <> nil then begin
                      cbInt32(recvdint32);
                    end else begin
                      exit(3);
                    end;
                  end;
        mtXY:     begin
                    success := parse2Ints(recvdstring, x, y);
                    if not success then exit(1);
                    if cbXY <> nil then begin
                      cbXY(x, y);
                    end else begin
                      exit(3);
                    end;
                  end;
        mtX4:     begin
                    success := parseInts(recvdstring, x1, x2, x3, x4);
                    if not success then exit(1);
                    if cbX4 <> nil then begin
                      cbX4(x1, x2, x3, x4);
                    end else begin
                      exit(3);
                    end;
                  end;
        mtIntStr: begin
                    success := parseIntStr(recvdstring, tmpint, tmpstr);
                    if not success then exit(1);
                    if cbIntStr <> nil then begin
                      cbIntStr(tmpint, pchar(tmpstr));
                    end else begin
                      exit(3);
                    end;
                  end;
      end; {case}
    end else begin
      DoSleep;
    end;
  except
     exit(2);
  end;
end; exports sIpcExecOnMsg;

function sIpcExecOnString(peektime: int32; sleeptime: int32; cb: TCallbackString): int32; cdecl;
begin
  result:= sIpcExecOnMsg(peektime, sleeptime, cb, nil, nil, nil, nil);
end; exports sIpcExecOnString;

function sIpcExecOnInt32(peektime: int32; sleeptime: int32; cb: TCallbackInt32): int32; cdecl;
begin
  result:= sIpcExecOnMsg(peektime, sleeptime, nil, cb, nil, nil, nil);
end; exports sIpcExecOnInt32;

function sIpcExecOnXY(peektime: int32; sleeptime: int32; cb: TCallbackXY): int32; cdecl;
begin
  result:= sIpcExecOnMsg(peektime, sleeptime, nil, nil, cb, nil, nil);
end; exports sIpcExecOnXY;

function sIpcExecOnX4(peektime: int32; sleeptime: int32; cb: TCallbackX4): int32; cdecl;
begin
  result:= sIpcExecOnMsg(peektime, sleeptime, nil, nil, nil, cb, nil);
end; exports sIpcExecOnX4;

function sIpcExecOnIntStr(peektime: int32; sleeptime: int32; cb: TCallbackIntStr): int32; cdecl;
begin
  result:= sIpcExecOnMsg(peektime, sleeptime, nil, nil, nil, nil, cb);
end; exports sIpcExecOnIntStr;


{ returns errors:
   0: no errors
   1: server already created, can't create again until freed first
   2: threaded IPC not available on this operating system or platform
   3: other error
}
function sIpcCreateServer(ServerID: pchar; threaded: int32): int32; cdecl;
  procedure cleanup;
  begin
    if assigned(ipc.srv) then ipc.srv.free;
    if assigned(ipc) then begin
       ipc.free; ipc := nil;
    end;
  end;
begin
  result := 0;
  if assigned(ipc) then begin
    // StatusLn('IPC not created, because an existing server was already created');
    exit(1);
  end;
  ipc := TIpc.Create;
  try
    ipc.srv := TSimpleIPCServer.Create(nil);
    try
      // StatusLn('IPC server created');
      ipc.srv.ServerID := ServerID;
      ipc.srv.global := true;
      case threaded of
        OPT_THREAD: ipc.Threaded := true;
        OPT_NO_THREAD: ipc.Threaded := false;
      else
        ipc.Threaded := false;
      end;

      if ipc.Threaded then begin
       {$IFDEF windows}
        cleanup;
        exit(2);
       {$ELSE}
        // StatusLn('using threaded ipc server');
        ipc.srv.OnMessageQueued := @ipc.MessageQueued;
       {$ENDIF}
      end;
    except
      exit(3);
    end;

  except
    exit(3);
  end;
end; exports sIpcCreateServer;

// simple test with default server ID of 12345test
procedure sIpcCreateServerTest; cdecl;
begin
  sIpcCreateServer(TEST_SERVER_ID, OPT_NO_THREAD);
end; exports sIpcCreateServerTest;

{ returns
  1: error stopping the server
  2: ipc variable not assigned, trying to free variable that doesn't exist
  3: ipc server variable not assigned, trying to free server that doesn't exist
  4: error freeing ipc server
}
function sIpcFreeServer: int32; cdecl;

  procedure cleanup;
  begin ipc.free; ipc := nil;
  end;

begin
  if not assigned(ipc) then exit(2);
  if not assigned(ipc.srv) then exit(3);
  try
    // StatusYesLn(ipc.srv.Active, 'Server active: ');
    ipc.srv.StopServer;
  except
    cleanup;
    exit(1);
  end;

  try
    ipc.srv.free; ipc.srv := nil;
  except
    cleanup;
    exit(4);
  end;
  cleanup;
end; exports sIpcFreeServer;

{ returns errors:
  0: success
  1: general error
  2: error running server
  3: IPC variable not created (it is nil)
}
function sIpcStartServer: int32; cdecl;
begin
  result := 1;
  if not assigned(ipc) then exit(3);
  try
    ipc.Start;
  except
    result := 2;
  end;
  result := 0;
end; exports sIpcStartServer;


{$ifdef TESTINGON}
// verification tests to ensure parsing is functional
procedure ParseIntTests;
var x,y,x1,x2,x3,x4: integer;
    success: boolean;
begin
  writeln('----- Parse Int Tests -----');
  writeln('should succeed:');
  success := parseInts('324;87689;65;54', x1,x2,x3,x4);
  writeln('ints: (',x1,',',x2,',',x3,',',x4,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should succeed:');
  success := parse2ints('324;87689', x, y);
  writeln('ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parseInts('324a;hk87689;65;54', x1,x2,x3,x4);
  writeln('ints: (',x1,',',x2,',',x3,',',x4,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success :=parse2ints('abc;87689', x, y);
  writeln('test2 ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324;abc', x, y);
  writeln('test3 ints: (',x,',',y,')'+ 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324a;87689', x, y);
  writeln('test4 ints: (',x,',',y,')' + 'success: ' + BoolToStr(success, true) );
  writeln('should fail:');
  success := parse2ints('324;87689a', x, y);
  writeln('test5 ints: (',x,',',y,')' + 'success: ' + BoolToStr(success, true) );
end;

procedure ParseIntStrTests;
var i: integer;
    s: string;
    success: boolean;
begin
  writeln('----- Parse Int Str Tests -----');
  writeln('should succeed:');
  success := parseIntStr('8732;a string is here', i, s);
  writeln('test 1: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should fail:');
  success := parseIntStr('8732acx;a string is here', i, s);
  writeln('test 2: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should succeed:');
  success := parseIntStr('1234;a string ;is ;here', i, s);
  writeln('test 3: success: ', success, ' int: ', i, ' string: "', s,'"');
  writeln('should succeed:');
  success := parseIntStr('1234;34;a string ;is ;here', i, s);
  writeln('test 4: success: ', success, ' int: ', i, ' string: "', s,'"');
end;
{$endif} // verification tests



end.

