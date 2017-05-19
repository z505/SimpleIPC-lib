unit ipcclientunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  simpleipc;

{$i libdeclare.inc}

const
  TEST_SERVER_ID = '123';

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    bStop: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure bStopClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure StatusMsg(s: string);
begin
  form1.memo1.lines.add(s);
end;

procedure RunIpcClientMsgTest(DoStop: boolean);
var
  I,Count : Integer;
begin
  Count:=1;
  with TSimpleIPCClient.Create(Nil) do
    try
      ServerID := TEST_SERVER_ID;
      Active:=True;
      if DoStop then
        SendStringMessage('stop')
      else for I:=1 to Count do
        SendStringMessage(Format('Testmessage %d from client',[i]));
      Active:=False;
    finally
      Free;
    end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  RunIpcClientMsgTest(false);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  I,Count : integer;
  sc: TSimpleIPCClient;
begin
  Count:=1;
  sc := TSimpleIPCClient.Create(nil);
  try
    sc.ServerID := TEST_SERVER_ID;
    sc.Active:=True;
    sc.SendStringMessage(Format('Testmessage %d from client',[i]));
    sc.Active:=False;
  finally
    sc.Free;
  end;
end;

procedure TForm1.bStopClick(Sender: TObject);
begin
  RunIpcClientMsgTest(true);
end;

end.

