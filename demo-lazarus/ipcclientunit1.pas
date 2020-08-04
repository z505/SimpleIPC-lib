unit ipcclientunit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  simpleipc;

{$i ../libdeclare.inc}

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
const
  id = 505;
  thx = 1138;
var
  i,j: int32;
begin
  with TSimpleIPCClient.Create(Nil) do
    try
      ServerID := TEST_SERVER_ID;
      Active:=True;
      i := id;
      inc(i);
      j := thx;
      inc(j);
      SendStringMessage(mtString, 'THX1138 incremented his punch card');
      SendStringMessage(mtInt32, inttostr(j));
      SendStringMessage(mtString, 'THX'+inttostr(j));
      SendStringMessage(mtInt32, inttostr(i));
      SendStringMessage(mtString, 'Z'+inttostr(i));
      Active:=False;
    finally
      Free;
    end;
end;

procedure TForm1.bStopClick(Sender: TObject);
begin
  RunIpcClientMsgTest(true);
end;

end.

