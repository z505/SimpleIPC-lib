{ Client delphi example for Simple IPC DLL (inter process communication)

  How to use: run ipcserv at the console, then use this program to send a msg
  to it }

unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

// {$i ../libdeclare.inc}

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  SERVER_ID = '12345';

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dynsimpleipcwrap;

procedure TForm1.Button1Click(Sender: TObject);
begin
  sIpcCreateClient;
  sIpcStartClient(SERVER_ID);
  sIpcSendIntsMsg(10,20,30,40);
  // this will stop the server since ipcserv is configured to stop on four 0's
  sIpcSendIntsMsg(0,0,0,0);
  sIpcFreeClient;
end;

end.
