unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    bCreateServer: TButton;
    mStatus: TMemo;
    bFreeServer: TButton;
    bWaitForMsgs: TButton;
    procedure bCreateServerClick(Sender: TObject);
    procedure bFreeServerClick(Sender: TObject);
    procedure bWaitForMsgsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  SERVER_ID = '12345';

implementation

{$R *.dfm}

procedure TForm1.bCreateServerClick(Sender: TObject);
begin
	sIpcCreateServer;
	sIpcStartServer(SERVER_ID, OPT_NO_THREAD);
	mStatus.Lines.Add('IPC Server running');
end;

procedure TForm1.bFreeServerClick(Sender: TObject);
begin
  sIpcFreeServer;
end;

procedure TForm1.bWaitForMsgsClick(Sender: TObject);
begin
	for
		// setup a callback when a msg is received
        C.sIpcExecOnMsg(10,10,
			(C.TCallbackString)(unsafe.Pointer(C.CallbackString)),  // one for a string message
			(C.TCallbackInt32)(unsafe.Pointer(C.CallbackInt32)), // one for an integer message
            (C.TCallbackXY)(unsafe.Pointer(C.CallbackXY)),
            (C.TCallbackInts)(unsafe.Pointer(C.CallbackInts)),
			(C.TCallbackIntStr)(unsafe.Pointer(C.CallbackIntStr)))
        if recvdstop {
			break
		}
	}

end;

end.
