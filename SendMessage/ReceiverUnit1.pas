unit ReceiverUnit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TReceiverForm = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
  private
    { Private declarations }
    procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA;
  public
    { Public declarations }
  end;

var
  ReceiverForm: TReceiverForm;

implementation

{$R *.dfm}

procedure TReceiverForm.WMCopyData(var Msg : TWMCopyData);
var
  s: string;
  MsgNum: integer;
begin
  MsgNum := Msg.CopyDataStruct.dwData;
  if MsgNum = 6000 then
  begin
    s := PChar(Msg.CopyDataStruct.lpData);
    ShowMessage('length: ' +inttostr(StrLen(PChar(Msg.CopyDataStruct.lpData))));
    ShowMessage(s);
  end;
end;


end.
