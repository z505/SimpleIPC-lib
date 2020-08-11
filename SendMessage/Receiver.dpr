program Receiver;

uses
  Vcl.Forms,
  ReceiverUnit1 in 'ReceiverUnit1.pas' {ReceiverForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TReceiverForm, ReceiverForm);
  Application.Run;
end.
