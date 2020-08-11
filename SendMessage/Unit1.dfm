object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 143
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 14
    Top = 20
    Width = 244
    Height = 16
    Caption = 'Sends a Message to a Receiver application'
  end
  object bSendMsgToReceiver: TButton
    Left = 98
    Top = 58
    Width = 173
    Height = 25
    Caption = 'bSendMsgToReceiver'
    TabOrder = 0
    OnClick = bSendMsgToReceiverClick
  end
  object bSendLocalMsg: TButton
    Left = 98
    Top = 86
    Width = 173
    Height = 25
    Caption = 'bSendLocalMsg'
    TabOrder = 1
    OnClick = bSendLocalMsgClick
  end
end
