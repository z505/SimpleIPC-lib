object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 122
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object bCreateServer: TButton
    Left = 20
    Top = 18
    Width = 157
    Height = 25
    Caption = 'Create Server'
    TabOrder = 0
    OnClick = bCreateServerClick
  end
  object mStatus: TMemo
    Left = 200
    Top = 12
    Width = 269
    Height = 89
    Lines.Strings = (
      'mStatus')
    TabOrder = 1
  end
  object bFreeServer: TButton
    Left = 20
    Top = 66
    Width = 157
    Height = 25
    Caption = 'Free Server'
    TabOrder = 2
    OnClick = bFreeServerClick
  end
  object bWaitForMsgs: TButton
    Left = 20
    Top = 42
    Width = 157
    Height = 25
    Caption = 'Wait For Msgs'
    TabOrder = 3
    OnClick = bWaitForMsgsClick
  end
end
