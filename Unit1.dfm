object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 388
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 128
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 0
    OnClick = btn1Click
  end
  object m: TMemo
    Left = 0
    Top = 96
    Width = 417
    Height = 257
    TabOrder = 1
  end
  object Button3: TButton
    Left = 209
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 290
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 3
    OnClick = Button1Click
  end
end
