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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object btn1: TButton
    Left = 128
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 1
    OnClick = btn1Click
  end
  object m: TMemo
    Left = 0
    Top = 96
    Width = 417
    Height = 257
    TabOrder = 2
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 209
    Top = 55
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 4
    OnClick = Button3Click
  end
end
