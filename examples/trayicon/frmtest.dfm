object Form1: TForm1
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 400
  OnCreate = FormCreate
  PixelsPerInch = 96
  Left = 290
  Height = 300
  Top = 175
  Width = 400
  object Button1: TButton
    Caption = 'Show'
    OnClick = Button1Click
    TabOrder = 0
    Left = 75
    Height = 25
    Top = 56
    Width = 75
  end
  object Button2: TButton
    Caption = 'Hide'
    OnClick = Button2Click
    TabOrder = 1
    Left = 75
    Height = 25
    Top = 95
    Width = 75
  end
  object SystrayIcon: TTrayIcon
    left = 221
    top = 145
  end
end
