object ExceptionDialog: TExceptionDialog
  Left = 294
  Top = 195
  BorderIcons = [biSystemMenu]
  Caption = 'JCF Exception'
  ClientHeight = 180
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 20
  object btnOk: TButton
    Left = 155
    Top = 133
    Width = 100
    Height = 34
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = btnOkClick
  end
  object mExceptionMessage: TMemo
    Left = 0
    Top = 0
    Width = 409
    Height = 124
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
end
