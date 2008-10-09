object FormAllSettings: TFormAllSettings
  Left = 171
  Top = 106
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  BorderStyle = bsDialog
  Caption = 'JCF Format Settings'
  ClientHeight = 528
  ClientWidth = 872
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  DesignSize = (
    872
    528)
  PixelsPerInch = 120
  TextHeight = 20
  object tvFrames: TTreeView
    Left = 3
    Top = 3
    Width = 240
    Height = 468
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvFramesChange
  end
  object pnlSet: TPanel
    Left = 240
    Top = 3
    Width = 629
    Height = 468
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 1
  end
  object bbOK: TBitBtn
    Left = 292
    Top = 480
    Width = 92
    Height = 36
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    OnClick = bbOKClick
    Kind = bkOK
  end
  object bbCancel: TBitBtn
    Left = 389
    Top = 480
    Width = 94
    Height = 36
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    OnClick = bbCancelClick
    Kind = bkCancel
  end
  object BitBtn1: TBitBtn
    Left = 488
    Top = 480
    Width = 92
    Height = 36
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    OnClick = bbHelpClick
    Kind = bkHelp
  end
end
