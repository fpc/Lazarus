inherited fReplace: TfReplace
  Width = 400
  Height = 358
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 400
  ExplicitHeight = 358
  object lblWordList: TLabel
    Left = 8
    Top = 35
    Width = 63
    Height = 20
    Caption = 'Word list:'
  end
  object cbEnable: TCheckBox
    Left = 8
    Top = 6
    Width = 219
    Height = 17
    Caption = 'Enable find and replace'
    TabOrder = 0
    OnClick = cbEnableClick
  end
  object mWords: TJvMemo
    Left = 0
    Top = 54
    Width = 399
    Height = 301
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
