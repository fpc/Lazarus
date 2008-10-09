inherited frUnitNameCaps: TfrUnitNameCaps
  Width = 362
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 362
  object Label1: TLabel
    Left = 108
    Top = 3
    Width = 250
    Height = 20
    Caption = 'Set capitalisation on these unit names'
  end
  object mWords: TJvMemo
    Left = 0
    Top = 29
    Width = 362
    Height = 211
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object cbEnableAnyWords: TCheckBox
    Left = 8
    Top = 5
    Width = 67
    Height = 17
    Caption = 'Enable'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
end
