inherited fNotIdentifierCapsSettings: TfNotIdentifierCapsSettings
  Width = 396
  Height = 232
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 396
  ExplicitHeight = 232
  object Label1: TLabel
    Left = 108
    Top = 3
    Width = 275
    Height = 20
    Caption = 'Set capitalisation on these non-identifiers'
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
    OnClick = cbEnableAnyWordsClick
  end
  object mWords: TJvMemo
    Left = 0
    Top = 34
    Width = 396
    Height = 198
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
