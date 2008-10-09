inherited fPreProcessor: TfPreProcessor
  Width = 409
  Height = 282
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 409
  ExplicitHeight = 282
  object lblSymbols: TLabel
    Left = 8
    Top = 30
    Width = 300
    Height = 20
    Caption = 'Symbols defined for conditional compilation:'
  end
  object lblCompilerOptions: TLabel
    Left = 8
    Top = 138
    Width = 359
    Height = 20
    Caption = 'Compiler options defined for conditional compilation:'
  end
  object mSymbols: TJvMemo
    Left = 8
    Top = 50
    Width = 364
    Height = 79
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object cbEnable: TCheckBox
    Left = 8
    Top = 6
    Width = 267
    Height = 17
    Caption = 'Enable preprocessor parsing'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object mOptions: TJvMemo
    Left = 8
    Top = 158
    Width = 364
    Height = 79
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
