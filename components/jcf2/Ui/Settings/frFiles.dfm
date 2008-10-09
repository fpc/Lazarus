inherited fFiles: TfFiles
  Width = 338
  Height = 281
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 338
  ExplicitHeight = 281
  object lblStatus: TLabel
    Left = 8
    Top = 42
    Width = 57
    Height = 20
    Caption = 'lblStatus'
  end
  object lblDate: TLabel
    Left = 8
    Top = 64
    Width = 49
    Height = 20
    Caption = 'lblDate'
  end
  object lblVersion: TLabel
    Left = 8
    Top = 88
    Width = 66
    Height = 20
    Caption = 'lblVersion'
  end
  object lblDescription: TLabel
    Left = 8
    Top = 114
    Width = 79
    Height = 20
    Caption = 'Description:'
  end
  object lblFormatFileName: TLabel
    Left = 8
    Top = 8
    Width = 127
    Height = 20
    Caption = 'lblFormatFileName'
  end
  object mDescription: TJvMemo
    Left = 8
    Top = 134
    Width = 301
    Height = 89
    AutoSize = False
    MaxLines = 0
    HideCaret = False
    TabOrder = 0
  end
end
