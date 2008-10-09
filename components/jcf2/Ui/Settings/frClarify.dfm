inherited fClarify: TfClarify
  Width = 426
  Height = 292
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 426
  ExplicitHeight = 292
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 166
    Height = 20
    Caption = 'File extensions to format:'
  end
  object rgRunOnceOffs: TRadioGroup
    Left = 136
    Top = 34
    Width = 141
    Height = 87
    Caption = 'Run once-offs'
    Items.Strings = (
      'Do &not run'
      'Do &run'
      'Run &only these')
    TabOrder = 0
  end
  object mFileExtensions: TMemo
    Left = 8
    Top = 34
    Width = 109
    Height = 127
    TabOrder = 1
  end
end
