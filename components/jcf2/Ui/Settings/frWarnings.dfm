inherited fWarnings: TfWarnings
  Height = 255
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitHeight = 255
  object Label1: TLabel
    Left = 8
    Top = 60
    Width = 226
    Height = 20
    Caption = '&Ignore unused parameters named:'
    FocusControl = mIgnoreUnusedParams
  end
  object cbWarningsOn: TCheckBox
    Left = 8
    Top = 8
    Width = 227
    Height = 17
    Caption = '&Warnings On'
    TabOrder = 0
  end
  object cbWarnUnusedParams: TCheckBox
    Left = 8
    Top = 31
    Width = 227
    Height = 17
    Caption = 'Warn &unused parameters'
    TabOrder = 1
  end
  object mIgnoreUnusedParams: TMemo
    Left = 8
    Top = 83
    Width = 227
    Height = 153
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
