inherited fObfuscateSettings: TfObfuscateSettings
  Width = 275
  Height = 271
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 275
  ExplicitHeight = 271
  object cbRemoveWhiteSpace: TCheckBox
    Left = 8
    Top = 160
    Width = 169
    Height = 17
    Caption = 'Remove &white space'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object cbRemoveComments: TCheckBox
    Left = 8
    Top = 180
    Width = 169
    Height = 17
    Caption = 'Remove c&omments'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object rgObfuscateCaps: TRadioGroup
    Left = 8
    Top = 38
    Width = 169
    Height = 108
    Caption = 'Obfuscate word &caps'
    ItemIndex = 0
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 1
  end
  object cbRebreak: TCheckBox
    Left = 8
    Top = 220
    Width = 169
    Height = 17
    Caption = 'Rebreak &lines'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object cbRemoveIndent: TCheckBox
    Left = 8
    Top = 200
    Width = 169
    Height = 17
    Caption = 'Remove &indent'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object cbEnabled: TCheckBox
    Left = 8
    Top = 8
    Width = 169
    Height = 17
    Caption = '&Obfuscate mode'
    TabOrder = 0
  end
end
