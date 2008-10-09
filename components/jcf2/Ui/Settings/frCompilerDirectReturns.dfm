inherited fCompilerDirectReturns: TfCompilerDirectReturns
  Width = 399
  Height = 261
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 399
  ExplicitHeight = 261
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 276
    Height = 20
    Caption = 'Use a new line before compiler directives:'
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 263
    Height = 20
    Caption = 'Use a new line after compiler directives:'
  end
  object rgBeforeUses: TRadioGroup
    Left = 8
    Top = 30
    Width = 117
    Height = 89
    Caption = 'Uses clause'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgBeforeStatements: TRadioGroup
    Left = 131
    Top = 30
    Width = 117
    Height = 89
    Caption = 'Statements'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgBeforeGeneral: TRadioGroup
    Left = 254
    Top = 30
    Width = 117
    Height = 89
    Caption = 'Other places'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgAfterGeneral: TRadioGroup
    Left = 254
    Top = 154
    Width = 117
    Height = 89
    Caption = 'Other places'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgAfterStatements: TRadioGroup
    Left = 131
    Top = 154
    Width = 117
    Height = 89
    Caption = 'Statements'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
  object rgAfterUses: TRadioGroup
    Left = 8
    Top = 154
    Width = 117
    Height = 89
    Caption = 'Uses clause'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
end
