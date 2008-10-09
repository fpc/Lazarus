inherited fClarifyCaseBlocks: TfClarifyCaseBlocks
  Width = 345
  Height = 337
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 345
  ExplicitHeight = 337
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 196
    Height = 20
    Caption = 'Block styles: use a new line at:'
  end
  object rgLabelBegin: TRadioGroup
    Left = 8
    Top = 30
    Width = 149
    Height = 89
    Caption = 'Label with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgLabel: TRadioGroup
    Left = 163
    Top = 30
    Width = 170
    Height = 89
    Caption = 'Label without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgCaseLabel: TRadioGroup
    Left = 163
    Top = 125
    Width = 170
    Height = 89
    Caption = 'Case without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgElseCase: TRadioGroup
    Left = 163
    Top = 220
    Width = 170
    Height = 89
    Caption = 'Else case without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 5
  end
  object rgCaseBegin: TRadioGroup
    Left = 8
    Top = 125
    Width = 149
    Height = 89
    Caption = 'Case with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgCaseElseBegin: TRadioGroup
    Left = 8
    Top = 220
    Width = 149
    Height = 89
    Caption = 'Else case with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
end
