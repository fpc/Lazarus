inherited fClarifyBlocks: TfClarifyBlocks
  Width = 334
  Height = 336
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 334
  ExplicitHeight = 336
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 196
    Height = 20
    Caption = 'Block styles: use a new line at:'
  end
  object rgBlockBegin: TRadioGroup
    Left = 8
    Top = 30
    Width = 149
    Height = 89
    Caption = 'Block with begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 0
  end
  object rgBlock: TRadioGroup
    Left = 163
    Top = 30
    Width = 162
    Height = 89
    Caption = 'Block without begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 1
  end
  object rgEndElse: TRadioGroup
    Left = 163
    Top = 127
    Width = 162
    Height = 89
    Caption = 'Between end and else'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 3
  end
  object rgElseIf: TRadioGroup
    Left = 8
    Top = 127
    Width = 149
    Height = 89
    Caption = 'Between else and if'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 2
  end
  object rgElseBegin: TRadioGroup
    Left = 8
    Top = 222
    Width = 149
    Height = 89
    Caption = 'Else begin'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 4
  end
end
