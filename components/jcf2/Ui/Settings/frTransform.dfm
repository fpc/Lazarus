inherited fTransform: TfTransform
  Width = 412
  Height = 315
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 412
  ExplicitHeight = 315
  object cbBlockEndSemicolons: TCheckBox
    Left = 8
    Top = 112
    Width = 385
    Height = 17
    Caption = 'Put &semicolons after last statement in a block'
    TabOrder = 0
  end
  object rbBeginEnd: TRadioGroup
    Left = 8
    Top = 4
    Width = 395
    Height = 93
    Caption = 'Add or Remove &begin and end from single statements'
    Items.Strings = (
      'Add begin and end to single statements'
      'Leave begin and end as is'
      'Remove begin and end from around single statements')
    TabOrder = 1
  end
  object bgSortUses: TGroupBox
    Left = 8
    Top = 139
    Width = 218
    Height = 171
    Caption = 'Sort &uses clauses'
    TabOrder = 2
    object cbSortInterfaceUses: TCheckBox
      Left = 8
      Top = 26
      Width = 185
      Height = 17
      Caption = 'Sort i&nterface uses'
      TabOrder = 0
    end
    object cbSortImplementationUses: TCheckBox
      Left = 8
      Top = 49
      Width = 201
      Height = 17
      Caption = 'Sort i&mplementation uses'
      TabOrder = 1
    end
    object cbBreakUsesSortOnComment: TCheckBox
      Left = 8
      Top = 118
      Width = 185
      Height = 17
      Caption = 'Break on &comment'
      TabOrder = 2
    end
    object cbBreakUsesSortOnReturn: TCheckBox
      Left = 8
      Top = 95
      Width = 201
      Height = 17
      Caption = 'Break on &return'
      TabOrder = 3
    end
    object cbNoComments: TCheckBox
      Left = 8
      Top = 141
      Width = 201
      Height = 17
      Caption = 'Only with no comments'
      TabOrder = 4
    end
    object cbSortProgramUses: TCheckBox
      Left = 8
      Top = 72
      Width = 201
      Height = 17
      Caption = 'Sort &program uses'
      TabOrder = 5
    end
  end
  object rgUsesSortOrder: TRadioGroup
    Left = 231
    Top = 139
    Width = 172
    Height = 114
    Caption = 'Uses sort &order'
    Items.Strings = (
      'Alphabetic'
      'Reverse Alphabetic'
      'Shortest to longest'
      'Longest to shortest')
    TabOrder = 3
  end
end
