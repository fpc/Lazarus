inherited fClarifySpaces: TfClarifySpaces
  Width = 472
  Height = 388
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 472
  ExplicitHeight = 388
  object cbFixSpacing: TCheckBox
    Left = 8
    Top = 6
    Width = 241
    Height = 17
    Caption = 'Fix &spacing'
    TabOrder = 0
  end
  object cbSpaceClassHeritage: TCheckBox
    Left = 8
    Top = 29
    Width = 241
    Height = 17
    Caption = 'Space before class &heritage'
    TabOrder = 1
  end
  object gbColon: TGroupBox
    Left = 8
    Top = 50
    Width = 246
    Height = 216
    Caption = 'Spaces &before colon in'
    TabOrder = 2
    object lblSpaceBeforeColonVar: TLabel
      Left = 8
      Top = 24
      Width = 175
      Height = 20
      Caption = '&Var and const declarations'
      FocusControl = eSpaceBeforeColonVar
    end
    object lblSpacesBeforeColonClassVar: TLabel
      Left = 8
      Top = 108
      Width = 96
      Height = 20
      Caption = '&Class variables'
      FocusControl = eSpacesBeforeColonClassVar
    end
    object lblSpaceBeforeColonFn: TLabel
      Left = 8
      Top = 80
      Width = 138
      Height = 20
      Caption = '&Function return types'
      FocusControl = eSpaceBeforeColonFn
    end
    object lblSpaceBeforeColonParam: TLabel
      Left = 8
      Top = 52
      Width = 146
      Height = 20
      Caption = '&Procedure parameters'
      FocusControl = eSpaceBeforeColonParam
    end
    object lblSpacesBeforeCaseLabel: TLabel
      Left = 8
      Top = 136
      Width = 68
      Height = 20
      Caption = 'Case l&abel'
      FocusControl = eSpacesBeforeCaseLabel
    end
    object lbSpacesBeforeLabel: TLabel
      Left = 8
      Top = 164
      Width = 36
      Height = 20
      Caption = '&Label'
      FocusControl = eSpacesBeforeLabel
    end
    object lblSpacesBeforeColonGeneric: TLabel
      Left = 8
      Top = 189
      Width = 65
      Height = 20
      Caption = 'In &generic'
      FocusControl = eSpacesBeforeColonGeneric
    end
    object eSpaceBeforeColonVar: TJvValidateEdit
      Left = 187
      Top = 21
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 0
    end
    object eSpaceBeforeColonParam: TJvValidateEdit
      Left = 187
      Top = 49
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 1
    end
    object eSpaceBeforeColonFn: TJvValidateEdit
      Left = 187
      Top = 73
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 2
    end
    object eSpacesBeforeColonClassVar: TJvValidateEdit
      Left = 187
      Top = 101
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 3
    end
    object eSpacesBeforeCaseLabel: TJvValidateEdit
      Left = 187
      Top = 129
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 4
    end
    object eSpacesBeforeLabel: TJvValidateEdit
      Left = 187
      Top = 157
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 5
    end
    object eSpacesBeforeColonGeneric: TJvValidateEdit
      Left = 187
      Top = 184
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 2
      TabOrder = 6
    end
  end
  object gbTabs: TGroupBox
    Left = 3
    Top = 268
    Width = 457
    Height = 78
    Caption = '&Tab characters'
    TabOrder = 3
    object Label1: TLabel
      Left = 240
      Top = 22
      Width = 98
      Height = 20
      Caption = 'Spaces per tab'
    end
    object Label3: TLabel
      Left = 240
      Top = 44
      Width = 95
      Height = 20
      Caption = 'Spaces for tab'
    end
    object cbTabsToSpaces: TCheckBox
      Left = 6
      Top = 24
      Width = 175
      Height = 17
      Caption = 'Turn tabs to spaces'
      TabOrder = 0
      OnClick = cbTabsToSpacesClick
    end
    object cbSpacesToTabs: TCheckBox
      Left = 6
      Top = 46
      Width = 175
      Height = 17
      Caption = 'Turn spaces to tabs'
      TabOrder = 2
      OnClick = cbSpacesToTabsClick
    end
    object edtSpacesPerTab: TJvValidateEdit
      Left = 356
      Top = 20
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 2
      MaxValue = 12.000000000000000000
      TabOrder = 1
    end
    object edtSpacesForTab: TJvValidateEdit
      Left = 356
      Top = 44
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 2
      MaxValue = 12.000000000000000000
      TabOrder = 3
    end
  end
  object cbMaxSpaces: TCheckBox
    Left = 12
    Top = 356
    Width = 179
    Height = 17
    Caption = '&Max spaces in code'
    TabOrder = 4
    OnClick = cbMaxSpacesClick
  end
  object edtMaxSpacesInCode: TJvValidateEdit
    Left = 193
    Top = 352
    Width = 50
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 2
    MaxValue = 99.000000000000000000
    TabOrder = 5
  end
  object rgOperators: TRadioGroup
    Left = 260
    Top = 6
    Width = 202
    Height = 92
    Caption = 'Spaces around &operators'
    Items.Strings = (
      'Always'
      'Leave as is'
      'Never')
    TabOrder = 6
  end
  object GroupBoxInsertSpaceBeforeBracket: TGroupBox
    Left = 260
    Top = 98
    Width = 202
    Height = 95
    Caption = '&Insert space before bracket'
    TabOrder = 7
    object cbInsertSpaceBeforeBracketinFunctionDeclaration: TCheckBox
      Left = 8
      Top = 26
      Width = 181
      Height = 17
      Caption = 'In function &declaration'
      TabOrder = 0
    end
    object cbInsertSpaceBeforeBracketinFunctionCall: TCheckBox
      Left = 8
      Top = 48
      Width = 181
      Height = 17
      Caption = 'In function &call'
      TabOrder = 1
    end
    object cbBeforeOpenSquareBracketInExpression: TCheckBox
      Left = 8
      Top = 68
      Width = 182
      Height = 17
      Caption = 'Before [ in expression'
      TabOrder = 2
    end
  end
  object GroupBoxSpacesInsideBrackets: TGroupBox
    Left = 260
    Top = 194
    Width = 202
    Height = 70
    Caption = 'Insert space inside brackets'
    TabOrder = 8
    object CheckBoxInsertSpaceBeforeEnd: TCheckBox
      Left = 8
      Top = 47
      Width = 181
      Height = 17
      Caption = 'Before end'
      TabOrder = 0
    end
    object cbInsertSpaceAfterOpen: TCheckBox
      Left = 8
      Top = 24
      Width = 181
      Height = 17
      Caption = 'After open'
      TabOrder = 1
    end
  end
end
