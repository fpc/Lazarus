inherited fAsm: TfAsm
  Width = 375
  Height = 216
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 375
  ExplicitHeight = 216
  object rgCaps: TRadioGroup
    Left = 165
    Top = 3
    Width = 196
    Height = 109
    Caption = '&Capitalization'
    ItemIndex = 1
    Items.Strings = (
      'ALL CAPITALS'
      'all lowercase'
      'Mixed Case'
      'Leave alone')
    TabOrder = 1
  end
  object gbStatementIndent: TGroupBox
    Left = 9
    Top = 3
    Width = 150
    Height = 86
    Caption = '&Statement Indents'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 50
      Width = 42
      Height = 20
      Caption = 'Indent'
    end
    object edtStatementIndent: TJvValidateEdit
      Left = 72
      Top = 48
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 1
    end
    object cbStatementIndent: TCheckBox
      Left = 8
      Top = 25
      Width = 97
      Height = 17
      Caption = '&Enabled'
      TabOrder = 0
    end
  end
  object gbBreaksAfterLabel: TGroupBox
    Left = 165
    Top = 118
    Width = 196
    Height = 91
    Caption = '&Breaks after label'
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 48
      Width = 119
      Height = 20
      Caption = '&Number of breaks'
      FocusControl = edtBreaksAfterLabel
    end
    object cbBreaksAfterLabelEnabled: TCheckBox
      Left = 8
      Top = 25
      Width = 97
      Height = 17
      Caption = 'En&abled'
      TabOrder = 0
    end
    object edtBreaksAfterLabel: TJvValidateEdit
      Left = 131
      Top = 45
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 1
      MaxValue = 9.000000000000000000
      TabOrder = 1
    end
  end
  object gbParamsIndent: TGroupBox
    Left = 9
    Top = 95
    Width = 150
    Height = 86
    Caption = '&Params Indents'
    TabOrder = 3
    object Label2: TLabel
      Left = 8
      Top = 50
      Width = 42
      Height = 20
      Caption = 'Indent'
    end
    object edtParamsIndent: TJvValidateEdit
      Left = 72
      Top = 47
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 1
    end
    object cbParamsIndent: TCheckBox
      Left = 8
      Top = 25
      Width = 97
      Height = 17
      Caption = '&Enabled'
      TabOrder = 0
    end
  end
end
