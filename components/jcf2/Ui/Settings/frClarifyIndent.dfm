inherited fClarifyIndent: TfClarifyIndent
  Width = 430
  Height = 363
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 430
  ExplicitHeight = 363
  object Label2: TLabel
    Left = 8
    Top = 80
    Width = 164
    Height = 20
    Caption = 'Block indentation spaces'
  end
  object edtIndentSpaces: TJvValidateEdit
    Left = 180
    Top = 77
    Width = 50
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 12.000000000000000000
    TabOrder = 3
  end
  object cbIndentGlobals: TCheckBox
    Left = 8
    Top = 6
    Width = 201
    Height = 17
    Caption = 'Indent globals'
    TabOrder = 0
  end
  object cbIndentProcedures: TCheckBox
    Left = 8
    Top = 30
    Width = 201
    Height = 17
    Caption = 'Indent procedures'
    TabOrder = 1
  end
  object cbIndentClasses: TCheckBox
    Left = 8
    Top = 54
    Width = 201
    Height = 17
    Caption = 'Indent classes'
    TabOrder = 2
  end
  object gbOptions: TGroupBox
    Left = 8
    Top = 105
    Width = 417
    Height = 240
    Caption = 'Options'
    TabOrder = 4
    object cbIndentBeginEnd: TCheckBox
      Left = 12
      Top = 28
      Width = 317
      Height = 18
      Caption = 'Extra indent for begin/end inside procedures'
      TabOrder = 0
      OnClick = cbIndentBeginEndClick
    end
    object eIndentBeginEndSpaces: TJvValidateEdit
      Left = 360
      Top = 18
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 10.000000000000000000
      MinValue = -10.000000000000000000
      TabOrder = 1
    end
    object cbHasFirstLevelIndent: TCheckBox
      Left = 12
      Top = 51
      Width = 285
      Height = 18
      Caption = 'Different indent for first level'
      TabOrder = 2
      OnClick = cbHasFirstLevelIndentClick
    end
    object eFirstLevelIndent: TJvValidateEdit
      Left = 360
      Top = 46
      Width = 50
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      MaxLength = 3
      TabOrder = 3
    end
    object cbKeepWithInProc: TCheckBox
      Left = 12
      Top = 74
      Width = 382
      Height = 18
      Caption = 'Keep single-line comments with code in procedures'
      TabOrder = 4
    end
    object cbKeepWithInGlobals: TCheckBox
      Left = 12
      Top = 97
      Width = 373
      Height = 18
      Caption = 'Keep single-line comments with code in globals'
      TabOrder = 5
    end
    object cbKeepWithInClassDef: TCheckBox
      Left = 12
      Top = 120
      Width = 402
      Height = 18
      Caption = 'Keep single-line comments with code in class definitions'
      TabOrder = 6
    end
    object cbKeepWithElsewhere: TCheckBox
      Left = 12
      Top = 143
      Width = 398
      Height = 18
      Caption = 'Keep single-line comments with code elsewhere'
      TabOrder = 7
    end
    object cbIndentIfElse: TCheckBox
      Left = 12
      Top = 166
      Width = 366
      Height = 18
      Caption = 'Extra Indent for If...Else blocks'
      TabOrder = 8
    end
    object cbIndentCaseElse: TCheckBox
      Left = 12
      Top = 188
      Width = 366
      Height = 18
      Caption = 'Extra Indent for Case...Else blocks'
      TabOrder = 9
    end
    object cbIndentLibraryProcs: TCheckBox
      Left = 12
      Top = 208
      Width = 366
      Height = 18
      Caption = 'Indent for procedures in library'
      TabOrder = 10
    end
  end
end
