inherited fClarifyIndent: TfClarifyIndent
  Width = 430
  Height = 342
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 430
  ExplicitHeight = 342
  object Label2: TLabel
    Left = 4
    Top = 6
    Width = 164
    Height = 20
    Caption = 'Block indentation spaces'
  end
  object edtIndentSpaces: TJvValidateEdit
    Left = 180
    Top = 4
    Width = 50
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 12.000000000000000000
    TabOrder = 0
  end
  object gbOptions: TGroupBox
    Left = 4
    Top = 32
    Width = 417
    Height = 310
    Caption = 'Options'
    TabOrder = 1
    object cbIndentBeginEnd: TCheckBox
      Left = 12
      Top = 30
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
      Top = 52
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
      Top = 96
      Width = 373
      Height = 18
      Caption = 'Keep single-line comments with code in globals'
      TabOrder = 5
    end
    object cbKeepWithInClassDef: TCheckBox
      Left = 12
      Top = 118
      Width = 402
      Height = 18
      Caption = 'Keep single-line comments with code in class definitions'
      TabOrder = 6
    end
    object cbKeepWithElsewhere: TCheckBox
      Left = 12
      Top = 140
      Width = 398
      Height = 18
      Caption = 'Keep single-line comments with code elsewhere'
      TabOrder = 7
    end
    object cbIndentIfElse: TCheckBox
      Left = 12
      Top = 162
      Width = 366
      Height = 18
      Caption = 'Extra Indent for If...Else blocks'
      TabOrder = 8
    end
    object cbIndentCaseElse: TCheckBox
      Left = 12
      Top = 184
      Width = 366
      Height = 18
      Caption = 'Extra Indent for Case...Else blocks'
      TabOrder = 9
    end
    object cbIndentLibraryProcs: TCheckBox
      Left = 12
      Top = 206
      Width = 366
      Height = 18
      Caption = 'Indent for procedures in library'
      TabOrder = 10
    end
    object cbIndentProcedureBody: TCheckBox
      Left = 12
      Top = 228
      Width = 366
      Height = 18
      Caption = 'Indent for procedure body'
      TabOrder = 11
    end
    object cbIndentNestedTypes: TCheckBox
      Left = 12
      Top = 250
      Width = 366
      Height = 18
      Caption = 'Indent nested types'
      TabOrder = 12
    end
    object cbIndentVarAndConstInClass: TCheckBox
      Left = 12
      Top = 272
      Width = 366
      Height = 18
      Caption = 'Indent var and const in class'
      TabOrder = 13
    end
  end
end
