inherited fClarifyAlign: TfClarifyAlign
  Width = 330
  Height = 367
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 330
  ExplicitHeight = 367
  object Label6: TLabel
    Left = 8
    Top = 268
    Width = 89
    Height = 20
    Caption = 'Max Variance'
  end
  object Label4: TLabel
    Left = 8
    Top = 240
    Width = 83
    Height = 20
    Caption = 'Max Column'
  end
  object Label5: TLabel
    Left = 8
    Top = 212
    Width = 80
    Height = 20
    Caption = 'Min Column'
  end
  object Label1: TLabel
    Left = 8
    Top = 328
    Width = 98
    Height = 20
    Caption = 'Max unaligned'
  end
  object Label2: TLabel
    Left = 8
    Top = 296
    Width = 151
    Height = 20
    Caption = 'Max Variance Interface'
  end
  object cbInterfaceOnly: TCheckBox
    Left = 8
    Top = 6
    Width = 249
    Height = 17
    Caption = 'Interface Only'
    TabOrder = 0
  end
  object edtMaxVariance: TJvValidateEdit
    Left = 176
    Top = 264
    Width = 57
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '1'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 4
    OnExit = edtMaxColumnExit
  end
  object edtMaxColumn: TJvValidateEdit
    Left = 176
    Top = 236
    Width = 57
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 150.000000000000000000
    TabOrder = 3
    OnExit = edtMaxColumnExit
  end
  object edtMinColumn: TJvValidateEdit
    Left = 176
    Top = 207
    Width = 57
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 150.000000000000000000
    TabOrder = 2
    OnExit = edtMinColumnExit
  end
  object gbWhat: TGroupBox
    Left = 8
    Top = 29
    Width = 231
    Height = 164
    Caption = 'What to Align'
    TabOrder = 1
    object cbAlignAsign: TCheckBox
      Left = 8
      Top = 23
      Width = 80
      Height = 17
      Caption = 'Assign'
      TabOrder = 0
    end
    object cbAlignConst: TCheckBox
      Left = 8
      Top = 46
      Width = 71
      Height = 17
      Caption = 'Const'
      TabOrder = 1
    end
    object cbAlignVar: TCheckBox
      Left = 8
      Top = 69
      Width = 151
      Height = 17
      Caption = 'Var declarations'
      TabOrder = 2
    end
    object cbAlignTypedef: TCheckBox
      Left = 8
      Top = 115
      Width = 203
      Height = 17
      Caption = 'Type defs'
      TabOrder = 3
    end
    object cbAlignComment: TCheckBox
      Left = 8
      Top = 138
      Width = 203
      Height = 17
      Caption = 'Comment'
      TabOrder = 4
    end
    object cbAlignField: TCheckBox
      Left = 8
      Top = 92
      Width = 203
      Height = 17
      Caption = 'Class and record field'
      TabOrder = 5
    end
  end
  object eMaxUnaligned: TJvValidateEdit
    Left = 176
    Top = 322
    Width = 57
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    TabOrder = 5
  end
  object edtMaxVarianceInterface: TJvValidateEdit
    Left = 176
    Top = 293
    Width = 57
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '1'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 6
    OnExit = edtMaxColumnExit
  end
end
