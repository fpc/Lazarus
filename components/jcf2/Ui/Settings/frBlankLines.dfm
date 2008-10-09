inherited fBlankLines: TfBlankLines
  Width = 380
  Height = 280
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 380
  ExplicitHeight = 280
  object Label1: TLabel
    Left = 8
    Top = 144
    Width = 284
    Height = 20
    Caption = 'Number of returns after the unit'#39's final end.'
  end
  object Label2: TLabel
    Left = 8
    Top = 200
    Width = 243
    Height = 20
    Caption = 'Max consecutive blank lines anwhere'
  end
  object Label3: TLabel
    Left = 8
    Top = 226
    Width = 153
    Height = 20
    Caption = 'Lines before procedure'
  end
  object eNumReturnsAfterFinalEnd: TJvValidateEdit
    Left = 314
    Top = 140
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 255.000000000000000000
    TabOrder = 1
  end
  object cbRemoveConsecutiveBlankLines: TCheckBox
    Left = 8
    Top = 176
    Width = 262
    Height = 17
    Caption = 'Remove consecutive blank lines'
    TabOrder = 2
  end
  object edtMaxConsecutiveBlankLines: TJvValidateEdit
    Left = 314
    Top = 192
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 99.000000000000000000
    TabOrder = 3
  end
  object gbRemoveBlankLines: TGroupBox
    Left = 8
    Top = 3
    Width = 366
    Height = 126
    Caption = 'Remove blank lines'
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 94
      Width = 289
      Height = 20
      Caption = 'Max consecutive blank lines before removal'
    end
    object cbRemoveBlockBlankLines: TCheckBox
      Left = 8
      Top = 67
      Width = 289
      Height = 17
      Caption = 'At start and end of begin..end block'
      TabOrder = 2
    end
    object cbRemoveBlankLinesAfterProcHeader: TCheckBox
      Left = 8
      Top = 44
      Width = 289
      Height = 17
      Caption = 'After procedure header'
      TabOrder = 1
    end
    object cbRemoveVarBlankLines: TCheckBox
      Left = 8
      Top = 21
      Width = 289
      Height = 17
      Caption = 'In procedure var section'
      TabOrder = 0
    end
    object edtMaxBlankLinesInSection: TJvValidateEdit
      Left = 306
      Top = 91
      Width = 49
      Height = 28
      CriticalPoints.MaxValueIncluded = False
      CriticalPoints.MinValueIncluded = False
      EditText = '0'
      HasMaxValue = True
      HasMinValue = True
      MaxLength = 3
      MaxValue = 99.000000000000000000
      TabOrder = 3
    end
  end
  object edtLinesBeforeProcedure: TJvValidateEdit
    Left = 314
    Top = 226
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 1
    MaxValue = 9.000000000000000000
    TabOrder = 4
  end
end
