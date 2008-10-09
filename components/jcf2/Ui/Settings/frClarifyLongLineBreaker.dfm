inherited fClarifyLongLineBreaker: TfClarifyLongLineBreaker
  Width = 437
  Height = 523
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 437
  ExplicitHeight = 523
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 102
    Height = 20
    Caption = 'Max line length'
  end
  object edtMaxLineLength: TJvValidateEdit
    Left = 116
    Top = 3
    Width = 49
    Height = 28
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '0'
    HasMaxValue = True
    HasMinValue = True
    MaxLength = 3
    MaxValue = 999.000000000000000000
    TabOrder = 0
  end
  object rgRebreakLongLines: TRadioGroup
    Left = 8
    Top = 44
    Width = 393
    Height = 89
    Caption = '&Break lines that are longer than max line length'
    ItemIndex = 1
    Items.Strings = (
      '&Never'
      '&Sometimes, if a good place to break is found'
      '&Usually, unless there is no acceptable place to break')
    TabOrder = 1
  end
end
