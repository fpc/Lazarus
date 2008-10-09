inherited fClarifyReturns: TfClarifyReturns
  Width = 453
  Height = 284
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  ExplicitWidth = 453
  ExplicitHeight = 284
  object rgReturnChars: TRadioGroup
    Left = 8
    Top = 174
    Width = 438
    Height = 99
    Caption = 'Return chars'
    Items.Strings = (
      'Leave as is'
      'Convert to Carriage Return (UNIX)'
      'Convert to Carriage Return + Linefeed (DOS/Windows)')
    TabOrder = 2
  end
  object gbRemoveReturns: TGroupBox
    Left = 8
    Top = 8
    Width = 202
    Height = 160
    Hint = 'bgRemove'
    Caption = 'Remove returns'
    TabOrder = 0
    object cbRemoveProcDefReturns: TCheckBox
      Left = 8
      Top = 77
      Width = 186
      Height = 17
      Caption = 'In procedure definitions'
      TabOrder = 2
    end
    object cbRemoveVarReturns: TCheckBox
      Left = 8
      Top = 102
      Width = 186
      Height = 17
      Caption = 'In variable declarations'
      TabOrder = 3
    end
    object cbRemoveExprReturns: TCheckBox
      Left = 8
      Top = 127
      Width = 186
      Height = 17
      Caption = 'In expressions'
      TabOrder = 4
    end
    object cbRemovePropertyReturns: TCheckBox
      Left = 8
      Top = 52
      Width = 186
      Height = 17
      Caption = 'In properties'
      TabOrder = 1
    end
    object cbRemoveReturns: TCheckBox
      Left = 8
      Top = 28
      Width = 186
      Height = 17
      Caption = 'In misc. bad places'
      TabOrder = 0
    end
  end
  object gbInsert: TGroupBox
    Left = 216
    Top = 7
    Width = 230
    Height = 161
    Caption = 'Insert returns'
    TabOrder = 1
    object cbUsesClauseOnePerLine: TCheckBox
      Left = 8
      Top = 53
      Width = 217
      Height = 17
      Caption = 'One uses clause item per line'
      TabOrder = 1
    end
    object cbInsertReturns: TCheckBox
      Left = 8
      Top = 29
      Width = 177
      Height = 17
      Caption = 'In misc. good places'
      TabOrder = 0
    end
    object cbBreakAfterUses: TCheckBox
      Left = 8
      Top = 76
      Width = 217
      Height = 17
      Caption = 'After uses'
      TabOrder = 2
    end
  end
end
