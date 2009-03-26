inherited frBasic: TfrBasic
  Width = 542
  Height = 171
  Font.Charset = ANSI_CHARSET
  Font.Height = -15
  Font.Name = 'Segoe UI'
  ParentFont = False
  OnResize = FrameResize
  ExplicitWidth = 542
  ExplicitHeight = 171
  object lblOutput: TLabel
    Left = 4
    Top = 133
    Width = 71
    Height = 20
    Caption = 'Output file'
  end
  object lblInput: TLabel
    Left = 4
    Top = 99
    Width = 59
    Height = 20
    Caption = 'Input file'
  end
  object sbOpen: TSpeedButton
    Left = 501
    Top = 99
    Width = 28
    Height = 28
    Caption = '...'
    OnClick = sbOpenClick
  end
  object rgFileRecurse: TRadioGroup
    Left = 4
    Top = 4
    Width = 317
    Height = 89
    Caption = 'Files'
    ItemIndex = 0
    Items.Strings = (
      'Single file'
      'All files in directory'
      'All files in directory and all subdirectories')
    TabOrder = 0
    OnClick = rgFileRecurseClick
  end
  object rgBackup: TRadioGroup
    Left = 327
    Top = 4
    Width = 202
    Height = 89
    Caption = 'Backup'
    ItemIndex = 0
    Items.Strings = (
      'No backup'
      'Backup to separate file'
      'Output to separate file')
    TabOrder = 1
    OnClick = rgBackupClick
  end
  object edtInput: TEdit
    Left = 96
    Top = 99
    Width = 404
    Height = 28
    MaxLength = 255
    TabOrder = 2
    OnDragDrop = edtInputDragDrop
    OnDragOver = edtInputDragOver
    OnKeyUp = edtInputKeyUp
  end
  object edtOutput: TEdit
    Left = 96
    Top = 133
    Width = 433
    Height = 28
    Color = clBtnFace
    MaxLength = 255
    ReadOnly = True
    TabOrder = 3
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'pas'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a Delphi source file'
    Left = 364
    Top = 108
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 328
    Top = 112
  end
end
