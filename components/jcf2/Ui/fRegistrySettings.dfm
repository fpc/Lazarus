object fmRegistrySettings: TfmRegistrySettings
  Left = 73
  Top = 151
  BorderStyle = bsDialog
  Caption = 'JCF Registry Settings'
  ClientHeight = 389
  ClientWidth = 771
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 20
  object pgPages: TPageControl
    Left = 0
    Top = 0
    Width = 771
    Height = 334
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = '&General'
      object sbFile: TSpeedButton
        Left = 731
        Top = 11
        Width = 28
        Height = 28
        Caption = '...'
        OnClick = sbFileClick
      end
      object Label1: TLabel
        Left = 11
        Top = 12
        Width = 131
        Height = 20
        Caption = 'Convert settings file'
      end
      object Label2: TLabel
        Left = 11
        Top = 255
        Width = 104
        Height = 20
        Caption = 'MRU max items'
      end
      object eSettingsFile: TEdit
        Left = 160
        Top = 11
        Width = 565
        Height = 28
        TabOrder = 0
        OnKeyUp = eSettingsFileKeyUp
      end
      object eMRUMaxItems: TJvValidateEdit
        Left = 131
        Top = 249
        Width = 49
        Height = 28
        CriticalPoints.MaxValueIncluded = False
        CriticalPoints.MinValueIncluded = False
        EditText = '0'
        MaxLength = 2
        MaxValue = 12.000000000000000000
        TabOrder = 1
      end
      object btnClearMRU: TButton
        Left = 187
        Top = 247
        Width = 92
        Height = 30
        Caption = 'Clear MRU'
        TabOrder = 2
        OnClick = btnClearMRUClick
      end
      object rgShowParseTree: TRadioGroup
        Left = 11
        Top = 139
        Width = 268
        Height = 92
        Caption = 'Show parse &tree during parse'
        Items.Strings = (
          '&Always'
          '&On parse error'
          '&Never')
        TabOrder = 3
      end
      object rgWriteSettingsFile: TRadioGroup
        Left = 11
        Top = 39
        Width = 268
        Height = 94
        Caption = '&Write settings file'
        Items.Strings = (
          '&Always'
          '&Fail quietly'
          '&Never')
        TabOrder = 4
      end
      object cbCheckMultibyteChars: TCheckBox
        Left = 296
        Top = 49
        Width = 200
        Height = 20
        Caption = 'Check for &multibyte chars'
        TabOrder = 5
      end
    end
    object tsLogFile: TTabSheet
      Caption = '&Log file'
      ImageIndex = 1
      object sbSpecifedDir: TSpeedButton
        Left = 180
        Top = 195
        Width = 29
        Height = 26
        Caption = '...'
        OnClick = sbSpecifedDirClick
      end
      object Label3: TLabel
        Left = 7
        Top = 197
        Width = 167
        Height = 20
        Caption = 'Select specified directory'
      end
      object lblBackupFileExt: TLabel
        Left = 177
        Top = 20
        Width = 140
        Height = 20
        Caption = 'Backup file extension'
      end
      object lblOutputFileExt: TLabel
        Left = 177
        Top = 49
        Width = 138
        Height = 20
        Caption = 'Output file extension'
      end
      object rgLogLevel: TRadioGroup
        Left = 7
        Top = 7
        Width = 154
        Height = 90
        Caption = 'Log file detail level'
        Items.Strings = (
          'Errors only'
          'File'
          'Token')
        TabOrder = 0
      end
      object rgLogDir: TRadioGroup
        Left = 7
        Top = 99
        Width = 746
        Height = 89
        Caption = 'Log file directory'
        Items.Strings = (
          'Temp'
          'Application'
          'Specified')
        TabOrder = 3
      end
      object btnViewLog: TButton
        Left = 391
        Top = 239
        Width = 110
        Height = 36
        Caption = 'View Log now'
        TabOrder = 6
        OnClick = btnViewLogClick
      end
      object cbViewLog: TCheckBox
        Left = 7
        Top = 231
        Width = 226
        Height = 21
        Caption = 'View log after each run'
        TabOrder = 4
      end
      object edtBackupExt: TEdit
        Left = 320
        Top = 15
        Width = 55
        Height = 28
        MaxLength = 3
        TabOrder = 1
      end
      object edtOutputExt: TEdit
        Left = 320
        Top = 49
        Width = 55
        Height = 28
        MaxLength = 3
        TabOrder = 2
      end
      object cbLogTime: TCheckBox
        Left = 7
        Top = 256
        Width = 226
        Height = 21
        Caption = 'Log time taken to process'
        TabOrder = 5
      end
    end
    object tsExclusions: TTabSheet
      Caption = '&Exclusions'
      ImageIndex = 2
      OnResize = tsExclusionsResize
      object lblFilesCaption: TLabel
        Left = 11
        Top = 5
        Width = 324
        Height = 20
        Caption = 'Individual files to exclude from batch processing:'
      end
      object lblDirsCaption: TLabel
        Left = 11
        Top = 148
        Width = 300
        Height = 20
        Caption = 'Directories to exclude from batch processing:'
      end
      object mFiles: TJvMemo
        Left = 11
        Top = 25
        Width = 742
        Height = 114
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 0
      end
      object mDirs: TJvMemo
        Left = 11
        Top = 168
        Width = 742
        Height = 119
        AutoSize = False
        MaxLines = 0
        HideCaret = False
        TabOrder = 1
      end
    end
    object tsIde: TTabSheet
      Caption = 'IDE'
      ImageIndex = 3
      object cbEditorIntegration: TCheckBox
        Left = 11
        Top = 15
        Width = 214
        Height = 21
        Caption = '&Editor Integration'
        TabOrder = 0
      end
      object cbFormatBeforeSave: TCheckBox
        Left = 11
        Top = 59
        Width = 214
        Height = 20
        Caption = 'Format before &Save'
        TabOrder = 1
        OnClick = cbFormatBeforeSaveClick
      end
      object cbFormatAfterLoad: TCheckBox
        Left = 11
        Top = 37
        Width = 214
        Height = 21
        Caption = 'Format after &Load'
        TabOrder = 2
        OnClick = cbFormatAfterLoadClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 334
    Width = 771
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 300
      Top = 9
      Width = 92
      Height = 38
      Kind = bkOK
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 408
      Top = 9
      Width = 92
      Height = 38
      Kind = bkCancel
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.cfg'
    Left = 20
    Top = 340
  end
  object JvBrowseForFolderDialog1: TJvBrowseForFolderDialog
    Left = 56
    Top = 344
  end
end
