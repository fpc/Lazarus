object MainForm: TMainForm
  Left = 319
  Top = 165
  Width = 734
  Height = 522
  ActiveControl = GDBA
  Caption = 'Addressbook'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MMain
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 120
  TextHeight = 16
  object PTop: TPanel
    Left = 0
    Top = 0
    Width = 726
    Height = 33
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 0
    object DBNavigator1: TDBNavigator
      Left = 448
      Top = 4
      Width = 260
      Height = 25
      DataSource = DSAddress
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
  end
  object GDBA: TDBGrid
    Left = 0
    Top = 33
    Width = 726
    Height = 203
    Align = alClient
    DataSource = DSAddress
    TabOrder = 1
    Columns = <
      item
        FieldName = 'FirstName'
        Title.Caption = 'First name'
        Width = 117
        Visible = True
      end
      item
        FieldName = 'LastName'
        Title.Caption = 'Last name'
        Width = 127
        Visible = True
      end
      item
        FieldName = 'Street'
        Title.Caption = 'Street '
        Width = 131
        Visible = True
      end
      item
        FieldName = 'Zip'
        Title.Caption = 'Zip '
        Visible = True
      end
      item
        FieldName = 'Town'
        Title.Caption = 'Town '
        Width = 100
        Visible = True
      end
      item
        FieldName = 'Country'
        Title.Caption = 'Country '
        Visible = True
      end
      item
        FieldName = 'Telephone'
        Title.Caption = 'Telephone '
        Visible = True
      end
      item
        FieldName = 'Fax'
        Title.Caption = 'Fax '
        Visible = True
      end
      item
        FieldName = 'Mobile'
        Title.Caption = 'Mobile '
        Visible = True
      end
      item
        FieldName = 'Email'
        Title.Caption = 'Email '
        Visible = True
      end
      item
        FieldName = 'WWW'
        Title.Caption = 'WWW '
        Visible = True
      end>
  end
  object PBottom: TPanel
    Left = 0
    Top = 236
    Width = 726
    Height = 230
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 2
    object LELastName: TLabel
      Left = 8
      Top = 8
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Last name'
      FocusControl = ELastName
      Layout = tlCenter
    end
    object LEFirstName: TLabel
      Left = 8
      Top = 32
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&First name'
      FocusControl = EFirstName
      Layout = tlCenter
    end
    object LEStreet: TLabel
      Left = 8
      Top = 64
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Street'
      FocusControl = EStreet
      Layout = tlCenter
    end
    object LEZip: TLabel
      Left = 8
      Top = 88
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Zip'
      FocusControl = EZip
      Layout = tlCenter
    end
    object LETown: TLabel
      Left = 264
      Top = 88
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'T&own'
      FocusControl = ETown
      Layout = tlCenter
    end
    object LETelephone: TLabel
      Left = 8
      Top = 144
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Telephone'
      FocusControl = ETelephone
      Layout = tlCenter
    end
    object LECountry: TLabel
      Left = 8
      Top = 112
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Countr&y'
      FocusControl = ECountry
      Layout = tlCenter
    end
    object LEFax: TLabel
      Left = 288
      Top = 144
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Fa&x'
      FocusControl = EFax
      Layout = tlCenter
    end
    object LEMobile: TLabel
      Left = 472
      Top = 144
      Width = 41
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Mobile'
      FocusControl = EMobile
      Layout = tlCenter
    end
    object LEEmail: TLabel
      Left = 8
      Top = 176
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Email'
      FocusControl = EEmail
      Layout = tlCenter
    end
    object LEWWW: TLabel
      Left = 8
      Top = 200
      Width = 144
      Height = 24
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&WWW'
      FocusControl = EWWW
      Layout = tlCenter
    end
    object ELastName: TDBEdit
      Left = 160
      Top = 8
      Width = 361
      Height = 24
      DataField = 'LastName'
      DataSource = DSAddress
      TabOrder = 0
    end
    object EFirstName: TDBEdit
      Left = 160
      Top = 32
      Width = 361
      Height = 24
      DataField = 'FirstName'
      DataSource = DSAddress
      TabOrder = 1
    end
    object EStreet: TDBEdit
      Left = 160
      Top = 64
      Width = 361
      Height = 24
      DataField = 'Street'
      DataSource = DSAddress
      TabOrder = 2
    end
    object EZip: TDBEdit
      Left = 160
      Top = 88
      Width = 89
      Height = 24
      DataField = 'Zip'
      DataSource = DSAddress
      TabOrder = 3
    end
    object ECountry: TDBEdit
      Left = 160
      Top = 112
      Width = 361
      Height = 24
      DataField = 'Country'
      DataSource = DSAddress
      TabOrder = 5
    end
    object EWWW: TDBEdit
      Left = 160
      Top = 200
      Width = 361
      Height = 24
      DataField = 'WWW'
      DataSource = DSAddress
      TabOrder = 10
    end
    object ETelephone: TDBEdit
      Left = 160
      Top = 144
      Width = 121
      Height = 24
      DataField = 'Telephone'
      DataSource = DSAddress
      TabOrder = 6
    end
    object EFax: TDBEdit
      Left = 336
      Top = 144
      Width = 121
      Height = 24
      DataField = 'Fax'
      DataSource = DSAddress
      TabOrder = 7
    end
    object EMobile: TDBEdit
      Left = 520
      Top = 144
      Width = 121
      Height = 24
      DataField = 'Mobile'
      DataSource = DSAddress
      TabOrder = 8
    end
    object EEmail: TDBEdit
      Left = 160
      Top = 176
      Width = 361
      Height = 24
      DataField = 'Email'
      DataSource = DSAddress
      TabOrder = 9
    end
    object ETown: TDBEdit
      Left = 312
      Top = 88
      Width = 209
      Height = 24
      DataField = 'Town'
      DataSource = DSAddress
      TabOrder = 4
    end
  end
  object MMain: TMainMenu
    Left = 64
    Top = 72
    object MFile: TMenuItem
      Caption = '&File'
      object MINew: TMenuItem
        Action = ANew
      end
      object MIOpen: TMenuItem
        Action = AOpen
      end
      object MIClose: TMenuItem
        Action = AClose
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIQuit: TMenuItem
        Action = AQuit
      end
    end
    object Record1: TMenuItem
      Caption = '&Record'
      object MIFirst: TMenuItem
        Action = AFirst
      end
      object MILAst: TMenuItem
        Action = ALast
      end
      object MIPrior: TMenuItem
        Action = APrior
      end
      object MINext: TMenuItem
        Action = ANext
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MIInsert: TMenuItem
        Action = AInsert
      end
      object MIEdit: TMenuItem
        Action = AEdit
      end
      object MIDelete: TMenuItem
        Action = ADelete
      end
      object MIPost: TMenuItem
        Action = APost
      end
      object MICancel: TMenuItem
        Action = ACancel
      end
      object MIRefresh: TMenuItem
        Action = ARefresh
      end
    end
  end
  object ALMain: TActionList
    Left = 64
    Top = 112
    object ANew: TAction
      Caption = '&New'
      OnExecute = ANewExecute
    end
    object AOpen: TAction
      Caption = '&Open'
      ShortCut = 16463
      OnExecute = AOpenExecute
    end
    object AClose: TAction
      Caption = '&Close'
      OnExecute = ACloseExecute
      OnUpdate = ACloseUpdate
    end
    object AQuit: TAction
      Caption = '&Quit'
      ShortCut = 16465
      OnExecute = AQuitExecute
    end
    object AFirst: TAction
      Category = 'Dataset'
      Caption = '&First'
      ShortCut = 16454
    end
    object APrior: TAction
      Category = 'Dataset'
      Caption = '&Prior'
    end
    object ANext: TAction
      Category = 'Dataset'
      Caption = '&Next'
    end
    object ALast: TAction
      Category = 'Dataset'
      Caption = '&Last'
      ShortCut = 16460
    end
    object AInsert: TAction
      Category = 'Dataset'
      Caption = '&Insert'
      ShortCut = 16462
    end
    object ADelete: TAction
      Category = 'Dataset'
      Caption = '&Delete'
      ShortCut = 16452
    end
    object AEdit: TAction
      Category = 'Dataset'
      Caption = '&Edit'
      ShortCut = 113
    end
    object APost: TAction
      Category = 'Dataset'
      Caption = 'P&ost'
      ShortCut = 16467
    end
    object ACancel: TAction
      Category = 'Dataset'
      Caption = '&Cancel'
    end
    object ARefresh: TAction
      Category = 'Dataset'
      Caption = '&Refresh'
      ShortCut = 116
    end
  end
  object DBA: TDbf
    IndexDefs = <>
    StoreDefs = True
    TableLevel = 4
    AfterInsert = DBAAfterInsert
    Left = 96
    Top = 112
  end
  object SDDBA: TSaveDialog
    DefaultExt = '.dbf'
    Filter = 'DBF files|*.dbf|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 96
    Top = 144
  end
  object ODDBA: TOpenDialog
    DefaultExt = '.dbf'
    Filter = 'DBF files|*.dbf|All files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 64
    Top = 144
  end
  object DSAddress: TDataSource
    DataSet = DBA
    Left = 128
    Top = 112
  end
end
