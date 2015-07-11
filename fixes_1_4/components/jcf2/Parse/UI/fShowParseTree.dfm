object frmShowParseTree: TfrmShowParseTree
  Left = 319
  Top = 116
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'JCF Parse Tree'
  ClientHeight = 555
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 20
  object StatusBar1: TStatusBar
    Left = 0
    Top = 530
    Width = 581
    Height = 25
    Panels = <>
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 581
    Height = 85
    Align = alTop
    BevelOuter = bvNone
    Constraints.MinHeight = 67
    Constraints.MinWidth = 67
    TabOrder = 1
    object lblTreeCount: TLabel
      Left = 11
      Top = 31
      Width = 110
      Height = 20
      Caption = 'Tree has ? nodes'
    end
    object lblTreeDepth: TLabel
      Left = 11
      Top = 55
      Width = 166
      Height = 20
      Caption = 'Tree has max depth of ??'
    end
    object cbShowWhiteSpace: TCheckBox
      Left = 11
      Top = 5
      Width = 144
      Height = 22
      Caption = 'Show whitespace'
      TabOrder = 0
      OnClick = cbShowWhiteSpaceClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 480
    Width = 581
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblCurrent: TLabel
      Left = 11
      Top = 2
      Width = 51
      Height = 20
      Caption = 'Current:'
    end
    object lblDepth: TLabel
      Left = 11
      Top = 26
      Width = 44
      Height = 20
      Caption = 'Depth:'
    end
    object lblTotalNodeCount: TLabel
      Left = 271
      Top = 26
      Width = 116
      Height = 20
      Caption = 'Total node count:'
    end
    object lblImmediateChildCount: TLabel
      Left = 271
      Top = 2
      Width = 152
      Height = 20
      Caption = 'Immediate child count:'
    end
  end
  object pcPages: TPageControl
    Left = 0
    Top = 85
    Width = 581
    Height = 395
    ActivePage = tsTokens
    Align = alClient
    TabOrder = 3
    object tsTokens: TTabSheet
      Caption = 'Tokens'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lvTokens: TListView
        Left = 0
        Top = 0
        Width = 573
        Height = 360
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 63
          end
          item
            Caption = 'Type'
            Width = 185
          end
          item
            Caption = 'Text'
            Width = 271
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvTokensDblClick
        OnSelectItem = lvTokensSelectItem
      end
    end
    object tsTree: TTabSheet
      Caption = 'Tree'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object tvParseTree: TTreeView
        Left = 0
        Top = 0
        Width = 573
        Height = 360
        Align = alClient
        HideSelection = False
        Indent = 22
        MultiSelectStyle = []
        ReadOnly = True
        TabOrder = 0
        OnChange = tvParseTreeChange
        OnDblClick = tvParseTreeDblClick
      end
    end
  end
end
