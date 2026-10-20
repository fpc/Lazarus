object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Metafile Creator'
  ClientHeight = 441
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    663
    441)
  TextHeight = 15
  object PaintBox: TPaintBox
    Left = 238
    Top = 8
    Width = 417
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnPaint = PaintBoxPaint
  end
  object btnSave: TButton
    Left = 8
    Top = 402
    Width = 73
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object rbEMF: TRadioButton
    Left = 103
    Top = 406
    Width = 49
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'EMF'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object rbWMF: TRadioButton
    Left = 158
    Top = 406
    Width = 58
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'WMF'
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 194
    Height = 377
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Objects to be written'
    TabOrder = 3
    object ListBox: TListBox
      AlignWithMargins = True
      Left = 8
      Top = 20
      Width = 178
      Height = 349
      Margins.Left = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Align = alClient
      ItemHeight = 15
      Items.Strings = (
        'Rectangle'
        'RoundRect'
        'Ellipse'
        'Polygon'
        'Polygon (Winding)'
        'PolyLine'
        'PolyBezier'
        'Rectangle cross-hatch'
        'Rectangle hor-hatch bkcolor'
        'Pie'
        'Chord'
        'ArcTo'
        'Text'
        'Text align'
        'Pen styles (cosmetic)'
        'Pen styles (geometric)'
        'Pen from stock'
        'Path'
        'Path (Polygon winding)'
        'Bitmap'
        'Gradient')
      TabOrder = 0
      OnClick = ListBoxClick
      ExplicitHeight = 329
    end
  end
end
