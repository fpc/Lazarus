{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit editor_color_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, StdCtrls, sysutils, ExtCtrls, Graphics, GraphUtil, ColorBox,
  ComCtrls, LCLProc, LCLType, LCLIntf, Dialogs, Menus, Laz_XMLCfg,
  SynEdit, SynEditMiscClasses, SynGutterCodeFolding, SynGutterLineNumber, SynEditTypes,
  SynGutterChanges, SynEditMouseCmds, SynEditHighlighter, SynTextDrawer, DividerBevel,
  EditorOptions, IDEOptionsIntf, editor_general_options, IDEImagesIntf,
  LazarusIDEStrConsts, IDEProcs, typinfo, LazConf, types;

type

  { TEditorColorOptionsFrame }

  TEditorColorOptionsFrame = class(TAbstractIDEOptionsEditor)
    BackGroundColorBox: TColorBox;
    BackGroundLabel: TLabel;
    bvlAttributeSection: TDividerBevel;
    ColumnPosBevel: TPanel;
    FrameStyleBox: TComboBox;
    FrameEdgesBox: TComboBox;
    FileExtensionsComboBox: TComboBox;
    FrameColorBox: TColorBox;
    BackGroundUseDefaultCheckBox: TCheckBox;
    FrameColorUseDefaultCheckBox: TCheckBox;
    ForegroundColorBox: TColorBox;
    ExportSaveDialog: TSaveDialog;
    pnlUnderline: TPanel;
    pnlBold: TPanel;
    pnlItalic: TPanel;
    PnlTop2: TPanel;
    pnlTop: TPanel;
    LanguageMenu: TPopupMenu;
    ColorSchemeMenu: TPopupMenu;
    Splitter1: TSplitter;
    TextBoldCheckBox: TCheckBox;
    TextBoldRadioInvert: TRadioButton;
    TextBoldRadioOff: TRadioButton;
    TextBoldRadioOn: TRadioButton;
    TextBoldRadioPanel: TPanel;
    TextItalicCheckBox: TCheckBox;
    TextItalicRadioInvert: TRadioButton;
    TextItalicRadioOff: TRadioButton;
    TextItalicRadioOn: TRadioButton;
    TextItalicRadioPanel: TPanel;
    TextUnderlineCheckBox: TCheckBox;
    TextUnderlineRadioInvert: TRadioButton;
    TextUnderlineRadioOff: TRadioButton;
    TextUnderlineRadioOn: TRadioButton;
    TextUnderlineRadioPanel: TPanel;
    ColorElementTree: TTreeView;
    ToolBar: TToolBar;
    ToolBar1: TToolBar;
    tbtnGlobal: TToolButton;
    tbtnLocal: TToolButton;
    ToolButton3: TToolButton;
    UseSyntaxHighlightCheckBox: TToolButton;
    ToolButton2: TToolButton;
    LanguageButton: TToolButton;
    ColorSchemeButton: TToolButton;
    ToolButton5: TToolButton;
    btnExport: TToolButton;
    SetAllAttributesToDefaultButton: TToolButton;
    SetAttributeToDefaultButton: TToolButton;
    ColorPreview: TSynEdit;
    ForeGroundLabel: TLabel;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    pnlElementAttributes: TPanel;
    procedure btnExportClick(Sender: TObject);
    procedure ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure ColorElementTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ColorElementTreeClick(Sender: TObject);
    procedure ColorElementTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ColorPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorSchemeButtonClick(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure ForegroundColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure pnlElementAttributesResize(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tglGlobalChange(Sender: TObject);
  private
    FTempColorSchemeSettings: TColorSchemeFactory;

    FDialog: TAbstractOptionsEditorDialog;
    FCurHighlightElement: TColorSchemeAttribute;

    UpdatingColor: Boolean;
    FFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    FColorSchemes: TStringList;    // list of LanguageName=ColorScheme

    FCurrentHighlighter: TSrcIDEHighlighter;
    FCurrentColorScheme: TColorSchemeLanguage;
    FIsEditingDefaults: Boolean;
    CurLanguageID: Integer;

    function  GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);
    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    procedure FillColorElementListBox;
    procedure SetColorElementsToDefaults(OnlySelected: Boolean);
    function  GetColorSchemeForLang(const LanguageName: String): String;
    procedure SetColorSchemeForLang(const LanguageName, ColorScheme: String);

    procedure SetCurrentScheme(SynClass: TCustomSynClass; const ColorScheme: String);
    procedure ApplyCurrentScheme;
    procedure UpdateCurrentScheme;

    procedure OnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure OnSpecialLineMarkup(Sender: TObject; Line: Integer;
      var Special: boolean; aMarkup: TSynSelectedColor);

    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    function DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
    procedure LanguageMenuItemClick(Sender: TObject);
    procedure ColorSchemeMenuItemClick(Sender: TObject);
    procedure SetLanguageItem(ALanguage: String);
    procedure SetColorSchemeItem(AScheme: String);
  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure SelectAhaColor(aha: TAdditionalHilightAttribute);
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  COLOR_NODE_PREFIX = ' abc  ';

function DefaultToNone(AColor: TColor): TColor;
begin
  if AColor = clDefault then
    Result := clNone
  else
    Result := AColor;
end;

function NoneToDefault(AColor: TColor): TColor;
begin
  if AColor = clNone then
    Result := clDefault
  else
    Result := AColor;
end;

{ TEditorColorOptionsFrame }

procedure TEditorColorOptionsFrame.ColorElementTreeChange(Sender: TObject; Node: TTreeNode);
begin
  FindCurHighlightElement;
end;

procedure TEditorColorOptionsFrame.ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean);
var
  NodeRect: TRect;
  FullAbcWidth, AbcWidth: Integer;
  Attri: TColorSchemeAttribute;
  TextY: Integer;
  AttriIdx: LongInt;
  c: TColor;
  s: String;
begin
  DefaultDraw := (node.Data = nil) or not (stage=cdPostPaint);
  if DefaultDraw  then exit;

  Attri := TColorSchemeAttribute(node.Data);
  if Attri.IsUsingSchemeGlobals then
    Attri := Attri.GetSchemeGlobal;


  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), Attri.StoredName);
  if FCurrentColorScheme = nil then exit;

  // Draw node background and name
  if cdsSelected in State then begin
    ColorElementTree.Canvas.Brush.Color := ColorElementTree.SelectionColor;
    ColorElementTree.Canvas.Font.Color := InvertColor(ColorElementTree.SelectionColor);
  end else begin
    ColorElementTree.Canvas.Brush.Color := ColorElementTree.BackgroundColor;
    ColorElementTree.Canvas.Font.Color := Font.Color;
  end;
  NodeRect := Node.DisplayRect(true);
  FullAbcWidth := ColorElementTree.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := (NodeRect.Top + NodeRect.Bottom - ColorElementTree.Canvas.TextHeight(Node.Text)) div 2;
  ColorElementTree.Canvas.FillRect(NodeRect);
  ColorElementTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, Attri.Name);

  // Draw preview box - Background
  c := clNone;
  if (hafBackColor in  Attri.Features) then
    c := Attri.Background;
  // Fallback Background-color for gutter
  if ((c = clNone) or (c = clDefault)) and
     (AttriIdx in [ord(ahaModifiedLine), ord(ahaCodeFoldingTree),
                   ord(ahaLineNumber), ord(ahaGutterSeparator)]) and
     (FCurrentColorScheme.AttributeByEnum[ahaGutter] <> nil)
  then
    c := FCurrentColorScheme.AttributeByEnum[ahaGutter].Background;
  // Fallback Background-color for text
  if (c = clNone) or (c = clDefault) then
    c := FCurrentColorScheme.DefaultAttribute.Background;
  if (c = clNone) or (c = clDefault) then
    c := ColorPreview.Color;
  ColorElementTree.Canvas.Brush.Color := c;
  ColorElementTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Special draw Modified line gutter
  if AttriIdx = ord(ahaModifiedLine) then begin
    TextY := NodeRect.Bottom - NodeRect.Top - 4;
    ColorElementTree.Canvas.Brush.Color := Attri.Foreground;
    ColorElementTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+5, NodeRect.Bottom-2);
    ColorElementTree.Canvas.Brush.Color := Attri.FrameColor;
    ColorElementTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2+ (TextY div 2), NodeRect.Left+5, NodeRect.Bottom-2);
    exit;
  end;

  // Draw preview Frame
  ColorElementTree.Canvas.Pen.Color := Attri.FrameColor;
  if (hafFrameColor in Attri.Features) and (AttriIdx <> ord(ahaCodeFoldingTree)) and
     (Attri.FrameColor <> clDefault) and (Attri.FrameColor <> clNone)
  then
    ColorElementTree.Canvas.Rectangle(NodeRect.Left+2, NodeRect.Top+2,
                                      NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Draw preview ForeGround
  if (hafForeColor in Attri.Features) //and
       //(ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].BG) )       // if no BG, then FG was used
  then begin
    c := Attri.Foreground;
    if ((c = clDefault) or (c = clNone)) and not (AttriIdx = ord(ahaLineNumber)) then
      c := FCurrentColorScheme.DefaultAttribute.Foreground;
    if (c = clNone) or (c = clDefault) then
      c := ColorPreview.Font.Color;

    if AttriIdx = ord(ahaCodeFoldingTree) then begin
      // Special draw fold gutter
      TextY := NodeRect.Bottom - NodeRect.Top - 8;

      // [-]
      ColorElementTree.Canvas.Pen.Color := c;
      ColorElementTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4,
                                        NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);

      // [+]
      inc(NodeRect.Left, TextY+2);
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);
      if (Attri.FrameColor <> clNone) and (Attri.FrameColor <> clDefault) then
        ColorElementTree.Canvas.Pen.Color := Attri.FrameColor;
      ColorElementTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4,
                                        NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Top+6);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-6);
      ColorElementTree.Canvas.Brush.Style := bsSolid;
    end
    else if AttriIdx = ord(ahaGutterSeparator) then begin
      ColorElementTree.Canvas.Pen.Color := c;
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+2);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+6, NodeRect.Bottom-2);
    end
    else if AttriIdx = ord(ahaRightMargin) then begin
      ColorElementTree.Canvas.Pen.Color := c;
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Top+2);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Bottom-2);
    end
    else begin
      s := 'abc';
      if AttriIdx = ord(ahaFoldedCode) then
        s:= '...';
      if AttriIdx = ord(ahaLineNumber) then
        s:= '123';
      ColorElementTree.Canvas.Font.Color := c;
      ColorElementTree.Canvas.Font.Style := Attri.Style;
      ColorElementTree.Canvas.Font.Height := -(NodeRect.Bottom - NodeRect.Top - 7);
      TextY := (NodeRect.Top + NodeRect.Bottom - canvas.TextHeight(s)) div 2;
      AbcWidth := ColorElementTree.Canvas.TextExtent(s).cx;
      SetBkMode(ColorElementTree.Canvas.Handle, TRANSPARENT);
      ColorElementTree.Canvas.TextOut(NodeRect.Left+(FullAbcWidth - AbcWidth) div 2, TextY, s);
      SetBkMode(ColorElementTree.Canvas.Handle, OPAQUE);

      ColorElementTree.Canvas.Font.Height := Font.Height;
      ColorElementTree.Canvas.Font.Style := [];
    end;
  end;
end;

procedure TEditorColorOptionsFrame.btnExportClick(Sender: TObject);
var
  XMLConfig: TRttiXMLConfig;
  NewScheme: TColorScheme;
  NewName: String;
  l: Integer;
begin
  ExportSaveDialog.InitialDir := UserSchemeDirectory(True);
  if ExportSaveDialog.Execute then begin
    NewName := ExtractFileName(ExportSaveDialog.FileName);
    l := length(ExtractFileExt(NewName));
    if (l > 0) and (l+1 < Length(NewName)) then
      NewName := Copy(NewName, 1, Length(NewName) - l);
    l := UTF8CharacterLength(PChar(NewName));
    if l > 0 then
      NewName := UTF8UpperCase(copy(NewName, 1, l)) + copy(NewName, 1+l, length(NewName));

    XMLConfig := TRttiXMLConfig.CreateClean(ExportSaveDialog.FileName);
    XMLConfig.SetValue('Lazarus/ColorSchemes/Names/Count', 1);
    XMLConfig.SetValue('Lazarus/ColorSchemes/Names/Item1/Value', NewName);

    NewScheme := TColorScheme.Create(NewName);
    NewScheme.Assign(FTempColorSchemeSettings.ColorSchemeGroup[ColorSchemeButton.Caption]);
    NewScheme.SaveToXml(XMLConfig, 'Lazarus/ColorSchemes/',nil);
    NewScheme.Free;

    InvalidateFileStateCache;
    XMLConfig.Flush;
    XMLConfig.Free;
  end;
end;

procedure TEditorColorOptionsFrame.ColorElementTreeClick(Sender: TObject);
begin
  FindCurHighlightElement;
end;

procedure TEditorColorOptionsFrame.ColorElementTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lnode: TTreeNode;
begin
  lnode := ColorElementTree.Selected;
  if (Key = VK_UP) and (lnode <> nil) then begin
    lnode := lnode.GetPrevExpanded;
    if (lnode <> nil) and (lnode.GetFirstChild <> nil) then
      lnode := lnode.GetPrevExpanded;
    if (lnode <> nil) then begin
      Key :=VK_UNKNOWN;
      ColorElementTree.Selected := lnode;
    end;
  end;
end;

procedure TEditorColorOptionsFrame.ColorPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Token: String;
  Attri: TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr: TAdditionalHilightAttribute;
  NewNode: TTreeNode;
begin
  MouseXY := Point(X - (ColorPreview.CharWidth div 2), Y);
  XY := ColorPreview.PixelsToRowColumn(MouseXY);
  NewNode := nil;
  // Gutter Colors
  if X <= ColorPreview.Gutter.Width then begin
    for i := 0 to ColorPreview.Gutter.Parts.Count-1 do begin
      if ColorPreview.Gutter.Parts[i].Width > X then begin
        if ColorPreview.Gutter.Parts[i] is TSynGutterLineNumber then
          SelectAhaColor(ahaLineNumber)
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterChanges then
          SelectAhaColor(ahaModifiedLine)
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterCodeFolding then
          SelectAhaColor(ahaCodeFoldingTree)
        else
          SelectAhaColor(ahaGutter);
        exit;
      end;
      X := X - ColorPreview.Gutter.Parts[i].Width;
    end;
    exit;
  end;
  // Line Highlights
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr = ahaFoldedCode then begin
      if not( (XY.X >= Length(ColorPreview.Lines[XY.Y-1]) + 4) and
              (XY.X <= Length(ColorPreview.Lines[XY.Y-1]) + 6) )
      then
        AddAttr := ahaNone;
        //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
    end;
    if AddAttr <> ahaNone then begin
      SelectAhaColor(AddAttr);
      exit;
    end;
      //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
  end;
  if (XY.Y = ColorPreview.CaretY) and
     (XY.X > Length(ColorPreview.Lines[XY.Y - 1])+1)
  then begin
    SelectAhaColor(ahaLineHighlight);
    exit;
    //NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[ahaLineHighlight]);
  end;
  if FIsEditingDefaults then
    exit;
  // Pascal Highlights
  Token := '';
  Attri := nil;
  ColorPreview.GetHighlighterAttriAtRowCol(XY, Token, Attri);
  if Attri = nil then
    Attri := FCurrentHighlighter.WhitespaceAttribute;
  if Attri <> nil then
    NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+Attri.Name);
  if NewNode <> nil then begin
    NewNode.Selected := True;
    FindCurHighlightElement;
  end;
end;

procedure TEditorColorOptionsFrame.ColorSchemeButtonClick(Sender: TObject);
begin
  ColorSchemeButton.CheckMenuDropdown;
end;

procedure TEditorColorOptionsFrame.ForegroundColorBoxChange(Sender: TObject);
var
  AttrToEdit: TColorSchemeAttribute;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    exit;
  UpdatingColor := True;

  AttrToEdit := FCurHighlightElement;
  if FCurHighlightElement.IsUsingSchemeGlobals then
    AttrToEdit := FCurHighlightElement.GetSchemeGlobal;

  if Sender = ForegroundColorBox then
  begin
    AttrToEdit.Foreground := DefaultToNone(ForeGroundColorBox.Selected);
    ForeGroundUseDefaultCheckBox.Checked := ForeGroundColorBox.Selected <> clDefault;
  end;
  if Sender = BackGroundColorBox then
  begin
    AttrToEdit.Background := DefaultToNone(BackGroundColorBox.Selected);
    BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;
  end;
  if Sender = FrameColorBox then
  begin
    AttrToEdit.FrameColor := DefaultToNone(FrameColorBox.Selected);
    FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.Enabled := FrameColorBox.Selected <> clDefault;
    FrameStyleBox.Enabled := FrameColorBox.Selected <> clDefault;
  end;
  if Sender = FrameEdgesBox then
  begin
    AttrToEdit.FrameEdges := TSynFrameEdges(FrameEdgesBox.ItemIndex);
  end;
  if Sender = FrameStyleBox then
  begin
    AttrToEdit.FrameStyle := TSynLineStyle(FrameStyleBox.ItemIndex);
  end;

  UpdatingColor := False;
  UpdateCurrentScheme;
end;

procedure TEditorColorOptionsFrame.ForegroundColorBoxGetColors(Sender: TCustomColorBox;
  Items: TStrings);
var
  i: longint;
begin
  i := Items.IndexOfObject(TObject(PtrInt(clDefault)));
  if i >= 0 then begin
    Items[i] := dlgColorNotModified;
    Items.Move(i, 1);
  end;
end;

procedure TEditorColorOptionsFrame.FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  PCol: Integer;
begin
  if Index  < 0 then exit;;

  r.top := ARect.top + 3;
  r.bottom := ARect.bottom - 3;
  r.left := ARect.left + 5;
  r.right := ARect.Right - 5;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 1;
    PCol := pen.Color;
    Pen.Color := clGray;
    Pen.Style := psDot;
    Pen.EndCap := pecFlat;
    Rectangle(r);
    Pen.Width := 2;
    pen.Color := PCol;
    Pen.Style := psSolid;
    case Index of
      0: Rectangle(r);
      1: begin
          MoveTo(r.Left, r.Bottom);
          LineTo(r.Right-1, r.Bottom);
        end;
      2: begin
          MoveTo(r.Left, r.Top);
          LineTo(r.Left, r.Bottom-1);
        end;
    end;
  end;
end;

procedure TEditorColorOptionsFrame.FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  p: TPoint;
begin
  if Index  < 0 then exit;;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 2;
    pen.EndCap := pecFlat;
    case Index of
      0: Pen.Style := psSolid;
      1: Pen.Style := psDash;
      2: Pen.Style := psDot;
      3: Pen.Style := psSolid;
    end;
    if Index = 3 then begin
      MoveToEx(Handle, ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2 - 2, @p);
      WaveTo(Handle, ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2 - 2, 4);
    end else begin
      MoveTo(ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2);
      LineTo(ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2);
    end;
  end;
end;

procedure TEditorColorOptionsFrame.GeneralCheckBoxOnChange(Sender: TObject);
var
  TheColorBox: TColorBox;
  AttrToEdit: TColorSchemeAttribute;
begin
  if Sender = UseSyntaxHighlightCheckBox then
  begin
    ApplyCurrentScheme;
    Exit;
  end;

  if FCurHighlightElement = nil then
    exit;

  AttrToEdit := FCurHighlightElement;
  if FCurHighlightElement.IsUsingSchemeGlobals then
    AttrToEdit := FCurHighlightElement.GetSchemeGlobal;

  if UpdatingColor = False then begin
    UpdatingColor := True;

    TheColorBox := nil;
    if Sender = ForeGroundUseDefaultCheckBox then TheColorBox := ForegroundColorBox;
    if Sender = BackGroundUseDefaultCheckBox then TheColorBox := BackGroundColorBox;
    if Sender = FrameColorUseDefaultCheckBox then TheColorBox := FrameColorBox;
    if Assigned(TheColorBox) then begin
      if TCheckBox(Sender).Checked then begin
        TheColorBox.Selected := TheColorBox.Tag;
      end
      else begin
        TheColorBox.Tag := TheColorBox.Selected;
        TheColorBox.Selected := clDefault;
      end;

      if (Sender = ForeGroundUseDefaultCheckBox) and
         (DefaultToNone(ForegroundColorBox.Selected) <> AttrToEdit.Foreground)
      then begin
        AttrToEdit.Foreground := DefaultToNone(ForegroundColorBox.Selected);
        UpdateCurrentScheme;
      end;
      if (Sender = BackGroundUseDefaultCheckBox) and
         (DefaultToNone(BackGroundColorBox.Selected) <> AttrToEdit.Background)
      then begin
        AttrToEdit.Background := DefaultToNone(BackGroundColorBox.Selected);
        UpdateCurrentScheme;
      end;
      if (Sender = FrameColorUseDefaultCheckBox) and
         (DefaultToNone(FrameColorBox.Selected) <> AttrToEdit.FrameColor)
      then begin
        AttrToEdit.FrameColor := DefaultToNone(FrameColorBox.Selected);
        FrameEdgesBox.Enabled := TCheckBox(Sender).Checked;
        FrameStyleBox.Enabled := TCheckBox(Sender).Checked;
        UpdateCurrentScheme;
      end;
    end;

    UpdatingColor := False;
  end;

  if Sender = TextBoldCheckBox then begin
    if hafStyleMask in AttrToEdit.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextBoldCheckBox.Checked xor (fsBold in AttrToEdit.Style) then
    begin
      if TextBoldCheckBox.Checked then
        AttrToEdit.Style := AttrToEdit.Style + [fsBold]
      else
        AttrToEdit.Style := AttrToEdit.Style - [fsBold];
      UpdateCurrentScheme;
    end;
  end;

  if Sender = TextItalicCheckBox then begin
    if hafStyleMask in AttrToEdit.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextItalicCheckBox.Checked xor (fsItalic in AttrToEdit.Style) then
    begin
      if TextItalicCheckBox.Checked then
        AttrToEdit.Style := AttrToEdit.Style + [fsItalic]
      else
        AttrToEdit.Style := AttrToEdit.Style - [fsItalic];
      UpdateCurrentScheme;
    end;
  end;

  if Sender = TextUnderlineCheckBox then begin
    if hafStyleMask in AttrToEdit.Features then
      TextStyleRadioOnChange(Sender)
    else
    if TextUnderlineCheckBox.Checked xor (fsUnderline in AttrToEdit.Style) then
    begin
      if TextUnderlineCheckBox.Checked then
        AttrToEdit.Style := AttrToEdit.Style + [fsUnderline]
      else
        AttrToEdit.Style := AttrToEdit.Style - [fsUnderline];
      UpdateCurrentScheme;
    end;
  end;
end;

procedure TEditorColorOptionsFrame.ComboBoxOnExit(Sender: TObject);
var
  Box: TComboBox absolute Sender;
begin
  if Sender = FileExtensionsComboBox then
  begin
    //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Old="',GetCurFileExtensions(FCurrentHighlighter.LanguageName),'" FCurrentHighlighter.LanguageName=',FCurrentHighlighter.LanguageName]);
    if Box.Text <> GetCurFileExtensions(FCurrentHighlighter.LanguageName) then
    begin
      SetCurFileExtensions(FCurrentHighlighter.LanguageName, Box.Text);
      SetComboBoxText(Box, Box.Text,cstCaseInsensitive);
    end;
    //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Now="',GetCurFileExtensions(FCurrentHighlighter.LanguageName),'" FCurrentHighlighter.LanguageName=',FCurrentHighlighter.LanguageName]);
  end
end;

procedure TEditorColorOptionsFrame.LanguageButtonClick(Sender: TObject);
begin
  LanguageButton.CheckMenuDropdown;
end;

procedure TEditorColorOptionsFrame.pnlElementAttributesResize(Sender: TObject);
var
  MinAnchor: TControl;
  MinWidth: Integer;

  procedure CheckControl(Other: TControl);
  var w,h: Integer;
  begin
    if not Other.Visible then exit;
    Other.GetPreferredSize(w,h);
    if w <= MinWidth then exit;
    MinAnchor := Other;
    MinWidth := w;
  end;
begin
  MinWidth := -1;
  MinAnchor := ForeGroundLabel;
  CheckControl(ForeGroundLabel);
  CheckControl(BackGroundLabel);
  CheckControl(ForeGroundUseDefaultCheckBox);
  CheckControl(BackGroundUseDefaultCheckBox);
  CheckControl(FrameColorUseDefaultCheckBox);

  ColumnPosBevel.AnchorSide[akLeft].Control := MinAnchor;
end;

procedure TEditorColorOptionsFrame.SetAllAttributesToDefaultButtonClick(
  Sender: TObject);
begin
  SetColorElementsToDefaults(False);
end;

procedure TEditorColorOptionsFrame.SetAttributeToDefaultButtonClick(
  Sender: TObject);
begin
  SetColorElementsToDefaults(True);
end;

procedure TEditorColorOptionsFrame.TextStyleRadioOnChange(Sender: TObject);
var
  AttrToEdit: TColorSchemeAttribute;

  procedure CalcNewStyle(CheckBox: TCheckBox; RadioOn, RadioOff,
                         RadioInvert: TRadioButton; fs : TFontStyle;
                         Panel: TPanel);
  begin
    if CheckBox.Checked then
    begin
      Panel.Enabled := True;
      if RadioInvert.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style + [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask - [fs];
      end
      else
      if RadioOn.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style + [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask + [fs];
      end
      else
      if RadioOff.Checked then
      begin
        AttrToEdit.Style     := AttrToEdit.Style - [fs];
        AttrToEdit.StyleMask := AttrToEdit.StyleMask + [fs];
      end
    end
    else
    begin
      Panel.Enabled := False;
      AttrToEdit.Style     := AttrToEdit.Style - [fs];
      AttrToEdit.StyleMask := AttrToEdit.StyleMask - [fs];
    end;
  end;
begin
  if UpdatingColor or not (hafStyleMask in FCurHighlightElement.Features) then
    Exit;

  AttrToEdit := FCurHighlightElement;
  if FCurHighlightElement.IsUsingSchemeGlobals then
    AttrToEdit := FCurHighlightElement.GetSchemeGlobal;

  if (Sender = TextBoldCheckBox) or
     (Sender = TextBoldRadioOn) or
     (Sender = TextBoldRadioOff) or
     (Sender = TextBoldRadioInvert) then
    CalcNewStyle(TextBoldCheckBox, TextBoldRadioOn, TextBoldRadioOff,
                    TextBoldRadioInvert, fsBold, TextBoldRadioPanel);

  if (Sender = TextItalicCheckBox) or
     (Sender = TextItalicRadioOn) or
     (Sender = TextItalicRadioOff) or
     (Sender = TextItalicRadioInvert) then
    CalcNewStyle(TextItalicCheckBox, TextItalicRadioOn, TextItalicRadioOff,
                    TextItalicRadioInvert, fsItalic, TextItalicRadioPanel);

  if (Sender = TextUnderlineCheckBox) or
     (Sender = TextUnderlineRadioOn) or
     (Sender = TextUnderlineRadioOff) or
     (Sender = TextUnderlineRadioInvert) then
    CalcNewStyle(TextUnderlineCheckBox, TextUnderlineRadioOn, TextUnderlineRadioOff,
                    TextUnderlineRadioInvert, fsUnderline, TextUnderlineRadioPanel);


  UpdateCurrentScheme;
end;

procedure TEditorColorOptionsFrame.ShowCurAttribute;
var
  AttrToShow: TColorSchemeAttribute;
  CanGlobal: Boolean;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    Exit;
  UpdatingColor := True;
  DisableAlign;
  try

  CanGlobal := (FCurHighlightElement.GetSchemeGlobal <> nil) and
                          not FIsEditingDefaults;
  tbtnGlobal.Enabled := CanGlobal;
  tbtnLocal.Enabled := CanGlobal;
  tbtnGlobal.AllowAllUp := not CanGlobal;
  tbtnLocal.AllowAllUp := not CanGlobal;
  tbtnGlobal.Down := FCurHighlightElement.IsUsingSchemeGlobals and
                     CanGlobal;
  tbtnLocal.Down  := (not FCurHighlightElement.IsUsingSchemeGlobals) and
                     CanGlobal;

  AttrToShow := FCurHighlightElement;
  if FCurHighlightElement.IsUsingSchemeGlobals then
    AttrToShow := FCurHighlightElement.GetSchemeGlobal;

  // Adjust color captions
  ForeGroundUseDefaultCheckBox.Caption := dlgForecolor;
  FrameColorUseDefaultCheckBox.Caption := dlgFrameColor;
  if (FCurrentColorScheme.AttributeByEnum[ahaModifiedLine] <> nil) and
     (AttrToShow.StoredName = FCurrentColorScheme.AttributeByEnum[ahaModifiedLine].StoredName)
  then begin
    ForeGroundUseDefaultCheckBox.Caption := dlgSavedLineColor;
    FrameColorUseDefaultCheckBox.Caption := dlgUnsavedLineColor;
  end else
  if (FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree] <> nil) and
     (AttrToShow.StoredName = FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree].StoredName)
  then begin
    FrameColorUseDefaultCheckBox.Caption := dlgGutterCollapsedColor;
  end;

  if AttrToShow.Group = agnDefault then begin
    ForegroundColorBox.Style := ForegroundColorBox.Style - [cbIncludeDefault];
    BackGroundColorBox.Style := BackGroundColorBox.Style - [cbIncludeDefault];
  end else begin
    ForegroundColorBox.Style := ForegroundColorBox.Style + [cbIncludeDefault];
    BackGroundColorBox.Style := BackGroundColorBox.Style + [cbIncludeDefault];
  end;

  // Forground
  ForeGroundLabel.Visible              := (hafForeColor in AttrToShow.Features) and
                                          (AttrToShow.Group = agnDefault);
  ForeGroundUseDefaultCheckBox.Visible := (hafForeColor in AttrToShow.Features) and
                                          not(AttrToShow.Group = agnDefault);
  ForegroundColorBox.Visible           := (hafForeColor in AttrToShow.Features);

  ForegroundColorBox.Selected := NoneToDefault(AttrToShow.Foreground);
  if ForegroundColorBox.Selected = clDefault then
    ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
  else
    ForegroundColorBox.Tag := ForegroundColorBox.Selected;
  ForeGroundUseDefaultCheckBox.Checked := ForegroundColorBox.Selected <> clDefault;

  // BackGround
  BackGroundLabel.Visible              := (hafBackColor in AttrToShow.Features) and
                                          (AttrToShow.Group = agnDefault);
  BackGroundUseDefaultCheckBox.Visible := (hafBackColor in AttrToShow.Features) and
                                          not(AttrToShow.Group = agnDefault);
  BackGroundColorBox.Visible           := (hafBackColor in AttrToShow.Features);

  BackGroundColorBox.Selected := NoneToDefault(AttrToShow.Background);
  if BackGroundColorBox.Selected = clDefault then
    BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
  else
    BackGroundColorBox.Tag := BackGroundColorBox.Selected;
  BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected <> clDefault;

  // Frame
  FrameColorUseDefaultCheckBox.Visible := hafFrameColor in AttrToShow.Features;
  FrameColorBox.Visible                := hafFrameColor in AttrToShow.Features;
  FrameEdgesBox.Visible                := hafFrameEdges in AttrToShow.Features;
  FrameStyleBox.Visible                := hafFrameStyle in AttrToShow.Features;

  FrameColorBox.Selected := NoneToDefault(AttrToShow.FrameColor);
  if FrameColorBox.Selected = clDefault then
    FrameColorBox.Tag := FrameColorBox.DefaultColorColor
  else
    FrameColorBox.Tag := FrameColorBox.Selected;
  FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected <> clDefault;
  FrameEdgesBox.ItemIndex := integer(AttrToShow.FrameEdges);
  FrameStyleBox.ItemIndex := integer(AttrToShow.FrameStyle);
  FrameEdgesBox.Enabled := FrameColorUseDefaultCheckBox.Checked;
  FrameStyleBox.Enabled := FrameColorUseDefaultCheckBox.Checked;

  // Styles
  TextBoldCheckBox.Visible      := hafStyle in AttrToShow.Features;
  TextItalicCheckBox.Visible    := hafStyle in AttrToShow.Features;
  TextUnderlineCheckBox.Visible := hafStyle in AttrToShow.Features;

  TextBoldRadioPanel.Visible      := hafStyleMask in AttrToShow.Features;
  TextItalicRadioPanel.Visible    := hafStyleMask in AttrToShow.Features;
  TextUnderlineRadioPanel.Visible := hafStyleMask in AttrToShow.Features;

  if hafStyleMask in AttrToShow.Features then begin
    TextBoldCheckBox.Checked   := (fsBold in AttrToShow.Style) or
                                  (fsBold in AttrToShow.StyleMask);
    TextBoldRadioPanel.Enabled := TextBoldCheckBox.Checked;

    if not(fsBold in AttrToShow.StyleMask) then
      TextBoldRadioInvert.Checked := True
    else
    if fsBold in AttrToShow.Style then
      TextBoldRadioOn.Checked := True
    else
      TextBoldRadioOff.Checked := True;

    TextItalicCheckBox.Checked   := (fsItalic in AttrToShow.Style) or
                                    (fsItalic in AttrToShow.StyleMask);
    TextItalicRadioPanel.Enabled := TextItalicCheckBox.Checked;

    if not(fsItalic in AttrToShow.StyleMask) then
      TextItalicRadioInvert.Checked := True
    else
    if fsItalic in AttrToShow.Style then
      TextItalicRadioOn.Checked := True
    else
      TextItalicRadioOff.Checked := True;

    TextUnderlineCheckBox.Checked := (fsUnderline in AttrToShow.Style) or
                                (fsUnderline in AttrToShow.StyleMask);
    TextUnderlineRadioPanel.Enabled := TextUnderlineCheckBox.Checked;

    if not(fsUnderline in AttrToShow.StyleMask) then
      TextUnderlineRadioInvert.Checked := True
    else
    if fsUnderline in AttrToShow.Style then
      TextUnderlineRadioOn.Checked := True
    else
      TextUnderlineRadioOff.Checked := True;
  end
  else
  begin
    TextBoldCheckBox.Checked      := fsBold in AttrToShow.Style;
    TextItalicCheckBox.Checked    := fsItalic in AttrToShow.Style;
    TextUnderlineCheckBox.Checked := fsUnderline in AttrToShow.Style;
  end;

  UpdatingColor := False;
  finally
    EnableAlign;
  end;
  pnlElementAttributesResize(nil);
end;

procedure TEditorColorOptionsFrame.FindCurHighlightElement;
var
  Old: TColorSchemeAttribute;
begin
  if (ColorElementTree.Selected <> nil) and
     (ColorElementTree.Selected.Parent = nil) and
     (ColorElementTree.Selected.GetFirstChild <> nil)
  then
    ColorElementTree.Selected.GetFirstChild.Selected := True;
  if (ColorElementTree.Selected = nil) or (ColorElementTree.Selected.Data = nil) then
    exit;

  Old := FCurHighlightElement;

  FCurHighlightElement := TColorSchemeAttribute(ColorElementTree.Selected.Data);

  if (Old <> FCurHighlightElement) then
    ShowCurAttribute;
end;

procedure TEditorColorOptionsFrame.FillColorElementListBox;
var
  i: Integer;
  ParentName: String;
  ParentNode: TTreeNode;
  j: TAhaGroupName;
  Attr: TColorSchemeAttribute;
  NewNode, DefNode: TTreeNode;
begin
  ColorElementTree.BeginUpdate;
  ColorElementTree.Items.Clear;

  // Create Groups
  if not FIsEditingDefaults then
    ColorElementTree.Items.Add(nil, FCurrentHighlighter.LanguageName)
  else
    ColorElementTree.Items.Add(nil, AdditionalHighlightGroupNames[agnDefault]);
  for j := low(TAhaGroupName) to high(TAhaGroupName) do
    if not(j in [agnDefault, agnLanguage]) then
      ColorElementTree.Items.Add(nil, AdditionalHighlightGroupNames[j]);

  // Fill Attributes in
  for i := 0 to FCurrentColorScheme.AttributeCount - 1 do begin
    Attr := FCurrentColorScheme.AttributeAtPos[i];
    if Attr.Name <> '' then begin
      case Attr.Group of
        agnDefault, //  continue; // default is currently not shown
        agnLanguage:
          begin
            if FIsEditingDefaults then
              ParentName := AdditionalHighlightGroupNames[agnDefault]
            else
              ParentName := FCurrentHighlighter.LanguageName;
          end;
        else
          ParentName := AdditionalHighlightGroupNames[Attr.Group];
      end;
      ParentNode := ColorElementTree.Items.FindTopLvlNode(ParentName);
      if ParentNode = nil then
        ParentNode := ColorElementTree.Items.Add(nil, ParentName);
      NewNode :=  ColorElementTree.Items.AddChild(ParentNode, COLOR_NODE_PREFIX + Attr.Name);
      NewNode.Data := Pointer(Attr);
      if Attr.Group = agnDefault then
        DefNode := NewNode;
    end;
  end;

  for i := 0 to ColorElementTree.Items.Count - 1 do
    ColorElementTree.Items[i].AlphaSort;
  if DefNode <> nil then
    DefNode.Index := 0;

  ColorElementTree.EndUpdate;
  ColorElementTree.FullExpand;
  if ColorElementTree.Items.GetFirstNode <> nil then
    ColorElementTree.Items.GetFirstNode.Selected := True;

  FCurHighlightElement := nil;
  FindCurHighlightElement;
end;

procedure TEditorColorOptionsFrame.SetColorElementsToDefaults(OnlySelected: Boolean);
var
  DefaultSchemeGrp: TColorScheme;
  DefaultColorScheme: TColorSchemeLanguage;
  DefAttri: TColorSchemeAttribute;
  i: Integer;
begin
  DefaultSchemeGrp := ColorSchemeFactory.ColorSchemeGroup[ColorSchemeButton.Caption];
  if DefaultSchemeGrp = nil then
    exit;
  if FIsEditingDefaults then
    DefaultColorScheme := DefaultSchemeGrp.DefaultColors
  else
    DefaultColorScheme := DefaultSchemeGrp.ColorScheme[FCurrentColorScheme.Language];

  if OnlySelected then begin
    DefAttri := DefaultColorScheme.Attribute[FCurHighlightElement.StoredName];
    FCurHighlightElement.Assign(DefAttri);
  end else begin
    FCurrentColorScheme.Assign(DefaultColorScheme);
  end;

  // reassign tree nodes => in case
  for i := 0 to ColorElementTree.Items.Count - 1 do begin
    if (ColorElementTree.Items[i].Data <> nil) and
       (FCurrentColorScheme.IndexOfAttr
         (TColorSchemeAttribute(ColorElementTree.Items[i].Data)) < 0)
    then begin
      debugln('Error: missing Attr after assign');
      FillColorElementListBox;
      break;
    end;
  end;

  FindCurHighlightElement;
  UpdateCurrentScheme;
  ShowCurAttribute;
end;

function TEditorColorOptionsFrame.GetColorSchemeForLang(const LanguageName: String): String;
begin
  if FColorSchemes = nil then
    Result := ''
  else
    Result := FColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := EditorOpts.ReadColorScheme(LanguageName);
end;

procedure TEditorColorOptionsFrame.SetColorSchemeForLang(const LanguageName,
  ColorScheme: String);
begin
  if FColorSchemes = nil then
    FColorSchemes := TStringList.Create;
  FColorSchemes.Values[LanguageName] := ColorScheme;
end;

procedure TEditorColorOptionsFrame.SetCurrentScheme(SynClass: TCustomSynClass;
  const ColorScheme: String);
var
  SchemeGrp: TColorScheme;
  NewColorScheme: TColorSchemeLanguage;
begin
  // Modfiy directly => will be re-read form XML if canceled
  SchemeGrp := FTempColorSchemeSettings.ColorSchemeGroup[ColorScheme];
  if SchemeGrp = nil then
    exit;

  if FIsEditingDefaults then
    NewColorScheme := SchemeGrp.DefaultColors
  else
    NewColorScheme := SchemeGrp.ColorSchemeBySynClass[SynClass];
  if (NewColorScheme = FCurrentColorScheme) then
    exit;

  FCurrentColorScheme := NewColorScheme;
  if not FIsEditingDefaults then
    FCurrentHighlighter := FCurrentColorScheme.Highlighter;
  ApplyCurrentScheme;
  FillColorElementListBox;
end;

procedure TEditorColorOptionsFrame.ApplyCurrentScheme;
var
  a: Integer;
begin
  // there is always a colorscheme selected, except during initialization
  if FCurrentColorScheme = nil then
    exit;
  with GeneralPage do begin
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      PreviewEdits[a].BeginUpdate;
    try
      for a := Low(PreviewEdits) to High(PreviewEdits) do begin
        if UseSyntaxHighlightCheckBox.Down then
          PreviewEdits[a].Highlighter := FCurrentHighlighter
        else
          PreviewEdits[a].Highlighter := nil;
        PreviewEdits[a].Lines.Text := EditorOpts.HighlighterList[CurLanguageID].SampleSource;
        PreviewEdits[a].CaretXY := EditorOpts.HighlighterList[CurLanguageID].CaretXY;
        PreviewEdits[a].TopLine := 1;
        PreviewEdits[a].LeftChar := 1;
        PreviewEdits[a].Keystrokes.Clear;
        PreviewEdits[a].MouseActions.Clear;
        PreviewEdits[a].AfterLoadFromFile;
        PreviewEdits[a].Keystrokes.Clear;
        PreviewEdits[a].MouseActions.Clear;
      end;
      UpdateCurrentScheme;
    finally
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        PreviewEdits[a].EndUpdate;
    end;
  end;
end;

procedure TEditorColorOptionsFrame.UpdateCurrentScheme;
var
  a: Integer;
begin
  // there is always a colorscheme selected, except during initialization
  with GeneralPage do begin
    if FCurrentColorScheme = nil then
      exit;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      PreviewEdits[a].BeginUpdate;
    try
      if not FIsEditingDefaults then
        FCurrentColorScheme.ApplyTo(FCurrentHighlighter);
      for a := Low(PreviewEdits) to High(PreviewEdits) do begin
        FCurrentColorScheme.ApplyTo(PreviewEdits[a]);
        PreviewEdits[a].Invalidate;
      end;
    finally
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        PreviewEdits[a].EndUpdate;
    end;
  end;
  ColorElementTree.Invalidate;
end;

function TEditorColorOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame; inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorColorOptionsFrame.DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := True;
end;

procedure TEditorColorOptionsFrame.LanguageMenuItemClick(Sender: TObject);
var
  Language: String;
  NewVal: LongInt;
begin
  if (Sender as TMenuItem).MenuIndex = 0 then
  begin
    if not FIsEditingDefaults then
    begin
      FIsEditingDefaults := True;
      SetCurrentScheme(TCustomSynClass(FCurrentHighlighter.ClassType), ColorSchemeButton.Caption);
    end;
    LanguageButton.Caption := (Sender as TMenuItem).Caption;
  end
  else
  begin
    Language := (Sender as TMenuItem).Caption;
    if (Language <> FCurrentHighlighter.LanguageName) or FIsEditingDefaults then
    begin
      FIsEditingDefaults := False;
      NewVal := EditorOpts.HighlighterList.FindByName(Language);
      if NewVal >= 0 then
      begin
        CurLanguageID := NewVal;
        SetCurrentScheme(EditorOpts.HighlighterList[CurLanguageID].SynClass,
                        GetColorSchemeForLang(EditorOpts.HighlighterList
                                     [CurLanguageID].SynClass.GetLanguageName));
        SetColorSchemeItem(GetColorSchemeForLang(FCurrentHighlighter.LanguageName));
        SetComboBoxText(FileExtensionsComboBox,
          GetCurFileExtensions(FCurrentHighlighter.LanguageName),cstFilename);
      end;
    end;
    LanguageButton.Caption := Language;
  end;
end;

procedure TEditorColorOptionsFrame.ColorSchemeMenuItemClick(Sender: TObject);
var
  Scheme: String;
begin
  Scheme := (Sender as TMenuItem).Caption;
  if Scheme <> FCurrentColorScheme.Name then
  begin
    // change the colorscheme
    if not FIsEditingDefaults then
      SetColorSchemeForLang(FCurrentHighlighter.LanguageName, Scheme);
    SetCurrentScheme(TCustomSynClass(FCurrentHighlighter.ClassType), Scheme);
  end;
  ColorSchemeButton.Caption := Scheme;
end;

procedure TEditorColorOptionsFrame.SetLanguageItem(ALanguage: String);
var
  i: Integer;
begin
  for i := 0 to LanguageMenu.Items.Count - 1 do
    LanguageMenu.Items[i].Checked := LanguageMenu.Items[i].Caption = ALanguage;
  LanguageButton.Caption := ALanguage;
end;

procedure TEditorColorOptionsFrame.SetColorSchemeItem(AScheme: String);
var
  i: Integer;
begin
  for i := 0 to ColorSchemeMenu.Items.Count - 1 do
    ColorSchemeMenu.Items[i].Checked := ColorSchemeMenu.Items[i].Caption = AScheme;
  ColorSchemeButton.Caption := AScheme;
end;

constructor TEditorColorOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTempColorSchemeSettings := TColorSchemeFactory.Create;
end;

destructor TEditorColorOptionsFrame.Destroy;
begin
  FreeAndNil(FTempColorSchemeSettings);
  FFileExtensions.Free;
  FColorSchemes.Free;
  inherited Destroy;
end;

function TEditorColorOptionsFrame.GetTitle: String;
begin
  Result := dlgColors;
end;

procedure TEditorColorOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Items: TStringList;
  Item: TMenuItem;
  I: Integer;
begin
  // Prevent the caret from moving
  ColorPreview.RegisterMouseActionSearchHandler(@DoSynEditMouse);
  FDialog := ADialog;
  UpdatingColor := False;
  FCurHighlightElement := nil;
  ToolBar.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;

  Items := TStringList.Create;
  try
    Items.Sorted := True;
    ColorSchemeFactory.GetRegisteredSchemes(Items);
    for I := 0 to Items.Count - 1 do
    begin
      Item := NewItem(Items[I], 0, I = 0, True, @ColorSchemeMenuItemClick, 0, '');
      Item.RadioItem := True;
      Item.AutoCheck := True;
      Item.GroupIndex := 1;
      ColorSchemeMenu.Items.Add(Item);
    end;
  finally
    Items.Free;
  end;

  ColumnPosBevel.Height := 1;

  UseSyntaxHighlightCheckBox.ImageIndex := IDEImages.LoadImage(16, 'laz_highlighter');
  UseSyntaxHighlightCheckBox.Hint := dlgUseSyntaxHighlight;
  LanguageButton.Hint := dlgLang;
  ColorSchemeButton.Hint := dlgClrScheme;
  FileExtensionsComboBox.hint := dlgFileExts;
  SetAttributeToDefaultButton.ImageIndex := IDEImages.LoadImage(16, 'laz_set_color_default');
  SetAttributeToDefaultButton.Hint := dlgSetElementDefault;
  SetAllAttributesToDefaultButton.ImageIndex := IDEImages.LoadImage(16, 'laz_set_colors_default');
  SetAllAttributesToDefaultButton.Hint := dlgSetAllElementDefault;
  btnExport.ImageIndex := IDEImages.LoadImage(16, 'laz_save');
  btnExport.Hint := dlgColorExportButton;

  tbtnGlobal.Caption := dlgUseSchemeDefaults;
  tbtnLocal.Caption := dlgUseSchemeLocal;

  ForeGroundLabel.Caption := dlgForecolor;
  BackGroundLabel.Caption := dlgBackColor;
  ForeGroundUseDefaultCheckBox.Caption := dlgForecolor;
  BackGroundUseDefaultCheckBox.Caption := dlgBackColor;
  FrameColorUseDefaultCheckBox.Caption := dlgFrameColor;
  bvlAttributeSection.Caption := dlgElementAttributes;

  TextBoldCheckBox.Caption := dlgEdBold;
  TextBoldRadioOn.Caption := dlgEdOn;
  TextBoldRadioOff.Caption := dlgEdOff;
  TextBoldRadioInvert.Caption := dlgEdInvert;

  TextItalicCheckBox.Caption := dlgEdItal;
  TextItalicRadioOn.Caption := dlgEdOn;
  TextItalicRadioOff.Caption := dlgEdOff;
  TextItalicRadioInvert.Caption := dlgEdInvert;

  TextUnderlineCheckBox.Caption := dlgEdUnder;
  TextUnderlineRadioOn.Caption := dlgEdOn;
  TextUnderlineRadioOff.Caption := dlgEdOff;
  TextUnderlineRadioInvert.Caption := dlgEdInvert;

  with GeneralPage do
    AddPreviewEdit(ColorPreview);
end;

procedure TEditorColorOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
  Item: TMenuItem;
begin
  // here we are sure that Setup has been called for every frame =>
  // we can assign events to every registered preview control

  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
    begin
      PreviewEdits[i].OnStatusChange := @OnStatusChange;
      PreviewEdits[i].OnSpecialLineMarkup := @OnSpecialLineMarkup;
    end;

  with AOptions as TEditorOptions do
  begin
    FTempColorSchemeSettings.Assign(UserColorSchemeGroup);
    UseSyntaxHighlightCheckBox.Down := UseSyntaxHighlight;

    Item := NewItem('- '+dlgEditSchemDefaults+' -', 0, False, True, @LanguageMenuItemClick, 0, '');
    Item.RadioItem := True;
    Item.AutoCheck := True;
    Item.GroupIndex := 1;
    LanguageMenu.Items.Add(Item);
    for i := 0 to EditorOpts.HighlighterList.Count - 1 do
    begin
      Item := NewItem(HighlighterList[i].SynClass.GetLanguageName, 0, False, True, @LanguageMenuItemClick, 0, '');
      Item.RadioItem := True;
      Item.AutoCheck := True;
      Item.GroupIndex := 1;
      LanguageMenu.Items.Add(Item);
    end;

    with FileExtensionsComboBox, GeneralPage do
      if CurLanguageID >= 0 then
        SetComboBoxText(FileExtensionsComboBox,
          HighlighterList[CurLanguageID].FileExtensions,cstFilename);

    SetCurrentScheme(TPreviewPasSyn, GetColorSchemeForLang(TPreviewPasSyn.GetLanguageName));
    CurLanguageID := HighlighterList.FindByClass(TCustomSynClass(FCurrentHighlighter.ClassType));
    SetLanguageItem(FCurrentHighlighter.LanguageName);
    SetColorSchemeItem(GetColorSchemeForLang(FCurrentHighlighter.LanguageName));

    ShowCurAttribute;
    UpdateCurrentScheme;
  end;
end;

procedure TEditorColorOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i, j: Integer;
begin
  with AOptions as TEditorOptions do
  begin
    UseSyntaxHighlight := UseSyntaxHighlightCheckBox.Down;

    if FFileExtensions <> nil then begin
      for i := 0 to FFileExtensions.Count - 1 do begin
        j := HighlighterList.FindByName(FFileExtensions.Names[i]);
        if j >= 0 then
          HighlighterList[j].FileExtensions := FFileExtensions.ValueFromIndex[i];
      end;
    end;

    if FColorSchemes <> nil then
      for i := 0 to FColorSchemes.Count - 1 do
         WriteColorScheme(FColorSchemes.Names[i],
                          FColorSchemes.Values[FColorSchemes.Names[i]]);

    // Write from userFactory
    UserColorSchemeGroup.Assign(FTempColorSchemeSettings);
  end;
end;

procedure TEditorColorOptionsFrame.SelectAhaColor(aha: TAdditionalHilightAttribute);
var
  i: Integer;
begin
  for i := 0 to ColorElementTree.Items.Count - 1 do begin
    if ColorElementTree.Items[i].Data = nil then continue;
    if TColorSchemeAttribute(ColorElementTree.Items[i].Data).StoredName <>
       GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha))
    then
      continue;
    ColorElementTree.Items[i].Selected := True;
    break;
  end;
end;

class function TEditorColorOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

procedure TEditorColorOptionsFrame.ComboBoxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorColorOptionsFrame.ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorColorOptionsFrame.tglGlobalChange(Sender: TObject);
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    exit;

  if (FCurHighlightElement.GetSchemeGlobal <> nil) then begin
    FCurHighlightElement.UseSchemeGlobals := tbtnGlobal.Down;
    ShowCurAttribute;
    UpdateCurrentScheme;
  end;
end;

function TEditorColorOptionsFrame.GetCurFileExtensions(const LanguageName: String): String;
var
  i: Integer;
begin
  if FFileExtensions = nil then
    Result := ''
  else
    Result := FFileExtensions.Values[LanguageName];
  if Result = '' then
  begin
    i := EditorOpts.HighlighterList.FindByName(LanguageName);
    if i >= 0 then
      Result := EditorOpts.HighlighterList[i].FileExtensions;
  end;
end;

procedure TEditorColorOptionsFrame.SetCurFileExtensions(const LanguageName, FileExtensions: String);
begin
  if FFileExtensions = nil then
    FFileExtensions := TStringList.Create;
  FFileExtensions.Values[LanguageName] := FileExtensions;
end;

procedure TEditorColorOptionsFrame.OnSpecialLineMarkup(Sender: TObject;
  Line: Integer; var Special: boolean; aMarkup: TSynSelectedColor);
var
  e: TColorSchemeAttribute;
  AddAttr: TAdditionalHilightAttribute;
begin
  if CurLanguageID < 0 then
    exit;
  AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
  if (AddAttr <> ahaNone) and (AddAttr <> ahaFoldedCode) then begin
    e := FCurrentColorScheme.AttributeByEnum[AddAttr];
    if e <> nil then begin
      Special := True;
      e.ApplyTo(aMarkup);
    end;
  end;
end;

procedure TEditorColorOptionsFrame.OnStatusChange(Sender : TObject; Changes : TSynStatusChanges);
var
  Syn: TSynEdit;
  p: TPoint;
begin
  p := EditorOpts.HighlighterList[CurLanguageID].CaretXY;
  Syn := Sender as TSynEdit;
  if p.y > Syn.Lines.Count then exit;
  if (Syn.CaretX = p.x) and (Syn.Carety = p.y) then exit;
  Syn.CaretXY:= p;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorColorOptionsFrame, EdtOptionsColors, EdtOptionsDisplay);
end.
