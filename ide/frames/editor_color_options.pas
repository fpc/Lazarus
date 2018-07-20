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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit editor_color_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, math, types, typinfo, sysutils,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils, LazUTF8,
  // LCL
  LCLProc, LCLType, LCLIntf, StdCtrls, ExtCtrls, Graphics, ComCtrls, Dialogs, Menus,
  // LazControls
  DividerBevel,
  // SynEdit
  SynEdit, SynEditMiscClasses, SynGutterCodeFolding, SynGutterLineNumber,
  SynEditTypes, SynGutterChanges, SynEditMouseCmds, SynEditHighlighter,
  SynColorAttribEditor,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, IDEUtils,
  // IDE
  EditorOptions, editor_general_options, LazarusIDEStrConsts, IDEProcs, LazConf,
  SourceMarks;

type

  // for priority
  TMarkupField = (mfForeGround, mfBackGround, mfFrame, mfStyle, mfUnknown);

  { TEditorColorOptionsFrame }

  TEditorColorOptionsFrame = class(TAbstractIDEOptionsEditor)
    StylePriorEdit: TEdit;
    StylePriorLabel: TLabel;
    BackPriorList: TTreeView;
    StylePriorList: TTreeView;
    StylePriorPanel: TPanel;
    StylePriorUpDown: TUpDown;
    BackPriorValPanel: TPanel;
    StylePriorValPanel: TPanel;
    bvlAttributeSection: TDividerBevel;
    BackPriorEdit: TEdit;
    FramePriorEdit: TEdit;
    ForePriorEdit: TEdit;
    FileExtensionsComboBox: TComboBox;
    ExportSaveDialog: TSaveDialog;
    ForePriorLabel: TLabel;
    BackPriorLabel: TLabel;
    ForePriorList: TTreeView;
    ForePriorValPanel: TPanel;
    FramePriorLabel: TLabel;
    ForePriorPanel: TPanel;
    BackPriorPanel: TPanel;
    FramePriorList: TTreeView;
    FramePriorPanel: TPanel;
    FramePriorValPanel: TPanel;
    PriorityEditor: TPanel;
    PnlTop2: TPanel;
    pnlTop: TPanel;
    LanguageMenu: TPopupMenu;
    ColorSchemeMenu: TPopupMenu;
    Splitter1: TSplitter;
    ColorElementTree: TTreeView;
    SynColorAttrEditor1: TSynColorAttrEditor;
    ToolBar: TToolBar;
    ToolBar1: TToolBar;
    tbtnGlobal: TToolButton;
    tbtnLocal: TToolButton;
    ToolButton1: TToolButton;
    ToolButton3: TToolButton;
    tbnColor: TToolButton;
    tbnPrior: TToolButton;
    ForePriorUpDown: TUpDown;
    BackPriorUpDown: TUpDown;
    FramePriorUpDown: TUpDown;
    UseSyntaxHighlightCheckBox: TToolButton;
    ToolButton2: TToolButton;
    LanguageButton: TToolButton;
    ColorSchemeButton: TToolButton;
    ToolButton5: TToolButton;
    btnExport: TToolButton;
    SetAllAttributesToDefaultButton: TToolButton;
    SetAttributeToDefaultButton: TToolButton;
    ColorPreview: TSynEdit;
    pnlElementAttributes: TPanel;
    procedure btnExportClick(Sender: TObject);
    procedure ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var {%H-}PaintImages, DefaultDraw: Boolean);
    procedure ColorElementTreeChange(Sender: TObject; {%H-}Node: TTreeNode);
    procedure ColorElementTreeClick(Sender: TObject);
    procedure ColorElementTreeKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ColorPreviewMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure ColorSchemeButtonClick(Sender: TObject);
    procedure DoColorChanged(Sender: TObject);
    procedure ForePriorEditKeyPress(Sender: TObject; var Key: char);
    procedure ForePriorListClick(Sender: TObject);
    procedure ForePriorListCompare(Sender: TObject; Node1, Node2: TTreeNode;
      var Compare: Integer);
    procedure ForePriorEditChange(Sender: TObject);
    procedure ForePriorUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tbnColorClick(Sender: TObject);
    procedure tglGlobalChange(Sender: TObject);
  private
    FTempColorSchemeSettings: TColorSchemeFactory;

    FDialog: TAbstractOptionsEditorDialog;
    FCurHighlightElement: TColorSchemeAttribute;

    FFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    FColorSchemes: TStringList;    // list of LanguageName=ColorScheme

    FCurrentHighlighter: TSrcIDEHighlighter;
    FCurrentColorScheme: TColorSchemeLanguage;
    FIsEditingDefaults, FInPriorUpdating: Boolean;
    CurLanguageID: Integer;

    procedure FillPriorEditor;
    procedure SelectCurInPriorEditor;

    function AttrForNode(ANode: TTreeNode): TColorSchemeAttribute;
    function  PriorSenderToField(ASender: TObject): TMarkupField;
    function  PriorEditForField(AField: TMarkupField): TEdit;
    function  PriorListForField(AField: TMarkupField): TTreeView;
    procedure SetAttrPriorVal(AnAttr: TColorSchemeAttribute; AField: TMarkupField; AValue: Integer);
    function  GetAttrPriorVal(AnAttr: TColorSchemeAttribute; AField: TMarkupField): Integer;
    procedure SetPriorEditVal(AnEdit: TEdit; AValue: Integer);
    function  GetPriorEditVal(AnEdit: TEdit): Integer;

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

    procedure OnStatusChange(Sender: TObject; {%H-}Changes: TSynStatusChanges);
    procedure OnSpecialLineMarkup(Sender: TObject; Line: Integer;
      var Special: boolean; aMarkup: TSynSelectedColor);

    function GeneralPage: TEditorGeneralOptionsFrame;
    function DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
                         {%H-}HandleActionProc: TSynEditMouseActionHandler): Boolean;
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
  MAX_PRIOR = 9999;
  MIN_PRIOR = 0;

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
  TheTree: TCustomTreeView;
begin
  DefaultDraw := (node.Data = nil) or not (stage=cdPostPaint);
  if DefaultDraw  then exit;

  TheTree := TCustomTreeView(Sender);

  Attri := TColorSchemeAttribute(node.Data);
  if Attri.IsUsingSchemeGlobals then
    Attri := Attri.GetSchemeGlobal;

  if FCurrentColorScheme = nil then exit;

  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), Attri.StoredName);

  // Draw node background and name
  if cdsSelected in State then begin
    TheTree.Canvas.Brush.Color := TheTree.SelectionColor;
    TheTree.Canvas.Font.Color := InvertColor(TheTree.SelectionColor);
  end else begin
    TheTree.Canvas.Brush.Color := TheTree.BackgroundColor;
    TheTree.Canvas.Font.Color := Font.Color;
  end;
  NodeRect := Node.DisplayRect(true);
  FullAbcWidth := TheTree.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := (NodeRect.Top + NodeRect.Bottom - TheTree.Canvas.TextHeight(Node.Text)) div 2;
  TheTree.Canvas.FillRect(NodeRect);
  TheTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, copy(Node.Text, 1+length(COLOR_NODE_PREFIX), MaxInt)); // Attri.Name);

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
  TheTree.Canvas.Brush.Color := c;
  TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Special draw Modified line gutter
  if AttriIdx = ord(ahaModifiedLine) then begin
    TextY := NodeRect.Bottom - NodeRect.Top - 4;
    TheTree.Canvas.Brush.Color := Attri.Foreground;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+5, NodeRect.Bottom-2);
    TheTree.Canvas.Brush.Color := Attri.FrameColor;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2+ (TextY div 2), NodeRect.Left+5, NodeRect.Bottom-2);
    exit;
  end;

  // Special draw oultine color
  if Attri.Group = agnOutlineColors then begin
    c := Attri.MarkupFoldLineColor;
    if c <> clNone then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+2, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+2, NodeRect.Bottom-2);
      TheTree.Canvas.MoveTo(NodeRect.Left+2+1, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+2+1, NodeRect.Bottom-2);
    end;
    NodeRect.Left := NodeRect.Left + 6;
    FullAbcWidth := FullAbcWidth - 6;
    TheTree.Canvas.Brush.Color := Attri.Background;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);
//    exit;
  end;

  // Draw preview Frame
  TheTree.Canvas.Pen.Color := Attri.FrameColor;
  if (hafFrameColor in Attri.Features) and (AttriIdx <> ord(ahaCodeFoldingTree)) and
     (Attri.FrameColor <> clDefault) and (Attri.FrameColor <> clNone)
  then
    TheTree.Canvas.Rectangle(NodeRect.Left+2, NodeRect.Top+2,
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
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4,
                                        NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);

      // [+]
      inc(NodeRect.Left, TextY+2);
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);
      if (Attri.FrameColor <> clNone) and (Attri.FrameColor <> clDefault) then
        TheTree.Canvas.Pen.Color := Attri.FrameColor;
      TheTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4,
                                        NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Top+6);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-6);
      TheTree.Canvas.Brush.Style := bsSolid;
    end
    else if AttriIdx = ord(ahaGutterSeparator) then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+6, NodeRect.Bottom-2);
    end
    else if AttriIdx = ord(ahaRightMargin) then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Bottom-2);
    end
    else begin
      s := 'abc';
      if AttriIdx = ord(ahaFoldedCode) then
        s:= '...';
      if (AttriIdx = ord(ahaFoldedCodeLine)) or (AttriIdx = ord(ahaHiddenCodeLine)) then
        s:= '---';
      if AttriIdx = ord(ahaLineNumber) then
        s:= '123';
      if Attri.Group = agnOutlineColors then
        s:= 'ab';
      TheTree.Canvas.Font.Color := c;
      TheTree.Canvas.Font.Style := Attri.Style;
      TheTree.Canvas.Font.Height := -(NodeRect.Bottom - NodeRect.Top - 7);
      TextY := (NodeRect.Top + NodeRect.Bottom - canvas.TextHeight(s)) div 2;
      AbcWidth := TheTree.Canvas.TextExtent(s).cx;
      SetBkMode(TheTree.Canvas.Handle, TRANSPARENT);
      TheTree.Canvas.TextOut(NodeRect.Left+(FullAbcWidth - AbcWidth) div 2, TextY, s);
      SetBkMode(TheTree.Canvas.Handle, OPAQUE);

      TheTree.Canvas.Font.Height := Font.Height;
      TheTree.Canvas.Font.Style := [];
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
    l := UTF8CodepointSize(PChar(NewName));
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
  if Attri <> nil then begin
    NewNode := ColorElementTree.Items.GetFirstNode;
    while Assigned(NewNode) do begin
      if (NewNode.Data <> nil)
      and (TColorSchemeAttribute(NewNode.Data).StoredName = Attri.StoredName) then
        break;
      NewNode := NewNode.GetNext;
    end;
  end;
  if NewNode <> nil then begin
    NewNode.Selected := True;
    FindCurHighlightElement;
  end;
end;

procedure TEditorColorOptionsFrame.ColorSchemeButtonClick(Sender: TObject);
begin
  ColorSchemeButton.CheckMenuDropdown;
end;

procedure TEditorColorOptionsFrame.DoColorChanged(Sender: TObject);
begin
  UpdateCurrentScheme;
end;

procedure TEditorColorOptionsFrame.ForePriorEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8 ,#9, #10, #13]) then Key := #0;
  if key = #13 then ForePriorEditChange(Sender);
end;

procedure TEditorColorOptionsFrame.ForePriorListClick(Sender: TObject);
var
  Node, N2: TTreeNode;
begin
  Node := TTreeView(Sender).Selected;
  if Node = nil then
    exit;
  FCurHighlightElement := TColorSchemeAttribute(Node.Data);

  N2 := ColorElementTree.Items.FindNodeWithData(FCurHighlightElement);
  if N2 <> nil then
    N2.Selected := True;

  //i := (Node.Index - TTreeView(Sender).TopItem.Index);
  //if (i < 0) or (i >= TTreeView(Sender).Height div Node.Height) then
  //  i := Max(0, (TTreeView(Sender).Height div Node.Height div 2) - 1);

  FInPriorUpdating := True;
  ShowCurAttribute;
  SelectCurInPriorEditor;
  FInPriorUpdating := False;

  //TTreeView(Sender).TopItem := TTreeView(Sender).Items[Max(0, Node.Index - i)];
end;

procedure TEditorColorOptionsFrame.ForePriorListCompare(Sender: TObject; Node1,
  Node2: TTreeNode; var Compare: Integer);
var
  a1, a2: TColorSchemeAttribute;
  p1, p2: Integer;
  f: TMarkupField;
begin
  a1 := TColorSchemeAttribute(Node1.Data);
  a2 := TColorSchemeAttribute(Node2.Data);
  f := PriorSenderToField(Sender);
  p1 := GetAttrPriorVal(a1, f);
  p2 := GetAttrPriorVal(a2, f);

  Compare := p2 - p1;
  if (Compare = 0) and (FCurrentColorScheme <> nil) then
    Compare := FCurrentColorScheme.IndexOfAttr(a1) - FCurrentColorScheme.IndexOfAttr(a2);
end;

procedure TEditorColorOptionsFrame.ForePriorEditChange(Sender: TObject);
var
  List: TTreeView;
  Node: TTreeNode;
  Attr: TColorSchemeAttribute;
  i: Integer;
  f: TMarkupField;
begin
  if FInPriorUpdating then
    exit;

  f := PriorSenderToField(Sender);
  List := PriorListForField(f);

  if List = nil then
    exit;

  Node := List.Selected;
  Attr := AttrForNode(Node);
  if Attr = nil then
    exit;

  i := GetPriorEditVal(Sender as TEdit);
  if i < MIN_PRIOR    then begin
    i := MIN_PRIOR;
    SetPriorEditVal(Sender as TEdit, i);
  end;
  if i > MAX_PRIOR then begin
    i := MAX_PRIOR;
    SetPriorEditVal(Sender as TEdit, i);
  end;

  SetAttrPriorVal(Attr, f, i);
  Node.Text := Format('%s%-3d %s', [COLOR_NODE_PREFIX, i, Attr.Caption^]);

  i := (Node.Index - List.TopItem.Index);
  if (i < 0) or (i >= List.Height div Node.Height) then
    i := Max(0, (List.Height div Node.Height div 2) - 1);
  List.AlphaSort;
  // Update SynColorAttrEditor1;
  DoColorChanged(nil);
  List.TopItem := List.Items[Max(0, Node.Index - i)];
end;

procedure TEditorColorOptionsFrame.ForePriorUpDownClick(Sender: TObject; Button: TUDBtnType);
  procedure ShiftUpNodes(ANode: TTreeNode; AField: TMarkupField);
  var
    Attr: TColorSchemeAttribute;
    Prior, NextPrior: Integer;
  begin
    Attr := AttrForNode(ANode);
    if Attr = nil then exit;
    NextPrior := GetAttrPriorVal(Attr, AField);
    Prior := NextPrior;
    while Prior >= NextPrior do begin
      Prior := NextPrior + 1;
      if Prior > MAX_PRIOR then exit;
      SetAttrPriorVal(Attr, AField, Prior);
      ANode.Text := Format('%s%-3d %s', [COLOR_NODE_PREFIX, Prior, Attr.Caption^]);

      ANode := ANode.GetPrev;
      Attr := AttrForNode(ANode);
      if Attr = nil then exit;
      NextPrior := GetAttrPriorVal(Attr, AField);
    end;
  end;

  procedure ShiftDownNodes(ANode: TTreeNode; AField: TMarkupField);
  var
    Attr: TColorSchemeAttribute;
    Prior, NextPrior: Integer;
  begin
    Attr := AttrForNode(ANode);
    if Attr = nil then exit;
    NextPrior := GetAttrPriorVal(Attr, AField);
    Prior := NextPrior;
    while Prior <= NextPrior do begin
      Prior := NextPrior - 1;
      if Prior < MIN_PRIOR then exit;
      SetAttrPriorVal(Attr, AField, Prior);
      ANode.Text := Format('%s%-3d %s', [COLOR_NODE_PREFIX, Prior, Attr.Caption^]);

      ANode := ANode.GetNext;
      Attr := AttrForNode(ANode);
      if Attr = nil then exit;
      NextPrior := GetAttrPriorVal(Attr, AField);
    end;
  end;

  function CanShiftUp(ANode: TTreeNode; AField: TMarkupField): Boolean;
  var
    Attr: TColorSchemeAttribute;
  begin
    Result := False;
    Attr := AttrForNode(ANode);
    if Attr = nil then
      exit;
    Result := GetAttrPriorVal(Attr, AField) + ANode.Index < MAX_PRIOR
  end;

  function CanShiftDown(ANode: TTreeNode; AField: TMarkupField): Boolean;
  var
    Attr: TColorSchemeAttribute;
  begin
    Result := False;
    Attr := AttrForNode(ANode);
    if Attr = nil then
      exit;
    Result := GetAttrPriorVal(Attr, AField) - (ANode.TreeView.Items.Count - ANode.Index - 1) > MIN_PRIOR;
  end;

var
  TheEdit: TEdit;
  List: TTreeView;
  Node, Node2, Node3: TTreeNode;
  CurPrior, NewPrior, d, i, j: Integer;
  Attr, Attr2, Attr3, Attr3s: TColorSchemeAttribute;
  f: TMarkupField;
begin
  f := PriorSenderToField(Sender);
  List := PriorListForField(f);
  TheEdit := PriorEditForField(f);
  if List = nil then
    exit;

  CurPrior := GetPriorEditVal(TheEdit);
  //if CurPrior = 0 then
  //  exit;

  case Button of
    btNext: d := 1;
    btPrev: d := -1;
  end;

  NewPrior := CurPrior + d;
  try

    Node := List.Selected;
    Attr := AttrForNode(Node);
    if Attr = nil then
      exit;

    case Button of
      btNext: Node2 := Node.GetPrev;
      btPrev: Node2 := Node.GetNext;
    end;

    Attr2 := AttrForNode(Node2);
    if Attr2 = nil then
      exit;
    NewPrior := GetAttrPriorVal(Attr2, f) + d;
    Node.Index := Node.Index - d; // must be possible, or node2 would be nil

    if NewPrior < MIN_PRIOR then begin // btPrev;
      NewPrior := MIN_PRIOR;
      if CanShiftUp(Node2, f) then
        ShiftUpNodes(Node2, f);
      exit;
    end;
    if NewPrior > MAX_PRIOR then begin // btNext;
      NewPrior := MAX_PRIOR;
      if CanShiftDown(Node2, f) then
        ShiftDownNodes(Node2, f);
      exit;
    end;

    case Button of
      btNext: Node3 := Node.GetPrev; // Since Node was shifted, NOde2 is now Node.GetNext
      btPrev: Node3 := Node.GetNext; // Since Node was shifted, NOde2 is now Node.GetPrev
    end;

    Attr3 := AttrForNode(Node3);
    if Attr3 = nil then
      exit;
    i := GetAttrPriorVal(Attr3, f);
    if NewPrior <> i then
      exit;

    Attr3s := Attr3.GetStoredValuesForAttrib;
    j := -1;
    if (Attr3s<> nil) then
      j := GetAttrPriorVal(Attr3s, f);
    case Button of
      btNext: begin
          if ((j = i) or not CanShiftUp(Node3, f)) and CanShiftDown(Node2, f)
          then begin
            NewPrior := NewPrior - d;
            ShiftDownNodes(Node2, f);
          end
          else
          if CanShiftUp(Node3, f) then
            ShiftUpNodes(Node3, f);
        end;
      btPrev: begin
          if ((j = i) or not CanShiftDown(Node3, f)) and CanShiftUp(Node2, f)
          then begin
            NewPrior := NewPrior - d;
            ShiftUpNodes(Node2, f);
          end
          else
          if CanShiftDown(Node3, f) then
            ShiftDownNodes(Node3, f);
        end;
    end;


  finally
    if NewPrior < MIN_PRIOR then NewPrior := MIN_PRIOR;
    if NewPrior > MAX_PRIOR then NewPrior := MAX_PRIOR;
    SetPriorEditVal(TheEdit, NewPrior);
    ForePriorEditChange(TheEdit);
  end;
end;

procedure TEditorColorOptionsFrame.GeneralCheckBoxOnChange(Sender: TObject);
begin
  if Sender = UseSyntaxHighlightCheckBox then
  begin
    ApplyCurrentScheme;
    Exit;
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

procedure TEditorColorOptionsFrame.ShowCurAttribute;
var
  CanGlobal: Boolean;
begin
  if (FCurHighlightElement = nil) then
    Exit;
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

    if FCurHighlightElement.IsUsingSchemeGlobals then
      SynColorAttrEditor1.CurHighlightElement := FCurHighlightElement.GetSchemeGlobal
    else
      SynColorAttrEditor1.CurHighlightElement := FCurHighlightElement;
    //SynColorAttrEditor1.UpdateAll;
    FillPriorEditor;
  finally
    EnableAlign;
  end;
end;

procedure TEditorColorOptionsFrame.FillPriorEditor;

  function IsEnabled(AnAttr: TColorSchemeAttribute; AnSelector: TMarkupField): Boolean;
  begin
    Result := False;
    if AnAttr.IsUsingSchemeGlobals then
      AnAttr := AnAttr.GetSchemeGlobal;
    //if AnAttr.StoredName = 'ahaDefault' then
    //  exit;
    if not (hafPrior in AnAttr.Features) then
      exit;
    case AnSelector of
        mfForeGround: Result := (hafForeColor in AnAttr.Features) and
                     (AnAttr.Foreground <> clNone) and
                     (AnAttr.Foreground <> clDefault);
        mfBackGround: Result := (hafBackColor in AnAttr.Features) and
                     (AnAttr.Background <> clNone) and
                     (AnAttr.Background <> clDefault);
        mfFrame:     Result := (hafFrameColor in AnAttr.Features) and
                     (AnAttr.FrameColor <> clNone) and
                     (AnAttr.FrameColor <> clDefault);
        mfStyle:     Result := (hafStyle in AnAttr.Features) and
                     ( (AnAttr.Style <> []) or (AnAttr.StyleMask <> []) );
    end
  end;

  procedure FillList(AList: TTreeView; ASelector: TMarkupField);
  var
    i, p: Integer;
    Attr: TColorSchemeAttribute;
  begin
    AList.BeginUpdate;
    AList.Items.Clear;
    for i := 0 to FCurrentColorScheme.AttributeCount - 1 do begin
      Attr := FCurrentColorScheme.AttributeAtPos[i];
      if IsEnabled(Attr, ASelector) then begin
        p := GetAttrPriorVal(Attr, ASelector);
        AList.Items.Add(nil, Format('%s%-3d %s', [COLOR_NODE_PREFIX, p, Attr.Caption^])).Data := Attr;
      end;
    end;
    AList.EndUpdate;
    AList.AlphaSort;
  end;
begin
  if (not PriorityEditor.Visible) or (FCurHighlightElement = nil) or FInPriorUpdating
  then
    exit;

  FInPriorUpdating := True;

  FillList(ForePriorList, mfForeGround);
  FillList(BackPriorList, mfBackGround);
  FillList(FramePriorList, mfFrame);
  FillList(StylePriorList, mfStyle);

  SelectCurInPriorEditor;

  FInPriorUpdating := False;
end;

procedure TEditorColorOptionsFrame.SelectCurInPriorEditor;
var
  n: TTreeNode;
  i: Integer;
begin
  n := ForePriorList.Items.FindNodeWithData(FCurHighlightElement);
  ForePriorValPanel.Enabled := n <> nil;
  if (n <> nil) and not(n.Selected) then begin
    n.Selected := True;
    i := Max(0, n.Index - Max(0, (ForePriorList.Height div n.Height div 2) -1 ));
    ForePriorList.TopItem := ForePriorList.Items[i];
  end
  else
  if n = nil then
    ForePriorList.Selected := nil;
  SetPriorEditVal(ForePriorEdit, GetAttrPriorVal(FCurHighlightElement, mfForeGround));

  n := BackPriorList.Items.FindNodeWithData(FCurHighlightElement);
  BackPriorValPanel.Enabled := n <> nil;
  if (n <> nil) and not(n.Selected) then begin
    n.Selected := True;
    i := Max(0, n.Index - Max(0, (BackPriorList.Height div n.Height div 2) - 1));
    BackPriorList.TopItem := BackPriorList.Items[i];
  end
  else
  if n = nil then
    BackPriorList.Selected := nil;
  SetPriorEditVal(BackPriorEdit, GetAttrPriorVal(FCurHighlightElement, mfBackGround));

  n := FramePriorList.Items.FindNodeWithData(FCurHighlightElement);
  FramePriorValPanel.Enabled := n <> nil;
  if (n <> nil) and not(n.Selected) then begin
    n.Selected := True;
    i := Max(0, n.Index - Max(0, (FramePriorList.Height div n.Height div 2) - 1));
    FramePriorList.TopItem := FramePriorList.Items[i];
  end
  else
  if n = nil then
    FramePriorList.Selected := nil;
  SetPriorEditVal(FramePriorEdit, GetAttrPriorVal(FCurHighlightElement, mfFrame));

  n := StylePriorList.Items.FindNodeWithData(FCurHighlightElement);
  StylePriorValPanel.Enabled := n <> nil;
  if (n <> nil) and not(n.Selected) then begin
    n.Selected := True;
    i := Max(0, n.Index - Max(0, (StylePriorList.Height div n.Height div 2) - 1));
    StylePriorList.TopItem := StylePriorList.Items[i];
  end
  else
  if n = nil then
    StylePriorList.Selected := nil;
  SetPriorEditVal(StylePriorEdit, GetAttrPriorVal(FCurHighlightElement, mfStyle));
end;

function TEditorColorOptionsFrame.AttrForNode(ANode: TTreeNode): TColorSchemeAttribute;
begin
  Result := nil;
  if ANode = nil then exit;
  Result := TColorSchemeAttribute(ANode.Data);
  if (Result <> nil) and Result.IsUsingSchemeGlobals then
    Result := Result.GetSchemeGlobal;
end;

function TEditorColorOptionsFrame.PriorSenderToField(ASender: TObject): TMarkupField;
begin
  If ASender = ForePriorEdit then
    Result := mfForeGround
  else
  If ASender = BackPriorEdit then
    Result := mfBackGround
  else
  If ASender = FramePriorEdit then
    Result := mfFrame
  else
  If ASender = StylePriorEdit then
    Result := mfStyle
  else
  if ASender = ForePriorUpDown then
    Result := mfForeGround
  else
  if ASender = BackPriorUpDown then
    Result := mfBackGround
  else
  if ASender = FramePriorUpDown then
    Result := mfFrame
  else
  if ASender = StylePriorUpDown then
    Result := mfStyle
  else
  if ASender = ForePriorList then
    Result := mfForeGround
  else
  if ASender = BackPriorList then
    Result := mfBackGround
  else
  if ASender = FramePriorList then
    Result := mfFrame
  else
  if ASender = StylePriorList then
    Result := mfStyle
  else
    Result := mfUnknown;
end;

function TEditorColorOptionsFrame.PriorEditForField(AField: TMarkupField): TEdit;
begin
  Result := nil;
  case AField of
    mfForeGround: Result := ForePriorEdit;
    mfBackGround: Result := BackPriorEdit;
    mfFrame:      Result := FramePriorEdit;
    mfStyle:      Result := StylePriorEdit;
  end;
end;

function TEditorColorOptionsFrame.PriorListForField(AField: TMarkupField): TTreeView;
begin
  Result := nil;
  case AField of
    mfForeGround: Result := ForePriorList;
    mfBackGround: Result := BackPriorList;
    mfFrame:      Result := FramePriorList;
    mfStyle:      Result := StylePriorList;
  end;
end;

procedure TEditorColorOptionsFrame.SetAttrPriorVal(AnAttr: TColorSchemeAttribute;
  AField: TMarkupField; AValue: Integer);
begin
  if AnAttr.IsUsingSchemeGlobals then
    AnAttr := AnAttr.GetSchemeGlobal;
  case AField of
    mfForeGround: AnAttr.ForePriority := AValue;
    mfBackGround: AnAttr.BackPriority := AValue;
    mfFrame:      AnAttr.FramePriority := AValue;
    mfStyle:      begin
        AnAttr.StylePriority[fsBold] := AValue;
        AnAttr.StylePriority[fsItalic] := AValue;
        AnAttr.StylePriority[fsUnderline] := AValue;
      end;
  end;
end;

function TEditorColorOptionsFrame.GetAttrPriorVal(AnAttr: TColorSchemeAttribute;
  AField: TMarkupField): Integer;
begin
  Result := 0;
  if AnAttr = nil then exit;
  if AnAttr.IsUsingSchemeGlobals then
    AnAttr := AnAttr.GetSchemeGlobal;
  case AField of
    mfForeGround: Result := AnAttr.ForePriority;
    mfBackGround: Result := AnAttr.BackPriority;
    mfFrame:      Result := AnAttr.FramePriority;
    mfStyle:      Result := AnAttr.StylePriority[fsBold];
  end;
end;

procedure TEditorColorOptionsFrame.SetPriorEditVal(AnEdit: TEdit; AValue: Integer);
begin
  AnEdit.Tag := AValue;
  AnEdit.Text := IntToStr(AValue);
end;

function TEditorColorOptionsFrame.GetPriorEditVal(AnEdit: TEdit): Integer;
begin
  Result := StrToIntDef(AnEdit.Text, -1);
  if Result = -1 then begin
    Result := AnEdit.Tag;
    AnEdit.Text := IntToStr(Result);
  end;
end;

procedure TEditorColorOptionsFrame.FindCurHighlightElement;
begin
  if (ColorElementTree.Selected <> nil) and
     (ColorElementTree.Selected.Parent = nil) and
     (ColorElementTree.Selected.GetFirstChild <> nil)
  then
    ColorElementTree.Selected.GetFirstChild.Selected := True;
  if (ColorElementTree.Selected = nil) or (ColorElementTree.Selected.Data = nil) then
    exit;

  if FCurHighlightElement = TColorSchemeAttribute(ColorElementTree.Selected.Data) then
    exit;

  FCurHighlightElement := TColorSchemeAttribute(ColorElementTree.Selected.Data);
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
  DefNode := nil;
  for i := 0 to FCurrentColorScheme.AttributeCount - 1 do begin
    Attr := FCurrentColorScheme.AttributeAtPos[i];
    if Attr.StoredName <> '' then begin
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
      NewNode :=  ColorElementTree.Items.AddChild(ParentNode, COLOR_NODE_PREFIX + Attr.Caption^);
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
  if not FIsEditingDefaults then begin
    FCurrentHighlighter := FCurrentColorScheme.Highlighter;
    SynColorAttrEditor1.CurrentColorScheme := FCurrentColorScheme;
    FillPriorEditor;
  end;
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
        PreviewEdits[a].MouseOptions := [emUseMouseActions];
        PreviewEdits[a].MouseActions.Clear;
        PreviewEdits[a].MouseActions.AddCommand(emcWheelVertScrollDown, False, mbXWheelDown, ccAny, cdDown, [], []);
        PreviewEdits[a].MouseActions.AddCommand(emcWheelVertScrollUp,   False, mbXWheelUp,   ccAny, cdDown, [], []);
        PreviewEdits[a].SetBookMark(1, 1, 2);
        PreviewEdits[a].SetBookMark(2, 1, 5);
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

function TEditorColorOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorColorOptionsFrame.DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := not(AnInfo.Button in [mbXWheelDown, mbXWheelUp]);
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

  ExportSaveDialog.Title := lisExport;
  ExportSaveDialog.Filter := Format('%s|*.xml|', [dlgFilterXML]);
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


  UseSyntaxHighlightCheckBox.ImageIndex := IDEImages.LoadImage('laz_highlighter');
  UseSyntaxHighlightCheckBox.Hint := dlgUseSyntaxHighlight;
  LanguageButton.Hint := dlgLang;
  ColorSchemeButton.Hint := dlgClrScheme;
  FileExtensionsComboBox.hint := dlgFileExts;
  SetAttributeToDefaultButton.ImageIndex := IDEImages.LoadImage('laz_set_color_default');
  SetAttributeToDefaultButton.Hint := dlgSetElementDefault;
  SetAllAttributesToDefaultButton.ImageIndex := IDEImages.LoadImage('laz_set_colors_default');
  SetAllAttributesToDefaultButton.Hint := dlgSetAllElementDefault;
  btnExport.ImageIndex := IDEImages.LoadImage('laz_save');
  btnExport.Hint := lisExport;

  tbtnGlobal.Caption := dlgUseSchemeDefaults;
  tbtnLocal.Caption := dlgUseSchemeLocal;

  tbnColor.Caption := dlgColors;
  tbnPrior.Caption := dlgPriorities;

  ForePriorLabel.Caption := dlgForecolor;
  BackPriorLabel.Caption := dlgBackColor;
  FramePriorLabel.Caption := dlgFrameColor;
  StylePriorLabel.Caption := dlgTextStyle;

  bvlAttributeSection.Caption := dlgElementAttributes;
  SynColorAttrEditor1.Setup;
  SynColorAttrEditor1.OnChanged := @DoColorChanged;
  SynColorAttrEditor1.ShowPrior := False;

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

    tbnColorClick(nil);
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

procedure TEditorColorOptionsFrame.tbnColorClick(Sender: TObject);
begin
  PriorityEditor.Visible      := tbnPrior.Down;
  SynColorAttrEditor1.Visible := not tbnPrior.Down;
  FillPriorEditor;
end;

procedure TEditorColorOptionsFrame.tglGlobalChange(Sender: TObject);
begin
  if (FCurHighlightElement = nil) then
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
