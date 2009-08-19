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
  Classes, LResources, StdCtrls, SynEdit, LCLIntf,
  SynGutterCodeFolding, SynGutterLineNumber, SynGutterChanges, SynEditMouseCmds,
  ExtCtrls, Graphics, LCLProc, SynEditMiscClasses, LCLType, Controls,
  EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, editor_general_options,
  IDEProcs, ColorBox, ComCtrls, SynEditHighlighter, typinfo;

type

  { TEditorColorOptionsFrame }

  TEditorColorOptionsFrame = class(TAbstractIDEOptionsEditor)
    BackGroundColorBox: TColorBox;
    FrameColorBox: TColorBox;
    BackGroundLabel: TLabel;
    FrameColorLabel: TLabel;
    BackGroundUseDefaultCheckBox: TCheckBox;
    FrameColorUseDefaultCheckBox: TCheckBox;
    ForegroundColorBox: TColorBox;
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
    UseSyntaxHighlightCheckBox: TCheckBox;
    ColorPreview: TSynEdit;
    ColorSchemeComboBox: TComboBox;
    ColorSchemeLabel: TLabel;
    FileExtensionsComboBox: TComboBox;
    FileExtensionsLabel: TLabel;
    ForeGroundLabel: TLabel;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    LanguageComboBox: TComboBox;
    LanguageLabel: TLabel;
    SetAllAttributesToDefaultButton: TButton;
    SetAttributeToDefaultButton: TButton;
    ElementAttributesGroupBox: TGroupBox;
    procedure ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure ColorElementTreeChange(Sender: TObject; Node: TTreeNode);
    procedure ColorElementTreeClick(Sender: TObject);
    procedure ColorPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDialog: TAbstractOptionsEditorDialog;
    // current index in EditorOpts.EditOptHighlighterList
    CurHighlightElement: TSynHighlightElement;
    CurHighlightElementIsExtra: Boolean;
    CurExtraElement: TAdditionalHilightAttribute;

    UpdatingColor: Boolean;
    FFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    FHighlighterList: TStringList; // list of "ColorScheme" Data=TSrcIDEHighlighter
    FColorSchemes: TStringList;    // list of LanguageName=ColorScheme

    PreviewSyn: TSrcIDEHighlighter;
    CurLanguageID: Integer;

    function GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);
    procedure ShowCurAttribute;
    function  IsAhaElement(aName: String; var aha: TAdditionalHilightAttribute): Boolean;
    procedure FindCurHighlightElement;
    procedure FillColorElementListBox;
    procedure SetColorElementsToDefaults(OnlySelected: Boolean);
    function GetCurColorScheme(const LanguageName: String): String;
    procedure SetCurColorScheme(const LanguageName, ColorScheme: String);
    function GetHighlighter(SynClass: TCustomSynClass;
      const ColorScheme: String; CreateIfNotExists: Boolean): TSrcIDEHighlighter;
    procedure ClearHighlighters;
    procedure InvalidatePreviews;
    procedure SetPreviewSynInAllPreviews;

    procedure OnStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure OnSpecialLineMarkup(Sender: TObject; Line: Integer;
      var Special: boolean; aMarkup: TSynSelectedColor);

    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    function DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
  public
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure SelectAhaColor(aha: TAdditionalHilightAttribute);
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

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
  Attri: TSynHighlighterAttributes;
  TextY: Integer;
  AttriIdx: LongInt;
  c: TColor;
  Scheme: TPascalColorScheme;
  s: String;
begin
  DefaultDraw := (node.Data = nil) or not (stage=cdPostPaint);
  if DefaultDraw  then exit;

  Attri := TSynHighlighterAttributes(node.Data);
  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), Attri.StoredName);
  Scheme := EditorOpts.GetColorScheme(ColorSchemeComboBox.Text);

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
  TextY := (NodeRect.Top + NodeRect.Bottom - canvas.TextHeight(Node.Text)) div 2;
  ColorElementTree.Canvas.FillRect(NodeRect);
  ColorElementTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, Attri.Name);

  // Draw preview box
  c := clNone;
  if (AttriIdx < 0) or (ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].BG) then begin
    if (Attri.Background <> clDefault) and (Attri.Background <> clNone) then
      c := Attri.Background;
  //end
  //else begin // Bg not used; use FG
  //  if (Attri.Foreground <> clDefault) and (Attri.Foreground <> clNone) then
  //    c := Attri.Foreground;
  end;
  // Fallback color for gutter
  if ((c = clNone) or (c = clDefault)) and
     (AttriIdx in [ord(ahaModifiedLine), ord(ahaCodeFoldingTree),
                   ord(ahaLineNumber), ord(ahaGutterSeparator)]) and
     (EditorOpts.GetSynAttributeByAha(PreviewSyn, ahaGutter) <> nil)
  then
    c := EditorOpts.GetSynAttributeByAha(PreviewSyn, ahaGutter).Background;
  // Fallback color for text
  if ((c = clNone) or (c = clDefault)) and (PreviewSyn.WhitespaceAttribute <> nil) then
    c := PreviewSyn.WhitespaceAttribute.Background;
  if (c = clNone) or (c = clDefault) then
    c := Scheme.Default.BG;
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
  if (AttriIdx < 0) or (ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].FF) then begin
    if (Attri.FrameColor <> clDefault) and (Attri.FrameColor <> clNone) then
      ColorElementTree.Canvas.Rectangle(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);
  end;

  // Draw preview ForeGround
  if (AttriIdx < 0) or (ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].FG) //and
       //(ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].BG) )       // if no BG, then FG was used
  then begin
    c := Attri.Foreground;
    if ((c = clDefault) or (c = clNone)) and not (AttriIdx = ord(ahaLineNumber)) then
      c := Scheme.Default.FG;
    if (c = clNone) or (c = clDefault) then
      c := ColorPreview.Font.Color;

    if AttriIdx = ord(ahaCodeFoldingTree) then begin
      // Special draw fold gutter
      TextY := NodeRect.Bottom - NodeRect.Top - 8;
      ColorElementTree.Canvas.Brush.Color := clWhite;
      ColorElementTree.Canvas.FillRect(NodeRect.Left+4, NodeRect.Top+4,
                                       NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      ColorElementTree.Canvas.Pen.Color := c;
      ColorElementTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4,
                                        NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      ColorElementTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      ColorElementTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);
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

procedure TEditorColorOptionsFrame.ColorElementTreeClick(Sender: TObject);
begin
  FindCurHighlightElement;
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
  if X <= ColorPreview.GutterWidth then begin
    for i := 0 to ColorPreview.Gutter.Parts.Count-1 do begin
      if ColorPreview.Gutter.Parts[i].Width > X then begin
        if ColorPreview.Gutter.Parts[i] is TSynGutterLineNumber then
          Token := AdditionalHighlightAttributes[ahaLineNumber]
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterChanges then
          Token := AdditionalHighlightAttributes[ahaModifiedLine]
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterCodeFolding then
          Token := AdditionalHighlightAttributes[ahaCodeFoldingTree]
        else
          Token := dlgGutter;
        NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+Token);
        break;
      end;
      X := X - ColorPreview.Gutter.Parts[i].Width;
    end;
  end
  // Line Highlights
  else
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr = ahaFoldedCode then begin
      if (XY.X >= Length(ColorPreview.Lines[XY.Y-1]) + 4) and
         (XY.X <= Length(ColorPreview.Lines[XY.Y-1]) + 6) then
        NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
    end
    else if AddAttr <> ahaNone then
      NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[AddAttr]);
  end;
  if (NewNode = nil) and (XY.Y = ColorPreview.CaretY) and
     (XY.X > Length(ColorPreview.Lines[XY.Y - 1])+1)
  then
    NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+AdditionalHighlightAttributes[ahaLineHighlight]);
  // Pascal Highlights
  if NewNode = nil then
  begin
    Token := '';
    Attri := nil;
    ColorPreview.GetHighlighterAttriAtRowCol(XY, Token, Attri);
    if Attri = nil then
      Attri := PreviewSyn.WhitespaceAttribute;
    if Attri <> nil then
      NewNode := ColorElementTree.Items.FindNodeWithText(COLOR_NODE_PREFIX+Attri.Name);
  end;
  if NewNode <> nil then begin
    NewNode.Selected := True;
    FindCurHighlightElement;
  end;
end;

procedure TEditorColorOptionsFrame.ForegroundColorBoxChange(Sender: TObject);
begin
  if Sender = ForegroundColorBox then
  begin
    if (CurHighlightElement = nil) or UpdatingColor then
      Exit;
    UpdatingColor := True;
    CurHighlightElement.Foreground := DefaultToNone(ForeGroundColorBox.Selected);
    ForeGroundUseDefaultCheckBox.Checked := ForeGroundColorBox.Selected = clDefault;
    InvalidatePreviews;
    UpdatingColor := False;
  end;
  if Sender = BackGroundColorBox then
  begin
    if (CurHighlightElement = nil) or UpdatingColor then
      Exit;
    UpdatingColor := True;
    CurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
    BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected = clDefault;
    InvalidatePreviews;
    UpdatingColor := False;
  end;
  if Sender = FrameColorBox then
  begin
    if (CurHighlightElement = nil) or UpdatingColor then
      Exit;
    UpdatingColor := True;
    CurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
    FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected = clDefault;
    InvalidatePreviews;
    UpdatingColor := False;
  end;
end;

procedure TEditorColorOptionsFrame.GeneralCheckBoxOnChange(Sender: TObject);
begin
  if Sender = UseSyntaxHighlightCheckBox then
  begin
    SetPreviewSynInAllPreviews;
    InvalidatePreviews;
    Exit;
  end;

  if CurHighlightElement <> nil then
  begin
    if Sender = ForeGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        UpdatingColor := True;
        if ForeGroundUseDefaultCheckBox.Checked then
        begin
          ForegroundColorBox.Tag := ForegroundColorBox.Selected;
          ForegroundColorBox.Selected := clDefault;
        end
        else
          ForegroundColorBox.Selected := ForegroundColorBox.Tag;
        if DefaultToNone(ForegroundColorBox.Selected) <> CurHighlightElement.Foreground then
        begin
          CurHighlightElement.Foreground := DefaultToNone(ForegroundColorBox.Selected);
          InvalidatePreviews;
        end;
        UpdatingColor := False;
      end;
    if Sender = BackGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        if BackGroundUseDefaultCheckBox.Checked then
        begin
          BackGroundColorBox.Tag := BackGroundColorBox.Selected;
          BackGroundColorBox.Selected := clDefault;
        end
        else
          BackGroundColorBox.Selected := BackGroundColorBox.Tag;
        if DefaultToNone(BackGroundColorBox.Selected) <> CurHighlightElement.Background then
        begin
          CurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
          InvalidatePreviews;
        end;
      end;
    if Sender = FrameColorUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        if FrameColorUseDefaultCheckBox.Checked then
        begin
          FrameColorBox.Tag := FrameColorBox.Selected;
          FrameColorBox.Selected := clDefault;
        end
        else
          FrameColorBox.Selected := FrameColorBox.Tag;
        if DefaultToNone(FrameColorBox.Selected) <> CurHighlightElement.FrameColor then
        begin
          CurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
          InvalidatePreviews;
        end;
      end;
    if Sender = TextBoldCheckBox then
      if CurHighlightElementIsExtra then
        TextStyleRadioOnChange(Sender)
      else
      if TextBoldCheckBox.Checked xor (fsBold in CurHighlightElement.Style) then
      begin
        if TextBoldCheckBox.Checked then
          CurHighlightElement.Style := CurHighlightElement.Style + [fsBold]
        else
          CurHighlightElement.Style := CurHighlightElement.Style - [fsBold];
        InvalidatePreviews;
      end;
    if Sender = TextItalicCheckBox then
      if CurHighlightElementIsExtra then
        TextStyleRadioOnChange(Sender)
      else
      if TextItalicCheckBox.Checked then
      begin
        if not (fsItalic in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsItalic];
          InvalidatePreviews;
        end;
      end
      else
      if (fsItalic in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsItalic];
        InvalidatePreviews;
      end;
    if Sender = TextUnderlineCheckBox then
      if CurHighlightElementIsExtra then
        TextStyleRadioOnChange(Sender)
      else
      if TextUnderlineCheckBox.Checked then
      begin
        if not (fsUnderline in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsUnderline];
          InvalidatePreviews;
        end;
      end
      else
      if (fsUnderline in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsUnderline];
        InvalidatePreviews;
      end;
  end;
end;

procedure TEditorColorOptionsFrame.ComboBoxOnExit(Sender: TObject);
var
  Box: TComboBox absolute Sender;
  NewVal, a: integer;
begin
  if Sender = ColorSchemeComboBox then
    with GeneralPage do
    begin
      if Box.Items.IndexOf(Box.Text) < 0 then
        SetComboBoxText(Box, GetCurColorScheme(PreviewSyn.LanguageName))
        // unknown color scheme -> switch back
      else
      if Box.Text <> GetCurColorScheme(PreviewSyn.LanguageName) then
      begin
        // change the colorscheme
        SetCurColorScheme(PreviewSyn.LanguageName, Box.Text);
        SetComboBoxText(Box, Box.Text);
        PreviewSyn := GetHighlighter(TCustomSynClass(PreviewSyn.ClassType),
          Box.Text, True);
        SetPreviewSynInAllPreviews;
        FillColorElementListBox;
        FindCurHighlightElement;
        InvalidatePreviews;
      end;
    end
  else
  if Sender = FileExtensionsComboBox then
  begin
    //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Old="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
    if Box.Text <> GetCurFileExtensions(PreviewSyn.LanguageName) then
    begin
      SetCurFileExtensions(PreviewSyn.LanguageName, Box.Text);
      SetComboBoxText(Box, Box.Text);
    end;
    //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Now="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
  end
  else
  if Sender = LanguageComboBox then
  begin
    if Box.Items.IndexOf(Box.Text) < 0 then
      SetComboBoxText(Box, PreviewSyn.LanguageName)// unknown language -> switch back
    else
    if Box.Text <> PreviewSyn.LanguageName then
    begin
      NewVal := EditorOpts.HighlighterList.FindByName(Box.Text);
      if NewVal >= 0 then
      begin
        SetComboBoxText(Box, Box.Text);
        CurLanguageID := NewVal;
        PreviewSyn    := GetHighlighter(
          EditorOpts.HighlighterList[CurLanguageID].SynClass,
          GetCurColorScheme(
          EditorOpts.HighlighterList[
          CurLanguageID].SynClass.GetLanguageName)
          , True);
        SetComboBoxText(ColorSchemeComboBox,
          GetCurColorScheme(PreviewSyn.LanguageName));
        SetComboBoxText(FileExtensionsComboBox,
          GetCurFileExtensions(PreviewSyn.LanguageName));
        SetPreviewSynInAllPreviews;
        with GeneralPage do
          for a := Low(PreviewEdits) to High(PreviewEdits) do
          begin
            PreviewEdits[a].Lines.Text := EditorOpts.HighlighterList[CurLanguageID].SampleSource;
            PreviewEdits[a].CaretXY := EditorOpts.HighlighterList[CurLanguageID].CaretXY;
            PreviewEdits[a].TopLine := 1;
            PreviewEdits[a].LeftChar := 1;
            PreviewEdits[a].Keystrokes.Clear;
            PreviewEdits[a].MouseActions.Clear;
          end;
        InvalidatePreviews;
        with GeneralPage do
          for a := Low(PreviewEdits) to High(PreviewEdits) do
            PreviewEdits[a].AfterLoadFromFile;
        FillColorElementListBox;
        FindCurHighlightElement;
      end;
    end;
  end
  else
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

  procedure CalcNewStyle(CheckBox: TCheckBox; RadioOn, RadioOff,
                         RadioInvert: TRadioButton; fs : TFontStyle;
                         Panel: TPanel);
  begin
    if CheckBox.Checked then
    begin
      Panel.Enabled := True;
      if RadioInvert.Checked then
      begin
        CurHighlightElement.Style     := CurHighlightElement.Style + [fs];
        CurHighlightElement.StyleMask := CurHighlightElement.StyleMask - [fs];
      end
      else
      if RadioOn.Checked then
      begin
        CurHighlightElement.Style     := CurHighlightElement.Style + [fs];
        CurHighlightElement.StyleMask := CurHighlightElement.StyleMask + [fs];
      end
      else
      if RadioOff.Checked then
      begin
        CurHighlightElement.Style     := CurHighlightElement.Style - [fs];
        CurHighlightElement.StyleMask := CurHighlightElement.StyleMask + [fs];
      end
    end
    else
    begin
      Panel.Enabled := False;
      CurHighlightElement.Style     := CurHighlightElement.Style - [fs];
      CurHighlightElement.StyleMask := CurHighlightElement.StyleMask - [fs];
    end;
  end;
begin
  if UpdatingColor or not CurHighlightElementIsExtra then
    Exit;

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


  InvalidatePreviews;
end;

procedure TEditorColorOptionsFrame.ShowCurAttribute;
begin
  if (CurHighlightElement = nil) or UpdatingColor then
    Exit;
  UpdatingColor := True;

  TextBoldRadioPanel.Visible      := CurHighlightElementIsExtra and
                                    ahaSupportedFeatures[CurExtraElement].Style;
  TextItalicRadioPanel.Visible    := CurHighlightElementIsExtra and
                                    ahaSupportedFeatures[CurExtraElement].Style;
  TextUnderlineRadioPanel.Visible := CurHighlightElementIsExtra and
                                    ahaSupportedFeatures[CurExtraElement].Style;

  TextBoldCheckBox.Visible      := not((CurHighlightElementIsExtra and
                                        not ahaSupportedFeatures[CurExtraElement].Style));
  TextItalicCheckBox.Visible    := not((CurHighlightElementIsExtra and
                                        not ahaSupportedFeatures[CurExtraElement].Style));
  TextUnderlineCheckBox.Visible := not((CurHighlightElementIsExtra and
                                        not ahaSupportedFeatures[CurExtraElement].Style));

  ForeGroundLabel.Caption := dlgForecolor;
  BackGroundLabel.Caption := dlgBackColor;
  FrameColorLabel.Caption := dlgFrameColor;

  if CurHighlightElementIsExtra and (CurExtraElement = ahaModifiedLine) then
  begin
    ForeGroundLabel.Caption := dlgSavedLineColor;
    FrameColorLabel.Caption := dlgUnsavedLineColor;
  end;

  if CurHighlightElementIsExtra then
  begin
    TextBoldCheckBox.Checked :=
      (fsBold in CurHighlightElement.Style) or
      (fsBold in CurHighlightElement.StyleMask);
    TextBoldRadioPanel.Enabled := TextBoldCheckBox.Checked;

    if not(fsBold in CurHighlightElement.StyleMask) then
      TextBoldRadioInvert.Checked := True
    else
    if fsBold in CurHighlightElement.Style then
      TextBoldRadioOn.Checked := True
    else
      TextBoldRadioOff.Checked := True;

    TextItalicCheckBox.Checked :=
      (fsItalic in CurHighlightElement.Style) or
      (fsItalic in CurHighlightElement.StyleMask);
    TextItalicRadioPanel.Enabled := TextItalicCheckBox.Checked;

    if not(fsItalic in CurHighlightElement.StyleMask) then
      TextItalicRadioInvert.Checked := True
    else
    if fsItalic in CurHighlightElement.Style then
      TextItalicRadioOn.Checked := True
    else
      TextItalicRadioOff.Checked := True;

    TextUnderlineCheckBox.Checked :=
      (fsUnderline in CurHighlightElement.Style) or
      (fsUnderline in CurHighlightElement.StyleMask);
    TextUnderlineRadioPanel.Enabled := TextUnderlineCheckBox.Checked;

    if not(fsUnderline in CurHighlightElement.StyleMask) then
      TextUnderlineRadioInvert.Checked := True
    else
    if fsUnderline in CurHighlightElement.Style then
      TextUnderlineRadioOn.Checked := True
    else
      TextUnderlineRadioOff.Checked := True;
  end
  else
  begin
    TextBoldCheckBox.Checked := fsBold in CurHighlightElement.Style;
    TextItalicCheckBox.Checked := fsItalic in CurHighlightElement.Style;
    TextUnderlineCheckBox.Checked := fsUnderline in CurHighlightElement.Style;
  end;

  if CurHighlightElementIsExtra then begin
    BackGroundColorBox.Enabled           := ahaSupportedFeatures[CurExtraElement].BG;
    BackGroundUseDefaultCheckBox.Enabled := ahaSupportedFeatures[CurExtraElement].BG;
    ForegroundColorBox.Enabled           := ahaSupportedFeatures[CurExtraElement].FG;
    ForeGroundUseDefaultCheckBox.Enabled := ahaSupportedFeatures[CurExtraElement].FG;
    FrameColorBox.Enabled                := ahaSupportedFeatures[CurExtraElement].FF;
    FrameColorUseDefaultCheckBox.Enabled := ahaSupportedFeatures[CurExtraElement].FF;
  end else begin
    BackGroundColorBox.Enabled           := True;
    BackGroundUseDefaultCheckBox.Enabled := True;
    ForegroundColorBox.Enabled           := True;
    ForeGroundUseDefaultCheckBox.Enabled := True;
    FrameColorBox.Enabled                := True;
    FrameColorUseDefaultCheckBox.Enabled := True;
  end;

  ForegroundColorBox.Selected := NoneToDefault(CurHighlightElement.Foreground);
  if ForegroundColorBox.Selected = clDefault then
    ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
  else
    ForegroundColorBox.Tag := ForegroundColorBox.Selected;
  ForeGroundUseDefaultCheckBox.Checked := ForegroundColorBox.Selected = clDefault;

  BackGroundColorBox.Selected := NoneToDefault(CurHighlightElement.Background);
  if BackGroundColorBox.Selected = clDefault then
    BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
  else
    BackGroundColorBox.Tag := BackGroundColorBox.Selected;
  BackGroundUseDefaultCheckBox.Checked := BackGroundColorBox.Selected = clDefault;

  FrameColorBox.Selected := NoneToDefault(CurHighlightElement.FrameColor);
  if FrameColorBox.Selected = clDefault then
    FrameColorBox.Tag := FrameColorBox.DefaultColorColor
  else
    FrameColorBox.Tag := FrameColorBox.Selected;
  FrameColorUseDefaultCheckBox.Checked := FrameColorBox.Selected = clDefault;

  UpdatingColor := False;
end;

function TEditorColorOptionsFrame.IsAhaElement(aName: String;
  var aha: TAdditionalHilightAttribute): Boolean;
var
  h: TAdditionalHilightAttribute;
begin
  Result := False;
  for h := Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute) do
    if aName = EditorOpts.GetAdditionalAttributeName(h) then begin
      Result := True;
      aha := h;
      break;
    end;
end;

procedure TEditorColorOptionsFrame.FindCurHighlightElement;
var
  i: Integer;
  Old: TSynHighlightElement;
  NewName: String;
begin
  if (ColorElementTree.Selected <> nil) and
     (ColorElementTree.Selected.Parent = nil) and
     (ColorElementTree.Selected.GetFirstChild <> nil)
  then
    ColorElementTree.Selected.GetFirstChild.Selected := True;
  if (ColorElementTree.Selected = nil) or (ColorElementTree.Selected.Data = nil) then
    exit;

  NewName := TSynHighlighterAttributes(ColorElementTree.Selected.Data).StoredName;

  Old := CurHighlightElement;
  CurHighlightElement := nil;
  CurExtraElement := Low(TAdditionalHilightAttribute);
  i := PreviewSyn.AttrCount - 1;
  while (i >= 0) do
  begin
    if NewName = PreviewSyn.Attribute[i].StoredName then
    begin
      CurHighlightElement := PreviewSyn.Attribute[i];
      break;
    end;
    dec(i);
  end;

  if (Old <> CurHighlightElement) or (CurHighlightElement = nil) then
  begin
    CurHighlightElementIsExtra := False;

    if CurHighlightElement <> nil then
      CurHighlightElementIsExtra := IsAhaElement(NewName, CurExtraElement);
    ShowCurAttribute;
  end;
end;

procedure TEditorColorOptionsFrame.FillColorElementListBox;
var
  i: Integer;
  ParentName: String;
  h: TAdditionalHilightAttribute;
  ParentNode: TTreeNode;
  j: TAhaGroupName;
begin
  ColorElementTree.BeginUpdate;
  ColorElementTree.Items.Clear;

  ColorElementTree.Items.Add(nil, PreviewSyn.LanguageName);
  for j := low(TAhaGroupName) to high(TAhaGroupName) do
    ColorElementTree.Items.Add(nil, AdditionalHighlightGroupNames[j]);

  for i := 0 to PreviewSyn.AttrCount - 1 do
    if PreviewSyn.Attribute[i].Name <> '' then begin
      ParentName := PreviewSyn.LanguageName;
      if IsAhaElement(PreviewSyn.Attribute[i].StoredName, h) then
        ParentName := AdditionalHighlightGroupNames[ahaSupportedFeatures[h].Group];
      ParentNode := ColorElementTree.Items.FindTopLvlNode(ParentName);
      if ParentNode = nil then begin
        ParentNode := ColorElementTree.Items.Add(nil, ParentName);
      end;
      with ColorElementTree.Items.AddChild(ParentNode, COLOR_NODE_PREFIX+PreviewSyn.Attribute[i].Name) do
        Data := Pointer(PreviewSyn.Attribute[i]);
    end;

  for i := 0 to ColorElementTree.Items.Count - 1 do
    ColorElementTree.Items[i].AlphaSort;
  ColorElementTree.EndUpdate;
  ColorElementTree.FullExpand;
  if ColorElementTree.Items.GetFirstNode <> nil then
    ColorElementTree.Items.GetFirstNode.Selected := True;

  CurHighlightElement := nil;
  CurExtraElement := Low(TAdditionalHilightAttribute);
  CurHighlightElementIsExtra := False;
  FindCurHighlightElement;
end;

procedure TEditorColorOptionsFrame.SetColorElementsToDefaults(
  OnlySelected: Boolean);
var
  DefaultSyn: TSrcIDEHighlighter;
  PascalSyn: TPreviewPasSyn;
  i, j: Integer;
  CurSynClass: TCustomSynClass;
begin
  PascalSyn := TPreviewPasSyn(GetHighlighter(TPreviewPasSyn,
    ColorSchemeComboBox.Text, True));
  CurSynClass := TCustomSynClass(PreviewSyn.ClassType);
  DefaultSyn := CurSynClass.Create(nil);
  try
    EditorOpts.AddSpecialHilightAttribsToHighlighter(DefaultSyn);
    EditorOpts.ReadDefaultsForHighlighterSettings(DefaultSyn,
      ColorSchemeComboBox.Text, PascalSyn);
    for i := 0 to DefaultSyn.AttrCount - 1 do
    begin
      if DefaultSyn.Attribute[i].Name = '' then
        continue;
      if OnlySelected then
      begin
        if (CurHighlightElement <> nil) and (DefaultSyn.Attribute[i].Name = CurHighlightElement.Name) then
          CopyHiLightAttributeValues(DefaultSyn.Attribute[i], CurHighlightElement);
      end
      else
        for j := 0 to PreviewSyn.AttrCount - 1 do
          if PreviewSyn.Attribute[j].Name = DefaultSyn.Attribute[i].Name then
            CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
              PreviewSyn.Attribute[j]);
    end;
  finally
    DefaultSyn.Free;
  end;
  ShowCurAttribute;
  InvalidatePreviews;
end;

function TEditorColorOptionsFrame.GetCurColorScheme(const LanguageName: String): String;
begin
  if FColorSchemes = nil then
    Result := ''
  else
    Result := FColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := EditorOpts.ReadColorScheme(LanguageName);
end;

procedure TEditorColorOptionsFrame.SetCurColorScheme(const LanguageName,
  ColorScheme: String);
begin
  if FColorSchemes = nil then
    FColorSchemes := TStringList.Create;
  FColorSchemes.Values[LanguageName] := ColorScheme;
end;

function TEditorColorOptionsFrame.GetHighlighter(SynClass: TCustomSynClass;
  const ColorScheme: String; CreateIfNotExists: Boolean): TSrcIDEHighlighter;
var
  i: Integer;
begin
  if FHighlighterList = nil then
    FHighlighterList := TStringList.Create;
  for i := 0 to FHighlighterList.Count - 1 do
    if (FHighlighterList[i] = ColorScheme) and
      (TCustomSynClass(TSrcIDEHighlighter(fHighlighterList.Objects[i]).ClassType) =
      SynClass) then
    begin
      Result := TSrcIDEHighlighter(FHighlighterList.Objects[i]);
      exit;
    end;
  if CreateIfNotExists then
  begin
    Result := SynClass.Create(nil);
    EditorOpts.AddSpecialHilightAttribsToHighlighter(Result);
    FHighlighterList.AddObject(ColorScheme, Result);
    EditorOpts.ReadHighlighterSettings(Result, ColorScheme);
  end;
end;

procedure TEditorColorOptionsFrame.ClearHighlighters;
var
  i: Integer;
begin
  if FHighlighterList = nil then
    Exit;
  for i := 0 to FHighlighterList.Count - 1 do
    TSrcIDEHighlighter(FHighlighterList.Objects[i]).Free;
  FHighlighterList.Free;
end;

procedure TEditorColorOptionsFrame.InvalidatePreviews;
var
  a: Integer;
begin
  ColorElementTree.Invalidate;
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        if UseSyntaxHighlightCheckBox.Checked and (PreviewSyn <> nil) then
          EditorOpts.SetMarkupColors(PreviewEdits[a].Highlighter, PreviewEdits[a],
                                     GetCurColorScheme(PreviewSyn.LanguageName))
        else
          EditorOpts.SetMarkupColors(nil, PreviewEdits[a]);
        PreviewEdits[a].Invalidate;
      end;
end;

procedure TEditorColorOptionsFrame.SetPreviewSynInAllPreviews;
var
  a: Integer;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
        if UseSyntaxHighlightCheckBox.Checked then
          PreviewEdits[a].Highlighter := PreviewSyn
        else
          PreviewEdits[a].Highlighter := nil;
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

destructor TEditorColorOptionsFrame.Destroy;
begin
  FFileExtensions.Free;
  ClearHighlighters;
  FColorSchemes.Free;
  inherited Destroy;
end;

function TEditorColorOptionsFrame.GetTitle: String;
begin
  Result := dlgEdColor;
end;

procedure TEditorColorOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // Prevent the caret from moving
  ColorPreview.RegisterMouseActionSearchHandler(@DoSynEditMouse);
  FDialog := ADialog;
  UpdatingColor := False;
  CurHighlightElement := nil;
  CurExtraElement := Low(TAdditionalHilightAttribute);
  CurHighlightElementIsExtra := False;

  UseSyntaxHighlightCheckBox.Caption := dlgUseSyntaxHighlight;
  LanguageLabel.Caption := dlgLang;
  ColorSchemeLabel.Caption := dlgClrScheme;

  with ColorSchemeComboBox do
  begin
    ColorSchemeFactory.GetRegisteredSchemes(Items);
    Text := DEFAULT_COLOR_SCHEME.Name;
  end;

  FileExtensionsLabel.Caption := dlgFileExts;
  SetAttributeToDefaultButton.Caption := dlgSetElementDefault;
  SetAllAttributesToDefaultButton.Caption := dlgSetAllElementDefault;
  ForeGroundLabel.Caption := dlgForecolor;
  ForeGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;
  BackGroundLabel.Caption := dlgBackColor;
  BackGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;
  FrameColorLabel.Caption := dlgFrameColor;
  FrameColorUseDefaultCheckBox.Caption := dlgEdUseDefColor;
  ElementAttributesGroupBox.Caption := dlgElementAttributes;

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
    UseSyntaxHighlightCheckBox.Checked := UseSyntaxHighlight;

    with LanguageComboBox do
      with Items do
      begin
        BeginUpdate;
        for i := 0 to EditorOpts.HighlighterList.Count - 1 do
          Add(HighlighterList[i].SynClass.GetLanguageName);
        EndUpdate;
      end;

    with FileExtensionsComboBox, GeneralPage do
      if CurLanguageID >= 0 then
        SetComboBoxText(FileExtensionsComboBox,
          HighlighterList[CurLanguageID].FileExtensions);

    PreviewSyn := GetHighlighter(TPreviewPasSyn, GetCurColorScheme(TPreviewPasSyn.GetLanguageName), True);
    CurLanguageID := HighlighterList.FindByClass(TCustomSynClass(PreviewSyn.ClassType));

    with GeneralPage do
      for i := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i] <> nil then
          with PreviewEdits[i] do
          begin
            if UseSyntaxHighlight then
              Highlighter := PreviewSyn
            else
              Highlighter := nil;
            Lines.Text := HighlighterList[CurLanguageID].SampleSource;
            CaretXY := HighlighterList[CurLanguageID].CaretXY;
            TopLine := 1;
            LeftChar := 1;
            AfterLoadFromFile;
            Keystrokes.Clear;
            MouseActions.Clear;
          end;

    LanguageComboBox.Text := PreviewSyn.LanguageName;
    SetComboBoxText(LanguageComboBox, LanguageComboBox.Text);
    ColorSchemeComboBox.Text := GetCurColorScheme(PreviewSyn.LanguageName);
    SetComboBoxText(ColorSchemeComboBox, ColorSchemeComboBox.Text);

    FillColorElementListBox;
    FindCurHighlightElement;
    ShowCurAttribute;
    InvalidatePreviews;
  end;
end;

procedure TEditorColorOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i, j: Integer;
  Syn: TSrcIDEHighlighter;
begin
  with AOptions as TEditorOptions do
  begin
    UseSyntaxHighlight := UseSyntaxHighlightCheckBox.Checked;

    if FFileExtensions <> nil then
    begin
      for i := 0 to FFileExtensions.Count - 1 do
      begin
        j := HighlighterList.FindByName(FFileExtensions.Names[i]);
        if j >= 0 then
          HighlighterList[j].FileExtensions := FFileExtensions.ValueFromIndex[i];
      end;
    end;

    if FColorSchemes <> nil then
    begin
      for i := 0 to FColorSchemes.Count - 1 do
         WriteColorScheme(FColorSchemes.Names[i],
           FColorSchemes.Values[FColorSchemes.Names[i]]);
    end;

    if FHighlighterList <> nil then
    begin
      for i := 0 to FHighlighterList.Count - 1 do
      begin
        Syn := TSrcIDEHighlighter(FHighlighterList.Objects[i]);
        WriteHighlighterSettings(Syn, FHighlighterList[i]);
      end;
    end;

  end;
end;

procedure TEditorColorOptionsFrame.SelectAhaColor(aha: TAdditionalHilightAttribute);
var
  i: Integer;
begin
  for i := 0 to ColorElementTree.Items.Count - 1 do begin
    if ColorElementTree.Items[i].Data = nil then continue;
    if TSynHighlighterAttributes(ColorElementTree.Items[i].Data).StoredName <>
       EditorOpts.GetAdditionalAttributeName(aha)
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
  e: TSynHighlightElement;
  AddAttr: TAdditionalHilightAttribute;
  i: Integer;
begin
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
    if (AddAttr <> ahaNone) and (AddAttr <> ahaFoldedCode) then
    begin
      i := PreviewSyn.AttrCount - 1;
      while (i >= 0) do
      begin
        e := PreviewSyn.Attribute[i];
        if e.Name = '' then begin
          dec(i);
          continue;
        end;
        if e.Name = AdditionalHighlightAttributes[AddAttr] then
        begin
          Special := True;
          EditorOpts.SetMarkupColor(PreviewSyn, AddAttr, aMarkup);
          exit;
        end;
        dec(i);
      end;
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
  {$I editor_color_options.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorColorOptionsFrame, EdtOptionsColors);
end.
