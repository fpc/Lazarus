{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Andrew Johnson, Mattias Gaertner

  Abstract:
    This units defines the property editors for graphic types.
}
unit GraphPropEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, TypInfo, SysUtils, Forms, Controls, LCLType, GraphType, Graphics,
  StdCtrls, Buttons, ComCtrls, Menus, ExtCtrls, Dialogs, LCLLinux, PropEdits;

type
 {TPixmapPropertyEditor
  The default property editor for all TGraphic's and sub types (e.g. TBitmap,
  TPixmap, TIcon, etc.). }

  TPixmapPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TPicturePropertyEditor
  The default property editor for TPicture}

  TPicturePropertyEditor = class(TPixmapPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TButtonGlyphPropEditor
  The default property editor for the Glyphs of TSpeedButton and TBitBtn }
  TButtonGlyphPropEditor = class(TPixmapPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TColorPropertyEditor
  PropertyEditor editor for the TColor type. Displays the color as a clXXX value
  if one exists, otherwise displays the value as hex.  Also allows the
  clXXX value to be picked from a list. }

  TColorPropertyEditor = class(TIntegerPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStringProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure ListMeasureWidth(const CurValue:ansistring; Index:integer;
      ACanvas:TCanvas;  var AWidth:Integer);  override;
    procedure ListDrawValue(const CurValue:ansistring; Index:integer;
      ACanvas:TCanvas;  const ARect:TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
      AState:TPropEditDrawState); override;
  end;

{ TBrushStylePropertyEditor
  PropertyEditor editor for TBrush's Style. Simply provides for custom render. }

  TBrushStylePropertyEditor = class(TEnumPropertyEditor)
  public
    procedure ListMeasureWidth(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  var AWidth: Integer); override;
    procedure ListDrawValue(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState:TPropEditDrawState); override;
  end;

{ TPenStylePropertyEditor
  PropertyEditor editor for TPen's Style. Simply provides for custom render. }

  TPenStylePropertyEditor = class(TEnumPropertyEditor)
  public
    procedure ListMeasureWidth(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  var AWidth: Integer); override;
    procedure ListDrawValue(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState:TPropEditDrawState); override;
  end;

{ TFontPropertyEditor
  PropertyEditor editor for the Font property.
  Brings up the font dialog as well as allowing the properties of the object to
  be edited. }

  TFontPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


//==============================================================================
// XXX
// This class is a workaround for the missing typeinfo function
type
  TDummyClassForGraphPropTypes = class(TDummyClassForPropTypes)
  private
    FColor:TColor;
    FBrushStyle:TBrushStyle;
    FPenStyle:TPenStyle;
  published
    property Color:TColor read FColor write FColor;
    property BrushStyle:TBrushStyle read FBrushStyle;
    property PenStyle:TPenStyle read FPenStyle;
  end;
//==============================================================================


implementation


var
  DummyClassForPropTypes: TDummyClassForGraphPropTypes;


{Form For Picture/Graphic Property Editor}
type
  TPicturePropertyEditorForm = class(TForm)
  protected
    Opendlg: TOPENDIALOG;
    Savedlg: TSAVEDIALOG;

    OKBTN: TBITBTN;
    CANCELBTN: TBITBTN;

    LoadBTN: TBUTTON;
    SaveBTN: TBUTTON;
    ClearBTN : TBUTTON;

    ScrollPanel : TPanel;

    ScrollBox : TScrollBox;

    procedure LoadBTNCLICK(Sender: TObject);
    procedure SaveBTNCLICK(Sender: TObject);
    procedure ClearBTNCLICK(Sender: TObject);

  public
    Preview: TIMAGE;

    FileName : String;
    Constructor Create(AOwner : TComponent); Override;
  end;

Constructor TPicturePropertyEditorForm.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  FileName := '';
  Position := poDesktopCenter;

  Caption := 'Load Image Dialog';

  HEIGHT := 419;
  WIDTH  := 403;

  With CONSTRAINTS do begin
    MAXHEIGHT := Height;
    MAXWIDTH  := Width;
    MINHEIGHT := Height;
    MINWIDTH  := Width;
  end;

  Opendlg := TOPENDIALOG.Create(Self);
  With Opendlg do begin
    OPTIONS := [ofextensiondifferent, ofpathmustexist, offilemustexist, ofenablesizing];
    DEFAULTEXT := '.xpm';
    FILTER := '*.xpm';
  end;

  Savedlg := TSAVEDIALOG.Create(Self);
  With Savedlg do begin
    OPTIONS := [ofextensiondifferent, ofpathmustexist, offilemustexist, ofenablesizing];
    DEFAULTEXT := '.xpm';
    FILTER := '*.xpm';
  end;

  OKBTN := TBITBTN.Create(Self);
  With OKBTN do begin
    Parent := Self;
    KIND := bkok;
    SPACING := 3;
    MODALRESULT := mrOK;
    CAPTION := '&OK';
    LEFT := 325;
    HEIGHT := 29;
    TOP := 8;
    WIDTH := 72;
    Show;
  end;

  ScrollBox := TScrollBox.Create(Self);
  With ScrollBox do begin
    Parent := Self;
    LEFT := 8;
    HEIGHT := 365;
    TOP := 8;
    WIDTH := 310;
    AutoSize := False;
    AutoScroll := True;
    Show;
  end;

  ScrollPanel := TPanel.Create(ScrollBox);
  With ScrollPanel do begin
    Parent := ScrollBox;
    Caption := '';
    BorderStyle := bsNone;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Color := clWhite;
    LEFT := 0;
    HEIGHT := 356;
    TOP := 0;
    WIDTH := 302;
    Show;
  end;

  Preview := TIMAGE.Create(ScrollPanel);
  With Preview do begin
    Parent := ScrollPanel;
    LEFT := 0;
    HEIGHT := 356;
    TOP := 0;
    WIDTH := 302;
    With CONSTRAINTS do begin
      MINHEIGHT := Height;
      MINWIDTH  := Width;
    end;
    AutoSize := True;
    Transparent := True;
    Center := True;
    Stretch := False;
    Show;
  end;

  ScrollPanel.AutoSize := True;

  CANCELBTN := TBITBTN.Create(Self);
  With CANCELBTN do begin
    Parent := Self;
    KIND := bkcancel;
    SPACING := 3;
    MODALRESULT := mrCancel;
    CAPTION := '&Cancel';
    LEFT := 325;
    HEIGHT := 30;
    TOP := 48;
    WIDTH := 72;
    Show;
  end;

  LoadBTN := TBUTTON.Create(Self);
  With LoadBTN do begin
    Parent := Self;
    CAPTION := '&Load';
    ONCLICK := @LoadBTNCLICK;
    LEFT := 8;
    HEIGHT := 29;
    TOP := 384;
    WIDTH := 82;
    Show;
  end;

  SaveBTN := TBUTTON.Create(Self);
  With SaveBTN do begin
    Parent := Self;
    ENABLED := False;
    CAPTION := '&Save';
    ONCLICK := @SaveBTNCLICK;
    LEFT := 123;
    HEIGHT := 29;
    TOP := 384;
    WIDTH := 76;
    Show;
  end;

  ClearBTN := TBUTTON.Create(Self);
  With ClearBTN do begin
    Parent := Self;
    CAPTION := 'C&lear';
    ONCLICK := @ClearBTNCLICK;
    LEFT := 236;
    HEIGHT := 29;
    TOP := 384;
    WIDTH := 82;
    Show;
  end;
end;

procedure TPicturePropertyEditorForm.LoadBTNCLICK(Sender: TObject);
Const
  Formats : Array[0..1{7}] of String =
    ('.xpm',
     '.bmp'{,
     '.ico',
     '.png',
     '.gif',
     '.jpg',
     '.jpeg',
     '.tiff'});
     //Until Graphics Handlers Have been Added for these others,
     //Only .xpm, and .bpm can actually load using TPicture.
     //We esspecially need to make an icon handler that will
     //support .ico AND .xpm,  for windows/*nix compatibility.
var
  Ext : String;
  I : Integer;
begin
  If OpenDlg.Execute then begin
    Ext := ExtractFileExt(OpenDlg.FileName);
    FileName := OpenDlg.FileName;
    For I := Low(Formats) to High(Formats) do
      If AnsiCompareText(Ext,Formats[I]) = 0 then
        Preview.Picture.LoadFromFile(OpenDlg.FileName);
  end;
  SaveBTN.Enabled := False;
  If Assigned(Preview.Picture.Graphic) then
    If not Preview.Picture.Graphic.Empty then
      SaveBTN.Enabled := True;
end;

procedure TPicturePropertyEditorForm.SaveBTNCLICK(Sender: TObject);
begin
  If SaveDlg.Execute then
    Preview.Picture.SaveToFile(SaveDlg.FileName);
end;

procedure TPicturePropertyEditorForm.ClearBTNCLICK(Sender: TObject);
begin
  With Preview do begin
    Picture.Graphic := nil;
    Width := 0;
    Height := 0;
  end;
  SaveBTN.Enabled := False;
end;

{ TPixmapPropertyEditor }
procedure TPixmapPropertyEditor.Edit;
var
  TheDialog: TPicturePropertyEditorForm;
  Pixmap : TPixmap;
begin
  Pixmap := TPixmap(GetOrdValue);
  TheDialog := TPicturePropertyEditorForm.Create(Application);
  If not Pixmap.Empty then begin
    TheDialog.Preview.Picture.Pixmap.Width := Pixmap.Width;
    TheDialog.Preview.Picture.Pixmap.Height := Pixmap.Height;
    With TheDialog.Preview.Picture.Pixmap.Canvas do begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Pixmap.Width, Pixmap.Height));
      Draw(0, 0, Pixmap);
    end;
  end;
  try
    if (TheDialog.ShowModal = mrOK) then begin
      If TheDialog.Preview.Picture.Graphic <> nil then begin
        If TheDialog.FileName <> '' then
          If FileExists(TheDialog.FileName) then
            Pixmap.LoadFromFile(TheDialog.FileName);
      end
      else
        Pixmap.FreeImage;
    end;
  finally
    TheDialog.Free;
  end;
end;

function TPixmapPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

{ TPicturePropertyEditor }
procedure TPicturePropertyEditor.Edit;
var
  TheDialog: TPicturePropertyEditorForm;
  Picture : TPicture;
begin
  Picture := TPicture(GetOrdValue);
  TheDialog := TPicturePropertyEditorForm.Create(Application);
  If (Picture.Graphic <> nil) and (Picture.Graphic is TBitmap) then begin
    TheDialog.Preview.Picture.Bitmap.Width := Picture.Width;
    TheDialog.Preview.Picture.Bitmap.Height := Picture.Height;
    With TheDialog.Preview.Picture.Bitmap.Canvas do begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Picture.Width, Picture.Height));
      Draw(0, 0, Picture.Graphic);
    end;
  end;
  try
    if (TheDialog.ShowModal = mrOK) then begin
      If TheDialog.Preview.Picture.Graphic <> nil then begin
        If TheDialog.FileName <> '' then
          If FileExists(TheDialog.FileName) then
            Picture.LoadFromFile(TheDialog.FileName);
      end
      else
        Picture.Graphic := nil;
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TButtonGlyphPropEditor }

procedure TButtonGlyphPropEditor.Edit;
var
  TheDialog: TPicturePropertyEditorForm;
  Pixmap : TPixmap;
  Component : TComponent;
begin
  Component := TComponent(GetComponent(0));
  If (Component = nil) or ((not (Component is TSpeedButton)) and
    (not (Component is TBitBtn))) or (LowerCase(GetName) <> 'glyph')
  then begin
    Inherited Edit;
    exit;
  end;
  Pixmap := TPixmap(GetOrdValue);
  TheDialog := TPicturePropertyEditorForm.Create(Application);
  If not Pixmap.Empty then begin
    TheDialog.Preview.Picture.Pixmap.Width := Pixmap.Width;
    TheDialog.Preview.Picture.Pixmap.Height := Pixmap.Height;
    With TheDialog.Preview.Picture.Pixmap.Canvas do begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, Pixmap.Width, Pixmap.Height));
      Draw(0, 0, Pixmap);
    end;
  end;
  try
    if (TheDialog.ShowModal = mrOK) then begin
      If TheDialog.Preview.Picture.Graphic <> nil then begin
        If Component is TSpeedButton then begin
          Pixmap := TPixmap.Create;
          If TheDialog.FileName <> '' then
            If FileExists(TheDialog.FileName) then
              Pixmap.LoadFromFile(TheDialog.FileName);
          TSpeedButton(Component).Glyph := Pixmap;
        end
        else
          If Component is TBitBTN then begin
            Pixmap := TPixmap.Create;
            If TheDialog.FileName <> '' then
              If FileExists(TheDialog.FileName) then
                Pixmap.LoadFromFile(TheDialog.FileName);
            TBitBTN(Component).Glyph := Pixmap;
          end
       else
         If TheDialog.FileName <> '' then
           If FileExists(TheDialog.FileName) then
             Pixmap.LoadFromFile(TheDialog.FileName);
      end
      else
        Pixmap.FreeImage;
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TColorPropertyEditor }

procedure TColorPropertyEditor.Edit;
var
  ColorDialog: TColorDialog;
  {IniFile: TRegIniFile;

  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      // Ignore errors reading values
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: ansistring;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S,
              CustomColors.Values[S]);
          end;
        end;
  end;
  }
begin
  {IniFile := nil;}
  ColorDialog := TColorDialog.Create(Application);
  try
    {GetCustomColors;}
    ColorDialog.Color := GetOrdValue;
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
    {SaveCustomColors;}
  finally
    {IniFile.Free;}
    ColorDialog.Free;
  end;
end;

function TColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TColorPropertyEditor.GetValue: ansistring;
begin
  Result := ColorToString(TColor(GetOrdValue));
end;

procedure TColorPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  GetColorValues(Proc);
end;

procedure TColorPropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInComboList])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TColorPropertyEditor.ListDrawValue(const CurValue:ansistring;
Index:integer; ACanvas:TCanvas;  const ARect:TRect; AState: TPropEditDrawState);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if pedsSelected in AState then
      Result := clWhite
    else
      Result := AColor;
  end;
var
  vRight,vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
begin
  vRight := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left - 2;
  vBottom:=ARect.Bottom-2;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // set things up and do the work
    Brush.Color := StringToColor(CurValue);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(CurValue, Index, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            AState);
  end;
end;

procedure TColorPropertyEditor.ListMeasureWidth(const CurValue:ansistring;
  Index:integer;  ACanvas:TCanvas;  var AWidth:Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('M') {* 2};
end;

procedure TColorPropertyEditor.SetValue(const NewValue: ansistring);
var
  CValue: Longint;
begin
  if IdentToColor(NewValue, CValue) then
    SetOrdValue(CValue)
  else
    inherited SetValue(NewValue);
end;

{ TBrushStylePropertyEditor }

procedure TBrushStylePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect;  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TBrushStylePropertyEditor.ListDrawValue(const CurValue: ansistring;
  Index:integer;  ACanvas: TCanvas; const ARect: TRect; AState:TPropEditDrawState);
var
  vRight, vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldBrushStyle: TBrushStyle;
begin
  vRight := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left -2;
  vBottom:= ARect.Bottom-2;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    vOldBrushStyle := Brush.Style;

    // frame things
    Pen.Color := Brush.Color;
    Brush.Color := clWindow;
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // set things up
    Pen.Color := clWindowText;
    Brush.Style := TBrushStyle(GetEnumValue(GetPropInfo^.PropType, CurValue));

    // bsClear hack
    if Brush.Style = bsClear then begin
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := clWindowText;

    // ok on with the show
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Brush.Style := vOldBrushStyle;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(CurValue, Index, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            AState);
  end;
end;

procedure TBrushStylePropertyEditor.ListMeasureWidth(const CurValue: ansistring;
  Index:integer;  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('A') {* 2};
end;

{ TPenStylePropertyEditor }

procedure TPenStylePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect;  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TPenStylePropertyEditor.ListDrawValue(const CurValue: ansistring;
  Index:integer;  ACanvas: TCanvas; const ARect: TRect; AState:TPropEditDrawState);
var
  vRight, vTop, vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldPenStyle: TPenStyle;
begin
  vRight := (ARect.Bottom - ARect.Top) * 2 + ARect.Left;
  vTop := (ARect.Bottom - ARect.Top) div 2 + ARect.Top;
  vBottom := ARect.Bottom-2;
  with ACanvas do
  try
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    vOldPenStyle := Pen.Style;

    // frame things
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // white out the background
    Pen.Color := clWindowText;
    Brush.Color := clWindow;
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);

    // set thing up and do work
    Pen.Color := clWindowText;
    Pen.Style := TPenStyle(GetEnumValue(GetPropInfo^.PropType, CurValue));
    MoveTo(ARect.Left + 1, vTop);
    LineTo(vRight - 1, vTop);
    MoveTo(ARect.Left + 1, vTop + 1);
    LineTo(vRight - 1, vTop + 1);

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Style := vOldPenStyle;
    Pen.Color := vOldPenColor;
  finally
    inherited ListDrawValue(CurValue, -1, ACanvas,
                            Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                            AState);
  end;
end;

procedure TPenStylePropertyEditor.ListMeasureWidth(const CurValue: ansistring;
  Index:integer;  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := AWidth + ACanvas.TextHeight('X') * 2;
end;

{ TFontPropertyEditor }

procedure TFontPropertyEditor.Edit;
var FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    //FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist];
    if FontDialog.Execute then
      SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

function TFontPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;


//------------------------------------------------------------------------------
initialization
  // XXX workaround for missing typeinfo function
  // Normally it should use something like this;
  // RegisterPropertyEditor(TypeInfo(TColor),nil,'',TColorPropertyEditor);
  DummyClassForPropTypes:=TDummyClassForGraphPropTypes.Create;
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TColor'),
    nil,'',TColorPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TBrushStyle'),
    nil,'',TBrushStylePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TPenStyle'),
    nil,'',TPenStylePropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TFont), nil,'',TFontPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TPixmap), nil,'',TPixmapPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), nil,'',TPixmapPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TPicture), nil,'',TPicturePropertyEditor);

  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TSpeedButton,'Glyph',
    TButtonGlyphPropEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TBitBtn,'Glyph',
    TButtonGlyphPropEditor);


finalization
  // XXX workaround for missing typeinfo function
  DummyClassForPropTypes.Free;

end.

