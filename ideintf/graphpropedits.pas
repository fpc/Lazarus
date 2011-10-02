{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  Classes, TypInfo, SysUtils, LCLProc, Forms, Controls, LCLType, GraphType,
  FileUtil, Graphics, StdCtrls, Buttons, ComCtrls, Menus, ExtCtrls, Dialogs,
  LCLIntf, ExtDlgs, PropEdits, PropEditUtils, ImgList, Math,
  GraphicPropEdit; // defines TGraphicPropertyEditorForm

type
{ TGraphicPropertyEditor
  The default property editor for all TGraphic's and sub types (e.g. TBitmap,
  TPixmap, TIcon, etc.). }

  TGraphicPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TPicturePropertyEditor
  The default property editor for TPicture}

  TPicturePropertyEditor = class(TGraphicPropertyEditor)
  public
    procedure Edit; override;
  end;

{ TButtonGlyphPropEditor
  The default property editor for the Glyphs of TSpeedButton and TBitBtn }
  TButtonGlyphPropEditor = class(TGraphicPropertyEditor)
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
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure ListMeasureWidth(const CurValue: ansistring; Index: integer;
      ACanvas: TCanvas; var AWidth: Integer);  override;
    procedure ListDrawValue(const CurValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect:TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  end;

{ TBrushStylePropertyEditor
  PropertyEditor editor for TBrush's Style. Provides custom render. }

  TBrushStylePropertyEditor = class(TEnumPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure ListMeasureWidth(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  var AWidth: Integer); override;
    procedure ListDrawValue(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
  end;

{ TPenStylePropertyEditor
  PropertyEditor editor for TPen's Style. Simply provides custom render. }

  TPenStylePropertyEditor = class(TEnumPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
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

{ TFontNamePropertyEditor
  PropertyEditor editor for TFont.Name. Simply provides listing font names. }

  TFontNamePropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TFontCharsetPropertyEditor
  PropertyEditor editor for the TFontCharset properties.
  Displays Charset as constant name if exists, otherwise an integer. }

  TFontCharsetPropertyEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

{ TImageIndexPropertyEditor
  PropertyEditor editor for ImageIndex. Provides list of glyphs. }

  TImageIndexPropertyEditor = class(TIntegerPropertyEditor)
  protected
    function GetImageList: TCustomImageList; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListMeasureHeight(const AValue: ansistring; Index:integer;
      ACanvas:TCanvas; var AHeight: Integer); override;
    procedure ListDrawValue(const CurValue: ansistring; Index:integer;
      ACanvas: TCanvas;  const ARect: TRect; AState: TPropEditDrawState); override;
  end;

//==============================================================================
// Delphi Compatible Property Editor Classnames

type
  TFontNameProperty =       TFontNamePropertyEditor;
  //TFontCharsetProperty =    TFontCharsetPropertyEditor;
  TColorProperty =          TColorPropertyEditor;
  TBrushStyleProperty =     TBrushStylePropertyEditor;
  TPenStyleProperty =       TPenStylePropertyEditor;
  TFontProperty =           TFontPropertyEditor;

implementation

{ TGraphicPropertyEditor }

procedure TGraphicPropertyEditor.Edit;
var
  TheDialog: TGraphicPropertyEditorForm;
  AGraphic: TGraphic;
  FreeGraphic: Boolean;
begin
  AGraphic := TGraphic(GetObjectValue(TGraphic));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
  FreeGraphic:=false;
  try
    if (AGraphic <> nil) then
      TheDialog.Preview.Picture.Assign(AGraphic)
    else
      AGraphic := nil;

    if (TheDialog.ShowModal = mrOK) then
    begin
      if TheDialog.Preview.Picture.Graphic <> nil then
      begin
        if TheDialog.Modified and FileExistsUTF8(TheDialog.FileName) then
        begin
          FreeGraphic := AGraphic = nil;

          if FreeGraphic then 
            AGraphic := TGraphicClass(GetTypeData(GetPropType)^.ClassType).Create;

          if AGraphic.ClassType = TheDialog.Preview.Picture.Graphic.ClassType then
            AGraphic.LoadFromFile(TheDialog.FileName)
          else
            AGraphic.Assign(TheDialog.Preview.Picture.Graphic);

          SetPtrValue(AGraphic);
          Modified;
        end;
      end
      else
      if AGraphic <> nil then
      begin
        AGraphic.Clear;
        Modified;
      end;
    end;
  finally
    if FreeGraphic then
      AGraphic.Free;
    TheDialog.Free;
  end;
end;

function TGraphicPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

{ TPicturePropertyEditor }

procedure TPicturePropertyEditor.Edit;

  procedure AddPackage(Picture: TPicture);
  begin
    if Picture.Graphic=nil then exit;
    //DebugLn(['AddPackage ',dbgsname(Picture.Graphic)]);
    GlobalDesignHook.AddDependency(Picture.Graphic.ClassType,'');
  end;

var
  TheDialog: TGraphicPropertyEditorForm;
  Picture: TPicture;
begin
  Picture := TPicture(GetObjectValue(TPicture));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
  if (Picture.Graphic <> nil) then
    TheDialog.Preview.Picture.Graphic := Picture.Graphic;
  try
    if (TheDialog.ShowModal = mrOK) then
    begin
      if TheDialog.Preview.Picture.Graphic <> nil then
      begin
        if TheDialog.FileName <> '' then
          if FileExistsUTF8(TheDialog.FileName) then
          begin
            Picture.LoadFromFile(TheDialog.FileName);
            AddPackage(Picture);
          end;
      end
      else
        Picture.Graphic := nil;
      Modified;
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TButtonGlyphPropEditor }

procedure TButtonGlyphPropEditor.Edit;
var
  TheDialog: TGraphicPropertyEditorForm;
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap(GetObjectValue(TBitmap));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
  try
    if not ABitmap.Empty then
      TheDialog.Preview.Picture.Assign(ABitmap);
    if (TheDialog.ShowModal = mrOK) then
    begin
      if TheDialog.Modified then
      begin
        ABitmap.Assign(TheDialog.Preview.Picture.Graphic);
        Modified;
      end;
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TColorPropertyEditor }

procedure TColorPropertyEditor.Edit;
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := GetOrdValue;
    if ColorDialog.Execute then
      SetOrdValue(ColorDialog.Color);
  finally
    ColorDialog.Free;
  end;
end;

function TColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paDialog,paValueList,paCustomDrawn,paRevertable];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

function TColorPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result := ColorToString(TColor(OrdValue));
end;

procedure TColorPropertyEditor.GetValues(Proc: TGetStrProc);
var
  CValue: Longint;
begin
  if not IdentToColor(GetVisualValue, CValue) then Proc(GetVisualValue);
  GetColorValues(Proc);
end;

procedure TColorPropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TColorPropertyEditor.ListDrawValue(const CurValue:ansistring;
  Index:integer; ACanvas:TCanvas;  const ARect:TRect;
  AState: TPropEditDrawState);

  function ColorToBorderColor(AColor: TColorRef): TColor;
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
    else
      if pedsInEdit in AState then
      begin
        if pedsSelected in AState then
          Result := clWindow
        else
         Result := TColor(AColor);
      end else
      begin
        if pedsSelected in AState then
          Result := clHighlight
        else
         Result := clWindow;
      end;
  end;
var
  vRight, vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldPenStyle: TPenStyle;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  vBottom:=ARect.Bottom-2;
  with ACanvas do
  begin
    // save off things
    vOldPenStyle := Pen.Style;
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    // frame things
    if pedsInEdit in AState then
    begin
      if pedsSelected in AState then
        Brush.Color := clWindow
      else
        Brush.Color := ACanvas.Brush.Color;
    end
    else
    begin
      if pedsSelected in AState then
        Brush.Color := clHighlightText
      else
       Brush.Color := clWindow;
    end;
    Pen.Color := Brush.Color;
    Pen.Style := psSolid;
    FillRect(ARect);
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // set things up and do the work
    Brush.Color := StringToColor(CurValue);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);
    
    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
    Pen.Style := vOldPenStyle;
  end;
  inherited ListDrawValue(CurValue, Index, ACanvas,
                          Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
                          AState);
end;

procedure TColorPropertyEditor.ListMeasureWidth(const CurValue:ansistring;
  Index:integer; ACanvas:TCanvas;  var AWidth:Integer);
begin
  AWidth := ACanvas.TextWidth('clGradientInactiveCaption')+25;
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

function TFontNamePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TFontNamePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count -1 do
    Proc(Screen.Fonts[I]);
end;

{ TFontCharsetPropertyEditor }

function TFontCharsetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=[paMultiSelect,paSortList,paValueList,paRevertable,paHasDefaultValue];
end;

function TFontCharsetPropertyEditor.OrdValueToVisualValue(OrdValue: longint
  ): string;
begin
  Result := CharsetToString(OrdValue);
end;

procedure TFontCharsetPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  proc(CharsetToString(ANSI_CHARSET));
  proc(CharsetToString(DEFAULT_CHARSET));
  proc(CharsetToString(SYMBOL_CHARSET));
  proc(CharsetToString(MAC_CHARSET));
  proc(CharsetToString(SHIFTJIS_CHARSET));
  proc(CharsetToString(HANGEUL_CHARSET));
  proc(CharsetToString(JOHAB_CHARSET));
  proc(CharsetToString(GB2312_CHARSET));
  proc(CharsetToString(CHINESEBIG5_CHARSET));
  proc(CharsetToString(GREEK_CHARSET));
  proc(CharsetToString(TURKISH_CHARSET));
  proc(CharsetToString(VIETNAMESE_CHARSET));
  proc(CharsetToString(HEBREW_CHARSET));
  proc(CharsetToString(ARABIC_CHARSET));
  proc(CharsetToString(BALTIC_CHARSET));
  proc(CharsetToString(RUSSIAN_CHARSET));
  proc(CharsetToString(THAI_CHARSET));
  proc(CharsetToString(EASTEUROPE_CHARSET));
  proc(CharsetToString(OEM_CHARSET));
  proc(CharsetToString(FCS_ISO_10646_1));
end;

procedure TFontCharsetPropertyEditor.SetValue(const NewValue: ansistring);
var
  CValue: Longint;
begin
  if not SameText(NewValue, 'DEFAULT_CHARSET') then
  begin
    CValue := StringToCharset(NewValue);
    if CValue = DEFAULT_CHARSET then
      inherited SetValue(NewValue)
    else
      SetOrdValue(CValue);
  end
  else
    SetOrdValue(DEFAULT_CHARSET);
end;

{ TBrushStylePropertyEditor }

procedure TBrushStylePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect;  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
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

function TBrushStylePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)-[paHasDefaultValue]+[paCustomDrawn];
end;

procedure TBrushStylePropertyEditor.ListMeasureWidth(const CurValue: ansistring;
  Index:integer; ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := 130;
end;

{ TPenStylePropertyEditor }

procedure TPenStylePropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect;  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TPenStylePropertyEditor.ListDrawValue(const CurValue: ansistring;
  Index:integer;  ACanvas: TCanvas;
  const ARect: TRect; AState:TPropEditDrawState);
var
  vRight, vTop, vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldPenStyle: TPenStyle;
  i: Integer;
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
    i:=GetEnumValue(GetPropInfo^.PropType, CurValue);
    Pen.Style := TPenStyle(i);
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

function TPenStylePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)-[paHasDefaultValue]+[paCustomDrawn];
end;

procedure TPenStylePropertyEditor.ListMeasureWidth(const CurValue: ansistring;
  Index:integer; ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := 130;
end;

{ TFontPropertyEditor }

procedure TFontPropertyEditor.Edit;
var FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(nil);
  try
    FontDialog.Font := TFont(GetObjectValue(TFont));
    FontDialog.Options := FontDialog.Options + [fdShowHelp, fdForceFontExist];
    if FontDialog.Execute then
      SetPtrValue(FontDialog.Font);
  finally
    FontDialog.Free;
  end;
end;

function TFontPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;


//------------------------------------------------------------------------------

{ TImageIndexPropertyEditor }

function TImageIndexPropertyEditor.GetImageList: TCustomImageList;
var
  Persistent: TPersistent;
  Component: TComponent absolute Persistent;
  PropInfo: PPropInfo;
  Obj: TObject;
begin
  Result := nil;
  Persistent := GetComponent(0);
  if not (Persistent is TComponent) then
    Exit;

  if Component is TMenuItem then
  begin
    Component := Component.GetParentComponent;
    while (Component <> nil) do
    begin
      if (Component is TMenuItem) and (TMenuItem(Component).SubMenuImages <> nil) then
        Exit(TMenuItem(Component).SubMenuImages);
      if (Component is TMenu) then
        Exit(TMenu(Component).Images);
      Component := Component.GetParentComponent;
    end;
  end
  else
  begin
    Component := Component.GetParentComponent;
    if Component = nil then
      Exit;
    PropInfo := TypInfo.GetPropInfo(Component, 'Images');
    if PropInfo = nil then
      Exit;
    Obj := GetObjectProp(Component, PropInfo);
    if Obj is TCustomImageList then
      Exit(TCustomImageList(Obj));
  end;
end;

function TImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paCustomDrawn, paRevertable];
  if GetDefaultOrdValue <> NoDefaultValue then
    Result := Result + [paHasDefaultValue];
end;

procedure TImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Images: TCustomImageList;
  I: Integer;
begin
  Proc(IntToStr(GetDefaultOrdValue));
  Images := GetImageList;
  if Assigned(Images) then
    for I := 0 to Images.Count - 1 do
      Proc(IntToStr(I));
end;

procedure TImageIndexPropertyEditor.ListMeasureHeight(const AValue: ansistring;
  Index: integer; ACanvas: TCanvas; var AHeight: Integer);
var
  Images: TCustomImageList;
begin
  AHeight := ACanvas.TextHeight('1');
  Images := GetImageList;
  if Assigned(Images) then
    AHeight := Max(AHeight, Images.Height + 2);
end;

procedure TImageIndexPropertyEditor.ListDrawValue(const CurValue: ansistring;
  Index: integer; ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
  Images: TCustomImageList;
  R: TRect;
  OldColor: TColor;
begin
  Dec(Index);
  Images := GetImageList;
  R := ARect;
  if Assigned(Images) then
  begin
    if (pedsInComboList in AState) and not (pedsInEdit in AState) then
    begin
      OldColor := ACanvas.Brush.Color;
      if pedsSelected in AState then
        ACanvas.Brush.Color := clHighlight
      else
        ACanvas.Brush.Color := clWhite;
      ACanvas.FillRect(R);
      ACanvas.Brush.Color := OldColor;
    end;

    Images.Draw(ACanvas, R.Left + 1, R.Top + 1, Index, True);
    R.Left := R.Left + Images.Width + 2;
  end;
  inherited ListDrawValue(CurValue, Index, ACanvas, R, AState);
end;

initialization
  RegisterPropertyEditor(TypeInfo(TGraphicsColor), nil, '', TColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPenStyle), nil, '', TPenStylePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBrushStyle), nil, '', TBrushStylePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TFont, 'Name', TFontNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFontCharset), nil, 'CharSet', TFontCharsetPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TComponent, 'ImageIndex', TImageIndexPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TFont), nil,'',TFontPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TGraphic), nil,'',TGraphicPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TPicture), nil,'',TPicturePropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TSpeedButton,'Glyph', TButtonGlyphPropEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TBitBtn,'Glyph', TButtonGlyphPropEditor);

end.

