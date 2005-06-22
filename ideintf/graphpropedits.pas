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
  Classes, TypInfo, SysUtils, LCLProc, Forms, Controls, LCLType, GraphType,
  Graphics, StdCtrls, Buttons, ComCtrls, Menus, ExtCtrls, Dialogs, LCLIntf,
  ExtDlgs, ObjInspStrConsts, PropEdits;

type
 {TGraphicPropertyEditor
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
    procedure GetValues(Proc: TGetStringProc); override;
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
    procedure GetValues(Proc: TGetStringProc); override;
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
  TGraphicPropertyEditorForm = class(TForm)
    procedure GraphicPropertyEditorFormResize(Sender: TObject);
  private
    FModified: boolean;
    procedure SetModified(const AValue: boolean);
  protected
    Opendlg: TOpenPictureDialog;
    Savedlg: TSavePictureDialog;

    OKBTN: TBITBTN;
    CANCELBTN: TBITBTN;

    LoadBTN: TBUTTON;
    SaveBTN: TBUTTON;
    ClearBTN : TBUTTON;

    ScrollBox : TScrollBox;

    procedure LoadBTNCLICK(Sender: TObject);
    procedure SaveBTNCLICK(Sender: TObject);
    procedure ClearBTNCLICK(Sender: TObject);

  public
    Preview: TIMAGE;

    FileName : String;
    Constructor Create(AOwner : TComponent); Override;
    property Modified: boolean read FModified write SetModified;
  end;

Constructor TGraphicPropertyEditorForm.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);

  FileName := '';
  Position := poDesktopCenter;

  Caption := oisLoadImageDialog;

  HEIGHT := 419;
  WIDTH  := 403;

  Opendlg := TOpenPictureDialog.Create(Self);

  Savedlg := TSavePictureDialog.Create(Self);

  OKBTN := TBITBTN.Create(Self);
  With OKBTN do begin
    Parent := Self;
    KIND := bkok;
    SPACING := 3;
    MODALRESULT := mrOK;
    CAPTION := oisOK;
  end;

  ScrollBox := TScrollBox.Create(Self);
  With ScrollBox do begin
    Parent := Self;
    AutoSize := False;
    Color := clWhite;
    AutoScroll := True;
  end;

  Preview := TIMAGE.Create(ScrollBox);
  With Preview do begin
    Parent := ScrollBox;
    AutoSize:=true;
    Transparent := True;
    Center := True;
    Stretch := False;
  end;

  CANCELBTN := TBITBTN.Create(Self);
  With CANCELBTN do begin
    Parent := Self;
    KIND := bkcancel;
    SPACING := 3;
    MODALRESULT := mrCancel;
    CAPTION := oisCancel;
  end;

  LoadBTN := TBUTTON.Create(Self);
  With LoadBTN do begin
    Parent := Self;
    CAPTION := oisLoad;
    ONCLICK := @LoadBTNCLICK;
  end;

  SaveBTN := TBUTTON.Create(Self);
  With SaveBTN do begin
    Parent := Self;
    ENABLED := False;
    CAPTION := oisSave;
    ONCLICK := @SaveBTNCLICK;
  end;

  ClearBTN := TBUTTON.Create(Self);
  With ClearBTN do begin
    Parent := Self;
    CAPTION := oisCLear;
    ONCLICK := @ClearBTNCLICK;
  end;
  
  OnResize:=@GraphicPropertyEditorFormResize;
  OnResize(Self);
end;

procedure TGraphicPropertyEditorForm.GraphicPropertyEditorFormResize(
  Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  with ScrollBox do begin
    SetBounds(8,8,Parent.ClientWidth-108,Parent.ClientHeight-43);
  end;

  with OKBTN do begin
    SetBounds(Parent.ClientWidth-95,10,90,Height);
  end;
  with CANCELBTN do begin
    SetBounds(OKBTN.Left,OKBTN.Top+OKBTN.Height+10,OKBTN.Width,Height);
  end;

  x:=5;
  y:=ClientHeight-30;
  w:=(ClientWidth-20) div 3;
  with LoadBTN do begin
    SetBounds(x,y,w,Height);
    inc(x,w+5);
  end;
  with SaveBTN do begin
    SetBounds(x,y,w,Height);
    inc(x,w+5);
  end;
  with ClearBTN do begin
    SetBounds(x,y,w,Height);
    inc(x,w+5);
  end;
end;

procedure TGraphicPropertyEditorForm.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

procedure TGraphicPropertyEditorForm.LoadBTNCLICK(Sender: TObject);
begin
  If OpenDlg.Execute then begin
    FileName := OpenDlg.FileName;
    try
      Preview.Picture.LoadFromFile(FileName);
      Modified:=true;
    except
      on E: Exception do begin
        MessageDlg(oisErrorLoadingImage,
          Format(oisErrorLoadingImage2, ['"', FileName, '"', #13, E.Message]),
          mtError,[mbOk],0);
      end;
    end;
  end;
  SaveBTN.Enabled := False;
  If Assigned(Preview.Picture.Graphic) then
    If not Preview.Picture.Graphic.Empty then
      SaveBTN.Enabled := True;
end;

procedure TGraphicPropertyEditorForm.SaveBTNCLICK(Sender: TObject);
begin
  If SaveDlg.Execute then
    Preview.Picture.SaveToFile(SaveDlg.FileName);
end;

procedure TGraphicPropertyEditorForm.ClearBTNCLICK(Sender: TObject);
begin
  With Preview do begin
    Picture.Graphic := nil;
    Width := 0;
    Height := 0;
  end;
  ScrollBox.Invalidate;
  SaveBTN.Enabled := False;
  Modified:=true;
end;

{ TGraphicPropertyEditor }
procedure TGraphicPropertyEditor.Edit;
var
  TheDialog: TGraphicPropertyEditorForm;
  ABitmap: TBitmap;
  Ext : String;
begin
  //debugln('TGraphicPropertyEditor.Edit');
  ABitmap := TBitmap(GetObjectValue(TBitmap));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
  try
    If (ABitmap <> nil) and not ABitmap.Empty then begin
      With TheDialog.Preview.Picture.Bitmap do begin
        Width := ABitmap.Width;
        Height := ABitmap.Height;
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
        Canvas.Draw(0, 0, ABitmap);
      end;
    end
    else
      ABitmap := nil;
      
    if (TheDialog.ShowModal = mrOK) then begin
      If TheDialog.Preview.Picture.Graphic <> nil then begin
        If TheDialog.Modified and FileExists(TheDialog.FileName) then begin
          Ext := ExtractFileExt(TheDialog.FileName);
          if ABitmap=nil then ABitmap:=TBitmap.Create;
          If (ABitmap is TBitmap)
          and ((AnsiCompareText(Ext, '.xpm') = 0)
            or (AnsiCompareText(Ext, '.bmp') = 0))
          then begin
            ABitmap.LoadFromFile(TheDialog.FileName);
          end else begin
            ABitmap.Width := TheDialog.Preview.Picture.Graphic.Width;
            ABitmap.Height := TheDialog.Preview.Picture.Graphic.Height;
            With ABitmap.Canvas do begin
              Brush.Color := clWhite;
              FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
              Draw(0, 0, TheDialog.Preview.Picture.Graphic);
            end;
          end;
          SetPtrValue(ABitmap);
          Modified;
        end;
      end
      else if ABitmap<>nil then begin
        ABitmap.FreeImage;
        Modified;
      end;
    end;
  finally
    TheDialog.Free;
  end;
end;

function TGraphicPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

{ TPicturePropertyEditor }

procedure TPicturePropertyEditor.Edit;
var
  TheDialog: TGraphicPropertyEditorForm;
  Picture : TPicture;
begin
  Picture := TPicture(GetObjectValue(TPicture));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
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

  Procedure LoadBitmap;
  var
    ext : String;
  begin
    Ext := ExtractFileExt(TheDialog.FileName);
    ABitmap := TBitmap.Create;
    if (AnsiCompareText(Ext, '.xpm') = 0)
    or (AnsiCompareText(Ext, '.bmp') = 0) then begin
      If FileExists(TheDialog.FileName) then
        ABitmap.LoadFromFile(TheDialog.FileName);
    end
    else begin
      ABitmap.Assign(TheDialog.Preview.Picture.Graphic);
      {ABitmap.Width := TheDialog.Preview.Picture.Graphic.Width;
      ABitmap.Height := TheDialog.Preview.Picture.Graphic.Height;
      With ABitmap.Canvas do begin
        Brush.Color := clWhite;
        FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
        Draw(0, 0, TheDialog.Preview.Picture.Graphic);
      end;}
    end;
  end;
  
begin
  debugln('TButtonGlyphPropEditor.Edit');
  ABitmap := TBitmap(GetObjectValue(TBitmap));
  TheDialog := TGraphicPropertyEditorForm.Create(nil);
  try
    If not ABitmap.Empty then begin
      TheDialog.Preview.Picture.Assign(ABitmap);
      {With TheDialog.Preview.Picture.Bitmap do begin
        Width := ABitmap.Width;
        Height := ABitmap.Height;
        Canvas.Brush.Color := clWhite;
        Canvas.FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));
        Canvas.Draw(0, 0, ABitmap);
      end;}
    end;
    if (TheDialog.ShowModal = mrOK) then begin
      If TheDialog.Preview.Picture.Graphic <> nil then begin
        if TheDialog.Modified then begin
          LoadBitmap;
          SetPtrValue(ABitmap);
        end;
      end
      else begin
        ABitmap.Width:=0;
        ABitmap.Height:=0;
      end;
      Modified;
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
    if ColorDialog.Execute then SetOrdValue(ColorDialog.Color);
  finally
    ColorDialog.Free;
  end;
end;

function TColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect,paDialog,paValueList,paRevertable,paHasDefaultValue];
end;

function TColorPropertyEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result := ColorToString(TColor(OrdValue));
end;

procedure TColorPropertyEditor.GetValues(Proc: TGetStringProc);
begin
  GetColorValues(Proc);
end;

procedure TColorPropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect:TRect;
  AState:TPropEditDrawState);
begin
  if GetVisualValue <> '' then begin
    ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  end
  else
    inherited PropDrawValue(ACanvas, ARect, AState);
end;

procedure TColorPropertyEditor.ListDrawValue(const CurValue:ansistring;
  Index:integer; ACanvas:TCanvas;  const ARect:TRect;
  AState: TPropEditDrawState);

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
    else
      if pedsInEdit in AState then begin
        if pedsSelected in AState then
          Result := clWindow
        else
         Result := AColor;
      end else begin
        if pedsSelected in AState then
          Result := clHighlight
        else
         Result := clWindow;
      end;
  end;
var
  vRight,vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  vBottom:=ARect.Bottom-2;
  with ACanvas do begin
    // save off things
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    // frame things
    if pedsInEdit in AState then begin
      if pedsSelected in AState then
        Brush.Color := clWindow
      else
        Brush.Color := ACanvas.Color;
      end else begin
        if pedsSelected in AState then
          Brush.Color := clHighlightText
        else
         Brush.Color := clWindow;
      end;
    Pen.Color := Brush.Color;
    FillRect(ARect);
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // set things up and do the work
    Brush.Color := StringToColor(CurValue);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);
    
    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
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

procedure TFontNamePropertyEditor.GetValues(Proc: TGetStringProc);
var
  I: Integer;
begin
  for I := 0 to Screen.Fonts.Count -1 do
    Proc(Screen.Fonts[I]);
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
  Result:=(inherited GetAttributes)-[paHasDefaultValue];
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
  Result:=(inherited GetAttributes)-[paHasDefaultValue];
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
initialization
  // XXX workaround for missing typeinfo function
  // Normally it should use something like this;
  // RegisterPropertyEditor(TypeInfo(TColor),nil,'',TColorPropertyEditor);
  DummyClassForPropTypes:=TDummyClassForGraphPropTypes.Create;
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TGraphicsColor'),
    nil,'',TColorPropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TBrushStyle'),
    nil,'',TBrushStylePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('TPenStyle'),
    nil,'',TPenStylePropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TFont), nil,'',TFontPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TGraphic), nil,'',TGraphicPropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TPicture), nil,'',TPicturePropertyEditor);
  RegisterPropertyEditor(DummyClassForPropTypes.PTypeInfos('AnsiString'),
    TFont,'Name', TFontNamePropertyEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TSpeedButton,'Glyph',
    TButtonGlyphPropEditor);
  RegisterPropertyEditor(ClassTypeInfo(TBitmap), TBitBtn,'Glyph',
    TButtonGlyphPropEditor);


finalization
  // XXX workaround for missing typeinfo function
  DummyClassForPropTypes.Free;

end.

