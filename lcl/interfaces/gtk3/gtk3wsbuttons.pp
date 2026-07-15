{
 *****************************************************************************
 *                               Gtk3WSButtons.pp                            *
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSButtons;

{$mode objfpc}{$H+}
{$I gtk3defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Classes, Controls, Buttons, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, WSButtons, WSProc,
  Gtk3WSControls, LCLType, LCLIntf, LCLProc,
  gtk3widgets, LazGtk3, LazGdk3, LazGdkPixbuf2, LazGObject2;

type

  { TGtk3WSBitBtn }

  TGtk3WSBitBtn = class(TWSBitBtn)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl;
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
  end;
  TGtk3WSBitBtnClass = class of TGtk3WSBitBtn;

  { TGtk3WSSpeedButton }

  TGtk3WSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;

implementation

uses
  Forms, graphtype,imglist,LResources, gtk3objects, gtk3procs;


procedure ApplyGlyphTransparentKey(ASourceGlyph: TBitmap; ATargetBitmap: TBitmap);
const
  EDGE_ALPHA = 64; //maybe not correct
var
  Pixbuf: PGdkPixbuf;
  W, H, Stride, NChan: integer;
  Pixels, Row, P, NP: PByte;
  X, Y, dx, dy, NX, NY: integer;
  KeyR, KeyG, KeyB: Byte;
  AllKey: Boolean;
  IsKey: Boolean;
  Mark: array of Byte;
begin

  if (ASourceGlyph = nil) or ASourceGlyph.Empty then
    exit;

  if (ATargetBitmap = nil) or ATargetBitmap.Empty then
    exit;

  if ASourceGlyph.RawImage.Description.AlphaPrec > 0 then
    exit;

  Pixbuf := TGtk3Image(ATargetBitmap.Handle).Handle;

  if Pixbuf = nil then
    exit;

  if not Pixbuf^.get_has_alpha then
    exit;

  W := Pixbuf^.get_width;
  H := Pixbuf^.get_height;

  if (W < 1) or (H < 1) then
    exit;

  NChan := Pixbuf^.get_n_channels;

  if NChan < 4 then
    exit;

  Stride := Pixbuf^.get_rowstride;
  Pixels := Pixbuf^.get_pixels;
  Row := Pixels + (H - 1) * Stride;

  //writeln('ApplyGlyphTranspkey: Stride=',Stride,' Row[0]=',Row[0],' Row[1]=',Row[1],' Row[2]=',Row[2],' NChan=',NChan,' W=',W,' H=',H);
  //writeln('ApplyGlyphTranspkey: Stride=',Stride,' Pixels=',Pixels,' Row=',Row,' NChan=',NChan);

  KeyR := Row[0];
  KeyG := Row[1];
  KeyB := Row[2];
  SetLength(Mark, W * H);

  for Y := 0 to H - 1 do
  begin
    Row := Pixels + Y * Stride;
    for X := 0 to W - 1 do
    begin
      P := Row + X * NChan;
      IsKey := (P[0] = KeyR) and (P[1] = KeyG) and (P[2] = KeyB);

      if not IsKey then
      begin
        Mark[Y * W + X] := 255;
        continue;
      end;

      AllKey := True;
      for dy := -1 to 1 do
      begin
        for dx := -1 to 1 do
        begin

          if (dx = 0) and (dy = 0) then
            continue;

          if (dx <> 0) and (dy <> 0) then
            continue;

          NX := X + dx;
          NY := Y + dy;

          if (NX < 0) or (NX >= W) or (NY < 0) or (NY >= H) then
            continue;

          NP := Pixels + NY * Stride + NX * NChan;

          if (NP[0] <> KeyR) or (NP[1] <> KeyG) or (NP[2] <> KeyB) then
          begin
            AllKey := False;
            break;
          end;

        end;
        if not AllKey then
          break;
      end;
      if AllKey then
        Mark[Y * W + X] := 0
      else
        Mark[Y * W + X] := EDGE_ALPHA;
    end;
  end;
  for Y := 0 to H - 1 do
  begin
    Row := Pixels + Y * Stride;
    for X := 0 to W - 1 do
      if Mark[Y * W + X] <> 255 then
      begin
        P := Row + X * NChan;
        P[3] := Mark[Y * W + X];
      end;
  end;
end;


{ TGtk3WSCustomBitBtn }

class function TGtk3WSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  ABitBtn: TGtk3Button;
  ARect: TGdkRectangle;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.CreateHandle');
  {$ENDIF}
  ABitBtn := TGtk3Button.Create(AWinControl, AParams);
{  with ARect do
  begin
    x := AWinControl.Left;
    y := AWinControl.Top;
    width := AWinControl.Width;
    height := AWinControl.Height;
  end;

  ABitBtn.Widget^.set_allocation(@ARect);}

  Result := TLCLHandle(ABitBtn);
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.CreateHandle Handle=',dbgs(Result));
  {$ENDIF}
end;

class procedure TGtk3WSBitBtn.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then
    Exit;
  TGtk3Button(AWinControl.Handle).preferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

class procedure TGtk3WSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph);
var
  AImage: PGtkImage;
  AGlyph: TBitmap;
  resolution: TCustomImageListResolution;
  ScaleFactor: Double;
  raw: TRawImage;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  AImageRes: TScaledImageListResolution;
  APPI, NominalW, TargetW, TargetH: Integer;
  SrcPixbuf, ScaledPixbuf: PGdkPixbuf;
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.SetGlyph');
  {$ENDIF}
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;
  if ABitBtn.CanShowGlyph(True) then
  begin
    { allocate image which would be cached in TGtk3Button instance }
    AGlyph := TBitmap.Create;
    ScaleFactor := ABitBtn.GetCanvasScaleFactor;
    if (ABitBtn.ImageIndex>=0) and Assigned(ABitBtn.Images) then
    begin
      { find imagelist scaled }
      resolution := ABitBtn.Images.Resolution[round(AbitBtn.Images.Width*ScaleFactor)];
      resolution.GetRawImage(ABitBtn.ImageIndex,raw);
      AGlyph.BeginUpdate();
      AGlyph.LoadFromRawImage(raw,false);
      AGlyph.EndUpdate();
    end else
    begin
      AGlyph.BeginUpdate();
      AGlyph.LoadFromRawImage(AValue.Glyph.RawImage, false);
      AGlyph.EndUpdate();
      //issue #42260
      ApplyGlyphTransparentKey(AValue.Glyph, AGlyph);
    end;

    if AGlyph.Empty then
    begin
      AValue.GetImageIndexAndEffect(bsUp, ABitBtn.Font.PixelsPerInch,
        ScaleFactor, AImageRes, AIndex, AEffect);
      if (AIndex <> -1) and AImageRes.Valid then
        AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
    end;

    if not AGlyph.Empty then
    begin
      SrcPixbuf := TGtk3Image(AGlyph.Handle).Handle;
      ScaledPixbuf := nil;
      APPI := Screen.PixelsPerInch;
      if APPI <= 0 then
        APPI := 96;
      NominalW := MulDiv(16, APPI, 96);
      if ScaleFactor > 1 then
        NominalW := Round(NominalW * ScaleFactor);
      if (AGlyph.Width > 0) and (AGlyph.Width < NominalW) then
      begin
        TargetW := NominalW;
        TargetH := Round(AGlyph.Height * (TargetW / AGlyph.Width));
        ScaledPixbuf := SrcPixbuf^.scale_simple(TargetW, TargetH,
          GDK_INTERP_BILINEAR);
      end;
      if ScaledPixbuf <> nil then
      begin
        AImage := gtk_image_new_from_pixbuf(ScaledPixbuf);
        g_object_unref(ScaledPixbuf);
      end else
        AImage := gtk_image_new_from_pixbuf(SrcPixbuf);
    end else
      AImage := nil;

    if Assigned(AImage) then
      gtk_button_set_image(PGtkButton(TGtk3Button(ABitBtn.Handle).Widget), AImage)
    else
      PGtkButton(TGtk3Button(ABitBtn.Handle).Widget)^.always_show_image:=false;

    { store glyph to prevent leaks }
    TGtk3Button(ABitBtn.Handle).Image:=AGlyph;
  end else
  begin
    gtk_button_set_image(PGtkButton(TGtk3Button(ABitBtn.Handle).Widget), nil);
    TGtk3Button(ABitBtn.Handle).Image := nil;
  end;
end;

class procedure TGtk3WSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.SetLayout');
  {$ENDIF}
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then
    Exit;
  TGtk3Button(ABitBtn.Handle).Layout := Ord(AValue);
end;

class procedure TGtk3WSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.SetMargin');
  {$ENDIF}
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin') then
    Exit;
  TGtk3Button(ABitBtn.Handle).Margin := AValue;
end;

class procedure TGtk3WSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  {$IFDEF GTK3DEBUGCORE}
  DebugLn('TGtk3WSBitBtn.SetSpacing');
  {$ENDIF}
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing') then
    Exit;
  TGtk3Button(ABitBtn.Handle).Spacing := AValue;
end;

end.
