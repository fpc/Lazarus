{
 *****************************************************************************
 *                              QtWSButtons.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSButtons;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Libs
  qt5,
  qtwidgets, qtobjects,
  // RTL
  SysUtils, Types,
  // LCL
  Controls, LCLType, Forms, InterfaceBase, Buttons, Graphics, ImgList,
  // LazUtils
  GraphType,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses;

type

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
    class procedure SetMargin(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
    class procedure SetSpacing(const ABitBtn: TCustomBitBtn; const AValue: Integer); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation


{ TQtWSBitBtn }

class function TQtWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  QtBitBtn: TQtBitBtn;
begin
  QtBitBtn := TQtBitBtn.Create(AWinControl, AParams);
  QtBitBtn.AttachEvents;
  Result := TLCLHandle(QtBitBtn);
end;

{------------------------------------------------------------------------------
  Function: TQtWSBitBtn.SetGlyph
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSBitBtn.SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph);
const
  IconModeToButtonState: array[QIconMode] of TButtonState =
  (
{ QIconNormal   } bsUp,
{ QIconDisabled } bsDisabled,
{ QIconActive   } bsHot,
{ QIconSelected } bsDown
  );

var
  AIcon: QIconH;
  APixmap, AScaledPixmap: QPixmapH;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  Mode: QIconMode;
  ASize: TSize;
  AImageRes: TScaledImageListResolution;
  DPR: Double;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;

  DPR := ABitBtn.GetCanvasScaleFactor;

  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  AIcon := QIcon_create();
  if ABitBtn.CanShowGlyph(True) then
  begin
    AGlyph := TBitmap.Create;


    for Mode := QIconNormal to QIconSelected do
    begin
      APixmap := QPixmap_create();
      AValue.GetImageIndexAndEffect(IconModeToButtonState[Mode],
        ABitBtn.Font.PixelsPerInch, 1.0 {we must rescale and apply DPR !},
        AImageRes, AIndex, AEffect);
      AImageRes.GetBitmap(AIndex, AGlyph, AEffect);
      QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).Handle);
      QIcon_addPixmap(AIcon, APixmap, Mode, QIconOn);

      if DPR > 1.0 then
      begin
        if QPixmap_devicePixelRatio(APixmap) <> DPR then
        begin
          AScaledPixmap := QPixmap_Create();
          QPixmap_scaled(APixmap, AScaledPixmap, Round(QPixmap_width(APixmap) * DPR), Round(QPixmap_height(APixmap) * DPR), QtIgnoreAspectRatio, QtSmoothTransformation);
          QPixmap_setDevicePixelRatio(AScaledPixmap, DPR);
          QIcon_addPixmap(AIcon, AScaledPixmap, Mode, QIconOn);
          QPixmap_destroy(AScaledPixmap);
        end;
      end;
      QPixmap_destroy(APixmap);
    end;

    AGlyph.Free;

    ASize.cx := AImageRes.Width;
    ASize.cy := AImageRes.Height;
    TQtBitBtn(ABitBtn.Handle).setIconSize(@ASize);
  end;

  TQtBitBtn(ABitBtn.Handle).setIcon(AIcon);
  QIcon_destroy(AIcon);
end;

class procedure TQtWSBitBtn.SetLayout(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonLayout);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetLayout') then
    Exit;
  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

class procedure TQtWSBitBtn.SetMargin(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetMargin') then
    Exit;
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

class procedure TQtWSBitBtn.SetSpacing(const ABitBtn: TCustomBitBtn;
  const AValue: Integer);
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetSpacing') then
    Exit;
  if TQtBitBtn(ABitBtn.Handle).getVisible then
    TQtBitBtn(ABitBtn.Handle).Update(nil);
end;

end.
