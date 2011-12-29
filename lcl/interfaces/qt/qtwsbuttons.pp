{ $Id$}
{
 *****************************************************************************
 *                              QtWSButtons.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit QtWSButtons;

{$mode objfpc}{$H+}

interface

{$I qtdefines.inc}

uses
  // Libs
  qt4,
  qtwidgets, qtobjects,
  // RTL
  SysUtils, Types,
  // LCL
  Controls, LCLType, Forms, InterfaceBase, Buttons, Graphics, GraphType,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses;

type

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
    class procedure SetLayout(const ABitBtn: TCustomBitBtn; const AValue: TButtonLayout); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

uses QtWSControls;

{ TQtWSBitBtn }

class function TQtWSBitBtn.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  QtBitBtn: TQtBitBtn;
begin
  QtBitBtn := TQtBitBtn.Create(AWinControl, AParams);
  QtBitBtn.AttachEvents;
  Result := TLCLIntfHandle(QtBitBtn);
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
  APixmap: QPixmapH;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
  Mode: QIconMode;
  ASize: TSize;
begin
  if not WSCheckHandleAllocated(ABitBtn, 'SetGlyph') then
    Exit;

  TQtBitBtn(ABitBtn.Handle).GlyphLayout := Ord(ABitBtn.Layout);
  AIcon := QIcon_create();
  if ABitBtn.CanShowGlyph then
  begin
    AGlyph := TBitmap.Create;
    APixmap := QPixmap_create();

    for Mode := QIconNormal to QIconSelected do
    begin
      AValue.GetImageIndexAndEffect(IconModeToButtonState[Mode], AIndex, AEffect);
      AValue.Images.GetBitmap(AIndex, AGlyph, AEffect);
      QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).FHandle);
      QIcon_addPixmap(AIcon, APixmap, Mode, QIconOn);
    end;
    QPixmap_destroy(APixmap);
    AGlyph.Free;

    ASize.cx := AValue.Images.Width;
    ASize.cy := AValue.Images.Height;
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

end.
