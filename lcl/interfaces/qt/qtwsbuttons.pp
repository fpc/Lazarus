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
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  published
  end;


implementation

uses QtWSControls;

{ TQtWSBitBtn }

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
    TQtAbstractButton(ABitBtn.Handle).setIconSize(@ASize);
  end;

  TQtAbstractButton(ABitBtn.Handle).setIcon(AIcon);
  QIcon_destroy(AIcon);
end;

end.
