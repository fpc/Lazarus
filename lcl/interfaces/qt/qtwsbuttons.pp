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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets, qtobjects,
  // LCL
  SysUtils, Controls, LCLType, Forms, InterfaceBase, Buttons, LMessages, Graphics, GraphType,
  // Widgetset
  WSProc, WSButtons, WSLCLClasses;

type

  { TQtWSBitBtn }

  TQtWSBitBtn = class(TWSBitBtn)
  private
  protected
  public
    class procedure SetGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph); override;
  end;

  { TQtWSSpeedButton }

  TQtWSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
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
var
  AIcon: QIconH;
  APixmap: QPixmapH;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
begin
  APixmap := QPixmap_create();

  AGlyph := TBitmap.Create;
  AValue.GetImageIndexAndEffect(bsUp, AIndex, AEffect);
  AValue.Images.GetBitmap(AIndex, AGlyph, AEffect);

  QPixmap_fromImage(APixmap, TQtImage(AGlyph.Handle).Handle);
  try
    if APixmap <> nil then
    begin
      AIcon := QIcon_create(APixmap);
      TQtAbstractButton(ABitBtn.Handle).setIcon(AIcon);
    end;
  finally
    QPixmap_destroy(APixmap);
    AGlyph.Free;
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomBitBtn, TQtWSBitBtn);
//  RegisterWSComponent(TCustomSpeedButton, TQtWSSpeedButton);
////////////////////////////////////////////////////
end.
