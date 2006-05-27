{ $Id$}
{
 *****************************************************************************
 *                               GtkWSArrow.pp                               * 
 *                               -------------                               * 
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
unit GtkWSArrow;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF gtk2}
  glib2, gdk2pixbuf, gdk2, gtk2, Pango,
  {$ELSE}
  glib, gdk, gtk, {$Ifndef NoGdkPixbufLib}gdkpixbuf,{$EndIf} GtkFontCache,
  {$ENDIF}
  Arrow, WSArrow, WSLCLClasses;

type

  { TGtkWSArrow }

  TGtkWSArrow = class(TWSArrow)
  private
  protected
  public
    class procedure SetType(const AArrow: TArrow; const AArrowType: TArrowType; 
      const AShadowType: TShadowType); override;
  end;


implementation

{ TGtkWSArrow }

class procedure TGtkWSArrow.SetType(const AArrow: TArrow; const AArrowType: TArrowType;
  const AShadowType: TShadowType);
var
  ArrowType : TGTKArrowType;
  ShadowType : TGTKShadowType;
begin
  case AArrowType of
    atUp:    ArrowType := GTK_ARROW_UP;
    atLeft:  ArrowType := GTK_ARROW_LEFT;
    atRight: ArrowType := GTK_ARROW_RIGHT;
  else
             ArrowType := GTK_ARROW_DOWN;
  end;

  case AShadowType of
    stNONE : ShadowType := GTK_SHADOW_NONE;
    stIN : ShadowType := GTK_SHADOW_IN;
    stOut : ShadowType := GTK_SHADOW_OUT;
    stEtchedIn : ShadowType := GTK_SHADOW_ETCHED_IN;
    stEtchedOut : ShadowType := GTK_SHADOW_ETCHED_OUT;
  else
    ShadowType := GTK_SHADOW_NONE;
  end;

  gtk_arrow_set(PGtkArrow(AArrow.Handle), ArrowType, ShadowType);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TArrow, TGtkWSArrow);
////////////////////////////////////////////////////
end.