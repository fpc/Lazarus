{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSButtons.pp                              * 
 *                             ----------------                              * 
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
unit Gtk2WSButtons;

{$mode objfpc}{$H+}

interface

uses
  glib2, gtk2, gdk2, gdk2pixbuf, Gtk2WSPrivate,
////////////////////////////////////////////////////
  Buttons, Graphics, GraphType,
////////////////////////////////////////////////////
  WSButtons, WSLCLClasses, GtkWSButtons, GtkDef, GtkDebug;

type

  { TGtk2WSBitBtn }

  TGtk2WSBitBtn = class(TGtkWSBitBtn)
  private
  protected
    class procedure UpdateGlyph(const ABitBtn: TCustomBitBtn; const AValue: TButtonGlyph; const AButtonState: TButtonState); override;
  public
  end;

  { TGtk2WSSpeedButton }

  TGtk2WSSpeedButton = class(TWSSpeedButton)
  private
  protected
  public
  end;


implementation

uses
  GtkProc;

{ TGtk2WSBitBtn }

class procedure TGtk2WSBitBtn.UpdateGlyph(const ABitBtn: TCustomBitBtn;
  const AValue: TButtonGlyph; const AButtonState: TButtonState);
var
  WidgetInfo: PWidgetInfo;
  BitBtnInfo: PBitBtnWidgetInfo;
  GDIObject: PGDIObject;
  Pixbuf: PGdkPixbuf;
  Mask: PGdkBitmap;
  AGlyph: TBitmap;
  AIndex: Integer;
  AEffect: TGraphicsDrawEffect;
begin
  WidgetInfo := GetWidgetInfo(Pointer(ABitBtn.Handle));
  BitBtnInfo := WidgetInfo^.UserData;

  if ABitBtn.CanShowGlyph then
  begin
    AGlyph := TBitmap.Create;
    AValue.GetImageIndexAndEffect(AButtonState, AIndex, AEffect);
    if (AIndex <> -1) and (AValue.Images <> nil) then
      AValue.Images.GetBitmap(AIndex, AGlyph, AEffect);
  end
  else
    AGlyph := nil;
  // check if an image is needed
  if (AGlyph = nil) or AGlyph.Empty
  then begin
    if BitBtnInfo^.ImageWidget <> nil
    then begin
      gtk_container_remove(BitBtnInfo^.TableWidget, BitBtnInfo^.ImageWidget);
      BitBtnInfo^.ImageWidget := nil;
    end;
    AGlyph.Free;
    Exit;
  end;

  GDIObject := PGDIObject(AGlyph.Handle);
  Mask := nil;
  Pixbuf := nil;
  if GDIObject^.GDIBitmapType = gbPixbuf then
    Pixbuf := GDIObject^.GDIPixbufObject
  else
    Mask := CreateGdkMaskBitmap(AGlyph.Handle, AGlyph.MaskHandle);
  // check for image
  if BitBtnInfo^.ImageWidget = nil
  then begin
    BitBtnInfo^.ImageWidget := gtk_image_new;
    gtk_widget_show(BitBtnInfo^.ImageWidget);
    UpdateLayout(BitBtnInfo, ABitBtn.Layout, ABitBtn.Margin);
  end;

  if Pixbuf <> nil then
  begin
    gtk_image_set_from_pixbuf(BitBtnInfo^.ImageWidget, Pixbuf);
    //DbgDumpPixbuf(Pixbuf);
  end
  else
  begin
    gtk_image_set_from_pixmap(BitBtnInfo^.ImageWidget, GDIObject^.GDIPixmapObject.Image, Mask);
    //DbgDumpPixmap(GDIObject^.GDIPixmapObject.Image);
    //DbgDumpBitmap(Mask);
  end;

  if Mask <> nil then
    gdk_pixmap_unref(Mask);
  AGlyph.Free;
end;

end.
