{******************************************************************************
                         All GNOME Winapi implementations.
                   Initial Revision  : Thu Oct 3 1:35:53 2002


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$EndIf}

Function TGnomeObject.LoadStockPixmap(StockID: longint) : HBitmap;
var
  Pixmap : PGDIObject;
  StockName : PChar;
begin
  Case StockID Of
    idButtonOk : StockName := GNOME_STOCK_BUTTON_OK;
    idButtonCancel : StockName := GNOME_STOCK_BUTTON_CANCEL;
    idButtonYes : StockName := GNOME_STOCK_BUTTON_YES;
    idButtonNo : StockName := GNOME_STOCK_BUTTON_NO;
    idButtonHelp : StockName := GNOME_STOCK_BUTTON_HELP;
    idButtonAbort : StockName := GNOME_STOCK_BUTTON_CANCEL;
    idButtonClose : StockName := GNOME_STOCK_PIXMAP_QUIT;
    else begin
      Result := inherited LoadStockPixmap(StockID);
      exit;
    end;
  end;
  Pixmap := NewGDIObject(gdiBitmap);
  With Pixmap^ do begin
    GDIBitmapType := gbPixmap;
    gnome_stock_pixmap_gdk(STOCKName, nil, @GDIPixmapObject, @GDIBitmapMaskObject);
  end;
  Result := HBitmap(Pixmap);
end;

{$IfDef ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$EndIf}

{
  $Log$
  Revision 1.2  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  
}
