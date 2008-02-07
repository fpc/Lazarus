{%MainUnit gtk2int.pp}
{ $Id$ }

{******************************************************************************
                 All GTK2 interface communication implementations.
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to gtk2proc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

procedure TGtk2WidgetSet.DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer);
var
  X, Y: Integer;
  W, H: Integer;
  SavedDC: Integer;
begin
  SavedDC := SaveDC(DC);
  try
    W := (R.Right - R.Left - 1) div DX;
    H := (R.Bottom - R.Top - 1) div DY;

    // remove rows from clip rect
    for Y := 0 to H do
    begin
      ExcludeClipRect(DC, R.Left, R.Top + Y * DY + 1, R.Right + 1, R.Top + (Y + 1) * DY);
    end;

    // draw vertical lines cross excluded rows -> only grid cross points painted
    for X := 0 to W do
    begin
      if MoveToEx(DC, R.Left + X * DX, R.Top, nil) then
        LineTo(DC, R.Left + X * DX, R.Bottom + 1);
    end;
  finally
    RestoreDC(DC, SavedDC);
  end;
end;

function TGtk2WidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  // all fonts are UTF-8 under gtk2 => no mapping needed
  Result:=ExtTextOut(DC,X,Y,Options,Rect,Str,Count,Dx);
end;

function TGtk2WidgetSet.TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar;
  Count: Longint): Boolean;
begin
  // all fonts are UTF-8 under gtk2 => no mapping needed
  Result := TextOut(DC, X, Y, Str, Count);
end;

function TGtk2WidgetSet.ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean;
var
  WidgetInfo: PWidgetInfo;
  Combo: PGtkComboBox;
begin
  WidgetInfo := GetWidgetInfo(Pointer(Handle), False);
  Combo := PGtkComboBox(WidgetInfo^.CoreWidget);
  
  case DropDown of
    True : gtk_combo_box_popup(Combo);
    False: gtk_combo_box_popdown(Combo);
  end;
  Result:=true;
end;

function TGtk2WidgetSet.SetComboMinDropDownSize(Handle: HWND; MinItemsWidth,
  MinItemsHeight, MinItemCount: integer): boolean;
var
  p: PGtkWidget;
  Menu: PGtkWidget;
  Requisition: TGtkRequisition;
begin
  Result:=True;
  p := GetWidgetInfo(Pointer(Handle), False)^.CoreWidget;
  
  Menu := PGtkWidget(g_object_get_data(G_OBJECT(p), 'Menu'));
  if Menu<>nil then begin
    Requisition.width := MinItemsWidth;
    Requisition.height := MinItemsHeight * MinItemCount;
    gtk_widget_size_request(Menu, @Requisition);
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
