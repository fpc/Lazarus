{ $ID: $ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

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

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Function: CheckMenuItem
  Params:  hndMenu: HMENU; uIDEnableItem: Integer; bChecked: Boolean
  Returns: Nothing


 ------------------------------------------------------------------------------}
function TgtkObject.CheckMenuItem(hndMenu: HMENU; uIDEnableItem: Integer; bChecked: Boolean): Boolean;
var
  LCLMenuItem: TMenuItem;
  IsRadio: Boolean;
  Group: PGSList;
begin
  IsRadio := gtk_is_radio_menu_item(Pointer(hndMenu));
  if IsRadio or gtk_is_check_menu_item(Pointer(hndMenu))
  then begin
    if IsRadio
    then begin
      Group :=  gtk_radio_menu_item_group(Pointer(hndMenu));
      LockRadioGroupOnChange(Group, +1);
    end
    else  LockOnChange(PgtkObject(hndMenu),1);
    gtk_check_menu_item_set_active(PGtkCheckMenuItem(hndMenu),bChecked);
    if IsRadio
    then begin
      LockRadioGroupOnChange(Group, -1);
    end
    else LockOnChange(PgtkObject(hndMenu),-1);
    Result := True;
  end else begin
    LCLMenuItem:=TMenuItem(GetLCLObject(Pointer(hndMenu)));
    if LCLMenuItem<>nil
    then begin
      LCLMenuItem.RecreateHandle;
      Result := True;
    end else
      Result := False;
  end;
end;

{------------------------------------------------------------------------------
  Function: EnableMenuItem
  Params: hndMenu:
          uIDEnableItem:
  Returns:

 ------------------------------------------------------------------------------}
function TGTKObject.EnableMenuItem(hndMenu: HMENU; uIDEnableItem: Integer;
  bEnable: Boolean): Boolean;
begin
  if hndMenu <> 0
  then gtk_widget_set_sensitive(pgtkwidget(hndMenu), bEnable);
  Result:=true;
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
