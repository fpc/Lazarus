{ $Id$ }
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
  Function: LCLCheckMenuItem
  Params:  AMenuItem
  Returns: Nothing
  
  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
function TgtkObject.LclCheckMenuItem(AMenuItem: TMenuItem): Boolean; 
var
  IsRadio: Boolean;
  Group: PGSList;
  Item: Pointer;
begin
  Item := Pointer(AMenuItem.Handle); 
  IsRadio := gtk_is_radio_menu_item(Item);
  if IsRadio or gtk_is_check_menu_item(Item)
  then begin
    if IsRadio
    then begin
      Group := gtk_radio_menu_item_group(Item);
      LockRadioGroupOnChange(Group, +1);
    end
    else LockOnChange(Item, +1);
    gtk_check_menu_item_set_active(Item, AMenuItem.Checked);
    if IsRadio
    then LockRadioGroupOnChange(Group, -1)
    else LockOnChange(Item, -1);
    Result := True;
  end 
  else begin
    AMenuItem.RecreateHandle;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  Function: LCLEnableMenuItem
  Params: AMenuItem: 
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
function TGTKObject.LclEnableMenuItem(AMenuItem: TMenuItem): Boolean; 
begin
  gtk_widget_set_sensitive(pgtkwidget(AMenuItem.Handle), AMenuItem.Enabled);
  Result := True;
end;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.2  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
