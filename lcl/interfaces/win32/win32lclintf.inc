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
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32Object.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
  if ListBox is TCustomListBox then begin
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
    if hi(Result)=0 then
      Result := lo(Result)
    else Result := -1;
  end;
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarInsertPanel
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarInsertPanel(StatusBar: TObject; index: integer);
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarPanelUpdate
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarPanelUpdate(StatusBar: TObject; index: integer);
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarRemovePanel
  Params: StatusBar:
          index:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarRemovePanel(StatusBar: TObject; index: integer);
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetSimpleText
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarSetSimpleText(StatusBar: TObject);
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetStyle
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarSetStyle(StatusBar: TObject);
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Function: LCLCheckMenuItem
  Params:  BaseMenuItem
  Returns: Nothing

  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.LclCheckMenuItem(BaseMenuItem: TComponent): Boolean;
var
  CheckFlag: Integer;
  AMenuItem: TMenuItem;
Begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  if AMenuItem.Checked then CheckFlag := MF_CHECKED
  else CheckFlag := MF_UNCHECKED;
  CheckFlag := CheckFlag or MF_BYCOMMAND;
  Result := Windows.CheckMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, CheckFlag) <> DWORD($FFFFFFFF);
End;

{------------------------------------------------------------------------------
  Function: LCLEnableMenuItem
  Params: BaseMenuItem:
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.LclEnableMenuItem(BaseMenuItem: TComponent): Boolean;
Var
  EnableFlag: Integer;
  AMenuItem: TMenuItem;
Begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  if AMenuItem.Enabled then EnableFlag := MF_ENABLED
  else EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
End;



//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.6  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:55:15  mattias
  fixed win32 TBaseMenuitem

  Revision 1.1  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32


}
