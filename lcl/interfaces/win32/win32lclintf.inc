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

procedure TWin32Object.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
begin
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, TControl(Arrow).ClientRect, DFC_MENU, DFCS_MENUARROW);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32Object.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWin32Object.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  SizeRect: TRect;
  Height: Integer;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    if (SizeConstraints.Control is TComboBox)
      and TWinControl(SizeConstraints.Control).HandleAllocated then begin
      // the height of a combobox is fixed
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);
      Height := SizeRect.Bottom - SizeRect.Top;
      SizeConstraints.SetInterfaceConstraints(0, Height, 0, Height);
    end;

  end;
end;

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
  Function: MenuItemSetCheck
  Params:  BaseMenuItem
  Returns: Nothing

  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.MenuItemSetCheck(BaseMenuItem: TComponent): Boolean;
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
  Function: MenuItemSetEnable
  Params: BaseMenuItem:
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32Object.MenuItemSetEnable(BaseMenuItem: TComponent): Boolean;
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

{------------------------------------------------------------------------------
  Procedure: StatusBarPanelUpdate
  Params: StatusBar:
          PanelIndex:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarPanelUpdate(StatusBar: TObject; PanelIndex: integer);
var
  TheStatusBar: TStatusBar;
begin
  TheStatusBar := (StatusBar as TStatusBar);
  UpdateStatusBarPanelWidths(TheStatusBar);
  UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetText
  Params: StatusBar:
          PanelIndex:
          Text:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarSetText(StatusBar: TObject; PanelIndex: integer);
var
  TheStatusBar: TStatusBar;
begin
  TheStatusBar := StatusBar as TStatusBar;
  if TheStatusBar.SimplePanel then
    Windows.SendMessage(TheStatusBar.Handle, SB_SETTEXT, 255, LPARAM(PChar(TheStatusBar.SimpleText)))
  else
    UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarUpdate
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWIN32Object.StatusBarUpdate(StatusBar: TObject);
var
  TheStatusBar: TStatusBar;
  PanelIndex: integer;
begin
  TheStatusBar := StatusBar as TStatusBar;
  Windows.SendMessage(TheStatusBar.Handle, SB_SIMPLE, WPARAM(TheStatusBar.SimplePanel), 0);
  if TheStatusBar.SimplePanel then
    StatusBarSetText(StatusBar, 0)
  else begin
    UpdateStatusBarPanelWidths(TheStatusBar);
    for PanelIndex := 0 to TheStatusBar.Panels.Count-1 do
      UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.11  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.10  2004/02/05 13:53:38  mattias
  fixed GetConstraints for win32 intf

  Revision 1.9  2004/02/02 16:56:43  micha
  implement GetControlConstraints for combobox

  Revision 1.8  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.7  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

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
