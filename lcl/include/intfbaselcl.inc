{ $Id$ }
{******************************************************************************
                                  TInterfaceBase

                           interface communication  stuff

  !! In this file only interface related code as defined in lclintfh.inc
     Most routines implement only the default

  !! Keep this alphabetical !!
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

procedure TIntfControl.AttachMenuToWindow(AMenuObject: TComponent);
begin
end;

procedure TIntfControl.CallDefaultWndHandler(Sender: TObject; var Message);
begin
end;

function TIntfControl.ComboBoxDropDown(Handle: HWND;
  DropDown: boolean): boolean;
begin
  Result := false;
end;

procedure TIntfControl.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
begin
end;

function TIntfControl.GetIntfClientBounds(Handle : HWND; var ARect: TRect) : Boolean;
begin
  Result := false;
end;

function TIntfControl.GetIntfClientRect(handle : HWND; var Rect : TRect) : Boolean;
begin
  Result := False;
end;

function TIntfControl.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=true;
end;

function TIntfControl.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
end;

function TIntfControl.GetListBoxItemRect(ListBox: TComponent; Index: integer;
  var ARect: TRect): boolean;
begin
  FillChar(ARect,SizeOf(ARect),0);
  Result:=false;
end;

function TIntfControl.GetNotebookTabIndexAtPos(Handle: HWND;
  const ClientPos: TPoint): integer;
begin
  Result:=-1;
end;

function TIntfControl.PairSplitterAddSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
end;

function TIntfControl.PairSplitterGetInterfaceInfo: boolean;
begin
  Result:=false;
end;

function TIntfControl.PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd;
  Side: integer): Boolean;
begin
  Result:=false;
end;

function TIntfControl.PairSplitterSetPosition(SplitterHandle: hWnd;
  var NewPosition: integer): Boolean;
begin
  Result:=false;
end;

function TIntfMenuItem.MenuItemSetCheck(AMenuItem: TComponent): Boolean;
begin
  Result := False;
end;

function TIntfMenuItem.MenuItemSetEnable(AMenuItem: TComponent): Boolean;
begin
  Result := False;
end;

function TIntfMenuItem.RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean;
begin
  Result := false;
end;

function TIntfMenuItem.RegroupMenuItem(hndMenu: HMENU; GroupIndex: Integer) : Boolean;
begin
  Result := false;
end;

function TIntfMenuItem.RightJustifyMenuItem(HndMenu: HMenu;
  bRightJustify: boolean): Boolean;
begin
  Result := False;
end;

procedure TIntfObject.SendCachedLCLMessages;
begin
end;

function TIntfControl.SetCaretRespondToFocus(Handle: HWnd;
  ShowHideOnFocus: Boolean): Boolean;
begin
  Result := False;
end;

function TIntfControl.SetComboMinDropDownSize(Handle: HWND;
  MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean;
begin
  Result := false;
end;

procedure TIntfControl.StatusBarPanelUpdate(StatusBar: TObject;
  PanelIndex: integer);
begin
end;

procedure TIntfControl.StatusBarSetText(StatusBar: TObject;
  PanelIndex: integer);
begin
end;

procedure TIntfControl.StatusBarUpdate(StatusBar: TObject);
begin
end;

//##apiwiz##eps##   // Do not remove
{ =============================================================================

  $Log$
  Revision 1.20  2004/02/22 22:52:58  micha
  split interface into non-lcl and lcl-component dependent parts

  Revision 1.19  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.18  2004/02/19 05:07:17  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.17  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.16  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.15  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.14  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.13  2004/01/12 13:43:12  mattias
  improved and activated new statusbar

  Revision 1.12  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.11  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.10  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.9  2004/01/10 18:00:42  mattias
  fixed GetWindowOrgEx, added GetDCOriginRelativeToWindow

  Revision 1.8  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.7  2004/01/03 11:57:47  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.6  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.5  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.4  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.3  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.2  2003/11/25 14:21:28  micha
  new api lclenable,checkmenuitem according to list

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
