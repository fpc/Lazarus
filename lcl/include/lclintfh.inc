(******************************************************************************
  All interface communication related stuff goes here.
  This file is used by both LCLLinux and InterfaceBase
  If a procedure is platform dependent then it should have the following tail:
    virtual;
  If a procedure isn't platform dependent, it is no part of InterfaseBase and
  should be surrounded by:
    {$IFNDEF IF_BASE_MEMBER}
      procedure MyProc;
    {$ENDIF}

  !! Keep this alphabetical and place a pointer to the independent part
  !! if needed

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

 ******************************************************************************)

{******************************************************************************
  Platform specific stuff

  Note:
    the IFNDEF IF_BASE_MEMBER section is at the end
 ******************************************************************************}
//##apiwiz##sps##    // Do not remove

{$ifdef INTF_CONTROL}
class procedure AttachMenuToWindow(AMenuObject: TComponent); virtual;

class procedure CallDefaultWndHandler(Sender: TObject; var Message); virtual;
class function ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean; virtual;

class procedure DrawArrow(Arrow: TComponent; Canvas: TPersistent); virtual;

class function GetIntfClientBounds(handle : HWND; var ARect: TRect) : Boolean; virtual;
class function GetIntfClientRect(handle : HWND; var Rect: TRect) : Boolean; virtual;
class function GetControlConstraints(Constraints: TObject): boolean; virtual;
class function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; virtual;
class function GetListBoxItemRect(ListBox: TComponent; Index: integer; var ARect: TRect): boolean; virtual;
class function GetNotebookTabIndexAtPos(Handle: HWND; const ClientPos: TPoint): integer; virtual;
class function PairSplitterAddSide(SplitterHandle, SideHandle: hWnd; Side: integer): Boolean; virtual;
class function PairSplitterGetInterfaceInfo: boolean; virtual;
class function PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd; Side: integer): Boolean; virtual;
class function PairSplitterSetPosition(SplitterHandle: hWnd; var NewPosition: integer): Boolean; virtual;

class function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; virtual;
class function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; virtual;
class procedure StatusBarPanelUpdate(StatusBar: TObject; PanelIndex: integer); virtual;
class procedure StatusBarSetText(StatusBar: TObject; PanelIndex: integer); virtual;
class procedure StatusBarUpdate(StatusBar: TObject); virtual;
{$endif INTF_CONTROL}

{$ifdef INTF_MENUITEM}
class function MenuItemSetCheck(AMenuItem: TComponent): Boolean; virtual;
class function MenuItemSetEnable(AMenuItem: TComponent): Boolean; virtual;
class function RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean; virtual;
class function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; virtual;
class function RightJustifyMenuItem(HndMenu: HMenu; bRightJustify: boolean): Boolean; virtual;
{$endif}

{$ifdef INTF_OBJECT}
class procedure SendCachedLCLMessages; virtual;
{$endif}


//##apiwiz##eps##    // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

{$IFNDEF IF_BASE_MEMBER}

//##apiwiz##spi##    // Do not remove

//##apiwiz##epi##    // Do not remove

{$ENDIF}

{ =============================================================================

  $Log$
  Revision 1.18  2004/02/22 22:52:58  micha
  split interface into non-lcl and lcl-component dependent parts

  Revision 1.17  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.16  2004/02/19 05:07:17  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.15  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.14  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.13  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.12  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.11  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.10  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.9  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.8  2004/01/10 18:00:42  mattias
  fixed GetWindowOrgEx, added GetDCOriginRelativeToWindow

  Revision 1.7  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.6  2004/01/03 11:57:47  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

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
