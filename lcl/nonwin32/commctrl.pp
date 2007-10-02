{ $Id$}
{
 /***************************************************************************
                               CommCtrl.pp
                             -------------------
                          An interface to Common Controls
                   Initial Revision : Tue Mar 26 2002


 ***************************************************************************/

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
{
@abstract(Just a try to provide the same functions and structs as the)
@abstract(Delphi commctrl unit)
@author(Marc Weustink <marc@@dommelstein.net>)
@created(Tue Mar 26 2002)
@lastmod($Date$)
}
unit CommCtrl;

{$mode objfpc}
{$H+}

interface          

uses
  Classes, lMessages, LCLType;

//-------------
// Common
//-------------
    
const
  //all controls
  NM_FIRST           = 0;      
  NM_OUTOFMEMORY     = NM_FIRST - 1;
  NM_CLICK           = NM_FIRST - 2;
  NM_DBLCLK          = NM_FIRST - 3;
  NM_RETURN          = NM_FIRST - 4;
  NM_RCLICK          = NM_FIRST - 5;
  NM_RDBLCLK         = NM_FIRST - 6;
  NM_SETFOCUS        = NM_FIRST - 7;
  NM_KILLFOCUS       = NM_FIRST - 8;
  NM_CUSTOMDRAW      = NM_FIRST - 12;
  NM_HOVER           = NM_FIRST - 13;
  NM_NCHITTEST       = NM_FIRST - 14;   
  NM_KEYDOWN         = NM_FIRST - 15;   
  NM_RELEASEDCAPTURE = NM_FIRST - 16;
  NM_SETCURSOR       = NM_FIRST - 17;   
  NM_CHAR            = NM_FIRST - 18;   

  NM_LAST            = NM_FIRST - 99;
    
  //listview
  LVN_FIRST    = -100;   
  LVN_LAST     = -199;
  
  //header
  HDN_FIRST    = -300;   
  HDN_LAST     = -399;
  
  //treeview
  TVN_FIRST    = -400;   
  TVN_LAST     = -499;
  
  //tooltips
  TTN_FIRST    = -520;   
  TTN_LAST     = -549;
  
  //tab control
  TCN_FIRST    = -550;   
  TCN_LAST     = -580;
  
  // toolbar
  TBN_First    = -700;
  TBN_Last     = -720;

const
  CCS_TOP                 = $00000001;
  CCS_NOMOVEY             = $00000002;
  CCS_BOTTOM              = $00000003;
  CCS_NORESIZE            = $00000004;
  CCS_NOPARENTALIGN       = $00000008;
  CCS_ADJUSTABLE          = $00000020;
  CCS_NODIVIDER           = $00000040;
  CCS_VERT                = $00000080;
  CCS_LEFT                = (CCS_VERT or CCS_TOP);
  CCS_RIGHT               = (CCS_VERT or CCS_BOTTOM);
  CCS_NOMOVEX             = (CCS_VERT or CCS_NOMOVEY);

  ICC_LISTVIEW_CLASSES   = $00000001;
  ICC_TREEVIEW_CLASSES   = $00000002;
  ICC_BAR_CLASSES        = $00000004;
  ICC_TAB_CLASSES        = $00000008;
  ICC_UPDOWN_CLASS       = $00000010;
  ICC_PROGRESS_CLASS     = $00000020;
  ICC_HOTKEY_CLASS       = $00000040;
  ICC_ANIMATE_CLASS      = $00000080;
  ICC_WIN95_CLASSES      = $000000FF;
  ICC_DATE_CLASSES       = $00000100;
  ICC_USEREX_CLASSES     = $00000200;
  ICC_COOL_CLASSES       = $00000400;
  ICC_INTERNET_CLASSES   = $00000800;
  ICC_PAGESCROLLER_CLASS = $00001000;
  ICC_NATIVEFNTCTL_CLASS = $00002000;

  
//-------------
// Toolbar
//-------------
const
  TBN_BEGINDRAG       = TBN_FIRST-1;
  TBN_ENDDRAG         = TBN_FIRST-2;
  TBN_BEGINADJUST     = TBN_FIRST-3;
  TBN_ENDADJUST       = TBN_FIRST-4;
  TBN_RESET           = TBN_FIRST-5;
  TBN_QUERYINSERT     = TBN_FIRST-6;
  TBN_QUERYDELETE     = TBN_FIRST-7;
  TBN_TOOLBARCHANGE   = TBN_FIRST-8;
  TBN_CUSTHELP        = TBN_FIRST-9;
  TBN_DROPDOWN        = TBN_FIRST-10;
  TBN_CLOSEUP         = TBN_FIRST-11;
  TBN_GETOBJECT       = TBN_FIRST-12;

const
  TBIF_IMAGE          = $00000001;
  TBIF_TEXT           = $00000002;
  TBIF_STATE          = $00000004;
  TBIF_STYLE          = $00000008;
  TBIF_LPARAM         = $00000010;
  TBIF_COMMAND        = $00000020;
  TBIF_SIZE           = $00000040;      
  
const   
  TB_ENABLEBUTTON         = WM_USER + 1;
  TB_CHECKBUTTON          = WM_USER + 2;
  TB_PRESSBUTTON          = WM_USER + 3;
  TB_HIDEBUTTON           = WM_USER + 4;
  TB_INDETERMINATE        = WM_USER + 5;
  TB_MARKBUTTON           = WM_USER + 6;
  TB_ISBUTTONENABLED      = WM_USER + 9;
  TB_ISBUTTONCHECKED      = WM_USER + 10;
  TB_ISBUTTONPRESSED      = WM_USER + 11;
  TB_ISBUTTONHIDDEN       = WM_USER + 12;
  TB_ISBUTTONINDETERMINATE= WM_USER + 13;
  TB_ISBUTTONHIGHLIGHTED  = WM_USER + 14;
  TB_SETSTATE             = WM_USER + 17;
  TB_GETSTATE             = WM_USER + 18;
  TB_ADDBITMAP            = WM_USER + 19;
  TB_INSERTBUTTONA        = WM_USER + 21;
  TB_DELETEBUTTON         = WM_USER + 22;
  TB_GETBUTTON            = WM_USER + 23;
  TB_BUTTONCOUNT          = WM_USER + 24;

  TB_CUSTOMIZE            = WM_USER + 27;
  TB_ADDSTRINGA           = WM_USER + 28;
  TB_GETITEMRECT          = WM_USER + 29;
  TB_BUTTONSTRUCTSIZE     = WM_USER + 30;
  TB_SETBUTTONSIZE        = WM_USER + 31;
  TB_SETBITMAPSIZE        = WM_USER + 32;
  TB_AUTOSIZE             = WM_USER + 33;
  TB_GETTOOLTIPS          = WM_USER + 35;
  TB_SETTOOLTIPS          = WM_USER + 36;
  TB_SETPARENT            = WM_USER + 37;
  TB_SETROWS              = WM_USER + 39;
  TB_GETROWS              = WM_USER + 40;
  TB_SETCMDID             = WM_USER + 42;
  TB_CHANGEBITMAP         = WM_USER + 43;
  TB_GETBITMAP            = WM_USER + 44;
  TB_REPLACEBITMAP        = WM_USER + 46;
  TB_SETINDENT            = WM_USER + 47;
  TB_SETIMAGELIST         = WM_USER + 48;
  TB_GETIMAGELIST         = WM_USER + 49;
  TB_LOADIMAGES           = WM_USER + 50;
  TB_GETRECT              = WM_USER + 51;
  TB_SETHOTIMAGELIST      = WM_USER + 52;
  TB_GETHOTIMAGELIST      = WM_USER + 53;
  TB_SETDISABLEDIMAGELIST = WM_USER + 54;
  TB_GETDISABLEDIMAGELIST = WM_USER + 55;
  TB_SETSTYLE             = WM_USER + 56;
  TB_GETSTYLE             = WM_USER + 57;
  TB_GETBUTTONSIZE        = WM_USER + 58;
  TB_SETBUTTONWIDTH       = WM_USER + 59;
  TB_SETMAXTEXTROWS       = WM_USER + 60;
  TB_GETTEXTROWS          = WM_USER + 61;
  TB_GETBUTTONINFOW       = WM_USER + 63;
  TB_SETBUTTONINFOW       = WM_USER + 64;
  TB_GETBUTTONINFOA       = WM_USER + 65;
  TB_SETBUTTONINFOA       = WM_USER + 66;

  TB_GETHOTITEM           = WM_USER + 71;
  TB_SETHOTITEM           = WM_USER + 72;

  TB_ADDSTRINGW           = WM_USER + 77;


  TB_INSERTBUTTON = TB_INSERTBUTTONA;
  TB_ADDSTRING = TB_ADDSTRINGA;
  TB_GETBUTTONINFO = TB_GETBUTTONINFOA;
  TB_SETBUTTONINFO = TB_SETBUTTONINFOA;
  
const
  TBSTATE_CHECKED         = $01;
  TBSTATE_PRESSED         = $02;
  TBSTATE_ENABLED         = $04;
  TBSTATE_HIDDEN          = $08;
  TBSTATE_INDETERMINATE   = $10;
  TBSTATE_WRAP            = $20;
  TBSTATE_ELLIPSES        = $40;
  TBSTATE_MARKED          = $80;

  TBSTYLE_BUTTON          = $00;
  TBSTYLE_SEP             = $01;
  TBSTYLE_CHECK           = $02;
  TBSTYLE_GROUP           = $04;
  TBSTYLE_CHECKGROUP      = TBSTYLE_GROUP or TBSTYLE_CHECK;
  TBSTYLE_DROPDOWN        = $08;
  TBSTYLE_AUTOSIZE        = $0010;
  TBSTYLE_NOPREFIX        = $0020;

  TBSTYLE_TOOLTIPS        = $0100;
  TBSTYLE_WRAPABLE        = $0200;
  TBSTYLE_ALTDRAG         = $0400;
  TBSTYLE_FLAT            = $0800;
  TBSTYLE_LIST            = $1000;
  TBSTYLE_CUSTOMERASE     = $2000;
  TBSTYLE_REGISTERDROP    = $4000;
  TBSTYLE_TRANSPARENT     = $8000;
  TBSTYLE_EX_DRAWDDARROWS = $00000001;

  // Toolbar custom draw result flags
  {Not used yet, but soon}
  TBCDRF_NOEDGES              = $00010000;  // Don't draw the button edges
  TBCDRF_HILITEHOTTRACK       = $00020000;  // Use color of the button bk when hottracked
  TBCDRF_NOOFFSET             = $00040000;  // Don't offset the button if pressed
  TBCDRF_NOMARK               = $00080000;  // Don't draw the default highlight of the image/text for TBSTATE_MARKED
  TBCDRF_NOETCHEDEFFECT       = $00100000;  // Don't draw the etched effect for disabled items

  ToolBarClassName = 'ToolbarWindow32';
  
type
   PTBButton = ^TTBButton;
  _TBBUTTON = packed record
    iBitmap: Integer;
    idCommand: Integer;
    fsState: Byte;
    fsStyle: Byte;
    bReserved: array[1..2] of Byte;
    dwData: Longint;
    iString: Integer;
  end;
  TTBButton = _TBBUTTON;

  TBBUTTONINFOA = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PAnsiChar;
    cchText: Integer;
  end;

  TBBUTTONINFOW = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PWideChar;
    cchText: Integer;
  end;
  TBBUTTONINFO = TBBUTTONINFOA;

  PTBButtonInfoA = ^TTBButtonInfoA;
  PTBButtonInfoW = ^TTBButtonInfoW;

  PTBButtonInfo = PTBButtonInfoA;

  TTBButtonInfoA = TBBUTTONINFOA;
  TTBButtonInfoW = TBBUTTONINFOW;
  TTBButtonInfo = TTBButtonInfoA;

type
  PTBAddBitmap = ^TTBAddBitmap;
  tagTBADDBITMAP = packed record
    hInst: THandle;
    nID: UINT;
  end;
  TTBAddBitmap = tagTBADDBITMAP;

  TBADDBITMAP = tagTBADDBITMAP;

type
  TBREPLACEBITMAP = packed record
    hInstOld: THandle;
    nIDOld: Cardinal;
    hInstNew: THandle;
    nIDNew: Cardinal;
    nButtons: Integer;
  end;
  PTBReplaceBitmap = ^TTBReplaceBitmap;
  TTBReplaceBitmap = TBREPLACEBITMAP;

  tagNMTOOLBARA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PAnsiChar;
  end;

  tagNMTOOLBARW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PWideChar;
  end;

  tagNMTOOLBAR = tagNMTOOLBARA;
  PNMToolBarA = ^TNMToolBarA;
  PNMToolBarW = ^TNMToolBarW;
  PNMToolBar = PNMToolBarA;
  TNMToolBarA = tagNMTOOLBARA;
  TNMToolBarW = tagNMTOOLBARW;
  TNMToolBar = TNMToolBarA;
  

//-------------
// Header
//-------------
const
  HDN_ITEMCHANGING    = HDN_FIRST-0;
  HDN_ITEMCHANGED     = HDN_FIRST-1;
  HDN_ITEMCLICK       = HDN_FIRST-2;
  HDN_ITEMDBLCLICK    = HDN_FIRST-3;
  HDN_DIVIDERDBLCLICK = HDN_FIRST-5;
  HDN_BEGINTRACK      = HDN_FIRST-6;
  HDN_ENDTRACK        = HDN_FIRST-7;
  HDN_TRACK           = HDN_FIRST-8;
  HDN_GETDISPINFO     = HDN_FIRST-9;
  HDN_BEGINDRAG       = HDN_FIRST-10;
  HDN_ENDDRAG         = HDN_FIRST-11;
  
//-------------
// Listview
//-------------
const
  LVN_ITEMCHANGING    = LVN_FIRST-0;
  LVN_ITEMCHANGED     = LVN_FIRST-1;
  LVN_INSERTITEM      = LVN_FIRST-2;
  LVN_DELETEITEM      = LVN_FIRST-3;
  LVN_DELETEALLITEMS  = LVN_FIRST-4;
  LVN_COLUMNCLICK     = LVN_FIRST-8;
  LVN_BEGINDRAG       = LVN_FIRST-9;
  LVN_BEGINRDRAG      = LVN_FIRST-11;

const
  LVIF_TEXT           = $0001;
  LVIF_IMAGE          = $0002;
  LVIF_PARAM          = $0004;
  LVIF_STATE          = $0008;
  LVIF_INDENT         = $0010;
  LVIF_NORECOMPUTE    = $0800;

  LVIS_FOCUSED        = $0001;
  LVIS_SELECTED       = $0002;
  LVIS_CUT            = $0004;
  LVIS_DROPHILITED    = $0008;
  LVIS_ACTIVATING     = $0020;

  LVIS_OVERLAYMASK    = $0F00;
  LVIS_STATEIMAGEMASK = $F000;

type
  PNMListView = ^TNMListView;
  TNMListView = packed record
    hdr: TNMHDR;
    iItem: Integer;
    iSubItem: Integer;
    uNewState: UINT;
    uOldState: UINT;
    uChanged: UINT;
    ptAction: TPoint;
    lParam: LPARAM;
  end;
  _NM_LISTVIEW = TNMListView;
  NM_LISTVIEW = TNMListView;
  tagNMLISTVIEW = TNMListView;

implementation

end.