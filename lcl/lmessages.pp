{  $Id$  }
{
 /***************************************************************************
                               lMessages.pp
                               ------------

                   Initial Revision  : Wed Jun 30 CST 1999
                Shane Miller

 ***************************************************************************/

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

unit LMessages;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, LCLType, GraphType
  {$ifdef WINDOWS}
  ,messages
  {$endif WINDOWS}
  ;

const
  // TODO: review this list of messages and remove obsoleted/uneeded, move
  // internal gtk things (like LM_CONFIGUREEVENT) directly to gtk interface code

  //-------------
  // lcl messages
  //
  // This should be a list of LCL specific messages
  // RECEIVED from the interface, here are no defines
  // of messages send to the interface
  //-------------
  LM_USER           = $400; // standard WM_USER value
  WM_USER           = LM_USER;

  // reserve some space for LM_USER messages
  LM_LCL            = LM_USER + $10000;
  
  LM_ACTIVATEITEM   = LM_LCL + 04; // GTK internal. Should be removed later
  LM_CHANGED        = LM_LCL + 05;
  LM_FOCUS          = LM_LCL + 06;
  LM_CLICKED        = LM_LCL + 07;
  LM_RELEASED       = LM_LCL + 09;
  LM_ENTER          = LM_LCL + 11;
  LM_LEAVE          = LM_LCL + 12;
  LM_CHECKRESIZE    = LM_LCL + 14;
  LM_SETEDITABLE    = LM_LCL + 18;
  LM_MOVEWORD       = LM_LCL + 19;
  LM_MOVEPAGE       = LM_LCL + 20;
  LM_MOVETOROW      = LM_LCL + 21;
  LM_MOVETOCOLUMN   = LM_LCL + 22;
  LM_KILLCHAR       = LM_LCL + 23;
  LM_KILLWORD       = LM_LCL + 24;
  LM_KILLLINE       = LM_LCL + 25;
  LM_CONFIGUREEVENT = LM_LCL + 31; // GTK internal. Should be removed later.
  LM_EXIT           = LM_LCL + 60;
  LM_CLOSEQUERY     = LM_LCL + 62;
  LM_DRAGSTART      = LM_LCL + 63;
  LM_DEACTIVATE     = LM_LCL + 64;  //used when a form is no longer in front
  LM_QUIT           = LM_LCL + 65;
  LM_MONTHCHANGED   = LM_LCL + 66;
  LM_YEARCHANGED    = LM_LCL + 67;
  LM_DAYCHANGED     = LM_LCL + 68;

  LM_MOUSEFIRST2    = LM_LCL + 70;
  LM_LBUTTONTRIPLECLK = LM_MOUSEFIRST2 + 0;
  LM_LBUTTONQUADCLK   = LM_MOUSEFIRST2 + 1;
  LM_MBUTTONTRIPLECLK = LM_MOUSEFIRST2 + 2;
  LM_MBUTTONQUADCLK   = LM_MOUSEFIRST2 + 3;
  LM_RBUTTONTRIPLECLK = LM_MOUSEFIRST2 + 4;
  LM_RBUTTONQUADCLK   = LM_MOUSEFIRST2 + 5;
  LM_MOUSEENTER       = LM_MOUSEFIRST2 + 6;
  LM_MOUSELEAVE       = LM_MOUSEFIRST2 + 7;
  LM_XBUTTONTRIPLECLK = LM_MOUSEFIRST2 + 8;
  LM_XBUTTONQUADCLK   = LM_MOUSEFIRST2 + 9;
  LM_MOUSELAST2       = LM_XBUTTONQUADCLK;
  // for triple and quad clicks see below

  LM_GRABFOCUS      = LM_LCL + 80;
  LM_DRAWLISTITEM   = LM_LCL + 81;

  // these IDs are reserved for internal messages in the interfaces
  LM_INTERFACEFIRST = LM_LCL + 99;
  LM_INTERFACELAST  = LM_LCL + 199;
  
  LM_UNKNOWN        = LM_INTERFACELAST + 1;

  //-------------
  //end of messages that are sent to the interface
  //-------------


  //-------------
  // Windows Compatability}
  //-------------
 { System Menu Commands }
  SC_SIZE           = 61440;
  SC_MOVE           = 61456;
  SC_MINIMIZE       = 61472;
  SC_MAXIMIZE       = 61488;
  SC_NEXTWINDOW     = 61504;
  SC_PREVWINDOW     = 61520;
  SC_CLOSE          = 61536;
  SC_VSCROLL        = 61552;
  SC_HSCROLL        = 61568;
  SC_MOUSEMENU      = 61584;
  SC_KEYMENU        = 61696;
  SC_ARRANGE        = 61712;
  SC_RESTORE        = 61728;
  SC_TASKLIST       = 61744;
  SC_SCREENSAVE     = 61760;
  SC_HOTKEY         = 61776;
  SC_DEFAULT        = 61792;
  SC_MONITORPOWER   = 61808;
  SC_CONTEXTHELP    = 61824;
  SC_SEPARATOR      = 61455;


  //-------------
  // Messages
  //-------------

  LM_NULL              = $0000;
  LM_CREATE            = $0001;
  LM_DESTROY           = $0002;
  LM_MOVE              = $0003;

  LM_SIZE              = $0005;
  LM_ACTIVATE          = $0006;
  LM_SETFOCUS          = $0007;
  LM_KILLFOCUS         = $0008;
  LM_ENABLE            = $000A;
  LM_GETTEXTLENGTH     = $000E;
  LM_PAINT             = $000F;
  LM_ERASEBKGND        = $0014;

  LM_SHOWWINDOW        = $0018;

  LM_CANCELMODE        = $001F;
  LM_SETCURSOR         = $0020;
  LM_DRAWITEM          = $002B;
  LM_MEASUREITEM       = $002C;
  LM_DELETEITEM        = $002D;
  LM_VKEYTOITEM        = $002E;
  LM_CHARTOITEM        = $002F;
  LM_SETFONT           = $0030;

  LM_COMPAREITEM       = $0039;
  LM_WINDOWPOSCHANGING = $0046;
  LM_WINDOWPOSCHANGED  = $0047;
  LM_NOTIFY            = $004E;
  LM_HELP              = $0053;
  LM_NOTIFYFORMAT      = $0055;
  LM_CONTEXTMENU       = $007B;

  LM_NCCALCSIZE        = $0083;
  LM_NCHITTEST         = $0084;
  LM_NCPAINT           = $0085;
  LM_NCACTIVATE        = $0086;
  LM_GETDLGCODE        = $0087;
  LM_NCMOUSEMOVE       = $00A0;
  LM_NCLBUTTONDOWN     = $00A1;
  LM_NCLBUTTONUP       = $00A2;
  LM_NCLBUTTONDBLCLK   = $00A3;

  LM_KEYFIRST          = $0100;
  LM_KEYDOWN           = $0100;
  LM_KEYUP             = $0101;
  LM_CHAR              = $0102;

  LM_SYSKEYDOWN        = $0104;
  LM_SYSKEYUP          = $0105;
  LM_SYSCHAR           = $0106;

  LM_KEYLAST           = $0108;

  LM_COMMAND           = $0111;
  LM_SYSCOMMAND        = $0112;
  LM_TIMER             = $0113;
  LM_HSCROLL           = $0114;
  LM_VSCROLL           = $0115;
  LM_CTLCOLORMSGBOX    = $0132;
  LM_CTLCOLOREDIT      = $0133;
  LM_CTLCOLORLISTBOX   = $0134;
  LM_CTLCOLORBTN       = $0135;
  LM_CTLCOLORDLG       = $0136;
  LM_CTLCOLORSCROLLBAR = $0137;
  LM_CTLCOLORSTATIC    = $0138;

  LM_MOUSEFIRST        = $0200;
  LM_MOUSEMOVE         = $0200;
  LM_LBUTTONDOWN       = $0201;
  LM_LBUTTONUP         = $0202;
  LM_LBUTTONDBLCLK     = $0203;
  LM_RBUTTONDOWN       = $0204;
  LM_RBUTTONUP         = $0205;
  LM_RBUTTONDBLCLK     = $0206;
  LM_MBUTTONDOWN       = $0207;
  LM_MBUTTONUP         = $0208;
  LM_MBUTTONDBLCLK     = $0209;
  LM_MOUSEWHEEL        = $020A;
  LM_XBUTTONDOWN       = $020B;
  LM_XBUTTONUP         = $020C;
  LM_XBUTTONDBLCLK     = $020D;
  LM_MOUSELAST         = $020D;

  LM_PARENTNOTIFY      = $0210;
  LM_CAPTURECHANGED    = $0215;
  LM_DROPFILES         = $0233;

  LM_SELCHANGE         = $0234;
  LM_CUT               = $0300;
  LM_COPY              = $0301;
  LM_PASTE             = $0302;
  LM_CLEAR             = $0303;

const
  // Moved from Controls to avoid circles
  CM_BASE                 = $B000;
  CM_LCLOFFSET            = $100;
  CM_ACTIVATE             = CM_BASE + 0;
  CM_DEACTIVATE           = CM_BASE + 1;
  CM_GOTFOCUS             = CM_BASE + 2 unimplemented;
  CM_LOSTFOCUS            = CM_BASE + 3 unimplemented;
  CM_CANCELMODE           = CM_BASE + 4 unimplemented;
  CM_DIALOGKEY            = CM_BASE + 5 unimplemented;
  CM_DIALOGCHAR           = CM_BASE + 6 unimplemented;
  CM_FOCUSCHANGED         = CM_BASE + 7;
  CM_PARENTFONTCHANGED    = CM_BASE + 8;
  CM_PARENTCOLORCHANGED   = CM_BASE + 9;
  CM_HITTEST              = CM_BASE + 10;
  CM_VISIBLECHANGED       = CM_BASE + 11;
  CM_ENABLEDCHANGED       = CM_BASE + 12;
  CM_COLORCHANGED         = CM_BASE + 13;
  CM_FONTCHANGED          = CM_BASE + 14;
  CM_CURSORCHANGED        = CM_BASE + 15;
  CM_CTL3DCHANGED         = CM_BASE + 16 unimplemented;
  CM_PARENTCTL3DCHANGED   = CM_BASE + 17 unimplemented;
  CM_TEXTCHANGED          = CM_BASE + 18;
  CM_MOUSEENTER           = CM_BASE + 19;
  CM_MOUSELEAVE           = CM_BASE + 20;
  CM_MENUCHANGED          = CM_BASE + 21;
  CM_APPKEYDOWN           = CM_BASE + 22 unimplemented;
  CM_APPSYSCOMMAND        = CM_BASE + 23;
  CM_BUTTONPRESSED        = CM_BASE + 24;
  CM_SHOWINGCHANGED       = CM_BASE + 25;
  CM_ENTER                = CM_BASE + 26;
  CM_EXIT                 = CM_BASE + 27;
  CM_DESIGNHITTEST        = CM_BASE + 28; // implemented by custom controls
  CM_ICONCHANGED          = CM_BASE + 29;
  CM_WANTSPECIALKEY       = CM_BASE + 30;
  CM_INVOKEHELP           = CM_BASE + 31 unimplemented;
  CM_WINDOWHOOK           = CM_BASE + 32 unimplemented;
  CM_RELEASE              = CM_BASE + 33;
  CM_SHOWHINTCHANGED      = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED= CM_BASE + 35;
  CM_SYSCOLORCHANGE       = CM_BASE + 36 unimplemented;
  CM_WININICHANGE         = CM_BASE + 37 unimplemented;
  CM_FONTCHANGE           = CM_BASE + 38;
  CM_TIMECHANGE           = CM_BASE + 39 unimplemented;
  CM_TABSTOPCHANGED       = CM_BASE + 40;
  CM_UIACTIVATE           = CM_BASE + 41;
  CM_UIDEACTIVATE         = CM_BASE + 42 unimplemented;
  CM_DOCWINDOWACTIVATE    = CM_BASE + 43 unimplemented;
  CM_CONTROLLISTCHANGE    = CM_BASE + 44;
  CM_GETDATALINK          = CM_BASE + 45;
  CM_CHILDKEY             = CM_BASE + 46;
  CM_DRAG                 = CM_BASE + 47 unimplemented; //Method TControl.DoDragMsg()
  CM_HINTSHOW             = CM_BASE + 48;
  CM_DIALOGHANDLE         = CM_BASE + 49 unimplemented;
  CM_ISTOOLCONTROL        = CM_BASE + 50 unimplemented;
  CM_RECREATEWND          = CM_BASE + 51 unimplemented;
  CM_INVALIDATE           = CM_BASE + 52 unimplemented;
  CM_SYSFONTCHANGED       = CM_BASE + 53;
  CM_CONTROLCHANGE        = CM_BASE + 54;
  CM_CHANGED              = CM_BASE + 55;
  CM_DOCKCLIENT           = CM_BASE + 56 unimplemented; //Method TWinControl.DoDockClientMsg()
  CM_UNDOCKCLIENT         = CM_BASE + 57 unimplemented; //Method TWinControl.DoUndockClientMsg()
  CM_FLOAT                = CM_BASE + 58 unimplemented; //Method TControl.DoFloatMsg()
  CM_BORDERCHANGED        = CM_BASE + 59;
  CM_BIDIMODECHANGED      = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED= CM_BASE + 61;
  CM_ALLCHILDRENFLIPPED   = CM_BASE + 62;
  CM_ACTIONUPDATE         = CM_BASE + 63;
  CM_ACTIONEXECUTE        = CM_BASE + 64;
  CM_HINTSHOWPAUSE        = CM_BASE + 65;
  CM_DOCKNOTIFICATION     = CM_BASE + 66;
  CM_MOUSEWHEEL           = CM_BASE + 67;
  // unsupported at the moment
  CM_ISSHORTCUT                  = CM_BASE + 68 unimplemented;
  CM_UPDATEACTIONS               = CM_BASE + 69 unimplemented;
  CM_INVALIDATEDOCKHOST          = CM_BASE + 70 unimplemented;
  CM_SETACTIVECONTROL            = CM_BASE + 71 unimplemented;
  CM_POPUPHWNDDESTROY            = CM_BASE + 72 unimplemented;
  CM_CREATEPOPUP                 = CM_BASE + 73 unimplemented;
  CM_DESTROYHANDLE               = CM_BASE + 74 unimplemented;
  CM_MOUSEACTIVATE               = CM_BASE + 75 unimplemented;
  CM_CONTROLLISTCHANGING         = CM_BASE + 76 unimplemented;
  CM_BUFFEREDPRINTCLIENT         = CM_BASE + 77 unimplemented;
  CM_UNTHEMECONTROL              = CM_BASE + 78 unimplemented;
  CM_DOUBLEBUFFEREDCHANGED       = CM_BASE + 79 unimplemented;
  CM_PARENTDOUBLEBUFFEREDCHANGED = CM_BASE + 80 unimplemented;
  CM_THEMECHANGED                = CM_BASE + 81 unimplemented;
  CM_GESTURE                     = CM_BASE + 82 unimplemented;
  CM_CUSTOMGESTURESCHANGED       = CM_BASE + 83 unimplemented;
  CM_GESTUREMANAGERCHANGED       = CM_BASE + 84 unimplemented;
  CM_STANDARDGESTURESCHANGED     = CM_BASE + 85 unimplemented;
  CM_INPUTLANGCHANGE             = CM_BASE + 86 unimplemented;
  CM_TABLETOPTIONSCHANGED        = CM_BASE + 87 unimplemented;
  CM_PARENTTABLETOPTIONSCHANGED  = CM_BASE + 88 unimplemented;
  // LCL only
  CM_APPSHOWBTNGLYPHCHANGED  = CM_BASE + CM_LCLOFFSET + 68;
  CM_APPSHOWMENUGLYPHCHANGED = CM_BASE + CM_LCLOFFSET + 69;

  CN_BASE              = $BC00;
  CN_CHARTOITEM        = CN_BASE + LM_CHARTOITEM;
  CN_COMMAND           = CN_BASE + LM_COMMAND;
  CN_COMPAREITEM       = CN_BASE + LM_COMPAREITEM;
  CN_CTLCOLORBTN       = CN_BASE + LM_CTLCOLORBTN;
  CN_CTLCOLORDLG       = CN_BASE + LM_CTLCOLORDLG;
  CN_CTLCOLOREDIT      = CN_BASE + LM_CTLCOLOREDIT;
  CN_CTLCOLORLISTBOX   = CN_BASE + LM_CTLCOLORLISTBOX;
  CN_CTLCOLORMSGBOX    = CN_BASE + LM_CTLCOLORMSGBOX;
  CN_CTLCOLORSCROLLBAR = CN_BASE + LM_CTLCOLORSCROLLBAR;
  CN_CTLCOLORSTATIC    = CN_BASE + LM_CTLCOLORSTATIC;
  CN_DELETEITEM        = CN_BASE + LM_DELETEITEM;
  CN_DRAWITEM          = CN_BASE + LM_DRAWITEM;
  CN_HSCROLL           = CN_BASE + LM_HSCROLL;
  CN_MEASUREITEM       = CN_BASE + LM_MEASUREITEM;
  CN_PARENTNOTIFY      = CN_BASE + LM_PARENTNOTIFY;
  CN_VKEYTOITEM        = CN_BASE + LM_VKEYTOITEM;
  CN_VSCROLL           = CN_BASE + LM_VSCROLL;
  CN_KEYDOWN           = CN_BASE + LM_KEYDOWN;
  CN_KEYUP             = CN_BASE + LM_KEYUP;
  CN_CHAR              = CN_BASE + LM_CHAR;
  CN_SYSKEYUP          = CN_BASE + LM_SYSKEYUP;
  CN_SYSKEYDOWN        = CN_BASE + LM_SYSKEYDOWN;
  CN_SYSCHAR           = CN_BASE + LM_SYSCHAR;
  CN_NOTIFY            = CN_BASE + LM_NOTIFY;

  //-------------
  // End of Windows Compatability and messages
  //-------------

type
  { LCL Messages }

  TLMDrawItems = record
    Msg: Cardinal;
{$ifdef cpu64}
   UnusedMsg: Cardinal;
{$endif}
    Ctl: HWND;
    DrawItemStruct: PDrawItemStruct;
    Result: LRESULT;
  end;

  TLMDrawListItem = record
    // message from the interface to the LCL
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: PtrInt;
    DrawListItemStruct : PDrawListItemStruct;
    Result: LRESULT;
  end;

  TLMMeasureItem = record
    // message from the interface to the LCL
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    idCtl: PtrUint;
    MeasureItemStruct: PMeasureItemStruct;
    Result: LRESULT;
  end;

{$ifndef WINDOWS}
  TLMNoParams = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: array[0..1] of PtrInt;
    Result: LRESULT;
  end;
{$else}
  TLMNoParams = TWMNoParams;
{$endif}

  TLMEraseBkgnd = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    DC: HDC;
    Unused: PtrInt;
    Result: LRESULT;
  end;

  TLMGetText = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    TextMax: PtrInt;
    Text: PChar;
    Result: LRESULT;
  end;

  TLMGetTextLength = TLMNoParams;

  TLMKey = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
    Unused: Word;
{$ELSE}
    Unused: Word;
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
{$ENDIF}
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    KeyData: PtrInt;
    Result: LRESULT;
  end;

  TLMChar = TLMKey;
  TLMKeyDown = TLMKey;
  TLMKeyUp = TLMKey;
  TLMSysChar = TLMKey;
  TLMSysKeyDown = TLMKey;
  TLMSysKeyUp = TLMKey;


  TLMCut = TLMNoParams;
  TLMCopy = TLMNoParams;
  TLMPaste = TLMNoParams;

  TLMSetCursor = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    CursorWnd: HWND;
    case Boolean of
      False: (
        HitTest: SmallInt;
        MouseMsg: Word;
      );
      True: (
        Dummy: LPARAM;
        Result: LRESULT;
      );
  end;

  TLMMouse = record
    Msg : Cardinal;
    Keys: PtrInt;
    case Integer of
    0: (
       XPos: SmallInt;
       YPos: SmallInt);
    1: (
       Pos : TSmallPoint);
    2: (
       Dummy: LPARAM; // needed for64 bit alignment
       Result: LRESULT);
  end;

  TLMMouseMove = TLMMouse;

  TLMMove = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    MoveType: PtrInt; // 0 = update, 1 = force RequestAlign,
                      // 128 = Source is Interface (Widget has moved)
    case Integer of
    0: (
       XPos: Smallint;
       YPos: Smallint);
    1: (
       Pos : TSmallPoint);
    2: (
       Dummy: LPARAM; // needed for64 bit alignment
       Result: LRESULT);
  end;

  TLMActivate = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    Active: WordBool;
    Minimized: WordBool;
{$ELSE}
    Minimized: WordBool;
    Active: WordBool;
{$ENDIF}
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    ActiveWindow: HWND;
    Result: LRESULT;
  end;

  TLMNCActivate = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Active: LongBool;
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    Unused: LPARAM;
    Result: LRESULT;
  end;

  TLMNotify = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    IDCtrl: PtrInt;
    NMHdr: PNMHdr;
    Result: LRESULT;
  end;

  TLMNotifyFormat = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    From: HWND;
    Command: LPARAM;
    Result: LRESULT;
  end;

  TLMPaint = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    DC: HDC;
    PaintStruct: PPaintStruct;
    Result: LRESULT;
  end;

  PWindowPos = ^TWindowPos;
  tagWINDOWPOS = record
    hwnd: THANDLE; //hwnd: hwnd doesnt compile on the next line
    hwndInsertAfter: THANDLE;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    flags: Cardinal;
  end;
  TWindowPos = tagWINDOWPOS;
  WINDOWPOS = tagWINDOWPOS;

  TLMWindowPosMsg = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: WPARAM;
    WindowPos: PWindowPos;
    Result: LPARAM;
  end;

  TLMWindowPosChanged = TLMWindowPosMsg;
  TLMWindowPosChanging = TLMWindowPosMsg;

  {PNCCalcSizeParams}
  PNCCalcSizeParams = ^TNCCalcSizeParams;
  tagNCCalcSize_Params = record
    rgrc: array[0..2] of TRect;
    lpPos: PWindowPos;
  end;
  TNCCalcSizeParams = tagNCCalcSize_Params;
  ncCalcSizeParams = tagNCCalcSize_Params;


  TLMNCCalcSize = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    CalcValidRects: LongBool;
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    CalcSize_Params: PNCCalcSizeParams;
    Result: LResult;
  end;

  TLMSysColorChange = TLMNoParams;

  TLMSysCommand = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    case CmdType: PtrInt of
      SC_HOTKEY: (
        ActivateWnd: HWND;
        Result: LRESULT);
      SC_KEYMENU: (
{$IFDEF FPC_LITTLE_ENDIAN}
        Key: Word);
{$ELSE}
        Unused: Word;
        Key: Word);
{$ENDIF}
      SC_CLOSE, SC_HSCROLL, SC_MAXIMIZE, SC_MINIMIZE, SC_MOUSEMENU, SC_MOVE,
      SC_NEXTWINDOW, SC_PREVWINDOW, SC_RESTORE, SC_SCREENSAVE, SC_SIZE,
      SC_TASKLIST, SC_VSCROLL: (
        XPos: Smallint;
        YPos: Smallint);
  end;

  TLMSysDeadChar = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    CharCode: Word;
    Unused: Word;
{$ELSE}
    Unused: Word;
    CharCode: Word;
{$ENDIF}
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    KeyData: LPARAM;
    Result: LRESULT;
  end;


  TLMSystemError = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    ErrSpec: Word;
    Unused1 : Word;
{$ELSE}
    Unused1 : Word;
    ErrSpec: Word;
{$ENDIF}
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    Unused: LPARAM;
    Result: LRESULT;
  end;

  TLMTimeChange = TLMNoParams;

// todo: remove TLMSetText
  TLMSetText = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: WPARAM;
    Text: PChar;
    Result: LRESULT;
  end;

  // This is the event for the mouse whell
  PLMMouseEvent = ^TLMMouseEvent;
  TLMMouseEvent = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    Button: Word;         // 1=left, 2=right, 3=middle, it should also contain the ShiftState via the or operator, see for example MK_CONTROL
    WheelDelta: SmallInt; // -1 for up, 1 for down
{$ELSE}
    WheelDelta: SmallInt; // -1 for up, 1 for down
    Button: Word;         // 1=left, 2=right, 3=middle, it should also contain the ShiftState via the or operator, see for example MK_CONTROL
{$ENDIF}
{$ifdef cpu64}
    Unused1 : Longint;
{$endif cpu64}
    X: Smallint;          // under gtk this is longint
    Y: Smallint;          // ditto
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    Result: LRESULT;      // to fit std message size
    UserData: pointer;    // used under gtk
  end;

  TLMLButtonDown = TLMMouse;
  TLMRButtonDown = TLMMouse;
  TLMMButtonDown = TLMMouse;
  TLMXButtonDown = TLMMouse;
  TLMLButtonDblClk = TLMMouse;
  TLMRButtonDblClk = TLMMouse;
  TLMMButtonDblClk = TLMMouse;
  TLMXButtonDblClk = TLMMouse;
  TLMLButtonTripleClk = TLMMouse;
  TLMRButtonTripleClk = TLMMouse;
  TLMMButtonTripleClk = TLMMouse;
  TLMXButtonTripleClk = TLMMouse;
  TLMLButtonQuadClk = TLMMouse;
  TLMRButtonQuadClk = TLMMouse;
  TLMMButtonQuadClk = TLMMouse;
  TLMXButtonQuadClk = TLMMouse;
  TLMLButtonUp = TLMMouse;
  TLMRButtonUp = TLMMouse;
  TLMMButtonUp = TLMMouse;
  TLMXButtonUp = TLMMouse;

  TLMSetFocus = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    FocusedWnd: HWND;
    Unused: LPARAM;
    Result: LRESULT;
  end;

{$ifndef windows}
  TLMSize = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    SizeType: PtrInt; // see LCLType.pp (e.g. Size_Restored)
    Width: Word;
    Height: Word;
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    Result: LResult;
  End;
{$else}
  TLMSize = TWMSize;
{$endif}

  PLMessage = ^TLMessage;
{$ifndef windows}
  TLMessage = record
    Msg : Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    case Integer of
      0 : (
        WParam: LclType.WPARAM;
        LParam: LclType.LPARAM;
        Result: LclType.LRESULT);
      {$IFNDEF CPU64}
      // on a 64 bit platform these make no sense
      1 : (
{$IFDEF FPC_LITTLE_ENDIAN}
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
{$ELSE}
        WParamHi: Word;
        WParamLo: Word;
        LParamHi: Word;
        LParamLo: Word;
        ResultHi: Word;
        ResultLo: Word);
{$ENDIF}
      {$endif}
    end;
{$else}
  TLMessage = TMessage;
{$endif}

  TLMEnter = TLMNoParams;
  TLMExit  = TLMNoParams;

// MWE: TLMScroll has not the same size as the VCL/Winapi counterpart.
// IMO we don't have to force all widgetsets to be compatible in a shortcoming
// in the win32 API.
// So POS: SmallInt -> LongInt and a win32compatible smallpos is added
// Due to this, the record is a LongInt to large.

  TLMScroll = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    ScrollCode: SmallInt; // SB_xxx
    SmallPos: SmallInt;
{$ELSE}
    SmallPos: SmallInt;
    ScrollCode: SmallInt; // SB_xxx
{$ENDIF}
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    ScrollBar: HWND;
    Result: LRESULT;  // See remark
    Pos: LongInt;     //
  end;

  TLMHScroll = TLMScroll;
  TLMVScroll = TLMScroll;

{$ifndef windows}
  TLMShowWindow = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Show: LongBool;
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    Status: LPARAM;
    Result: LRESULT;
  end;
{$else}
  TLMShowWindow = TWMShowWindow;
{$endif}

{$ifndef windows}
  TLMKILLFOCUS = TLMSetFocus;
{$else}
  TLMKillFocus = TWMKillFocus;
{$endif}

  TLMNCHITTEST = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: WPARAM;
    case Integer of
    0 : (
         XPos : SmallInt;
         YPos : SmallInt);
    1 : (
         Pos : TSmallPoint);
    2: (
       Dummy: LPARAM; // needed for64 bit alignment
       Result: LRESULT);
  end;

  TLMDestroy = TLMNoParams;

  TLMCommand = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    ItemID: Word;
    NotifyCode: Word;
{$ELSE}
    NotifyCode: Word;
    ItemID: Word;
{$ENDIF}
{$ifdef cpu64}
    Unused : Longint;
{$endif cpu64}
    Ctl: HWND;
    Result: LRESULT;
  end;

  TLMContextMenu = record
    Msg: Cardinal;
    {$ifdef cpu64}
    UnusedMsg: Cardinal;
    {$endif}
    hWnd: HWND;
    case Integer of
    0: (
       XPos: Smallint;
       YPos: Smallint
       );
    1: (
       Pos: TSmallPoint;
       );
    2: (
       Dummy: LPARAM; // needed for64 bit alignment
       Result: LResult);
  end;

  TLMHelp = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
    Unused: LCLType.WPARAM;
    HelpInfo: PHelpInfo;
    Result: LRESULT;
  end;

{ Combo Box Notification Codes }

const
  CBN_ERRSPACE     = (-1);
  CBN_SELCHANGE    = 1;
  CBN_DBLCLK       = 2;
  CBN_SETFOCUS     = 3;
  CBN_KILLFOCUS    = 4;
  CBN_EDITCHANGE   = 5;
  CBN_EDITUPDATE   = 6;
  CBN_DROPDOWN     = 7;
  CBN_CLOSEUP      = 8;
  CBN_SELENDOK     = 9;
  CBN_SELENDCANCEL = 10;


function GetMessageName(const AMessage: Integer):  String;


Implementation


function GetMessageName(const AMessage: Integer):  String;
begin
  case AMessage of
  //-------------
  // lcl messages
  //
  // This should be a list of LCL specific messages
  // RECEIVED from the interface, here are no defines
  // of messages send to the interface
  //-------------

  LM_ACTIVATE        : Result:='LM_ACTIVATE';
  LM_SETFOCUS        : Result:='LM_SETFOCUS';
  LM_KILLFOCUS       : Result:='LM_KILLFOCUS';
  LM_SHOWWINDOW      : Result:='LM_SHOWWINDOW';
  LM_SIZE            : Result:='LM_SIZE';
  LM_MOVE            : Result:='LM_MOVE';

//  LM_MOUSEFIRST      : Result:='LM_MOUSEFIRST';
  LM_MOUSEMOVE       : Result:='LM_MOUSEMOVE';
  LM_LBUTTONDOWN     : Result:='LM_LBUTTONDOWN';
  LM_LBUTTONUP       : Result:='LM_LBUTTONUP';
  LM_CAPTURECHANGED  : Result:='LM_CAPTURECHANGED';
  LM_SELCHANGE       : Result:='LM_SELCHANGE';


  LM_USER           :Result:='LM_USER';
  LM_DESTROY        :Result:='LM_DESTROY';
  LM_ACTIVATEITEM   :Result:='LM_ACTIVATEITEM';
  LM_CHANGED        :Result:='LM_CHANGED';
  LM_FOCUS          :Result:='LM_FOCUS';
  LM_CLICKED        :Result:='LM_CLICKED';
  LM_ENTER          :Result:='LM_ENTER';
  LM_LEAVE          :Result:='LM_LEAVE';
  LM_CHECKRESIZE    :Result:='LM_CHECKRESIZE';
  LM_SETEDITABLE    :Result:='LM_SETEDITABLE';
  LM_MOVEWORD       :Result:='LM_MOVEWORD';
  LM_MOVEPAGE       :Result:='LM_MOVEPAGE';
  LM_MOVETOROW      :Result:='LM_MOVETOROW';
  LM_MOVETOCOLUMN   :Result:='LM_MOVETOCOLUMN';
  LM_KILLCHAR       :Result:='LM_KILLCHAR';
  LM_KILLWORD       :Result:='LM_KILLWORD';
  LM_KILLLINE       :Result:='LM_KILLLINE';
  LM_CUT            :Result:='LM_CUT';
  LM_COPY           :Result:='LM_COPY';
  LM_PASTE          :Result:='LM_PASTE';
  LM_CLEAR          :Result:='LM_CLEAR';
  LM_CONFIGUREEVENT :Result:='LM_CONFIGUREEVENT';
  LM_PAINT          :Result:='LM_PAINT';
  LM_KEYDOWN        :Result:='LM_KEYDOWN';
  LM_KEYUP          :Result:='LM_KEYUP';
  LM_TIMER          :Result:='LM_TIMER';
  LM_EXIT           :Result:='LM_EXIT';
  LM_CLOSEQUERY     :Result:='LM_CLOSEQUERY';
  LM_DRAGSTART      :Result:='LM_DRAGSTART';
  LM_DEACTIVATE     :Result:='LM_DEACTIVATE';

  LM_MONTHCHANGED   :Result:='LM_MONTHCHANGED';
  LM_YEARCHANGED    :Result:='LM_YEARCHANGED';
  LM_DAYCHANGED     :Result:='LM_DAYCHANGED';

  //LM_MOUSEFIRST2    :Result:='';
  LM_LBUTTONTRIPLECLK :Result:='LM_LBUTTONTRIPLECLK';
  LM_LBUTTONQUADCLK :Result:='LM_LBUTTONQUADCLK';
  LM_MBUTTONTRIPLECLK :Result:='LM_MBUTTONTRIPLECLK';
  LM_MBUTTONQUADCLK :Result:='LM_MBUTTONQUADCLK';
  LM_RBUTTONTRIPLECLK :Result:='LM_RBUTTONTRIPLECLK';
  LM_RBUTTONQUADCLK :Result:='LM_RBUTTONQUADCLK';
  LM_MOUSEENTER     :Result:='LM_MOUSEENTER';
  LM_MOUSELEAVE     :Result:='LM_MOUSELEAVE';
  //LM_MOUSELAST2     :Result:='';

  LM_GRABFOCUS      :Result:='LM_GRABFOCUS';

  LM_DRAWLISTITEM   :Result:='LM_DRAWLISTITEM';

  // these IDs are reserved for internal messages in the interfaces
  LM_INTERFACEFIRST :Result:='LM_INTERFACEFIRST';
  LM_INTERFACELAST  :Result:='LM_INTERFACELAST';

  LM_UNKNOWN        :Result:='LM_UNKNOWN';
  else
    Result := Format('Unknown message 0x%x (%d)', [AMessage, AMessage]);
  end;
  Result  := Trim(Result);
end;


end.
