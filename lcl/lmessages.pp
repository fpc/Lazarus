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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  {$ifdef win32}
  {$ifndef ver1_0}
  ,messages
  {$endif ver1_0}
  {$endif win32}
  ;

const
  //-------------
  // lcl messages
  //
  // This should be a list of LCL specific messages
  // RECEIVED from the interface, here are no defines
  // of messages send to the interface
  //-------------
  LM_USER           = $400; // MWE: changed from $100 to $400 since they were in the windows range
  WM_USER           = LM_USER;
  LM_DESTROY        = LM_User+2;
  LM_ACTIVATEITEM   = LM_User+4;
  LM_CHANGED        = LM_User+5;
  LM_FOCUS          = LM_User+6;
  LM_CLICKED        = LM_User+7;
  LM_PRESSED        = LM_User+8;
  LM_RELEASED       = LM_User+9;
  LM_MOVECURSOR     = LM_User+10;
  LM_ENTER          = LM_User+11;
  LM_LEAVE          = LM_User+12;
  //LM_SIZEALLOCATE = LM_User+13;
  LM_CHECKRESIZE    = LM_User+14;
  //LM_SHOW = LM_User+15; // Windows Compatability
  LM_INSERTTEXT     = LM_User+16;
  LM_DELETETEXT     = LM_User+17;
  LM_SETEDITABLE    = LM_User+18;
  LM_MOVEWORD       = LM_User+19;
  LM_MOVEPAGE       = LM_User+20;
  LM_MOVETOROW      = LM_User+21;
  LM_MOVETOCOLUMN   = LM_User+22;
  LM_KILLCHAR       = LM_User+23;
  LM_KILLWORD       = LM_User+24;
  LM_KILLLINE       = LM_User+25;
  LM_CUTTOCLIP      = LM_User+26;
  LM_COPYTOCLIP     = LM_User+27;
  LM_PASTEFROMCLIP  = LM_User+28;
  //LM_MOVERESIZE   = LM_User+29;
  LM_EXPOSEEVENT    = LM_User+30;
  LM_CONFIGUREEVENT = LM_User+31;
  //LM_DRAW         = LM_User+32;  //LM_DRAW and LM_PAINT are the same.
  LM_PAINT          = LM_User+32;
  LM_OK_CLICKED     = LM_USER+36;
  LM_CANCEL_CLICKED = LM_USER+37;
  //LM_KEYDOWN      = LM_User+38; // Windows Compatability
  //LM_KEYUP        = LM_USER+39;  // Windows Compatability
  LM_TIMER          = LM_USER+40;
  //LM_MOUSEBTNPRESS  = LM_USER+41;
  //LM_MOUSEBTNRELEASE  = LM_USER+42;
  LM_EXIT           = LM_USER+60;
  LM_CLOSEQUERY     = LM_USER+62;
  LM_DRAGSTART      = LM_USER+63;
  LM_DEACTIVATE     = LM_USER+64;  //used when a form is no longer in front
  LM_QUIT           = LM_USER+65;

  LM_MONTHCHANGED   = LM_USER+66;
  LM_YEARCHANGED    = LM_USER+67;
  LM_DAYCHANGED     = LM_USER+68;

  LM_MOUSEFIRST2    = LM_USER+70;
  LM_LBUTTONTRIPLECLK = LM_MOUSEFIRST2 +0;
  LM_LBUTTONQUADCLK = LM_MOUSEFIRST2   +1;
  LM_MBUTTONTRIPLECLK = LM_MOUSEFIRST2 +2;
  LM_MBUTTONQUADCLK = LM_MOUSEFIRST2   +3;
  LM_RBUTTONTRIPLECLK = LM_MOUSEFIRST2 +4;
  LM_RBUTTONQUADCLK = LM_MOUSEFIRST2   +5;
  LM_MOUSEENTER     = LM_MOUSEFIRST2   +6;
  LM_MOUSELEAVE     = LM_MOUSEFIRST2   +7;
  LM_MOUSELAST2     = LM_MOUSELEAVE;
  // for triple and quad clicks see below

  LM_GRABFOCUS      = LM_USER+79;

  LM_DRAWLISTITEM   = LM_User+80;

  LM_SETCURSOR      = LM_User+81;

  LM_INTERNALPAINT  = LM_User + 90;

  // these IDs are reserved for internal messages in the interfaces
  LM_INTERFACEFIRST = LM_User+99;
  LM_INTERFACELAST  = LM_User+199;

  LM_UNKNOWN        = LM_INTERFACELAST+1;


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
  // not yet these are defined as messages to the interface
  //LM_CREATE            = $0001;
  //LM_DESTROY           = $0002;
  LM_MOVE              = $0003;

  LM_SIZE              = $0005;
  LM_ACTIVATE          = $0006;
  LM_SETFOCUS          = $0007;
  LM_KILLFOCUS         = $0008;
  LM_ENABLE            = $000A;
  LM_GETTEXTLENGTH     = $000E;
  LM_ERASEBKGND        = $0014;

  LM_SHOWWINDOW        = $0018;

  LM_CANCELMODE        = $001F;
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
  LM_NOTIFYFORMAT      = $0055;

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
  LM_MButtonDown       = $0207;
  LM_MBUTTONUP         = $0208;
  LM_MBUTTONDBLCLK     = $0209;
  LM_MOUSEWHEEL        = $020A;
  LM_MOUSELAST         = $020A;

  LM_PARENTNOTIFY      = $0210;
  LM_CAPTURECHANGED    = $0215;
  LM_DROPFILES         = $0233;
  
  LM_SELCHANGE         = $0234;


  //-------------
  // End of Windows Compatability and messages
  //-------------

type
// Defined in LCLType
//  UINT = LongWord;
//  BOOL = Boolean;


  { LCL Messages }

  TLMDrawItems = record
    Msg: Cardinal;
    Ctl: HWND;
    DrawItemStruct: PDrawItemStruct;
    Result: LRESULT;
  end;

  TLMDrawListItem = record
    // message from the interface to the LCL
    Msg: Cardinal;
    Unused: PtrInt;
    DrawListItemStruct : PDrawListItemStruct;
    Result: LRESULT;
  end;

  TLMMeasureItem = record
    // message from the interface to the LCL
    Msg: Cardinal;
    idCtl: PtrUint;
    MeasureItemStruct: PMeasureItemStruct;
    Result: LRESULT;
  end;

{$if defined(ver1_0) or not(defined(win32))}
  TLMNoParams = record
    Msg: Cardinal;
    Unused: array[0..1] of PtrInt;
    Result: LRESULT;
  end;
{$else}
  TLMNoParams = TWMNoParams;
{$endif}

(*
//TODO: Remove
  TLMCanvasCreate = Record
     pparent : Pointer;
     pCanvas : Pointer;
  end;

  pTLMCanvasCreate = ^TLMCanvasCreate;

  PLMCanvasDrawRect = ^TLMCanvasDrawRect;
  TLMCanvasDrawRect = Record
     R : TRect;
     ReDraw : Boolean;
     PenColor : TGraphicsColor;
    end;

  PLMCanvasDrawLine = ^TLMCanvasDrawLine;
  TLMCanvasDrawLine = Record
     x1 : Integer;
     y1 : Integer;
     x2 : Integer;
     y2 : Integer;
     PenColor : TGraphicsColor;
     ReDraw : Boolean;
    end;

  PLMCanvasDrawText = ^TLMCanvasDrawText;
  TLMCanvasDrawText = Record
     x1 : Integer;
     y1 : Integer;
     Str : String;
     Font : TObject;
     PenColor : TGraphicsColor;
     ReDraw : Boolean;
    end;
*)

  TLMEraseBkgnd = record
    Msg: Cardinal;
    DC: HDC;
    Unused: PtrInt;
    Result: LRESULT;
  end;

  TLMGetText = record
    Msg: Cardinal;
    TextMax: PtrInt;
    Text: PChar;
    Result: LRESULT;
  end;

  TLMGetTextLength = TLMNoParams;

(*
//TODO: remove

  PLMInsertText = ^TLMInsertText;
  TLMInsertText = record
    Msg : Cardinal;
    NewText : String;
    Length : Integer;
    Position : Integer;
    UserData : Pointer;
  end;
*)

  TLMKey = record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: PtrInt;
    Result: LRESULT;
  end;

  TLMChar = TLMKey;
  TLMKeyDown = TLMKey;
  TLMKeyUp = TLMKey;
  TLMSysChar = TLMKey;
  TLMSysKeyDown = TLMKey;
  TLMSysKeyUp = TLMKey;


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
    Active: WordBool;
    Minimized: WordBool;
    ActiveWindow: HWND;
    Result: LRESULT;
  end;

  TLMNCActivate = record
    Msg: Cardinal;
    Active: LongBool;
    Unused: LPARAM;
    Result: LRESULT;
  end;

  TLMNotify = record
    Msg: Cardinal;
    IDCtrl: PtrInt;
    NMHdr: PNMHdr;
    Result: LRESULT;
  end;

  TLMNotifyFormat = record
    Msg: Cardinal;
    From: HWND;
    Command: LPARAM;
    Result: LRESULT;
  end;

  TLMPaint = record
    Msg: Cardinal;
    DC: HDC;
    PaintStruct: PPaintStruct;
    Result: LRESULT;
  end;

(*
//TODO: Remove
  TLMResize = record
    Msg : Cardinal;
    Left  : Integer;
    Top : Integer;
    Width : Integer;
    Height : Integer;
    UserData : Pointer;
  end;

  TLMMoveResize = TLMResize;
*)

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
    Unused: WPARAM;
    WindowPos: PWindowPos;
    Result: LPARAM;
  end;

  TLMWindowPosChanged = TLMWindowPosMsg;
  TLMWindowPosChanging = TLMWindowPosMsg;

  {PNCCalcSizeParams}
  PNCCalcSizeParams = ^TNCCalcSizeParams;
  tagNCCalcSize_Params = packed record
    rgrc: array[0..2] of TRect;
    lpPos: PWindowPos;
  end;
  TNCCalcSizeParams = tagNCCalcSize_Params;
  ncCalcSizeParams = tagNCCalcSize_Params;


  TLMNCCalcSize = record
    Msg: Cardinal;
    CalcValidRects: LongBool;
    CalcSize_Params: PNCCalcSizeParams;
    Result: Longint;
  end;

  TLMSysColorChange = TLMNoParams;

  TLMSysCommand = record
    Msg: Cardinal;
    case CmdType: PtrInt of
      SC_HOTKEY: (
        ActivateWnd: HWND;
        Result: LRESULT);
      SC_KEYMENU: (
        Key: Word);
      SC_CLOSE, SC_HSCROLL, SC_MAXIMIZE, SC_MINIMIZE, SC_MOUSEMENU, SC_MOVE,
      SC_NEXTWINDOW, SC_PREVWINDOW, SC_RESTORE, SC_SCREENSAVE, SC_SIZE,
      SC_TASKLIST, SC_VSCROLL: (
        XPos: Smallint;
        YPos: Smallint);
  end;

  TLMSysDeadChar = record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: LPARAM;
    Result: LRESULT;
  end;


  TLMSystemError = record
    Msg: Cardinal;
    ErrSpec: Word;
    Unused: LPARAM;
    Result: LRESULT;
  end;

  TLMTimeChange = TLMNoParams;

//  TODO: REmove
(*
  //Used to set the statusbar's text
  PLMSetControlText = ^TLMSetControlText;
  TLMSetControlText = record
    fCompStyle : Longint;
    Panel : Integer;
    Userdata : PChar;
  end;
*)

  TLMSetText = record
    Msg: Cardinal;
    Unused: WPARAM;
    Text: PChar;
    Result: LRESULT;
  end;

//TODO: Remove
(*
  TLMKeyEvent = Record
    Msg: Cardinal;
    KeyChar: Char;
    Key: Word;
    State : TShiftState;
    Length : Integer;
    Str : PChar;
    UserData : Pointer;
   end;
*)

//TODO: make compatible with WM_MOUSEWHEEL ?
  PLMMouseEvent = ^TLMMouseEvent;
  TLMMouseEvent = record
    Msg : Cardinal;
    Button : LongInt;
    WheelDelta : Longint; { -1 for up, 1 for down }
    State : TShiftState;
    X     : Integer;
    Y    : Integer;
    UserData : Pointer;
  end;

  TLMLButtonDown = TLMMouse;
  TLMRButtonDown = TLMMouse;
  TLMMButtonDown = TLMMouse;
  TLMLButtonDblClk = TLMMouse;
  TLMRButtonDblClk = TLMMouse;
  TLMMButtonDblClk = TLMMouse;
  TLMLButtonTripleClk = TLMMouse;
  TLMRButtonTripleClk = TLMMouse;
  TLMMButtonTripleClk = TLMMouse;
  TLMLButtonQuadClk = TLMMouse;
  TLMRButtonQuadClk = TLMMouse;
  TLMMButtonQuadClk = TLMMouse;
  TLMLButtonUp = TLMMouse;
  TLMRButtonUp = TLMMouse;
  TLMMButtonUp = TLMMouse;

  TLMSetFocus = record
    Msg: Cardinal;
    FocusedWnd: HWND;
    Unused: LPARAM;
    Result: LRESULT;
  end;

{$if defined(ver1_0) or not(defined(win32))}
  TLMSize = record
    Msg: Cardinal;
    SizeType: PtrInt; // see LCLType.pp (e.g. Size_Restored)
    Width: Word;
    Height: Word;
    Result: LResult;
  End;
{$else}
  TLMSize = TWMSize;
{$endif}

  TLMNoPara = record
    Msg: Cardinal;
  end;

  PLMessage = ^TLMessage;
{$if defined(ver1_0) or not(defined(win32))}
  TLMessage = record
    Msg : Cardinal;
    case Integer of
      0 : (
        WParam: LclType.WPARAM;
      	LParam: LclType.LPARAM;
      	Result: LclType.LRESULT);
      {$IFNDEF CPU64}
      // on a 64 bit platform these make no sense
      1 : (
      	WParamLo: Word;
      	WParamHi: Word;
      	LParamLo: Word;
      	LParamHi: Word;
      	ResultLo: Word;
      	ResultHi: Word);
      {$ENDIF}
    end;
{$else}
  TLMessage = TMessage;
{$endif}

  TLMEnter = TLMNoPara;
  TLMExit  = TLMNoPara;

{$if defined(ver1_0) or not(defined(win32))}
  TLMScroll = record
    Msg: Cardinal;
    ScrollCode: SmallInt; // SB_xxx
    Pos: SmallInt;
    ScrollBar: HWND;
    Result: LRESULT;
  end;

  TLMHScroll = TLMScroll;
  TLMVScroll = TLMScroll;
{$else}
  TLMScroll = TWMScroll;
  TLMHScroll = TWMScroll;
  TLMVScroll = TWMScroll;
{$endif}

{$if defined(ver1_0) or not(defined(win32))}
  TLMShowWindow = record
    Msg: Cardinal;
    Show: LongBool;
    Status: LPARAM;
    Result: LRESULT;
  end;
{$else}
  TLMShowWindow = TWMShowWindow;
{$endif}

{$if defined(ver1_0) or not(defined(win32))}
  TLMKILLFOCUS = TLMSetFocus;
{$else}
  TLMKillFocus = TWMKillFocus;
{$endif}

  TLMNCHITTEST = record
    Msg: cardinal;
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
    ItemID: Word;
    NotifyCode: Word;
    Ctl: HWND;
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
  LM_USER           :Result:='LM_USER';
  //WM_USER           :Result:='';
  LM_DESTROY        :Result:='LM_DESTROY';
  LM_ACTIVATEITEM   :Result:='LM_ACTIVATEITEM';
  LM_CHANGED        :Result:='LM_CHANGED';
  LM_FOCUS          :Result:='LM_FOCUS';
  LM_CLICKED        :Result:='LM_CLICKED';
  LM_PRESSED        :Result:='LM_PRESSED';
  LM_RELEASED       :Result:='LM_RELEASED';
  LM_MOVECURSOR     :Result:='LM_MOVECURSOR';
  LM_ENTER          :Result:='LM_ENTER';
  LM_LEAVE          :Result:='LM_LEAVE';
  //LM_SIZEALLOCATE :Result:='LM_SIZEALLOCATE';
  LM_CHECKRESIZE    :Result:='LM_CHECKRESIZE';
  //LM_SHOW :Result:='LM_SHOW';
  LM_INSERTTEXT     :Result:='LM_INSERTTEXT';
  LM_DELETETEXT     :Result:='LM_DELETETEXT';
  LM_SETEDITABLE    :Result:='LM_SETEDITABLE';
  LM_MOVEWORD       :Result:='LM_MOVEWORD';
  LM_MOVEPAGE       :Result:='LM_MOVEPAGE';
  LM_MOVETOROW      :Result:='LM_MOVETOROW';
  LM_MOVETOCOLUMN   :Result:='LM_MOVETOCOLUMN';
  LM_KILLCHAR       :Result:='LM_KILLCHAR';
  LM_KILLWORD       :Result:='LM_KILLWORD';
  LM_KILLLINE       :Result:='LM_KILLLINE';
  LM_CUTTOCLIP      :Result:='LM_CUTTOCLIP';
  LM_COPYTOCLIP     :Result:='LM_COPYTOCLIP';
  LM_PASTEFROMCLIP  :Result:='LM_PASTEFROMCLIP';
  //LM_MOVERESIZE   :Result:='LM_MOVERESIZE';
  LM_EXPOSEEVENT    :Result:='LM_EXPOSEEVENT';
  LM_CONFIGUREEVENT :Result:='LM_CONFIGUREEVENT';
  //LM_DRAW         :Result:='LM_DRAW';
  LM_PAINT          :Result:='LM_PAINT';
  LM_OK_CLICKED     :Result:='LM_OK_CLICKED';
  LM_CANCEL_CLICKED :Result:='LM_CANCEL_CLICKED';
  //LM_KEYDOWN      :Result:='LM_KEYDOWN';
  //LM_KEYUP        :Result:='LM_KEYUP';
  LM_TIMER          :Result:='LM_TIMER';
  //LM_MOUSEBTNPRESS  :Result:='LM_MOUSEBTNPRESS';
  //LM_MOUSEBTNRELEASE  :Result:='LM_MOUSEBTNRELEASE';
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

  LM_INTERNALPAINT  :Result:='LM_INTERNALPAINT';

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

{
  $Log$
  Revision 1.125  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.124  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.123  2004/09/24 19:02:38  micha
  convert LM_MOVEPAGE message to interface method

  Revision 1.122  2004/09/24 18:00:51  micha
  convert LM_NB_UPDATETAB message to interface method

  Revision 1.121  2004/09/24 17:20:43  micha
  convert LM_SETGEOMETRY message to interface method

  Revision 1.120  2004/09/24 15:31:01  micha
  convert LM_LB_GETTOPINDEX and LM_LB_SETTOPINDEX message to interface methods

  Revision 1.119  2004/09/24 14:50:57  micha
  convert LM_SETDESIGNING message to TWidgetSet method

  Revision 1.118  2004/09/24 10:49:56  micha
  remove obsolete messages

  Revision 1.117  2004/09/24 07:52:35  micha
  convert LM_SETPROPERTIES message to interface method for TCustomTrackBar
  remove message LM_SETPROPERTIES, conversion done

  Revision 1.116  2004/09/19 19:39:10  micha
  undo removal of LM_SETDESIGNING; used by lazarus ide (main.pp)

  Revision 1.114  2004/09/19 18:50:28  micha
  convert LM_SETVALUE message to interface methods

  Revision 1.113  2004/09/18 17:07:57  micha
  convert LM_GETVALUE message to interface method

  Revision 1.112  2004/09/18 11:06:47  micha
  remove LM_RECREATEWND message, as it is not used by LCL

  Revision 1.111  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.110  2004/09/17 10:56:24  micha
  convert LM_SHORTCUT message to interface methods

  Revision 1.109  2004/09/17 07:55:13  micha
  convert LM_SETBORDER message to interface method
  fix widgetsets virtual methods to be published
  fix compilation debugging widgetset registration

  Revision 1.108  2004/09/16 14:32:31  micha
  convert LM_SETSELMODE message to interface method

  Revision 1.107  2004/09/16 13:57:29  micha
  convert LM_SETSEL message to interface method

  Revision 1.106  2004/09/16 13:30:48  micha
  convert LM_SORT message to interface method

  Revision 1.105  2004/09/15 19:38:55  micha
  convert LM_GETSEL message to interface method

  Revision 1.104  2004/09/15 19:04:39  micha
  convert LM_GETSELCOUNT message to interface method

  Revision 1.103  2004/09/15 18:50:33  micha
  remove LM_GETLINECOUNT message as it is not used by the LCL

  Revision 1.102  2004/09/15 17:21:22  micha
  convert LM_GETITEMINDEX and LM_SETITEMINDEX messages to interface methods

  Revision 1.101  2004/09/15 14:45:39  micha
  convert LM_GETITEMS message to interface method

  Revision 1.100  2004/09/15 07:57:59  micha
  convert LM_SETFORMICON message to interface method

  Revision 1.99  2004/09/14 15:48:28  micha
  convert LM_INVALIDATE message to interface method

  Revision 1.98  2004/09/14 14:41:17  micha
  convert LM_INSERTTOOLBUTTON and LM_DELETETOOLBUTTON messages to interface methods; warning: still very ugly code, as if it is "OldToolbar" so probably, obsolete

  Revision 1.97  2004/09/14 12:45:29  micha
  convert LM_SETTABPOSITION message to interface method

  Revision 1.96  2004/09/14 10:06:26  micha
  convert LM_REDRAW message to interface method (in twidgetset)

  Revision 1.95  2004/09/13 19:57:30  micha
  convert LM_SHOWTABS message to interface method

  Revision 1.94  2004/09/13 19:06:04  micha
  convert LM_ADDPAGE and LM_REMOVEPAGE messages to new interface methods

  Revision 1.93  2004/09/13 14:34:53  micha
  convert LM_TB_BUTTONCOUNT to interface method

  Revision 1.92  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.91  2004/09/12 19:50:35  micha
  convert LM_SETSIZE message to new interface method

  Revision 1.90  2004/09/12 13:21:37  micha
  remove obsolete message LM_DRAGINFOCHANGED

  Revision 1.89  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.88  2004/09/11 17:29:10  micha
  convert LM_POPUPSHOW message to interface method

  Revision 1.87  2004/09/11 15:01:22  micha
  remove obsolete LM_SETFILTER and LM_SETFILENAME messages

  Revision 1.86  2004/09/11 14:54:01  micha
  convert LM_BTNDEFAULT_CHANGED message to interface method

  Revision 1.85  2004/09/11 13:38:37  micha
  convert LM_BRINGTOFRONT message to interface method
  NOTE: was only used for tapplication, not from other controls

  Revision 1.84  2004/09/11 13:06:48  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.83  2004/09/10 20:19:13  micha
  convert LM_CLB_G/SETCHECKED to interface methods

  Revision 1.82  2004/09/10 18:58:21  micha
  convert LM_ATTACHMENU to interface method

  Revision 1.81  2004/09/10 18:06:12  micha
  remove LM_GETLABEL since it is not used (anymore)

  Revision 1.80  2004/09/10 17:59:57  micha
  convert LM_APPENDTEXT to interface method

  Revision 1.79  2004/09/10 14:38:29  micha
  convert lm_gettext to new interface methods
  remove lm_settext replacement settext methods in twidgetsets

  Revision 1.78  2004/09/10 11:20:44  micha
  remove LM_SETTEXT message as it is not used

  Revision 1.77  2004/09/10 09:43:12  micha
  convert LM_SETLABEL message to interface methods

  Revision 1.76  2004/09/08 20:47:16  micha
  convert LM_SHOWHIDE message to new intf method TWSWinControl.ShowHide

  Revision 1.75  2004/09/08 19:09:34  micha
  convert LM_SETCOLOR message to new intf method TWSWinControl.SetColor

  Revision 1.74  2004/09/07 10:26:16  micha
  fix logs to get rid of comment level 2 warning

  Revision 1.73  2004/09/07 09:44:46  micha
  convert lcl messages to new interface using methods: LM_G/SETSELSTART, LM_G/SETSELLEN, LM_G/SETLIMITTEXT

  Revision 1.72  2004/08/30 16:37:58  mattias
  added OnUTF8KeyPresss

  Revision 1.71  2004/08/25 17:08:10  micha
  use new lcl interface methods instead of messages (for win32; twscustomlistview)

  Revision 1.70  2004/08/25 15:04:44  micha
  use new lcl interface methods instead of messages (for win32; twsbitbtn)

  Revision 1.69  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.68  2004/08/11 20:57:09  mattias
  moved intfstrconsts.pp to lclstrconsts.pas, implemented TPenHandleCache

  Revision 1.67  2004/07/16 21:49:00  mattias
  added RTTI controls

  Revision 1.66  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.65  2004/06/18 20:15:06  micha
  remove obsolete LM_LOADXPM message

  Revision 1.64  2004/06/14 12:54:02  micha
  fix designer cursor to not set Form.Cursor directly

  Revision 1.63  2004/05/14 17:17:29  micha
  add measureitem message and struct

  Revision 1.62  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.61  2004/04/04 17:10:05  marc
  Patch from Andrew Haines

  Revision 1.60  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.59  2004/01/03 11:57:47  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.58  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.57  2003/12/21 18:21:32  mattias
  implemented ShowAll and hide hints for unused package units option

  Revision 1.56  2003/11/15 13:07:09  mattias
  added ambigious unit check for IDE

  Revision 1.55  2003/10/23 16:15:30  micha
  compatibility with new 1.1

  Revision 1.54  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.53  2003/09/17 19:40:46  ajgenius
  Initial DoubleBuffering Support for GTK2

  Revision 1.52  2003/08/25 16:43:32  mattias
  moved many graphics types form graphtype.pp to graphics.pp

  Revision 1.51  2003/08/19 12:23:23  mattias
  moved types from graphtype.pp back to graphics.pp

  Revision 1.50  2003/07/26 13:26:56  mattias
  fixed WindowProc

  Revision 1.49  2003/07/07 23:58:43  marc
  + Implemented TCheckListBox.Checked[] property

  Revision 1.48  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.47  2003/04/08 00:09:03  mattias
  added LM_APPENDTEXT from hernan

  Revision 1.46  2003/02/27 09:52:00  mattias
  published TImgList.Width and Height

  Revision 1.45  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.44  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.43  2002/12/27 08:46:32  mattias
  changes for fpc 1.1

  Revision 1.42  2002/12/16 09:02:27  mattias
  applied win32 notebook patch from Vincent

  Revision 1.41  2002/12/01 22:00:34  mattias
  fixed DeleteCriticalSection

  Revision 1.40  2002/11/23 13:48:43  mattias
  added Timer patch from Vincent Snijders

  Revision 1.39  2002/11/21 18:49:52  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.38  2002/10/25 10:42:08  lazarus
  MG: broke minor circles

  Revision 1.37  2002/10/25 08:25:43  lazarus
  MG: broke circle stdctrls.pp <-> forms.pp

  Revision 1.36  2002/10/10 08:51:13  lazarus
  MG: added paint messages for some gtk internal widgets

  Revision 1.35  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.34  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.33  2002/10/03 14:47:29  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.32  2002/09/16 08:54:03  lazarus
  MG: gtk mlouse events can now be fetched before or after

  Revision 1.31  2002/09/10 10:00:27  lazarus
  MG: TListView now works handleless and SetSelection implemented

  Revision 1.30  2002/09/04 09:32:17  lazarus
  MG: improved streaming error handling

  Revision 1.29  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.28  2002/08/28 09:40:48  lazarus
  MG: reduced paint messages and DC getting/releasing

  Revision 1.27  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.26  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.25  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.24  2002/03/29 17:12:52  lazarus
  MG: added Triple and Quad mouse clicks to lcl and synedit

  Revision 1.23  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.22  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.21  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.20  2002/02/18 22:46:11  lazarus
  Implented TMenuItem.ShortCut (not much tested).

  Revision 1.19  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.18  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.17  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.16  2001/12/14 18:38:55  lazarus
  Changed code for TListView
  Added a generic Breakpoints dialog
  Shane

  Revision 1.15  2001/12/10 07:47:58  lazarus
  MG: fixed typo

  Revision 1.14  2001/12/05 18:23:47  lazarus
  Added events to Calendar
  Shane

  Revision 1.13  2001/11/21 19:32:32  lazarus
  TComboBox can now be moved in FormEditor
  Shane

  Revision 1.12  2001/11/14 17:46:58  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.11  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.10  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.9  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.8  2001/01/23 23:33:54  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.7  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.6  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.5  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.4  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.3  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.2  2000/07/23 10:49:47  lazarus
  added text for LM_Destroy, stoppok

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.71  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.70  2000/06/13 20:50:42  lazarus
  MWE:
    - Started to remove obsolete/dead code/messages

  HJO:
    * Fixed messages in showmodal of 2nd form
    * Fixed modal result for button

  Revision 1.69  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.68  2000/05/11 22:04:15  lazarus
  MWE:
    + Added messagequeue
    * Recoded SendMessage and Peekmessage
    + Added postmessage
    + added DeliverPostMessage

  Revision 1.67  2000/05/03 17:19:29  lazarus
  Added the TScreem forms code by hongli@telekabel.nl
  Shane

  Revision 1.66  2000/04/18 14:02:32  lazarus
  Added Double Clicks.  Changed the callback in gtkcallback for the buttonpress event to check the event type.
  Shane

  Revision 1.65  2000/04/17 19:50:06  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.64  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.63  2000/03/23 20:40:03  lazarus
  Added some drag code
  Shane

  Revision 1.62  2000/03/21 18:53:28  lazarus
  Added code for TBitBtn. Not finished but looks like mostly working.
  Shane

  Revision 1.61  2000/03/15 20:15:32  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.60  2000/03/03 20:22:03  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.59  2000/03/01 00:41:03  lazarus
  MWE:
    Fixed updateshowing problem
    Added some debug code to display the name of messages
    Did a bit of cleanup in main.pp to get the code a bit more readable
      (my editor does funny things with tabs if the indent differs)

  Revision 1.58  2000/02/22 22:19:50  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.57  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.56  2000/01/18 22:18:35  lazarus

  Moved bitmap creation into appropriate place. Cleaned up a bit.
  Finished DeleteObject procedure.

  Revision 1.55  2000/01/17 20:36:26  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.54  2000/01/16 20:24:42  lazarus
  Did some introductory work on TScreen.
  Only the PixelsPerInch property is implemented at the moment.

  Revision 1.53  2000/01/14 00:33:31  lazarus
  MWE:
    Added Scrollbar messages

  Revision 1.52  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.51  2000/01/10 21:24:12  lazarus
  Minor cleanup and changes.

  Revision 1.50  2000/01/10 00:07:13  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.49  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.48  1999/12/30 19:49:07  lazarus
  *** empty log message ***

  Revision 1.47  1999/12/30 10:38:59  lazarus

    Some changes to Checkbox code.
      stoppok

  Revision 1.46  1999/12/29 20:38:23  lazarus
  Modified the toolbar so it now displays itself.  However, I can only add one button at this point.  I will fix that soon....

  Shane

  Revision 1.45  1999/12/22 01:16:04  lazarus
  MWE:
    Changed/recoded keyevent callbacks
    We Can Edit!
    Commented out toolbar stuff

  Revision 1.44  1999/12/21 21:35:54  lazarus
  committed the latest toolbar code.  Currently it doesn't appear anywhere and I have to get it to add buttons correctly through (I think) setstyle.  I think I'll implement the LM_TOOLBARINSERTBUTTON call there.
  Shane

  Revision 1.43  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.42  1999/12/20 21:37:12  lazarus
  Added ISRIGHTTOLEFT in menus file.
  Added ISACCEL in forms.pp
  Shane

  Revision 1.40  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.39  1999/12/14 22:21:11  lazarus
  *** empty log message ***

  Revision 1.38  1999/12/07 01:19:26  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.37  1999/11/13 13:03:34  lazarus
  MWE:
    Started to implement some platform dependent WINAPI stuff
    Added a baseclass for InterfaceObject
    Started messing around with canvasses

  Revision 1.36  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.35  1999/11/05 00:34:11  lazarus
  MWE: Menu structure updated, events and visible code not added yet

  Revision 1.34  1999/11/02 16:02:34  lazarus
  Added a bunch of wndproc stuff and a lot of functions that really don't do a thing at this point.
  Shane

  Revision 1.33  1999/10/28 19:25:10  lazarus
  Added a ton of messaging stuff
  Shane

  Revision 1.32  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.31  1999/10/27 13:13:23  lazarus
  Fixed a mesage prob.
  Shane

  Revision 1.29  1999/10/25 21:07:49  lazarus
  Many changes for compatability made again..

  Shane

  Revision 1.28  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.27  1999/10/22 18:39:43  lazarus
  Added kEYUP- KeyPress - Keydown, etc.

  Shane

  Revision 1.26  1999/10/21 21:33:29  lazarus
  Made many changes to the Messages and LMessages units
  Shane

  Revision 1.25  1999/10/19 21:16:23  lazarus
  TColor added to graphics.pp

  Revision 1.24  1999/09/26 13:30:15  lazarus

     Implemented OnEnter & OnExit events for TTrackbar. These properties
     and handler functions have been added to TWincontrol, two new
     callbacks have been added to gtkcallback.
      stoppok

  Revision 1.23  1999/09/22 20:07:15  lazarus
  *** empty log message ***

  Revision 1.22  1999/09/17 23:12:58  lazarus
  *** empty log message ***

  Revision 1.21  1999/09/13 03:32:09  lazarus
  Added version control to the file.                caw


}
