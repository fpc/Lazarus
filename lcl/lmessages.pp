{  $Id$  }
 {
 /***************************************************************************
                          lMessages.pp  -
                             -------------------

                   Initial Revision  : Wed Jun 30 CST 1999
                Shane Miller

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

unit lMessages;

{$mode objfpc}{$H+}

interface

uses Classes,vclGlobals,LCLLinux;

const

// Commands sent to the interface units
// add also a description to a message at the end of this unit
LM_ComUser       = $1000;
LM_Create        = LM_ComUser+1;
LM_SetLabel      = LM_ComUser+2;
LM_SetLeft       = LM_ComUser+3;
LM_SetTop        = LM_ComUser+4;
LM_SetWidth      = LM_ComUser+5;
LM_SetHeight     = LM_ComUser+6;
LM_AddChild      = LM_ComUser+7;
LM_Setsize       = LM_ComUser+8;
LM_GetLabel      = LM_ComUser+9;
LM_AssignEvent   = LM_ComUser+10;
LM_AssignSelf    = LM_ComUser+11;
LM_SetName       = LM_ComUser+12;
LM_RESIZECHILDREN = LM_ComUser+13;
LM_ShowHide      = LM_ComUser+14;
LM_AddPage       = LM_ComUser+15;
LM_GetLineCount  = LM_ComUser+16;
LM_SETTEXT       = LM_ComUser+17;
LM_GETTEXT       = LM_ComUser+18;
LM_CANVASCREATE  = LM_ComUser+19;
LM_ReDraw        = LM_ComUser+26;
LM_SetColor      = LM_ComUser+27;
LM_RemovePage    = LM_ComUser+28;
LM_ShowTabs      = LM_ComUser+29;
LM_SetTabPosition = LM_ComUser+30;
LM_Invalidate    = LM_ComUser+32;
LM_SetPixel      = LM_ComUser+34;
LM_GetPixel      = LM_ComUser+35;

LM_SETPROPERTIES = LM_ComUser+39;         // update object to reflect current properties 
LM_SETVALUE      = LM_ComUser+40;         // set actual value of object to visual object
LM_GETVALUE      = LM_ComUser+41;         // get actual value from visual object
LM_ATTACHMENU    = LM_ComUser+42;

LM_TB_BUTTONCOUNT = LM_ComUser+45;
LM_INSERTTOOLBUTTON = LM_ComUser+46;
LM_DELETETOOLBUTTON = LM_ComUser+47;

//LM_SetCursor = LM_ComUser+48;  We define this later for Windows compatability.

LM_IMAGECHANGED = LM_ComUser+49;
LM_LAYOUTCHANGED = LM_ComUser+50;
LM_BTNDEFAULT_CHANGED = LM_ComUser+51;

LM_LOADXPM = LM_ComUser+52;

LM_DRAGINFOCHANGED = LM_COMUSER+53;

//LM_SETENABLED = LM_COMUSER+54;
LM_BRINGTOFRONT = LM_COMUSER+55;
LM_POPUPSHOW = LM_COMUSER+56;


LM_RECREATEWND = LM_COMUSER+57;
LM_SETFORMICON = LM_COMUSER+58;

LM_MINIMIZE = LM_COMUSER+59;

LM_SETDESIGNING = LM_COMUSER+60;

//end of messages that are sent to the interface


{Windows Compatability}
 { System Menu Commands }
  SC_SIZE = 61440;
  SC_MOVE = 61456;
  SC_MINIMIZE = 61472;
  SC_MAXIMIZE = 61488;
  SC_NEXTWINDOW = 61504;
  SC_PREVWINDOW = 61520;
  SC_CLOSE = 61536;
  SC_VSCROLL = 61552;
  SC_HSCROLL = 61568;
  SC_MOUSEMENU = 61584;
  SC_KEYMENU = 61696;
  SC_ARRANGE = 61712;
  SC_RESTORE = 61728;
  SC_TASKLIST = 61744;
  SC_SCREENSAVE = 61760;
  SC_HOTKEY = 61776;
  SC_DEFAULT = 61792;
  SC_MONITORPOWER = 61808;
  SC_CONTEXTHELP = 61824;
  SC_SEPARATOR = 61455;

//-------------
// Messages
//-------------

LM_NULL = $0000;
// not yet these are defined as messages to the interface
//LM_CREATE = $0001;
//LM_DESTROY = $0002;
LM_MOVE = $0003;

LM_SIZE = $0005;
LM_ACTIVATE = $0006;
LM_SETFOCUS = $0007;
LM_KILLFOCUS = $0008;
LM_ENABLE = $000A;
LM_GETTEXTLENGTH = $000E;
LM_ERASEBKGND = $0014;

LM_SHOWWINDOW   = $0018;

LM_CANCELMODE   = $001F;
LM_SETCURSOR = $0020;
LM_DRAWITEM     = $002B;
LM_MEASUREITEM  = $002C;
LM_DELETEITEM   = $002D;
LM_VKEYTOITEM   = $002E;
LM_CHARTOITEM   = $002F;
LM_SETFONT      = $0030;

LM_COMPAREITEM = $0039;
LM_WINDOWPOSCHANGING = $0046;
LM_WINDOWPOSCHANGED = $0047;
LM_NOTIFY  = $004E;
LM_NOTIFYFORMAT = $0055;

LM_NCCALCSIZE = $0083;
LM_NCHITTEST = $0084;
LM_NCPAINT = $0085;
LM_NCACTIVATE = $0086;
LM_GETDLGCODE = $0087;
LM_NCMOUSEMOVE = $00A0;
LM_NCLBUTTONDOWN = $00A1;
LM_NCLBUTTONUP = $00A2;
LM_NCLBUTTONDBLCLK  = $00A3;

LM_KEYFIRST = $0100;
LM_KEYDOWN = $0100;
LM_KEYUP = $0101;
LM_CHAR = $0102;

LM_SYSKEYDOWN = $0104;
LM_SYSKEYUP = $0105;
LM_SYSCHAR = $0106;

LM_KEYLAST = $0108;

LM_COMMAND = $0111;

LM_HSCROLL = $0114;
LM_VSCROLL = $0115;
LM_CTLCOLORMSGBOX   = $0132;
LM_CTLCOLOREDIT     = $0133;
LM_CTLCOLORLISTBOX  = $0134;
LM_CTLCOLORBTN      = $0135;
LM_CTLCOLORDLG      = $0136;
LM_CTLCOLORSCROLLBAR= $0137;
LM_CTLCOLORSTATIC   = $0138;

LM_MOUSEFIRST = $0200;
LM_MOUSEMOVE = $0200;
LM_LBUTTONDOWN = $0201;
LM_LBUTTONUP = $0202;
LM_LBUTTONDBLCLK = $0203;
LM_RBUTTONDOWN = $0204;
LM_RBUTTONUP = $0205;
LM_RBUTTONDBLCLK = $0206;
LM_MButtonDown = $0207;
LM_MBUTTONUP = $0208;
LM_MBUTTONDBLCLK = $0209;
LM_MOUSEWHEEL = $020A;
LM_MOUSELAST = $020A;

LM_CAPTURECHANGED = $0215;
LM_DROPFILES = $0233;

LM_PARENTNOTIFY  = $0210;

//Calendar



// lcl messages

LM_USER = $400; // MWE: changed from $100 to $400 since they were in the windows range
WM_USER = LM_USER;
LM_DESTROY = LM_User+2;
LM_ACTIVATEITEM = LM_User+4;
LM_CHANGED = LM_User+5;
LM_FOCUS = LM_User+6;
LM_CLICKED = LM_User+7;
LM_PRESSED = LM_User+8;
LM_RELEASED = LM_User+9;
LM_MOVECURSOR = LM_User+10;
LM_ENTER = LM_User+11;
LM_LEAVE = LM_User+12;
//LM_SIZEALLOCATE = LM_User+13;
LM_CHECKRESIZE = LM_User+14;
//LM_SHOW = LM_User+15; // Windows Compatability
LM_INSERTTEXT = LM_User+16;
LM_DELETETEXT = LM_User+17;
LM_SETEDITABLE = LM_User+18;
LM_MOVEWORD = LM_User+19;
LM_MOVEPAGE = LM_User+20;
LM_MOVETOROW = LM_User+21;
LM_MOVETOCOLUMN = LM_User+22;
LM_KILLCHAR = LM_User+23;
LM_KILLWORD = LM_User+24;
LM_KILLLINE = LM_User+25;
LM_CUTTOCLIP = LM_User+26;
LM_COPYTOCLIP = LM_User+27;
LM_PASTEFROMCLIP = LM_User+28;
//LM_MOVERESIZE = LM_User+29;
LM_EXPOSEEVENT = LM_User+30;
LM_CONFIGUREEVENT = LM_User+31;
//LM_DRAW = LM_User+32;  //LM_DRAW and LM_PAINT are the same.
LM_PAINT = LM_User+32;
LM_SHOWMODAL = LM_USER+33;
LM_SETFILTER = LM_USER+34;
LM_SETFILENAME = LM_USER+35;
LM_OK_CLICKED = LM_USER+36;
LM_CANCEL_CLICKED = LM_USER+37;
//LM_KEYDOWN = LM_User+38; // Windows Compatability
//LM_KEYUP = LM_USER+39;  // Windows Compatability
LM_TIMER  = LM_USER+40;
//LM_MOUSEBTNPRESS  = LM_USER+41;
//LM_MOUSEBTNRELEASE  = LM_USER+42;
LM_GETITEMS = LM_USER+43;
LM_GETITEMINDEX = LM_USER+44;
LM_SETITEMINDEX = LM_USER+45;
LM_GETSELTEXT = LM_USER+46;
LM_SETSELTEXT = LM_USER+47;
LM_GETSELSTART = LM_USER+48;
LM_SETSELSTART = LM_USER+49;
LM_GETSELLEN = LM_USER+50;
LM_SETSELLEN = LM_USER+51;
LM_GETLIMITTEXT = LM_USER+52;
LM_SETLIMITTEXT = LM_USER+53;
LM_SORT = LM_USER+54;
LM_GETSELCOUNT = LM_USER+55;
LM_GETSEL = LM_USER+56;
LM_SETSEL = LM_USER+57;
LM_SETSELMODE = LM_USER+58;
LM_SETBORDER = LM_USER+59;
LM_EXIT	   = LM_USER+60;	
LM_SCREENINIT = LM_USER+61;
LM_CLOSEQUERY = LM_USER+62;
LM_DRAGSTART = LM_USER+63;
LM_DEACTIVATE = LM_USER+64;  //used when a form is no longer in front

LM_MONTHCHANGED = LM_USER+65;
LM_YEARCHANGED = LM_USER+66;
LM_DAYCHANGED = LM_USER+67;

//TListView
LM_LV_ADDITEM = LM_USER+68;
LM_LV_CHANGEITEM = LM_USER + 69;
LM_LV_DELETEITEM = LM_USER + 70;

LM_CB_FIRST = LM_USER+100;
LM_CB_GETCOUNT = LM_CB_FIRST+1;
LM_CB_GETTEXT = LM_CB_FIRST+2;
LM_CB_ADDTEXT = LM_CB_FIRST+3;

LM_CB_LAST = LM_CB_FIRST+10;  //LM_USER+110



LM_UNKNOWN = LM_User+99;

LM_SYSCOMMAND = $0112;

type

TColor = LongInt;  //Also defined in graphics.pp
UINT = LongWord;
BOOL = Boolean;


{Linux Messages}

TLMDrawItems = record
  Msg: Cardinal;
  Ctl : HWND;
  DrawItemStruct : PDrawItemStruct;
  Result : LongInt;
end;

TLMNoParams = record
    Msg: Cardinal;
    Unused: array[0..3] of Word;
    Result: Longint;
  end;

TLMScreenInit = record
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;
  ColorDepth : Integer;
end;

TLMSETCURSOR = record
  Msg : Cardinal;
  CursorWnd : HWND;
  HitText : Word;
  MOuseMsg : Word;
  Result : Longint;
end;

PLMScreenInit = ^TLMScreenInit;

TLMCanvasCreate = Record
   pparent : Pointer;
   pCanvas : Pointer;
  end;

pTLMCanvasCreate = ^TLMCanvasCreate;

PLMCanvasDrawRect = ^TLMCanvasDrawRect;
TLMCanvasDrawRect = Record
   R : TRect;
   ReDraw : Boolean;
   PenColor : TColor;
  end;

PLMCanvasDrawLine = ^TLMCanvasDrawLine;
TLMCanvasDrawLine = Record
   x1 : Integer;
   y1 : Integer;
   x2 : Integer;
   y2 : Integer;
   PenColor : TColor;
   ReDraw : Boolean;
  end;

PLMCanvasDrawText = ^TLMCanvasDrawText;
TLMCanvasDrawText = Record
   x1 : Integer;
   y1 : Integer;
   Str : String;
   Font : TObject;
   PenColor : TColor;
   ReDraw : Boolean;
  end;

TLMEraseBkgnd = record
    Msg: Cardinal;
    DC: HDC;
    Unused: Longint;
    Result: Longint;
  end;

TLMGetText = record
    Msg: Cardinal;
    TextMax: Integer;
    Text: PChar;
    Result: Longint;
  end;

TLMGetTextLength = TLMNoParams;


TLMInsertText = record
  Msg : Cardinal;
  NewText : String;
  Length : Integer;
  Position : Integer;
  UserData : Pointer;
end;




TLMKey = record
  Msg: Cardinal;
  CharCode: Word;
  Unused: Word;
  KeyData: Longint;
  Result: Longint;
end;

TLMChar = TLMKey;
TLMKeyDown = TLMKey;
TLMKeyUp = TLMKey;
TLMSysChar = TLMKey;
TLMSysKeyDown = TLMKey;
TLMSysKeyUp = TLMKey;



TLMMouse = packed record
   Msg : Cardinal;
   Keys : LongInt;
   case Integer of
   0: ( 
      XPos: SmallInt;
      YPos: SmallInt);
   1: (
      Pos : TSmallPoint;
      Result : LongInt);
   end;


TLMMouseMove = TLMMOuse;

TLMMove = record
  Msg: Cardinal;
  Unused: Integer;
  case Integer of
    0: (
      XPos: Smallint;
      YPos: Smallint);
    1: (
      Pos: TSmallPoint;
      Result: Longint);
end;

TLMActivate = record
    Msg: Cardinal;
    Active: BOOL;
    Minimized : WordBool;
    ActiveWindow : HWND;
    Result: Longint;
  end;

TLMNCActivate = record
    Msg: Cardinal;
    Active: BOOL;
    Unused: Longint;
    Result: Longint;
  end;

 TLMNotify = packed record
    Msg: Cardinal;
    IDCtrl: Longint;
    NMHdr: PNMHdr;
    Result: Longint;
  end;

  TLMNotifyFormat = packed record
    Msg: Cardinal;
    From: HWND;
    Command: Longint;
    Result: Longint;
  end;

  TLMPaint = packed record
    Msg: Cardinal;
    DC: HDC;
    Unused: Longint;
    Result: Longint;
  end;

TLMResize = record
  Msg : Cardinal;
  Left  : Integer;
  Top : Integer;
  Width : Integer;
  Height : Integer;
  UserData : Pointer;
end;

TLMMoveResize = TLMResize;

  PWindowPos = ^TWindowPos;
  tagWINDOWPOS = packed record
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
    Unused: Integer;
    WindowPos: PWindowPos;
    Result: Longint;
  end;

  TLMWindowPosChanged = TLMWindowPosMsg;
  TLMWindowPosChanging = TLMWindowPosMsg;
           
  {PNCCalcSizeParams}
  PNCCalcSizeParams = ^TNCCalcSizeParams;
  tagNCCalcSize_Params = packed record
    rgrc : Array[0..2] of TRect;
    lpPos : pWindowPos;
  end;
  TNCCalcSizeParams = tagNCCalcSize_Params;
  ncCalcSizeParams = tagNCCalcSize_Params;
  

  TLMNCCalcSize = record
    Msg: Cardinal;
    CalcValidRects: BOOL;
    CalcSize_Params: PNCCalcSizeParams;
    Result: Longint;
  end;

TLMSysColorChange = TLMNoParams;

TLMSysCommand = record
    Msg: Cardinal;
    case CmdType: Longint of
      SC_HOTKEY: (
        ActivateWnd: HWND);
      SC_KEYMENU: (
        Key: Word);
      SC_CLOSE, SC_HSCROLL, SC_MAXIMIZE, SC_MINIMIZE, SC_MOUSEMENU, SC_MOVE,
      SC_NEXTWINDOW, SC_PREVWINDOW, SC_RESTORE, SC_SCREENSAVE, SC_SIZE,
      SC_TASKLIST, SC_VSCROLL: (
        XPos: Smallint;
        YPos: Smallint;
        Result: Longint);
  end;

TLMSysDeadChar = record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: Longint;
    Result: Longint;
  end;


TLMSystemError = record
    Msg: Cardinal;
    ErrSpec: Word;
    Unused: Longint;
    Result: Longint;
  end;

TLMTimeChange = TLMNoParams;

TLMSort = record
  Msg : Cardinal;
  List : TObject;
  IsSorted : boolean;
end;

//Used to set the statusbar's text
TLMSetControlText = record 
  fCompStyle : Longint;
  Panel : Integer;
  Userdata : PChar;
end;

TLMSetText = packed record 
  Msg : cardinal;
  Unused : Longint;
  Text : PChar;
  Result : Longint;
end;

  
TLMKeyEvent = Record
  Msg : Cardinal;
  KeyChar : Char;
  Key : Word;
  State : TShiftState;
  Length : Integer;
  Str : PChar;
  UserData : Pointer;
 end;

TLMMouseEvent = Record
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
TLMLButtonUp = TLMMouse;
TLMRButtonUp = TLMMouse;
TLMMButtonUp = TLMMouse;

TLMNotebookEvent = record
  Parent: TObject;
  Child: TObject;
  fCompStyle: Integer;
  Str: String;
  Page: Integer;
  ShowTabs: Boolean;
  TabPosition: Pointer;
end;

TLMSetSel = record
  Index : integer;
  Selected : boolean;
end;

TLMSetSelMode = record
  MultiSelect : boolean;
  ExtendedSelect : boolean;
end;

TLMSetFocus = packed record
  Msg: Cardinal;
  FocusedWnd: HWND;
  Unused: LongInt;
  Result : LongInt;
End;

TLMSetGetPixel = record
  X,Y : Integer;
  PixColor : TColor;
end;

TLMSize = packed record
  Msg: Cardinal;
  SizeType: LongInt;
  Width : Word;
  Height : Word;
  Result : LongInt;
End;

TLMNoPara = packed record
  Msg : Cardinal;
end;

  PLMessage = ^TLMessage;
  TLMessage = packed record
    Msg : Cardinal;
    case Integer of
      0 : (
	      WParam: LongInt;
      	LParam : LongInt;
      	Result : LongInt);
      1 : (
      	WParamLo: Word;
      	WParamHi: Word;
      	LParamLo: Word;
      	LParamHi: Word;
      	ResultLo: Word;
      	ResultHi: Word);
    end;


TLMScroll = record
  Msg : Cardinal;
  ScrollCode : SmallInt;
  Pos : SmallInt;
  ScrollBar : HWND;
  Result : LongInt;
end;

TLMHScroll = TLMScroll;
TLMVScroll = TLMScroll;

TLMShowWindow = record
  Msg: Cardinal;
  Show: LongBool;
  Status: Longint;
  Result: Longint;
end;

TLMKILLFOCUS = packed Record
   Msg : Cardinal;
   FocusedWnd: HWND;
   UnUsed : LongInt;
   Result : LongInt;
  End;

TLMNCHITTEST = packed record
  Msg : cardinal;
  Unused : LongInt;
  case Integer of
    0 : (
         XPos : SmallInt;
         YPos : SmallInt);
    1 : (
         Pos : TSmallPoint;
         Result : LongInt);
end;


TLMDestroy = TLMNoParams;



function GetMessageName(const AMessage: Integer):  String;


Implementation

uses
  SysUtils;

function GetMessageName(const AMessage: Integer):  String;
begin
  case AMessage of
    LM_Create           : Result :='LM_Create          ';
    LM_Destroy          : Result :='LM_Destroy         ';
    LM_SetLabel         : Result :='LM_SetLabel        ';
    LM_SetLeft          : Result :='LM_SetLeft         ';
    LM_SetTop           : Result :='LM_SetTop          ';
    LM_SetWidth         : Result :='LM_SetWidth        ';
    LM_SetHeight        : Result :='LM_SetHeight       ';
    LM_AddChild         : Result :='LM_AddChild        ';
    LM_Setsize          : Result :='LM_Setsize         ';
    LM_GetLabel         : Result :='LM_GetLabel        ';
    LM_AssignEvent      : Result :='LM_AssignEvent     ';
    LM_AssignSelf       : Result :='LM_AssignSelf      ';
    LM_SetName          : Result :='LM_SetName         ';
    LM_RESIZECHILDREN   : Result :='LM_RESIZECHILDREN  ';
    LM_ShowHide         : Result :='LM_ShowHide        ';
    LM_AddPAge          : Result :='LM_AddPAge         ';
    LM_GetLineCount     : Result :='LM_GetLineCount    ';
    LM_SETTEXT          : Result :='LM_SETTEXT         ';
    LM_GETTEXT          : Result :='LM_GETTEXT         ';
    LM_CANVASCREATE     : Result :='LM_CANVASCREATE    ';
    LM_ReDraw           : Result :='LM_ReDraw          ';
    LM_SetColor         : Result :='LM_SetColor        ';
    LM_RemovePage       : Result :='LM_RemovePage      ';
    LM_ShowTabs         : Result :='LM_ShowTabs        ';
    LM_SetTabPosition   : Result :='LM_SetTabPosition  ';
    LM_Invalidate       : Result :='LM_Invalidate      ';
    LM_SetPixel         : Result :='LM_SetPixel        ';
    LM_GetPixel         : Result :='LM_GetPixel        ';
    LM_SETPROPERTIES    : Result :='LM_SETPROPERTIES   ';      
    LM_SETVALUE         : Result :='LM_SETVALUE        ';     
    LM_GETVALUE         : Result :='LM_GETVALUE        ';     
    LM_ATTACHMENU       : Result :='LM_ATTACHMENU      ';
    LM_TB_BUTTONCOUNT   : Result :='LM_TB_BUTTONCOUNT  ';
    LM_INSERTTOOLBUTTON : Result :='LM_INSERTTOOLBUTTON';
    LM_DELETETOOLBUTTON : Result :='LM_DELETETOOLBUTTON';
    LM_SetCursor        : Result :='LM_SetCursor       ';
    LM_IMAGECHANGED     : Result :='LM_IMAGECHANGED     ';
    LM_LAYOUTCHANGED    : Result :='LM_LAYOUTCHANGED    ';
    LM_BTNDEFAULT_CHANGED: Result :='LM_BTNDEFAULT_CHANGED';
    LM_LOADXPM          : Result :='LM_LOADXPM          ';
    LM_DRAGINFOCHANGED  : Result :='LM_DRAGINFOCHANGED  ';
//    LM_SETENABLED       : Result :='LM_SETENABLED       ';
    LM_BRINGTOFRONT     : Result :='LM_BRINGTOFRONT     ';   
    LM_CB_GETCOUNT         : Result :='LM_CB_GETCOUNT         ';
  else
    Result := Format('Unknown message 0x%x (%d)', [AMessage, AMessage]);
  end;
  Result  := Trim(Result);  
  
end;


end.

{
  $Log$
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
