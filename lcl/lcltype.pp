{  $Id$  }
{
 /***************************************************************************
                                LCLType.pp
                                ----------
                             Component Library Windows Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


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

{
@author(Curtis White <cwhite@aracnet.com>)
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

This unit is being created specifically for compatibility with Delphi. It
should only be used for constants and type definitions that are included in
the Delphi Windows unit. This is only done for compatibiltiy.

}

unit LCLType;

{$mode objfpc}{$H+}

interface


{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
{$IFDEF USE_UTF8BIDI_LCL}
  UTF8BIDI,
{$ENDIF USE_UTF8BIDI_LCL}
{$ifdef win32}
  windows,
{$endif win32}
  Classes, SysUtils;

type
{$IFDEF USE_UTF8BIDI_LCL}
  TUTF8Char = UTF8BIDI.TUTF8Char;
{$ELSE USE_UTF8BIDI_LCL}
  TUTF8Char = String[7];
{$ENDIF USE_UTF8BIDI_LCL}
  PRect = ^TRect;
  UINT = LongWord;
  PPoint = ^TPoint;

  {PLongInt = ^LongInt;
  PInteger = ^Integer;
  PSmallInt = ^SmallInt;
  PDouble = ^Double;
  PAnsiChar = ^AnsiChar;
  PWideChar = ^WideChar;}

  TCriticalSection = longint;
  PCriticalSection = ^TCriticalSection;
{$IFNDEF VER1_0}
  TRTLCriticalSection = pointer;
{$ENDIF}


{$ifndef win32}
  { Provided for compatibility with Windows registry ONLY }
  HKEY  = Integer;
  HDC   = type LongWord;
  HHOOK = type LongWord;
  HFONT = type LongWord;
  HGDIOBJ = type LongWord;
  HPEN  = type LongWord;
  HRGN  = type LongWord;
  HINST = type LongWord;
  HICON = type LongWord;
  HCURSOR = HICON;
  Bool    = LongBool;
  HGLOBAL = THandle;
  Short   = SmallInt;
  HWND    = THandle;
  HMENU   = type LongWord;
  HBITMAP = type LongWord;
  HPALETTE = type LongWord;

  HBRUSH = type LongWord;

  WPARAM = type LongInt;
  LPARAM = type LongInt;
  LRESULT = type LongInt;

{$else}
  HKEY  = Windows.HKEY;
  HDC   = Windows.HDC;
  HHOOK = Windows.HHOOK;
  HFONT = Windows.HFont;
  HGDIOBJ = Windows.HGDIOBJ;
  HPEN    = Windows.HPEN;
  HRGN    = Windows.HRGN;
  HINST   = Windows.HINST;
  HICON   = Windows.HICON;
  HCURSOR = HICON;
  BOOL    = Windows.BOOL;
  HGLOBAL = Windows.HGLOBAL;
  Short   = Windows.Short;
  HWND    = Windows.HWND;
  HMENU   = Windows.HMENU;
  HBITMAP = Windows.HBITMAP;
  HPALETTE = Windows.HPALETTE;

  HBRUSH = Windows.HBRUSH;

  WPARAM = Windows.WPARAM;
  LPARAM = Windows.LPARAM;
  LRESULT = Windows.LRESULT;
{$endif}

  PHKEY = ^HKEY;

// from Delphis sysutils.pas
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;

var
  hInstance: HINST;

const
  INVALID_HANDLE_VALUE  = 0;
  MaxByte               = 255;

  { Ternary raster operations }
  SRCCOPY     = $00CC0020;     { dest = source                    }
  SRCPAINT    = $00EE0086;     { dest = source OR dest            }
  SRCAND      = $008800C6;     { dest = source AND dest           }
  SRCINVERT   = $00660046;     { dest = source XOR dest           }
  SRCERASE    = $00440328;     { dest = source AND (NOT dest )    }
  NOTSRCCOPY  = $00330008;     { dest = (NOT source)              }
  NOTSRCERASE = $001100A6;     { dest = (NOT src) AND (NOT dest)  }
  MERGECOPY   = $00C000CA;     { dest = (source AND pattern)      }
  MERGEPAINT  = $00BB0226;     { dest = (NOT source) OR dest      }
  PATCOPY     = $00F00021;     { dest = pattern                   }
  PATPAINT    = $00FB0A09;     { dest = DPSnoo                    }
  PATINVERT   = $005A0049;     { dest = pattern XOR dest          }
  DSTINVERT   = $00550009;     { dest = (NOT dest)                }
  BLACKNESS   = $00000042;     { dest = BLACK                     }
  WHITENESS   = $00FF0062;     { dest = WHITE                     }

type
  TKeyBoardState = array[0..255] of byte;

  PABC = ^TABC;

  _ABC = packed record
    abcA: Integer;
    abcB: UINT;
    abcC: Integer;
  end;
  TABC = _ABC;


const
  ETO_OPAQUE = 2;
  ETO_CLIPPED = 4;

  CS_VREDRAW = dword(1);
  CS_HREDRAW = dword(2);

//------------
// CombineRgn Mode flags
//------------
  RGN_AND = 1;
  RGN_OR = 2;
  RGN_XOR = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;

//------------
// DrawText flags
//------------
  DT_TOP = 0;
  DT_LEFT = 0;
  DT_CENTER = 1;
  DT_RIGHT = 2;
  DT_VCENTER = 4;
  DT_BOTTOM = 8;
  DT_WORDBREAK = $10;
  DT_SINGLELINE = $20;
  DT_NOCLIP = $100;
  DT_CALCRECT = $400;
  DT_NOPREFIX = $800;
  DT_INTERNAL = $1000;
  DT_EDITCONTROL = $2000;

//==============================================
// Draw frame constants
//==============================================

//------------
// Draw frame control flags
//------------
  dfc_Caption = $01;
  dfc_Menu = $02;
  dfc_Scroll = $03;
  dfc_Button =  $04;

//------------
// Draw frame control Styles
//------------
  DFCS_BUTTONCHECK = 0;
  DFCS_BUTTONRADIOIMAGE = 1;
  DFCS_BUTTONRADIOMASK = 2;
  DFCS_BUTTONRADIO = 4;
  DFCS_BUTTON3STATE = 8;
  DFCS_BUTTONPUSH = 16;

  DFCS_CAPTIONCLOSE = 0;
  DFCS_CAPTIONMIN = 1;
  DFCS_CAPTIONMAX = 2;
  DFCS_CAPTIONRESTORE = 3;
  DFCS_CAPTIONHELP = 4;

  DFCS_MENUARROW = 0;
  DFCS_MENUCHECK = 1;
  DFCS_MENUBULLET = 2;

  DFCS_SCROLLDOWN = 1;
  DFCS_SCROLLLEFT = 2;
  DFCS_SCROLLRIGHT = 3;
  DFCS_SCROLLCOMBOBOX = 5;
  DFCS_SCROLLSIZEGRIP = 8;
  DFCS_SCROLLUP = 0;

  DFCS_ADJUSTRECT = 8192;
  DFCS_CHECKED = 1024;
  DFCS_FLAT = 16384;
  DFCS_INACTIVE = 256;
  DFCS_MONO = 32768;
  DFCS_PUSHED = 512;

//==============================================
// Registry constants
//==============================================
  { Provided for compatibility with Windows registry ONLY }
  HKEY_CLASSES_ROOT     = $80000000;
  HKEY_CURRENT_USER     = $80000001;
  HKEY_LOCAL_MACHINE    = $80000002;
  HKEY_USERS            = $80000003;
  HKEY_PERFORMANCE_DATA = $80000004;
  HKEY_CURRENT_CONFIG   = $80000005;
  HKEY_DYN_DATA         = $80000006;

  HTERROR = -2;
  HTTRANSPARENT = -1;
  HTNOWHERE = 0;
  HTCLIENT = 1;
  HTCAPTION = 2;

  MSGF_DIALOGBOX = 0;
  MSGF_MESSAGEBOX = 1;
  MSGF_MENU = 2;
  MSGF_MOVE = 3;
  MSGF_SIZE = 4;
  MSGF_SCROLLBAR = 5;
  MSGF_NEXTWINDOW = 6;


// PEEKMESSAGE stuff
  PM_Noremove = 0;
  PM_Remove = 1;

//==============================================
// Menu constants
//==============================================

  MF_BYCOMMAND       = 0;
  MF_BYPOSITION      = $400;
  MF_BYHANDLE        = $800; // introduced by the LCL

  MF_SEPARATOR       = $800;

  MF_ENABLED         = 0;
  MF_GRAYED          = 1;
  MF_DISABLED        = 2;

  MF_UNCHECKED       = 0;
  MF_CHECKED         = 8;
  MF_USECHECKBITMAPS = $200;

  MF_STRING          = 0;
  MF_BITMAP          = 4;
  MF_OWNERDRAW       = $100;

  MF_POPUP           = $10;
  MF_MENUBARBREAK    = $20;
  MF_MENUBREAK       = $40;

  MF_UNHILITE        = 0;
  MF_HILITE          = $80;

  MF_DEFAULT         = $1000;
  MF_SYSMENU         = $2000;
  MF_HELP            = $4000;
  MF_RIGHTJUSTIFY    = $4000;

  MF_MOUSESELECT     = $8000;

  MF_UNSUPPORTED     = $FFFFFFFF;

//==============================================
// Keyboard constants
//==============================================

//------------
// KeyFlags (High word part !!!)
//------------
  KF_EXTENDED = $100;
  KF_DLGMODE = $800;
  KF_MENUMODE = $1000;
  KF_ALTDOWN = $2000;
  KF_REPEAT = $4000;
  KF_UP = $8000;

//-------------
// Virtual keys
//-------------
//
// Source: http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winui/WinUI/WindowsUserInterface/UserInput/VirtualKeyCodes.asp
//
  VK_UNKNOWN    = 0; // defined by LCL
  VK_LBUTTON    = 1;
  VK_RBUTTON    = 2;
  VK_CANCEL     = 3;
  VK_MBUTTON    = 4;
  VK_BACK       = 8;
  VK_TAB        = 9;
  VK_CLEAR      = 12;
  VK_RETURN     = 13;
  VK_SHIFT      = 16;
  VK_CONTROL    = 17;
  VK_MENU       = 18;
  VK_PAUSE      = 19;
  VK_CAPITAL    = 20;
  VK_KANA       = 21;
  VK_HANGUL     = 21;
  VK_JUNJA      = 23;
  VK_FINAL      = 24;
  VK_HANJA      = 25;
  VK_KANJI      = 25;
  VK_ESCAPE     = 27;
  VK_CONVERT    = 28;
  VK_NONCONVERT = 29;
  VK_ACCEPT     = 30;
  VK_MODECHANGE = 31;
  VK_SPACE      = 32;
  VK_PRIOR      = 33;
  VK_NEXT       = 34;
  VK_END        = 35;
  VK_HOME       = 36;
  VK_LEFT       = 37;
  VK_UP         = 38;
  VK_RIGHT      = 39;
  VK_DOWN       = 40;
  VK_SELECT     = 41;
  VK_PRINT      = 42;
  VK_EXECUTE    = 43;
  VK_SNAPSHOT   = 44;
  VK_INSERT     = 45;
  VK_DELETE     = 46;
  VK_HELP       = 47;
  VK_0          = $30;
  VK_1          = $31;
  VK_2          = $32;
  VK_3          = $33;
  VK_4          = $34;
  VK_5          = $35;
  VK_6          = $36;
  VK_7          = $37;
  VK_8          = $38;
  VK_9          = $39;
  //3A-40 Undefined
  VK_A	        = $41;
  VK_B	        = $42;
  VK_C	        = $43;
  VK_D	        = $44;
  VK_E	        = $45;
  VK_F	        = $46;
  VK_G	        = $47;
  VK_H	        = $48;
  VK_I	        = $49;
  VK_J	        = $4A;
  VK_K	        = $4B;
  VK_L	        = $4C;
  VK_M	        = $4D;
  VK_N	        = $4E;
  VK_O	        = $4F;
  VK_P	        = $50;
  VK_Q	        = $51;
  VK_R	        = $52;
  VK_S	        = $53;
  VK_T	        = $54;
  VK_U	        = $55;
  VK_V	        = $56;
  VK_W	        = $57;
  VK_X	        = $58;
  VK_Y	        = $59;
  VK_Z	        = $5A;

  VK_LWIN       = $5B;
  VK_RWIN       = $5C;
  VK_APPS       = $5D;
  // $5E reserved
  VK_SLEEP      = $5F;

  VK_NUMPAD0    = 96; // $60
  VK_NUMPAD1    = 97;
  VK_NUMPAD2    = 98;
  VK_NUMPAD3    = 99;
  VK_NUMPAD4    = 100;
  VK_NUMPAD5    = 101;
  VK_NUMPAD6    = 102;
  VK_NUMPAD7    = 103;
  VK_NUMPAD8    = 104;
  VK_NUMPAD9    = 105;
  VK_MULTIPLY   = 106;
  VK_ADD        = 107;
  VK_SEPARATOR  = 108;
  VK_SUBTRACT   = 109;
  VK_DECIMAL    = 110;
  VK_DIVIDE     = 111;
  VK_F1         = 112;
  VK_F2         = 113;
  VK_F3         = 114;
  VK_F4         = 115;
  VK_F5         = 116;
  VK_F6         = 117;
  VK_F7         = 118;
  VK_F8         = 119;
  VK_F9         = 120;
  VK_F10        = 121;
  VK_F11        = 122;
  VK_F12        = 123;
  VK_F13        = 124;
  VK_F14        = 125;
  VK_F15        = 126;
  VK_F16        = 127;
  VK_F17        = 128;
  VK_F18        = 129;
  VK_F19        = 130;
  VK_F20        = 131;
  VK_F21        = 132;
  VK_F22        = 133;
  VK_F23        = 134;
  VK_F24        = 135; // $87

  // $88-$8F unassigned

  VK_NUMLOCK    = $90;
  VK_SCROLL     = $91;

  // $92-$96  OEM specific
  // $97-$9F  Unassigned

  // not in VCL defined:
  //MWE: And should not be used.
  //     The keys they are on map to another VK

(*
  VK_EQUAL      = 187;
  VK_COMMA      = 188;
  VK_POINT      = 190;
  VK_SLASH      = 191;
  VK_AT         = 192;
*)

  // VK_L & VK_R - left and right Alt, Ctrl and Shift virtual keys.
  // Used only as parameters to GetAsyncKeyState() and GetKeyState().
  // No other API or message will distinguish left and right keys in this way
  VK_LSHIFT     = $A0;
  VK_RSHIFT     = $A1;
  VK_LCONTROL   = $A2;
  VK_RCONTROL   = $A3;
  VK_LMENU      = $A4;
  VK_RMENU      = $A5;

  VK_BROWSER_BACK        = $A6;
  VK_BROWSER_FORWARD     = $A7;
  VK_BROWSER_REFRESH     = $A8;
  VK_BROWSER_STOP        = $A9;
  VK_BROWSER_SEARCH      = $AA;
  VK_BROWSER_FAVORITES   = $AB;
  VK_BROWSER_HOME        = $AC;
  VK_VOLUME_MUTE         = $AD;
  VK_VOLUME_DOWN         = $AE;
  VK_VOLUME_UP           = $AF;
  VK_MEDIA_NEXT_TRACK    = $B0;
  VK_MEDIA_PREV_TRACK    = $B1;
  VK_MEDIA_STOP          = $B2;
  VK_MEDIA_PLAY_PAUSE    = $B3;
  VK_LAUNCH_MAIL         = $B4;
  VK_LAUNCH_MEDIA_SELECT = $B5;
  VK_LAUNCH_APP1         = $B6;
  VK_LAUNCH_APP2         = $B7;
  // $B8-$B9 Reserved
  VK_OEM_1               = $BA; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the ';:' key
  VK_OEM_PLUS            = $BB; // For any country/region, the '+' key
  VK_OEM_COMMA           = $BC; // For any country/region, the ',' key
  VK_OEM_MINUS           = $BD; // For any country/region, the '-' key
  VK_OEM_PERIOD          = $BE; // For any country/region, the '.' key
  VK_OEM_2               = $BF; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '/?' key
  VK_OEM_3               = $C0; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '`~' key
  // $C1-$D7 Reserved
  // $D8-$DA Unassigned
  VK_OEM_4               = $DB; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '[{' key
  VK_OEM_5               = $DC; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the '\|' key
  VK_OEM_6               = $DD; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the ']}' key
  VK_OEM_7               = $DE; // Used for miscellaneous characters; it can vary by keyboard.
                                // For the US standard keyboard, the 'single-quote/double-quote' key
  VK_OEM_8               = $DF; // Used for miscellaneous characters; it can vary by keyboard.

  // $E0 Reserved
  // $E1 OEM specific
  VK_OEM_102             = $E2; // Either the angle bracket key or the backslash key on the RT 102-key keyboard

  // $E3-$E4 OEM specific

  VK_PROCESSKEY          = $E7; // IME Process key

  // $E8 Unassigned
  // $E9-$F5 OEM specific

  VK_ATTN       = $F6;
  VK_CRSEL      = $F7;
  VK_EXSEL      = $F8;
  VK_EREOF      = $F9;
  VK_PLAY       = $FA;
  VK_ZOOM       = $FB;
  VK_NONAME     = $FC;
  VK_PA1        = $FD;
  VK_OEM_CLEAR  = $FE;
  
  VK_HIGHESTVALUE = $FE;

//==============================================
//
//==============================================

const

  { 3D border styles }
  BDR_RAISEDOUTER = 1;
  BDR_SUNKENOUTER = 2;
  BDR_RAISEDINNER = 4;
  BDR_SUNKENINNER = 8;

  BDR_OUTER = 3;
  BDR_INNER = 12;
  BDR_RAISED = 5;
  BDR_SUNKEN = 10;

  EDGE_BUMP = (BDR_RAISEDOUTER or BDR_SUNKENINNER);
  EDGE_ETCHED = (BDR_SUNKENOUTER or BDR_RAISEDINNER);
  EDGE_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  EDGE_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);

  { Border flags }
  BF_LEFT = 1;
  BF_TOP = 2;
  BF_RIGHT = 4;
  BF_BOTTOM = 8;

  BF_TOPLEFT = (BF_TOP or BF_LEFT);
  BF_TOPRIGHT = (BF_TOP or BF_RIGHT);
  BF_BOTTOMLEFT = (BF_BOTTOM or BF_LEFT);
  BF_BOTTOMRIGHT = (BF_BOTTOM or BF_RIGHT);
  BF_RECT = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);
  BF_DIAGONAL = $10;

  BF_DIAGONAL_ENDTOPRIGHT = (BF_DIAGONAL or BF_TOP or BF_RIGHT);
  BF_DIAGONAL_ENDTOPLEFT = (BF_DIAGONAL or BF_TOP or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMLEFT = (BF_DIAGONAL or BF_BOTTOM or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMRIGHT = (BF_DIAGONAL or BF_BOTTOM or BF_RIGHT);

  BF_MIDDLE = $800;   { Fill in the middle }
  BF_SOFT = $1000;    { For softer buttons.  Not sure what the use is for this }
  BF_ADJUST = $2000;  { Calculate the space left over }
  BF_FLAT = $4000;    { For flat rather than 3D borders }
  BF_MONO = $8000;    { For monochrome borders }

{Dialog codes}
  DLGC_WANTARROWS = 1;
  DLGC_WANTTAB = 2;
  DLGC_WANTALLKEYS = 4;

  DLGC_WANTCHARS = $80;

{ScrollWindowEx}
  SW_SMOOTHSCROLL = 16;
  SW_ERASE = 4;
  SW_INVALIDATE = 2;
  SW_SCROLLCHILDREN = 1;

{owner drawn constants}
  ODT_MENU = 1;
  ODT_LISTBOX = 2;
  ODT_COMBOBOX = 3;
  ODT_BUTTON = 4;
  ODT_STATIC = 5;

  GWL_WNDPROC = -4;
  GWL_HINSTANCE = -6;
  GWL_HWNDPARENT = -8;
  GWL_STYLE = -16;
  GWL_EXSTYLE = -20;
  GWL_USERDATA = -21;
  GWL_ID = -12;

  MB_OK = $00000000;
  MB_OKCANCEL = $00000001;
  MB_ABORTRETRYIGNORE = $00000002;
  MB_YESNOCANCEL = $00000003;
  MB_YESNO = $00000004;
  MB_RETRYCANCEL = $00000005;
  MB_ICONHAND = $00000010;
  MB_ICONQUESTION = $00000020;
  MB_ICONEXCLAMATION = $00000030;
  MB_ICONASTERICK = $00000040;
  MB_ICONWARNING = MB_ICONEXCLAMATION;
  MB_ICONERROR = MB_ICONHAND;
  MB_ICONINFORMATION = MB_ICONASTERICK;

  IDOK = 1; 	ID_OK = IDOK;
  IDCANCEL = 2;	ID_CANCEL = IDCANCEL;
  IDABORT = 3;	ID_ABORT = IDABORT;
  IDRETRY = 4;	ID_RETRY = IDRETRY;
  IDIGNORE = 5;	ID_IGNORE = IDIGNORE;
  IDYES = 6;	ID_YES = IDYES;
  IDNO = 7;	ID_NO = IDNO;
  IDCLOSE = 8;	ID_CLOSE = IDCLOSE;
  IDHELP = 9;	ID_HELP = IDHELP;

  MB_DEFBUTTON1 = $00000000;
  MB_DEFBUTTON2 = $00000100;
  MB_DEFBUTTON3 = $00000200;
  MB_DEFBUTTON4 = $00000300;

{Region Flags}
  Error = 0;
  NullRegion = 1;
  SimpleRegion = 2;
  ComplexRegion = 3;
  Region_Error = Error;


{Scroll bar stuff}
  SB_Horz = 0;
  SB_Vert = 1;
  SB_CTL = 2;
  SB_BOTH = 3;

{Scroll Bar Commands}
  SB_LINEUP = 0;
  SB_LINELEFT = 0;
  SB_LINEDOWN = 1;
  SB_LINERIGHT = 1;
  SB_PAGEUP = 2;
  SB_PAGELEFT = 2;
  SB_PAGEDOWN = 3;
  SB_PAGERIGHT = 3;
  SB_THUMBPOSITION = 4;
  SB_THUMBTRACK = 5;
  SB_TOP = 6;
  SB_LEFT = 6;
  SB_BOTTOM = 7;
  SB_RIGHT = 7;
  SB_ENDSCROLL = 8;

  SBS_HORZ = 0;
  SBS_VERT = 1;
  SBS_TOPALIGN = 2;
  SBS_LEFTALIGN = 2;
  SBS_BOTTOMALIGN = 4;
  SBS_RIGHTALIGN = 4;
  SBS_SIZEBOXTOPLEFTALIGN = 2;
  SBS_SIZEBOXBOTTOMRIGHTALIGN = 4;
  SBS_SIZEBOX = 8;
  SBS_SIZEGRIP = $10;

  SIF_Range = 1;
  SIF_PAGE = 2;
  SIF_POS = 4;
  SIF_DISABLENOSCROLL = 8;
  SIF_TRACKPOS = $10;
  SIF_ALL = (SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS);


  SWP_NOSIZE = 1;
  SWP_NOMOVE = 2;
  SWP_NOZORDER = 4;
  SWP_NOREDRAW = 8;
  SWP_NOACTIVATE = $10;

{ WMSIZE Message Constants}
  Size_Restored = 0; // the default
  Size_Minimized = 1;
  Size_Maximized = 2;
  Size_MaxShow = 3;
  Size_MaxHide = 4;
  Size_SourceIsInterface = 128; // this is a flag. Can be combined with the above

  SIZENORMAL = Size_Restored;
  SIZEICONIC = Size_Minimized;
  SIZEFULLSCREEN = Size_Maximized;
  SIZEZOOMSHOW = Size_MaxShow;
  SIZEZOOMHIDE = Size_MaxHide;

{ WMMove Message Constants }
  Move_Default = 0;
  Move_SourceIsInterface = 128; // this is flag. Can be combined with the above

{ Window Styles }
  WS_OVERLAPPED = 0;
  WS_POPUP = DWORD($80000000);
  WS_CHILD = DWORD($40000000);
  WS_MINIMIZE = DWORD($20000000);
  WS_VISIBLE = DWORD($10000000);
  WS_DISABLED = DWORD($8000000);
  WS_CLIPSIBLINGS = DWORD($4000000);
  WS_CLIPCHILDREN = DWORD($2000000);
  WS_MAXIMIZE = DWORD($1000000);
  WS_BORDER = DWORD($800000);
  WS_DLGFRAME = DWORD($400000);
  WS_CAPTION = DWORD(WS_BORDER or WS_DLGFRAME);
  WS_VSCROLL = DWORD($200000);
  WS_HSCROLL = DWORD($100000);
  WS_SYSMENU = DWORD($80000);
  WS_THICKFRAME = DWORD($40000);
  WS_GROUP = DWORD($20000);
  WS_TABSTOP = DWORD($10000);

  WS_MINIMIZEBOX = DWORD($20000);
  WS_MAXIMIZEBOX = DWORD($10000);

  WS_TILED = WS_OVERLAPPED;

  WS_ICONIC = WS_MINIMIZE;

  WS_SIZEBOX = WS_THICKFRAME;

  { Common Window Styles }
  WS_OVERLAPPEDWINDOW = (WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU
                         or WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW;
  WS_POPUPWINDOW = (WS_POPUP or WS_BORDER or WS_SYSMENU);
  WS_CHILDWINDOW = (WS_CHILD);

  { Extended Window Styles }
  WS_EX_DLGMODALFRAME = 1;
  WS_EX_NOPARENTNOTIFY = 4;
  WS_EX_TOPMOST = 8;
  WS_EX_ACCEPTFILES = $10;
  WS_EX_TRANSPARENT = $20;
  WS_EX_MDICHILD = $40;
  WS_EX_TOOLWINDOW = $80;
  WS_EX_WINDOWEDGE = $100;
  WS_EX_CLIENTEDGE = $200;
  WS_EX_CONTEXTHELP = $400;

  WS_EX_RIGHT = $1000;
  WS_EX_LEFT = 0;
  WS_EX_RTLREADING = $2000;
  WS_EX_LTRREADING = 0;
  WS_EX_LEFTSCROLLBAR = $4000;
  WS_EX_RIGHTSCROLLBAR = 0;

  WS_EX_CONTROLPARENT = $10000;
  WS_EX_STATICEDGE = $20000;
  WS_EX_APPWINDOW = $40000;
  WS_EX_OVERLAPPEDWINDOW = (WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE);
  WS_EX_PALETTEWINDOW = (WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST);

const
  { SetWindowPos Flags }
  HWND_TOP = 0;
  HWND_BOTTOM = 1;
  HWND_TOPMOST = HWND(-1);
  HWND_NOTOPMOST = HWND(-2);

  { ShowWindow() Commands }
  SW_HIDE           = 0;
  SW_SHOWNORMAL     = 1;
  SW_NORMAL         = 1;
  SW_SHOWMINIMIZED  = 2;
  SW_SHOWMAXIMIZED  = 3;
  SW_MAXIMIZE       = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW           = 5;
  SW_MINIMIZE       = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA         = 8;
  SW_RESTORE        = 9;
  SW_SHOWDEFAULT    = 10;
  SW_MAX            = 10;

const
  { DIB color table identifiers }

  DIB_RGB_COLORS = 0;     { color table in RGBs  }
  DIB_PAL_COLORS = 1;     { color table in palette indices  }

const
  { Gradient Fill Modes }
  GRADIENT_FILL_RECT_H = 0;
  GRADIENT_FILL_RECT_V = 1;
  GRADIENT_FILL_TRIANGLE = 2;


type
  PNMHdr = ^TNMHdr;
  tagNMHDR = packed record
    hwndFrom: HWND;
    idFrom: UINT;
    code: Integer;
  end;
  TNMHdr = tagNMHDR;
  NMHDR = tagNMHDR;

  PScreenInfo = ^TScreenInfo;
  TScreenInfo = record
    PixelsPerInchX : Integer;
    PixelsPerInchY : Integer;
    ColorDepth : Integer;
  end;

{painting stuff}

  PDrawItemStruct = ^TDrawItemStruct;
  tagDrawItemStruct = packed record
    ctlType: UINT;
    ctlID : UINT;
    itemID : UINT;
    itemAction : UINT;
    itemState : UINT;
    hwndItem: HWND;
    _hDC: HDC;
    rcItem: TRect;
    itemData : DWord;
  end;
  TDrawItemStruct = tagDrawItemStruct;
  DrawItemStruct = tagDrawItemStruct;

  // ToDo: move this to StdCtrls
  TOwnerDrawStateType = (
    odSelected, odGrayed, odDisabled, odChecked,
    odFocused, odDefault, odHotLight, odInactive, odNoAccel,
    odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit,
    odPainted  // item already painted
    );
  TBaseOwnerDrawState = set of TOwnerDrawStateType;

  PDrawListItemStruct = ^TDrawListItemStruct;
  TDrawListItemStruct = record
    ItemID: UINT;
    Area: TRect;
    DC: HDC;
    ItemState: TBaseOwnerDrawState;
  end;

  PMeasureItemStruct = ^TMeasureItemStruct;
  TMeasureItemStruct = record
    CtlType : UINT;
    CtlID : UINT;
    itemID : UINT;
    itemWidth : UINT;
    itemHeight : UINT;
    itemData : DWORD;
  end;

  PPaintStruct = ^TPaintStruct;
  tagPAINTSTRUCT = packed record
    hdc: HDC;
    fErase: BOOL;
    rcPaint: TRect;
    fRestore: BOOL;
    fIncUpdate: BOOL;
    rgbReserved: array[0..31] of Byte;
  end;
  TPaintStruct = tagPAINTSTRUCT;

  PAINTSTRUCT = tagPAINTSTRUCT;


  PWindowPos = ^TWIndowPos;
  tagWindowPos = packed record
    _hwnd : HWND;
    hwndInsertAfter: HWND;
    x : Integer;
    y : Integer;
    cx : Integer;
    cy : Integer;
    flags: UINT;
    end;
  TWindowPos = tagWindowPos;
  WindowPos = tagWindowPos;

  tagScrollInfo = packed record
    cbSize : UINT;
    fMask : UINT;
    nMin: Integer;
    nMax: Integer;
    nPage: UInt;
    nPos: Integer;
    nTrackPos: Integer;
  end;
  PScrollInfo = ^TScrollInfo;
  TScrollInfo = tagScrollinfo;

  PSize = ^TSize;
  tagSIZE = record
    cx: Longint;
    cy: Longint;
  end;
  TSize = tagSIZE;
  SIZE = tagSIZE;

{ Palette Structures }
  PPaletteEntry = ^tagPaletteEntry;
  tagPALETTEENTRY = packed record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
  TPaletteEntry = tagPALETTEENTRY;
  PALETTEENTRY = tagPALETTEENTRY;

  PLogPalette = ^tagLogPalette;
  tagLOGPALETTE = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..0] of tagPaletteEntry;
  end;
  LOGPALETTE = tagLOGPALETTE;
  TLOGPALETTE = tagLOGPALETTE;

{ GradientFill Structures }
  PTriVertex = ^tagTriVertex;
  tagTRIVERTEX = packed record
    x: Longint;
    y: Longint;
    Red: Shortint;
    Green: Shortint;
    Blue: Shortint;
    Alpha: Shortint;
  end;
  TRIVERTEX = tagTRIVERTEX;

  PGradientTriangle = ^tagGradientTriangle;
  tagGRADIENTTRIANGLE = packed record
    Vertex1: Longint;
    Vertex2: Longint;
    Vertex3: Longint;
  end;
  GRADIENTTRIANGLE = tagGRADIENTTRIANGLE;

  PGradientRect = ^tagGradientRect;
  tagGRADIENTRECT = packed record
    UpperLeft: Longint;
    LowerRight: Longint;
  end;
  GRADIENTRECT = tagGRADIENTRECT;

{ ********************************** }
{        B I T M A P    S T U F F    }

  { TBitmap is an encapsulation of a matrix of pixels. }
  PBitmap = ^TagBitmap;
  tagBITMAP = packed record
    bmType: Longint;
    bmWidth: Longint;
    bmHeight: Longint;
    bmWidthBytes: Longint;
    bmPlanes: Word;
    bmBitsPixel: Word;
    bmBits: Pointer;
  end;
  BITMAP = tagBITMAP;


  PBitmapCoreHeader = ^TBitmapCoreHeader;
  tagBITMAPCOREHEADER = packed record
    bcSize: DWORD;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;
  TBitmapCoreHeader = tagBITMAPCOREHEADER;
  BITMAPCOREHEADER = tagBITMAPCOREHEADER;


  PBitmapInfoHeader = ^TBitmapInfoHeader;
  tagBITMAPINFOHEADER = packed record
    biSize : DWORD;
    biWidth : Longint;
    biHeight : Longint;
    biPlanes : WORD;
    biBitCount : WORD;
    biCompression : DWORD;
    biSizeImage : DWORD;
    biXPelsPerMeter : Longint;
    biYPelsPerMeter : Longint;
    biClrUsed : DWORD;
    biClrImportant : DWORD;
  end;
  TBitmapInfoHeader = tagBITMAPINFOHEADER;
  BITMAPINFOHEADER = tagBITMAPINFOHEADER;


  PRGBTriple = ^TRGBTriple;
  tagRGBTRIPLE = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;
  TRGBTriple = tagRGBTRIPLE;
  RGBTRIPLE = tagRGBTRIPLE;

  PRGBQUAD = ^TRGBQUAD;
  tagRGBQUAD = packed record
          rgbBlue : BYTE;
          rgbGreen : BYTE;
          rgbRed : BYTE;
          rgbReserved : BYTE;
       end;
  TRGBQuad = tagRGBQUAD;
  RGBQUAD = tagRGBQUAD;

  PBitmapInfo = ^TBitmapInfo;
  tagBITMAPINFO = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..0] of TRGBQuad;
  end;
  TBitmapInfo = tagBITMAPINFO;
  BITMAPINFO = tagBITMAPINFO;

  PBitmapCoreInfo = ^TBitmapCoreInfo;
  tagBITMAPCOREINFO = record
    bmciHeader: TBitmapCoreHeader;
    bmciColors: array[0..0] of TRGBTriple;
    Reserved: array[0..0] of Char;
  end;
  TBitmapCoreInfo = tagBITMAPCOREINFO;
  BITMAPCOREINFO = tagBITMAPCOREINFO;


  PBitmapFileHeader = ^TBitmapFileHeader;
  tagBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;
  TBitmapFileHeader = tagBITMAPFILEHEADER;
  BITMAPFILEHEADER = tagBITMAPFILEHEADER;


  PDIBSection = ^TDIBSection;
  tagDIBSECTION = packed record
    dsBm: TagBitmap;
    dsBmih: tagBITMAPINFOHEADER;
    dsBitfields: array[0..2] of DWORD;
    dshSection: THandle;
    dsOffset: DWORD;
  end;
  TDIBSection = tagDIBSECTION;
  DIBSECTION = tagDIBSECTION;

const
  TRUETYPE_FONTTYPE = 4;

  GCP_DBCS = 1;


//==============================================
// Background Modes
//==============================================
  TRANSPARENT = 1;
  OPAQUE = 2;
  BKMODE_LAST = 2;

//==============================================
// Font constants
//==============================================

//------------
// Font Styles
//------------
  LF_FULLFACESIZE = 64;
  LF_FACESIZE = 32;

  OUT_DEFAULT_PRECIS = 0;
  OUT_STRING_PRECIS = 1;
  OUT_CHARACTER_PRECIS = 2;
  OUT_STROKE_PRECIS = 3;
  OUT_TT_PRECIS = 4;
  OUT_DEVICE_PRECIS = 5;
  OUT_RASTER_PRECIS = 6;
  OUT_TT_ONLY_PRECIS = 7;
  OUT_OUTLINE_PRECIS = 8;
  OUT_SCREEN_OUTLINE_PRECIS = 9;

  CLIP_DEFAULT_PRECIS = 0;
  CLIP_CHARACTER_PRECIS = 1;
  CLIP_STROKE_PRECIS = 2;
  CLIP_MASK = 15;
  CLIP_LH_ANGLES = (1 shl 4);
  CLIP_TT_ALWAYS = (2 shl 4);
  CLIP_EMBEDDED  = (8 shl 4);

  DEFAULT_QUALITY     = 0;
  DRAFT_QUALITY       = 1;
  PROOF_QUALITY       = 2;
  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY = 4;

  DEFAULT_PITCH  = 0;
  FIXED_PITCH    = 1;
  VARIABLE_PITCH = 2;
  MONO_FONT      = 8;

  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  FCS_ISO_8859_2       = 3;   // added for ISO_8859_2 under gtk
  MAC_CHARSET         = 77;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  JOHAB_CHARSET       = 130;
  GB2312_CHARSET      = 134;
  CHINESEBIG5_CHARSET = 136;
  GREEK_CHARSET       = 161;
  TURKISH_CHARSET     = 162;
  VIETNAMESE_CHARSET  = 163;
  HEBREW_CHARSET      = 177;
  ARABIC_CHARSET      = 178;
  BALTIC_CHARSET      = 186;
  RUSSIAN_CHARSET     = 204;
  THAI_CHARSET        = 222;
  EASTEUROPE_CHARSET  = 238;
  OEM_CHARSET         = 255;

//-----------
// Font Sets
//-----------
  FS_LATIN1 = 1;
  FS_LATIN2 = 2;
  FS_CYRILLIC = 4;
  FS_GREEK = 8;
  FS_TURKISH = $10;
  FS_HEBREW = $20;
  FS_ARABIC = $40;
  FS_BALTIC = $80;
  FS_VIETNAMESE = $00000100;
  FS_THAI = $10000;
  FS_JISJAPAN = $20000;
  FS_CHINESESIMP = $40000;
  FS_WANSUNG = $80000;
  FS_CHINESETRAD = $100000;
  FS_JOHAB = $200000;
  FS_SYMBOL = DWORD($80000000);

//---------------
// Font Families
//---------------
  FF_DONTCARE   = (0 shl 4);
  FF_ROMAN      = (1 shl 4);
  FF_SWISS      = (2 shl 4);
  FF_MODERN     = (3 shl 4);
  FF_SCRIPT     = (4 shl 4);
  FF_DECORATIVE = (5 shl 4);

//--------------
// Font Weights
//--------------
  FW_DONTCARE   = 0;
  FW_THIN       = 100;
  FW_EXTRALIGHT = 200;
  FW_LIGHT      = 300;
  FW_NORMAL     = 400;
  FW_MEDIUM     = 500;
  FW_SEMIBOLD   = 600;
  FW_BOLD       = 700;
  FW_EXTRABOLD  = 800;
  FW_HEAVY      = 900;
  FW_ULTRALIGHT = FW_EXTRALIGHT;
  FW_REGULAR    = FW_NORMAL;
  FW_DEMIBOLD   = FW_SEMIBOLD;
  FW_ULTRABOLD  = FW_EXTRABOLD;
  FW_BLACK      = FW_HEAVY;

//==============================================
// Brush constants
//==============================================

//--------------
// Brush Styles
//--------------

  BS_SOLID                = 0;
  BS_NULL                 = 1;
  BS_HOLLOW               = BS_NULL;
  BS_HATCHED              = 2;
  BS_PATTERN              = 3;
  BS_INDEXED              = 4;
  BS_DIBPATTERN           = 5;
  BS_DIBPATTERNPT         = 6;
  BS_PATTERN8X8           = 7;
  BS_DIBPATTERN8X8        = 8;
  BS_MONOPATTERN          = 9;

//--------------
// Hatch Styles
//--------------

  HS_HORIZONTAL = 0;       { ----- }
  HS_VERTICAL   = 1;       { ||||| }
  HS_FDIAGONAL  = 2;       { ///// }
  HS_BDIAGONAL  = 3;       { \\\\\ }
  HS_CROSS      = 4;       { +++++ }
  HS_DIAGCROSS  = 5;       { xxxxx }

//==============================================
// Pen constants
//==============================================

//------------
// Pen Styles
//------------

  PS_SOLID       = 0;
  PS_DASH        = 1;      { ------- }
  PS_DOT         = 2;      { ....... }
  PS_DASHDOT     = 3;      { _._._._ }
  PS_DASHDOTDOT  = 4;      { _.._.._ }
  PS_NULL = 5;
  PS_INSIDEFRAME = 6;
  PS_USERSTYLE = 7;
  PS_ALTERNATE = 8;
  PS_STYLE_MASK = 15;

  PS_ENDCAP_ROUND = 0;
  PS_ENDCAP_SQUARE = $100;
  PS_ENDCAP_FLAT = $200;
  PS_ENDCAP_MASK = 3840;

  PS_JOIN_ROUND = 0;
  PS_JOIN_BEVEL = $1000;
  PS_JOIN_MITER = $2000;
  PS_JOIN_MASK = 61440;

  PS_COSMETIC = 0;
  PS_GEOMETRIC = $10000;
  PS_TYPE_MASK = $F0000;


//==============================================
// API system Color constants  pbd
// note these are usually shown ORed with
// $80000000 as these would have interfered with
// other MS color enumerations
// GetSysColor and SetSysColor expects the values
// below
//==============================================

type
  COLORREF = LongInt;
  TColorRef = COLORREF;

const
  CLR_INVALID = TColorRef($FFFFFFFF);

  COLOR_SCROLLBAR = 0;
  COLOR_BACKGROUND = 1;
  COLOR_ACTIVECAPTION = 2;
  COLOR_INACTIVECAPTION = 3;
  COLOR_MENU = 4;
  COLOR_WINDOW = 5;
  COLOR_WINDOWFRAME = 6;
  COLOR_MENUTEXT = 7;
  COLOR_WINDOWTEXT = 8;
  COLOR_CAPTIONTEXT = 9;
  COLOR_ACTIVEBORDER = 10;
  COLOR_INACTIVEBORDER = 11;
  COLOR_APPWORKSPACE = 12;
  COLOR_HIGHLIGHT = 13;
  COLOR_HIGHLIGHTTEXT = 14;
  COLOR_BTNFACE = 15;
  COLOR_BTNSHADOW = 16;
  COLOR_GRAYTEXT = 17;
  COLOR_BTNTEXT = 18;
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT = 20;
  COLOR_3DDKSHADOW = 21;
  COLOR_3DLIGHT = 22;
  COLOR_INFOTEXT = 23;
  COLOR_INFOBK = 24;
  // PBD: 25 is unassigned in all the docs I can find
  //      if someone finds what this is supposed to be then fill it in
  //      note defaults below, and cl[ColorConst] in graphics
  COLOR_HOTLIGHT = 26;
  COLOR_GRADIENTACTIVECAPTION = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_FORM = 29;

  // CLX base, mapped, pseudo, rgb values
  COLOR_clForeground =  30;
  COLOR_clButton =  COLOR_clForeground+1;
  COLOR_clLight =  COLOR_clForeground+2;
  COLOR_clMidlight =  COLOR_clForeground+3;
  COLOR_clDark =  COLOR_clForeground+4;
  COLOR_clMid =  COLOR_clForeground+5;
  COLOR_clText =  COLOR_clForeground+6;
  COLOR_clBrightText =  COLOR_clForeground+7;
  COLOR_clButtonText =  COLOR_clForeground+8;
  COLOR_clBase =  COLOR_clForeground+9;
  //clBackground
  COLOR_clShadow =  COLOR_clForeground+10;
  //clHighlight
  COLOR_clHighlightedText =  COLOR_clForeground+11;

  // CLX normal, mapped, pseudo, rgb values
  COLOR_clNormalForeground =  42;
  COLOR_clNormalButton =  COLOR_clNormalForeground+1;
  COLOR_clNormalLight =  COLOR_clNormalForeground+2;
  COLOR_clNormalMidlight =  COLOR_clNormalForeground+3;
  COLOR_clNormalDark =  COLOR_clNormalForeground+4;
  COLOR_clNormalMid =  COLOR_clNormalForeground+5;
  COLOR_clNormalText =  COLOR_clNormalForeground+6;
  COLOR_clNormalBrightText =  COLOR_clNormalForeground+7;
  COLOR_clNormalButtonText =  COLOR_clNormalForeground+8;
  COLOR_clNormalBase =  COLOR_clNormalForeground+9;
  COLOR_clNormalBackground =  COLOR_clNormalForeground+10;
  COLOR_clNormalShadow =  COLOR_clNormalForeground+11;
  COLOR_clNormalHighlight =  COLOR_clNormalForeground+12;
  COLOR_clNormalHighlightedText =  COLOR_clNormalForeground+13;

  // CLX disabled, mapped, pseudo, rgb values
  COLOR_clDisabledForeground =  56;
  COLOR_clDisabledButton =  COLOR_clDisabledForeground+1;
  COLOR_clDisabledLight =  COLOR_clDisabledForeground+2;
  COLOR_clDisabledMidlight =  COLOR_clDisabledForeground+3;
  COLOR_clDisabledDark =  COLOR_clDisabledForeground+4;
  COLOR_clDisabledMid =  COLOR_clDisabledForeground+5;
  COLOR_clDisabledText =  COLOR_clDisabledForeground+6;
  COLOR_clDisabledBrightText =  COLOR_clDisabledForeground+7;
  COLOR_clDisabledButtonText =  COLOR_clDisabledForeground+8;
  COLOR_clDisabledBase =  COLOR_clDisabledForeground+9;
  COLOR_clDisabledBackground =  COLOR_clDisabledForeground+10;
  COLOR_clDisabledShadow =  COLOR_clDisabledForeground+11;
  COLOR_clDisabledHighlight =  COLOR_clDisabledForeground+12;
  COLOR_clDisabledHighlightedText =  COLOR_clDisabledForeground+13;

  // CLX active, mapped, pseudo, rgb values
  COLOR_clActiveForeground =  70;
  COLOR_clActiveButton =  COLOR_clActiveForeground+1;
  COLOR_clActiveLight =  COLOR_clActiveForeground+2;
  COLOR_clActiveMidlight =  COLOR_clActiveForeground+3;
  COLOR_clActiveDark =  COLOR_clActiveForeground+4;
  COLOR_clActiveMid =  COLOR_clActiveForeground+5;
  COLOR_clActiveText =  COLOR_clActiveForeground+6;
  COLOR_clActiveBrightText =  COLOR_clActiveForeground+7;
  COLOR_clActiveButtonText =  COLOR_clActiveForeground+8;
  COLOR_clActiveBase =  COLOR_clActiveForeground+9;
  COLOR_clActiveBackground =  COLOR_clActiveForeground+10;
  COLOR_clActiveShadow =  COLOR_clActiveForeground+11;
  COLOR_clActiveHighlight =  COLOR_clActiveForeground+12;
  COLOR_clActiveHighlightedText =  COLOR_clActiveForeground+13;

  COLOR_ENDCOLORS = COLOR_clActiveHighlightedText;

  COLOR_DESKTOP = COLOR_BACKGROUND;
  COLOR_3DFACE = COLOR_BTNFACE;
  COLOR_3DSHADOW = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT;


  MAX_SYS_COLORS = COLOR_ENDCOLORS;
  SYS_COLOR_BASE = TColorRef($80000000);


//==============================================
// Stock Objects
//==============================================

  WHITE_BRUSH = 0;
  LTGRAY_BRUSH = 1;
  GRAY_BRUSH = 2;
  DKGRAY_BRUSH = 3;
  BLACK_BRUSH = 4;
  NULL_BRUSH = 5;
  HOLLOW_BRUSH = NULL_BRUSH;
  WHITE_PEN = 6;
  BLACK_PEN = 7;
  NULL_PEN = 8;
  OEM_FIXED_FONT = 10;
  ANSI_FIXED_FONT = 11;
  ANSI_VAR_FONT = 12;
  SYSTEM_FONT = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE = 15;
  SYSTEM_FIXED_FONT = 16;
  DEFAULT_GUI_FONT = 17;
  DC_BRUSH = 18;
  DC_PEN = 19;
  STOCK_LAST = 19;


//==============================================
// Stock Pixmap Types
//==============================================
  idButtonBase = 0;
  idButtonOk = idButtonBase + 1;
  idButtonCancel = idButtonBase + 2;
  idButtonHelp = idButtonBase + 3;
  idButtonYes = idButtonBase + 4;
  idButtonNo = idButtonBase + 5;
  idButtonClose = idButtonBase + 6;
  idButtonAbort = idButtonBase + 7;
  idButtonRetry = idButtonBase + 8;
  idButtonIgnore = idButtonBase + 9;
  idButtonAll = idButtonBase + 10;
  idButtonYesToAll = idButtonBase + 11;
  idButtonNoToAll = idButtonBase + 12;

  idDialogBase = $FF;
  idDialogWarning = idDialogBase + 1;
  idDialogError = idDialogBase + 2;
  idDialogInfo = idDialogBase + 3;
  idDialogConfirm = idDialogBase + 4;

//==============================================
// SystemMetrics constants
//==============================================

  SM_CXSCREEN = 0;
  SM_CYSCREEN = 1;
  SM_CXVSCROLL = 2;
  SM_CYHSCROLL = 3;
  SM_CYCAPTION = 4;
  SM_CXBORDER = 5;
  SM_CYBORDER = 6;
  SM_CXDLGFRAME = 7;
  SM_CYDLGFRAME = 8;
  SM_CYVTHUMB = 9;
  SM_CXHTHUMB = 10;
  SM_CXICON = 11;
  SM_CYICON = 12;
  SM_CXCURSOR = 13;
  SM_CYCURSOR = 14;
  SM_CYMENU = 15;
  SM_CXFULLSCREEN = 16;
  SM_CYFULLSCREEN = 17;
  SM_CYKANJIWINDOW = 18;
  SM_MOUSEPRESENT = 19;
  SM_CYVSCROLL = 20;
  SM_CXHSCROLL = 21;
  SM_DEBUG = 22;
  SM_SWAPBUTTON = 23;
  SM_RESERVED1 = 24;
  SM_RESERVED2 = 25;
  SM_RESERVED3 = 26;
  SM_RESERVED4 = 27;
  SM_CXMIN = 28;
  SM_CYMIN = 29;
  SM_CXSIZE = 30;
  SM_CYSIZE = 31;
  SM_CXFRAME = 32;
  SM_CYFRAME = 33;
  SM_CXMINTRACK = 34;
  SM_CYMINTRACK = 35;
  SM_CXDOUBLECLK = 36;
  SM_CYDOUBLECLK = 37;
  SM_CXICONSPACING = 38;
  SM_CYICONSPACING = 39;
  SM_MENUDROPALIGNMENT = 40;
  SM_PENWINDOWS = 41;
  SM_DBCSENABLED = 42;
  SM_CMOUSEBUTTONS = 43;

  SM_CXFIXEDFRAME = SM_CXDLGFRAME;
  SM_CYFIXEDFRAME = SM_CYDLGFRAME;
  SM_CXSIZEFRAME = SM_CXFRAME;
  SM_CYSIZEFRAME = SM_CYFRAME;

  SM_SECURE = 44;
  SM_CXEDGE = 45;
  SM_CYEDGE = 46;
  SM_CXMINSPACING = 47;
  SM_CYMINSPACING = 48;
  SM_CXSMICON = 49;
  SM_CYSMICON = 50;
  SM_CYSMCAPTION = 51;
  SM_CXSMSIZE = 52;
  SM_CYSMSIZE = 53;
  SM_CXMENUSIZE = 54;
  SM_CYMENUSIZE = 55;
  SM_ARRANGE = 56;
  SM_CXMINIMIZED = 57;
  SM_CYMINIMIZED = 58;
  SM_CXMAXTRACK = 59;
  SM_CYMAXTRACK = 60;
  SM_CXMAXIMIZED = 61;
  SM_CYMAXIMIZED = 62;
  SM_NETWORK = 63;
  SM_CLEANBOOT = 67;
  SM_CXDRAG = 68;
  SM_CYDRAG = 69;
  SM_SHOWSOUNDS = 70;
  SM_CXMENUCHECK = 71;
  SM_CYMENUCHECK = 72;
  SM_SLOWMACHINE = 73;
  SM_MIDEASTENABLED = 74;
  SM_MOUSEWHEELPRESENT = 75;
  SM_CMETRICS = 76;

//==============================================
// GetDeviceCaps constants
//==============================================
  BI_RGB        = 0;
  BI_BITFIELDS  = 3;


  HORZSIZE      = 4;   { Horizontal size in millimeters           }
  VERTSIZE      = 6;   { Vertical size in millimeters             }
  HORZRES       = 8;   { Horizontal width in pixels               }
  VERTRES       = 10;  { Vertical height in pixels                }
  BITSPIXEL     = 12;  { Number of bits per pixel                 }
  PLANES        = 14;  { Number of planes                         }
  LOGPIXELSX    = 88;  { Logical pixelsinch in X                  }
  LOGPIXELSY    = 90;  { Logical pixelsinch in Y                  }
  SIZEPALETTE   = 104; { Number of entries in physical palette    }
  NUMRESERVED   = 106; { Number of reserved entries in palette    }


  { Text Alignment Options }

  TA_NOUPDATECP = 0;
  TA_UPDATECP   = 1;
  TA_LEFT       = 0;
  TA_RIGHT      = 2;
  TA_CENTER     = 6;
  TA_TOP        = 0;
  TA_BOTTOM     = 8;
  TA_BASELINE   = $18;
  TA_RTLREADING = $100;
  TA_MASK       = (TA_BASELINE+TA_CENTER+TA_UPDATECP+TA_RTLREADING);

  { PolyFill() Modes }
  ALTERNATE     = 1;
  WINDING       = 2;
  POLYFILL_LAST = 2;

  { StretchBlt() Modes }
  BLACKONWHITE      = 1;
  WHITEONBLACK      = 2;
  COLORONCOLOR      = 3;
  HALFTONE          = 4;
  MAXSTRETCHBLTMODE = 4;

  { constants for CreateDIBitmap }

  CBM_INIT = 4;     { initialize bitmap  }


type
  TFarProc = Pointer;

  TFNWndProc = TFarProc;

  MakeIntResourceA = PAnsiChar;
  MakeIntResource = MakeIntResourceA;

  PLogFontA = ^TLogFontA;
  PLogFontW = ^TLogFontW;
  PLogFont = PLogFontA;

  tagLOGFONTA = packed record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  tagLOGFONTW = packed record
    lfHeight: Longint;
    lfWidth: Longint;
    lfEscapement: Longint;
    lfOrientation: Longint;
    lfWeight: Longint;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..LF_FACESIZE - 1] of WideChar;
  end;

  tagLOGFONT = tagLOGFONTA;
  TLogFontA = tagLOGFONTA;
  TLogFontW = tagLOGFONTW;
  TLogFont = TLogFontA;

  LOGFONTA = tagLOGFONTA;

  LOGFONTW = tagLOGFONTW;

  LOGFONT = LOGFONTA;

  PLogBrush = ^TLogBrush;
  tagLOGBRUSH = packed record
    lbStyle: LongWord;
    lbColor: TColorRef;
    lbHatch: Longint;
  end;
  TLogBrush = tagLOGBRUSH;
  LOGBRUSH = tagLOGBRUSH;


  PMaxLogPalette = ^TMaxLogPalette; // not in Windows Headers
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array [Byte] of TPaletteEntry;
  end;

  PEnumLogFontA = ^TEnumLogFontA;
  PEnumLogFontW = ^TEnumLogFontW;
  PEnumLogFont = PEnumLogFontA;

  tagENUMLOGFONTA = packed record
    elfLogFont: TLogFontA;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of AnsiChar;
    elfStyle: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  tagENUMLOGFONTW = packed record
    elfLogFont: TLogFontW;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of WideChar;
    elfStyle: array[0..LF_FACESIZE - 1] of WideChar;
  end;

  tagENUMLOGFONT = tagENUMLOGFONTA;
  TEnumLogFontA = tagENUMLOGFONTA;
  TEnumLogFontW = tagENUMLOGFONTW;
  TEnumLogFont = TEnumLogFontA;

  ENUMLOGFONTA = tagENUMLOGFONTA;

  ENUMLOGFONTW = tagENUMLOGFONTW;

  ENUMLOGFONT = ENUMLOGFONTA;

  PEnumLogFontExA = ^TEnumLogFontExA;
  PEnumLogFontExW = ^TEnumLogFontExW;
  PEnumLogFontEx = PEnumLogFontExA;

  tagENUMLOGFONTEXA = packed record
    elfLogFont: TLogFontA;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of AnsiChar;
    elfStyle: array[0..LF_FACESIZE - 1] of AnsiChar;
    elfScript: array[0..LF_FACESIZE - 1] of AnsiChar;
  end;

  tagENUMLOGFONTEXW = packed record
    elfLogFont: TLogFontW;
    elfFullName: array[0..LF_FULLFACESIZE - 1] of WideChar;
    elfStyle: array[0..LF_FACESIZE - 1] of WideChar;
    elfScript: array[0..LF_FACESIZE - 1] of WideChar;
  end;

  tagENUMLOGFONTEX = tagENUMLOGFONTEXA;
  TEnumLogFontExA = tagENUMLOGFONTEXA;
  TEnumLogFontExW = tagENUMLOGFONTEXW;
  TEnumLogFontEx = TEnumLogFontExA;

  ENUMLOGFONTEXA = tagENUMLOGFONTEXA;

  ENUMLOGFONTEXW = tagENUMLOGFONTEXW;

  ENUMLOGFONTEX = ENUMLOGFONTEXA;

  PLogPen = ^TLogPen;
  tagLOGPEN = packed record
    lopnStyle: LongWord;
    lopnWidth: TPoint;
    lopnColor: TColorRef;
  end;
  TLogPen = tagLOGPEN;
  LOGPEN = tagLOGPEN;

type

  PTextMetricA = ^TTextMetricA;
  PTextMetricW = ^TTextMetricW;
  PTextMetric = PTextMetricA;

  tagTextMetricA = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: AnsiChar;
    tmLastChar: AnsiChar;
    tmDefaultChar: AnsiChar;
    tmBreakChar: AnsiChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;


  tagTEXTMETRICW = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: WideChar;
    tmLastChar: WideChar;
    tmDefaultChar: WideChar;
    tmBreakChar: WideChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;

  tagTEXTMETRIC = tagTEXTMETRICA;
  TTextMetricA = tagTEXTMETRICA;
  TTextMetricW = tagTEXTMETRICW;
  TTextMetric = TTextMetricA;
  TEXTMETRICA = tagTEXTMETRICA;
  TEXTMETRICW = tagTEXTMETRICW;
  TEXTMETRIC = TEXTMETRICA;



  PWndClassExA = ^TWndClassExA;
  PWndClassExW = ^TWndClassExW;
  PWndClassEx = PWndClassExA;

  tagWNDCLASSEXA = packed record
    cbSize: UINT;
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    _hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PAnsiChar;
    lpszClassName: PAnsiChar;
    hIconSm: HICON;
  end;

  tagWNDCLASSEXW = packed record
    cbSize: UINT;
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    _hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PWideChar;
    lpszClassName: PWideChar;
    hIconSm: HICON;
  end;

  tagWNDCLASSEX = tagWNDCLASSEXA;
  TWndClassExA = tagWNDCLASSEXA;
  TWndClassExW = tagWNDCLASSEXW;
  TWndClassEx = TWndClassExA;
  WNDCLASSEXA = tagWNDCLASSEXA;
  WNDCLASSEXW = tagWNDCLASSEXW;
  WNDCLASSEX = WNDCLASSEXA;

  PWndClassA = ^TWndClassA;
  PWndClassW = ^TWndClassW;
  PWndClass = PWndClassA;

  tagWNDCLASSA = packed record
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PAnsiChar;
    lpszClassName: PAnsiChar;
  end;

  tagWNDCLASSW = packed record
    style: UINT;
    lpfnWndProc: TFNWndProc;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINST;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: PWideChar;
    lpszClassName: PWideChar;
  end;

  tagWNDCLASS = tagWNDCLASSA;
  TWndClassA = tagWNDCLASSA;
  TWndClassW = tagWNDCLASSW;
  TWndClass = TWndClassA;
  WNDCLASSA = tagWNDCLASSA;
  WNDCLASSW = tagWNDCLASSW;
  WNDCLASS = WNDCLASSA;

type
  PMsg = ^TMsg;
  tagMSG = packed record
    hwnd: HWND;
    message: LongWord;
    wParam: Longint;
    lParam: Longint;
    time: DWORD;
    pt: TPoint;
  end;
  TMsg = tagMSG;
  _MSG = tagMSG;


type
  // Moved from Controls to avoid circles
  // Since it is part of the interface now
  TCreateParams = record
    Caption: PChar;
    Style: Cardinal;
    ExStyle: Cardinal;
    X, Y: Integer;
    Width, Height: Integer;
    WndParent: HWnd;
    Param: Pointer;
    WindowClass: TWndClass;
    WinClassName: array[0..63] of Char;
  end;

//------------------------------------------------------------------------------
// prototype for timer callback
type
  TFNTimerProc = procedure of object;

//------------------------------------------------------------------------------
// clipboard
type
  TClipboardFormat = cardinal;
  PClipboardFormat = ^TClipboardFormat;

  TClipboardRequestEvent = procedure(const RequestedFormatID: TClipboardFormat;
    Data: TStream) of object;

  TClipboardType = (ctPrimarySelection, ctSecondarySelection, ctClipboard);

const
  ClipboardTypeName : array[TClipboardType] of string = (
      'primary selection', 'secondary selection', 'clipboard'
    );

type
  TPredefinedClipboardFormat = (
      pcfText,
      pcfBitmap,
      pcfPixmap,
      pcfIcon,
      pcfPicture,
      pcfObject,
      pcfComponent,
      pcfCustomData,

      // Delphi definitions (only for compatibility)
      pcfDelphiText,
      pcfDelphiBitmap,
      pcfDelphiPicture,
      pcfDelphiMetaFilePict,
      pcfDelphiObject,
      pcfDelphiComponent,

      // Kylix definitions (only for compatibility)
      pcfKylixPicture,
      pcfKylixBitmap,
      pcfKylixDrawing,
      pcfKylixComponent
    );

const
  PredefinedClipboardMimeTypes : array[TPredefinedClipboardFormat] of string = (
     'text/plain',
     'image/bmp',
     'image/xpm',
     'image/lcl.icon',
     'image/lcl.picture',
     'application/lcl.object',
     'application/lcl.component',
     'application/lcl.customdata',

     // Delphi definitions (only for compatibility)
     'text/plain',
     'image/delphi.bitmap',
     'Delphi Picture',
     'image/delphi.metafilepict',
     'application/delphi.object',
     'Delphi Component',

     // Kylix definitons (only for compatibility)
     'image/delphi.picture',
     'image/delphi.bitmap',
     'image/delphi.drawing',
     'application/delphi.component'
  );


const
  csNone = 0;
  csAlignment = 1;
//  csBox = 2;
  csButton = 3;
  csComboBox = 4;
  csCheckbox = 5;
  csEdit = 6;
  csForm= 7;
  csLabel = 8;
//  csgtkTable = 9;
  csScrollBar = 10;
  csListView = 11;
//  csMainForm = 12;
  csMemo = 13;
  csMainMenu = 14;
  csMenuBar = 15;
  csMenuItem = 16;
  csNotebook = 17;
  csFileDialog = 18;
  csRadioButton = 19;
  csScrolledWindow= 20;
  csSpinedit = 21;
  csStatusBar = 22;
//  csTable = 23;
  csToggleBox = 24;
  //csVScrollBar = 25;
//  csFrame = 26;
//  csButtonBox = 27;
//  csCanvas = 28;
  csGroupBox = 29;

//  csFont = 30;
//  csPen = 31;
//  csBrush = 32;
  //csTimer = 33;
  csPage = 34;

  csColorDialog = 35;
  csListBox = 36;
  csFontDialog = 37;
  csProgressBar = 38;
  csTrackBar = 39;
  csWinControl = 40;
  csFixed = csWinControl; //TODO remove
  csImage = 41;
  csToolbar = 42;
  csToolButton = 43;
  csBitBtn = 44;
  csCListBox = 45;
  csSpeedButton = 46;
  csPopupMenu = 47;
  csHintWindow = 48;

  csCalendar = 49;

  csArrow = 50;
  csPanel = 51;
  csScrollBox = 52;

  csCheckListBox = 53;
  csPairSplitter = 54;
  csPairSplitterSide = 55;

  csOpenFileDialog = 56;
  csSaveFileDialog = 57;
  csSelectDirectoryDialog = 58;
  csPreviewFileControl = 59;
  csPreviewFileDialog = 60;

  csNonLCL = 61; // for non LCL controls, that create their own handles


const
  // Mouse message key states
  MK_LBUTTON  = 1;
  MK_RBUTTON = 2;
  MK_SHIFT = 4;
  MK_CONTROL = 8;
  MK_MBUTTON = $10;


Function CS_To_String(CompStyle: Integer): String;
// key mapping

type
  TShortCut = Low(Word)..High(Word);   {should be moved to classes}

function HiWord(i: integer): word;
function LoWord(i: integer): word;
Function Char2VK(C : Char) : Word;
function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;


implementation


function HiWord(i: integer): word;
begin
  Result:=Hi(i);
end;

function LoWord(i: integer): word;
begin
  Result:=Lo(i);
end;

Function Char2VK(C : Char) : Word;
begin
  Case C of
    '0'..'9' :Result := VK_0 + Ord(C) - Ord('0');
    'a'..'z' :Result := VK_A + Ord(C) - Ord('a');
    'A'..'Z' :Result := VK_A + Ord(C) - Ord('A');
  else
    Result:=0;
  end;
end;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer;
begin
  Result:=(int64(nNumber)*int64(nNumerator)) div nDenominator;
end;

{------------------------------------------------------------------------------
  Function: CS_To_String
  Params: CompStyle - Component Style
  Returns: The component style name

  Converts a component style identIfier into the correct component style name
 ------------------------------------------------------------------------------}
Function CS_To_String(CompStyle: Integer): String;
Begin
  Case CompStyle of
    csNone:
      Result := 'csNone';
    csAlignment:
      Result := 'csAlignment';
//    csBox:
//      Result := 'csBox';
    csButton:
      Result := 'csButton';
    csComboBox:
      Result := 'csComboBox';
    csCheckbox:
      Result := 'csCheckbox';
    csEdit:
      Result := 'csEdit';
    csForm:
      Result := 'csForm';
    csLabel:
      Result := 'csLabel';
//    csGTKTable:
//      Result := 'csGTKTable';
    csScrollBar:
      Result := 'csScrollBar';
    csListView:
      Result := 'csListView';
//    csMainForm:
//      Result := 'csMainForm';
    csMemo:
      Result := 'csMemo';
    csMainMenu:
      Result := 'csMainMenu';
    csMenuBar:
      Result := 'csMenuBar';
    csMenuItem:
      Result := 'csMenuItem';
    csNotebook:
      Result := 'csNotebook';
    csFileDialog:
      Result := 'csFileDialog';
    csOpenFileDialog:
      Result := 'csOpenFileDialog';
    csSaveFileDialog:
      Result := 'csSaveFileDialog';
    csSelectDirectoryDialog:
      Result := 'csSelectDirectoryDialog';
    csRadioButton:
      Result := 'csRadioButton';
    csScrolledWinDow:
      Result := 'csScrolledWinDow';
    csSpinEdit:
      Result := 'csSpinEdit';
    csStatusBar:
      Result := 'csStatusBar';
//    csTable:
//      Result := 'csTable';
    csToggleBox:
      Result := 'csToggleBox';
//    25: //csVScrollBar
//      Result := 'csVScrollBar';
//    csFrame:
//      Result := 'csFrame';
//    csButtonBox:
//      Result := 'csButtonBox';
//    csCanvas:
//      Result := 'csCanvas';
    csGroupBox:
      Result := 'csGroupBox';
//    csFont:
//      Result := 'csFont';
//    csPen:
//      Result := 'csPen';
//    csBrush:
//      Result := 'csBrush';
//    33: //csTimer
//      Result := 'csTimer';
    csPage:
      Result := 'csPage';
    csColorDialog:
      Result := 'csColorDialog';
    csListBox:
      Result := 'csListBox';
    csFontDialog:
      Result := 'csFontDialog';
    csProgressBar:
      Result := 'csProgressBar';
    csTrackBar:
      Result := 'csTrackBar';
    csFixed:
      Result := 'csFixed';
    csImage:
      Result := 'csImage';
    csToolbar:
      Result := 'csToolbar';
    csToolButton:
      Result := 'csToolButton';
    csBitBtn:
      Result := 'csBitBtn';
    csCListBox:
      Result := 'csCListBox';
    csSpeedButton:
      Result := 'csSpeedButton';
    csPopupMenu:
      Result := 'csPopupMenu';
    csHintWinDow:
      Result := 'csHintWinDow';
    csCalendar:
      Result := 'csCalendar';
    csArrow:
      Result := 'csArrow';
    csPanel:
      Result := 'csPanel';
    csScrollBox:
      Result := 'csScrollBox';
    csCheckListBox:
      Result := 'csCheckListBox';
    csPairSplitter:
      Result := 'csPairSplitter';
    csPairSplitterSide:
      Result := 'csPairSplitterSide';
    csPreviewFileControl:
      Result := 'csPreviewFileControl';
    csPreviewFileDialog:
      Result := 'csPreviewFileDialog';
    csNonLCL:
      Result := 'csNonLCL';
    Else
      Result := Format('Unknown component style %D', [CompStyle]);
  End; {Case}
End;


end.

{
  $Log$
  Revision 1.69  2004/11/20 11:49:15  mattias
  implemented stopping project on close project

  Revision 1.68  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.67  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.66  2004/09/02 09:16:59  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.65  2004/08/30 16:37:58  mattias
  added OnUTF8KeyPresss

  Revision 1.64  2004/08/30 16:11:02  mattias
  changed GTK2 IFDEF to USE_UTF8BIDI_LCL

  Revision 1.63  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.62  2004/06/30 17:14:11  micha
  let synedit use scrollwindowex, if that does not work, use invalidate

  Revision 1.61  2004/05/14 17:17:29  micha
  add measureitem message and struct

  Revision 1.60  2004/03/08 00:48:05  mattias
  moved TOnwerDrawState to StdCtrls

  Revision 1.59  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.58  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.57  2004/02/04 13:40:19  mattias
  ShortCutToText now deletes any modifier

  Revision 1.56  2004/02/04 12:48:17  mattias
  added CLX colors

  Revision 1.55  2004/01/17 11:11:42  micha
  scrollbar fix (from martin smat)

  Revision 1.54  2004/01/03 21:06:05  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.53  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.52  2003/12/26 10:16:54  mattias
  changed TColorRef from longword to longint

  Revision 1.51  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.50  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

  Revision 1.49  2003/11/15 13:07:09  mattias
  added ambigious unit check for IDE

  Revision 1.48  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.47  2003/10/30 18:25:18  mattias
  fixed keymapping for irregular keys

  Revision 1.46  2003/10/23 19:03:13  ajgenius
  re-add TByteArray

  Revision 1.45  2003/10/23 16:15:30  micha
  compatibility with new 1.1

  Revision 1.44  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.43  2003/10/15 20:33:36  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.42  2003/09/10 19:15:15  mattias
  implemented copying graphics from/to clipboard

  Revision 1.41  2003/09/10 16:29:13  mattias
  added Kylix 3 specials

  Revision 1.40  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.39  2003/07/02 10:02:51  mattias
  fixed TPaintStruct

  Revision 1.38  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.37  2003/05/19 08:16:33  mattias
  fixed allocation of dc backcolor

  Revision 1.36  2003/04/12 09:26:58  mattias
  fixes for current fpc 1.1

  Revision 1.35  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.34  2003/03/16 13:47:45  mattias
  improved rpm building and added support for 1.0.7

  Revision 1.33  2003/02/28 19:54:05  mattias
  added ShowWindow

  Revision 1.32  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.31  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.30  2002/12/25 13:30:36  mattias
  added more windows funcs and fixed jump to compiler error end of file

  Revision 1.29  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.28  2002/12/12 17:47:45  mattias
  new constants for compatibility

  Revision 1.27  2002/12/05 22:16:28  mattias
  double byte char font started

  Revision 1.26  2002/11/23 13:48:43  mattias
  added Timer patch from Vincent Snijders

  Revision 1.25  2002/11/22 09:59:29  mattias
  removed duplicate PPoint

  Revision 1.24  2002/10/26 10:21:01  lazarus
  MG: broke actnlist <-> menus circle

  Revision 1.23  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.22  2002/10/15 17:09:53  lazarus
  AJ: fixed GTK DrawText to use WordWrap, and add DT_EditControl

  Revision 1.21  2002/10/12 16:36:39  lazarus
  AJ: added new QueryUser/NotifyUser

  Revision 1.20  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.19  2002/10/04 14:24:14  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.18  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.17  2002/09/27 20:52:22  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.16  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.15  2002/09/11 15:04:49  lazarus
  MG: added stringhashlist.pas

  Revision 1.14  2002/09/10 06:49:18  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.13  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.12  2002/08/27 06:40:50  lazarus
  MG: ShortCut support for buttons from Andrew

  Revision 1.11  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.10  2002/08/16 20:13:09  lazarus
  MG: custom external tools are now shown in the menu

  Revision 1.9  2002/08/15 15:46:48  lazarus
  MG: added changes from Andrew (Clipping)

  Revision 1.8  2002/07/29 13:39:07  lazarus
  MG: removed ambigious TBitmap from LCLType and added Escape key to MessageDlgs

  Revision 1.7  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.6  2002/05/28 19:39:45  lazarus
  MG: added gtk rc file support and started stule dependent syscolors

  Revision 1.5  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.4  2002/03/31 23:20:38  lazarus
  MG: fixed initial size of TPage

  Revision 1.3  2002/03/31 22:01:37  lazarus
  MG: fixed unreleased/unpressed Ctrl/Alt/Shift

  Revision 1.2  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.1  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.14  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.13  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.12  2001/11/01 18:48:52  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.11  2001/10/31 21:43:28  lazarus
  Added code for TApplication to get it ready to accept exceptions.
  Shane

  Revision 1.10  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.9  2001/06/20 13:35:51  lazarus
  MG: added VK_IRREGULAR and key grabbing

  Revision 1.8  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.7  2001/04/06 22:28:09  lazarus
  * TTimer uses winapi interface now instead of sendmessage interface, stoppok

  Revision 1.6  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.5  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.4  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.3  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import
}
