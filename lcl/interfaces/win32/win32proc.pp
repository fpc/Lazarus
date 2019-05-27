{
 /***************************************************************************
                         win32proc.pp  -  Misc Support Functions
                             -------------------



 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Win32Proc;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
  Windows, Win32Extra, Classes, SysUtils,
  LMessages, LCLType, LCLProc, LCLMessageGlue, LazUTF8, Controls, Forms, Menus,
  GraphType, IntfGraphics, Themes;

const
  LV_DISP_INFO_COUNT = 2;  
  
Type
  TEventType = (etNotify, etKey, etKeyPress, etMouseWheel, etMouseUpDown);

  TParentMsgHandlerProc = function(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;

  TDrawItemHandlerProc = procedure (const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; const DrawIS: TDrawItemStruct;
      var ItemMsg: Integer; var DrawListItem: Boolean);

  TGetClientOffsetProc = procedure (const AWinControl: TWinControl; var Rect: TRect);

  PWin32WindowInfo = ^TWin32WindowInfo;
  TWin32WindowInfo = record
    Overlay: HWND;           // overlay, transparent window on top, used by designer
    UpDown: HWND;
    PopupMenu: TPopupMenu;
    DefWndProc: WNDPROC;
    ParentMsgHandler: TParentMsgHandlerProc;
    WinControl: TWinControl;
    PWinControl: TWinControl; // control to paint for
    AWinControl: TWinControl; // control associated with (for buddy controls)
    List: TStrings;
    StayOnTopList: TFPList;   // a list of windows that were normalized when showing modal
    MaxLength: integer;
    RestoreState: LongInt;       // restore window state
    MouseX, MouseY: smallint; // noticing spurious WM_MOUSEMOVE messages
    DispInfoTextA: array [0..LV_DISP_INFO_COUNT-1] of AnsiString; // buffer for ListView LVN_GETDISPINFO notification
    DispInfoTextW: array [0..LV_DISP_INFO_COUNT-1] of WideString; // it's recommended to keep buffer unchanged
    DispInfoIndex: Integer;                    // between 2 calls of LVN_GETDISPINFO
    DrawItemIndex: integer;   // in case of listbox, when handling WM_DRAWITEM
    DrawItemSelected: boolean;// whether this item is selected LB_GETSEL not uptodate yet
    needParentPaint: boolean; // has a tabpage as parent, and is winxp themed
    isTabPage: boolean;       // is window of tabpage
    isComboEdit: boolean;     // is buddy of combobox, the edit control
    isChildEdit: boolean;     // is buddy edit of a control
    ThemedCustomDraw: boolean;// controls needs themed drawing in wm_notify/nm_customdraw
    IMEComposed: Boolean;
    DrawItemHandler: TDrawItemHandlerProc;
    ClientOffsetProc: TGetClientOffsetProc;   // used by GetLCLClientBoundsOffset
    case integer of
      0: (spinValue: Double);
      1: (
        TrackValid: Boolean; // Set when we have a valid trackpos
        TrackPos: Integer    // keeps the thumb position while tracking
      );
  end;

function WM_To_String(WM_Message: Integer): string;
function WindowPosFlagsToString(Flags: UINT): string;
function ObjectToHWND(const AObject: TObject): HWND;
function LCLControlSizeNeedsUpdate(Sender: TWinControl; SendSizeMsgOnDiff: boolean): boolean;
function GetLCLClientBoundsOffset(Sender: TObject; out ORect: TRect): boolean;
function GetLCLClientBoundsOffset(Handle: HWnd; out Rect: TRect): boolean;
procedure LCLBoundsToWin32Bounds(Sender: TObject; var Left, Top: Integer);
procedure Win32PosToLCLPos(Sender: TObject; var Left, Top: SmallInt);
procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);
function GetWin32NativeDoubleBuffered(Sender: TWinControl): boolean;

procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);

function AllocWindowInfo(Window: HWND): PWin32WindowInfo;
function DisposeWindowInfo(Window: HWND): boolean;
function GetWin32WindowInfo(Window: HWND): PWin32WindowInfo;

procedure RemoveStayOnTopFlags(AppHandle: HWND; ASystemTopAlso: Boolean = False);
procedure RestoreStayOnTopFlags(AppHandle: HWND);
procedure HidePopups(AppHandle: HWND);
procedure RestorePopups;
function LookupTopWindow(AppHandle: HWND): HWND;

procedure AddToChangedMenus(Window: HWnd);
procedure RedrawMenus;
function MeasureTextForWnd(const AWindow: HWND; Text: string; var Width, Height: integer): boolean;
function MeasureText(const AWinControl: TWinControl; Text: string; var Width, Height: integer): boolean;
function GetControlText(AHandle: HWND): string;

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);

function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP):TRawImageLineOrder;
function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
function IsAlphaBitmap(ABitmap: HBITMAP): Boolean;
function IsAlphaDC(ADC: HDC): Boolean;

procedure BlendRect(ADC: HDC; const ARect: TRect; Color: ColorRef);
function GetLastErrorText(AErrorCode: Cardinal): String;
function BitmapToRegion(hBmp: HBITMAP; cTransparentColor: COLORREF = 0; cTolerance: COLORREF  = $101010): HRGN;

function WndClassName(Wnd: HWND): String; inline;
function WndText(Wnd: HWND): String; inline;

{ String functions that may be moved to the RTL in the future }
function WideStrLCopy(dest, source: PWideChar; maxlen: SizeInt): PWideChar;
procedure UpdateWindowsVersion;

type 
  PStayOnTopWindowsInfo = ^TStayOnTopWindowsInfo;
  TStayOnTopWindowsInfo = record
    AppHandle: HWND;
    StayOnTopList: TFPList;
    SystemTopAlso: Boolean;
  end;

  PPopupOwnersWindowInfo = ^TPopupOwnersWindowInfo;
  TPopupOwnersWindowInfo = record
    AppHandle: HWND;
    OwnersList: TFPList;
  end;

  PLookupTopWindowInfo = ^TLookupTopWindowInfo;
  TLookupTopWindowInfo = record
    AppHandle: HWND;
    TopWindow: HWND;
  end;
  
  TWindowsVersion = (
    wvUnknown,
    wv95,
    wvNT4,
    wv98,
    wvMe,
    wv2000,
    wvXP,
    wvServer2003,
    //wvServer2003R2,  // has the same major/minor as wvServer2003
    wvVista,
    //wvServer2008,    // has the same major/minor as wvVista
    wv7,
    wv8,
    wv8_1,
    wv10,
    wvLater
  );

var
  DefaultWindowInfo: TWin32WindowInfo;
  WindowInfoAtom: ATOM;
  ChangedMenus: TFPList; // list of HWNDs which menus needs to be redrawn
  WindowsVersion: TWindowsVersion = wvUnknown;
  ComCtlVersion: Cardinal = 0; //initialized in Win32Extra


implementation

uses
  LCLStrConsts, Dialogs, StdCtrls, ExtCtrls,
  LCLIntf; //remove this unit when GetWindowSize is moved to TWSWinControl

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

var
  InRemoveStayOnTopFlags: Integer = 0;
  PopupOwnersList: TFPList = nil;
{------------------------------------------------------------------------------
  function: WM_To_String
  Params: WM_Message - a WinDows message
  Returns: A WinDows-message name

  Converts a winDows message identIfier to a string
 ------------------------------------------------------------------------------}
function WM_To_String(WM_Message: Integer): string;
begin
  case WM_Message of
    $0000: Result := 'WM_NULL';
    $0001: Result := 'WM_CREATE';
    $0002: Result := 'WM_DESTROY';
    $0003: Result := 'WM_MOVE';
    $0005: Result := 'WM_SIZE';
    $0006: Result := 'WM_ACTIVATE';
    $0007: Result := 'WM_SETFOCUS';
    $0008: Result := 'WM_KILLFOCUS';
    $000A: Result := 'WM_ENABLE';
    $000B: Result := 'WM_SETREDRAW';
    $000C: Result := 'WM_SETTEXT';
    $000D: Result := 'WM_GETTEXT';
    $000E: Result := 'WM_GETTEXTLENGTH';
    $000F: Result := 'WM_PAINT';
    $0010: Result := 'WM_CLOSE';
    $0011: Result := 'WM_QUERYENDSESSION';
    $0012: Result := 'WM_QUIT';
    $0013: Result := 'WM_QUERYOPEN';
    $0014: Result := 'WM_ERASEBKGND';
    $0015: Result := 'WM_SYSCOLORCHANGE';
    $0016: Result := 'WM_EndSESSION';
    $0017: Result := 'WM_SYSTEMERROR';
    $0018: Result := 'WM_SHOWWINDOW';
    $0019: Result := 'WM_CTLCOLOR';
    $001A: Result := 'WM_WININICHANGE or WM_SETTINGCHANGE';
    $001B: Result := 'WM_DEVMODECHANGE';
    $001C: Result := 'WM_ACTIVATEAPP';
    $001D: Result := 'WM_FONTCHANGE';
    $001E: Result := 'WM_TIMECHANGE';
    $001F: Result := 'WM_CANCELMODE';
    $0020: Result := 'WM_SETCURSOR';
    $0021: Result := 'WM_MOUSEACTIVATE';
    $0022: Result := 'WM_CHILDACTIVATE';
    $0023: Result := 'WM_QUEUESYNC';
    $0024: Result := 'WM_GETMINMAXINFO';
    $0026: Result := 'WM_PAINTICON';
    $0027: Result := 'WM_ICONERASEBKGND';
    $0028: Result := 'WM_NEXTDLGCTL';
    $002A: Result := 'WM_SPOOLERSTATUS';
    $002B: Result := 'WM_DRAWITEM';
    $002C: Result := 'WM_MEASUREITEM';
    $002D: Result := 'WM_DELETEITEM';
    $002E: Result := 'WM_VKEYTOITEM';
    $002F: Result := 'WM_CHARTOITEM';
    $0030: Result := 'WM_SETFONT';
    $0031: Result := 'WM_GETFONT';
    $0032: Result := 'WM_SETHOTKEY';
    $0033: Result := 'WM_GETHOTKEY';
    $0037: Result := 'WM_QUERYDRAGICON';
    $0039: Result := 'WM_COMPAREITEM';
    $003D: Result := 'WM_GETOBJECT';
    $0041: Result := 'WM_COMPACTING';
    $0044: Result := 'WM_COMMNOTIFY { obsolete in Win32}';
    $0046: Result := 'WM_WINDOWPOSCHANGING';
    $0047: Result := 'WM_WINDOWPOSCHANGED';
    $0048: Result := 'WM_POWER';
    $004A: Result := 'WM_COPYDATA';
    $004B: Result := 'WM_CANCELJOURNAL';
    $004E: Result := 'WM_NOTIFY';
    $0050: Result := 'WM_INPUTLANGCHANGEREQUEST';
    $0051: Result := 'WM_INPUTLANGCHANGE';
    $0052: Result := 'WM_TCARD';
    $0053: Result := 'WM_HELP';
    $0054: Result := 'WM_USERCHANGED';
    $0055: Result := 'WM_NOTIFYFORMAT';
    $007B: Result := 'WM_CONTEXTMENU';
    $007C: Result := 'WM_STYLECHANGING';
    $007D: Result := 'WM_STYLECHANGED';
    $007E: Result := 'WM_DISPLAYCHANGE';
    $007F: Result := 'WM_GETICON';
    $0080: Result := 'WM_SETICON';
    $0081: Result := 'WM_NCCREATE';
    $0082: Result := 'WM_NCDESTROY';
    $0083: Result := 'WM_NCCALCSIZE';
    $0084: Result := 'WM_NCHITTEST';
    $0085: Result := 'WM_NCPAINT';
    $0086: Result := 'WM_NCACTIVATE';
    $0087: Result := 'WM_GETDLGCODE';
    $0088: Result := 'WM_SYNCPAINT';
    $00A0: Result := 'WM_NCMOUSEMOVE';
    $00A1: Result := 'WM_NCLBUTTONDOWN';
    $00A2: Result := 'WM_NCLBUTTONUP';
    $00A3: Result := 'WM_NCLBUTTONDBLCLK';
    $00A4: Result := 'WM_NCRBUTTONDOWN';
    $00A5: Result := 'WM_NCRBUTTONUP';
    $00A6: Result := 'WM_NCRBUTTONDBLCLK';
    $00A7: Result := 'WM_NCMBUTTONDOWN';
    $00A8: Result := 'WM_NCMBUTTONUP';
    $00A9: Result := 'WM_NCMBUTTONDBLCLK';
    // edit control messages start (todo: add more if needed)
    $00B0: Result := 'EM_GETSEL';
    $00B1: Result := 'EM_SETSEL';
    $00B2: Result := 'EM_GETRECT';
    $00B3: Result := 'EM_SETRECT';
    $00B4: Result := 'EM_SETRECTNP';
    $00B5: Result := 'EM_SCROLL';
    $00B6: Result := 'EM_LINESCROLL';
    $00B7: Result := 'EM_SCROLLCARET';
    $00B8: Result := 'EM_GETMODIFY';
    $00B9: Result := 'EM_SETMODIFY';
    $00BA: Result := 'EM_GETLINECOUNT';
    $00BB: Result := 'EM_LINEINDEX';
    $00BC: Result := 'EM_SETHANDLE';
    $00BD: Result := 'EM_GETHANDLE';
    $00BE: Result := 'EM_GETTHUMB';
    $00C1: Result := 'EM_LINELENGTH';
    $00C2: Result := 'EM_REPLACESEL';
    $00C4: Result := 'EM_GETLINE';
    $00C5: Result := 'EM_LIMITTEXT';
    $00C6: Result := 'EM_CANUNDO';
    $00C7: Result := 'EM_UNDO';
    $00C8: Result := 'EM_FMTLINES';
    $00C9: Result := 'EM_LINEFROMCHAR';
    $00CB: Result := 'EM_SETTABSTOPS';
    $00CC: Result := 'EM_SETPASSWORDCHAR';
    $00CD: Result := 'EM_EMPTYUNDOBUFFER';
    $00CE: Result := 'EM_GETFIRSTVISIBLELINE';
    $00CF: Result := 'EM_SETREADONLY';
    $00D0: Result := 'EM_SETWORDBREAKPROC';
    $00D1: Result := 'EM_GETWORDBREAKPROC';
    $00D2: Result := 'EM_GETPASSWORDCHAR';
    $00D3: Result := 'EM_SETMARGINS';
    $00D4: Result := 'EM_GETMARGINS';
    $00D5: Result := 'EM_GETLIMITTEXT';
    $00D6: Result := 'EM_POSFROMCHAR';
    $00D7: Result := 'EM_CHARFROMPOS';
    // edit control messages end
    // scrollbar control messages start
    $00E0: Result := 'SBM_SETPOS';
    $00E1: Result := 'SBM_GETPOS';
    $00E2: Result := 'SBM_SETRANGE';
    $00E3: Result := 'SBM_GETRANGE';
    $00E4: Result := 'SBM_ENABLE_ARROWS';
    $00E6: Result := 'SBM_SETRANGEREDRAW';
    $00E9: Result := 'SBM_SETSCROLLINFO';
    $00EA: Result := 'SBM_GETSCROLLINFO';
    $00EB: Result := 'SBM_GETSCROLLBARINFO';
    // scrollbar control messages end
    // button control messages start
    $00F0: Result := 'BM_GETCHECK';
    $00F1: Result := 'BM_SETCHECK';
    $00F2: Result := 'BM_GETSTATE';
    $00F3: Result := 'BM_SETSTATE';
    $00F4: Result := 'BM_SETSTYLE';
    $00F5: Result := 'BM_CLICK';
    $00F6: Result := 'BM_GETIMAGE';
    $00F7: Result := 'BM_SETIMAGE';
    $00F8: Result := 'BM_SETDONTCLICK';
    // button control messages end
    $0100: Result := 'WM_KEYFIRST or WM_KEYDOWN';
    $0101: Result := 'WM_KEYUP';
    $0102: Result := 'WM_CHAR';
    $0103: Result := 'WM_DEADCHAR';
    $0104: Result := 'WM_SYSKEYDOWN';
    $0105: Result := 'WM_SYSKEYUP';
    $0106: Result := 'WM_SYSCHAR';
    $0107: Result := 'WM_SYSDEADCHAR';
    $0108: Result := 'WM_KEYLAST';
    $010D: Result := 'WM_IME_STARTCOMPOSITION';
    $010E: Result := 'WM_IME_ENDCOMPOSITION';
    $010F: Result := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
    $0110: Result := 'WM_INITDIALOG';
    $0111: Result := 'WM_COMMAND';
    $0112: Result := 'WM_SYSCOMMAND';
    $0113: Result := 'WM_TIMER';
    $0114: Result := 'WM_HSCROLL';
    $0115: Result := 'WM_VSCROLL';
    $0116: Result := 'WM_INITMENU';
    $0117: Result := 'WM_INITMENUPOPUP';
    $011F: Result := 'WM_MENUSELECT';
    $0120: Result := 'WM_MENUCHAR';
    $0121: Result := 'WM_ENTERIDLE';
    $0122: Result := 'WM_MENURBUTTONUP';
    $0123: Result := 'WM_MENUDRAG';
    $0124: Result := 'WM_MENUGETOBJECT';
    $0125: Result := 'WM_UNINITMENUPOPUP';
    $0126: Result := 'WM_MENUCOMMAND';
    $0127: Result := 'WM_CHANGEUISTATE';
    $0128: Result := 'WM_UPDATEUISTATE';
    $0129: Result := 'WM_QUERYUISTATE';
    $0132: Result := 'WM_CTLCOLORMSGBOX';
    $0133: Result := 'WM_CTLCOLOREDIT';
    $0134: Result := 'WM_CTLCOLORLISTBOX';
    $0135: Result := 'WM_CTLCOLORBTN';
    $0136: Result := 'WM_CTLCOLORDLG';
    $0137: Result := 'WM_CTLCOLORSCROLLBAR';
    $0138: Result := 'WM_CTLCOLORSTATIC';
    $0140: Result := 'CB_GETEDITSEL';
    $0141: Result := 'CB_LIMITTEXT';
    $0142: Result := 'CB_SETEDITSEL';
    $0143: Result := 'CB_ADDSTRING';
    $0144: Result := 'CB_DELETESTRING';
    $0145: Result := 'CB_DIR';
    $0146: Result := 'CB_GETCOUNT';
    $0147: Result := 'CB_GETCURSEL';
    $0148: Result := 'CB_GETLBTEXT';
    $0149: Result := 'CB_GETLBTEXTLEN';
    $014A: Result := 'CB_INSERTSTRING';
    $014B: Result := 'CB_RESETCONTENT';
    $014C: Result := 'CB_FINDSTRING';
    $014D: Result := 'CB_SELECTSTRING';
    $014E: Result := 'CB_SETCURSEL';
    $014F: Result := 'CB_SHOWDROPDOWN';
    $0150: Result := 'CB_GETITEMDATA';
    $0151: Result := 'CB_SETITEMDATA';
    $0152: Result := 'CB_GETDROPPEDCONTROLRECT';
    $0153: Result := 'CB_SETITEMHEIGHT';
    $0154: Result := 'CB_GETITEMHEIGHT';
    $0155: Result := 'CB_SETEXTENDEDUI';
    $0156: Result := 'CB_GETEXTENDEDUI';
    $0157: Result := 'CB_GETDROPPEDSTATE';
    $0158: Result := 'CB_FINDSTRINGEXACT';
    $0159: Result := 'CB_SETLOCALE';
    $015A: Result := 'CB_GETLOCALE';
    $015B: Result := 'CB_GETTOPINDEX';
    $015C: Result := 'CB_SETTOPINDEX';
    $015D: Result := 'CB_GETHORIZONTALEXTENT';
    $015E: Result := 'CB_SETHORIZONTALEXTENT';
    $015F: Result := 'CB_GETDROPPEDWIDTH';
    $0160: Result := 'CB_SETDROPPEDWIDTH';
    $0161: Result := 'CB_INITSTORAGE';
    $0163: Result := 'CB_MULTIPLEADDSTRING';
    $0164: Result := 'CB_GETCOMBOBOXINFO';
    $0200: Result := 'WM_MOUSEFIRST or WM_MOUSEMOVE';
    $0201: Result := 'WM_LBUTTONDOWN';
    $0202: Result := 'WM_LBUTTONUP';
    $0203: Result := 'WM_LBUTTONDBLCLK';
    $0204: Result := 'WM_RBUTTONDOWN';
    $0205: Result := 'WM_RBUTTONUP';
    $0206: Result := 'WM_RBUTTONDBLCLK';
    $0207: Result := 'WM_MBUTTONDOWN';
    $0208: Result := 'WM_MBUTTONUP';
    $0209: Result := 'WM_MBUTTONDBLCLK';
    $020A: Result := 'WM_MOUSEWHEEL or WM_MOUSELAST';
    $0210: Result := 'WM_PARENTNOTIFY';
    $0211: Result := 'WM_ENTERMENULOOP';
    $0212: Result := 'WM_EXITMENULOOP';
    $0213: Result := 'WM_NEXTMENU';
    $0214: Result := 'WM_SIZING';
    $0215: Result := 'WM_CAPTURECHANGED';
    $0216: Result := 'WM_MOVING';
    $0218: Result := 'WM_POWERBROADCAST';
    $0219: Result := 'WM_DEVICECHANGE';
    $0220: Result := 'WM_MDICREATE';
    $0221: Result := 'WM_MDIDESTROY';
    $0222: Result := 'WM_MDIACTIVATE';
    $0223: Result := 'WM_MDIRESTORE';
    $0224: Result := 'WM_MDINEXT';
    $0225: Result := 'WM_MDIMAXIMIZE';
    $0226: Result := 'WM_MDITILE';
    $0227: Result := 'WM_MDICASCADE';
    $0228: Result := 'WM_MDIICONARRANGE';
    $0229: Result := 'WM_MDIGETACTIVE';
    $0230: Result := 'WM_MDISETMENU';
    $0231: Result := 'WM_ENTERSIZEMOVE';
    $0232: Result := 'WM_EXITSIZEMOVE';
    $0233: Result := 'WM_DROPFILES';
    $0234: Result := 'WM_MDIREFRESHMENU';
    $0281: Result := 'WM_IME_SETCONTEXT';
    $0282: Result := 'WM_IME_NOTIFY';
    $0283: Result := 'WM_IME_CONTROL';
    $0284: Result := 'WM_IME_COMPOSITIONFULL';
    $0285: Result := 'WM_IME_SELECT';
    $0286: Result := 'WM_IME_CHAR';
    $0288: Result := 'WM_IME_REQUEST';
    $0290: Result := 'WM_IME_KEYDOWN';
    $0291: Result := 'WM_IME_KEYUP';
    $02A1: Result := 'WM_MOUSEHOVER';
    $02A2: Result := 'WM_NCMOUSELEAVE';
    $02A3: Result := 'WM_MOUSELEAVE';
    $0300: Result := 'WM_CUT';
    $0301: Result := 'WM_COPY';
    $0302: Result := 'WM_PASTE';
    $0303: Result := 'WM_CLEAR';
    $0304: Result := 'WM_UNDO';
    $0305: Result := 'WM_RENDERFORMAT';
    $0306: Result := 'WM_RENDERALLFORMATS';
    $0307: Result := 'WM_DESTROYCLIPBOARD';
    $0308: Result := 'WM_DRAWCLIPBOARD';
    $0309: Result := 'WM_PAINTCLIPBOARD';
    $030A: Result := 'WM_VSCROLLCLIPBOARD';
    $030B: Result := 'WM_SIZECLIPBOARD';
    $030C: Result := 'WM_ASKCBFORMATNAME';
    $030D: Result := 'WM_CHANGECBCHAIN';
    $030E: Result := 'WM_HSCROLLCLIPBOARD';
    $030F: Result := 'WM_QUERYNEWPALETTE';
    $0310: Result := 'WM_PALETTEISCHANGING';
    $0311: Result := 'WM_PALETTECHANGED';
    $0312: Result := 'WM_HOTKEY';
    $0317: Result := 'WM_PRINT';
    $0318: Result := 'WM_PRINTCLIENT';
    $031F: Result := 'WM_DWMNCRENDERINGCHANGED';
    $0358: Result := 'WM_HANDHELDFIRST';
    $035F: Result := 'WM_HANDHELDLAST';
    $0380: Result := 'WM_PENWINFIRST';
    $038F: Result := 'WM_PENWINLAST';
    $0390: Result := 'WM_COALESCE_FIRST';
    $039F: Result := 'WM_COALESCE_LAST';
    $03E0: Result := 'WM_DDE_FIRST or WM_DDE_INITIATE';
    $03E1: Result := 'WM_DDE_TERMINATE';
    $03E2: Result := 'WM_DDE_ADVISE';
    $03E3: Result := 'WM_DDE_UNADVISE';
    $03E4: Result := 'WM_DDE_ACK';
    $03E5: Result := 'WM_DDE_DATA';
    $03E6: Result := 'WM_DDE_REQUEST';
    $03E7: Result := 'WM_DDE_POKE';
    $03E8: Result := 'WM_DDE_EXECUTE or WM_DDE_LAST';
    $0400: Result := 'WM_USER';
    // progress bar
    $0401: Result := 'PBM_SETRANGE';
    $0402: Result := 'PBM_SETPOS';
    $0403: Result := 'PBM_DELTAPOS';
    $0404: Result := 'PBM_SETSTEP';
    $0405: Result := 'PBM_STEPIT';
    $0406: Result := 'PBM_SETRANGE32';
    $0407: Result := 'PBM_GETRANGE';
    $0408: Result := 'PBM_GETPOS';
    $0409: Result := 'PBM_SETBARCOLOR';
    $040A: Result := 'PBM_SETMARQUEE';
    $040D: Result := 'PBM_GETSTEP';
    $040E: Result := 'PBM_GETBKCOLOR';
    $040F: Result := 'PBM_GETBARCOLOR';
    $0410: Result := 'PBM_SETSTATE';
    $0411: Result := 'PBM_GETSTATE';
    // misc
    $0469: Result := 'UDM_SETBUDDY';
    $046A: Result := 'UDM_GETBUDDY';
    $102C: Result := 'LVM_GETITEMSTATE';
    $8000: Result := 'WM_APP';
  else
    Result := 'Unknown(' + IntToStr(WM_Message) + ')';
  end; {Case}
end;

function WindowPosFlagsToString(Flags: UINT): string;
var
  FlagsStr: string;
begin
  FlagsStr := '';
  if (Flags and SWP_DRAWFRAME) <> 0 then
    FlagsStr := FlagsStr + '|SWP_DRAWFRAME';
  if (Flags and SWP_HIDEWINDOW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_HIDEWINDOW';
  if (Flags and SWP_NOACTIVATE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOACTIVATE';
  if (Flags and SWP_NOCOPYBITS) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOCOPYBITS';
  if (Flags and SWP_NOMOVE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOMOVE';
  if (Flags and SWP_NOOWNERZORDER) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOOWNERZORDER';
  if (Flags and SWP_NOREDRAW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOREDRAW';
  if (Flags and SWP_NOSENDCHANGING) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOSENDCHANGING';
  if (Flags and SWP_NOSIZE) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOSIZE';
  if (Flags and SWP_NOZORDER) <> 0 then
    FlagsStr := FlagsStr + '|SWP_NOZORDER';
  if (Flags and SWP_SHOWWINDOW) <> 0 then
    FlagsStr := FlagsStr + '|SWP_SHOWWINDOW';
  if Length(FlagsStr) > 0 then
    FlagsStr := Copy(FlagsStr, 2, Length(FlagsStr)-1);
  Result := FlagsStr;
end;

{------------------------------------------------------------------------------
  procedure: GetWin32KeyInfo
  Params:  Event      - Requested info
           KeyCode    - the ASCII key code of the eventkey
           VirtualKey - the virtual key code of the eventkey
           SysKey     - True if the key is a syskey
           ExtEnded   - True if the key is an extended key
           Toggle     - True if the key is a toggle key and its value is on
  Returns: Nothing

  GetWin32KeyInfo returns information about the given key event
 ------------------------------------------------------------------------------}
{
procedure GetWin32KeyInfo(const Event: Integer; var KeyCode, VirtualKey: Integer; var SysKey, Extended, Toggle: Boolean);
const
  MVK_UNIFY_SIDES = 1;
begin
  KeyCode := Word(Event);
  VirtualKey := MapVirtualKey(KeyCode, MVK_UNIFY_SIDES);
  SysKey := (VirtualKey = VK_SHIFT) Or (VirtualKey = VK_CONTROL) Or (VirtualKey = VK_MENU);
  ExtEnded := (SysKey) Or (VirtualKey = VK_INSERT) Or (VirtualKey = VK_HOME) Or (VirtualKey = VK_LEFT) Or (VirtualKey = VK_UP) Or (VirtualKey = VK_RIGHT) Or (VirtualKey = VK_DOWN) Or (VirtualKey = VK_PRIOR) Or (VirtualKey = VK_NEXT) Or (VirtualKey = VK_END) Or (VirtualKey = VK_DIVIDE);
  Toggle := Lo(GetKeyState(VirtualKey)) = 1;
end;
}

{------------------------------------------------------------------------------
  function: ObjectToHWND
  Params: AObject - An LCL Object
  Returns: The Window handle of the given object

  Returns the Window handle of the given object, 0 if no object available
 ------------------------------------------------------------------------------}
function ObjectToHWND(const AObject: TObject): HWND;
begin
  Result := 0;
  if not assigned(AObject) then
    Assert (False, 'TRACE:[ObjectToHWND] Object not assigned')
  else
  if (AObject is TWinControl) then
  begin
    if TWinControl(AObject).HandleAllocated then
      Result := TWinControl(AObject).Handle
  end
  else
  if (AObject is TMenuItem) then
  begin
    if TMenuItem(AObject).HandleAllocated then
      Result := TMenuItem(AObject).Handle
  end
  else
  if (AObject is TMenu) then
  begin
    if TMenu(AObject).HandleAllocated then
      Result := TMenu(AObject).Items.Handle
  end
  else
  if (AObject is TCommonDialog) then
  begin
    {if TCommonDialog(AObject).HandleAllocated then }
    Result := TCommonDialog(AObject).Handle
  end;
end;

(***********************************************************************
  Widget member functions
************************************************************************)

{-------------------------------------------------------------------------------
  function LCLBoundsNeedsUpdate(Sender: TWinControl;
    SendSizeMsgOnDiff: boolean): boolean;

  Returns true if LCL bounds and win32 bounds differ for the control.
-------------------------------------------------------------------------------}
function LCLControlSizeNeedsUpdate(Sender: TWinControl;
  SendSizeMsgOnDiff: boolean): boolean;
var
  LMessage: TLMSize;
  IntfWidth, IntfHeight: integer;
begin
  Result := False;
  LCLIntf.GetWindowSize(Sender.Handle, IntfWidth, IntfHeight);
  if (Sender.Width = IntfWidth) and (Sender.Height = IntfHeight) and (not Sender.ClientRectNeedsInterfaceUpdate) then
    Exit;
  Result := True;
  if SendSizeMsgOnDiff then
  begin
    //writeln('LCLBoundsNeedsUpdate B ',TheWinControl.Name,':',TheWinControl.ClassName,' Sending WM_SIZE');
    Sender.InvalidateClientRectCache(True);
    // send message directly to LCL, some controls not subclassed -> message
    // never reaches LCL
    with LMessage do
    begin
      Msg := LM_SIZE;
      SizeType := SIZE_RESTORED or Size_SourceIsInterface;
      Width := IntfWidth;
      Height := IntfHeight;
    end;
    DeliverMessage(Sender, LMessage);
  end;
end;

{-------------------------------------------------------------------------------
  function GetLCLClientBoundsOffset(Sender: TObject; out ORect: TRect): boolean;

  Returns the difference between the client origin of a win32 handle
  and the definition of the LCL counterpart.
  For example:
    TGroupBox's client area is the area inside the groupbox frame.
    Hence, the LeftOffset is the frame width and the TopOffset is the caption
    height.
  It is used in GetClientBounds to define LCL bounds from win32 bounds.
-------------------------------------------------------------------------------}
function GetLCLClientBoundsOffset(Sender: TObject; out ORect: TRect): boolean;
var
  TM: Windows.TextMetric;
  DC: HDC;
  Handle: HWND;
  TheWinControl: TWinControl absolute Sender;
  Win32Info: PWin32WindowInfo;
begin
  Result := False;
  if not (Sender is TWinControl) then exit;
  if not TheWinControl.HandleAllocated then exit;
  Handle := TheWinControl.Handle;
  FillChar(ORect, SizeOf(ORect), 0);
  if TheWinControl is TScrollingWinControl then
  begin
    {$ifdef RedirectDestroyMessages}
    with TScrollingWinControl(TheWinControl) do
    begin
      OffsetRect(ORect, -HorzScrollBar.Position, -VertScrollBar.Position);
    end;
    {$else}
    with TScrollingWinControl(TheWinControl) do
    begin
      if HorzScrollBar <> nil then
      begin
        // left and right bounds are shifted by scroll position
        ORect.Left := -HorzScrollBar.Position;
        ORect.Right := -HorzScrollBar.Position;
      end;
      if VertScrollBar <> nil then
      begin
        // top and bottom bounds are shifted by scroll position
        ORect.Top := -VertScrollBar.Position;
        ORect.Bottom := -VertScrollBar.Position;
      end;
    end;
    {$endif}
  end else
  if (TheWinControl is TCustomGroupBox) then
  begin
    // The client area of a groupbox under winapi is the whole size, including
    // the frame. The LCL defines the client area without the frame.
    // -> Adjust the position
    // add the upper frame with the caption
    DC := Windows.GetDC(Handle);
    SelectObject(DC, TheWinControl.Font.Reference.Handle);
    Windows.GetTextMetrics(DC, TM);
    ORect.Top := TM.TMHeight + 3;
    Windows.ReleaseDC(Handle, DC);
    // add the left, right and bottom frame borders
    ORect.Left := 2;
    ORect.Right := -2;
    ORect.Bottom := -2;
  end else
  begin
    Win32Info:=GetWin32WindowInfo(Handle);
    if Assigned(Win32Info^.ClientOffsetProc) then
      Win32Info^.ClientOffsetProc(TheWinControl, ORect);
  end;
{
  if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE) <> 0 then
  begin
    Dec(LeftOffset, Windows.GetSystemMetrics(SM_CXEDGE));
    Dec(TopOffset, Windows.GetSystemMetrics(SM_CYEDGE));
  end;
}
  Result := True;
end;

function GetLCLClientBoundsOffset(Handle: HWnd; out Rect: TRect): boolean;
var
  OwnerObject: TObject;
begin
  OwnerObject := GetWin32WindowInfo(Handle)^.WinControl;
  Result := GetLCLClientBoundsOffset(OwnerObject, Rect);
end;

procedure LCLBoundsToWin32Bounds(Sender: TObject; var Left, Top: Integer);
var
  ORect: TRect;
begin
  if (Sender=nil) or (not (Sender is TWinControl)) then exit;
  if not GetLCLClientBoundsOffset(TWinControl(Sender).Parent, ORect) then exit;
  inc(Left, ORect.Left);
  inc(Top, ORect.Top);
end;

procedure Win32PosToLCLPos(Sender: TObject; var Left, Top: SmallInt);
var
  ORect: TRect;
begin
  if (Sender=nil) or (not (Sender is TWinControl)) then exit;
  if not GetLCLClientBoundsOffset(TWinControl(Sender).Parent, ORect) then exit;
  dec(Left, ORect.Left);
  dec(Top, ORect.Top);
end;

procedure GetWin32ControlPos(Window, Parent: HWND; var Left, Top: integer);
var
  parRect, winRect: Windows.TRect;
begin
  Windows.GetWindowRect(Window, winRect);
  Windows.GetWindowRect(Parent, parRect);
  Left := winRect.Left - parRect.Left;
  Top := winRect.Top - parRect.Top;
end;

function GetWin32NativeDoubleBuffered(Sender: TWinControl): boolean;
begin
  // disable auto-DoubleBuffered to allow animations, see #33832
  Result :=
    Sender.DoubleBuffered
    and not(ThemeServices.ThemesEnabled and (Application.DoubleBuffered=adbDefault) and Sender.ParentDoubleBuffered);
end;

{
  Updates the window style of the window indicated by Handle.
  The new style is the Style parameter.
  Only the bits set in the StyleMask are changed,
  the other bits remain untouched.
  if the bits in the StyleMask are not used in the Style,
  there are cleared.
}
procedure UpdateWindowStyle(Handle: HWnd; Style: integer; StyleMask: integer);
var
  CurrentStyle,
  NewStyle : PtrInt;
begin
  CurrentStyle := GetWindowLong(Handle, GWL_STYLE);
  NewStyle := (Style and StyleMask) or (CurrentStyle and (not StyleMask));
  SetWindowLong(Handle, GWL_STYLE, NewStyle);
end;

function AllocWindowInfo(Window: HWND): PWin32WindowInfo;
var
  WindowInfo: PWin32WindowInfo;
begin
  New(WindowInfo);
  FillChar(WindowInfo^, sizeof(WindowInfo^), 0);
  WindowInfo^.DrawItemIndex := -1;
  Windows.SetProp(Window, PTChar(PtrUInt(WindowInfoAtom)), PtrUInt(WindowInfo));
  Result := WindowInfo;
end;

function DisposeWindowInfo(Window: HWND): boolean;
var
  WindowInfo: PWin32WindowInfo;
begin
  WindowInfo := PWin32WindowInfo(Windows.GetProp(Window, PTChar(PtrUInt(WindowInfoAtom))));
  Result := Windows.RemoveProp(Window, PTChar(PtrUInt(WindowInfoAtom))) <> 0;
  if Result then
  begin
    WindowInfo^.StayOnTopList.Free;
    Dispose(WindowInfo);
  end;
end;

function GetWin32WindowInfo(Window: HWND): PWin32WindowInfo;
begin
  Result := PWin32WindowInfo(Windows.GetProp(Window, PTChar(PtrUInt(WindowInfoAtom))));
  if Result = nil then
    Result := @DefaultWindowInfo;
end;

function EnumStayOnTopRemove(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  StayOnTopWindowsInfo: PStayOnTopWindowsInfo absolute Param;
  lWindowInfo: PWin32WindowInfo;
  lWinControl: TWinControl;
begin
  Result := True;
  if ((GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOPMOST) <> 0) then
  begin
    // Don't remove system-wide stay on top, unless desired
    if not StayOnTopWindowsInfo^.SystemTopAlso then
    begin
      lWindowInfo := GetWin32WindowInfo(Handle);
      if Assigned(lWindowInfo) then
      begin
        lWinControl := lWindowInfo^.WinControl;
        if (lWinControl is TCustomForm) and
          (TCustomForm(lWinControl).FormStyle = fsSystemStayOnTop) then
        Exit;
      end;
    end;

    StayOnTopWindowsInfo^.StayOnTopList.Add(Pointer(Handle));
  end;
end;

procedure RemoveStayOnTopFlags(AppHandle: HWND; ASystemTopAlso: Boolean = False);
var
  StayOnTopWindowsInfo: PStayOnTopWindowsInfo;
  WindowInfo: PWin32WindowInfo;
  I: Integer;
begin
  //WriteLn('RemoveStayOnTopFlags ', InRemoveStayOnTopFlags);
  if InRemoveStayOnTopFlags = 0 then
  begin
    New(StayOnTopWindowsInfo);
    StayOnTopWindowsInfo^.AppHandle := AppHandle;
    StayOnTopWindowsInfo^.SystemTopAlso := ASystemTopAlso;
    StayOnTopWindowsInfo^.StayOnTopList := TFPList.Create;
    WindowInfo := GetWin32WindowInfo(AppHandle);
    WindowInfo^.StayOnTopList := StayOnTopWindowsInfo^.StayOnTopList;
    // EnumWindow() suggests the order from top-most to bottom windows
    // EnumThreadWindoes() doesn't suggest the order, but assuming the same.
    // See RestoreStayOnTopFlags
    EnumThreadWindows(GetWindowThreadProcessId(AppHandle, nil),
      @EnumStayOnTopRemove, LPARAM(StayOnTopWindowsInfo));
    for I := 0 to WindowInfo^.StayOnTopList.Count - 1 do
    begin
      // Starting with Windows 7, setting HWND_NOTOPMOST keeps the window
      // on top of another active window. So currently, the code sends
      // the window to the very bottom and then restores it as "_TOP"
      // to prevent the overlapping
      SetWindowPos(HWND(WindowInfo^.StayOnTopList[I]), HWND_BOTTOM, 0, 0, 0, 0,
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_DRAWFRAME);
      SetWindowPos(HWND(WindowInfo^.StayOnTopList[I]), HWND_TOP, 0, 0, 0, 0,
        SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_DRAWFRAME);
    end;
    Dispose(StayOnTopWindowsInfo);
  end;
  inc(InRemoveStayOnTopFlags);
end;

procedure RestoreStayOnTopFlags(AppHandle: HWND);
var
  WindowInfo: PWin32WindowInfo;
  I: integer;
begin
  //WriteLn('RestoreStayOnTopFlags ', InRemoveStayOnTopFlags);
  if InRemoveStayOnTopFlags = 1 then
  begin
    WindowInfo := GetWin32WindowInfo(AppHandle);
    if WindowInfo^.StayOnTopList <> nil then
    begin
      // the order of the list is assumed to be
      // from top to bottom, thus the restoration of the list
      // should be from bottom to top as well
      for I := WindowInfo^.StayOnTopList.Count - 1 downto 0  do
        SetWindowPos(HWND(WindowInfo^.StayOnTopList.Items[I]),
          HWND_TOPMOST, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_DRAWFRAME);
      FreeAndNil(WindowInfo^.StayOnTopList);
    end;
  end;
  if InRemoveStayOnTopFlags > 0 then
    dec(InRemoveStayOnTopFlags);
end;

function EnumHidePopups(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  Owner: HWND;
begin
  Owner := GetWindow(Handle, GW_OWNER);
  if (Owner <> 0) and (Owner <> PPopupOwnersWindowInfo(Param)^.AppHandle) then
    PPopupOwnersWindowInfo(Param)^.OwnersList.Add(Pointer(Owner));
  Result := True;
end;

procedure HidePopups(AppHandle: HWND);
var
  i: Integer;
  Info: PPopupOwnersWindowInfo;
begin
  if not Assigned(PopupOwnersList) then
  begin
    PopupOwnersList := TFPList.Create;
    New(Info);
    try
      Info^.AppHandle := AppHandle;
      Info^.OwnersList := PopupOwnersList;
      EnumThreadWindows(GetWindowThreadProcessId(Application.MainFormHandle, nil),
        @EnumHidePopups, LPARAM(Info));
      for i := 0 to PopupOwnersList.Count - 1 do
        ShowOwnedPopups(HWND(PopupOwnersList[i]), False);
    finally
      Dispose(Info);
    end;
  end;
end;

procedure RestorePopups;
var
  i: Integer;
begin
  if Assigned(PopupOwnersList) then
  begin
    for i := 0 to PopupOwnersList.Count - 1 do
      ShowOwnedPopups(HWND(PopupOwnersList[i]), True);
    FreeAndNil(PopupOwnersList);
  end;
end;

function EnumLookupTopWindow(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
begin
  Result := True;
  if IsWindowVisible(Handle) and IsWindowEnabled(Handle) then
  begin
    with PLookupTopWindowInfo(Param)^ do
    begin
      if (Handle = AppHandle) or (Handle = TopWindow) then
        Exit;
      if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then
      begin
        // we've found the top most window => stop
        TopWindow := Handle;
        Result := False;
      end
      else
      if TopWindow = 0 then
        TopWindow := Handle;
    end;
  end;
end;

function LookupTopWindow(AppHandle: HWND): HWND;
var
  Info: PLookupTopWindowInfo;
begin
  New(Info);
  Info^.AppHandle := AppHandle;
  Info^.TopWindow := 0;
  EnumThreadWindows(GetWindowThreadProcessId(AppHandle, nil),
      @EnumLookupTopWindow, LPARAM(Info));
  Result := Info^.TopWindow;
  if Result = 0 then
    Result := AppHandle;
  Dispose(Info);
end;

{-------------------------------------------------------------------------------
  procedure AddToChangedMenus(Window: HWnd);

  Adds Window to the list of windows which need to redraw the main menu.
-------------------------------------------------------------------------------}
procedure AddToChangedMenus(Window: HWnd);
begin
  if ChangedMenus.IndexOf(Pointer(Window)) = -1 then // Window handle is not yet in the list
    ChangedMenus.Add(Pointer(Window));
end;

{------------------------------------------------------------------------------
  Method: RedrawMenus
  Params:  None
  Returns: Nothing

  Redraws all changed menus
 ------------------------------------------------------------------------------}
procedure RedrawMenus;
var
  I: integer;
begin
  for I := 0 to ChangedMenus.Count - 1 do
    DrawMenuBar(HWND(ChangedMenus[I]));
  ChangedMenus.Clear;
end;

function MeasureTextForWnd(const AWindow: HWND; Text: string; var Width,
  Height: integer): boolean;
var
  textSize: Windows.SIZE;
  canvasHandle: HDC;
  oldFontHandle, newFontHandle: HFONT;
begin
  canvasHandle := Windows.GetDC(AWindow);
  newFontHandle := HFONT(SendMessage(AWindow, WM_GETFONT, 0, 0));
  oldFontHandle := SelectObject(canvasHandle, newFontHandle);
  DeleteAmpersands(Text);

  Result := LCLIntf.GetTextExtentPoint32(canvasHandle, PChar(Text), Length(Text), textSize);

  if Result then
  begin
    Width := textSize.cx;
    Height := textSize.cy;
  end;
  SelectObject(canvasHandle, oldFontHandle);
  Windows.ReleaseDC(AWindow, canvasHandle);
end;

function MeasureText(const AWinControl: TWinControl; Text: string; var Width, Height: integer): boolean;
begin
  Result := MeasureTextForWnd(AWinControl.Handle, Text, Width, Height);
end;

function GetControlText(AHandle: HWND): string;
var
  TextLen: longint;
  WideBuffer: WideString;
begin
  TextLen := Windows.GetWindowTextLengthW(AHandle);
  SetLength(WideBuffer, TextLen);
  if TextLen > 0 // Never give Windows the chance to write to System.emptychar
  then TextLen := Windows.GetWindowTextW(AHandle, PWideChar(WideBuffer), TextLen + 1);
  SetLength(WideBuffer, TextLen);
  Result := UTF16ToUTF8(WideBuffer);
end;

procedure FillRawImageDescriptionColors(var ADesc: TRawImageDescription);
begin
  case ADesc.BitsPerPixel of
    1,4,8:
      begin
        // palette mode, no offsets
        ADesc.Format := ricfGray;
        ADesc.RedPrec := ADesc.BitsPerPixel;
        ADesc.GreenPrec := 0;
        ADesc.BluePrec := 0;
        ADesc.RedShift := 0;
        ADesc.GreenShift := 0;
        ADesc.BlueShift := 0;
      end;
    16:
      begin
        // 5-5-5 mode
        ADesc.RedPrec := 5;
        ADesc.GreenPrec := 5;
        ADesc.BluePrec := 5;
        ADesc.RedShift := 10;
        ADesc.GreenShift := 5;
        ADesc.BlueShift := 0;
        ADesc.Depth := 15;
      end;
    24:
      begin
        // 8-8-8 mode
        ADesc.RedPrec := 8;
        ADesc.GreenPrec := 8;
        ADesc.BluePrec := 8;
        ADesc.RedShift := 16;
        ADesc.GreenShift := 8;
        ADesc.BlueShift := 0;
      end;
  else    //  32:
    // 8-8-8-8 mode, high byte can be native alpha or custom 1bit maskalpha
    ADesc.AlphaPrec := 8;
    ADesc.RedPrec := 8;
    ADesc.GreenPrec := 8;
    ADesc.BluePrec := 8;
    ADesc.AlphaShift := 24;
    ADesc.RedShift := 16;
    ADesc.GreenShift := 8;
    ADesc.BlueShift := 0;
    ADesc.Depth := 32;
  end;
end;

procedure FillRawImageDescription(const ABitmapInfo: Windows.TBitmap; out ADesc: TRawImageDescription);
begin
  ADesc.Format := ricfRGBA;

  ADesc.Depth := ABitmapInfo.bmBitsPixel;             // used bits per pixel
  ADesc.Width := ABitmapInfo.bmWidth;
  ADesc.Height := ABitmapInfo.bmHeight;
  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboLSBFirst;
  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := ABitmapInfo.bmBitsPixel;      // bits per pixel. can be greater than Depth.
  ADesc.LineEnd := rileDWordBoundary;

  if ABitmapInfo.bmBitsPixel <= 8
  then begin
    // each pixel is an index in the palette
    // TODO, ColorCount
    ADesc.PaletteColorCount := 0;
  end
  else ADesc.PaletteColorCount := 0;


  FillRawImageDescriptionColors(ADesc);

  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskShift := 0;
  ADesc.MaskLineEnd := rileWordBoundary; // CreateBitmap requires word boundary
  ADesc.MaskBitOrder := riboReversedBits;
end;

function GetBitmapOrder(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP): TRawImageLineOrder;
  procedure DbgLog(const AFunc: String);
  begin
    DebugLn('GetBitmapOrder - GetDIBits ', AFunc, ' failed: ', GetLastErrorText(Windows.GetLastError));
  end;

var
  SrcPixel: PCardinal absolute AWinBmp.bmBits;
  OrgPixel, TstPixel: Cardinal;
  Scanline: Pointer;
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of Cardinal; // reserve extra color for colormasks
  end;
  
  FullScanLine: Boolean; // win9x requires a full scanline to be retrieved
                         // others won't fail when one pixel is requested
begin
  if AWinBmp.bmBits = nil
  then begin
    // no DIBsection so always bottom-up
    Exit(riloBottomToTop);
  end;

  // try to figure out the orientation of the given bitmap.
  // Unfortunately MS doesn't provide a direct function for this.
  // So modify the first pixel to see if it changes. This pixel is always part
  // of the first scanline of the given bitmap.
  // When we request the data through GetDIBits as bottom-up, windows adjusts
  // the data when it is a top-down. So if the pixel doesn't change the bitmap
  // was internally a top-down image.
  
  FullScanLine := Win32Platform = VER_PLATFORM_WIN32_WINDOWS;
  if FullScanLine
  then ScanLine := GetMem(AWinBmp.bmWidthBytes)
  else ScanLine := nil;

  FillChar(Info.Header, sizeof(Windows.TBitmapInfoHeader), 0);
  Info.Header.biSize := sizeof(Windows.TBitmapInfoHeader);
  DC := Windows.GetDC(0);
  if Windows.GetDIBits(DC, ABitmap, 0, 1, nil, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
  then begin
    DbgLog('Getinfo');
    // failed ???
    Windows.ReleaseDC(0, DC);
    Exit(riloBottomToTop);
  end;

  // Get only 1 pixel (or full scanline for win9x)
  OrgPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel')
    else OrgPixel := PCardinal(ScanLine)^;
  end
  else begin
    Info.Header.biWidth := 1;
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @OrgPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('OrgPixel');
  end;

  // modify pixel
  SrcPixel^ := not SrcPixel^;
  
  // get test
  TstPixel := 0;
  if FullScanLine
  then begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, ScanLine, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel')
    else TstPixel := PCardinal(ScanLine)^;
  end
  else begin
    if Windows.GetDIBits(DC, ABitmap, 0, 1, @TstPixel, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) = 0
    then DbgLog('TstPixel');
  end;

  if OrgPixel = TstPixel
  then Result := riloTopToBottom
  else Result := riloBottomToTop;

  // restore pixel & cleanup
  SrcPixel^ := not SrcPixel^;
  Windows.ReleaseDC(0, DC);
  if FullScanLine
  then FreeMem(Scanline);
end;

function GetBitmapBytes(AWinBmp: Windows.TBitmap; ABitmap: HBITMAP; const ARect: TRect; ALineEnd: TRawImageLineEnd; ALineOrder: TRawImageLineOrder; out AData: Pointer; out ADataSize: PtrUInt): Boolean;
var
  DC: HDC;
  Info: record
    Header: Windows.TBitmapInfoHeader;
    Colors: array[Byte] of TRGBQuad; // reserve extra colors for palette (256 max)
  end;
  H: Cardinal;
  R: TRect;
  SrcData: PByte;
  SrcSize: PtrUInt;
  SrcLineBytes: Cardinal;
  SrcLineOrder: TRawImageLineOrder;
  StartScan: Integer;
begin
  SrcLineOrder := GetBitmapOrder(AWinBmp, ABitmap);
  SrcLineBytes := (AWinBmp.bmWidthBytes + 3) and not 3;

  if AWinBmp.bmBits <> nil
  then begin
    // this is bitmapsection data :) we can just copy the bits

    // We cannot trust windows with bmWidthBytes. Use SrcLineBytes which takes
    // DWORD alignment into consideration
    with AWinBmp do
      Result := CopyImageData(bmWidth, bmHeight, SrcLineBytes, bmBitsPixel, bmBits, ARect, SrcLineOrder, ALineOrder, ALineEnd, AData, ADataSize);
    Exit;
  end;

  // retrieve the data though GetDIBits

  // initialize bitmapinfo structure
  Info.Header.biSize := sizeof(Info.Header);
  Info.Header.biPlanes := 1;
  Info.Header.biBitCount := AWinBmp.bmBitsPixel;
  Info.Header.biCompression := BI_RGB;
  Info.Header.biSizeImage := 0;

  Info.Header.biWidth := AWinBmp.bmWidth;
  H := ARect.Bottom - ARect.Top;
  // request a top-down DIB
  if AWinBmp.bmHeight > 0
  then begin
    Info.Header.biHeight := -AWinBmp.bmHeight;
    StartScan := AWinBmp.bmHeight - ARect.Bottom;
  end
  else begin
    Info.Header.biHeight := AWinBmp.bmHeight;
    StartScan := ARect.Top;
  end;
  // adjust height
  if StartScan < 0
  then begin
    Inc(H, StartScan);
    StartScan := 0;
  end;

  // alloc buffer
  SrcSize := SrcLineBytes * H;
  GetMem(SrcData, SrcSize);

  DC := Windows.GetDC(0);
  Result := Windows.GetDIBits(DC, ABitmap, StartScan, H, SrcData, Windows.PBitmapInfo(@Info)^, DIB_RGB_COLORS) <> 0;
  Windows.ReleaseDC(0, DC);
  
  // since we only got the needed scanlines, adjust top and bottom
  R.Left := ARect.Left;
  R.Top := 0;
  R.Right := ARect.Right;
  R.Bottom := H;

  with Info.Header do
    Result := Result and CopyImageData(biWidth, H, SrcLineBytes, biBitCount, SrcData, R, riloTopToBottom, ALineOrder, ALineEnd, AData, ADataSize);

  FreeMem(SrcData);
end;

function IsAlphaBitmap(ABitmap: HBITMAP): Boolean;
var
  Info: Windows.BITMAP;
begin
  FillChar(Info, SizeOf(Info), 0);
  Result := (GetObject(ABitmap, SizeOf(Info), @Info) <> 0)
        and (Info.bmBitsPixel = 32);
end;

function IsAlphaDC(ADC: HDC): Boolean;
begin
  Result := (GetObjectType(ADC) = OBJ_MEMDC)
        and IsAlphaBitmap(GetCurrentObject(ADC, OBJ_BITMAP));
end;

procedure BlendRect(ADC: HDC; const ARect: TRect; Color: ColorRef);
var
  bmp, oldBmp: HBitmap;
  MemDC: HDC;
  Blend: TBlendFunction;
  Pixel: TRGBAQuad;
  Brush: HBrush;
begin
  if IsRectEmpty(ARect) then Exit;

  Pixel.Blue := Color shr 16;
  Pixel.Green := Color shr 8;
  Pixel.Red := Color;

  bmp := CreateBitmap(1, 1, 1, 32, @Pixel);
  MemDC := CreateCompatibleDC(ADC);
  OldBmp := SelectObject(MemDC, Bmp);

  Blend.BlendOp := AC_SRC_OVER;
  Blend.BlendFlags := 0;
  Blend.SourceConstantAlpha := 128;
  Blend.AlphaFormat := 0;

  AlphaBlend(ADC, ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top, MemDC, 0, 0, 1, 1, Blend);

  SelectObject(MemDC, OldBmp);
  DeleteDC(MemDC);
  DeleteObject(Bmp);

  Brush := CreateSolidBrush(Color);
  FrameRect(ADC, ARect, Brush);
  DeleteObject(Brush);
end;

function GetLastErrorText(AErrorCode: Cardinal): String;
var
  r: cardinal;
  tmp: PChar;
begin
  tmp := nil;
  r := Windows.FormatMessage(
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, AErrorCode, LANG_NEUTRAL, @tmp, 0, nil);

  if r = 0 then Exit('');

  Result := tmp;
  SetLength(Result, Length(Result)-2);

  if tmp <> nil
  then LocalFree(HLOCAL(tmp));
end;

(*
   BitmapToRegion :	Create a region from the "non-transparent" pixels of a bitma
   Author :		Jean-Edouard Lachand-Robert (http://www.geocities.com/Paris/LeftBank/1160/resume.htm), June 1998

   hBmp :               Source bitmap
   cTransparentColor :	Color base for the "transparent" pixels (default is black)
   cTolerance :		Color tolerance for the "transparent" pixels

   A pixel is assumed to be transparent if the value of each of its 3 components (blue, green and red) is
   greater or equal to the corresponding value in cTransparentColor and is lower or equal to the
   corresponding value in cTransparentColor + cTolerance
*)

function BitmapToRegion(hBmp: HBITMAP; cTransparentColor: COLORREF = 0; cTolerance: COLORREF  = $101010): HRGN;

const
  ALLOC_UNIT = 100;

var
  AWidth, AHeight: Integer;

  maxRects: DWORD;
  hData: THANDLE;
  pData: PRGNDATA;
  lr, lg, lb, hr, hg, hb: Byte;
  x, y, x0: Integer;
  pr: PRect;
  h: HRGN;

  WinBmp: Windows.TBitmap;
  P, Data: PRGBAQuad;
  RS: PtrUInt;
  ARawImage, DstRawImage: TRawImage;
  SourceImage, DestImage: TLazIntfImage;

  procedure FillDescription(out ADesc: TRawImageDescription);
  begin
    ADesc.Init;
    ADesc.Format := ricfRGBA;
    ADesc.PaletteColorCount := 0;
    ADesc.MaskBitsPerPixel := 0;
    ADesc.Depth := 32;
    ADesc.Width := AWidth;
    ADesc.Height := AHeight;
    ADesc.BitOrder := riboBitsInOrder;
    ADesc.ByteOrder := riboMSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.BitsPerPixel := 32;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.RedPrec := 8; // red precision. bits for red
    ADesc.RedShift := 8;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 16;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 24;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 0;
  end;
begin
  Result := 0;
  
  if Windows.GetObject(hBmp, sizeof(WinBmp), @WinBmp) = 0 then
    Exit;
    
  if not RawImage_FromBitmap(ARawImage, hBmp, 0) then
    Exit;
    
  AWidth := ARawImage.Description.Width;
  AHeight := ARawImage.Description.Height;

  SourceImage := TLazIntfImage.Create(ARawImage, True);

  DstRawImage.Init;
  FillDescription(DstRawImage.Description);
  DstRawImage.DataSize := AWidth * AHeight * SizeOf(TRGBAQuad);
  Data := AllocMem(DstRawImage.DataSize);
  DstRawImage.Data := PByte(Data);

  DestImage := TLazIntfImage.Create(DstRawImage, False);
  DestImage.CopyPixels(SourceImage);
  SourceImage.Free;
  DestImage.Free;

  RS := GetBytesPerLine(AWidth, 32, rileDWordBoundary);

  // For better performances, we will use the ExtCreateRegion() function to create the
  // region. This function take a RGNDATA structure on entry. We will add rectangles by
  // amount of ALLOC_UNIT number in this structure
  maxRects := ALLOC_UNIT;
  hData := GlobalAlloc(GMEM_MOVEABLE, sizeof(RGNDATAHEADER) + (sizeof(TRECT) * maxRects));
  if hData = 0 then
  begin
    FreeMem(Data);
    Exit;
  end;
  pData := GlobalLock(hData);
  pData^.rdh.dwSize := sizeof(RGNDATAHEADER);
  pData^.rdh.iType := RDH_RECTANGLES;
  pData^.rdh.nCount := 0;
  pData^.rdh.nRgnSize := 0;
  Windows.SetRect(pData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);

  // Keep on hand highest and lowest values for the "transparent" pixel
  lr := GetRValue(cTransparentColor);
  lg := GetGValue(cTransparentColor);
  lb := GetBValue(cTransparentColor);
  hr := min($ff, lr + GetRValue(cTolerance));
  hg := min($ff, lg + GetGValue(cTolerance));
  hb := min($ff, lb + GetBValue(cTolerance));
  
  P := Data;

  // Scan each bitmap row from bottom to top (the bitmap is inverted vertically)
  for y := 0 to AHeight - 1 do
  begin
    // Scan each bitmap pixel from left to righ
    x := 0;
    while (x < AWidth) do
    begin
      // Search for a continuous range of "non transparent pixels"
      x0 := x;
      while (x < AWidth) do
      begin
        with P[x] do
          if (Red >= lr) and (Red <= hr) then
          begin
            if (Green >= lg) and (Green <= hg) then
            begin
              if (Blue >= lb) and (Blue <= hb) then
                break; //This pixel is "transparent"
            end;
          end;
        inc(x);
      end;
      
      if (x > x0) then
      begin
        // Add the pixels (x0, y) to (x, y+1) as a new rectangle in the region
        if (pData^.rdh.nCount >= maxRects) then
        begin
          GlobalUnlock(hData);
          maxRects := maxRects + ALLOC_UNIT;
          hData := GlobalReAlloc(hData, sizeof(RGNDATAHEADER) + (sizeof(TRECT) * maxRects), GMEM_MOVEABLE);
          pData := GlobalLock(hData);
        end;
        pr := PRect(PChar(pData^.Buffer));
        SetRect(pr[pData^.rdh.nCount], x0, y, x, y+1);
        if (x0 < pData^.rdh.rcBound.left) then
          pData^.rdh.rcBound.left := x0;
        if (y < pData^.rdh.rcBound.top) then
          pData^.rdh.rcBound.top := y;
        if (x > pData^.rdh.rcBound.right) then
          pData^.rdh.rcBound.right := x;
        if (y+1 > pData^.rdh.rcBound.bottom) then
        pData^.rdh.rcBound.bottom := y+1;
        inc(pData^.rdh.nCount);

        // On Windows98, ExtCreateRegion() may fail if the number of rectangles is to
        // large (ie: > 4000). Therefore, we have to create the region by multiple steps
        if (pData^.rdh.nCount = 2000) then
        begin
          h := Windows.ExtCreateRegion(nil, sizeof(RGNDATAHEADER) + (sizeof(TRECT) * maxRects), pData^);
          if (Result <> 0) then
          begin
            Windows.CombineRgn(Result, Result, h, RGN_OR);
            Windows.DeleteObject(h);
          end
          else
            Result := h;

          pData^.rdh.nCount := 0;
          Windows.SetRect(pData^.rdh.rcBound, MAXLONG, MAXLONG, 0, 0);
        end;
      end;
      inc(x);
    end;
    // Go to next row (remember, the bitmap is inverted vertically
    P := PRGBAQuad(PByte(P) + RS);
  end;
  // Create or extend the region with the remaining rectangle
  h := Windows.ExtCreateRegion(nil, sizeof(RGNDATAHEADER) + (sizeof(TRECT) * maxRects), pData^);
  if (Result <> 0) then
  begin
    Windows.CombineRgn(Result, Result, h, RGN_OR);
    Windows.DeleteObject(h);
  end
  else
    Result := h;

  FreeMem(Data);
end;

{ Exactly equal to StrLCopy but for PWideChars
  Copyes a widestring up to a maximal length, in WideChars }
function WideStrLCopy(dest, source: PWideChar; maxlen: SizeInt): PWideChar;
var
  counter: SizeInt;
begin
  counter := 0;

  while (Source[counter] <> #0)  and (counter < MaxLen) do
  begin
    Dest[counter] := Source[counter];
    Inc(counter);
  end;

  { terminate the string }
  Dest[counter] := #0;
  Result := Dest;
end;

function WndClassName(Wnd: HWND): String; inline;
var
  winClassName: array[0..19] of char;
begin
  GetClassName(Wnd, @winClassName, 20);
  Result := winClassName;
end;

function WndText(Wnd: HWND): String; inline;
var
  winText: array[0..255] of char;
begin
  GetWindowText(Wnd, @winText, 256);
  Result := winText;
end;


procedure UpdateWindowsVersion;
begin
  case Win32MajorVersion of
    0..3:;
    4: begin
     if Win32Platform = VER_PLATFORM_WIN32_NT
     then WindowsVersion := wvNT4
     else
       case Win32MinorVersion of
         10: WindowsVersion := wv98;
         90: WindowsVersion := wvME;
       else
         WindowsVersion :=wv95;
       end;
    end;
    5: begin
     case Win32MinorVersion of
       0: WindowsVersion := wv2000;
       1: WindowsVersion := wvXP;
     else
       // XP64 has also a 5.2 version
       // we could detect that based on arch and versioninfo.Producttype
       WindowsVersion := wvServer2003;
     end;
    end;
    6: begin
     case Win32MinorVersion of
       0: WindowsVersion := wvVista;
       1: WindowsVersion := wv7;
       2: WindowsVersion := wv8;
     else
       WindowsVersion := wv8_1;
     end;
    end;
    10: begin
     case Win32MinorVersion of
       0: WindowsVersion := wv10;
     else
       WindowsVersion := wvLater;
     end;
    end;
  else
    WindowsVersion := wvLater;
  end;
end;

procedure DoInitialization;
begin
  FillChar(DefaultWindowInfo, sizeof(DefaultWindowInfo), 0);
  DefaultWindowInfo.DrawItemIndex := -1;
  WindowInfoAtom := Windows.GlobalAddAtom('WindowInfo');
  ChangedMenus := TFPList.Create;
  if WindowsVersion = wvUnknown then
    UpdateWindowsVersion;
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

initialization
  DoInitialization;

finalization
  Windows.GlobalDeleteAtom(WindowInfoAtom);
  WindowInfoAtom := 0;
  ChangedMenus.Free;
  FreeAndNil(PopupOwnersList);

end.
