(******************************************************************************
  All interface communication related stuff goes here.
  This file is used by both LCLLinux and InterfaceBase
  If a procedure is platform dependent then it should have the following tail:
    {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
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

procedure AttachMenuToWindow(AMenuObject: TComponent); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

procedure CallDefaultWndHandler(Sender: TObject; var Message); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ComboBoxDropDown(Handle: HWND; DropDown: boolean): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateBitmapFromRawImage(const RawImage: TRawImage; var Bitmap, MaskBitmap: HBitmap): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateEmptyRegion: hRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRegionCopy(SrcRGN: hRGN): hRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function DCClipRegionValid(DC: HDC): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GetBitmapRawImageDescription(Bitmap: HBITMAP; Desc: PRawImageDescription): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetClientBounds(handle : HWND; var ARect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetCmdLineParamDescForInterface: string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDesignerDC(WindowHandle: HWND): HDC; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceRawImageDescription(DC: HDC; Desc: PRawImageDescription): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDeviceSize(DC: HDC; var p: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetListBoxItemRect(ListBox: TComponent; Index: integer; var ARect: TRect): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetNotebookTabIndexAtPos(Handle: HWND; const ClientPos: TPoint): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetRawImageFromDevice(SrcDC: HDC; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetRawImageFromBitmap(SrcBitmap, SrcMaskBitmap: HBITMAP; const SrcRect: TRect; var NewRawImage: TRawImage): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetWindowRelativePosition(Handle : hwnd; var Left, Top: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function Frame(DC: HDC; const ARect: TRect): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TGraphicsBevelCut): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function LoadStockPixmap(StockID: longint) : HBitmap; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function MenuItemSetCheck(AMenuItem: TComponent): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MenuItemSetEnable(AMenuItem: TComponent): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function PairSplitterAddSide(SplitterHandle, SideHandle: hWnd; Side: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PairSplitterGetInterfaceInfo: boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PairSplitterRemoveSide(SplitterHandle, SideHandle: hWnd; Side: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PairSplitterSetPosition(SplitterHandle: hWnd; var NewPosition: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function RadialArc(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialChord(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialPie(DC: HDC; x,y,width,height,sx,sy,ex,ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadialPieWithAngles(DC: HDC; X,Y,Width,Height,Angle1,Angle2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RadioMenuItemGroup(hndMenu: HMENU; bRadio: Boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RightJustifyMenuItem(HndMenu: HMenu; bRightJustify: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

procedure SendCachedLCLMessages; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure StatusBarPanelUpdate(StatusBar: TObject; PanelIndex: integer); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure StatusBarSetText(StatusBar: TObject; PanelIndex: integer); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure StatusBarUpdate(StatusBar: TObject); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}


//##apiwiz##eps##    // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}

{$IFNDEF IF_BASE_MEMBER}

//##apiwiz##spi##    // Do not remove

function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer;

procedure NotifyUser(const DialogMessage : String; DialogType : longint);
procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);
procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);

function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;

// should go to sysutils.pp
procedure RaiseLastOSError;

//##apiwiz##epi##    // Do not remove

{$ENDIF}

{ =============================================================================

  $Log$
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
