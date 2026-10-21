{
 win32richmemo.pas 
 
 Author: Dmitry 'skalogryz' Boyarintsev 

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

unit Win32RichMemo;

{$mode objfpc}{$H+}
{$OBJECTCHECKS OFF}

interface

uses
  // Win32 headers  
  Windows, RichEdit, ActiveX,
  // RTL headers
  Classes, SysUtils, 
  // LCL headers
  LCLType, LCLIntf, LCLProc, WSLCLClasses, LMessages, LCLMessageGlue,
  Graphics, Controls, StdCtrls, Printers, Themes,
  // Win32WidgetSet
  Win32WSControls, Win32Int, Win32WSStdCtrls, win32proc,
  // RichMemo headers
  RichMemo, WSRichMemo, Win32RichMemoProc, Win32RichMemoOle;

type

  { TWin32RichMemoStrings }

  TWin32RichMemoStrings = class(TWin32MemoStrings)
  protected
    fUpd    : Boolean;
    fHandle : HWND;
    procedure SetUpdateState(Updating: Boolean); override;
    function GetTextStr: string; override;
  public
    constructor Create(AHandle: HWND; TheOwner: TWinControl);
  end;

  { TWin32WSCustomRichMemo }

  TWin32WSCustomRichMemo = class(TWSCustomRichMemo)
  published
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class function GetStrings(const ACustomMemo: TCustomMemo): TStrings; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
  
    class procedure SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer); override;
    class procedure SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;
    
    class procedure CutToClipboard(const AWinControl: TWinControl); override;
    class procedure CopyToClipboard(const AWinControl: TWinControl); override;
    class procedure PasteFromClipboard(const AWinControl: TWinControl); override;
    class function CanPasteFromClipboard(Const AWinControl: TWinControl): boolean; override;

    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class function GetTextAttributes(const AWinControl: TWinControl; TextStart: Integer;
      var Params: TIntFontParams): Boolean; override;

    class function isInternalChange(const AWinControl: TWinControl; Params: TTextModifyMask): Boolean; override;
    class procedure SetTextAttributesInternal(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AModifyMask: TTextModifyMask; const Params: TIntFontParams); override;

    class procedure SetTextAttributes(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const Params: TIntFontParams); override;
    class procedure SetHideSelection(const ACustomEdit: TCustomEdit; AHideSelection: Boolean); override;
    class procedure SetMaxLength(const ACustomEdit: TCustomEdit; NewLength: integer); override;

    class function GetStyleRange(const AWinControl: TWinControl; TextStart: Integer; var RangeStart, RangeLen: Integer): Boolean; override;

    class procedure SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ui: TTextUIParam); override;
    class function GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer;
      var ui: TTextUIParam): Boolean; override;

    class function LoadRichText(const AWinControl: TWinControl; Source: TStream): Boolean; override;
    class function SaveRichText(const AWinControl: TWinControl; Dst: TStream): Boolean; override;

    class function GetParaAlignment(const AWinControl: TWinControl; TextStart: Integer;
      var AAlign: TParaAlignment): Boolean; override;
    class procedure SetParaAlignment(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AAlign: TIntParaAlignment); override;

    class function GetParaMetric(const AWinControl: TWinControl; TextStart: Integer;
      var AMetrics: TIntParaMetric): Boolean; override;
    class procedure SetParaMetric(const AWinControl: TWinControl; TextStart, TextLength: Integer;
      const AMetrics: TIntParaMetric); override;
    class function GetParaRange(const AWinControl: TWinControl; TextStart: Integer;
      var ParaRange: TParaRange): Boolean; override;

    class function GetParaNumbering(const AWinControl: TWinControl; TextStart: Integer;
      var ANumber: TIntParaNumbering): Boolean; override;
    class procedure SetParaNumbering(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ANumber: TIntParaNumbering); override;

    class procedure SetParaTabs(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const AStopList: TTabStopList); override;
    class function GetParaTabs(const AWinControl: TWinControl; TextStart: integer;
      var AStopList: TTabStopList): Boolean; override;

    class procedure InDelText(const AWinControl: TWinControl; const TextUTF8: String; DstStart, DstLen: Integer); override;
    class function GetSubText(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      AsUnicode: Boolean; var isUnicode: Boolean; var txt: string; var utxt: UnicodeString): Boolean; override;
    class function CharAtPos(const AWinControl: TWinControl; x,y: Integer): Integer; override;

    class function Search(const AWinControl: TWinControl; const ANiddle: string;
      const SearchOpts: TIntSearchOpt): Integer; override;
    class function isSearchEx: Boolean; override;
    class function SearchEx(const AWinControl: TWinControl; const ANiddle: string;
      const SearchOpts: TIntSearchOpt; var ATextStart, ATextLength: Integer ): Boolean; override;

    class procedure SetZoomFactor(const AWinControl: TWinControl; AZoomFactor: Double); override;
    class function GetZoomFactor(const AWinControl: TWinControl; var AZoomFactor: Double): Boolean; override;

    class function InlineInsert(const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
      const ASize: TSize; AHandler: TRichMemoInline; var wsObj: TRichMemoInlineWSObject): Boolean; override;
    class procedure InlineInvalidate(const AWinControl: TWinControl;
       AHandler: TRichMemoInline; wsObj: TRichMemoInlineWSObject); override;

    class function Print(const AWinControl: TWinControl; APrinter: TPrinter;
      const AParams: TPrintParams; DoPrint: Boolean): Integer; override;

    class procedure Redo(const AWinControl: TWinControl); override;
    class function GetCanRedo(const AWinControl: TWinControl): Boolean; override;

    class procedure ScrollBy(const AWinControl: TWinControl;  DeltaX, DeltaY: integer); override;

    class procedure SetTransparentBackground(const AWinControl: TWinControl; ATransparent: Boolean); override;
  end;

  { TWin32Inline }

  TWin32Inline = class(TCustomDataViewObject, IOleObject, IDataObject, IViewObject)
  public
    richMemo : TCustomRichMemo;
    canvas   : TCanvas;
    rminline : TRichMemoInline;
    isvis    : Boolean;
    function Draw(dwDrawAspect:DWord;LIndex:Long;pvaspect:pointer;ptd:PDVTARGETDEVICE;hdcTargetDev:HDC; hdcDraw:HDC;lprcBounds:PRECTL;lprcWBounds:PRECTL;pfncontinue:TContinueCallback;dwcontinue:ULONG_PTR):HResult; stdcall;
    function GetExtent(dwDrawAspect: DWORD; out size: TPoint): HResult;StdCall;
    function Close(dwSaveOption: DWORD): HResult;StdCall;
    destructor Destroy; override;
  end;

var
  // whenever print range is used - insert an additional line break, so EN_FORMATRANGE
  // doesn't overprint the selected text (until the end of the line).
  // No info is found online, about the bug
  FixPrintSelRange : Boolean = true;

type
  // the function is called during WM_NCPAINT message handling
  // Handled must be set to "true" to prevent Windows default handling of the message
  // if set to true, the resulting value of the function would be used as result for message handler
  TNCPaintProc = function (AHandle: Windows.HANDLE; RichMemo: TCustomRichMemo; WParam: WParam; LParam: LParam; var Handled: Boolean): LResult;

var
  // the value can be set to nil to use system-native drawing only.
  // or set it to whatever function desired
  NCPaint : TNCPaintProc = nil;
  AllocOLEObject : procedure (ARichMemo: TCustomRichMemo; AHandle: Windows.THandle; out OleCallback: IRichEditOleCallback);
  InsertInlineFlags : Integer = REO_OWNERDRAWSELECT;
  RichEditClass   : String = ''; // manually assigned by RichMemo user

function GetSelRTF(amemo: TCustomRichMemo): string;
function GetRichEditOLE(amemo: TCustomRichMemo): IRichEditOle; overload;
function GetRichEditOLE(AHandle: THandle): IRichEditOle; overload;
function GetOleObject(ole: IRichEditOle; SelStart: Integer; out res: TREOBJECT): Boolean;
function SetOleObjectSize(ARichMemo: TCustomRichMemo; SelStart: Integer; const ASize: TSize): Boolean;
function GetOleObjectSize(ARichMemo: TCustomRichMemo; SelStart: Integer; var ASize: TSize): Boolean;

(*type
  TWinLangOptions = set of (
     wloAutokeyboard             // IMF_AUTOKEYBOARD        = $0001
     , wloAutoFont               // IMF_AUTOFONT            = $0002
     , wloImeCancelComplete      // IMF_IMECANCELCOMPLETE   = $0004	// High completes comp string when aborting, low cancels
     , wloAlwaysSendNotify       // IMF_IMEALWAYSSENDNOTIFY = $0008
     , wloAutoFontSizeAdjust     // IMF_AUTOFONTSIZEADJUST  = $0010
     , wloUOFonts                // IMF_UIFONTS             = $0020
     , wloDualFont               // IMF_DUALFONT	          = $0080
  );

function GetLangOptions(hnd: THandle):  TWinLangOptions; overload;
function GetLangOptions(rm: TCustomRichMemo):  TWinLangOptions; overload;
procedure SetLangOptions(hnd: THandle; const opts: TWinLangOptions); overload;
procedure SetLangOptions(rm: TCustomRichMemo; const opts: TWinLangOptions); overload;
*)

procedure WMPrintRichMemo(const AWinControl: TWinControl; ADC: HDC);

implementation

const
  PointSize     = 72.0;
  RtfSizeToInch = 2.54 * 1000.0;
  SizeFactor    = 1 / PointSize * RtfSizeToInch; // pt to 0.01 mlmete
  RevSizeFactor = 1 / SizeFactor;

type
  TIntCustomRichMemo = class(TCustomRichMemo);

const
  AlignmentToEditFlags: array[TAlignment] of DWord =
  (
{ taLeftJustify  } ES_LEFT,
{ taRightJustify } ES_RIGHT,
{ taCenter       } ES_CENTER
  );

const
  ScrollStyleToEditFlags : array [TScrollStyle] of LongWord = (
      {ssNone}           0
    , {ssHorizontal}     WS_HSCROLL or ES_DISABLENOSCROLL
    , {ssVertical}       WS_VSCROLL or ES_DISABLENOSCROLL
    , {ssBoth}           WS_HSCROLL or WS_HSCROLL or ES_DISABLENOSCROLL
    , {ssAutoHorizontal} WS_HSCROLL
    , {ssAutoVertical}   WS_VSCROLL
    , {ssAutoBoth}       WS_HSCROLL or WS_HSCROLL
  );


const
  TAB_OFFSET_MASK = $7FFFFF;
  TAB_OFFSET_BITS = 24;
  TAB_ALIGN_MASK  = 3;
  TWIP_PT         = 20; // Twips in Point. Twips are commonly used measurement unit for RichEdit inteface
  POINTS_INCH     = 72;
  TWIP_INCH       = POINTS_INCH * TWIP_PT;

  {%H-}TAB_LEFT      = 0;  // Ordinary tab
  TAB_CENTER    = 1;  // Center tab
  TAB_RIGHT     = 2;  // Right-aligned tab
  TAB_DECIMAL   = 3;  // Decimal tab
  TAB_WORD      = 4;  // Word bar tab (vertical bar)

  FORMAT_RENDER   = 1;
  FORMAT_ESTIMATE = 0;
  
procedure LockRedraw(rm: TCustomRichMemo; AHandle: THandle);
var
  ln: TWin32RichMemoStrings;
begin
  ln:=TWin32RichMemoStrings(rm.Lines);
  if not Assigned(ln) or not ln.fUpd then
    SendMessage(AHandle, WM_SETREDRAW, 0, 0);
end;

procedure UnlockRedraw(rm: TCustomRichMemo; AHandle: HWND; NeedInvalidate: Boolean = true);
var
  ln: TWin32RichMemoStrings;
begin
  ln:=TWin32RichMemoStrings(rm.Lines);
  if not Assigned(ln) or not ln.fUpd then begin
    SendMessage(AHandle, WM_SETREDRAW, 1, 0);
    if NeedInvalidate then
      Windows.InvalidateRect(AHandle, nil, true);
  end;
end;

type
 PENLINK = ^TENLINK;

function RichEditNotifyProc(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  lnk : PENLINK;
  hdr : PNMHDR;
  mb  : TMouseButton;
  mmsg : UINT;
  isClick : Boolean;
  minfo   : TLinkMouseInfo;
begin
  Result:=false; // we need to catch just notifications,
    // any other message should be handled in a "Default" manner
    // So, default result is false;
  case Msg of
    WM_COMMAND: begin
      case HIWORD(WParam) of
        EN_UPDATE: begin
          // https://msdn.microsoft.com/en-us/library/windows/desktop/bb761687(v=vs.85).aspx
          // EN_UPDATE is just a notification, that the edit is to be redrawn
          Result:=true;
        end;
        EN_CHANGE: begin
          // https://msdn.microsoft.com/en-us/library/windows/desktop/hh768384(v=vs.85).aspx
          // Despite of what documentation claims (WM_NOTIFY), EN_CHANGE is notified by WM_COMMAND
          if Assigned(AWinControl) and (AWinControl is TCustomRichMemo) then
            TIntCustomRichMemo(AWinControl).Change;
          Result:=true;
          if IsIconic( GetAncestor(AWinControl.Handle, GA_ROOTOWNER)) then
            // hack: the RichMemo has problems repainting itself
            //       if changes were done while the root window was minimized
            //       see #34391
            GetWin32WindowInfo(AWinControl.Handle)^.TrackValid:=true;
        end;
      end;
    end;
    WM_NOTIFY: begin
      hdr:=PNMHDR(LParam);
      case hdr^.code of
        EN_SELCHANGE:
          begin
            if Assigned(AWinControl) and (AWinControl is TCustomRichMemo) then
              TIntCustomRichMemo(AWinControl).DoSelectionChange;
            Result:=true;
          end;
        EN_LINK:
          begin
            lnk:=PENLINK(LPARAM);
            if Assigned(AWinControl) and (AWinControl is TCustomRichMemo) then begin
              isClick:=true;
              mmsg:=lnk^.msg;
              mb:=mbLeft;
              case mmsg of
                WM_LBUTTONUP: mb:=mbLeft;
                WM_RBUTTONUP: mb:=mbRight;
                WM_MBUTTONUP: mb:=mbMiddle;
              else
                isClick:=false;
              end;
              if isClick then begin
                FillChar(minfo, sizeof(minfo), 0);
                minfo.button:=mb;
                RichEditManager.LinkNotifyToInfo( AWinControl.Handle,
                  lnk^, minfo );
                TIntCustomRichMemo(AWinControl).DoLinkAction(laClick, minfo, lnk^.chrg.cpMin, lnk^.chrg.cpMax-lnk^.chrg.cpMin);
              end;

            end;
            Result:=true;
          end;
      end;
    end;
  end;
end;

function KeysToShiftState(Keys: PtrUInt): TShiftState;
begin
  Result := [];
  if Keys and MK_Shift <> 0 then Include(Result, ssShift);
  if Keys and MK_Control <> 0 then Include(Result, ssCtrl);
  if Keys and MK_LButton <> 0 then Include(Result, ssLeft);
  if Keys and MK_RButton <> 0 then Include(Result, ssRight);
  if Keys and MK_MButton <> 0 then Include(Result, ssMiddle);
  if Keys and MK_XBUTTON1 <> 0 then Include(Result, ssExtra1);
  if Keys and MK_XBUTTON2 <> 0 then Include(Result, ssExtra2);
  if Keys and MK_DOUBLECLICK <> 0 then Include(Result, ssDouble);
  if Keys and MK_TRIPLECLICK <> 0 then Include(Result, ssTriple);
  if Keys and MK_QUADCLICK <> 0 then Include(Result, ssQuad);

  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  if (GetKeyState(VK_LWIN) < 0) or (GetKeyState(VK_RWIN) < 0) then Include(Result, ssMeta);
end;


procedure WMPrintRichMemo(const AWinControl: TWinControl; ADC: HDC);
var
  fmt          : TFormatRange;
  cxPhysOffset : Integer;
  cyPhysOffset : Integer;
  cxPhys       : Integer;
  cyPhys       : Integer;
  r : TRect;
  p : POINTL;
begin
  // why would WinAPI make things easier if it can make them harder?!
  // WM_PRINT doesn't seem to be fully functinoal for RICHEDIT
  // EM_FORMATRANGE needs to be used
  fmt.hdc:=ADC;
  fmt.hdcTarget:=ADC;

  p.x:=1;
  p.y:=1;
  fmt.chrg.cpMin:=SendMessage(AWinControl.Handle, EM_CHARFROMPOS, 0, LPARAM(@p));
  fmt.chrg.cpMax:=TCustomRichMemo(AWinControl).GetTextLen;

  case GetDeviceCaps(adc, TECHNOLOGY) of
    DT_RASDISPLAY, DT_DISPFILE: begin
      Windows.GetClientRect(AWinControl.Handle, r);
      r:=Bounds(0,0,AWinControl.Width,AWinControl.Height);
      //todo: to be properly read from the border settings
      InflateRect(r, -4, -4);
      fmt.rcPage.left   := MulDiv(r.Left,  1440, GetDeviceCaps(adc, LOGPIXELSX));
      fmt.rcPage.top    := MulDiv(r.Top,   1440, GetDeviceCaps(adc, LOGPIXELSY));
      fmt.rcPage.right  := MulDiv(r.Right, 1440, GetDeviceCaps(adc, LOGPIXELSX));
      fmt.rcPage.bottom := MulDiv(r.Bottom,1440, GetDeviceCaps(adc, LOGPIXELSY));
      fmt.rc:=Bounds(
        MulDiv(2,1440, GetDeviceCaps(adc, LOGPIXELSY)),
        MulDiv(2,1440, GetDeviceCaps(adc, LOGPIXELSY)),
        fmt.rcPage.Right-fmt.rcPage.Left, fmt.rcPage.Bottom-fmt.rcPage.top);;
    end;
    DT_RASPRINTER,DT_PLOTTER,DT_METAFILE: begin
      cxPhysOffset := GetDeviceCaps(adc, PHYSICALOFFSETX);
      cyPhysOffset := GetDeviceCaps(adc, PHYSICALOFFSETY);
      cxPhys := GetDeviceCaps(adc, PHYSICALWIDTH);
      cyPhys := GetDeviceCaps(adc, PHYSICALHEIGHT);

      // Set page rect to physical page size in twips.
      fmt.rcPage.top    := 0;
      fmt.rcPage.left   := 0;
      fmt.rcPage.right  := MulDiv(cxPhys, 1440, GetDeviceCaps(adc, LOGPIXELSX));
      fmt.rcPage.bottom := MulDiv(cyPhys, 1440, GetDeviceCaps(adc, LOGPIXELSY));

      // Set the rendering rectangle to the pintable area of the page.
      fmt.rc.left   := cxPhysOffset;
      fmt.rc.right  := cxPhysOffset + cxPhys;
      fmt.rc.top    := cyPhysOffset;
      fmt.rc.bottom := cyPhysOffset + cyPhys;
    end;
  end;
  SendMessage(AWinControl.Handle, EM_FORMATRANGE, 1, LPARAM(@fmt));

  // free mem. See MSDN
  SendMessage(AWinControl.Handle, EM_FORMATRANGE, 0, 0);
end;

function RichEditProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
   LParam: Windows.LParam): LResult; stdcall;
var
  WindowInfo : PWin32WindowInfo;
  NcHandled  : Boolean; // NCPaint has painted by itself
  r: TRect;
  PrevWndProc: Windows.WNDPROC;
  P: TPoint;
begin
  case Msg of
    WM_PAINT : begin
      //todo: LCL WM_PAINT handling prevents richedit from drawing correctly
      Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
      //Result := WindowProc(Window, Msg, WParam, LParam)
      // hack: the RichMemo has problems repainting itself
      //       if changes were done while the root window was minimized
      //       see #34391
      if GetWin32WindowInfo(Window)^.TrackValid then begin
        Windows.GetWindowRect(Window, r);
        OffsetRect(r, -r.left, -r.top);
        r.Left:=-r.Right;
        r.Top:=-r.Bottom;
        InvalidateRect(Window, @r, false);
        PrevWndProc := GetWin32WindowInfo(Window)^.DefWndProc;
        Windows.CallWindowProcW(PrevWndProc, Window, Msg, WParam, LParam);
        GetWin32WindowInfo(Window)^.TrackValid:=false;
      end;
    end;
      //When theming is enabled, and the component should have a border around it,
    WM_NCPAINT: begin
      if Assigned(NCPaint) then begin
        NcHandled :=false;
        WindowInfo := GetWin32WindowInfo(Window);
        if WindowInfo^.WinControl is TCustomRichMemo then
          try
            Result:=NCPaint(Window, TCustomRichMemo(WindowInfo^.WinControl), WParam, LParam, NcHandled);
          except
          end;
        // not handled by LCL pass it to WinAPI
        if not NcHandled then
          Result:=WindowProc(Window, Msg, WParam, LParam);
      end else
        Result:=WindowProc(Window, Msg, WParam, LParam);
      end;

    // The handling is needed, due to LCL scrolling is making everything slow
    // for whatever reason. (because it doesn't pass the message to WinAPI?)
    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
    begin
      WindowInfo := GetWin32WindowInfo(Window);

      // WinAPI sends Screen Coordinates, LCL expects client coordinates
      p := Point(GET_X_LPARAM(LParam), GET_Y_LPARAM(LParam));
      if Assigned(WindowInfo^.WinControl) then
        p := WindowInfo^.WinControl.ScreenToClient(p);

      Result := LCLSendMouseWheelMsg(WindowInfo^.WinControl,
        p.x, p.y,
        SmallInt(HIWORD(Integer(WParam))),
        KeysToShiftState(LOWORD(Integer(WParam))));

      // Non zero value is returned, if LCL marked the message as Handeld
      if Result = 0 then
        Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
    end;
  else
    Result := WindowProc(Window, Msg, WParam, LParam);
  end;

  case msg of
    WM_PRINT: begin
      WindowInfo := GetWin32WindowInfo(Window);
      WMPrintRichMemo(WindowInfo^.WinControl, HDC(WParam));
    end;
  end;
end;

{ TWin32Inline }

function TWin32Inline.Draw(dwDrawAspect: DWord; LIndex: Long;
  pvaspect: pointer; ptd: PDVTARGETDEVICE; hdcTargetDev: HDC; hdcDraw: HDC;
  lprcBounds: PRECTL; lprcWBounds: PRECTL; pfncontinue: TContinueCallback;
  dwcontinue: ULONG_PTR): HResult; stdcall;
var
  rst : Boolean;
  pts : Windows.TPOINT;
  sz  : TSize;
begin
  if not isvis then begin
    isvis:=true;
    rminline.SetVisible(isvis);
  end;
  canvas.Handle:=hdcDraw;

  rst:= Assigned(lprcBounds);
  if rst then begin
    Windows.OffsetViewportOrgEx(hdcDraw, lprcBounds^.left, lprcBounds^.top, @pts);
    sz.cx:=lprcBounds^.right - lprcBounds^.left;
    sz.cy:=lprcBounds^.bottom - lprcBounds^.top;
  end else begin
    sz.cx:=0;
    sz.cy:=0;
    pts:=Point(0,0);
  end;

  rminline.Draw(canvas, sz);
  if rst then Windows.OffsetViewportOrgEx(hdcDraw, pts.x, pts.y, nil);

  Result:=S_OK;
end;

function TWin32Inline.GetExtent(dwDrawAspect: DWORD; out size: TPoint
  ): HResult; StdCall;
begin
  if not isvis then begin
    rminline.SetVisible(true);
    isvis:=true;
  end;
  Result:=inherited GetExtent(dwDrawAspect, size);
end;

function TWin32Inline.Close(dwSaveOption: DWORD): HResult; StdCall;
begin
  if isvis then begin
    rminline.SetVisible(false);
    isvis:=false;
  end;
  Result:=inherited Close(dwSaveOption);
end;

destructor TWin32Inline.Destroy;
begin
  rminline.Free;
  canvas.free;
  inherited Destroy;
end;

{ TWin32RichMemoStrings }

constructor TWin32RichMemoStrings.Create(AHandle: HWND; TheOwner: TWinControl);
begin
  inherited Create(AHandle, TheOwner);
  fHandle:=AHandle;
end;

procedure TWin32RichMemoStrings.SetUpdateState(Updating: Boolean);
begin
  fUpd:=Updating;
  inherited SetUpdateState(Updating);
end;

function TWin32RichMemoStrings.GetTextStr: string;
begin
  if Assigned(RichEditManager) then
    Result:=RichEditManager.GetTextUtf8(fHandle, false)
  else
    Result:='';
end;

{ TWin32WSCustomRichMemo }

class function TWin32WSCustomRichMemo.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result:=Assigned(RichEditManager);
  if Result then
    AText:=RichEditManager.GetTextUtf8(AWinControl.Handle, false);
end;

class function TWin32WSCustomRichMemo.GetTextLen(
  const AWinControl: TWinControl; var ALength: Integer): Boolean;
begin
  Result:=false;
  ALength:=0;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  Result:=true;
  ALength:=RichEditManager.GetTextLength(AWinControl.Handle);
end;

class function TWin32WSCustomRichMemo.GetStrings(const ACustomMemo: TCustomMemo
  ): TStrings;
begin
  Result := TWin32RichMemoStrings.Create(ACustomMemo.Handle, ACustomMemo)
end;

class procedure TWin32WSCustomRichMemo.SetColor(const AWinControl: TWinControl);  
var
  Color: TColor;
begin
  // this methos is implemented, because Win32RichMemo doesn't use
  // default LCL WM_PAINT message!
  Color := AWinControl.Color;
  if Color = clDefault then
    Color := AWinControl.GetDefaultColor(dctBrush);
  SendMessage(AWinControl.Handle, EM_SETBKGNDCOLOR, 0, ColorToRGB(Color));
end;

class procedure TWin32WSCustomRichMemo.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not Assigned(AWinControl) then Exit;
  Windows.SendMessage(AWinControl.Handle, WM_SETFONT, Windows.WParam(AFont.Reference.Handle), 1);
  RichEditManager.SetDefaultTextStyle(AWinControl.Handle, GetFontParams(AFont));
end;

class procedure TWin32WSCustomRichMemo.SetSelStart(const ACustomEdit: TCustomEdit; NewStart: integer);  
var
  range : Tcharrange;
begin
  range.cpMin := NewStart;
  range.cpMax := NewStart;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);
end;

class procedure TWin32WSCustomRichMemo.SetSelLength(const ACustomEdit: TCustomEdit; NewLength: integer);  
var
  range : Tcharrange;
begin
  SendMessage(ACustomEdit.Handle, EM_EXGETSEL, 0, LPARAM(@range));
  range.cpMax := range.cpMin + NewLength;
  SendMessage(ACustomEdit.Handle, EM_EXSETSEL, 0, LPARAM(@range));
  InvalidateRect(ACustomEdit.Handle, nil, false);
end;

class procedure TWin32WSCustomRichMemo.CutToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_CUT, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.CopyToClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_COPY, 0,0);  
end;

class procedure TWin32WSCustomRichMemo.PasteFromClipboard(const AWinControl: TWinControl);  
begin
  SendMessage(AWinControl.Handle, WM_PASTE, 0,0);
end;

class function TWin32WSCustomRichMemo.CanPasteFromClipboard(
  const AWinControl: TWinControl): boolean;
begin
  Result:=Assigned(AWinControl) and (SendMessage(AWinControl.Handle, EM_CANPASTE, 0, 0)<>0);
end;

procedure AssignOLECallback(ARichMemo: TCustomRichMemo; ahandle: Windows.THandle);
var
  cb : IRichEditOleCallback;
begin
  if not Assigned(AllocOLEObject) then Exit;
  AllocOLEObject(ARichMemo, ahandle, cb);
  if Assigned(cb) then
    Windows.SendMessage(ahandle, EM_SETOLECALLBACK, 0, LPARAM(cb));
end;

class function TWin32WSCustomRichMemo.CreateHandle(const AWinControl: TWinControl;  
  const AParams: TCreateParams): HWND;  
var
  Params      : TCreateWindowExParams;
  RichClass   : AnsiString;
  ACustomMemo : TCustomMemo;
  eventmask   : LPARAM;
begin
  if RichEditClass='' then begin
    InitRichEdit;
    RichClass := GetRichEditClass;
  end else
    RichClass := RichEditClass;

  if RichClass = '' then begin
    Result := 0;
    Exit;
  end;

  // general initialization of Params

  // if you're using 0.9.28.2 lazarus, uncomment the line,
  // PrepareCreateWindow(AWinControl, Params);
  // and comment the following like (it's for 0.9.30 compatiblity):
  PrepareCreateWindow(AWinControl, AParams, Params);

  Params.SubClassWndProc := @RichEditProc;

  // customization of Params
  ACustomMemo := TCustomMemo(AWinControl);
  with Params do
  begin
    Flags := Flags or ES_AUTOVSCROLL or ES_MULTILINE or ES_WANTRETURN;

    if ACustomMemo.ReadOnly then
      Flags := Flags or ES_READONLY;
    Flags := Flags or AlignmentToEditFlags[ACustomMemo.Alignment];
    Flags := Flags or ScrollStyleToEditFlags[ACustomMemo.ScrollBars];

    if ACustomMemo.WordWrap then
      Flags := Flags and not WS_HSCROLL
    else
      Flags := Flags or ES_AUTOHSCROLL;
      
    if ACustomMemo.BorderStyle=bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
      
    pClassName := @RichClass[1];
    WindowTitle := StrCaption;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);

  eventmask := SendMessage(AWinControl.Handle, EM_GETEVENTMASK, 0, 0);
  eventmask := eventmask or ENM_SELCHANGE or ENM_LINK or ENM_CHANGE;
  SendMessage(AWinControl.Handle, EM_SETEVENTMASK, 0, eventmask);

  // Limitless text. However, the value would be overwritten by a consequent
  // SetMaxLength call, see above.
  SendMessage(AWincontrol.Handle, EM_EXLIMITTEXT, 0, LParam(-1));

  // Setting OLE callback.
  if AWinControl is TCustomRichMemo then // sanity checl
    AssignOLECallback(TCustomRichMemo(AWincontrol), AWincontrol.Handle);


  // memo is not a transparent control -> no need for parentpainting
  Params.WindowInfo^.ParentMsgHandler := @RichEditNotifyProc;
  Params.WindowInfo^.needParentPaint := false;
  Result := Params.Window;
end;

class procedure TWin32WSCustomRichMemo.SetTextAttributes(const AWinControl: TWinControl; 
  TextStart, TextLen: Integer; const Params: TIntFontParams);  
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;
  eventmask : Integer;
  pt        : TPoint;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  RichEditManager.GetScroll(AWinControl.Handle, pt);
  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);
  
  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, TextLen);
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params );
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
  end else 
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params);

  RichEditManager.SetScroll(AWinControl.Handle, pt);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetTextAttributes(const AWinControl: TWinControl; 
  TextStart: Integer; var Params: TIntFontParams): Boolean;  
var
  Orig      : TCHARRANGE;
  eventmask : LongWord;
  pt        : TPoint;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;
  InitFontParams(Params);

  RichEditManager.GetScroll(AWinControl.Handle, pt);
  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);

  LockRedraw(TCustomRichMemo(AWinControl), AWinControl.Handle);

  RichEditManager.GetSelRange(AWinControl.Handle, Orig);

  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  Result := RichEditManager.GetSelectedTextStyle(AWinControl.Handle, Params );

  RichEditManager.SetSelRange(AWinControl.Handle, Orig);
  RichEditManager.SetScroll(AWinControl.Handle, pt);
  UnlockRedraw(TCustomRichMemo(AWinControl), AWinControl.Handle, false);

  RichEditManager.SetEventMask(AWinControl.Handle,eventmask);
end;

class function TWin32WSCustomRichMemo.isInternalChange(
  const AWinControl: TWinControl; Params: TTextModifyMask): Boolean;
begin
  Result:=True;
end;

class procedure TWin32WSCustomRichMemo.SetTextAttributesInternal(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AModifyMask: TTextModifyMask; const Params: TIntFontParams);
var
  OrigStart : Integer;
  OrigLen   : Integer;
  eventmask : longword;
  NeedLock  : Boolean;
  pt        : TPoint;
begin
  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetScroll(AWinControl.Handle, pt);
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);

  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, TextLen);
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params, True, AModifyMask);
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
  end else
    RichEditManager.SetSelectedTextStyle(AWinControl.Handle, Params, True, AModifyMask);

  RichEditManager.SetScroll(AWinControl.Handle, pt);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;


class procedure TWin32WSCustomRichMemo.SetHideSelection(
  const ACustomEdit: TCustomEdit; AHideSelection: Boolean);  
begin
  if not Assigned(RichEditManager) or not Assigned(ACustomEdit) then Exit;
  RichEditManager.SetHideSelection(ACustomEdit.Handle, AHideSelection);
end;

class procedure TWin32WSCustomRichMemo.SetMaxLength(
  const ACustomEdit: TCustomEdit; NewLength: integer);
var
  winhandle: HWND;
  relen : Integer;
begin
  winhandle := ACustomEdit.Handle;
  // By default NewLength=0. For RichEdit that means 64K or 32K limitation.
  // Which makes no sense and isn't cross platform!
  // RichMemo assumes NewLength=0 as limitless
  if NewLength=0 then relen:=-1 // limitless
  else relen:=NewLength;
  SendMessage(winhandle, EM_EXLIMITTEXT, 0, LParam(relen));
  GetWin32WindowInfo(winhandle)^.MaxLength := NewLength;
end;

procedure InitScrollInfo(var info: TScrollInfo);
begin
  FillChar(info, sizeof(info), 0);
  info.cbSize := sizeof(info);
  info.fMask := SIF_ALL;
end;

class function TWin32WSCustomRichMemo.GetStyleRange(
  const AWinControl: TWinControl; TextStart: Integer; var RangeStart, 
  RangeLen: Integer): Boolean;  
var
  Orig      : TCharRange;
  eventmask : longword;
  pt  : TPoint;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result := false;
    Exit;
  end;

  RichEditManager.GetScroll(AWinControl.Handle, pt);

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  LockRedraw(TCustomRichMemo(AWinControl), AWinControl.Handle);

  RichEditManager.GetSelRange(AWinControl.Handle, Orig);

  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  try
    Result := RichEditManager.GetStyleRange(AWinControl.Handle, TextStart, RangeStart, RangeLen);
  except
  end;
  
  RichEditManager.SetSelRange(AWinControl.Handle, Orig);
  RichEditManager.SetScroll(AWinControl.Handle, pt);
  UnlockRedraw(TCustomRichMemo(AWinControl), AWinControl.Handle, false);
  
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class procedure TWin32WSCustomRichMemo.SetTextUIParams(const AWinControl: TWinControl; TextStart, TextLen: Integer;
      const ui: TTextUIParam);
var
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;
  eventmask : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetSelection(AWinControl.Handle, OrigStart, OrigLen);

  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
    RichEditManager.SetSelection(AWinControl.Handle, TextStart, TextLen);
    RichEditManager.SetTextUIStyle(AWinControl.Handle, ui);
    RichEditManager.SetSelection(AWinControl.Handle, OrigStart, OrigLen);
    UnlockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);
  end else
    RichEditManager.SetTextUIStyle(AWinControl.Handle, ui);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetTextUIParams(const AWinControl: TWinControl; TextStart: Integer;
  var ui: TTextUIParam): Boolean;
var
  Orig      : TCHARRANGE;
  eventmask : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result:=false;
    Exit;
  end;

  eventmask := RichEditManager.SetEventMask(AWinControl.Handle, 0);
  LockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);

  RichEditManager.GetSelRange(AWinControl.Handle, Orig);

  RichEditManager.SetSelection(AWinControl.Handle, TextStart, 1);
  RichEditManager.GetTextUIStyle(AWinControl.Handle, ui);

  RichEditManager.SetSelRange(AWinControl.Handle, Orig);
  UnlockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
  Result:=true;
end;

class function TWin32WSCustomRichMemo.LoadRichText(
  const AWinControl: TWinControl; Source: TStream): Boolean;  
begin
  Result := False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.LoadRichText(AWinControl.Handle, Source);
end;

class function TWin32WSCustomRichMemo.SaveRichText(
  const AWinControl: TWinControl; Dst: TStream): Boolean;  
begin
  Result := false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  Result := RichEditManager.SaveRichText(AWinControl.Handle, Dst);
end;

class function TWin32WSCustomRichMemo.GetParaAlignment(
  const AWinControl: TWinControl; TextStart: Integer; var AAlign: TParaAlignment
  ): Boolean;
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  LockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle);

  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  case para.wAlignment of
    PFA_CENTER:  AAlign:=paCenter;
    PFA_RIGHT:   AAlign:=paRight;
    PFA_JUSTIFY: AAlign:=paJustify;
  else
    AAlign:=paLeft;
  end;
  UnlockRedraw( TCustomRichMemo(AWinControl), AWinControl.Handle );
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);

  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.SetParaAlignment(
  const AWinControl: TWinControl; TextStart, TextLen: Integer; const AAlign: TIntParaAlignment);
var
  para : PARAFORMAT2;
  eventmask: Integer;
const
  WinPara : array [TIntParaAlignment] of word = (PFA_LEFT, PFA_RIGHT, PFA_CENTER, PFA_JUSTIFY);
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  FillChar(para, sizeof(para), 0);
  para.cbSize:=sizeof(para);
  para.dwMask:=PFM_ALIGNMENT;
  para.wAlignment:=WinPara[AAlign];
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetParaMetric(
  const AWinControl: TWinControl; TextStart: Integer;
  var AMetrics: TIntParaMetric): Boolean;
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  Result:=false;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);

  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);

  AMetrics.FirstLine:=para.dxStartIndent/TWIP_PT;
  AMetrics.TailIndent:=para.dxRightIndent/TWIP_PT;
  AMetrics.HeadIndent:=(para.dxStartIndent+para.dxOffset)/TWIP_PT;
  AMetrics.SpaceAfter:=para.dySpaceAfter/TWIP_PT;
  AMetrics.SpaceBefore:=para.dySpaceBefore/TWIP_PT;
  AMetrics.LineSpacing:=para.dyLineSpacing*DefLineSpacing/TWIP_PT;

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class procedure TWin32WSCustomRichMemo.SetParaMetric(
  const AWinControl: TWinControl; TextStart, TextLength: Integer;
  const AMetrics: TIntParaMetric);
var
  para : PARAFORMAT2;
  eventmask: Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_STARTINDENT or PFM_RIGHTINDENT
     or PFM_OFFSET
     or PFM_SPACEAFTER or PFM_SPACEBEFORE
     or PFM_LINESPACING;
  para.dxStartIndent:=round(AMetrics.FirstLine*TWIP_PT);
  para.dxRightIndent:=round(AMetrics.TailIndent*TWIP_PT);
  para.dxOffset:=round((AMetrics.HeadIndent-AMetrics.FirstLine)*TWIP_PT);
    //round(AMetrics.HeadIndent*TWIP_PT);
  para.dySpaceAfter:=round(AMetrics.SpaceAfter*TWIP_PT);
  para.dySpaceBefore:=round(AMetrics.SpaceBefore*TWIP_PT);
  if AMetrics.LineSpacing > 0 then begin
    para.dyLineSpacing:=round(AMetrics.LineSpacing/DefLineSpacing*TWIP_PT);
    para.bLineSpacingRule:=5; // always line spacing?
  end;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLength, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetParaRange(const AWinControl: TWinControl;
  TextStart: Integer; var ParaRange: TParaRange): Boolean;
begin
  if not Assigned(AWinControl) then
    Result:=False
  else begin
    RichEditManager.GetParaRange(AWinControl.Handle, TextStart, ParaRange);
    Result:=true;
  end;
end;

const
  PFN_ARABIC   = 2;
  PFN_LCLETTER = 3;
  PFN_LCROMAN  = 4;
  PFN_UCLETTER = 5;
  PFN_UCROMAN  = 6;
  PFN_CUSTOM   = 7;

class function TWin32WSCustomRichMemo.GetParaNumbering(
  const AWinControl: TWinControl; TextStart: Integer;
  var ANumber: TIntParaNumbering): Boolean;
var
  para : PARAFORMAT2;
  eventmask: INteger;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  InitParaNumbering(ANumber);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);

  case para.wNumbering of
    PFN_BULLET:   ANumber.Style:=pnBullet;
    PFN_ARABIC:   ANumber.Style:=pnNumber;
    PFN_LCLETTER: ANumber.Style:=pnLowLetter;
    PFN_LCROMAN:  ANumber.Style:=pnLowRoman;
    PFN_UCLETTER: ANumber.Style:=pnUpLetter;
    PFN_UCROMAN:  ANumber.Style:=pnUpRoman;
    PFN_CUSTOM:   begin
      ANumber.Style:=pnCustomChar;
      ANumber.CustomChar:=WideChar(para.wNumberingStart);
    end;
  else
    ANumber.Style:=pnNone;
  end;
  if para.wNumberingStyle or PFNS_PLAIN > 0 then
    ANumber.SepChar:=SepNone
  else if para.wNumberingStyle or PFNS_PERIOD > 0 then
    ANumber.SepChar:=SepDot
  else if (ANumber.Style<>pnNone) and ((para.wNumberingStyle and PFNS_SOMESEPCHAR)= 0) then
    ANumber.SepChar:=SepPar;
  ANumber.Indent:=para.wNumberingTab/TWIP_PT;
  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.SetParaNumbering(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const ANumber: TIntParaNumbering);
var
  para : PARAFORMAT2;
  eventmask: Integer;
  numbstyle: Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=
     PFM_NUMBERING or PFM_NUMBERINGTAB;

  numbstyle:=0;
  case ANumber.SepChar of
    SepPar: numbstyle:=numbstyle or PFNS_PAREN;
    SepDot: numbstyle:=numbstyle or PFNS_PERIOD;
    SepNone: numbstyle:=numbstyle or PFNS_PLAIN;
  end;
  case ANumber.Style of
    pnNone:       para.wNumbering:=0;
    pnBullet:     para.wNumbering:=PFN_BULLET;
    pnNumber: begin
      para.wNumbering:=PFN_ARABIC;
      para.dwMask:=para.dwMask or PFM_NUMBERINGSTART;
      para.wNumberingStart:=ANumber.NumberStart;
      if ANumber.ForceNewNum then numbstyle:=numbstyle or PFNS_NEWNUMBER;
    end;
    pnLowLetter:  para.wNumbering:=PFN_LCLETTER;
    pnLowRoman:   para.wNumbering:=PFN_LCROMAN;
    pnUpLetter:   para.wNumbering:=PFN_UCLETTER;
    pnUpRoman:    para.wNumbering:=PFN_UCROMAN;
    pnCustomChar: begin
      para.wNumbering:=PFN_CUSTOM;
      para.wNumberingStart:=Word(ANumber.CustomChar);
      para.dwMask:=para.dwMask or PFM_NUMBERINGSTART;
    end;
  end;
  if numbstyle<> 0 then begin
    para.dwMask:=para.dwMask or PFM_NUMBERINGSTYLE;
    para.wNumberingStyle:=numbstyle;
  end;

  para.wNumberingTab:=round(ANumber.Indent*TWIP_PT);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask)
end;

class procedure TWin32WSCustomRichMemo.SetParaTabs(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  const AStopList: TTabStopList);
var
  para : PARAFORMAT2;
  eventmask: Integer;
  cnt : Integer;
  i   : Integer;
const
  PARAALIGN   : array [TTabAlignment] of LongWord = (
     0 shl TAB_OFFSET_BITS, // tabHead,
     1 shl TAB_OFFSET_BITS, // tabCenter,
     2 shl TAB_OFFSET_BITS, // tabTail,
     3 shl TAB_OFFSET_BITS, // tabDecimal,
     4 shl TAB_OFFSET_BITS  // tabWordBar
     );

begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  FillChar(para, SizeOf(para), 0);

  para.cbSize:=sizeof(para);
  para.dwMask:=PFM_TABSTOPS;

  if AStopList.Count > MAX_TAB_STOPS then cnt:=MAX_TAB_STOPS
  else cnt:=AStopList.Count;

  para.cTabCount:=cnt;
  for i:=0 to cnt-1 do begin
    para.rgxTabs[i]:=((round(AStopList.Tabs[i].Offset*TWIP_PT)) and TAB_OFFSET_MASK) or PARAALIGN[AStopList.Tabs[i].Align] ;
  end;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetPara2(AWinControl.Handle, TextStart, TextLen, para);

  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetParaTabs(
  const AWinControl: TWinControl; TextStart: integer;
  var AStopList: TTabStopList): Boolean;
var
  para : PARAFORMAT2;
  eventmask: Integer;
  v  : LongWord;
  al : TTabAlignment;
  i  : Integer;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.GetPara2(AWinControl.Handle, TextStart, para);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);

  InitTabStopList(AStopList);
  AStopList.Count:=para.cTabCount;
  SetLength(AStopList.Tabs, AStopList.Count);
  for i:=0 to AStopList.Count-1 do begin
    v:=para.rgxTabs[i];
    AStopList.Tabs[i].Offset:=(v and TAB_OFFSET_MASK) / TWIP_PT;
    case (v shr TAB_OFFSET_BITS) and TAB_ALIGN_MASK of
      TAB_CENTER : al:=tabCenter;
      TAB_RIGHT  : al:=tabRight;
      TAB_DECIMAL: al:=tabDecimal;
      TAB_WORD   : al:=tabWordBar;
    else
      al:=tabLeft;
    end;
    AStopList.Tabs[i].Align:=al;
  end;
  Result:=true;
end;

class procedure TWin32WSCustomRichMemo.InDelText(const AWinControl:TWinControl;
  const TextUTF8:String;DstStart,DstLen:Integer);
var
  eventmask : Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  RichEditManager.SetText(AWinControl.Handle,UTF8Decode(TextUTF8),DstStart,DstLen);
  RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
end;

class function TWin32WSCustomRichMemo.GetSubText(
  const AWinControl: TWinControl; TextStart, TextLen: Integer;
  AsUnicode: Boolean; var isUnicode: Boolean; var txt: string;
  var utxt: UnicodeString): Boolean;
var
  eventmask : Integer;
  Orig      : TCharRange;
  OrigStart : Integer;
  OrigLen   : Integer;
  NeedLock  : Boolean;
  Hnd       : THandle;
begin
  Result:=Assigned(RichEditManager) and Assigned(AWinControl);
  if not Result then Exit;

  Hnd:=AWinControl.Handle;
  eventmask := RichEditManager.SetEventMask(Hnd, 0);
  RichEditManager.GetSelection(Hnd, OrigStart, OrigLen);

  NeedLock := (OrigStart <> TextStart) or (OrigLen <> TextLen);
  if NeedLock then begin
    LockRedraw( TCustomRichMemo(AWinControl), Hnd);
    RichEditManager.GetSelRange(Hnd, Orig);
  end;

  RichEditManager.SetSelection(Hnd, TextStart, TextLen);
  isUnicode:=AsUnicode;
  if AsUnicode then
    utxt:=RichEditManager.GetTextW(Hnd, true)
  else begin
    txt:=RichEditManager.GetTextUtf8(Hnd, true);
  end;

  if NeedLock then begin
    RichEditManager.SetSelRange(Hnd, Orig);
    UnlockRedraw( TCustomRichMemo(AWinControl), Hnd);
  end;
  RichEditManager.SetEventMask(Hnd, eventmask);
end;

class function TWin32WSCustomRichMemo.CharAtPos(const AWinControl: TWinControl;
  x, y: Integer): Integer;
var
  p : POINTL;
begin
  if not Assigned(AWinControl) then
    Result:=0
  else begin
    p.x:=x;
    p.y:=y;
    Result:=Windows.SendMessage(AWinControl.Handle, EM_CHARFROMPOS, 0, LPARAM(@p));
  end;
end;

class function TWin32WSCustomRichMemo.Search(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt): Integer;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then begin
    Result:=-1;
    Exit;
  end;
  Result:=RichEditManager.Find(AWinControl.Handle, UTF8Decode(ANiddle), SearchOpts);
end;

class function TWin32WSCustomRichMemo.isSearchEx: Boolean;
begin
  Result:=true;
end;

class function TWin32WSCustomRichMemo.SearchEx(const AWinControl: TWinControl;
  const ANiddle: string; const SearchOpts: TIntSearchOpt; var ATextStart,
  ATextLength: Integer): Boolean;
begin
  ATextStart:=RichEditManager.Find(AWinControl.Handle, UTF8Decode(ANiddle), SearchOpts, ATextLength);
  Result:=ATextStart>=0;
end;

class procedure TWin32WSCustomRichMemo.SetZoomFactor(
  const AWinControl: TWinControl; AZoomFactor: Double);
var
  DN : WParam;
begin
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;
  DN := 1000;
  SendMessage( AWinControl.Handle, EM_SETZOOM, round(AZoomFactor * DN), DN);
end;

class function TWin32WSCustomRichMemo.GetZoomFactor(const AWinControl: TWinControl; var AZoomFactor: Double): Boolean;
var
  numerator, denominator: Integer; // todo: Are they always integers? or can they be something else in 64-bit?
begin
  Result:=SendMessage(AWinControl.Handle, EM_GETZOOM, WParam(@numerator), LParam(@denominator))<>0;
  if Result then begin
    Result:=(numerator <> 0) and (denominator <> 0);
    if Result then
      AZoomFactor := double(numerator)/ double(denominator);
  end;
end;

class function TWin32WSCustomRichMemo.InlineInsert(
  const AWinControl: TWinControl; ATextStart, ATextLength: Integer;
  const ASize: TSize; AHandler: TRichMemoInline;
  var wsObj: TRichMemoInlineWSObject): Boolean;
var
  hnd : THandle;
  rch : IRichEditOle;
  Fmt : FORMATETC;
  LockBytes: ILockBytes;
  ClientSite: IOleClientSite;
  Storage: IStorage;
  Image: IOleObject;
  c: TWin32Inline;
  Obj: TREOBJECT;
  sl, ss: Integer;
  eventmask: Integer;
begin
  Result:=False;
  if not Assigned(RichEditManager) or not Assigned(AWinControl) then Exit;

  hnd:=(AWinControl.Handle);

  RichEditManager.GetSelection(hnd, ss, sl);
  eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
  try
    RichEditManager.SetSelection(hnd, ATextStart, ATextLength);
    SendMessage(hnd, EM_GETOLEINTERFACE, 0, LPARAM(@rch));

    FillChar(Fmt, sizeoF(Fmt), 0);
    Fmt.dwAspect:=DVASPECT_CONTENT;
    Fmt.lindex:=-1;

    CreateILockBytesOnHGlobal(0, True, LockBytes);
    StgCreateDocfileOnILockBytes(LockBytes, STGM_SHARE_EXCLUSIVE or STGM_CREATE or STGM_READWRITE, 0, Storage);
    rch.GetClientSite(ClientSite);

    c:=TWin32Inline.Create;
    c.richMemo:=TCustomRichMemo(AWinControl);
    c.canvas:=TCanvas.Create;
    c.rminline:=AHandler;

    Image:=c;
    OleSetContainedObject(Image, True);

    FillChar(Obj, sizeof(Obj),0);
    Obj.cbStruct := SizeOf(Obj);
    Obj.cp := REO_CP_SELECTION;
    Image.GetUserClassID(Obj.clsid);
    Obj.poleobj := Image;
    Obj.pstg := Storage;
    Obj.polesite := ClientSite;
    Obj.dvaspect := DVASPECT_CONTENT;
    Obj.dwFlags := InsertInlineFlags;

    Obj.sizel.cx:=round(ASize.cx * SizeFactor);
    Obj.sizel.cy:=round(ASize.cy * SizeFactor);

    Result:= Succeeded(rch.InsertObject(obj));
    if Result then wsObj:=c;
  finally
    RichEditManager.SetSelection(hnd, ss, sl);
    RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
  end;
end;

class procedure TWin32WSCustomRichMemo.InlineInvalidate(
  const AWinControl: TWinControl; AHandler: TRichMemoInline;
  wsObj: TRichMemoInlineWSObject);
begin
  //inherited InlineInvalidate(AWinControl, AHandler, wsObj);
  if not Assigned(AHandler) or not Assigned(wsObj) or (not (wsObj is TWin32Inline)) then Exit;
  if not Assigned(TWin32Inline(wsObj).fSink) then Exit;
  TWin32Inline(wsObj).fSink.OnViewChange(DVASPECT_CONTENT, -1);
end;

type
  TPrintRichMemo = class(TCustomRichMemo)
  end;

class function TWin32WSCustomRichMemo.Print(const AWinControl: TWinControl;
  APrinter: TPrinter;
  const AParams: TPrintParams; DoPrint: Boolean): Integer;
var
  Rng         : TFormatRange;
  Ofs, MaxLen : Integer;
  LogX, LogY  : Integer;
  OldMap      : Integer;
  SaveRect    : TRect;
  hnd         : THandle;
  hdc         : Windows.HDC;
  PrCh        : Integer;
  maxch       : Integer;

  fixedRange  : Boolean;
  eventMask   : LongWord;
  rm          : TPrintRichMemo;
  doAbort     : Boolean;
const
  PrintFlag : array [Boolean] of byte = (FORMAT_ESTIMATE, FORMAT_RENDER);
begin
  Result:=0;
  if not Assigned(RichEditManager) or not Assigned(AWinControl)
    or not (AWinControl is TCustomRichMemo) then Exit;

  hnd:=(AWinControl.Handle);
  if (hnd=0) or (hnd=INVALID_HANDLE_VALUE) then Exit;
  rm:=TPrintRichMemo(AWinControl);

  FillChar(Rng, SizeOf(Rng), 0);

  if DoPrint then begin
    APrinter.Title:=AParams.JobTitle;
    APrinter.BeginDoc;
  end;

  fixedRange:=false;
  try
    if DoPrint then begin
      hdc:=APrinter.Canvas.Handle;
      Rng.hdc:=hdc;
      Rng.hdcTarget:=hdc;
    end else begin
      Rng.hdc:=GetDC(hnd);
      Rng.hdcTarget:=rng.hdc;
    end;
    LogX:=APrinter.XDPI;
    LogY:=APrinter.YDPI;
    if (LogX=0) or (LogY=0) then Exit;

    Rng.rcPage:=Bounds( 0, 0
      ,round(APrinter.PageWidth  / LogX * TWIP_INCH)
      ,round(APrinter.PageHeight / LogY * TWIP_INCH)
    );

    Rng.rc.left   := round(AParams.Margins.Left   * TWIP_PT);
    Rng.rc.top    := round(AParams.Margins.Top    * TWIP_PT);
    Rng.rc.right  := round(Rng.rcPage.Right - AParams.Margins.Right * TWIP_PT);
    Rng.rc.bottom := round(Rng.rcPage.Bottom - AParams.Margins.Bottom * TWIP_PT);
    SaveRect:=Rng.rc;

    if AParams.SelLength<=0 then begin
      Ofs:=0;
      MaxLen:=RichEditManager.GetTextLength(hnd);

    end else begin
      if AParams.SelStart<0 then Ofs:=0
      else Ofs:=AParams.SelStart;
      MaxLen:=AParams.SelLength;

      if FixPrintSelRange  then begin
        fixedRange:=true;
        Windows.SendMessage(hnd, WM_SETREDRAW, WPARAM(false), 0);
        eventmask:=RichEditManager.SetEventMask(hnd, 0);
        RichEditManager.SetText(hnd,#10,Ofs+MaxLen,0);
        RichEditManager.SetEventMask(hnd, eventmask);
      end;
    end;
    maxch:=Ofs+MaxLen;

    OldMap := SetMapMode(hdc, MM_TEXT);
    SendMessage(hnd, EM_FORMATRANGE, 0, 0);
    try
      Result:=1;
      doAbort:=false;
      rm.DoPrintAction(paDocStart, APrinter.Canvas, Result, doAbort);
      if not doAbort then begin
        repeat
          rm.DoPrintAction(paPageStart, APrinter.Canvas, Result, doAbort);
          if doAbort then break;

          Rng.rc := SaveRect;
          Rng.chrg.cpMin := Ofs;
          Rng.chrg.cpMax := maxch;
          PrCh := Ofs;
          Ofs := SendMessage(hnd, EM_FORMATRANGE, PrintFlag[DoPrint], LPARAM(@Rng));
          if (Ofs < MaxLen) and (Ofs <> -1) then begin
             if DoPrint then begin
               rm.DoPrintAction(paPageEnd, APrinter.Canvas, Result, doAbort);
               inc(Result);
               if not doAbort then APrinter.NewPage;
             end else
               inc(Result);
          end;

        until (Ofs >= MaxLen) or (Ofs = -1) or (PrCh = Ofs) or doAbort;

        if not doAbort then begin
          rm.DoPrintAction(paPageEnd, APrinter.Canvas, Result, doAbort);
          if not doAbort then
            rm.DoPrintAction(paDocEnd, APrinter.Canvas, Result, doAbort);
         end;
        if doAbort then APrinter.Abort;
      end;

    finally
      SendMessage(hnd, EM_FORMATRANGE, 0, 0);
      SetMapMode(hdc, OldMap);
    end;

  finally
    if fixedRange then begin
      eventmask:=RichEditManager.SetEventMask(AWinControl.Handle, 0);
      RichEditManager.SetText(AWinControl.Handle,'',maxch,1);
      RichEditManager.SetEventMask(AWinControl.Handle, eventmask);
      Windows.SendMessage(hnd, WM_SETREDRAW, WPARAM(not false), 0);
    end;

    if DoPrint and not APrinter.Aborted and not doAbort then
      APrinter.EndDoc
    else
      ReleaseDC(hnd, Rng.hdc);
  end;
end;

class procedure TWin32WSCustomRichMemo.Redo(const AWinControl: TWinControl);
begin
  SendMessage( AWinControl.Handle, EM_REDO, 0, 0);
end;

class function TWin32WSCustomRichMemo.GetCanRedo(const AWinControl: TWinControl
  ): Boolean;
begin
  Result:=SendMessage( AWinControl.Handle, EM_CANREDO, 0, 0)<>0;
end;

class procedure TWin32WSCustomRichMemo.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
var
  pt : TPoint;
begin
  if not Assigned(AWinControl) or not (AWinControl.HandleAllocated) or ((DeltaX=0) and (DeltaY=0)) then Exit;

  RichEditManager.GetScroll(AWinControl.Handle, pt);
  dec(pt.x,DeltaX);
  dec(pt.y,Deltay);
  RichEditManager.SetScroll(AWinControl.Handle, pt);
end;

class procedure TWin32WSCustomRichMemo.SetTransparentBackground(
  const AWinControl: TWinControl; ATransparent: Boolean);
var
  l  : Windows.Long;
  nl : Windows.Long;
begin
  if not Assigned(AWinControl) or not (AWinControl.HandleAllocated) then Exit;

  // Make sure there's TImage resides on the same form as RichMemo
  l := GetWindowLong(AWinControl.Handle, GWL_EXSTYLE);
  if ATransparent then
    nl := l or WS_EX_TRANSPARENT
  else
    nl := l and not WS_EX_TRANSPARENT;
  if nl <> l then
    SetWindowLong(AWinControl.Handle, GWL_EXSTYLE, nl);
end;

// The function doesn't use Windows 7 (Vista?) animations. And should.
function ThemedNCPaint(AWindow: Windows.HANDLE; RichMemo: TCustomRichMemo; WParam: WParam; LParam: LParam; var Handled: Boolean): LResult;
begin
  // When theming is enabled, and the component should have a border around it,
  // let the theme manager handle it
  Handled:=(GetWindowLong(AWindow, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0) and (ThemeServices.ThemesEnabled);
  Result := 0;
  if Handled then begin
    // Paint into this DC
    WindowProc(AWindow, WM_NCPAINT, WParam, LParam);
    ThemeServices.PaintBorder(RichMemo, True);
  end;
end;

type
  TStreamText = record
    buf : AnsiString;
  end;
  PStreamText = ^TStreamText;

{$IF FPC_FULLVERSION>=30202}
function Read(dwCookie:DWORD_PTR; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ELSE}
function Read(dwCookie:PDWORD; pbBuff:LPBYTE; cb:LONG; var pcb:LONG):DWORD; stdcall;
{$ENDIF}
var
  //p : PStreamText;
  b : string;
  i : integer;
begin
  b:=PStreamText(dwCookie)^.buf;
  i:=length(b);
  SetLength(b, length(b)+cb);
  Move(pbBuff^, b[i+1], cb);
  pcb:=cb;
  PStreamText(dwCookie)^.buf:=b;
  Result:=0;
end;

type
  _editstream = record
     dwCookie : PTRUINT;
     dwError : DWORD;
     pfnCallback : EDITSTREAMCALLBACK;
  end;

function GetSelRTF(amemo: TCustomRichMemo): string;
var
  str : _EDITSTREAM;
  tt  : TStreamText;
begin
  if not Assigned(amemo) or (not amemo.HandleAllocated) then begin
    Result:='';
    Exit;
  end;

  FillChar(str, sizeof(str),0);
  str.dwCookie:=PtrUInt(@tt);
  str.pfnCallback:=@Read;

  SendMessage( amemo.Handle, EM_STREAMOUT, SF_RTFNOOBJS or SFF_PLAINRTF or SFF_SELECTION,  LParam(@str));
  Result:=tt.buf;
end;

function GetRichEditOLE(amemo: TCustomRichMemo): IRichEditOle;
begin
  if Assigned(amemo) then Result:=GetRichEditOle(amemo.Handle)
  else Result:=nil;
end;

function GetRichEditOLE(AHandle: THandle): IRichEditOle;
begin
  SendMessage(Ahandle, EM_GETOLEINTERFACE, 0, LPARAM(@Result));
end;

type
  { TRichEditCallback }

  TRichEditCallback = class(TInterfacedObject, IRichEditOleCallback)
  public
    fOwner: TWinControl;
    function GetNewStorage(out stg: IStorage): HRESULT; stdcall;
    function GetInPlaceContext(out Frame: IOleInPlaceFrame;
      out Doc: IOleInPlaceUIWindow;
      lpFrameInfo: POleInPlaceFrameInfo): HRESULT; stdcall;
    function ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage;
      cp: LongInt): HRESULT; stdcall;
    function DeleteObject(const oleobj: IOleObject): HRESULT; stdcall;
    function QueryAcceptData(const dataobj: IDataObject;
      var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL;
      hMetaPict: HGLOBAL): HRESULT; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
    function GetClipboardData(const chrg: RichEdit.TCharRange; reco: DWORD;
      out dataobj: IDataObject): HRESULT; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
      var dwEffect: DWORD): HRESULT; stdcall;
    function GetContextMenu(seltype: Word; oleobj: IOleObject;
      const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
  end;

{ TRichEditCallback }

function TRichEditCallback.GetNewStorage(out stg: IStorage): HRESULT; stdcall;
begin
  StgCreateDocfile(nil, STGM_READWRITE or STGM_SHARE_EXCLUSIVE, 0,stg);
  Result := S_OK;
end;

function TRichEditCallback.GetInPlaceContext(out Frame: IOleInPlaceFrame; out
  Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HRESULT;
  stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.ShowContainerUI(fShow: BOOL): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.QueryInsertObject(const clsid: TCLSID;
  const stg: IStorage; cp: LongInt): HRESULT; stdcall;
begin
  Result := S_OK;
end;

function TRichEditCallback.DeleteObject(const oleobj: IOleObject): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.QueryAcceptData(const dataobj: IDataObject;
  var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL
  ): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.ContextSensitiveHelp(fEnterMode: BOOL): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD; out
  dataobj: IDataObject): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD;
  var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TRichEditCallback.GetContextMenu(seltype: Word; oleobj: IOleObject;
  const chrg: TCharRange; var menu: HMENU): HRESULT; stdcall;
var
  msg : TLMContextMenu;
begin
  FillChar(msg, sizeof(msg), 0);
  msg.Msg:=LM_CONTEXTMENU;
  msg.XPos:=Mouse.CursorPos.x;
  msg.YPos:=Mouse.CursorPos.y;
  msg.hWnd:=fOwner.Handle;
  DeliverMessage(fOwner, msg);

  // do not give hmenu back to RichEdit, it will destory it!
  menu:=0;
  Result:=S_OK;
end;



procedure DefAllocOleObject(ARichMemo: TCustomRichMemo; AHandle: Windows.THandle; out OleCallback: IRichEditOleCallback);
var
  cb : TRichEditCallback;
begin
  cb:=TRichEditCallback.Create;
  cb.fOwner:=ARichMemo;
  OleCallBack:=cb;
end;

function GetOleObject(ole: IRichEditOle; SelStart: Integer; out res: TREOBJECT): Boolean;
begin
  Result:=Assigned(ole);
  if not Result then Exit;
  FillChar(res{%H-}, sizeof(res), 0);
  res.cbStruct:=sizeof(res);
  res.cp:=SelStart;

  Result:=ole.GetObject(REO_IOB_USE_CP, res, REO_GETOBJ_ALL_INTERFACES)=S_OK;
end;

function SetOleObjectSize(ARichMemo: TCustomRichMemo; SelStart: Integer; const ASize: TSize): Boolean;
var
  obj : TREOBJECT;
  ole : IRichEditOle;
  ss  : integer;
  sl  : integer;
begin
  ole:=GetRichEditOLE(ARichMemo);
  if not Assigned(ole) then begin
    Result:=false;
    Exit;
  end;
    Result:=GetOleObject(ole, SelStart, obj);
    if Result then begin
      ARichMemo.Lines.BeginUpdate;
      ss:=ARichMemo.SelStart;
      sl:=ARichMemo.SelLength;
      try
        obj.polesite:=nil;
        ARichMemo.SelStart:=SelStart;
        ARichMemo.SelLength:=1;
        ARichMemo.SelText:='';
        ole.GetClientSite(obj.polesite);
        obj.sizel.cx:=round(ASize.cx * SizeFactor);
        obj.sizel.cy:=round(ASize.cy * SizeFactor);
        Result:=ole.InsertObject(obj)=S_OK;
      finally
        ARichMemo.SelStart:=ss;
        ARichMemo.SelLength:=sl;
        ARichMemo.Lines.EndUpdate;
      end;
    end;
end;

function GetOleObjectSize(ARichMemo: TCustomRichMemo; SelStart: Integer; var ASize: TSize): Boolean;
var
  res : TREOBJECT;
  ole : IRichEditOle;
begin
  ole:=GetRichEditOLE(ARichMemo);
  Result:=Assigned(ole) and (GetOleObject(ole, SelStart, res));
  if not Result then Exit;
  ASize.cx:=round(res.sizel.cx*RevSizeFactor);
  ASize.cy:=round(res.sizel.cy*RevSizeFactor);
end;
(*
function GetLangOptions(hnd: THandle): TWinLangOptions;
var
  r: LRESULT;
begin
  r:=SendMessage(hnd, EM_GETLANGOPTIONS, 0, 0);
  Result:=TWinLangOptions(r);
end;

function GetLangOptions(rm: TCustomRichMemo): TWinLangOptions;
begin
  if Assigned(rm) then begin
    if not rm.HandleAllocated then rm.HandleNeeded;
    if rm.HandleAllocated
      then Result:=GetLangOptions(rm.Handle)
      else Result:=[];
  end else
    Result:=[];
end;

procedure SetLangOptions(hnd: THandle; const opts: TWinLangOptions); overload;
var
  lp : LPARAM;
begin
  lp:=LPARAM(opts);
  SendMessage(hnd, EM_SETLANGOPTIONS, 0, lp);
end;

procedure SetLangOptions(rm: TCustomRichMemo; const opts: TWinLangOptions); overload;
begin
  if Assigned(rm) then begin
    if not rm.HandleAllocated then rm.HandleNeeded;
    if rm.HandleAllocated then
      SetLangOptions(rm.Handle, opts);
  end;
end;
*)
initialization
  NCPaint := @ThemedNCPaint;
  AllocOLEObject := @DefAllocOleObject;
 
end.

