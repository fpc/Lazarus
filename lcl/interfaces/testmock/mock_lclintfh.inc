{%MainUnit TestMockInt}

(******************************************************************************
  All interface communication related MOCKING goes here.

  This file is based on lclintfh.inc.inc

  !! Keep this alphabetical and place a pointer to the independent part
  !! if needed

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 ******************************************************************************)

{******************************************************************************
  Platform specific stuff

  Note:
    the IFNDEF IF_BASE_MEMBER section is at the end
 ******************************************************************************}
//##apiwiz##sps##    // Do not remove

function AddEventHandler(AHandle: TLCLHandle; AFlags: dword; AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler; override;
function AddProcessEventHandler(AHandle: TLCLHandle; AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler; override;
function AddPipeEventHandler(AHandle: TLCLHandle; AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler; override;
function AllocateHWnd(Method: TLCLWndMethod): HWND; override;
function AskUser(const DialogCaption, DialogMessage: string; DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt; override;

procedure CallDefaultWndHandler(Sender: TObject; var Message); override;
function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; override;
function ClipboardGetData(ClipboardType: TClipboardType;
  FormatID: TClipboardFormat; Stream: TStream): boolean; override;
// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function ClipboardGetFormats(ClipboardType: TClipboardType;
  var Count: integer; var List: PClipboardFormat): boolean;  override;
function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
  Formats: PClipboardFormat): boolean; override;
function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; override;
function ClipboardFormatNeedsNullByte(const AFormat: TPredefinedClipboardFormat): Boolean; override;
function CreateEmptyRegion: hRGN; override;
function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; override;
function CreateRegionCopy(SrcRGN: hRGN): hRGN; override;
function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND; override;
function CreateStandardCursor(ACursor: SmallInt): hCursor; override;

function DCClipRegionValid(DC: HDC): boolean; override;
procedure DeallocateHWnd(Wnd: HWND); override;
procedure DestroyRubberBand(ARubberBand: HWND); override;
procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation); override;
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  override;
function FontIsMonoSpace(Font: HFont): boolean; override;
function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TGraphicsBevelCut): Boolean; override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetAvailableNativeCanvasTypes(DC: HDC; AAllowFallbackToParent: Boolean = False): TNativeCanvasTypes; override;
function GetAvailableNativeHandleTypes(Handle: HWND; AAllowFallbackToParent: Boolean = False): TNativeHandleTypes; override;
function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean; override;
function GetClientBounds(handle : HWND; var ARect: TRect) : Boolean; override;
function GetCmdLineParamDescForInterface: TStringList; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; override;
function GetDesignerDC(WindowHandle: HWND): HDC; override;
function GetDeviceSize(DC: HDC; var p: TPoint): boolean; override;
function GetLCLOwnerObject(Handle: HWnd): TObject; override;
function GetNativeCanvas(DC: HDC; AHandleType: TNativeCanvasType; AAllowFallbackToParent: Boolean = False): PtrInt; override;
function GetNativeHandle(Handle: HWND; AHandleType: TNativeHandleType; AAllowFallbackToParent: Boolean = False): PtrInt; override;
function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer; override;
function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean; override;
function GetWindowRelativePosition(Handle : hwnd; var Left, Top: integer): boolean; override;

function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean; override;
function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; override;
function IsMobilePlatform: Boolean; override;
function IsCDIntfControl(AWinControl: TObject): Boolean; override;

function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean; override;

function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;override;
function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;override;

function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey : Integer): Boolean; override;
function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey : Integer): Boolean; override;
function RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean; override;
function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean; override;
function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;
function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; override;
function ReleaseDesignerDC(hWnd: HWND; DC: HDC): Integer;  override;
procedure RemoveEventHandler(var AHandler: PEventHandler); override;
procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler); override;
procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler); override;
function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; override;

procedure SendCachedLCLMessages; override;
procedure SetCanvasScaleFactor(DC: HDC; const AScaleFactor: double); override;
function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; override;
function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; override;
procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword); override;
procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); override;
function ShowSelectItemDialog(const AItems: TStrings; APos: TPoint): Boolean;override;
function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; override;

function TextUTF8Out(DC: HDC; X, Y: Integer;  Str: PChar; Count: Longint): Boolean;  override;

//##apiwiz##eps##    // Do not remove

{******************************************************************************
  Platform independent stuff
 ******************************************************************************}
//
//{$IFNDEF IF_BASE_MEMBER}
//
////##apiwiz##spi##    // Do not remove
//
//function ExcludeClipRgn(dc: hdc; rgn : hrgn) : Integer;
//
//
//procedure NotifyUser(const DialogMessage : String; DialogType : longint);
//procedure NotifyUser(const DialogCaption, DialogMessage : String; DialogType : longint);
//procedure NotifyUserAtXY(const DialogMessage : String; DialogType : longint; X, Y : Longint);
//procedure NotifyUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; X, Y : Longint);
//
//function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
//function PromptUser(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint;
//function PromptUser(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint) : Longint;
//function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
//function PromptUserAtXY(const DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
//function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : Array of Longint; DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;
//
////##apiwiz##epi##    // Do not remove
//
//{$ENDIF}
//
