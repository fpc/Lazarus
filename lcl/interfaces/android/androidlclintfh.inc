{%MainUnit androidint.pas}
{ $Id: qtlclintfh.inc 28454 2010-11-24 13:59:27Z paul $ }

{******************************************************************************
                  All QT interface communication implementations.
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

 ******************************************************************************
 Interface
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

(*
function AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler; override;
function AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler; override;
function AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler; override;
function AskUser(const DialogCaption, DialogMessage: string; DialogType:
   LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt; override;

//procedure CallDefaultWndHandler(Sender: TObject; var Message); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ClipboardGetData(ClipboardType: TClipboardType;
//  FormatID: TClipboardFormat; Stream: TStream): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
/// ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
//function ClipboardGetFormats(ClipboardType: TClipboardType;
//  var Count: integer; var List: PClipboardFormat): boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ClipboardGetOwnerShip(ClipboardType: TClipboardType;
//  OnRequestProc: TClipboardRequestEvent;  FormatCount: integer;
//  Formats: PClipboardFormat): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateEmptyRegion: hRGN; override;
//function CreatePixmapIndirect(const Data: Pointer; const TransColor: Longint): HBITMAP; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function CreateRegionCopy(SrcRGN: hRGN): hRGN; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND; override;
function CreateStandardCursor(ACursor: SmallInt): HCURSOR; override;

//function DCClipRegionValid(DC: HDC): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//procedure DeallocateHWnd(Wnd: HWND); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
procedure DestroyRubberBand(ARubberBand: HWND); override;
procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation); override;
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;

//function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function FontCanUTF8(Font: HFont): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function FontIsMonoSpace(Font: HFont): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function Frame(DC: HDC; const ARect: TRect): Integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function Frame3d(DC: HDC; var ARect: TRect; const FrameWidth : integer; const Style : TGraphicsBevelCut): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function FontCanUTF8(Font: HFont): boolean; override;
function FontIsMonoSpace(Font: HFont): boolean; override;

//function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetCaretRespondToFocus(handle: HWND; var ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetClientBounds(handle : HWND; var ARect: TRect) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetCmdLineParamDescForInterface: string; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetControlConstraints(Constraints: TObject): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetDCOriginRelativeToWindow(PaintDC: HDC; WindowHandle: HWND; var OriginDiff: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function GetDesignerDC(WindowHandle: HWND): HDC; override;
///fu/nction GetDeviceSize(DC: HDC; var p: TPoint): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetLCLOwnerObject(Handle: HWnd): TObject; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetScrollBarSize(Handle: HWND; SBStyle: Integer): integer; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetScrollbarVisible(Handle: HWND; SBStyle: Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function GetWindowRelativePosition(Handle : hwnd; var Left, Top: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function IntfSendsUTF8KeyPress: boolean; override;
function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; override;
//function InvalidateFrame(aHandle : HWND; ARect : pRect; bErase : Boolean; BorderWidth: integer) : Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

//function MoveWindowOrgEx(dc : hdc; dX,dY : Integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function PromptUser(const DialogCaption : string;
                    const DialogMessage : string;
                          DialogType    : LongInt;
                          Buttons       : PLongInt;
                          ButtonCount   : LongInt;
                          DefaultIndex  : LongInt;
                          EscapeResult  : LongInt) : LongInt; override;
//function PromptUserAtXY(const DialogCaption, DialogMessage : String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint; X, Y : Longint) : Longint;{$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}


//function RadialArc(DC: HDC; left, top, right, bottom, sx, sy, ex, ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function RadialChord(DC: HDC; x1, y1, x2, y2, sx, sy, ex, ey : Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean; override;
function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;
//function RegroupMenuItem(hndMenu: HMENU; GroupIndex: integer): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
function ReleaseDesignerDC(Window: HWND; DC: HDC): Integer; override;
procedure RemoveEventHandler(var AHandler: PEventHandler); override;
procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler); override;
procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler); override;*)
function RequestInput(const InputCaption, InputPrompt : String; MaskInput : Boolean; var Value : String) : Boolean; override;

(*procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword); override;
procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); override;
//procedure SendCachedLCLMessages; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetCaretRespondToFocus(handle: HWND; ShowHideOnFocus: boolean): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function SetComboMinDropDownSize(Handle: HWND; MinItemsWidth, MinItemsHeight, MinItemCount: integer): boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}
//function StretchMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc, SrcWidth, SrcHeight: Integer; Mask: HBITMAP; XMask, YMask: Integer; Rop: DWORD): Boolean; {$IFDEF IF_BASE_MEMBER}virtual;{$ENDIF}

function TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean; override;*)

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
