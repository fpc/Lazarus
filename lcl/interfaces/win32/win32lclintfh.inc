{%MainUnit win32int.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to win32proc.inc

 ******************************************************************************
 Interface
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

function AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler; override;
function AddPipeEventHandler(AHandle: THandle; 
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler; override;
function AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler; override;
function AllocateHWnd(Method: TLCLWndMethod): HWND; override;
function AskUser(const DialogCaption, DialogMessage: string; DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt; override;

function CreateStandardCursor(ACursor: SmallInt): hCursor; override;
function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND; override;

procedure DeallocateHWnd(Wnd: HWND); override;
procedure DestroyRubberBand(ARubberBand: HWND); override;
procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation); override;
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean; override;
function FontIsMonoSpace(Font: HFont): boolean; override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetDesignerDC(WindowHandle: HWND): HDC; override;

function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; override;

function PromptUser(const DialogCaption, DialogMessage: String; DialogType : longint; Buttons : PLongint; ButtonCount, DefaultIndex, EscapeResult : Longint) : Longint; override;

function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean; override;
function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;

function ReleaseDesignerDC(Window: HWND; DC: HDC): Integer; override;
procedure RemoveEventHandler(var AHandler: PEventHandler); override;
procedure RemovePipeEventHandler(var AHandler: PPipeEventHandler); override;
procedure RemoveProcessEventHandler(var AHandler: PProcessEventHandler); override;

procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
