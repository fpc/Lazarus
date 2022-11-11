{%MainUnit qtint.pp}
{ $Id: qtlclintfh.inc 46199 2014-09-12 07:16:53Z zeljko $ }

{******************************************************************************
                  All QT interface communication implementations.
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

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

procedure SocketNotifierRead_cb(aSocket: Integer); cdecl; // internal callback, don't use!
procedure SocketNotifierWrite_cb(aSocket: Integer); cdecl; // internal callback, don't use!
procedure SocketNotifierError_cb(aSocket: Integer); cdecl; // internal callback, don't use!
function AddEventHandler(AHandle: THandle; AFlags: dword;
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler; override;
function AddPipeEventHandler(AHandle: THandle;
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler; override;
function AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler; override;
function AskUser(const DialogCaption, DialogMessage: string; DialogType:
   LongInt; Buttons: TDialogButtons; HelpCtx: Longint): LongInt; override;

function CreateEmptyRegion: hRGN; override;
function CreateStandardCursor(ACursor: SmallInt): HCURSOR; override;
function CreateRubberBand(const ARect: TRect; const ABrush: HBrush = 0): HWND; override;
procedure DrawDefaultDockImage(AOldRect, ANewRect: TRect; AOperation: TDockImageOperation); override;
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;
procedure DestroyRubberBand(ARubberBand: HWND); override;

function ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;  override;
function FontIsMonoSpace(Font: HFont): boolean; override;

function GetDesignerDC(WindowHandle: HWND): HDC; override;
function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; override;

function PromptUser(const DialogCaption : string;
                    const DialogMessage : string;
                          DialogType    : LongInt;
                          Buttons       : PLongInt;
                          ButtonCount   : LongInt;
                          DefaultIndex  : LongInt;
                          EscapeResult  : LongInt) : LongInt; override;

function RadialPie(DC: HDC; x1, y1, x2, y2, Angle1, Angle2: Integer): Boolean; override;

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

procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword); override;
procedure SetRubberBandRect(const ARubberBand: HWND; const ARect: TRect); override;

function TextUTF8Out(DC: HDC; X, Y: Integer; Str: PChar; Count: Longint): Boolean; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
