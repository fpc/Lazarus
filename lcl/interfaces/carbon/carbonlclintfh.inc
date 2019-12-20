{%MainUnit carbonint.pas}

{******************************************************************************
  All Carbon interface communication implementations.
  This are the overrides of the Carbon Interface for the methods defined in the
  lcl/include/lclintfh.inc

  !! Keep alphabetical !!

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

function CreateStandardCursor(ACursor: SmallInt): hCursor; override;
procedure DrawGrid(DC: HDC; const R: TRect; DX, DY: Integer); override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetDesignerDC(WindowHandle: HWND): HDC; override;
function GetLCLOwnerObject(Handle: HWnd): TObject; override;

function IsDesignerDC(WindowHandle: HWND; DC: HDC): Boolean; override;

{procedure LazDeviceAPIs_RequestPositionInfo(AMethod: TLazPositionMethod); override;
procedure LazDeviceAPIs_SendMessage(AMsg: TLazDeviceMessage); override;
procedure LazDeviceAPIs_StartReadingAccelerometerData(); override;
procedure LazDeviceAPIs_StopReadingAccelerometerData(); override;}

function PromptUser(const DialogCaption : string;
                    const DialogMessage : string;
                          DialogType    : LongInt;
                          Buttons       : PLongInt;
                          ButtonCount   : LongInt;
                          DefaultIndex  : LongInt;
                          EscapeResult  : LongInt) : LongInt; override;

function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean = False): Boolean; override;
function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
// override only when queried formats are different from screen description
//function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;

function ReleaseDesignerDC(Window: HWND; {%H-}DC: HDC): Integer; override;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
