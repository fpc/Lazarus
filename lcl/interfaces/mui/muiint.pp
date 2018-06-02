{
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

unit MUIInt;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  // rtl+fcl
  agraphics, Types, Classes, SysUtils, FPCAdds, Math,
  // interfacebase
  InterfaceBase,
  // LCL
  lclplatformdef, Dialogs, Controls, Forms, LCLStrConsts, LMessages, stdctrls,
  LCLProc, LCLIntf, LCLType, GraphType, Graphics, Menus, Themes, muithemes,
  // Amiga units
  MUIBaseUnit, MUIFormsUnit, muidrawing, tagsparamshelper, muiglobal,
  {$ifdef HASAMIGA}
  exec, intuition, mui, utility, AmigaDos, icon,
  cybergraphics,
  inputevent, Cliputils,
  {$endif}
  // widgetset
  WSLCLClasses, LCLMessageGlue;

const
  IdButtonTexts: array[idButtonOk..idButtonShield] of string = (
 { idButtonOk       } 'OK',
 { idButtonCancel   } 'Cancel',
 { idButtonHelp     } 'Help',
 { idButtonYes      } 'Yes',
 { idButtonNo       } 'No',
 { idButtonClose    } 'Close',
 { idButtonAbort    } 'Abort',
 { idButtonRetry    } 'Retry',
 { idButtonIgnore   } 'Ignore',
 { idButtonAll      } 'All',
 { idButtonYesToAll } 'YesToAll',
 { idButtonNoToAll  } 'NoToAll',
 { idButtonOpen     } 'Open',
 { idButtonSave     } 'Save',
 { idButtonShield   } 'Shield'
  );
type
  { TMUIWidgetSet }

  TMUIWidgetSet = class(TWidgetSet)
  protected
    ThisAppDiskIcon: Pointer;
    function CreateThemeServices: TThemeServices; override;
    function GetAppHandle: THandle; override;
  public
    procedure PassCmdLineOptions; override;
  public
    function LCLPlatform: TLCLPlatform; override;
    function GetLCLCapability(ACapability: TLCLCapability):PtrUInt; override;
    // Application
    procedure AppInit(var ScreenInfo: TScreenInfo); override;
    procedure AppProcessMessages; override;
    procedure AppWaitMessage; override;
    procedure AppTerminate; override;
    procedure AppMinimize; override;
    procedure AppRestore; override;
    procedure AppBringToFront; override;
    procedure AppSetTitle(const ATitle: string); override;
    function EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont; Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint; override;
    //function MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;  uType: Cardinal): Integer; override;
    function PromptUser(const DialogCaption: String; const DialogMessage: String; DialogType: LongInt; Buttons: PLongint; ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt):LongInt; override;
    function RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean = false):Boolean; override;
    function RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): boolean; override;
    function RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean; override;
    function RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean; override;
    function RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean; override;
    function RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean; override;
    function  DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor; override;
    procedure DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor); override;
    function CreateStandardCursor(ACursor: SmallInt): hCursor; override;
    // Clipboard
    function ClipboardFormatToMimeType(FormatID: TClipboardFormat): string; override;
    function ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): boolean; override;
    // ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
    function ClipboardGetFormats(ClipboardType: TClipboardType; var Count: integer; var List: PClipboardFormat): boolean; override;
    function ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent;  FormatCount: integer; Formats: PClipboardFormat): boolean; override;
    function ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    // debugging
    procedure DebugOutEvent(Sender: TObject;s: string; var Handled: Boolean);
    procedure DebugOutLNEvent(Sender: TObject;s: string; var Handled: Boolean);

    // create and destroy
    function CreateTimer(Interval: integer; TimerFunc: TWSTimerProc) : THandle; override;
    function DestroyTimer(TimerHandle: THandle) : boolean; override;
    procedure DestroyLCLComponent(Sender: TObject);virtual;

    {$I muiwinapih.inc}
  public
  end;

var
  MUIWidgetSet: TMUIWidgetSet;
  FocusWidget: Hwnd;
implementation

uses
  MUIWSFactory, MUIWSForms, VInfo, muistdctrls, lazloggerbase;


{$I muiwinapi.inc}

{ TMUIWidgetSet }

function TMUIWidgetSet.GetAppHandle: THandle;
begin
  Result := THandle(MUIApp);
end;

procedure TMUIWidgetSet.PassCmdLineOptions;
begin
  inherited PassCmdLineOptions;
end;

function TMUIWidgetSet.LCLPlatform: TLCLPlatform;
begin
  Result:=lpMUI;
end;

function TMUIWidgetSet.GetLCLCapability(ACapability: TLCLCapability): PtrUInt;
begin
  case ACapability of
    lcCanDrawOutsideOnPaint: Result := LCL_CAPABILITY_NO;
    lcDragDockStartOnTitleClick: Result := LCL_CAPABILITY_NO;
    lcNeedMininimizeAppWithMainForm: Result := LCL_CAPABILITY_NO;
    lcAsyncProcess: Result := LCL_CAPABILITY_NO;
    lcApplicationTitle: Result := LCL_CAPABILITY_YES;
    lcApplicationWindow:Result := LCL_CAPABILITY_YES;
    lcFormIcon: Result := LCL_CAPABILITY_NO;
    lcModalWindow: Result := LCL_CAPABILITY_NO;
    lcAntialiasingEnabledByDefault: Result := LCL_CAPABILITY_NO;
    lcLMHelpSupport: Result := LCL_CAPABILITY_NO;
    lcSendsUTF8KeyPress: Result := LCL_CAPABILITY_NO;
  else
    Result := inherited GetLCLCapability(ACapability);
  end;
end;

var
  // MUI does not copy this values, so we keep them here
  AppTitle, FinalVers, Vers, CopyR, Comment, PrgName, Author: string;

procedure TMUIWidgetSet.DebugOutEvent(Sender: TObject;s: string; var Handled: Boolean);
begin
  SysDebugln('(LCL:'+Sender.classname+'): '+ s);
  Handled := True;
end;

procedure TMUIWidgetSet.DebugOutLNEvent(Sender: TObject;s: string; var Handled: Boolean);
begin
  SysDebugln('(LCL:'+Sender.classname+'): '+ s);
  Handled := True;
end;

procedure TMUIWidgetSet.AppInit(var ScreenInfo: TScreenInfo);
type
  TVerArray = array[0..3] of Word;
var
  Info: TVersionInfo;
  i,j: Integer;
  TagList: TATagList;
  Dollar: string;

  function PV2Str(PV: TVerArray): String;
   begin
     Result := SysUtils.Format('%d.%d.%d.%d', [PV[0],PV[1],PV[2],PV[3]])
   end;

begin
  // connect Debug log output
  DebugLogger.OnDbgOut := @DebugOutEvent;
  DebugLogger.OnDebugLn := @DebugOutLNEvent;
  // Initial Application Values
  Vers := '';
  CopyR := '';
  Comment := '';
  Dollar := '$';
  // Get the name from Application.Title, remove the Path Part
  PrgName := ExtractFilename(Application.Title);
  AppTitle := PrgName;
  // Miu can't handle empty AppTitle, use Exename
  if AppTitle = '' then
    AppTitle := ExtractFilename(ParamStr(0));
  // load Informations from resource
  Info := TVersionInfo.Create;
  try
    Info.Load(HINSTANCE);
    Vers := PV2Str(Info.FixedInfo.FileVersion);
    for i := 0 to Info.StringFileInfo.Count - 1 do
    begin
      for j := 0 to Info.StringFileInfo.Items[i].Count - 1 do
      begin
        if Info.StringFileInfo.Items[i].Keys[j] = 'LegalCopyright' then
          CopyR := Info.StringFileInfo.Items[i].Values[j]
        else
        if Info.StringFileInfo.Items[i].Keys[j] = 'Comments' then
          Comment := Info.StringFileInfo.Items[i].Values[j]
        else
        if Info.StringFileInfo.Items[i].Keys[j] = 'CompanyName' then
          Author := Info.StringFileInfo.Items[i].Values[j]
        else
        if Info.StringFileInfo.Items[i].Keys[j] = 'ProductName' then
        begin
          if Length(Trim(Info.StringFileInfo.Items[i].Values[j])) > 0  then
            PrgName := Info.StringFileInfo.Items[i].Values[j];
        end;
      end;
    end;
  except
  end;
  // end resource loading
  Info.Free;
  // get the Icon (to use as Iconify Image), nil is no problem, MUI handle that and use the default
  ThisAppDiskIcon := GetDiskObject(PChar(ParamStr(0)));
  // Version information as Standard AMIGA Version string
  FinalVers := Dollar + 'VER: ' + PrgName + ' ' + Vers + '('+{$I %DATE%}+')';
  // Create the Application
  TagList.AddTags([
    NativeInt(MUIA_Application_Base), NativeUInt(PChar(AppTitle)),
    MUIA_Application_DiskObject, NativeUInt(ThisAppDiskIcon),
    MUIA_Application_Title, NativeUInt(PChar(AppTitle)),
    MUIA_Application_Version, NativeUInt(PChar(FinalVers)),
    MUIA_Application_Copyright, NativeUInt(PChar(CopyR)),
    MUIA_Application_Description, NativeUInt(PChar(Comment)),
    MUIA_Application_Author, NativeUInt(PChar(Author))
    ]);
  MUIApp := TMuiApplication.Create(TagList);
  if not Assigned(MUIApp) or not Assigned(MUIApp.Obj) then
    raise EInvalidOperation.Create('Unable to Create Application object.');
  // same basic Screen info, no idea where to get that
  ScreenInfo.PixelsPerInchX := 72;
  ScreenInfo.PixelsPerInchY := 72;
  ScreenInfo.ColorDepth := 32;
end;

procedure TMUIWidgetSet.AppProcessMessages;
begin;
  MuiApp.ProcessMessages;
end;

procedure TMUIWidgetSet.AppWaitMessage;
begin
  MuiApp.WaitMessages;
end;

procedure TMUIWidgetSet.AppTerminate;
begin
  FreeDiskObject(ThisAppDiskIcon);
end;

procedure TMUIWidgetSet.AppMinimize;
begin
  MuiApp.Iconified := True;
end;

procedure TMUIWidgetSet.AppRestore;
begin
  MuiApp.Iconified := False;
end;

procedure TMUIWidgetSet.AppBringToFront;
begin

end;

procedure TMUIWidgetSet.AppSetTitle(const ATitle: string);
begin

end;

function TMUIWidgetSet.EnumFontFamiliesEx(DC: HDC; lpLogFont: PLogFont;
  Callback: FontEnumExProc; Lparam: LParam; Flags: dword): longint;
begin
  Result:=0;
end;

(*
function TMUIWidgetSet.MessageBox(hWnd: HWND; lpText: PChar; lpCaption: PChar;
  uType: Cardinal): Integer;
begin
end;*)

function TMUIWidgetSet.PromptUser(const DialogCaption: String;
  const DialogMessage: String; DialogType: LongInt; Buttons: PLongint;
  ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt): LongInt;
var
  ES: PEasyStruct;
  BtnText: string;
  Res: LongInt;
  BtnIdx : LongInt;
  BtnId: LongInt;
begin
  New(ES);
  ES^.es_StructSize := SizeOf(TEasyStruct);
  ES^.es_Flags := 0;
  ES^.es_Title := PChar(DialogCaption);
  ES^.es_TextFormat := PChar(DialogMessage);
  for BtnIdx := 0 to ButtonCount-1 do
  begin
    BtnID := Buttons[BtnIdx];
    if (BtnID >= Low(IdButtonTexts)) and (BtnID <= High(IdButtonTexts)) then
    begin
      if BtnIdx = 0 then
        BtnText := IdButtonTexts[BtnID]
      else
        BtnText := BtnText + '|'+ IdButtonTexts[BtnID];
    end else
    begin
      if BtnIdx = 0 then
        BtnText := IntToStr(BtnID)
      else
        BtnText := BtnText + '|'+ IntToStr(BtnID);
    end;
  end;
  ES^.es_GadgetFormat := PChar(BtnText);
  {$ifdef MorphOS}
  // App after MUI_RequestA is blocked
  Res := EasyRequestArgs(nil, ES, nil, nil);
  {$else}
  Res := MUI_RequestA(MuiApp.Obj, MuiApp.MainWin, 0, ES^.es_Title, ES^.es_GadgetFormat, ES^.es_TextFormat, nil);
  {$endif}
  Result := EscapeResult;
  Res := Res - 1;
  if Res < 0 then
    Res := ButtonCount - 1;
  if (Res >= 0) and (Res < ButtonCount) then
    Result := Buttons[Res];
  Dispose(ES);
end;

type
  TARGBPixel = packed record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PARGBPixel = ^TARGBPixel;

  {TABGRPixel = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;}
  TABGRPixel = array[0..3] of Byte;
  PABGRPixel = ^TABGRPixel;

{.$define VERBOSEAROS}

function TMUIWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out
  ABitmap: HBITMAP; out AMask: HBITMAP; ASkipMask: Boolean): Boolean;
var
  Bit: TMUIBitmap;
  //Ridx, GIdx, BIdx, AIdx: Byte;
begin
  {$ifdef VERBOSEAROS}
  writeln('RawImage_CreateBitmaps ' + IntToStr(ARawImage.Description.Width) + ' x ' + IntToStr(ARawImage.Description.Height) + ' - ' + IntToStr(ARawImage.Description.Depth) + ' = ' + IntToStr(ARawImage.DataSize));
  {$endif}
  Bit := TMUIBitmap.Create(ARawImage.Description.Width, ARawImage.Description.Height, ARawImage.Description.Depth);
  //ARawImage.Description.GetRGBIndices(Ridx, GIdx, BIdx, AIdx);
  //writeln('R: ',Ridx, ' G: ', GIdx, ' B: ', BIdx, ' A: ', AIdx);
  if ARawImage.DataSize > 0 then
    Move(ARawImage.Data^, Bit.FImage^, ARawImage.DataSize);
  //PLongWord(Bit.FImage)^ := $FFFFFFFF;
  ABitmap := HBITMAP(Bit);
  AMask := 0;
  Result := True;
  //writeln('created Bitmap: ', HexStr(Bit), ' width: ', Bit.FWidth, ' ??? ', ARawImage.Description.Width, ' Datasize: ', ARawImage.DataSize);
  //writeln(' create image: ', ARawImage.Description.Width,'x', ARawImage.Description.Height,' : ',ARawImage.Description.Depth, ' - ', ARawImage.DataSize, ' $', HexStr(Bit));
  //writeln('   Desc: ', HexStr(@(ARawImage.Description)));
end;

function RawImage_DescriptionFromDrawable(out
  ADesc: TRawImageDescription; ACustomAlpha: Boolean
  ): boolean;
var
  IsBitmap: Boolean;
begin
  {$ifdef VERBOSEAROS}
  writeln('RawImage_DescriptionFromDrawable');
  {$endif}
  //writeln('GetDescription from Drawable');
  IsBitMap := False;

  ADesc.Init;
  ADesc.Width := cardinal(0);
  ADesc.Height := cardinal(0);
  ADesc.BitOrder := riboBitsInOrder;
  ADesc.PaletteColorCount := 0;
  if ACustomAlpha then
  begin
    // always give pixbuf description for alpha images
    ADesc.Format:=ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitsPerPixel := 32;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.ByteOrder := riboLSBFirst;

    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 8;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 16;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;

    Exit(True);
  end;

  // Format
  if IsBitmap then
  begin
    ADesc.Format := ricfGray;
  end else
  begin
    ADesc.Format:=ricfRGBA;
    ADesc.RedPrec := 8;
    ADesc.RedShift := 0;
    ADesc.GreenPrec := 8;
    ADesc.GreenShift := 8;
    ADesc.BluePrec := 8;
    ADesc.BlueShift := 16;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;
  end;

  // Palette
  ADesc.PaletteColorCount:=0;

  // Depth
  if IsBitmap then
    ADesc.Depth := 1
  else
    ADesc.Depth := 32;

  if IsBitmap then
    ADesc.ByteOrder := riboMSBFirst
  else
    ADesc.ByteOrder := riboLSBFirst;

  ADesc.LineOrder := riloTopToBottom;

  case ADesc.Depth of
    0..8:   ADesc.BitsPerPixel := ADesc.Depth;
    9..16:  ADesc.BitsPerPixel := 16;
    17..32: ADesc.BitsPerPixel := 32;
  else
    ADesc.BitsPerPixel := 64;
  end;

  if IsBitmap then
  begin
    ADesc.LineEnd  := rileByteBoundary;
    ADesc.RedPrec  := 1;
    ADesc.RedShift := 0;
  end else
  begin
    // Try retrieving the lineend
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.MaskBitsPerPixel := 1;
    ADesc.MaskShift := 0;
    ADesc.MaskLineEnd := rileByteBoundary;
    ADesc.MaskBitOrder := riboBitsInOrder;
  end;

  Result := True;
end;

function TMUIWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): boolean;
begin
  RawImage_QueryDescription([riqfRGB, riqfAlpha], ADesc);
  ADesc.Width := TMuiBitmap(ABitmap).FWidth;
  ADesc.Height := TMuiBitmap(ABitmap).FHeight;

  {$ifdef VERBOSEAROS}
  writeln('RawImage_DescriptionFromBitmap ', HexStr(Pointer(ABitmap)));
  {$endif}
  Result := True;
end;

function TMUIWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  W, H: Integer;
  MUICanvas: TMUICanvas absolute ADC;
begin
  if Assigned(MUICanvas) then
  begin
    w := MUICanvas.DrawRect.Right;
    h := MUICanvas.DrawRect.Bottom;
  end else
  begin
    w := IntuitionBase^.ActiveScreen^.Width;
    h := IntuitionBase^.ActiveScreen^.Height;
  end;
  {$ifdef VERBOSEAROS}
  writeln('RawImage_DescriptionFromDevice ', HexStr(Pointer(ADC)));
  {$endif}
  ADesc.Width := w;
  ADesc.Height := h;
  RawImage_QueryDescription([riqfRGB], ADesc);
  Result := True;
end;

function TMUIWidgetSet.RawImage_FromBitmap(out ARawImage: TRawImage; ABitmap, AMask: HBITMAP; ARect: PRect = nil): Boolean;
var
  Bit: TMUIBitmap absolute ABitmap;
begin
  ARawImage.Init;
  {$ifdef VERBOSEAROS}
  writeln('RawImage_FromBitmap');
  {$endif}
  if Assigned(Bit) then
  begin
    Bit.GetFromCanvas;
    RawImage_QueryDescription([riqfUpdate,riqfRGB], ARawImage.Description);
    ARawImage.Description.Width := Bit.FWidth;
    ARawImage.Description.Height := Bit.FHeight;
    ARawImage.Description.Depth := 32;
    ARawImage.DataSize := Bit.FWidth * Bit.FHeight * SizeOf(LongWord);
    ReAllocMem(ARawImage.Data, ARawImage.DataSize);
    Move(Bit.FImage^, ARawImage.Data^, ARawImage.DataSize);
  end;
  Result := True;
end;

function TMUIWidgetSet.RawImage_FromDevice(out ARawImage: TRawImage; ADC: HDC; const ARect: TRect): Boolean;
var
  W, H: Integer;
  MUICanvas: TMUICanvas absolute ADC;
  T: AGraphics.TPoint;
begin
  ARawImage.Init;
  w := ARect.Right;
  h := ARect.Bottom;
  {$ifdef VERBOSEAROS}
  writeln('RawImage_FromDevice ', w, ' x ', h);
  {$endif}
  ARawImage.Description.Width := w;
  ARawImage.Description.Height := h;
  RawImage_QueryDescription([riqfUpdate,riqfRGB], ARawImage.Description);
  ARawImage.DataSize := w * h * SizeOf(LongWord);
  ReAllocMem(ARawImage.Data, ARawImage.DataSize);
  T := MUICanvas.GetOffset;
  if Assigned(CyberGfxBase) then
    Cybergraphics.ReadPixelArray(ARawImage.Data, 0, 0, w * SizeOf(LongWord), MUICanvas.RastPort, T.X, T.Y, w, h, RECTFMT_ARGB);
  Result := True;
end;

function TMUIWidgetSet.RawImage_QueryDescription(AFlags: TRawImageQueryFlags; var ADesc: TRawImageDescription): Boolean;
begin
  //writeln('QueryDescription');
  //if riqfAlpha in AFlags then
  begin
    //always return rgba description
    if not (riqfUpdate in AFlags)  then
    begin
      //writeln('Init ', ADesc.Width);
      ADesc.Init;
    end;

    ADesc.Format := ricfRGBA;
    ADesc.Depth := 32;
    ADesc.BitOrder := riboReversedBits;
    ADesc.ByteOrder := riboLSBFirst;
    ADesc.LineOrder := riloTopToBottom;
    ADesc.LineEnd := rileDWordBoundary;
    ADesc.BitsPerPixel := 32;
    if ADesc.Width = 0 then
    begin
      ADesc.Width := cardinal(640);
      ADesc.Height := cardinal(480);
    end;

    if riqfAlpha in AFlags then
      ADesc.Depth := 32;
    ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 0;

    if riqfMask in AFlags then
    begin
      //ADesc.MaskBitsPerPixel := 8;
      //ADesc.MaskShift := 0;
      //ADesc.MaskLineEnd := rileByteBoundary;
      //ADesc.MaskBitOrder := riboBitsInOrder;
    end;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 8;
      ADesc.GreenShift := 16;
      ADesc.BlueShift := 24;
    end;


    {ADesc.AlphaPrec := 8;
    ADesc.AlphaShift := 24;

    if riqfRGB in AFlags
    then begin
      ADesc.RedPrec := 8;
      ADesc.GreenPrec := 8;
      ADesc.BluePrec := 8;
      ADesc.RedShift := 16;
      ADesc.GreenShift := 8;
      ADesc.BlueShift := 0;
    end;
    }
    AFlags := AFlags - [riqfRGB, riqfAlpha, riqfUpdate];
    if AFlags = [] then Exit(True);

    // continue with default
    Include(AFlags, riqfUpdate);
  end;
  //Result := inherited RawImage_QueryDescription(AFlags, ADesc);
  // reduce mem
  //if Result and (ADesc.Depth = 24)
  //then ADesc.BitsPerPixel := 24;
end;

function TMUIWidgetSet.DCGetPixel(CanvasHandle: HDC; X, Y: integer): TGraphicsColor;
var
  Canvas: TMUICanvas;
begin
  Canvas := TMUICanvas(CanvasHandle);
  if Assigned(Canvas) then
  begin
    Result := Canvas.GetPixel(X, Y);
  end;
end;

procedure TMUIWidgetSet.DCSetPixel(CanvasHandle: HDC; X, Y: integer; AColor: TGraphicsColor);
var
  Canvas: TMUICanvas;
begin
  Canvas := TMUICanvas(CanvasHandle);
  if Assigned(Canvas) then
  begin
    Canvas.SetPixel(X, Y, AColor);
  end;
end;

function TMUIWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 1;
end;

constructor TMUIWidgetSet.Create;
begin
  inherited Create;
  MUIWidgetSet := self;
end;

destructor TMUIWidgetSet.Destroy;
begin
  MUIWidgetSet := nil;
  inherited Destroy;
end;

function TMUIWidgetSet.CreateTimer(Interval: integer; TimerFunc: TWSTimerProc): THandle;
begin
  Result := 0;
  if Assigned(MUIApp) then
  begin
    Result := MUIApp.CreateTimer(Interval, TimerFunc);
  end;
end;

function TMUIWidgetSet.DestroyTimer(TimerHandle: THandle): boolean;
begin
  Result:=false;
  if Assigned(MUIApp) then
  begin
    Result := MUIApp.DestroyTimer(TimerHandle);
  end;
end;

procedure TMUIWidgetSet.DestroyLCLComponent(Sender: TObject);
begin

end;


Const
  CLIP_PLAINTEXT = 2;

function TMUIWidgetSet.CreateThemeServices: TThemeServices;
begin
  Result := TMUIThemeServices.Create;
end;

function TMUIWidgetSet.ClipboardFormatToMimeType(FormatID: TClipboardFormat): string;
begin
  Result := '';
  if FormatID = CLIP_PLAINTEXT then
    Result := 'text/plain';
end;

function TMUIWidgetSet.ClipboardGetData(ClipboardType: TClipboardType; FormatID: TClipboardFormat; Stream: TStream): boolean;
var
  temp: string;
begin
  Result := False;
  if FormatID = CLIP_PLAINTEXT then
  begin
    Temp := GetTextFromClip(0);
    Stream.Write(temp[1], Length(temp));
    Result := True;
  end;
end;
    // ! ClipboardGetFormats: List will be created. You must free it yourself with FreeMem(List) !
function TMUIWidgetSet.ClipboardGetFormats(ClipboardType: TClipboardType; var Count: integer; var List: PClipboardFormat): boolean;
begin
  Count := 1;
  GetMem(List, SizeOf(TClipBoardFormat));
  List^ := CLIP_PLAINTEXT;
  Result := True;
end;


function TMUIWidgetSet.ClipboardGetOwnerShip(ClipboardType: TClipboardType; OnRequestProc: TClipboardRequestEvent;  FormatCount: integer; Formats: PClipboardFormat): boolean;
var
  DataStream: TStringStream;
  Temp: string;
  i: Integer;
begin
  Result := True;
  if (FormatCount = 0) or (OnRequestProc = nil) then
  begin
  end else
  begin
    DataStream := TStringStream.Create('');
    DataStream.Size := 0;
    DataStream.Position := 0;
    For i := 0 to FormatCount - 1 do
    begin
      if Formats[i] <> CLIP_PLAINTEXT then
        Continue;
      OnRequestProc(Formats[i], DataStream);
      if DataStream.Size > 0 then
      begin
        DataStream.Seek(0, soFromBeginning);
        Temp := DataStream.ReadString(DataStream.Size - 1);
        PutTextToClip(0, Temp);
      end;
    end;
  end;
end;


function TMUIWidgetSet.ClipboardRegisterFormat(const AMimeType: string): TClipboardFormat;
begin
  Result := TClipboardFormat(-1);
  if AMimeType = 'text/plain' then
    Result := CLIP_PLAINTEXT;
end;


end.
