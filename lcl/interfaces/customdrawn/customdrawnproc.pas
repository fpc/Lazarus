unit customdrawnproc;

{$mode objfpc}{$H+}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  fpimage, fpcanvas,
  // Custom Drawn Canvas
  IntfGraphics, lazcanvas, lazregions,
  //
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc,
  Forms;

type
  TUpdateLazImageFormat = (
    clfRGB16_R5G6B5,
    clfRGB24, clfRGB24UpsideDown, clfBGR24,
    clfBGRA32, clfRGBA32, clfARGB32);

  TCDWinControl = class
  public
    Region: TLazRegionWithChilds;
    WinControl: TWinControl;
    //CDControl: TCDControl;
  end;

  TCDNonNativeForm = class
  public
    LCLForm: TCustomForm;
    Children: TFPList; // of TCDWinControl;
    // painting objects
    Image: TLazIntfImage;
    Canvas: TLazCanvas;
  end;

// Routines for non-native form

procedure InitNonNativeForms();
function GetCurrentForm(): TCDNonNativeForm;
function AddNewForm(AForm: TCustomForm): TCDNonNativeForm;
procedure ShowForm(ACDForm: TCDNonNativeForm);
procedure HideForm(ACDForm: TCDNonNativeForm);

// Routines for non-native wincontrol

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False; AFreeImageOnUpdate: Boolean = True);
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
//procedure RenderWinControl(var AImage: TLazIntfImage;
//  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;

// Other routines

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function IsValidDC(ADC: HDC): Boolean;
function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;

implementation

// List with the Z-order of non-native forms, index=0 is the bottom-most form
var
  NonNativeForms: TFPList = nil;

procedure InitNonNativeForms();
begin
  if NonNativeForms <> nil then Exit;
  NonNativeForms := TFPList.Create;
end;

function GetCurrentForm(): TCDNonNativeForm;
var
  lCount: Integer;
begin
  {$IFDEF VerboseCDForms}
    DebugLn('GetCurrentForm');
  {$ENDIF}
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  if lCount = 0 then Result := nil
  else Result := TCDNonNativeForm(NonNativeForms.Items[lCount-1]);
end;

function AddNewForm(AForm: TCustomForm): TCDNonNativeForm;
var
  lFormInfo: TCDNonNativeForm;
begin
  {$IFDEF VerboseCDForms}
    DebugLn('AddNewForm');
  {$ENDIF}
  InitNonNativeForms();
  lFormInfo := TCDNonNativeForm.Create;
  lFormInfo.LCLForm := AForm;
  lFormInfo.Children := TFPList.Create;
  NonNativeForms.Insert(0, lFormInfo);
  Result := lFormInfo;
end;

procedure ShowForm(ACDForm: TCDNonNativeForm);
var
  lCount, lCurIndex: Integer;
begin
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('ShowForm lOldIndex=%d lNewIndex=%d', [lCurIndex, lCount-1]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, lCount-1);
end;

procedure HideForm(ACDForm: TCDNonNativeForm);
var
  lCount, lCurIndex: Integer;
begin
  InitNonNativeForms();
  lCount := NonNativeForms.Count;
  lCurIndex := NonNativeForms.IndexOf(ACDForm);
  {$IFDEF VerboseCDForms}
    DebugLn(Format('HideForm lOldIndex=%d lNewIndex=0', [lCurIndex]));
  {$ENDIF}
  NonNativeForms.Move(lCurIndex, 0);
end;

// If AForceUpdate=True then it will update even if the width and height remain the same
procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat;
  AData: Pointer = nil; AForceUpdate: Boolean = False; AFreeImageOnUpdate: Boolean = True);
var
  lRawImage: TRawImage;
  lPixelSize: Byte;
begin
  {$IFDEF VerboseCDForms}
    DebugLn(Format(':>[UpdateControlLazImageAndCanvas] Input Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
  // Check if the image needs update
  if (AImage = nil) or (AWidth <> AImage.Width) or (AHeight <> AImage.Height)
    or AForceUpdate then
  begin
    if (AImage <> nil) and AFreeImageOnUpdate then AImage.Free;

    lRawImage.Init;
    case AFormat of
    clfRGB16_R5G6B5:  lRawImage.Description.Init_BPP16_R5G6B5(AWidth, AHeight);
    clfRGB24:  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight);
    clfRGB24UpsideDown: lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight);
    clfBGR24:  lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
    clfBGRA32: lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    clfRGBA32: lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(AWidth, AHeight);
    clfARGB32: lRawImage.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
    end;

    // Now connect the pixel buffer or create one
    if AData = nil then lRawImage.CreateData(True)
    else
    begin
      case AFormat of
      clfRGB16_R5G6B5:
        lPixelSize := 2;
      clfRGB24, clfRGB24UpsideDown, clfBGR24:
        lPixelSize := 3;
      clfBGRA32, clfRGBA32, clfARGB32:
        lPixelSize := 4;
      end;

      lRawImage.Data := AData;
      lRawImage.DataSize := AWidth * lPixelSize * AHeight;
    end;

    AImage := TLazIntfImage.Create(AWidth, AHeight);
    AImage.SetRawImage(lRawImage);

    if (ACanvas <> nil) then ACanvas.Free;
    ACanvas := TLazCanvas.Create(AImage);
  end;
  {$IFDEF VerboseCDForms}
    DebugLn(Format(':<[UpdateControlLazImageAndCanvas] Output Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
end;

procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
var
  i, lChildrenCount: Integer;
  lCDWinControl: TCDWinControl;
  lWinControl: TWinControl;
  struct : TPaintStruct;
begin
  lChildrenCount := ACDControlsList.Count;
  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderChildWinControls] ACanvas=%x ACDControlsList=%x lChildrenCount=%d',
    [PtrInt(ACanvas), PtrInt(ACDControlsList), lChildrenCount]));
  {$endif}
  FillChar(struct, SizeOf(TPaintStruct), 0);
  struct.hdc := HDC(ACanvas);

  for i := 0 to lChildrenCount-1 do
  begin
    lCDWinControl := TCDWinControl(ACDControlsList.Items[i]);
    lWinControl := lCDWinControl.WinControl;
    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d lWinControl=%x Left=%d'
      + ' Top=%d Width=%d Height=%d', [i, PtrInt(lWinControl),
      lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height]));
    {$endif}
    if lWinControl.Visible = False then Continue;

    // Prepare the clippping
    ACanvas.Clipping := True;
    lCDWinControl.Region.Rect := Bounds(lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height);
    ACanvas.ClipRegion := lCDWinControl.Region;
    ACanvas.UseRegionClipping := True;
    ACanvas.BaseWindowOrg := Point(lWinControl.Left, lWinControl.Top);
    ACanvas.WindowOrg := Point(0, 0);

    // Save the Canvas state
    ACanvas.SaveState;
    ACanvas.ResetCanvasState;

    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d before LCLSendPaintMsg', [i]));
    {$endif}
    LCLSendPaintMsg(lCDWinControl.WinControl, struct.hdc, @struct);

    // Now restore it
    ACanvas.RestoreState;
  end;

  ACanvas.Clipping := False;
  ACanvas.BaseWindowOrg := Point(0, 0);
  ACanvas.WindowOrg := Point(0, 0);
end;

function FindControlWhichReceivedEvent(AForm: TCustomForm;
  AControlsList: TFPList; AX, AY: Integer): TWinControl;
var
  i: Integer;
  lRegionOfEvent: TLazRegionWithChilds;
  lCurCDControl: TCDWinControl;
begin
  Result := AForm;
  for i := 0 to AControlsList.Count-1 do
  begin
    lCurCDControl := TCDWinControl(AControlsList.Items[i]);
    if lCurCDControl.Region = nil then Continue;
    lRegionOfEvent := lCurCDControl.Region.IsPointInRegion(AX, AY);
    if lRegionOfEvent <> nil then
    begin
      if lRegionOfEvent.UserData = nil then
        raise Exception.Create('[FindControlWhichReceivedEvent] Malformed tree of regions');
      Result := TWinControl(lRegionOfEvent.UserData);
      Exit;
    end;
  end;
end;

function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
var
  TimeStamp: TTimeStamp;
begin
  {Call DateTimeToTimeStamp to convert DateTime to TimeStamp:}
  TimeStamp:= DateTimeToTimeStamp (aDateTime);
  {Multiply and add to complete the conversion:}
  Result:= TimeStamp.Time;
end;

function IsValidDC(ADC: HDC): Boolean;
begin
  Result := ADC <> 0;
end;

function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;
begin
  Result := AGDIObj <> 0;
end;

end.

