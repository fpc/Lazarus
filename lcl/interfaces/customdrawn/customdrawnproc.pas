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
  GraphType, Controls, LCLMessageGlue, WSControls, LCLType, LCLProc;

type
  TUpdateLazImageFormat = (clfRGB24,clfRGB24UpsideDown, clfBGR24, clfBGRA32);

  TCDWinControl = class
  public
    Region: TLazRegionWithChilds;
    WinControl: TWinControl;
  end;

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat);
procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
function DateTimeToMilliseconds(aDateTime: TDateTime): Int64;
function IsValidDC(ADC: HDC): Boolean;
function IsValidGDIObject(AGDIObj: HGDIOBJ): Boolean;

implementation

procedure UpdateControlLazImageAndCanvas(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; AWidth, AHeight: Integer; AFormat: TUpdateLazImageFormat);
var
  lRawImage: TRawImage;
begin
  {$IFDEF VerboseWinAPI}
    DebugLn(Format(':>[UpdateControlLazImageAndCanvas] Input Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
  // Check if the image needs update
  if (AImage = nil) or (AWidth <> AImage.Width) or (AHeight <> AImage.Height) then
  begin
    if (AImage <> nil) then AImage.Free;

    lRawImage.Init;
    case AFormat of
    clfRGB24:  lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB(AWidth, AHeight);
    clfRGB24UpsideDown: lRawImage.Description.Init_BPP24_R8G8B8_BIO_TTB_UpsideDown(AWidth, AHeight);
    clfBGR24:  lRawImage.Description.Init_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight);
    clfBGRA32: lRawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
    end;
    lRawImage.CreateData(True);

    AImage := TLazIntfImage.Create(AWidth, AHeight);
    AImage.SetRawImage(lRawImage);

    if (ACanvas <> nil) then ACanvas.Free;
    ACanvas := TLazCanvas.Create(AImage);
  end;
  {$IFDEF VerboseWinAPI}
    DebugLn(Format(':<[UpdateControlLazImageAndCanvas] Output Image: %x Canvas: %x',
      [PtrInt(AImage), PtrInt(ACanvas)]));
  {$ENDIF}
end;

procedure RenderChildWinControls(var AImage: TLazIntfImage;
  var ACanvas: TLazCanvas; ACDControlsList: TFPList);
var
  i: Integer;
  lCDWinControl: TCDWinControl;
  lWinControl: TWinControl;
  struct : TPaintStruct;
begin
  {$ifdef VerboseCDWinControl}
  DebugLn(Format('[RenderChildWinControls] ACanvas=%x ACDControlsList=%x',
    [PtrInt(ACanvas), PtrInt(ACDControlsList)]));
  {$endif}
  FillChar(struct, SizeOf(TPaintStruct), 0);
  struct.hdc := HDC(ACanvas);

  for i := 0 to ACDControlsList.Count-1 do
  begin
    lCDWinControl := TCDWinControl(ACDControlsList.Items[i]);
    lWinControl := lCDWinControl.WinControl;
    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d lWinControl=%x', [i, PtrInt(lWinControl)]));
    {$endif}
    if lWinControl.Visible = False then Continue;

    // Prepare the clippping
    ACanvas.Clipping := True;
    ACanvas.ClipRect := Bounds(lWinControl.Left, lWinControl.Top, lWinControl.Width, lWinControl.Height);
    ACanvas.UseSimpleRectClipping := True;
    ACanvas.WindowOrg := Point(lWinControl.Left, lWinControl.Top);

    {$ifdef VerboseCDWinControl}
    DebugLn(Format('[RenderChildWinControls] i=%d before LCLSendPaintMsg', [i]));
    {$endif}
    LCLSendPaintMsg(lCDWinControl.WinControl, struct.hdc, @struct);
  end;

  ACanvas.Clipping := False;
  ACanvas.WindowOrg := Point(0, 0);
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

