{ $Id$ }
{
                     -------------------------------------
                     carbondebug.pp  -  graphic dump utils 
                     -------------------------------------
 
 @created(Mon Jun 18th WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains utility functions to show the contents of graphics
 
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

unit CarbonDebug;

{$mode objfpc}{$H+}

interface 

uses
  MacOSAll,
  sysutils, CarbonUtils;

procedure DbgDumpImage(AImage: CGImageRef; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
procedure DbgDumpLayer(ALayer: CGLayerRef; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);

implementation

type
  TDbgDumpKind = (dkImage, dkLayer);

  PDbgDumpInfo = ^TDbgDumpInfo;
  TDbgDumpInfo = record                            
    Width, Height: Integer;
    OrgWidth, OrgHeight: Integer;
    Control: ControlRef;
    Text: String;
    case Kind: TDbgDumpKind of
      dkImage: (Image: CGImageRef);
      dkLayer: (Layer: CGLayerRef);
  end;

function DbgWindowDraw(ANextHandler: EventHandlerCallRef;
                  AEvent: EventRef;
                  AInfo: PDbgDumpInfo): OSStatus; mwpascal;
var
  bounds, R: CGRect;
  context: CGContextRef;
  status: Integer;
  affine: CGAffineTransform;
  l: Single;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);

  HIViewGetBounds(AInfo^.Control, bounds);

  status := GetEventParameter(AEvent, kEventParamCGContextRef, typeCGContextRef, nil, SizeOf(context), nil, @Context);

  CGContextSetRGBFillColor(context, 0, 1, 0, 0.3);
  CGContextFillRect(context, bounds);
  CGContextScaleCTM(context, 1, -1);

  R := CGRectMake(2, -15, AInfo^.Width, -AInfo^.Height);
  case AInfo^.Kind of
    dkImage: begin
      if AInfo^.Image <> nil
      then begin
        if CGImageIsMask(AInfo^.Image) <> 0
        then begin
          CGContextSaveGState(context);
          CGContextSetRGBFillColor(context, 0, 0, 0, 1);
          CGContextClipToMask(context, R, AInfo^.Image);
          CGContextFillRect(context, R);
          CGContextRestoreGState(context);
        end
        else begin
          CGContextDrawImage(Context, R, AInfo^.Image);
        end;
      end;
    end;
    dkLayer: begin
      CGContextDrawLayerInRect(Context, R, AInfo^.Layer);
    end;
  end;

  CGContextSelectFont(context, Pointer(PChar('Helvetica')), 10, kCGEncodingMacRoman);
//  CGContextSetTextDrawingMode (context, kCGTextFillStroke);
  CGContextSetTextDrawingMode (context, kCGTextFill);
  CGContextSetRGBFillColor(context, 0, 0, 0, 1);
  CGContextSetRGBStrokeColor(context, 0, 0, 0, 1);

  CGContextShowTextAtPoint(context, 2, -12, Pointer(PChar(AInfo^.Text)), Length(AInfo^.Text));
end;

function DbgWindowClosed(ANextHandler: EventHandlerCallRef; AEvent: EventRef;
  AInfo: PDbgDumpInfo): OSStatus; mwpascal;
begin
  Result := CallNextEventHandler(ANextHandler, AEvent);
  case AInfo^.Kind of
    dkImage: CGImageRelease(AInfo^.Image);
    dkLayer: CGLayerRelease(AInfo^.Layer);
  end;
  Dispose(AInfo);
end;


procedure DbgCreateWindow(AInfo: PDbgDumpInfo; const ATitle: String);
var
  R: Rect;
  w: WindowRef;
  DbgWindowDrawUPP, DbgWindowClosedUPP: EventHandlerUPP;
  Spec: EventTypeSpec;
begin
  R.Top := 50; R.Left := 0;
  if AInfo^.Width < 50 then R.Right := 50 else R.Right := AInfo^.Width;
  if AInfo^.Height < 25 then R.Bottom := 25 else R.Bottom := AInfo^.Height;
  Inc(R.Bottom, R.Top + 15);
  Inc(R.Right, R.Left + 2);

  W := nil;
  CreateNewWindow(
    kUtilityWindowClass,
    kWindowCompositingAttribute or
    kWindowStandardDocumentAttributes or
//    kWindowLiveResizeAttribute or
    kWindowStandardHandlerAttribute,
    R, W
  );
  if W = nil then Exit;

  case AInfo^.Kind of
    dkImage: CGImageRetain(AInfo^.Image);
    dkLayer: CGLayerRetain(AInfo^.Layer);
  end;
  AInfo^.Text := ATitle;

  DbgWindowClosedUPP := NewEventHandlerUPP(EventHandlerProcPtr(@DbgWindowClosed));
  Spec := MakeEventSpec(kEventClassWindow, kEventWindowClosed);
  InstallWindowEventHandler(W, DbgWindowClosedUPP, 1, @Spec, AInfo, nil);

  AInfo^.Control := nil;
  GetRootControl(W, AInfo^.Control);

  DbgWindowDrawUPP := NewEventHandlerUPP(EventHandlerProcPtr(@DbgWindowDraw));
  Spec := MakeEventSpec(kEventClassControl, kEventControlDraw);
  if AInfo^.Control <> nil
  then InstallControlEventHandler(AInfo^.Control, DbgWindowDrawUPP, 1, @Spec, AInfo, nil);

  ShowWindow(W);
end;

procedure DbgDumpImage(AImage: CGImageRef; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
var
  Info: PDbgDumpInfo;
  h,w,bpp: Integer;
begin
  New(Info);

  if (AImage = nil)
  then begin
    w := 0; h:= 0; bpp := 0;
    if AWidth = -1 then AWidth := 0;
    if AHeight = -1 then AHeight := 0;
  end
  else begin
    w := CGImageGetWidth(AImage);
    h := CGImageGetHeight(AImage);
    bpp := CGImageGetBitsPerPixel(AImage);
    if AWidth = -1 then AWidth := W;
    if AHeight = -1 then AHeight := H;
  end;

  Info^.Kind := dkImage;
  Info^.Image := AImage;
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  Info^.OrgWidth := w;
  Info^.OrgHeight := h;

  ATitle := ATitle + Format(' (Image: %p W:%d H:%d bpp:%d)', [AImage, w, h, bpp]);
  DbgCreateWindow(Info, ATitle);
end;

procedure DbgDumpLayer(ALayer: CGLayerRef; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
var
  Info: PDbgDumpInfo;
  S: CGSize;
begin
  New(Info);

  if (ALayer = nil)
  then begin
    S.height := 0; s.width := 0;
    if AWidth = -1 then AWidth := 0;
    if AHeight = -1 then AHeight := 0;
  end
  else begin
    S := CGLayerGetSize(ALayer);
    if AWidth = -1 then AWidth := Round(S.width);
    if AHeight = -1 then AHeight := Round(S.height);
  end;

  Info^.Kind := dkLayer;
  Info^.Layer := ALayer;
  Info^.Width := AWidth;
  Info^.Height := AHeight;
  Info^.OrgWidth := Round(S.width);
  Info^.OrgHeight := Round(S.height);

  ATitle := ATitle + Format(' (Layer: %p W:%f H:%f)', [ALayer, S.width, s.height]);
  DbgCreateWindow(Info, ATitle);
end;


end.
