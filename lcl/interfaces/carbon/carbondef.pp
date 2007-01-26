{ $Id$
                    -----------------------------------------
                    carbondef.pp  -  Type & Const definitions
                    -----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type & const definitions needed in the Carbon <-> LCL interface

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}


unit CarbonDef;

{$mode objfpc}{$H+}

interface

uses
  WSLCLClasses,
  LCLType, Classes, Controls,
  FPCMacOSAll, CarbonUtils;

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

var
  LAZARUS_FOURCC: FourCharCode; // = 'Laz ';
  WIDGETINFO_FOURCC: FourCharCode; // = 'WInf';

type
  TCarbonWidgetType = (cwtWindowRef, cwtControlRef, cwtUnknown);

  // Info needed by the API of a HWND (=Widget)
  PWidgetInfo = ^TWidgetInfo;
  TWidgetInfo = record
    LCLObject: TObject;               // the object which created this widget
    Widget: Pointer;                  // Reference to the Carbon window or control
    WidgetType: TCarbonWidgetType;
    WSClass: TWSLCLComponentClass;    // The Widgetsetclass for this info
    DataOwner: Boolean;               // Set if the UserData should be freed when the info is freed
    UserData: Pointer;
  end;
  
type
  TCarbonWSEventHandlerProc = function (ANextHandler: EventHandlerCallRef;
                                        AEvent: EventRef;
                                        AInfo: PWidgetInfo): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}

type
  TEventInt = packed record
    case integer of
    1: (Chars: array[0..4] of char);
    2: (Int: FPCMacOSAll.UInt32);
  end;
  
const
  LCLCarbonEventClass    = 'Laz ';
  LCLCarbonEventKindWake = 'Wake';
  LCLCarbonEventKindMain = 'Main';
  
  CursorToThemeCursor: array[crLow..crHigh] of ThemeCursor =
    ({crSizeSE      } kThemeResizeLeftCursor, {!!}
     {crSizeS       } kThemeResizeLeftCursor, {!!}
     {crSizeSW      } kThemeResizeRightCursor, {!!}
     {crSizeE       } kThemeResizeLeftCursor,
     {crSizeW       } kThemeResizeRightCursor,
     {crSizeNE      } kThemeResizeLeftCursor, {!!}
     {crSizeN       } kThemeResizeRightCursor, {!!}
     {crSizeNW      } kThemeResizeRightCursor, {!!}
     {crSizeAll     } kThemeResizeLeftRightCursor, {!!}
     {crHandPoint   } kThemePointingHandCursor,
     {crHelp        } kThemeContextualMenuArrowCursor, {!!}
     {crAppStart    } kThemeSpinningCursor, {!!}
     {crNo          } kThemeArrowCursor, {!!}
     {crSQLWait     } kThemeSpinningCursor, {!!}
     {crMultiDrag   } kThemeCopyArrowCursor, {!!}
     {crVSplit      } kThemeResizeLeftRightCursor,
     {crHSplit      } kThemeResizeLeftRightCursor, {!!}
     {crNoDrop      } kThemeArrowCursor, {!!}
     {crDrag        } kThemeCopyArrowCursor,
     {crHourGlass   } kThemeSpinningCursor,
     {crUpArrow     } kThemeArrowCursor, {!!}
     {crSizeWE      } kThemeResizeLeftRightCursor,
     {crSizeNWSE    } kThemeResizeLeftRightCursor, {!!}
     {crSizeNS      } kThemeResizeLeftRightCursor, {!!}
     {crSizeNESW    } kThemeResizeLeftRightCursor, {!!}
     {undefined     } kThemeArrowCursor, {!!}
     {crIBeam       } kThemeIBeamCursor,
     {crCross       } kThemeCrossCursor,
     {crArrow       } kThemeArrowCursor,
     {crNone        } kThemeArrowCursor,
     {crDefault     } kThemeArrowCursor);
  
type

  { TCarbonDeviceContext }

  TCarbonDeviceContext = class
  public
    Info: PWidgetInfo;    // owner
    Port: CGrafPtr;       // Carbon graphics port reference
    
    CurrentFont: HFONT;
    
    constructor Create(AInfo: PWidgetInfo);
    destructor Destroy; override;
    
    procedure Activate;
  end;
  
  { TCarbonFont }

  TCarbonFont = class
    Style: ATSUStyle;
    
    constructor Create(ALogFont: TLogFont; AFaceName: String);
    destructor Destroy; override;
  end;
  
var
  DafultTextStyle: ATSUStyle; // default Carbon text style
  

implementation

uses
  CarbonProc;

{ TCarbonFont }

constructor TCarbonFont.Create(ALogFont: TLogFont; AFaceName: String);
var
  Attr: ATSUAttributeTag;
  M: ATSUTextMeasurement;
  B: Boolean;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  ID: ATSUFontID;
begin
  ID := FindCarbonFontID(AFaceName);
  
  ATSUCreateStyle(Style);
  
  if ID <> 0 then
  begin
    Attr := kATSUFontTag;
    A := @ID;
    S := SizeOf(ID);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfHeight <> 0 then
  begin
    Attr := kATSUSizeTag;
    M := Abs(ALogFont.lfHeight) shl 16;
    A := @M;
    S := SizeOf(M);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfEscapement <> 0 then
  begin
    Attr := kATSULineRotationTag;
    M := (ALogFont.lfEscapement shl 16) div 10;
    A := @M;
    S := SizeOf(M);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfWeight > FW_NORMAL then
  begin
    Attr := kATSUQDBoldfaceTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfItalic > 0 then
  begin
    Attr := kATSUQDItalicTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfUnderline > 0 then
  begin
    Attr := kATSUQDUnderlineTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
  
  if ALogFont.lfStrikeOut > 0 then
  begin
    Attr := kATSUStyleStrikeThroughTag;
    B := True;
    A := @B;
    S := SizeOf(B);
    ATSUSetAttributes(Style, 1, @Attr, @S, @A);
  end;
end;

destructor TCarbonFont.Destroy;
begin
  ATSUDisposeStyle(Style);

  inherited;
end;

{ TCarbonDeviceContext }


constructor TCarbonDeviceContext.Create(AInfo: PWidgetInfo);
begin
  if AInfo = nil then
  begin
    Info := nil;
    Port := nil;
  end
  else
  begin
    Info := AInfo;
    
    if Info^.WidgetType = cwtWindowRef then
      Port := GetWindowPort(WindowRef(Info^.Widget))
    else
    begin
      Port := GetWindowPort(GetTopParentWindow(Info^.LCLObject as TWinControl));
      // TODO: clip to control and offset
    end;
  end;
end;

destructor TCarbonDeviceContext.Destroy;
begin
  inherited Destroy;
end;

procedure TCarbonDeviceContext.Activate;
begin
  SetPort(Port);
end;

initialization
  LAZARUS_FOURCC := MakeFourCC('Laz ');
  WIDGETINFO_FOURCC := MakeFourCC('WInf');
  
  ATSUCreateStyle(DafultTextStyle);
  
finalization
  ATSUDisposeStyle(DafultTextStyle);

end.
