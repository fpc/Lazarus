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
  FPCMacOSAll, CarbonUtils;

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

var
  LAZARUS_FOURCC: FourCharCode; // = 'Laz ';
  WIDGETINFO_FOURCC: FourCharCode; // = 'WInf';

type
  TCarbonWidgetType = (cwtWindowRef, cwtControlRef);

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
  
type

  { TDeviceContext }

  TDeviceContext = class
  public
    Info: PWidgetInfo;
  end;

implementation

initialization
  LAZARUS_FOURCC := MakeFourCC('Laz ');
  WIDGETINFO_FOURCC := MakeFourCC('WInf');

end.
