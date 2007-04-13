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

// debugging defines
{$I carbondebug.inc}

uses
  // libs
  FPCMacOSAll,
  // wdgetset
  WSLCLClasses, LCLClasses,
  // LCL + RTL
  Types, Classes, Controls, LCLType, LCLProc, Graphics, Math, LMessages,
  LCLMessageGlue;

const
  DEFAULT_CFSTRING_ENCODING = kCFStringEncodingUTF8;

var
  LAZARUS_FOURCC: FourCharCode;    // = 'Laz ';
  WIDGETINFO_FOURCC: FourCharCode; // = 'WInf';
  MENU_FOURCC: FourCharCode;       // = 'Menu';

type

  { TCarbonContext }

  TCarbonContext = class
  public
    CGContext: CGContextRef;
    constructor Create;
    procedure Reset; virtual; abstract;
  end;

  { TCarbonWidget }
  
  TCarbonWidget = class
  private
    FProperties: TStringList;
    function GetProperty(AIndex: String): Pointer;
    procedure SetProperty(AIndex: String; const AValue: Pointer);
  protected
    procedure RegisterEvents; virtual; abstract;
    procedure UnregisterEvents; virtual; abstract;
    procedure CreateWidget(const AParams: TCreateParams); virtual; abstract;
    procedure DestroyWidget; virtual; abstract;
    function GetContent: ControlRef; virtual; abstract;
  public
    LCLObject: TWinControl;  // LCL control which created this widget
    Context: TCarbonContext; // Carbon content area context
    Widget: Pointer;         // Reference to the Carbon window or control
  public
    constructor Create(const AObject: TWinControl; const AParams: TCreateParams);
    destructor Destroy; override;
    procedure AddToWidget(AParent: TCarbonWidget); virtual; abstract;
    procedure BoundsChanged; virtual;
    function GetClientRect(var ARect: TRect): Boolean; virtual; abstract;
    function GetPreferredSize: TPoint; virtual;
    function GetMousePos: TPoint; virtual; abstract;
    function GetTopParentWindow: WindowRef; virtual; abstract;
    procedure Invalidate(Rect: PRect = nil); virtual; abstract;
    function IsEnabled: Boolean; virtual; abstract;
    function IsVisible: Boolean; virtual; abstract;
    function Enable(AEnable: Boolean): Boolean; virtual; abstract;
    
    function GetBounds(var ARect: TRect): Boolean; virtual; abstract;
    function GetScreenBounds(var ARect: TRect): Boolean; virtual; abstract;
    function SetBounds(const ARect: TRect): Boolean; virtual; abstract;
    
    procedure SetColor(const AColor: TColor); virtual; abstract;
    procedure SetFont(const AFont: TFont); virtual; abstract;
    procedure ShowHide(AVisible: Boolean); virtual; abstract;
    
    function GetText(var S: String): Boolean; virtual; abstract;
    function SetText(const S: String): Boolean; virtual; abstract;
    function Update: Boolean; virtual; abstract;
  public
  { Content:
     = widget in controls without special client control
     - client area control of control or window
     - origin of local coordinates
     - area for embedding child controls
     - processes track and draw event                  }
    property Content: ControlRef read GetContent;
    property Properties[AIndex: String]: Pointer read GetProperty write SetProperty;
  end;
  
type
  TCarbonEventHandlerProc = function (ANextHandler: EventHandlerCallRef;
    AEvent: EventRef;
    AWidget: TCarbonWidget): OSStatus; {$IFDEF darwin}mwpascal;{$ENDIF}

type
  TEventInt = packed record
    case Integer of
    1: (Chars: array[0..4] of Char);
    2: (Int: FPCMacOSAll.UInt32);
  end;
  
const
  LCLCarbonEventClass    = 'Laz ';
  LCLCarbonEventKindWake = 'Wake';
  LCLCarbonEventKindMain = 'Main';

implementation

uses
  CarbonProc, CarbonCanvas, CarbonConsts, CarbonUtils;
  
{ TCarbonContext }

constructor TCarbonContext.Create;
begin
  inherited;

  CGContext := nil;
end;
  
{ TCarbonWidget }

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetProperty
  Params:  AIndex - Property name
  Returns: Property data, nil if the property is not listed

  Returns the specified property data or nil if the property is not listed
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetProperty(AIndex: String): Pointer;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(AIndex);
    
    if I >= 0 then // the property is listed
    begin
      Result := FProperties.Objects[I];
      Exit;
    end;
  end;
  Result := nil;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.SetProperty
  Params:  AIndex - Property name
           AValue - Property data, nil means remove the property

  Sets the specified property data or removes the property
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.SetProperty(AIndex: String; const AValue: Pointer);
var
  I: Integer;
begin
  if FProperties = nil then
  begin
    if AValue = nil then Exit;
    // create string list for storing properties
    FProperties := TStringList.Create;
    FProperties.Sorted := True; // to enable binary searching
  end;
  
  I := FProperties.IndexOf(AIndex);
  if I >= 0 then // the property is listed -> update or remove if AValue = nil
  begin
    if AValue = nil then
    begin
      FProperties.Delete(I);
      if FProperties.Count = 0 then
      begin
        FProperties.Free; // free if the list is clear
        FProperties := nil;
      end;
    end
    else FProperties.Objects[I] := TObject(AValue);
  end
  else // the property is not listed -> add
  begin
    FProperties.AddObject(AIndex, TObject(AValue));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.BoundsChanged

  Handles bounds change
 ------------------------------------------------------------------------------}
procedure TCarbonWidget.BoundsChanged;
var
  WidgetBounds, OldBounds: TRect;
  Resized: Boolean;
begin
  //DebugLn('TCarbonWidget.BoundsChanged ' + LCLObject.Name);

  GetBounds(WidgetBounds);
  OldBounds := LCLObject.BoundsRect;
  
  Resized := False;

  if LCLObject.ClientRectNeedsInterfaceUpdate then
  begin
    //DebugLn('TCarbonWidget.BoundsChanged Update client rects cache');
    // update client rects cache
    LCLObject.InvalidateClientRectCache(True);
    LCLObject.DoAdjustClientRectChange;
    
    Resized := True;
  end;
    
  // then send a LM_SIZE message
  if (OldBounds.Right - OldBounds.Left <> WidgetBounds.Right - WidgetBounds.Left) or
     (OldBounds.Bottom - OldBounds.Top <> WidgetBounds.Bottom - WidgetBounds.Top) then
  begin
    LCLSendSizeMsg(LCLObject, WidgetBounds.Right - WidgetBounds.Left,
      WidgetBounds.Bottom - WidgetBounds.Top, Size_SourceIsInterface);
    
    Resized := True;
  end;

  // then send a LM_MOVE message
  if (OldBounds.Left <> WidgetBounds.Left) or
     (OldBounds.Top <> WidgetBounds.Top) then
  begin
    LCLSendMoveMsg(LCLObject, WidgetBounds.Left,
      WidgetBounds.Top, Move_SourceIsInterface);
  end;

  // invalidate control canvas
  if Resized then Invalidate;

  // invalidate parent client area, previously covered by control
  if Resized and (LCLObject.Parent <> nil) and LCLObject.Parent.HandleAllocated then
  begin
    TCarbonWidget(LCLObject.Parent.Handle).Invalidate(@OldBounds);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.Create
  Params:  AObject - LCL conrol
           AParams - Creation parameters
  Returns: The Carbon widget

  Creates basic widget for the specified LCL control
 ------------------------------------------------------------------------------}
constructor TCarbonWidget.Create(const AObject: TWinControl;
  const AParams: TCreateParams);
begin
  LCLObject := AObject;
  FProperties := nil;
  Widget := nil;
  Context := nil;
  
  CreateWidget(AParams);
  
  LCLObject.InvalidateClientRectCache(True);
  
  {$IFDEF VerboseWidget}
    DebugLn('TCarbonWidget.Create ', ClassName, ' ', LCLObject.Name, ': ',
      LCLObject.ClassName);
  {$ENDIF}
  
  RegisterEvents;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.Destroy

  Frees the widget
 ------------------------------------------------------------------------------}
destructor TCarbonWidget.Destroy;
begin
  UnregisterEvents;
  
  DestroyWidget;
  
  FProperties.Free;

  {$IFDEF VerboseWidget}
    DebugLn('TCarbonWidget.Destroy ', ClassName, ' ', LCLObject.Name, ': ',
      LCLObject.ClassName);
  {$ENDIF}

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWidget.GetPreferredSize
  Returns: The preffered size of widget for autosizing or (0, 0)
 ------------------------------------------------------------------------------}
function TCarbonWidget.GetPreferredSize: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

initialization

  LAZARUS_FOURCC := MakeFourCC('Laz ');
  WIDGETINFO_FOURCC := MakeFourCC('WInf');
  MENU_FOURCC := MakeFourCC('Menu');

end.
