{
 *****************************************************************************
 *                            CarbonWSControls.pp                            *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CarbonWSControls;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll, Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, LCLType, LCLProc, LCLIntf,
  // widgetset
  WSControls, WSLCLClasses,
  // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonEdits, CarbonListViews;

type

  { TCarbonWSDragImageListResolution }

  TCarbonWSDragImageListResolution = class(TWSDragImageListResolution)
  published
  end;

  { TCarbonWSLazAccessibleObject }

  TCarbonWSLazAccessibleObject = class(TWSLazAccessibleObject)
  private
    class procedure GetCarbonAXIdentifiers(const AObject: TLazAccessibleObject; out AHIObject: HIObjectRef; out AID64: UInt64);
  public
    class function CreateHandle(const AObject: TLazAccessibleObject): HWND; override;
    class procedure DestroyHandle(const AObject: TLazAccessibleObject); override;
    class procedure SetAccessibleDescription(const {%H-}AObject: TLazAccessibleObject; const {%H-}ADescription: string); override;
    class procedure SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string); override;
    class procedure SetAccessibleRole(const {%H-}AObject: TLazAccessibleObject; const {%H-}ARole: TLazAccessibilityRole); override;
  end;

  { TCarbonWSControl }

  TCarbonWSControl = class(TWSControl)
  published
  end;

  { TCarbonWSWinControl }

  TCarbonWSWinControlClass = class of TCarbonWSWincontrol;
  TCarbonWSWinControl = class(TWSWinControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function  CanFocus(const AWincontrol: TWinControl): Boolean; override;

    class procedure AddControl(const AControl: TControl); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
  end;

  { TCarbonWSGraphicControl }

  TCarbonWSGraphicControl = class(TWSGraphicControl)
  published
  end;

  { TCarbonWSCustomControl }

  TCarbonWSCustomControl = class(TWSCustomControl)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSImageList }

  TCarbonWSImageList = class(TWSImageList)
  published
  end;


implementation

uses
  CarbonProc;

{ TCarbonWSLazAccessibleObject }

class procedure TCarbonWSLazAccessibleObject.GetCarbonAXIdentifiers(
  const AObject: TLazAccessibleObject; out AHIObject: HIObjectRef; out
  AID64: UInt64);
var
  lControlHandle: TCarbonControl;
  lWinControl: TWinControl;
begin
  AHIObject := nil;
  AID64 := 0;
  lWinControl := AObject.FindOwnerWinControl();
  if lWinControl = nil then Exit;
  // Requesting a handle allocation here might be too soon and crash, so cancel the whole action
  if not lWinControl.HandleAllocated then Exit;

  if (AObject.OwnerControl <> nil) and (AObject.OwnerControl is TWinControl) and
   (AObject.OwnerControl.GetAccessibleObject() = AObject) then
  begin
    lControlHandle := TCarbonControl(TWinControl(AObject.OwnerControl).Handle);
    AHIObject := HIObjectRef(lControlHandle.Widget);
    AID64 := 0;
  end
  else
  begin
    lControlHandle := TCarbonControl(lWinControl.Handle);
    // If this is an internal sub-element, then simply represent it with the
    // memory address of the object
    AID64 := UInt64(PtrInt(AObject));
    AHIObject := HIObjectRef(lControlHandle.Widget);
  end;
end;

class function TCarbonWSLazAccessibleObject.CreateHandle(
  const AObject: TLazAccessibleObject): HWND;
var
  lElement: AXUIElementRef;
  lHIObject: HIObjectRef;
  lID64: UInt64;
begin
  Result := 0;
  GetCarbonAXIdentifiers(AObject, lHIObject, lID64);
  if lHIObject = nil then Exit;

  lElement := AXUIElementCreateWithHIObjectAndIdentifier(lHIObject, lID64);
  Result := {%H-}HWND(lElement);
end;

class procedure TCarbonWSLazAccessibleObject.DestroyHandle(
  const AObject: TLazAccessibleObject);
var
  lElement: AXUIElementRef;
begin
  if AObject.Handle = 0 then Exit;
  lElement := {%H-}AXUIElementRef(AObject.Handle);
  CFRelease(lElement);
end;

class procedure TCarbonWSLazAccessibleObject.SetAccessibleDescription(const AObject: TLazAccessibleObject; const ADescription: string);
begin

end;

class procedure TCarbonWSLazAccessibleObject.SetAccessibleValue(const AObject: TLazAccessibleObject; const AValue: string);
var
  lHIObject: HIObjectRef;
  lID64: UInt64;
  lValueStr, lNotification: CFStringRef;
begin
  GetCarbonAXIdentifiers(AObject, lHIObject, lID64);
  if lHIObject = nil then Exit;

  CreateCFString(AValue, lValueStr);
  HIObjectSetAuxiliaryAccessibilityAttribute(lHIObject, lID64, CFStr('AXValue'), lValueStr);
  FreeCFString(lValueStr);

  lNotification := CFSTR('AXValueChanged');
  AXNotificationHIObjectNotify(lNotification, lHIObject, lID64);

  if AObject.AccessibleRole in larAXListRoles then
  begin
    lNotification := CFSTR('AXFocusedUIElementChanged');
    AXNotificationHIObjectNotify(lNotification, lHIObject, lID64);
  end;
end;

class procedure TCarbonWSLazAccessibleObject.SetAccessibleRole(const AObject: TLazAccessibleObject; const ARole: TLazAccessibilityRole);
begin

end;

{ TCarbonWSWinControl }

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetDesignInteractive
  Params:  AWinControl - LCL win control
           AClientPos  - Pos
  Returns: If client pos should be interactive in designer
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result := False;
  if not CheckHandle(AWinControl, Self, 'GetDesignInteractive') then Exit;
  Result := TCarbonWidget(AWinControl.Handle).IsDesignInteractive(AClientPos);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetPreferredSize
  Params:  AWinControl     - LCL control
           PreferredWidth  - Preferred width, valid if > 0
           PreferredHeight - Preferred height, valid if > 0
           WithThemeSpace  - Whether to include space for theme

  Retrieves the preferred size of control in Carbon interface to support
  autosizing of controls
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  S: TPoint;
begin
  if not CheckHandle(AWinControl, Self, 'GetPreferredSize') then Exit;
  
  S := TCarbonWidget(AWinControl.Handle).GetPreferredSize;
  PreferredWidth := S.X;
  PreferredHeight := S.Y;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: If the function succeeds

  Retrieves the text (caption) of control in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
begin
  Result := False;
  if not CheckHandle(AWinControl, Self,'GetText') then Exit;

  Result := TCarbonWidget(AWinControl.Handle).GetText(AText);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetBounds
  Params:  AWinControl - LCL control
           ALeft, ATop, AWidth, AHeight - Coordinates

  Sets the bounds of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not CheckHandle(AWinControl, Self, 'SetBounds') then Exit;

  TCarbonWidget(AWinControl.Handle).SetBounds(Bounds(ALeft, ATop, AWidth, AHeight));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetChildZPosition
  Params:  AWinControl - Parent LCL control
           AChild      - Child LCL control
           AOldPos     - Old z position
           ANewPos     - New z position
           AChildren   - List of all child controls

  Sets the child z position of control in Carbon interface in the parent control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetChildZPosition(const AWinControl,
  AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList);
begin
  if not CheckHandle(AWinControl, Self, 'SetChildZPosition') then Exit;
  if not CheckHandle(AChild, Self, 'SetChildZPosition AChild') then Exit;
  
  TCarbonWidget(AWinControl.Handle).SetChildZPosition(TCarbonWidget(AChild.Handle),
    AOldPos, ANewPos, AChildren);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetColor
  Params:  AWinControl - LCL control

  Sets the color of control in Carbon interface according to the LCL control
  NOTE: Functions only for controls with edit
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'SetColor') then Exit;

  TCarbonWidget(AWinControl.Handle).SetColor(AWinControl.Color);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetFont
  Params:  AWinControl - LCL control
           ACursor     - Cursor

  Sets the cursor of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
var
  HitControl: ControlRef;
  TopWindow: WindowRef;
  TopView: HIViewRef;
  MousePos: HIPoint;
begin
  if not CheckHandle(AWinControl, Self, 'SetCursor') then Exit;

  TCarbonWidget(AWinControl.Handle).SetCursor(ACursor);

  if (Screen.RealCursor <> crDefault) then Exit;

  TopWindow := TCarbonWidget(AWinControl.Handle).GetTopParentWindow;
  TopView := HIViewGetRoot(TopWindow);
  HIGetMousePosition(kHICoordSpaceView, TopView, MousePos);
  if OSError(
     HIViewGetSubviewHit(TopView, MousePos, True, HitControl),
     Self, 'TCarbonWSWinControl.SetCursor', 'GetSubviewHit') then Exit;

  if HitControl =  TCarbonWidget(AWinControl.Handle).Widget then
    LCLIntf.SetCursor(ACursor);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetFont
  Params:  AWinControl - LCL control
           AFont       - Font

  Sets the font of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
begin
  if not CheckHandle(AWinControl, Self, 'SetFont') then Exit;
  
  TCarbonWidget(AWinControl.Handle).SetFont(AFont);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetText
  Params:  AWinControl - LCL control
           AText       - Text

  Sets the text (caption) of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: String);
begin
  if not CheckHandle(AWinControl, Self, 'SetText') then Exit;
  
  //DebugLn('TCarbonWSWinControl.SetText ',dbgsName(AWinControl),' ',AText);
  TCarbonWidget(AWinControl.Handle).SetText(AText);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.Invalidate
  Params:  AWinControl - LCL control

  Invalidates control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'Invalidate') then Exit;

  TCarbonWidget(AWinControl.Handle).Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.ShowHide
  Params:  AWinControl - LCL control

  Changes visibility of control in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'ShowHide') then Exit;
  
  TCarbonWidget(AWinControl.Handle).ShowHide(AWinControl.HandleObjectShouldBeVisible);
end;

class procedure TCarbonWSWinControl.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  if not CheckHandle(AWinControl, Self, 'ScrollBy') then Exit;

  TCarbonWidget(AWinControl.Handle).ScrollBy(DeltaX, DeltaY);
  AWinControl.Invalidate;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new win control in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  DebugLn('Warning: Using Carbon custom control instead of not implemented win control!');
    
  Result := TLCLIntfHandle(TCarbonCustomControl.Create(AWinControl, AParams));
end;

class function TCarbonWSWinControl.CanFocus(const AWincontrol: TWinControl): Boolean;
var
  Bit: CFIndex;
  Valid: Boolean;
begin
  Result := inherited;
  if not AWinControl.HandleAllocated then
  begin
    Result:=AWinControl.CanFocus;
    Exit;
  end;
  if not CheckHandle(AWincontrol, Self, 'CanFocus') then Exit;

  Bit := CFPreferencesGetAppIntegerValue(CFSTR('AppleKeyboardUIMode'),
    kCFPreferencesCurrentApplication, Valid{%H-});

  if Valid then
  begin
    if (Bit <> 0) then
      Result := True
    else
      Result := (TCarbonWidget(AWincontrol.Handle) is TCarbonControlWithEdit) or
        (TCarbonWidget(AWincontrol.Handle) is TCarbonDataBrowser) or
          (TCarbonWidget(AWincontrol.Handle) is TCarbonCustomControl);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.AddControl
  Params:  AControl - LCL control to add

  Adds new control to parent control or window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.AddControl(const AControl: TControl);
begin
  if not CheckHandle(AControl as TWinControl, Self, 'AddControl') then Exit;
  if not CheckHandle(AControl.Parent, Self, 'AddControl Parent') then Exit;
  
  TCarbonWidget((AControl as
    TWinControl).Handle).AddToWidget(TCarbonWidget(AControl.Parent.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.DestroyHandle
  Params:  AWinControl - LCL control

  Destroys control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'DestroyHandle') then Exit;

  TCarbonWidget(AWinControl.Handle).FreeCarbonWidget;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetClientBounds
  Params:  AWinControl - LCL control
           ARect       - Record for client bounding rect

  Retrieves the client bounding rect of control in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetClientBounds(
  const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result := False;
  if not CheckHandle(AWinControl, Self, 'GetClientBounds') then Exit;
  
  Result := TCarbonWidget(AWinControl.Handle).GetClientRect(ARect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetClientRect
  Params:  AWinControl - LCL control
           ARect       - Record for client rect

  Retrieves the client rect of control in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetClientRect(
  const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result := False;
  if not CheckHandle(AWinControl, Self, 'GetClientRect') then Exit;
  
  Result := TCarbonWidget(AWinControl.Handle).GetClientRect(ARect);
  if Result then OffsetRect(ARect, -ARect.Left, -ARect.Top);
end;

{ TCarbonWSCustomControl }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomControl.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new custom control in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSCustomControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonCustomControl.Create(AWinControl, AParams));
end;

end.
