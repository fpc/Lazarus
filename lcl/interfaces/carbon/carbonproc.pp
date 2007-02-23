{ $Id$
                  ----------------------------------------
                  carbonproc.pp  -  Carbon interface procs
                  ----------------------------------------

 @created(Wed Aug 26st WET 2005)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains procedures/functions needed for the Carbon <-> LCL interface
 Common carbon untilities (usable by other projects) go to CarbonUtils

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

unit CarbonProc;
{$mode objfpc}{$H+}

interface

uses
  FPCMacOSAll, Classes, LCLType,
  LCLProc, LCLClasses, Controls, LMessages, Forms, Avl_Tree, SysUtils, Graphics,
  CarbonDef;

function CreateWidgetInfo(AWidget: Pointer; AObject: TLCLComponent;
  TheType: TCarbonWidgetType): PWidgetInfo;
function CreateWndWidgetInfo(AWidget: Pointer; AObject: TLCLComponent) : PWidgetInfo;
function CreateCtrlWidgetInfo(AWidget: Pointer; AObject: TLCLComponent) : PWidgetInfo;

procedure FreeWidgetInfo(AInfo: PWidgetInfo);

function GetCtrlWidgetInfo(AWidget: Pointer): PWidgetInfo;
function GetWndWidgetInfo(AWidget: Pointer): PWidgetInfo;
function GetWidgetInfo(AWidget: Pointer): PWidgetInfo;
function GetWidgetType(AWidget: Pointer): TCarbonWidgetType;
function GetWidgetType(AWidget: Pointer; var AInfo: PWidgetInfo): TCarbonWidgetType;

function DeliverMessage(ATarget: TObject; var AMessage): Integer;

function GetTopParentWindow(AWinControl: TWinControl): WindowRef;
function GetCarbonLocalWindowRect(Handle: hwnd; var ARect: TRect; Info: PWidgetInfo = nil): Boolean;
function GetCarbonClientRect(Handle: hwnd; var ARect: TRect; Info: PWidgetInfo = nil): Boolean;

procedure InvalidateCarbonControl(AControl: HWnd); inline;
function FindCarbonFontID(const FontName: String): ATSUFontID;

function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);

procedure CreateCarbonString(const S: String; var AString: CFStringRef); inline;
procedure FreeCarbonString(var AString: CFStringRef); inline;
function CarbonStringToString(AString: CFStringRef): String;

function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;

function ColorToCarbonColor(const AColor: TColor): RGBColor;
function CarbonColorToColor(const AColor: RGBColor): TColor; inline;

function Dbgs(const ARect: FPCMacOSAll.Rect): string; overload;
function Dbgs(const AColor: FPCMacOSAll.RGBColor): string; overload;

implementation

{------------------------------------------------------------------------------
  Name:    CreateWidgetInfo
  Params:  AWidget - Pointer to widget
           AObject - LCL object
           TheType - Type of Carbon widget (Control or Window)
  Returns: Pointer to widget info

  Creates basic info for specified widget and LCL object
 ------------------------------------------------------------------------------}
function CreateWidgetInfo(AWidget: Pointer; AObject: TLCLComponent;
  TheType: TCarbonWidgetType): PWidgetInfo;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
  Result^.LCLObject := AObject;
  Result^.Widget := AWidget;
  Result^.WSClass := AObject.WidgetSetClass;
  Result^.widgetType := TheType;
  
  case TheType of
    cwtControlRef: SetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @Result);
    cwtWindowRef : SetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @Result);
  else Debugln('[CreateWidgetInfo] ***WARNING! Unknown widget type***');
  end;
end;


{------------------------------------------------------------------------------
  Name:    CreateWndWidgetInfo
  Params:  AWidget - Pointer to widget
           AObject - LCL object
  Returns: Pointer to widget info

  Creates basic info for specified window widget and LCL object
 ------------------------------------------------------------------------------}
function CreateWndWidgetInfo(AWidget: Pointer; AObject: TLCLComponent) : PWidgetInfo;
begin
  Result := CreateWidgetInfo(AWidget, AObject, cwtWindowRef);
end;

{------------------------------------------------------------------------------
  Name:    CreateWidgetInfo
  Params:  AWidget - Pointer to widget
           AObject - LCL object
  Returns: Pointer to widget info

  Creates basic info for specified control widget and LCL object
 ------------------------------------------------------------------------------}
function CreateCtrlWidgetInfo(AWidget: Pointer; AObject: TLCLComponent) : PWidgetInfo;
begin
  Result := CreateWidgetInfo(AWidget, AObject, cwtControlRef);
end;

{------------------------------------------------------------------------------
  Name:    FreeWidgetInfo
  Params:  AInfo - Pointer to widget info
  Returns: Nothing

  Frees the specified widget info
 ------------------------------------------------------------------------------}
procedure FreeWidgetInfo(AInfo: PWidgetInfo);
begin
  if AInfo = nil then Exit;
  
  case AInfo^.WidgetType of
    cwtControlRef: RemoveControlProperty(AInfo^.Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
    cwtWindowRef : RemoveWindowProperty(AInfo^.Widget, LAZARUS_FOURCC, WIDGETINFO_FOURCC);
  else Debugln('[FreeWidgetInfo] ***WARNING! Unknown widget type***');
  end;

  if (AInfo^.UserData <> nil) and (AInfo^.DataOwner) then
  begin
    System.FreeMem(AInfo^.UserData);
    AInfo^.UserData := nil;
  end;

  Dispose(AInfo);
end;

{------------------------------------------------------------------------------
  Name:    GetCtrlWidgetInfo
  Params:  AWidget - Pointer to control widget
  Returns: Pointer to widget info

  Retrieves basic info for specified control widget
 ------------------------------------------------------------------------------}
function GetCtrlWidgetInfo(AWidget: Pointer): PWidgetInfo;
var
  M: LongWord;
begin
  GetControlProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @M, @Result);
end;

{------------------------------------------------------------------------------
  Name:    GetWndWidgetInfo
  Params:  AWidget - Pointer to window widget
  Returns: Pointer to widget info

  Retrieves basic info for specified window widget
 ------------------------------------------------------------------------------}
function GetWndWidgetInfo(AWidget: Pointer): PWidgetInfo;
var
  M: LongWord;
begin
  GetWindowProperty(AWidget, LAZARUS_FOURCC, WIDGETINFO_FOURCC, SizeOf(Result), @M, @Result);
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetInfo
  Params:  AWidget - Pointer to widget
  Returns: Pointer to widget info

  Retrieves basic info for specified widget (control or window)
 ------------------------------------------------------------------------------}
function GetWidgetInfo(AWidget: Pointer): PWidgetInfo;
begin
  if IsValidControlHandle(AWidget) then Result := GetCtrlWidgetInfo(AWidget)
  else
    // there is no (cheap) check for windows so assume a window
    // when it is not a control.
    Result := GetWndWidgetInfo(AWidget);
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetType
  Params:  AWidget - Pointer to widget
  Returns: Widget type

  Retrieves the type of specified widget (Control or Window)
 ------------------------------------------------------------------------------}
function GetWidgetType(AWidget: Pointer): TCarbonWidgetType;
var
   AInfo: PWidgetInfo;
begin
  Result := cwtUnknown;
  
  AInfo := GetWidgetInfo(AWidget);
  if AInfo = nil then Exit;
  
  Result := AInfo^.WidgetType;
end;

{------------------------------------------------------------------------------
  Name:    GetWidgetType
  Params:  AWidget - Pointer to widget
           AInfo   - Pointer to widget info
  Returns: Widget type

  Retrieves the type of specified widget (Control or Window) according to
  passed info. If info is not set then it is retrieved from widget.
 ------------------------------------------------------------------------------}
function GetWidgetType(AWidget: Pointer; var AInfo: PWidgetInfo): TCarbonWidgetType;
begin
  Result := cwtUnknown;
  if AInfo = nil then AInfo := GetWidgetInfo(AWidget);
  if AInfo = nil then Exit;

  Result := AInfo^.WidgetType;
end;

{------------------------------------------------------------------------------
  Name:    DeliverMessage
  Params:  Message: the message to process
  Returns: True if handled

  Generic function which calls the WindowProc if defined, otherwise the
  dispatcher
 ------------------------------------------------------------------------------}
function DeliverMessage(ATarget: TObject; var AMessage): Integer;
begin
  if ATarget = nil
  then begin
    DebugLn('[DeliverMessage] Target = nil');
    Result := 0;
    Exit;
  end;

  try
    if TObject(ATarget) is TControl
    then TControl(ATarget).WindowProc(TLMessage(AMessage))
    else TObject(ATarget).Dispatch(TLMessage(AMessage));
  except
    Application.HandleException(nil);
  end;

  Result := TLMessage(AMessage).Result;
end;

//=====================================================
// UPP mamanger
//=====================================================
type
  TUPPAVLTreeNode = class(TAVLTreeNode)
  public
    UPP: EventHandlerUPP;
    RefCount: Integer;
    procedure Clear; reintroduce; // not overridable, so reintroduce since we only will call this clear
    destructor Destroy; override;
  end;

var
  UPPTree: TAVLTree = nil;

procedure TUPPAVLTreeNode.Clear;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Clear;
end;

destructor TUPPAVLTreeNode.Destroy;
begin
  if UPP <> nil
  then begin
    DisposeEventHandlerUPP(UPP);
    UPP := nil;
  end;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Name:    GetTopParentWindow
  Params:  AWinControl - Window control
  Returns: Window reference

  Retrieves the parent window reference of the control
 ------------------------------------------------------------------------------}
function GetTopParentWindow(AWinControl: TWinControl): WindowRef;
var
  Window: TControl;
begin
  if AWinControl = nil then
  begin
    Result := nil;
    Exit;
  end;
   
  Window := AWinControl.GetTopParent;

  if Window is TCustomForm then Result := WindowRef((Window as TCustomForm).Handle)
  else Result := nil;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonLocalWindowRect
  Params:  Handle - Handle of window
           ARect  - Record for window coordinates
           Info   - Pointer to widget info (optional)
  Returns: If function succeeds

  Returns the window bounding rectangle relative to the client origin of its
  parent
 ------------------------------------------------------------------------------}
function GetCarbonLocalWindowRect(Handle: hwnd; var ARect: TRect; Info: PWidgetInfo): Boolean;
var
  AWndRect: FPCMacOSAll.Rect;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle), Info) of
  cwtWindowRef:
    Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowStructureRgn, AWndRect) = noErr;
  cwtControlRef:
    Result := FPCMacOSAll.GetControlBounds(ControlRef(Handle), AWndRect) <> nil;
  end;
  
  if not Result then Exit;
  ARect := CarbonRectToRect(AWndRect);
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonClientRect
  Params:  Handle - Handle of window
           ARect  - Record for client area coordinates
           Info   - Pointer to widget info (optional)
  Returns: If function succeeds

  Returns the window client rectangle relative to the client area parent
  window origin
 ------------------------------------------------------------------------------}
function GetCarbonClientRect(Handle: hwnd; var ARect: TRect; Info: PWidgetInfo): Boolean;
var
  AWndRect, AClientRect: FPCMacOSAll.Rect;
  OSResult: OSStatus;
  ClientRegion: FPCMacOSAll.RgnHandle;
begin
  Result := False;
  
  case GetWidgetType(Pointer(Handle), Info) of
  cwtWindowRef:
  begin
    Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowStructureRgn, AWndRect) = noErr;
    if Result then
    begin
      Result := FPCMacOSAll.GetWindowBounds(WindowRef(Handle), kWindowContentRgn, AClientRect) = noErr;
    end;
    if Result then
    begin
      ARect.Left := AClientRect.Left - AWndRect.Left;
      ARect.Top := AClientRect.Top - AWndRect.Top;
      ARect.Right := AClientRect.Right - AWndRect.Left;
      ARect.Bottom := AClientRect.Bottom - AWndRect.Top;
    end;
  end;
  cwtControlRef:
  begin
    ClientRegion := FPCMacOSAll.NewRgn();
    try
      OSResult := GetControlRegion(ControlRef(Handle), kControlContentMetaPart,
        ClientRegion);

      if OSResult = errInvalidPartCode then
      begin
        // controls without content area have clientrect = boundsrect
        Result := FPCMacOSAll.GetControlBounds(ControlRef(Handle), AClientRect) <> nil;
        if Result then
        begin
          ARect := CarbonRectToRect(AClientRect);
          OffsetRect(ARect, -ARect.Left, -ARect.Top);
        end;
      end
      else
      begin
        Result := OSResult = noErr;

        if Result then
        begin
          Result := GetRegionBounds(ClientRegion, AClientRect) <> nil;
          if Result then ARect := CarbonRectToRect(AClientRect);
        end;
      end;
    finally
      FPCMacOSAll.DisposeRgn(ClientRegion);
    end;
  end;
  end;
end;

{------------------------------------------------------------------------------
  Name:    InvalidateCarbonControl
  Params:  AControl - Handle of control
  Returns: Nothing
  
  Invalidates specified control
 ------------------------------------------------------------------------------}
procedure InvalidateCarbonControl(AControl: HWnd);
begin
  HiViewSetNeedsDisplay(HIViewRef(AControl), True);
end;

{------------------------------------------------------------------------------
  Name:    FindCarbonFontID
  Params:  FontName - The font name
  Returns: Caron font ID

  Finds ID of specified font
 ------------------------------------------------------------------------------}
function FindCarbonFontID(const FontName: String): ATSUFontID;
begin
  Result := 0;

  if (FontName <> '') and not SameText(FontName, 'default') then
   ATSUFindFontFromName(@FontName[1], Length(FontName), kFontFamilyName,
     kFontNoPlatform, kFontNoScript, kFontNoLanguage, Result);
end;

{------------------------------------------------------------------------------
  Name:    RegisterEventHandler
  Params:  AHandler - Carbon event handler procedure
  Returns: Event handler UPP

  Registers new carbon event handler procedure
 ------------------------------------------------------------------------------}
function RegisterEventHandler(AHandler: TCarbonWSEventHandlerProc): EventHandlerUPP;
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then UPPTree := TAVLTree.Create;
  
  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then
  begin
    Node := TUPPAVLTreeNode.Create;
    Node.Data := AHandler;
    Node.UPP := NewEventHandlerUPP(EventHandlerProcPtr(AHandler));
    UPPTree.Add(Node);
  end;
  
  Inc(Node.Refcount);
  Result := Node.UPP;
end;

{------------------------------------------------------------------------------
  Name:    UnRegisterEventHandler
  Params:  AHandler - Carbon event handler procedure
  Returns: Nothing

  Unregisters event handler procedure
 ------------------------------------------------------------------------------}
procedure UnRegisterEventHandler(AHandler: TCarbonWSEventHandlerProc);
var
  Node: TUPPAVLTreeNode;
begin
  if UPPTree = nil then Exit; //???
  Node := TUPPAVLTreeNode(UPPTree.Find(AHandler));
  if Node = nil then Exit; //???
  if Node.Refcount <= 0 then
  begin
    DebugLn('[UnRegisterEventHandler] UPPInconsistency, Node.RefCount <= 0');
    Exit;
  end;

  Dec(Node.Refcount);
  if Node.Refcount > 0 then Exit;

  // Sigh !
  // there doesn't exist a light version of the avltree without buildin memmanager
  // So, just free it and "pollute" the memmanager with our classes;
  // Freeing our node is also not an option, since that would
  // corrupt the tree (no handling for that).
  // Tweaking the memmanager is also not possible since only the class is public
  // and not the manager itself.

  Node.Clear;
  UPPTree.Delete(Node);
end;

{------------------------------------------------------------------------------
  Name:    CreateCarbonString
  Params:  S       - UTF-8 string
           AString - Core Foundation string ref
  Returns: Nothing

  Creates new Core Foundation string form specified string
 ------------------------------------------------------------------------------}
procedure CreateCarbonString(const S: String; var AString: CFStringRef);
begin
  AString := CFStringCreateWithCString(nil, Pointer(PChar(S)), DEFAULT_CFSTRING_ENCODING);
end;

{------------------------------------------------------------------------------
  Name:    FreeCarbonString
  Params:  AString - Core Foundation string ref to free
  Returns: Nothing

  Frees specified Core Foundation string
 ------------------------------------------------------------------------------}
procedure FreeCarbonString(var AString: CFStringRef);
begin
  if AString <> nil then
    CFRelease(Pointer(AString));
end;

{------------------------------------------------------------------------------
  Name:    CarbonStringToString
  Params:  AString - Core Foundation string ref
  Returns: UTF-8 string

  Converts Core Foundation string to string
 ------------------------------------------------------------------------------}
function CarbonStringToString(AString: CFStringRef): String;
var
  Str: Pointer;
  StrSize: CFIndex;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, DEFAULT_CFSTRING_ENCODING);
  {if Str <> nil then
    Result := PChar(Str)
  else}
  begin
    // if that doesn't work this will
    StrSize := CFStringGetLength(AString) + 1; // + 1 for null terminator
    GetMem(Str, StrSize);
    try
      CFStringGetCString(AString, Str, StrSize, DEFAULT_CFSTRING_ENCODING);
      Result := PChar(Str);
    finally
      System.FreeMem(Str);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonRect
  Params:  Left, Top, Width, Height - coordinates
  Returns: Carbon Rect

  Carbon Rect constructor
 ------------------------------------------------------------------------------}
function GetCarbonRect(Left, Top, Width, Height: Integer): FPCMacOSAll.Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

{------------------------------------------------------------------------------
  Name:    GetCarbonRect
  Params:  ARect - Rectangle
  Returns: Carbon Rect

  Carbon Rect constructor
 ------------------------------------------------------------------------------}
function GetCarbonRect(const ARect: TRect): FPCMacOSAll.Rect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    ParamsToCarbonRect
  Params:  AParams - Creation parameters
  Returns: Carbon Rect

  Carbon Rect constructor from creation parameters
 ------------------------------------------------------------------------------}
function ParamsToCarbonRect(const AParams: TCreateParams): FPCMacOSAll.Rect;
begin
  Result.Left := AParams.X;
  Result.Top := AParams.Y;
  Result.Right := AParams.X + AParams.Width;
  Result.Bottom := AParams.Y + AParams.Height;
end;

{------------------------------------------------------------------------------
  Name:    CarbonRectToRect
  Params:  ARect - Carbon Rect
  Returns: Rectangle

  Converts Carbon Rect to rectangle
 ------------------------------------------------------------------------------}
function CarbonRectToRect(const ARect: FPCMacOSAll.Rect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Right := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

{------------------------------------------------------------------------------
  Name:    ColorToCarbonColor
  Params:  AColor - color
  Returns: RGBColor

  Converts color to Carbon RGBColor
 ------------------------------------------------------------------------------}
function ColorToCarbonColor(const AColor: TColor): RGBColor;
var
  V: TColor;
begin
  V := ColorToRGB(AColor);
  
  Result.Red := Red(V);
  Result.Red := (Result.Red shl 8) or Result.Red;
  Result.Green := Green(V);
  Result.Green := (Result.Green shl 8) or Result.Green;
  Result.Blue := Blue(V);
  Result.Blue := (Result.Blue shl 8) or Result.Blue;
end;

{------------------------------------------------------------------------------
  Name:    CarbonColorToColor
  Params:  AColor - Carbon RGBColor
  Returns: Color

  Converts Carbon RGBColor to color
 ------------------------------------------------------------------------------}
function CarbonColorToColor(const AColor: RGBColor): TColor;
begin
  Result := RGBToColor(AColor.Red shr 8, AColor.Green shr 8, AColor.Blue shr 8);
end;

function Dbgs(const ARect: FPCMacOSAll.Rect): String;
begin
  Result:=IntToStr(ARect.left)+','+IntToStr(ARect.top)
          +','+IntToStr(ARect.right)+','+IntToStr(ARect.bottom);
end;

function Dbgs(const AColor: FPCMacOSAll.RGBColor): String;
begin
  Result := 'R: ' + IntToHex(AColor.Red, 4)
    + 'G: ' + IntToHex(AColor.Green, 4)
    + 'B: ' + IntToHex(AColor.Blue, 4);
end;

finalization
  if UPPTree <> nil
  then FreeAndNil(UPPTree);

end.
