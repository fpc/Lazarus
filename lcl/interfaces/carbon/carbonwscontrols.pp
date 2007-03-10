{ $Id$}
{
 *****************************************************************************
 *                            CarbonWSControls.pp                            *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit CarbonWSControls;

{$mode objfpc}{$H+}

interface

uses
  // libs
  FPCMacOSAll, CarbonUtils, CarbonExtra, Classes,
  // LCL
  Forms, Controls, Graphics, LCLType, LMessages, LCLProc,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // interface
  CarbonDef, CarbonProc, CarbonPrivate;

type

  { TCarbonWSDragImageList }

  TCarbonWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TCarbonWSControl }

  TCarbonWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TCarbonWSWinControl }

  TCarbonWSWinControlClass = class of TCarbonWSWincontrol;
  TCarbonWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure AddControl(const AControl: TControl); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TCarbonWSGraphicControl }

  TCarbonWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomControl }

  TCarbonWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCarbonWSImageList }

  TCarbonWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation

{ TCarbonWSWinControl }

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetPreferredSize
  Params:  AWinControl     - LCL control
           PreferredWidth  - Preferred width, valid if > 0
           PreferredHeight - Preferred height, valid if > 0
           WithThemeSpace  - Whether to include space for theme
  Returns: Nothing

  Retrieves the preferred size of control in Carbon interface to support
  autosizing of controls
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  R: FPCMacOSAll.Rect;
  S: SmallInt;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetPreferredSize') then Exit;
  
  R := GetCarbonRect(0, 0, 0, 0);
  if GetBestControlRect(ControlRef(AWinControl.Handle), R, S) = noErr then
  begin
    PreferredWidth := R.right - R.left;
    PreferredHeight := R.bottom - R.top;
  end
  else
  begin
    PreferredWidth := 0;
    PreferredHeight := 0;
  end;
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
var
  CFString: CFStringRef;
begin
  Result := False;
  if not WSCheckHandleAllocated(AWinControl, 'GetText') then Exit;

  CFString := HIViewCopyText(HIViewRef(AWinControl.Handle));
  if CFString = nil then Exit;
  try
    AText := CarbonStringToString(CFString);
    Result := True;
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetBounds
  Params:  AWinControl - LCL control
           ALeft, ATop, AWidth, AHeight - Coordinates
  Returns: Nothing

  Sets the bounds of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  NewBounds: FPCMacOSAll.Rect;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBounds') then Exit;

  NewBounds.Left := AWinControl.Left;
  NewBounds.Top := AWinControl.Top;
  NewBounds.Right := AWinControl.Left + AWinControl.Width;
  NewBounds.Bottom := AWinControl.Top + AWinControl.Height;
  DebugLn('TCarbonWSWinControl.SetBounds ', dbgsName(AWinControl), ' NewBounds=', dbgs(NewBounds));

  SetControlBounds(ControlRef(AWinControl.Handle), NewBounds);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetChildZPosition
  Params:  AWinControl - Parent LCL control
           AChild      - Child LCL control
           AOldPos     - Old z position
           ANewPos     - New z position
           AChildren   - List of all child controls
  Returns: Nothing

  Sets the child z position of control in Carbon interface in the parent control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetChildZPosition(const AWinControl,
  AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList);
var
  RefView: HIViewRef;
  Order: HIViewZOrderOp;
  I, StopPos: Integer;
  Child: TWinControl;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetChildZPosition') then Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition AChild') then Exit;
  
  RefView := nil;
  if ANewPos <= 0 then // send behind all
    Order := kHIViewZOrderBelow
  else
    if ANewPos >= Pred(AChildren.Count) then // bring to front of all
      Order := kHIViewZOrderAbove
    else // custom position
    begin
      // Search for the first child above us with a handle.
      // The child list is reversed form the windows order.
      // If we don't find an allocated handle then exit.
      
      if AOldPos > ANewPos then
        StopPos := AOldPos // the child is moved to the bottom
      else
        StopPos := Pred(AChildren.Count); // the child is moved to the top

      for I := Succ(ANewPos) to StopPos do
      begin
        Child := TWinControl(AChildren[I]);
        if Child.HandleAllocated then
        begin
          RefView := HIViewRef(Child.Handle);
          Order := kHIViewZOrderBelow;
          Break;
        end;
      end;
      
      if RefView = nil then Exit;
    end;
    
  HIViewSetZOrder(HIViewRef(AChild.Handle), Order, RefView);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetColor
  Params:  AWinControl - LCL control
  Returns: Nothing

  Sets the color of control in Carbon interface according to the LCL control
  NOTE: Functions only for edit
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetColor(const AWinControl: TWinControl);
var
  FontStyle: ControlFontStyleRec;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetColor') then Exit;

  // preserve other font settings
  GetControlData(ControlRef(AWinControl.Handle), kControlEntireControl,
    kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil);

  FontStyle.flags := FontStyle.flags or kControlUseBackColorMask;
  FontStyle.backColor := ColorToCarbonColor(AWinControl.Color);

  SetControlFontStyle(ControlRef(AWinControl.Handle), FontStyle);
  // invalidate control
  InvalidateCarbonControl(AWinControl.Handle);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetFont
  Params:  AWinControl - LCL control
           AFont       - Font
  Returns: Nothing

  Sets the font of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  FontStyle: ControlFontStyleRec;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont') then Exit;

  // get current font style to preserve justification
  GetControlData(ControlRef(AWinControl.Handle), kControlEntireControl,
    kControlFontStyleTag, SizeOf(FontStyle), @FontStyle, nil);
  
  FontStyle.flags := FontStyle.flags or kControlUseFontMask or kControlUseSizeMask or
    kControlUseFaceMask or kControlUseForeColorMask;

  FontStyle.font := FindCarbonFontID(AFont.Name);
  FontStyle.size := AFont.Size;
  
  FontStyle.style := 0;
  if fsBold in AFont.Style then FontStyle.style := FontStyle.style or 1;
  if fsItalic in AFont.Style then FontStyle.style := FontStyle.style or 2;
  if fsUnderline in AFont.Style then FontStyle.style := FontStyle.style or 4;
  // fsStrikeOut has no counterpart?
  
  FontStyle.foreColor := ColorToCarbonColor(AFont.Color);
  
  DebugLn('Set Font: ', DbgS(FontStyle.font) + ' style: ' + DbgS(AFont.Style));
  
  SetControlFontStyle(ControlRef(AWinControl.Handle), FontStyle);
  // invalidate control
  InvalidateCarbonControl(AWinControl.Handle);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetText
  Params:  AWinControl - LCL control
           AText       - Text
  Returns: Nothing

  Sets the text (caption) of control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  CFString: CFStringRef;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetText') then Exit;
  
  DebugLn('TCarbonWSWinControl.SetText ',dbgsName(AWinControl),' ',AText);

  CreateCarbonString(AText, CFString);
  try
    HIViewSetText(HIViewRef(AWinControl.Handle), CFString);
  finally
    FreeCarbonString(CFString);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.Invalidate
  Params:  AWinControl - LCL control
  Returns: Nothing

  Invalidates control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'Invalidate') then Exit;

  HIViewSetNeedsDisplay(HIViewRef(AWinControl.Handle), True);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.ShowHide
  Params:  AWinControl - LCL control
  Returns: Nothing

  Changes visibility of control in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'ShowHide') then Exit;
  
  HIViewSetVisible(HIViewRef(AWinControl.Handle), AWinControl.Visible);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.AddControl
  Params:  AControl - LCL control to add
  Returns: Nothing

  Adds new control to parent control or window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.AddControl(const AControl: TControl);
var
  ParentView: HIViewRef;
begin
  if not WSCheckHandleAllocated(AControl as TWinControl, 'AddControl') then Exit;
  if not WSCheckHandleAllocated(AControl.Parent, 'AddControl Parent') then Exit;
  
  if AControl.Parent is TCustomForm then
  begin // parent is Carbon window -> add to window content
    DebugLn('AddControl ' + AControl.Name + ' in form ' + AControl.Parent.Name);
    ParentView := GetCarbonWindowContent(AControl.Parent.Handle);
  end
  else
  begin
    DebugLn('AddControl ' + AControl.Name + ' in ' + AControl.Parent.Name);
    ParentView := HIViewRef(AControl.Parent.Handle);
  end;
  
  HIViewAddSubview(ParentView, HIViewRef((AControl as TWinControl).Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.DestroyHandle
  Params:  AWinControl - LCL control
  Returns: Nothing

  Destroys control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  if not WSCheckHandleAllocated(AWinControl, 'DestroyHandle') then Exit;
  
  DebugLn('TCarbonWSWinControl.DestroyHandle ',dbgsName(AWinControl));
  DisposeControl(ControlRef(AWinControl.Handle));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetClientBounds
  Params:  AWinControl - LCL control
           ARect       - Record for client bounding rect
  Returns: Nothing

  Retrieves the client bounding rect of control in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetClientBounds(const AWinControl: TWinControl;
  var ARect: TRect): Boolean;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetClientBounds') then Exit;
  
  Result := GetCarbonClientRect(AWinControl.Handle, ARect);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetClientRect
  Params:  AWinControl - LCL control
           ARect       - Record for client rect
  Returns: Nothing

  Retrieves the client rect of control in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSWinControl.GetClientRect(const AWinControl: TWinControl;
  var ARect: TRect): Boolean;
begin
  if not WSCheckHandleAllocated(AWinControl, 'GetClientRect') then Exit;
  
  Result := GetCarbonClientRect(AWinControl.Handle, ARect);
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
var
  Control: ControlRef;
  Attrs: LongWord;
  Info: TCarbonWidgetInfo;
begin
  Result := 0;

  Attrs := 0;

  if CreateUserPaneControl(GetTopParentWindow(AWinControl), ParamsToCarbonRect(AParams),
    Attrs, Control) = noErr
  then
    Result := TLCLIntfHandle(Control);

  if Result = 0 then Exit;

  Info := TCarbonWidgetInfo.CreateForControl(Control, AWinControl);
  TCarbonPrivateHandleClass(WSPrivate).RegisterEvents(Info);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TCarbonWSDragImageList);
//  RegisterWSComponent(TControl, TCarbonWSControl);
  RegisterWSComponent(TWinControl, TCarbonWSWinControl, TCarbonPrivateHiView);
//  RegisterWSComponent(TGraphicControl, TCarbonWSGraphicControl);
  RegisterWSComponent(TCustomControl, TCarbonWSCustomControl);
//  RegisterWSComponent(TImageList, TCarbonWSImageList);
////////////////////////////////////////////////////

finalization
//  if MCarbonWSWinControl_Dispose_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Dispose_UPP);
//  if MCarbonWSWinControl_Hit_UPP <> nil then DisposeEventHandlerUPP(MCarbonWSWinControl_Hit_UPP);

end.
