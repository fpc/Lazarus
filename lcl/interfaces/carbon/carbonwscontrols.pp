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
  Returns: Nothing

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
  if not CheckHandle(AWinControl, Self, 'SetChildZPosition') then Exit;
  if not CheckHandle(AChild, Self, 'SetChildZPosition AChild') then Exit;
  
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
          RefView := AsControlRef(Child.Handle);
          Order := kHIViewZOrderBelow;
          Break;
        end;
      end;
      
      if RefView = nil then Exit;
    end;
    
  HIViewSetZOrder(AsControlRef(AChild.Handle), Order, RefView);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.SetColor
  Params:  AWinControl - LCL control
  Returns: Nothing

  Sets the color of control in Carbon interface according to the LCL control
  NOTE: Functions only for edit control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'SetColor') then Exit;

  TCarbonWidget(AWinControl.Handle).SetColor(AWinControl.Color);
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
begin
  if not CheckHandle(AWinControl, Self, 'SetFont') then Exit;
  
  TCarbonWidget(AWinControl.Handle).SetFont(AFont);
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
begin
  if not CheckHandle(AWinControl, Self, 'SetText') then Exit;
  
  //DebugLn('TCarbonWSWinControl.SetText ',dbgsName(AWinControl),' ',AText);
  TCarbonWidget(AWinControl.Handle).SetText(AText);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.Invalidate
  Params:  AWinControl - LCL control
  Returns: Nothing

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
  Returns: Nothing

  Changes visibility of control in Carbon interface according to the LCL control
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'ShowHide') then Exit;
  
  TCarbonWidget(AWinControl.Handle).ShowHide(AWinControl.Visible);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.AddControl
  Params:  AControl - LCL control to add
  Returns: Nothing

  Adds new control to parent control or window in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.AddControl(const AControl: TControl);
begin
  if not CheckHandle(AControl as TWinControl, Self, 'AddControl') then Exit;
  if not CheckHandle(AControl.Parent, Self, 'AddControl Parent') then Exit;
  
  // add frame control to content area
  HIViewAddSubview(TCarbonWidget(AControl.Parent.Handle).Content,
    TCarbonControl((AControl as TWinControl).Handle).Frame);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.DestroyHandle
  Params:  AWinControl - LCL control
  Returns: Nothing

  Destroys control in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
  if not CheckHandle(AWinControl, Self, 'DestroyHandle') then Exit;

  TCarbonWidget(AWinControl.Handle).Free;
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
  Result := False;
  if not CheckHandle(AWinControl, Self, 'GetClientBounds') then Exit;
  
  Result := TCarbonWidget(AWinControl.Handle).GetClientRect(ARect);
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

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TCarbonWSDragImageList);
//  RegisterWSComponent(TControl, TCarbonWSControl);
  RegisterWSComponent(TWinControl, TCarbonWSWinControl);
//  RegisterWSComponent(TGraphicControl, TCarbonWSGraphicControl);
  RegisterWSComponent(TCustomControl, TCarbonWSCustomControl);
//  RegisterWSComponent(TImageList, TCarbonWSImageList);
////////////////////////////////////////////////////


end.
