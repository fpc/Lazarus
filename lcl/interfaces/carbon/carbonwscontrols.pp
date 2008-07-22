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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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

// debugging defines
{$I carbondebug.inc}

uses
  // libs
{$ifdef ver2_2_0}
  FPCMacOSAll,
{$else}
  MacOSAll,
{$endif}
  CarbonUtils, Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, LCLType, LMessages, LCLProc,
  // widgetset
  WSControls, WSLCLClasses, WSProc,
  // LCL Carbon
  CarbonDef, CarbonPrivate;

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
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddControl(const AControl: TControl); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
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

uses
  CarbonProc;

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
begin
  if not CheckHandle(AWinControl, Self, 'SetCursor') then Exit;

  TCarbonWidget(AWinControl.Handle).SetCursor(ACursor);
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
  
  TCarbonWidget(AWinControl.Handle).ShowHide(AWinControl.Visible);
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
  raise
    Exception.CreateFmt(ClassName + '.CreateHandle Error:' +
      'Not implemented - unable to create Carbon win control for %s: %s!',
    [AWinControl.Name, AWinControl.ClassName]);
    
  Result := TLCLIntfHandle(nil);
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

  TCarbonWidget(AWinControl.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSWinControl.GetClientBounds
  Params:  AWinControl - LCL control
           ARect       - Record for client bounding rect

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
