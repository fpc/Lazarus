{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSComCtrls.pp                              * 
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
unit CarbonWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  FPCMacOSAll, CarbonDef, CarbonProc, CarbonUtils,
  Classes, Controls, ComCtrls, LCLType, LCLProc, LMessages, LCLMessageGlue, Math,
////////////////////////////////////////////////////
  WSComCtrls, WSLCLClasses, WSControls, WSProc,
  CarbonWSControls, CarbonPrivate;

type

  { TCarbonWSStatusBar }

  TCarbonWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TCarbonWSTabSheet }

  TCarbonWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TCarbonWSPageControl }

  TCarbonWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomListView }

  TCarbonWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TCarbonWSListView }

  TCarbonWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TCarbonWSProgressBar }

  TCarbonWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
  end;

  { TCarbonWSCustomUpDown }

  TCarbonWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TCarbonWSUpDown }

  TCarbonWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TCarbonWSToolButton }

  TCarbonWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TCarbonWSTrackBar }

  TCarbonWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
  end;

  { TCarbonWSCustomTreeView }

  TCarbonWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TCarbonWSTreeView }

  TCarbonWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

{ TCarbonWSProgressBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new progress bar in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSProgressBar.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonProgressBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.ApplyChanges
  Params:  AProgressBar - LCL custom progress bar

  Sets the parameters (Min, Max, Position) of progress bar in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
begin
  if not WSCheckHandleAllocated(AProgressBar, 'ApplyChanges') then Exit;

  TCarbonCustomBar(AProgressBar.Handle).SetData(AProgressBar.Position,
    AProgressBar.Min, AProgressBar.Max);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSProgressBar.SetPosition
  Params:  AProgressBar - LCL custom progress bar
           NewPosition  - New position

  Sets the position of progress bar in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if not WSCheckHandleAllocated(AProgressBar, 'SetPosition') then Exit;

  TCarbonCustomBar(AProgressBar.Handle).SetData(AProgressBar.Position);
end;

{ TCarbonWSTrackBar }

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new slider in Carbon interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCarbonWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result := TLCLIntfHandle(TCarbonTrackBar.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.ApplyChanges
  Params:  ATrackBar - LCL custom track bar

  Sets the parameters (Min, Max, Position, Ticks) of slider in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  CarbonTrackBar: TCarbonTrackBar;
begin
  if not WSCheckHandleAllocated(ATrackBar, 'ApplyChanges') then Exit;

  CarbonTrackBar := TCarbonTrackBar(ATrackBar.Handle);
  
  if CarbonTrackBar.Ticks <> CarbonTrackBar.GetTicks then
    RecreateWnd(ATrackBar) // recreate track bar if ticks have changed
  else
    CarbonTrackBar.SetData(ATrackBar.Position, ATrackBar.Min, ATrackBar.Max);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.GetPosition
  Params:  ATrackBar - LCL custom track bar
  Returns: Position of sliderr in Carbon interface
 ------------------------------------------------------------------------------}
class function TCarbonWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
begin
  Result := 0;
  if not WSCheckHandleAllocated(ATrackBar, 'GetPosition') then Exit;

  Result := TCarbonTrackBar(ATrackBar.Handle).GetPos;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSTrackBar.SetPosition
  Params:  ATrackBar - LCL custom track bar
           NewPosition  - New position

  Sets the position of slider in Carbon interface
 ------------------------------------------------------------------------------}
class procedure TCarbonWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
begin
  if not WSCheckHandleAllocated(ATrackBar, 'SetPosition') then Exit;

  TCarbonTrackBar(ATrackBar.Handle).SetData(ATrackBar.Position);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TCarbonWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TCarbonWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TCarbonWSPageControl);
//  RegisterWSComponent(TCustomListView, TCarbonWSCustomListView);
//  RegisterWSComponent(TCustomListView, TCarbonWSListView);
  RegisterWSComponent(TCustomProgressBar, TCarbonWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TCarbonWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TCarbonWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TCarbonWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TCarbonWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TCarbonWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TCarbonWSToolBar);
  RegisterWSComponent(TCustomTrackBar, TCarbonWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TCarbonWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TCarbonWSTreeView);
////////////////////////////////////////////////////
end.
