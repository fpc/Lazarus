{ $Id: FpGuiwscontrols.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                              FpGuiWSControls.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit FpGuiWSControls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Controls, LCLType,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses;

type

  { TFpGuiWSDragImageList }

  TFpGuiWSDragImageList = class(TWSDragImageList)
  private
  protected
  public
  end;

  { TFpGuiWSControl }

  TFpGuiWSControl = class(TWSControl)
  private
  protected
  public
    class procedure AddControl(const AControl: TControl); override;
  end;

  { TFpGuiWSWinControl }

  TFpGuiWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
  end;

  { TFpGuiWSGraphicControl }

  TFpGuiWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomControl }

  TFpGuiWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TFpGuiWSImageList }

  TFpGuiWSImageList = class(TWSImageList)
  private
  protected
  public
  end;


implementation
uses fpgui, FPGUIWSPrivate;

{ TFpGuiWSWinControl }

class procedure TFpGuiWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  FPWidget: TFPGUIPrivateWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWincontrol.Handle);
  FPWidget.Visible := not FPWidget.Visible;
end;

class procedure TFpGuiWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  FPWidget: TWidget;
begin
  FPWidget := TWidget(AWincontrol.Handle);
  FPWIdget.SetBounds(ALeft, ATop, AWincontrol.Width, AWinControl.Height);
end;

class procedure TFpGuiWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  FPWidget: TWidget;
begin
  FPWidget := TWidget(AWincontrol.Handle);
  FPWIdget.SetBounds(AWinControl.Left, AWinControl.Top, AWidth, AHeight);
end;

{ TFpGuiWSControl }

class procedure TFpGuiWSControl.AddControl(const AControl: TControl);
var
  AParent: TWinControl;
  ParentWidget: TFPGUIPrivateWidget;
begin
  AParent := TWinControl(AControl).Parent;
  ParentWidget := TFPGUIPrivateWidget(AParent.Handle);
  
  (ParentWidget as IContainer).AddChild(TWinControl(AControl));
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TFpGuiWSDragImageList);
  RegisterWSComponent(TControl, TFpGuiWSControl);
  RegisterWSComponent(TWinControl, TFpGuiWSWinControl);
//  RegisterWSComponent(TGraphicControl, TFpGuiWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TFpGuiWSCustomControl);
//  RegisterWSComponent(TImageList, TFpGuiWSImageList);
////////////////////////////////////////////////////
end.
