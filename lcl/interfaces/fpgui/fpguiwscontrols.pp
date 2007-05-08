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
  // Bindings
  fpgui, fpguiwsprivate,
  // LCL
  Controls, LCLType,
  // Widgetset
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
  end;

  { TFpGuiWSWinControl }

  TFpGuiWSWinControl = class(TWSWinControl)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
//    class procedure Invalidate(const AWinControl: TWinControl); override;
  public
//    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetPos(const AWinControl: TWinControl; const ALeft, ATop: Integer); override;
    class procedure SetSize(const AWinControl: TWinControl; const AWidth, AHeight: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
//    class procedure SetColor(const AWinControl: TWinControl); override;
//    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;

//    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
//    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

{    class procedure AddControl(const AControl: TControl); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;}
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

{ TFpGuiWSWinControl }

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.CreateHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TFpGuiWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  {$ifdef VerboseFPGUI}
    WriteLn('TFpGuiWSWinControl.CreateHandle for ',AWinControl.Name);
  {$endif}

  Result := TLCLIntfHandle(TFPGUIPrivateWidget.Create(AWinControl, AParams));
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSWinControl.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
//  TFPGUIPrivateWidget(AWinControl.Handle).Free;

//  AWinControl.Handle := 0;
end;

class procedure TFpGuiWSWinControl.SetPos(const AWinControl: TWinControl;
  const ALeft, ATop: Integer);
var
  FPWidget: TFWidget;
begin
  FPWidget := TFWidget(AWincontrol.Handle);
  FPWIdget.SetBounds(ALeft, ATop, AWincontrol.Width, AWinControl.Height);
end;

class procedure TFpGuiWSWinControl.SetSize(const AWinControl: TWinControl;
  const AWidth, AHeight: Integer);
var
  FPWidget: TFWidget;
begin
  FPWidget := TFWidget(AWincontrol.Handle);
  FPWIdget.SetBounds(AWinControl.Left, AWinControl.Top, AWidth, AHeight);
end;

class procedure TFpGuiWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  FPWidget: TFPGUIPrivateWidget;
begin
  FPWidget := TFPGUIPrivateWidget(AWincontrol.Handle);
  FPWidget.Visible := not FPWidget.Visible;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TDragImageList, TFpGuiWSDragImageList);
//  RegisterWSComponent(TControl, TFpGuiWSControl);
  RegisterWSComponent(TWinControl, TFpGuiWSWinControl);
//  RegisterWSComponent(TGraphicControl, TFpGuiWSGraphicControl);
//  RegisterWSComponent(TCustomControl, TFpGuiWSCustomControl);
//  RegisterWSComponent(TImageList, TFpGuiWSImageList);
////////////////////////////////////////////////////
end.
