{ $Id: FpGuiwsforms.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               FpGuiWSForms.pp                                * 
 *                               ------------                                * 
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
unit FpGuiWSForms;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpgfx, gfxbase, fpguiwsprivate,
  // LCL
  Classes, Forms, LCLType, Controls,
  // Widgetset
  WSForms, WSLCLClasses;

type

  { TFpGuiWSScrollingWinControl }

  TFpGuiWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TFpGuiWSScrollBox }

  TFpGuiWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TFpGuiWSCustomFrame }

  TFpGuiWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TFpGuiWSFrame }

  TFpGuiWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TFpGuiWSCustomForm }

  TFpGuiWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetFormBorderStyle(const AForm: Forms.TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
  end;

  { TFpGuiWSForm }

  TFpGuiWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TFpGuiWSHintWindow }

  TFpGuiWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TFpGuiWSScreen }

  TFpGuiWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TFpGuiWSApplicationProperties }

  TFpGuiWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

{ TFpGuiWSCustomForm }

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Allocates memory and resources for the control and shows it
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  FPForm: TFPGUIPrivateWindow;
begin
  {$ifdef VerboseFPGUIIntf}
    WriteLn('TFpGuiWSCustomForm.CreateHandle');
  {$endif}

  FPForm := TFPGUIPrivateWindow.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(FPForm);
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomForm.DestroyHandle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  {$ifdef VerboseFPGUIIntf}
    WriteLn('TFpGuiWSCustomForm.DestroyHandle');
  {$endif}

  TFPGUIPrivateWindow(AWinControl.Handle).Free;

  AWinControl.Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomForm.GetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class function TFpGuiWSCustomForm.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  FPForm: TFPGUIPrivateWindow;
begin
  Result := True;
  FPForm := TFPGUIPrivateWindow(AWinControl.Handle);
  AText := FPForm.GetText;
end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomForm.SetFormBorderStyle
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomForm.SetFormBorderStyle(const AForm: Forms.TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
//var
//  FPForm: TFPGUIPrivateWindow;
begin
//  FPForm := TFPGUIPrivateWindow(AForm.Handle);

end;

{------------------------------------------------------------------------------
  Method: TFpGuiWSCustomForm.SetText
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TFpGuiWSCustomForm.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(AWincontrol.Handle);
  FPForm.SetText(AText);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TFpGuiWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TFpGuiWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TFpGuiWSCustomFrame);
//  RegisterWSComponent(TFrame, TFpGuiWSFrame);
  RegisterWSComponent(TCustomForm, TFpGuiWSCustomForm);
//  RegisterWSComponent(Forms.TForm, TFpGuiWSForm);
//  RegisterWSComponent(THintWindow, TFpGuiWSHintWindow);
//  RegisterWSComponent(TScreen, TFpGuiWSScreen);
//  RegisterWSComponent(TApplicationProperties, TFpGuiWSApplicationProperties);
////////////////////////////////////////////////////
end.
