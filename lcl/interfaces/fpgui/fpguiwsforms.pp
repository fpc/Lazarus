{ $Id: FpGuiwsforms.pp 5319 2004-03-17 20:11:29Z marc $}
{
 *****************************************************************************
 *                               FpGuiWSForms.pp                                * 
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FpGuiWSForms;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpg_base, fpg_main, fpg_form, fpguiwsprivate, fpguiproc,
  // LCL
  Classes, Forms, LCLType, Controls, Graphics,
  // Widgetset
  WSForms, WSLCLClasses;

type

  { TFpGuiWSScrollingWinControl }

  TFpGuiWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure SetColor(const AWinControl: TWinControl); override;
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
  published
    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class procedure SetFormBorderStyle(const AForm: Forms.TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFont(const AWinControl: TWinControl;
                           const AFont: TFont); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure Repaint(const AWinControl: TWinControl); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult); override;
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

{ TFpGuiWSScrollingWinControl }

class function TFpGuiWSScrollingWinControl.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  FPForm: TFPGUIPrivateScrollingWinControl;
begin
  {$ifdef VerboseFPGUIIntf}
    WriteLn(Self.ClassName,'.CreateHandle ',AWinControl.Name);
  {$endif}

  FPForm := TFPGUIPrivateScrollingWinControl.Create(AWinControl, AParams);
  Result := TLCLIntfHandle(FPForm);
end;

class procedure TFpGuiWSScrollingWinControl.SetColor(
  const AWinControl: TWinControl);
var
  FPForm: TFPGUIPrivateScrollingWinControl;
begin
  FPForm := TFPGUIPrivateScrollingWinControl(AWinControl.Handle);
  FPForm.ScrollFrame.ContentFrame.BackgroundColor:=TColorToTfpgColor(AWinControl.Color);
  Inherited;
end;

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
    WriteLn(Self.ClassName,'.CreateHandle ',AWinControl.Name);
  {$endif}

  FPForm := TFPGUIPrivateWindow.Create(AWinControl, AParams);
  FPForm.SetFormBorderStyle(TForm(AWinControl).BorderStyle);

  { This fixes the AV error when trying to use TLabel components }
  TfpgForm(FPForm.Widget).Show;

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
    WriteLn(Self.ClassName,'.DestroyHandle ',AWinControl.Name);
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
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(AForm.Handle);
  FPForm.SetFormBorderStyle(AFormBorderStyle);
end;

class procedure TFpGuiWSCustomForm.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(AWinControl.Handle);
  FPForm.Font:=AFont;
end;

class procedure TFpGuiWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  FPForm: TfpgForm;
  AForm: TCustomForm;
begin
  FPForm := TFPGUIPrivateWindow(AWincontrol.Handle).Form;
  AForm:=TCustomForm(AWinControl);
  if AWinControl.Visible then begin
    if not FPForm.Visible then begin
      if (fsModal in AForm.FormState) and AForm.HandleObjectShouldBeVisible then begin
        FPForm.ShowModal;
      end else begin
        FPForm.Show;
      end;
    end;
  end else begin
    if FPForm.Visible then begin
      FPForm.Hide;
    end;
  end;
  if AWinControl.Visible then begin
    Invalidate(AWinControl);
  end;
end;

class procedure TFpGuiWSCustomForm.SetColor(const AWinControl: TWinControl);
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(AWinControl.Handle);
  FPForm.Widget.BackgroundColor:=TColorToTfpgColor(AWinControl.Color);
end;

class procedure TFpGuiWSCustomForm.Repaint(const AWinControl: TWinControl);
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(AWinControl.Handle);
  FPForm.Paint;
end;

class procedure TFpGuiWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  // Modal is handled in ShowHide to avoid double show (flicker).
  (*
  FPForm := TFPGUIPrivateWindow(ACustomForm.Handle);
  TfpgForm(FPForm.Widget).ShowModal;
  *)
end;

class procedure TFpGuiWSCustomForm.SetModalResult(
  const ACustomForm: TCustomForm; ANewValue: TModalResult);
var
  FPForm: TFPGUIPrivateWindow;
begin
  FPForm := TFPGUIPrivateWindow(ACustomForm.Handle);
  case ANewValue of
    Controls.mrOK     : TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrOK;
    Controls.mrCancel : TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrCancel;
    Controls.mrAbort: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrAbort;
    Controls.mrRetry: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrRetry;
    Controls.mrIgnore: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrIgnore;
    Controls.mrYes: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrYes;
    Controls.mrNo: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrNo;
    Controls.mrAll: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrAll;
    Controls.mrNoToAll: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrNoToAll;
    Controls.mrYesToAll: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrYesToAll;
    Controls.mrClose: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrCancel;
    Controls.mrNone: TfpgForm(FPForm.Widget).ModalResult:=TfpgModalResult.mrNone;
  end;
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

end.
