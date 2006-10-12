{ $Id$}
{
 *****************************************************************************
 *                               QtWSForms.pp                                * 
 *                               ------------                                * 
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
unit QtWSForms;

{$mode delphi}{$H+}

interface

uses
  // Bindings
  qt4, qtwidgets,
  // LCL
  SysUtils, Controls, LCLType, Forms,
  // Widgetset
  InterfaceBase, WSForms, WSLCLClasses;

type

  { TQtWSScrollingWinControl }

  TQtWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
  end;

  { TQtWSScrollBox }

  TQtWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TQtWSCustomFrame }

  TQtWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TQtWSFrame }

  TQtWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TQtWSCustomForm }

  TQtWSCustomForm = class(TWSCustomForm)
  private
    class procedure SetQtWindowBorderStyle(const AHandle: TQtMainWindow; const AFormBorderStyle: TFormBorderStyle);
    class procedure SetQtBorderIcons(const AHandle: TQtMainWindow; const ABorderIcons: TBorderIcons);
  protected
  public
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
  end;

  { TQtWSForm }

  TQtWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TQtWSHintWindow }

  TQtWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TQtWSScreen }

  TQtWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TQtWSApplicationProperties }

  TQtWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

uses QtWSControls;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Qt Form and initializes it according to it´s properties
 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtMainWindow: TQtMainWindow;
  Str: WideString;
  Method: TMethod;
  Hook : QObject_hookH;
begin
  // Creates the window

  QtMainWindow := TQtMainWindow.Create(AWinControl, AParams);

  // Set´s initial properties

  Str := UTF8Decode(AWinControl.Caption);

  QtMainWindow.SetWindowTitle(@Str);

  SetQtWindowBorderStyle(QtMainWindow,  TCustomForm(AWinControl).BorderStyle);

  SetQtBorderIcons(QtMainWindow, TCustomForm(AWinControl).BorderIcons);

  // Sets Various Events

  Hook := QObject_hook_create(QtMainWindow.Widget);

  TEventFilterMethod(Method) := QtMainWindow.EventFilter;

  QObject_hook_hook_events(Hook, Method);

  // Return the handle

  Result := THandle(QtMainWindow);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.DestroyHandle
  Params:  None
  Returns: Nothing

  Destroys a Qt Form and releases allocated memory and resources
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
  TQtMainWindow(AWinControl.Handle).Free;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.GetText
  Params:  AWinControl     - the calling object
           AText           - The Text
  Returns: Nothing

 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  Str: WideString;
begin
  TQtWidget(AWinControl.Handle).WindowTitle(@Str);

  AText := UTF8Encode(Str);

  Result := True;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetText
  Params:  AWinControl     - the calling object
           AText           - The Text
  Returns: Nothing

 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetText(const AWinControl: TWinControl; const AText: string);
var
  Str: WideString;
begin
  Str := UTF8Decode(AText);

  TQtWidget(AWinControl.Handle).SetWindowTitle(@Str);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.CloseModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  inherited CloseModal(ACustomForm);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetFormBorderStyle
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  SetQtWindowBorderStyle(TQtMainWindow(AForm.Handle), AFormBorderStyle);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetIcon
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
begin
  inherited SetIcon(AForm, AIcon);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetShowInTaskbar
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar);
begin
  inherited SetShowInTaskbar(AForm, AValue);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.ShowModal
  Params:
  Returns: Nothing
  
  What we do here is put the entere window inside a QDialog component, because QDialog
 is the only Qt component with a exec method that won´t return until the dialog is closed,
 and that behavior makes implementing ShowModal much easier.
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  QtDialog: TQtDialog;
begin
  QtDialog := TQtDialog.Create;
  try
    TQtWidget(ACustomForm.Handle).setParent(QtDialog.Widget);

    if QtDialog.exec = Integer(QDialogRejected) then ACustomForm.ModalResult := mrCancel
    else ACustomForm.ModalResult := mrOk;
    
    TQtWidget(ACustomForm.Handle).setParent(nil);
  finally
    QtDialog.Free;
  end;
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetBorderIcons
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  SetQtBorderIcons(TQtMainWindow(AForm.Handle), ABorderIcons);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetQtWindowBorderStyle
  Params:  None
  Returns: Nothing

  We need this method because LCL doesn´t automatically calls SetWindowBorder
 after the window is created, so we need to do it on CreateHandle procedure,
 but CreateHandle cannot call other methods from TQtWSCustomForm because the
 Handle isn´t created yet before it is finished.
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetQtWindowBorderStyle(const AHandle: TQtMainWindow;
  const AFormBorderStyle: TFormBorderStyle);
var
  Flags: QtWindowFlags;
  QtOnlyDialog: Integer;
begin
  Flags := AHandle.windowFlags;

  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomForm.SetFormBorderStyle] Flags: ', IntToHex(Flags, 8));
  {$endif}

  QtOnlyDialog := QtDialog and not QtWindow;

  case AFormBorderStyle of
   bsNone:
   begin
     Flags := Flags or QtFramelessWindowHint;
     Flags := Flags and not QtOnlyDialog;
   end;
   bsSingle:
   begin
     Flags := Flags or QtFramelessWindowHint;
     Flags := Flags and not QtOnlyDialog;
   end;
   bsSizeable:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags and not QtOnlyDialog;
   end;
   bsDialog:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags or QtOnlyDialog;
   end;
   bsToolWindow:
   begin
     Flags := Flags or QtFramelessWindowHint;
     Flags := Flags or QtOnlyDialog;
   end;
   bsSizeToolWin:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags or QtOnlyDialog;
   end;
  end;

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomForm.SetFormBorderStyle] Flags: ', IntToHex(Flags, 8));
  {$endif}

  AHandle.setWindowFlags(Flags);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.SetQtBorderIcons
  Params:  None
  Returns: Nothing

  Same comment as SetQtWindowBorderStyle above
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetQtBorderIcons(const AHandle: TQtMainWindow;
  const ABorderIcons: TBorderIcons);
var
  Flags: QtWindowFlags;
begin
  AHandle.windowFlags;

  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomForm.SetBorderIcons] Flags: ', IntToHex(Flags, 8));
  {$endif}

  if biSystemMenu in ABorderIcons then Flags := Flags or QtWindowSystemMenuHint
  else Flags := Flags and not QtWindowSystemMenuHint;

  if biMinimize in ABorderIcons then Flags := Flags or QtWindowMinimizeButtonHint
  else Flags := Flags and not QtWindowMinimizeButtonHint;

  if biMaximize in ABorderIcons then Flags := Flags or QtWindowMaximizeButtonHint
  else Flags := Flags and not QtWindowMaximizeButtonHint;

  if biHelp in ABorderIcons then Flags := Flags or QtWindowContextHelpButtonHint
  else Flags := Flags and not QtWindowContextHelpButtonHint;

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomForm.SetBorderIcons] Flags: ', IntToHex(Flags, 8));
  {$endif}

  AHandle.setWindowFlags(Flags);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TScrollingWinControl, TQtWSScrollingWinControl);
//  RegisterWSComponent(TScrollBox, TQtWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TQtWSCustomFrame);
//  RegisterWSComponent(TFrame, TQtWSFrame);
  RegisterWSComponent(TCustomForm, TQtWSCustomForm);
//  RegisterWSComponent(TForm, TQtWSForm);
//  RegisterWSComponent(THintWindow, TQtWSHintWindow);
//  RegisterWSComponent(TScreen, TQtWSScreen);
//  RegisterWSComponent(TApplicationProperties, TQtWSApplicationProperties);
////////////////////////////////////////////////////
end.
