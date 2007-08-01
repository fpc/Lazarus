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

{$mode objfpc}{$H+}

interface

uses
  // Bindings
{$ifdef USE_QT_4_3}
  qt43,
{$else}
  qt4,
{$endif}
  qtwidgets,
  // LCL
  SysUtils, Classes, Controls, LCLType, Forms,
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
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
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

  Creates a Qt Form and initializes it according to it's properties
 ------------------------------------------------------------------------------}
class function TQtWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  QtMainWindow: TQtMainWindow;
  Str: WideString;
  R: TRect;
  R1: TRect;
begin
  {$ifdef VerboseQt}
    WriteLn('[TQtWSCustomForm.CreateHandle] Height: ', IntToStr(AWinControl.Height),
     ' Width: ', IntToStr(AWinControl.Width));
  {$endif}

  // Creates the window

  QtMainWindow := TQtMainWindow.Create(AWinControl, AParams);
  
  if (TCustomForm(AWinControl).ShowInTaskBar in [stDefault, stNever]) and not
     (TCustomForm(AWinControl).FormStyle in [fsMDIChild]) and
     (Application <> nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm.HandleAllocated) and
     (Application.MainForm <> AWinControl) then
    QtMainWindow.setShowInTaskBar(False);

  // Set´s initial properties

  Str := UTF8Decode(AWinControl.Caption);

  QtMainWindow.SetWindowTitle(@Str);

  if not (csDesigning in TCustomForm(AWinControl).ComponentState) then
  begin
    SetQtWindowBorderStyle(QtMainWindow, TCustomForm(AWinControl).BorderStyle);
    SetQtBorderIcons(QtMainWindow, TCustomForm(AWinControl).BorderIcons);
  end;

  // Sets Various Events
  QtMainWindow.AttachEvents;
  
  {$ifdef USE_QT_4_3}
  if (TCustomForm(AWinControl).FormStyle in [fsMDIChild]) and
     (Application.MainForm.FormStyle = fsMdiForm) and
     not (csDesigning in AWinControl.ComponentState) then
    QMdiArea_addSubWindow(TQtMainWindow(Application.MainForm.Handle).MDIAreaHandle, QtMainWindow.Widget, QtWindow);
  {$endif}

  R := AWinControl.ClientRect;
  R1 := QtMainWindow.MenuBar.getGeometry;
  R1.Right := R.Right;
  QtMainWindow.MenuBar.setGeometry(R1);
  QtMainWindow.setMenuBar(QMenuBarH(QtMainWindow.MenuBar.Widget));

  // Return the handle
  Result := THandle(QtMainWindow);
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
var
  Enable: Boolean;
begin
  if (AForm.Parent<>nil) or not (AForm.HandleAllocated) then exit;

  Enable := AValue <> stNever;
  if (AValue = stDefault) and
     (Application<>nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm <> AForm) then
    Enable := false;
  TQtMainWindow(AForm.Handle).setShowInTaskBar(Enable);
end;

{------------------------------------------------------------------------------
  Method: TQtWSCustomForm.ShowModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  {
    Setting modal flags is done in TQtWSCustomControl.ShowHide
    Since that flags has effect only when Widget is not visible
    
    We can ofcource hide widget, set flags here and then show it, but we dont
    want window flickering :)
  }
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

  We need this method because LCL doesn't automatically calls SetWindowBorder
 after the window is created, so we need to do it on CreateHandle procedure,
 but CreateHandle cannot call other methods from TQtWSCustomForm because the
 Handle isn't created yet before it is finished.
 ------------------------------------------------------------------------------}
class procedure TQtWSCustomForm.SetQtWindowBorderStyle(const AHandle: TQtMainWindow;
  const AFormBorderStyle: TFormBorderStyle);
const
  QtOnlyDialog: Integer = QtDialog and not QtWindow;
  QtOnlyTool: Integer = QtTool and not QtWindow;
  QtOnlyToolOrDialog: Integer = (QtDialog or QtTool) and not QtWindow;
var
  Flags: QtWindowFlags;
  AVisible: Boolean;
begin
  Flags := AHandle.windowFlags;
  // after setting flags showing can be changed
  AVisible := AHandle.getVisible;

  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomForm.SetFormBorderStyle] Flags: ', IntToHex(Flags, 8));
  {$endif}

  case AFormBorderStyle of
   bsNone:
   begin
     Flags := (Flags or QtFramelessWindowHint) and not QtWindowTitleHint;
     Flags := Flags and not QtOnlyToolOrDialog;
   end;
   bsSingle:
   begin
     Flags := Flags or QtFramelessWindowHint;
     Flags := Flags and not QtOnlyToolOrDialog;
   end;
   bsSizeable:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags and not QtOnlyToolOrDialog;
   end;
   bsDialog:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags or QtOnlyDialog;
   end;
   bsToolWindow:
   begin
     Flags := Flags or QtFramelessWindowHint;
     Flags := Flags or QtOnlyTool;
   end;
   bsSizeToolWin:
   begin
     Flags := Flags and not QtFramelessWindowHint;
     Flags := Flags or QtOnlyTool;
   end;
  end;

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomForm.SetFormBorderStyle] Flags: ', IntToHex(Flags, 8));
  {$endif}

  AHandle.setWindowFlags(Flags);
  AHandle.setVisible(AVisible);
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
  ShowAny: Boolean;
begin
  Flags := AHandle.windowFlags;

  {$ifdef VerboseQt}
    WriteLn('Trace:> [TQtWSCustomForm.SetBorderIcons] Flags: ', IntToHex(Flags, 8));
  {$endif}

  ShowAny := ((Flags and QtFramelessWindowHint) = 0) or
             ((Flags and QtWindowTitleHint) <> 0);

  if (biSystemMenu in ABorderIcons) and ShowAny then
    Flags := Flags or QtWindowSystemMenuHint
  else
    Flags := Flags and not QtWindowSystemMenuHint;

  if (biMinimize in ABorderIcons) and ShowAny then
    Flags := Flags or QtWindowMinimizeButtonHint
  else
    Flags := Flags and not QtWindowMinimizeButtonHint;

  if (biMaximize in ABorderIcons) and ShowAny then
    Flags := Flags or QtWindowMaximizeButtonHint
  else
    Flags := Flags and not QtWindowMaximizeButtonHint;

  if (biHelp in ABorderIcons) and ShowAny then
    Flags := Flags or QtWindowContextHelpButtonHint
  else
    Flags := Flags and not QtWindowContextHelpButtonHint;

  {$ifdef VerboseQt}
    WriteLn('Trace:< [TQtWSCustomForm.SetBorderIcons] Flags: ', IntToHex(Flags, 8));
  {$endif}

  AHandle.setWindowFlags(Flags);
end;

{ TQtWSHintWindow }

class function TQtWSHintWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  QtMainWindow: TQtMainWindow;
begin
  QtMainWindow := TQtHintWindow.Create(AWinControl, AParams);
  // Sets Various Events
  QtMainWindow.AttachEvents;
  Result := THandle(QtMainWindow);
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
  RegisterWSComponent(THintWindow, TQtWSHintWindow);
//  RegisterWSComponent(TScreen, TQtWSScreen);
//  RegisterWSComponent(TApplicationProperties, TQtWSApplicationProperties);
////////////////////////////////////////////////////
end.
