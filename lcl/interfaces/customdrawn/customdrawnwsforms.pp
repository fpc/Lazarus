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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSForms;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  // LCL
  SysUtils, Classes, Controls, LCLType, Forms,
  // Widgetset
  InterfaceBase, WSForms, WSProc, WSLCLClasses;

type

  { TCDWSScrollingWinControl }

  TCDWSScrollingWinControl = class(TWSScrollingWinControl)
  published
  end;

  { TCDWSScrollBox }

  TCDWSScrollBox = class(TWSScrollBox)
  published
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
//      const DeltaX, DeltaY: integer); override;
  end;

  { TCDWSCustomFrame }

  TCDWSCustomFrame = class(TWSCustomFrame)
  published
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
//      const DeltaX, DeltaY: integer); override;
  end;

  { TCDWSFrame }

  TCDWSFrame = class(TWSFrame)
  published
  end;

  { TCDWSCustomForm }

  TCDWSCustomForm = class(TWSCustomForm)
  private
{    class function GetCDBorderStyle(const AFormBorderStyle: TFormBorderStyle): CDWindowFlags;
    class function GetCDBorderIcons(const AFormBorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons): CDWindowFlags;
    class function GetCDFormStyle(const AFormStyle: TFormStyle): CDWindowFlags;
    class procedure UpdateWindowFlags(const AWidget: TCDMainWindow;
      ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons; AFormStyle: TFormStyle);
  published
    class function  CanFocus(const AWinControl: TWinControl): Boolean; override;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure SetPopupParent(const ACustomForm: TCustomForm;
       const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override; //TODO: rename to SetVisible(control, visible)
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm;
       const AlphaBlend: Boolean; const Alpha: Byte); override;}
  end;

  { TCDWSForm }

  TCDWSForm = class(TWSForm)
  published
  end;

  { TCDWSHintWindow }

  TCDWSHintWindow = class(TWSHintWindow)
  published
//    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCDWSScreen }

  TCDWSScreen = class(TWSScreen)
  published
  end;

  { TCDWSApplicationProperties }

  TCDWSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

(*uses CDint, CDWSControls, LCLIntf;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a CD Form and initializes it according to it's properties
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  CDMainWindow: TCDMainWindow;
  Str: WideString;
  PopupParent: QWidgetH;
  AForm: TCustomForm;
begin
  {$ifdef VerboseCD}
    WriteLn('[TCDWSCustomForm.CreateHandle] Height: ', IntToStr(AWinControl.Height),
     ' Width: ', IntToStr(AWinControl.Width));
  {$endif}

  // Creates the window
  {$IFDEF HASX11}
  if (CDVersionMajor = 4) and (CDVersionMinor >= 6) then
    QCoreApplication_setAttribute(CDAA_ImmediateWidgetCreation, True);
  {$ENDIF}
  if csDesigning in AWinControl.ComponentState then
    CDMainWindow := TCDDesignWidget.Create(AWinControl, AParams)
  else
    CDMainWindow := TCDMainWindow.Create(AWinControl, AParams);

  AForm := TCustomForm(AWinControl);

  CDMainWindow.CDFormBorderStyle := Ord(AForm.BorderStyle);
  CDMainWindow.CDFormStyle := Ord(AForm.FormStyle);

  Str := GetUtf8String(AWinControl.Caption);

  CDMainWindow.SetWindowTitle(@Str);

  if not (csDesigning in AForm.ComponentState) then
  begin
    UpdateWindowFlags(CDMainWindow, AForm.BorderStyle,
      AForm.BorderIcons, AForm.FormStyle);
  end;

  if not (AForm.FormStyle in [fsMDIChild]) and
     (Application <> nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm.HandleAllocated) and
     (Application.MainForm <> AForm) then
  begin
    if (AForm.ShowInTaskBar in [stDefault, stNever])
       {$ifdef HASX11}
       {CDTool have not minimize button !}
       and not (AForm.BorderStyle in [bsSizeToolWin, bsToolWindow])
       {$endif} then
      CDMainWindow.setShowInTaskBar(False);
    if Assigned(AForm.PopupParent) then
      PopupParent := TCDWidget(AForm.PopupParent.Handle).Widget
    else
      PopupParent := nil;
    CDMainWindow.setPopupParent(AForm.PopupMode, PopupParent);
  end;

  {$IFDEF HASX11}
  if (CDVersionMajor = 4) and (CDVersionMinor >= 6) then
    QCoreApplication_setAttribute(CDAA_ImmediateWidgetCreation, False);
  {$ENDIF}

  // Sets Various Events
  CDMainWindow.AttachEvents;
  CDMainWindow.MenuBar.AttachEvents;
  
  if (AForm.FormStyle in [fsMDIChild]) and
     (Application.MainForm.FormStyle = fsMdiForm) and
     not (csDesigning in AWinControl.ComponentState) then
  begin
    QMdiArea_addSubWindow(TCDMainWindow(Application.MainForm.Handle).MDIAreaHandle, CDMainWindow.Widget, CDWindow);
    QWidget_setFocusProxy(CDMainWindow.Widget, CDMainWindow.getContainerWidget);
  end;
    

  // Return the handle
  Result := TLCLIntfHandle(CDMainWindow);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.CloseModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  inherited CloseModal(ACustomForm);
end;

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
var
  w: TCDWidget;
begin
  w := TCDWidget(AWinControl.Handle);
  {forms which have another widget as parent
   eg.form inside tabpage or mdichilds
   can segfault without hiding before release.
   So we save our day here.}
  if w.getVisible and (w.getParent <> nil) then
    w.Hide;
  w.Release;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetAllowDropFiles
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  if AForm.HandleAllocated then
    TCDMainWindow(AForm.Handle).setAcceptDropFiles(AValue);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetFormBorderStyle
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
var
  CDWin: TCDMainWindow;
begin
  CDWin := TCDMainWindow(AForm.Handle);
  if (AForm.Parent <> nil) and (CDWin.CDFormBorderStyle <> Ord(AFormBorderStyle)) then
    RecreateWnd(AForm)
  else
  begin
    CDWin.CDFormBorderStyle := Ord(AFormBorderStyle);
    UpdateWindowFlags(CDWin, AFormBorderStyle,
      AForm.BorderIcons, AForm.FormStyle);
  end;
end;

class procedure TCDWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
var
  CDWin: TCDMainWindow;
begin
  CDWin := TCDMainWindow(AForm.Handle);
  if (AForm.Parent <> nil) and (CDWin.CDFormStyle <> Ord(AFormStyle)) then
    RecreateWnd(AForm)
  else
  begin
    CDWin.CDFormStyle := Ord(AFormStyle);
    UpdateWindowFlags(CDWin, AForm.BorderStyle, AForm.BorderIcons, AFormStyle);
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetIcon
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
var
  Icon: TCDIcon;
begin
  Icon := TCDIcon(Big);
  if Icon <> nil then
    TCDWidget(AForm.Handle).setWindowIcon(Icon.Handle)
  else
    TCDWidget(AForm.Handle).setWindowIcon(nil);
end;

class procedure TCDWSCustomForm.SetPopupParent(const ACustomForm: TCustomForm;
  const APopupMode: TPopupMode; const APopupParent: TCustomForm);
var
  PopupParent: QWidgetH;
begin
  if Assigned(APopupParent) then
    PopupParent := TCDWidget(APopupParent.Handle).Widget
  else
    PopupParent := nil;
  TCDMainWindow(ACustomForm.Handle).setPopupParent(APopupMode, PopupParent);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetShowInTaskbar
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar);
var
  Enable: Boolean;
begin
  if (AForm.Parent<>nil) or not (AForm.HandleAllocated) then exit;

  Enable := AValue <> stNever;
  if (AValue = stDefault) and
    {$IFDEF HASX11}
    TCDMainWindow(AForm.Handle).ShowOnTaskBar and
    {$ENDIF}
     (Application<>nil) and
     (Application.MainForm <> nil) and
     (Application.MainForm <> AForm) then
    Enable := false;
  {$IFDEF HASX11}
  if AForm.FormStyle <> fsMDIChild then
    SetSkipX11Taskbar(TCDMainWindow(AForm.Handle).Widget, not Enable);
  {$ENDIF}
  TCDMainWindow(AForm.Handle).setShowInTaskBar(Enable);
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
const
  LCLToCDWindowState: array[TWindowState] of CDWindowState = (
 { wsNormal    } CDWindowNoState,
 { wsMinimized } CDWindowMinimized,
 { wsMaximized } CDWindowMaximized,
 { wsFullScreen} CDWindowFullScreen
  );
var
  Widget: TCDMainWindow;
  R: TRect;
  ActiveWin: HWND;
  W: QWidgetH;
  Flags: Cardinal;
begin
  if not WSCheckHandleAllocated(AWinControl, 'ShowHide') then
    Exit;

  Widget := TCDMainWindow(AWinControl.Handle);

  if AWinControl.HandleObjectShouldBeVisible then
  begin
    if fsModal in TForm(AWinControl).FormState then
    begin
      {$ifdef MSWINDOWS}
      // CD doesn't modal windows as CDTool.see issue #18709
      if (TForm(AWinControl).BorderStyle in [bsToolWindow, bsSizeToolWin]) then
        QWidget_setWindowFlags(Widget.Widget, CDDialog);
      {$endif}

      {$ifdef HASX11}
      W := nil;
      ActiveWin := GetActiveWindow;
      if ActiveWin <> 0 then
      begin
        if Assigned(TCDWidget(ActiveWin).LCLObject) then
        begin
          if (TCDWidget(ActiveWin).LCLObject is TCustomForm) then
          begin
            with TCustomForm(TCDWidget(ActiveWin).LCLObject) do
            begin
              if Visible and (FormStyle <> fsSplash) then
                W := TCDWidget(Handle).Widget;
            end;
          end;
        end;
      end;
      QWidget_setParent(Widget.Widget, W);
      QWidget_setWindowFlags(Widget.Widget, CDDialog);
      {$endif}
      {$ifdef darwin}
      QWidget_setWindowFlags(Widget.Widget, CDDialog or CDWindowSystemMenuHint or CDCustomizeWindowHint
        or CDWindowTitleHint or CDWindowCloseButtonHint);
      {$endif}
      Widget.setWindowModality(CDApplicationModal);
    end;

    if TForm(AWinControl).FormStyle = fsMDIChild then
    begin
      {MDI windows have to be resized , since titlebar is included into widget geometry !}
      if not (csDesigning in AWinControl.ComponentState)
        and not Widget.isMaximized then
      begin
        QWidget_contentsRect(Widget.Widget, @R);
        R.Right := TForm(AWinControl).Width + R.Left;
        R.Bottom := TForm(AWinControl).Height + R.Top;
        R.Left := Widget.MdiChildCount * 10;
        R.Top := Widget.MdiChildCount * 10;
        Widget.move(R.Left, R.Top);
        Widget.resize(R.Right, R.Bottom);
      end;
    end;

    if TForm(AWinControl).FormStyle <> fsMDIChild then
    begin
      if (csDesigning in AWinControl.ComponentState) and
        (TCustomForm(AWinControl).WindowState = wsMaximized) then
        Widget.setWindowState(LCLToCDWindowState[wsNormal])
      else
        Widget.setWindowState(LCLToCDWindowState[TCustomForm(AWinControl).WindowState]);
    end;
  end;

  Widget.BeginUpdate;
  if not (csDesigning in AWinControl.ComponentState) then
  begin
    if AWinControl.HandleObjectShouldBeVisible
      and (TCustomForm(AWinControl).FormStyle in fsAllStayOnTop) then
    begin
      Flags := Widget.windowFlags;
      if (Flags and CDWindowStaysOnTopHint = 0) then
      begin
        Flags := Flags or CDWindowStaysOnTopHint;
        Widget.setWindowFlags(Flags);
      end;
    end else
    begin
      if (TCustomForm(AWinControl).FormStyle in fsAllStayOnTop)
      and not (csDestroying in AWinControl.ComponentState) then
      begin
        Flags := Widget.windowFlags;
        Flags := Flags and not CDWindowStaysOnTopHint;
        Widget.setWindowFlags(Flags);
      end;
    end;
  end;

  Widget.setVisible(AWinControl.HandleObjectShouldBeVisible);
  Widget.EndUpdate;

  {$IFDEF HASX11}
  if AWinControl.HandleObjectShouldBeVisible and
    (fsModal in TForm(AWinControl).FormState) and
    (CDWidgetSet.WindowManagerName = 'metacity') then
      X11Raise(QWidget_winID(Widget.Widget));
  {$ENDIF}

  {$IFDEF HASX11}
  if (CDVersionMajor = 4) and (CDVersionMinor >= 6)
    and (TForm(AWinControl).FormStyle <> fsMDIChild) then
    QApplication_syncX();
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.ShowModal
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  {
    Setting modal flags is done in TCDWSCustomControl.ShowHide
    Since that flags has effect only when Widget is not visible
    
    We can of course hide widget, set flags here and then show it, but we do not
    want window flickering :)
  }
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetBorderIcons
  Params:
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  UpdateWindowFlags(TCDMainWindow(AForm.Handle), AForm.BorderStyle, ABorderIcons, AForm.FormStyle);
end;

class procedure TCDWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
begin
  if AlphaBlend then
    TCDMainWindow(ACustomForm.Handle).setWindowOpacity(Alpha / 255)
  else
    TCDMainWindow(ACustomForm.Handle).setWindowOpacity(1);
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.GetCDWindowBorderStyle
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.GetCDBorderStyle(const AFormBorderStyle: TFormBorderStyle): CDWindowFlags;
begin
  case AFormBorderStyle of
    bsNone:
      Result := CDWindow or CDFramelessWindowHint;
    bsSingle:
      Result := CDWindow or CDMSWindowsFixedSizeDialogHint;
    bsSizeable:
      Result := CDWindow;
    bsDialog:
      Result := CDDialog or CDMSWindowsFixedSizeDialogHint;
    bsToolWindow:
      Result := CDTool or CDMSWindowsFixedSizeDialogHint;
    bsSizeToolWin:
      // CD on most platforms (except windows) doesn't have sizeToolWin, it's regular CDWindow
      Result := {$ifdef windows}CDTool{$else}CDWindow{$endif};
    else
      Result := CDWidget;
  end;
end;

{------------------------------------------------------------------------------
  Method: TCDWSCustomForm.SetCDBorderIcons
  Params:  None
  Returns: Nothing

  Same comment as SetCDWindowBorderStyle above
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.GetCDBorderIcons(const AFormBorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons): CDWindowFlags;
begin
  Result := 0;

  case AFormBorderStyle of
    bsNone: Exit;
    bsDialog: ABorderIcons := ABorderIcons - [biMaximize, biMinimize];
    bsToolWindow, bsSizeToolWin: ABorderIcons := ABorderIcons - [biMaximize, biMinimize, biHelp];
  end;

  if (biSystemMenu in ABorderIcons) then
    Result := Result or CDWindowSystemMenuHint;

  if (biMinimize in ABorderIcons) then
    Result := Result or CDWindowMinimizeButtonHint;

  if (biMaximize in ABorderIcons) then
    Result := Result or CDWindowMaximizeButtonHint;

  if (biHelp in ABorderIcons) then
    Result := Result or CDWindowContextHelpButtonHint;
end;

class function TCDWSCustomForm.GetCDFormStyle(const AFormStyle: TFormStyle): CDWindowFlags;
begin
  if AFormStyle in fsAllStayOnTop then
    Result := CDWindowStaysOnTopHint
  else
    Result := 0;
end;

class procedure TCDWSCustomForm.UpdateWindowFlags(const AWidget: TCDMainWindow;
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons; AFormStyle: TFormStyle);
var
  Flags: CDWindowFlags;
  AVisible: Boolean;
begin
  AWidget.BeginUpdate;
  AVisible := AWidget.getVisible;
  Flags := GetCDBorderStyle(ABorderStyle) or GetCDFormStyle(AFormStyle) or GetCDBorderIcons(ABorderStyle, ABorderIcons);
  if (Flags and CDFramelessWindowHint) = 0 then
    Flags := Flags or CDWindowTitleHint or CDCustomizeWindowHint
      or CDWindowCloseButtonHint;

  if not (csDesigning in AWidget.LCLObject.ComponentState) then
  begin
    AWidget.setWindowFlags(Flags);
    if ABorderStyle in [bsDialog, bsNone, bsSingle] then
      AWidget.Resize(AWidget.LCLObject.Width, AWidget.LCLObject.Height)
    else
    begin
      // Reset constraints.
      AWidget.setMinimumSize(CDMinimumWidgetSize, CDMinimumWidgetSize);
      AWidget.setMaximumSize(CDMaximumWidgetSize, CDMaximumWidgetSize);
    end;
  end;

  AWidget.setVisible(AVisible);
  AWidget.EndUpdate;
end;

class function TCDWSCustomForm.CanFocus(const AWinControl: TWinControl
  ): Boolean;
var
  Widget: TCDWidget;
begin
  if AWinControl.HandleAllocated then
  begin
    Widget := TCDWidget(AWinControl.Handle);
    Result := Widget.getVisible and Widget.getEnabled;
  end else
    Result := False;
end;

{ TCDWSHintWindow }

class function TCDWSHintWindow.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  CDMainWindow: TCDMainWindow;
begin
  CDMainWindow := TCDHintWindow.Create(AWinControl, AParams);
  // Sets Various Events
  CDMainWindow.AttachEvents;
  Result := TLCLIntfHandle(CDMainWindow);
end;

{ TCDWSScrollBox }

class procedure TCDWSScrollBox.ScrollBy(
  const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
var
  Widget: TCDCustomControl;
begin
  if not WSCheckHandleAllocated(AWinControl, 'ScrollBy') then
    Exit;
  Widget := TCDCustomControl(AWinControl.Handle);
  Widget.viewport.scroll(DeltaX, DeltaY);
end;

{ TCDWSCustomFrame }

class procedure TCDWSCustomFrame.ScrollBy(
  const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer);
var
  Widget: TCDCustomControl;
begin
  if not WSCheckHandleAllocated(AWinControl, 'ScrollBy') then
    Exit;
  Widget := TCDCustomControl(AWinControl.Handle);
  Widget.viewport.scroll(DeltaX, DeltaY);
end;*)

end.
