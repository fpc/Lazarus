{ $Id: cocoawsforms.pp 12783 2007-11-08 11:45:39Z tombo $}
{
 *****************************************************************************
 *                             CocoaWSForms.pp                               *
 *                               ------------                                *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSForms;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // RTL,FCL
  MacOSAll, CocoaAll, Classes,
  // LCL
  Controls, Forms, Graphics, LCLType, LMessages, LCLProc,
  // Widgetset
  WSForms, WSLCLClasses, WSProc, LCLMessageGlue,
  // LCL Cocoa
  CocoaPrivate, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls;

type
  { TLCLWindowCallback }

  TLCLWindowCallback = class(TLCLCommonCallBack, IWindowCallback)
  public
    function CanActivate: Boolean; virtual;
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure CloseQuery(var CanClose: Boolean); virtual;
    procedure Close; virtual;
    procedure Resize; virtual;
    procedure Move; virtual;

    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(AValue: Boolean); virtual;

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;


  { TCocoaWSScrollingWinControl }

  TCocoaWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure ScrollBy(const AWinControl: TScrollingWinControl; const DeltaX, DeltaY: integer); override;
  end;

  { TCocoaWSScrollBox }

  TCocoaWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomFrame }

  TCocoaWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TCocoaWSFrame }

  TCocoaWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TCocoaWSCustomForm }
  TCocoaWSCustomFormClass = class of TCocoaWSCustomForm;
  TCocoaWSCustomForm = class(TWSCustomForm)
  private
    class function GetStyleMaskFor(ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons): NSUInteger;
    class procedure UpdateWindowIcons(AWindow: NSWindow; ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons);
    class procedure UpdateWindowMask(AWindow: NSWindow; ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons);
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetPopupParent(const ACustomForm: TCustomForm;
      const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;

    {need to override these }
    class function GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TCocoaWSForm }

  TCocoaWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TCocoaWSHintWindow }

  TCocoaWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
  end;

  { TCocoaWSScreen }

  TCocoaWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TCocoaWSApplicationProperties }

  TCocoaWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

uses
  CocoaInt;

const
  FormStyleToWindowLevel: array[TFormStyle] of NSInteger = (
 { fsNormal          } 0, // NSNormalWindowLevel,
 { fsMDIChild        } 0, // NSNormalWindowLevel,
 { fsMDIForm         } 0, // NSNormalWindowLevel,
 { fsStayOnTop       } 3, // NSFloatingWindowLevel,
 { fsSplash          } 3, // NSFloatingWindowLevel,
 { fsSystemStayOnTop } 8  // NSModalPanelWindowLevel or maybe NSStatusWindowLevel?
  );

function GetDesigningBorderStyle(const AForm: TCustomForm): TFormBorderStyle;
begin
  if csDesigning in AForm.ComponentState then
    Result := bsSizeable
  else
    Result := AForm.BorderStyle;
end;

{ TCocoaWSHintWindow }

class function TCocoaWSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  win: TCocoaPanel;
  cnt: TCocoaCustomControl;
  R: NSRect;
const
  WinMask = NSBorderlessWindowMask or NSUtilityWindowMask;
begin
  win := TCocoaPanel(TCocoaPanel.alloc);

  if not Assigned(win) then
  begin
    Result := 0;
    Exit;
  end;

  R := CreateParamsToNSRect(AParams);
  win := TCocoaPanel(win.initWithContentRect_styleMask_backing_defer(R, WinMask, NSBackingStoreBuffered, False));
  win.enableCursorRects;
  TCocoaPanel(win).callback := TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  win.setAcceptsMouseMovedEvents(True);

  R.origin.x := 0;
  R.origin.y := 0;
  cnt := TCocoaCustomControl.alloc.initWithFrame(R);
  cnt.callback := TCocoaPanel(win).callback;
  win.setContentView(cnt);

  Result := TLCLIntfHandle(win);
end;

{ TLCLWindowCallback }

function TLCLWindowCallback.CanActivate: Boolean;
begin
  Result := Enabled;
end;

procedure TLCLWindowCallback.Activate;
begin
  LCLSendActivateMsg(Target, WA_ACTIVE, false);
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendActivateMsg(Target, WA_INACTIVE, false);
end;

procedure TLCLWindowCallback.CloseQuery(var CanClose: Boolean);
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose := LCLSendCloseQueryMsg(Target) > 0;
end;

procedure TLCLWindowCallback.Close;
begin
  LCLSendCloseUpMsg(Target);
end;

procedure TLCLWindowCallback.Resize;
begin
  boundsDidChange;
end;

procedure TLCLWindowCallback.Move;
begin
  boundsDidChange;
end;

function TLCLWindowCallback.GetEnabled: Boolean;
begin
  Result := NSWindow(Owner).contentView.lclIsEnabled;
end;

procedure TLCLWindowCallback.SetEnabled(AValue: Boolean);
begin
  NSWindow(Owner).contentView.lclSetEnabled(AValue);
end;


{ TCocoaWSCustomForm }

class function TCocoaWSCustomForm.GetStyleMaskFor(
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons): NSUInteger;
begin
  case ABorderStyle of
    bsSizeable, bsSizeToolWin:
      Result := NSTitledWindowMask or NSResizableWindowMask;
    bsSingle, bsDialog, bsToolWindow:
      Result := NSTitledWindowMask;
  else
    Result := NSBorderlessWindowMask;
  end;
  if biSystemMenu in ABorderIcons then
  begin
    Result := Result or NSClosableWindowMask;
    if biMinimize in ABorderIcons then
      Result := Result or NSMiniaturizableWindowMask;
  end;
end;

class procedure TCocoaWSCustomForm.UpdateWindowIcons(AWindow: NSWindow;
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons);

  procedure SetWindowButtonState(AButton: NSWindowButton; AEnabled, AVisible: Boolean);
  var
    Btn: NSButton;
  begin
    Btn := AWindow.standardWindowButton(AButton);
    if Assigned(Btn) then
    begin
      Btn.setHidden(not AVisible);
      Btn.setEnabled(AEnabled);
    end;
  end;

begin
  SetWindowButtonState(NSWindowMiniaturizeButton, biMinimize in ABorderIcons, (ABorderStyle in [bsSingle, bsSizeable]) and (biSystemMenu in ABorderIcons));
  SetWindowButtonState(NSWindowZoomButton, (biMaximize in ABorderIcons) and (ABorderStyle in [bsSizeable, bsSizeToolWin]), (ABorderStyle in [bsSingle, bsSizeable]) and (biSystemMenu in ABorderIcons));
  SetWindowButtonState(NSWindowCloseButton, True, (ABorderStyle <> bsNone) and (biSystemMenu in ABorderIcons));
end;

class procedure TCocoaWSCustomForm.UpdateWindowMask(AWindow: NSWindow;
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons);
var
  StyleMask: NSUInteger;
begin
  StyleMask := GetStyleMaskFor(ABorderStyle, ABorderIcons);
  AWindow.setStyleMask(StyleMask);
  UpdateWindowIcons(AWindow, ABorderStyle, ABorderIcons);
end;

class function TCocoaWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Form: TCustomForm absolute AWinControl;
  win: TCocoaPanel;
  cnt: TCocoaCustomControl;
  ns: NSString;
  R: NSRect;
begin
  win := TCocoaPanel(TCocoaPanel.alloc);

  if not Assigned(win) then
  begin
    Result := 0;
    Exit;
  end;

  R := CreateParamsToNSRect(AParams);
  win := TCocoaPanel(win.initWithContentRect_styleMask_backing_defer(R, GetStyleMaskFor(GetDesigningBorderStyle(Form), Form.BorderIcons), NSBackingStoreBuffered, False));
  win.setHidesOnDeactivate(False);
  UpdateWindowIcons(win, GetDesigningBorderStyle(Form), Form.BorderIcons);
  win.setLevel(FormStyleToWindowLevel[Form.FormStyle]);
  win.enableCursorRects;
  TCocoaPanel(win).callback := TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  ns := NSStringUtf8(AWinControl.Caption);
  win.setTitle(ns);
  ns.release;
  win.setAcceptsMouseMovedEvents(True);

  R.origin.x := 0;
  R.origin.y := 0;

  cnt := TCocoaCustomControl.alloc.initWithFrame(R);
  cnt.callback := TCocoaPanel(win).callback;
  win.setContentView(cnt);

  if (AParams.Style and WS_CHILD) = 0 then
  begin
    if AParams.WndParent <> 0 then
      NSWindow(AParams.WndParent).addChildWindow_ordered(win, NSWindowAbove);
  end
  else
  begin
    // TODO: docked forms
  end;

  Result := TLCLIntfHandle(win);
end;

class function TCocoaWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    AText := NSStringToString(NSWindow(AWinControl.Handle).title);
end;

class function TCocoaWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    ALength := NSWindow(AWinControl.Handle).title.length;
end;

class procedure TCocoaWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  ns: NSString;
begin
  if not AWinControl.HandleAllocated then Exit;
  ns := NSStringUtf8(AText);
  NSwindow(AWinControl.Handle).setTitle(ns);
  ns.release;
end;

class procedure TCocoaWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
//  if ACustomForm.HandleAllocated then
//    NSPanel(ACustomForm.Handle).setStyleMask(NSwindow(ACustomForm.Handle).styleMask and not NSDocModalWindowMask);
end;

class procedure TCocoaWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
//  if ACustomForm.HandleAllocated then
//    NSPanel(ACustomForm.Handle).setStyleMask(NSwindow(ACustomForm.Handle).styleMask or NSDocModalWindowMask);
end;

class procedure TCocoaWSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte);
begin
  if ACustomForm.HandleAllocated then
    if AlphaBlend then
      NSWindow(ACustomForm.Handle).setAlphaValue(Alpha / 255)
    else
      NSWindow(ACustomForm.Handle).setAlphaValue(1);
end;

class procedure TCocoaWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
begin
  if AForm.HandleAllocated then
    UpdateWindowMask(NSWindow(AForm.Handle), GetDesigningBorderStyle(AForm), ABorderIcons);
end;

class procedure TCocoaWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if AForm.HandleAllocated then
    UpdateWindowMask(NSWindow(AForm.Handle), AFormBorderStyle, AForm.BorderIcons);
end;

class procedure TCocoaWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
begin
  if AForm.HandleAllocated then
    NSWindow(AForm.Handle).setLevel(FormStyleToWindowLevel[AFormStyle]);
end;

class procedure TCocoaWSCustomForm.SetPopupParent(
  const ACustomForm: TCustomForm; const APopupMode: TPopupMode;
  const APopupParent: TCustomForm);
var
  PopupParent: TCustomForm;
begin
  if not ACustomForm.HandleAllocated then Exit;
  case APopupMode of
    pmNone:
      PopupParent := nil;
    pmAuto:
      PopupParent := Screen.ActiveForm;
    pmExplicit:
      PopupParent := APopupParent;
  end;

  if Assigned(PopupParent) then
    NSWindow(PopupParent.Handle).addChildWindow_ordered(NSWindow(ACustomForm.Handle), NSWindowAbove)
  else
  if Assigned(NSWindow(ACustomForm.Handle).parentWindow) then
    NSWindow(ACustomForm.Handle).parentWindow.removeChildWindow(NSWindow(ACustomForm.Handle));
end;

class function TCocoaWSCustomForm.GetClientBounds(const AWinControl: TWinControl; var ARect: TRect): Boolean;
begin
  if AWinControl.HandleAllocated then
    ARect := NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSCustomForm.GetClientRect(const AWinControl: TWinControl; var ARect: TRect): Boolean;
var
  x, y: Integer;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then Exit;
  ARect := NSObject(AWinControl.Handle).lclClientFrame;
  x := 0;
  y := 0;
  NSObject(AWinControl.Handle).lclLocalToScreen(x, y);
  MoveRect(ARect, x, y);
end;

class procedure TCocoaWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AWinControl.HandleAllocated then
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
end;

end.
