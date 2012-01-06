{ $Id: cocoawsforms.pp 12783 2007-11-08 11:45:39Z tombo $}
{
 *****************************************************************************
 *                             CocoaWSForms.pp                               *
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
    procedure Activate; virtual;
    procedure Deactivate; virtual;
    procedure CloseQuery(var CanClose: Boolean); virtual;
    procedure Close; virtual;
    procedure Resize; virtual;
    procedure Move; virtual;
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
    class procedure SetStyleMaskFor(AWindow: NSWindow; ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons; ADesigning: Boolean);
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
//    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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

{ TLCLWindowCallback }

procedure TLCLWindowCallback.Activate;
begin
  LCLSendActivateMsg(Target, True, false);
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendDeactivateStartMsg(Target);
end;

procedure TLCLWindowCallback.CloseQuery(var CanClose: Boolean);
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose:=LCLSendCloseQueryMsg(Target)>0;
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


{ TCocoaWSCustomForm }

class procedure TCocoaWSCustomForm.SetStyleMaskFor(AWindow: NSWindow;
  ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons;
  ADesigning: Boolean);

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

var
  StyleMask: NSUInteger;
begin
  if ADesigning then
    ABorderStyle := bsSingle;

  case ABorderStyle of
    bsSizeable, bsSizeToolWin:
      StyleMask := NSTitledWindowMask or NSResizableWindowMask;
    bsSingle, bsDialog, bsToolWindow:
      StyleMask := NSTitledWindowMask;
  else
    StyleMask := NSBorderlessWindowMask;
  end;
  if biSystemMenu in ABorderIcons then
  begin
    StyleMask := StyleMask or NSClosableWindowMask;
    if biMinimize in ABorderIcons then
      StyleMask := StyleMask or NSMiniaturizableWindowMask;
  end;
  AWindow.setStyleMask(StyleMask);
  // also change enable state for standard window buttons
  SetWindowButtonState(NSWindowMiniaturizeButton, biMinimize in ABorderIcons, (ABorderStyle in [bsSingle, bsSizeable]) and (biSystemMenu in ABorderIcons));
  SetWindowButtonState(NSWindowZoomButton, (biMaximize in ABorderIcons) and (ABorderStyle in [bsSizeable, bsSizeToolWin]), (ABorderStyle in [bsSingle, bsSizeable]) and (biSystemMenu in ABorderIcons));
  SetWindowButtonState(NSWindowCloseButton, True, (ABorderStyle <> bsNone) and (biSystemMenu in ABorderIcons));
end;

class function TCocoaWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  win: TCocoaWindow;
  cnt: TCocoaCustomControl;
  ns: NSString;
const
  WinMask = NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask;
begin
  win := TCocoaWindow(TCocoaWindow.alloc);

  if not Assigned(win) then
  begin
    Result := 0;
    Exit;
  end;

  win := TCocoaWindow(win.initWithContentRect_styleMask_backing_defer(CreateParamsToNSRect(AParams), WinMask, NSBackingStoreBuffered, False));
  win.enableCursorRects;
  TCocoaWindow(win).callback := TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  ns := NSStringUtf8(AWinControl.Caption);
  win.setTitle(ns);
  ns.release;
  win.setAcceptsMouseMovedEvents(True);

  cnt := TCocoaCustomControl.alloc.init;
  cnt.callback := TCocoaWindow(win).callback;
  win.setContentView(cnt);

  Result := TLCLIntfHandle(win);
end;

class function TCocoaWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    AText := NSStringToString(TCocoaWindow(AWinControl.Handle).title);
end;

class function TCocoaWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    ALength := TCocoaWindow(AWinControl.Handle).title.length;
end;

class procedure TCocoaWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  ns: NSString;
begin
  if not AWinControl.HandleAllocated then Exit;
  ns := NSStringUtf8(AText);
  TCocoaWindow(AWinControl.Handle).setTitle(ns);
  ns.release;
end;

class procedure TCocoaWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  if ACustomForm.HandleAllocated then
    if CocoaWidgetSet.NSApp.modalWindow = NSWindow(ACustomForm.Handle) then
      CocoaWidgetSet.NSApp.stopModal;
end;

class procedure TCocoaWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  if ACustomForm.HandleAllocated then
    CocoaWidgetSet.NSApp.runModalForWindow(NSWindow(ACustomForm.Handle));
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
    SetStyleMaskFor(NSWindow(AForm.Handle), AForm.BorderStyle, ABorderIcons, csDesigning in AForm.ComponentState);
end;

class procedure TCocoaWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
begin
  if AForm.HandleAllocated then
    SetStyleMaskFor(NSWindow(AForm.Handle), AFormBorderStyle, AForm.BorderIcons, csDesigning in AForm.ComponentState);
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
