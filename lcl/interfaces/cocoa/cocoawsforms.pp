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
  private
    IsActivating: boolean;
  public
    constructor Create(AOwner: NSObject; ATarget: TWinControl); override;
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
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
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
 { fsNormal          } 4, // NSNormalWindowLevel
 { fsMDIChild        } 4, // NSNormalWindowLevel
 { fsMDIForm         } 4, // NSNormalWindowLevel
 { fsStayOnTop       } 9, // NSStatusWindowLevel
 { fsSplash          } 9, // NSStatusWindowLevel
 { fsSystemStayOnTop } 10  // NSModalPanelWindowLevel
  );

  HintWindowLevel = 11;  // NSPopUpMenuWindowLevel

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
  cnt: TCocoaWindowContent;
  R: NSRect;
  Form: TCustomForm absolute AWinControl;
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
  win.setLevel(HintWindowLevel);
  TCocoaPanel(win).callback := TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  win.setAcceptsMouseMovedEvents(True);

  R.origin.x := 0;
  R.origin.y := 0;
  cnt := TCocoaWindowContent.alloc.initWithFrame(R);
  cnt.callback := TCocoaPanel(win).callback;
  win.setContentView(cnt);

  Result := TLCLIntfHandle(cnt);
end;


class procedure TCocoaWSHintWindow.DestroyHandle(const AWinControl: TWinControl);
var
  cnt: TCocoaWindowContent;
  Callback: ICommonCallback;
  CallbackObject: TObject;
begin
  if not AWinControl.HandleAllocated then
    Exit;

  cnt:= TCocoaWindowContent(AWinControl.Handle);
  cnt.removeFromSuperview;

  cnt.ownwin.close;

  Callback := cnt.lclGetCallback;
  if Assigned(Callback) then
  begin
    CallbackObject := Callback.GetCallbackObject;
    Callback := nil;
    cnt.lclClearCallback;
    CallbackObject.Free;
  end;

  cnt.release;
end;

{ TLCLWindowCallback }

function TLCLWindowCallback.CanActivate: Boolean;
begin
  Result := Enabled;
end;

constructor TLCLWindowCallback.Create(AOwner: NSObject; ATarget: TWinControl);
begin
  inherited;
  IsActivating:=false;
end;

procedure TLCLWindowCallback.Activate;
var
  ACustForm: TCustomForm;
begin
  if not IsActivating then
    begin
    IsActivating:=True;
    ACustForm := Target as TCustomForm;

    if (ACustForm.Menu <> nil) and
       (ACustForm.Menu.HandleAllocated) then
      begin
      if NSObject(ACustForm.Menu.Handle).isKindOfClass_(TCocoaMenuItem) then
        begin
        if TCocoaMenuItem(ACustForm.Menu.Handle).hasSubmenu then
          CocoaWidgetSet.SetMainMenu(HMENU(TCocoaMenuItem(ACustForm.Menu.Handle).submenu))
        else
          debugln('Warning: Menu does not have a valid handle.');
        end
      else
        CocoaWidgetSet.SetMainMenu(ACustForm.Menu.Handle);
      end
    else
      CocoaWidgetSet.SetMainMenu(0);

    LCLSendActivateMsg(Target, WA_ACTIVE, false);
    LCLSendSetFocusMsg(Target);
    IsActivating:=False;
    end;
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendActivateMsg(Target, WA_INACTIVE, false);
  LCLSendKillFocusMsg(Target);
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
  win: TCocoaWindow;
  cnt: TCocoaWindowContent;
  ns: NSString;
  R: NSRect;
  pool:NSAutoReleasePool;
 begin
  //todo: create TCocoaWindow or TCocoaPanel depending on the border style
  //      if parent is specified neither Window nor Panel needs to be created
  //      the only thing that needs to be created is Content

  pool := NSAutoreleasePool.alloc.init;
  R := CreateParamsToNSRect(AParams);
  cnt := TCocoaWindowContent.alloc.initWithFrame(R);

  if (AParams.Style and WS_CHILD) = 0 then
  begin

    win := TCocoaWindow(TCocoaWindow.alloc);

    if not Assigned(win) then
       begin
       Result := 0;
       Exit;
       end;

    win := TCocoaWindow(win.initWithContentRect_styleMask_backing_defer(R, GetStyleMaskFor(GetDesigningBorderStyle(Form), Form.BorderIcons), NSBackingStoreBuffered, False));
    UpdateWindowIcons(win, GetDesigningBorderStyle(Form), Form.BorderIcons);
    win.setLevel(FormStyleToWindowLevel[Form.FormStyle]);
    win.enableCursorRects;
    TCocoaWindow(win).callback := TLCLWindowCallback.Create(win, AWinControl);
    win.setDelegate(win);
    ns := NSStringUtf8(AWinControl.Caption);
    win.setTitle(ns);
    ns.release;
    win.setAcceptsMouseMovedEvents(True);


    cnt.callback := TCocoaWindow(win).callback;
    cnt.callback.IsOpaque:=true;
    win.setContentView(cnt);

    if (AParams.WndParent <> 0) then
    begin
      if (NSObject(AParams.WndParent).isKindOfClass(TCocoaWindowContent)) and (not TCocoaWindowContent(AParams.WndParent).isembedded) then
        TCocoaWindowContent(AParams.WndParent).window.addChildWindow_ordered(win, NSWindowAbove)
      else
        NSWindow(AParams.WndParent).addChildWindow_ordered(win, NSWindowAbove);
    end;
  end
  else
  begin
    cnt.callback := TLCLCustomControlCallback.Create(cnt, AWinControl);
    if AParams.WndParent <> 0 then
      begin
      NSView(APArams.WndParent).addSubView(cnt);
      if cnt.window<>nil then
         cnt.window.setAcceptsMouseMovedEvents(True);
      cnt.callback.IsOpaque:=true;
    //  todo: We have to find a way to remove the following notifications save before cnt will be released
    //  NSNotificationCenter.defaultCenter.addObserver_selector_name_object(cnt, objcselector('didBecomeKeyNotification:'), NSWindowDidBecomeKeyNotification, cnt.window);
    //  NSNotificationCenter.defaultCenter.addObserver_selector_name_object(cnt, objcselector('didResignKeyNotification:'), NSWindowDidResignKeyNotification, cnt.window);
      end;
  end;

  pool.release;
  Result := TLCLIntfHandle(cnt);
end;


class function TCocoaWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  win : NSWindow;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
  begin
    win := TCocoaWindowContent(AWinControl.Handle).lclOwnWindow;
    if not Assigned(win) then
      AText := ''
    else
      AText := NSStringToString(win.title);
  end;
end;

class function TCocoaWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  win : NSWindow;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
  begin
    win := TCocoaWindowContent(AWinControl.Handle).lclOwnWindow;
    if Assigned(win) then
      ALength := NSWindow(AWinControl.Handle).title.length
    else
      ALength := 0;
  end;
end;

class procedure TCocoaWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  ns: NSString;
  win : NSWindow;
begin
  if not AWinControl.HandleAllocated then
    Exit;
  win := TCocoaWindowContent(AWinControl.Handle).lclOwnWindow;
  if Assigned(win) then
  begin
    ns := NSStringUtf8(AText);
    NSwindow(win).setTitle(ns);
    ns.release;
  end;
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
var
  win : NSWindow;
begin
  if ACustomForm.HandleAllocated then
  begin
    win := TCocoaWindowContent(ACustomForm.Handle).lclOwnWindow;
    if not Assigned(win) then
      Exit;
    if AlphaBlend then
      win.setAlphaValue(Alpha / 255)
    else
      win.setAlphaValue(1);
  end;
end;

class procedure TCocoaWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
  const ABorderIcons: TBorderIcons);
var
  win : NSWindow;
begin
  if AForm.HandleAllocated then
  begin
    win := TCocoaWindowContent(AForm.Handle).lclOwnWindow;
    if Assigned(win) then
      UpdateWindowMask(win, GetDesigningBorderStyle(AForm), ABorderIcons);
  end;
end;

class procedure TCocoaWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
  const AFormBorderStyle: TFormBorderStyle);
var
  win : NSWindow;
begin
  if AForm.HandleAllocated then
  begin
    win := TCocoaWindowContent(AForm.Handle).lclOwnWindow;
    if Assigned(win) then
      UpdateWindowMask(win, AFormBorderStyle, AForm.BorderIcons);
  end;
end;

class procedure TCocoaWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
var
  win : NSWindow;
begin
  if AForm.HandleAllocated then
  begin
    win := TCocoaWindowContent(AForm.Handle).lclOwnWindow;
    if Assigned(win) then
      win.setLevel(FormStyleToWindowLevel[AFormStyle]);
  end;
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
var pool: NSAutoreleasePool;
begin
  if AWinControl.HandleAllocated then
  begin
    pool := NSAutoreleasePool.alloc.init;
    //debugln('TCocoaWSCustomForm.SetBounds: '+AWinControl.Name+'Bounds='+dbgs(Bounds(ALeft, ATop, AWidth, AHeight)));
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
    TCocoaWindowContent(AwinControl.Handle).callback.boundsDidChange;
    pool.release;
  end;
end;

end.
