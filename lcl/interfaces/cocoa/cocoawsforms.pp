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
{$include cocoadefines.inc}

interface

uses
  // RTL,FCL
  MacOSAll, CocoaAll, Classes,
  // LCL
  Controls, Forms, Graphics, LCLType, Messages, LMessages, LCLProc,
  // Widgetset
  WSForms, WSLCLClasses, WSProc, LCLMessageGlue,
  // LCL Cocoa
  CocoaPrivate, CocoaUtils, CocoaWSCommon, CocoaWSStdCtrls, CocoaWSMenus,
  CocoaWindows, CocoaScrollers, cocoa_extra;

type
  { TLCLWindowCallback }

  TLCLWindowCallback = class(TLCLCommonCallBack, IWindowCallback)
  private
    IsActivating: boolean;
  public
    window : CocoaAll.NSWindow;
    constructor Create(AOwner: NSObject; ATarget: TWinControl); override;
    destructor Destroy; override;

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
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
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
  public
    class procedure UpdateWindowMask(AWindow: NSWindow; ABorderStyle: TFormBorderStyle; ABorderIcons: TBorderIcons);
    class function GetWindowFromHandle(const ACustomForm: TCustomForm): TCocoaWindow;
    class function GetWindowContentFromHandle(const ACustomForm: TCustomForm): TCocoaWindowContent;
  published
    class function AllocWindowHandle: TCocoaWindow; virtual;
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;

    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetModalResult(const ACustomForm: TCustomForm; ANewValue: TModalResult); override;

    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean; const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm; const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm; const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetRealPopupParent(const ACustomForm: TCustomForm;
      const APopupParent: TCustomForm); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;

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

procedure ArrangeTabOrder(const AWinControl: TWinControl);

implementation

uses
  CocoaInt;

const
  // The documentation says we should use NSNormalWindowLevel=4 for normal forms,
  // but in practice this causes the issue http://bugs.freepascal.org/view.php?id=28473
  // The only value that works is zero =(
  FormStyleToWindowLevel: array[TFormStyle] of NSInteger = (
 { fsNormal          } 0,
 { fsMDIChild        } 0,
 { fsMDIForm         } 0,
 { fsStayOnTop       } 9, // NSStatusWindowLevel
 { fsSplash          } 9, // NSStatusWindowLevel
 { fsSystemStayOnTop } 10  // NSModalPanelWindowLevel
  );
  // Window levels make the form always stay on top, so if it is supposed to
  // stay on top of the app only, then a workaround is to hide it while the app
  // is deactivated
  FormStyleToHideOnDeactivate: array[TFormStyle] of Boolean = (
 { fsNormal          } False,
 { fsMDIChild        } False,
 { fsMDIForm         } False,
 { fsStayOnTop       } True,
 { fsSplash          } True,
 { fsSystemStayOnTop } False
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
  cb: TLCLWindowCallback;
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
  win.setDelegate(win);
  if AWinControl.Perform(WM_NCHITTEST, 0, 0)=HTTRANSPARENT then
    win.setIgnoresMouseEvents(True)
  else
    win.setAcceptsMouseMovedEvents(True);

  R.origin.x := 0;
  R.origin.y := 0;
  cnt := TCocoaWindowContent.alloc.initWithFrame(R);
  cb := TLCLWindowCallback.Create(cnt, AWinControl);
  cb.window := win;
  cnt.callback := cb;
  cnt.preventKeyOnShow := true;
  TCocoaPanel(win).callback := cb;

  win.setContentView(cnt);

  Result := TLCLIntfHandle(cnt);
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

destructor TLCLWindowCallback.Destroy;
begin
  if Assigned(window) then window.lclClearCallback;
  inherited Destroy;
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
      if NSObject(ACustForm.Menu.Handle).isKindOfClass_(TCocoaMenu) then
      begin
        CocoaWidgetSet.SetMainMenu(ACustForm.Menu.Handle, ACustForm.Menu);
        TCocoaMenu(ACustForm.Menu.Handle).attachAppleMenu();
      end
      else
        debugln('Warning: Menu does not have a valid handle.');
    end
    else
      CocoaWidgetSet.SetMainMenu(0, nil);

    LCLSendActivateMsg(Target, WA_ACTIVE, false);
    LCLSendSetFocusMsg(Target);
    // The only way to update Forms.ActiveCustomForm for the main form
    // is calling TCustomForm.SetFocusedControl, see bug 31056
    ACustForm.SetFocusedControl(ACustForm.ActiveControl);

    IsActivating:=False;
  end;
end;

procedure TLCLWindowCallback.Deactivate;
begin
  LCLSendActivateMsg(Target, WA_INACTIVE, false);
  LCLSendKillFocusMsg(Target);
end;

procedure TLCLWindowCallback.CloseQuery(var CanClose: Boolean);
var
  i: Integer;
begin
  // Message results : 0 - do nothing, 1 - destroy window
  CanClose := LCLSendCloseQueryMsg(Target) > 0;

  // Special code for modal forms, which otherwise would get 0 here and not call Close
  if (CocoaWidgetSet.CurModalForm = FTarget) and
    (TCustomForm(Target).ModalResult <> mrNone) then
  begin
    NSApp.stopModal();
    CocoaWidgetSet.CurModalForm := nil;
    {// Felipe: This code forces focusing another form, its a work around
    // for a gdb issue, gdb doesn't start the app properly
    //
    // At this point the modal form is closed, but the previously open form isn't focused
    // Focus the main window if it is visible
    if Application.MainForm.Visible then Application.MainForm.SetFocus()
    else
    begin
      // if the mainform is hidden, just choose any visible form
      // ToDo: Figure out a better solution
      for i := 0 to Screen.FormCount-1 do
        if Screen.Forms[i].Visible then
        begin
          Screen.Forms[i].SetFocus();
          Break;
        end;
    end;}
  end;
end;

procedure TLCLWindowCallback.Close;
begin
  LCLSendCloseUpMsg(Target);
end;

procedure TLCLWindowCallback.Resize;
begin
  boundsDidChange(Owner);
end;

procedure TLCLWindowCallback.Move;
begin
  boundsDidChange(Owner);
end;

function TLCLWindowCallback.GetEnabled: Boolean;
begin
  Result := Owner.lclIsEnabled;
end;

procedure TLCLWindowCallback.SetEnabled(AValue: Boolean);
begin
  Owner.lclSetEnabled(AValue);
end;

{ TCocoaWSScrollingWinControl}

class function  TCocoaWSScrollingWinControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  scrollcon: TCocoaScrollView;
  docview: TCocoaCustomControl;
  lcl : TLCLCommonCallback;
begin
  docview := TCocoaCustomControl.alloc.lclInitWithCreateParams(AParams);
  scrollcon:=EmbedInScrollView(docView);
  scrollcon.setBackgroundColor(NSColor.windowBackgroundColor);
  scrollcon.setAutohidesScrollers(True);
  scrollcon.setHasHorizontalScroller(True);
  scrollcon.setHasVerticalScroller(True);
  scrollcon.isCustomRange := true;

  lcl := TLCLCommonCallback.Create(docview, AWinControl);
  lcl.BlockCocoaUpDown := true;
  docview.callback := lcl;
  docview.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  scrollcon.callback := lcl;
  lcl.Frame:=scrollcon;
  scrollcon.setDocumentView(docview);
  Result := TLCLIntfHandle(scrollcon);
end;


{ TCocoaWSCustomForm }

procedure ArrangeTabOrder(const AWinControl: TWinControl);
var
  lList: TFPList;
  prevControl, curControl: TWinControl;
  lPrevView, lCurView: NSView;
  i: Integer;
begin
  lList := TFPList.Create;
  try
    AWinControl.GetTabOrderList(lList);
    if lList.Count>0 then
      begin
      prevControl := TWinControl(lList.Items[lList.Count-1]);
      lPrevView := GetNSObjectView(NSObject(prevControl.Handle));
      for i := 0 to lList.Count-1 do
      begin
        curControl := TWinControl(lList.Items[i]);
        lCurView := GetNSObjectView(NSObject(curControl.Handle));

        if (lCurView <> nil) and (lPrevView <> nil) then
          lPrevView.setNextKeyView(lCurView);

        lPrevView := lCurView;
      end;
    end;
  finally
    lList.Free;
  end;
end;


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

class function TCocoaWSCustomForm.GetWindowFromHandle(const ACustomForm: TCustomForm): TCocoaWindow;
begin
  Result := nil;
  if not ACustomForm.HandleAllocated then Exit;
  Result := TCocoaWindow(TCocoaWindowContent(ACustomForm.Handle).lclOwnWindow);
end;

class function TCocoaWSCustomForm.GetWindowContentFromHandle(const ACustomForm: TCustomForm): TCocoaWindowContent;
begin
  Result := nil;
  if not ACustomForm.HandleAllocated then Exit;
  Result := TCocoaWindowContent(ACustomForm.Handle);
end;

// Some projects that use the LCL need to override this
class function TCocoaWSCustomForm.AllocWindowHandle: TCocoaWindow;
begin
  Result := TCocoaWindow(TCocoaWindow.alloc);
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
  lDestView: NSView;
  ds: TCocoaDesignOverlay;
  cb: TLCLWindowCallback;
begin
  //todo: create TCocoaWindow or TCocoaPanel depending on the border style
  //      if parent is specified neither Window nor Panel needs to be created
  //      the only thing that needs to be created is Content

  pool := NSAutoreleasePool.alloc.init;
  R := CreateParamsToNSRect(AParams);
  cnt := TCocoaWindowContent.alloc.initWithFrame(R);
  cb := TLCLWindowCallback.Create(cnt, AWinControl);
  cnt.callback := cb;

  if (AParams.Style and WS_CHILD) = 0 then
  begin

    win := AllocWindowHandle;

    if not Assigned(win) then
    begin
      pool.release;
      Result := 0;
      Exit;
    end;

    win := TCocoaWindow(win.initWithContentRect_styleMask_backing_defer(R,
      GetStyleMaskFor(GetDesigningBorderStyle(Form), Form.BorderIcons), NSBackingStoreBuffered, False));
    UpdateWindowIcons(win, GetDesigningBorderStyle(Form), Form.BorderIcons);
    // For safety, it is better to not apply any setLevel & similar if the form is just a standard style
    // see issue http://bugs.freepascal.org/view.php?id=28473
    if not (Form.FormStyle in [fsNormal, fsMDIChild, fsMDIForm])
      and not (csDesigning in AWinControl.ComponentState) then
    begin
      win.setLevel(FormStyleToWindowLevel[Form.FormStyle]);
      win.setHidesOnDeactivate(FormStyleToHideOnDeactivate[Form.FormStyle]);
    end;
    win.enableCursorRects;

    TCocoaWindow(win).callback := cb;
    cb.window := win;

    win.setDelegate(win);
    ns := NSStringUtf8(AWinControl.Caption);
    win.setTitle(ns);
    ns.release;
    win.setReleasedWhenClosed(False); // do not release automatically
    win.setAcceptsMouseMovedEvents(True);

    if win.respondsToSelector(ObjCSelector('setTabbingMode:')) then
      win.setTabbingMode(NSWindowTabbingModeDisallowed);

    if AWinControl.Perform(WM_NCHITTEST, 0, 0)=HTTRANSPARENT then
      win.setIgnoresMouseEvents(True);

    cnt.callback := TCocoaWindow(win).callback;
    cnt.callback.IsOpaque:=true;
    win.LCLForm := Form;
    win.setContentView(cnt);

    // Don't call addChildWindow_ordered here because this function can cause
    // events to arrive for this window, creating a second call to TCocoaWSCustomForm.CreateHandle
    // while the first didn't finish yet, instead delay the call
    cnt.popup_parent := AParams.WndParent;

    // support for drag & drop
    win.registerForDraggedTypes(NSArray.arrayWithObjects_count(@NSFilenamesPboardType, 1));

    if IsFormDesign(AWinControl) then begin
      ds:=(TCocoaDesignOverlay.alloc).initWithFrame(cnt.frame);
      ds.callback := cnt.callback;
      ds.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
      cnt.addSubview_positioned_relativeTo(ds, NSWindowAbove, nil);
      cnt.overlay := ds;
    end;

  end
  else
  begin
    cnt.callback := TLCLCommonCallback.Create(cnt, AWinControl);
    if AParams.WndParent <> 0 then
    begin
      lDestView := GetNSObjectView(NSObject(AParams.WndParent));
      lDestView.addSubView(cnt);
      if cnt.window <> nil then
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

class procedure TCocoaWSCustomForm.DestroyHandle(const AWinControl: TWinControl
  );
var
  win : NSWindow;
begin
  if not AWinControl.HandleAllocated then
    Exit;

  win := TCocoaWindowContent(AWinControl.Handle).lclOwnWindow;
  if Assigned(win) then win.close;

  TCocoaWSWinControl.DestroyHandle(AWinControl);
  //inherited DestroyHandle(AWinControl);
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
  {$ifdef COCOA_MODAL_USE_SESSION}
  if ACustomForm.HandleAllocated then
    NSPanel(ACustomForm.Handle).setStyleMask(NSwindow(ACustomForm.Handle).styleMask and not NSDocModalWindowMask);
  if CocoaWidgetSet.CurModalSession <> nil then
    NSApp.endModalSession(CocoaWidgetSet.CurModalSession);
  CocoaWidgetSet.CurModalSession := nil;
  {$endif}

  {$ifdef COCOA_USE_NATIVE_MODAL}
  NSApp.stopModal();
  {$endif}

  CocoaWidgetSet.CurModalForm := nil;
end;

class procedure TCocoaWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  lWinContent: TCocoaWindowContent;
  {$ifdef COCOA_MODAL_USE_SESSION}
  win: TCocoaWindow;
  CurModalSession: NSModalSession;
  {$endif}
  {$ifdef COCOA_USE_NATIVE_MODAL}
  win: TCocoaWindow;
  {$endif}
  fullscreen: Boolean;
begin
  // Another possible implementation is to have modal started in ShowHide with (fsModal in AForm.FormState)

  // Handle PopupParent
  lWinContent := GetWindowContentFromHandle(ACustomForm);

  fullscreen := ACustomForm.WindowState = wsFullScreen;
  if (not fullscreen) and (lWinContent.window.isKindOfClass(TCocoaWindow)) then
    fullscreen := TCocoaWindow(lWinContent.window).lclIsFullScreen;

  // A window opening in full screen doesn't like to be added as someones popup
  // Thus resolvePopupParent should only be used for non full-screens forms
  if (lWinContent <> nil) and (not fullscreen) then
    lWinContent.resolvePopupParent();

  CocoaWidgetSet.CurModalForm := ACustomForm;
  // LCL initialization code would cause the custom form to be disabled
  // (due to the fact, ShowModal() has not been called yet, and a previous form
  // might be disabled at the time.
  // ...
  // The fact there's a single global variable is used to indicate, that there's
  // a modal form (neglecting the need for stack of modal forms)
  // makes a developer want to rewrite the whole approach for something more
  // Cocoa and good-practicies friendly.
  // ...
  // At this point of time, we simply force enabling of the new modal form
  // (which is happening in LCL code, but at the wrong time)
  NSObject(ACustomForm.Handle).lclSetEnabled(true);

  // Another possible implementation is using a session, but this requires
  //  disabling the other windows ourselves
  {$ifdef COCOA_MODAL_USE_SESSION}
  win := TCocoaWSCustomForm.GetWindowFromHandle(ACustomForm);
  if win = nil then Exit;
  CocoaWidgetSet.CurModalSession := NSApp.beginModalSessionForWindow(win);
  NSApp.runModalSession(CocoaWidgetSet.CurModalSession);}
  {$endif}

  // Another possible implementation is using runModalForWindow
  {$ifdef COCOA_USE_NATIVE_MODAL}
  win := TCocoaWSCustomForm.GetWindowFromHandle(ACustomForm);
  if win = nil then Exit;
  NSApp.runModalForWindow(win);
  {$endif}
end;

// If ShowModal will not be fully blocking in the future this can be removed
class procedure TCocoaWSCustomForm.SetModalResult(const ACustomForm: TCustomForm;
  ANewValue: TModalResult);
begin
  if (CocoaWidgetSet.CurModalForm = ACustomForm) and (ANewValue <> 0) then
    CloseModal(ACustomForm);
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
    win := NSWindow(TCocoaWindowContent(AForm.Handle).lclOwnWindow);
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
    win := NSWindow(TCocoaWindowContent(AForm.Handle).lclOwnWindow);
    if Assigned(win) then
      UpdateWindowMask(win, AFormBorderStyle, AForm.BorderIcons);
  end;
end;

class procedure TCocoaWSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
var
  win : NSWindow;
begin
  if AForm.HandleAllocated and not (csDesigning in AForm.ComponentState) then
  begin
    win := TCocoaWindowContent(AForm.Handle).lclOwnWindow;
    if Assigned(win) then
    begin
      win.setLevel(FormStyleToWindowLevel[AFormStyle]);
      win.setHidesOnDeactivate(FormStyleToHideOnDeactivate[AFormStyle]);
    end;
  end;
end;

class procedure TCocoaWSCustomForm.SetRealPopupParent(
  const ACustomForm: TCustomForm; const APopupParent: TCustomForm);
var
  win : NSWindow;
begin
  if not ACustomForm.HandleAllocated then Exit;

  win := TCocoaWindowContent(ACustomForm.Handle).lclOwnWindow;
  if Assigned(win.parentWindow) then
    win.parentWindow.removeChildWindow(win);
  if Assigned(APopupParent) then
    NSWindow( NSView(APopupParent.Handle).window).addChildWindow_ordered(win, NSWindowAbove);
end;

class procedure TCocoaWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  lShow : Boolean;
  w : NSWindow;
begin
  lShow := AWinControl.HandleObjectShouldBeVisible;
  // TCustomForm class of LCL doesn't do anything specific about first time showing
  // of wsFullScreen window. Thus it should be taken care of in WS size
  if lShow and (TCustomForm(AWinControl).WindowState = wsFullScreen) then
  begin
    w := NSView(AWinControl.Handle).window;
    if Assigned(w) and (w.isKindOfClass(TCocoaWindow)) then
      TCocoaWindow(w).lclSwitchFullScreen(true);
  end
  else
    TCocoaWSWinControl.ShowHide(AWinControl);

  if (lShow) then
    ArrangeTabOrder(AWinControl);
end;

class function TCocoaWSCustomForm.GetClientBounds(
  const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result := False;
  if not AWinControl.HandleAllocated then Exit;
  ARect := NSObject(AWinControl.Handle).lclClientFrame;
  Result := True;
end;

class function TCocoaWSCustomForm.GetClientRect(const AWincontrol: TWinControl;
  var ARect: TRect): Boolean;
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
    TCocoaWindowContent(AwinControl.Handle).callback.boundsDidChange(NSObject(AWinControl.Handle));
    pool.release;
  end;
end;

end.
