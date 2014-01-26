{ $Id: $}
{                  --------------------------------------------
                  cocoaprivate.pp  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaPrivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$interfaces corba}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  CocoaAll, CocoaUtils, CocoaGDIObjects,
  // LCL
  LCLType, LCLProc, Controls;

type

  { ICommonCallback }

  ICommonCallback = interface
    // mouse events
    function MouseUpDownEvent(Event: NSEvent): Boolean;
    procedure MouseClick;
    function MouseMove(Event: NSEvent): Boolean;
    function KeyEvent(Event: NSEvent): Boolean;
    // size, pos events
    procedure frameDidChange;
    procedure boundsDidChange;
    // misc events
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    function ResetCursorRects: Boolean;
    procedure BecomeFirstResponder;
    procedure ResignFirstResponder;
    // non event methods
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function GetHasCaret: Boolean;
    function GetCallbackObject: TObject;
    procedure SetHasCaret(AValue: Boolean);

    // properties
    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
  end;

  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:';
    function lclWindowState: Integer; message 'lclWindowState';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclUpdate; message 'lclUpdate';
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::';
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::';
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
    function lclGetCallback: ICommonCallback; message 'lclGetCallback';
    procedure lclClearCallback; message 'lclClearCallback';
    function lclGetPropStorage: TStringList; message 'lclGetPropStorage';
    function lclGetTarget: TObject; message 'lclGetTarget';
    function lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; message 'lclDeliverMessage:::';
    function lclIsHandle: Boolean; message 'lclIsHandle';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';

    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsPainting: Boolean; message 'lclIsPainting';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  NSViewFix = objccategory external (NSView)
    function fittingSize: NSSize; message 'fittingSize';
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  { LCLWindowExtension }

  LCLWindowExtension = objccategory(NSWindow)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;

    function lclWindowState: Integer; message 'lclWindowState'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
    function lclGetTopBarHeight:integer; message 'lclGetTopBarHeight'; reintroduce;
  end;

  { IButtonCallback }

  IButtonCallback = interface(ICommonCallback)
    procedure ButtonClick;
  end;

    { IListBoxCallBack }

  IListBoxCallBack = interface(ICommonCallback)
    procedure SelectionChanged;
  end;

  { IWindowCallback }

  IWindowCallback = interface(ICommonCallBack)
    function CanActivate: Boolean;
    procedure Activate;
    procedure Deactivate;
    procedure CloseQuery(var CanClose: Boolean);
    procedure Close;
    procedure Resize;
    procedure Move;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  IMenuItemCallback = interface(ICommonCallBack)
    procedure ItemSelected;
  end;

  { TCocoaMenu }

  TCocoaMenu = objcclass(NSMenu)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaMenuItem }

  TCocoaMenuItem = objcclass(NSMenuItem)
  public
    menuItemCallback: IMenuItemCallback;
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
    function lclGetCallback: IMenuItemCallback; override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaButton }

  TCocoaButton = objcclass(NSButton)
  protected
    procedure actionButtonClick(sender: NSObject); message 'actionButtonClick:';
    procedure boundsDidChange(sender: NSNotification); message 'boundsDidChange:';
    procedure frameDidChange(sender: NSNotification); message 'frameDidChange:';
  public
    callback: IButtonCallback;
    function initWithFrame(frameRect: NSRect): id; override;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;

    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;

  end;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;


  { TCocoaTextView }

  TCocoaTextView = objcclass(NSTextView)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaPanel }

  TCocoaPanel = objcclass(NSPanel, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
  public
    callback: IWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure sendEvent(event: NSEvent); override;
    function lclIsHandle: Boolean; override;
  end;


  TCocoaWindow = objcclass(NSWindow, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
  public
    callback: IWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure sendEvent(event: NSEvent); override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // other
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaWindowContent }

  TCocoaWindowContent = objcclass(TCocoaCustomControl)
  public
    isembedded: Boolean; // true - if the content is inside of another control, false - if the content is in its own window;
    ownwin: NSWindow;
    function lclOwnWindow: NSWindow; message 'lclOwnWindow';
    procedure lclSetFrame(const r: TRect); override;
    procedure viewDidMoveToSuperview; override;
    procedure viewDidMoveToWindow; override;
    procedure viewWillMoveToWindow(newWindow: NSWindow); override;
    procedure dealloc; override;
    procedure setHidden(aisHidden: Boolean); override;
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;


  TCocoaComboBox = objcclass;

  { TCocoaComboBoxList }

  TCocoaComboBoxList = class(TStringList)
  private
    FOwner: TCocoaComboBox;
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TCocoaComboBox);
    property Owner: TCocoaComboBox read fOwner;
  end;

  IComboboxCallBack = interface(ICommonCallBack)
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;
  end;

  { TCocoaComboBox }

  TCocoaComboBox = objcclass(NSComboBox, NSComboBoxDataSourceProtocol, NSComboBoxDelegateProtocol)
  public
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function comboBox_objectValueForItemAtIndex_(combo: TCocoaComboBox; row: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
    function numberOfItemsInComboBox(combo: TCocoaComboBox): NSInteger; message 'numberOfItemsInComboBox:';
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    procedure comboBoxWillPopUp(notification: NSNotification); message 'comboBoxWillPopUp:';
    procedure comboBoxWillDismiss(notification: NSNotification); message 'comboBoxWillDismiss:';
    procedure comboBoxSelectionDidChange(notification: NSNotification); message 'comboBoxSelectionDidChange:';
    procedure comboBoxSelectionIsChanging(notification: NSNotification); message 'comboBoxSelectionIsChanging:';
    function lclIsHandle: Boolean; override;
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;

  TCocoaListView = objcclass;

  { TCocoaStringList }

  TCocoaStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner: TCocoaListView;
    constructor Create(AOwner: TCocoaListView);
  end;

  { TCocoaListView }

  TCocoaListView = objcclass(NSTableView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol )
  public
    callback: IListBoxCallback;
    list: TCocoaStringList;
    resultNS: NSString;  //use to return values to combo
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function numberOfRowsInTableView(aTableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';

    function tableView_shouldEditTableColumn_row(tableView: NSTableView;
      tableColumn: NSTableColumn; row: NSInteger): Boolean;
      message 'tableView:shouldEditTableColumn:row:';

    function tableView_objectValueForTableColumn_row(tableView: NSTableView;
      objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
      message 'tableView:objectValueForTableColumn:row:';

    procedure tableViewSelectionDidChange(notification: NSNotification); message 'tableViewSelectionDidChange:';

    procedure dealloc; override;
    procedure resetCursorRects; override;

    // mouse
    procedure mouseDown(event: NSEvent); override;
    // procedure mouseUp(event: NSEvent); override;   This is eaten by NSTableView - worked around with NSTableViewDelegateProtocol
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    function lclIsHandle: Boolean; override;

  end;

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
  end;

procedure SetViewDefaults(AView: NSView);
function CheckMainThread: Boolean;

implementation

procedure SetViewDefaults(AView: NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

function CheckMainThread: Boolean;
begin
  Result := NSThread.currentThread.isMainThread;
end;

{ TCocoaWindowContent }

function TCocoaWindowContent.lclIsHandle: Boolean;
begin
  Result:=true;
end;

function TCocoaWindowContent.lclOwnWindow: NSWindow;
begin
  if not isembedded then
    Result := window
  else
    Result := nil;
end;

procedure TCocoaWindowContent.lclSetFrame(const r: TRect);
begin
  if isembedded then
    inherited lclSetFrame(r)
  else
    window.lclSetFrame(r);
end;

procedure TCocoaWindowContent.viewDidMoveToSuperview;
begin
  inherited viewDidMoveToSuperview;
end;

procedure TCocoaWindowContent.viewDidMoveToWindow;
begin
  isembedded := window.contentView <> self;
  if isembedded then
  begin
    if Assigned(ownwin) then
      ownwin.close;
    ownwin := nil;
  end
  else
  begin
    ownwin := window;
  end;
  inherited viewDidMoveToWindow;
end;

procedure TCocoaWindowContent.viewWillMoveToWindow(newWindow: NSWindow);
begin
  if not isembedded and (newWindow <> window) then
  begin
    window.close;
    ownwin := nil;
    isembedded := false;
  end;
  inherited viewWillMoveToWindow(newWindow);
end;

procedure TCocoaWindowContent.dealloc;
begin
  inherited dealloc;
end;

procedure TCocoaWindowContent.setHidden(aisHidden: Boolean);
begin
  if isembedded then
  begin
    inherited setHidden(aisHidden);
  end
  else
  begin
    if aisHidden and window.isVisible then
      window.orderOut(nil)
    else
    if not aisHidden and not window.isVisible then
      window.orderBack(nil);
  end;
end;

{ TCocoaPanel }

function TCocoaPanel.lclIsHandle: Boolean;
begin
  Result:=true;
end;

function TCocoaPanel.windowShouldClose(sender: id): LongBool;
var
  canClose: Boolean;
begin
  canClose := True;
  if Assigned(callback) then
    callback.CloseQuery(canClose);
  Result := canClose;
end;

procedure TCocoaPanel.windowWillClose(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Close;
end;

procedure TCocoaPanel.windowDidBecomeKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Activate;
end;

procedure TCocoaPanel.windowDidResignKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Deactivate;
end;

procedure TCocoaPanel.windowDidResize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Resize;
end;

procedure TCocoaPanel.windowDidMove(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Move;
end;

function TCocoaPanel.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaPanel.canBecomeKeyWindow: Boolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaPanel.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaPanel.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
//  if Assigned(callback) then
//    callback.ResignFirstResponder;
end;

function TCocoaPanel.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaPanel.lclClearCallback;
begin
  callback := nil;
  contentView.lclClearCallback;
end;

procedure TCocoaPanel.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaPanel.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaPanel.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaPanel.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaPanel.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaPanel.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaPanel.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaPanel.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaPanel.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaPanel.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaPanel.sendEvent(event: NSEvent);
var
  Message: NSMutableDictionary;
  Handle: HWND;
  Msg: Cardinal;
  WP: WParam;
  LP: LParam;
  Result: NSNumber;
  Obj: NSObject;
begin
  if event.type_ = NSApplicationDefined then
  begin
    // event which we get through PostMessage or SendMessage
    if event.subtype = LCLEventSubTypeMessage then
    begin
      // extract message data
      Message := NSMutableDictionary(event.data1);
      Handle := NSNumber(Message.objectForKey(NSMessageWnd)).unsignedIntegerValue;
      Msg := NSNumber(Message.objectForKey(NSMessageMsg)).unsignedLongValue;
      WP := NSNumber(Message.objectForKey(NSMessageWParam)).integerValue;
      LP := NSNumber(Message.objectForKey(NSMessageLParam)).integerValue;
      // deliver message and set result
      Obj := NSObject(Handle);
      // todo: check that Obj is still a valid NSView/NSWindow
      Result := NSNumber.numberWithInteger(Obj.lclDeliverMessage(Msg, WP, LP));
      Message.setObject_forKey(Result, NSMessageResult);
      Result.release;
    end;
  end
  else
    inherited sendEvent(event);
end;

{ TCocoaWindow }

function TCocoaWindow.lclIsHandle: Boolean;
begin
  Result:=true;
end;

function TCocoaWindow.windowShouldClose(sender: id): LongBool;
var
  canClose: Boolean;
begin
  canClose := True;
  if Assigned(callback) then
    callback.CloseQuery(canClose);
  Result := canClose;
end;

procedure TCocoaWindow.windowWillClose(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Close;
end;

procedure TCocoaWindow.windowDidBecomeKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Activate;
end;

procedure TCocoaWindow.windowDidResignKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Deactivate;
end;

procedure TCocoaWindow.windowDidResize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Resize;
end;

procedure TCocoaWindow.windowDidMove(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Move;
end;

function TCocoaWindow.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaWindow.canBecomeKeyWindow: Boolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaWindow.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  // uncommenting the following lines starts an endless focus loop

//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaWindow.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
//  if Assigned(callback) then
//    callback.ResignFirstResponder;
end;

function TCocoaWindow.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaWindow.lclClearCallback;
begin
  callback := nil;
  contentView.lclClearCallback;
end;

procedure TCocoaWindow.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaWindow.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaWindow.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaWindow.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaWindow.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaWindow.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaWindow.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaWindow.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaWindow.sendEvent(event: NSEvent);
var
  Message: NSMutableDictionary;
  Handle: HWND;
  Msg: Cardinal;
  WP: WParam;
  LP: LParam;
  Result: NSNumber;
  Obj: NSObject;
begin
  if event.type_ = NSApplicationDefined then
  begin
    // event which we get through PostMessage or SendMessage
    if event.subtype = LCLEventSubTypeMessage then
    begin
      // extract message data
      Message := NSMutableDictionary(event.data1);
      Handle := NSNumber(Message.objectForKey(NSMessageWnd)).unsignedIntegerValue;
      Msg := NSNumber(Message.objectForKey(NSMessageMsg)).unsignedLongValue;
      WP := NSNumber(Message.objectForKey(NSMessageWParam)).integerValue;
      LP := NSNumber(Message.objectForKey(NSMessageLParam)).integerValue;
      // deliver message and set result
      Obj := NSObject(Handle);
      // todo: check that Obj is still a valid NSView/NSWindow
      Result := NSNumber.numberWithInteger(Obj.lclDeliverMessage(Msg, WP, LP));
      Message.setObject_forKey(Result, NSMessageResult);
      Result.release;
    end;
  end
  else
    inherited sendEvent(event);
end;

{ TCocoaScrollView }

function TCocoaScrollView.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaScrollView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaScrollView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaScrollView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaScrollBar }

function TCocoaScrollBar.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaScrollBar.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollBar.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaScrollBar.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaScrollBar.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollBar.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaScrollBar.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaGroupBox }

function TCocoaGroupBox.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaGroupBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaGroupBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaGroupBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaGroupBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaGroupBox.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaGroupBox.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaButton }

function TCocoaButton.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaButton.actionButtonClick(sender: NSObject);
begin
  // this is the action handler of button
  if Assigned(callback) then
    callback.ButtonClick;
end;

procedure TCocoaButton.boundsDidChange(sender: NSNotification);
begin
  if Assigned(callback) then
    callback.boundsDidChange;
end;

procedure TCocoaButton.frameDidChange(sender: NSNotification);
begin
  if Assigned(callback) then
    callback.frameDidChange;
end;

function TCocoaButton.initWithFrame(frameRect: NSRect): id;
begin
  Result := inherited initWithFrame(frameRect);
  if Assigned(Result) then
  begin
    setTarget(Self);
    setAction(objcselector('actionButtonClick:'));
    NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector('boundsDidChange:'), NSViewBoundsDidChangeNotification, Result);
    NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector('frameDidChange:'), NSViewFrameDidChangeNotification, Result);
    Result.setPostsBoundsChangedNotifications(True);
    Result.setPostsFrameChangedNotifications(True);
  end;
end;

function TCocoaButton.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaButton.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaButton.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaButton.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaButton.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaButton.mouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaButton.rightMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaButton.rightMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaButton.otherMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaButton.otherMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaButton.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaButton.mouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaButton.mouseDragged(event: NSEvent);
begin
  if not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaButton.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaButton.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaButton.mouseMoved(event: NSEvent);
begin
  if not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

{ TCocoaTextField }

function TCocoaTextField.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextField.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextField.resetCursorRects;
begin
  // this will not work well because
  // cocoa replaced TextField and TextView cursors in
  // mouseEntered, mouseMoved and CursorUpdate
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaTextView }

function TCocoaTextView.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaTextView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaTextView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaSecureTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSecureTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaSecureTextField.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaSecureTextField.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaCustomControl }

function TCocoaCustomControl.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaCustomControl.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaCustomControl.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaCustomControl.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

procedure TCocoaCustomControl.drawRect(dirtyRect: NSRect);
begin
  inherited drawRect(dirtyRect);
  if CheckMainThread and ASsigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

function TCocoaCustomControl.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaCustomControl.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaCustomControl.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaCustomControl.mouseDragged(event: NSEvent);
begin
if not Assigned(callback) or not callback.MouseMove(event) then
  inherited mouseDragged(event);
end;

procedure TCocoaCustomControl.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaCustomControl.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaCustomControl.mouseMoved(event: NSEvent);
begin
if not Assigned(callback) or not callback.MouseMove(event) then
  inherited mouseMoved(event);
end;

procedure TCocoaCustomControl.keyDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyDown(event);
end;

procedure TCocoaCustomControl.keyUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

procedure TCocoaCustomControl.flagsChanged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited flagsChanged(event);
end;

procedure TCocoaCustomControl.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaCustomControl.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaCustomControl.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaCustomControl.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaCustomControl.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaCustomControl.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled: Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetVisible(AVisible: Boolean);
begin
end;

function LCLObjectExtension.lclWindowState: Integer;
begin
  Result := SIZE_RESTORED;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r: TRect);
begin
end;

procedure LCLObjectExtension.lclInvalidate;
begin
end;

procedure LCLObjectExtension.lclUpdate;
begin
end;

procedure LCLObjectExtension.lclRelativePos(var Left,Top: Integer);
begin
end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y: Integer);
begin
end;

procedure LCLObjectExtension.lclScreenToLocal(var X, Y: Integer);
begin
end;

function LCLObjectExtension.lclParent:id;
begin
  Result:=nil;
end;

function LCLObjectExtension.lclFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

procedure LCLObjectExtension.lclSetFrame(const r:TRect);
begin

end;

function LCLObjectExtension.lclClientFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

function LCLObjectExtension.lclGetCallback: ICommonCallback;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclClearCallback;
begin
end;

function LCLObjectExtension.lclGetPropStorage: TStringList;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetPropStorage
  else
    Result := nil;
end;

function LCLObjectExtension.lclGetTarget: TObject;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetTarget
  else
    Result := nil;
end;

function LCLObjectExtension.lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.DeliverMessage(Msg, WParam, LParam)
  else
    Result := 0;
end;

function LCLObjectExtension.lclIsHandle: Boolean;
begin
result:=false;
end;

{ LCLControlExtension }

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  if not Assigned(view) then Exit;
  b := view.bounds;
  with r do
  begin
    Result.origin.x := Left;
    Result.origin.y := b.size.height - Bottom;
    Result.size.width := Right - Left;
    Result.size.height := Bottom - Top;
  end;
end;

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result := IsEnabled;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  SetEnabled(AEnabled);
end;

function LCLViewExtension.lclInitWithCreateParams(const AParams: TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
begin
  p := nil;
  if (AParams.WndParent <> 0) then
  begin
    if (NSObject(AParams.WndParent).isKindOfClass_(NSView)) then
      p := NSView(AParams.WndParent)
    else
    if (NSObject(AParams.WndParent).isKindOfClass_(NSWindow)) then
      p := NSWindow(AParams.WndParent).contentView;
  end;
  with AParams do
    if Assigned(p) then
      LCLToNSRect(Types.Bounds(X,Y,Width, Height), p.frame.size.height, ns)
    else
      ns := GetNSRect(X, Y, Width, Height);

  Result := initWithFrame(ns);
  if not Assigned(Result) then
    Exit;

  setHidden(AParams.Style and WS_VISIBLE = 0);

  if Assigned(p) then
    p.addSubview(Result);
  SetViewDefaults(Result);
end;

function LCLViewExtension.lclIsVisible: Boolean;
begin
  Result := not isHidden;
end;

procedure LCLViewExtension.lclSetVisible(AVisible: Boolean);
begin
  setHidden(not AVisible);
end;

function LCLViewExtension.lclIsPainting: Boolean;
begin
  Result := Assigned(lclGetCallback) and Assigned(lclGetCallback.GetContext);
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
begin
  setNeedsDisplayInRect(RectToViewCoord(Self, r));
end;

procedure LCLViewExtension.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure LCLViewExtension.lclUpdate;
begin
  display;
end;

procedure LCLViewExtension.lclRelativePos(var Left, Top: Integer);
begin
  with frame.origin do
  begin
    Left := Round(x);
    Top := Round(y);
  end;
end;

procedure LCLViewExtension.lclLocalToScreen(var X, Y:Integer);
var
  P: NSPoint;

begin
  // 1. convert to window base
  P.x := X;
  P.y := frame.size.height-y;   // convert to Cocoa system
  P := convertPoint_ToView(P, nil);

  X := Round(P.X);
  Y := Round(window.frame.size.height-P.Y); // convert to LCL system

  // 2. convert window to screen
  window.lclLocalToScreen(X, Y);
end;

procedure LCLViewExtension.lclScreenToLocal(var X, Y: Integer);
var
  P: NSPoint;
begin
  // 1. convert from screen to window

  window.lclScreenToLocal(X, Y);
  P.x := X;
  P.y := Round(window.frame.size.height-Y); // convert to Cocoa system

  // 2. convert from window to local
  P := convertPoint_FromView(P, nil);
  X := Round(P.x);
  Y := Round(frame.size.height-P.y);   // convert to Cocoa system
end;

function LCLViewExtension.lclParent:id;
begin
  Result := superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v: NSView;
begin
  v := superview;
  if Assigned(v) then
    NSToLCLRect(frame, v.frame.size.height, Result)
  else
    Result := NSRectToRect(frame);
end;

procedure LCLViewExtension.lclSetFrame(const r: TRect);
var
  ns: NSRect;
begin
  if Assigned(superview)  then
    LCLToNSRect(r, superview.frame.size.height, ns)
  else
    ns := RectToNSRect(r);
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame: TRect;
var
  r: NSRect;
begin
  r := bounds;
  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := Round(r.size.width);
    Bottom := Round(r.size.height);
  end;
end;


{ LCLWindowExtension }

function LCLWindowExtension.lclIsVisible: Boolean;
begin
  Result := isVisible;
end;

procedure LCLWindowExtension.lclSetVisible(AVisible: Boolean);
begin
  if AVisible then
    orderFrontRegardless
  else
    orderOut(nil);
end;

function LCLWindowExtension.lclIsEnabled: Boolean;
begin
  Result := contentView.lclIsEnabled;
end;

procedure LCLWindowExtension.lclSetEnabled(AEnabled: Boolean);
begin
  contentView.lclSetEnabled(AEnabled);
end;

function LCLWindowExtension.lclWindowState: Integer;
const
  NSFullScreenWindowMask = 1 shl 14;
begin
  if isMiniaturized then
    Result := SIZE_MINIMIZED
  else
  if (styleMask and NSFullScreenWindowMask) <> 0 then
    Result := SIZE_FULLSCREEN
  else
  if isZoomed then
    Result := SIZE_MAXIMIZED
  else
    Result := SIZE_RESTORED;
end;

procedure LCLWindowExtension.lclInvalidateRect(const r: TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure LCLWindowExtension.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure LCLWindowExtension.lclUpdate;
begin
  contentView.lclUpdate;
end;

procedure LCLWindowExtension.lclRelativePos(var Left, Top: Integer);
var
   f: NSRect;
begin
  if Assigned(screen) then
  begin
    f:=frame;
    Left := Round(f.origin.x);
    Top := Round(screen.frame.size.height - f.size.height - f.origin.y);
    //debugln('Top:'+dbgs(Top));
  end;
end;

procedure LCLWindowExtension.lclLocalToScreen(var X, Y:Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    inc(X, Round(f.origin.x));
    inc(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

procedure LCLWindowExtension.lclScreenToLocal(var X, Y: Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    dec(X, Round(f.origin.x));
    dec(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

function LCLWindowExtension.lclFrame: TRect;
begin
  if Assigned(screen) then
    NSToLCLRect(frame, screen.frame.size.height, Result)
  else
    Result := NSRectToRect(frame);
end;

function LCLWindowExtension.lclGetTopBarHeight:integer;
var nw,nf: NSRect;
begin
  nf:= NSMakeRect (0, 0, 100, 100);
  nw:=contentRectForFrameRect(nf);
  result:=round(nf.size.height-nw.size.height);
end;

procedure LCLWindowExtension.lclSetFrame(const r: TRect);
var
  ns: NSRect;
  h:integer;
begin
  if Assigned(screen) then
    LCLToNSRect(r, screen.frame.size.height, ns)
  else
    ns := RectToNSRect(r);

  // add topbar height
  h:=lclGetTopBarHeight;
  ns.size.height:=ns.size.height+h;
  ns.origin.y:=ns.origin.y-h;

  setFrame_display(ns, isVisible);
end;

function LCLWindowExtension.lclClientFrame: TRect;
var
  wFrame, cFrame: NSRect;
begin
  wFrame := frame;
  cFrame := contentRectForFrameRect(wFrame);
  with Result do
  begin
    Left := Round(cFrame.origin.x - wFrame.origin.x);
    Top := Round(wFrame.origin.y + wFrame.size.height - cFrame.origin.y - cFrame.size.height);
    Right := Left + Round(cFrame.size.width);
    Bottom := Top + Round(cFrame.size.height);
  end;
end;

{ TCocoaListView }

function TCocoaListView.lclIsHandle: Boolean;
begin
  Result:=true;
end;

function TCocoaListView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaListView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaListView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaListView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaListView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaListView.numberOfRowsInTableView(aTableView:NSTableView): NSInteger;
begin
  if Assigned(list) then
    Result := list.Count
  else
    Result := 0;
end;


function TCocoaListView.tableView_shouldEditTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean;
begin
result:=false;  // disable cell editing by default
end;

function TCocoaListView.tableView_objectValueForTableColumn_row(tableView: NSTableView;
  objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
begin
  if not Assigned(list) then
    Result:=nil
  else begin
    if row>=list.count then Result:=nil
    else
    begin
      resultNS.release;  //so we can reuse it
      resultNS := NSStringUtf8(list[row]);
      Result:= ResultNS;
    end;
  end;
end;

procedure TCocoaListView.dealloc;
begin
  FreeAndNil(list);
  resultNS.release;
  inherited dealloc;
end;

procedure TCocoaListView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaListView.tableViewSelectionDidChange(notification: NSNotification);
begin
   if Assigned(callback) then
      callback.SelectionChanged;
end;

procedure TCocoaListView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaListView.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaListView.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaListView.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaListView.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaListView.mouseDragged(event: NSEvent);
begin
if not Assigned(callback) or not callback.MouseMove(event) then
  inherited mouseDragged(event);
end;

procedure TCocoaListView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaListView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaListView.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;

procedure TCocoaListView.keyDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyDown(event);
end;

procedure TCocoaListView.keyUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

{ TCocoaStringList }

procedure TCocoaStringList.Changed;
begin
  inherited Changed;
  Owner.reloadData;
end;

constructor TCocoaStringList.Create(AOwner:TCocoaListView);
begin
  Owner:=AOwner;
  inherited Create;
end;

{ TCocoaComboBoxList }

procedure TCocoaComboBoxList.Changed;
begin
  fOwner.reloadData;
  inherited Changed;
end;

constructor TCocoaComboBoxList.Create(AOwner:TCocoaComboBox);
begin
  fOwner:=AOwner;
end;

{ TCocoaComboBox }

function TCocoaComboBox.lclIsHandle: Boolean;
begin
  Result:=true;
end;

function TCocoaComboBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaComboBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaComboBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaComboBox.comboBox_objectValueForItemAtIndex_(combo:TCocoaComboBox;
  row: NSInteger):id;
begin
  if not Assigned(list) or (row<0) or (row>=list.Count)
    then Result:=nil
    else Result:=NSStringUtf8(list[row]);
end;

function TCocoaComboBox.numberOfItemsInComboBox(combo:TCocoaComboBox):NSInteger;
begin
  if not Assigned(list) then Result:=0
  else Result:=list.Count;
end;

procedure TCocoaComboBox.dealloc;
begin
  if Assigned(list) then
  begin
    list.Free;
    list:=nil;
  end;
  resultNS.release;
  inherited dealloc;
end;

function TCocoaComboBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaComboBox.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaComboBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaComboBox.comboBoxWillPopUp(notification: NSNotification);
begin
  callback.ComboBoxWillPopUp;
end;

procedure TCocoaComboBox.comboBoxWillDismiss(notification: NSNotification);
begin
  callback.ComboBoxWillDismiss;
end;

procedure TCocoaComboBox.comboboxSelectionDidChange(notification: NSNotification);
begin
  callback.ComboBoxSelectionDidChange;
end;

procedure TCocoaComboBox.comboBoxSelectionIsChanging(notification: NSNotification);
begin
  callback.ComboBoxSelectionIsChanging;
end;

{ TCocoaMenu }

function TCocoaMenu.lclIsHandle: Boolean;
begin
  Result:=true;
end;

procedure TCocoaMenu.lclItemSelected(sender:id);
begin

end;

{ TCocoaMenuITem }

function TCocoaMenuItem.lclIsHandle: Boolean;
begin
  Result:=true;
end;

procedure TCocoaMenuItem.lclItemSelected(sender:id);
begin
  menuItemCallback.ItemSelected;
end;

function TCocoaMenuItem.lclGetCallback: IMenuItemCallback;
begin
  result:=menuItemCallback;
end;

end.

