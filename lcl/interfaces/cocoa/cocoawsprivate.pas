unit CocoaWSPrivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}
{.$DEFINE COCOA_DEBUG_SETBOUNDS}

interface

uses
  Types, Classes, Controls, SysUtils,
  WSControls, LCLType, LCLMessageGlue, LMessages, LCLProc, LCLIntf, Graphics, Forms,
  CocoaAll,
  CocoaPrivate, CocoaWSService, CocoaCallback, CocoaGDIObjects,
  CocoaCursor, CocoaCaret, CocoaConfig, CocoaUtils, Cocoa_Extra,
  CocoaWSCustomControl;

type

  { TCocoaWSControl }

  TCocoaWSControl = class(TWSControl)
  published
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
  end;

  { TCocoaWSWinControl }

  TCocoaWSWinControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetCanvasScaleFactor(const AControl: TControl): Double; override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class function GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean; override;

    class function  GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class function  GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean; override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
  end;

implementation

var
  LastMouse: TLastMouseInfo;
  LastMouseLeftButtonAsRight: Boolean;

{ TCocoaWSControl }

class function TCocoaWSControl.GetCanvasScaleFactor(const AControl: TControl
  ): Double;
begin
  if Assigned(AControl.Parent) then
    Result := AControl.Parent.GetCanvasScaleFactor
  else
    Result := 1;
end;

{ TCocoaWSWinControl }

class function TCocoaWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
begin
  Result := TCocoaWSCustomControl.CreateHandle(AWinControl, AParams);
end;

class procedure TCocoaWSWinControl.DestroyHandle(const AWinControl: TWinControl);
var
  obj: NSObject;
  Callback: ICommonCallback;
  CallbackObject: TObject;

  {
    it's equivalent to TWinControl.Focused(), and it's to avoid problems from
    directly calling control.Focused().
    it's because subclasses might override Focused(), calling the overridden
    Focused() during Destroy/DestroyHandle might then cause unforeseen issues.
    e.g. TSynBaseCompletionForm:
    1. free 'SizeDrag' in TSynBaseCompletionForm.Destroy() first
    2. then calling DestroyHandle() in TWinControl.Destroy()
    3. and reference to 'SizeDrag' in TSynBaseCompletionForm.Focused()
  }
  function Focused( const control: TWinControl ): Boolean;
  begin
    Result:= ( FindOwnerControl(GetFocus) = control );
  end;

begin
  if not AWinControl.HandleAllocated then
    Exit;

  if Assigned(CocoaWidgetSetState) and (AWinControl.Handle = CocoaWidgetSetState.CaptureControl) then
    CocoaWidgetSetState.releaseCapture;

  obj := NSObject(AWinControl.Handle);
  Callback := obj.lclGetCallback;

  if Focused(AWinControl) and Assigned(Callback) then
    Callback.ResignFirstResponder;   // dont' call LCLSendKillFocusMsg
  LCLSendDestroyMsg( AWinControl );

  if obj.isKindOfClass_(NSView) then
  begin
    // no need to "retain" prior to "removeFromSuperview"
    // the original referecnce count with "alloc" is not being released
    // after "addToSuperview"
    NSView(obj).removeFromSuperview;
  end
  else
  if obj.isKindOfClass_(NSWindow) then
    NSWindow(obj).close;

  // destroy the callback
  if Assigned(Callback) then
  begin
    if Callback.HasCaret then DestroyCaret(nil);
    CallbackObject := Callback.GetCallbackObject;
    Callback.RemoveTarget;
    Callback := nil;
    obj.lclClearCallback;
    // Do not free the callback object here. It might be processing an event
    // and is performing a self destruction. Thus there might be a code performing
    // even after DestroyHandle() was called. The destruction needs to be delayed
    // until after the event processing is done
    CocoaWidgetSetService.addToBeReleasedLCLObjects(CallbackObject);
  end;
  obj.release;
end;

class function TCocoaWSWinControl.GetCanvasScaleFactor(const AControl: TControl
  ): Double;
var
  obj: NSObject;
  win: NSWindow;
begin
  win := nil;
  Result := 1;

  if TWinControl(AControl).HandleAllocated then
  begin
    obj := NSObject(TWinControl(AControl).Handle);
    if obj.isKindOfClass_(NSView) then
      win := NSView(obj).window
    else if obj.isKindOfClass_(NSWindow) then
      win := NSWindow(obj);
  end;

  if Assigned(win) then
  begin
    if win.respondsToSelector( ObjCSelector('backingScaleFactor')) then
      Result := win.backingScaleFactor
    else if win.respondsToSelector( ObjCSelector('userSpaceScaleFactor')) then // for older OSX
      Result := win.userSpaceScaleFactor;
  end;
end;

class procedure TCocoaWSWinControl.SetText(const AWinControl: TWinControl; const AText: String);
var
  obj: NSObject;
begin
  if not AWinControl.HandleAllocated then
    Exit;
  obj := NSObject(AWinControl.Handle);
  if obj.isKindOfClass_(NSControl) then
    TCocoaControlUtil.setStringValue(NSControl(obj), AText);
end;

class function TCocoaWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  obj: NSObject;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    Exit;
  obj := NSObject(AWinControl.Handle);
  Result := obj.isKindOfClass_(NSControl);
  if Result then
    AText := TCocoaControlUtil.getStringValue(NSControl(obj));
end;

class function TCocoaWSWinControl.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  obj: NSObject;
  s: NSString;
begin
  Result := AWinControl.HandleAllocated;
  if not Result then
    Exit;

  obj := NSObject(AWinControl.Handle);
  Result := obj.isKindOfClass_(NSControl);
  if not Result then Exit;

  s := NSControl(obj).stringValue;
  if Assigned(s) then
    ALength := s.length
  else
    ALength := 0
end;

class function TCocoaWSWinControl.GetClientBounds(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result := AWinControl.HandleAllocated;
  if Result then
    ARect := NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCocoaWSWinControl.GetClientRect(const AWincontrol: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=(AWinControl.Handle<>0);
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
  if (ARect.Left<>0) or (ARect.Top<>0) then
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
end;

class procedure TCocoaWSWinControl.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lView: NSView;
  Size: NSSize;
  r: TRect;
begin
  if not AWinControl.HandleAllocated then Exit;

  lView := NSObject(AWinControl.Handle).lclContentView;
  if lView = nil then Exit;

  //todo: using fittingSize is wrong - it's based on constraints of the control solely.
  //CocoaWidgetset is not using these constrains. As a result, CocoaComboBox
  //produces wrong size: width 3 and height 26 (or OSX 10.9)
  //as well as SpinEdit itself. The better approach is to use intrinsicContentSize method.
  // Felipe: intrinsicContentSize doesn't give any better results in my tests, it results in even smaller controls
  if lView.respondsToSelector(objcselector('fittingSize')) then // fittingSize is 10.7+
  begin
    Size := lView.fittingSize();
    r := lview.lclGetFrameToLayoutDelta;
    PreferredWidth := Round(Size.width) - r.Left + r.Right;
    PreferredHeight := Round(Size.height) - r.Top + r.Bottom;
  end;
end;

class procedure TCocoaWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  cb : ICommonCallBack;
  r  : TRect;
begin
  if AWinControl.HandleAllocated then
  begin
    {$IFDEF COCOA_DEBUG_SETBOUNDS}
    writeln(Format('TCocoaWSWinControl.SetBounds: %s Bounds=%s',
      [AWinControl.Name, dbgs(Bounds(ALeft, ATop, AWidth, AHeight))]));
    {$ENDIF}
    NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));
  end;
end;

function getWinControlAtMouse(): TControl;
begin
  Result:= Application.GetControlAtMouse;
  if not Assigned(Result) then
    exit;

  // find TWinControl (not TGraphicControl)
  // see also TControl.SetTempCursor()
  while not (Result is TWinControl) do
  begin
    Result:= Result.Parent;
    if not Assigned(Result) then
      exit;
  end;
end;

class procedure TCocoaWSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
var
  control: TControl;
  topParent: TControl;
begin
  //debugln('SetCursor '+AWinControl.name+' '+dbgs(ACursor));

  // screen cursor has higher priority than control cursor.
  if Screen.Cursor<>crDefault
    then exit;

  // control cursor only should be set when mouse in the keyWindow or hintWindow.
  // for the MacOS system automatic restore cursor feature(also an issue),
  // it is more appropriate to set the default cursor when the mouse is out
  // of the keyWindow, which has been set in TLCLCommonCallback.MouseMove().
  // the keyWindow here refers to the Client Frame, excluding TitleBar.
  // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40515
  topParent:= AWinControl.GetTopParent;
  if (topParent is TCustomForm) and not (topParent is THintWindow) then
  begin
    if NSView(TCustomForm(topParent).handle).window <> NSApp.keyWindow then
    begin
      CursorHelper.ForceSetDefaultCursor;
      Exit;
    end;
  end;

  // control cursor only need be set when mouse in AWinControl.
  // suppose there is a Button, which is to set a Cursor of a ListBox.
  // without the code here, it will be set to the Cursor of the ListBox
  // after clicking the Button.
  if not ((AWinControl is TCustomForm) and (csDesigning in AWinControl.ComponentState)) then
  begin
    control:= getWinControlAtMouse;
    if control<>AWinControl then
      exit;
  end;

  CursorHelper.SetNewCursor( TCocoaCursor(ACursor) );
end;

type
  NSFontSetter = objccategory external(NSObject)
    procedure setFont(afont: NSFont); message 'setFont:';
    procedure setTextColor(clr: NSColor); message 'setTextColor:';
  end;

class procedure TCocoaWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
var
  Obj: NSObject;
  Cell: NSCell;
  Str: NSAttributedString;
  NewStr: NSMutableAttributedString;
  Range: NSRange;
begin
  if not (AWinControl.HandleAllocated) then Exit;

  Obj := NSObject(AWinControl.Handle).lclContentView;

  if Obj.respondsToSelector(ObjCSelector('setFont:')) then
    Obj.setFont(TCocoaFont(AFont.Reference.Handle).Font);

  if Obj.respondsToSelector(ObjCSelector('setTextColor:')) then
  begin
    if AFont.Color = clDefault then
      Obj.setTextColor(NSColor.controlTextColor)
    else
      Obj.setTextColor(TCocoaColorUtil.toColor(ColorToRGB(AFont.Color)));
  end;
end;

class procedure TCocoaWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  invalidate(AWinControl);
end;

function indexInList(ctrl: id; l: TFPList): integer;
var
 i : integer;
begin
  for i:=0 to l.Count-1 do
    if PtrUInt(TWinControl(l[i]).Handle)=PtrUInt(ctrl) then
    begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

function SortHandles(param1: id; param2: id; param3: Pointer): NSComparisonResult; cdecl;
var
  i1,i2: integer;
begin
  i1:=indexInList(param1, TFPList(param3));
  i2:=indexInList(param2, TFPList(param3));
  if i1<i2 then Result:=NSOrderedDescending
  else if i1>i2 then Result:=NSOrderedAscending
  else Result:=NSOrderedSame;
end;

class procedure TCocoaWSWinControl.SetChildZPosition(const AWinControl,
  AChild: TWinControl; const AOldPos, ANewPos: Integer; const AChildren: TFPList
  );
var
  pr : NSView;
begin
  if (not AWinControl.HandleAllocated) or (not AChild.HandleAllocated) then Exit;

  pr := NSView(AWinControl.Handle).lclContentView;

  if pr.subviews.count <= 1 then exit;

  // 1. sorting is a better option than removing / adding a view.
  //    whenever a focused (firstrepsonder view) is removed and added to front,
  //    focus is lost. the issue was exposed by
  //    https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/853461fed65fed2ce1614bcc06eca5b880810f0e
  //    <LCL: fixed TWinControl.SetChildZPosition, WS must be informed about change of order in any case. issue #40450>
  // 2. regarding performance, though on every comparison an index must be
  //    searched withing a list, but performance still does not need to be
  //    paid too much attention to.
  // 3. it's because SetChildZPosition() rarely needs to be called at runtime,
  //    and usually there are only a few sibling controls.
  pr.sortSubviewsUsingFunction_context(@SortHandles, AChildren);
end;

class procedure TCocoaWSWinControl.ShowHide(const AWinControl: TWinControl);
var
  lShow: Boolean;
begin
  //WriteLn(Format('[TCocoaWSWinControl.ShowHide] AWinControl=%s %s', [AWinControl.Name, AWinControl.ClassName]));
  if AWinControl.HandleAllocated then
  begin
    lShow := AWinControl.HandleObjectShouldBeVisible;

    NSObject(AWinControl.Handle).lclSetVisible(lShow);
  end;
end;

class procedure TCocoaWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  if AWinControl.HandleAllocated then
     NSObject(AWinControl.Handle).lclInvalidate;
end;

class procedure TCocoaWSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  bc : TCocoaBitmapContext;
  v  : NSView;
  b  : NSBitmapImageRep;
  obj : NSObject;
  f  : NSRect;
begin
  if not (TObject(ADC) is TCocoaBitmapContext) then Exit;
  if not NSObject(AWinControl.Handle).isKindOfClass(NSView) then Exit;
  bc := TCocoaBitmapContext(ADC);
  v := NSView(AWinControl.Handle);
  f := v.frame;
  f.origin.x := 0;
  f.origin.y := 0;

  b := v.bitmapImageRepForCachingDisplayInRect(f);

  v.cacheDisplayInRect_toBitmapImageRep(f, b);
  bc.DrawImageRep(
    NSMakeRect(0,0, f.size.width, f.size.height),
    f, b);
end;

end.

