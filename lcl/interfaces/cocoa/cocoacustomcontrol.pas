unit CocoaCustomControl;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  Forms,
  MacOSAll, CocoaAll, CocoaPrivate, CocoaCallback,
  CocoaCursor, Cocoa_Extra, CocoaUtils;

type
  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  private
    fstr : NSString;

    isdrawing   : integer;
    faileddraw  : Boolean;
  public
    callback: ICommonCallback;
    auxMouseByParent: Boolean;
    procedure dealloc; override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsMouseInAuxArea(Event: NSevent): Boolean; override;
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
    // nsview
    procedure setFrame(aframe: NSRect); override;
    // value
    procedure setStringValue(avalue: NSString); override;
    function stringValue: NSString; override;
    procedure addSubView(aview: NSView); override;
    procedure doCommandBySelector (aSelector: SEL); override;
  end;

  { TCocoaCustomControlWithBaseInputClient }

  TCocoaCustomControlWithBaseInputClient = objcclass(TCocoaCustomControl, NSTextInputClientProtocol)
  private
    _inIME: Boolean;
  private
    function getWindowEditor(): NSTextView; message 'getWindowEditor';
    procedure DoCallInputClientInsertText(nsText:NSString); message 'DoCallInputClientInsertText:';
  public
    // NSTextInputClientProtocol related.
    // implements a base NSTextInputClient for non-editable LCL CustomControl,
    // like Form, Grid, ListView, that are not system control and not FullEditControl.
    // 1. when using IME in these controls, a temporary and one-time editor is shown
    //    at the bottom of the control, supporting IME such as Chinese.
    // 2. refers to MacOS Finder, when using IME in the file list view,
    //    a small window will pop up at the bottom of the screen for input.
    //    the text can then be used for filename starting character match.
    // 3. it is useful for implementing IME support for controls that do not
    //    have a text input window.
    procedure keyDown(theEvent: NSEvent); override;
    procedure insertText_replacementRange (aString: id; replacementRange: NSRange);
    procedure setMarkedText_selectedRange_replacementRange (aString: id; selectedRange: NSRange; replacementRange: NSRange);
    procedure unmarkText;
    function selectedRange: NSRange;
    function markedRange: NSRange;
    function hasMarkedText: LCLObjCBoolean;
    function firstRectForCharacterRange_actualRange (aRange: NSRange; actualRange: NSRangePointer): NSRect;

    function attributedSubstringForProposedRange_actualRange (aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
    function validAttributesForMarkedText: NSArray;
    function characterIndexForPoint (aPoint: NSPoint): NSUInteger;
  end;

implementation

{ TCocoaCustomControl }

procedure TCocoaCustomControl.setStringValue(avalue: NSString);
begin
  if Assigned(fstr) then fstr.release;
  if ASsigned(avalue) then
    fstr:=avalue.copyWithZone(nil)
  else
    fstr:=nil;
  inherited setStringValue(avalue);
end;

function TCocoaCustomControl.stringValue: NSString;
begin
  Result:=fstr;
end;

procedure TCocoaCustomControl.addSubView(aview: NSView);
var
  mask: NSUInteger;
begin
  inherited addSubView(aview);

  if Assigned(aview) then
  begin
    // forcing LCL compatible "auto-move" mode. Sticking to left/top corner
    if not autoresizesSubviews then
      {$ifdef BOOLFIX}
      setAutoresizesSubviews_(Ord(true));
      {$else}
      setAutoresizesSubviews(true);
      {$endif}
    if self.isFlipped then
      mask:= NSViewMaxYMargin or NSViewMaxXMargin
    else
      mask:= NSViewMinYMargin or NSViewMaxXMargin;
    aview.setAutoresizingMask(mask);
  end;
end;

procedure TCocoaCustomControl.dealloc;
begin
  if Assigned(fstr) then fstr.release;
  inherited dealloc;
end;

function TCocoaCustomControl.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaCustomControl.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  // By default, a mouse-down event in a window that isn’t the key window
  // simply brings the window forward and makes it key; the event isn’t sent
  // to the NSView object over which the mouse click occurs. The NSView can
  // claim an initial mouse-down event, however, by overriding acceptsFirstMouse: to return YES.
  // see bug #33034
  Result:=true;
end;

procedure TCocoaCustomControl.drawRect(dirtyRect: NSRect);
begin
  if isdrawing=0 then faileddraw:=false;
  inc(isdrawing);
  inherited drawRect(dirtyRect);

  // Implement Color property
  if Assigned(callback) then
    callback.DrawBackground(NSGraphicsContext.currentContext, bounds, dirtyRect);

  if CheckMainThread and Assigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
  dec(isdrawing);

  if (isdrawing=0) and (faileddraw) then
  begin
    // if the frame is changed during the Paint event,
    // redrawing must be triggered to ensure that the
    // correctly updated NSGraphicsContext is obtained.
    // 1. for the new version of macOS, just set setNeedsDisplay()
    // 2. for older versions of macOS, display() must be called by itself,
    //    and just setting by setNeedsDisplay() will not work.
    if NSAppKitVersionNumber >= NSAppKitVersionNumber11_0 then
      self.lclInvalidate
    else
      self.display;
  end;
end;

function TCocoaCustomControl.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaCustomControl.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaCustomControl.lclIsMouseInAuxArea(Event: NSevent): Boolean;
begin
  if auxMouseByParent and Assigned(superview) then
    Result := superview.lclIsMouseInAuxArea(Event)
  else
    Result := false;
end;

procedure TCocoaCustomControl.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaCustomControl.mouseDragged(event: NSEvent);
begin
  window.disableCursorRects;

  if not Assigned(callback) or not callback.MouseMove(event) then
    // calling inherited causes the drag event to be passed to the
    // parent controls

    //inherited mouseDragged(event);
    ;
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

procedure TCocoaCustomControl.scrollWheel(event: NSEvent);
var
  eatEvent: Boolean;
begin
  if Assigned(self.lclGetTarget) and (self.lclGetTarget is TScrollingWinControl) then begin
    inherited scrollWheel(event);
    if Assigned(callback) then
      callback.scrollWheel(event);
  end else begin
    if NOT Assigned(callback) or NOT callback.scrollWheel(event) then begin
      inherited scrollWheel(event);
    end else begin
      eatEvent:= NSAppKitVersionNumber < (NSAppKitVersionNumber14_0+1);
      if eatEvent then
        Exit;
      if Assigned(self.enclosingScrollView) then
        self.enclosingScrollView.scrollWheel(event);
    end
  end;
end;

procedure TCocoaCustomControl.setFrame(aframe: NSRect);
begin
  if NSEqualRects(aframe, frame) then Exit;
  if isdrawing>0 then
    faileddraw := true;

  inherited setFrame(aframe);
  // it actually should come from a notifcation
  if Assigned(callback) then callback.frameDidChange(self);
end;

procedure TCocoaCustomControl.mouseUp(event: NSEvent);
begin
  if not window.areCursorRectsEnabled then
  begin
    window.enableCursorRects;
    window.resetCursorRects;
    CursorHelper.SetCursorAtMousePos;
  end;

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

procedure TCocoaCustomControl.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
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

procedure TCocoaCustomControl.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaCustomControl.doCommandBySelector(aSelector: SEL);
begin
  inherited doCommandBySelector(ASelector);
end;

{ TCocoaCustomControlWithBaseInputClient }

function TCocoaCustomControlWithBaseInputClient.getWindowEditor(): NSTextView;
begin
  Result:= NSTextView( self.window.fieldEditor_forObject(true,nil) );
end;

procedure TCocoaCustomControlWithBaseInputClient.DoCallInputClientInsertText(nsText:NSString);
begin
  if Assigned(callback) then
    callback.InputClientInsertText(nsText.UTF8String);
  nsText.release;
end;

procedure TCocoaCustomControlWithBaseInputClient.keyDown(theEvent: NSEvent);
var
  textView: NSView;
  isFirst: Boolean;

  function isControlKey: Boolean;
  begin
    Result:= False;
    if _inIME then
      Exit;
    Result:= theEvent.keyCode in [kVK_Return, kVK_ANSI_KeypadEnter, kVK_Escape];
  end;

  function isSpaceShortCut: Boolean;
  const
    ModifiersOfInterest: NSUInteger = (NSControlKeyMask or NSShiftKeyMask or NSAlternateKeyMask or NSCommandKeyMask or NSFunctionKeyMask);
  begin
    Result:= False;
    if _inIME then
      Exit;
    if theEvent.keyCode <> kVK_Space then
      Exit;
    Result:= (theEvent.modifierFlags and ModifiersOfInterest) <> 0;
  end;

begin
  if isControlKey or isSpaceShortCut then
  begin
    inherited;
    exit;
  end;

  isFirst:= not _inIME;
  inputContext.handleEvent(theEvent);
  if _inIME and isFirst then
  begin
    textView:= getWindowEditor();
    textView.setFrameSize( NSMakeSize(self.frame.size.width,16) );
    self.addSubView( textView );
  end
  else if not _inIME then
    inputContext.discardMarkedText;
end;

// in TCocoaCustomControl, such as Form, Grid, ListView,
// after inputting text, another control may be focused.
// in insertText_replacementRange(), Cocoa/InputContext doesn't like it,
// so calling InputClientInsertText() asynchronously.
procedure TCocoaCustomControlWithBaseInputClient.insertText_replacementRange(aString: id;
  replacementRange: NSRange);
var
  nsText: NSString;
begin
  if not _inIME then exit;

  unmarkText;

  nsText:= getNSStringObject(aString).copy;
  performSelector_withObject_afterDelay(ObjCSelector('DoCallInputClientInsertText:'), nsText, 0 );
end;

procedure TCocoaCustomControlWithBaseInputClient.setMarkedText_selectedRange_replacementRange(
  aString: id; selectedRange: NSRange; replacementRange: NSRange);
var
  textView: NSTextView;
  nsText: NSString;
begin
  nsText:= getNSStringObject(aString);
  if nsText.length > 0 then
  begin
    _inIME:= true;
    textView:= getWindowEditor();
    if Assigned(textView) then
      textView.setMarkedText_selectedRange_replacementRange(aString,selectedRange,replacementRange);
  end
  else
    unmarkText;
end;

function TCocoaCustomControlWithBaseInputClient.hasMarkedText: LCLObjCBoolean;
begin
  Result := _inIME;
end;

procedure TCocoaCustomControlWithBaseInputClient.unmarkText;
var
  textView: NSTextView;
begin
  _inIME:= false;
  textView:= getWindowEditor();
  if Assigned(textView) then
    textView.removeFromSuperview;
end;

function TCocoaCustomControlWithBaseInputClient.firstRectForCharacterRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSRect;
var
  point: NSPoint;
  rect: NSRect;
begin
  point:= self.convertPoint_toView(NSZeroPoint, nil);
  rect:= NSMakeRect(point.x, point.y, 0, 16);
  Result:= self.window.convertRectToScreen(rect);
end;

function TCocoaCustomControlWithBaseInputClient.selectedRange: NSRange;
var
  textView: NSText;
begin
  textView:= getWindowEditor();
  if not Assigned(textView) then
    Result:= NSMakeRange( NSNotFound, 0 )
  else
    Result:= textView.selectedRange;
end;

function TCocoaCustomControlWithBaseInputClient.markedRange: NSRange;
var
  textView: NSTextView;
begin
  textView:= getWindowEditor();
  if not Assigned(textView) then
    Result:= NSMakeRange( NSNotFound, 0 )
  else
    Result:= textView.markedRange;
end;

function TCocoaCustomControlWithBaseInputClient.attributedSubstringForProposedRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
begin
  Result := nil;
end;

function TCocoaCustomControlWithBaseInputClient.validAttributesForMarkedText: NSArray;
begin
  Result := nil;
end;

function TCocoaCustomControlWithBaseInputClient.characterIndexForPoint(aPoint: NSPoint
  ): NSUInteger;
begin
  Result := 0;
end;

end.

