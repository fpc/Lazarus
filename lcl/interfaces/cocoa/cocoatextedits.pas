{ $Id: $}
{                  --------------------------------------------
                  cocoatextedits.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaTextEdits;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_SETBOUNDS}
{.$DEFINE COCOA_SPIN_DEBUG}
{.$DEFINE COCOA_SPINEDIT_INSIDE_CONTAINER}

interface

uses
  Types, Classes, SysUtils,
  Math, // needed for MinDouble, MaxDouble
  LCLType, Graphics, Controls,
  MacOSAll, CocoaAll, CocoaConfig, CocoaUtils, CocoaGDIObjects,
  CocoaPrivate;

type

  TCocoaFieldEditor = objcclass;

  NSTextField_LCLExt = objcprotocol
    procedure lclSetMaxLength(amax: integer); message 'lclSetMaxLength:';
  end;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField, NSTextField_LCLExt)
    callback: ICommonCallback;
    maxLength: Integer;
    fixedInitSetting: Boolean;

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // key
    procedure textDidChange(notification: NSNotification); override;
    function textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'textView:shouldChangeTextInRange:replacementString:';
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    procedure lclSetMaxLength(amax: integer);
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField, NSTextField_LCLExt)
  public
    maxLength: Integer;
    callback: ICommonCallback;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // key
    procedure textDidChange(notification: NSNotification); override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    procedure lclSetMaxLength(amax: integer);
  end;

  { TCocoaTextView }

  TCocoaTextView = objcclass(NSTextView, NSTextDelegateProtocol, NSTextViewDelegateProtocol)
  public
    callback: ICommonCallback;
    FEnabled: Boolean;
    FUndoManager: NSUndoManager;

    supressTextChangeEvent: Integer; // if above zero, then don't send text change event
    wantReturns: Boolean;

    procedure dealloc; override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    procedure changeColor(sender: id); override;
    // keyboard
    procedure cut(sender: id); override;
    procedure paste(sender: id); override;
    procedure insertNewline(sender: id); override;
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

    function lclIsEnabled: Boolean; override;
    procedure lclSetEnabled(AEnabled: Boolean); override;

    // delegate methods
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); override;
    function undoManagerForTextView(view: NSTextView): NSUndoManager; message 'undoManagerForTextView:';
  end;

  { TCococaFieldEditorExt }

  TCococaFieldEditorExt = objccategory(NSTextView)
    // this override should take care of any Cocoa editors possible
    // for example NSSecureTextView used with TCocoaSecureField aka NSSecureTextField
    function lclGetCallback: ICommonCallback; reintroduce;
  end;

  { TCocoaFieldEditor }

  TCocoaFieldEditor = objcclass(NSTextView)
  public
    // This flag is used to hack an infinite loop
    // when switching "editable" (readonly) mode of NSTextField
    // see TCocoaWSCustomEdit.SetReadOnly
    goingReadOnly: Boolean;
    function lclGetCallback: ICommonCallback; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    procedure setDelegate(adelegate: NSTextDelegateProtocol); override;
    procedure lclReviseCursorColor; message 'lclReviseCursorColor';
    // keyboard
    procedure cut(sender: id); override;
    procedure paste(sender: id); override;
    procedure insertNewline(sender: id); override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  { TCocoaTextControlUtil }

  TCocoaTextControlUtil = class
  public
    class procedure setStringValue(
      const text: NSText;
      const s: String );
    class function getStringValue(
      const text: NSText ): String;
    class function setLCLFont(
      const textField: NSTextField;
      const lclFont: TFont ): Boolean; overload;
    class function setLCLFont(
      const textField: NSTextField;
      const lclControl: TObject ): Boolean; overload;
    class procedure setWordWrap(
      const textView: NSTextView;
      const scrollView: NSScrollView;
      const wordWrap: Boolean );
    class procedure setAllignment(
      const textView: NSTextView;
      const align: TAlignment );
    class procedure setAllignment(
      const textField: NSTextField;
      const align: TAlignment );
    class procedure setBorderStyle(
      const textField: NSTextField;
      const borderStyle: TBorderStyle );
    class procedure setTextHint(
      const textField: NSTextField;
      const str: String ); overload;
    class procedure setTextHint(
      const obj: NSObject;
      const str: String ); overload;
  end;

  { TCocoaSpinEdit }
{$IFDEF COCOA_SPINEDIT_INSIDE_CONTAINER}
  TCocoaSpinEdit = objcclass(NSControl)
  public
    callback: ICommonCallback;
    Stepper: NSStepper;
    Edit: NSTextField;
    Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    procedure UpdateControl(ASpinEdit: TCustomFloatSpinEdit); message 'UpdateControl:';
    procedure CreateSubcontrols(ASpinEdit: TCustomFloatSpinEdit; const AParams: TCreateParams); message 'CreateSubControls:AParams:';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure CalculateSubcontrolPos(const ASpinLCLBounds: TRect; out AEditBounds, AStepperBounds: TRect); message 'CalculateSubcontrolPos:AEditBounds:AStepperBounds:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    // lcl
    function acceptsFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // NSViewFix
    function fittingSize: NSSize; override;
  end;
{$ELSE}

  { TCocoaSpinEditStepper }

  TCocoaSpinEditStepper = objcclass(NSStepper)
    callback: ICommonCallback;
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
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  TCocoaSpinEdit = objcclass(TCocoaTextField)
    Stepper: NSStepper;
    NumberFormatter: NSNumberFormatter;
    decimalPlaces: Integer;

    avoidChangeEvent: Integer;
    anyChange: Boolean;

    //Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    function updateStepper: boolean; message 'updateStepper';
    procedure UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer; checkMinMax: Boolean); message 'UpdateControl::::::';
    procedure lclCreateSubcontrols(const AParams: TCreateParams); message 'lclCreateSubControls:';
    procedure lclReleaseSubcontrols; message 'lclReleaseSubcontrols';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    procedure textDidChange(notification: NSNotification); override;
    procedure textDidEndEditing(notification: NSNotification); override;
    // lcl
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure lclSetVisible(AVisible: Boolean); override;
    procedure lclSetFrame(const r: TRect); override;
    // NSViewFix
    function fittingSize: NSSize; override;
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
    procedure mouseMoved(event: NSEvent); override;
  end;
{$ENDIF}

// they have already been added to Cocoa_Extra;
// they are kept here just for historical compatibility.
const
  NSTextAlignmentLeft      = 0;
  NSTextAlignmentRight     = {$ifdef USE_IOS_VALUES}2{$else}1{$endif}; // it's 2 for iOS and family
  NSTextAlignmentCenter    = {$ifdef USE_IOS_VALUES}1{$else}2{$endif}; // it's 1 for iOS and family
  NSTextAlignmentJustified = 3;
  NSTextAlignmentNatural   = 4;

implementation

{ TCococaFieldEditorExt }

function TCococaFieldEditorExt.lclGetCallback: ICommonCallback;
begin
  if isFieldEditor and Assigned(delegate) then
  begin
    Result := NSObject(delegate).lclGetCallback;
  end
  else
    Result := inherited lclGetCallback;
end;

{ TCocoaSpinEditStepper }

function TCocoaSpinEditStepper.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

procedure TCocoaSpinEditStepper.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    if Assigned(Callback) then
      callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSpinEditStepper.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSpinEditStepper.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSpinEditStepper.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSpinEditStepper.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSpinEditStepper.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSpinEditStepper.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSpinEditStepper.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSpinEditStepper.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSpinEditStepper.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSpinEditStepper.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

{ TCocoaFieldEditor }

function GetEditBox(src: TCocoaFieldEditor): NSView;
var
  v : NSObject;
begin
  Result := nil;
  if not Assigned(src) then Exit;
  v := NSObject(src.delegate);
  if Assigned(v) and (v.isKindOfClass(NSView)) then
    Result := NSView(v);
end;

function TCocoaFieldEditor.lclGetCallback: ICommonCallback;
begin
  if Assigned(delegate) then Result := NSObject(delegate).lclGetCallback
  else Result := nil;
end;

function TCocoaFieldEditor.becomeFirstResponder: LCLObjCBoolean;
begin
  if goingReadOnly then Result := false
  else Result:=inherited becomeFirstResponder;
end;

procedure TCocoaFieldEditor.setDelegate(adelegate: NSTextDelegateProtocol);
begin
  inherited setDelegate(adelegate);
  lclReviseCursorColor;
end;

procedure TCocoaFieldEditor.lclReviseCursorColor;
var
  clr :NSColor;
begin
  if not Assigned(delegate) then Exit;
  if not (NSObject(delegate).isKindOfClass(NSTextField)) then Exit;
  clr := NSTextField(delegate).textColor;
  self.setInsertionPointColor(clr);
end;

procedure TCocoaFieldEditor.cut(sender: id);
var
  accept: Boolean = False;
begin
  accept:= TCocoaLCLMessageUtil.SendOnEditCut(self);
  if accept then
    inherited cut(sender);
end;

procedure TCocoaFieldEditor.paste(sender: id);
var
  accept: Boolean = False;
begin
  accept:= TCocoaLCLMessageUtil.SendOnEditPaste(self);
  if accept then
    inherited paste(sender);
end;

procedure TCocoaFieldEditor.insertNewline(sender: id);
begin
  // 10.6 cocoa handles the editors Return key as "insertNewLine" command (that makes sense)
  // which turns into textDidEndEditing done command (that also makes sense)
  // however, it ends up in an endless loop of "end-editing" calls.
  //
  // todo: find the reason for the endless loop and resolve it properly
end;

procedure TCocoaFieldEditor.mouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
    begin
      v := GetEditBox(Self);
      if NOT Assigned(v) then
        Exit;
      inherited mouseDown(event);
      // NSTextView runs internal mouse-tracking loop in it's mouseDown implemenation.
      // Thus "inherited mouseDown" only returns after the mouse has been released.
      // why is TCocoaTextView not affected?
      if Assigned(v) and Assigned(v.lclGetCallback) then
        v.lclGetCallback.MouseUpDownEvent(event, true);
    end;
  end else
    inherited mouseDown(event);
end;

procedure TCocoaFieldEditor.mouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited mouseUp(event);
  end else
    inherited mouseUp(event);
end;

procedure TCocoaFieldEditor.rightMouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseDown(event);
  end else
    inherited rightMouseDown(event);
end;

procedure TCocoaFieldEditor.rightMouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseUp(event);
  end else
    inherited rightMouseUp(event);
end;

procedure TCocoaFieldEditor.otherMouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseDown(event);
  end else
    inherited otherMouseDown(event);
end;

procedure TCocoaFieldEditor.otherMouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseUp(event);
  end else
    inherited otherMouseUp(event);
end;

procedure TCocoaFieldEditor.mouseDragged(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseMove(event) then
      inherited mouseDragged(event);
  end else
    inherited mouseDragged(event);
end;

procedure TCocoaFieldEditor.mouseMoved(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseMove(event) then
      inherited mouseMoved(event);
  end else
    inherited mouseMoved(event);
end;

procedure TCocoaFieldEditor.scrollWheel(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.scrollWheel(event) then
      inherited mouseMoved(event);
  end else
    inherited scrollWheel(event);
end;

{ TCocoaTextControlUtil }

class procedure TCocoaTextControlUtil.setStringValue(
  const text: NSText;
  const s: String );
var
  ns: NSString;
begin
  if Assigned(text) then
  begin
    ns := NSStringUTF8(s);
    text.setString(ns);
    ns.release;
    if Assigned(text.undoManager) then
      text.undoManager.removeAllActions;
  end;
end;

class function TCocoaTextControlUtil.getStringValue( const text: NSText ): String;
begin
  if Assigned(text) then
    Result := NSStringToString(text.string_)
  else
    Result := '';
end;

class function TCocoaTextControlUtil.setLCLFont(
  const textField: NSTextField;
  const lclFont: TFont): Boolean;
var
  cocoaFont: NSFont;
  cocoaColor: NSColor;
  tempFont: TFont;
begin
  Result:= False;

  tempFont:= TFont( lclFont.CopyFont );
  tempFont.Color:= clDefault;
  if NOT tempFont.isDefault then begin
    cocoaFont:= TCocoaFont(lclFont.Reference.Handle).Font;
    textField.setFont( cocoaFont );
    Result:= True;
  end;

  if lclFont.Color <> clDefault then begin
    cocoaColor:= TCocoaColorUtil.toColor(ColorToRGB(lclFont.Color));
    textField.setTextColor( cocoaColor );
  end;
end;

class function TCocoaTextControlUtil.setLCLFont(
  const textField: NSTextField;
  const lclControl: TObject): Boolean;
begin
  Result:= False;
  if NOT Assigned(lclControl) then
    Exit;
  if NOT (lclControl is TControl) then
    Exit;
  Result:= TCocoaTextControlUtil.setLCLFont( textField, TControl(lclControl).Font );
end;

class procedure TCocoaTextControlUtil.setWordWrap(
  const textView: NSTextView;
  const scrollView: NSScrollView;
  const wordWrap: Boolean);
var
  layoutSize: NSSize;
begin
  if wordWrap then
  begin
    layoutSize := scrollView.contentSize();
    layoutSize := NSMakeSize(layoutSize.width, CGFloat_Max);
    textView.textContainer.setContainerSize(layoutSize);
    textView.textContainer.setWidthTracksTextView(True);
    textView.setHorizontallyResizable(false);
    textView.setAutoresizingMask(NSViewWidthSizable);
    layoutSize.height:=textView.frame.size.height;
    textView.setFrameSize(layoutSize);
  end
  else
  begin
    textView.textContainer.setWidthTracksTextView(False);
    layoutSize := NSMakeSize(CGFloat_Max, CGFloat_Max);
    textView.textContainer.setContainerSize(layoutSize);
    textView.textContainer.setWidthTracksTextView(False);
    textView.setHorizontallyResizable(true);
    textView.setAutoresizingMask(0);
  end;
  textView.sizeToFit;
end;

class procedure TCocoaTextControlUtil.setAllignment(
  const textView: NSTextView;
  const align: TAlignment);
begin
  if NOT Assigned(textView) then
    Exit;
  //todo: for bidi modes, there's "NSTextAlignmentNatural"
  textView.setAlignment( TCocoaTypeUtil.toAlignment(align) );
end;

class procedure TCocoaTextControlUtil.setAllignment(
  const textField: NSTextField;
  const align: TAlignment);
begin
  if NOT Assigned(textField) then
    Exit;
  //todo: for bidi modes, there's "NSTextAlignmentNatural"
  textField.setAlignment( TCocoaTypeUtil.toAlignment(align) );
end;

class procedure TCocoaTextControlUtil.setBorderStyle(
  const textField: NSTextField;
  const borderStyle: TBorderStyle );
begin
  if not Assigned(textField) then
    Exit;
  {$ifdef BOOLFIX}
  textField.setBezeled_(Ord(borderStyle <> bsNone));
  {$else}
  textField.setBezeled(borderStyle <> bsNone);
  {$endif}
end;

class procedure TCocoaTextControlUtil.setTextHint(
  const textField: NSTextField;
  const str: String);
var
  ns : NSString;
begin
  if not Assigned(textField) then Exit;
  if str <> '' then begin
    ns := NSStringUtf8(str);
    textField.setPlaceholderString(ns);
    ns.release;
  end else begin
    textField.setPlaceholderString(nil);
  end;
end;

class procedure TCocoaTextControlUtil.setTextHint(
  const obj: NSObject;
  const str: String );
begin
  if not Assigned(obj) or not obj.isKindOfClass(NSTextField) then
    Exit;
  TCocoaTextControlUtil.setTextHint( NSTextField(obj), str );
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

function TCocoaTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextField.textDidChange(notification: NSNotification);
begin
  if (maxLength>0) and Assigned(stringValue) and (stringValue.length > maxLength) then
    setStringValue(stringValue.substringWithRange(NSMakeRange(0,maxLength)));
  TCocoaLCLMessageUtil.SendOnTextChanged(self);
end;

// detect and remove line-break when entering text
// TextField (TEdit) should be single line
function TCocoaTextField.textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL;
var
  newString: NSString;   // need not release
begin
  Result:= true;
  newString:= TCocoaStringUtil.removeLineBreak( replacementString );
  if newString.length <> replacementString.length then
  begin
    // only handled if there is line-break in replacementString
    // use insertText() for undo/redo support
    // textDidChange will be called in insertText()
    if newString.length>0 then currentEditor.insertText( newString );
    Result:= false;
  end;
end;

procedure TCocoaTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    // the text selection is handled withing mouseDown
    if Assigned(callback) and isSelectable  then
      callback.MouseUpDownEvent(event, True);
  end;
end;

procedure TCocoaTextField.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseUp(event);
  end;
end;

procedure TCocoaTextField.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTextField.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTextField.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTextField.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTextField.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTextField.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaTextField.scrollWheel(event: NSEvent);
begin
  if Assigned(callback) and not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaTextField.lclSetMaxLength(amax: integer);
begin
  maxLength := amax;
end;

{ TCocoaTextView }

procedure TCocoaTextView.changeColor(sender: id);
begin
  //preventing text color from being changed
  //inherited changeColor(sender);
end;

procedure TCocoaTextView.cut(sender: id);
var
  accept: Boolean;
begin
  accept:= TCocoaLCLMessageUtil.SendOnEditCut(self);
  if accept then
    inherited cut(sender);
end;

procedure TCocoaTextView.paste(sender: id);
var
  accept: Boolean;
begin
  accept:= TCocoaLCLMessageUtil.SendOnEditPaste(self);
  if accept then
    inherited paste(sender);
end;

procedure TCocoaTextView.dealloc;
begin
  if Assigned(FUndoManager) then
    FUndoManager.release;
  inherited dealloc;
end;

function TCocoaTextView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

function TCocoaTextView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextView.insertNewline(sender: id);
begin
  if wantReturns then
    inherited insertNewline(sender);
end;

procedure TCocoaTextView.mouseDown(event: NSEvent);
begin
  if Assigned(callback) then
  begin
    if not callback.MouseUpDownEvent(event) then
    begin
      inherited mouseDown(event);

      // Cocoa doesn't call mouseUp for NSTextView, so we have to emulate it here :(
      // See bug 29000
      if Assigned(callback) then
        callback.MouseUpDownEvent(event, True);
    end;
  end else
    inherited mouseDown(event);
end;

procedure TCocoaTextView.mouseUp(event: NSEvent);
begin
  if callback <> nil then
    callback.MouseUpDownEvent(event);
  inherited mouseUp(event);
end;

procedure TCocoaTextView.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTextView.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTextView.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTextView.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTextView.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTextView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaTextView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaTextView.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

function TCocoaTextView.lclIsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TCocoaTextView.lclSetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

procedure TCocoaTextView.textDidChange(notification: NSNotification);
begin
  if supressTextChangeEvent = 0 then
    TCocoaLCLMessageUtil.SendOnTextChanged(self);
end;

procedure TCocoaTextView.lclExpectedKeys(var wantTabs, wantArrows, wantReturn,
  wantAll: Boolean);
begin
  wantTabs := true;
  wantArrows := true;
  wantReturn := true;
  wantAll := true;
end;

function TCocoaTextView.undoManagerForTextView(view: NSTextView): NSUndoManager;
begin
  if not Assigned(FUndoManager) then
    FUndoManager := NSUndoManager.alloc.init;
  Result := FUndoManager;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

function TCocoaSecureTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSecureTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSecureTextField.textDidChange(notification: NSNotification);
begin
  inherited;
  if (maxLength>0) and Assigned(stringValue) and (stringValue.length > maxLength) then
    setStringValue(stringValue.substringWithRange(NSMakeRange(0,maxLength)));
  TCocoaLCLMessageUtil.SendOnTextChanged(self);
end;

procedure TCocoaSecureTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);

    if Assigned(callback) then
      callback.MouseUpDownEvent(event, True);
  end;
end;

procedure TCocoaSecureTextField.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSecureTextField.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSecureTextField.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSecureTextField.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSecureTextField.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSecureTextField.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSecureTextField.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSecureTextField.scrollWheel(event: NSEvent);
begin
  if Assigned(callback) and not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaSecureTextField.lclSetMaxLength(amax: integer);
begin
  MaxLength := amax;
end;

{ TCocoaSpinEdit }

{$IFDEF COCOA_SPINEDIT_INSIDE_CONTAINER}

procedure TCocoaSpinEdit.dealloc;
begin
  if Stepper <> nil then
    Stepper.release;
  if Edit <> nil then
    Edit.release;
  inherited dealloc;
end;

procedure TCocoaSpinEdit.UpdateControl(ASpinEdit: TCustomFloatSpinEdit);
begin
  Stepper.setMaxValue(ASpinEdit.MaxValue);
  Stepper.setMinValue(ASpinEdit.MinValue);
  Stepper.setIncrement(ASpinEdit.Increment);
  Stepper.setDoubleValue(ASpinEdit.Value);

  // update the UI too
  StepperChanged(Self);
end;

procedure TCocoaSpinEdit.CreateSubcontrols(ASpinEdit: TCustomFloatSpinEdit; const AParams: TCreateParams);
var
  lParams: TCreateParams;
  lEditRect, lStepperRect: TRect;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CreateSubcontrols]');
  {$ENDIF}

  Spin := ASpinEdit;
  CalculateSubcontrolPos(Types.Bounds(AParams.X, AParams.Y, AParams.Width,
    AParams.Height), lEditRect, lStepperRect);

  // Now creates the subcontrols
  lParams := AParams;
  lParams.WndParent := HWND(Self);
  lParams.Style := AParams.Style or WS_VISIBLE;

  // Stepper
  lParams.X := lStepperRect.Left;
  lParams.Y := lStepperRect.Top;
  lParams.Width := lStepperRect.Right - lStepperRect.Left;
  lParams.Height := lStepperRect.Bottom - lStepperRect.Top;
  Stepper := NSStepper.alloc.lclInitWithCreateParams(lParams);
  Stepper.setValueWraps(False);

  // Edit
  lParams.X := lEditRect.Left;
  lParams.Y := lEditRect.Top;
  lParams.Width := lEditRect.Right - lEditRect.Left;
  lParams.Height := lEditRect.Bottom - lEditRect.Top;
  Edit := NSTextField.alloc.lclInitWithCreateParams(lParams);

  // Change event for the stepper
  Stepper.setTarget(Self);
  Stepper.setAction(objcselector('StepperChanged:'));
end;

procedure TCocoaSpinEdit.PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer);
var
  lNSStepperRect, lRect: NSRect;
  lStepperRect, lEditRect: TRect;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.PositionSubcontrols] AHeight=', AHeight);
  {$ENDIF}

  CalculateSubcontrolPos(Types.Bounds(ALeft, ATop, AWidth, AHeight), lEditRect, lStepperRect);

  // Stepper
  LCLToNSRect(lStepperRect, AHeight, lNSStepperRect);
  Stepper.setBounds(lNSStepperRect);

  // Edit
  LCLToNSRect(lEditRect, AHeight, lRect);
  Edit.setBounds(lRect);

  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn(':<[TCocoaSpinEdit.PositionSubcontrols] Edit=> X=', lRect.origin.x,
    ' Y=', lRect.origin.y, ' W=', lRect.size.width, ' H=', lRect.size.height,
    ' Stepper X=', lNSStepperRect.origin.x, ' Y=', lNSStepperRect.origin.y,
    ' W=', lNSStepperRect.size.width, ' H=', lNSStepperRect.size.height,
    ' frame.size.height=', frame.size.height);
  {$ENDIF}
end;

procedure TCocoaSpinEdit.CalculateSubcontrolPos(
  const ASpinLCLBounds: TRect; out AEditBounds, AStepperBounds: TRect);
var
  lWidth, lHeight: Integer;
begin
  lWidth := ASpinLCLBounds.Right - ASpinLCLBounds.Left;
  lHeight := ASpinLCLBounds.Bottom - ASpinLCLBounds.Top;

  // Stepper
  AStepperBounds.Left := lWidth - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  AStepperBounds.Top := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AStepperBounds.Right := lWidth;
  AStepperBounds.Bottom := lHeight - SPINEDIT_EDIT_SPACING_FOR_SELECTION;

  // Edit
  AEditBounds.Left := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AEditBounds.Top := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AEditBounds.Right := lWidth - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  AEditBounds.Bottom := lHeight - SPINEDIT_EDIT_SPACING_FOR_SELECTION;

  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CalculateSubcontrolPos] lWidth=', lWidth, ' lHeight=', lHeight,
    ' Stepper.Left=', AStepperBounds.Left, ' Stepper.Top=', AStepperBounds.Top,
    ' Stepper.Right=', AStepperBounds.Right, ' Stepper.Bottom=', AStepperBounds.Bottom,
    ' Edit.Left=', AEditBounds.Left, ' Edit.Top=', AEditBounds.Top,
    ' Edit.Right=', AEditBounds.Right, ' Edit.Bottom=', AEditBounds.Bottom
    );
  {$ENDIF}
end;

procedure TCocoaSpinEdit.StepperChanged(sender: NSObject);
var
  lNSStr: NSString;
  lStr: string;
begin
  lStr := Format('%.*f', [Spin.DecimalPlaces, Stepper.doubleValue()]);
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  Edit.setStringValue(lNSStr);
  lNSStr.release;
  // This implements OnChange for both user and code changes
  if callback <> nil then callback.SendOnTextChanged();
end;

function TCocoaSpinEdit.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaSpinEdit.fittingSize: NSSize;
begin
  Result.width := -1;
  Edit.sizeToFit();
  Result.height := Edit.bounds.size.height + SPINEDIT_EDIT_SPACING_FOR_SELECTION * 2;
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.fittingSize] width=', Result.width,
    ' height=', Result.height);
  {$ENDIF}
end;

{$ELSE}

procedure TCocoaSpinEdit.dealloc;
begin
  lclReleaseSubControls;
  inherited dealloc;
end;

function TCocoaSpinEdit.updateStepper: boolean;
var
  lValid: Boolean = False;
  lValue: String;
  lFloat: Double;
begin
  lValue := CocoaUtils.NSStringToString(stringValue());
  lValid := SysUtils.TryStrToFloat(lValue, lFloat);
  if lValid then
  begin
    Stepper.setDoubleValue(lFloat);
    Result := true;
  end else
    Result := false;
end;


// * value       - the current value to be selected. Note: it will be adjusted
//                 to "min" and "max", if "checkMinMax" is set to true
// * min, max    - minimum maximum values allowed. for NSStepper, there's no "unlimited"
//                 check. "Something" should be set as minimum and maximum
// * checkMinMax - indicates if the min and max values should be verified.
//                 in LCL is Min = Max then there's not boundries check applied
//                 Since "min" and "max" are doubles, it's safer to use a booleanFlag
//                 so the comparison is done by the caller, rather than using approximation
// * inc         - incremeantal value to be used when "up" or "down" are pressed
// * DecimalPlaces - the number of decimals to used when showing the value
procedure TCocoaSpinEdit.UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer; checkMinMax: Boolean);
var
  notifyChange : Boolean;
  v : double;
begin
  Stepper.setIncrement(inc);

  v := avalue;
  if checkMinMax then
  begin
    if v < min then v := min
    else if v > max then v := max;
  end;

  notifyChange := (v <> Stepper.doubleValue) or (decimalPlaces <> ADecimalPlaces);

  // set min/max after checking for notify change
  // .doubleValue would be adjusted by setting min/max
  decimalPlaces := ADecimalPlaces;
  if checkMinMax then begin
    Stepper.setMinValue(min);
    Stepper.setMaxValue(max);
  end else begin
    // "Math" unit declared MaxDouble and MinDouble constants
    // however, using setMinValue to MinDouble, seems to be ignored
    // and Cocoa falls back to the default "0". not sure why.
    // Instead using -MaxDouble.
    Stepper.setMinValue(-MaxDouble);
    Stepper.setMaxValue(+MaxDouble);
  end;

  if notifychange then
  begin
    Stepper.setDoubleValue(v);
    StepperChanged(Self);
  end;
end;

procedure TCocoaSpinEdit.lclCreateSubcontrols(const AParams: TCreateParams);
var
  lParams: TCreateParams;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CreateSubcontrols]');
  {$ENDIF}

  // Now creates the subcontrols
  lParams := AParams;
  //lParams.Style := AParams.Style or WS_VISIBLE;

  // Stepper
  lParams.X := AParams.X + AParams.Width - CocoaConfigSpinEdit.stepperDefaultWidth;
  lParams.Width := CocoaConfigSpinEdit.stepperDefaultWidth;
  Stepper := TCocoaSpinEditStepper.alloc.lclInitWithCreateParams(lParams);
  TCocoaSpinEditStepper(Stepper).callback := callback;
  Stepper.setValueWraps(False);

  // Change event for the stepper
  Stepper.setTarget(Self);
  Stepper.setAction(objcselector('StepperChanged:'));

  { The default way to do this in Cocoa is with NSNumberFormatter
    But it is a bit annoying, it just disallows losing focus from the control
    instead of the Windows like solution to just override with the last value
    If we ever want the Cocoa behavior, instead of implementing controlTextDidChange
    do this:
  var
  lNSStr: NSString;
  lStr: string;
  i: Integer;

  NumberFormatter := NSNumberFormatter.alloc.init;
  lStr := '##0';
  if ASpinEdit.DecimalPlaces > 0 then lStr := lStr + '.';
  for i := 0 to ASpinEdit.DecimalPlaces-1 do
    lStr := lStr + '0';
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  NumberFormatter.setFormat(lNSStr);
  lNSStr.release;
  NumberFormatter.setNumberStyle(NSNumberFormatterDecimalStyle);
  setFormatter(NumberFormatter);}
end;

procedure TCocoaSpinEdit.lclReleaseSubcontrols;
begin
  if Assigned(Stepper) then
  begin
    Stepper.removeFromSuperview;
    Stepper.release;
    Stepper := nil;
  end;
  if Assigned(NumberFormatter) then
  begin
    NumberFormatter.release;
    NumberFormatter := nil;
  end;
end;

procedure TCocoaSpinEdit.PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  lclSetFrame(Types.Bounds(ALeft, ATop, AWidth, AHeight));
end;

procedure TCocoaSpinEdit.StepperChanged(sender: NSObject);
var
  lNSStr: NSString;
  lStr: string;
begin
  // Stepper not might be assigend while creating or destroying handle
  if not Assigned(Stepper) then Exit;

  lStr := Format('%.*f', [DecimalPlaces, Stepper.doubleValue()]);
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  setStringValue(lNSStr);
  lNSStr.release;
  // This implements OnChange for both user and code changes
  if avoidChangeEvent=0 then
    TCocoaLCLMessageUtil.SendOnTextChanged(self);
end;

procedure TCocoaSpinEdit.textDidChange(notification: NSNotification);
var
  w : NSWindow;
begin
  w := Self.window;
  if Assigned(w)
      and (w.firstResponder.isKindOfClass(NSTextView))
      and (NSObject(NSTextView(w.firstResponder).delegate) = self) // is focused
  then
  begin
    anyChange:=true;
    updateStepper; // just update float value, keep text as is!
    if avoidChangeEvent=0 then
      TCocoaLCLMessageUtil.SendOnTextChanged(self);
  end
  else
  begin
    // not focused
    inc(avoidChangeEvent);
    try
      updateStepper;
      StepperChanged(nil); // and refresh self
      inherited textDidChange(notification);
    finally
      dec(avoidChangeEvent);
    end;
  end;
end;

procedure TCocoaSpinEdit.textDidEndEditing(notification: NSNotification);
begin
  if anyChange then
  begin
    updateStepper;
    StepperChanged(nil); // and refresh self
    anyChange := false;
  end;
  inherited textDidEndEditing(notification);
end;

function TCocoaSpinEdit.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSpinEdit.lclSetVisible(AVisible: Boolean);
begin
  inherited lclSetVisible(AVisible);
  {$ifdef BOOLFIX}
  Stepper.setHidden_(Ord(not AVisible));
  {$else}
  Stepper.setHidden(not AVisible);
  {$endif}
end;

procedure TCocoaSpinEdit.lclSetFrame(const r: TRect);
var
  ns, lStepperNS: NSRect;
  lRect, lStepperRect: TRect;
begin
  lRect := r;
  lStepperRect := r;
  lRect.Right := lRect.Right - CocoaConfigSpinEdit.stepperDefaultWidth;
  lStepperRect.Left := lRect.Right;

  ns := TCocoaTypeUtil.toRect(lRect, superview);
  lStepperNS := TCocoaTypeUtil.toRect(lStepperRect, superview);

  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetFrame: %s Bounds=%s height=%d ns_pos=%d %d ns_size=%d %d',
    [NSStringToString(Self.ClassName), dbgs(r), Round(svHeight),
     Round(ns.origin.x), Round(ns.origin.y), Round(ns.size.width), Round(ns.size.height)]));
  {$ENDIF}
  setFrame(ns);
  Stepper.setFrame(lStepperNS);
end;

function TCocoaSpinEdit.fittingSize: NSSize;
var
  fr : NSRect;
begin
  Result.width := -1;
  fr:=frame;
  sizeToFit();
  Result.height := bounds.size.height;
  if not NSEqualRects(frame, fr) then setFrame(fr); // prevent changes of frame after sizeToFit();
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.fittingSize] width=', Result.width:0:0, ' height=', Result.height:0:0);
  {$ENDIF}
end;

function TCocoaSpinEdit.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaSpinEdit.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    if Assigned(callback) then
      callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSpinEdit.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSpinEdit.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSpinEdit.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSpinEdit.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSpinEdit.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSpinEdit.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSpinEdit.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSpinEdit.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSpinEdit.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

{$ENDIF}

end.

