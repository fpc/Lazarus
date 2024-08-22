unit CocoaFullControlEdit;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  LazUTF8,
  CocoaAll, CocoaPrivate, CocoaCustomControl, CocoaUtils;

type
  { ICocoaIMEControl }

  // IME Parameters for Cocoa Interface internal and LCL Full Control Edit
  // intentionally keep the Record type, emphasizing that it is only a simple type,
  // only used as parameters, don‘t put into logical functions
  TCocoaIMEParameters = record
    text: ShortString;           // Marked Text
    textCharLength: Integer;     // length in code point
    textByteLength: Integer;     // length in bytes
    textNSLength:   Integer;     // length in code unit (NSString)
    selectedStart:  Integer;     // selected range start in code point
    selectedLength: Integer;     // selected range length in code point
    eatAmount:      Integer;     // delete char out of Marked Text
    isFirstCall: Boolean;        // if first in the IME session
  end;

  // the LCL Component that need Cocoa IME support need to
  // implement this simple interface
  // class LazSynCocoaIMM in SynEdit Component for reference
  // class ATSynEdit_Adapter_CocoaIME in ATSynEdit Component for reference
  ICocoaIMEControl = interface
    procedure IMESessionBegin;
    procedure IMESessionEnd;
    procedure IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
    procedure IMEInsertFinalText( var params: TCocoaIMEParameters );
    function  IMEGetTextBound( var params: TCocoaIMEParameters ) : TRect;
  end;

  { TCocoaFullControlEdit }

  // backend of LCL Full Control Edit Component (such as SynEdit/ATSynEdit)
  // Key Class for Cocoa IME support
  // 1. obtain IME capability from Cocoa by implementing NSTextInputClientProtocol
  // 2. synchronize IME data with LCL via ICocoaIMEControl
  TCocoaFullControlEdit = objcclass(TCocoaCustomControl)
  private
    _currentParams: TCocoaIMEParameters;
    _currentMarkedText: NSString;
  public
    imeHandler: ICocoaIMEControl;
  public
    procedure keyDown(theEvent: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    function resignFirstResponder: ObjCBOOL; override;

    procedure setMarkedText_selectedRange_replacementRange (aString: id; newRange: NSRange; replacementRange: NSRange); override;
    procedure insertText_replacementRange (aString: id; replacementRange: NSRange); override;
    procedure unmarkText; override;
    function markedRange: NSRange; override;
    function selectedRange: NSRange; override;
    function hasMarkedText: LCLObjCBoolean; override;
    function firstRectForCharacterRange_actualRange ({%H-}aRange: NSRange; {%H-}actualRange: NSRangePointer): NSRect; override;
  end;

implementation

{ TCocoaIMEParameters }

// set text and length in params
procedure setIMEParamsText( var params: TCocoaIMEParameters; const nsText: NSString );
begin
  params.text := NSStringToString( nsText );
  params.textCharLength := UTF8Length( params.text );
  params.textByteLength := Length( params.text );
  params.textNSLength := nsText.length;
end;

// set selected range in code point
procedure setIMESelectedRange( var params: TCocoaIMEParameters; const nsText: NSString; range: NSRange );
begin
  if range.location<>NSNotFound then
  begin
    if range.location>nsText.length then
      range.location:= 0;
    if range.location+range.length>nsText.length then
      range.length:= nsText.length-range.location;
  end;

  if range.location=NSNotFound then
    params.selectedStart:= 0
  else
    params.selectedStart:= UTF8Length( nsText.substringToIndex(range.location).UTF8String );

  if range.length=0 then
    params.selectedLength:= 0
  else
    params.selectedLength:= UTF8Length( nsText.substringWithRange(range).UTF8String );
end;

{ TCocoaFullControlEdit }

{
  for IME Key Down:
  Key step for IME (such as Chinese/Japanese/Korean and DeadKeys)
  1. forward key event to NSInputContext
  2. NSInputContext will call TCocoaFullControlEdit(NSTextControlClient)
     and then call LCL via imeHandler
}
procedure TCocoaFullControlEdit.keyDown(theEvent: NSEvent);
begin
  inputContext.handleEvent(theEvent);
end;

{
  for IME Close:
  1. mouseDown() will not be called when click in the IME Popup Window,
     so it must be clicking outside the IME Popup Windows,
     which should end the IME input
  2. Cocoa had called setMarkedText_selectedRange_replacementRange()
     or insertText_replacementRange() first, then mouseDown() here
  3. NSInputContext.handleEvent() just close IME window here
  4. LCL actually handle mouse event
}
procedure TCocoaFullControlEdit.mouseDown(event: NSEvent);
begin
  inputContext.handleEvent(event);
  Inherited;
end;

procedure TCocoaFullControlEdit.mouseUp(event: NSEvent);
begin
  inputContext.handleEvent(event);
  Inherited;
end;

// prevent switch to other control when in IME input state
function TCocoaFullControlEdit.resignFirstResponder: ObjCBOOL;
begin
  Result := not hasMarkedText();
end;

function isIMEDuplicateCall( const newParams, currentParams: TCocoaIMEParameters ) : Boolean;
begin
  Result:= false;
  if newParams.isFirstCall then exit;
  if newParams.text <> currentParams.text then exit;
  if newParams.selectedStart<>currentParams.selectedStart then exit;
  if newParams.selectedLength<>currentParams.selectedLength then exit;
  Result:= true;
end;

// send Marked/Intermediate Text to LCL Edit Control which has IME Handler
// Key step for IME (such as Chinese/Japanese/Korean and DeadKeys)
procedure TCocoaFullControlEdit.setMarkedText_selectedRange_replacementRange(
  aString: id; newRange: NSRange; replacementRange: NSRange);
var
  params : TCocoaIMEParameters;
  nsText : NSString;
begin
  params.isFirstCall:= not hasMarkedText();

  // no markedText before, the first call
  if params.isFirstCall then imeHandler.IMESessionBegin;

  // get IME Intermediate Text
  nsText:= getNSStringObject( aString );
  setIMEParamsText( params, nsText );

  // some IME want to select subRange of Intermediate Text
  // such as Japanese
  setIMESelectedRange( params, nsText, newRange );

  // some IME incorrectly call setMarkedText() twice with the same parameters
  if isIMEDuplicateCall( params, _currentParams ) then
    exit;

  // some IME want to eat some chars
  // such as inputting DeadKeys
  if replacementRange.location<>NSNotFound then
    params.eatAmount:= 1 - replacementRange.location
  else
    params.eatAmount:= 0;

  // Key Step to update(display) Marked/Intermediate Text
  imeHandler.IMEUpdateIntermediateText( params );

  if params.textNSLength=0 then
  begin
    // cancel Marked/Intermediate Text
    imeHandler.IMESessionEnd;
    unmarkText;
  end
  else
  begin
    // update Marked/Intermediate Text internal status
    _currentParams:= params;
    _currentMarkedText.release;
    _currentMarkedText:= nsText;
    _currentMarkedText.retain;
  end;
end;

{
  send final Text to LCL Edit Control which has IME Handler
  Key step for IME (such as Chinese/Japanese/Korean and DeadKeys)
  1. if in IME input state, handle text via imeHandler.IMEInsertFinalText()
  2. otherwise via lclGetCallback.InputClientInsertText,
     mainly for maximum forward compatibility with TCocoaCustomControl
}
procedure TCocoaFullControlEdit.insertText_replacementRange(aString: id;
  replacementRange: NSRange);
var
  params: TCocoaIMEParameters;
  nsText : NSString;
begin
  params.isFirstCall:= not hasMarkedText();

  // IME final text
  nsText:= getNSStringObject( aString );
  setIMEParamsText( params, nsText );

  // some IME want to eat some chars, such as inputting DeadKeys
  if replacementRange.location<>NSNotFound then
    params.eatAmount:= 1 - replacementRange.location
  else
    params.eatAmount:= 0;

  if (not params.isFirstCall) or (params.eatAmount<>0) then
    // insert IME final text
    imeHandler.IMEInsertFinalText( params )
  else
    // insert normal text (without IME) by LCLControl.IntfUTF8KeyPress()
    lclGetCallback.InputClientInsertText( params.text );

  if not params.isFirstCall then
  begin
    imeHandler.IMESessionEnd;
    unmarkText;
  end;
end;

// cursor tracking
function TCocoaFullControlEdit.firstRectForCharacterRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSRect;
var
  params: TCocoaIMEParameters;
  rect : TRect;
begin
  params:= _currentParams;
  setIMESelectedRange( params, _currentMarkedText, aRange );
  params.isFirstCall:= not hasMarkedText();

  rect:= imeHandler.IMEGetTextBound( params );
  LCLToNSRect( rect, NSGlobalScreenBottom, Result );
end;

procedure TCocoaFullControlEdit.unmarkText;
begin
  setIMEParamsText( _currentParams, nil );
  _currentParams.selectedStart:= 0;
  _currentParams.selectedLength:= 0;
  _currentParams.eatAmount:= 0;
  _currentParams.isFirstCall:= true;
  _currentMarkedText.release;
  _currentMarkedText:= nil;
end;

function TCocoaFullControlEdit.markedRange: NSRange;
begin
  if _currentParams.textNSLength=0 then
    Result:= NSMakeRange( NSNotFound, 0 )
  else
    Result:= NSMakeRange( 0, _currentParams.textNSLength );
end;

function TCocoaFullControlEdit.selectedRange: NSRange;
begin
  if _currentParams.textNSLength=0 then
    Result:= NSMakeRange( 0, 0 )
  else
    Result:= NSMakeRange( _currentParams.selectedStart, _currentParams.selectedLength );
end;

function TCocoaFullControlEdit.hasMarkedText: LCLObjCBoolean;
begin
  Result:= ( _currentParams.textNSLength > 0 );
end;

end.

