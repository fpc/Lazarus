unit CocoaFullControlEdit;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  LazUTF8, Graphics, CocoaGDIObjects,
  CocoaAll, CocoaPrivate, CocoaCustomControl, CocoaUtils;

const
  IM_MESSAGE_WPARAM_GET_IME_HANDLER = 0;
  IM_MESSAGE_WPARAM_GET_LW_HANDLER  = 1;

type
  { ICocoaIMEControl }

  // IME Parameters for Cocoa Interface internal and LCL Full Control Edit
  // intentionally keep the Record type, emphasizing that it is only a simple type,
  // only used as parameters, don't put into logical functions
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
  ICocoaIMEControl = interface ['{AAD5C3AD-C8E0-20A4-E779-6F5D4F8380BD}']
    procedure IMESessionBegin;
    procedure IMESessionEnd;
    procedure IMEUpdateIntermediateText( var params: TCocoaIMEParameters );
    procedure IMEInsertFinalText( var params: TCocoaIMEParameters );
    function  IMEGetTextBound( var params: TCocoaIMEParameters ): TRect;
  end;

  { ICocoaLookupWord }

  // Lookup Word Parameters for Cocoa Interface internal and LCL Full Control Edit
  // intentionally keep the Record type, emphasizing that it is only a simple type,
  // only used as parameters, don't put into logical functions
  TCocoaLWParameters = record
    text: String;               // return line text from LCL Full Control Edit
    row: Integer;               // the row being looked up
    col: Integer;               // the column being looked up
    length: Integer;            // the length the text we want to obtain
  end;

  // the LCL Component that need Cocoa Lookup Word support need to
  // implement this simple interface
  ICocoaLookupWord = interface ['{F5B0D020-1F29-9E8C-33DD-AA122597E6A2}']
    procedure LWRowColForScreenPoint( var params: TCocoaLWParameters;
      const screenPoint: TPoint );
    procedure LWLineForRow( var params: TCocoaLWParameters );
    function  LWGetTextBound( var params: TCocoaLWParameters ): TRect;
    function  LWGetFont( var params: TCocoaLWParameters ): TFont;
  end;

  { TCocoaFullControlEdit }

  // backend of LCL Full Control Edit Component (such as SynEdit/ATSynEdit)
  // Key Class for Cocoa IME support
  // 1. obtain IME capability from Cocoa by implementing NSTextInputClientProtocol
  // 2. synchronize IME data with LCL via ICocoaIMEControl
  TCocoaFullControlEdit = objcclass(TCocoaCustomControl, NSTextInputClientProtocol)
  private
    _currentParams: TCocoaIMEParameters;
    _currentMarkedText: NSString;
  public
    imeHandler: ICocoaIMEControl;
    lwHandler: ICocoaLookupWord;
  public
    function initWithFrame(frameRect: NSRect): id; override;

    procedure keyDown(theEvent: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    function resignFirstResponder: ObjCBOOL; override;

    procedure insertText_replacementRange (aString: id; replacementRange: NSRange);
    procedure setMarkedText_selectedRange_replacementRange (aString: id; newRange: NSRange; replacementRange: NSRange);
    procedure unmarkText;
    function selectedRange: NSRange;
    function markedRange: NSRange;
    function hasMarkedText: LCLObjCBoolean;
    function firstRectForCharacterRange_actualRange ({%H-}aRange: NSRange; {%H-}actualRange: NSRangePointer): NSRect;

    function attributedSubstringForProposedRange_actualRange (aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
    function validAttributesForMarkedText: NSArray;
    function characterIndexForPoint (aPoint: NSPoint): NSUInteger;
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

function TCocoaFullControlEdit.initWithFrame(frameRect: NSRect): id;
begin
  Result:=inherited initWithFrame(frameRect);
  self.unmarkText;
end;

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


{
  the Cocoa API uses a continuous positive integer to locate the text position,
  mainly in:
  1. get the text index corresponding to the current mouse cursor through
     the return value of characterIndexForPoint()
  2. the text is obtained through the parameter aRange.location passed in by
     attributedSubstringForProposedRange_actualRange().
  note that Cocoa assumes that the index are continuous, with -1 corresponding
  to the previous character and +1 corresponding to the next character,
  which is where the trouble comes in.

  however, in the controls such as SynEdit, there is no corresponding Index.
  although the index can be calculated by traversing each line, it increases
  the workload and complexity.

  in Lookup Word, we only need to ensure that the index is continuous in a line,
  so a simplified method is used:
  1. controls such as SynEdit are indexed by row + column
  2. the index required by Cooca is a 64-bit integer
  3. so the rows and columns are encoded into 64-bit Index, with
     the high 32 bits corresponding to the rows and
     the lower 32 bits corresponding to the columns.
  4. the operation on the Index cannot cross line. if it does,
     a simple correction is required. see rangeToLWParams().
}

const
  LW_LOCATION_BASE = $1000000000000000;

function rangeToLWParams( const aRange: NSRange ): TCocoaLWParameters;
var
  location: NSUInteger;
begin
  location:= aRange.location;
  if location >= (LW_LOCATION_BASE/2) then
    location:= location - LW_LOCATION_BASE;
  Result.row:= location shr 32;
  Result.col:= location and $FFFFFFFF;
  Result.length:= aRange.length;
  if Result.col < 0 then begin
    Result.col:= 0;
    Result.row:= Result.row + 1;
  end;
end;

function LWParamsToRange( const params: TCocoaLWParameters ): NSRange;
var
  location: NSUInteger;
begin
  location:= (QWord(params.row) shl 32) + params.col;
  Result.location:= location + LW_LOCATION_BASE;
  Result.length:= params.length;
end;

// cursor tracking
function TCocoaFullControlEdit.firstRectForCharacterRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSRect;

  function getImeTextBound: TRect;
  var
    params: TCocoaIMEParameters;
  begin
    params:= _currentParams;
    setIMESelectedRange( params, _currentMarkedText, aRange );
    params.isFirstCall:= not hasMarkedText();
    Result:= imeHandler.IMEGetTextBound( params );
  end;

  function getLookupWordBound: TRect;
  var
    params: TCocoaLWParameters;
  begin
    params:= rangeToLWParams( aRange );
    Result:= lwHandler.LWGetTextBound( params );
  end;

var
  rect : TRect;
begin
  if aRange.location < LW_LOCATION_BASE then begin
    rect:= getImeTextBound;
  end else begin
    rect:= getLookupWordBound;
  end;
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

{
  1. given the previous description of not crossing lines,
     at most one line of text is returned.
  2. LW_LOCATION_BASE has been added to location to distinguish
     between IME and Lookup Word.
}
function TCocoaFullControlEdit.attributedSubstringForProposedRange_actualRange(
  aRange: NSRange; actualRange: NSRangePointer): NSAttributedString;
var
  params: TCocoaLWParameters;
  textWord: NSString;

  procedure initParams;
  begin
    params:= rangeToLWParams( aRange );
    self.lwHandler.LWLineForRow( params );
  end;

  procedure initTextWord;
  var
    lineText: NSString;
    subRange: NSRange;
  begin
    lineText:= StrToNSString( params.text );
    subRange.location:= params.col;
    subRange.length:= aRange.length;
    if subRange.location >= lineText.length then begin
      textWord:= NSSTR( ' ' );
      Exit;
    end;
    if subRange.location + subRange.length > lineText.length then
      subRange.length:= lineText.length - subRange.location;
    textWord:= lineText.substringWithRange( subRange );
  end;

  function getAttributeWord: NSAttributedString;
  var
    attribs: NSDictionary;
    lclFont: TFont;
    cocoaFont: NSFont;
  begin
    lclFont:= self.lwHandler.LWGetFont( params );
    cocoaFont:= TCocoaFont(lclFont.Reference.Handle).Font;
    attribs:= NSMutableDictionary.alloc.initWithCapacity(1);
    attribs.setValue_forKey( cocoaFont, NSFontAttributeName );
    Result:= NSAttributedString.alloc.initWithString_attributes(
              textWord, attribs );
    Result.autorelease;
    attribs.release;
  end;

  procedure setActualRange;
  begin
    if aRange.location >= (LW_LOCATION_BASE/2) then begin
      params.length:= textWord.length;
      actualRange^:= LWParamsToRange( params );
    end else begin
      actualRange^.location:= aRange.location;
      actualRange^.length:= textWord.length;
    end
  end;

begin
  Result:= nil;

  if NOT Assigned(self.lwHandler) then
    Exit;

  initParams;
  initTextWord;
  setActualRange;
  Result:= getAttributeWord;
end;

function TCocoaFullControlEdit.characterIndexForPoint(aPoint: NSPoint
  ): NSUInteger;
var
  params: TCocoaLWParameters;
  lclPoint: TPoint;
begin
  Result:= NSNotFound;
  if NOT Assigned(self.lwHandler) then
    Exit;
  lclPoint:= ScreenPointFromNSToLCL( aPoint );
  self.lwHandler.LWRowColForScreenPoint( params, lclPoint );
  if params.col >= 0 then
    Result:= LWParamsToRange(params).location;
end;

function TCocoaFullControlEdit.validAttributesForMarkedText: NSArray;
begin
  Result := nil;
end;

end.

