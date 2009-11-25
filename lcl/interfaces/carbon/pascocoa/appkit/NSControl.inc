{%mainunit appkit.pas}
(*
	NSControl.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSCONTROL_PAS_H}
{$define NSCONTROL_PAS_H}

{$include NSView.inc}
{$include NSText.inc}

//APPKIT_EXTERN NSString *NSControlTextDidBeginEditingNotification;	//	@"NSFieldEditor"
//APPKIT_EXTERN NSString *NSControlTextDidEndEditingNotification;	//	@"NSFieldEditor"
//APPKIT_EXTERN NSString *NSControlTextDidChangeNotification;		//	@"NSFieldEditor"
{$endif}
{$endif}

{$ifdef FORWARD}
  NSControl = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCONTROL_PAS_C}
{$define NSCONTROL_PAS_C}

{$include NSView.inc}
{$include NSText.inc}

  { NSControl }

  NSControl = class(NSView)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    class procedure setCellClass(_factoryId: _Class);
    class function cellClass: _Class;
    constructor initWithFrame(_frameRect: NSRect);
    procedure sizeToFit;
    procedure calcSize;
    function cell: lobjc.id;
    procedure setCell(_aCell: lobjc.id {NSCell});
    function selectedCell: lobjc.id;
    function target: lobjc.id;
    procedure setTarget(_anObject: lobjc.id);
    function action: SEL;
    procedure setAction(_aSelector: SEL);
    function tag: Integer;
    procedure setTag(_anInt: Integer);
    function selectedTag: Integer;
    procedure setIgnoresMultiClick(_flag: LongBool);
    function ignoresMultiClick: LongBool;
    function sendActionOn(_mask: Integer): Integer;
    function isContinuous: LongBool;
    procedure setContinuous(_flag: LongBool);
    function isEnabled: LongBool;
    procedure setEnabled(_flag: LongBool);
    procedure setFloatingPointFormat_left_right(_autoRange: LongBool; _leftDigits: LongWord; _rightDigits: LongWord);
    function alignment: NSTextAlignment;
    procedure setAlignment(_mode: NSTextAlignment);
    function font: lobjc.id;{NSFont}
    procedure setFont(_fontObj: lobjc.id {NSFont});
    procedure setFormatter(_newFormatter: lobjc.id {NSFormatter});
    function formatter: lobjc.id;
    procedure setObjectValue(_obj: lobjc.id);
    procedure setStringValue(_aString: CFStringRef);
    procedure setIntValue(_anInt: Integer);
    procedure setFloatValue(_aFloat: Single);
    procedure setDoubleValue(_aDouble: double);
    function objectValue: lobjc.id;
    function stringValue: CFStringRef;
    function intValue: Integer;
    function floatValue: Single;
    function doubleValue: double;
    procedure setNeedsDisplay;
    procedure updateCell(_aCell: lobjc.id {NSCell});
    procedure updateCellInside(_aCell: lobjc.id {NSCell});
    procedure drawCellInside(_aCell: lobjc.id {NSCell});
    procedure drawCell(_aCell: lobjc.id {NSCell});
    procedure selectCell(_aCell: lobjc.id {NSCell});
    function sendAction_to(_theAction: SEL; _theTarget: lobjc.id): LongBool;
    procedure takeIntValueFrom(_sender: lobjc.id);
    procedure takeFloatValueFrom(_sender: lobjc.id);
    procedure takeDoubleValueFrom(_sender: lobjc.id);
    procedure takeStringValueFrom(_sender: lobjc.id);
    procedure takeObjectValueFrom(_sender: lobjc.id);
    function currentEditor: lobjc.id;{NSText}
    function abortEditing: LongBool;
    procedure validateEditing;
    procedure mouseDown(_theEvent: lobjc.id {NSEvent});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function baseWritingDirection: NSWritingDirection;
    procedure setBaseWritingDirection(_writingDirection: NSWritingDirection);
    {.$endif}
    procedure performClick;
    procedure setRefusesFirstResponder(_flag: LongBool);
    function refusesFirstResponder: LongBool;
    function attributedStringValue: lobjc.id;{NSAttributedString}
    procedure setAttributedStringValue(_obj: lobjc.id {NSAttributedString});
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSControl_NSControl = 'NSControl';
  StrNSControl_setCellClass = 'setCellClass:';
  StrNSControl_cellClass = 'cellClass';
  StrNSControl_initWithFrame = 'initWithFrame:';
  StrNSControl_sizeToFit = 'sizeToFit';
  StrNSControl_calcSize = 'calcSize';
  StrNSControl_cell = 'cell';
  StrNSControl_setCell = 'setCell:';
  StrNSControl_selectedCell = 'selectedCell';
  StrNSControl_target = 'target';
  StrNSControl_setTarget = 'setTarget:';
  StrNSControl_action = 'action';
  StrNSControl_setAction = 'setAction:';
  StrNSControl_tag = 'tag';
  StrNSControl_setTag = 'setTag:';
  StrNSControl_selectedTag = 'selectedTag';
  StrNSControl_setIgnoresMultiClick = 'setIgnoresMultiClick:';
  StrNSControl_ignoresMultiClick = 'ignoresMultiClick';
  StrNSControl_sendActionOn = 'sendActionOn:';
  StrNSControl_isContinuous = 'isContinuous';
  StrNSControl_setContinuous = 'setContinuous:';
  StrNSControl_isEnabled = 'isEnabled';
  StrNSControl_setEnabled = 'setEnabled:';
  StrNSControl_setFloatingPointFormat_left_right = 'setFloatingPointFormat:left:right:';
  StrNSControl_alignment = 'alignment';
  StrNSControl_setAlignment = 'setAlignment:';
  StrNSControl_font = 'font';
  StrNSControl_setFont = 'setFont:';
  StrNSControl_setFormatter = 'setFormatter:';
  StrNSControl_formatter = 'formatter';
  StrNSControl_setObjectValue = 'setObjectValue:';
  StrNSControl_setStringValue = 'setStringValue:';
  StrNSControl_setIntValue = 'setIntValue:';
  StrNSControl_setFloatValue = 'setFloatValue:';
  StrNSControl_setDoubleValue = 'setDoubleValue:';
  StrNSControl_objectValue = 'objectValue';
  StrNSControl_stringValue = 'stringValue';
  StrNSControl_intValue = 'intValue';
  StrNSControl_floatValue = 'floatValue';
  StrNSControl_doubleValue = 'doubleValue';
  StrNSControl_setNeedsDisplay = 'setNeedsDisplay';
  StrNSControl_updateCell = 'updateCell:';
  StrNSControl_updateCellInside = 'updateCellInside:';
  StrNSControl_drawCellInside = 'drawCellInside:';
  StrNSControl_drawCell = 'drawCell:';
  StrNSControl_selectCell = 'selectCell:';
  StrNSControl_sendAction_to = 'sendAction:to:';
  StrNSControl_takeIntValueFrom = 'takeIntValueFrom:';
  StrNSControl_takeFloatValueFrom = 'takeFloatValueFrom:';
  StrNSControl_takeDoubleValueFrom = 'takeDoubleValueFrom:';
  StrNSControl_takeStringValueFrom = 'takeStringValueFrom:';
  StrNSControl_takeObjectValueFrom = 'takeObjectValueFrom:';
  StrNSControl_currentEditor = 'currentEditor';
  StrNSControl_abortEditing = 'abortEditing';
  StrNSControl_validateEditing = 'validateEditing';
  StrNSControl_mouseDown = 'mouseDown:';
  StrNSControl_baseWritingDirection = 'baseWritingDirection';
  StrNSControl_setBaseWritingDirection = 'setBaseWritingDirection:';

  StrNSControl_performClick = 'performClick';
  StrNSControl_setRefusesFirstResponder = 'setRefusesFirstResponder:';
  StrNSControl_refusesFirstResponder = 'refusesFirstResponder';

  StrNSControl_attributedStringValue = 'attributedStringValue';
  StrNSControl_setAttributedStringValue = 'setAttributedStringValue:';

{ NSControl }

class function NSControl.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSControl_NSControl);
end;

class procedure NSControl.setCellClass(_factoryId: _Class);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_factoryId: _Class); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSControl_setCellClass)), _factoryId);
end;

class function NSControl.cellClass: _Class;
begin
  Result := _Class(objc_msgSend(getClass, sel_registerName(PChar(StrNSControl_cellClass)), []));
end;

constructor NSControl.initWithFrame(_frameRect: NSRect);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_frameRect: NSRect): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSControl_initWithFrame)), _frameRect);
end;

procedure NSControl.sizeToFit;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_sizeToFit)), []);
end;

procedure NSControl.calcSize;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_calcSize)), []);
end;

function NSControl.cell: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_cell)), []));
end;

procedure NSControl.setCell(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setCell)), _aCell);
end;

function NSControl.selectedCell: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_selectedCell)), []));
end;

function NSControl.target: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_target)), []));
end;

procedure NSControl.setTarget(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setTarget)), _anObject);
end;

function NSControl.action: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_action)), []));
end;

procedure NSControl.setAction(_aSelector: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSelector: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setAction)), _aSelector);
end;

function NSControl.tag: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_tag)), []));
end;

procedure NSControl.setTag(_anInt: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setTag)), _anInt);
end;

function NSControl.selectedTag: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_selectedTag)), []));
end;

procedure NSControl.setIgnoresMultiClick(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setIgnoresMultiClick)), _flag);
end;

function NSControl.ignoresMultiClick: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_ignoresMultiClick)), []));
end;

function NSControl.sendActionOn(_mask: Integer): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_mask: Integer): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSControl_sendActionOn)), _mask));
end;

function NSControl.isContinuous: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_isContinuous)), []));
end;

procedure NSControl.setContinuous(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setContinuous)), _flag);
end;

function NSControl.isEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_isEnabled)), []));
end;

procedure NSControl.setEnabled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setEnabled)), _flag);
end;

procedure NSControl.setFloatingPointFormat_left_right(_autoRange: LongBool; _leftDigits: LongWord; _rightDigits: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_autoRange: LongBool; _leftDigits: LongWord; _rightDigits: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setFloatingPointFormat_left_right)), _autoRange, _leftDigits, _rightDigits);
end;

function NSControl.alignment: NSTextAlignment;
begin
  Result := NSTextAlignment(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_alignment)), []));
end;

procedure NSControl.setAlignment(_mode: NSTextAlignment);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mode: NSTextAlignment); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setAlignment)), _mode);
end;

function NSControl.font: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_font)), []));
end;

procedure NSControl.setFont(_fontObj: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setFont)), _fontObj);
end;

procedure NSControl.setFormatter(_newFormatter: lobjc.id {NSFormatter});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newFormatter: lobjc.id {NSFormatter}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setFormatter)), _newFormatter);
end;

function NSControl.formatter: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_formatter)), []));
end;

procedure NSControl.setObjectValue(_obj: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_obj: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setObjectValue)), _obj);
end;

procedure NSControl.setStringValue(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setStringValue)), _aString);
end;

procedure NSControl.setIntValue(_anInt: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setIntValue)), _anInt);
end;

procedure NSControl.setFloatValue(_aFloat: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aFloat: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setFloatValue)), _aFloat);
end;

procedure NSControl.setDoubleValue(_aDouble: double);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aDouble: double); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setDoubleValue)), _aDouble);
end;

function NSControl.objectValue: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_objectValue)), []));
end;

function NSControl.stringValue: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_stringValue)), []));
end;

function NSControl.intValue: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_intValue)), []));
end;

function NSControl.floatValue: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_floatValue)), []));
end;

function NSControl.doubleValue: double;
begin
  Result := objc_msgSend_fpret(Handle, sel_registerName(PChar(StrNSControl_doubleValue)), []);
end;

procedure NSControl.setNeedsDisplay;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_setNeedsDisplay)), []);
end;

procedure NSControl.updateCell(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_updateCell)), _aCell);
end;

procedure NSControl.updateCellInside(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_updateCellInside)), _aCell);
end;

procedure NSControl.drawCellInside(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_drawCellInside)), _aCell);
end;

procedure NSControl.drawCell(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_drawCell)), _aCell);
end;

procedure NSControl.selectCell(_aCell: lobjc.id {NSCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aCell: lobjc.id {NSCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_selectCell)), _aCell);
end;

function NSControl.sendAction_to(_theAction: SEL; _theTarget: lobjc.id): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theAction: SEL; _theTarget: lobjc.id): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSControl_sendAction_to)), _theAction, _theTarget));
end;

procedure NSControl.takeIntValueFrom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_takeIntValueFrom)), _sender);
end;

procedure NSControl.takeFloatValueFrom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_takeFloatValueFrom)), _sender);
end;

procedure NSControl.takeDoubleValueFrom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_takeDoubleValueFrom)), _sender);
end;

procedure NSControl.takeStringValueFrom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_takeStringValueFrom)), _sender);
end;

procedure NSControl.takeObjectValueFrom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_takeObjectValueFrom)), _sender);
end;

function NSControl.currentEditor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_currentEditor)), []));
end;

function NSControl.abortEditing: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_abortEditing)), []));
end;

procedure NSControl.validateEditing;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_validateEditing)), []);
end;

procedure NSControl.mouseDown(_theEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_mouseDown)), _theEvent);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSControl.baseWritingDirection: NSWritingDirection;
begin
  Result := NSWritingDirection(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_baseWritingDirection)), []));
end;

procedure NSControl.setBaseWritingDirection(_writingDirection: NSWritingDirection);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_writingDirection: NSWritingDirection); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setBaseWritingDirection)), _writingDirection);
end;

{.$endif}
procedure NSControl.performClick;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_performClick)), []);
end;

procedure NSControl.setRefusesFirstResponder(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setRefusesFirstResponder)), _flag);
end;

function NSControl.refusesFirstResponder: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_refusesFirstResponder)), []));
end;

function NSControl.attributedStringValue: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSControl_attributedStringValue)), []));
end;

procedure NSControl.setAttributedStringValue(_obj: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_obj: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSControl_setAttributedStringValue)), _obj);
end;

{$endif}
