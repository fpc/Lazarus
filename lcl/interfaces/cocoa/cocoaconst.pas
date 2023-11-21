unit CocoaConst;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  CocoaAll;

const
  CocoaBasePPI : Integer = 96; // for compatiblity with LCL 1.8 release. The macOS base is 72ppi

  // if set to true, then WS would not assign icons via TCocoaWSForm SetIcon
  // The icon would have to be changed manually. By default LCL behaviour is used
  CocoaIconUse: Boolean = false;
  CocoaToggleBezel : NSBezelStyle = NSRoundedBezelStyle;
  CocoaToggleType  : NSButtonType = NSPushOnPushOffButton;

  CocoaHideFocusNoBorder : Boolean = true;

  CocoaUseLocalizedFontName : Boolean = false; // some localized named might be too long to be returned properly by APIs

  {$ifdef COCOALOOPHIJACK}
  // The flag is set to true once hi-jacked loop is finished (at the end of app)
  // The flag is checked in Menus to avoid "double" Cmd+Q menu
  LoopHiJackEnded : Boolean = false;
  {$endif}

function NSSTR_EMPTY: NSString;

function NSSTR_DARK_NAME: NSString;
function NSSTR_DARK_NAME_VIBRANT: NSString;

function NSSTR_LINE_FEED: NSString;
function NSSTR_CARRIAGE_RETURN: NSString;
function NSSTR_LINE_SEPARATOR: NSString;
function NSSTR_PARAGRAPH_SEPARATOR: NSString;

function NSSTR_KEY_ENTER: NSString;
function NSSTR_KEY_ESC: NSString;
function NSSTR_KEY_EQUALS: NSString;
function NSSTR_KEY_PLUS: NSString;

function NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
function NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;

implementation

const
  DarkName = 'NSAppearanceNameDarkAqua'; // used in 10.14
  DarkNameVibrant = 'NSAppearanceNameVibrantDark'; // used in 10.13

var
  _NSSTR_EMPTY: NSString;

  _NSSTR_DARK_NAME: NSString;
  _NSSTR_DARK_NAME_VIBRANT: NSString;

  _NSSTR_LINE_FEED: NSString;
  _NSSTR_CARRIAGE_RETURN: NSString;
  _NSSTR_LINE_SEPARATOR: NSString;
  _NSSTR_PARAGRAPH_SEPARATOR: NSString;

  _NSSTR_KEY_ENTER: NSString;
  _NSSTR_KEY_ESC: NSString;
  _NSSTR_KEY_EQUALS: NSString;
  _NSSTR_KEY_PLUS: NSString;

  _NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
  _NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;

function NSSTR_EMPTY: NSString;
begin
  Result:= _NSSTR_EMPTY;
end;

function NSSTR_DARK_NAME: NSString;
begin
  Result:= _NSSTR_DARK_NAME;
end;

function NSSTR_DARK_NAME_VIBRANT: NSString;
begin
  Result:= _NSSTR_DARK_NAME_VIBRANT;
end;


function NSSTR_LINE_FEED: NSString;
begin
  Result:= _NSSTR_LINE_FEED;
end;

function NSSTR_CARRIAGE_RETURN: NSString;
begin
  Result:= _NSSTR_CARRIAGE_RETURN;
end;

function NSSTR_LINE_SEPARATOR: NSString;
begin
  Result:= _NSSTR_LINE_SEPARATOR;
end;

function NSSTR_PARAGRAPH_SEPARATOR: NSString;
begin
  Result:= _NSSTR_PARAGRAPH_SEPARATOR;
end;


function NSSTR_KEY_ENTER: NSString;
begin
  Result:= _NSSTR_KEY_ENTER;
end;

function NSSTR_KEY_ESC: NSString;
begin
  Result:= _NSSTR_KEY_ESC;
end;

function NSSTR_KEY_EQUALS: NSString;
begin
  Result:= _NSSTR_KEY_EQUALS;
end;

function NSSTR_KEY_PLUS: NSString;
begin
  Result:= _NSSTR_KEY_PLUS;
end;


function NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
begin
  Result:= _NSSTR_TABCONTROL_PREV_ARROW;
end;

function NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;
begin
  Result:= _NSSTR_TABCONTROL_NEXT_ARROW;
end;


initialization
  _NSSTR_EMPTY:= NSString.string_;

  _NSSTR_DARK_NAME:= NSSTR(DarkName);
  _NSSTR_DARK_NAME_VIBRANT:= NSSTR(DarkNameVibrant);

  _NSSTR_LINE_FEED:= NSSTR(#10);
  _NSSTR_CARRIAGE_RETURN:= NSSTR(#13);
  _NSSTR_LINE_SEPARATOR:= NSString.alloc.initWithUTF8String(#$E2#$80#$A8);
  _NSSTR_PARAGRAPH_SEPARATOR:= NSString.alloc.initWithUTF8String(#$E2#$80#$A9);

  _NSSTR_KEY_ENTER:= NSSTR(#13);
  _NSSTR_KEY_ESC:= NSSTR(#27);
  _NSSTR_KEY_EQUALS:= NSSTR('=');
  _NSSTR_KEY_PLUS:= NSSTR('+');

  _NSSTR_TABCONTROL_PREV_ARROW:= NSString.alloc.initWithUTF8String('◀');
  _NSSTR_TABCONTROL_NEXT_ARROW:= NSString.alloc.initWithUTF8String('▶');

finalization;
  _NSSTR_LINE_SEPARATOR.release;
  _NSSTR_PARAGRAPH_SEPARATOR.release;

  _NSSTR_TABCONTROL_PREV_ARROW.release;
  _NSSTR_TABCONTROL_NEXT_ARROW.release;

end.

