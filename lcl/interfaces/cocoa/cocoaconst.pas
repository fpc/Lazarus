unit CocoaConst;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  SysUtils, LCLStrConsts, LCLType, LCLProc,
  CocoaAll;

type
  TNSStringArray = Array of NSString;

function BUTTON_CAPTION_ARRAY: TNSStringArray;

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

function NSSTR_ATTACHMENT_CHARACTER: NSString;

function NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
function NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;

function NSSTR_EDIT_MENU: NSSTRING;
function NSSTR_EDIT_MENU_UNDO: NSSTRING;
function NSSTR_EDIT_MENU_REDO: NSSTRING;
function NSSTR_EDIT_MENU_CUT: NSSTRING;
function NSSTR_EDIT_MENU_COPY: NSSTRING;
function NSSTR_EDIT_MENU_PASTE: NSSTRING;
function NSSTR_EDIT_MENU_SELECTALL: NSSTRING;

const
  // if the height of NSButton with NSRegularSquareBezelStyle is too small,
  // a strange rectangular color block will be shown in the Button,
  // in dark mode.
  BUTTON_MIN_HEIGHT_NSRegularSquareBezelStyle = 26;

implementation

const
  DarkName = 'NSAppearanceNameDarkAqua'; // used in 10.14
  DarkNameVibrant = 'NSAppearanceNameVibrantDark'; // used in 10.13

var
  _BUTTON_CAPTION_ARRAY: TNSStringArray;

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

  _NSSTR_ATTACHMENT_CHARACTER: NSSTRING;

  _NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
  _NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;

  _NSSTR_EDIT_MENU: NSSTRING;
  _NSSTR_EDIT_MENU_UNDO: NSSTRING;
  _NSSTR_EDIT_MENU_REDO: NSSTRING;
  _NSSTR_EDIT_MENU_CUT: NSSTRING;
  _NSSTR_EDIT_MENU_COPY: NSSTRING;
  _NSSTR_EDIT_MENU_PASTE: NSSTRING;
  _NSSTR_EDIT_MENU_SELECTALL: NSSTRING;

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

function NSSTR_ATTACHMENT_CHARACTER: NSString;
begin
  Result:= _NSSTR_ATTACHMENT_CHARACTER;
end;

function NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
begin
  Result:= _NSSTR_TABCONTROL_PREV_ARROW;
end;

function NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;
begin
  Result:= _NSSTR_TABCONTROL_NEXT_ARROW;
end;

function NSSTR_EDIT_MENU: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU) then
    _NSSTR_EDIT_MENU:= NSString.alloc.initWithUTF8String(pchar(
                         LCLStrConsts.rsMacOSEditMenu));
  Result:= _NSSTR_EDIT_MENU;
end;

function NSSTR_EDIT_MENU_UNDO: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_UNDO) then
    _NSSTR_EDIT_MENU_UNDO:= NSString.alloc.initWithUTF8String(pchar(
                              LCLStrConsts.rsMacOSEditMenuUndo));
  Result:= _NSSTR_EDIT_MENU_UNDO;
end;

function NSSTR_EDIT_MENU_REDO: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_REDO) then
    _NSSTR_EDIT_MENU_REDO:= NSString.alloc.initWithUTF8String(pchar(
                              LCLStrConsts.rsMacOSEditMenuRedo));
  Result:= _NSSTR_EDIT_MENU_REDO;
end;

function NSSTR_EDIT_MENU_CUT: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_CUT) then
    _NSSTR_EDIT_MENU_CUT:= NSString.alloc.initWithUTF8String(pchar(
                             LCLStrConsts.rsMacOSEditMenuCut));
  Result:= _NSSTR_EDIT_MENU_CUT;
end;

function NSSTR_EDIT_MENU_COPY: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_COPY) then
    _NSSTR_EDIT_MENU_COPY:= NSString.alloc.initWithUTF8String(pchar(
                             LCLStrConsts.rsMacOSEditMenuCopy));
  Result:= _NSSTR_EDIT_MENU_COPY;
end;

function NSSTR_EDIT_MENU_PASTE: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_PASTE) then
    _NSSTR_EDIT_MENU_PASTE:= NSString.alloc.initWithUTF8String(pchar(
                             LCLStrConsts.rsMacOSEditMenuPaste));
  Result:= _NSSTR_EDIT_MENU_PASTE;
end;

function NSSTR_EDIT_MENU_SELECTALL: NSSTRING;
begin
  if NOT Assigned(_NSSTR_EDIT_MENU_SELECTALL) then
    _NSSTR_EDIT_MENU_SELECTALL:= NSString.alloc.initWithUTF8String(pchar(
                             LCLStrConsts.rsMacOSEditMenuSelectAll));
  Result:= _NSSTR_EDIT_MENU_SELECTALL;
end;

function StringToNSString( const s:String ): NSString;
begin
  Result:= NSString.alloc.initWithUTF8String( pchar(s) );
end;

function StringRemoveAcceleration(const str: String): String;
var
  posAmp: Integer;
  posRight: Integer;
  posLeft: Integer;
begin
  Result:= str;
  posAmp:= DeleteAmpersands(Result);
  if posAmp < 0 then
    Exit;

  posRight:= str.IndexOf( ')' );
  if (posRight<0) or (posRight<posAmp) then
    Exit;

  posLeft:= str.IndexOf( '(' );
  if (posLeft<0) or (posLeft>posAmp) then
    Exit;

  Result:= str.Substring(0,posLeft).Trim;
end;

function LclTitleToNSString( const title:String ): NSString;
begin
  Result:= StringToNSString( StringRemoveAcceleration(title) );
end;

function BUTTON_CAPTION_ARRAY: TNSStringArray;
begin
  if length(_BUTTON_CAPTION_ARRAY)=0 then begin
    setlength( _BUTTON_CAPTION_ARRAY, idButtonNoToAll+1 );
    _BUTTON_CAPTION_ARRAY[idButtonOk]:= LclTitleToNSString( rsMbOK );
    _BUTTON_CAPTION_ARRAY[idButtonCancel]:= LclTitleToNSString( rsMbCancel );
    _BUTTON_CAPTION_ARRAY[idButtonHelp]:= LclTitleToNSString( rsMbHelp );
    _BUTTON_CAPTION_ARRAY[idButtonYes]:= LclTitleToNSString( rsMbYes );
    _BUTTON_CAPTION_ARRAY[idButtonNo]:= LclTitleToNSString( rsMbNo );
    _BUTTON_CAPTION_ARRAY[idButtonClose]:= LclTitleToNSString( rsMbClose );
    _BUTTON_CAPTION_ARRAY[idButtonAbort]:= LclTitleToNSString( rsMbAbort );
    _BUTTON_CAPTION_ARRAY[idButtonRetry]:= LclTitleToNSString( rsMbRetry );
    _BUTTON_CAPTION_ARRAY[idButtonIgnore]:= LclTitleToNSString( rsMbIgnore );
    _BUTTON_CAPTION_ARRAY[idButtonAll]:= LclTitleToNSString( rsMbAll );
    _BUTTON_CAPTION_ARRAY[idButtonYesToAll]:= LclTitleToNSString( rsMbYesToAll );
    _BUTTON_CAPTION_ARRAY[idButtonNoToAll]:= LclTitleToNSString( rsMbNoToAll );
  end;
  Result:= _BUTTON_CAPTION_ARRAY;
end;

initialization
  _NSSTR_EMPTY:= NSString.string_;

  _NSSTR_DARK_NAME:= NSSTR(DarkName);
  _NSSTR_DARK_NAME_VIBRANT:= NSSTR(DarkNameVibrant);

  _NSSTR_LINE_FEED:= NSSTR(#10);
  _NSSTR_CARRIAGE_RETURN:= NSSTR(#13);
  _NSSTR_LINE_SEPARATOR:= StringToNSString(#$E2#$80#$A8);
  _NSSTR_PARAGRAPH_SEPARATOR:= StringToNSString(#$E2#$80#$A9);

  _NSSTR_KEY_ENTER:= NSSTR(#13);
  _NSSTR_KEY_ESC:= NSSTR(#27);
  _NSSTR_KEY_EQUALS:= NSSTR('=');
  _NSSTR_KEY_PLUS:= NSSTR('+');

  _NSSTR_ATTACHMENT_CHARACTER:= StringToNSString( #$EF#$BF#$BC );

  _NSSTR_TABCONTROL_PREV_ARROW:= StringToNSString('◀');
  _NSSTR_TABCONTROL_NEXT_ARROW:= StringToNSString('▶');

finalization;
  _NSSTR_LINE_SEPARATOR.release;
  _NSSTR_PARAGRAPH_SEPARATOR.release;

  _NSSTR_ATTACHMENT_CHARACTER.release;

  _NSSTR_TABCONTROL_PREV_ARROW.release;
  _NSSTR_TABCONTROL_NEXT_ARROW.release;

  _NSSTR_EDIT_MENU.release;
  _NSSTR_EDIT_MENU_UNDO.release;
  _NSSTR_EDIT_MENU_REDO.release;
  _NSSTR_EDIT_MENU_CUT.release;
  _NSSTR_EDIT_MENU_COPY.release;
  _NSSTR_EDIT_MENU_PASTE.release;
  _NSSTR_EDIT_MENU_SELECTALL.release;

end.
