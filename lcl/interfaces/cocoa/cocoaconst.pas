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

var
  NSSTR_DARK_NAME: NSString;
  NSSTR_DARK_NAME_VIBRANT: NSString;

  NSSTR_LINE_FEED: NSString;
  NSSTR_CARRIAGE_RETURN: NSString;
  NSSTR_LINE_SEPARATOR: NSString;
  NSSTR_PARAGRAPH_SEPARATOR: NSString;

  NSSTR_KEY_EQUALS: NSString;
  NSSTR_KEY_PLUS: NSString;

  NSSTR_TABCONTROL_PREV_ARROW: NSSTRING;
  NSSTR_TABCONTROL_NEXT_ARROW: NSSTRING;

implementation

const
  DarkName = 'NSAppearanceNameDarkAqua'; // used in 10.14
  DarkNameVibrant = 'NSAppearanceNameVibrantDark'; // used in 10.13

initialization
  NSSTR_DARK_NAME:= NSSTR(DarkName);
  NSSTR_DARK_NAME_VIBRANT:= NSSTR(DarkNameVibrant);

  NSSTR_LINE_FEED:= NSStr(#10);
  NSSTR_CARRIAGE_RETURN:= NSStr(#13);
  NSSTR_LINE_SEPARATOR:= NSString.alloc.initWithUTF8String(#$E2#$80#$A8);
  NSSTR_PARAGRAPH_SEPARATOR:= NSString.alloc.initWithUTF8String(#$E2#$80#$A9);

  NSSTR_KEY_EQUALS:= NSSTR('=');
  NSSTR_KEY_PLUS:= NSSTR('+');

  NSSTR_TABCONTROL_PREV_ARROW:= NSString.alloc.initWithUTF8String('◀');
  NSSTR_TABCONTROL_NEXT_ARROW:= NSString.alloc.initWithUTF8String('▶');

finalization;
  NSSTR_LINE_SEPARATOR.release;
  NSSTR_PARAGRAPH_SEPARATOR.release;

  NSSTR_TABCONTROL_PREV_ARROW.release;
  NSSTR_TABCONTROL_NEXT_ARROW.release;

end.

