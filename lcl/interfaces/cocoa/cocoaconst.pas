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

implementation

end.

