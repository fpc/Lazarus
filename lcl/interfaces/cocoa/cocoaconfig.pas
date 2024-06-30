unit CocoaConfig;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  CocoaAll, Cocoa_Extra, CocoaConst;

type
  NSColorFunction = Function(): NSColor;

  function getCocoaScrollerDefaultKnobColor: NSColor;

var
  // the style of the TCocoaScrollBar managed by TCocoaManualScrollView,
  // the default value is System Preferred.
  // note: TCocoaScrollBar not managed by TCocoaManualScrollView is always
  //       Legacy Style.
  CocoaScrollerPreferredStyle : NSScrollerStyle = -1;

  // Scroller Knob Fade in/out time interval, in Seconds
  CocoaScrollerKnobFadeTimeInterval : Double = 0.02;

  // Scroller Knob Radius, in Dots
  CocoaScrollerKnobRadius : Double = 4;


  // Legacy Style Scroller Knob Color
  CocoaScrollerOverlayStyleKnobColor : NSColorFunction = @getCocoaScrollerDefaultKnobColor;

  // Legacy Style Scroller Knob Alpha in Normal
  CocoaScrollerLegacyStyleAlpha : Double = 0.25;

  // Legacy Style Scroller Knob Alpha when mouse enters
  CocoaScrollerLegacyStyleAlphaBlack : Double = 0.50;

  // Legacy Style Scroller Knob Alpha Step when fading in/out
  CocoaScrollerLegacyStyleFadeStep : Double = 0.05;

  // Legacy Style Scroller Knob Position, in Dots
  CocoaScrollerLegacyStyleKnobPos : Double = 3;

  // Legacy Style Scroller Knob Shrunk Size, in Dots
  CocoaScrollerLegacyStyleKnobShrunkSize : Double = 5;


  // Overly Style Scroller Knob Color
  CocoaScrollerLegacyStyleKnobColor : NSColorFunction = @getCocoaScrollerDefaultKnobColor;

  // Overly Style Scroller Auto Hide Delay Time, in Seconds
  CocoaScrollerOverlayStyleAutoHideDelayTime : Double = 0.9;

  // Overlay Style Scroller Alpha in Normal
  CocoaScrollerOverlayStyleAlpha : Double = 0.5;

  // Overlay Style Scroller Alpha Step when fading out
  CocoaScrollerOverlayStyleFadeStep : Double = -0.1;

  // Overlay Style Scroller Alpha when fade out ends
  CocoaScrollerOverlayStyleFadeTo : Double = 0;

  // Overlay Style Scroller expands time interval, in Seconds
  CocoaScrollerOverlayStyleExpandTimeInterval : Double = 0.03;

  // Overlay Style Scroller expands when the mouse enters, in Dots
  CocoaScrollerOverlayStyleExpandSize : Double = 4;

  // Overlay Style Scroller Knob Position, in Dots
  CocoaScrollerOverlayStyleKnobPos : Double = 5;

  // Overlay Style Scroller Knob Shrunk Size, in Dots
  CocoaScrollerOverlayStyleKnobShrunkSize : Double = 6;

  // in extreme cases, the normally calculated Knob size of Overlay Scroller
  // may be too small, keep the min size.
  // min height for the Knob of VerticalScroller
  // min width for the Knob of HorizontalScroller
  CocoaScrollerOverlayStyleKnobMinSize : Double = 25;


  // for compatiblity with LCL 1.8 release. The macOS base is 72ppi
  CocoaBasePPI : Integer = 96;

  // if set to true, then WS would not assign icons via TCocoaWSForm SetIcon
  // The icon would have to be changed manually. By default LCL behaviour is used
  CocoaIconUse: Boolean = false;
  CocoaToggleBezel : NSBezelStyle = NSRoundedBezelStyle;
  CocoaToggleType  : NSButtonType = NSPushOnPushOffButton;

  CocoaHideFocusNoBorder : Boolean = true;

  // some localized named might be too long to be returned properly by APIs
  CocoaUseLocalizedFontName : Boolean = false;

  // default NSTableViewStyle
  CocoaTableViewStyle : NSTableViewStyle = NSTableViewStyleAutomatic;

  // default Image Name for MenuItem
  CocoaDefaultCheckMenuImageName : NSString;
  CocoaDefaultRadioMenuImageName : NSString;

  {$ifdef COCOALOOPHIJACK}
  // The flag is set to true once hi-jacked loop is finished (at the end of app)
  // The flag is checked in Menus to avoid "double" Cmd+Q menu
  LoopHiJackEnded : Boolean = false;
  {$endif}

implementation

function getCocoaScrollerDefaultKnobColor: NSColor;
begin
  Result:= NSColor.controlTextColor;
end;

initialization
  CocoaDefaultCheckMenuImageName:= NSSTR('NSMenuCheckmark');
  CocoaDefaultRadioMenuImageName:= NSSTR('NSDatePickerCalendarHome');

end.

