{%mainunit appkit.pas}
(*
	NSColor.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSCOLOR_PAS_H}
{$define NSCOLOR_PAS_H}


//#import <Foundation/NSObject.h>
//#import <Foundation/NSGeometry.h>
//#import <AppKit/AppKitDefines.h>
//#import <AppKit/NSCell.h>
//APPKIT_EXTERN NSString *NSSystemColorsDidChangeNotification;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSColor = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLOR_PAS_C}
{$define NSCOLOR_PAS_C}


  { NSColor }

  NSColor = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    //  Create NSCalibratedWhiteColorSpace colors.
    constructor colorWithCalibratedWhite_alpha(_white: Single; _alpha: Single);
    //  Create NSCalibratedRGBColorSpace colors.
    constructor colorWithCalibratedHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
    constructor colorWithCalibratedRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
    //  Create colors in various device color spaces. In PostScript these colorspaces correspond
    // directly to the device-dependent operators setgray, sethsbcolor, setrgbcolor, and
    // setcmykcolor.
    constructor colorWithDeviceWhite_alpha(_white: Single; _alpha: Single);
    constructor colorWithDeviceHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
    constructor colorWithDeviceRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
    constructor colorWithDeviceCyan_magenta_yellow_black_alpha(_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single);
    //  Create named colors from standard color catalogs (such as Pantone).
    constructor colorWithCatalogName_colorName(_listName: CFStringRef; _colorName: CFStringRef);
    constructor colorWithColorSpace_components_count(_space: lobjc.id {NSColorSpace}; _components: Single; _numberOfComponents: Integer);
    constructor blackColor;
    //  0.0 white
    constructor darkGrayColor;
    //  0.333 white
    constructor lightGrayColor;
    //  0.667 white
    constructor whiteColor;
    //  1.0 white
    constructor grayColor;
    //  0.5 white
    constructor redColor;
    //  1.0, 0.0, 0.0 RGB
    constructor greenColor;
    //  0.0, 1.0, 0.0 RGB
    constructor blueColor;
    //  0.0, 0.0, 1.0 RGB
    constructor cyanColor;
    //  0.0, 1.0, 1.0 RGB
    constructor yellowColor;
    //  1.0, 1.0, 0.0 RGB
    constructor magentaColor;
    //  1.0, 0.0, 1.0 RGB
    constructor orangeColor;
    //  1.0, 0.5, 0.0 RGB
    constructor purpleColor;
    //  0.5, 0.0, 0.5 RGB
    constructor brownColor;
    //  0.6, 0.4, 0.2 RGB
    constructor clearColor;
    //  0.0 white, 0.0 alpha
    constructor controlShadowColor;
    //  Dark border for controls
    constructor controlDarkShadowColor;
    //  Darker border for controls
    constructor controlColor;
    //  Control face and old window background color
    constructor controlHighlightColor;
    //  Light border for controls
    constructor controlLightHighlightColor;
    //  Lighter border for controls
    constructor controlTextColor;
    //  Text on controls
    constructor controlBackgroundColor;
    //  Background of large controls (browser, tableview, clipview, ...)
    constructor selectedControlColor;
    //  Control face for selected controls
    constructor secondarySelectedControlColor;
    //  Color for selected controls when control is not active (that is, not focused)
    constructor selectedControlTextColor;
    //  Text on selected controls
    constructor disabledControlTextColor;
    //  Text on disabled controls
    constructor textColor;
    //  Document text
    constructor textBackgroundColor;
    //  Document text background
    constructor selectedTextColor;
    //  Selected document text
    constructor selectedTextBackgroundColor;
    //  Selected document text background
    constructor gridColor;
    //  Grids in controls
    constructor keyboardFocusIndicatorColor;
    //  Keyboard focus ring around controls
    constructor windowBackgroundColor;
    //  Background fill for window contents
    constructor scrollBarColor;
    //  Scroll bar slot color
    constructor knobColor;
    //  Knob face color for controls
    constructor selectedKnobColor;
    //  Knob face color for selected controls
    constructor windowFrameColor;
    //  Window frames
    constructor windowFrameTextColor;
    //  Text on window frames
    constructor selectedMenuItemColor;
    //  Highlight color for menus
    constructor selectedMenuItemTextColor;
    //  Highlight color for menu text
    constructor highlightColor;
    //  Highlight color for UI elements (this is abstract and defines the color all highlights
    // tend toward)
    constructor shadowColor;
    //  Shadow color for UI elements (this is abstract and defines the color all shadows
    // tend toward)
    constructor headerColor;
    //  Background color for header cells in Table/OutlineView
    constructor headerTextColor;
    constructor alternateSelectedControlColor;
    //  Similar to selectedControlColor; for use in lists and tables
    constructor alternateSelectedControlTextColor;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    class function controlAlternatingRowBackgroundColors: CFArrayRef;
    function highlightWithLevel(_val: Single): lobjc.id;{NSColor}
    //  val = 0 => receiver, val = 1 => highlightColor
    function shadowWithLevel(_val: Single): lobjc.id;{NSColor}
    //  val = 0 => receiver, val = 1 => shadowColor
    constructor colorForControlTint(_controlTint: NSControlTint);
    class function currentControlTint: NSControlTint;
    procedure set_;
    procedure setFill;
    procedure setStroke;
    function colorSpaceName: CFStringRef;
    //  Convert the color to another colorspace, using a colorspace name. This may return
    // nil if the specified conversion cannot be done. The abstract implementation of this
    // method returns the receiver if the specified colorspace matches that of the receiver;
    // otherwise it returns nil. Subclassers who can convert themselves to other colorspaces
    // override this method to do something better.
    // The version of this method which takes a device description allows the color to specialize
    // itself for the given device.  Device descriptions can be obtained from windows, screens,
    // and printers with the "deviceDescription" method.
    // If device is nil then the current device (as obtained from the currently lockFocus'ed
    // view's window or, if printing, the current printer) is used. The method without the
    // device: argument passes nil for the device.
    // If colorSpace is nil, then the most appropriate color space is used.
    function colorUsingColorSpaceName(_colorSpace: CFStringRef): lobjc.id;{NSColor}
    function colorUsingColorSpaceName_device(_colorSpace: CFStringRef; _deviceDescription: CFDictionaryRef): lobjc.id;{NSColor}
    function colorUsingColorSpace(_space: lobjc.id {NSColorSpace}): lobjc.id;{NSColor}
    function blendedColorWithFraction_ofColor(_fraction: Single; _color: lobjc.id {NSColor}): lobjc.id;{NSColor}
    //  Returns a color in the same color space as the receiver with the specified alpha
    // component. The abstract implementation of this method returns the receiver if alpha
    // is 1.0, otherwise it returns nil; subclassers who have explicit opacity components
    // override this method to actually return a color with the specified alpha.
    function colorWithAlphaComponent(_alpha: Single): lobjc.id;{NSColor}
    // ** Methods to get various components of colors. Not all of the methods apply to all
    // colors; if called, they raise. **
    //  Get the catalog and color name of standard colors from catalogs. These colors are
    // special colors which are usually looked up on each device by their name.
    function catalogNameComponent: CFStringRef;
    function colorNameComponent: CFStringRef;
    //  Return localized versions of the above.
    function localizedCatalogNameComponent: CFStringRef;
    function localizedColorNameComponent: CFStringRef;
    //  Get the red, green, or blue components of NSCalibratedRGB or NSDeviceRGB colors.
    function redComponent: Single;
    function greenComponent: Single;
    function blueComponent: Single;
    procedure getRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
    //  Get the components of NSCalibratedRGB or NSDeviceRGB colors as hue, saturation,
    // or brightness.
    function hueComponent: Single;
    function saturationComponent: Single;
    function brightnessComponent: Single;
    procedure getHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
    //  Get the white component of NSCalibratedWhite or NSDeviceWhite colors.
    function whiteComponent: Single;
    procedure getWhite_alpha(_white: Single; _alpha: Single);
    //  Get the CMYK components of NSDeviceCMYK colors.
    function cyanComponent: Single;
    function magentaComponent: Single;
    function yellowComponent: Single;
    function blackComponent: Single;
    procedure getCyan_magenta_yellow_black_alpha(_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single);
    function colorSpace: lobjc.id;{NSColorSpace}
    function numberOfComponents: Integer;
    procedure getComponents(_components: Single);
    function alphaComponent: Single;
    //  Pasteboard methods
    constructor colorFromPasteboard(_pasteBoard: lobjc.id {NSPasteboard});
    procedure writeToPasteboard(_pasteBoard: lobjc.id {NSPasteboard});
    //  Pattern methods. Note that colorWithPatternImage: mistakenly returns a non-autoreleased
    // color in 10.1.x and earlier. This has been fixed in (NSAppKitVersionNumber >= NSAppKitVersionNumberWithPatternColorLeakFix),
    // for apps linked post-10.1.x.
    constructor colorWithPatternImage(_image: lobjc.id {NSImage});
    function patternImage: lobjc.id;{NSImage}
    //  Draws the color and adorns it to indicate the type of color. Used by colorwells,
    // swatches, and other UI objects that need to display colors. Implementation in NSColor
    // simply draws the color (with an indication of transparency if the color isn't fully
    // opaque); subclassers can draw more stuff as they see fit.
    procedure drawSwatchInRect(_rect: NSRect);
    //  Global flag for determining whether an application supports alpha.  This flag is
    // consulted when an application imports alpha (through color dragging, for instance).
    // The value of this flag also determines whether the color panel has an opacity slider.
    // This value is YES by default, indicating that the opacity components of imported
    // colors will be set to 1.0. If an application wants alpha, it can either set the "NSIgnoreAlpha"
    // default to NO or call the set method below.
    // This method provides a global approach to removing alpha which might not always be
    // appropriate. Applications which need to import alpha sometimes should set this flag
    // to NO and explicitly make colors opaque in cases where it matters to them.
    class procedure setIgnoresAlpha(_flag: LongBool);
    class function ignoresAlpha: LongBool;
    constructor colorWithCIColor(_color: lobjc.id {CIColor});
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSColor_NSColor = 'NSColor';
  StrNSColor_colorWithCalibratedWhite_alpha = 'colorWithCalibratedWhite:alpha:';
  StrNSColor_colorWithCalibratedHue_saturation_brightness_alpha = 'colorWithCalibratedHue:saturation:brightness:alpha:';
  StrNSColor_colorWithCalibratedRed_green_blue_alpha = 'colorWithCalibratedRed:green:blue:alpha:';
  StrNSColor_colorWithDeviceWhite_alpha = 'colorWithDeviceWhite:alpha:';
  StrNSColor_colorWithDeviceHue_saturation_brightness_alpha = 'colorWithDeviceHue:saturation:brightness:alpha:';
  StrNSColor_colorWithDeviceRed_green_blue_alpha = 'colorWithDeviceRed:green:blue:alpha:';
  StrNSColor_colorWithDeviceCyan_magenta_yellow_black_alpha = 'colorWithDeviceCyan:magenta:yellow:black:alpha:';
  StrNSColor_colorWithCatalogName_colorName = 'colorWithCatalogName:colorName:';
  StrNSColor_colorWithColorSpace_components_count = 'colorWithColorSpace:components:count:';
  StrNSColor_blackColor = 'blackColor';
  StrNSColor_darkGrayColor = 'darkGrayColor';
  StrNSColor_lightGrayColor = 'lightGrayColor';
  StrNSColor_whiteColor = 'whiteColor';
  StrNSColor_grayColor = 'grayColor';
  StrNSColor_redColor = 'redColor';
  StrNSColor_greenColor = 'greenColor';
  StrNSColor_blueColor = 'blueColor';
  StrNSColor_cyanColor = 'cyanColor';
  StrNSColor_yellowColor = 'yellowColor';
  StrNSColor_magentaColor = 'magentaColor';
  StrNSColor_orangeColor = 'orangeColor';
  StrNSColor_purpleColor = 'purpleColor';
  StrNSColor_brownColor = 'brownColor';
  StrNSColor_clearColor = 'clearColor';
  StrNSColor_controlShadowColor = 'controlShadowColor';
  StrNSColor_controlDarkShadowColor = 'controlDarkShadowColor';
  StrNSColor_controlColor = 'controlColor';
  StrNSColor_controlHighlightColor = 'controlHighlightColor';
  StrNSColor_controlLightHighlightColor = 'controlLightHighlightColor';
  StrNSColor_controlTextColor = 'controlTextColor';
  StrNSColor_controlBackgroundColor = 'controlBackgroundColor';
  StrNSColor_selectedControlColor = 'selectedControlColor';
  StrNSColor_secondarySelectedControlColor = 'secondarySelectedControlColor';
  StrNSColor_selectedControlTextColor = 'selectedControlTextColor';
  StrNSColor_disabledControlTextColor = 'disabledControlTextColor';
  StrNSColor_textColor = 'textColor';
  StrNSColor_textBackgroundColor = 'textBackgroundColor';
  StrNSColor_selectedTextColor = 'selectedTextColor';
  StrNSColor_selectedTextBackgroundColor = 'selectedTextBackgroundColor';
  StrNSColor_gridColor = 'gridColor';
  StrNSColor_keyboardFocusIndicatorColor = 'keyboardFocusIndicatorColor';
  StrNSColor_windowBackgroundColor = 'windowBackgroundColor';
  StrNSColor_scrollBarColor = 'scrollBarColor';
  StrNSColor_knobColor = 'knobColor';
  StrNSColor_selectedKnobColor = 'selectedKnobColor';
  StrNSColor_windowFrameColor = 'windowFrameColor';
  StrNSColor_windowFrameTextColor = 'windowFrameTextColor';
  StrNSColor_selectedMenuItemColor = 'selectedMenuItemColor';
  StrNSColor_selectedMenuItemTextColor = 'selectedMenuItemTextColor';
  StrNSColor_highlightColor = 'highlightColor';
  StrNSColor_shadowColor = 'shadowColor';
  StrNSColor_headerColor = 'headerColor';
  StrNSColor_headerTextColor = 'headerTextColor';
  StrNSColor_alternateSelectedControlColor = 'alternateSelectedControlColor';
  StrNSColor_alternateSelectedControlTextColor = 'alternateSelectedControlTextColor';
  StrNSColor_controlAlternatingRowBackgroundColors = 'controlAlternatingRowBackgroundColors';
  StrNSColor_highlightWithLevel = 'highlightWithLevel:';
  StrNSColor_shadowWithLevel = 'shadowWithLevel:';
  StrNSColor_colorForControlTint = 'colorForControlTint:';
  StrNSColor_currentControlTint = 'currentControlTint';
  StrNSColor_set = 'set';
  StrNSColor_setFill = 'setFill';
  StrNSColor_setStroke = 'setStroke';
  StrNSColor_colorSpaceName = 'colorSpaceName';
  StrNSColor_colorUsingColorSpaceName = 'colorUsingColorSpaceName:';
  StrNSColor_colorUsingColorSpaceName_device = 'colorUsingColorSpaceName:device:';
  StrNSColor_colorUsingColorSpace = 'colorUsingColorSpace:';
  StrNSColor_blendedColorWithFraction_ofColor = 'blendedColorWithFraction:ofColor:';
  StrNSColor_colorWithAlphaComponent = 'colorWithAlphaComponent:';
  StrNSColor_catalogNameComponent = 'catalogNameComponent';
  StrNSColor_colorNameComponent = 'colorNameComponent';
  StrNSColor_localizedCatalogNameComponent = 'localizedCatalogNameComponent';
  StrNSColor_localizedColorNameComponent = 'localizedColorNameComponent';
  StrNSColor_redComponent = 'redComponent';
  StrNSColor_greenComponent = 'greenComponent';
  StrNSColor_blueComponent = 'blueComponent';
  StrNSColor_getRed_green_blue_alpha = 'getRed:green:blue:alpha:';
  StrNSColor_hueComponent = 'hueComponent';
  StrNSColor_saturationComponent = 'saturationComponent';
  StrNSColor_brightnessComponent = 'brightnessComponent';
  StrNSColor_getHue_saturation_brightness_alpha = 'getHue:saturation:brightness:alpha:';
  StrNSColor_whiteComponent = 'whiteComponent';
  StrNSColor_getWhite_alpha = 'getWhite:alpha:';
  StrNSColor_cyanComponent = 'cyanComponent';
  StrNSColor_magentaComponent = 'magentaComponent';
  StrNSColor_yellowComponent = 'yellowComponent';
  StrNSColor_blackComponent = 'blackComponent';
  StrNSColor_getCyan_magenta_yellow_black_alpha = 'getCyan:magenta:yellow:black:alpha:';
  StrNSColor_colorSpace = 'colorSpace';
  StrNSColor_numberOfComponents = 'numberOfComponents';
  StrNSColor_getComponents = 'getComponents:';
  StrNSColor_alphaComponent = 'alphaComponent';
  StrNSColor_colorFromPasteboard = 'colorFromPasteboard:';
  StrNSColor_writeToPasteboard = 'writeToPasteboard:';
  StrNSColor_colorWithPatternImage = 'colorWithPatternImage:';
  StrNSColor_patternImage = 'patternImage';
  StrNSColor_drawSwatchInRect = 'drawSwatchInRect:';
  StrNSColor_setIgnoresAlpha = 'setIgnoresAlpha:';
  StrNSColor_ignoresAlpha = 'ignoresAlpha';

  StrNSColor_colorWithCIColor = 'colorWithCIColor:';

{ NSColor }

class function NSColor.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSColor_NSColor);
end;

constructor NSColor.colorWithCalibratedWhite_alpha(_white: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_white: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithCalibratedWhite_alpha)), _white, _alpha);
end;

constructor NSColor.colorWithCalibratedHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithCalibratedHue_saturation_brightness_alpha)), _hue, _saturation, _brightness, _alpha);
end;

constructor NSColor.colorWithCalibratedRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_red: Single; _green: Single; _blue: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithCalibratedRed_green_blue_alpha)), _red, _green, _blue, _alpha);
end;

constructor NSColor.colorWithDeviceWhite_alpha(_white: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_white: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithDeviceWhite_alpha)), _white, _alpha);
end;

constructor NSColor.colorWithDeviceHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithDeviceHue_saturation_brightness_alpha)), _hue, _saturation, _brightness, _alpha);
end;

constructor NSColor.colorWithDeviceRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_red: Single; _green: Single; _blue: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithDeviceRed_green_blue_alpha)), _red, _green, _blue, _alpha);
end;

constructor NSColor.colorWithDeviceCyan_magenta_yellow_black_alpha(_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithDeviceCyan_magenta_yellow_black_alpha)), _cyan, _magenta, _yellow, _black, _alpha);
end;

constructor NSColor.colorWithCatalogName_colorName(_listName: CFStringRef; _colorName: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_listName: CFStringRef; _colorName: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithCatalogName_colorName)), _listName, _colorName);
end;

constructor NSColor.colorWithColorSpace_components_count(_space: lobjc.id {NSColorSpace}; _components: Single; _numberOfComponents: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_space: lobjc.id {NSColorSpace}; _components: Single; _numberOfComponents: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithColorSpace_components_count)), _space, _components, _numberOfComponents);
end;

constructor NSColor.blackColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_blackColor)));
end;

constructor NSColor.darkGrayColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_darkGrayColor)));
end;

constructor NSColor.lightGrayColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_lightGrayColor)));
end;

constructor NSColor.whiteColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_whiteColor)));
end;

constructor NSColor.grayColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_grayColor)));
end;

constructor NSColor.redColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_redColor)));
end;

constructor NSColor.greenColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_greenColor)));
end;

constructor NSColor.blueColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_blueColor)));
end;

constructor NSColor.cyanColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_cyanColor)));
end;

constructor NSColor.yellowColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_yellowColor)));
end;

constructor NSColor.magentaColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_magentaColor)));
end;

constructor NSColor.orangeColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_orangeColor)));
end;

constructor NSColor.purpleColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_purpleColor)));
end;

constructor NSColor.brownColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_brownColor)));
end;

constructor NSColor.clearColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_clearColor)));
end;

constructor NSColor.controlShadowColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlShadowColor)));
end;

constructor NSColor.controlDarkShadowColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlDarkShadowColor)));
end;

constructor NSColor.controlColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlColor)));
end;

constructor NSColor.controlHighlightColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlHighlightColor)));
end;

constructor NSColor.controlLightHighlightColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlLightHighlightColor)));
end;

constructor NSColor.controlTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlTextColor)));
end;

constructor NSColor.controlBackgroundColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_controlBackgroundColor)));
end;

constructor NSColor.selectedControlColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedControlColor)));
end;

constructor NSColor.secondarySelectedControlColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_secondarySelectedControlColor)));
end;

constructor NSColor.selectedControlTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedControlTextColor)));
end;

constructor NSColor.disabledControlTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_disabledControlTextColor)));
end;

constructor NSColor.textColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_textColor)));
end;

constructor NSColor.textBackgroundColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_textBackgroundColor)));
end;

constructor NSColor.selectedTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedTextColor)));
end;

constructor NSColor.selectedTextBackgroundColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedTextBackgroundColor)));
end;

constructor NSColor.gridColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_gridColor)));
end;

constructor NSColor.keyboardFocusIndicatorColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_keyboardFocusIndicatorColor)));
end;

constructor NSColor.windowBackgroundColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_windowBackgroundColor)));
end;

constructor NSColor.scrollBarColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_scrollBarColor)));
end;

constructor NSColor.knobColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_knobColor)));
end;

constructor NSColor.selectedKnobColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedKnobColor)));
end;

constructor NSColor.windowFrameColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_windowFrameColor)));
end;

constructor NSColor.windowFrameTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_windowFrameTextColor)));
end;

constructor NSColor.selectedMenuItemColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedMenuItemColor)));
end;

constructor NSColor.selectedMenuItemTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_selectedMenuItemTextColor)));
end;

constructor NSColor.highlightColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_highlightColor)));
end;

constructor NSColor.shadowColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_shadowColor)));
end;

constructor NSColor.headerColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_headerColor)));
end;

constructor NSColor.headerTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_headerTextColor)));
end;

constructor NSColor.alternateSelectedControlColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_alternateSelectedControlColor)));
end;

constructor NSColor.alternateSelectedControlTextColor;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_alternateSelectedControlTextColor)));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
class function NSColor.controlAlternatingRowBackgroundColors: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSColor_controlAlternatingRowBackgroundColors)), []));
end;

function NSColor.highlightWithLevel(_val: Single): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_val: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_highlightWithLevel)), _val));
end;

function NSColor.shadowWithLevel(_val: Single): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_val: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_shadowWithLevel)), _val));
end;

constructor NSColor.colorForControlTint(_controlTint: NSControlTint);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_controlTint: NSControlTint): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorForControlTint)), _controlTint);
end;

class function NSColor.currentControlTint: NSControlTint;
begin
  Result := NSControlTint(objc_msgSend(getClass, sel_registerName(PChar(StrNSColor_currentControlTint)), []));
end;

procedure NSColor.set_;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_set)), []);
end;

procedure NSColor.setFill;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_setFill)), []);
end;

procedure NSColor.setStroke;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_setStroke)), []);
end;

function NSColor.colorSpaceName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_colorSpaceName)), []));
end;

function NSColor.colorUsingColorSpaceName(_colorSpace: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_colorSpace: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_colorUsingColorSpaceName)), _colorSpace));
end;

function NSColor.colorUsingColorSpaceName_device(_colorSpace: CFStringRef; _deviceDescription: CFDictionaryRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_colorSpace: CFStringRef; _deviceDescription: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_colorUsingColorSpaceName_device)), _colorSpace, _deviceDescription));
end;

function NSColor.colorUsingColorSpace(_space: lobjc.id {NSColorSpace}): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_space: lobjc.id {NSColorSpace}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_colorUsingColorSpace)), _space));
end;

function NSColor.blendedColorWithFraction_ofColor(_fraction: Single; _color: lobjc.id {NSColor}): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fraction: Single; _color: lobjc.id {NSColor}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_blendedColorWithFraction_ofColor)), _fraction, _color));
end;

function NSColor.colorWithAlphaComponent(_alpha: Single): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_alpha: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSColor_colorWithAlphaComponent)), _alpha));
end;

function NSColor.catalogNameComponent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_catalogNameComponent)), []));
end;

function NSColor.colorNameComponent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_colorNameComponent)), []));
end;

function NSColor.localizedCatalogNameComponent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_localizedCatalogNameComponent)), []));
end;

function NSColor.localizedColorNameComponent: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_localizedColorNameComponent)), []));
end;

function NSColor.redComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_redComponent)), []));
end;

function NSColor.greenComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_greenComponent)), []));
end;

function NSColor.blueComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_blueComponent)), []));
end;

procedure NSColor.getRed_green_blue_alpha(_red: Single; _green: Single; _blue: Single; _alpha: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_red: Single; _green: Single; _blue: Single; _alpha: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_getRed_green_blue_alpha)), _red, _green, _blue, _alpha);
end;

function NSColor.hueComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_hueComponent)), []));
end;

function NSColor.saturationComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_saturationComponent)), []));
end;

function NSColor.brightnessComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_brightnessComponent)), []));
end;

procedure NSColor.getHue_saturation_brightness_alpha(_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_hue: Single; _saturation: Single; _brightness: Single; _alpha: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_getHue_saturation_brightness_alpha)), _hue, _saturation, _brightness, _alpha);
end;

function NSColor.whiteComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_whiteComponent)), []));
end;

procedure NSColor.getWhite_alpha(_white: Single; _alpha: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_white: Single; _alpha: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_getWhite_alpha)), _white, _alpha);
end;

function NSColor.cyanComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_cyanComponent)), []));
end;

function NSColor.magentaComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_magentaComponent)), []));
end;

function NSColor.yellowComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_yellowComponent)), []));
end;

function NSColor.blackComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_blackComponent)), []));
end;

procedure NSColor.getCyan_magenta_yellow_black_alpha(_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_cyan: Single; _magenta: Single; _yellow: Single; _black: Single; _alpha: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_getCyan_magenta_yellow_black_alpha)), _cyan, _magenta, _yellow, _black, _alpha);
end;

function NSColor.colorSpace: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_colorSpace)), []));
end;

function NSColor.numberOfComponents: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_numberOfComponents)), []));
end;

procedure NSColor.getComponents(_components: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_components: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_getComponents)), _components);
end;

function NSColor.alphaComponent: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_alphaComponent)), []));
end;

constructor NSColor.colorFromPasteboard(_pasteBoard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteBoard: lobjc.id {NSPasteboard}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorFromPasteboard)), _pasteBoard);
end;

procedure NSColor.writeToPasteboard(_pasteBoard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_pasteBoard: lobjc.id {NSPasteboard}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_writeToPasteboard)), _pasteBoard);
end;

constructor NSColor.colorWithPatternImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithPatternImage)), _image);
end;

function NSColor.patternImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSColor_patternImage)), []));
end;

procedure NSColor.drawSwatchInRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSColor_drawSwatchInRect)), _rect);
end;

class procedure NSColor.setIgnoresAlpha(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSColor_setIgnoresAlpha)), _flag);
end;

class function NSColor.ignoresAlpha: LongBool;
begin
  Result := LongBool(objc_msgSend(getClass, sel_registerName(PChar(StrNSColor_ignoresAlpha)), []));
end;

constructor NSColor.colorWithCIColor(_color: lobjc.id {CIColor});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_color: lobjc.id {CIColor}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSColor_colorWithCIColor)), _color);
end;

{$endif}
