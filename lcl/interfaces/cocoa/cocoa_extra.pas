{ $Id: $}
{                  --------------------------------------------
                  cocoa_extra.pp  -  Cocoa headers not available in FPC
                  --------------------------------------------

 This unit contains Cocoa headers which are not yet available in the latest FPC release

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Cocoa_Extra;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll;

{$if (FPC_VERSION>3) or ((FPC_VERSION=3) and (FPC_RELEASE>=2))}
{$define HASBOOLEAN8}
{$endif}

type
  // Due to backwards incompatible changes in FPC sources
  // (switching from Boolean to Boolean8), LCL has to adopt
  // either type, depending on FPC version
  LCLObjCBoolean = {$ifdef HASBOOLEAN8}
                   Boolean8  // FPC 3.2.0 and earlier are using "boolean8" type
                   {$else}
                   Boolean   // FPC 3.0.4 and earlier are using "boolean" type
                   {$endif};

type
  NSImageScaling = NSUInteger;
const // NSImageScaling values
  NSImageScaleProportionallyDown = 0;
  NSImageScaleAxesIndependently = 1;
  NSImageScaleNone = 2;
  NSImageScaleProportionallyUpOrDown = 3;

type
  NSMenuFix = objccategory external (NSMenu)
    function itemAtIndex(index: NSInteger): NSMenuItem; message 'itemAtIndex:';
  end;

  {$ifdef BOOLFIX}
  ObjCBool = ShortInt; // Matches BOOL declaration in ObjC "signed char"
                       // Note that this is different than LCLObjCBoolean
                       // even though it's trying to resolve the same problem
                       // for FPC3.0.4. ObjCBool should be removed after the officail
                       // fpc3.2+ release

  NSMenuItemFix = objccategory external (NSMenuItem)
    procedure setEnabled_(aenabled: ObjCBool); message 'setEnabled:';
    procedure setHidden_(ahidden: ObjCBool); message 'setHidden:';
  end;

  NSControlFix = objccategory external (NSControl)
    procedure setEnabled_(aenabled: ObjCBool); message 'setEnabled:';
  end;

  NSStatusItemFix = objccategory external (NSStatusItem)
    procedure setEnabled_(aenabled: ObjCBool); message 'setEnabled:';
  end;

{$if FPC_FULLVERSION < 30301}
  NSAppearance = objcclass external(NSObject)
    function name: NSString; message 'name';
    class function currentAppearance: NSAppearance; message 'currentAppearance';
  end;
{$endif}

  NSApplicationFix = objccategory external (NSApplication)
    procedure activateIgnoringOtherApps_(flag: ObjCBool); message 'activateIgnoringOtherApps:';
    function nextEventMatchingMask_untilDate_inMode_dequeue_(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: ObjCBool): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure postEvent_atStart_(event: NSEvent; flag: ObjCBool); message 'postEvent:atStart:';

    function appearance: NSAppearance; message 'appearance'; // 10.14 (10.13)
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance'; // 10.14 (10.13)
  end;

  NSButtonFix = objccategory external(NSButton)
    procedure setBordered_(flag: ObjCBool); message 'setBordered:';
    procedure setAllowsMixedState_(flag: ObjCBool); message 'setAllowsMixedState:';
  end;

  NSTextFieldFix = objccategory external(NSTextField)
    procedure setDrawsBackground_(flag: ObjCBool); message 'setDrawsBackground:';
    procedure setBordered_(flag: ObjCBool); message 'setBordered:';
    procedure setBezeled_(flag: ObjCBool); message 'setBezeled:';
    procedure setEditable_(flag: ObjCBool); message 'setEditable:';
    procedure setSelectable_(flag: ObjCBool); message 'setSelectable:';
  end;
  {$endif}

  NSEdgeInsets = packed record
    top     : CGFloat;
    left    : CGFloat;
    bottom  : CGFloat;
    right   : CGFloat;
  end;

  NSViewFix = objccategory external (NSView)
    // 10.7+
    function fittingSize: NSSize; message 'fittingSize';
    function alignmentRectInsets: NSEdgeInsets; message 'alignmentRectInsets';
    function alignmentRectForFrame(ns: NSRect): NSRect; message 'alignmentRectForFrame:';
    function frameForAlignmentRect(ns: NSRect): NSRect; message 'frameForAlignmentRect:';
    {$ifdef BOOLFIX}
    procedure setHidden_(flag: ObjCBool); message 'setHidden:';
    procedure setAutoresizesSubviews_(flag: ObjCBool); message 'setAutoresizesSubviews:';
    procedure setNeedsDisplay__(flag: ObjCBool); message 'setNeedsDisplay:';
    {$endif}
  end;

  NSLayoutConstraint = objcclass external (NSObject)
    function isActive: Boolean; message 'isActive';
    procedure setActive(Active: Boolean); message 'setActive:';
  end;

  NSButtonSoundExtensionsCategory = objccategory external (NSButton)
    function intrinsicContentSize(): NSSize; message 'intrinsicContentSize';
    procedure setImageScaling(aScaling: NSImageScaling); message 'setImageScaling:';
  end;

  // The following dummy categories fix bugs in the Cocoa bindings available in FPC
  // Remove them when the FPC binding parser is fixed.
  // More details:
  // http://wiki.freepascal.org/FPC_PasCocoa/Differences#Sending_messages_to_id
  // http://wiki.lazarus.freepascal.org/FPC_PasCocoa#Category_declaration
  NSBitmapImageRepFix = objccategory external(NSBitmapImageRep)
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bytesPerRow_bitsPerPixel(planes: PPByte; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(planes: PPByte; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; bitmapFormat_: NSBitmapFormat; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
  end;

  NSGraphicsContextFix = objccategory external(NSGraphicsContext)
{$if FPC_FULLVERSION < 30300}
    class procedure classSaveGraphicsState; message 'saveGraphicsState';
    class procedure classRestoreGraphicsState; message 'restoreGraphicsState';
    procedure instanceSaveGraphicsState; message 'saveGraphicsState';
    procedure instanceRestoreGraphicsState; message 'restoreGraphicsState';
{$endif}
    procedure setImageInterpolation(interpolation: NSImageInterpolation); message 'setImageInterpolation:';
    procedure setShouldAntialias(antialias: Boolean); message 'setShouldAntialias:';
    // 10.10
    function CGContext: CGContextRef; message 'CGContext';
  end;

  NSEventFix = objccategory external (NSEvent)
    class function modifierFlags_: NSUInteger; message 'modifierFlags';
    // available in 10.7+
    function hasPreciseScrollingDeltas: LCLObjCBoolean; message 'hasPreciseScrollingDeltas';
    function scrollingDeltaX: CGFloat; message 'scrollingDeltaX';
    function scrollingDeltaY: CGFloat; message 'scrollingDeltaY';
  end;

  NSWindowTabbingMode = NSInteger;

  NSWindowFix = objccategory external (NSWindow)
    // 10.4-10.7
    // userSpaceScaleFactor is declare in the latest CocoaAll
    //function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor'; //deprecated
    // 10.7+
    procedure toggleFullScreen(sender: id); message 'toggleFullScreen:';
    function backingScaleFactor: CGFloat; message 'backingScaleFactor';
    // 10.12
    procedure setTabbingMode(amode: NSWindowTabbingMode); message 'setTabbingMode:';
    function tabbingMode: NSWindowTabbingMode; message 'tabbingMode';
    class procedure setAllowsAutomaticWindowTabbing(aflag: Boolean); message 'setAllowsAutomaticWindowTabbing:';
    class function allowsAutomaticWindowTabbing: Boolean; message 'allowsAutomaticWindowTabbing';
    {$ifdef BOOLFIX}
    function initWithContentRect_styleMask_backing_defer_(contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBool): id; message 'initWithContentRect:styleMask:backing:defer:';
    procedure setFrame_display_(frameRect: NSRect; flag: ObjCBool); message 'setFrame:display:';
    function fieldEditor_forObject_(createFlag: ObjCBool; anObject: id): NSText; message 'fieldEditor:forObject:';
    procedure setReleasedWhenClosed_(flag: ObjCBool); message 'setReleasedWhenClosed:';
    procedure setAcceptsMouseMovedEvents_(flag: ObjCBool); message 'setAcceptsMouseMovedEvents:';
    procedure setHidesOnDeactivate_(flag: ObjCBool); message 'setHidesOnDeactivate:';
    procedure setHasShadow_(hasShadow_: ObjCBool); message 'setHasShadow:';
    procedure setIgnoresMouseEvents_(flag: ObjCBool); message 'setIgnoresMouseEvents:';
    {$endif}
    // 10.14
    function appearance: NSAppearance; message 'appearance'; // 10.14 (10.13)
  end;

  NSTableColumnFix = objccategory external (NSTableColumn)
    procedure setTitle(atitle: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    {$ifdef BOOLFIX}
    procedure setHidden_(flag: ObjCBool); message 'setHidden:';
    {$endif}
  end;

  NSUserInterfaceItemIdentifier = NSString;

  NSTableViewAnimationOptions = NSUInteger;

  NSTableViewFix = objccategory external (NSTableView)
    // 10.7
    function rowForView(AView: NSView): NSInteger; message 'rowForView:';
    function columnForView(AView: NSView): NSInteger; message 'columnForView:';
    function makeViewWithIdentifier_owner(identifier_: NSUserInterfaceItemIdentifier; owner: id): NSView ; message 'makeViewWithIdentifier:owner:';
    function viewAtColumn_row_makeIfNecessary(column, row: NSInteger; makeifNecessary: Boolean): NSview; message 'viewAtColumn:row:makeIfNecessary:';
    procedure insertRowsAtIndexes_withAnimation(indexes: NSIndexSet; withAnimation: NSTableViewAnimationOptions);
      message 'insertRowsAtIndexes:withAnimation:';
    procedure removeRowsAtIndexes_withAnimation(indexes: NSIndexSet; withAnimation: NSTableViewAnimationOptions);
      message 'removeRowsAtIndexes:withAnimation:';
  end;

  {// private since 10.5, doesn't seam to do anything in 10.10
  NSApplicationSetAppleMenu = objccategory external(NSApplication)
    procedure setAppleMenu(AMenu: NSMenu); message 'setAppleMenu:';
  end;}

  NSOperatingSystemVersion = record
    majorVersion: NSInteger;
    minorVersion: NSInteger;
    patchVersion: NSInteger;
  end;

const
  // defined in NSApplication.h
  NSAppKitVersionNumber10_5 = 949;
  NSAppKitVersionNumber10_6 = 1038;
  NSAppKitVersionNumber10_7 = 1138;
  NSAppKitVersionNumber10_8 = 1187;
  NSAppKitVersionNumber10_9 = 1265;
  NSAppKitVersionNumber10_10 = 1343;
  NSAppKitVersionNumber10_11 = 1404;
  NSAppKitVersionNumber10_12 = 1504;
  NSAppKitVersionNumber10_13 = 1561;
  //NSAppKitVersionNumber10_14 = 1641.10; // Mojave's beta?
  NSAppKitVersionNumber10_14 = 1671;



function NSNormalWindowLevel: NSInteger; inline;
function NSFloatingWindowLevel: NSInteger; inline;
function NSSubmenuWindowLevel: NSInteger; inline;
function NSTornOffMenuWindowLevel: NSInteger; inline;
function NSMainMenuWindowLevel: NSInteger; inline;
function NSStatusWindowLevel: NSInteger; inline;
function NSModalPanelWindowLevel: NSInteger; inline;
function NSPopUpMenuWindowLevel: NSInteger; inline;
function NSScreenSaverWindowLevel: NSInteger; inline;
  //kCGScreenSaverWindowLevelKey = 13;
  //kCGMaximumWindowLevelKey = 14;
  //kCGOverlayWindowLevelKey = 15;
  //kCGHelpWindowLevelKey = 16;
  //kCGUtilityWindowLevelKey = 17;
  //kCGDesktopIconWindowLevelKey = 18;
  //kCGCursorWindowLevelKey = 19;
  //kCGAssistiveTechHighWindowLevelKey = 20;
  //kCGNumberOfWindowLevelKeys = 21;	{ Must be last. }

const
  NSWindowCollectionBehaviorFullScreenPrimary = 1 shl 7;  // 10.7
  NSWindowCollectionBehaviorFullScreenAuxiliary = 1 shl 8;  // 10.7
  NSWindowCollectionBehaviorFullScreenAllowsTiling = 1 shl 11; // 10.11
  NSWindowCollectionBehaviorFullScreenDisallowsTiling = 1 shl 12; // 10.11

  NSWindowTabbingModeAutomatic  = 0; // The system automatically prefers to tab this window when appropriate
  NSWindowTabbingModePreferred  = 1; // The window explicitly should prefer to tab when shown
  NSWindowTabbingModeDisallowed = 2; // The window explicitly should not prefer to tab when shown

const
  NSKeyCodeTab  = 48;

{ NSTableView Animation Options }

const
  { Use to not apply any animation effect (the default).
     Specifying any animation from the effect groups below
     negates this effect.  }
  NSTableViewAnimationEffectNone = $0;

  { Row animation Effect (optional). The effect can be combined
    with other any NSTableViewRowAnimationSlide* option.
   }
  NSTableViewAnimationEffectFade = $1; // Fades in new rows.
  NSTableViewAnimationEffectGap  = $2; // Creates a gap for newly inserted rows. This is useful for drag and drop animations that animate to a newly opened gap and should be used in -tableView:acceptDrop:row:dropOperation:.

  {Row Animation Sliding (optional). Currently only one option from this group may be specified at a time.
   }
  NSTableViewAnimationSlideUp    = $10; // Animates a row in or out by sliding upward.
  NSTableViewAnimationSlideDown  = $20; // Animates a row in or out by sliding downward.
  NSTableViewAnimationSlideLeft  = $30; // Animates a row in by sliding from the left. Animates a row out by sliding towards the left.
  NSTableViewAnimationSlideRight = $40; // Animates a row in by sliding from the right. Animates a row out by sliding towards the right.

implementation

function NSNormalWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGNormalWindowLevelKey);
end;

function NSFloatingWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGFloatingWindowLevelKey);
end;

function NSSubmenuWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGTornOffMenuWindowLevelKey);
end;

function NSTornOffMenuWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGTornOffMenuWindowLevelKey);
end;

function NSMainMenuWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGMainMenuWindowLevelKey);
end;

function NSStatusWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGStatusWindowLevelKey);
end;

function NSModalPanelWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGModalPanelWindowLevelKey);
end;

function NSPopUpMenuWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGPopUpMenuWindowLevelKey);
end;

function NSScreenSaverWindowLevel: NSInteger;
begin
  Result:=CGWindowLevelForKey(kCGScreenSaverWindowLevelKey);
end;

end.

