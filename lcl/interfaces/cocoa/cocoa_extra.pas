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
  Classes, SysUtils,
  MacOSAll, CocoaAll;

{$define HASObjCBOOL}

type
  // Due to backwards incompatible changes in FPC sources
  // (switching from Boolean to Boolean8), LCL has to adopt
  // either type, depending on FPC version
  LCLObjCBoolean = ObjCBOOL;

type
  NSImageScaling = NSUInteger;
const // NSImageScaling values
  NSImageScaleProportionallyDown = 0;
  NSImageScaleAxesIndependently = 1;
  NSImageScaleNone = 2;
  NSImageScaleProportionallyUpOrDown = 3;

type
  NSPasteboardType = NSString;
var
  NSPasteboardTypeString: NSPasteboardType; cvar; external;
  NSPasteboardTypePNG: NSPasteboardType; cvar; external;
  NSPasteboardTypeTIFF: NSPasteboardType; cvar; external;
  NSPasteboardTypePDF: NSPasteboardType; cvar; external;
  NSPasteboardTypeHTML: NSPasteboardType; cvar; external;
  NSPasteboardTypeRTF: NSPasteboardType; cvar; external;
  NSPasteboardTypeColor: NSPasteboardType; cvar; external;
  NSPasteboardTypeFont: NSPasteboardType; cvar; external;
  NSPasteboardTypeRuler: NSPasteboardType; cvar; external;
  NSPasteboardTypeSound: NSPasteboardType; cvar; external;

type
  NSCollectionViewItemHighlightState = NSInteger; { available in 10_11 }
const
  NSCollectionViewItemHighlightNone: NSInteger           = 0;
  NSCollectionViewItemHighlightForSelection: NSInteger   = 1;
  NSCollectionViewItemHighlightForDeselection: NSInteger = 2;
  NSCollectionViewItemHighlightAsDropTarget: NSInteger   = 3;

type
  NSCollectionViewScrollPosition = NSUInteger; { available in 10_11 }
const
  NSCollectionViewScrollPositionNone: NSUInteger                  = 0;
  NSCollectionViewScrollPositionTop: NSUInteger                   = 1 shl 0;
  NSCollectionViewScrollPositionCenteredVertically: NSUInteger    = 1 shl 1;
  NSCollectionViewScrollPositionBottom: NSUInteger                = 1 shl 2;
  NSCollectionViewScrollPositionLeft: NSUInteger                  = 1 shl 3;
  NSCollectionViewScrollPositionCenteredHorizontally: NSUInteger  = 1 shl 4;
  NSCollectionViewScrollPositionRight: NSUInteger                 = 1 shl 5;
  NSCollectionViewScrollPositionLeadingEdge: NSUInteger           = 1 shl 6;
  NSCollectionViewScrollPositionTrailingEdge: NSUInteger          = 1 shl 7;
  NSCollectionViewScrollPositionNearestVerticalEdge: NSUInteger   = 1 shl 8;
  NSCollectionViewScrollPositionNearestHorizontalEdge: NSUInteger = 1 shl 9;

type
  NSCollectionViewScrollDirection = NSInteger; { available in 10_11 }
const
  NSCollectionViewScrollDirectionVertical:   NSInteger = 0;
  NSCollectionViewScrollDirectionHorizontal: NSInteger = 1;

type
  NSUserInterfaceItemIdentifier = NSString; { available in 10_12 }
  NSCollectionViewSupplementaryElementKind = NSString; { available in 10_11 }

  NSCollectionViewLayout = objcclass external  (NSObject)
  end;

  NSCollectionViewFlowLayout = objcclass external (NSCollectionViewLayout)
  public
    procedure setMinimumInteritemSpacing( newValue:CGFloat );
      message 'setMinimumInteritemSpacing:'; { available in 10_11 }
    procedure setMinimumLineSpacing( newValue:CGFloat );
      message 'setMinimumLineSpacing:'; { available in 10_11 }
    function itemSize: NSSize; message 'itemSize'; { available in 10_11 }
    procedure setItemSize( newValue:NSSize );
      message 'setItemSize:'; { available in 10_11 }
    function scrollDirection:  NSCollectionViewScrollDirection;
      message 'scrollDirection'; { available in 10_11 }
    procedure setScrollDirection( newValue: NSCollectionViewScrollDirection );
      message 'setScrollDirection:'; { available in 10_11 }
  end;

  NSCollectionViewGridLayout = objcclass external (NSCollectionViewLayout)
  public
    procedure setMinimumItemSize( newValue:NSSize );
      message 'setMinimumItemSize:'; { available in 10_11 }
    procedure setMaximumItemSize( newValue:NSSize );
      message 'setMaximumItemSize:'; { available in 10_11 }
    procedure setMinimumInteritemSpacing( newValue:CGFloat );
      message 'setMinimumInteritemSpacing:'; { available in 10_11 }
    procedure setMinimumLineSpacing( newValue:CGFloat );
      message 'setMinimumLineSpacing:'; { available in 10_11 }
    procedure setMargins( newValue:NSEdgeInsets );
      message 'setMargins:'; { available in 10_11 }
    procedure setBackgroundColors( newValue:NSArray );
      message 'setBackgroundColors:'; { available in 10_11 }
  end;

  NSCollectionViewLayoutAttributes = objcclass external (NSObject)
    function frame: NSRect; message 'frame'; { available in 10_11 }
    function size: NSSize; message 'size'; { available in 10_11 }
    function alpha: CGFloat; message 'alpha'; { available in 10_11 }
    function hidden: Boolean; message 'hidden'; { available in 10_11 }
    function zIndex: NSInteger; message 'zIndex'; { available in 10_11 }
  end;

  NSCollectionViewDataSourceProtocol = objcprotocol external name 'NSCollectionViewDataSource'
    function collectionView_numberOfItemsInSection(
      collectionView: NSCollectionView; section: NSInteger ): NSInteger;
      message 'collectionView:numberOfItemsInSection:'; { available in 10_11 }
    function collectionView_itemForRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView; indexPath: NSIndexPath ): NSCollectionViewItem;
      message 'collectionView:itemForRepresentedObjectAtIndexPath:'; { available in 10_11 }
  optional
    function numberOfSectionsInCollectionView(collectionView: NSCollectionView): NSInteger;
      message 'numberOfSectionsInCollectionView:'; { available in 10_11 }
    function collectionView_viewForSupplementaryElementOfKind_atIndexPath(
      collectionView: NSCollectionView;
      kind: NSCollectionViewSupplementaryElementKind;
      indexPath: NSIndexPath ): NSView;
      message 'collectionView:viewForSupplementaryElementOfKind:atIndexPath:'; { available in 10_11 }
  end;

  NSCollectionViewDelegateProtocol_1011 = objcprotocol (NSCollectionViewDelegateProtocol)
  optional
    function collectionView_shouldSelectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet ): NSSet;
      message 'collectionView:shouldSelectItemsAtIndexPaths:';
    procedure collectionView_didSelectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet );
      message 'collectionView:didSelectItemsAtIndexPaths:';
    function collectionView_shouldDeselectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet ): NSSet;
      message 'collectionView:shouldDeselectItemsAtIndexPaths:';
    procedure collectionView_didDeselectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet );
      message 'collectionView:didDeselectItemsAtIndexPaths:';

    function collectionView_shouldChangeItemsAtIndexPaths_toHighlightState(
      collectionView: NSCollectionView;
      indexPaths: NSSet;
      highlightState: NSCollectionViewItemHighlightState ): NSSet;
      message 'collectionView:shouldChangeItemsAtIndexPaths:toHighlightState:';
    procedure collectionView_didChangeItemsAtIndexPaths_toHighlightState(
      collectionView: NSCollectionView;
      indexPaths: NSSet;
      highlightState: NSCollectionViewItemHighlightState );
      message 'collectionView:didChangeItemsAtIndexPaths:toHighlightState:';

    procedure collectionView_willDisplayItem_forRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView;
      item:NSCollectionViewItem;
      indexPath:NSIndexPath );
      message 'collectionView:willDisplayItem:forRepresentedObjectAtIndexPath:';
    procedure collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView;
      item:NSCollectionViewItem;
      indexPath:NSIndexPath );
      message 'collectionView:didEndDisplayingItem:forRepresentedObjectAtIndexPath:';
  end;

  NSCollectionViewItemFix = objccategory external (NSCollectionViewItem)
    function highlightState: NSCollectionViewItemHighlightState;
      message 'highlightState'; { available in 10_11 }
    procedure setHighlightState( newValue:NSCollectionViewItemHighlightState );
      message 'setHighlightState:'; { available in 10_11 }
  end;

  NSCollectionViewFix = objccategory external (NSCollectionView)
    procedure registerClass_forItemWithIdentifier(
      itemClass: pobjc_class; itemIdentifier: NSUserInterfaceItemIdentifier );
      message 'registerClass:forItemWithIdentifier:'; { available in 10_11 }
    function makeItemWithIdentifier_forIndexPath(
      identifier_: NSUserInterfaceItemIdentifier;
      indexPath: NSIndexPath ): NSCollectionViewItem;
      message 'makeItemWithIdentifier:forIndexPath:';  { available in 10_11 }

    function numberOfItemsInSection( section: NSInteger ): NSInteger;
      message 'numberOfItemsInSection:'; { available in 10_11 }
    function itemAtIndexPath( indexPath: NSIndexPath ): NSCollectionViewItem;
      message 'itemAtIndexPath:'; { available in 10_11 }
    function indexPathForItem( item:NSCollectionViewItem ): NSIndexPath;
      message 'indexPathForItem:'; { available in 10_11 }
    function indexPathForItemAtPoint( point: NSPoint ): NSIndexPath;
      message 'indexPathForItemAtPoint:'; { available in 10_11 }

    function dataSource: NSCollectionViewDataSourceProtocol;
      message 'dataSource'; { available in 10_11 }
    procedure setDataSource(newValue: NSCollectionViewDataSourceProtocol);
      message 'setDataSource:'; { available in 10_11 }

    function collectionViewLayout: NSCollectionViewLayout;
      message 'collectionViewLayout'; { available in 10_11 }
    procedure setCollectionViewLayout( newValue: NSCollectionViewLayout );
      message 'setCollectionViewLayout:'; { available in 10_11 }
    function layoutAttributesForItemAtIndexPath(
      indexPaht: NSIndexPath ): NSCollectionViewLayoutAttributes;
      message 'layoutAttributesForItemAtIndexPath:'; { available in 10_11 }

    function visibleItems: NSArray;
      message 'visibleItems'; { available in 10_11 }
    procedure scrollToItemsAtIndexPaths_scrollPosition(
      indexPaths: NSSet; scrollPosition: NSCollectionViewScrollPosition );
      message 'scrollToItemsAtIndexPaths:scrollPosition:'; { available in 10_11 }

    procedure reloadData; message 'reloadData'; { available in 10_11 }

    function allowsEmptySelection: Boolean;
      message 'allowsEmptySelection'; { available in 10_11 }
    procedure setAllowsEmptySelection( newValue: Boolean );
      message 'setAllowsEmptySelection:'; { available in 10_11 }

    function selectionIndexPaths: NSSet;
      message 'selectionIndexPaths'; { available in 10_11 }
    procedure setSelectionIndexPaths( newValue: NSSet );
      message 'setSelectionIndexPaths:'; { available in 10_11 }
    procedure selectItemsAtIndexPaths_scrollPosition(
      indexPaths: NSSet; scrollPosition: NSCollectionViewScrollPosition );
      message 'selectItemsAtIndexPaths:scrollPosition:'; { available in 10_11 }
    procedure deselectItemsAtIndexPaths( indexPaths: NSSet );
      message 'deselectItemsAtIndexPaths:'; { available in 10_11 }
    procedure selectAll( sender: id );
      message 'selectAll:'; { available in 10_11 }
    procedure deselectAll( sender: id );
      message 'deselectAll:'; { available in 10_11 }
  end;

  NSIndexPathFix = objccategory external (NSIndexPath)
    function item: NSInteger; message 'item';
    function section: NSInteger; message 'section';
    class function indexPathForItem_inSection(
      item: NSInteger; section: NSInteger ): id;
      message 'indexPathForItem:inSection:'; { available in 10_11 }
  end;

const
  UNAuthorizationOptionBadge   = 1 shl 0;
  UNAuthorizationOptionSound   = 1 shl 1;
  UNAuthorizationOptionAlert   = 1 shl 2;
  UNAuthorizationOptionCarPlay = 1 shl 3;

type
  UNNotificationSound = objcclass external (NSObject)
    class function defaultSound: UNNotificationSound; message 'defaultSound';
  end;

  UNNotificationContent = objcclass external (NSObject)
  public
    function title: NSString; message 'title';
    function subtitle: NSString; message 'subtitle';
    function body: NSString; message 'body';
    function badge: NSNumber; message 'badge';
    function sound: UNNotificationSound; message 'sound';
  end;

  UNMutableNotificationContent = objcclass external (UNNotificationContent)
  public
    procedure setTitle( newValue: NSString ); message 'setTitle:';
    procedure setSubtitle( newValue: NSString ); message 'setSubtitle:';
    procedure setBody( newValue: NSString ); message 'setBody:';
    procedure setBadge( newValue: NSNumber ); message 'setBadge:';
    procedure setSound( newValue: UNNotificationSound ); message 'setSound:';
  end;

  UNNotificationTrigger = objcclass external (NSObject)
  public
    function repeats: ObjCBool; message 'repeats';
  end;

  UNTimeIntervalNotificationTrigger = objcclass external (UNNotificationTrigger)
  public
    class function triggerWithTimeInterval_repeats(
      timeInterval: NSTimeInterval; repeats_: ObjCBool ): id;
      message 'triggerWithTimeInterval:repeats:';
  end;

  UNNotificationRequest = objcclass external (NSObject)
  public
    class function requestWithIdentifier_content_trigger(
      identifier: NSString;
      content: UNNotificationContent;
      trigger: UNNotificationTrigger ): id;
      message 'requestWithIdentifier:content:trigger:';
    function identifier: NSString; message 'identifier';
    function content: UNNotificationContent; message 'content';
    function trigger: UNNotificationTrigger; message 'trigger';
  end;

  UNAuthorizationOptions = NSUInteger;

  UNUserNotificationCenter = objcclass external (NSObject)
  public
    class function currentNotificationCenter: UNUserNotificationCenter;
      message 'currentNotificationCenter';
    procedure requestAuthorizationWithOptions_completionHandler(
      options: UNAuthorizationOptions; completionHandler: OpaqueCBlock = nil );
      message 'requestAuthorizationWithOptions:completionHandler:';
    procedure addNotificationRequest_withCompletionHandler(
      request: UNNotificationRequest;
      completionHandler: OpaqueCBlock = nil );
      message 'addNotificationRequest:withCompletionHandler:';
  end;

  NSMenuFix = objccategory external (NSMenu)
    function itemAtIndex(index: NSInteger): NSMenuItem; message 'itemAtIndex:';
  end;

  {$ifdef BOOLFIX}
  ObjCBool = ShortInt; // Matches BOOL declaration in ObjC "signed char"
                       // Note that this is different than LCLObjCBoolean
                       // even though it's trying to resolve the same problem
                       // for FPC3.0.4. ObjCBool should be removed after the officail
                       // fpc3.2+ release
  {$endif}

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

type
  NSFontWeight = CGFloat;
const
  NSFontWeightRegular = 0.0;

type
  NSFontFix = objccategory external (NSFont)
    // available in 10.15+
    class function monospacedSystemFontOfSize_weight (fontSize: CGFloat; weight: NSFontWeight): NSFont;
      message 'monospacedSystemFontOfSize:weight:';
  end;

  NSApplicationFix = objccategory external (NSApplication)
    {$ifdef BOOLFIX}
    procedure activateIgnoringOtherApps_(flag: ObjCBool); message 'activateIgnoringOtherApps:';
    function nextEventMatchingMask_untilDate_inMode_dequeue_(mask: NSUInteger; expiration: NSDate; mode: NSString; deqFlag: ObjCBool): NSEvent; message 'nextEventMatchingMask:untilDate:inMode:dequeue:';
    procedure postEvent_atStart_(event: NSEvent; flag: ObjCBool); message 'postEvent:atStart:';
    {$endif}

    function appearance: NSAppearance; message 'appearance'; // 10.14 (10.13)
    procedure setAppearance(newValue:NSAppearance); message 'setAppearance:'; // 10.14 (10.13)
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance'; // 10.14 (10.13)
  end;

  {$ifdef BOOLFIX}
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
    procedure setPlaceholderString(str: NSString); message 'setPlaceholderString:';
  end;
  {$endif}

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

  NSImageFix = objccategory external (NSImage)
    class function imageWithSystemSymbolName_accessibilityDescription(
      aName: NSString; aAccessibilityDescription: NSString ): id;
      message 'imageWithSystemSymbolName:accessibilityDescription:'; { available in 11.0 }
  end;

  NSEventFix = objccategory external (NSEvent)
    class function modifierFlags_: NSUInteger; message 'modifierFlags';
    // available in 10.7+
    function hasPreciseScrollingDeltas: LCLObjCBoolean; message 'hasPreciseScrollingDeltas';
    function scrollingDeltaX: CGFloat; message 'scrollingDeltaX';
    function scrollingDeltaY: CGFloat; message 'scrollingDeltaY';
  end;

type
  NSTitlebarSeparatorStyle = NSInteger;     { available in 11.0 }
const
  NSTitlebarSeparatorStyleAutomatic = 0;
  NSTitlebarSeparatorStyleNone      = 1;
  NSTitlebarSeparatorStyleLine      = 2;
  NSTitlebarSeparatorStyleShadow    = 3;

type
  NSWindowToolbarStyle = NSInteger;    { available in 11.0 }
const
  NSWindowToolbarStyleAutomatic      = 0;
  NSWindowToolbarStyleExpanded       = 1;
  NSWindowToolbarStylePreference     = 2;
  NSWindowToolbarStyleUnified        = 3;
  NSWindowToolbarStyleUnifiedCompact = 4;

type
  NSWindowTabbingMode = NSInteger;

type
  NSWindowFix = objccategory external (NSWindow)
    // 10.4-10.7
    // userSpaceScaleFactor is declare in the latest CocoaAll
    //function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor'; //deprecated
    // 10.7+
    procedure toggleFullScreen(sender: id); message 'toggleFullScreen:';
    function backingScaleFactor: CGFloat; message 'backingScaleFactor';
    function isRestorable: LCLObjCBoolean; message 'isRestorable';
    procedure setRestorable(ARestore: LCLObjCBoolean); message 'setRestorable:';
    // 10.11
    procedure performWindowDragWithEvent(event: NSEvent); message 'performWindowDragWithEvent:';
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
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance'; // 10.14 (10.13)
    // 11.0
    procedure setTitlebarSeparatorStyle( newValue: NSTitlebarSeparatorStyle );
      message 'setTitlebarSeparatorStyle:';  { available in 11.0 }
    procedure setToolbarStyle( newValue: NSWindowToolbarStyle );
      message 'setToolbarStyle:';  { available in 11.0 }
  end;

type
  NSToolBarItemFix = objccategory external (NSToolBarItem)
    procedure setBordered( newValue: Boolean ); message 'setBordered:'; { available in 10.15 }
    procedure setNavigational( newValue: Boolean ); message 'setNavigational:'; { available in 11.0 }
  end;

type
  NSSearchFieldDelegateProtocol = objcprotocol external (NSTextFieldDelegateProtocol)
    procedure searchFieldDidStartSearching( sender: NSSearchField );
      message 'searchFieldDidStartSearching:';
    procedure searchFieldDidEndSearching( sender: NSSearchField );
      message 'searchFieldDidEndSearching:';
  end;

  NSSearchToolbarItem = objcclass external (NSToolBarItem)
    procedure setSearchField( newValue: NSSearchField );
      message 'setSearchField:';  { available in 11.0 }
    function searchField: NSSearchField;
      message 'searchField';  { available in 11.0 }

    procedure setPreferredWidthForSearchField( newValue: CGFloat );
      message 'setPreferredWidthForSearchField:';  { available in 11.0 }

    procedure setResignsFirstResponderWithCancel( newValue: Boolean );
      message 'setResignsFirstResponderWithCancel:';  { available in 11.0 }
  end;

type
  NSSharingServicePickerToolbarItem = objcclass;

  NSSharingServicePickerToolbarItemDelegateProtocol = objcprotocol external name 'NSSharingServicePickerToolbarItemDelegate'
    function itemsForSharingServicePickerToolbarItem(
      pickerToolbarItem: NSSharingServicePickerToolbarItem ): NSArray;
      message 'itemsForSharingServicePickerToolbarItem:';  { available in 10.15 }
  end;

  NSSharingServicePickerToolbarItem = objcclass external (NSToolBarItem)
    procedure setDelegate( aDelegate: NSSharingServicePickerToolbarItemDelegateProtocol );
      message 'setDelegate:';  { available in 10.15 }
    function delegate: NSSharingServicePickerToolbarItemDelegateProtocol;
      message 'delegate';  { available in 10.15 }
  end;

type
  NSMenuToolbarItem = objcclass external (NSToolBarItem)
    function menu: NSMenu; message 'menu';  { available in 10.15 }
    procedure setMenu( newValue: NSMenu ); message 'setMenu:';  { available in 10.15 }
    procedure setShowsIndicator( newValue: Boolean ); message 'setShowsIndicator:';  { available in 10.15 }
  end;

type
  NSToolbarItemGroupSelectionMode = NSInteger;
const
  NSToolbarItemGroupSelectionModeSelectOne = 0;
  NSToolbarItemGroupSelectionModeSelectAny = 1;
  NSToolbarItemGroupSelectionModeMomentary = 2;
type
  NSToolbarItemGroupControlRepresentation = NSInteger;
const
  NSToolbarItemGroupControlRepresentationAutomatic = 0;
  NSToolbarItemGroupControlRepresentationExpanded = 1;
  NSToolbarItemGroupControlRepresentationCollapsed = 2;

type
  NSToolbarItemGroupFix = objccategory external (NSToolbarItemGroup)
    procedure setControlRepresentation( newValue: NSToolbarItemGroupControlRepresentation );
      message 'setControlRepresentation:';  { available in 10.15 }
    procedure setSelectionMode( newValue: NSToolbarItemGroupSelectionMode );
      message 'setSelectionMode:';  { available in 10.15 }

    function selectedIndex: NSInteger; message 'selectedIndex'; { available in 10.15 }
    function isSelectedAtIndex: Boolean; message 'isSelectedAtIndex'; { available in 10.15 }
    procedure setSelected_atIndex( newValue: Boolean; index: NSInteger );
      message 'setSelected:atIndex:';  { available in 10.15 }

    class function groupWithItemIdentifier_titles_selectionMode_labels_target_action(
        aItemIdentifier: NSString;
        titles: NSArray;
        selectionMode: NSToolbarItemGroupSelectionMode;
        labels: NSArray;
        aTarget: id;
        aAction: SEL ): id;
      message 'groupWithItemIdentifier:titles:selectionMode:labels:target:action:'; { available in 10.15 }
    class function groupWithItemIdentifier_images_selectionMode_labels_target_action(
        aItemIdentifier: NSString;
        images: NSArray;
        selectionMode: NSToolbarItemGroupSelectionMode;
        labels: NSArray;
        aTarget: id;
        aAction: SEL ): id;
      message 'groupWithItemIdentifier:images:selectionMode:labels:target:action:'; { available in 10.15 }
  end;

type
  NSTableColumnFix = objccategory external (NSTableColumn)
    procedure setTitle(atitle: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    {$ifdef BOOLFIX}
    procedure setHidden_(flag: ObjCBool); message 'setHidden:';
    {$endif}
  end;

  NSTableViewAnimationOptions = NSUInteger;

  NSTableViewStyle = NSInteger;    // 11.0

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
    // 11.0
    procedure setStyle(newValue: NSTableViewStyle); message 'setStyle:';
    function style: NSTableViewStyle; message 'style';
    function effectiveStyle: NSTableViewStyle; message 'effectiveStyle';
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
  NSAppKitVersionNumber10_15 = 1894;
  NSAppKitVersionNumber11_0  = 2022; // 2000 starts with beta?
  NSAppKitVersionNumber12_0  = 2113;
  NSAppKitVersionNumber13_0  = 2299;
  NSAppKitVersionNumber14_0  = 2487;

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

{ NSVisualEffectView }
// Taken from macOS 10.10 headers at https://github.com/genericptr/MacOS_10_10
type
  NSVisualEffectMaterial = NSInteger;
  NSVisualEffectMaterialPtr = ^NSVisualEffectMaterial;
const
  NSVisualEffectMaterialAppearanceBased = 0 deprecated;
  NSVisualEffectMaterialLight = 1 deprecated;
  NSVisualEffectMaterialDark = 2 deprecated;
  NSVisualEffectMaterialTitlebar = 3;
  NSVisualEffectMaterialSelection = 4;
  // 10.11
  NSVisualEffectMaterialMenu = 5;
  NSVisualEffectMaterialPopover = 6;
  NSVisualEffectMaterialSidebar =  7;
  NSVisualEffectMaterialMediumLight = 8 deprecated;
  NSVisualEffectMaterialUltraDark = 9 deprecated;
  // 10.14
  NSVisualEffectMaterialHeaderView = 10;
  NSVisualEffectMaterialSheet = 11;
  NSVisualEffectMaterialWindowBackground = 12;
  NSVisualEffectMaterialHUDWindow = 13;
  NSVisualEffectMaterialFullScreenUI = 15;
  NSVisualEffectMaterialToolTip = 17;
  NSVisualEffectMaterialContentBackground = 18;
  NSVisualEffectMaterialUnderWindowBackground = 21;
  NSVisualEffectMaterialUnderPageBackground = 22;
type
  NSColorFix = objccategory external (NSColor)
    class function linkColor: NSColor; message 'linkColor';
  end;


// all of the sudden those are gone! in FPC 3.2.0rc
const
  NSVariableStatusItemLength = -1;
  NSSquareStatusItemLength = -2;

type
  NSSavePanelFix = objccategory external (NSSavePanel)
    // available in 10.9+
    procedure setShowsTagField(AShow: LCLObjCBoolean); message 'setShowsTagField:';
    function showsTagField: LCLObjCBoolean; message 'showsTagField';
  end;

const
  // 11.0
  NSTableViewStyleAutomatic = 0;
  NSTableViewStyleFullWidth = 1;
  NSTableViewStyleInset = 2;
  NSTableViewStyleSourceList = 3;
  NSTableViewStylePlain = 4;

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

