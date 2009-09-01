{%mainunit appkit.pas}
(*
	NSGraphics.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSGRAPHICS_PAS_H}
{$define NSGRAPHICS_PAS_H}

{$include AppKitDefines.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
  {.$endif}

type
  NSCompositingOperation = (
    NSCompositeClear = 0, 
    NSCompositeCopy = 1, 
    NSCompositeSourceOver = 2, 
    NSCompositeSourceIn = 3, 
    NSCompositeSourceOut = 4, 
    NSCompositeSourceAtop = 5, 
    NSCompositeDestinationOver = 6, 
    NSCompositeDestinationIn = 7, 
    NSCompositeDestinationOut = 8, 
    NSCompositeDestinationAtop = 9, 
    NSCompositeXOR = 10, 
    NSCompositePlusDarker = 11, 
    NSCompositeHighlight = 12, 
    NSCompositePlusLighter = 13
  );


type
  NSBackingStoreType = (
    NSBackingStoreRetained = 0, 
    NSBackingStoreNonretained = 1, 
    NSBackingStoreBuffered = 2
  );


type
  NSWindowOrderingMode = (
    NSWindowAbove = 1, 
    NSWindowBelow = 1, 
    NSWindowOut = 0
  );


type
  NSFocusRingPlacement = (
    NSFocusRingOnly = 0, 
    NSFocusRingBelow = 1, 
    NSFocusRingAbove = 2
  );


type
  NSFocusRingType = (
    NSFocusRingTypeDefault = 0, 
    NSFocusRingTypeNone = 1, 
    NSFocusRingTypeExterior = 2
  );


//APPKIT_EXTERN NSString *NSCalibratedWhiteColorSpace;	/* 1.0 == white */
//APPKIT_EXTERN NSString *NSCalibratedBlackColorSpace;	/* 1.0 == black */
//APPKIT_EXTERN NSString *NSCalibratedRGBColorSpace;
//APPKIT_EXTERN NSString *NSDeviceWhiteColorSpace;	/* 1.0 == white */
//APPKIT_EXTERN NSString *NSDeviceBlackColorSpace;	/* 1.0 == black */
//APPKIT_EXTERN NSString *NSDeviceRGBColorSpace;
//APPKIT_EXTERN NSString *NSDeviceCMYKColorSpace;
//APPKIT_EXTERN NSString *NSNamedColorSpace;		/* Used for "catalog" colors */
//APPKIT_EXTERN NSString *NSPatternColorSpace;
//APPKIT_EXTERN NSString *NSCustomColorSpace;		/* Used to indicate a custom gstate in images */
type
  NSWindowDepth = Integer;

//APPKIT_EXTERN NSWindowDepth NSBestDepth (NSString *colorSpace, int bps, int bpp, BOOL planar, BOOL *exactMatch);
//APPKIT_EXTERN BOOL NSPlanarFromDepth (NSWindowDepth depth);
//APPKIT_EXTERN NSString *NSColorSpaceFromDepth (NSWindowDepth depth);
//APPKIT_EXTERN int NSBitsPerSampleFromDepth (NSWindowDepth depth);
//APPKIT_EXTERN int NSBitsPerPixelFromDepth (NSWindowDepth depth);
//APPKIT_EXTERN int NSNumberOfColorComponents (NSString *colorSpaceName);
//APPKIT_EXTERN const NSWindowDepth *NSAvailableWindowDepths (void); /* 0 terminated */
//APPKIT_EXTERN const float NSWhite;
//APPKIT_EXTERN const float NSLightGray;
//APPKIT_EXTERN const float NSDarkGray;
//APPKIT_EXTERN const float NSBlack;
//APPKIT_EXTERN NSString *NSDeviceResolution;		/* NSValue containing NSSize, basically dpi */
//APPKIT_EXTERN NSString *NSDeviceColorSpaceName;	/* NSString */
//APPKIT_EXTERN NSString *NSDeviceBitsPerSample;		/* NSValue containing int */
//APPKIT_EXTERN NSString *NSDeviceIsScreen;		/* "YES" or not there */
//APPKIT_EXTERN NSString *NSDeviceIsPrinter;		/* "YES" or not there */
//APPKIT_EXTERN NSString *NSDeviceSize;			/* NSValue containing NSSize */
//APPKIT_EXTERN void NSRectFill(NSRect aRect);
//APPKIT_EXTERN void NSRectFillList(const NSRect *rects, int count);
//APPKIT_EXTERN void NSRectFillListWithGrays(const NSRect *rects, const float *grays, int num);
//APPKIT_EXTERN void NSRectFillListWithColors(const NSRect *rects, NSColor **colors, int num);
//APPKIT_EXTERN void NSRectFillUsingOperation(NSRect aRect, NSCompositingOperation op);
//APPKIT_EXTERN void NSRectFillListUsingOperation(const NSRect *rects, int count, NSCompositingOperation op);
//APPKIT_EXTERN void NSRectFillListWithColorsUsingOperation(const NSRect *rects, NSColor **colors, int num, NSCompositingOperation op);
//APPKIT_EXTERN void NSFrameRect(NSRect aRect);
//APPKIT_EXTERN void NSFrameRectWithWidth(NSRect aRect, float frameWidth);
//APPKIT_EXTERN void NSFrameRectWithWidthUsingOperation(NSRect aRect, float frameWidth, NSCompositingOperation op);
//APPKIT_EXTERN void NSRectClip(NSRect aRect);
//APPKIT_EXTERN void NSRectClipList(const NSRect *rects, int count);
//APPKIT_EXTERN NSRect NSDrawTiledRects(NSRect boundsRect, NSRect clipRect, const NSRectEdge *sides, const float *grays, int count);
//APPKIT_EXTERN void NSDrawGrayBezel(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSDrawGroove(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSDrawWhiteBezel(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSDrawButton(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSEraseRect(NSRect aRect);
//APPKIT_EXTERN NSColor *NSReadPixel(NSPoint passedPoint);
//APPKIT_EXTERN void NSDrawBitmap(NSRect rect, int width, int height, int bps, int spp, int bpp, int bpr, BOOL isPlanar, BOOL hasAlpha, NSString *colorSpaceName, const unsigned char *const data[5]);
//APPKIT_EXTERN void NSCopyBits(int srcGState, NSRect srcRect, NSPoint destPoint);
//APPKIT_EXTERN void NSHighlightRect(NSRect aRect);
//APPKIT_EXTERN void NSBeep(void);
//APPKIT_EXTERN void NSCountWindows(int *count);
//APPKIT_EXTERN void NSWindowList(int size, int list[]);
//APPKIT_EXTERN void NSCountWindowsForContext(int context, int *count);
//APPKIT_EXTERN void NSWindowListForContext(int context, int size, int list[]);
//APPKIT_EXTERN int NSGetWindowServerMemory(int context, int *virtualMemory, int *windowBackingMemory, NSString **windowDumpString);
//APPKIT_EXTERN NSRect NSDrawColorTiledRects(NSRect boundsRect, NSRect clipRect, const NSRectEdge *sides, NSColor **colors, int count);
//APPKIT_EXTERN void NSDrawDarkBezel(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSDrawLightBezel(NSRect aRect, NSRect clipRect);
//APPKIT_EXTERN void NSDottedFrameRect(NSRect aRect);
//APPKIT_EXTERN void NSDrawWindowBackground(NSRect aRect);
//APPKIT_EXTERN void NSSetFocusRingStyle(NSFocusRingPlacement placement);
//APPKIT_EXTERN void NSDisableScreenUpdates(void)	AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
//APPKIT_EXTERN void NSEnableScreenUpdates(void)	AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
type
  NSAnimationEffect = (
    NSAnimationEffectDisappearingItemDefault = 0, 
    NSAnimationEffectPoof = 10
  );


//APPKIT_EXTERN void NSShowAnimationEffect(NSAnimationEffect animationEffect, NSPoint centerLocation, NSSize size, id animationDelegate, SEL didEndSelector, void *contextInfo) AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
{$endif}
{$endif}

{$ifdef CLASSES}
{$ifndef NSGRAPHICS_PAS_C}
{$define NSGRAPHICS_PAS_C}

{$include AppKitDefines.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    {.$endif}
{$endif}
{$endif}
