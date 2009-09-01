{%mainunit appkit.pas}
{
        NSActionCell.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
}

{$ifdef CLASSES}
{$ifndef NSACTIONCELL_PAS_C}
{$define NSACTIONCELL_PAS_C}

{$include NSCell.inc}

  NSActionCell = class(NSCell)
  public

{- (NSView *)controlView;
#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
- (void)setControlView:(NSView*)view;
#endif
- (void)setFont:(NSFont *)fontObj;
- (void)setAlignment:(NSTextAlignment)mode;
- (void)setBordered:(BOOL)flag;
- (void)setBezeled:(BOOL)flag;
- (void)setEnabled:(BOOL)flag;
- (void)setFloatingPointFormat:(BOOL)autoRange left:(unsigned int)leftDigits right:(unsigned int)rightDigits;
- (void)setImage:(NSImage *)image;
- (id)target;
- (void)setTarget:(id)anObject;
- (SEL)action;
- (void)setAction:(SEL)aSelector;
- (int)tag;
- (void)setTag:(int)anInt;
- (NSString *)stringValue;
- (int)intValue;
- (float)floatValue;
- (double)doubleValue;
- (void)setObjectValue:(id<NSCopying>)obj;}

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

{$endif}

