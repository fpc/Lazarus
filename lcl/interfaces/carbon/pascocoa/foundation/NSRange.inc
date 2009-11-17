{%mainunit foundation.pas}
(*	NSRange.h
	Copyright (c) 1994-2005, Apple, Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSRANGE_PAS_H}
{$define NSRANGE_PAS_H}

{$include NSValue.inc}

type
  _NSRange = packed  record 
    location : LongWord; 
    length : LongWord; 
  end;
  NSRange = _NSRange;

type
  NSRangePointer = NSRange;

//FOUNDATION_STATIC_INLINE NSRange NSMakeRange(unsigned int loc, unsigned int len) {
//NSRange r;
//r.location = loc;
//r.length = len;
//return r;
//}
//FOUNDATION_STATIC_INLINE unsigned int NSMaxRange(NSRange range) {
//return (range.location + range.length);
//}
//FOUNDATION_STATIC_INLINE BOOL NSLocationInRange(unsigned int loc, NSRange range) {
//return (loc - range.location < range.length);
//}
//FOUNDATION_STATIC_INLINE BOOL NSEqualRanges(NSRange range1, NSRange range2) {
//return (range1.location == range2.location && range1.length == range2.length);
//}
//FOUNDATION_EXPORT NSRange NSUnionRange(NSRange range1, NSRange range2);
//FOUNDATION_EXPORT NSRange NSIntersectionRange(NSRange range1, NSRange range2);
//FOUNDATION_EXPORT NSString *NSStringFromRange(NSRange range);
//FOUNDATION_EXPORT NSRange NSRangeFromString(NSString *aString);
{$endif}
{$endif}

{$ifdef CLASSES}
{$ifndef NSRANGE_PAS_C}
{$define NSRANGE_PAS_C}

{$include NSValue.inc}
{$endif}
{$endif}
