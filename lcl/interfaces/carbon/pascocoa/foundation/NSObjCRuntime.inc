{%mainunit foundation.pas}
(*	NSObjCRuntime.h
	Copyright (c) 1994-2007, Apple Inc. All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSOBJCRUNTIME_PAS_H}
{$define NSOBJCRUNTIME_PAS_H}

type
  NSInteger  = Integer;
  NSUInteger = LongWord;

const
  NSOrderedAscending  = 1;
  NSOrderedSame       = 2;
  NSOrderedDescending = 3;

type
  NSComparisonResult = Integer;

const
  NSNotFound = MaxInt;

{$endif}
{$endif}
