{
	AppKit.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.

	This file is included by all AppKit application source files for easy building.  Using this file is preferred over importing individual files because it will use a precompiled version.
}
unit appkit;

{$ifdef fpc}
  {$mode objfpc}
  {$packrecords c}
{$endif}

interface

uses ctypes, MacOSAll, lobjc, foundation;

{$define HEADER}
{$include AppKit.inc}
{$undef HEADER}

type
{$define FORWARD}
{$include AppKit.inc}
{$undef FORWARD}

{$define CLASSES}
{$include AppKit.inc}
{$undef CLASSES}

var
  NSApp: NSApplication;

implementation

{$define IMPLEMENTATION}
{$include AppKit.inc}
{$undef IMPLEMENTATION}

end.

