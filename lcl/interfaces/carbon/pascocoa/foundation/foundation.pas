unit foundation;

{$ifdef fpc}
  {$mode objfpc}{$H+}
  {$packrecords c}
{$endif}

{$linkframework Cocoa}

interface

uses SysUtils, ctypes, lobjc, MacOSAll;

{$define HEADER}
{$include Foundation.inc}
{$undef HEADER}

type
{$define CLASSES}
{$include Foundation.inc}
{$undef CLASSES}

implementation

{$define IMPLEMENTATION}
{$include Foundation.inc}
{$undef IMPLEMENTATION}

end.

