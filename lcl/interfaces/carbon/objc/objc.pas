{
 objc.pas

 Copyright (C) 2007 Felipe Monteiro de Carvalho

 This unit is a pascal binding for the Objective-C Run-time Library
 headers included with XCode 2.4.1
 The original copyright note of is kept on each include file
}
unit objc;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

{$Packrecords c}

{$linklib objc}

interface

uses ctypes, unix;

{$LinkLib objc}

{$include objc-api.inc}
{$include objc.inc}
{$include objc-class.inc}

{$include objc-auto.inc}
{$include objc-exception.inc}
{.$include objc-load.inc} // This module is obsolete
{$include objc-runtime.inc}
{$include objc-sync.inc}

{$include error.inc}
{.$include hashtable.inc}
{.$include hashtable2.inc}
{$include malloc.inc}
{.$include zone.inc}

{ Extra declarations }

implementation

{ Macros from error.h }
procedure NX_RAISE(code: cint; const data1, data2: Pointer);
begin
  //the function is not avaialbe in OX 10.5
  //todo: (use objcrtl?)
  //_NXRaiseError(code, data1, data2);
end;

procedure NX_RERAISE();
begin
//  _NXRaiseError(NXLocalHandler.code, NXLocalHandler.data1, NXLocalHandler.data2);
end;

end.
