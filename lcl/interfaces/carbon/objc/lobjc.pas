{
 lobjc.pas

 Copyright (C) 2007 Felipe Monteiro de Carvalho

 This unit is a pascal binding for the Objective-C Run-time Library
 headers included with XCode 2.4.1
 The original copyright note of is kept on each include file
}
unit lobjc;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

{$Packrecords c}

{$linklib objc}

interface

uses ctypes, unix;

{$include lobjc-api.inc}
{$include lobjc.inc}
{$include lobjc-class.inc}

{$include lobjc-auto.inc}
{$include lobjc-exception.inc}
{.$include lobjc-load.inc} // This module is obsolete
{$include lobjc-runtime.inc}
{$include lobjc-sync.inc}

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
