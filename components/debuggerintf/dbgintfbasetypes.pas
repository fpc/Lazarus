unit DbgIntfBaseTypes;
(*                 DebuggerTypes

  Basic types for any Pascal debugger. (not just IDE)

*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  // datatype pointing to data on the target
  TDBGPtr = QWord;

implementation

end.

