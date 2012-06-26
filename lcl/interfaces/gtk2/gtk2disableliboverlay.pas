unit gtk2DisableLibOverlay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  ctypes;

function setenv(name, value: PChar; replace: cint): cint; cdecl; external 'c';

initialization
  setenv('LIBOVERLAY_SCROLLBAR', '0', 1);
end.

