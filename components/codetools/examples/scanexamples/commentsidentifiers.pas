unit commentsidentifiers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMyRange = 1..10;

{$IFDEF doesnotexist}
var
  i: TMyRange;
{$ENDIF}

implementation

end.

