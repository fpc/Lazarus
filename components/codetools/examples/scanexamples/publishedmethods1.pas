unit publishedmethods1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PublishedMethods2;

type
  TMyMethodWithAnInteger = procedure (x:T) of object;
  T = char;
  TMyMethodWithAnChar = procedure (x:T) of object;

  { TMyClass }
  {$M+}
  TMyClass = class
  published
    procedure F(x: T);
  end;
  {$M-}

implementation

{ TMyClass }

procedure TMyClass.F(x: T);
begin

end;

end.

