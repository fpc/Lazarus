unit UnusedUnits1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyClass }

  TMyClass = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TMyClass }

constructor TMyClass.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  for i:=1 to 3 do begin
    WriteLn('TMyClass.Create ',i);
  end;
end;

end.

