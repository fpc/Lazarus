unit RemoveWith1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TParent1 = class(TComponent)
  public
    Mother: TParent1;
  end;

  TSon1 = class(TParent1)
  public

  end;

  TSon2 = class(TParent1)
  public
  end;

procedure DoSomething;

implementation

procedure DoSomething;
var
  Parent1: TParent1;
  Son1: TSon1;
  Son2: TSon2;
begin
  Parent1:=TParent1.Create(nil);
  Son1:=TSon1.Create(nil);
  Son2:=TSon2.Create(nil);
  with Parent1, Son1 do begin
    Name:='name of TSon1';
    with Son2 do begin
      Name:='name of TSon2';
      //
    end;
  end;
end;

end.

