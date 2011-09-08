unit addwith1;

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
  Son1.Name:=Son2.Name;
  Son2.Mother:=Son1;
  Son1.Mother:=Son2.Mother.Mother;
  Son1.Components[0].Create(nil);
end;

end.

