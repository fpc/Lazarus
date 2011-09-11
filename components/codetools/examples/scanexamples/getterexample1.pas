unit getterexample1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TOldest }

  TOldest = class
  private
    FBar: integer;
    function GetBar: integer; virtual;
  public
    property Bar: integer read GetBar;
  end;

  { TOlder }

  TOlder = class(TOldest)
  private
    function GetBar: integer; override;
  public
    procedure DoSomething;
    function GetOldest(Older: TOlder): TOldest;
  end;

implementation

{ TOldest }

function TOldest.GetBar: integer;
begin
  Result:=FBar;
end;

{ TOlder }

function TOlder.GetBar: integer;
begin
  Result:=FBar;
end;

procedure TOlder.DoSomething;
var
  Older: TOlder;
begin
  Older:=TOlder.Create;
  with TOlder(Older) do
    writeln(Bar);
  with Older.GetOldest(Self) do
    writeln(Bar);
end;

function TOlder.GetOldest(Older: TOlder): TOldest;
begin
  Result:=Older;
end;

end.

