program TestStacktrace1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type

  { TMainClass }

  TMainClass = class
  public
    type

      { TSubClass }

      TSubClass = class
      public
        constructor Create;
        destructor Destroy; override;
        procedure RaiseSomething(Msg: string);
      end;
  public
    Sub: TSubClass;
    constructor Create;
    destructor Destroy; override;
    function CallSub(i: integer): TSubClass;
  end;

procedure SimpleCall;
var
  c: TMainClass;
begin
  c:=TMainClass.Create;
  try
    c.CallSub(123);
  finally
    c.Free;
  end;
end;

{ TMainClass.TSubClass }

constructor TMainClass.TSubClass.Create;
begin

end;

destructor TMainClass.TSubClass.Destroy;
begin
  inherited Destroy;
end;

procedure TMainClass.TSubClass.RaiseSomething(Msg: string);
begin
  if (length(Msg) div (length(Msg) div 10000))=0 then ;
end;

{ TMainClass }

constructor TMainClass.Create;
begin
  Sub:=TSubClass.Create;
end;

destructor TMainClass.Destroy;
begin
  FreeAndNil(Sub);
  inherited Destroy;
end;

function TMainClass.CallSub(i: integer): TSubClass;
begin
  if i=0 then ;
  Sub.RaiseSomething('');
  Result:=nil;
end;

begin
  SimpleCall;
end.

