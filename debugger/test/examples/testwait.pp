unit TestWait;
{$mode objfpc}{$H+}
interface

type
  TWait = class
  private
    FTime: TDateTime;
    FInt: Integer;
  public
    constructor Create(const ATime: Integer);
    procedure Wait(const ATime: Integer);
  end;

implementation

uses
  SysUtils; 

procedure Wait(const ATime: Integer);
var
  time: TDateTime;
begin
  time := now;
  while (now - time) * SecsPerDay < ATime  do;
end;

constructor TWait.Create(const ATime: Integer);
var
  n: Integer;
begin
  FTime := ATime;
  FInt := ATime;
  inherited Create; 
  n := 0;
  while n < ATime do Inc(n); //something useles 
end;

procedure TWait.Wait(const ATime: Integer);
begin
  TestWait.Wait(ATime);
end;

  
var
  n: Integer;
begin
  n := 0;
  while n < 1001 do Inc(n); //something useles 
end.
