unit muistringsunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
type
  TStringPtrs= array of PChar;
  PStringPtrs= ^TStringPtrs;
  { TMUIStrings }

  TMUIStrings = TStringList;

  {
  TMUIStrings = class(TStringList)
  private
    FStrings: TStringPtrs;
    FOnChanged: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: String): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1: Integer; Index2: Integer); override;
    procedure Put(Index: Integer; const S: string); override;
    procedure Insert(Index: Integer; const S: String); override;
    property StringPtrs: TStringPtrs read FStrings;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;
   }


implementation

{ TMUIStrings }
(*
constructor TMUIStrings.Create;
begin
  inherited;
  SetLength(FStrings, 1);
  FStrings[0] := nil;
end;

destructor TMUIStrings.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FStrings) do
  begin
    if FStrings[i] <> nil then
      FreeMem(FStrings[i])
  end;
  inherited;
end;

function TMUIStrings.Add(const S: String): Integer;
var
  str: string;
begin
  inherited;
  Str := S + #0;
  SetLength(FStrings, Length(FStrings) + 1);
  GetMem(FStrings[High(FStrings) - 1], Length(Str));
  StrCopy(FStrings[High(FStrings) - 1], PChar(Str));
  FStrings[High(FStrings)] := nil;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMUIStrings.Clear;
var
  i: Integer;
begin
  inherited;;
  for i := 0 to High(FStrings) do
  begin
    if FStrings[i] <> nil then
      FreeMem(FStrings[i])
  end;
  SetLength(FStrings, 1);
  FStrings[0] := nil;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMUIStrings.Delete(Index: Integer);
var
  i: LongInt;
begin
  inherited;
  if (Index >= 0) and (Index < High(FStrings)) then
  begin
    if FStrings[Index] <> nil then
      FreeMem(FStrings[Index]);
    for i := Index to (High(FStrings) - 1) do
    begin
      FStrings[i] := FStrings[i + 1];
    end;
    SetLength(FStrings, Length(FStrings) - 1);
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMUIStrings.Exchange(Index1: Integer; Index2: Integer);
var
  PC: PChar;
begin
  inherited;
  if (Index1 >= 0) and (Index2 < High(FStrings)) and
     (Index1 >= 0) and (Index2 < High(FStrings)) then
  begin
    PC := FStrings[Index1];
    FStrings[Index1] := FStrings[Index2];
    FStrings[Index2] := PC;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMUIStrings.Put(Index: Integer; const S: string);
var
  str: string;
begin
  inherited Put(Index, S);
  if (Index >= 0) and (Index < High(FStrings)) then
  begin
    if FStrings[Index]<> nil then
      FreeMem(FStrings[Index]);
    Str := S + #0;
    GetMem(FStrings[Index], Length(Str));
    StrCopy(FStrings[Index], PChar(Str));
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TMUIStrings.Insert(Index: Integer; const S: String);
var
  Str: string;
  i: LongInt;
begin
  inherited;

  SetLength(FStrings, Length(FStrings) + 1);
  for i:= High(FStrings) downto (Index + 1) do
  begin
    FStrings[i] := FStrings[i - 1];
  end;
  Str := S + #0;
  GetMem(FStrings[Index], Length(Str));
  StrCopy(FStrings[Index], PChar(Str));

  if Assigned(FOnChanged) then
    FOnChanged(Self);
end; *)

end.

