{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linking exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit DbgUtilsTypePatternList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

type

  { TDbgTypePatternList

    A list of patterns to match against a target (usually a typename of a watch)
    Special chars:
       "*"   placeholder for 0 or more of any char
       "is:" prefix only, this pattern should also be matched against inherited class type names.

  }

  TDbgTypePatternList = class(TStringList)
  private
    FMatchesExpression: boolean;
    FMatchesInheritedTypes: boolean;
    FMatchesName: boolean;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    function CheckTypeName(ATypeName: String): boolean;
    function CheckUpperCaseTypeName(const AnUpperTypeName: String): boolean; // already upper case
    function CheckInheritedTypeName(ATypeName: String): boolean;
    function CheckUpperCaseName(const AnUpperName: String): boolean;
    function CheckUpperCaseExpression(const AnUpperExpression: String): boolean;

    property MatchesInheritedTypes: boolean read FMatchesInheritedTypes; // perform "is TFooClass"
    property MatchesExpression: boolean read FMatchesExpression; // expr: pattern
    property MatchesName: boolean read FMatchesName; // var: pattern
  end;

implementation

type

  { TDbgTypePattern }

  TDbgTypePattern = class
  private
    FMatchesInheritedTypes: boolean;
    FMatchesExpression: boolean;
    FMatchesName: boolean;
    FPatterType: (ptNever, ptAny, ptExact, ptExactAtStart, ptExactAtEnd, ptSubString, ptMatch);
    FStartsWithMatch, FEndsWithMatch: Boolean;
    FPatternParts: array of string;
  public
    procedure SetPattern(APattern: String);
    function CheckTypeName(const ATypeName: String): boolean;
    property MatchesInheritedTypes: boolean read FMatchesInheritedTypes; // is: pattern // perform "is TFooClass"
    property MatchesExpression: boolean read FMatchesExpression; // expr: pattern
    property MatchesName: boolean read FMatchesName; // var: pattern
  end;

{ TDbgTypePattern }

procedure TDbgTypePattern.SetPattern(APattern: String);
var
  nxt, cur, i: integer;
begin
  APattern := Trim(AnsiUpperCase(APattern));
  FMatchesInheritedTypes := (Length(APattern) >= 3) and
     (APattern[1] = 'I') and (APattern[2] = 'S') and  (APattern[3] = ':');
  if FMatchesInheritedTypes then begin
    Delete(APattern, 1, 3);
    APattern := TrimLeft(APattern);
  end;

  FMatchesExpression := (Length(APattern) >= 5) and
     (APattern[1] = 'E') and (APattern[2] = 'X') and
     (APattern[3] = 'P') and  (APattern[4] = 'R') and  (APattern[5] = ':');
  if FMatchesExpression then begin
    Delete(APattern, 1, 5);
    APattern := TrimLeft(APattern);
  end;

  FMatchesName := (Length(APattern) >= 4) and
     (APattern[1] = 'V') and (APattern[2] = 'A') and  (APattern[3] = 'R') and  (APattern[4] = ':');
  if FMatchesName then begin
    Delete(APattern, 1, 4);
    APattern := TrimLeft(APattern);
  end;

  if APattern = '' then begin
    FPatterType := ptNever;
    SetLength(FPatternParts, 0);
    exit;
  end;

  if APattern = '*' then begin
    FPatterType := ptAny;
    SetLength(FPatternParts, 0);
    exit;
  end;

  FStartsWithMatch := APattern[1] = '*';
  if FStartsWithMatch then begin
    delete(APattern, 1, 1);
    while (APattern <> '') and (APattern[1] = '*') do
      delete(APattern, 1, 1);
  end;

  FEndsWithMatch := (APattern <> '') and (APattern[Length(APattern)] = '*');
  if FEndsWithMatch then begin
    delete(APattern, Length(APattern), 1);
    while (APattern <> '') and (APattern[Length(APattern)] = '*') do
      delete(APattern, Length(APattern), 1);
  end;

  if APattern = '' then begin
    FPatterType := ptAny;
    SetLength(FPatternParts, 0);
    exit;
  end;

  nxt := pos('*', APattern);

  if nxt < 1 then begin
    if FStartsWithMatch       and FEndsWithMatch then
      FPatterType := ptSubString
    else
    if FStartsWithMatch       and (not FEndsWithMatch) then
      FPatterType := ptExactAtEnd
    else
    if (not FStartsWithMatch) and FEndsWithMatch then
      FPatterType := ptExactAtStart
    else
      FPatterType := ptExact;

    SetLength(FPatternParts, 1);
    FPatternParts[0] := APattern;
    exit;
  end;

  FPatterType := ptMatch;
  SetLength(FPatternParts, Length(APattern) div 2 + 1);
  i := 0;
  cur := 1;
  while True do begin
    FPatternParts[i] := Copy(APattern, cur, nxt-cur);
    inc(i);

    cur := nxt + 1;
    while (cur <= Length(APattern)) and (APattern[cur] = '*') do
      inc(cur);
    if cur > Length(APattern) then
      Break;
    nxt := PosEx('*', APattern, cur);
    if nxt < 1 then
      nxt := Length(APattern) + 1;
  end;

  SetLength(FPatternParts, i);
end;

function TDbgTypePattern.CheckTypeName(const ATypeName: String): boolean;
var
  i, j, cur, nxt, max: Integer;
begin
  case FPatterType of
    ptNever:        Result := False;
    ptAny:          Result := True;
    ptExact:        Result := ATypeName = FPatternParts[0];
    ptExactAtStart: Result := strlcomp(pchar(ATypeName), PChar(FPatternParts[0]), length(FPatternParts[0])) = 0;
    ptExactAtEnd:   Result := strlcomp(pchar(ATypeName) + Length(ATypeName) - Length(FPatternParts[0]),
                                       PChar(FPatternParts[0]),
                                       length(FPatternParts[0])) = 0;
    ptSubString:    Result := pos(FPatternParts[0], ATypeName) > 0;
    ptMatch: begin
      Result := False;
      if FStartsWithMatch then begin
        i := 0;
        cur := 1;
      end
      else begin
        if strlcomp(pchar(ATypeName), PChar(FPatternParts[0]), length(FPatternParts[0])) <> 0 then
          exit;
        i := 1;
        cur := 1 + Length(FPatternParts[0]);
      end;

      if FEndsWithMatch then begin
        j := Length(FPatternParts);
        max := Length(ATypeName) + 1;
      end
      else begin
        j := Length(FPatternParts) - 1;
        max := Length(ATypeName) - Length(FPatternParts[j]);
        if strlcomp(pchar(ATypeName) + max,
                          PChar(FPatternParts[j]),
                          length(FPatternParts[j])) <> 0
        then
          exit;
        inc(max);
      end;

      while i < j do begin
        nxt := PosEx(FPatternParts[i], ATypeName, cur);
        if nxt < 1 then
          exit;
        cur := nxt + Length(FPatternParts[i]);
        if cur > max then
          exit;
        inc(i);
      end;

      Result := True;
    end;
  end;
end;

{ TDbgTypePatternList }

procedure TDbgTypePatternList.Put(Index: Integer; const S: string);
var
  p: TDbgTypePattern;
  wasInheritMatch, wasNameMatch, wasExpressionMatch: Boolean;
  i: Integer;
begin
  inherited Put(Index, S);
  p := TDbgTypePattern(Objects[Index]);
  if p = nil then begin
    p := TDbgTypePattern.Create;
    Objects[Index] := p;
  end;
  wasInheritMatch := p.MatchesInheritedTypes;
  wasNameMatch := p.MatchesName;
  wasExpressionMatch := p.MatchesExpression;
  p.SetPattern(S);

  if p.MatchesInheritedTypes then begin
    FMatchesInheritedTypes := True;
  end
  else
  if wasInheritMatch then begin
    i := Count - 1;
    while (i >= 0) and not TDbgTypePattern(Objects[Index]).MatchesInheritedTypes do
      dec(i);
    FMatchesInheritedTypes := i >= 0;
  end;

  if p.MatchesName then begin
    FMatchesName := True;
  end
  else
  if wasInheritMatch then begin
    i := Count - 1;
    while (i >= 0) and not TDbgTypePattern(Objects[Index]).MatchesName do
      dec(i);
    FMatchesName := i >= 0;
  end;

  if p.MatchesExpression then begin
    FMatchesExpression := True;
  end
  else
  if wasInheritMatch then begin
    i := Count - 1;
    while (i >= 0) and not TDbgTypePattern(Objects[Index]).MatchesExpression do
      dec(i);
    FMatchesExpression := i >= 0;
  end;
end;

procedure TDbgTypePatternList.InsertItem(Index: Integer; const S: string);
var
  p: TDbgTypePattern;
begin
  p := TDbgTypePattern.Create;
  inherited InsertItem(Index, S, p);
  p.SetPattern(S);
  if p.MatchesInheritedTypes then
    FMatchesInheritedTypes := True;
  if p.MatchesName then
    FMatchesName := True;
  if p.MatchesExpression then
    FMatchesExpression := True;
end;

procedure TDbgTypePatternList.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  if Index=0 then ;
  if S='' then ;
  if O=nil then ;
  raise Exception.Create('');
end;

function TDbgTypePatternList.AddObject(const S: string; AObject: TObject
  ): Integer;
begin
  if AObject=nil then ;
  Result := Add(S); // used in assign, ignore object from other list
end;

constructor TDbgTypePatternList.Create;
begin
  inherited Create;
  OwnsObjects := True;
end;

function TDbgTypePatternList.CheckTypeName(ATypeName: String): boolean;
var
  i: Integer;
begin
  ATypeName := AnsiUpperCase(ATypeName);
  i := Count - 1;
  while (i >= 0) and not TDbgTypePattern(Objects[i]).CheckTypeName(ATypeName) do
    dec(i);
  Result := i >= 0;
end;

function TDbgTypePatternList.CheckUpperCaseTypeName(const AnUpperTypeName: String): boolean;
var
  i: Integer;
begin
  i := Count - 1;
  while (i >= 0) and not TDbgTypePattern(Objects[i]).CheckTypeName(AnUpperTypeName) do
    dec(i);
  Result := i >= 0;
end;

function TDbgTypePatternList.CheckInheritedTypeName(ATypeName: String): boolean;
var
  i: Integer;
begin
  Result := FMatchesInheritedTypes;
  if not Result then
    exit;
  ATypeName := AnsiUpperCase(ATypeName);
  i := Count - 1;
  while (i >= 0) and
        ( (not TDbgTypePattern(Objects[i]).MatchesInheritedTypes) or
          (not TDbgTypePattern(Objects[i]).CheckTypeName(ATypeName))
        )
  do
    dec(i);
  Result := i >= 0;
end;

function TDbgTypePatternList.CheckUpperCasename(const AnUpperName: String): boolean;
var
  i: Integer;
begin
  Result := FMatchesName;
  if not Result then
    exit;
  i := Count - 1;
  while (i >= 0) and not
        ( TDbgTypePattern(Objects[i]).MatchesName and
          TDbgTypePattern(Objects[i]).CheckTypeName(AnUpperName)
        )
  do
    dec(i);
  Result := i >= 0;
end;

function TDbgTypePatternList.CheckUpperCaseExpression(const AnUpperExpression: String): boolean;
var
  i: Integer;
begin
  Result := FMatchesExpression;
  if not Result then
    exit;
  i := Count - 1;
  while (i >= 0) and not
        ( TDbgTypePattern(Objects[i]).MatchesExpression and
          TDbgTypePattern(Objects[i]).CheckTypeName(AnUpperExpression)
        )
  do
    dec(i);
  Result := i >= 0;
end;

end.

