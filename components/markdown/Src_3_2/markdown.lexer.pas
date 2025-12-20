unit markdown.lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, markdown.elements;

type
  { TMarkDownTextLexer }
  TMarkDownTextLexer = class
  private
    FText : String;
    FCursor : integer;
    FMark  : Integer;
    FBuilder : TStringBuilder;
    FMarkPos : TPosition;
    FPos : TPosition;
    FStartLine : integer;
    function GetDone: boolean;
    function GetPeek: char;
    function GetPeekNext: char;
    function GetPeekLast: char;
    function GetPeekEndRun: char;
  public
    constructor Create(aText : String; aStartline : integer);
    destructor Destroy; override;
    procedure mark;
    procedure rewind;
    function peekRun(checkBefore : boolean): String;
    function peekLen(aLength : integer) : String;
    function peekUntil(chs : TSysCharSet) : String;
    function peekWhile(chs : TSysCharSet) : String;
    function NextChar : char;
    function NextEquals : String; overload;
    function NextChars(aCount : integer) : String;
    function has(s : string) : boolean;
    function runExistsAfter(s : String) : boolean;
    function runExistsAfterBeforeChar(s : String; c : char) : boolean;
    procedure skipWhitespace;
    function location : TPosition;
    property done : boolean read GetDone;
    property peek : char read GetPeek;
    property peekNext : char read GetPeekNext;
    property peekLast : char read GetPeekLast;
    property peekEndRun : char read GetPeekEndRun;
  end;

implementation

uses markdown.utils;

{ TMarkDownTextLexer }

constructor TMarkDownTextLexer.Create(aText: String; aStartline : integer);
begin
  inherited Create;
  FText := aText;
  FCursor := 1;
  FBuilder := TStringBuilder.create;
  FStartLine := aStartline;
  FPos.Line := 0;
  FPos.Col := 1;
end;

destructor TMarkDownTextLexer.Destroy;
begin
  FBuilder.Free;
  inherited;
end;

procedure TMarkDownTextLexer.rewind;
begin
  FCursor := FMark;
  FPos := FMarkPos;
end;

function TMarkDownTextLexer.runExistsAfter(s: String): boolean;
var
  i, len : integer;
begin
  len := Length(S);
  i := FCursor + len + 1;
  result := false;
  while i+len <= Length(FText) + 1 do
  begin
    if (copy(FText, i, len) = s) and (FText[i-1] <> s[1]) and (((i+len) = (Length(FText)+1)) or (FText[i+len] <> s[1])) then
      exit(true);
    inc(i);
  end;
end;

function TMarkDownTextLexer.runExistsAfterBeforeChar(s: String; c : char): boolean;
var
  i, len : integer;
begin
  len := Length(s);
  i := FCursor + len;
  result := false;
  while i+len <= Length(FText) + 1 do
  begin
    if FText[i] = c then
      exit(false);
    if (copy(FText, i, len) = s) and (FText[i-1] <> s[1]) and ((i+len = Length(FText)+1) or (FText[i+len] <> s[1])) then
      exit(true);
    inc(i);
  end;
end;

procedure TMarkDownTextLexer.skipWhitespace;
begin
  while isWhitespaceChar(peek) do
    NextChar();
end;

function TMarkDownTextLexer.GetDone: boolean;
begin
  result := FCursor > Length(FText);
end;

function TMarkDownTextLexer.GetPeek: char;
begin
  if done then
    result := #0
  else
    result := FText[FCursor];
end;

function TMarkDownTextLexer.GetPeekEndRun: char;
var
  i : integer;
  c : char;
begin
  if FCursor >= Length(FText) then
    result := #0
  else
  begin
    i := FCursor;
    c := FText[i];
    while (i <= Length(FText)) and (FText[i] = c) do
      inc(i);
    if i > Length(FText) then
      result := #0
    else
      result := FText[i];
  end;
end;

function TMarkDownTextLexer.GetPeekLast: char;
begin
  if FCursor = 1 then
    result := #0
  else
    result := FText[FCursor-1];
end;

function TMarkDownTextLexer.GetPeekNext: char;
begin
  if FCursor >= Length(FText) then
    result := #0
  else
    result := FText[FCursor+1];
end;

function TMarkDownTextLexer.peekRun(checkBefore : boolean): String;
var
  i : integer;
  c : char;
begin
  FBuilder.clear;
  c := peek;
  i := FCursor;
  if checkBefore and (i > 2) and (FText[i-1] = c) then
    exit('');

  while (i <= Length(FText)) and (FText[i] = c) do
  begin
    FBuilder.Append(c);
    inc(i);
  end;
  result := FBuilder.ToString;
end;

function TMarkDownTextLexer.NextChars(aCount: integer): String;
var
  i,lCount : integer;
begin
  Result:='';
  SetLength(Result,aCount);
  lCount:=0;
  for i := 1 to aCount do
    begin
    if not done then
      begin
      Result[I]:=NextChar();
      inc(lCount);
      end;
    end;
  if lCount<aCount then
    SetLength(Result,lCount);
end;

function TMarkDownTextLexer.NextEquals: String;
const
  Delta = 10;
var
  c : char;
  lLen,lCount : Integer;
begin
  Result:='';
  lLen:=0;
  lCount:=0;
  c := peek;
  while peek = c do
    begin
    if lCount=lLen then
      begin
      inc(lLen,Delta);
      SetLength(Result,LLen);
      end;
    inc(lCount);
    Result[lCount]:=nextChar;
    end;
  SetLength(Result,lCount);
end;

function TMarkDownTextLexer.NextChar: char;

begin
  if done then
    result := #0
  else
  begin
    result := FText[FCursor];
    inc(FCursor);
    if result = #10 then
    begin
      FPos.Col := 1;
      inc(FPos.Line);
    end
    else
      inc(FPos.Col);
  end;
end;

function TMarkDownTextLexer.has(s: string): boolean;
begin
  result := peekLen(Length(s)) = s;
end;

function TMarkDownTextLexer.location: TPosition;
begin
  result:=fPos;
  inc(Result.line,FStartLine);
end;

procedure TMarkDownTextLexer.mark;
begin
  FMark := FCursor;
  FMarkPos:=FPos;
end;

function TMarkDownTextLexer.peekUntil(chs : TSysCharSet) : String;
var
  i : integer;
begin
  FBuilder.clear;
  i := FCursor;
  while (i <= Length(FText)) and not CharInSet(FText[i], chs) do
  begin
    FBuilder.Append(FText[i]);
    inc(i);
  end;
  if (i > Length(FText)) then
    result := ''
  else
    result := FBuilder.ToString;
end;

function TMarkDownTextLexer.peekWhile(chs: TSysCharSet): String;
var
  i : integer;
begin
  FBuilder.clear;
  i := FCursor;
  while (i <= Length(FText)) and CharInSet(FText[i], chs) do
  begin
    FBuilder.Append(FText[i]);
    inc(i);
  end;
  result := FBuilder.ToString;
end;

function TMarkDownTextLexer.peekLen(aLength: integer): String;
var
  i : integer;
begin
  FBuilder.clear;
  for i := FCursor to FCursor+aLength-1 do
  begin
    if i <= Length(FText) then
      FBuilder.Append(FText[i]);
  end;
  result := FBuilder.ToString;
end;

end.

