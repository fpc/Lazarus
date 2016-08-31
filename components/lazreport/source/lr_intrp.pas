
{******************************************}
{                                          }
{             FastReport v2.3              }
{               Interpreter                }
{                                          }
{ Copyright (c) 1998-2000 by Tzyganenko A. }
{                                          }
{******************************************}

unit LR_Intrp;

interface

{$I LR_Vers.inc}

uses Classes, SysUtils, LR_Pars;

type
  TCharArray = array [0..31999] of Char;
  PCharArray = ^TCharArray;

type

  { TfrInterpretator }

  TfrInterpretator = class(TObject)
  private
    Buf: PCharArray;
    Cur:integer;
    Len: Integer;
    procedure SkipSpace;
    function GetToken: String;
    function CopyArr(ACur, ACnt: Integer): String;
  private
    FParser: TfrParser;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetValue(const {%H-}Name: String; var {%H-}Value: Variant); virtual;
    procedure SetValue(const {%H-}Name: String; {%H-}Value: Variant); virtual;
    procedure DoFunction(const {%H-}name: String; {%H-}p1, {%H-}p2, {%H-}p3: Variant;
                         var {%H-}val: Variant); virtual;
    //procedure PrepareScript(MemoFrom, MemoTo, MemoErr: TStringList); virtual;
    procedure PrepareScript(MemoFrom, MemoTo, MemoErr: TStrings); virtual;
    procedure DoScript(Memo: TStringList); virtual;
  end;

  TfrVariables = class(TObject)
  private
    FList: TFpList;
    procedure SetVariable(aName: String; AValue: Variant);
    function GetVariable(aName: String): Variant;
    procedure SetValue(Index: Integer; AValue: Variant);
    function GetValue(Index: Integer): Variant;
    function GetName(Index: Integer): String;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(aName: String): Integer;
    property Variable[aName: String]: Variant
      read GetVariable write SetVariable; default;
    property Value[Index: Integer]: Variant read GetValue write SetValue;
    property Name[Index: Integer]: String read GetName;
    property Count: Integer read GetCount;
  end;


implementation

uses Variants, LR_Const;

type
  LRec = record
    name: String[16];
    n: Integer;
  end;

  PVariable = ^TVariable;
  TVariable = record
    //Name : PString;
    Name : String;
    Value: Variant;
  end;

const
  ttIf    = #1;
  ttGoto  = #2;
  ttProc  = #3;

var
  labels: Array[0..100] of lrec;
  labc: Integer;

{------------------------------------------------------------------------------}
constructor TfrVariables.Create;
begin
  inherited Create;
  FList := TFpList.Create;
end;

destructor TfrVariables.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TfrVariables.Clear;
begin
  while FList.Count > 0 do
    Delete(0);
end;

procedure TfrVariables.SetVariable(aName: String; AValue: Variant);
var
  i: Integer;
  p: PVariable;
begin
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(PVariable(FList[i])^.Name, aName) = 0 then
    begin
      PVariable(FList[i])^.Value := AValue;
      Exit;
    end;
  GetMem(p, SizeOf(TVariable));
  FillChar(p^, SizeOf(TVariable), 0);
  p^.Name := aName; //NewStr(aName);
  p^.Value := AValue;
  FList.Add(p);
end;

function TfrVariables.GetVariable(aName: String): Variant;
var
  i: Integer;
begin
  Result := Null;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(PVariable(FList[i])^.Name, aName) = 0 then
    begin
      Result := PVariable(FList[i])^.Value;
      break;
    end;
end;

procedure TfrVariables.SetValue(Index: Integer; AValue: Variant);
begin
  if (Index < 0) or (Index >= FList.Count) then Exit;
  PVariable(FList[Index])^.Value := AValue;
end;

function TfrVariables.GetValue(Index: Integer): Variant;
begin
  Result := 0;
  if (Index < 0) or (Index >= FList.Count) then Exit;
  Result := PVariable(FList[Index])^.Value;
end;

function TfrVariables.IndexOf(AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
    if AnsiCompareText(PVariable(FList[i])^.Name, AName) = 0 then
    begin
      Result := i;
      break;
    end;
end;

function TfrVariables.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TfrVariables.GetName(Index: Integer): String;
begin
  Result := '';
  if (Index < 0) or (Index >= FList.Count) then Exit;
  Result := PVariable(FList[Index])^.Name;
end;

procedure TfrVariables.Delete(Index: Integer);
var
  p: PVariable;
begin
  if (Index < 0) or (Index >= FList.Count) then Exit;
  p := FList[Index];
  //DisposeStr(p^.Name);
  p^.Name:='';
  p^.Value := 0;
  FreeMem(p, SizeOf(TVariable));
  FList.Delete(Index);
end;

{------------------------------------------------------------------------------}
function Remain(const S: String; From: Integer): String;
begin
  Result := Copy(s, From, Length(s) - 1);
end;

function GetIdentify(const s: String; var i: Integer): String;
var
  k: Integer;
begin
  while (i <= Length(s)) and (s[i] = ' ') do
    Inc(i);
  k := i;
  while (i <= Length(s)) and (s[i] <> ' ') do
    Inc(i);
  Result := Copy(s, k, i - k);
end;

procedure TfrInterpretator.SkipSpace;
begin
  while (Buf^[cur] <= ' ') and (Cur < Len) do Inc(Cur);
end;

function TfrInterpretator.GetToken: String;
var
  ST, SC: Integer;
begin
  if Cur<Len then
  begin
    repeat
      SkipSpace;
      ST := Cur; //Start of token
      SC:=-1;
      while (Buf^[Cur] > ' ') and (Cur < Len) and (SC=-1) do
      begin
        if Buf^[ST] <> '''' then
        begin
          case Buf^[Cur] of
            '{':begin
                  //Skip {...} comment
                  SC := Cur; //Start of comment
                  while (Buf^[Cur] <> '}') and (cur < len) do Inc(cur);
                  Move(Buf^[Cur + 1], Buf^[SC], Len - Cur);
                  Dec(Len, Cur - SC + 1);
                  Cur := SC;
                  Continue;
                end;
            '/':if (Buf^[Cur + 1] = '/') then
                begin
                  //Skip // comment
                  SC:= Cur; //Start of comment
                  while (Buf^[Cur] <> #13) and (Cur < Len) do Inc(Cur);
                  Move(Buf^[Cur + 1], Buf^[SC], Len - Cur);
                  Dec(Len, Cur - SC + 1);
                  Cur := SC;
                  Continue;
                end;
          end
        end;
        Inc(Cur);
      end;
    until (SC=-1) or (Cur>=Len);
    Result := UpperCase(CopyArr(ST, Cur - ST));
  end
  else
    Result:='';
end;

function TfrInterpretator.CopyArr(ACur, ACnt: Integer): String;
begin
  SetLength(Result, ACnt);
  Move(Buf^[ACur], Result[1], ACnt);
end;

{-----------------------------------------------------------------------------}
constructor TfrInterpretator.Create;
begin
  inherited Create;
  FParser := TfrParser.Create;
  FParser.OnGetValue := @GetValue;
  FParser.OnFunction := @DoFunction;
end;

destructor TfrInterpretator.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

//procedure TfrInterpretator.PrepareScript(MemoFrom, MemoTo, MemoErr: TStringList);
procedure TfrInterpretator.PrepareScript(MemoFrom, MemoTo, MemoErr: TStrings);
var
  i, j, lastp: Integer;
  s, bs: String;
  Error: Boolean;

procedure DoCommand; forward;
procedure DoBegin; forward;
procedure DoIf; forward;
procedure DoRepeat; forward;
procedure DoWhile; forward;
procedure DoGoto; forward;
procedure DoEqual; forward;
procedure DoExpression; forward;
procedure DoSExpression; forward;
procedure DoTerm; forward;
procedure DoFactor; forward;
procedure DoVariable; forward;
procedure DoConst; forward;
procedure DoLabel; forward;
procedure DoFunc; forward;
procedure DoFuncId; forward;

  function last: Integer;
  begin
    Result := MemoTo.Count;
  end;

  procedure AddLabel(const s: String; n: Integer);
  var
    i: Integer;
    f: Boolean;
  begin
    f := True;
    for i := 0 to labc - 1 do
      if labels[i].name = s then f := False;
    if f then
    begin
      labels[labc].name := s;
      labels[labc].n := n;
      Inc(labc);
    end;
  end;

  procedure AddError(const s: String);
  var
    i, j, c: Integer;
    s1: String;
  begin
    Error := True;
    cur := lastp;
    SkipSpace;
    c := 0;
    for i := 0 to cur do
      if buf^[i] > ' ' then Inc(c);
    i := 0;
    j := 1;
    while (c > 0) and (i < MemoFrom.Count) do
    begin
      s1 := MemoFrom[i];
      j := 1;
      while (j <= Length(s1)) and (c > 0) do
      begin
        if s1[j] = '{' then break;
        if s1[j] > ' ' then Dec(c);
        Inc(j);
      end;
      if c = 0 then break;
      Inc(i);
    end;
    MemoErr.Add(Format('%s %d/%d: %s', [sErrLine, i+1, j-1, s]));
  end;

  procedure ProcessBrackets(var i: Integer);
  var
    c: Integer;
    fl1, fl2: Boolean;
  begin
    fl1 := True; fl2 := True; c := 0;
    Dec(i);
    repeat
      Inc(i);
      if fl1 and fl2 then
        if buf^[i] = '[' then
          Inc(c) else
          if buf^[i] = ']' then Dec(c);
      if fl1 then
        if buf^[i] = '"' then fl2 := not fl2;
      if fl2 then
        if buf^[i] = '''' then fl1 := not fl1;
    until (c = 0) or (i >= len);
  end;

  {----------------------------------------------}
  procedure DoDigit;
  begin
    while (buf^[cur] = ' ') and (cur < len) do Inc(cur);
    if buf^[cur] in ['0'..'9'] then
      while (buf^[cur] in ['0'..'9']) and (cur < len) do Inc(cur)
    else Error := True;
  end;

  procedure DoBegin;
  label 1;
  begin
  1:DoCommand;
    if Error then Exit;
    lastp := cur;
    bs := GetToken;
    if (bs = '') or (bs[1] = ';') then
    begin
      cur := cur - Length(bs) + 1;
      goto 1;
    end
    else
    if Pos('END', bs) = 1 then
      cur := cur - Length(bs) + 3
    else
      AddError(sErrExpectedEnd);
  end;

  procedure DoIf;
  var
    nsm, nl, nl1: Integer;
  begin
    nsm := cur;
    DoExpression;
    if Error then Exit;
    bs := ttIf + '  ' + CopyArr(nsm, cur - nsm);
    nl := last;
    MemoTo.Add(bs);
    lastp := cur;
    if GetToken = 'THEN' then
    begin
      DoCommand;
      if Error then Exit;
      nsm := cur;
      if GetToken = 'ELSE' then
      begin
        nl1 := last;
        MemoTo.Add(ttGoto + '  ');
        bs := MemoTo[nl]; bs[2] := Chr(last); bs[3] := Chr(last div 256); MemoTo[nl] := bs;
        DoCommand;
        bs := MemoTo[nl1]; bs[2] := Chr(last); bs[3] := Chr(last div 256); MemoTo[nl1] := bs;
      end
      else
      begin
        bs := MemoTo[nl]; bs[2] := Chr(last); bs[3] := Chr(last div 256); MemoTo[nl] := bs;
        cur := nsm;
      end;
    end
    else AddError(sErrExpectedThen);
  end;

  procedure DoRepeat;
  label 1;
  var
    nl, nsm: Integer;
  begin
    nl := last;
  1:DoCommand;
    if Error then Exit;
    lastp := cur;
    bs := GetToken;
    if (bs<>'') and (bs[1] = ';') then
    begin
      cur := cur - Length(bs) + 1;
      goto 1;
    end
    else
    if bs = 'UNTIL' then
    begin
      nsm := cur;
      DoExpression;
      MemoTo.Add(ttIf + Chr(nl) + Chr(nl div 256) + CopyArr(nsm, cur - nsm));
    end
     else AddError(sErrExpectedUntil);
  end;

  procedure DoWhile;
  var
    nl, nsm: Integer;
  begin
    nl := last;
    nsm := cur;
    DoExpression;
    if Error then Exit;
    MemoTo.Add(ttIf + '  ' + CopyArr(nsm, cur - nsm));
    lastp := cur;
    if GetToken = 'DO' then
    begin
      DoCommand;
      MemoTo.Add(ttGoto + Chr(nl) + Chr(nl div 256));
      bs := MemoTo[nl];
      bs[2] := Chr(last);
      bs[3] := Chr(last div 256);
      MemoTo[nl] := bs;
    end
    else
      AddError(sErrExpectedDO);
  end;

  procedure DoGoto;
  var
    nsm: Integer;
  begin
    SkipSpace;
    nsm := cur;
    lastp := cur;
    DoDigit;
    if Error then AddError(sErrLabelGoto);
    MemoTo.Add(ttGoto + Trim(CopyArr(nsm, cur - nsm)));
  end;

  procedure DoEqual;
  var
    s: String;
    n, nsm: Integer;
  begin
    nsm := cur;
    DoVariable;
    s := Trim(CopyArr(nsm, cur - nsm)) + ' ';
    lastp := cur;
    bs := GetToken;
    if (bs = ';') or (bs = '') or (bs = #0) then
    begin
      s := Trim(CopyArr(nsm, lastp - nsm));
      MemoTo.Add(ttProc + s + '(0)');
      cur := lastp;
    end
    else
    if Pos(':=', bs) = 1 then
    begin
      cur := cur - Length(bs) + 2;
      nsm := cur;
      DoExpression;
      n := Pos('[', s);
      if n <> 0 then
      begin
        s := ttProc + 'SETARRAY(' + Copy(s, 1, n - 1) + ', ' +
          Copy(s, n + 1, Length(s) - n - 2) + ', ' + CopyArr(nsm, cur - nsm) + ')';
      end
      else
        s := s + CopyArr(nsm, cur - nsm);
      MemoTo.Add(s);
    end
    else
      AddError(sErrExpectedAssign);
  end;
  {-------------------------------------}
  procedure DoExpression;
  var
    nsm: Integer;
  begin
    DoSExpression;
    nsm := cur;
    bs := GetToken;
    if (Pos('>=', bs) = 1) or (Pos('<=', bs) = 1) or (Pos('<>', bs) = 1) then
    begin
      cur := cur - Length(bs) + 2;
      DoSExpression;
    end
    else
    if (bs<>'') and ((bs[1] = '>') or (bs[1] = '<') or (bs[1] = '=')) then
    begin
      cur := cur - Length(bs) + 1;
      DoSExpression;
    end
    else
      cur := nsm;
  end;

  procedure DoSExpression;
  var
    nsm: Integer;
  begin
    DoTerm;
    nsm := cur;
    bs := GetToken;
    if (bs<>'') and ((bs[1] = '+') or (bs[1] = '-')) then
    begin
      cur := cur - Length(bs) + 1;
      DoSExpression;
    end
    else
    if Pos('OR', bs) = 1 then
    begin
      cur := cur - Length(bs) + 2;
      DoSExpression;
    end
    else
      cur := nsm;
  end;

  procedure DoTerm;
  var
    nsm: Integer;
  begin
    DoFactor;
    nsm := cur;
    bs := GetToken;
    if (bs<>'') and ((bs[1] = '*') or (bs[1] = '/')) then
    begin
      cur := cur - Length(bs) + 1;
      DoTerm;
    end
    else
    if (Pos('AND', bs) = 1) or (Pos('MOD', bs) = 1) then
    begin
      cur := cur - Length(bs) + 3;
      DoTerm;
    end
    else
      cur := nsm;
  end;

  procedure DoFactor;
  var
    nsm: Integer;
  begin
    nsm := cur;
    bs := GetToken;
    if (bs<>'') and (bs[1] = '(') then
    begin
      cur := cur - Length(bs) + 1;
      DoExpression;
      SkipSpace;
      lastp := cur;
      if buf^[cur] = ')' then Inc(cur)
      else AddError(sErrExpectedClosingBracket1);
    end
    else
    if (bs<>'') and (bs[1] = '[') then
    begin
      cur := cur - Length(bs);
      ProcessBrackets(cur);
      SkipSpace;
      lastp := cur;
      if buf^[cur] = ']' then Inc(cur)
      else AddError(sErrExpectedClosingBracket2);
    end
    else
    if (bs<>'') and ((bs[1] = '+') or (bs[1] = '-')) then
    begin
      cur := cur - Length(bs) + 1;
      DoExpression;
    end
    else
    if bs = 'NOT' then
    begin
      cur := cur - Length(bs) + 3;
      DoExpression;
    end
    else
    begin
      cur := nsm;
      DoVariable;
      if Error then
      begin
        Error := False;
        cur := nsm;
        DoConst;
        if Error then
        begin
          Error := False;
          cur := nsm;
          DoFunc;
        end;
      end;
    end;
  end;

  procedure DoVariable;
  begin
    SkipSpace;
    if (buf^[cur] in ['a'..'z', 'A'..'Z']) then
    begin
      Inc(cur);
      while buf^[cur] in ['0'..'9', '_', '.', 'A'..'Z', 'a'..'z'] do Inc(cur);
      if buf^[cur] = '(' then
        Error := True
      else
      if buf^[cur] = '[' then
      begin
        Inc(cur);
        DoExpression;
        if buf^[cur] <> ']' then
          Error := True else
          Inc(cur);
      end;
    end
    else Error := True;
  end;

  procedure DoConst;
  label
    lblLine;
  begin
    SkipSpace;
    if buf^[cur] = #$27 then
    begin

      lblLine:

      Inc(cur);
      while (buf^[cur] <> #$27) and (cur < len) do
      begin
        Inc(cur);
      end;

      if (cur < len) and (buf^[cur + 1] = #$27) then
      begin
        Inc(cur);
        goto lblLine;
      end;

      if cur = len then Error := True
      else Inc(cur);
    end
    else
    begin
      DoDigit;
      if buf^[cur] = '.' then
      begin
        Inc(cur);
        DoDigit;
      end;
    end;
  end;

  procedure DoLabel;
  begin
    DoDigit;
    if buf^[cur] = ':' then Inc(cur)
    else Error := True;
  end;

  procedure DoFunc;
  label 1;
  begin
    DoFuncId;
    if buf^[cur] = '(' then
    begin
      Inc(cur);
  1:  DoExpression;
      lastp := cur;
      SkipSpace;
      if buf^[cur] = ',' then
      begin
        Inc(cur);
        goto 1;
      end
      else if buf^[cur] = ')' then Inc(cur)
      else AddError(sErrExpectedComma);
    end;
  end;

  procedure DoFor;
  var
    nsm, nl: Integer;
    loopvar: String;
    S:string;
  begin
    nsm := cur;
    DoEqual;
    if Error then Exit;
    bs := Trim(CopyArr(nsm, cur - nsm));
    loopvar := Copy(bs, 1, Pos(':=', bs) - 1);
    lastp := cur;

    S:=GetToken;

    if S = 'TO' then
    begin
      nsm := cur;
      DoExpression;
      if Error then Exit;
      nl := last;
      MemoTo.Add(ttIf + '  ' + loopvar + '<=' + CopyArr(nsm, cur - nsm));

      lastp := cur;
      if GetToken = 'DO' then
      begin
        DoCommand;
        if Error then Exit;
        MemoTo.Add(loopvar + ' ' + loopvar + '+1');
        MemoTo.Add(ttGoto + Chr(nl) + Chr(nl div 256));
        bs := MemoTo[nl];
        bs[2] := Chr(last);
        bs[3] := Chr(last div 256);
        MemoTo[nl] := bs;
      end
      else
        AddError(sErrNeedDo);
    end
    else
    if S = 'DOWNTO' then
    begin
      nsm := cur;
      DoExpression;
      if Error then Exit;
      nl := last;
      MemoTo.Add(ttIf + '  ' + loopvar + '>=' + CopyArr(nsm, cur - nsm));

      lastp := cur;
      if GetToken = 'DO' then
      begin
        DoCommand;
        if Error then Exit;
        MemoTo.Add(loopvar + ' ' + loopvar + '-1');
        MemoTo.Add(ttGoto + Chr(nl) + Chr(nl div 256));
        bs := MemoTo[nl];
        bs[2] := Chr(last);
        bs[3] := Chr(last div 256);
        MemoTo[nl] := bs;
      end
      else
        AddError(sErrNeedDo);
    end
    else
      AddError(sErrNeedTo);
  end;


  procedure DoFuncId;
  begin
    SkipSpace;
    if buf^[cur] in ['A'..'Z', 'a'..'z'] then
      while buf^[cur] in ['0'..'9', '_', '.', 'A'..'Z', 'a'..'z'] do Inc(cur)
    else Error := True;
  end;

  procedure DoCommand;
  label 1;
  var
    nsm: Integer;
  begin
  1:Error := False;
    nsm := cur;
    lastp := cur;
    bs := GetToken;
    if bs = 'BEGIN' then DoBegin
    else
    if bs = 'IF' then DoIf
    else
    if bs = 'REPEAT' then DoRepeat
    else
    if bs = 'WHILE' then DoWhile
    else
    if bs = 'GOTO' then DoGoto
    else
    if bs = 'FOR' then
      DoFor
    else
    if (bs = 'END') or (bs = 'END;') then
    begin
      cur := nsm;
      Error := False;
    end
    else
    begin
      cur := nsm;
      DoLabel;
      if Error then
      begin
        Error := False;
        cur := nsm;
        DoVariable;
        if not Error then
        begin
          cur := nsm;
          DoEqual;
        end
        else
        begin
          cur := nsm;
          Error := False;
          DoExpression;
          MemoTo.Add(ttProc + Trim(CopyArr(nsm, cur - nsm)));
        end;
      end
      else
      begin
        AddLabel(Trim(CopyArr(nsm, cur - nsm)), last);
        goto 1;
      end;
    end;
  end;

begin
  Error := False;
  GetMem(buf, 32000);
  FillChar(buf^, 32000, 0);
  len := 0;
  for i := 0 to MemoFrom.Count - 1 do
  begin
    s := ' ' + MemoFrom[i] + #13;

    while Pos(#9, s) <> 0 do
      s[Pos(#9, s)] := ' ';

    Move(S[1], Buf^[len], Length(S));
    Inc(len, Length(s));
  end;
  cur := 0; labc := 0;
  MemoTo.Clear;
  MemoErr.Clear;
  if len > 0 then
    DoCommand;
  FreeMem(buf, 32000);
  for i := 0 to MemoTo.Count - 1 do
    if MemoTo[i][1] = ttGoto then
    begin
      s := Remain(MemoTo[i], 2) + ':';
      for j := 0 to labc do
        if labels[j].name = s then
        begin
          s := MemoTo[i]; s[2] := Chr(labels[j].n);
          s[3] := Chr(labels[j].n div 256); MemoTo[i] := s;
          break;
        end;
    end
    else if MemoTo[i][1] = ttIf then
    begin
      s := FParser.Str2OPZ(Remain(MemoTo[i], 4));
      MemoTo[i] := Copy(MemoTo[i], 1, 3) + s;
    end
    else if MemoTo[i][1] = ttProc then
    begin
      s := FParser.Str2OPZ(Remain(MemoTo[i], 2));
      MemoTo[i] := Copy(MemoTo[i], 1, 1) + s;
    end
    else
    begin
      j := 1;
      GetIdentify(MemoTo[i], j);
      len := j;
      s := FParser.Str2OPZ(Remain(MemoTo[i], j));
      MemoTo[i] := Copy(MemoTo[i], 1, len) + s;
    end;
end;

procedure TfrInterpretator.DoScript(Memo: TStringList);
var
  i, j: Integer;
  s, s1: String;
begin
  i := 0;
  while i < Memo.Count do
  begin
    s := Memo[i];
    j := 1;
    if s[1] = ttIf then
    begin
      if FParser.CalcOPZ(Remain(s, 4)) = 0 then
      begin
        i := Ord(s[2]) + Ord(s[3]) * 256;
        continue;
      end;
    end
    else if s[1] = ttGoto then
    begin
      i := Ord(s[2]) + Ord(s[3]) * 256;
      continue;
    end
    else if s[1] = ttProc then
      FParser.CalcOPZ(Remain(s, 2))
    else
    begin
      s1 := GetIdentify(s, j);
      SetValue(s1, FParser.CalcOPZ(Remain(s, j)));
    end;
    Inc(i);
  end;
end;

procedure TfrInterpretator.GetValue(const Name: String; var Value: Variant);
begin
// abstract method
end;

procedure TfrInterpretator.SetValue(const Name: String; Value: Variant);
begin
// abstract method
end;

procedure TfrInterpretator.DoFunction(const name: String; p1, p2, p3: Variant;
  var val: Variant);
begin
// abstract method
end;

end.
