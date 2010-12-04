
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Expression parser            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Pars;

interface

{$I LR_Vers.inc}

type
  TGetPValueEvent = procedure(const s: String; var v: Variant) of object;
  TFunctionEvent = procedure(const AName: String; p1, p2, p3: Variant;
                             var Val: Variant) of object;

  TfrParser = class
  private
    FOnGetValue: TGetPValueEvent;
    FOnFunction: TFunctionEvent;
    function GetIdentify(const s: String; var i: Integer): String;
    function GetString(const s: String; var i: Integer):String;
    procedure Get3Parameters(const s: String; var i: Integer;
      var s1, s2, s3: String);
  public
    function Str2OPZ(s: String): String;
    function CalcOPZ(const s: String): Variant;
    function Calc(const s: String): Variant;
    property OnGetValue: TGetPValueEvent read FOnGetValue write FOnGetValue;
    property OnFunction: TFunctionEvent read FOnFunction write FOnFunction;
  end;

function GetBrackedVariable(s: String; var i, j: Integer): String;

implementation

uses SysUtils, Variants
  {$IFDEF DebugLRCalcs}
  , LCLProc
  {$ENDIF}
  ;

const
  ttGe = #1; ttLe = #2;
  ttNe = #3; ttOr = #4; ttAnd = #5;
  ttInt = #6;  ttFrac = #7;
  ttUnMinus = #9; ttUnPlus = #10; ttStr = #11;
  ttNot = #12; ttMod = #13; ttRound = #14;


function GetBrackedVariable(s: String; var i, j: Integer): String;
var
  c: Integer;
  fl1, fl2: Boolean;
begin
  j := i; fl1 := True; fl2 := True; c := 0;
  Result := '';
  if s = '' then Exit;
  Dec(j);
  repeat
    Inc(j);
    if fl1 and fl2 then
      if s[j] = '[' then
      begin
        if c = 0 then i := j;
        Inc(c);
      end
      else if s[j] = ']' then Dec(c);
    if fl1 then
      if s[j] = '"' then fl2 := not fl2;
    if fl2 then
      if s[j] = '''' then fl1 := not fl1;
  until (c = 0) or (j >= Length(s));
  Result := Copy(s, i + 1, j - i - 1);
end;

function SumOrConcat(const V1,V2: Variant): Variant;
begin
  if VarIsStr(V1) then
    result := V1 + VarToStr(V2)
  else
  if VarIsStr(V2) then
    result := VarToStr(V1) + V2
  else
    result := V1 + V2
end;

function TfrParser.CalcOPZ(const s: String): Variant;
var
  i, j, k, i1, st, ci, cn: Integer;
  s1, s2, s3, s4: String;
  nm: Array[1..32] of Variant;
  v: Double;
begin
  {$IFDEF DebugLRCalcs}
  DebugLnEnter('TfrParser.CalcOPZ INIT s=%s',[dbgstr(s)]);
  {$ENDIF}
  st := 1;
  i := 1;
  nm[1] := 0;
  Result := 0;
  while i <= Length(s) do
  begin
    j := i;
    case s[i] of
      '+':
        nm[st - 2] := SumOrConcat(nm[st - 2], nm[st - 1]);
      ttOr:
        nm[st - 2] := nm[st - 2] or nm[st - 1];
      '-':
        nm[st - 2] := nm[st - 2] - nm[st - 1];
      '*':
        nm[st - 2] := nm[st - 2] * nm[st - 1];
      ttAnd:
        nm[st - 2] := nm[st - 2] and nm[st - 1];
      '/':
        if nm[st - 1] <> 0 then
          nm[st - 2] := nm[st - 2] / nm[st - 1] else
          nm[st - 2] := 0;
      '>':
        if nm[st - 2] > nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      '<':
        if nm[st - 2] < nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      '=':
        if nm[st - 2] = nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttNe:
        if nm[st - 2] <> nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttGe:
        if nm[st - 2] >= nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttLe:
        if nm[st - 2] <= nm[st - 1] then nm[st - 2] := 1
        else nm[st - 2] := 0;
      ttInt:
        begin
          v := nm[st - 1];
          if Abs(Round(v) - v) < 1e-10 then
            v := Round(v) else
            v := Int(v);

          nm[st - 1] := v;
        end;
      ttFrac:
        begin
          v := nm[st - 1];
          if Abs(Round(v) - v) < 1e-10 then
            v := Round(v);

          nm[st - 1] := Frac(v);
        end;
      ttRound:
      begin
        v := Round(Extended(nm[st - 1]));
        nm[st - 1] := v;
      end;
      ttUnMinus:
        nm[st - 1] := -nm[st - 1];
      ttUnPlus:;
      ttStr:
        begin
          if nm[st - 1] <> Null then
            s1 := nm[st - 1] else
            s1 := '';
          nm[st - 1] := s1;
        end;
      ttNot:
        if nm[st - 1] = 0 then nm[st - 1] := 1 else nm[st - 1] := 0;
      ttMod:
        nm[st - 2] := nm[st - 2] mod nm[st - 1];
      ' ': ;
      '[':
        begin
          k := i;
          s1 := GetBrackedVariable(s, k, i);
          if Assigned(FOnGetValue) then
          begin
            nm[st] := Null;
            {$IFDEF DebugLRCalcs}
            DebugLnEnter('TfrParser.CalcOPZ "[" -> FOnGetValue s1=%s',[s1]);
            {$ENDIF}
            FOnGetValue(s1, nm[st]);
            {$IFDEF DebugLRCalcs}
            DebugLnExit('TfrParser.CalcOPZ "[" <- FOnGetValue res=%s',[string(nm[st])]);
            {$ENDIF}
          end;
          Inc(st);
        end
      else
        begin
          if s[i] = '''' then
          begin
            s1 := GetString(s, i);
            s1 := Copy(s1, 2, Length(s1) - 2);
            while Pos('''' + '''', s1) <> 0 do
              Delete(s1, Pos('''' + '''', s1), 1);
            nm[st] := s1;
            k := i;
          end
          else
          begin
            k := i;
            s1 := GetIdentify(s, k);
            if (s1 <> '') and (s1[1] in ['0'..'9', '.', ',']) then
            begin
              for i1 := 1 to Length(s1) do
                if s1[i1] in ['.', ','] then s1[i1] := DecimalSeparator;
              nm[st] := StrToFloat(s1);
            end
            else if AnsiCompareText(s1, 'TRUE') = 0 then
              nm[st] := True
            else if AnsiCompareText(s1, 'FALSE') = 0 then
              nm[st] := False
            else if s[k] = '[' then
            begin
              s1 := 'GETARRAY(' + s1 + ', ' + GetBrackedVariable(s, k, i) + ')';
              nm[st] := Calc(s1);
              k := i;
            end
            else if s[k] = '(' then
            begin
              s1 := AnsiUpperCase(s1);
              Get3Parameters(s, k, s2, s3, s4);
              if s1 = 'COPY' then
              begin
                ci := StrToInt(Calc(s3));
                cn := StrToInt(Calc(s4));
                nm[st] := Copy(Calc(s2), ci, cn);
              end
              else if s1 = 'IF' then
              begin
                if Int(StrToFloat(Calc(s2))) <> 0 then
                  s1 := s3 else
                  s1 := s4;
                {$IFDEF DebugLRCalcs}
                DebugLnEnter('TfrParser.CalcOPZ IF -> S1=%s',[s1]);
                {$ENDIF}
                nm[st] := Calc(s1);
                {$IFDEF DebugLRCalcs}
                DebugLnExit('TfrParser.CalcOPZ IF <- Res=%s',[string(nm[st])]);
                {$ENDIF}
              end
              else if s1 = 'STRTODATE' then
                nm[st] := StrToDate(Calc(s2))
              else if s1 = 'STRTOTIME' then
                nm[st] := StrToTime(Calc(s2))
              else if Assigned(FOnFunction) then
              begin
                nm[st] := Null;
                FOnFunction(s1, s2, s3, s4, nm[st]);
              end;
              Dec(k);
            end
            else
              if Assigned(FOnGetValue) then
              begin
                nm[st] := Null;
                FOnGetValue(AnsiUpperCase(s1), nm[st]);
              end;
          end;
          i := k;
          Inc(st);
        end;
    end;
    if s[j] in ['+', '-', '*', '/', '>', '<', '=', ttGe, ttLe, ttNe,
      ttOr, ttAnd, ttMod] then
      Dec(st);
    Inc(i);
  end;
  Result := nm[1];
  {$IFDEF DebugLRCalcs}
  DebugLnExit('TfrParser.CalcOPZ DONE res=%s',[string(result)]);
  {$ENDIF}
end;

function TfrParser.GetIdentify(const s: String; var i: Integer): String;
var
  k, n: Integer;
begin
  n := 0;
  while (i <= Length(s)) and (s[i] = ' ') do
    Inc(i);
  k := i; Dec(i);
  repeat
    Inc(i);
    while (i <= Length(s)) and
      not (s[i] in [' ', '+', '-', '*', '/', '>', '<', '=', '(', ')']) do
    begin
      if s[i] = '"' then Inc(n);
      Inc(i);
    end;
  until n mod 2 = 0;
  Result := Copy(s, k, i - k);
end;

function TfrParser.GetString(const s: String; var i: Integer): String;
var
  k: Integer;
  f: Boolean;
begin
  k := i; Inc(i);
  repeat
    while (i <= Length(s)) and (s[i] <> '''') do
      Inc(i);
    f := True;
    if (i < Length(s)) and (s[i + 1] = '''') then
    begin
      f := False;
      Inc(i, 2);
    end;
  until f;
  Result := Copy(s, k, i - k + 1);
  Inc(i);
end;

procedure TfrParser.Get3Parameters(const s: String; var i: Integer;
  var s1, s2, s3: String);
var
  c, d, oi, ci: Integer;
begin
  s1 := ''; s2 := ''; s3 := '';
  c := 1; d := 1; oi := i + 1; ci := 1;
  repeat
    Inc(i);
    if s[i] = '''' then
      if d = 1 then Inc(d) else d := 1;
    if d = 1 then
    begin
      if s[i] = '(' then
        Inc(c) else
      if s[i] = ')' then Dec(c);
      if (s[i] = ',') and (c = 1) then
      begin
        if ci = 1 then
          s1 := Copy(s, oi, i - oi) else
          s2 := Copy(s, oi, i - oi);
        oi := i + 1; Inc(ci);
      end;
    end;
  until (c = 0) or (i >= Length(s));
  case ci of
    1: s1 := Copy(s, oi, i - oi);
    2: s2 := Copy(s, oi, i - oi);
    3: s3 := Copy(s, oi, i - oi);
  end;
  if c <> 0 then
    raise Exception.Create('');
  Inc(i);
end;

function TfrParser.Str2OPZ(s: String): String;
label 1;
var
  i, i1, j, p: Integer;
  stack: String;
  res, s1, s2, s3, s4: String;
  vr: Boolean;
  c: Char;

  function Priority(c: Char): Integer;
  begin
    case c of
      '(': Priority := 5;
      ')': Priority := 4;
      '=', '>', '<', ttGe, ttLe, ttNe: Priority := 3;
      '+', '-', ttUnMinus, ttUnPlus: Priority := 2;
      '*', '/', ttOr, ttAnd, ttNot, ttMod: Priority := 1;
      ttInt, ttFrac, ttRound, ttStr: Priority := 0;
      else Priority := 0;
    end;
  end;

  procedure ProcessQuotes(var s: String);
  var
    i: Integer;
  begin
    if (Length(s) = 0) or (s[1] <> '''') then Exit;
    i := 2;
    if Length(s) > 2 then
      while i <= Length(s) do
      begin
        if (s[i] = '''') and (i < Length(s)) then
        begin
          Insert('''', s, i);
          Inc(i);
        end;
        Inc(i);
      end;
  end;

begin
  {$IFDEF DebugLRCalcs}
  DebugLnEnter('TfrParser.Str2OPZ INIT s=%s',[dbgstr(s)]);
  {$ENDIF}
  res := '';
  stack := '';
  i := 1; vr := False;
  while i <= Length(s) do
  begin
    case s[i] of
      '(':
        begin
          stack := '(' + stack;
          vr := False;
        end;
      ')':
        begin
          p := Pos('(', stack);
          res := res + Copy(stack, 1, p - 1);
          stack := Copy(stack, p + 1, Length(stack) - p);
        end;
      '+', '-', '*', '/', '>', '<', '=':
        begin
          if (s[i] = '<') and (s[i + 1] = '>') then
          begin
            Inc(i);
            s[i] := ttNe;
          end else
          if (s[i] = '>') and (s[i + 1] = '=') then
          begin
            Inc(i);
            s[i] := ttGe;
          end else
          if (s[i] = '<') and (s[i + 1] = '=') then
          begin
            Inc(i);
            s[i] := ttLe;
          end;

1:        if not vr then
          begin
            if s[i] = '-' then s[i] := ttUnMinus;
            if s[i] = '+' then s[i] := ttUnPlus;
          end;
          vr := False;
          if stack = '' then stack := s[i] + stack
          else
            if Priority(s[i]) < Priority(stack[1]) then
              stack := s[i] + stack
            else
            begin
              repeat
                res := res + stack[1];
                stack := Copy(stack, 2, Length(stack) - 1);
              until (stack = '') or (Priority(stack[1]) > Priority(s[i]));
              stack := s[i] + stack;
            end;
        end;
      ';': break;
      ' ', #13: ;
      else
      begin
        vr := True;
        s2 := '';
        i1 := i;
        if s[i] = '%' then
        begin
          s2 := '%' + s[i + 1];
          Inc(i, 2);
        end;
        if s[i] = '''' then
          s2 := s2 + GetString(s, i)
        else if s[i] = '[' then
        begin
          s2 := s2 + '[' + GetBrackedVariable(s, i, j) + ']';
          i := j + 1;
        end
        else
        begin
          s2 := s2 + GetIdentify(s, i);
          if s[i] = '[' then
          begin
            s2 := s2 + '[' + GetBrackedVariable(s, i, j) + ']';
            i := j + 1;
          end;
        end;
        c := s[i];
        if (Length(s2) > 0) and (s2[1] in ['0'..'9', '.', ',']) then
          res := res + s2 + ' '
        else
        begin
          s1 := AnsiUpperCase(s2);
          if s1 = 'INT' then
          begin
            s[i - 1] := ttInt;
            Dec(i);
            goto 1;
          end
          else if s1 = 'FRAC' then
          begin
            s[i - 1] := ttFrac;
            Dec(i);
            goto 1;
          end
          else if s1 = 'ROUND' then
          begin
            s[i - 1] := ttRound;
            Dec(i);
            goto 1;
          end
          else if s1 = 'OR' then
          begin
            s[i - 1] := ttOr;
            Dec(i);
            goto 1;
          end
          else if s1 = 'AND' then
          begin
            s[i - 1] := ttAnd;
            Dec(i);
            goto 1;
          end
          else if s1 = 'NOT' then
          begin
            s[i - 1] := ttNot;
            Dec(i);
            goto 1;
          end
          else if s1 = 'STR' then
          begin
            s[i - 1] := ttStr;
            Dec(i);
            goto 1;
          end
          else if s1 = 'MOD' then
          begin
            s[i - 1] := ttMod;
            Dec(i);
            goto 1;
          end
          else if c = '(' then
          begin
            Get3Parameters(s, i, s2, s3, s4);
            res := res + Copy(s, i1, i - i1);
          end
          else res := res + s2 + ' ';
        end;
        Dec(i);
      end;
    end;
    Inc(i);
  end;
  if stack <> '' then res := res + stack;
  Result := res;
  {$IFDEF DebugLRCalcs}
  DebugLnExit('TfrParser.Str2OPZ DONE result=%s',[dbgstr(string(result))]);
  {$ENDIF}
end;

function TfrParser.Calc(const s: String): Variant;
begin
  {$IFDEF DebugLRCalcs}
  DebugLnEnter('TfrParser.Calc INIT s=%s',[dbgstr(s)]);
  {$ENDIF}
  Result := CalcOPZ(Str2OPZ(s));
  {$IFDEF DebugLRCalcs}
  DebugLnExit('TfrParser.Calc DONE res=%s',[string(result)]);
  {$ENDIF}
end;

end.

