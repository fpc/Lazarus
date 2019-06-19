(*
Used by:
- LazLogger
- LazLoggerBase
- LazLoggerDummy
*)

procedure DebuglnStack(const s: string);
begin
  {$IFnDEF LazLoggerDummy}
  DebugLogger.DebuglnStack(s);
  {$ENDIF}
end;

procedure DbgOut(const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(s);
  {$ENDIF}
end;

procedure DbgOut(Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(Args);
  {$ENDIF}
end;

procedure DbgOut(const S: String; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(S, Args);
  {$ENDIF}
end;

procedure DbgOut(const s1, s2: string; const s3: string; const s4: string; const s5: string;
  const s6: string; const s7: string; const s8: string; const s9: string; const s10: string;
  const s11: string; const s12: string; const s13: string; const s14: string;
  const s15: string; const s16: string; const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLn(const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(s);
  {$ENDIF}
end;

procedure DebugLn(Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(Args);
  {$ENDIF}
end;

procedure DebugLn(const S: String; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(S, Args);
  {$ENDIF}
end;

procedure DebugLn(const s1, s2: string; const s3: string; const s4: string; const s5: string;
  const s6: string; const s7: string; const s8: string; const s9: string; const s10: string;
  const s11: string; const s12: string; const s13: string; const s14: string;
  const s15: string; const s16: string; const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLnEnter();
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter();
  {$ENDIF}
end;

procedure DebugLnEnter(const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(s);
  {$ENDIF}
end;

procedure DebugLnEnter(Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(Args);
  {$ENDIF}
end;

procedure DebugLnEnter(s: string; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(s, Args);
  {$ENDIF}
end;

procedure DebugLnEnter(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLnExit();
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit();
  {$ENDIF}
end;

procedure DebugLnExit(const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(s);
  {$ENDIF}
end;

procedure DebugLnExit(Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(Args);
  {$ENDIF}
end;

procedure DebugLnExit(s: string; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(s, Args);
  {$ENDIF}
end;

procedure DebugLnExit(const s1, s2: string; const s3: string; const s4: string;
  const s5: string; const s6: string; const s7: string; const s8: string; const s9: string;
  const s10: string; const s11: string; const s12: string; const s13: string;
  const s14: string; const s15: string; const s16: string; const s17: string;
  const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebuglnStack(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebuglnStack(LogEnabled, s);
  {$ENDIF}
end;

procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(LogEnabled, s);
  {$ENDIF}
end;

procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(LogEnabled, Args);
  {$ENDIF}
end;

procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const S: String; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(LogEnabled, s, Args);
  {$ENDIF}
end;

procedure DbgOut(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DbgOut(LogEnabled, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(LogEnabled, s);
  {$ENDIF}
end;

procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(LogEnabled, Args);
  {$ENDIF}
end;

procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const S: String; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(LogEnabled, s, Args);
  {$ENDIF}
end;

procedure DebugLn(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn(LogEnabled, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(LogEnabled);
  {$ENDIF}
end;

procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(LogEnabled, s);
  {$ENDIF}
end;

procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(LogEnabled, Args);
  {$ENDIF}
end;

procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; s: string; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(LogEnabled, s, Args);
  {$ENDIF}
end;

procedure DebugLnEnter(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnEnter(LogEnabled, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(LogEnabled);
  {$ENDIF}
end;

procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(LogEnabled, s);
  {$ENDIF}
end;

procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(LogEnabled, Args);
  {$ENDIF}
end;

procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; s: string; Args: array of const);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(LogEnabled, s, Args);
  {$ENDIF}
end;

procedure DebugLnExit(LogEnabled: TLazLoggerLogEnabled; const s1, s2: string; const s3: string;
  const s4: string; const s5: string; const s6: string; const s7: string; const s8: string;
  const s9: string; const s10: string; const s11: string; const s12: string;
  const s13: string; const s14: string; const s15: string; const s16: string;
  const s17: string; const s18: string);
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLnExit(LogEnabled, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18);
  {$ENDIF}
end;

function DbgS(const c: cardinal): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=IntToStr(c);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const i: longint): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=IntToStr(i);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const i: int64): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=IntToStr(i);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const q: qword): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=IntToStr(q);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const r: TRect): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='l='+IntToStr(r.Left)+',t='+IntToStr(r.Top)
         +',r='+IntToStr(r.Right)+',b='+IntToStr(r.Bottom);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const p: TPoint): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='(x='+IntToStr(p.x)+',y='+IntToStr(p.y)+')';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const p: pointer): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=HexStr({%H-}PtrUInt(p),2*sizeof(PtrInt));
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const e: extended; MaxDecimals: integer): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=copy(FloatToStr(e),1,MaxDecimals);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const b: boolean): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  if b then Result:='True' else Result:='False';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const m: TMethod): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
var
  o: TObject;
  aMethodName: ShortString;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  o:=TObject(m.Data);
  Result:=dbgsname(o)+'.'+dbgs(m.Code);
  if (o<>nil) and (m.Code<>nil) then begin
    aMethodName:=o.MethodName(m.Code);
    Result:=Result+'='''+aMethodName+'''';
  end;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgSName(const p: TObject): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  if p=nil then
    Result:='nil'
  else if p is TComponent then
    Result:=TComponent(p).Name+':'+p.ClassName
  else
    Result:=p.ClassName;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgSName(const p: TClass): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  if p=nil then
    Result:='nil'
  else
    Result:=p.ClassName;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const ASize: TSize): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result := 'x=' + DbgS(ASize.cx) + ',y=' + DbgS(ASize.cy);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgSJoin(const s1, s2: string): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  if s1 <> '' then
    Result := s1 + ',' + s2
  else
    Result := s2;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const s: TComponentState): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
type
  TComponentStateEnum = low(TComponentState)..high(TComponentState);
  function DbgSComponentStateEnum(const se: TComponentStateEnum): string;
  begin
    WriteStr(Result{%H-}, se);
  end;
var
  i: TComponentStateEnum;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  for i := low(TComponentState) to high(TComponentState) do
    if i in s then
      Result := DbgSJoin(Result, dbgsComponentStateEnum(i));
  Result:='['+Result+']';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function dbgObjMem(AnObject: TObject): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  if AnObject=nil then exit;
  Result:=dbgMemRange(PByte(AnObject),AnObject.InstanceSize);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function dbghex(i: Int64): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
const
  Hex = '0123456789ABCDEF';
var
  Negated: Boolean;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  if i<0 then begin
    Negated:=true;
    i:=-i;
  end else
    Negated:=false;
  repeat
    Result:=Hex[(i mod 16)+1]+Result;
    i:=i div 16;
  until i=0;
  if Negated then
    Result:='-'+Result;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const i1, i2, i3, i4: integer): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:=dbgs(i1)+','+dbgs(i2)+','+dbgs(i3)+','+dbgs(i4);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const Shift: TShiftStateEnum): string;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  WriteStr(Result{%H-}, Shift);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function DbgS(const Shift: TShiftState): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
var
  i: TShiftStateEnum;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  for i := low(TShiftStateEnum) to high(TShiftStateEnum) do
    if i in Shift then
      Result := DbgSJoin(Result, DbgS(i));
  Result:='['+Result+']';
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function dbgMemRange(P: Pointer; Count: integer; Width: integer): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
const
  HexChars: array[0..15] of char = '0123456789ABCDEF';
  LineEnd: shortstring = LineEnding;
var
  i: Integer;
  NewLen: Integer;
  Dest: PChar;
  Col: Integer;
  j: Integer;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  if (p=nil) or (Count<=0) then exit;
  NewLen:=Count*2;
  if Width>0 then begin
    inc(NewLen,(Count div Width)*length(LineEnd));
  end;
  SetLength(Result,NewLen);
  Dest:=PChar(Result);
  Col:=1;
  for i:=0 to Count-1 do begin
    Dest^:=HexChars[PByte(P)[i] shr 4];
    inc(Dest);
    Dest^:=HexChars[PByte(P)[i] and $f];
    inc(Dest);
    inc(Col);
    if (Width>0) and (Col>Width) then begin
      Col:=1;
      for j:=1 to length(LineEnd) do begin
        Dest^:=LineEnd[j];
        inc(Dest);
      end;
    end;
  end;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function dbgMemStream(MemStream: TCustomMemoryStream; Count: integer): string;
{$IFnDEF USED_BY_LAZLOGGER_DUMMY}
var
  s: string;
{$ENDIF}
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  Result:='';
  if (MemStream=nil) or (not (MemStream is TCustomMemoryStream)) or (Count<=0)
  then exit;
  Count:=Min(Count,MemStream.Size);
  if Count<=0 then exit;
  SetLength(s,Count);
  Count:=MemStream.Read(s[1],Count);
  Result:=dbgMemRange(PByte(s),Count);
  {$ELSE}
  Result := '';
  {$ENDIF}
end;


procedure DumpExceptionBackTrace;
begin
  {$IFnDEF USED_BY_LAZLOGGER_DUMMY}
  DebugLogger.DebugLn('  Stack trace:');
  DebugLogger.DumpExceptionBackTrace;
  {$ENDIF}
end;

{$IFDEF USED_BY_LAZLOGGER_DUMMY}
  operator := (g: PLazLoggerLogGroup): TLazLoggerLogEnabled;
  begin
    Result := Default(TLazLoggerLogEnabled);
  end;

  operator := (g: Boolean): TLazLoggerLogEnabled;
  begin
    Result := Default(TLazLoggerLogEnabled);
  end;

  operator and(g1, g2: TLazLoggerLogEnabled): TLazLoggerLogEnabled;
  begin
    Result := Default(TLazLoggerLogEnabled);
  end;

  operator or(g1, g2: TLazLoggerLogEnabled): TLazLoggerLogEnabled;
  begin
    Result := Default(TLazLoggerLogEnabled);
  end;
{$ELSE}
  operator := (g: PLazLoggerLogGroup): TLazLoggerLogEnabled;
  begin
    Result.Enabled := (g = nil) or (g^.Enabled);
    Result.Group := g;
  end;

  operator := (g: Boolean): TLazLoggerLogEnabled;
  begin
    Result.Enabled := g;
    Result.Group := nil;
  end;

  operator and(g1, g2: TLazLoggerLogEnabled): TLazLoggerLogEnabled;
  begin
    Result.Enabled := g1.Enabled and g2.Enabled;
    if (g1.Group = nil) and g1.Enabled then
      Result.Group := g2.Group
    else
    if (g2.Group = nil) and g2.Enabled then
      Result.Group := g1.Group
    else
      Result.Group := nil;
  end;

  operator or(g1, g2: TLazLoggerLogEnabled): TLazLoggerLogEnabled;
  begin
    Result.Enabled := g1.Enabled or g2.Enabled;
    if (g1.Group = nil) then
      Result.Group := g2.Group
    else
    if (g2.Group = nil) then
      Result.Group := g1.Group
    else
      Result.Group := nil;
  end;
{$ENDIF}

