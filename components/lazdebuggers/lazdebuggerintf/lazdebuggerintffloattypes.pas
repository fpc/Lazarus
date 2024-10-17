unit LazDebuggerIntfFloatTypes;

{$mode objfpc}{$H+}

interface

{$UNDEF HAS_EXTENDED}
{$UNDEF HAS_SOFT_EXTENDED}
{$UNDEF NO_EXTENDED}

{$IF sizeof(Extended) = 10}
  {$DEFINE HAS_EXTENDED}
{$ELSE}
  {$IFDEF windows}
  {$IF FPC_Fullversion>30202}
    {$DEFINE HAS_SOFT_EXTENDED}
  {$ENDIF}
  {$ENDIF}

  {$IFnDEF HAS_SOFT_EXTENDED}
    {$DEFINE NO_EXTENDED}
  {$ENDIF}
{$ENDIF}


uses
  SysUtils, Math
  {$ifdef HAS_SOFT_EXTENDED} , ufloatx80, sfpux80 {$endif}
  ;

const
  {$ifdef HAS_EXTENDED}
  DBG_HAS_EXTENDED = True;
  DBG_EXTENDED_SIZE = 10;
  {$endif}
  {$ifdef HAS_SOFT_EXTENDED}
  DBG_HAS_EXTENDED = True;
  DBG_EXTENDED_SIZE = 10;
  {$endif}
  {$ifdef NO_EXTENDED}
  DBG_HAS_EXTENDED = False;
  DBG_EXTENDED_SIZE = 0;
  {$endif}

procedure DisableFloatExceptions;
procedure EnableFloatExceptions;

type
  {$ifdef HAS_EXTENDED}
  TDbgExtended = extended;
  {$endif}
  {$ifdef HAS_SOFT_EXTENDED}
  TDbgExtended = type floatx80;
  {$endif}
  {$ifdef NO_EXTENDED}
  TDbgExtended = type double;
  {$endif}

  PDbgExtended = ^TDbgExtended;

  {$ifdef HAS_SOFT_EXTENDED}
  operator+ (const f1,f2 : TDbgExtended) : TDbgExtended;inline;
  operator* (const f1,f2 : TDbgExtended) : TDbgExtended;inline;
  operator- (const f1,f2 : TDbgExtended) : TDbgExtended;inline;
  operator/ (const f1,f2 : TDbgExtended) : TDbgExtended;inline;

  operator :=(const source : double) dest : TDbgExtended;inline;
  operator :=(const source : TDbgExtended) dest : double;inline;
  operator :=(const source : TDbgExtended) dest : single;inline;
  {$endif}

implementation

threadvar
  EM: TFPUExceptionMask;
  {$ifdef HAS_SOFT_EXTENDED}
  SEM: TFPUExceptionMask;
  {$endif}
  FloatExceptionLock: integer;


procedure DisableFloatExceptions;
begin
  if FloatExceptionLock = 0 then begin
    EM := GetExceptionMask;
    SetExceptionMask(EM + [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
    {$ifdef HAS_SOFT_EXTENDED}
    SEM := softfloat_exception_mask;
    softfloat_exception_mask := SEM + [exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision];
    {$endif}
  end;
  inc(FloatExceptionLock);
end;

procedure EnableFloatExceptions;
begin
  dec(FloatExceptionLock);
  if FloatExceptionLock <= 0 then begin
    FloatExceptionLock := 0;
    ClearExceptions(False);
    SetExceptionMask(EM);
    {$ifdef HAS_SOFT_EXTENDED}
    softfloat_exception_mask := SEM;
    {$endif}
  end;

end;

{$ifdef HAS_SOFT_EXTENDED}
operator + (const f1, f2: TDbgExtended): TDbgExtended;
begin
  floatx80(Result) := floatx80(f1) + floatx80(f2);
end;

operator * (const f1, f2: TDbgExtended): TDbgExtended;
begin
  floatx80(Result) := floatx80(f1) * floatx80(f2);
end;

operator - (const f1, f2: TDbgExtended): TDbgExtended;
begin
  floatx80(Result) := floatx80(f1) - floatx80(f2);
end;

operator / (const f1, f2: TDbgExtended): TDbgExtended;
begin
  floatx80(Result) := floatx80(f1) / floatx80(f2);
end;

operator := (const source: double)dest: TDbgExtended;
begin
  floatx80(Result) := source;
end;

operator := (const source: TDbgExtended)dest: double;
begin
  Result := floatx80(source);
end;

operator := (const source: TDbgExtended)dest: single;
var d: double;
begin
  d := floatx80(source);
  Result := d;
end;
{$endif}

end.

