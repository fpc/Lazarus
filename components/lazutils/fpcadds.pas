{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FPCAdds;

{$mode objfpc}{$H+}{$inline on}

{$i lazutils_defines.inc}

interface

uses
  Classes, SysUtils, SysConst;

type
  TStreamSeekType = int64;
  TMemStreamSeekType = PtrInt;
  TCompareMemSize = PtrUInt;

function GetCompiledTargetOS: string;
function GetCompiledTargetCPU: string;

function AlignToPtr(const p: Pointer): Pointer; inline;
function AlignToInt(const p: Pointer): Pointer; inline;


function TryStrToShortInt(const S: String; out ShInt: ShortInt): Boolean;
function TryStrToSmallInt(const S: String; out SmInt: SmallInt): Boolean;
function TryStrToByte(const S: String; out B: Byte): Boolean;
function TryStrToWord(const S: String; out W: Word): Boolean;

function StrToShortInt(const S: String): ShortInt;
function StrToSmallInt(const S: String): SmallInt;
function StrToByte(const S: String): Byte;
function StrToWord(const S: String): Word;

function StrToShortIntDef(const S: String; Def: ShortInt): ShortInt;
function StrToSmallIntDef(const S: String; Def: SmallInt): SmallInt;
function StrToByteDef(const S: String; Def: Byte): Byte;
function StrToWordDef(const S: String; Def: Word): Word;


implementation

function GetCompiledTargetOS: string;
begin
  Result:=lowerCase({$I %FPCTARGETOS%});
end;

function GetCompiledTargetCPU: string;
begin
  Result:=lowerCase({$I %FPCTARGETCPU%});
end;

function AlignToPtr(const p: Pointer): Pointer; inline;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(Pointer));
{$ELSE}
  Result := p;
{$ENDIF}
end;

function AlignToInt(const p: Pointer): Pointer; inline;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(integer));
{$ELSE}
  Result := p;
{$ENDIF}
end;





function TryStrToShortInt(const S: String; out ShInt: ShortInt): Boolean;
var
  Err: Integer;
  I64: Int64;
begin
  {$if fpc_fullversion < 30301}  //Prevent RangeCheck Error on fpc 3.2.x
  Val(S, I64, Err);
  Result := (Err = 0) and (I64 <= High(ShortInt)) and (I64 >= Low(ShortInt));
  if Result then
    ShInt := I64;
  {$else}
  Val(S, ShInt, Err);
  Result := (Err = 0);
  {$endif < 30301}
end;

function TryStrToSmallInt(const S: String; out SmInt: SmallInt): Boolean;
var
  Err: Integer;
  I64: Int64;
begin
  {$if fpc_fullversion < 30301}  //Prevent RangeCheck Error on fpc 3.2.x
  Val(S, I64, Err);
  Result := (Err = 0) and (I64 <= High(SmallInt)) and (I64 >= Low(SmallInt));
  if Result then
    SmInt := I64;
  {$else}
  Val(S, SmInt, Err);
  Result := (Err = 0);
  {$endif < 30301}
end;

function TryStrToByte(const S: String; out B: Byte): Boolean;
var
  Err: Integer;
  QW: QWord;
begin
  {$if fpc_fullversion < 30301}  //Prevent RangeCheck Error on fpc 3.2.x
  Val(S, QW, Err);
  Result := (Err = 0) and (QW <= High(Byte));
  if Result then
    B := QW;
  {$else}
  {
    Current bug in fpc trunk (3.3.1):
    https://gitlab.com/freepascal.org/fpc/source/-/work_items/41981
    With LongStrings the outcome depends on status of RangeChecking in fpc trunk and may be wrong for S outside the range of Byte (e.g. '256' will return 0)
  }
  Val(ShortString(S), B, Err);
  Result := (Err = 0);
  {$endif < 30301}
end;

function TryStrToWord(const S: String; out W: Word): Boolean;
var
  Err: Integer;
  QW: QWord;
begin
  {$if fpc_fullversion < 30301}  //Prevent RangeCheck Error on fpc 3.2.x
  Val(S, QW, Err);
  Result := (Err = 0) and (QW <= High(Word));
  if Result then
    W := QW;
  {$else}
  {
    Current bug in fpc trunk (3.3.1):
    https://gitlab.com/freepascal.org/fpc/source/-/work_items/41981
    With LongStrings the outcome depends on status of RangeChecking in fpc trunk and may be wrong for S outside the range of Byte (e.g. '65536' will return 0)
  }
  Val(ShortString(S), W, Err);
  Result := (Err = 0);
  {$endif < 30301}
end;

function StrToShortInt(const S: String): ShortInt;
begin
  if not TryStrToShortInt(S, Result) then
    raise EConvertError.createfmt(SInvalidInteger,[S]);
end;

function StrToSmallInt(const S: String): SmallInt;
begin
  if not TryStrToSmallInt(S, Result) then
    raise EConvertError.createfmt(SInvalidInteger,[S]);
end;

function StrToByte(const S: String): Byte;
begin
  if not TryStrToByte(S, Result) then
    raise EConvertError.createfmt(SInvalidInteger,[S]);
end;

function StrToWord(const S: String): Word;
begin
  if not TryStrToWord(S, Result) then
    raise EConvertError.createfmt(SInvalidInteger,[S]);
end;

function StrToShortIntDef(const S: String; Def: ShortInt): ShortInt;
begin
  if not TryStrToShortInt(S, Result) then
    Result := Def;
end;

function StrToSmallIntDef(const S: String; Def: SmallInt): SmallInt;
begin
  if not TryStrToSmallInt(S, Result) then
    Result := Def;
end;

function StrToByteDef(const S: String; Def: Byte): Byte;
begin
  if not TryStrToByte(S, Result) then
    Result := Def;
end;

function StrToWordDef(const S: String; Def: Word): Word;
begin
  if not TryStrToWord(S, Result) then
    Result := Def;
end;



{$ifdef UTF8_RTL}
initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$IFEND}



end.
