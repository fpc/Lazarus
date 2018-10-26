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
  Classes, SysUtils;

type
  TStreamSeekType = int64;
  TMemStreamSeekType = PtrInt;
  TCompareMemSize = PtrUInt;
  PHandle = ^THandle;

function StrToWord(const s: string): word;

function AlignToPtr(const p: Pointer): Pointer; inline;
function AlignToInt(const p: Pointer): Pointer; inline;

implementation

function StrToWord(const s: string): word;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to Length(s) do
    Result:=Result*10+ord(s[i])-ord('0');
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

{$ifdef UTF8_RTL}
initialization
  SetMultiByteConversionCodePage(CP_UTF8);
  // SetMultiByteFileSystemCodePage(CP_UTF8); not needed, this is the default under Windows
  SetMultiByteRTLFileSystemCodePage(CP_UTF8);
{$IFEND}

end.
