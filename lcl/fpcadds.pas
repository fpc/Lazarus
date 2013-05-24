{  $Id$  }
{
 /***************************************************************************
                                FPCAdds.pas
                                -----------

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit FPCAdds;

{$mode objfpc}{$H+}{$inline on}

interface

uses
  Classes, SysUtils, Math;

// current TStream calculates in int64, old in longint
type
  TStreamSeekType = int64;
  TMemStreamSeekType = integer;
  TCompareMemSize = integer;
  PHandle = ^THandle;

function StrToWord(const s: string): word;

function AlignToPtr(const p: Pointer): Pointer;
function AlignToInt(const p: Pointer): Pointer;

implementation

function StrToWord(const s: string): word;
var
  p: Integer;
begin
  Result:=0;
  p:=1;
  while (p<=length(s)) do begin
    Result:=Result*10+ord(s[p])-ord('0');
    inc(p);
  end;
end;

function AlignToPtr(const p: Pointer): Pointer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(Pointer));
{$ELSE}
  Result := p;
{$ENDIF}
end;

function AlignToInt(const p: Pointer): Pointer;
begin
{$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Result := Align(p, SizeOf(integer));
{$ELSE}
  Result := p;
{$ENDIF}
end;

end.
