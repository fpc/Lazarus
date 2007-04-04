{  $Id$  }
{
 /***************************************************************************
                                FPCAdds.pas
                                -----------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit FPCAdds;

{$mode objfpc}{$H+}{$inline on}

{$IFDEF VER2_3}
{$DEFINE FPC_HAS_QWORDCOMPAREVALUE}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math;

// current TStream calculates in int64, old in longint
type
  TStreamSeekType = int64;
  TMemStreamSeekType = integer;
  TCompareMemSize = integer;
  PHandle = ^THandle;

{$IFNDEF FPC_HAS_QWORDCOMPAREVALUE}
function CompareValue ( const A, B  : QWord) : TValueRelationship; inline;
{$ENDIF}
function StrToWord(const s: string): word;

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

{$IFNDEF FPC_HAS_QWORDCOMPAREVALUE}
function CompareValue ( const A, B  : QWord) : TValueRelationship;
begin
  result:=GreaterThanValue;
  if a=b then
    result:=EqualsValue
  else
   if a<b then
     result:=LessThanValue;
end;
{$ENDIF}

end.
