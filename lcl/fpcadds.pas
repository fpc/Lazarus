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
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit FPCAdds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// current TStream calculates in int64, old in longint
type
  TStreamSeekType = int64;
  TMemStreamSeekType = integer;
  TCompareMemSize = integer;
  
  {$IFDEF VER1_0}
  PCardinal = ^Cardinal;
  PtrInt = Longint;
  PtrUInt = Cardinal;
  {$ENDIF}
  
  PHandle = ^THandle;
  
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

end.

