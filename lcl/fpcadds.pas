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

function RoundToInt(const e: Extended): integer;
function TruncToInt(const e: Extended): integer;
function StrToDouble(const s: string): double;

implementation

function RoundToInt(const e: Extended): integer;
begin
  Result:=integer(Round(e));
end;

function TruncToInt(const e: Extended): integer;
begin
  Result:=integer(Trunc(e));
end;

function StrToDouble(const s: string): double;
begin
  Result:=Double(StrToFloat(s));
end;

end.

