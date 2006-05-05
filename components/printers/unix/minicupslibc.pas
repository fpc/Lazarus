{
 *****************************************************************************
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
unit MiniCUPSLibc;

{$mode objfpc}{$H+}

interface

const
  clib = 'c';

Type
  Time_t = longint;
  TTime_T = Time_t;
  Ptime_t = ^TTime_T;

type
  Ptm = ^tm;
  tm = record
        tm_sec : longint;
        tm_min : longint;
        tm_hour : longint;
        tm_mday : longint;
        tm_mon : longint;
        tm_year : longint;
        tm_wday : longint;
        tm_yday : longint;
        tm_isdst : longint;
        case boolean of
         false : (tm_gmtoff : longint;tm_zone : Pchar);
         true  : (__tm_gmtoff : longint;__tm_zone : Pchar);
   end;

function __time(__timer:Ptime_t):time_t;cdecl;external clib name 'time';
function localtime(__timer:Ptime_t):Ptm;cdecl;external clib name 'localtime';
function localtime(var __timer : ttime_t):Ptm;cdecl;external clib name 'localtime';

implementation

end.
