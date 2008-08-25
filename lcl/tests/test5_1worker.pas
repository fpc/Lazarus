{
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

  This program is used by the TAsyncProcess test.
  It runs endless and writes lines.
}
program test5_1worker;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils; 

var
  j: Integer;
  i: Integer;
  fs: TFileStream;
  s: String;
begin
  j:=0;
  while true do begin
    s:=FormatDateTime('NN:SS.ZZZZ',Now);
    writeln(s,' .............................................................');
    s:=s+LineEnding;
    fs:=TFileStream.Create(UTF8ToSys('worker.log'),fmCreate);
    fs.Position:=fs.Size;
    fs.Write(s[1],length(s));
    fs.Free;
    for i:=0 to 10000000 do begin
      if (i mod 15000)=0 then inc(j);
    end;
  end;
end.

