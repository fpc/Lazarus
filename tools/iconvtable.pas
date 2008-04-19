{ Copyright (C) 2008 Mattias Gaertner

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  Example:
  ppc386 -gl -Fu../lcl/units/i386-linux/ iconvtable.pas && ./iconvtable CP1250 UTF-8
}
program iconvtable;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Unix, LCLProc;

function ToStringConstant(const s: string): string;
var
  i: Integer;
  RangeIsString: Boolean;
begin
  Result:='';
  if s='' then begin
    Result:='''''';
    exit;
  end;
  
  RangeIsString:=false;
  for i:=1 to length(s) do begin
    if s[i] in [#32..#126] then begin
      if not RangeIsString then
        Result:=Result+'''';
      Result:=Result+s[i];
      if s[i]='''' then
        Result:=Result+'''';
    end else begin
      if RangeIsString then
        Result:=Result+'''';
      Result:=Result+'#'+IntToStr(ord(s[i]));
    end;
  end;
  if RangeIsString then
    Result:=Result+'''';
end;

var
  i: Integer;
  Filename1, Filename2: String;
  SL: TStringList;
  FromEncoding: String;
  ToEncoding: String;
  Table: array[0..255] of shortstring;
  s: String;
  UniCode: LongWord;
  CharLen: integer;
begin
  // single byte to UTF-8
  if ParamCount=0 then
    raise Exception.Create('Usage: '+ParamStr(0)+' <encoding>');

  FromEncoding:=ParamStr(1);
  ToEncoding:='UTF-8';

  SL:=TStringList.Create;
  for i:=0 to 255 do begin
    Table[i]:=chr(i);
    if i<32 then continue;
    Filename1:='test1.txt';
    Filename2:='test2.txt';
    DeleteFile(Filename1);
    DeleteFile(Filename2);
    SL.Clear;
    SL.Add(chr(i));
    SL.SaveToFile(Filename1);
    if fpSystem('iconv -f '+FromEncoding+' -t '+ToEncoding+' '+Filename1+' >'+Filename2)=0
    then begin
      SL.LoadFromFile(Filename2);
      Table[i]:=SL[0];
      //writeln(i,'=',length(Table[i]));
    end else begin
      Table[i]:='';
    end;
  end;
  SL.Free;
  
  // write table char to shortstring
  writeln('  EncodingToUTF8: array[char] of shortstring = (');
  for i:=0 to 255 do begin
    s:=ToStringConstant(Table[i]);
    if i<255 then s:=s+',';
    s:=s+StringOfChar(' ',20-length(s))+'// '+ToStringConstant(chr(i));
    writeln('    '+s);
  end;
  writeln('  );');

  // write table unicode to character
  writeln('  case Unicode of');
  writeln('  0..127: Result:=chr(Unicode);');
  for i:=0 to 255 do begin
    s:=Table[i];
    if (length(s)=1) and (ord(s[1])<=127) then begin
      continue;
    end else if s<>'' then begin
      UniCode:=UTF8CharacterToUnicode(@s[1],CharLen);
      writeln('  '+IntToStr(UniCode)+': Result:='+ToStringConstant(chr(i))+';');
    end;
  end;
  writeln('  else Result:='''';');
  writeln('  end;');
end.

