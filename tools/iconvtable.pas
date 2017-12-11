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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
  
  Example:
    ./iconvtable CP1250 UTF-8
    ./iconvtable CP1250 UTF-8
}
program iconvtable;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Unix,
  LazFileUtils, LazUTF8, LazUTF8Classes;

var
  Table: array[0..255] of shortstring;
  SortedTable: array[0..255] of shortstring;

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
      RangeIsString:=true;
    end else begin
      if RangeIsString then
        Result:=Result+'''';
      Result:=Result+'#'+IntToStr(ord(s[i]));
    end;
  end;
  if RangeIsString then
    Result:=Result+'''';
end;

function CompareChars(s1, s2: shortstring): integer;
var
  k: Integer;
begin
  k:=1;
  repeat
    if k>length(s1) then begin
      if k<=length(s2) then
        Result:=1;
      break;
    end else begin
      if k>length(s2) then begin
        Result:=-1;
        break;
      end else begin
        Result:=ord(s1[k])-ord(s2[k]);
        if Result<>0 then break;
      end;
    end;
    inc(k);
  until false;
end;

function StrToTableIndex(const s: shortstring): integer;
begin
  for Result:=0 to 255 do
    if Table[Result]=s then exit;
  Result:=-1;
end;

procedure CreateSortedTable;
var
  i: Integer;
  j: Integer;
  s: shortstring;
begin
  for i:=0 to 255 do
    SortedTable[i]:=Table[i];
  for i:=0 to 254 do
    for j:=i+1 to 255 do
      if CompareChars(SortedTable[i],SortedTable[j])>0 then begin
        s:=SortedTable[i];
        SortedTable[i]:=SortedTable[j];
        SortedTable[j]:=s;
      end;
end;

var
  SL: TStringListUTF8;
  Filename1, Filename2: String;
  FromEncoding, ToEncoding: String;
  s: String;
  UniCode: Cardinal;
  TableIndex, CharLen, i, j, k: Integer;
begin
  // single byte to UTF-8
  if ParamCount=0 then
    raise Exception.Create('Usage: '+ParamStrUTF8(0)+' <encoding>');

  FromEncoding:=ParamStrUTF8(1);
  ToEncoding:='UTF-8';

  SL:=TStringListUTF8.Create;
  for i:=0 to 255 do begin
    Table[i]:=chr(i);
    if i<32 then continue;
    Filename1:='test1.txt';
    Filename2:='test2.txt';
    DeleteFileUTF8(Filename1);
    DeleteFileUTF8(Filename2);
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
  // fill up table
  if ToEncoding='UTF-8' then begin
    for i:=33 to 255 do begin
      if Table[i]<>'' then continue;
      j:=i;
      k:=255;
      s:=UnicodeToUTF8(j);
      while (k>32) do begin
        if Table[i]<>s then
          dec(k)
        else begin
          inc(j);
          s:=UnicodeToUTF8(j);
          k:=255;
        end;
      end;
      Table[i]:=s;
    end;
  end;
  CreateSortedTable;

  // write table: char to shortstring
  writeln('  EncodingToUTF8: array[char] of shortstring = (');
  for i:=0 to 255 do begin
    s:=ToStringConstant(Table[i]);
    if i<255 then s:=s+',';
    s:=s+StringOfChar(' ',20-length(s))+'// '+ToStringConstant(chr(i));
    writeln('    '+s);
  end;
  writeln('  );');

  // write table: unicode to char
  writeln('  case Unicode of');
  writeln('  0..127: Result:=Unicode;');
  i:=0;
  while i<256 do begin
    s:=SortedTable[i];
    if (length(s)=1) and (ord(s[1])<=127) then begin
    end else if s<>'' then begin
      UniCode:=UTF8CodepointToUnicode(@s[1],CharLen);
      TableIndex:=StrToTableIndex(s);
      j:=1;
      while (i+j<256) do begin
        if SortedTable[i+j]='' then break;
        {writeln('DEBUG i=',i,' j=',j,
          ' SortedTable[i]=',ToStringConstant(s),
          ' SortedTable[i+j]=',ToStringConstant(SortedTable[i+j]),
          ' UniCode[i]=',UniCode,
          ' UniCode[i+j]=',UTF8CodepointToUnicode(@SortedTable[i+j][1],CharLen),
          ' TableIndex[i]=',TableIndex,
          ' TableIndex[i+j]=',StrToTableIndex(SortedTable[i+j]),
          '');}
        if UTF8CodepointToUnicode(@SortedTable[i+j][1],CharLen)<>UniCode+j then
          break;
        if StrToTableIndex(SortedTable[i+j])<>TableIndex+j then
          break;
        inc(j);
      end;
      dec(j);
      if j=0 then
        writeln('  '+IntToStr(UniCode)
                +': Result:='+IntToStr(StrToTableIndex(s))+';')
      else if UniCode=TableIndex then
        writeln('  '+IntToStr(UniCode)+'..'+IntToStr(UniCode+j)
                +': Result:=Unicode;')
      else begin
        s:=IntToStr(TableIndex-UniCode);
        if s[1]<>'-' then s:='+'+s;
        writeln('  '+IntToStr(UniCode)+'..'+IntToStr(UniCode+j)
                +': Result:=Unicode'+s+';');
      end;
      inc(i,j);
    end;
    inc(i);
  end;
  writeln('  else Result:=-1;');
  writeln('  end;');

  // create SortedTable
end.

