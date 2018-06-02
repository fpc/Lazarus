{ Copyright (C) 2012 Mattias Gaertner

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
    ./iconvtable_dbcs CP936 UTF-8
}
program iconvtable_dbcs;

{$mode objfpc}{$H+}

uses
  {$IFDEF Unix}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Unix, MTProcs,
  LazFileUtils, LazUTF8, LazUTF8Classes, LazLoggerBase, LConvEncoding;

var
  FromEncoding: String;
  ToEncoding: String;
  DBCSToUTF8: array of cardinal;

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

var
  CritSec: TRTLCriticalSection;
  MaxThreadIndex: integer = 0;
threadvar ThreadIndex: integer;

function GetThreadIndex: integer;
begin
  if ThreadIndex=0 then begin
    EnterCriticalsection(CritSec);
    try
      inc(MaxThreadIndex);
      ThreadIndex:=MaxThreadIndex;
    finally
      LeaveCriticalsection(CritSec);
    end;
  end;
  Result:=ThreadIndex;
end;

procedure AskIconvInParallel(Index: PtrInt; {%H-}Data: Pointer;
  {%H-}Item: TMultiThreadProcItem);
var
  FilenameOrig: String;
  FilenameUTF8: String;
  SL: TStringListUTF8;
  s: String;
  CharLen: integer;
  i: Integer;
begin
  if Index<128 then begin
    // 7bit ASCII characters are represented as single byte (SBCS)
    DBCSToUTF8[Index]:=Index;
    exit;
  end;
  // double byte characters
  DBCSToUTF8[Index]:=0;
  if (Index shr 8)<128 then begin
    exit;
  end;
  i:=GetThreadIndex;
  FilenameOrig:='testorig'+IntToStr(i)+'.txt';
  FilenameUTF8:='testutf'+IntToStr(i)+'.txt';
  DeleteFileUTF8(FilenameOrig);
  DeleteFileUTF8(FilenameUTF8);
  SL:=TStringListUTF8.Create;
  SL.Add(chr(Index shr 8)+chr(Index and 255));
  SL.SaveToFile(FilenameOrig);
  if fpSystem('iconv -f '+FromEncoding+' -t '+ToEncoding+' '+FilenameOrig+' >'+FilenameUTF8)=0
  then begin
    SL.LoadFromFile(FilenameUTF8);
    s:=SL[0];
    if s<>'' then begin
      DBCSToUTF8[Index]:=UTF8CodepointToUnicode(PChar(s),CharLen);
      if CharLen=0 then DBCSToUTF8[Index]:=0;
      writeln(IntToStr(Index)+'='+IntToStr(DBCSToUTF8[Index])+' s='+ToStringConstant(s)+' '+IntToStr(DBCSToUTF8[Index]-DBCSToUTF8[Index-1]-1));
    end;
  end;
  SL.Free;
end;

procedure CreateDBCSToUTF8;
begin
  if length(DBCSToUTF8)>0 then exit;
  SetLength(DBCSToUTF8,65536);
  InitCriticalSection(CritSec);
  ProcThreadPool.DoParallel(@AskIconvInParallel,0,65535,nil,1);
  DoneCriticalsection(CritSec);
end;

procedure WriteRaw(Filename: string);
var
  sl: TStringList;
  i: Integer;
begin
  Filename:=CleanAndExpandFilename(Filename);
  if DirPathExists(Filename) then
    raise Exception.Create('invalid raw file name "'+Filename+'"');
  CreateDBCSToUTF8;
  sl:=TStringList.Create;
  try
    sl.Add('dbcs code point=utf code point');
    for i:=32768 to 65535 do begin
      if DBCSToUTF8[i]>0 then
        sl.Add(IntToStr(i)+'='+IntToStr(DBCSToUTF8[i]));
    end;
    sl.SaveToFile(Filename);
  finally
    sl.Free;
  end;
end;

procedure ReadRaw(Filename: string);
var
  i: Integer;
  sl: TStringList;
  s: String;
  p: SizeInt;
  DBCS: Integer;
  UTF: Integer;
begin
  Filename:=CleanAndExpandFilename(Filename);
  if DirPathExists(Filename) then
    raise Exception.Create('invalid input file name "'+Filename+'"');
  if not FileExistsUTF8(Filename) then
    raise Exception.Create('input file not found "'+Filename+'"');
  SetLength(DBCSToUTF8,65536);
  for i:=0 to 65535 do
    DBCSToUTF8[i]:=0;
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(Filename);
    for s in sl do begin
      p:=Pos('=',s);
      if p<1 then continue;
      DBCS:=StrToIntDef(copy(s,1,p-1),0);
      UTF:=StrToIntDef(copy(s,p+1,10),0);
      if DBCS<1 then continue;
      if UTF<1 then Continue;
      DBCSToUTF8[DBCS]:=UTF;
      writeln('ReadRaw ',DBCS,'=',UTF);
    end;
  finally
    sl.Free;
  end;
end;

procedure Analyze;
const MaxGapSize=8;
var
  i: Integer;
  BlockStart: Integer;
  BlockID: Integer;
  BlockEnd: Integer;
  LastBlockEnd: Integer;
  j: Integer;
  MinValue: Integer;
  MaxValue: Integer;
begin
  MinValue:=0;
  MaxValue:=0;
  for i:=32768 to High(DBCSToUTF8) do begin
    j:=DBCSToUTF8[i];
    if j=0 then continue;
    if (MinValue=0) or (MinValue>j) then MinValue:=j;
    if (MaxValue=0) or (MaxValue<j) then MaxValue:=j;
  end;
  writeln('Analyze Min=',MinValue,' Max=',MaxValue);

  i:=32678;
  LastBlockEnd:=32767;
  BlockID:=0;
  while (i<High(DBCSToUTF8)) do begin
    // search block start
    while (i<High(DBCSToUTF8)) and (DBCSToUTF8[i]=0) do inc(i);
    BlockStart:=i;
    // search block end
    while (i<High(DBCSToUTF8)) do begin
      if DBCSToUTF8[i]=0 then begin
        BlockEnd:=i-1;
        j:=1;
        while (j<MaxGapSize) and (i<High(DBCSToUTF8))
        and (DBCSToUTF8[i]=0) do begin
          inc(i);
          inc(j);
        end;
        if (i=High(DBCSToUTF8)) or (j=MaxGapSize) then begin
          // block end found
          writeln('Analyze ',BlockID,' ',BlockStart,'..',BlockEnd,
            ' len=',BlockEnd-BlockStart+1,' gap=',BlockStart-LastBlockEnd-1);
          inc(BlockID);
          LastBlockEnd:=BlockEnd;
          break;
        end;
      end;
      inc(i);
    end;
  end;
end;

const
  ParamRaw = '--raw=';
  ParamInput='--input=';
procedure WriteUsage;
begin
  writeln('Usage: '+ParamStrUTF8(0)+' <encoding> [',ParamRaw,'|',ParamInput,'<filename>]');
  writeln('  encoding: a dbcs encoding like cp936');
  writeln;
  writeln('  ',ParamRaw,'<filename> write a list of lines of dbcs=unicode code points');
  writeln('  ',ParamInput,'<filename> read a list created by ',ParamRaw);
  Halt(1);
end;

var
  s: String;
begin
  if ParamCount=0 then
    WriteUsage;

  FromEncoding:=ParamStrUTF8(1);
  ToEncoding:='UTF-8';

  s:=ParamStrUTF8(2);
  if LeftStr(s,length(ParamRaw))=ParamRaw then begin
    Delete(s,1,length(ParamRaw));
    WriteRaw(s);
  end else if LeftStr(s,length(ParamInput))=ParamInput then begin
    Delete(s,1,length(ParamInput));
    ReadRaw(s);
    Analyze;

  end else begin
    WriteUsage;
  end;


  // write table: char to shortstring
  {writeln('  EncodingToUTF8: array[char] of shortstring = (');
  for i:=0 to 255 do begin
    s:=ToStringConstant(Table[i]);
    if i<255 then s:=s+',';
    s:=s+StringOfChar(' ',20-length(s))+'// '+ToStringConstant(chr(i));
    writeln('    '+s);
  end;
  writeln('  );');}

  // write table: unicode to char
  {writeln('  case Unicode of');
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
        (*writeln('DEBUG i=',i,' j=',j,
          ' SortedTable[i]=',ToStringConstant(s),
          ' SortedTable[i+j]=',ToStringConstant(SortedTable[i+j]),
          ' UniCode[i]=',UniCode,
          ' UniCode[i+j]=',UTF8CodepointToUnicode(@SortedTable[i+j][1],CharLen),
          ' TableIndex[i]=',TableIndex,
          ' TableIndex[i+j]=',StrToTableIndex(SortedTable[i+j]),
          '');*)
        if integer(UTF8CodepointToUnicode(@SortedTable[i+j][1],CharLen))<>UniCode+j then
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
      else
        writeln('  '+IntToStr(UniCode)+'..'+IntToStr(UniCode+j)
                +': Result:=Unicode-'+IntToStr(UniCode-TableIndex)+';');
      inc(i,j);
    end;
    inc(i);
  end;
  writeln('  else Result:=-1;');
  writeln('  end;');}
end.

