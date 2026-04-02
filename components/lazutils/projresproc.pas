{
 *****************************************************************************
  This file is part of LazUtils

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Abstract:
   A lazarus resource is an ansistring, with a name and a valuetype. Both, name
   and valuetype, are ansistrings as well.
   Lazarus resources are normally included via an include directive in the
   initialization part of a unit. To create such include files use the
   BinaryToLazarusResourceCode procedure.
}
unit ProjResProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// This was moved from LResources.
procedure BinaryToLazarusResourceCode(BinStream, ResStream: TStream;
  const ResourceName, ResourceType: String);

implementation

var
  ByteToStr: array[char] of shortstring;
  ByteToStrValid: boolean=false;

procedure InitByteToStr;
var
  c: Char;
begin
  if ByteToStrValid then exit;
  for c:=Low(char) to High(char) do
    ByteToStr[c]:=IntToStr(ord(c));
  ByteToStrValid:=true;
end;

procedure BinaryToLazarusResourceCode(BinStream,ResStream:TStream;
  const ResourceName, ResourceType: String);
{ example ResStream:
  LazarusResources.Add('ResourceName','ResourceType',
    #123#45#34#78#18#72#45#34#78#18#72#72##45#34#78#45#34#78#184#34#78#145#34#78
    +#83#187#6#78#83
  );
}
const
  ReadBufSize = 4096;
  WriteBufSize = 4096;
var
  s, Indent: string;
  x: integer;
  c: char;
  RangeString, NewRangeString: boolean;
  RightMargin, CurLine: integer;
  WriteBufStart, Writebuf: PChar;
  WriteBufPos: Integer;
  ReadBufStart, ReadBuf: PChar;
  ReadBufPos, ReadBufLen: integer;
  MinCharCount: Integer;

  procedure FillReadBuf;
  begin
    ReadBuf:=ReadBufStart;
    ReadBufPos:=0;
    ReadBufLen:=BinStream.Read(ReadBuf^,ReadBufSize);
  end;

  procedure InitReadBuf;
  begin
    GetMem(ReadBufStart,ReadBufSize);
    FillReadBuf;
  end;

  function ReadChar(var c: char): boolean;
  begin
    if ReadBufPos>=ReadBufLen then begin
      FillReadBuf;
      if ReadBufLen=0 then begin
        Result:=false;
        exit;
      end;
    end;
    c:=ReadBuf^;
    inc(ReadBuf);
    inc(ReadBufPos);
    Result:=true;
  end;

  procedure InitWriteBuf;
  begin
    GetMem(WriteBufStart,WriteBufSize);
    WriteBuf:=WriteBufStart;
    WriteBufPos:=0;
  end;

  procedure FlushWriteBuf;
  begin
    if WriteBufPos>0 then begin
      ResStream.Write(WriteBufStart^,WriteBufPos);
      WriteBuf:=WriteBufStart;
      WriteBufPos:=0;
    end;
  end;

  procedure WriteChar(c: char);
  begin
    WriteBuf^:=c;
    inc(WriteBufPos);
    inc(WriteBuf);
    if WriteBufPos>=WriteBufSize then
      FlushWriteBuf;
  end;

  procedure WriteString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

  procedure WriteShortString(const s: string);
  var
    i: Integer;
  begin
    for i:=1 to length(s) do WriteChar(s[i]);
  end;

begin
  // fpc is not optimized for building a constant string out of thousands of
  // lines. It needs huge amounts of memory and becomes very slow. Therefore big
  // files are split into several strings.

  InitReadBuf;
  InitWriteBuf;
  InitByteToStr;

  Indent:='';
  s:=Indent+'LazarusResources.Add('''+ResourceName+''','''+ResourceType+''',['+LineEnding;
  WriteString(s);
  Indent:='  '+Indent;
  WriteString(Indent);
  x:=length(Indent);
  RangeString:=false;
  CurLine:=1;
  RightMargin:=80;
  if ReadBufLen>0 then begin
    while ReadChar(c{%H-}) do begin
      NewRangeString:=(ord(c)>=32) and (ord(c)<127);
      // check if new char fits into line or if a new line must be started
      if NewRangeString then begin
        if RangeString then
          MinCharCount:=2 // char plus '
        else
          MinCharCount:=3; // ' plus char plus '
        if c='''' then inc(MinCharCount);
      end else begin
        MinCharCount:=1+length(ByteToStr[c]); // # plus number
        if RangeString then
          inc(MinCharCount); // plus ' for ending last string constant
      end;
      if x+MinCharCount>RightMargin then begin
        // break line
        if RangeString then begin
          // end string constant
          WriteChar('''');
        end;
        // write line ending
        WriteShortString(LineEnding);
        x:=0;
        inc(CurLine);
        // write indention
        WriteString(Indent);
        inc(x,length(Indent));
        // write operator
        if (CurLine and 63)<>1 then
          WriteChar('+')
        else
          WriteChar(',');
        inc(x);
        RangeString:=false;
      end;
      // write converted byte
      if RangeString<>NewRangeString then begin
        WriteChar('''');
        inc(x);
      end;
      if NewRangeString then begin
        WriteChar(c);
        inc(x);
        if c='''' then begin
          WriteChar(c);
          inc(x);
        end;
      end else begin
        WriteChar('#');
        inc(x);
        WriteShortString(ByteToStr[c]);
        inc(x,length(ByteToStr[c]));
      end;
      // next
      RangeString:=NewRangeString;
    end;
    if RangeString then begin
      WriteChar('''');
    end;
  end else begin
    WriteShortString('''''');
  end;
  Indent:=copy(Indent,3,length(Indent)-2);
  s:=LineEnding+Indent+']);'+LineEnding;
  WriteString(s);
  FlushWriteBuf;
  FreeMem(ReadBufStart);
  FreeMem(WriteBufStart);
end;

end.

