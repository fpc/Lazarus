{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Functions for string manipulation.

}
unit LazStringUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazUTF8, LazLoggerBase, LazTracer;

type
  // comments
  TCommentType = (
    comtDefault,    // decide automatically
    comtNone,       // no comment
    comtPascal,     // {}
    comtDelphi,     // //
    comtTurboPascal,// (* *)
    comtCPP,        // /* */
    comtPerl,       // #
    comtHtml        // <!-- -->
    );
  TCommentTypes = set of TCommentType;

const
  EndOfLine: shortstring = LineEnding;

function IsNumber(s: String): Boolean;

// Functions for line endings
function LineEndingCount(const Txt: string; var LengthOfLastLine: integer): integer;
function ChangeLineEndings(const s, NewLineEnding: string): string;
function LineBreaksToSystemLineBreaks(const s: string): string;
function LineBreaksToDelimiter(const s: string; Delimiter: char): string;
function ConvertLineEndings(const s: string): string; inline; deprecated 'use LineBreaksToSystemLineBreaks instead';

// Conversions
function TabsToSpaces(const s: string; TabWidth: integer; UseUTF8: boolean): string;
//function CommentLines(const s: string): string;
function CommentText(const s: string; CommentType: TCommentType): string;
//function UncommentLines(const s: string): string;
//function CrossReplaceChars(const Src: string; PrefixChar: char;
//                           const SpecialChars: string): string;
function SimpleSyntaxToRegExpr(const Src: string): string;
function BinaryStrToText(const s: string): string;
function SpecialCharsToSpaces(const s: string; FixUTF8: boolean): string;
function SpecialCharsToHex(const s: string): string;
function BreakString(const s: string; MaxLineLength, Indent: integer): string;

// Conversions to and from a StringList
function SplitString(const s: string; Delimiter: char): TStrings;
procedure SplitString(const s: string; Delimiter: char; AddTo: TStrings;
                      ClearList: boolean = true);
function StringListToText(List: TStrings; const Delimiter: string;
                          IgnoreEmptyLines: boolean = false): string;
function StringListPartToText(List: TStrings; FromIndex, ToIndex: integer;
                              const Delimiter: string;
                              IgnoreEmptyLines: boolean = false): string;
function StringListToString(List: TStrings; FromIndex, ToIndex: integer;
                            IgnoreEmptyLines: boolean = false): string;
procedure StringToStringList(const s: string; List: TStrings);

// Text with delimiters
function GetNextDelimitedItem(const List: string; Delimiter: char;
                              var Position: integer): string;
function HasDelimitedItem(const List: string; Delimiter: char; FindItem: string
                          ): boolean;
function FindNextDelimitedItem(const List: string; Delimiter: char;
                               var Position: integer; FindItem: string): string;
function MergeWithDelimiter(const a, b: string; Delimiter: char): string;


implementation

function IsNumber(s: String): Boolean;
var
  i: Integer;
begin
  i := Length(s);
  while (i >= 1) and (s[i] in ['0'..'9']) do
    dec(i);
  Result := i = 0;
end;

function LineEndingCount(const Txt: string; var LengthOfLastLine: integer): integer;
var
  i, LastLineEndPos, TxtLen: integer;
begin
  i:=1;
  LastLineEndPos:=0;
  Result:=0;
  TxtLen:=length(Txt);
  while i<TxtLen do begin
    if (Txt[i] in [#10,#13]) then begin
      inc(Result);
      inc(i);
      if (i<=TxtLen) and (Txt[i] in [#10,#13]) and (Txt[i-1]<>Txt[i]) then
        inc(i);
      LastLineEndPos:=i-1;
    end else
      inc(i);
  end;
  LengthOfLastLine:=TxtLen-LastLineEndPos;
end;

function ChangeLineEndings(const s, NewLineEnding: string): string;
var
  p, NewLength, EndLen: Integer;
  Src, Dest, EndPos: PChar;
begin
  if s='' then begin
    Result:=s;
    exit;
  end;
  EndLen:=length(NewLineEnding);
  NewLength:=length(s);
  Src:=PChar(s);
  repeat
    case Src^ of
    #0:
      if Src-PChar(s)=length(s) then
        break
      else
        inc(Src);
    #10,#13:
      begin
        if (Src[1] in [#10,#13]) and (Src^<>Src[1]) then begin
          inc(Src,2);
          inc(NewLength,EndLen-2);
        end else begin
          inc(Src);
          inc(NewLength,EndLen-1);
        end;
      end;
    else
      inc(Src);
    end;
  until false;
  SetLength(Result,NewLength);
  Src:=PChar(s);
  Dest:=PChar(Result);
  EndPos:=Dest+NewLength;
  while (Dest<EndPos) do begin
    if Src^ in [#10,#13] then begin
      for p:=1 to EndLen do begin
        Dest^:=NewLineEnding[p];
        inc(Dest);
      end;
      if (Src[1] in [#10,#13]) and (Src^<>Src[1]) then
        inc(Src,2)
      else
        inc(Src);
    end else begin
      Dest^:=Src^;
      inc(Src);
      inc(Dest);
    end;
  end;
  //if Src-1<>@s[length(s)] then RaiseGDBException('');
end;

function LineBreaksToSystemLineBreaks(const s: string): string;
begin
  Result:=ChangeLineEndings(s,LineEnding);
end;

function LineBreaksToDelimiter(const s: string; Delimiter: char): string;
var
  p: Integer;
  StartPos: LongInt;
begin
  Result:=s;
  p:=1;
  while (p<=length(Result)) do begin
    if Result[p] in [#10,#13] then begin
      StartPos:=p;
      repeat
        inc(p);
      until (p>length(Result)) or (not (Result[p] in [#10,#13]));
      if p<=length(Result) then
        Result:=copy(Result,1,StartPos-1)+Delimiter+copy(Result,p,length(Result))
      else
        Result:=copy(Result,1,StartPos-1);
    end else begin
      inc(p);
    end;
  end;
end;

function ConvertLineEndings(const s: string): string;
begin
  Result:=LineBreaksToSystemLineBreaks(s);
end;

function TabsToSpaces(const s: string; TabWidth: integer; UseUTF8: boolean): string;
// Convert all tabs to TabWidth number of spaces.

  function ConvertTabsToSpaces(const Src: string; var Dest: string): integer;
  var
    SrcLen: Integer;
    SrcPos: Integer;
    PhysicalX: Integer;
    CurTabWidth: Integer;
    i: Integer;
    CharLen: Integer;
    DestPos: Integer;
  begin
    //DebugLn('ConvertTabsToSpaces ',dbgs(length(Dest)));
    SrcLen:=length(Src);
    SrcPos:=1;
    DestPos:=1;
    PhysicalX:=1;
    while (SrcPos<=SrcLen) do begin
      if (SrcPos and $fffff)=0 then
        DebugLn('ConvertTabsToSpaces ',dbgs(SrcPos));
      case Src[SrcPos] of
      #9:
        begin
          CurTabWidth:=TabWidth - ((PhysicalX-1) mod TabWidth);
          for i:=1 to CurTabWidth do begin
            if Dest<>'' then
              Dest[DestPos]:=' ';
            inc(DestPos);
          end;
          inc(PhysicalX,CurTabWidth);
          inc(SrcPos);
        end;
      #10,#13:
        begin
          if Dest<>'' then
            Dest[DestPos]:=Src[SrcPos];
          inc(SrcPos);
          inc(DestPos);
          if (SrcPos<=SrcLen) and (s[SrcPos] in [#10,#13])
          and (s[SrcPos-1]<>s[SrcPos]) then
            inc(SrcPos);
          PhysicalX:=1;
        end;
      else
        begin
          if Dest<>'' then
            Dest[DestPos]:=Src[SrcPos];
          inc(PhysicalX);
          if UseUTF8 then
            CharLen:=UTF8CodepointSize(@s[SrcPos])
          else
            CharLen:=1;
          for i:=1 to CharLen do begin
            if Dest<>'' then
              Dest[DestPos]:=Src[SrcPos];
            inc(DestPos);
            inc(SrcPos);
          end;
        end;
      end;
    end;
    Result:=DestPos-1;
  end;

var
  NewLen: LongInt;
begin
  Result:='';
  NewLen:=ConvertTabsToSpaces(s,Result);
  if NewLen=length(s) then
    Result:=s
  else begin
    SetLength(Result,NewLen);
    ConvertTabsToSpaces(s,Result);
  end;
  //DebugLn('TabsToSpaces ',dbgs(length(Result)));
end;
{
function CommentLines(const s: string): string;
// Comment every line with a Delphicomment //
var
  CurPos: integer;
  Dest: string;

  procedure FindLineEnd;
  begin
    while (CurPos<=length(Dest))
    and (not (Dest[CurPos] in [#10,#13])) do
      inc(CurPos);
  end;

  procedure CommentLine;
  begin
    Dest:=LeftStr(Dest,CurPos-1)+'//'+RightStr(Dest,length(Dest)-CurPos+1);
    FindLineEnd;
  end;

begin
  Dest:=s;
  CurPos:=1;
  // find code start in line
  while (CurPos<=length(Dest)) do begin
    case Dest[CurPos] of

    ' ',#9:
      // skip space
      inc(CurPos);

    #10,#13:
      // line end found -> skip
      inc(CurPos);

    else
      // code start found
      CommentLine;
    end;
  end;
  Result:=Dest;
end;
}
function CommentText(const s: string; CommentType: TCommentType): string;
// Comment s.

  procedure GetTextInfo(out Len, LineCount: integer; out LastLineEmpty: boolean);
  var
    p: integer;
  begin
    Len:=length(s);
    LineCount:=1;
    p:=1;
    while p<=Len do
      if not (s[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        inc(p);
        inc(LineCount);
        if (p<=Len) and (s[p] in [#10,#13]) and (s[p]<>s[p-1]) then
          inc(p);
      end;
    LastLineEmpty:=(Len=0) or (s[Len] in [#10,#13]);
  end;

  procedure DoCommentBlock(const FirstLineStart, LineStart, LastLine: string);
  var
    OldLen, NewLen, LineCount, OldPos, NewPos: integer;
    LastLineEmpty: boolean;
  begin
    GetTextInfo(OldLen,LineCount,LastLineEmpty);

    NewLen:=OldLen+length(FirstLineStart)
                  +(LineCount-1)*length(LineStart);
    if LastLineEmpty then
      dec(NewLen,length(LineStart))
    else
      inc(NewLen,length(EndOfLine));
    if (LastLine<>'') then begin
      inc(NewLen,length(LastLine)+length(EndOfLine));
    end;

    SetLength(Result,NewLen);
    NewPos:=1;
    OldPos:=1;

    // add first line start
    if FirstLineStart<>'' then begin
      System.Move(FirstLineStart[1],Result[NewPos],length(FirstLineStart));
      inc(NewPos,length(FirstLineStart));
    end;
    // copy all lines and add new linestart
    while (OldPos<=OldLen) do begin
      if (not (s[OldPos] in [#10,#13])) then begin
        Result[NewPos]:=s[OldPos];
        inc(OldPos);
        inc(NewPos);
      end else begin
        Result[NewPos]:=s[OldPos];
        inc(OldPos);
        inc(NewPos);
        if (OldPos<=OldLen) and (s[OldPos] in [#10,#13])
        and (s[OldPos]<>s[OldPos-1]) then begin
          Result[NewPos]:=s[OldPos];
          inc(OldPos);
          inc(NewPos);
        end;
        // start new line
        if (LineStart<>'') and (OldPos<OldLen) then begin
          System.Move(LineStart[1],Result[NewPos],length(LineStart));
          inc(NewPos,length(LineStart));
        end;
      end;
    end;
    if not LastLineEmpty then begin
      System.Move(EndOfLine[1],Result[NewPos],length(EndOfLine));
      inc(NewPos,length(EndOfLine));
    end;
    // add last line
    if LastLine<>'' then begin
      System.Move(LastLine[1],Result[NewPos],length(LastLine));
      inc(NewPos,length(LastLine));
      System.Move(EndOfLine[1],Result[NewPos],length(EndOfLine));
      inc(NewPos,length(EndOfLine));
    end;
    if NewPos<>NewLen+1 then
      raise Exception.Create('IDEProcs.CommentText ERROR: '
        +IntToStr(NewPos-1)+'<>'+IntToStr(NewLen));
  end;

begin
  Result:=s;
  if CommentType=comtNone then exit;
  if CommentType=comtDefault then CommentType:=comtPascal;

  case CommentType of
    comtPascal: DoCommentBlock('{ ','  ','}');
    comtDelphi: DoCommentBlock('// ','// ','');
    comtTurboPascal: DoCommentBlock('(* ',' * ',' *)');
    comtCPP: DoCommentBlock('/* ',' * ',' */');
    comtPerl: DoCommentBlock('# ','# ','');
    comtHtml: DoCommentBlock('<!-- ','  ','-->');
  end;
end;
{
function UncommentLines(const s: string): string;
// Uncomment every line with a Delphicomment //
var
  CurPos: integer;
  Dest: string;

  procedure FindLineEnd;
  begin
    while (CurPos<=length(Dest))
    and (not (Dest[CurPos] in [#10,#13])) do
      inc(CurPos);
  end;

  procedure UncommentLine;
  begin
    Dest:=LeftStr(Dest,CurPos-1)+RightStr(Dest,length(Dest)-CurPos-1);
    FindLineEnd;
  end;

begin
  Dest:=s;
  CurPos:=1;
  // find Delphi comment line
  while (CurPos<=length(Dest)) do begin
    case Dest[CurPos] of

    ' ',#9:
      // skip space
      inc(CurPos);

    #10,#13:
      // line end found -> skip
      inc(CurPos);

    else
      // code start found
      if (Dest[CurPos]='/') and (CurPos<length(Dest)) and (Dest[CurPos+1]='/')
      then
        UncommentLine;
      FindLineEnd;
    end;
  end;
  Result:=Dest;
end;

function CrossReplaceChars(const Src: string; PrefixChar: char;
  const SpecialChars: string): string;
var
  SrcLen, SrcPos: Integer;
  DestLen: Integer;
  c: Char;
  NeedsChange: boolean;
  DestPos: Integer;
begin
  Result:=Src;
  SrcLen:=length(Src);
  SrcPos:=1;
  DestLen:=SrcLen;
  NeedsChange:=false;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    if (c<>PrefixChar) then begin
      if System.Pos(c,SpecialChars)>=1 then begin
        // in front of each SpecialChar will be a PrefixChar inserted
        inc(DestLen);
        NeedsChange:=true;
      end;
      inc(SrcPos);
    end else begin
      inc(SrcPos);
      if (SrcPos<=SrcLen) and (System.Pos(Src[SrcPos],SpecialChars)>=1) then
      begin
        // each prefixed SpecialChars will be reduced
        dec(DestLen);
        NeedsChange:=true;
      end;
      inc(SrcPos);
    end;
  end;
  if not NeedsChange then exit;
  SetLength(Result,DestLen);
  SrcPos:=1;
  DestPos:=1;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    if (c<>PrefixChar) then begin
      if System.Pos(c,SpecialChars)>=1 then begin
        // in front of each SpecialChars will be PrefixChar inserted
        Result[DestPos]:=PrefixChar;
        inc(DestPos);
      end;
      Result[DestPos]:=c;
      inc(SrcPos);
      inc(DestPos);
    end else begin
      inc(SrcPos);
      if SrcPos<=SrcLen then begin
        if (System.Pos(Src[SrcPos],SpecialChars)<1) then begin
          Result[DestPos]:=c;
          inc(DestPos);
        end;
        Result[DestPos]:=Src[SrcPos];
        inc(DestPos);
        inc(SrcPos);
      end else begin
        Result[DestPos]:=c;
        inc(DestPos);
      end;
    end;
  end;
end;
}
function SimpleSyntaxToRegExpr(const Src: string): string;
// * -> .*
// ? -> .
// , -> |
// ; -> |
// Backslash characters .+
// Finally enclose by ^( )$
var
  SrcLen, SrcPos: Integer;
  DestLen: Integer;
  c: Char;
  DestPos: Integer;
begin
  Result:=Src;
  SrcLen:=length(Src);
  SrcPos:=1;
  DestLen:=SrcLen+4;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    case c of
    '\': inc(SrcPos);
    '*','.','+':
      inc(DestLen);
    end;
    inc(SrcPos);
  end;
  SetLength(Result,DestLen);
  SrcPos:=1;
  Result[1]:='^';
  Result[2]:='(';
  DestPos:=3;
  while (SrcPos<=SrcLen) do begin
    c:=Src[SrcPos];
    case c of
    '\':
      begin
        Result[DestPos]:=c;
        inc(DestPos);
        inc(SrcPos);
        Result[DestPos]:=Src[SrcPos];
        inc(DestPos);
      end;
    '.','+':
      begin
        Result[DestPos]:='\';
        inc(DestPos);
        Result[DestPos]:=c;
        inc(DestPos);
      end;
    '*':
      begin
        Result[DestPos]:='.';
        inc(DestPos);
        Result[DestPos]:='*';
        inc(DestPos);
      end;
    '?':
      begin
        Result[DestPos]:='.';
        inc(DestPos);
      end;
    ',',';':
      begin
        Result[DestPos]:='|';
        inc(DestPos);
      end;
    else
      Result[DestPos]:=Src[SrcPos];
      inc(DestPos);
    end;
    inc(SrcPos);
  end;
  Result[DestPos]:=')';
  inc(DestPos);
  Result[DestPos]:='$';
end;

function BinaryStrToText(const s: string): string;
// Replaces special chars (<#32) into pascal char constants #xxx.
var
  i, OldLen, NewLen, OldPos, NewPos: integer;
begin
  OldLen:=length(s);
  NewLen:=OldLen;
  for i:=1 to OldLen do begin
    if s[i]<' ' then begin
      inc(NewLen); // one additional char for #
      if ord(s[i])>9 then inc(NewLen);
      if ord(s[i])>99 then inc(NewLen);
    end;
  end;
  if OldLen=NewLen then begin
    Result:=s;
    exit;
  end;
  SetLength(Result,NewLen);
  OldPos:=1;
  NewPos:=1;
  while OldPos<=OldLen do begin
    if s[OldPos]>=' ' then begin
      Result[NewPos]:=s[OldPos];
    end else begin
      Result[NewPos]:='#';
      inc(NewPos);
      i:=ord(s[OldPos]);
      if i>99 then begin
        Result[NewPos]:=chr((i div 100)+ord('0'));
        inc(NewPos);
        i:=i mod 100;
      end;
      if i>9 then begin
        Result[NewPos]:=chr((i div 10)+ord('0'));
        inc(NewPos);
        i:=i mod 10;
      end;
      Result[NewPos]:=chr(i+ord('0'));
    end;
    inc(NewPos);
    inc(OldPos);
  end;
  if NewPos-1<>NewLen then
    RaiseGDBException('ERROR: BinaryStrToText: '+IntToStr(NewLen)+'<>'+IntToStr(NewPos-1));
end;

function SpecialCharsToSpaces(const s: string; FixUTF8: boolean): string;
// Converts illegal characters to spaces. Trim leading and trailing spaces.
var
  i: Integer;
  p: LongInt;
begin
  Result:=s;
  if Result='' then exit;
  // convert line breaks to single spaces
  i:=length(Result);
  while (i>=1) do begin
    if Result[i] in [#10,#13] then begin
      Result[i]:=' ';
      p:=i;
      while (i>1) and (Result[i-1] in [#10,#13]) do dec(i);
      if p>i then
        System.Delete(Result,i,p-i);
    end;
    dec(i);
  end;

  // convert special characters to spaces
  for i:=1 to length(Result) do
    if Result[i] in [#0..#31,#127] then Result[i]:=' ';
  if Result='' then exit;
  if FixUTF8 then
    UTF8FixBroken(Result);
  Result:=UTF8Trim(Result);
end;

function SpecialCharsToHex(const s: string): string;
var
  i: Integer;
begin
  Result:=s;
  if Result='' then exit;
  for i:=length(Result) downto 1 do
    if Result[i]<' ' then
      Result:=copy(Result,1,i-1)
              +'#'+Format('%d',[ord(Result[i])])
              +copy(Result,i+1,length(Result));
end;

function BreakString(const s: string; MaxLineLength, Indent: integer): string;
var
  SrcLen: Integer;
  APos: Integer;
  Src: String;
  SplitPos: Integer;
  CurMaxLineLength: Integer;
begin
  Result:='';
  Src:=s;
  CurMaxLineLength:=MaxLineLength;
  if Indent>MaxLineLength-2 then
    Indent:=MaxLineLength-2;
  if Indent<0 then
    MaxLineLength:=0;
  repeat
    SrcLen:=length(Src);
    if SrcLen<=CurMaxLineLength then begin
      Result:=Result+Src;
      break;
    end;
    // split line
    SplitPos:=0;
    // search new line chars
    APos:=1;
    while (APos<=CurMaxLineLength) do begin
      if Src[APos] in [#13,#10] then begin
        SplitPos:=APos;
        break;
      end;
      inc(APos);
    end;
    // search a space boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos-1] in [' ',#9])
        and (not (Src[APos] in [' ',#9])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    // search a word boundary
    if SplitPos=0 then begin
      APos:=CurMaxLineLength;
      while APos>1 do begin
        if (Src[APos] in ['A'..'Z','a'..'z'])
        and (not (Src[APos-1] in ['A'..'Z','a'..'z'])) then begin
          SplitPos:=APos;
          break;
        end;
        dec(APos);
      end;
    end;
    if SplitPos=0 then begin
      // no word boundary found -> split chars
      SplitPos:=CurMaxLineLength;
    end;
    // append part and newline
    if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13]) then begin
      // there is already a new line char at position
      inc(SplitPos);
      if (SplitPos<=SrcLen) and (Src[SplitPos] in [#10,#13])
      and (Src[SplitPos]<>Src[SplitPos-1]) then
        inc(SplitPos);
      Result:=Result+copy(Src,1,SplitPos-1);
    end else begin
      Result:=Result+copy(Src,1,SplitPos-1)+LineEnding;
    end;
    // append indent
    if Indent>0 then
      Result:=Result+StringOfChar(' ',Indent);
    // calculate new LineLength
    CurMaxLineLength:=MaxLineLength-Indent;
    // cut string
    Src:=copy(Src,SplitPos,length(Src)-SplitPos+1);
  until false;
end;

function SplitString(const s: string; Delimiter: char): TStrings;
begin
  Result:=TStringList.Create;
  SplitString(s,Delimiter,Result,false);
end;

procedure SplitString(const s: string; Delimiter: char; AddTo: TStrings;
  ClearList: boolean);
var
  SLen: Integer;
  StartPos: Integer;
  EndPos: Integer;
begin
  if ClearList then
    AddTo.Clear;
  SLen:=length(s);
  StartPos:=1;
  EndPos:=1;
  repeat
    if (EndPos<=sLen) and (s[EndPos]<>Delimiter) then
      inc(EndPos)
    else begin
      if EndPos>StartPos then
        AddTo.Add(copy(s,StartPos,EndPos-StartPos));
      StartPos:=EndPos+1;
      if StartPos>sLen then exit;
      inc(EndPos);
    end;
  until false;
end;

function StringListToText(List: TStrings; const Delimiter: string;
  IgnoreEmptyLines: boolean): string;
begin
  if List=nil then
    Result:=''
  else
    Result:=StringListPartToText(List,0,List.Count-1,Delimiter,IgnoreEmptyLines);
end;

function StringListPartToText(List: TStrings; FromIndex, ToIndex: integer;
  const Delimiter: string; IgnoreEmptyLines: boolean): string;
var
  i: Integer;
  s: string;
  Size: Integer;
  p: Integer;
begin
  if (List=nil) or (FromIndex>ToIndex) or (FromIndex>=List.Count) then begin
    Result:='';
    exit;
  end;
  if FromIndex<0 then FromIndex:=0;
  if ToIndex>=List.Count then ToIndex:=List.Count-1;
  // calculate size
  Size:=0;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if Size>0 then
      inc(Size,length(Delimiter));
    inc(Size,length(s));
  end;
  // build string
  SetLength(Result,Size);
  p:=1;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if (p>1) and (Delimiter<>'') then begin
      System.Move(Delimiter[1],Result[p],length(Delimiter));
      inc(p,length(Delimiter));
    end;
    if s<>'' then begin
      System.Move(s[1],Result[p],length(s));
      inc(p,length(s));
    end;
  end;
end;

function StringListToString(List: TStrings; FromIndex, ToIndex: integer;
  IgnoreEmptyLines: boolean): string;
// concatenates strings with #10 characters
// and quotes strings containing #10 with '
var
  Size: PtrInt;
  i: PtrInt;
  s: string;
  j: PtrInt;
  p: PtrInt;
begin
  if (List=nil) or (FromIndex>ToIndex) or (FromIndex>=List.Count) then begin
    Result:='';
    exit;
  end;
  if FromIndex<0 then FromIndex:=0;
  if ToIndex>=List.Count then ToIndex:=List.Count-1;
  // calculate size
  Size:=0;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if Size>0 then
      inc(Size);// adding #10 as delimiter
    inc(Size,length(s));
    if System.Pos(#10,s)>0 then begin
      inc(Size,2);
      for j:=1 to length(s) do begin
        if s[j]='''' then
          inc(Size);
      end;
    end;
  end;
  // build string
  SetLength(Result,Size);
  p:=1;
  for i:=FromIndex to ToIndex do begin
    s:=List[i];
    if IgnoreEmptyLines and (s='') then continue;
    if p>1 then begin
      Result[p]:=#10;
      inc(p);
    end;
    if System.Pos(#10,s)<1 then begin
      // just copy the string
      if s<>'' then begin
        System.Move(s[1],Result[p],length(s));
        inc(p,length(s));
      end;
    end else begin
      // quote
      Result[p]:='''';
      inc(p);
      for j:=1 to length(s) do begin
        if s[p]='''' then begin
          Result[p]:='''';
          inc(p);
        end;
        Result[p]:=s[j];
        inc(p);
      end;
      Result[p]:='''';
      inc(p);
    end;
  end;
  //DebugLn(['StringListToString ',dbgstr(Result),' ',Size,' ',p]);
  if Size<>p-1 then
    RaiseGDBException('StringListToString');
end;

procedure StringToStringList(const s: string; List: TStrings);
var
  p: PtrInt;
  LineStartPos: PtrInt;
  Size: PtrInt;
  DstPos: PtrInt;
  Line: string;
begin
  if s='' then exit;
  p:=1;
  while true do begin
    if s[p]='''' then begin
      // quoted
      Size:=0;
      inc(p);
      LineStartPos:=p;
      while p<=length(s) do begin
        if (s[p]='''') then begin
          inc(p);
          if (p>length(s)) or (s[p]<>'''') then break;
        end;
        inc(Size);
        inc(p);
      end;
      SetLength(Line,Size);
      p:=LineStartPos;
      DstPos:=1;
      while p<=length(s) do begin
        if (s[p]='''') then begin
          inc(p);
          if (p>length(s)) or (s[p]<>'''') then break;
        end;
        Line[DstPos]:=s[p];
        inc(DstPos);
        inc(p);
      end;
      List.Add(Line);
      // skip line end
      if p>length(s) then exit;
      if s[p]=#10 then
        inc(p);
    end else begin
      // just copy the string
      LineStartPos:=p;
      while (p<=length(s)) and (s[p]<>#10) do inc(p);
      List.Add(copy(s,LineStartPos,p-LineStartPos));
      // skip line end
      if p>length(s) then exit;
      inc(p);
    end;
    if p>length(s) then begin
      List.Add('');
      exit;
    end;
  end;
end;

function GetNextDelimitedItem(const List: string; Delimiter: char;
  var Position: integer): string;
var
  StartPos: LongInt;
begin
  StartPos:=Position;
  while (Position<=length(List)) and (List[Position]<>Delimiter) do
    inc(Position);
  Result:=copy(List,StartPos,Position-StartPos);
  if Position<=length(List) then inc(Position); // skip Delimiter
end;

function HasDelimitedItem(const List: string; Delimiter: char; FindItem: string
  ): boolean;
var
  p: Integer;
begin
  p:=1;
  Result:=FindNextDelimitedItem(List,Delimiter,p,FindItem)<>'';
end;

function FindNextDelimitedItem(const List: string; Delimiter: char;
  var Position: integer; FindItem: string): string;
begin
  while Position<=length(List) do begin
    Result:=GetNextDelimitedItem(List,Delimiter,Position);
    if Result=FindItem then exit;
  end;
  Result:='';
end;

function MergeWithDelimiter(const a, b: string; Delimiter: char): string;
begin
  if a<>'' then begin
    if b<>'' then
      Result:=a+Delimiter+b
    else
      Result:=a;
  end else
    Result:=b;
end;

end.

