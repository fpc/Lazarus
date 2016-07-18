{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit lcsvutils;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils;

type
  TCSVRecordProc = procedure(Fields: TStringList) is nested;
  TCSVEncoding = (ceAuto, ceUTF8, ceUTF16, ceUTF16be);

  procedure LoadFromCSVStream(AStream:TStream; AProc: TCSVRecordProc;
    ADelimiter:Char=','; CSVEncoding:TCSVEncoding=ceAuto);
  procedure LoadFromCSVFile(aFilename: string; AProc: TCSVRecordProc;
    ADelimiter:Char=','; CSVEncoding:TCSVEncoding=ceAuto);

implementation

const
  BUFSIZE=1024;
  MAXGROW = 1 shl 29;

type
  TSoc = set of char;

procedure LoadFromCSVStream(AStream: TStream; AProc: TCSVRecordProc;
  ADelimiter:Char; CSVEncoding: TCSVEncoding);
var
  Buffer, curWord: ansistring;
  BytesRead, BufLen, I, BufDelta: Longint;
  leadPtr, tailPtr, wordPtr, X:Pchar;
  Line: TStringList = nil;

  function SkipSet(const aSet: TSoc): boolean;
  begin
    while (leadPtr<tailPtr) and (leadPtr^ in aSet) do Inc(leadPtr);
    result := leadPtr<tailPtr;
  end;

  function FindSet(const aSet: TSoc): boolean;
  begin
    while (leadPtr<tailPtr) and (not (leadPtr^ in ASet)) do Inc(leadPtr);
    result := leadPtr<tailPtr;
  end;

  procedure NotifyLine;
  begin
    if (Line<>nil) and (Line.Count>0) then begin
      AProc(Line);
      Line.Clear;
    end;
  end;

  procedure StorePart;
  var
    Len, AddLen: SizeInt;
  begin
    Len := Length(curWord);
    AddLen := leadPtr-wordPtr;
    if AddLen > 0 then begin
      SetLength(curWord, Len+AddLen);
      Move(wordPtr^, curWord[Len+1], AddLen);
    end;
    if leadPtr<tailPtr then
      Inc(leadPtr);
    wordPtr := leadPtr;
  end;

  procedure StoreWord;
  begin
    StorePart;
    if Line=nil then
      Line := TStringList.Create;
    Line.Add(curWord);
    curWord := '';
  end;

  procedure StoreLine;
  begin
    StoreWord;
    NotifyLine;
  end;

  procedure ProcessEndline;
  var
    le: PChar;
  begin
    le := leadPtr;
    StoreLine;
    if leadPtr>=tailPtr then
      exit;
    if (le^=#13) and (leadPtr^=#10) then
      Inc(leadPtr);
    wordPtr := leadPtr;
  end;

  procedure ProcessQuote;
  var
    endQuote,endField: pchar;
    isDelimiter: boolean;
  begin
    // got a valid opening quote
    Inc(leadPtr);
    wordPtr := leadPtr;
    // look for valid ending quote
    while leadPtr<tailPtr do begin
      if FindSet(['"']) then begin
        // is this an encoded quote?
        if (leadPtr+1)^='"' then begin
          // yes, store part and keep looking
          inc(leadPtr);           // points to second quote
          StorePart;              // add to current word including the first "
        end else begin
          // try to match: "\s*(,|$|EOF)
          endQuote := leadPtr;    // points to valid closing quote (if found later)
          Inc(leadPtr);           // points to \s if exists
          SkipSet([' ']);         // skip \s if exists
          endField := leadPtr;    // points to field terminator
          if (leadPtr>=tailPtr) or (leadPtr^ in [ADelimiter, #10, #13]) then begin
            isDelimiter := (leadPtr<tailPtr) and (leadPtr^=ADelimiter);
            if leadPtr<tailPtr then begin
              if (leadPtr^=#13) and (leadPtr[1]=#10) then
                Inc(endField);    // point to second byte of line ending
              Inc(endField);      // skip last byte of line ending or delimiter
            end;
            leadPtr := endQuote;  // leadPtr points to closing quote
            if isDelimiter then
              StoreWord
            else
              StoreLine;
            leadPtr := endField;  // restore next position
            wordPtr := leadPtr;
            break;
          end;
        end;
      end;
    end;
    if leadPtr<>wordPtr then begin
      StoreLine;
      wordPtr := leadPtr;
    end;
  end;

  procedure ConvertToUTF16;
  var
    n: Integer;
    u: pchar;
    ch: char;
  begin
    n := (tailPtr-leadPtr) div 2;
    u := leadPtr;
    while n>0 do begin
      ch := u^;
      u^ := (u+1)^;
      (u+1)^ := ch;
      inc(u, 2);
      dec(n);
    end;
  end;

  procedure ConvertEncoding;
  var
    W: WideString;
  begin
    if (CSVEncoding=ceAuto) and (BufLen>1) then begin
      if (leadPtr[0]=#$FF) and (leadPtr[1]=#$FE) then begin
        inc(leadPtr,2); // skip little endian UTF-16 BOM
        CSVEncoding := ceUTF16;
      end else
      if (leadPtr[0]=#$FE) and (leadPtr[1]=#$FF) then begin
        inc(leadPtr,2); // skip big endian UTF-16 BOM
        CSVEncoding := ceUTF16be;
      end else
      if (leadPtr[0]<>#$00) and (leadPtr[1]=#$00) then    // quick guess
        CSVEncoding := ceUTF16
      else
      if (leadPtr[0]=#$00) and (leadPtr[1]<>#$00) then    // quick guess
        CSVEncoding := ceUTF16be
    end;
    if (CSVEncoding=ceAuto) and (BufLen>2) then begin
      if (leadPtr[0]=#$EF) and (leadPtr[1]=#$BB) and (leadPtr[2]=#$BF) then
        inc(leadPtr,3); // skip UTF-8 BOM
    end;
    if CSVEncoding=ceAuto then
      CSVEncoding := ceUTF8;

    case CSVEncoding of
      ceUTF16, ceUTF16be:
        begin
          if CSVEncoding=ceUTF16be then
            ConvertToUTF16;
          SetLength(W,(tailPtr-leadPtr) div 2);
          System.Move(leadPtr^,W[1],length(W)*2);
          Buffer := UTF8Encode(W);
          leadPtr := @Buffer[1];
          tailPtr := leadPtr+length(Buffer);
        end;
    end;
  end;

begin

  if AProc=nil then
    exit;

  // read buffer ala fpc tstrings
  Buffer:='';
  BufLen:=0;
  I:=1;
  repeat
    BufDelta:=BUFSIZE*I;
    SetLength(Buffer,BufLen+BufDelta);
    BytesRead:=AStream.Read(Buffer[BufLen+1],BufDelta);
    inc(BufLen,BufDelta);
    If I<MAXGROW then
      I:=I shl 1;
  until BytesRead<>BufDelta;
  BufLen := BufLen-BufDelta+BytesRead;
  SetLength(Buffer, BufLen);
  if BufLen=0 then
    exit;

  curWord := '';
  leadPtr := @Buffer[1];
  tailPtr := leadPtr + BufLen;

  ConvertEncoding;
  // Note: BufLen now invalid and leadPtr points into Buffer, not neccesarily at Buffer[1]

  try
    wordPtr := leadPtr;                    // wordPtr always points to starting word or part
    while leadPtr<tailPtr do begin
      // skip initial spaces
      SkipSet([' ']);
      X := leadPtr;
      // find next marker
      if not FindSet([ADelimiter, '"', #10, #13]) then
        break;
      case leadPtr^ of
        '"':
          begin
            // is the first char?
            if leadPtr=X then
              ProcessQuote
            else begin
              // got an invalid open quote, sync until next delimiter, $ or EOB
              FindSet([ADelimiter, #10, #13]);
              if leadPtr^=ADelimiter then
                StoreWord
              else
                ProcessEndline;
            end;
          end;
        #10, #13:
            ProcessEndline;
        else
          if leadPtr^=ADelimiter then
            StoreWord
      end;
    end;
    if wordPtr<>leadPtr then
      StoreWord;
    NotifyLine;
  finally
    Line.Free;
    SetLength(Buffer,0);
  end;
end;

procedure LoadFromCSVFile(aFilename: string; AProc: TCSVRecordProc;
  ADelimiter: Char; CSVEncoding: TCSVEncoding);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromCSVStream(Stream, AProc, ADelimiter, CSVEncoding);
  finally
    Stream.Free;
  end;
end;

end.

