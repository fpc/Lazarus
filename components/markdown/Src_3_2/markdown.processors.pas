{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Basic Markdown block processors

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Processors;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CodePages.unicodedata, System.Types,  System.SysUtils, 
  System.Classes, System.StrUtils, System.Contnrs,
{$ELSE}
  UnicodeData, Types,  SysUtils, Classes, StrUtils, Contnrs,
{$ENDIF}  
  MarkDown.Elements,
  MarkDown.Utils,
  MarkDown.Line,
  MarkDown.HtmlEntities,
  MarkDown.Parser;

type
  { TMarkDownQuoteProcessor }

  TMarkDownQuoteProcessor = class (TMarkDownBlockProcessor)
  private
    FLevel : integer;
    function SkipQuotes(aLine: TMarkDownLine; aLevel: Integer): Integer;
  protected
    function IsQuotedLine(aLine: TMarkDownLine; SkipLevels: Boolean): boolean;
    function inListOrQuote : boolean; override;
    function IsRoot(aParent : TMarkDownContainerBlock) : Boolean;
  public
    function LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkDownListProcessor }

  TMarkDownListProcessor = class (TMarkDownBlockProcessor)
  private
    FHasContent : boolean;
    FEmptyLine : integer;
    FLastList : TMarkDownListBlock;
    FLastItem : TMarkDownListItemBlock;
    FIndent : Integer;
    FMarker : String;
  protected
    function isList(aOrdered : boolean; const aMarker : String; aIndent : integer) : boolean; override;
    function inListOrQuote : boolean; override;
    function Root : Boolean;
    function LastList : TMarkDownListBlock;
  public
    function prepareline(aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : boolean;
  end;

  { TUListProcessor }

  TUListProcessor = class (TMarkDownListProcessor)
  private
    function IsItemInList(aList: TMarkDownListBlock; aLine: TMarkDownLine): boolean;
  public
    function LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : boolean; override;
  end;

  { TOListProcessor }

  TOListProcessor = class (TMarkDownListProcessor)
  private
    FStart : Integer;
    function IsItemInList(aList: TMarkDownListBlock; aLine: TMarkDownLine): boolean;
  Public
    function LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : boolean; override;
  end;

  { TSeTextHeaderProcessor }

  TSeTextHeaderProcessor = class(TMarkDownBlockProcessor)
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : boolean; override;
  end;

  { TThematicBreakProcessor }

  TThematicBreakProcessor = class(TMarkDownBlockProcessor)
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;


  { TCodeBlockProcessor }

  TCodeBlockProcessor = class (TMarkDownBlockProcessor)
  private
    procedure ProcessCodeBlock(C: TMarkDownCodeBlock; aLine: TMarkDownLine);
  public
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;

  { TFencedCodeBlockProcessor }

  TFencedCodeBlockProcessor = class (TMarkDownBlockProcessor)
  Private
    FIndent : Integer;
    FLang : String;
    FTerminal: string;
  Public
    function LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;

  { TMarkDownHeadingProcessor }

  TMarkDownHeadingProcessor = class(TMarkDownBlockProcessor)
    FLen : Integer;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;

  { TTableProcessor }

  TTableProcessor = class(TMarkDownBlockProcessor)
  private
    function CountCells(aLine: TMarkDownLine): Integer;
  protected
    procedure ParseTableLine(aTable: TMarkDownTableBlock; aLine: TMarkDownLine);
    function IsEndOfTable(aLine : TMarkDownLine) : boolean;
    // Check that number of cells in second line of table is the same as in first line
    function Strict : boolean; virtual;
  public
    class var
      DefaultStrict : Boolean;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
  end;

  { TParagraphProcessor }

  TParagraphProcessor = class(TMarkDownBlockProcessor)
  public
    function processLine(aParent : TMarkDownContainerBlock; aLine : TMarkDownLine; aContext : TMarkDownBlockProcessingContext) : Boolean; override;
    function HandlesLine(aParent : TMarkDownContainerBlock; aLine: TMarkDownLine): boolean; override;
  end;

implementation

{ ---------------------------------------------------------------------
  TMarkDownQuoteProcessor
  ---------------------------------------------------------------------}

function TMarkDownQuoteProcessor.LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean;

var
  len : integer;
  Inquote : Boolean;

begin
  Result:=(aLine=Nil);
  if Result then
    exit;
  Result:=(aLine.LineNo<>aBlock.line);
  if not Result then
    exit;
  // See if we have enough > markers
  Len:=SkipQuotes(aLine,FLevel);
  if Len=0 then
    Exit(False);
  inQuote:=(Len=FLevel);
//  InQuote:=StartsWithWhiteSpace(aLine.Remainder,'>',len);
  // Not enough markers -> check continuation.
  // end of block if:
  // - empty line
  // - starts a new kind of block
  // - It is not plain text (laziness rule)
  if aLine.isWhitespace and not InQuote then // empty line
    Result:=True
  else if IsBlock(aBlock, aBlock.Blocks, aLine.Remainder) then // New kind of block
    Result:=True
  else if not TMarkDownParser.inPara(aBlock.blocks, false) then
    Result:=true
  else
    begin
    Result:=False;
    Parser.Lazy:=true;
    end;
end;

function TMarkDownQuoteProcessor.SkipQuotes(aLine : TMarkDownLine; aLevel : Integer) : integer;

var
  len,lLevel : integer;
begin
  // eat > markers up to current level
  aLine.Mark;
  lLevel:=aLevel;
  While (lLevel>0) and StartsWithWhitespace(aLine.Remainder, '>', len) do
    begin
    aLine.advance(len+1);
    if not aLine.isEmpty and isWhitespaceChar(aLine.Remainder[1]) then
      aLine.advance(1);
    Dec(lLevel);
    end;
  Result:=lLevel;
end;

function TMarkDownQuoteProcessor.IsQuotedLine(aLine: TMarkDownLine; SkipLevels : Boolean): boolean;

var
  len : integer;

begin
  if SkipLevels and (FLevel>0) then
    SkipQuotes(aLine,FLevel);
  Result:=StartsWithWhiteSpace(aLine.Remainder,'>',len);
  if not Result then
    exit;
  aLine.advance(len+1);
  if not aLine.isEmpty and isWhitespaceChar(aLine.Remainder[1]) then
    aLine.advance(1);
end;


function TMarkDownQuoteProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

begin
  Result:=IsQuotedLine(aLine,False);
end;


function TMarkDownQuoteProcessor.inListOrQuote: boolean;
begin
  Result:=true;
end;

function TMarkDownQuoteProcessor.IsRoot(aParent : TMarkDownContainerBlock): Boolean;
var
  lParent : TMarkDownBlock;
begin
  Result:=aParent is TMarkDownDocument;
  if Result then
    exit;
  lParent:=aParent;
  While lParent<>Nil do
    begin
    Result:=Not (lParent is TMarkDownListBlock);
    if Result then
      Exit;
    lParent:=lParent.Parent;
    end;
end;

function TMarkDownQuoteProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

var
  oldLevel : Integer;
  lBlock : TMarkDownQuoteBlock;

begin
  oldLevel:=Flevel;
  inc(FLevel);
  lBlock:=TMarkDownQuoteBlock.Create(aParent,aLine.LineNo);
  RedoLine(false);
  Parse(lBlock,Self);
  FLevel:=OldLevel;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TMarkDownListProcessor
  ---------------------------------------------------------------------}

function TMarkDownListProcessor.inListOrQuote: boolean;
begin
  Result:=true;
end;

function TMarkDownListProcessor.Root: Boolean;
begin
  Result:=False;
end;

function TMarkDownListProcessor.LastList: TMarkDownListBlock;
begin
  Result:=FLastList;
end;


function TMarkDownListProcessor.isList(aOrdered: boolean; const aMarker: String; aIndent: integer): boolean;

begin
  Result:=Assigned(FLastList);
  if Result then
    begin
    Result:=(FLastList.Ordered=aOrdered)
            and (FLastList.Marker=aMarker)
            and (aIndent<FLastList.lastIndent + 1);
    end;
end;

function TMarkDownListProcessor.prepareLine(aLine: TMarkDownLine; aContext : TMarkDownBlockProcessingContext): boolean;

var
  lWhiteSpace, lCurrLineNo,lLastIndent,len : integer;

begin
  Result:=False;
  lCurrLineNo:=aLine.LineNo;
  lLastIndent:=LastList.LastIndent;
  lWhiteSpace:=aLine.LeadingWhitespace;
  if lCurrLineNo=FLastItem.Line then
    aLine.advance(lLastIndent)
  else if lWhitespace >= lLastIndent then
    begin
    len:=lWhitespace;
    if (len>lLastIndent+1) then
      len:=LastList.baseIndent;
    aLine.advance(len);
    end
  else if (mdoGithubFlavoured in Parser.Options) and (lWhitespace>3) and (aLine.Remainder.Trim.StartsWith('-')) then
    Result:=False
  else if not aLine.isWhitespace and isBlock(LastList, FLastItem.blocks, aLine.Remainder, LastList.LastIndent+LastList.grace) then
    Result:=true;
  if not root then
    Exit;
  if not aLine.isWhiteSpace then
   begin
      FHasContent:=true;
      if root and not Result and LastList.HasSeenEmptyLine then
        LastList.loose:=true;
    end
    else if not Result  then
    begin
      Parser.Lazy:=true;
      if not FHasContent then
      begin
        if FEmptyLine = -1 then
          FEmptyLine:=lCurrLineNo
        else if FEmptyLine <> CurrentLine.LineNo then
          Result:=true;
      end;
      if (aContext = bpGeneral) and (lCurrLineNo<>FLastItem.Line) then
        LastList.HasSeenEmptyLine:=true;
    end;
end;


{ ---------------------------------------------------------------------
  TMarkDownHeadingProcessor
  ---------------------------------------------------------------------}

function TMarkDownHeadingProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;
var
  ls : string;
  lLen,lCount : integer;
begin
  FLen:=0;
  if aLine.LeadingWhitespace >= 4 then
    Exit(false);
  ls:=Trim(aLine.Remainder);
  lLen:=Length(ls);
  lCount:=CountStartChars(ls,'#');
  Result:=(lCount>0) and ((lCount=lLen)) or ((lCount<lLen) and (ls[lCount+1] in [' ',#9]));
  if Result then
    FLen:=lCount;
end;


function TMarkDownHeadingProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

var
  lBlock : TMarkDownHeadingBlock;
  lLen : integer;
  s : String;

begin
  lBlock:=TMarkDownHeadingBlock.Create(aParent,aLine.LineNo,Flen);
  aLine.Advance(Flen);
  aLine.SkipWhiteSpace;
  s:=Trim(aLine.Remainder);
  if not isWhitespace(s) then
    begin
    lLen:=length(s);
    while (lLen>0) and (s[llen]='#') do
      Dec(lLen);
    if (lLen = 0) then
      s:=''
    else if (s[lLen]=' ') then
      s:=Copy(s,1,lLen-1);
    end;
  Parser.parseInline(lBlock,S);
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TUListProcessor
  ---------------------------------------------------------------------}

function TUListProcessor.LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean;
var
  lBlock : TMarkDownContainerBlock;
  lList : TMarkDownListBlock absolute lBlock;

begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  if (lBlock is TMarkDownListItemBlock) and (lBlock.Parent is TMarkDownListBlock) then
     lBlock:=lBlock.Parent as TMarkDownListBlock;
  if (lBlock is TMarkDownListBlock) then
    if aLine.LeadingWhitespace>=lList.baseIndent then
      Result:=False
end;

function TUListProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  ws,i, i2 : integer;
  s : String;
  list : TMarkDownListBlock;

begin
  Result:=False;
  if aLine.isEmpty then
    Exit;
  ws:=aLine.LeadingWhitespace;
  if ws>3 then
    begin
    FMarker:=CopySkipped(aLine.Remainder,[' ',#9]);
    if (FMarker='') or not inList(aParent.blocks, false, FMarker[1], ws, 1, list) then
      Exit;
    end;
  i:=aLine.LeadingWhitespace;
  s:=aLine.Remainder;
  if (Length(s)<1+i) then
    Exit;
  if not CharInSet(s[1+i],['+','-','*']) then
    Exit;
  FMarker:=s[1+i];
  s:=Copy(S,2+i,Length(S)-i);
  if isWhitespace(s) and Parser.inPara(aParent.blocks, false) then
    Exit;
  if isWhitespace(s) then // nothing after if it's the only thing on the aLine
    i2:=1
  else
    begin
    i2:=LeadingWhitespace(s);
    if (i2 = 0) then
      Exit;
    if (i2 >= 5) then
      i2:=1;
    end;
  FIndent:=i+i2+1;
  Result:=True;
end;

function TUListProcessor.IsItemInList(aList : TMarkDownListBlock; aLine : TMarkDownLine) : boolean;

var
  len : integer;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  Result:=StartsWithWhitespace(aLine.line,aList.marker[1],len,aList.baseIndent);
end;

function TUListProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): boolean;

var
  lOldLastList, lList : TMarkDownListBlock;
  lOldLastItem, lItem : TMarkDownListItemBlock;
  lMarker : string;
  lNewItem : Boolean;

begin
  lOldLastList:=FLastList;
  lOldLastItem:=FLastItem;
  lMarker:=FMarker;
  if InList(aParent.blocks,False,lMarker,FIndent,1,lList) then
    lList.Lastindent:=FIndent
  else
    begin
    lList:=TMarkDownListBlock.Create(aParent,aLine.LineNo);
    lList.Ordered:=false;
    lList.BaseIndent:=FIndent;
    lList.LastIndent:=lList.BaseIndent;
    lList.Marker:=lMarker;
    FLastList:=lList;
    end;
  // While we have a list item part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkDownListItemBlock.Create(lList,aLine.LineNo);
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    lNewItem:=Not Done;
    if lNewItem then
      begin
      aLine:=NextLine;
      lNewItem:=IsItemInList(lList,aLine);
      end;
    lItem.Closed:=true;
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  Result:=True;
end;


{ ---------------------------------------------------------------------
  TOListProcessor
  ---------------------------------------------------------------------}

function TOListProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  list : TMarkDownListBlock;
  lMarkerLen,lIndent, i2 : integer;
  lMarker,lLine : String;

begin
  Result:=false;
  if aLine.isEmpty then
    Exit;
  if (aLine.LeadingWhitespace >= 4) then
    begin
    FMarker:=CopySkipped(aLine.Remainder, [' ']);
    FMarker:=CopySkipped(FMarker, ['0'..'9']);
    if (FMarker = '') or not inList(aParent.blocks, true, FMarker[1], aLine.LeadingWhitespace, 2, list) then
      Exit;
    end;
  lIndent:=aLine.LeadingWhitespace;
  aLine.mark;
  try
    aLine.SkipWhiteSpace;
    lLine:=aLine.Remainder;
    lMarker:=CopyMatching(lLine, ['0'..'9']);
    lMarkerLen:=length(lMarker)+1; // ending dot or )
    if (lMarkerLen=1) or (lMarkerLen>10) or (lMarkerLen>Length(lLine)) then
      Exit;
    if not CharInSet(lLine[lMarkerLen], ['.', ')']) then
      Exit;
    // rule 267
    if Parser.inPara(aParent.blocks, false) and (lMarker<>'1') then
      Exit;
    FMarker:=lLine[lMarkerLen];
    FStart:=StrToIntDef(lMarker,1);
    inc(lIndent,lMarkerLen);
    lLine:=Copy(lLine,lMarkerLen+1,Length(lLine)-lMarkerLen);
    if isWhitespace(lLine) and Parser.inPara(aParent.blocks, false) then
      Exit;
    Result:=true;
  finally
    if not Result then
      aLine.rewind;
  end;
  // Calculate indent to use...
  if isWhitespace(lLine) then
    i2:=1
  else
    begin
    i2:=LeadingWhitespace(lLine);
    if (i2 = 0) then
      Exit(false);
    if (i2 >= 5) then
      i2:=1;
    end;
  FIndent:=lIndent+i2;
end;

function TOListProcessor.IsItemInList(aList : TMarkDownListBlock; aLine : TMarkDownLine) : boolean;

var
  len : integer;
  lLine : string;

begin
  Result:=Assigned(aLine);
  if Not Result then
    exit;
  Result:=StartsWithWhitespace(aLine.Line,['0'..'9'],len,aList.baseIndent);
  if Not Result then
    exit;
  if len<=aList.baseIndent then
    begin
    lLine:=aLine.Line;
    if Len>0 then
      Delete(lLine,1,Len);
    lLine:=CopySkipped(lLine,['0'..'9']);
    Result:=(lLine<>'') and (aList.marker=lLine[1]);
    if Result then
      Result:=(Length(lLine)>1) and (lLine[2]=' ');
    end;
end;

function TOListProcessor.LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean;
var
  lBlock : TMarkDownContainerBlock;
  lList : TMarkDownListBlock absolute lBlock;

begin
  lBlock:=aBlock;
  Result:=aBlock.line<>aLine.LineNo;
  if Not Result then
    Exit;
  if (lBlock is TMarkDownListItemBlock) and (lBlock.Parent is TMarkDownListBlock) then
     lBlock:=lBlock.Parent as TMarkDownListBlock;
  if (lBlock is TMarkDownListBlock) then
    if aLine.LeadingWhitespace>=lList.baseIndent then
      Result:=False
end;


function TOListProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): boolean;
var
  lOldLastList, lList : TMarkDownListBlock;
  lOldLastItem, lItem : TMarkDownListItemBlock;
  lNewItem : Boolean;
begin
  lOldLastList:=FLastList;
  if inList(aParent.blocks, true, FMarker, Findent, 2, lList) then
    begin
    lList.lastIndent:=FIndent;
    FLastList:=lList;
    end
  else
    begin
    lList:=TMarkDownListBlock.Create(aParent,aLine.LineNo);
    lList.ordered:=true;
    lList.baseIndent:=Findent;
    lList.lastIndent:=lList.baseIndent;
    lList.marker:=FMarker;
    lList.Start:=FStart;
    FLastList:=lList;
    end;
  // While we have a line that part of this list block, add an item and parse it.
  repeat
    lItem:=TMarkDownListItemBlock.Create(lList,aLine.LineNo);
    lOldLastItem:=FLastItem;
    FLastItem:=lItem;
    PrepareLine(aLine,bpGeneral);
    RedoLine(False);
    Parse(lItem,Self);
    lNewItem:=Not Done;
    if lNewItem then
      begin
      aLine:=NextLine;
      lNewItem:=IsItemInList(lList,aLine);
      end;
  until not lNewItem;
  FLastItem:=lOldLastItem;
  FLastList:=lOldLastList;
  Result:=True;
end;

{ ---------------------------------------------------------------------
  TCodeBlockProcessor
  ---------------------------------------------------------------------}

function TCodeBlockProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  Indent : integer;

begin
  Result:=False;
  Indent:=aLine.LeadingWhitespace;
  if Indent < 4 then
    Exit;
  if aLine.isWhitespace then
    Exit;
  if Parser.inPara(aParent.blocks,True) then
    Exit;
  Result:=true;
end;

procedure TCodeBlockProcessor.ProcessCodeBlock(C: TMarkDownCodeBlock; aLine: TMarkDownLine);

var
  S : String;
  lContinue : boolean;
  lIndent : Integer;
begin
  lIndent:=aLine.LeadingWhitespace;
  aLine.advance(lIndent);
  S:=aLine.Remainder;
  lContinue:=True;
  While lContinue do
    begin
    TMarkDownTextBlock.Create(C,aLine.LineNo,S);
    aLine:=peekLine;
    if (aLine = nil) then
      Exit;
    if not Handlesline(C,aLine) then
      Exit;
    S:=aLine.Remainder;
    if lengthWhitespaceCorrected(S)<=lIndent then
      lContinue:=isWhitespace(S)
    else
      lContinue:=LeadingWhitespace(S)>=lIndent;
    if lContinue then
      begin
      // Actually get the line
      aLine:=NextLine;
      aLine.advance(lIndent);
      S:=aLine.Remainder;
      end;
    end;
end;

function TCodeBlockProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

var
  lBlock : TMarkDownCodeBlock;

begin
  lBlock:=TMarkDownCodeBlock.Create(aParent,aLine.LineNo);
  ProcessCodeBlock(lBlock,aLine);
  // No whitespace at the end
  while (lBlock.ChildCount>0) and isWhitespace((lBlock.LastChild as TMarkDownTextBlock).text) do
    lBlock.DeleteChild(lBlock.ChildCount-1);
  Result:=true;
end;


function TFencedCodeBlockProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  s, s1 : String;
  c : char;
  len, i, j : integer;
begin
  Result:=False;
  FLang:='';
  if aLine.LeadingWhitespace >= 4 then
    Exit;
  s:=aLine.Remainder.Trim;
  if (S='') then
    Exit;
  c:=S[1];
  if not (C in ['`','~']) then
    Exit;
  len:=CountStartChars(S,c);
  if (len<3) or (Pos(c,S,len+2)<>0) then
    Exit;
  FTerminal:=Copy(s,1,Len);
  Result:=true;
  // now, try to find anything off the end of the fence
  s:=aLine.Remainder;
  Findent:=1;
  while s[FIndent] = ' ' do
    inc(FIndent);
  if FIndent>1 then
    Delete(s,1,FIndent-1);
  S:=CopySkipped(S,[FTerminal[1]]).trim;
  i:=1;
  while (i<=Length(s)) do
    begin
    if S[i] = '\' then
      delete(s, i, 1) // and omit checking the next character
    else if s[i] = '`' then
      Exit(false)
    else if s[i] = '&' then
    begin
      j:=i+1;
      while (j <= Length(s)) and (s[j] <> ';') do
        inc(j);
      if j <= length(s) then
      begin
        s1:=parseEntityString(Parser.Entities,copy(s, i, j-i+1));
        delete(s, i, j-1);
        insert(s1, s, i);
        inc(i, Length(s1)-1);
      end;
    end;
    inc(i);
  end;

  if (s <> '') then
  begin
    Flang:=CopyUpTo(s, [' ']);
    if Flang.contains(FTerminal[1]) then
      Exit(false);
  end;
end;

function TFencedCodeBlockProcessor.LineEndsBlock(aBlock: TMarkDownContainerBlock; aLine: TMarkDownLine): Boolean;

var
  s : String;

begin
  Result:=(aLine=nil);
  if Result then
    Exit;
  // Ending may be preceded by 3 spaces
  Result:=aLine.LeadingWhitespace>=4;
  if Result then
    Exit;
  S:=aLine.Remainder.Trim;
  Result:=IsStringOfChar(s) and s.StartsWith(FTerminal);
end;

function TFencedCodeBlockProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

var
  lBlock : TMarkDownCodeBlock;
  s : String;
  i : integer;

begin
  lBlock:=TMarkDownCodeBlock.Create(aParent,aLine.LineNo);
  lBlock.fenced:=true;
  lBlock.lang:=Flang;
  while Not LineEndsBlock(lBlock,PeekLine) do
    begin
    aLine:=NextLine;
    s:=aLine.Remainder;
    if (FIndent>0) then
      begin
      if FIndent>Length(S) then
        FIndent:=Length(S);
      I:=1;
      while (I<=Findent) and (s[i]=' ') do
        Inc(i);
      if I>1 then
        Delete(S,1,I-1);
      aLine.advance(I-1);
      end;
    TMarkDownTextBlock.Create(lBlock,aLine.LineNo,S);
    end;
  NextLine;
  Result:=true;
end;

function TTableProcessor.CountCells(aLine: TMarkDownLine): Integer;

var
  c : char;
  lEscaped: boolean;
  S : String;

begin
  Result:=0;
  lEscaped:=false;
  for c in aLine.Line do
    begin
    if not lEscaped and (c = '|') then
      inc(Result)
    else if lEscaped then
      lEscaped:=false
    else if c = '\' then
      lEscaped:=true;
    end;
  if Result=0 then
    exit;
  S:=Trim(aLine.Line);
  if (S<>'') then
    begin
    if not StartsStr('|',S) then
      inc(Result);
    if not EndsStr('|',S) or EndsStr('\|',S) then
      inc(Result);
    end;
end;

function TTableProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  lCount,lCountNext  : integer;
  lNextLine : TMarkDownLine;
  lLine : string;
  c : char;

begin
  Result:=False;
  if aLine.isEmpty then
    exit;
  lCount:=CountCells(aLine);
  if lCount=0 then
    Exit;
  lNextLine:=peekLine;
  if (lNextLine=nil) then
    Exit;
  lLine:=lNextLine.Line;
  for c in lLine do
    if not CharInSet(c,[' ','|','-',':']) then
      Exit(false);
  Result:=not Strict;
  lCountNext:=CountCells(lNextLine);
  // We have a table if the number of cells is equal
  Result:=(lCount=lCountNext)
end;

function TTableProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

var
  i : integer;
  lLine,lCell : String;
  lTable : TMarkDownTableBlock;
  lCells : TStringDynArray;
  lColumns : TCellAlignArray;

begin
  LColumns:=[];
  lTable:=TMarkDownTableBlock.Create(aParent,aLine.LineNo);
  parseTableLine(lTable, aLine);
  lLine:=Trim(NextLine.Line);
  if StartsStr('|',lLine) then
    lLine:=Copy(lLine,2,Length(lLine)-1);
  if EndsStr('|',lLine) then
    SetLength(lLine,Length(lLine)-1);
  lCells:=lLine.Split(['|']);
  SetLength(LColumns, length(lCells));
  for i:=0 to length(lCells) - 1 do
    begin
    lColumns[i]:=caLeft;
    lCell:=Trim(lCells[i]);
    if StartsStr(':',lCell) and EndsStr(':',lCell) then
      lColumns[i]:=caCenter
    else if EndsStr(':',lCell) then
      lColumns[i]:=caRight
    end;
  lTable.Columns:=lColumns;
  while not isEndOfTable(peekLine) do
    ParseTableLine(lTable,NextLine);
  Result:=True;
end;

procedure TTableProcessor.ParseTableLine(aTable: TMarkDownTableBlock; aLine: TMarkDownLine);

var
  lRow : TMarkDownTableRowBlock;
  lLen,lStart, i : integer;
  lEscaped : boolean;
  lLine : string;
  lChar : char;

  procedure AddCell(aStart,aEnd : integer);

  var
    lCell : String;

  begin
    lCell:=copy(aLine.Line, aStart, aEnd-aStart);
    lCell:=StringReplace(Trim(lCell),'\|', '|',[rfReplaceAll]);
    TMarkDownTextBlock.Create(lRow,aLine.LineNo,lCell);
  end;

begin
  lRow:=TMarkDownTableRowBlock.Create(aTable,aLine.LineNo);
  i:=1;
  lStart:=1;
  lEscaped:=false;
  lLine:=aLine.Line;
  lLen:=Length(lLine);
  while i<=lLen do
    begin
    lChar:=lLine[i];
    if (i=1) and (lChar='|') then
      lStart:=2
    else if lEscaped then
      lEscaped:=False
    else if lChar = '\' then
      lEscaped:=True
    else if lChar = '|' then
      begin
      AddCell(lStart,i);
      lStart:=i+1;
      end;
    Inc(i);
    end;
  if (lStart<i) then
    addCell(lStart,i);
end;

function TTableProcessor.IsEndOfTable(aLine: TMarkDownLine): boolean;

var
  S : String;
  lLen : integer;

begin
  if aLine=nil then
    Exit(true);
  S:=aLine.Line;
  lLen:=Length(S);
  Result:=(Trim(S) = '')
          or ((lLen>0) and (S[1] in ['#','~']))
          or StartsStr('   ',S);
end;

function TTableProcessor.Strict: boolean;
begin
  Result:=DefaultStrict;
end;

{ ---------------------------------------------------------------------
  TParagraphProcessor
  ---------------------------------------------------------------------}

function TParagraphProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;
begin
  Result:=True;
end;

function TParagraphProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): boolean;

var
  lPar : TMarkDownParagraphBlock;

begin
  Result:=true;
  if Parser.inPara(aParent.blocks, true) then
    lPar:=aParent.Blocks.Last as TMarkDownParagraphBlock
  else
    lPar:=nil;
  if aLine.isEmpty or aLine.isWhitespace then
    begin
    if Assigned(lPar) then
      lPar.closed:=true;
    end
  else
    begin
    if Not Assigned(lPar) then
      lPar:=TMarkDownParagraphBlock.Create(aParent,aLine.LineNo);
    Parser.parseInLine(lPar,aLine.Remainder);
    end;
end;

{ ---------------------------------------------------------------------
  TSeTextHeaderProcessor
  ---------------------------------------------------------------------}

function TSeTextHeaderProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  p : TMarkDownParagraphBlock;
  s : String;

begin
  Result:=False;
  if aParent.blocks.Count=0 then
    Exit;
  if aLine.LeadingWhitespace>=4 then
    Exit;
  if not TMarkDownParser.inPara(aParent.blocks,False) then
    Exit;
  p:=aParent.blocks.Last as TMarkDownParagraphBlock;
  if p.closed then
    Exit;
  if p.header<>0 then
    Exit;
  if Parser.Lazy and inListOrQuote then
    Exit;
  s:=aLine.Remainder.Trim;
  if (s='') then
    Exit;
  if not IsStringOfChar(s) then
    Exit;
  Result:=True;
end;


function TSeTextHeaderProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): boolean;

var
  S : String;
  h : integer;
  P : TMarkDownParagraphBlock;

begin
  s:=aLine.Remainder.Trim;
  h:=Pos(S[1],'=-');
  Result:=h>0;
  if Result then
    begin
    p:=aParent.blocks.Last as TMarkDownParagraphBlock;
    p.header:=h;
    end;
end;

{ ---------------------------------------------------------------------
  TThematicBreakProcessor
  ---------------------------------------------------------------------}

function TThematicBreakProcessor.HandlesLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine): boolean;

var
  S : String;
  C : char;

begin
  Result:=False;
  if aLine.LeadingWhitespace >= 4 then
    Exit;
  S:=StripWhitespace(aLine.Remainder);
  if (S='') then
    Exit;
  C:=S[1];
  Result:=(C in ['*','-','_']) and (CountStartChars(S,C)>2) and IsStringOfChar(S);
end;


function TThematicBreakProcessor.processLine(aParent: TMarkDownContainerBlock; aLine: TMarkDownLine; aContext: TMarkDownBlockProcessingContext): Boolean;

begin
  TMarkDownThematicBreakBlock.Create(aParent,aLine.LineNo);
  Result:=true;
end;

Procedure RegisterDefaultProcessors;

begin
  TMarkDownQuoteProcessor.Register('quote');
  // Must be registered before thematic break
  TSeTextHeaderProcessor.register('setextheader');
  TThematicBreakProcessor.Register('break');
  TCodeBlockProcessor.register('code');
  TFencedCodeBlockProcessor.register('fencedcode');
  TMarkDownHeadingProcessor.register('heading');
  TUListProcessor.Register('unorderedlist');
  TOListProcessor.Register('orderedlist');
  TTableProcessor.Register('table');
  TParagraphProcessor.Register('paragraph');
end;

initialization
  TTableProcessor.DefaultStrict:=True;
  RegisterDefaultProcessors;
end.

