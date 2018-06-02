{ Simple Wiki parser for the FreePascal/Lazarus Wiki export pages

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.

ToDo:
  links without brackets: http://...  see bidimode
  div, div class="key", font: this is pure HTML and maybe should be better fixed in the wiki
  attributes in pre  <pre>'''Text'''</pre> see page BidiMode
  code in list items, see Compile_and_Develop_on_Maemo_device
}
unit WikiParser;

{$mode objfpc}{$H+}

{ $DEFINE VerboseWikiStack}
{ $DEFINE VerboseWikiOnToken}
{ $DEFINE VerboseUnknownOpenTags}

interface

uses
  Classes, SysUtils, laz2_XMLRead, laz2_DOM, LazLoggerBase, LazUTF8,
  BasicCodeTools, KeywordFuncLists;

const
  MaxHeaderDepth = 6;
type
  TWPTokenType = (
    wptText,  // TWPTextToken
    wptAttribute, // e.g. class="code" TWPNameValueToken
    wptLineBreak, // <br> /br> <br/>
    wptBold,    // '''
    wptBoldTag, // <b>
    wptItalic,  // ''
    wptItalicTag, // <i>
    wptStrikeTagShort, // <s>
    wptStrikeTagLong, // <strike>
    wptUnderlineTag, // <u>
    wptTT, // <tt>
    wptSup, // <sup>
    wptSub, // <sub>
    wptSmall, // <small>
    wptEm, // <em>
    wptString, // <string>
    wptVar, // <var>
    wptKey, // <key>
    wptCmt, // <cmt>
    wptSpan, // <span>
    wptCode, // TWPNameValueToken
    wptSpecial, // {{text}}
    wptPre,  // space at line start
    wptPreTag,  // <pre>
    wptP, // paragraph
    wptPTag, // <p>
    wptDivTag, // <div>
    wptCenter, // <center>
    wptInternLink, // [[]]
    wptExternLink, // []
    wptHorizontalRow, // ----
    wptNumberedList, // #
    wptOrderedListTag, // <ol>
    wptBulletList, // *
    wptUnorderedListTag, // <ul>
    wptDefinitionList, // : or ;
    wptListItem,
    wptListItemTag, // <li>
    wptTable, // wiki tag for table
    wptTableTag, // <table>
    wptTableRow, // wiki tag for table row
    wptTableRowTag, // <tr>
    wptTableHeadCell, // wiki tag for table head cell
    wptTableHeadCellTag, // <th>
    wptTableCell, // wiki tag for table cell
    wptTableCellTag, // <td>
    wptSection, // started/ended by =
    wptSubSection, // started automatically, ended on empty line
    wptHeader, // =Text=
    wptHeader1, // <h1>
    wptHeader2, // <h2>
    wptHeader3  // <h3>
    );
  TWPTokenTypes = set of TWPTokenType;

  TWPTokenInfoFlag = (
    wpifCanStart,
    wpifCanEnd,
    wpifWarnOnAutoClose
    );
  TWPTokenInfoFlags = set of TWPTokenInfoFlag;

  TWPTokenGroup = (
    wpgFont,
    wpgParagraph,
    wpgList,
    wpgTable,
    wpgSubSection,
    wpgSection
    );
  TWPTokenGroups = set of TWPTokenGroup;

  TWPTokenRange = (
    wprNone,
    wprOpen,
    wprClose
    );

  TWPTokenInfo = record
    Caption: string;
    Flags: TWPTokenInfoFlags;
    Group: TWPTokenGroup;
    BaseToken: TWPTokenType;
  end;

const
  WPTWikiLists = [wptNumberedList,wptBulletList,wptDefinitionList,wptListItem];

  WPTokenInfos: array[TWPTokenType] of TWPTokenInfo = (
    (Caption: 'Text'; Flags: []; Group: wpgFont; BaseToken: wptText), // wptText,
    (Caption: 'Attribute'; Flags: []; Group: wpgFont; BaseToken: wptAttribute), // wptAttribute,
    (Caption: 'LineBreak'; Flags: []; Group: wpgFont; BaseToken: wptLineBreak), // wptLineBreak,
    (Caption: 'Bold'; Flags: []; Group: wpgFont; BaseToken: wptBold), // wptBold,
    (Caption: 'BoldTag'; Flags: []; Group: wpgFont; BaseToken: wptBold), // wptBoldTag,
    (Caption: 'Italic'; Flags: []; Group: wpgFont; BaseToken: wptItalic), // wptItalic,
    (Caption: 'ItalicTag'; Flags: []; Group: wpgFont; BaseToken: wptItalic), // wptItalicTag,
    (Caption: 'StrikeTagShort'; Flags: []; Group: wpgFont; BaseToken: wptStrikeTagShort), // wptStrikeTagShort,
    (Caption: 'StrikeTagLong'; Flags: []; Group: wpgFont; BaseToken: wptStrikeTagShort), // wptStrikeTagLong,
    (Caption: 'UnderlineTag'; Flags: []; Group: wpgFont; BaseToken: wptUnderlineTag), // wptUnderlineTag,
    (Caption: 'TT'; Flags: []; Group: wpgFont; BaseToken: wptTT), // wptTT,
    (Caption: 'Sup'; Flags: []; Group: wpgFont; BaseToken: wptSup), // wptSup,
    (Caption: 'Sub'; Flags: []; Group: wpgFont; BaseToken: wptSub), // wptSub,
    (Caption: 'Small'; Flags: []; Group: wpgFont; BaseToken: wptSmall), // wptSmall,
    (Caption: 'Em'; Flags: []; Group: wpgFont; BaseToken: wptEm), // wptEm,
    (Caption: 'String'; Flags: []; Group: wpgFont; BaseToken: wptString), // wptString,
    (Caption: 'Var'; Flags: []; Group: wpgFont; BaseToken: wptVar), // wptVar,
    (Caption: 'Key'; Flags: []; Group: wpgFont; BaseToken: wptKey), // wptKey,
    (Caption: 'Cmt'; Flags: []; Group: wpgFont; BaseToken: wptCmt), // wptCmt,
    (Caption: 'Span'; Flags: []; Group: wpgFont; BaseToken: wptSpan), // wptSpan,
    (Caption: 'Code'; Flags: []; Group: wpgFont; BaseToken: wptCode), // wptCode,
    (Caption: 'Special'; Flags: []; Group: wpgFont; BaseToken: wptSpecial), // wptSpecial,
    (Caption: 'Pre'; Flags: []; Group: wpgParagraph; BaseToken: wptPre), // wptPre,
    (Caption: 'PreTag'; Flags: []; Group: wpgParagraph; BaseToken: wptPre), // wptPreTag,
    (Caption: 'P'; Flags: []; Group: wpgParagraph; BaseToken: wptP), // wptP,
    (Caption: 'PTag'; Flags: []; Group: wpgParagraph; BaseToken: wptP), // wptPTag,
    (Caption: 'DivTag'; Flags: []; Group: wpgParagraph; BaseToken: wptP), // wptDivTag,
    (Caption: 'Center'; Flags: []; Group: wpgParagraph; BaseToken: wptCenter), // wptCenter
    (Caption: 'InternLink'; Flags: []; Group: wpgParagraph; BaseToken: wptInternLink), // wptInternLink,
    (Caption: 'ExternLink'; Flags: []; Group: wpgParagraph; BaseToken: wptExternLink),  // wptExternLink,
    (Caption: 'HorizontalRow'; Flags: []; Group: wpgParagraph; BaseToken: wptHorizontalRow), // wptHorizontalRow,
    (Caption: 'NumberedList'; Flags: []; Group: wpgList; BaseToken: wptNumberedList),  // wptNumberedList,
    (Caption: 'OrderedListTag'; Flags: []; Group: wpgList; BaseToken: wptNumberedList),  // wptOrderedListTag,
    (Caption: 'BulletList'; Flags: []; Group: wpgList; BaseToken: wptBulletList),  // wptBulletList,
    (Caption: 'UnorderedListTag'; Flags: []; Group: wpgList; BaseToken: wptBulletList),  // wptUnorderedListTag,
    (Caption: 'DefinitionList'; Flags: []; Group: wpgList; BaseToken: wptDefinitionList),  // wptDefinitionList,
    (Caption: 'ListItem'; Flags: []; Group: wpgList; BaseToken: wptListItem),  // wptListItem,
    (Caption: 'ListItemTag'; Flags: []; Group: wpgList; BaseToken: wptListItem), // wptListItemTag,
    (Caption: 'Table'; Flags: []; Group: wpgTable; BaseToken: wptTable), // wptTable,
    (Caption: 'TableTag'; Flags: []; Group: wpgTable; BaseToken: wptTable), // wptTableTag,
    (Caption: 'TableRow'; Flags: []; Group: wpgTable; BaseToken: wptTableRow), // wptTableRow,
    (Caption: 'TableRowTag'; Flags: []; Group: wpgTable; BaseToken: wptTableRow), // wptTableRowTag,
    (Caption: 'TableHeadCell'; Flags: []; Group: wpgTable; BaseToken: wptTableHeadCell), // wptTableHeadCell,
    (Caption: 'TableHeadCellTag'; Flags: []; Group: wpgTable; BaseToken: wptTableHeadCell), // wptTableHeadCellTag,
    (Caption: 'TableCell'; Flags: []; Group: wpgTable; BaseToken: wptTableCell), // wptTableCell,
    (Caption: 'TableCellTag'; Flags: []; Group: wpgTable; BaseToken: wptTableCell), // wptTableCellTag,
    (Caption: 'Section'; Flags: []; Group: wpgSection; BaseToken: wptSection), // wptSection,
    (Caption: 'SubSection'; Flags: []; Group: wpgSubSection; BaseToken: wptP), // wptSubSection,
    (Caption: 'Header'; Flags: []; Group: wpgSection; BaseToken: wptHeader), // wptHeader,
    (Caption: 'Header1'; Flags: []; Group: wpgSection; BaseToken: wptHeader), // wptHeader1,
    (Caption: 'Header2'; Flags: []; Group: wpgSection; BaseToken: wptHeader), // wptHeader2,
    (Caption: 'Header3'; Flags: []; Group: wpgSection; BaseToken: wptHeader) // wptHeader3,
  );
  WPTokenRangeNames: array[TWPTokenRange] of string = (
    'Point' ,// wprNone
    'Open', // wprOpen,
    'Close' // wprClose,
    );

type
  TWikiPage = class;

  { TWPToken }

  TWPToken = class
  public
    Token: TWPTokenType;
    SubToken: TWPTokenType;
    Range: TWPTokenRange;
    UserData: Pointer;
    Page: TWikiPage;
    constructor Create(ThePage: TWikiPage; TheUserDate: Pointer);
  end;

  TWPTextToken = class(TWPToken)
  public
    StartPos, EndPos: integer;
  end;

  TWPLinkToken = class(TWPToken)
  public
    LinkStartPos, LinkEndPos: integer;
    Link: string; // trimmed and cleaned up
    CaptionStartPos, CaptionEndPos: integer;
  end;

  TWPNameValueToken = class(TWPToken)
  public
    NameStartPos, NameEndPos: integer;
    ValueStartPos, ValueEndPos: integer;
  end;

  TWikiTokenEvent = procedure(Token: TWPToken) of object;

  TWikiPageVerbosity = (
    wpvNone,
    wpvFatal,
    wpvError,
    wpvWarning,
    wpvHint
    );

  TWikiOnLog = procedure(Msg: string) of object;

  { TWikiPage }

  TWikiPage = class
  private
    type
      TWPStackItem = record
        Token: TWPTokenType;
        StartPos: PChar;
      end;
      PStackItem = ^TWPStackItem;
  private
    FBaseURL: string;
    FFilename: string;
    FAutoFixUTF8: boolean;
    FLanguageTags: TKeyWordFunctionList;
    FOnLog: TWikiOnLog;
    FStack: PStackItem;
    FStackPtr: integer;
    FStackCapacity: integer;
    FID: String;
    FRevision: String;
    FTimeStamp: String;
    FTitle: String;
    FCurP: PChar;
    FLine: integer;
    FLastEmitPos: PChar;
    FRangeToken: TWPToken;
    FSrc: string;
    FTextToken: TWPTextToken;
    FLinkToken: TWPLinkToken;
    FNameValueToken: TWPNameValueToken;
    FOnToken: TWikiTokenEvent;
    FVerbosity: TWikiPageVerbosity;
    FInPre: integer; // >0 means in a pre range
    FInTableHeader: Boolean;
    procedure HandleAngleBracket; // tags
    procedure HandleCode; // <code>
    procedure HandleApostroph; // bold, italic
    procedure HandleCurlyBracketOpen; // special, start of table
    procedure HandlePipe; // new row, end of table
    procedure HandleExclamationMark;  // head cell
    procedure HandleEdgedBracketOpen; // links
    procedure HandleUnderScore; // __TOC__
    procedure HandleEqual;   // headers
    procedure HandleListChar; // lists '*', '#', ':', ';'
    procedure HandleSpace; // preserve space
    procedure EmitFlag(Typ: TWPTokenType; Range: TWPTokenRange; TagLen: integer);
    procedure EmitToggle(Typ: TWPTokenType; TagLen: integer);
    procedure EmitTag(Typ: TWPTokenType; Range: TWPTokenRange);
    procedure EmitLineBreak;
    procedure EmitTextToken;
    procedure ParseCell;
    procedure ParseAttributes(StartPos, EndPos: PChar);
    procedure ParseNoWiki;
    procedure CloseTableCell;
    procedure CloseRangeToken(Typ: TWPTokenType);
    procedure OpenRangeToken(Typ: TWPTokenType);
    function FindTagEnd(TagStart: PChar): PChar;
    procedure SetAutoFixUTF8(AValue: boolean);
    procedure SetSrc(AValue: string);
    function TokenIs(Tag: PChar): boolean;
    procedure ClearStack;
    procedure Push(Token: TWPTokenType; StartPos: PChar);
    function Pop(Token: TWPTokenType): boolean;
    procedure Pop(Index: integer);
    function TopToken: TWPTokenType;
    function FindGroupStackPos(Group: TWPTokenGroup; OrHigher: boolean): integer;
    function FindStackItem(Typ: TWPTokenType): integer;
    procedure DoToken(Token: TWPToken);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(Filename: string);
    procedure LoadFromDoc(doc: TDOMNode);
    procedure Parse(const OnToken: TWikiTokenEvent; Data: Pointer = nil);
    property ID: String read FID write FID; // mediawiki/siteinfo/page/id
    property Title: String read FTitle write FTitle; // mediawiki/siteinfo/page/title
    property Revision: String read FRevision write FRevision; // mediawiki/siteinfo/page/revision/id
    property TimeStamp: String read FTimeStamp write FTimeStamp; // mediawiki/siteinfo/page/timestamp
    property Filename: string read FFilename write FFilename; // mediawiki/siteinfo/page/id
    property BaseURL: string read FBaseURL write FBaseURL; // ExtractFilePath(mediawiki/siteinfo/base)
    property Verbosity: TWikiPageVerbosity read FVerbosity write FVerbosity;
    property AutoFixUTF8: boolean read FAutoFixUTF8 write SetAutoFixUTF8;
    procedure FixUTF8;
    property Src: string read FSrc write SetSrc;
    function StrPos(p: PChar): integer;
    function PosToStr(p: PChar; WithFilename: boolean = false): string;
    function PosToStr(p: integer; WithFilename: boolean = false): string;
    function AtLineStart(p: PChar): boolean;
    function TrimLink(const Link: string): string;
    function CurrentPos: integer;
    property LanguageTags: TKeyWordFunctionList read FLanguageTags write FLanguageTags;
    procedure Log(Msg: string);
    property OnLog: TWikiOnLog read FOnLog write FOnLog;
  end;

var
  IsWikiTagStartChar,
  IsWikiTagChar: array[char] of boolean;

// normalize link to get the page, e.g. convert spaces to underscores, delete #0,$[]{}<>
function WikiTitleToPage(Link: string): string;
function WikiIsExternalLink(Link: string): boolean;

function GetWikiPageID(doc: TDOMNode): string;
function GetWikiPageID(s: TStream): string;
function WikiPageToCaseID(Page: string): string; // create a bit vector for each letter

function dbgs(t: TWPTokenType): string; overload;
function dbgs(r: TWPTokenRange): string; overload;

implementation

{ TWPToken }

constructor TWPToken.Create(ThePage: TWikiPage; TheUserDate: Pointer);
begin
  Page:=ThePage;
  UserData:=TheUserDate;
end;

{ TWikiPage }

function TWikiPage.StrPos(p: PChar): integer;
begin
  Result:=p-PChar(FSrc)+1;
end;

function TWikiPage.AtLineStart(p: PChar): boolean;
begin
  Result:=(p=PChar(FSrc)) or (p[-1] in [#10,#13]);
end;

function TWikiPage.PosToStr(p: PChar; WithFilename: boolean): string;
begin
  if (p=nil) then
    Result:='(nil)'
  else if (Src='') then
    Result:='(outside)'
  else if p<PChar(FSrc) then
    Result:='(invalid pos <0)'
  else begin
    Result:=PosToStr(StrPos(p),WithFilename);
  end;
end;

function TWikiPage.PosToStr(p: integer; WithFilename: boolean): string;
var
  y: Integer;
  x: integer;
  s: String;
begin
  if SrcPosToLineCol(FSrc,p,y,x) then
    Result:='('+IntToStr(y)+','+IntToStr(x)+')'
  else
    Result:='(outside)';
  if WithFilename then begin
    s:='';
    if Filename<>'' then
      s:=ExtractFilename(Filename)
    else if Title<>'' then begin
      s:=Title;
      if length(s)>40 then
        s:=LeftStr(s,19)+'...'+RightStr(s,19);
    end;
    Result:=Result+' in "'+s+'"'
  end;
end;

procedure TWikiPage.SetAutoFixUTF8(AValue: boolean);
begin
  if FAutoFixUTF8=AValue then Exit;
  FAutoFixUTF8:=AValue;
  if FAutoFixUTF8 then
    FixUTF8;
end;

procedure TWikiPage.SetSrc(AValue: string);
begin
  if FSrc=AValue then Exit;
  FSrc:=AValue;
  if AutoFixUTF8 then FixUTF8;
end;

function TWikiPage.TokenIs(Tag: PChar): boolean;
var
  p2: PChar;
begin
  p2:=FCurP;
  while (p2^<>#0) and (UpChars[p2^]=UpChars[Tag^]) do begin
    inc(p2);
    inc(Tag);
  end;
  Result:=Tag^=#0;
end;

procedure TWikiPage.ClearStack;
begin
  ReAllocMem(FStack,0);
  FStackCapacity:=0;
  FStackPtr:=-1;
  FInPre:=0;
end;

procedure TWikiPage.Push(Token: TWPTokenType; StartPos: PChar);
var
  NewCapacity: Integer;
  Item: PStackItem;
begin
  inc(FStackPtr);
  {$IFDEF VerboseWikiStack}
  Log(['Push :',GetIndentStr(FStackPtr*2),dbgs(Token),' at ',PosToStr(FCurP)]);
  {$ENDIF}
  if FStackPtr>=FStackCapacity then begin
    NewCapacity:=FStackCapacity*2+8;
    ReAllocMem(FStack,SizeOf(TWPStackItem)*NewCapacity);
    FStackCapacity:=NewCapacity;
  end;
  Item:=@FStack[FStackPtr];
  Item^.Token:=Token;
  Item^.StartPos:=StartPos;
  if Token in [wptPre,wptPreTag] then
    inc(FInPre);
  OpenRangeToken(Token);
end;

function TWikiPage.Pop(Token: TWPTokenType): boolean;

  procedure LogMissingClose;
  var
    Item: PStackItem;
  begin
    Item:=@FStack[FStackPtr];
    Log('TWikiPage.Pop WARNING: missing closing for '+dbgs(Item^.Token)+' at '+PosToStr(FCurP,true));
  end;

  procedure LogNotOpen;
  begin
    Log('TWikiPage.Pop Hint: tag was not open: '+dbgs(Token)+' at '+PosToStr(FCurP,true));
  end;

var
  i: Integer;
  Group: TWPTokenGroup;
  Item: PStackItem;
begin
  Result:=false;
  i:=FStackPtr;
  Group:=WPTokenInfos[Token].Group;
  while (i>=0) and (ord(WPTokenInfos[FStack[i].Token].Group) <= ord(Group)) do
  begin
    if FStack[i].Token=Token then begin
      // found
      while FStackPtr>=i do begin
        Item:=@FStack[FStackPtr];
        if (Verbosity>=wpvWarning)
        and (FStackPtr>i) and (wpifWarnOnAutoClose in WPTokenInfos[Item^.Token].Flags)
        then
          LogMissingClose;
        {$IFDEF VerboseWikiStack}
        Log('Pop  :'+GetIndentStr(FStackPtr*2)+dbgs(Item^.Token)+' at '+PosToStr(FCurP));
        {$ENDIF}
        if Item^.Token in [wptPre,wptPreTag] then
          dec(FInPre);
        CloseRangeToken(Item^.Token);
        dec(FStackPtr);
      end;
      exit(true);
    end;
    dec(i);
  end;
  // not found
  if Verbosity>=wpvHint then
    LogNotOpen;
end;

procedure TWikiPage.Pop(Index: integer);
begin
  if Index<0 then Index:=0;
  while FStackPtr>=Index do
    Pop(TopToken);
end;

function TWikiPage.TopToken: TWPTokenType;
begin
  if FStackPtr>=0 then
    Result:=FStack[FStackPtr].Token
  else
    Result:=wptText;
end;

function TWikiPage.FindGroupStackPos(Group: TWPTokenGroup; OrHigher: boolean
  ): integer;
var
  CurGroup: TWPTokenGroup;
begin
  Result:=FStackPtr;
  while (Result>=0) do begin
    CurGroup:=WPTokenInfos[FStack[Result].Token].Group;
    if (ord(CurGroup)>=ord(Group)) then begin
      if (not OrHigher) and (CurGroup<>Group) then
        exit(-1);
      exit;
    end;
    dec(Result);
  end;
end;

function TWikiPage.FindStackItem(Typ: TWPTokenType): integer;
begin
  Result:=FStackPtr;
  while (Result>=0) and (FStack[Result].Token<>Typ) do dec(Result);
end;

procedure TWikiPage.DoToken(Token: TWPToken);
{$IFDEF VerboseWikiOnToken}
var
  i: Integer;
{$ENDIF}
begin
  Token.Token:=WPTokenInfos[Token.SubToken].BaseToken;
  {$IFDEF VerboseWikiOnToken}
  i:=FStackPtr;
  if i<0 then i:=0;
  Log('Token:'+GetIndentStr(i*2)+dbgs(Token.Token)+' at '+PosToStr(FCurP));
  {$ENDIF}
  FOnToken(Token);
end;

procedure TWikiPage.EmitTextToken;
begin
  if FStackPtr<0 then begin
    // highest level => skip space at start
    while (FLastEmitPos<FCurP) and (FLastEmitPos^ in [#1..#32]) do
      inc(FLastEmitPos);
  end;
  if FCurP<=FLastEmitPos then exit;

  if (FStackPtr<0) or (TopToken=wptSection) then begin
    // highest level => start a paragraph
    Push(wptSubSection,FCurP);
  end;
  // maybe: add an option and when enabled combine multiple spaces and replace line breaks with space
  FTextToken.SubToken:=wptText;
  FTextToken.Range:=wprNone;
  FTextToken.StartPos:=StrPos(FLastEmitPos);
  FTextToken.EndPos:=StrPos(FCurP);
  FLastEmitPos:=FCurP;
  DoToken(FTextToken);
end;

procedure TWikiPage.ParseAttributes(StartPos, EndPos: PChar);
var
  p: PChar;
begin
  //Log('TWikiPage.ParseAttributes '+PosToStr(StartPos)+' '+PosToStr(EndPos)+' <'+dbgstr(StartPos,EndPos-StartPos),'>');
  p:=StartPos;
  repeat
    // skip whitespace
    while p^ in [' ',#9,#10,#13] do inc(p);
    if p>=EndPos then break;
    // read name
    if not IsIdentStartChar[p^] then break;
    FNameValueToken.NameStartPos:=StrPos(p);
    while IsIdentChar[p^] do inc(p);
    FNameValueToken.NameEndPos:=StrPos(p);
    // whitespace
    while p^ in [' ',#9,#10,#13] do inc(p);
    if p>=EndPos then break;
    // =
    if p^<>'=' then break;
    inc(p);
    // whitespace
    while p^ in [' ',#9,#10,#13] do inc(p);
    if p>=EndPos then break;
    // value
    if p^<>'"' then break;
    inc(p);
    FNameValueToken.ValueStartPos:=StrPos(p);
    while not (p^ in ['"',#0]) do inc(p);
    if p^<>'"' then break;
    FNameValueToken.ValueEndPos:=StrPos(p);
    if p>=EndPos then break;
    FNameValueToken.SubToken:=wptAttribute;
    DoToken(FNameValueToken);
    inc(p);
  until p>=EndPos;
  //Log(['TWikiPage.ParseAttributes stopped at <'+dbgstr(StartPos,p-StartPos)+'>');
end;

procedure TWikiPage.ParseNoWiki;
begin
  // ignore all tags
  // this is not the same as pre (preformatted treats spaces and line breaks)
  EmitTextToken;
  FCurP:=FindTagEnd(FCurP);
  FLastEmitPos:=FCurP;
  repeat
    case FCurP^ of
    #0: break;
    '<':
      if TokenIs('</nowiki>') then
        break;
    end;
    inc(FCurP);
  until false;
  EmitTextToken;
  FCurP:=FindTagEnd(FCurP);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.CloseTableCell;
var
  t: TWPTokenType;
begin
  while FStackPtr>=0 do begin
    t:=TopToken;
    if (t in [wptTableCell,wptTableHeadCell])
    or (WPTokenInfos[t].Group<wpgTable) then
      Pop(t)
    else
      exit;
  end;
end;

function TWikiPage.TrimLink(const Link: string): string;
begin
  Result:=UTF8Trim(Link);
end;

function TWikiPage.CurrentPos: integer;
begin
  Result:=StrPos(FCurP);
end;

procedure TWikiPage.Log(Msg: string);
begin
  if Assigned(OnLog) then
    OnLog(Msg)
  else
    debugln(Msg);
end;

procedure TWikiPage.CloseRangeToken(Typ: TWPTokenType);
begin
  FRangeToken.SubToken:=Typ;
  FRangeToken.Range:=wprClose;
  DoToken(FRangeToken);
end;

procedure TWikiPage.OpenRangeToken(Typ: TWPTokenType);
begin
  FRangeToken.SubToken:=Typ;
  FRangeToken.Range:=wprOpen;
  DoToken(FRangeToken);
end;

function TWikiPage.FindTagEnd(TagStart: PChar): PChar;
begin
  Result:=TagStart;
  if Result^='<' then inc(Result);
  if Result^='/' then inc(Result);
  while IsWikiTagChar[Result^] do inc(Result);
  while Result^<>#0 do begin
    case Result^ of
    #0,'<','>','/': break;
    '"':
      repeat
        inc(Result);
      until Result^ in ['"','>','<',#0];
    '''':
      repeat
        inc(Result);
      until Result^ in ['''','>','<',#0];
    else
      inc(Result);
    end;
  end;
  if Result^='/' then inc(Result);
  if Result^='>' then inc(Result);
end;

procedure TWikiPage.HandleUnderScore;
begin
  if (FCurP[1]='_') and (AtLineStart(FCurP)) and TokenIs('__TOC__') then begin
    EmitTextToken;
    inc(FCurP, length('__TOC__'));
    FLastEmitPos:=FCurP;
  end else
   inc(FCurP);
end;

procedure TWikiPage.HandleEqual;
var
  Depth: Integer;
  i: Integer;
  OldDepth: Integer;
  t: TWPTokenType;
begin
  if (FInPre>0) then begin
    inc(FCurP);
    exit;
  end;
  // header => close section(s), start new section
  Depth:=1;
  while (Depth<MaxHeaderDepth) and (FCurP[Depth]='=') do inc(Depth);

  i:=0;
  OldDepth:=0;
  while (i<=FStackPtr) do begin
    t:=FStack[i].Token;
    if t=wptSection then
      inc(OldDepth)
    else if t=wptHeader then begin
      // this is the end of the header
      EmitTextToken;
      Pop(t);
      inc(FCurP,Depth);
      FLastEmitPos:=FCurP;
      exit;
    end;
    inc(i);
  end;
  // maybe new header
  //Log(['HandleHeader START '+PosToStr(FCurP)+' '+AtLineStart(FCurP));
  if not AtLineStart(FCurP) then begin
    // normal =
    inc(FCurP);
    exit;
  end;
  EmitTextToken;

  // close section(s)
  while (FStackPtr>=0) and (OldDepth>=Depth) do begin
    if FStack[FStackPtr].Token=wptSection then
      dec(OldDepth);
    Pop(TopToken);
  end;
  // start new section(s) (it is allowed to start a subsubsection without a subsection)
  for i:=OldDepth+1 to Depth do
    Push(wptSection,FCurP);
  // start header
  Push(wptHeader,FCurP);
  inc(FCurP,Depth);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.HandleListChar;

  function CharToListType(c: char): TWPTokenType;
  begin
    case c of
    '*': Result:=wptBulletList;
    '#': Result:=wptNumberedList;
    ':',';': Result:=wptDefinitionList;
    else Result:=wptText;
    end;
  end;

var
  NewDepth: Integer;
  i: Integer;
  CurDepth: Integer;
begin
  if (not AtLineStart(FCurP)) or (FInPre>0) then begin
    inc(FCurP);
    exit;
  end;
  EmitTextToken;
  NewDepth:=1;
  while FCurP[NewDepth] in ['*','#',':',';'] do inc(NewDepth);

  // a list closes all fonts and spans => skip all fonts and spans
  i:=FindGroupStackPos(wpgList,true);
  // check all lists with wiki syntax, keep lists with html syntax
  while (i>=0)
  and (FStack[i].Token in WPTWikiLists) do
    dec(i);
  inc(i);
  CurDepth:=0;
  while (CurDepth<NewDepth) do begin
    // compare old list hierarchy with new list hierarchy
    if (i>FStackPtr) or (FStack[i].Token<>CharToListType(FCurP[CurDepth])) then begin
      {dbgout(['TWikiPage.HandleListChar listtype does not fit: i=',i,' CurDepth=',CurDepth,' should=',dbgs(CharToListType(FCurP[CurDepth]))]);
      if i<=FStackPtr then dbgout(' is=',dbgs(FStack[i].Token));
      debugln;}
      // does not fit
      Pop(i);
      // start new list
      EmitFlag(CharToListType(FCurP[CurDepth]),wprOpen,0);
    end;
    inc(i);
    inc(CurDepth);
    if CurDepth=NewDepth then begin
      // close fonts, spans and previous list item
      //Log('TWikiPage.HandleListChar close fonts, spans, listitem');
      Pop(i);
    end;
    if (i>FStackPtr) then
      EmitFlag(wptListItem,wprOpen,0); // new list item
    if FStack[i].Token<>wptListItem then
      raise Exception.Create('broken list: should='+dbgs(wptListItem)+' is='+dbgs(FStack[i].Token));
    inc(i);
  end;

  inc(FCurP,NewDepth);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.HandleSpace;
var
  NonSpace: PChar;
begin
  if (not AtLineStart(FCurP)) or (FInPre>0) then begin
    inc(FCurP);
    exit;
  end;
  NonSpace:=FCurP;
  while (NonSpace^ in [' ',#9]) do inc(NonSpace);
  if NonSpace^ in [#10,#13,#0] then begin
    // empty line
    inc(FCurP);
    exit;
  end;
  // preformatted text
  //Log('TWikiPage.HandleSpace start pre "'+dbgstr(GetLineInSrc(Src,StrPos(FCurP)))+'"');
  // ToDo: flags
  EmitFlag(wptPre,wprOpen,1);
  repeat
    while not (FCurP^ in [#10,#13,#0]) do inc(FCurP);
    EmitTextToken;
    if FCurP^=#0 then break;
    if (FCurP[1] in [#10,#13]) and (FCurP^<>FCurP[1]) then
      inc(FCurP,2)
    else
      inc(FCurP);
    if FCurP^<>' ' then break;
    // next line is also preformatted
    inc(FCurP);
    //Log('TWikiPage.HandleSpace line break');
    FLastEmitPos:=FCurP;
    EmitFlag(wptLineBreak,wprNone,0);
  until false;
  //Log('TWikiPage.HandleSpace end pre');
  FLastEmitPos:=FCurP;
  EmitFlag(wptPre,wprClose,0);
end;

procedure TWikiPage.HandleCurlyBracketOpen;
begin
  if (FCurP[1]='{') and (FInPre=0) then begin
    // {{special}} or {{name|special}}
    EmitTextToken;
    inc(FCurP,2);
    FNameValueToken.NameStartPos:=StrPos(FCurP);
    repeat
      case FCurP^ of
      #0..#31,'|': break;
      '}': if FCurP[1]='}' then break;
      end;
      inc(FCurP);
    until false;
    if FCurP^='|' then begin
      FNameValueToken.NameEndPos:=StrPos(FCurP);
      inc(FCurP);
      FNameValueToken.ValueStartPos:=StrPos(FCurP);
    end else begin
      FNameValueToken.NameEndPos:=FNameValueToken.NameStartPos;
      FNameValueToken.ValueStartPos:=FNameValueToken.NameStartPos;
    end;
    repeat
      case FCurP^ of
      #0..#31: break;
      '}': if FCurP[1]='}' then break;
      end;
      inc(FCurP);
    until false;
    FNameValueToken.ValueEndPos:=StrPos(FCurP);
    if FCurP^='}' then inc(FCurP,2);
    FLastEmitPos:=FCurP;
    FNameValueToken.SubToken:=wptSpecial;
    DoToken(FNameValueToken);
  end else if (FCurP[1]='|') and AtLineStart(FCurP) and (FInPre=0) then begin
    // {| table
    EmitTextToken;
    EmitFlag(wptTable,wprOpen,2);
    // rest of line are attributes
    while not (FCurP^ in [#0,#10,#13]) do inc(FCurP);
    ParseAttributes(FLastEmitPos,FCurP);
    while FCurP^ in [#10,#13] do inc(FCurP);
    if (FCurP^='|') and (FCurP[1]='+') then begin
      // table caption
      FLastEmitPos:=FCurP;
      EmitFlag(wptTableRow,wprOpen,2);
      EmitFlag(wptTableHeadCell,wprOpen,0);
    end;
    FLastEmitPos:=FCurP;
  end else
    inc(FCurP);
end;

procedure TWikiPage.HandlePipe;
var
  i: Integer;
begin
  i:=FindGroupStackPos(wpgTable,false);
  if i>=0 then begin
    // in a table
    if AtLineStart(FCurP) then begin
      if (FCurP[1]='-') then begin
        // new row
        CloseTableCell;
        if TopToken=wptTableRow then
          Pop(wptTableRow);
        EmitFlag(wptTableRow,wprOpen,2);
        while FCurP^='-' do inc(FCurP);
        FLastEmitPos:=FCurP;
        // attributes
        while not (FCurP^ in [#0,#10,#13]) do inc(FCurP);
        ParseAttributes(FLastEmitPos,FCurP);
        FLastEmitPos:=FCurP;
        exit;
      end else if FCurP[1]='}' then begin
        // |} end of table
        EmitFlag(wptTable,wprClose,2);
        exit;
      end;
    end;
    if AtLineStart(FCurP) or (FCurP[1]='|') then begin
      ParseCell;
      exit;
    end;
  end;
  inc(FCurP);
end;

procedure TWikiPage.HandleExclamationMark;
var
  i: Integer;
begin
  i:=FindGroupStackPos(wpgTable,false);
  if i>=0 then begin
    // in a table
    if AtLineStart(FCurP) then begin
      ParseCell;
      exit;
    end;
  end;
  inc(FCurP);
end;

procedure TWikiPage.HandleApostroph;
begin
  if FCurP[1]='''' then begin
    if FCurP[2]='''' then begin
      // bold
      EmitToggle(wptBold, 3);
    end else begin
      // italic
      EmitToggle(wptItalic, 2);
    end;
  end else begin
    // normal apostroph
    inc(FCurP);
  end;
end;

procedure TWikiPage.HandleEdgedBracketOpen;
var
  p: PChar;
begin
  if FCurP[1] in [#0..#31,' '] then begin
    inc(FCurP);
    exit;
  end;
  EmitTextToken;
  inc(FCurP);
  // link
  if FCurP^='[' then begin
    // internal link
    // for example [[url|caption]]
    inc(FCurP);
    FLinkToken.SubToken:=wptInternLink;
    FLinkToken.LinkStartPos:=StrPos(FCurP);
    while not (FCurP^ in [#0..#31, '|', ']']) do inc(FCurP);
    FLinkToken.LinkEndPos:=StrPos(FCurP);
    FLinkToken.Link:=TrimLink(copy(Src,FLinkToken.LinkStartPos,FLinkToken.LinkEndPos-FLinkToken.LinkStartPos));
    FLinkToken.CaptionStartPos:=FLinkToken.LinkStartPos;
    FLinkToken.CaptionEndPos:=FLinkToken.LinkEndPos;
  end else begin
    // external link
    // for example [url|caption] or [url caption]
    p:=FCurP;
    if not IsIdentStartChar[p^] then exit; // not a valid scheme
    inc(p);
    while IsIdentChar[p^] do inc(p);
    if (p^<>':') or (p[1]<>'/') or (p[2]<>'/') then exit; // not a valid scheme
    FLinkToken.SubToken:=wptExternLink;
    FLinkToken.LinkStartPos:=StrPos(FCurP);
    while not (FCurP^ in [#0..#31, ' ' , '|' , ']']) do inc(FCurP);
    FLinkToken.LinkEndPos:=StrPos(FCurP);
    FLinkToken.Link:=TrimLink(copy(Src,FLinkToken.LinkStartPos,FLinkToken.LinkEndPos-FLinkToken.LinkStartPos));
    if FCurP^=' ' then begin
      // separate caption
      inc(FCurP);
      FLinkToken.CaptionStartPos:=StrPos(FCurP);
      while not (FCurP^ in [#0..#31, '|', ']']) do inc(FCurP);
      FLinkToken.CaptionEndPos:=StrPos(FCurP);
    end else begin
      // caption = URL
      FLinkToken.CaptionStartPos:=FLinkToken.LinkStartPos;
      FLinkToken.CaptionEndPos:=FLinkToken.LinkEndPos;
    end;
  end;

  if (BaseURL<>'')
  and (LeftStr(FLinkToken.Link,length(BaseURL))=BaseURL) then begin
    // a link to a wiki page, but with full URL => shorten
    FLinkToken.SubToken:=wptInternLink;
    Delete(FLinkToken.Link,1,length(BaseURL));
    while (FLinkToken.Link<>'') and (FLinkToken.Link[1]='/') do
      Delete(FLinkToken.Link,1,1);
  end;

  if FCurP^='|' then begin
    // link with caption
    inc(FCurP);
    FLinkToken.CaptionStartPos:=StrPos(FCurP);
    while not (FCurP^ in [#0..#31, ']']) do inc(FCurP);
    FLinkToken.CaptionEndPos:=StrPos(FCurP);
  end;

  if FCurP^=']' then begin
    inc(FCurP);
    if (FLinkToken.SubToken=wptInternLink) and (FCurP^=']') then
      inc(FCurP);
    DoToken(FLinkToken);
  end;
  FLastEmitPos:=FCurP;

  // ToDo: implement postfix notation [[url]]caption and [[url]]''caption''

end;

procedure TWikiPage.ParseCell;
var
  NextBar: PChar;
begin
  // linestart | or linestart ! or ||
  // => new cell
  // => close previous cell
  EmitTextToken;
  CloseTableCell;
  if TopToken=wptTable then
    EmitFlag(wptTableRow, wprOpen, 0);

  if AtLineStart(FCurP) then begin
    FInTableHeader := (FCurP^='!');
    if FInTableHeader then
      EmitFlag(wptTableHeadCell, wprOpen, 1)    // linestart !
    else
      EmitFlag(wptTableCell, wprOpen, 1)        // linestart |
  end else begin
    if FInTableHeader then
      EmitFlag(wptTableHeadCell, wprOpen, 2)    // !!
    else
      EmitFlag(wptTableCell, wprOpen, 2);       // ||
  end;
  NextBar:=FCurP;
  while not (NextBar^ in [#0, #10, #13, '|']) do begin
    if NextBar^='[' then begin
      // a link
      break;
    end else if (NextBar^='<') and IsIdentStartChar[NextBar[1]] then begin
      // a tag
      break;
    end;
    inc(NextBar);
  end;
  if (NextBar^='|') and (NextBar[1]<>'|') then begin
    // the text in front of the first single | are attributes
    ParseAttributes(FCurP, NextBar);
    FCurP:=NextBar+1;
  end;
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.HandleAngleBracket;

  procedure UnknownTag;
  begin
    // unknown tag
    if Verbosity>=wpvWarning then begin
      if IsWikiTagStartChar[FCurP[1]] then begin
        {$IFDEF VerboseUnknownOpenTags}
        Log('WARNING: TWikiPage.Parse unknown opening tag: <'+GetIdentifier(FCurP+1)+'> at '+PosToStr(FCurP,true));
        {$ENDIF}
      end else if (FCurP[1]='/') and IsWikiTagStartChar[FCurP[2]] then
        Log('WARNING: TWikiPage.Parse unknown closing tag: </'+GetIdentifier(FCurP+2)+'> at '+PosToStr(FCurP,true))
      else
        Log('WARNING: TWikiPage.Parse broken close tag at '+PosToStr(FCurP,true));
    end;
    inc(FCurP);
  end;

var
  TagEndP: PChar;
  Range: TWPTokenRange;
  NameP: PChar;
begin
  NameP:=FCurP+1;
  if NameP^='/' then begin
    Range:=wprClose;
    inc(NameP);
  end else
    Range:=wprOpen;
  if IsWikiTagStartChar[NameP^] then begin
    TagEndP:=FindTagEnd(FCurP);
    if ((TagEndP-1)^='>') and ((TagEndP-2)^='/') then begin
      // e.g. <br/>
      if CompareIdentifiers(NameP,'br')=0 then EmitTag(wptLineBreak,
        wprNone)
      else if CompareIdentifiers(NameP,'p')=0 then EmitTag(wptPTag,
        wprNone)
      else UnknownTag;
    end
    else if CompareIdentifiers(NameP,'br')=0 then EmitTag(wptLineBreak, wprNone)
    else if CompareIdentifiers(NameP,'b')=0 then EmitTag(wptBoldTag,Range)
    else if CompareIdentifiers(NameP,'i')=0 then EmitTag(wptItalicTag,Range)
    else if CompareIdentifiers(NameP,'u')=0 then EmitTag(wptUnderlineTag,Range)
    else if CompareIdentifiers(NameP,'s')=0 then EmitTag(wptStrikeTagShort, Range)
    else if CompareIdentifiers(NameP,'strike')=0 then EmitTag(wptStrikeTagLong, Range)
    else if CompareIdentifiers(NameP,'tt')=0 then EmitTag(wptTT,Range)
    else if CompareIdentifiers(NameP,'sup')=0 then EmitTag(wptSup,Range)
    else if CompareIdentifiers(NameP,'sub')=0 then EmitTag(wptSub,Range)
    else if CompareIdentifiers(NameP,'small')=0 then EmitTag(wptSmall,Range)
    else if CompareIdentifiers(NameP,'em')=0 then EmitTag(wptEm,Range)
    else if CompareIdentifiers(NameP,'string')=0 then EmitTag(wptString, Range)
    else if CompareIdentifiers(NameP,'var')=0 then EmitTag(wptVar,Range)
    else if CompareIdentifiers(NameP,'key')=0 then EmitTag(wptKey,Range)
    else if CompareIdentifiers(NameP,'cmt')=0 then EmitTag(wptCmt,Range)
    else if CompareIdentifiers(NameP,'span')=0 then EmitTag(wptSpan,Range)
    else if CompareIdentifiers(NameP,'p')=0 then EmitTag(wptPTag,Range)
    else if CompareIdentifiers(NameP,'div')=0 then EmitTag(wptDivTag,Range)
    else if CompareIdentifiers(NameP,'pre')=0 then EmitTag(wptPreTag,Range)
    else if CompareIdentifiers(NameP,'center')=0 then EmitTag(wptCenter,Range)
    else if CompareIdentifiers(NameP,'ol')=0 then EmitTag(wptOrderedListTag,Range)
    else if CompareIdentifiers(NameP,'ul')=0 then EmitTag(wptUnorderedListTag,Range)
    else if (CompareIdentifiers(NameP,'li')=0) and (TopToken in [wptOrderedListTag,wptUnorderedListTag])
    then EmitTag(wptUnorderedListTag, Range)
    else if CompareIdentifiers(NameP,'table')=0 then EmitTag(wptTableTag,Range)
    else if CompareIdentifiers(NameP,'tr')=0 then EmitTag(wptTableRowTag,Range)
    else if CompareIdentifiers(NameP,'td')=0 then EmitTag(wptTableCellTag,Range)
    else if CompareIdentifiers(NameP,'th')=0 then EmitTag(wptTableHeadCellTag,Range)
    else if CompareIdentifiers(NameP,'h1')=0 then EmitTag(wptHeader1,Range)
    else if CompareIdentifiers(NameP,'h2')=0 then EmitTag(wptHeader2,Range)
    else if CompareIdentifiers(NameP,'h3')=0 then EmitTag(wptHeader3,Range)
    else if (Range=wprOpen)
        and (FLanguageTags<>nil)
        and FLanguageTags.DoIdentifier(NameP)
    then begin
      // special parse for different language
      //Log('TWikiPage.Parse code tag '+dbgs(Pointer(FCurP))+' tag='+GetIdentifier(NameP)+' '+FindTagEnd(FCurP)-FCurP);
      HandleCode;
    end else if TokenIs('<nowiki>') then begin
      ParseNoWiki;
    end else begin
      UnknownTag;
    end;
  end else begin
    // normal <
    inc(FCurP);
  end;
end;

procedure TWikiPage.HandleCode;
var
  p: PChar;
  NameP: PChar;
begin
  if (FCurP^<>'<') or (not IsIdentStartChar[FCurP[1]]) then begin
    inc(FCurP);
    exit;
  end;
  EmitTextToken;
  p:=FCurP+1;
  NameP:=p;
  // by default the lange is the tag, e.g. "pascal" of <pascal>
  FNameValueToken.NameStartPos:=StrPos(p);
  while IsIdentChar[p^] do inc(p);
  FNameValueToken.NameEndPos:=StrPos(p);
  while p^ in [' ',#9,#10,#13] do inc(p);
  if CompareIdentifiers(p,'lang')=0 then begin
    // read language from lang attribute
    // e.g. <code lang=pascal">
    inc(p,4);
    while p^ in [' ',#9,#10,#13] do inc(p);
    if p^='=' then begin
      inc(p);
      while p^ in [' ',#9,#10,#13] do inc(p);
      if p^='"' then begin
        inc(p);
        FNameValueToken.NameStartPos:=StrPos(p);
        while not (p^ in ['"',#0,'<','>']) do inc(p);
        FNameValueToken.NameEndPos:=StrPos(p);
        inc(p);
      end;
    end;
  end;
  p:=FindTagEnd(FCurP);
  FNameValueToken.ValueStartPos:=StrPos(p);
  repeat
    case p^ of
    #0:
      break;
    '<':
      if (p[1]='/') and (CompareIdentifiers(NameP, p+2)=0) then
        break;
    end;
    inc(p);
  until false;
  FNameValueToken.ValueEndPos:=StrPos(p);
  FCurP:=FindTagEnd(p);
  FNameValueToken.SubToken:=wptCode;
  //Log('TWikiPage.HandleCode name="'+copy(Src,FNameValueToken.NameStartPos,FNameValueToken.NameEndPos-FNameValueToken.NameStartPos)+'"');
  DoToken(FNameValueToken);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.EmitFlag(Typ: TWPTokenType; Range: TWPTokenRange;
  TagLen: integer);
begin
  EmitTextToken;
  if ord(WPTokenInfos[Typ].Group)>ord(wpgFont) then begin
    // auto close paragraph
    while TopToken=wptP do
      Pop(wptP);
  end else if (Range=wprOpen) and (WPTokenInfos[Typ].Group=wpgFont) then begin
    // font changes
    if (FStackPtr<0) or (TopToken=wptSection) then begin
      // highest level => start a sub section
      Push(wptSubSection,FCurP);
    end;
  end;
  if Range=wprOpen then begin
    Push(Typ,FCurP);
  end
  else if Range=wprClose then
    Pop(Typ)
  else begin
    FRangeToken.SubToken:=Typ;
    FRangeToken.Range:=Range;
    DoToken(FRangeToken);
  end;
  inc(FCurP,TagLen);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.EmitToggle(Typ: TWPTokenType; TagLen: integer);
var
  i: Integer;
begin
  EmitTextToken;
  i:=FStackPtr;
  while (i>=0) do begin
    if FStack[i].Token=Typ then begin
      // disable
      Pop(Typ);
      break;
    end;
    if (WPTokenInfos[FStack[i].Token].Group<>wpgFont) then begin
      // toggles can only skip the font group
      i:=-1;
      break;
    end;
    dec(i);
  end;
  if i<0 then begin
    // enable
    Push(Typ,FCurP);
  end;
  inc(FCurP,TagLen);
  FLastEmitPos:=FCurP;
end;

procedure TWikiPage.EmitTag(Typ: TWPTokenType; Range: TWPTokenRange);

  function GetAttributesStart: PChar;
  var
    p: PChar;
  begin
    Result:=nil;
    p:=FCurP;
    if p^<>'<' then exit;
    inc(p);
    while IsWikiTagChar[p^] do inc(p);
    while p^ in [' ',#9] do inc(p); // wiki does not allow multiline attributes
    if not IsWikiTagChar[p^] then exit;
    Result:=p;
  end;

var
  p: PChar;
  StartPos: PChar;
begin
  if Range<>wprClose then begin
    StartPos:=GetAttributesStart;
    if StartPos<>nil then begin
      // has attributes
      p:=StartPos;
      while not (p^ in [#0,#10,#13,'>']) do inc(p);
      if p^='>' then
        inc(p);
      EmitFlag(Typ,wprOpen,p-FCurP);
      ParseAttributes(StartPos,p);
      if Range=wprNone then
        Pop(Typ);
      exit;
    end;
  end;
  // has no attributes
  EmitFlag(Typ,Range,FindTagEnd(FCurP)-FCurP);
end;

procedure TWikiPage.EmitLineBreak;
begin
  if FCurP[1] in [#10,#13] then
    EmitFlag(wptLineBreak,wprNone,2)
  else
    EmitFlag(wptLineBreak,wprNone,1);
  inc(FLine);
end;

constructor TWikiPage.Create;
begin
  FStackPtr:=-1;
  Verbosity:=wpvHint;
end;

destructor TWikiPage.Destroy;
begin
  ClearStack;
  inherited Destroy;
end;

procedure TWikiPage.LoadFromFile(Filename: string);
var
  doc: TXMLDocument;
begin
  doc:=nil;
  try
    ReadXMLFile(doc,Filename);
    LoadFromDoc(doc);
  finally
    doc.Free;
  end;
end;

procedure TWikiPage.LoadFromDoc(doc: TDOMNode);
var
  Node: TDOMNode;
  ParentName: DOMString;
  GrandParentName: String;
  Data: DOMString;
  p: Integer;
begin
  for Node in doc.GetEnumeratorAllChildren do begin
    ParentName:='';
    GrandParentName:='';
    if Node.ParentNode is TDOMElement then begin
      ParentName:=TDOMElement(Node.ParentNode).TagName;
      if Node.ParentNode.ParentNode is TDOMElement then
        GrandParentName:=TDOMElement(Node.ParentNode.ParentNode).TagName;
    end;
    if Node is TDOMText then begin
      Data:=TDOMText(Node).Data;
      if (GrandParentName='page') then begin
        if ParentName='id' then
          ID:=Data
        else if ParentName='title' then
          Title:=Data;
      end else if GrandParentName='revision' then begin
        if ParentName='id' then
          Revision:=Data
        else if ParentName='timestamp' then
          TimeStamp:=Data
        else if ParentName='text' then
          Src:=Data;
      end else if (ParentName='base') and (GrandParentName='siteinfo') then begin
        p:=length(Data);
        while (p>=1) and (Data[p]<>'/') do dec(p);
        BaseURL:=copy(Data,1,p-1);
      end;
    end;
  end;
end;

procedure TWikiPage.Parse(const OnToken: TWikiTokenEvent; Data: Pointer);
begin
  if FSrc='' then exit;
  FOnToken:=OnToken;
  FCurP:=PChar(FSrc);
  FLine:=1;
  FLastEmitPos:=FCurP;
  ClearStack;
  try
    FTextToken:=TWPTextToken.Create(Self,Data);
    FRangeToken:=TWPToken.Create(Self,Data);
    FLinkToken:=TWPLinkToken.Create(Self,Data);
    FNameValueToken:=TWPNameValueToken.Create(Self,Data);
    while FCurP^<>#0 do begin
      case FCurP^ of
      { wp: this case is responsible that backslashes do no appear in converted
            windows pathnames. I don't know if a backslash has a special meaning
            in the wiki syntax.
      '\':
        begin
          // special character as normal character
          EmitTextToken;
          inc(FCurP);
          FLastEmitPos:=FCurP;
          if FCurP^<>#0 then inc(FCurP);
        end;
      }
      #10,#13:
        begin
          EmitTextToken;
          if (FCurP[1] in [#10,#13]) and (FCurP^<>FCurP[1]) then
            inc(FCurP,2)
          else
            inc(FCurP);
          if FCurP^ in [#10,#13] then begin
            // empty line(s) closes lists, paragraphs and subsections
            while TopToken in ([wptP,wptSubSection]+WPTWikiLists) do
              Pop(TopToken);
            while FCurP^ in [#10,#13] do inc(FCurP);
          end;
          // line breaks closes head cells
          if TopToken=wptTableHeadCell then
            Pop(wptTableHeadCell);
        end;

      '''': HandleApostroph;
      '{':  HandleCurlyBracketOpen;
      '|':  HandlePipe;
      '!':  HandleExclamationMark;
      '=':  HandleEqual;
      '_':  HandleUnderScore;
      '[':  HandleEdgedBracketOpen;
      '<':  HandleAngleBracket;
      '*','#',':',';':  HandleListChar;
      ' ':  HandleSpace;

      '-':
        if (FCurP[1]='-') and AtLineStart(FCurP) and TokenIs('----') then
          EmitFlag(wptHorizontalRow,wprNone,4)
        else
          inc(FCurP);

      else
        inc(FCurP);
      end;
    end;
    EmitTextToken;
    while FStackPtr>=0 do
      Pop(TopToken);
  finally
    FreeAndNil(FRangeToken);
    FreeAndNil(FTextToken);
    FreeAndNil(FLinkToken);
    FreeAndNil(FNameValueToken);
    ClearStack;
  end;
end;

procedure TWikiPage.FixUTF8;
begin
  UTF8FixBroken(FSrc);
end;

procedure Init;
var
  c: Char;
begin
  for c:=low(char) to high(char) do begin
    IsWikiTagStartChar[c]:=c in ['a'..'z','A'..'Z','_',#192..#255];
    IsWikiTagChar[c]:=c in ['a'..'z','A'..'Z','_','0'..'9',#128..#255];
  end;
end;

function WikiTitleToPage(Link: string): string;
var
  i: Integer;
  j: Integer;
  c: Char;
  Code: Integer;
begin
  Result:=Link;
  i:=length(Result);
  while i>0 do begin
    case Result[i] of
    ' ',#9:
      Result[i]:='_';
    #0..#8,#10..#31,'$','[',']','{','}','<','>':
      Delete(Result,i,1);
    '%':
      begin
        Code:=0;
        j:=1;
        while (i+j<=length(Result)) do begin
          c:=Result[i+j];
          case c of
          '0'..'9': if Code<16 then Code:=Code*16+ord(c)-ord('0');
          'a'..'f': if Code<16 then Code:=Code*16+ord(c)-ord('a')+10;
          'A'..'F': if Code<16 then Code:=Code*16+ord(c)-ord('A')+10;
          else break;
          end;
          if j=2 then break;
          inc(j);
        end;
        ReplaceSubstring(Result,i,j+1,chr(Code));
        continue; // check the new character
      end;
    end;
    dec(i);
  end;
end;

function WikiIsExternalLink(Link: string): boolean;
// check if Link starts with a scheme http://
var
  p: PChar;
begin
  Result:=false;
  if Link='' then exit;
  p:=PChar(Link);
  while p^ in ['a'..'z','A'..'Z'] do inc(p);
  if p=PChar(Link) then exit;
  if p^<>':' then exit;
  inc(p);
  if p^<>'/' then exit;
  inc(p);
  if p^<>'/' then exit;
  inc(p);
  Result:=true;
end;

function GetWikiPageID(doc: TDOMNode): string;
var
  Node: TDOMNode;
begin
  Result:='';
  for Node in doc.GetEnumeratorAllChildren do begin
    if (Node is TDOMText)
    and (Node.ParentNode is TDOMElement)
    and (TDOMElement(Node.ParentNode).TagName='id')
    and (Node.ParentNode.ParentNode is TDOMElement)
    and (TDOMElement(Node.ParentNode.ParentNode).TagName='page') then begin
      Result:=TDOMText(Node).Data;
    end;
  end;
end;

function GetWikiPageID(s: TStream): string;
var
  doc: TXMLDocument;
begin
  doc:=nil;
  try
    Result:='';
    try
      ReadXMLFile(doc,s);
      Result:=GetWikiPageID(doc);
    except
    end;
  finally
    doc.Free;
  end;
end;

function WikiPageToCaseID(Page: string): string;
var
  CaseFlags: String;
  UpPage: String;
  PageP: PChar;
  PageUpP: PChar;
  CharLen: Integer;
  UpCharLen: Integer;
  n: Integer;
  i: Integer;
begin
  Result:='';
  if Page='' then exit;

  // for each letter check if it is uppercased
  CaseFlags:='';
  UpPage:=UTF8UpperCase(Page);
  PageP:=PChar(Page);
  PageUpP:=PChar(UpPage);
  while (PageP^<>#0) and (PageUpP^<>#0) do begin
    if PageP^='%' then begin
      // skip encoded characters, it does not matter if they are written lower or uppercase
      inc(PageP);
      inc(PageUpP);
      for i:=1 to 2 do begin
        if PageUpP^ in ['0'..'9','A'..'F'] then begin
          inc(PageP);
          inc(PageUpP);
        end;
      end;
    end else begin
      CharLen:=UTF8CodepointSize(PageP);
      UpCharLen:=UTF8CodepointSize(PageUpP);
      if (CharLen>1) or (PageP^ in ['a'..'z','A'..'Z']) then begin
        if (CharLen=UpCharLen) and CompareMem(PageP,PageUpP,CharLen) then
          CaseFlags:=CaseFlags+'u'
        else
          CaseFlags:=CaseFlags+'l';
      end;
      inc(PageP,CharLen);
      inc(PageUpP,UpCharLen);
    end;
  end;

  // encode bit vector (one character per 5bit)
  while CaseFlags<>'' do begin
    n:=0;
    for i:=1 to 5 do begin
      if i>length(CaseFlags) then break;
      n:=n*2;
      if CaseFlags[i]='u' then n+=1;
    end;
    case n of
    0..9: Result:=Result+chr(n+ord('0'));
    10..31: Result:=Result+chr(n-10+ord('a'));
    end;
    system.Delete(CaseFlags,1,5);
  end;
end;

function dbgs(t: TWPTokenType): string;
begin
  Result:=WPTokenInfos[t].Caption;
end;

function dbgs(r: TWPTokenRange): string;
begin
  Result:=WPTokenRangeNames[r];
end;

initialization
  Init;

end.

