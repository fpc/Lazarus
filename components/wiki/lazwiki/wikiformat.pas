{ Base classes for converters from wiki to something else.

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

}
unit WikiFormat;

{$mode objfpc}{$H+}

{off $DEFINE VerboseWikiFileCode}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, laz2_XMLRead, laz2_DOM, LazLoggerBase, LazUTF8, AvgLvlTree,
  // Codetools
  BasicCodeTools, KeywordFuncLists,
  // LazWiki
  WikiParser, WikiStrConsts;

type
  TWiki2FormatConverter = class;

  { TW2FormatPage }

  TW2FormatPage = class
  public
    Converter: TWiki2FormatConverter;
    CategoryList: TStringList;
    WikiFilename: string;
    WikiErrorMsg: string;
    WikiDoc: TXMLDocument;
    WikiPage: TWikiPage;
    WikiDocumentName: string; // the path in the Wiki
    WikiLanguage: string;
    constructor Create(TheConverter: TWiki2FormatConverter); virtual;
    destructor Destroy; override;
    procedure ParseWikiDoc(KeepWikiDoc: boolean);
    procedure ClearConversion; virtual;
    procedure ClearPageConnections; virtual;
  end;
  TW2FormatPageClass = class of TW2FormatPage;

  { TWiki2FormatConverter }

  TWiki2FormatConverter = class
  private
    FCodeTags: TKeyWordFunctionList;
    FNoWarnBaseURLs: TStringToStringTree;
    FOnLog: TWikiOnLog;
    FQuiet: boolean;
    FTitle: string;
    FVerbose: boolean;
    FWarnMissingPageLinks: boolean;
    procedure SetTitle(AValue: string);
  protected
    FImagesDir: string;
    FOutputDir: string;
    fPages: TFPList; // list of TW2FormatPage
    fPagesSortFilename: TAvlTree; // TW2FormatPage sorted for WikiFilename
    fPagesSortDocumentName: TAvlTree; // TW2FormatPage sorted for WikiDocumentName
    FPageClass: TW2FormatPageClass;
    function GetPages(Index: integer): TW2FormatPage;
    procedure SetOutputDir(AValue: string);
    procedure SetImagesDir(AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function GetPageWithFilename(Filename: string): TW2FormatPage; virtual;
    function GetPageWithDocumentName(DocumentName: string): TW2FormatPage; virtual;
    function AddWikiPage(Filename: string; ParseNow: boolean = true): TW2FormatPage;
    procedure Convert; virtual;
    procedure Log(Msg: string);
    property OnLog: TWikiOnLog read FOnLog write FOnLog;
    function Count: integer;
    property Pages[Index: integer]: TW2FormatPage read GetPages; default;
    property PageClass: TW2FormatPageClass read FPageClass;
    property OutputDir: string read FOutputDir write SetOutputDir;
    property ImagesDir: string read FImagesDir write SetImagesDir;
    property Title: string read FTitle write SetTitle;
    property WarnMissingPageLinks: boolean read FWarnMissingPageLinks write FWarnMissingPageLinks; // warn if an internal link links to non existing page
    property NoWarnBaseURLs: TStringToStringTree read FNoWarnBaseURLs;
    property CodeTags: TKeyWordFunctionList read FCodeTags write FCodeTags;
    function CollectAllLangCodes(Delimiter: char = ','): string;
    procedure GetPageTranslations(DocumentName: string; out LangToPage: TStringToPointerTree);
    property Verbose: boolean read FVerbose write FVerbose;
    property Quiet: boolean read FQuiet write FQuiet;
  end;

function WikiPageToFilename(DocumentName: string; IsInternalLink, AppendCaseID: boolean): string;
function WikiFilenameToPage(Filename: string): string;
function WikiImageToFilename(Image: string; IsInternalLink, InsertCaseID: boolean;
  KeepScheme: boolean = false): string;
function WikiHeaderToLink(Header: string): string;
function WikiCreateCommonCodeTagList(AddLazWikiLangs: boolean): TKeyWordFunctionList;

function UTF8ToWikiFileCode(AnUTF8: string): string;
function WikiFileCodeToUTF8(aFileCode: string): string;
procedure TestWikiPageToFilename;

// language
function GetWikiPageLanguage(const DocumentName: string): string;
function GetWikiPageLanguageDelimiterPos(const DocumentName: string): integer;
function ChompWikiPageLanguage(const DocumentName: string): string;
function WikiPageHasLanguage(const DocumentName, Languages: string): boolean;
function WikiLangInLanguageFilter(const Lang, Languages: string): boolean;
function WikiLangCodeToCaption(ID: string): string;
function WikiLangCaptionToCode(Caption: string): string;

function ComparePagesWithFilenames(Page1, Page2: Pointer): integer;
function CompareFilenameWithPage(Filename, Page: Pointer): integer;
function ComparePagesWithDocumentNames(Page1, Page2: Pointer): integer;
function CompareDocumentNameWithPage(DocumentName, Page: Pointer): integer;
function CompareDocumentNameWithPageIgnoreLang(DocumentName, Page: Pointer): integer;

implementation

{ TWiki2FormatConverter }

procedure TWiki2FormatConverter.SetImagesDir(AValue: string);
var
  NewValue: String;
begin
  NewValue:=AppendPathDelim(TrimFilename(AValue));
  if FImagesDir=NewValue then Exit;
  FImagesDir:=NewValue;
end;

procedure TWiki2FormatConverter.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
end;

function TWiki2FormatConverter.GetPages(Index: integer): TW2FormatPage;
begin
  Result:=TW2FormatPage(fPages[Index]);
end;

procedure TWiki2FormatConverter.SetOutputDir(AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimAndExpandDirectory(AValue);
  if FOutputDir=NewValue then Exit;
  FOutputDir:=NewValue;
end;

constructor TWiki2FormatConverter.Create;
begin
  FPageClass:=TW2FormatPage;
  fPages:=TFPList.Create;
  fPagesSortFilename:=TAvlTree.Create(@ComparePagesWithFilenames);
  fPagesSortDocumentName:=TAvlTree.Create(@ComparePagesWithDocumentNames);
  FTitle:='FPC/Lazarus Wiki (offline, generated '+DatetoStr(Now)+')';
  FImagesDir:='images';
  FNoWarnBaseURLs:=TStringToStringTree.Create(true);
end;

destructor TWiki2FormatConverter.Destroy;
begin
  Clear;
  FreeAndNil(FNoWarnBaseURLs);
  FreeAndNil(fPagesSortFilename);
  FreeAndNil(fPagesSortDocumentName);
  FreeAndNil(fPages);
  inherited Destroy;
end;

procedure TWiki2FormatConverter.Clear;
var
  i: Integer;
begin
  fPagesSortFilename.Clear;
  fPagesSortDocumentName.Clear;
  for i:=0 to Count-1 do
    Pages[i].Free;
  fPages.Clear;
  FNoWarnBaseURLs.Clear;
end;

function TWiki2FormatConverter.GetPageWithFilename(Filename: string): TW2FormatPage;
var
  Node: TAvlTreeNode;
begin
  if Filename='' then exit(nil);
  Node:=fPagesSortFilename.FindKey(Pointer(Filename),@CompareFilenameWithPage);
  if Node<>nil then
    Result:=TW2FormatPage(Node.Data)
  else
    Result:=nil;
end;

function TWiki2FormatConverter.GetPageWithDocumentName(DocumentName: string
  ): TW2FormatPage;
var
  Node: TAvlTreeNode;
begin
  if DocumentName='' then exit(nil);
  Node:=fPagesSortDocumentName.FindKey(Pointer(DocumentName),@CompareDocumentNameWithPage);
  if Node<>nil then
    Result:=TW2FormatPage(Node.Data)
  else
    Result:=nil;
end;

function TWiki2FormatConverter.AddWikiPage(Filename: string; ParseNow: boolean
  ): TW2FormatPage;
begin
  Result:=GetPageWithFilename(Filename);
  if Result=nil then begin
    Result:=PageClass.Create(Self);
    Result.WikiFilename:=Filename;
    Result.WikiDocumentName:=WikiFilenameToPage(Filename);
    Result.WikiLanguage:=GetWikiPageLanguage(Result.WikiDocumentName);
    fPages.Add(Result);
    fPagesSortFilename.Add(Result);
    fPagesSortDocumentName.Add(Result);
  end;
  if ParseNow then
    Result.ParseWikiDoc(false);
end;

procedure TWiki2FormatConverter.Convert;
var
  i: Integer;
  Page: TW2FormatPage;
begin
  if (OutputDir<>'') and (not DirPathExists(OutputDir)) then
    raise Exception.Create('fpdoc output directory not found: "'+OutputDir+'"');

  // load wiki pages
  for i:=0 to Count-1 do begin
    Page:=Pages[i];
    Page.WikiPage.LanguageTags:=CodeTags;
    Page.ParseWikiDoc(false);
  end;
end;

procedure TWiki2FormatConverter.Log(Msg: string);
begin
  if Assigned(OnLog) then
    OnLog(Msg)
  else
    debugln(Msg);
end;

function TWiki2FormatConverter.Count: integer;
begin
  Result:=fPages.Count;
end;

function TWiki2FormatConverter.CollectAllLangCodes(Delimiter: char): string;
var
  i: Integer;
  Page: TW2FormatPage;
  Lang: String;
  Langs: TStringToStringTree;
begin
  Result:='';
  Langs:=TStringToStringTree.Create(false);
  try
    for i:=0 to Count-1 do begin
      Page:=Pages[i];
      Lang:=Page.WikiLanguage;
      if Lang='' then continue;
      if Langs.Contains(Lang) then continue;
      Langs[Lang]:='1';
      Result+=Delimiter+Lang;
    end;
  finally
    Langs.Free;
  end;
end;

procedure TWiki2FormatConverter.GetPageTranslations(DocumentName: string; out
  LangToPage: TStringToPointerTree);
var
  AVLNode: TAvlTreeNode;
  Page: TW2FormatPage;
begin
  LangToPage:=TStringToPointerTree.Create(false);
  DocumentName:=ChompWikiPageLanguage(DocumentName);
  AVLNode:=fPagesSortDocumentName.FindLeftMostKey(Pointer(DocumentName),@CompareDocumentNameWithPageIgnoreLang);
  while (AVLNode<>nil)
  and (CompareDocumentNameWithPageIgnoreLang(Pointer(DocumentName),AVLNode.Data)=0) do begin
    Page:=TW2FormatPage(AVLNode.Data);
    LangToPage[GetWikiPageLanguage(Page.WikiDocumentName)]:=Page;
    AVLNode:=fPagesSortDocumentName.FindSuccessor(AVLNode);
  end;
end;

{ TW2FormatPage }

constructor TW2FormatPage.Create(TheConverter: TWiki2FormatConverter);
begin
  Converter:=TheConverter;
  CategoryList := TStringList.Create;
end;

destructor TW2FormatPage.Destroy;
begin
  ClearPageConnections;
  ClearConversion;
  FreeAndNil(CategoryList);
  FreeAndNil(WikiDoc);
  FreeAndNil(WikiPage);
  inherited Destroy;
end;

procedure TW2FormatPage.ParseWikiDoc(KeepWikiDoc: boolean);
begin
  try
    if WikiDoc=nil then begin
      try
        ReadXMLFile(WikiDoc,WikiFilename,[]);
      except
        on E: Exception do
          WikiErrorMsg:=E.Message;
      end;
    end;
    if WikiErrorMsg<>'' then
      raise Exception.Create(WikiFilename+': '+WikiErrorMsg);
    if WikiPage=nil then begin
      WikiPage:=TWikiPage.Create;
      try
        if Converter<>nil then
          WikiPage.OnLog:=Converter.OnLog;
        WikiPage.LoadFromDoc(WikiDoc);
      except
        on E: Exception do
          WikiErrorMsg:=E.Message;
      end;
    end;
    if WikiErrorMsg<>'' then
      raise Exception.Create(WikiFilename+': '+WikiErrorMsg);
  finally
    if not KeepWikiDoc then
      FreeAndNil(WikiDoc);
  end;
end;

procedure TW2FormatPage.ClearConversion;
begin

end;

procedure TW2FormatPage.ClearPageConnections;
begin
end;

function WikiPageToFilename(DocumentName: string; IsInternalLink, AppendCaseID: boolean): string;
{ IsInternalLink:
  AppendCaseID=true: append a string encoding upper/lower case of letters
}
begin
  Result:=DocumentName;
  // optional: convert title to wiki link
  if IsInternalLink then
    Result:=WikiTitleToPage(Result);
  // convert special characaters
  Result:=UTF8ToWikiFileCode(Result);
  // append case if
  if AppendCaseID and (Result<>'') then
    Result:=Result+'.'+WikiPageToCaseID(Result);
end;

function WikiFilenameToPage(Filename: string): string;
var
  Ext: String;
  p: Integer;
begin
  Result:=ExtractFileName(Filename);
  if Result='' then exit;
  // delete .xml
  Ext:=lowercase(ExtractFileExt(Result));
  if Ext='.xml' then
    Result:=LeftStr(Result,length(Result)-length(Ext));
  // delete case id
  p:=length(Result);
  while (p>=1) and (Result[p] in ['0'..'9','a'..'z']) do
    dec(p);
  if (p>=1) and (Result[p]='.') then
    Delete(Result,p,length(Result));
  // convert special characaters
  Result:=WikiFileCodeToUTF8(Result);
end;

function WikiImageToFilename(Image: string;
  IsInternalLink, InsertCaseID: boolean;
  KeepScheme: boolean): string;
var
  id: String;
  Ext: String;
  p: SizeInt;
begin
  Result:=Image;
  if not KeepScheme then begin
    // delete 'Image:'
    p:=Pos(':',Result);
    if p>0 then
      Delete(Result,1,p);
  end;
  if IsInternalLink then
    Result:=WikiTitleToPage(Result);
  Ext:=ExtractFileExt(Result);
  // encode file name without extension
  Result:=WikiPageToFilename(copy(Result,1,Length(Result)-length(Ext)),false,false);
  // encode extension
  Ext:=LowerCase(WikiPageToFilename(copy(Ext,2,length(Ext)),false,false));
  id:='';
  if InsertCaseID then
    id:='.'+WikiPageToCaseID(Result+Ext); // Note: compute case id with extension
  Result:=Result+id+'.'+Ext;
end;

function WikiHeaderToLink(Header: string): string;
var
  i: Integer;
  s: string;
begin
  Result:=UTF8Trim(Header);
  i:=1;
  while i<=length(Result) do begin
    s:=Result[i];
    case s[1] of
    '-','_',':','.','0'..'9','a'..'z','A'..'Z',#128..#255: ; // keep
    ' ': s:='_';
    '+': s:=''; // delete
    else s:='.'+HexStr(ord(s[1]),2); // non-literal
    end;
    if s<>Result[i] then
      ReplaceSubstring(Result,i,1,s);
    inc(i,length(s));
  end;
end;

function WikiCreateCommonCodeTagList(AddLazWikiLangs: boolean): TKeyWordFunctionList;
begin
  Result:=TKeyWordFunctionList.Create('LanguageTags');
  with Result do begin
    Add('code',@AllwaysTrue);
    Add('source',@AllwaysTrue);
    Add('syntaxhighlight',@AllwaysTrue);
    Add('pascal',@AllwaysTrue);
    Add('delphi',@AllwaysTrue);
    if AddLazWikiLangs then begin
      Add('bash',@AllwaysTrue);
      Add('java',@AllwaysTrue);
      Add('javascript',@AllwaysTrue);
      Add('xml',@AllwaysTrue);
      Add('perl',@AllwaysTrue);
      Add('python',@AllwaysTrue);
      Add('sql',@AllwaysTrue);
      Add('objc',@AllwaysTrue);
    end;
  end;
  Result.Sort; // init now, so that multiple threads can use it
end;

const
  WFCAllowedChars = ['a'..'z','A'..'Z','0'..'9',',','!','#','%','(',')','-','_',' '];
function UTF8ToWikiFileCode(AnUTF8: string): string;
{ Keep a..z, A..Z, 0..9, ,!#-%()-_
  Replace + with +-
  Replace = with =-
  Replace single invalid byte with =HexHex
  Replace sequences of invalid bytes as +base64-
}
const
  Base64Chars: array[0..63] of char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789,_';
var
  p: PChar;
  CharLen: integer;
  BufBits: byte;
  BufBitLen: integer;
  c: Char;
  SrcBits: Integer;
  i: Integer;
  AtEnd: Boolean;
begin
  {$ifdef VerboseWikiFileCode}
  writeln('UTF8ToWikiFileCode START AnUTF8="',AnUTF8,'"');
  {$endif}
  Result:='';
  if AnUTF8='' then exit;
  p:=PChar(AnUTF8);
  repeat
    c:=p^;
    if (c=#0) and (p-PChar(AnUTF8)=length(AnUTF8)) then break;
    AtEnd:=(p[1]=#0) and (p+1-PChar(AnUTF8)=length(AnUTF8));
    if c in WFCAllowedChars then begin
      // common English character: keep
      {$ifdef VerboseWikiFileCode}
      writeln('UTF8ToWikiFileCode normal char "',c,'"');
      {$endif}
      Result+=c;
      inc(p);
    end else if (c in ['+','=']) and ((p[1] in WFCAllowedChars) or AtEnd) then begin
      // replace '+' with '+-' and '=' with '=-'
      {$ifdef VerboseWikiFileCode}
      writeln('UTF8ToWikiFileCode single + or = "',c,'"');
      {$endif}
      Result+=c+'-';
      inc(p);
    end else if (p[1] in WFCAllowedChars) or AtEnd then begin
      // replace single special byte with =HexHex
      {$ifdef VerboseWikiFileCode}
      writeln('UTF8ToWikiFileCode single special char "',HexStr(ord(c),2),'"');
      {$endif}
      Result+='='+HexStr(ord(c),2);
      inc(p);
    end else begin
      // special characters -> encode base64
      // start marker: '+'
      Result+='+';
      // encode as base64
      BufBits:=0;
      BufBitLen:=0;
      repeat
        if (p^=#0) and (p-PChar(AnUTF8)=length(AnUTF8)) then
          break; // end of string
        if (p^ in WFCAllowedChars) and (p[1] in WFCAllowedChars)
        and (p[2] in WFCAllowedChars) then
          break; // the next three are normal characters -> stop encoding as base64
        CharLen:=UTF8CodepointSize(p);
        {$ifdef VerboseWikiFileCode}
        writeln('UTF8ToWikiFileCode sequence UTF8CharLen=',CharLen);
        {$endif}
        for i:=1 to CharLen do begin
          SrcBits:=ord(p^);
          BufBits:=BufBits or (SrcBits shr (2+BufBitLen));
          {$ifdef VerboseWikiFileCode}
          writeln('UTF8ToWikiFileCode sequence 6bitA Byte=',i,' SrcBits=',binstr(SrcBits,8),' BufBits=',binstr(BufBits,6),' BufBitLen=',6);
          {$endif}
          Result+=Base64Chars[BufBits];
          BufBits:=(SrcBits shl (4-BufBitLen)) and %111111;
          BufBitLen:=2+BufBitLen;
          if BufBitLen=6 then begin
            {$ifdef VerboseWikiFileCode}
            writeln('UTF8ToWikiFileCode sequence 6bitB Byte=',i,' SrcBits=',binstr(SrcBits,8),' BufBits=',binstr(BufBits,6),' BufBitLen=',BufBitLen);
            {$endif}
            Result+=Base64Chars[BufBits];
            BufBitLen:=0;
            BufBits:=0;
          end else begin
            {$ifdef VerboseWikiFileCode}
            writeln('UTF8ToWikiFileCode sequence <6bit Byte=',i,' SrcBits=',binstr(SrcBits,8),' BufBits=',binstr(BufBits,6),' BufBitLen=',BufBitLen);
            {$endif}
          end;
          inc(p);
        end;
      until false;
      if BufBitLen>0 then begin
        {$ifdef VerboseWikiFileCode}
        writeln('UTF8ToWikiFileCode sequence Paddi Byte=',i,' SrcBits=',binstr(SrcBits,8),' BufBits=',binstr(BufBits,6),' BufBitLen=',BufBitLen);
        {$endif}
        Result+=Base64Chars[BufBits];
      end;
      // end marker: '-'
      Result+='-';
    end;
  until false;
end;

function WikiFileCodeToUTF8(aFileCode: string): string;
var
  p: PChar;
  SrcBits: Integer;
  BufBits: byte;
  BufBitLen: Integer;
  c: Char;
begin
  {$ifdef VerboseWikiFileCode}
  writeln('WikiFileCodeToUTF8 Code="',aFileCode,'"');
  {$endif}
  Result:='';
  if aFileCode='' then exit;
  p:=PChar(aFileCode);
  repeat
    c:=p^;
    if (c=#0) and (p-PChar(aFileCode)=length(aFileCode)) then break;
    if c='+' then begin
      inc(p);
      if p^='-' then begin
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 +- to +');
        {$endif}
        inc(p);
        Result+='+'; // single '+'
      end else begin
        // decode base64, read til '-'
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 base64 sequence');
        {$endif}
        BufBits:=0;
        BufBitLen:=0;
        repeat
          c:=p^;
          case c of
          '-':
            begin
              inc(p);
              break;
            end;
          'A'..'Z': SrcBits:=ord(c)-ord('A');
          'a'..'z': SrcBits:=ord(c)-ord('a')+26;
          '0'..'9': SrcBits:=ord(c)-ord('0')+52;
          ',': SrcBits:=62;
          '_': SrcBits:=63;
          else
            raise Exception.Create('invalid wiki file code: invalid base64 character');
          end;
          {$ifdef VerboseWikiFileCode}
          writeln('WikiFileCodeToUTF8 SrcBits=',binstr(SrcBits,6));
          {$endif}
          if BufBitLen=0 then begin
            BufBits:=BufBits or (SrcBits shl 2);
            BufBitLen:=6;
            {$ifdef VerboseWikiFileCode}
            writeln('WikiFileCodeToUTF8 new byte      BufBits=',binstr(BufBits,8),' BufBitLen=',BufBitLen);
            {$endif}
          end else begin
            BufBits:=BufBits or (SrcBits shr (BufBitLen-2));
            {$ifdef VerboseWikiFileCode}
            writeln('WikiFileCodeToUTF8 byte complete BufBits=',binstr(BufBits,8),' BufBitLen=',8);
            {$endif}
            Result+=chr(BufBits);
            BufBitLen-=2;
            BufBits:=(SrcBits shl (8-BufBitLen)) and $FF;
            {$ifdef VerboseWikiFileCode}
            writeln('WikiFileCodeToUTF8 rest byte     BufBits=',binstr(BufBits,8),' BufBitLen=',BufBitLen);
            {$endif}
          end;
          inc(p);
        until false;
        // Note: BufBitLen can be >0 (the last byte contains padding bits)
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 D BufBits=',binstr(BufBits,8),' BufBitLen=',BufBitLen);
        {$endif}
        if (BufBits shr (8-BufBitLen))>0 then
          raise Exception.Create('invalid wiki file code: padding bits not empty');
      end;
    end else if c='=' then begin
      inc(p);
      if p^='-' then begin
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 =- to =');
        {$endif}
        Result+='=';
        inc(p);
      end else begin
        // one byte as hex code
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 =hex ',p^,p[1]);
        {$endif}
        SrcBits:=0;
        case p^ of
        '0'..'9': SrcBits:=ord(p^)-ord('0');
        'A'..'F': SrcBits:=ord(p^)-ord('A')+10;
        'a'..'f': SrcBits:=ord(p^)-ord('a')+10;
        else
          raise Exception.Create('invalid wiki file code: invalid hex code');
        end;
        inc(p);
        SrcBits:=SrcBits*16;
        case p^ of
        '0'..'9': SrcBits+=ord(p^)-ord('0');
        'A'..'F': SrcBits+=ord(p^)-ord('A')+10;
        'a'..'f': SrcBits+=ord(p^)-ord('a')+10;
        else
          raise Exception.Create('invalid wiki file code: invalid hex code');
        end;
        inc(p);
        {$ifdef VerboseWikiFileCode}
        writeln('WikiFileCodeToUTF8 =hex byte=',SrcBits);
        {$endif}
        Result+=chr(SrcBits);
      end;
    end else if c in WFCAllowedChars then begin
      // normal char
      {$ifdef VerboseWikiFileCode}
      writeln('WikiFileCodeToUTF8 normal char "',c,'"');
      {$endif}
      Result+=c;
      inc(p);
    end else
      raise Exception.Create('invalid wiki file code: invalid character');
  until false;
  if FindInvalidUTF8Codepoint(PChar(Result),length(Result))>=0 then
    raise Exception.Create('invalid wiki file code: result is not UTF-8');
end;

procedure TestWikiPageToFilename;

  procedure t(PageName: string);
  var
    Filename: String;
    NewPageName: String;
    step: integer;
  begin
    step:=0;
    try
      Filename:=WikiPageToFilename(PageName,false,true);
      NewPageName:=WikiFilenameToPage(Filename);
      inc(step);
      if PageName=NewPageName then
        inc(step);
    finally
      if step<2 then begin
        writeln('TestPageToFilename failed:');
        writeln('  PageName   ="',PageName,'"');
        writeln('  NewPageName="',NewPageName,'"');
        writeln('  Filename   ="',Filename,'"');
      end;
      if step=0 then
        raise Exception.Create('TestPageToFilename failed');
    end;
  end;

begin
  t('');
  t('a');
  t('A');
  t('A1');
  t('3');
  t('/');
  t('A/B');
  t('A+B');
  t('A-B');
  t('A=B');
  t('A+=B');
  t('A+-B');
  t('A+');
  t('A=');
  t('A*');
  t('A*$');
  t('A*$%');
  t('A*$*$');
  t('A*$*$*');
  t('A*$*$*$');
end;

function GetWikiPageLanguage(const DocumentName: string): string;
var
  p: Integer;
begin
  p:=GetWikiPageLanguageDelimiterPos(DocumentName);
  if p>0 then
    Result:=copy(DocumentName,p+1,length(DocumentName))
  else
    Result:='';
end;

function GetWikiPageLanguageDelimiterPos(const DocumentName: string): integer;
var
  l: Integer;
  p: PChar;
begin
  Result:=0;
  l:=length(DocumentName);
  if l=0 then exit;
  if (l>3) then begin
    p:=PChar(DocumentName)+l-3;
    if (p^='/')
    and (p[1] in ['a'..'z']) and (p[2] in ['a'..'z']) then
      // short form: /de
      exit(l-2);
  end;
  if (l>6) then begin
    p:=PChar(DocumentName)+l-6;
    if (p^='/')
    and (p[1] in ['a'..'z']) and (p[2] in ['a'..'z'])
    and (p[3]='_')
    and (p[4] in ['A'..'Z']) and (p[5] in ['A'..'Z'])
    then
      // long form: /zh_TW
      exit(l-5);
  end;
end;

function ChompWikiPageLanguage(const DocumentName: string): string;
var
  p: Integer;
begin
  p:=GetWikiPageLanguageDelimiterPos(DocumentName);
  if p>0 then
    Result:=LeftStr(DocumentName,p-1)
  else
    Result:=DocumentName;
end;

function WikiPageHasLanguage(const DocumentName, Languages: string): boolean;
begin
  Result:=WikiLangInLanguageFilter(GetWikiPageLanguage(DocumentName),Languages);
end;

function WikiLangInLanguageFilter(const Lang, Languages: string): boolean;
// * = fits any
// de = fits 'de' and original
// -,de = fits only 'de'
var
  p: PChar;
  StartPos: PChar;
begin
  if (Languages='') then
    exit(Lang='');
  p:=PChar(Languages);
  while p^<>#0 do begin
    StartPos:=p;
    while not (p^ in [#0,',']) do inc(p);
    if p>StartPos then begin
      if StartPos^='-' then begin
        // not original language
        if Lang='' then exit(false);
      end else if StartPos^='*' then begin
        // fit any
        exit(true);
      end else if (Lang<>'') and (CompareIdentifiers(StartPos,PChar(Lang))=0)
      then begin
        // fits specific
        exit(true);
      end;
    end;
    while p^=',' do inc(p);
  end;
  Result:=false;
end;

function WikiLangCodeToCaption(ID: string): string;
begin
  if ID='' then Result:=wrsLanguageEnglishOriginal
  else if ID='*' then Result:=wrsAll
  else if CompareText(ID,'af')=0 then Result:=wrsLanguageAfrikaans
  else if CompareText(ID,'ar')=0 then Result:=wrsLanguageArabic
  else if CompareText(ID,'ca')=0 then Result:=wrsLanguageCatalan
  else if CompareText(ID,'cs')=0 then Result:=wrsLanguageCzech
  else if CompareText(ID,'de')=0 then Result:=wrsLanguageGerman
  else if CompareText(ID,'en')=0 then Result:=wrsLanguageEnglish
  else if CompareText(ID,'es')=0 then Result:=wrsLanguageSpanish
  else if CompareText(ID,'fa')=0 then Result:=wrsLanguagePersian
  else if CompareText(ID,'fi')=0 then Result:=wrsLanguageFinnish
  else if CompareText(ID,'fr')=0 then Result:=wrsLanguageFrench
  else if CompareText(ID,'he')=0 then Result:=wrsLanguageHebrew
  else if CompareText(ID,'hu')=0 then Result:=wrsLanguageHungarian
  else if CompareText(ID,'id')=0 then Result:=wrsLanguageIndonesian
  else if CompareText(ID,'it')=0 then Result:=wrsLanguageItalian
  else if CompareText(ID,'ja')=0 then Result:=wrsLanguageJapanese
  else if CompareText(ID,'ko')=0 then Result:=wrsLanguageKorean
  else if CompareText(ID,'lt')=0 then Result:=wrsLanguageLithuanian
  else if CompareText(ID,'mk')=0 then Result:=wrsLanguageMacedonien
  else if CompareText(ID,'nl')=0 then Result:=wrsLanguageDutch
  else if CompareText(ID,'pl')=0 then Result:=wrsLanguagePolish
  else if CompareText(ID,'pt')=0 then Result:=wrsLanguagePortuguese
  else if CompareText(ID,'pt_BR')=0 then Result:=wrsLanguagePortugueseBr
  else if CompareText(ID,'ro')=0 then Result:=wrsLanguageRomanian
  else if CompareText(ID,'ru')=0 then Result:=wrsLanguageRussian
  else if CompareText(ID,'sk')=0 then Result:=wrsLanguageSlovak
  else if CompareText(ID,'sq')=0 then Result:=wrsLanguageAlbania
  else if CompareText(ID,'tr')=0 then Result:=wrsLanguageTurkish
  else if CompareText(ID,'uk')=0 then Result:=wrsLanguageUkrainian
  else if CompareText(ID,'zh_CN')=0 then Result:=wrsLanguageChinese
  else if CompareText(ID,'zh_TW')=0 then Result:=wrsLanguageChineseTaiwan
  else Result:=ID;
end;

function WikiLangCaptionToCode(Caption: string): string;
begin
  if Caption=wrsLanguageEnglishOriginal then Result:=''
  else if Caption=wrsAll then Result:='*'
  else if Caption=wrsLanguageAfrikaans then Result:='af'
  else if Caption=wrsLanguageAlbania then Result:='sq'
  else if Caption=wrsLanguageArabic then Result:='ar'
  else if Caption=wrsLanguageCatalan then Result:='ca'
  else if Caption=wrsLanguageChinese then Result:='zh_CN'
  else if Caption=wrsLanguageChineseTaiwan then Result:='zh_TW'
  else if Caption=wrsLanguageCzech then Result:='cs'
  else if Caption=wrsLanguageDutch then Result:='nl'
  else if Caption=wrsLanguageEnglish then Result:='en'
  else if Caption=wrsLanguageFinnish then Result:='fi'
  else if Caption=wrsLanguageFrench then Result:='fr'
  else if Caption=wrsLanguageGerman then Result:='de'
  else if Caption=wrsLanguageHebrew then Result:='he'
  else if Caption=wrsLanguageHungarian then Result:='hu'
  else if Caption=wrsLanguageIndonesian then Result:='id'
  else if Caption=wrsLanguageItalian then Result:='it'
  else if Caption=wrsLanguageJapanese then Result:='ja'
  else if Caption=wrsLanguageKorean then Result:='ko'
  else if Caption=wrsLanguageLithuanian then Result:='lt'
  else if Caption=wrsLanguageMacedonien then Result:='mk'
  else if Caption=wrsLanguagePersian then Result:='fa'
  else if Caption=wrsLanguagePolish then Result:='pl'
  else if Caption=wrsLanguagePortuguese then Result:='pt'
  else if Caption=wrsLanguagePortugueseBr then Result:='pt_BR'
  else if Caption=wrsLanguageRomanian then Result:='ro'
  else if Caption=wrsLanguageRussian then Result:='ru'
  else if Caption=wrsLanguageSlovak then Result:='sk'
  else if Caption=wrsLanguageSpanish then Result:='es'
  else if Caption=wrsLanguageTurkish then Result:='tk'
  else if Caption=wrsLanguageUkrainian then Result:='uk'
  else Result:=Caption;
end;

function ComparePagesWithFilenames(Page1, Page2: Pointer): integer;
var
  p1: TW2FormatPage absolute Page1;
  p2: TW2FormatPage absolute Page2;
begin
  Result:=CompareFilenames(p1.WikiFilename,p2.WikiFilename);
end;

function CompareFilenameWithPage(Filename, Page: Pointer): integer;
var
  p: TW2FormatPage absolute Page;
begin
  Result:=CompareFilenames(AnsiString(Filename),p.WikiFilename);
end;

function ComparePagesWithDocumentNames(Page1, Page2: Pointer): integer;
var
  p1: TW2FormatPage absolute Page1;
  p2: TW2FormatPage absolute Page2;
begin
  Result:=BasicCodeTools.CompareTextCT(p1.WikiDocumentName,p2.WikiDocumentName,true);
end;

function CompareDocumentNameWithPage(DocumentName, Page: Pointer): integer;
var
  p: TW2FormatPage absolute Page;
begin
  Result:=BasicCodeTools.CompareTextCT(AnsiString(DocumentName),p.WikiDocumentName,true);
end;

function CompareDocumentNameWithPageIgnoreLang(DocumentName, Page: Pointer
  ): integer;
var
  p: TW2FormatPage absolute Page;
  DocName: String;
  l1: Integer;
  l2: Integer;
begin
  DocName:=AnsiString(DocumentName);
  l1:=GetWikiPageLanguageDelimiterPos(DocName)-1;
  if l1<0 then l1:=length(DocName);
  l2:=GetWikiPageLanguageDelimiterPos(p.WikiDocumentName)-1;
  if l2<0 then l2:=length(p.WikiDocumentName);
  Result:=BasicCodeTools.CompareText(PChar(DocName),l1,
                                     PChar(p.WikiDocumentName),l2,true);
end;

end.

