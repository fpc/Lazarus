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
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}
unit WikiFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WikiParser, laz2_XMLRead, LazFileUtils, laz2_DOM,
  LazLogger, LazUTF8, AvgLvlTree, KeywordFuncLists, CodeToolsStructs;

type
  TWiki2FormatConverter = class;

  { TW2FormatPage }

  TW2FormatPage = class
  public
    Converter: TWiki2FormatConverter;
    WikiFilename: string;
    WikiErrorMsg: string;
    WikiDoc: TXMLDocument;
    WikiPage: TWikiPage;
    WikiDocumentName: string; // the path in the Wiki
    constructor Create(TheConverter: TWiki2FormatConverter); virtual;
    destructor Destroy; override;
    procedure ParseWikiDoc(KeepWikiDoc: boolean);
    procedure ClearConversion; virtual;
  end;
  TW2FormatPageClass = class of TW2FormatPage;

  { TWiki2FormatConverter }

  TWiki2FormatConverter = class
  private
    FLanguageTags: TKeyWordFunctionList;
    FNoWarnBaseURLs: TStringToStringTree;
    FOnLog: TWikiOnLog;
    FTitle: string;
    FWarnMissingPageLinks: boolean;
    procedure SetTitle(AValue: string);
  protected
    FImagesDir: string;
    FOutputDir: string;
    fPages: TFPList; // list of TW2FormatPage
    fPagesSortFilename: TAvgLvlTree; // TW2FormatPage sorted for WikiFilename
    fPagesSortDocumentName: TAvgLvlTree; // TW2FormatPage sorted for WikiDocumentName
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
    property LanguageTags: TKeyWordFunctionList read FLanguageTags write FLanguageTags;
  end;

function WikiPageToFilename(Page: string; IsInternalLink, AppendCaseID: boolean): string;
function WikiFilenameToPage(Filename: string): string;
function WikiImageToFilename(Image: string; IsInternalLink, InsertCaseID: boolean;
  KeepScheme: boolean = false): string;
function WikiHeaderToLink(Header: string): string;
function WikiCreateCommonLanguageList(AddLazWikiLangs: boolean): TKeyWordFunctionList;
function GetWikiPageLanguage(const Page: string): string;
function WikiPageHasLanguage(const Page, Languages: string): boolean;

function ComparePagesWithFilenames(Page1, Page2: Pointer): integer;
function CompareFilenameWithPage(Filename, Page: Pointer): integer;
function ComparePagesWithDocumentNames(Page1, Page2: Pointer): integer;
function CompareDocumentNameWithPage(DocumentName, Page: Pointer): integer;

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
  fPagesSortFilename:=TAvgLvlTree.Create(@ComparePagesWithFilenames);
  fPagesSortDocumentName:=TAvgLvlTree.Create(@ComparePagesWithDocumentNames);
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

function TWiki2FormatConverter.GetPageWithFilename(Filename: string
  ): TW2FormatPage;
var
  Node: TAvgLvlTreeNode;
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
  Node: TAvgLvlTreeNode;
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
    Page.WikiPage.LanguageTags:=LanguageTags;
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

{ TW2FormatPage }

constructor TW2FormatPage.Create(TheConverter: TWiki2FormatConverter);
begin
  Converter:=TheConverter;
end;

destructor TW2FormatPage.Destroy;
begin
  ClearConversion;
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

function WikiPageToFilename(Page: string; IsInternalLink, AppendCaseID: boolean): string;
var
  i: Integer;
  s: string;
begin
  Result:=Page;
  if IsInternalLink then
    Result:=WikiInternalLinkToPage(Result);
  i:=1;
  while i<=length(Result) do begin
    s:=Result[i];
    case s[1] of
    ',','-','_','0'..'9','a'..'z','A'..'Z': ;
    // Note: UTF-8 characters do not work with svn on OS X
    else s:='%'+HexStr(ord(s[1]),2);
    end;
    if s<>Result[i] then
      ReplaceSubstring(Result,i,1,s);
    inc(i,length(s));
  end;
  if AppendCaseID and (Result<>'') then
    Result:=Result+'.'+WikiPageToCaseID(Result);
end;

function WikiFilenameToPage(Filename: string): string;
var
  Ext: String;
  p: Integer;
  i: Integer;
  Code: Integer;
  j: Integer;
  c: Char;
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
  // convert non literals
  for i:=length(Result) downto 1 do begin
    if Result[i]<>'%' then continue;
    Code:=0;
    for j:=1 to 2 do begin
      if i+j>length(Result) then break;
      c:=Result[i+j];
      case c of
      '0'..'9': Code:=Code*16+ord(c)-ord('0');
      'a'..'z': Code:=Code*16+ord(c)-ord('a')+10;
      'A'..'Z': Code:=Code*16+ord(c)-ord('A')+10;
      else break;
      end;
    end;
    ReplaceSubstring(Result,i,1+j,chr(Code));
  end;
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
    Result:=WikiInternalLinkToPage(Result);
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

function WikiCreateCommonLanguageList(AddLazWikiLangs: boolean): TKeyWordFunctionList;
begin
  Result:=TKeyWordFunctionList.Create('LanguageTags');
  with Result do begin
    Add('code',@AllwaysTrue);
    Add('source',@AllwaysTrue);
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
end;

function GetWikiPageLanguage(const Page: string): string;
var
  l: Integer;
  p: PChar;
begin
  Result:='';
  l:=length(Page);
  if l=0 then exit;
  if (l>3) then begin
    p:=PChar(Page)+l-3;
    if (p^='/')
    and (p[1] in ['a'..'z']) and (p[2] in ['a'..'z']) then
      // short form: /de
      exit(RightStr(Page,2));
  end;
  if (l>6) then begin
    p:=PChar(Page)+l-6;
    if (p^='/')
    and (p[1] in ['a'..'z']) and (p[2] in ['a'..'z'])
    and (p[3]='_')
    and (p[4] in ['A'..'Z']) and (p[5] in ['A'..'Z'])
    then
      // long form: /zh_TW
      exit(RightStr(Page,5));
  end;
end;

function WikiPageHasLanguage(const Page, Languages: string): boolean;
// * = fits any
// de = fits 'de' and original
// -,de = fits only 'de'
var
  Lang: String;
  p: PChar;
  StartPos: PChar;
begin
  Lang:=GetWikiPageLanguage(Page);
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
  Result:=CompareStr(p1.WikiDocumentName,p2.WikiDocumentName);
end;

function CompareDocumentNameWithPage(DocumentName, Page: Pointer): integer;
var
  p: TW2FormatPage absolute Page;
begin
  Result:=CompareStr(AnsiString(DocumentName),p.WikiDocumentName);
end;

end.

