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
  LazLogger, LazUTF8, KeywordFuncLists, CodeToolsStructs;

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
    procedure ParseWikiDoc;
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
    FPageClass: TW2FormatPageClass;
    function GetPages(Index: integer): TW2FormatPage;
    procedure SetOutputDir(AValue: string);
    procedure SetImagesDir(AValue: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function IndexOfWikiFilename(Filename: string): integer; virtual;
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
function WikiCreateCommonLanguageList(AddLazWikiLangs: boolean): TKeyWordFunctionList;

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
  FTitle:='FPC/Lazarus Wiki (offline, generated '+DatetoStr(Now)+')';
  FImagesDir:='images';
  FNoWarnBaseURLs:=TStringToStringTree.Create(true);
end;

destructor TWiki2FormatConverter.Destroy;
begin
  Clear;
  FreeAndNil(FNoWarnBaseURLs);
  FreeAndNil(fPages);
  inherited Destroy;
end;

procedure TWiki2FormatConverter.Clear;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Pages[i].Free;
  fPages.Clear;
  FNoWarnBaseURLs.Clear;
end;

function TWiki2FormatConverter.IndexOfWikiFilename(Filename: string): integer;
begin
  Result:=Count-1;
  while (Result>=0)
  and (CompareFilenames(Filename,Pages[Result].WikiFilename)<>0) do
    dec(Result);
end;

function TWiki2FormatConverter.AddWikiPage(Filename: string; ParseNow: boolean
  ): TW2FormatPage;
var
  i: Integer;
begin
  i:=IndexOfWikiFilename(Filename);
  if i>=0 then
    Result:=Pages[i]
  else begin
    Result:=PageClass.Create(Self);
    Result.WikiFilename:=Filename;
    Result.WikiDocumentName:=WikiFilenameToPage(Filename);
    fPages.Add(Result);
  end;
  if ParseNow then
    Result.ParseWikiDoc;
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
    Page.ParseWikiDoc;
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
  FreeAndNil(WikiDoc);
  FreeAndNil(WikiPage);
  inherited Destroy;
end;

procedure TW2FormatPage.ParseWikiDoc;
begin
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

end.

