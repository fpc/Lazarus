{ Converter for wiki pages to a chm file

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
  Full text search
  images

}
unit Wiki2CHMConvert;

{$mode objfpc}{$H+}

{ $DEFINE VerboseCHMIndex}
{ $DEFINE EnableWikiCHMWriter}

interface

uses
  Classes, SysUtils,
  {$IFDEF EnableWikiCHMWriter}
  wikichmwriter, wikichmfilewriter, wikichmsitemap,
  {$ELSE}
  chmwriter, chmfilewriter, chmsitemap,
  {$ENDIF}
  // LazUtils
  LazLoggerBase, LazUTF8, LazFileUtils, AvgLvlTree,
  // LazWiki
  Wiki2HTMLConvert, Wiki2XHTMLConvert;

const
  CHMImagesDir = '/images/';
type

  { TW2CHMPage
    for future extensions and descendants }

  TW2CHMPage = class(TW2HTMLPage)
  public
  end;

  { TWiki2CHMConverter }

  TWiki2CHMConverter = class(TWiki2HTMLConverter)
  private
    FCHMFile: string;
    FTOCRootName: String;
    FIndexFileName, FTOCFileName: String;
    FTocSitemap, FIndexSitemap: TCHMSiteMap;
    FTocStream, FIndexStream: TMemoryStream;
    procedure SetCHMFile(AValue: string);
    procedure SetIndexFileName(AValue: string);
    procedure SetTOCFilename(AValue: string);
  protected
    Writer: TChmWriter;
    FilesCompressed: integer;
    DocumentNameToPage: TStringToPointerTree; // Page.WikiDocumentName+'.html' to Page
    procedure AddIndexItem(AText, AUrl: String); override;
    procedure AddTocItem(ALevel: Integer; AText, AUrl: String); override;
    procedure ConvertInit; override;
    function OnWriterGetFileData(const DataName: String; out PathInChm: String;
      out FileName: String; var Stream: TStream): Boolean;
    procedure OnWriterLastFileAdded(Sender: TObject);
    function GetImageLink(ImgFilename: string): string; override;
    function GetInternalImageLink(ImgFilename: String): String; override;
    function GetPageLink(Page: TW2XHTMLPage): string; override;
    procedure SaveAllPages; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    function GetRelativeCSSFileName: String; override;
    property CHMFile: string read FCHMFile write SetCHMFile;
    property IndexFileName: string read FIndexFileName write SetIndexFileName;
    property TOCFileName: String read FTOCFilename write SetTOCFilename;
    property TOCRootName: String read FTOCRootName write FTOCRootName;
  end;

implementation

{ TWiki2HTMLConverter }

function TWiki2CHMConverter.OnWriterGetFileData(const DataName: String; out
  PathInChm: String; out FileName: String; var Stream: TStream): Boolean;
var
  Page: TW2CHMPage;
  ImgFilename: String;
begin
  Stream.Size:=0;
  inc(FilesCompressed);
  debugln(['chm processing ',FilesCompressed,' of ',Writer.FilesToCompress.Count,' "',DataName,'" ...']);
  Result := False; // Return true to abort compressing files
  Filename:=ExtractFileName(DataName);
  PathInChm:='/'+ExtractFilePath(DataName);
  // cleanup string
  PathInChm:=StringReplace(PathInChm, '\','/',[rfReplaceAll]);
  PathInChm:=StringReplace(PathInChm, '//','/',[rfReplaceAll]);
  Page:=TW2CHMPage(DocumentNameToPage[DataName]);
  if Page<>nil then begin
    // a page
    SavePageToStream(Page,Stream);
  end else if copy(DataName,1,length(CHMImagesDir))=CHMImagesDir then begin
    // an image
    ImgFilename:=ImagesDir+copy(DataName,length(CHMImagesDir)+1,length(DataName));
    //debugln(['TWiki2CHMConverter.OnWriterGetFileData img="',DataName,'" File="',ImgFilename,'" FileSize=',FileSize(ImgFilename)]);
    TMemoryStream(Stream).LoadFromFile(ImgFilename);
  end else if DataName=GetRelativeCSSFilename then begin
    // the css file
    TMemoryStream(Stream).LoadFromFile(CSSFilename);
  end else
   raise Exception.Create('TWiki2CHMConverter.OnWriterGetFileData failed DataName="'+dbgstr(DataName)+'"');
end;

procedure TWiki2CHMConverter.AddIndexItem(AText, AUrl: String);
var
  item: TCHMSiteMapItem;
  i: Integer;
  txt, url, itemtxt, itemurl, itemlocal: String;
begin
  // Avoid empty index data
  if (AText = '') or (AUrl = '') then
    exit;

  AText := EscapeToHTML(AText);

  // Avoid duplicate index items.
  txt := UTF8Trim(UTF8Lowercase(AText));
  url := UTF8Trim(UTF8Lowercase(AUrl));
  for i:=0 to FIndexSiteMap.Items.Count-1 do begin
    item := FIndexSiteMap.Items.Item[i];
    itemtxt := UTF8Lowercase(item.Text);
    itemurl := UTF8Lowercase(item.URL);
    itemlocal := UTF8Lowercase(item.Local);
    if (txt = itemtxt) and ((url = itemurl) or (url = itemlocal)) then
      exit;
  end;

  item := FIndexSiteMap.Items.NewItem;
  item.Local := Trim(AUrl);
  item.Text := UTF8Trim(AText);
  item.Keyword := UTF8Trim(AText);
end;

procedure TWiki2CHMConverter.AddTocItem(ALevel: Integer; AText, AUrl: String);
// Is called whenever a new node is added to the xml TOC

  function NewItemAtLevel(ALevel: Integer): TChmSiteMapItem;
  var
    item: TChmSiteMapItem;
    items: TChmSiteMapItems;
    level: Integer;
  begin
    level := 0;
    items := FTOCSiteMap.Items;
    while level < ALevel do begin
      if items.Count = 0 then
        item := items.NewItem
      else
        item := items.Item[items.Count-1];
      items := item.Children;
      inc(level);
    end;
    Result := items.NewItem;
  end;

var
  item: TCHMSitemapItem;
begin
  item := NewItemAtLevel(ALevel);
  item.Local := AUrl;
  item.Text := EscapeToHTML(AText);
  item.ImageNumber := 0;
end;

function CompareIndex(Item1, Item2: Pointer): Integer;
var
  indexItem1, indexItem2: TChmSiteMapItem;
begin
  indexItem1 := TChmSiteMapItem(Item1);
  indexItem2 := TChmSiteMapItem(Item2);
  Result := UTF8CompareStr(UTF8Lowercase(indexItem1.Text), UTF8Lowercase(indexItem2.Text));
end;

procedure TWiki2CHMConverter.OnWriterLastFileAdded(Sender: TObject);
var
  CurWriter: TChmWriter;
begin
  // Assign the TOC and index files
  CurWriter := TChmWriter(Sender);
  if CurWriter=nil then exit;

  // write Index (see TChmProject.LastFileAdded)
  if (IndexFileName <> '') then begin
    FIndexStream := TMemoryStream.Create;
    FIndexSiteMap.Items.Sort(@CompareIndex);
    FIndexSiteMap.SaveToStream(FIndexStream);
    CurWriter.AppendIndex(FIndexStream);
    CurWriter.AppendBinaryIndexFromSitemap(FIndexSitemap,false);
  end;

  // write TOC (see TChmProject.LastFileAdded)
  if (TOCFileName <> '') then begin
    FTOCStream := TMemoryStream.Create;
    FTocSitemap.SaveToStream(FTOCStream);
    CurWriter.AppendTOC(FTocStream);
    CurWriter.AppendBinaryTOCFromSiteMap(FTOCSitemap);
  end;
end;

function TWiki2CHMConverter.GetImageLink(ImgFilename: string): string;
begin
  Result:=CHMImagesDir+ExtractFileName(ImgFilename);
end;

function TWiki2CHMConverter.GetInternalImageLink(ImgFilename: String): String;
begin
  Result := CHMImagesDir + 'internal/' + ExtractFilename(ImgFilename);
end;

function TWiki2CHMConverter.GetPageLink(Page: TW2XHTMLPage): string;
begin
//  Result:=Page.WikiDocumentName+'.html';
  Result :=Page.Filename;
end;

function TWiki2CHMConverter.GetRelativeCSSFilename: String;
begin
  Result := ExtractFileName(CSSFilename);
end;

procedure TWiki2CHMConverter.SetCHMFile(AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FCHMFile=NewValue then Exit;
  FCHMFile:=NewValue;
end;

procedure TWiki2CHMConverter.SetIndexFileName(AValue: string);
begin
  if FIndexFileName=AValue then Exit;
  FIndexFileName:=AValue;
end;

procedure TWiki2CHMConverter.SetTOCFilename(AValue: String);
begin
  if FTOCFileName = AValue then Exit;
  FTOCFilename := AValue;
end;

procedure TWiki2CHMConverter.ConvertInit;
var
  i: Integer;
  Page: TW2CHMPage;
begin
  inherited ConvertInit;

  if CHMFile='' then
    raise Exception.Create('chm file not set');

  FIndexSitemap := TChmSitemap.Create(stIndex);
  FTOCSitemap := TChmSitemap.Create(stTOC);
  AddTocItem(0, FTOCRootName, '');

  for i:=0 to Count-1 do begin
    Page:=TW2CHMPage(Pages[i]);
    DocumentNameToPage[GetPageLink(Page)]:=Page;
  end;
end;

procedure TWiki2CHMConverter.SaveAllPages;
var
  ms: TMemoryStream;
  Filename: String;
  i: Integer;
  F2PItem: PStringToPointerTreeItem;
begin
  Filename:=CHMFile;
  debugln(['initializing chm ...']);
  Writer:=nil;
  ms:=TMemoryStream.Create;
  try
    Writer:=TChmWriter.Create(ms,false);
    // see TChmProject.WriteChm();

    // our callback to get data
    Writer.OnGetFileData :=@OnWriterGetFileData;
    Writer.OnLastFile    :=@OnWriterLastFileAdded;
    FilesCompressed:=0;

    // give it the html files
    for i:=0 to Count-1 do
      Writer.FilesToCompress.Add(GetPageLink(TW2CHMPage(Pages[i])));

    // give it the css file
    Writer.FilesToCompress.Add(GetRelativeCSSFilename);

    // give it the image files
    for F2PItem in UsedImages do
      Writer.FilesToCompress.Add(F2PItem^.Name);

    // now some settings in the chm
    Writer.DefaultPage := GetPageLink(TW2CHMPage(Pages[0]));
    Writer.Title := Title;
    //Writer.DefaultFont := DefaultFont;
    Writer.FullTextSearch := true;
    Writer.HasBinaryIndex := true;
    Writer.IndexName := IndexFileName;
    Writer.HasBinaryTOC := true;
    Writer.TOCName := FTOCFileName;
    //Writer.ReadmeMessage := ReadmeMessage;
    {for i:=0 to Count-1 do
      begin
        ContextNode:=TChmContextNode(files.objects[i]);
        if assigned(ContextNode) and (ContextNode.contextnumber<>0) then
          Writer.AddContext(ContextNode.ContextNumber,files[i]);
      end;}
    //if FWindows.Count>0 then
    //  Writer.Windows:=FWIndows;

    if assigned(FTocSiteMap) then
      Writer.TocSitemap := FTocSiteMap;

    // and write!
    debugln(['creating chm ...']);
    Writer.Execute;

    debugln(['writing file "',Filename,'" ...']);
    ms.SaveToFile(Filename);
  finally
    FreeAndNil(Writer);
    ms.Free;
  end;
end;

constructor TWiki2CHMConverter.Create;
begin
  inherited Create;
  FPageClass:=TW2CHMPage;
  FOutputDir:='chm';
  FCHMFile:='wiki.chm';
  FIndexFileName:='_index.hhk';
  FTOCFileName:='_table_of_contents.hhc';
  FTOCRootName:='Pages available';
  DocumentNameToPage:=TStringToPointerTree.Create(true);
end;

destructor TWiki2CHMConverter.Destroy;
begin
  FreeAndNil(FTocSiteMap);
  FreeAndNil(FTocStream);
  FreeAndNil(FIndexSitemap);
  FreeAndNil(FIndexStream);
  FreeAndNil(DocumentNameToPage);
  inherited Destroy;
end;

procedure TWiki2CHMConverter.Clear;
begin
  if DocumentNameToPage <> nil then
    DocumentNameToPage.Clear;
  inherited Clear;
end;

end.

