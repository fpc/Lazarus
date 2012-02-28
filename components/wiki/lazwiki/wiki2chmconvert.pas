{ Converter for wiki pages to fpdoc topics

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
  Classes, SysUtils, Wiki2HTMLConvert, Wiki2XHTMLConvert, LazLogger, laz2_DOM,
  {$IFDEF EnableWikiCHMWriter}
  wikichmwriter, wikichmfilewriter, wikichmsitemap,
  {$ELSE}
  chmwriter, chmfilewriter, chmsitemap,
  {$ENDIF}
  LazUTF8, FileUtil, CodeToolsStructs;

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
    FIndexFileName: string;
    procedure SetCHMFile(AValue: string);
    procedure SetIndexFileName(AValue: string);
  protected
    Writer: TChmWriter;
    FilesCompressed: integer;
    DocumentNameToPage: TStringToPointerTree; // Page.WikiDocumentName+'.html' to Page
    procedure ConvertInit; override;
    function OnWriterGetFileData(const DataName: String; out PathInChm: String;
      out FileName: String; var Stream: TStream): Boolean;
    procedure WriteIndexToStream(aStream: TStream);
    procedure OnWriterLastFileAdded(Sender: TObject);
    function GetImageLink(ImgFilename: string): string; override;
    function GetPageLink(Page: TW2XHTMLPage): string; override;
    procedure SaveAllPages; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    property CHMFile: string read FCHMFile write SetCHMFile;
    property IndexFileName: string read FIndexFileName write SetIndexFileName;
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
  Filename:=DataName;
  PathInChm:='/';
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

procedure TWiki2CHMConverter.WriteIndexToStream(aStream: TStream);

  procedure w(const s: string);
  begin
    if s='' then exit;
    aStream.Write(s[1],length(s));
    {$IFDEF VerboseCHMIndex}
    dbgout(s);
    {$ENDIF}
  end;

  procedure wl(const s: string);
  begin
    w(s);
    w(#13#10);
  end;

var
  i: Integer;
  Page: TW2CHMPage;
  List: TStringList;
  CurTitle: String;
begin
  wl('<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">');
  wl('<html>');
  wl('<head>');
  wl('<meta name="generator" content="lazwiki">');
  wl('</head>');
  wl('<body>');
  wl('<object type="text/site properties">');
  wl('</object>');
  wl('<ul>');
  List:=TStringList.Create;
  try
    // add pages sorted by title
    for i:=0 to Count-1 do begin
      Page:=TW2CHMPage(Pages[i]);
      CurTitle:=UTF8Trim(Page.WikiPage.Title);
      if CurTitle='' then continue;
      List.AddObject(CurTitle,Page);
    end;
    List.Sort;
    for i:=0 to List.Count-1 do begin
      Page:=TW2CHMPage(List.Objects[i]);
      wl('  <li> <object type="text/sitemap">');
      wl('       <param name="Name" value="'+StrToXMLValue(Page.WikiPage.Title)+'">');
      wl('       <param name="Local" value="'+StrToXMLValue(GetPageLink(Page))+'">');
      wl('       <object type="text/sitemap">');
    end;
  finally
    List.Free;
  end;
  wl('</ul>');
  wl('</body>');
  wl('</html>');
end;

procedure TWiki2CHMConverter.OnWriterLastFileAdded(Sender: TObject);
var
  IndexStream: TMemoryStream;
  IndexSitemap: TChmSiteMap;
  CurWriter: TChmWriter;
begin
  // Assign the TOC and index files
  CurWriter := TChmWriter(Sender);
  if CurWriter=nil then exit;

  // write Index see TChmProject.LastFileAdded
  if (IndexFileName <> '') then begin
    IndexSitemap := nil;
    IndexStream := TMemoryStream.Create;
    try
      WriteIndexToStream(IndexStream);
      CurWriter.AppendIndex(IndexStream);
      IndexStream.Position := 0;
      IndexSitemap := TChmSiteMap.Create(stIndex);
      indexSitemap.LoadFromStream(IndexStream);
      CurWriter.AppendBinaryIndexFromSiteMap(IndexSitemap,False);
    finally
      IndexSitemap.Free;
      IndexStream.Free;
    end;
  end;

  // ToDo: write TOC see TChmProject.LastFileAdded
end;

function TWiki2CHMConverter.GetImageLink(ImgFilename: string): string;
begin
  Result:=CHMImagesDir+ExtractFileName(ImgFilename);
end;

function TWiki2CHMConverter.GetPageLink(Page: TW2XHTMLPage): string;
begin
  Result:=Page.WikiDocumentName+'.html';
end;

procedure TWiki2CHMConverter.SetCHMFile(AValue: string);
var
  NewValue: String;
begin
  NewValue:=AppendPathDelim(TrimFilename(AValue));
  if FCHMFile=NewValue then Exit;
  FCHMFile:=NewValue;
end;

procedure TWiki2CHMConverter.SetIndexFileName(AValue: string);
begin
  if FIndexFileName=AValue then Exit;
  FIndexFileName:=AValue;
end;

procedure TWiki2CHMConverter.ConvertInit;
var
  i: Integer;
  Page: TW2CHMPage;
begin
  inherited ConvertInit;
  if CHMFile='' then
    raise Exception.Create('chm file not set');
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
    //Writer.HasBinaryTOC := MakeBinaryTOC;
    //Writer.TocName   := TableOfContentsFileName;
    //Writer.ReadmeMessage := ReadmeMessage;
    {for i:=0 to Count-1 do
      begin
        ContextNode:=TChmContextNode(files.objects[i]);
        if assigned(ContextNode) and (ContextNode.contextnumber<>0) then
          Writer.AddContext(ContextNode.ContextNumber,files[i]);
      end;}
    //if FWindows.Count>0 then
    //  Writer.Windows:=FWIndows;

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
  DocumentNameToPage:=TStringToPointerTree.Create(true);
end;

destructor TWiki2CHMConverter.Destroy;
begin
  inherited Destroy;
  FreeAndNil(DocumentNameToPage);
end;

procedure TWiki2CHMConverter.Clear;
begin
  DocumentNameToPage.Clear;
  inherited Clear;
end;

end.

