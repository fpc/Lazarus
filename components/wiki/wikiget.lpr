{ Console utility to download the Lazarus wiki.
  Maybe it also works for other MediaWikis sites.

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
  - option to download only files changed in last n days.
}
program wikiget;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, LazFileUtils, laz2_XMLRead, laz2_DOM, laz2_XMLWrite,
  LazUTF8, LazLogger, CodeToolsStructs, CustApp, AVL_Tree,
  {$IF FPC_FULLVERSION<20701}
  myfphttpclient,
  {$ELSE}
  fphttpclient,
  {$ENDIF}
  WikiParser, WikiFormat;

type

  { TFetchWikiPage }

  TFetchWikiPage = class(TWikiPage)
  public
    PageName: string;
  end;

  { TWikiGet }

  TWikiGet = class(TCustomApplication)
  private
    FBaseURL: string;
    FFirstPage: string;
    FImagesDir: string;
    FNoWrite: boolean;
    FOutputDir: string;
    FNeededPages: TStringToPointerTree; // PageName to TFetchWikiPage
    FAllPages: TStringToPointerTree; // PageName to TFetchWikiPage
    FAllImages: TStringToStringTree; // image name to filename
  protected
    procedure DoRun; override;
    procedure GetAll;
    procedure DownloadPage(Page: string);
    procedure DownloadFirstNeededPage;
    procedure CheckNotUsedPages(Show, Delete: boolean);
    procedure DownloadImages;
    procedure DownloadPageImages(Page: string);
    procedure OnParseForImages(Token: TWPToken);
    procedure CheckNotUsedImages(Show, Delete: boolean);
    function LoadPageFromDisk(Page: string): TFetchWikiPage;
    function AddWikiPage(Page: string): TFetchWikiPage;
    function NeedWikiPage(Page: string): TFetchWikiPage;
    function PageToFilename(Page: string; IsInternalLink: boolean): string;
    function ImageToFilename(Image: string; IsInternalLink, KeepScheme: boolean): string;
    function EscapeDocumentName(aName: string): string;
    procedure Test;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property OutputDir: string read FOutputDir;
    property ImagesDir: string read FImagesDir;
    property BaseURL: string read FBaseURL;
    property NoWrite: boolean read FNoWrite;
  end;

{ TWikiGet }

procedure TWikiGet.DoRun;
const
  pPage = '--page=';

  procedure E(Msg: string; DoWriteHelp: boolean = false);
  begin
    if Msg<>'' then begin
      writeln('ERROR: ',Msg);
      writeln;
    end;
    if DoWriteHelp then
      WriteHelp;
    Terminate;
    Halt;
  end;

var
  ErrorMsg: String;
  i: Integer;
  Param: String;
begin
  //Test;
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help dir: images: baseurl: page: allmissing nowrite'
    +' shownotusedpages deletenotusedpages'
    +' shownotusedimages deletenotusedimages');
  if ErrorMsg<>'' then
    E(ErrorMsg,true);

  // parse parameters
  if HasOption('h','help') then
    E('',true);

  FNoWrite:=HasOption('nowrite');

  if HasOption('dir') then begin
    fOutputDir:=GetOptionValue('dir');
    if fOutputDir='' then
      E('output directory missing',true);
  end;
  fOutputDir:=CleanAndExpandDirectory(OutputDir);

  if HasOption('images') then begin
    FImagesDir:=GetOptionValue('images');
    if FImagesDir='' then
      E('images directory missing',true);
  end;
  FImagesDir:=CleanAndExpandDirectory(ImagesDir);

  if HasOption('baseurl') then
    FBaseURL:=GetOptionValue('baseurl');
  if HasOption('page') then
    fFirstPage:=GetOptionValue('page');

  // check parameters
  if not DirectoryExistsUTF8(OutputDir) then
    E('output directory not found "'+OutputDir+'"');
  if not DirectoryExistsUTF8(ImagesDir) then
    E('images directory not found "'+ImagesDir+'"');
  if copy(BaseURL,1,7)<>'http://' then
    E('invalid baseurl "'+BaseURL+'"');

  if HasOption('allmissing') then begin
    GetAll;
  end else begin
    for i:=1 to GetParamCount do begin
      Param:=GetParams(i);
      //writeln('TWikiGet.DoRun Param="',Param,'"');
      if copy(Param,1,length(pPage))=pPage then
        NeedWikiPage(WikiInternalLinkToPage(copy(Param,length(pPage)+1,length(Param))));
    end;
    if FNeededPages.Tree.Count=0 then
      E('nothing to do',true);
  end;

  while FNeededPages.Tree.Count>0 do
    DownloadFirstNeededPage;

  DownloadImages;

  CheckNotUsedPages(HasOption('shownotusedpages'),HasOption('deletenotusedpages'));
  CheckNotUsedImages(HasOption('shownotusedimages'),HasOption('deletenotusedimages'));

  // stop program loop
  Terminate;
end;

procedure TWikiGet.GetAll;
const
  IgnorePrefixes: array[1..9] of string = (
    'Special:',
    'Help:',
    'Random:',
    'User:',
    'http:',
    'https:',
    'doc:',
    'Category:',
    'index.php'
    );
var
  Client: TFPHTTPClient;
  Response: TMemoryStream;
  URL: String;
  Filename: String;
  s: string;
  p: SizeInt;
  StartPos: SizeInt;
  URLs: TStringList;
  i: Integer;
  j: Integer;
  Page: String;
  SaveTOC: Boolean;
begin
  Client:=nil;
  SaveTOC:=false;
  URLs:=TStringList.Create;
  SaveTOC:=false;
  try
    Client:=TFPHTTPClient.Create(nil);
    Response:=TMemoryStream.Create;
    // get list of range pages
    //URL:=BaseURL+'index.php?title=Special:Allpages&action=submit&namespace=0&from=';
    URL:=BaseURL+'index.php?title=Special:Allpages';
    writeln('getting page "',URL,'" ...');
    Client.Get(URL,Response);
    //Client.ResponseHeaders.SaveToFile('responseheaders.txt');
    if SaveTOC then begin
      Response.Position:=0;
      Filename:='all.html';
      writeln('saving page "',Filename,'" ...');
      if not NoWrite then
        Response.SaveToFile(Filename);
    end;
    if Response.Size>0 then begin
      Response.Position:=0;
      SetLength(s,Response.Size);
      Response.Read(s[1],length(s));
      repeat
        p:=Pos('<a href="/Special:Allpages/',s);
        if p<1 then break;
        inc(p,length('<a href="'));
        StartPos:=p;
        while (p<=length(s)) and (s[p]<>'"') do inc(p);
        URL:=copy(s,StartPos,p-StartPos);
        if (URL<>'') and (URLs.IndexOf(URL)<0) then begin;
          //writeln('TWikiGet.GetAll URL="',URL,'"');
          URLs.Add(URL);
        end;
        System.Delete(s,1,p);
      until false;
    end;

    // get all range pages
    for i:=0 to URLs.Count-1 do begin
      URL:=EscapeDocumentName(URLs[i]);
      URL:=BaseURL+URL;
      Response.Clear;
      writeln('getting page "',URL,'" ...');
      Client.Get(URL,Response);
      //Client.ResponseHeaders.SaveToFile('responseheaders.txt');
      if SaveTOC then begin
        Response.Position:=0;
        Filename:='all_'+IntToStr(i+1)+'.html';
        writeln('saving page "',Filename,'" ...');
        if not NoWrite then
          Response.SaveToFile(Filename);
      end;
      if Response.Size>0 then begin
        Response.Position:=0;
        SetLength(s,Response.Size);
        Response.Read(s[1],length(s));
        repeat
          p:=Pos('<a href="/',s);
          if p<1 then break;
          inc(p,length('<a href="'));
          StartPos:=p;
          while (p<=length(s)) and (s[p]<>'"') do inc(p);
          Page:=copy(s,StartPos,p-StartPos);
          while (Page<>'') and (Page[1]='/') do
            System.Delete(Page,1,1);
          if (Page<>'') then begin;
            j:=low(IgnorePrefixes);
            while j<=high(IgnorePrefixes) do begin
              if copy(Page,1,length(IgnorePrefixes[j]))=IgnorePrefixes[j] then
                break;
              inc(j);
            end;
            if j>high(IgnorePrefixes) then begin
              //writeln('TWikiGet.GetAll Page="',Page,'"');
              Filename:=PageToFilename(Page,false);
              AddWikiPage(Page);
              if not FileExistsUTF8(Filename) then begin
                writeln('TWikiGet.GetAll missing Page="',Page,'"');
                NeedWikiPage(Page);
              end;
            end;
          end;
          System.Delete(s,1,p);
        until false;
      end;
    end;
  finally
    URLs.Free;
    Client.Free;
    Response.Free;
  end;
end;

procedure TWikiGet.DownloadPage(Page: string);
var
  Response: TMemoryStream;
  Client: TFPHTTPClient;
  URL: String;
  Filename: String;
begin
  Filename:=PageToFilename(Page,false);
  Response:=nil;
  Client:=nil;
  try
    Client:=TFPHTTPClient.Create(nil);
    Response:=TMemoryStream.Create;
    URL:=BaseURL+'index.php?title=Special:Export&pages='+EscapeDocumentName(Page)+'&curonly=1&action=submit';
    writeln('getting page "',URL,'" ...');
    Client.Get(URL,Response);
    //Client.ResponseHeaders.SaveToFile('responseheaders.txt');
    Response.Position:=0;
    writeln('saving page "',Filename,'" ...');
    Response.Position:=0;
    if not NoWrite then
      Response.SaveToFile(Filename);
  finally
    Client.Free;
    Response.Free;
  end;
end;

procedure TWikiGet.DownloadFirstNeededPage;
var
  Node: TAVLTreeNode;
  Page: String;
begin
  Node:=FNeededPages.Tree.FindLowest;
  if Node=nil then exit;
  Page:=PStringMapItem(Node.Data)^.Name;
  FNeededPages.Remove(Page);
  DownloadPage(Page);
end;

procedure TWikiGet.CheckNotUsedPages(Show, Delete: boolean);
var
  Item: PStringToPointerTreeItem;
  Files: TFilenameToPointerTree;
  FileInfo: TSearchRec;
  Page: TFetchWikiPage;
  Filename: TFilename;
begin
  Files:=TFilenameToPointerTree.Create(false);
  try
    for Item in FAllPages do begin
      Page:=TFetchWikiPage(Item^.Value);
      Files[Page.Filename]:=Page;
    end;
    if Show then
      writeln('Not needed files in the output directory "',OutputDir,'":');
    if FindFirstUTF8(OutputDir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        if (faDirectory and FileInfo.Attr)<>0 then continue;
        Filename:=OutputDir+FileInfo.Name;
        if Files.Contains(Filename) then continue;
        if Show then
          writeln('page:',FileInfo.Name);
        if Delete then begin
          writeln('deleting page: ',FileInfo.Name);
          if (not NoWrite) and (not DeleteFileUTF8(Filename)) then
            writeln('failed to delete page "',Filename,'"');
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  finally
    Files.Free;
  end;
end;

procedure TWikiGet.DownloadImages;
var
  Item: PStringToPointerTreeItem;
begin
  writeln('checking images of ',FAllPages.Tree.Count,' pages ...');
  for Item in FAllPages do
    DownloadPageImages(Item^.Name);
  writeln('total images: ',FAllImages.Tree.Count);
end;

procedure TWikiGet.DownloadPageImages(Page: string);
var
  p: TFetchWikiPage;
begin
  p:=LoadPageFromDisk(Page);
  if p=nil then exit;
  //writeln('TWikiGet.DownloadPageImages ',p.Filename,' ',length(p.Src));
  p.Verbosity:=wpvError;
  p.Parse(@OnParseForImages,p);
end;

procedure TWikiGet.OnParseForImages(Token: TWPToken);
var
  p: TFetchWikiPage;
  LinkToken: TWPLinkToken;
  Link: String;
  Filename: String;
  Client: TFPHTTPClient;
  Response: TMemoryStream;
  URL: String;
  i: SizeInt;
  StartPos: SizeInt;
  ImageLink: String;
  ColonPos: SizeInt;
  Prefix: String;
  Data: string;
  SrcLink: String;
begin
  p:=TFetchWikiPage(Token.UserData);
  if Token.Token=wptInternLink then begin
    LinkToken:=Token as TWPLinkToken;
    SrcLink:=LinkToken.Link;
    Link:=SrcLink;
    ColonPos:=Pos(':',Link);
    //writeln('TWikiGet.OnParseForImages Link="',Link,'" ColonPos=',ColonPos);
    if ColonPos<1 then exit;
    if ColonPos=length(Link) then exit;
    Prefix:=lowercase(copy(Link,1,ColonPos-1));
    if Prefix<>'image' then exit;
    Link:=UTF8Trim(copy(Link,ColonPos+1,length(Link)));
    if Link='' then exit;
    Filename:=ImageToFilename(Link,true,true);
    //writeln('TWikiGet.OnParseForImages page="',p.Filename,'" Link="',Link,'" => ',Filename);
    if FAllImages.Contains(Link) then exit; // already tried
    FAllImages[Link]:=Filename;
    if FileExistsUTF8(Filename) then exit;
    //writeln('TWikiGet.OnParseForImages ',FileExists(Filename),' ',FileExistsUTF8(Filename),' "',Filename,'"');
    // download image page
    Response:=nil;
    Client:=nil;
    try
      try
        Client:=TFPHTTPClient.Create(nil);
        Response:=TMemoryStream.Create;
        URL:=BaseURL+EscapeDocumentName('Image:'+WikiInternalLinkToPage(Link));
        writeln('getting image page "',URL,'" ...');
        Client.Get(URL,Response);
        //Client.ResponseHeaders.SaveToFile('responseheaders.txt');
        Response.Position:=0;
        SetLength(Data,Response.Size);
        if Data<>'' then
          Response.Read(Data[1],length(Data));
        i:=Pos('class="fullImageLink"',Data);
        if i<1 then begin
          writeln('TWikiGet.OnParseForImages WARNING: image page has no fullImageLink marker: "',URL,'"');
          writeln('saving responseheaders.txt ...');
          if not NoWrite then
            Client.ResponseHeaders.SaveToFile('responseheaders.txt');
          writeln('saving response.txt ...');
          if not NoWrite then
            Response.SaveToFile('response.txt');
        end;
        while i<=length(Data) do begin
          if (copy(Data,i,5)='src="') then begin
            //writeln('TWikiGet.OnParseForImages src found ...');
            inc(i,5);
            StartPos:=i;
            while (i<=length(Data)) and (Data[i]<>'"') do
              inc(i);
            ImageLink:=UTF8Trim(copy(Data,StartPos,i-StartPos));
            if ImageLink='' then exit;
            //writeln('TWikiGet.OnParseForImages Img="',ImageLink,'"');
            URL:=BaseURL+EscapeDocumentName(ImageLink);
            writeln('getting image "',URL,'" ...');
            Response.Clear;
            Client.Get(URL,Response);
            writeln('saving image to "',Filename,'" ...');
            if not NoWrite then
              Response.SaveToFile(Filename);
            break;
          end;
          inc(i);
        end;
      except
        on E: EHTTPClient do begin
          writeln('TWikiGet.OnParseForImages WARNING: page="'+p.Filename+'" Link="'+Link+'" SrcLink="'+SrcLink+'" URL="'+URL+'": '+E.Message);
        end;
      end;
    finally
      Client.Free;
      Response.Free;
    end;
  end;
end;

procedure TWikiGet.CheckNotUsedImages(Show, Delete: boolean);
var
  Files: TFilenameToStringTree;
  FileInfo: TSearchRec;
  Filename: string;
  Item: PStringToStringTreeItem;
begin
  Files:=TFilenameToStringTree.Create(false);
  try
    for Item in FAllImages do begin
      Filename:=Item^.Value;
      Files[Filename]:='1';
    end;
    if Show then
      writeln('Not needed files in the images directory "',ImagesDir,'":');
    if FindFirstUTF8(ImagesDir+AllFilesMask,faAnyFile,FileInfo)=0 then begin
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        if (faDirectory and FileInfo.Attr)<>0 then continue;
        Filename:=ImagesDir+FileInfo.Name;
        if Files.Contains(Filename) then continue;
        if Show then
          writeln('image:',FileInfo.Name);
        if Delete then begin
          writeln('deleting image: ',FileInfo.Name);
          if (not NoWrite) and (not DeleteFileUTF8(Filename)) then
            writeln('failed to delete image "',Filename,'"');
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  finally
    Files.Free;
  end;
end;

function TWikiGet.LoadPageFromDisk(Page: string): TFetchWikiPage;
var
  Filename: String;
begin
  Result:=AddWikiPage(Page);
  if (Result=nil) or (Result.Src<>'') then exit;
  Filename:=PageToFilename(Page,false);
  //writeln('TWikiGet.LoadPageFromDisk ',Page,' File=',Filename);
  if not FileExistsUTF8(Filename) then begin
    writeln('TWikiGet.LoadPageFromDisk page "',Page,'": file not found "',Filename,'"');
    exit;
  end;
  Result.LoadFromFile(Filename);
end;

function TWikiGet.AddWikiPage(Page: string): TFetchWikiPage;
begin
  if Page='' then exit(nil);
  Result:=TFetchWikiPage(FAllPages[Page]);
  if Result=nil then begin
    Result:=TFetchWikiPage.Create;
    Result.PageName:=Page;
    Result.Filename:=PageToFilename(Page,false);
    FAllPages[Page]:=Result;
  end;
end;

function TWikiGet.NeedWikiPage(Page: string): TFetchWikiPage;
begin
  Result:=AddWikiPage(Page);
  if Result=nil then exit;
  FNeededPages[Page]:=Result;
end;

function TWikiGet.PageToFilename(Page: string; IsInternalLink: boolean): string;
begin
  Result:=OutputDir+WikiPageToFilename(Page,IsInternalLink,true)+'.xml';
end;

function TWikiGet.ImageToFilename(Image: string; IsInternalLink,
  KeepScheme: boolean): string;
begin
  Result:=ImagesDir+WikiImageToFilename(Image,IsInternalLink,true,KeepScheme);
end;

function TWikiGet.EscapeDocumentName(aName: string): string;
var
  i: Integer;
  s: String;
begin
  Result:=aName;
  i:=1;
  while i<=length(Result) do begin
    s:=Result[i];
    case s[1] of
    ':': s:='%'+HexStr(ord(s[1]),2);
    end;
    if s<>Result[i] then
      ReplaceSubstring(Result,i,1,s);
    inc(i,length(s));
  end;

  if (Result<>'') and (Result[1]='/') then
    Delete(Result,1,1);
end;

procedure TWikiGet.Test;

  procedure w(URL: string);
  var
    Page: String;
    Filename: String;
  begin
    debugln(['TWikiGet.Test [',URL,']']);
    Page:=WikiInternalLinkToPage(URL);
    debugln(['  URL=[',dbgstr(URL),']  Page=[',Page,']']);
    Filename:=WikiImageToFilename(Page,false,true);
    debugln(['  URL=[',dbgstr(URL),']  Filename="',Filename,'"']);
  end;

begin
  //w('Image:Acs_demos.jpg');
  //w('Image:Acs demos.jpg');
  w('Image:Acs%20demos.jpg');
  //w('Image:Acs demos.JPG');

  Halt;
end;

constructor TWikiGet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  fOutputDir:='wikixml';
  FImagesDir:='images';
  FBaseURL:='http://wiki.lazarus.freepascal.org/';
  fFirstPage:='Lazarus_Documentation';
  FAllPages:=TStringToPointerTree.Create(true);
  FNeededPages:=TStringToPointerTree.Create(true);
  FAllImages:=TStringToStringTree.Create(true);
end;

destructor TWikiGet.Destroy;
begin
  FAllImages.Free;
  FAllPages.Free;
  FNeededPages.Free;
  inherited Destroy;
end;

procedure TWikiGet.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('--dir=<directory>     : directory where to store the files. Default: ',OutputDir);
  writeln('--images=<directory>  : directory where to store the images. Default: ',ImagesDir);
  writeln('--baseurl=<URL>       : URL of the wiki. Default: ',BaseURL);
  writeln('--page=<pagename>     : download this wiki page. Can be given multiple times.');
  writeln('--allmissing          : download all wiki pages, if file not already there.');
  writeln('--shownotusedpages    : show not used files in the output directory.');
  writeln('--deletenotusedpages  : delete the files in the output directory that are not used.');
  writeln('--shownotusedimages   : show not used files in the images directory.');
  writeln('--deletenotusedimages : delete the files in the images directory that are not used.');
  writeln('--nowrite             : do not write files, just print what would be written.');
  writeln;
  writeln('Example:');
  writeln('  ',ExeName,' --dir=. --images=images --page=Install_Packages');
end;

var
  Application: TWikiGet;
begin
  Application:=TWikiGet.Create(nil);
  Application.Title:='Wiki Get';
  Application.Run;
  Application.Free;
end.

