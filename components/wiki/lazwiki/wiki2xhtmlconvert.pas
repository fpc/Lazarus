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

}
unit Wiki2XHTMLConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WikiFormat, WikiParser, laz2_DOM, LazLogger, LazFileUtils,
  laz2_XMLWrite, LazUTF8, BasicCodeTools, KeywordFuncLists, CodeToolsStructs;

type

  { TW2XHTMLPage }

  TW2XHTMLPage = class(TW2FormatPage)
  private
    BodyNode: TDOMElement;
    CurNode: TDOMElement; // current xhtml node
    SectionLevel: integer;
  public
    XHTML: TXMLDocument;
    Filename: string;
    destructor Destroy; override;
  end;

  { TWiki2XHTMLConverter }

  TWiki2XHTMLConverter = class(TWiki2FormatConverter)
  private
    FCSSFilename: string;
    FMaxH: integer;
    FPageFileExt: string;
    procedure SetCSSFilename(AValue: string);
    procedure SetMaxH(AValue: integer);
    procedure SetPageFileExt(AValue: string);
  protected
    ShortFilenameToPage: TFilenameToPointerTree;
    UsedImages: TFilenameToPointerTree; // image name to first page using the image
    procedure ConvertPage(Page: TW2XHTMLPage); virtual;
    procedure OnWikiToken(Token: TWPToken); virtual;
    function GetImageLink(ImgFilename: string): string; virtual;
    function GetPageLink(Page: TW2XHTMLPage): string; virtual;
    function InsertLink(const LinkToken: TWPLinkToken): boolean;
    procedure InsertText(Token: TWPToken; Txt: string); virtual;
    procedure InsertCode(Token: TWPNameValueToken); virtual;
    procedure SavePage(Page: TW2XHTMLPage); virtual;
    procedure ConvertInit; virtual;
    procedure ConvertAllPages; virtual;
    procedure SaveAllPages; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Convert; override;
    function GetRelativeCSSFilename: string; virtual;
    property CSSFilename: string read FCSSFilename write SetCSSFilename;
    property MaxH: integer read FMaxH write SetMaxH;
    function PageToFilename(Page: string; IsInternalLink, Full: boolean): string;
    function PageToFilename(Page: TW2XHTMLPage; Full: boolean): string;
    property PageFileExt: string read FPageFileExt write SetPageFileExt;
  end;

implementation

{ TWiki2XHTMLConverter }

procedure TWiki2XHTMLConverter.SetCSSFilename(AValue: string);
begin
  if FCSSFilename=AValue then Exit;
  FCSSFilename:=AValue;
end;

procedure TWiki2XHTMLConverter.SetMaxH(AValue: integer);
begin
  if AValue<1 then AValue:=1;
  if AValue>MaxHeaderDepth then AValue:=MaxHeaderDepth;
  if FMaxH=AValue then Exit;
  FMaxH:=AValue;
end;

procedure TWiki2XHTMLConverter.SetPageFileExt(AValue: string);
var
  NewValue: String;
begin
  NewValue:=Trim(AValue);
  if (NewValue<>'') and (NewValue[1]<>'.') then
    NewValue:='.'+NewValue;
  if FPageFileExt=NewValue then Exit;
  FPageFileExt:=NewValue;
end;

function TWiki2XHTMLConverter.InsertLink(const LinkToken: TWPLinkToken
  ): boolean;
var
  Page: TW2XHTMLPage;
  W: TWikiPage;
  doc: TXMLDocument;

  function WarnURL(URL: string): boolean;
  var
    p: Integer;
  begin
    p:=1;
    while p<=length(URL) do begin
      if URL[p]='/' then begin
        if NoWarnBaseURLs.Contains(copy(URL,1,p-1)) then exit(false);
      end;
      inc(p);
    end;
    Result:=true;
  end;

  function HandleLink(var URL, Caption: string): boolean;
  var
    p: SizeInt;
    Scheme: String;
    Filename: String;
    Node: TDOMElement;
    TargetPage: TW2XHTMLPage;
  begin
    Result:=false;
    if LinkToken.Token=wptExternLink then exit;

    p:=Pos(':', URL);
    if p>1 then begin
      // check special pages
      Scheme:=lowercase(copy(URL, 1, p-1));
      if Scheme='user' then begin
        URL:=''; // show name without link
        exit;
      end
      else if Scheme='category' then begin
        URL:=''; // show category without link
        exit;
      end
      else if Scheme='image' then begin
        if ImagesDir='' then exit;
        URL:=copy(URL,p+1,length(URL));
        URL:=UTF8Trim(URL);
        URL:=WikiInternalLinkToPage(URL);
        if URL='' then exit;
        Filename:=ImagesDir+WikiImageToFilename(URL,false,true,true);
        if FileExistsUTF8(Filename) then begin
          Filename:=GetImageLink(Filename);
          if not UsedImages.Contains(Filename) then
            UsedImages[Filename]:=Page;
          Node:=doc.CreateElement('img');
          Node.SetAttribute('src', Filename);
          if Caption<>'' then
            Node.SetAttribute('alt', Caption);
          Page.CurNode.AppendChild(Node);
          exit(true);
        end;
        if WarnURL(LinkToken.Link) then
          debugln(['WARNING: TWiki2XHTMLConverter.InsertLink "'+dbgstr(LinkToken.Link)+'": image file not found: "',Filename,'" at ',W.PosToStr(LinkToken.LinkStartPos,true)]);
        URL:='';
        exit;
      end;
    end;

    // convert %hh and remove special characters
    URL:=WikiInternalLinkToPage(URL);

    // check if link to wiki page but with a full baseurl
    if (Page.WikiPage.BaseURL<>'')
    and (LeftStr(URL,length(Page.WikiPage.BaseURL))=Page.WikiPage.BaseURL)
    then begin
      Delete(URL,1,length(Page.WikiPage.BaseURL));
      while (URL<>'') and (URL[1]='/') do Delete(URL,1,1);
    end;

    // check if an external link but accidentally marked as internal
    p:=pos('://',URL);
    if p>1 then begin
      Scheme:=lowercase(copy(URL, 1, p-1));
      if (Scheme='http') or (Scheme='https') then begin
        // external link
        exit;
      end;
    end;

    // default: a page
    if URL<>'' then begin
      Filename:=PageToFilename(URL,true,false);
      TargetPage:=TW2XHTMLPage(ShortFilenameToPage[Filename]);
      if (TargetPage<>nil) then begin
        URL:=GetPageLink(TargetPage);
      end else if (not FileExistsUTF8(Filename)) then begin
        if WarnMissingPageLinks and WarnURL(LinkToken.Link) then
          debugln(['WARNING: TWiki2XHTMLConverter.InsertLink "'+dbgstr(LinkToken.Link)+'": file not found: "',Filename,'" at ',W.PosToStr(LinkToken.LinkStartPos,true)]);
        URL:='';
      end;
    end;
  end;

var
  Caption: String;
  URL: String;
  Node: TDOMElement;
begin
  Result:=false;
  Page:=TW2XHTMLPage(LinkToken.UserData);
  W:=Page.WikiPage;
  doc:=Page.XHTML;
  URL:=LinkToken.Link;
  if URL='' then exit;
  Caption:=copy(W.Src, LinkToken.CaptionStartPos, LinkToken.CaptionEndPos-
    LinkToken.CaptionStartPos);
  if Caption='' then exit(true);
  if HandleLink(URL,Caption) then exit;

  if URL<>'' then begin
    Node:=doc.CreateElement('a');
    Node.SetAttribute('href', URL);
    if Caption<>'' then
      Node.AppendChild(doc.CreateTextNode(Caption));
    Page.CurNode.AppendChild(Node);
  end else if Caption<>'' then begin
    InsertText(LinkToken, Caption);
    Result:=true;
  end;
end;

procedure TWiki2XHTMLConverter.ConvertPage(Page: TW2XHTMLPage);
var
  doc: TXMLDocument;
  RootNode: TDOMElement;
  HeadNode: TDOMElement;
  MetaNode: TDOMElement;
  TitleNode: TDOMElement;
  CSSNode: TDOMElement;
  Node: TDOMElement;
  CurCSSFilename: String;
begin
  FreeAndNil(Page.XHTML);
  Page.XHTML:=TXMLDocument.Create;
  doc:=Page.XHTML;
  // <html>
  RootNode:=doc.CreateElement('html');
  doc.AppendChild(RootNode);
  // <head>
  HeadNode:=doc.CreateElement('head');
  RootNode.AppendChild(HeadNode);
  // <meta content="text/html; charset=utf-8" http-equiv="Content-Type">
  MetaNode:=doc.CreateElement('meta');
  HeadNode.AppendChild(MetaNode);
  MetaNode.SetAttribute('content','text/html; charset=utf-8');
  MetaNode.SetAttribute('http-equiv','Content-Type');
  // <title>
  TitleNode:=doc.CreateElement('title');
  HeadNode.AppendChild(TitleNode);
  TitleNode.AppendChild(doc.CreateTextNode(Page.WikiPage.Title));
  CurCSSFilename:=GetRelativeCSSFilename;
  if CurCSSFilename<>'' then begin
    // stylesheet <link href="fp.css" type="text/css" rel="stylesheet">
    CSSNode:=doc.CreateElement('link');
    HeadNode.AppendChild(CSSNode);
    CSSNode.SetAttribute('href',CurCSSFilename);
    CSSNode.SetAttribute('type','text/css');
    CSSNode.SetAttribute('rel','stylesheet');
  end;
  // <body>
  Page.BodyNode:=doc.CreateElement('body');
  RootNode.AppendChild(Page.BodyNode);
  // title <h1 class="firstHeading">
  Node:=doc.CreateElement('h1');
  Page.BodyNode.AppendChild(Node);
  Node.SetAttribute('class','firstHeading');
  Node.AppendChild(doc.CreateTextNode(Page.WikiPage.Title));

  try
    Page.SectionLevel:=0;
    Page.CurNode:=Page.BodyNode;
    Page.WikiPage.Parse(@OnWikiToken,Page);
  finally
    Page.BodyNode:=nil;
    Page.CurNode:=nil;
  end;
end;

procedure TWiki2XHTMLConverter.OnWikiToken(Token: TWPToken);
var
  Page: TW2XHTMLPage;
  W: TWikiPage;

  procedure NodeNotOpen;
  begin
    raise Exception.Create('TWiki2XHTMLConverter.OnWikiToken can not close:'
      +' Token='+dbgs(Token.Token)+' '+DbgSName(Token)+' CurNode='+Page.CurNode.TagName);
  end;

  procedure MissingNodeName;
  begin
    raise Exception.Create('TWiki2XHTMLConverter.OnWikiToken have no node name:'
      +' Token='+dbgs(Token.Token)+' '+DbgSName(Token));
  end;

var
  doc: TXMLDocument;
  NodeName: string;
  Node: TDOMElement;
  LinkToken: TWPLinkToken;
  NodeClass: String;
  NameValueToken: TWPNameValueToken;
  CurName: String;
  CurValue: String;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  W:=Page.WikiPage;
  doc:=Page.XHTML;
  //debugln(['TWiki2XHTMLConverter.OnWikiToken Token=',dbgs(Token.Token),' ',dbgs(Token)]);
  case Token.Token of
  wptText:
    if Token is TWPTextToken then begin
      InsertText(Token,copy(W.Src,TWPTextToken(Token).StartPos,
                 TWPTextToken(Token).EndPos-TWPTextToken(Token).StartPos));
      exit;
    end;

  wptLineBreak:
    begin
      // only append a br if there is something in front
      if Page.CurNode.FirstChild<>nil then
        Page.CurNode.AppendChild(doc.CreateElement('br'));
      exit;
    end;

  wptHorizontalRow:
    begin
      // only append a hr if there is something in front
      if Page.CurNode.FirstChild<>nil then
        Page.CurNode.AppendChild(doc.CreateElement('hr'));
      exit;
    end;

  wptP, wptBold, wptItalic, wptStrikeTagShort, wptUnderlineTag, wptTT,
  wptSup, wptSub, wptSmall, wptEm, wptSpan, wptString, wptVar, wptKey,
  wptPre, wptCenter,
  wptBulletList, wptNumberedList, wptDefinitionList,
  wptTable, wptTableRow, wptTableCell, wptTableHeadCell,
  wptSection, wptHeader:
    begin
      // simple range
      NodeClass:='';
      NodeName:='';
      case Token.Token of
      wptP: NodeName:='p';
      wptBold: NodeName:='b';
      wptItalic: NodeName:='i';
      wptStrikeTagShort: NodeName:='s';
      wptUnderlineTag: NodeName:='u';
      wptTT: NodeName:='tt';
      wptSup: NodeName:='sup';
      wptSub: NodeName:='sub';
      wptSmall: NodeName:='small';
      wptEm: NodeName:='em';
      wptSpan: NodeName:='span';
      wptString: begin NodeName:='span'; NodeClass:='string'; end;
      wptVar: begin NodeName:='span'; NodeClass:='var'; end;
      wptKey: begin NodeName:='span'; NodeClass:='key'; end;

      wptCode: begin NodeName:='pre'; NodeClass:='code'; end;
      wptPre: NodeName:='pre';
      wptCenter: NodeName:='center';

      wptNumberedList: NodeName:='ol';
      wptBulletList: NodeName:='ul';
      wptDefinitionList: NodeName:='dl';

      wptTable: NodeName:='table';
      wptTableRow: NodeName:='tr';
      wptTableCell: NodeName:='td';
      wptTableHeadCell: NodeName:='th';

      wptSection:
        begin
          NodeName:='div';
          NodeClass:='section';
          if Token.Range=wprOpen then
            inc(Page.SectionLevel)
          else if Token.Range=wprClose then
            dec(Page.SectionLevel);
        end;
      wptHeader:
        begin
          if Page.SectionLevel<=1 then
            NodeName:='h1'
          else if Page.SectionLevel<=MaxH then
            NodeName:='h'+IntToStr(Page.SectionLevel)
          else if Page.SectionLevel>MaxH then begin
            NodeName:='h'+IntToStr(MaxH);
            NodeClass:='subTitle';
          end;
        end;
      end;
      if NodeName='' then
        MissingNodeName;

      if Token.Range=wprOpen then begin
        Node:=doc.CreateElement(NodeName);
        Page.CurNode.AppendChild(Node);
        if NodeClass<>'' then
          Node.SetAttribute('class',NodeClass);
        Page.CurNode:=Node;
        exit;
      end else if Token.Range=wprClose then begin
        if Page.CurNode.TagName<>NodeName then
          NodeNotOpen;
        Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
        exit;
      end;
    end;

  wptListItem:
    if Token.Range=wprOpen then begin
      if Page.CurNode.TagName='dl' then
        NodeName:='dd'
      else
        NodeName:='li';
      Node:=doc.CreateElement(NodeName);
      Page.CurNode.AppendChild(Node);
      Page.CurNode:=Node;
      exit;
    end else if Token.Range=wprClose then begin
      if (Page.CurNode.TagName<>'dd')
      and (Page.CurNode.TagName<>'li') then
        NodeNotOpen;
      Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
      exit;
    end;

  wptAttribute:
    if Token is TWPNameValueToken then begin
      NameValueToken:=TWPNameValueToken(Token);
      CurName:=copy(W.Src,NameValueToken.NameStartPos,NameValueToken.NameEndPos-NameValueToken.NameStartPos);
      CurValue:=copy(W.Src,NameValueToken.ValueStartPos,NameValueToken.ValueEndPos-NameValueToken.ValueStartPos);
      Page.CurNode.SetAttribute(CurName,CurValue);
      exit;
    end;

  wptSpecial:
    if Token is TWPNameValueToken then begin
      NameValueToken:=TWPNameValueToken(Token);
      CurName:=copy(W.Src,NameValueToken.NameStartPos,NameValueToken.NameEndPos-NameValueToken.NameStartPos);
      CurValue:=copy(W.Src,NameValueToken.ValueStartPos,NameValueToken.ValueEndPos-NameValueToken.ValueStartPos);
      Node:=doc.CreateElement('span');
      if CurName<>'' then
        Node.SetAttribute('class',CurName);
      Page.CurNode.AppendChild(Node);
      if CurValue<>'' then
        Node.AppendChild(doc.CreateTextNode(CurValue));
      exit;
    end;

  wptCode:
    if Token is TWPNameValueToken then begin
      InsertCode(TWPNameValueToken(Token));
      exit;
    end;

  wptInternLink, wptExternLink:
    if Token is TWPLinkToken then begin
      LinkToken:=TWPLinkToken(Token);
      InsertLink(LinkToken);
      exit;
    end;

  end;

  debugln(['TWiki2XHTMLConverter.OnWikiToken ToDo: Token=',dbgs(Token.Token),' Range=',dbgs(Token.Range),' Class=',Token.ClassName,' ',W.PosToStr(W.CurrentPos)]);
end;

function TWiki2XHTMLConverter.GetImageLink(ImgFilename: string): string;
begin
  Result:=CreateRelativePath(ImgFilename,OutputDir);
end;

function TWiki2XHTMLConverter.GetPageLink(Page: TW2XHTMLPage): string;
begin
  Result:=PageToFilename(Page,false);
end;

procedure TWiki2XHTMLConverter.InsertText(Token: TWPToken; Txt: string);
var
  Page: TW2XHTMLPage;
  doc: TXMLDocument;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  doc:=Page.XHTML;
  //debugln(['TWiki2XHTMLConverter.InsertText Txt="',dbgstr(Txt),'"']);
  if Txt='' then exit;
  if Page.CurNode.TagName<>'pre' then begin
    if UTF8Trim(Txt)='' then begin
      // skip empty text
      exit;
    end;
    if Page.CurNode.FirstChild=nil then
      Txt:=UTF8Trim(Txt,[u8tKeepEnd]);
  end;
  //debugln(['TWiki2XHTMLConverter.InsertText Node="',Page.CurNode.TagName,'" Text="',Txt,'"']);
  Txt:=EncodeLesserAndGreaterThan(Txt);
  Page.CurNode.AppendChild(doc.CreateTextNode(Txt));
end;

procedure TWiki2XHTMLConverter.InsertCode(Token: TWPNameValueToken);
type
  TPascalToken = (
    pNone,
    pString,
    pKey,
    pNumber,
    pSymbol,
    pComment
    );
const
  PascalTokenClass: array[TPascalToken] of string = (
    '', // pNone,
    'string', // pString,
    'key', // pKey,
    'num', // pNumber,
    'sym', // pSymbol
    'cmt' // pComment
    );
var
  doc: TXMLDocument;
  LastToken: TPascalToken;
  LastRangeStart: PChar;
  CodeNode: TDOMElement;

  procedure Flush(TxtStart: PChar);
  var
    l: PtrUInt;
    Code: string;
    TokenNode: TDOMElement;
  begin
    l:=TxtStart-LastRangeStart;
    if l=0 then exit;
    SetLength(Code,l);
    Move(LastRangeStart^,Pointer(Code)^,l);
    if LastToken=pNone then begin
      CodeNode.AppendChild(doc.CreateTextNode(Code))
    end else begin
      TokenNode:=doc.CreateElement('span');
      TokenNode.SetAttribute('class',PascalTokenClass[LastToken]);
      TokenNode.AppendChild(doc.CreateTextNode(Code));
      CodeNode.AppendChild(TokenNode);
    end;
    LastRangeStart:=TxtStart;
  end;

  procedure AddSpan(PascalToken: TPascalToken; TxtStart: PChar);
  begin
    if PascalToken<>LastToken then
      Flush(TxtStart);
    LastToken:=PascalToken;
  end;

var
  Page: TW2XHTMLPage;
  W: TWikiPage;
  CurName: String;
  CurValue: String;
  p: PChar;
  AtomStart: PChar;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  W:=Page.WikiPage;
  doc:=Page.XHTML;
  CurName:=lowercase(copy(W.Src,Token.NameStartPos,Token.NameEndPos-Token.NameStartPos));
  CurValue:=copy(W.Src,Token.ValueStartPos,Token.ValueEndPos-Token.ValueStartPos);
  CodeNode:=doc.CreateElement('pre');
  if (CurName='pascal')
  or (CurName='delphi')
  or (CurName='code')
  or (CurName='source')
  or (CurName='fpc')
  then
    CurName:='pascal';
  if CurName<>'' then
    CodeNode.SetAttribute('class',CurName);
  Page.CurNode.AppendChild(CodeNode);
  if CurValue<>'' then begin
    if (CurName='pascal') then begin
      p:=PChar(CurValue);
      AtomStart:=p;
      LastToken:=pNone;
      LastRangeStart:=p;
      repeat
        // skip space
        while p^ in [#1..#31,' '] do inc(p);
        // read token
        if (p^='{') or ((p^='/') and (p[1]='/')) or ((p^='(') and (p[1]='*'))
        then begin
          // comment
          AddSpan(pComment,p);
          p:=FindCommentEnd(p,false);
        end else begin
          ReadRawNextPascalAtom(p,AtomStart);
          if AtomStart^=#0 then break;
          case AtomStart^ of
          '''','#':
            AddSpan(pString,AtomStart);
          '0'..'9','%','$','&':
            AddSpan(pNumber,AtomStart);
          'a'..'z','A'..'Z','_':
            if WordIsKeyWord.DoIdentifier(AtomStart) then
              AddSpan(pKey,AtomStart)
            else
              AddSpan(pNone,AtomStart);
          else
            AddSpan(pSymbol,AtomStart);
          end;
        end;
      until false;
      Flush(p);
    end else begin
      // default: add as text
      CodeNode.AppendChild(doc.CreateTextNode(CurValue));
    end;
  end;
end;

procedure TWiki2XHTMLConverter.SavePage(Page: TW2XHTMLPage);
var
  Filename: String;
begin
  Filename:=PageToFilename(Page,true);
  DebugLn(['TWiki2HTMLConverter.SavePage ',Filename]);
  WriteXMLFile(Page.XHTML,Filename);
end;

procedure TWiki2XHTMLConverter.ConvertInit;
var
  i: Integer;
  Page: TW2XHTMLPage;
begin
  ShortFilenameToPage.Clear;
  UsedImages.Clear;
  for i:=0 to Count-1 do begin
    Page:=TW2XHTMLPage(Pages[i]);
    Page.Filename:=PageToFilename(Page,false);
    ShortFilenameToPage[Page.Filename]:=Page;
  end;
end;

procedure TWiki2XHTMLConverter.ConvertAllPages;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    ConvertPage(TW2XHTMLPage(Pages[i]));
end;

procedure TWiki2XHTMLConverter.SaveAllPages;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    SavePage(TW2XHTMLPage(Pages[i]));
end;

constructor TWiki2XHTMLConverter.Create;
begin
  inherited Create;
  FPageClass:=TW2XHTMLPage;
  FOutputDir:='xhtml';
  FMaxH:=MaxHeaderDepth;
  FPageFileExt:='.xhtml';
  ShortFilenameToPage:=TFilenameToPointerTree.Create(false);
  UsedImages:=TFilenameToPointerTree.Create(false);
end;

destructor TWiki2XHTMLConverter.Destroy;
begin
  inherited Destroy;
  FreeAndNil(ShortFilenameToPage);
  FreeAndNil(UsedImages);
end;

procedure TWiki2XHTMLConverter.Clear;
begin
  ShortFilenameToPage.Clear;
  UsedImages.Clear;
  inherited Clear;
end;

procedure TWiki2XHTMLConverter.Convert;
var
  Filename: String;
begin
  inherited Convert;
  if (CSSFilename<>'') and (OutputDir<>'') then begin
    Filename:=TrimAndExpandFilename(CSSFilename);
    if not FileExistsUTF8(Filename) then
      raise Exception.Create('css file not found: "'+Filename+'"');
  end;

  ConvertInit;
  ConvertAllPages;
  SaveAllPages;
end;

function TWiki2XHTMLConverter.GetRelativeCSSFilename: string;
begin
  Result:=CSSFilename;
  if (OutputDir='') or not FilenameIsAbsolute(OutputDir) then
    exit;
  Result:=TrimAndExpandFilename(Result);
  if Result='' then exit;
  Result:=CreateRelativePath(Result,OutputDir);
end;

function TWiki2XHTMLConverter.PageToFilename(Page: string; IsInternalLink,
  Full: boolean): string;
begin
  Result:=WikiPageToFilename(Page,IsInternalLink,true);
  if Result='' then exit;
  Result:=Result+PageFileExt;
  if Full then
    Result:=AppendPathDelim(OutputDir)+Result;
end;

function TWiki2XHTMLConverter.PageToFilename(Page: TW2XHTMLPage; Full: boolean
  ): string;
begin
  Result:=ChangeFileExt(ExtractFilename(Page.WikiFilename),PageFileExt);
  if Full then
    Result:=AppendPathDelim(OutputDir)+Result;
end;

{ TW2XHTMLPage }

destructor TW2XHTMLPage.Destroy;
begin
  FreeAndNil(XHTML);
  inherited Destroy;
end;

end.

