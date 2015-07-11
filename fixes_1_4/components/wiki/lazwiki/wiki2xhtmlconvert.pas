{ Converter for wiki pages to xhtml pages

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
  TW2XHTMLStackItem = record
    Node: TDOMElement;
    Token: TWPTokenType;
    Txt: string;
  end;
  PW2XHTMLStackItem = ^TW2XHTMLStackItem;

  { TW2XHTMLPage }

  TW2XHTMLPage = class(TW2FormatPage)
  protected
    BodyDOMNode: TDOMElement;
    CurDOMNode: TDOMElement; // current xhtml node
    TOCNode: TDOMElement;
    CurTOCNode: TDOMElement;
    TOCNodeCount: integer;
    SectionLevel: integer;
    Stack: PW2XHTMLStackItem;
    StackPtr: integer;
    StackCapacity: integer;
    procedure Push(Node: TDOMElement; Token: TWPTokenType);
    procedure Pop;
  public
    XHTML: TXMLDocument;
    Filename: string;
    constructor Create(TheConverter: TWiki2FormatConverter); override;
    procedure ClearConversion; override;
  end;

  { TWiki2XHTMLConverter }

  TWiki2XHTMLConverter = class(TWiki2FormatConverter)
  private
    FAddLinksToTranslations: boolean;
    FAddTOCIfHeaderCountMoreThan: integer;
    FCSSFilename: string;
    FLinkToBaseDocument: string;
    FMaxH: integer;
    FPageFileExt: string;
    procedure DoAddLinksToTranslations(Page: TW2XHTMLPage);
    procedure DoAddLinkToBaseDocument(Page: TW2XHTMLPage);
    procedure OnHeaderToken(Token: TWPToken);
    procedure SetCSSFilename(AValue: string);
    procedure SetMaxH(AValue: integer);
    procedure SetPageFileExt(AValue: string);
  protected
    ShortFilenameToPage: TFilenameToPointerTree; // created in ConvertInit
    UsedImages: TFilenameToPointerTree; // image name to first page using the image
    procedure OnWikiToken(Token: TWPToken); virtual;
    procedure RaiseNodeNotOpen(Token: TWPToken);
    function GetImageLink(ImgFilename: string): string; virtual;
    function FindImage(const ImgFilename: string): string; virtual;
    procedure MarkImageAsUsed(const ImgFilename: string; Page: TW2XHTMLPage); virtual;
    function GetPageLink(Page: TW2XHTMLPage): string; virtual;
    function InsertLink(const LinkToken: TWPLinkToken): boolean;
    procedure InsertText(Token: TWPToken; Txt: string); virtual;
    procedure InsertCode(Token: TWPNameValueToken); virtual;
    procedure ConvertPage(Page: TW2XHTMLPage); virtual;
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
    function PageToFilename(Page: string; IsInternalLink, Full: boolean): string; virtual;
    function PageToFilename(Page: TW2XHTMLPage; Full: boolean): string; virtual;
    property PageFileExt: string read FPageFileExt write SetPageFileExt;
    property LinkToBaseDocument: string read FLinkToBaseDocument write FLinkToBaseDocument;
    property AddLinksToTranslations: boolean read FAddLinksToTranslations write FAddLinksToTranslations default true;
    property AddTOCIfHeaderCountMoreThan: integer read FAddTOCIfHeaderCountMoreThan
                 write FAddTOCIfHeaderCountMoreThan default 2;
  end;

implementation

{ TWiki2XHTMLConverter }

procedure TWiki2XHTMLConverter.SetCSSFilename(AValue: string);
begin
  if FCSSFilename=AValue then Exit;
  FCSSFilename:=AValue;
end;

procedure TWiki2XHTMLConverter.DoAddLinksToTranslations(Page: TW2XHTMLPage);
var
  TranslationPage: TW2XHTMLPage;
  Lang: String;
  S2PItem: PStringToPointerTreeItem;
  LangToPage: TStringToPointerTree;
  TranslationsNode: TDOMElement;
  LinkCaption: String;
  LinkNode: TDOMElement;
  Captions: TStringList;
  i: Integer;
  doc: TXMLDocument;
begin
  doc:=Page.XHTML;
  GetPageTranslations(Page.WikiDocumentName, LangToPage);
  //debugln(['TWiki2XHTMLConverter.DoAddLinksToTranslations ',Page.WikiDocumentName,' ',LangToPage.Count]);
  Captions:=TStringList.Create;
  try
    if (LangToPage=nil) or (LangToPage.Count<2) then exit;
    // add translations
    TranslationsNode:=doc.CreateElement('p');
    TranslationsNode.SetAttribute('class','translationLinks');
    Page.BodyDOMNode.AppendChild(TranslationsNode);
    // get all translations
    for S2PItem in LangToPage do begin
      Lang:=S2PItem^.Name;
      TranslationPage:=TW2XHTMLPage(S2PItem^.Value);
      LinkCaption:=WikiLangCodeToCaption(Lang);
      if Lang<>'' then LinkCaption+=' ('+Lang+')';
      Captions.AddObject(LinkCaption,TranslationPage);
    end;
    // sort them alphabetically
    Captions.CustomSort(@CompareStrListUTF8LowerCase);
    // add links
    for i:=0 to Captions.Count-1 do begin
      LinkCaption:=Captions[i];
      TranslationPage:=TW2XHTMLPage(Captions.Objects[i]);
      // add separator |
      if TranslationsNode.FirstChild<>nil then
        TranslationsNode.AppendChild(doc.CreateTextNode(' | '));
      if TranslationPage=Page then
        // add current page as normal text
        TranslationsNode.AppendChild(doc.CreateTextNode(LinkCaption))
      else begin
        // add link to other translations
        LinkNode:=doc.CreateElement('a');
        LinkNode.SetAttribute('href',TranslationPage.WikiDocumentName);
        TranslationsNode.AppendChild(LinkNode);
        LinkNode.AppendChild(doc.CreateTextNode(LinkCaption));
      end;
    end;
  finally
    LangToPage.Free;
    Captions.Free;
  end;
end;

procedure TWiki2XHTMLConverter.DoAddLinkToBaseDocument(Page: TW2XHTMLPage);
var
  Link: String;
  Node: TDOMElement;
  doc: TXMLDocument;
begin
  // add <a href="BaseURL+WikiDocumentName">LinkToBaseDocument</a><br>
  doc:=Page.XHTML;
  Node:=doc.CreateElement('a');
  Page.BodyDOMNode.AppendChild(Node);
  Link:=Page.WikiPage.BaseURL;
  if (Link<>'') and (Link[length(Link)]<>'/') then
    Link+='/';
  Link+=Page.WikiDocumentName;
  Node.SetAttribute('href', Link);
  Node.AppendChild(doc.CreateTextNode(LinkToBaseDocument));
  Node:=doc.CreateElement('br');
  Page.BodyDOMNode.AppendChild(Node);
end;

procedure TWiki2XHTMLConverter.OnHeaderToken(Token: TWPToken);
var
  LinkNode: TDOMElement;
  HeaderTxt: DOMString;
  Page: TW2XHTMLPage;
  doc: TXMLDocument;
  NodeName: String;
  NodeClass: String;
  Node: TDOMElement;
  HRef: String;
  LINode: TDOMElement;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  doc:=Page.XHTML;
  NodeClass:='';
  if Page.SectionLevel<=1 then
    NodeName:='h1'
  else if Page.SectionLevel<=MaxH then
    NodeName:='h'+IntToStr(Page.SectionLevel)
  else if Page.SectionLevel>MaxH then begin
    NodeName:='h'+IntToStr(MaxH);
    NodeClass:='subTitle';
  end;
  if Token.Range=wprOpen then begin
    // open header
    Node:=doc.CreateElement(NodeName);
    Page.CurDOMNode.AppendChild(Node);
    if NodeClass<>'' then
      Node.SetAttribute('class', NodeClass);
    Page.CurDOMNode:=Node;
    Page.Push(Node, wptHeader);
  end else if Token.Range=wprClose then begin
    // close header
    if Page.CurDOMNode.TagName<>NodeName then
      RaiseNodeNotOpen(Token);
    HeaderTxt:='';
    if Page.CurDOMNode.FirstChild is TDOMText then
      HeaderTxt:=TDOMText(Page.CurDOMNode.FirstChild).Data;
    if HeaderTxt<>'' then begin
      HRef:=WikiHeaderToLink(HeaderTxt);
      // add anchor
      LinkNode:=doc.CreateElement('a');
      LinkNode.SetAttribute('name', HRef);
      Page.CurDOMNode.ParentNode.InsertBefore(LinkNode, Page.CurDOMNode);
      // add TOC link
      LINode:=doc.CreateElement('li');
      LINode.SetAttribute('class', 'toclevel-'+IntToStr(Page.SectionLevel));
      Page.CurTOCNode.AppendChild(LINode);
      LinkNode:=doc.CreateElement('a');
      LinkNode.SetAttribute('href', '#'+HRef);
      LinkNode.AppendChild(doc.CreateTextNode(HeaderTxt));
      LINode.AppendChild(LinkNode);
      inc(Page.TOCNodeCount);
    end;
    Page.CurDOMNode:=Page.CurDOMNode.ParentNode as TDOMElement;
    Page.Pop;
  end;
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
    FoundImgFile: String;
    DocumentName: String;
    Anchor: String;
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
        URL:=copy(URL,p+1,length(URL));
        URL:=UTF8Trim(URL);
        URL:=WikiInternalLinkToPage(URL);
        if URL='' then exit;
        Filename:=WikiImageToFilename(URL,false,true,true);
        FoundImgFile:=FindImage(Filename);
        if FoundImgFile<>'' then begin
          Filename:=GetImageLink(FoundImgFile);
          MarkImageAsUsed(Filename,Page);
          Node:=doc.CreateElement('img');
          Node.SetAttribute('src', Filename);
          if Caption<>'' then
            Node.SetAttribute('alt', Caption);
          Page.CurDOMNode.AppendChild(Node);
          exit(true);
        end;
        if WarnURL(LinkToken.Link) then
          Log('WARNING: TWiki2XHTMLConverter.InsertLink "'+dbgstr(LinkToken.Link)+'": image file not found: "'+Filename+'" at '+W.PosToStr(LinkToken.LinkStartPos,true));
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
      p:=Pos('#',URL);
      if p<1 then begin
        DocumentName:=URL;
        Anchor:='';
      end else begin
        DocumentName:=LeftStr(URL,p-1);
        Anchor:=copy(URL,p+1,length(URL));
      end;
      Filename:=PageToFilename(DocumentName,true,false);
      TargetPage:=TW2XHTMLPage(ShortFilenameToPage[Filename]);
      if (TargetPage<>nil) then begin
        URL:=GetPageLink(TargetPage);
        if Anchor<>'' then
          URL+='#'+Anchor;
      end else if (not FileExistsUTF8(Filename)) then begin
        if WarnMissingPageLinks and WarnURL(LinkToken.Link) then
          Log('WARNING: TWiki2XHTMLConverter.InsertLink "'+dbgstr(LinkToken.Link)+'": file not found: "'+Filename+'" at '+W.PosToStr(LinkToken.LinkStartPos,true));
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
    Page.CurDOMNode.AppendChild(Node);
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
  Page.ClearConversion;
  if Page.WikiPage=nil then exit;
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
    // stylesheet <link href="wiki.css" type="text/css" rel="stylesheet">
    CSSNode:=doc.CreateElement('link');
    HeadNode.AppendChild(CSSNode);
    CSSNode.SetAttribute('href',CurCSSFilename);
    CSSNode.SetAttribute('type','text/css');
    CSSNode.SetAttribute('rel','stylesheet');
  end;
  // <body>
  Page.BodyDOMNode:=doc.CreateElement('body');
  RootNode.AppendChild(Page.BodyDOMNode);
  // title <h1 class="firstHeading">
  Node:=doc.CreateElement('h1');
  Page.BodyDOMNode.AppendChild(Node);
  Node.SetAttribute('class','firstHeading');
  Node.AppendChild(doc.CreateTextNode(Page.WikiPage.Title));

  // links to translations
  if AddLinksToTranslations then
    DoAddLinksToTranslations(Page);

  try
    Page.SectionLevel:=0;
    Page.TOCNode:=doc.CreateElement('ul');
    Page.TOCNodeCount:=0;
    Page.CurTOCNode:=Page.TOCNode;
    Page.BodyDOMNode.AppendChild(Page.TOCNode);

    Page.CurDOMNode:=Page.BodyDOMNode;
    Page.WikiPage.Parse(@OnWikiToken,Page);

    if LinkToBaseDocument<>'' then
      DoAddLinkToBaseDocument(Page);

    if Page.TOCNodeCount<=AddTOCIfHeaderCountMoreThan then
      Page.TOCNode.Free;
  finally
    Page.BodyDOMNode:=nil;
    Page.CurDOMNode:=nil;
    Page.TOCNode:=nil;
    Page.CurTOCNode:=nil;
  end;
end;

procedure TWiki2XHTMLConverter.OnWikiToken(Token: TWPToken);
var
  Page: TW2XHTMLPage;
  W: TWikiPage;

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
  //Log(['TWiki2XHTMLConverter.OnWikiToken Token='+dbgs(Token.Token)+' '+dbgs(Token));
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
      if Page.CurDOMNode.FirstChild<>nil then
        Page.CurDOMNode.AppendChild(doc.CreateElement('br'));
      exit;
    end;

  wptHorizontalRow:
    begin
      // only append a hr if there is something in front
      if Page.CurDOMNode.FirstChild<>nil then
        Page.CurDOMNode.AppendChild(doc.CreateElement('hr'));
      exit;
    end;

  wptP, wptBold, wptItalic, wptStrikeTagShort, wptUnderlineTag, wptTT,
  wptSup, wptSub, wptSmall, wptEm, wptSpan, wptString, wptVar, wptKey,
  wptPre, wptCenter,
  wptBulletList, wptNumberedList, wptDefinitionList,
  wptTable, wptTableRow, wptTableCell, wptTableHeadCell:
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
      end;
      if NodeName='' then
        MissingNodeName;

      if Token.Range=wprOpen then begin
        Node:=doc.CreateElement(NodeName);
        Page.CurDOMNode.AppendChild(Node);
        if NodeClass<>'' then
          Node.SetAttribute('class',NodeClass);
        Page.CurDOMNode:=Node;
        exit;
      end else if Token.Range=wprClose then begin
        if Page.CurDOMNode.TagName<>NodeName then
          RaiseNodeNotOpen(Token);
        Page.CurDOMNode:=Page.CurDOMNode.ParentNode as TDOMElement;
        exit;
      end;
    end;

  wptSection:
    begin
      NodeName:='div';
      if Token.Range=wprOpen then begin
        inc(Page.SectionLevel);
        // start div
        Node:=doc.CreateElement(NodeName);
        Page.CurDOMNode.AppendChild(Node);
        Node.SetAttribute('class','section');
        Page.CurDOMNode:=Node;
        // start TOC list
        Node:=doc.CreateElement('ul');
        Page.CurTOCNode.AppendChild(Node);
        Page.CurTOCNode:=Node;

        Page.Push(Node,Token.Token);
        exit;
      end else if Token.Range=wprClose then begin
        dec(Page.SectionLevel);
        // end div
        if Page.CurDOMNode.TagName<>NodeName then
          RaiseNodeNotOpen(Token);
        Page.CurDOMNode:=Page.CurDOMNode.ParentNode as TDOMElement;
        // end TOC list
        Node:=Page.CurTOCNode;
        Page.CurTOCNode:=Node.ParentNode as TDOMElement;
        if Node.FirstChild=nil then
          Node.Free;
        Page.Pop;
        exit;
      end;
    end;

  wptHeader:
    begin
      OnHeaderToken(Token);
      exit;
    end;

  wptListItem:
    if Token.Range=wprOpen then begin
      if Page.CurDOMNode.TagName='dl' then
        NodeName:='dd'
      else
        NodeName:='li';
      Node:=doc.CreateElement(NodeName);
      Page.CurDOMNode.AppendChild(Node);
      Page.CurDOMNode:=Node;
      exit;
    end else if Token.Range=wprClose then begin
      if (Page.CurDOMNode.TagName<>'dd')
      and (Page.CurDOMNode.TagName<>'li') then
        RaiseNodeNotOpen(Token);
      Page.CurDOMNode:=Page.CurDOMNode.ParentNode as TDOMElement;
      exit;
    end;

  wptAttribute:
    if Token is TWPNameValueToken then begin
      NameValueToken:=TWPNameValueToken(Token);
      CurName:=copy(W.Src,NameValueToken.NameStartPos,NameValueToken.NameEndPos-NameValueToken.NameStartPos);
      CurValue:=copy(W.Src,NameValueToken.ValueStartPos,NameValueToken.ValueEndPos-NameValueToken.ValueStartPos);
      Page.CurDOMNode.SetAttribute(CurName,CurValue);
      exit;
    end;

  wptSpecial:
    if Token is TWPNameValueToken then begin
      NameValueToken:=TWPNameValueToken(Token);
      CurName:=copy(W.Src,NameValueToken.NameStartPos,NameValueToken.NameEndPos-NameValueToken.NameStartPos);
      if CurName='' then begin
        // special without class = alternative title
        // the title is already written
        exit;
      end;
      CurValue:=copy(W.Src,NameValueToken.ValueStartPos,NameValueToken.ValueEndPos-NameValueToken.ValueStartPos);
      Node:=doc.CreateElement('span');
      if CurName<>'' then
        Node.SetAttribute('class',CurName);
      Page.CurDOMNode.AppendChild(Node);
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

  Log('TWiki2XHTMLConverter.OnWikiToken ToDo: Token='+dbgs(Token.Token)+' Range='+dbgs(Token.Range)+' Class='+Token.ClassName+' '+W.PosToStr(W.CurrentPos));
end;

procedure TWiki2XHTMLConverter.RaiseNodeNotOpen(Token: TWPToken);
var
  Page: TW2XHTMLPage;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  raise Exception.Create('TWiki2XHTMLConverter.OnWikiToken can not close:'
    +' Token='+dbgs(Token.Token)+' '+DbgSName(Token)+' CurNode='+Page.CurDOMNode.TagName);
end;

function TWiki2XHTMLConverter.GetImageLink(ImgFilename: string): string;
begin
  Result:=CreateRelativePath(ImgFilename,OutputDir);
end;

function TWiki2XHTMLConverter.FindImage(const ImgFilename: string): string;
begin
  if ImagesDir='' then exit('');
  Result:=ImgFilename;
  if (not FilenameIsAbsolute(Result)) then
    Result:=ImagesDir+Result;
  if not FileExistsUTF8(Result) then
    Result:='';
end;

procedure TWiki2XHTMLConverter.MarkImageAsUsed(const ImgFilename: string;
  Page: TW2XHTMLPage);
begin
  if not UsedImages.Contains(ImgFilename) then
    UsedImages[ImgFilename]:=Page;
end;

function TWiki2XHTMLConverter.GetPageLink(Page: TW2XHTMLPage): string;
begin
  Result:=PageToFilename(Page,false);
end;

procedure TWiki2XHTMLConverter.InsertText(Token: TWPToken; Txt: string);
var
  Page: TW2XHTMLPage;
  doc: TXMLDocument;
  CurNode: TDOMElement;
begin
  Page:=TW2XHTMLPage(Token.UserData);
  doc:=Page.XHTML;
  //Log(['TWiki2XHTMLConverter.InsertText Txt="'+dbgstr(Txt)+'"']);
  if Txt='' then exit;
  CurNode:=Page.CurDOMNode;
  if CurNode.TagName<>'pre' then begin
    if UTF8Trim(Txt)='' then begin
      // skip empty text
      exit;
    end;
    if CurNode.FirstChild=nil then
      Txt:=UTF8Trim(Txt,[u8tKeepEnd]);
  end;
  //Log('TWiki2XHTMLConverter.InsertText Node="'+Page.CurDOMNode.TagName+'" Text="'+Txt+'"');
  CurNode.AppendChild(doc.CreateTextNode(Txt));
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
  or (CurName='syntaxhighlight')
  or (CurName='source')
  or (CurName='fpc')
  then
    CurName:='pascal';
  if CurName<>'' then
    CodeNode.SetAttribute('class',CurName);
  Page.CurDOMNode.AppendChild(CodeNode);
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
  if OutputDir='' then exit;
  if Page.XHTML=nil then exit;
  Filename:=PageToFilename(Page,true);
  Log('TWiki2HTMLConverter.SavePage '+Filename);
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
    if Page.WikiPage<>nil then
      Page.WikiPage.LanguageTags:=CodeTags;
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
  fLinkToBaseDocument:='Online version';
  FAddLinksToTranslations:=true;
  FAddTOCIfHeaderCountMoreThan:=2;
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

procedure TW2XHTMLPage.Push(Node: TDOMElement; Token: TWPTokenType);
var
  s: PW2XHTMLStackItem;
  OldCapacity: Integer;
begin
  inc(StackPtr);
  if StackPtr=StackCapacity then begin
    OldCapacity:=StackCapacity;
    StackCapacity:=StackCapacity*2+8;
    ReAllocMem(Stack,SizeOf(TW2XHTMLStackItem)*StackCapacity);
    FillByte(Stack[StackPtr],SizeOf(TW2XHTMLStackItem)*(StackCapacity-OldCapacity),0);
  end;
  s:=@Stack[StackPtr];
  s^.Node:=Node;
  s^.Token:=Token;
end;

procedure TW2XHTMLPage.Pop;
begin
  if StackPtr<0 then
    raise Exception.Create('bug'); // push and pop are not balanced
  dec(StackPtr);
end;

constructor TW2XHTMLPage.Create(TheConverter: TWiki2FormatConverter);
begin
  inherited Create(TheConverter);
  StackPtr:=-1;
end;

procedure TW2XHTMLPage.ClearConversion;
begin
  BodyDOMNode:=nil;
  CurDOMNode:=nil;
  SectionLevel:=0;
  ReAllocMem(Stack,0);
  StackPtr:=-1;
  StackCapacity:=0;
  FreeAndNil(XHTML);
end;

end.

