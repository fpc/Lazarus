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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.


<fpdoc>
  <package="wiki"/>
    <module name='wiki_page_name'>
      <!-- header 1-->
      <short>Page title</short>
      <description> text under page header</description>
      <!-- header 2-->
      <topic name="wiki_page_name_title_1">
        <short>Title header 1</short>
        <description> text under header1</description>
        <!-- header 3-->
        <topic name="wiki_page_name_subtitle_1">
          <short>Title header subtitle</short>
          <descr>text</descr>
        </topic>
      </topic>
    </module>
  </package>
</fpdoc>


  wptText,  // TWPTextToken
  wptAttribute, // e.g. class="code" TWPNameValueToken
  wptLineBreak, // <br> /br> <br/>
  wptBold,    // '''
  wptItalic,  // ''
  wptStrikeTagShort, // <s>
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
  wptSpecial, // double curly bracket: title, shortcut like Ctrl+Shift+1
  wptPre,  // space at line start
  wptP, // paragraph
  wptCenter, // <center>
  wptInternLink, // [[]]
  wptExternLink, // []
  wptHorizontalRow, // ----
  wptNumberedList, // #
  wptBulletList, // *
  wptDefinitionList, // : or ;
  wptListItem,
  wptTable, // wiki tag for table
  wptTableRow, // wiki tag for table row
  wptTableHeadCell, // wiki tag for table head cell
  wptTableCell, // wiki tag for table cell
  wptSection, // started/ended by =
  wptHeader, // =Text=

}
unit Wiki2FPDocConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, WikiParser, laz2_DOM, LazFileUtils, laz2_XMLRead,
  laz2_XMLWrite, LazLoggerBase, WikiFormat;

type

  { TW2FPDocPage }

  TW2FPDocPage = class(TW2FormatPage)
  private
    DescrNode: TDOMElement; // the fpdoc <descr> node
    CurNode: TDOMElement; // current fpdoc node
  public
    FPDoc: TXMLDocument;
    procedure ClearConversion; override;
  end;

  { TWiki2FPDocConverter }

  TWiki2FPDocConverter = class(TWiki2FormatConverter)
  protected
    FPackageName: string;
    FRootName: string;
    procedure ConvertPage(Page: TW2FPDocPage);
    procedure ConvertContent(Page: TW2FPDocPage; DescrNode: TDOMElement);
    procedure OnWikiToken(Token: TWPToken);
    procedure SavePage(Page: TW2FPDocPage);
    procedure SetPackageName(AValue: string);
    procedure SetRootName(AValue: string);
    procedure SaveProject;
  public
    constructor Create; override;
    procedure Convert; override;
    property RootName: string read FRootName write SetRootName;
    property PackageName: string read FPackageName write SetPackageName;
  end;

implementation

{ TW2FPDocPage }

procedure TW2FPDocPage.ClearConversion;
begin
  FreeAndNil(FPDoc);
end;

{ TWiki2FPDocConverter }

procedure TWiki2FPDocConverter.OnWikiToken(Token: TWPToken);
var
  Page: TW2FPDocPage;

  procedure NodeNotOpen;
  begin
    raise Exception.Create('TWiki2FPDocConverter.OnWikiToken can not close:'
      +' Token='+dbgs(Token.Token)+' '+DbgSName(Token)+' CurNode='+Page.CurNode.TagName);
  end;

var
  TextToken: TWPTextToken;
  Txt: String;
  Node: TDOMElement;
  LinkToken: TWPLinkToken;
  URL: String;
  Caption: String;
  NodeName: String;
  W: TWikiPage;
  doc: TXMLDocument;
begin
  Page:=TW2FPDocPage(Token.UserData);
  W:=Page.WikiPage;
  doc:=Page.FPDoc;
  //debugln(['TWiki2FPDocConverter.OnWikiToken Token=',dbgs(Token.Token),' ',dbgs(Token)]);
  case Token.Token of

  wptText:
    if Token is TWPTextToken then begin
      TextToken:=TWPTextToken(Token);
      Txt:=copy(W.Src,TextToken.StartPos,TextToken.EndPos-TextToken.StartPos);
      if Page.CurNode.FirstChild=nil then
        Txt:=TrimLeft(Txt);
      if Txt<>'' then begin
        //debugln(['TWiki2FPDocConverter.OnWikiToken Text="',Txt,'"']);
        Page.CurNode.AppendChild(doc.CreateTextNode(Txt));
      end;
      exit;
    end;

  wptLineBreak, wptHorizontalRow:
    begin
      // ToDo: find out if fpdoc supports hr
      // only append a br if there is something in front
      if Page.CurNode.FirstChild<>nil then
        Page.CurNode.AppendChild(doc.CreateElement('br'));
      exit;
    end;

  wptSection:
    begin
      // close descr
      if Page.CurNode.TagName='descr' then
        Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
      if Token.Range=wprOpen then begin
        Node:=doc.CreateElement('topic');
        Page.CurNode.AppendChild(Node);
        Page.CurNode:=Node;
        Node:=doc.CreateElement('descr');
        Page.CurNode.AppendChild(Node);
        Page.CurNode:=Node;
        exit;
      end else if Token.Range=wprClose then begin
        // close topic
        if Page.CurNode.TagName<>'topic' then
          NodeNotOpen;
        Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
        exit;
      end;
    end;

  wptHeader:
    if Token.Range=wprOpen then begin
      Node:=doc.CreateElement('short');
      if Page.CurNode.TagName='descr' then
        // insert <short> before <descr>
        Page.CurNode.ParentNode.InsertBefore(Node,Page.CurNode)
      else
        Page.CurNode.AppendChild(Node);
      Page.CurNode:=Node;
      exit;
    end else if Token.Range=wprClose then begin
      // close header
      if Page.CurNode.TagName<>'short' then
        NodeNotOpen;
      // continue in <descr>
      Node:=TDOMElement(Page.CurNode.ParentNode.FindNode('descr'));
      if Node<>nil then
        Page.CurNode:=Node
      else
        Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
      exit;
    end;

  wptBold, wptItalic, wptUnderlineTag, wptTT, wptPre,
  wptBulletList, wptNumberedList, wptListItem,
  wptTable, wptTableRow, wptTableCell:
    begin
      // simple range
      case Token.Token of
      wptBold: NodeName:='b';
      wptItalic: NodeName:='i';
      wptUnderlineTag: NodeName:='u';
      wptTT: NodeName:='var';
      wptCode: NodeName:='code';
      wptPre: NodeName:='pre';
      wptNumberedList: NodeName:='ol';
      wptBulletList: NodeName:='ul';
      wptListItem: NodeName:='li';
      wptTable: NodeName:='table';
      wptTableRow: NodeName:='tr';
      wptTableCell: NodeName:='td';
      else NodeName:='';
      end;
      if Token.Range=wprOpen then begin
        Node:=doc.CreateElement(NodeName);
        Page.CurNode.AppendChild(Node);
        Page.CurNode:=Node;
        exit;
      end else if Token.Range=wprClose then begin
        if Page.CurNode.TagName<>NodeName then
          NodeNotOpen;
        Page.CurNode:=Page.CurNode.ParentNode as TDOMElement;
        exit;
      end;
    end;

  wptInternLink, wptExternLink:
    if Token is TWPLinkToken then begin
      LinkToken:=TWPLinkToken(Token);
      URL:=LinkToken.Link;
      if URL<>'' then begin
        // ToDo: convert URL
        debugln(['TWiki2FPDocConverter.OnWikiToken ToDo: convert ',dbgs(Token.Token),' link "',URL,'"']);
        Caption:=copy(W.Src,LinkToken.CaptionStartPos,LinkToken.CaptionEndPos-LinkToken.CaptionStartPos);
        Node:=doc.CreateElement('link');
        Node.SetAttribute('id',URL);
        if Caption<>'' then
          Node.AppendChild(doc.CreateTextNode(Caption));
        Page.CurNode.AppendChild(Node);
      end;
      exit;
    end;

  end;
  debugln(['TWiki2FPDocConverter.OnWikiToken ToDo: Token=',dbgs(Token.Token),' Range=',dbgs(Token.Range),' Class=',Token.ClassName,' ',W.PosToStr(W.CurrentPos)]);
end;

{   IsValidIdent returns true if the first character of Ident is in:
    'A' to 'Z', 'a' to 'z' or '_' and the following characters are one of:
    'A' to 'Z', 'a' to 'z', '0'..'9', '_' or '-'  }
function IsValidNodeName(const Ident: string): boolean;
var
  p: PChar;
begin
  p:=PChar(Ident);
  if not (p^ in ['A'..'Z', 'a'..'z', '_', '-']) then exit(false);
  inc(p);
  while true do begin
    if p^ in ['A'..'Z', 'a'..'z', '0'..'9', '_', '-'] then
      inc(p)
    else if (p^=#0) and (p-PChar(Ident)=length(Ident)) then
      exit(true)
    else
      exit(false);
  end;
end;

procedure TWiki2FPDocConverter.SetRootName(AValue: string);
begin
  if (AValue='') or not IsValidNodeName(AValue) then
    raise Exception.Create('invalid root name "'+AValue+'"');
  if FRootName=AValue then Exit;
  FRootName:=AValue;
end;

procedure TWiki2FPDocConverter.SaveProject;
var
  sl: TStringList;
  Filename: String;
  i: Integer;
begin
  Filename:=AppendPathDelim(OutputDir)+PackageName+'.xml';
  sl:=TStringList.Create;
  try
    sl.Add('<?xml version="1.0" encoding="utf-8"?>');
    sl.Add('<docproject>');
    sl.Add('  <options>');
    sl.Add('    <option name="auto-index" value="1"/>');
    sl.Add('    <option name="auto-toc" value="1"/>');
    sl.Add('    <option name="make-searchable" value="1"/>');
    sl.Add('    <option name="css-file" value="fpdoc.css"/>');
    sl.Add('    <option name="charset" value="UTF8"/>');
    // chm
    //sl.Add('    <option name="format" value="chm"/>');
    //sl.Add('    <option name="chm-title" value="Lazarus IDE Help"/>');
    // html
    //sl.Add('    <option name="format" value="html"/>');

    sl.Add('  </options>');
    sl.Add('  <packages>');
    // chm
    //sl.Add('    <package name="'+PackageName+'" output="'+PackageName+'.chm"/>');
    // html
    sl.Add('    <package name="'+PackageName+'" output="."/>');
    sl.Add('    <units>');
    sl.Add('    </units>');
    sl.Add('    <descriptions>');
    for i:=0 to Count-1 do
      sl.Add('      <description file="'+TW2FPDocPage(Pages[i]).WikiFilename+'"/>');
    sl.Add('    </descriptions>');
    sl.Add('  </packages>');
    sl.Add('</docproject>');

    sl.SaveToFile(Filename);
    if not Quiet then
      debugln(['fpdoc project file: ',Filename]);
  finally
    sl.Free;
  end;
end;

procedure TWiki2FPDocConverter.ConvertPage(Page: TW2FPDocPage);
var
  doc: TXMLDocument;
  RootNode: TDOMElement;
  PackageNode: TDOMElement;
  ModuleNode: TDOMElement;
  ModuleName: String;
  ShortNode: TDOMElement;
  DescrNode: TDOMElement;
begin
  FreeAndNil(Page.FPDoc);
  Page.FPDoc:=TXMLDocument.Create;
  doc:=Page.FPDoc;
  // <wiki>
  RootNode:=doc.CreateElement(RootName);
  doc.AppendChild(RootNode);
  // <package name="wiki">
  PackageNode:=doc.CreateElement('package');
  RootNode.AppendChild(PackageNode);
  PackageNode.SetAttribute('name',PackageName);
  // <module name="wiki_page_name">
  ModuleNode:=doc.CreateElement('module');
  PackageNode.AppendChild(ModuleNode);
  ModuleName:=ExtractFileNameOnly(Page.WikiFilename);
  ModuleNode.SetAttribute('name',ModuleName);
  // <short>Page title</short>
  ShortNode:=doc.CreateElement('short');
  ModuleNode.AppendChild(ShortNode);
  ShortNode.AppendChild(doc.CreateTextNode(Page.WikiPage.Title));
  // <descr>Text</descr>
  DescrNode:=doc.CreateElement('descr');
  ModuleNode.AppendChild(DescrNode);

  ConvertContent(Page,DescrNode);
end;

procedure TWiki2FPDocConverter.ConvertContent(Page: TW2FPDocPage;
  DescrNode: TDOMElement);
begin
  try
    Page.DescrNode:=DescrNode;
    Page.CurNode:=DescrNode;
    Page.WikiPage.Parse(@OnWikiToken,Page);
  finally
    Page.DescrNode:=nil;
    Page.CurNode:=nil;
  end;
end;

procedure TWiki2FPDocConverter.SavePage(Page: TW2FPDocPage);
var
  Filename: String;
begin
  Filename:=AppendPathDelim(OutputDir)+ExtractFilename(Page.WikiFilename);
  DebugLn(['TWiki2FPDocConverter.SavePage ',Filename]);
  WriteXMLFile(Page.FPDoc,Filename);
end;

procedure TWiki2FPDocConverter.SetPackageName(AValue: string);
begin
  if not IsValidIdent(AValue) then
    raise Exception.Create('invalid package name "'+AValue+'"');
  if FPackageName=AValue then Exit;
  FPackageName:=AValue;
end;

constructor TWiki2FPDocConverter.Create;
begin
  inherited Create;
  FPageClass:=TW2FPDocPage;
  FOutputDir:='fpdocxml';
  FPackageName:='wiki';
  FRootName:='fpdoc-descriptions';
end;

procedure TWiki2FPDocConverter.Convert;
var
  i: Integer;
begin
  inherited Convert;
  // convert
  for i:=0 to Count-1 do
    ConvertPage(TW2FPDocPage(Pages[i]));
  // save
  for i:=0 to Count-1 do
    SavePage(TW2FPDocPage(Pages[i]));
  SaveProject;
end;

end.

