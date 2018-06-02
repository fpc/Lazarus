{ Converter for wiki pages to html pages

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
unit Wiki2HTMLConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Wiki2XHTMLConvert, WikiParser, LazLoggerBase, laz2_DOM,
  LazUTF8;

type

  { TW2HTMLPage
    for future extensions and descendants }

  TW2HTMLPage = class(TW2XHTMLPage)
  public

  end;

  { TWiki2HTMLConverter }

  TWiki2HTMLConverter = class(TWiki2XHTMLConverter)
  protected
    procedure SavePageToStream(Page: TW2XHTMLPage; aStream: TStream); virtual;
    procedure SavePage(Page: TW2XHTMLPage); override;
  public
    constructor Create; override;
  end;

implementation

{ TWiki2HTMLConverter }

procedure TWiki2HTMLConverter.SavePageToStream(Page: TW2XHTMLPage;
  aStream: TStream);
var
  LastNodeWasText: boolean;
  FirstLine: boolean;

  procedure w(s: string);
  begin
    if s='' then exit;
    aStream.Write(s[1],length(s));
  end;

  procedure Traverse(ParentNode: TDOMNode; PreserveSpace: boolean);
  var
    Node: TDOMNode;
    Element: TDOMElement;
    i: Integer;
    s: DOMString;
    TagName: DOMString;
    OldPreserveSpace: Boolean;
  begin
    for Node in ParentNode do begin
      if Node is TDOMText then begin
        s:=TDOMText(Node).Data;
        if UTF8Trim(s)='' then continue;
        LastNodeWasText:=true;
        w(s);
      end else if Node is TDOMElement then begin
        if not LastNodeWasText then begin
          if FirstLine then
            FirstLine:=false
          else
            w(LineEnding);
          w(Space(Node.GetLevel*2-2));
        end;
        Element:=TDOMElement(Node);
        w('<');
        w(Element.TagName);
        OldPreserveSpace:=PreserveSpace;
        if Element.TagName='pre' then
          PreserveSpace:=true;
        for i:=0 to Element.Attributes.Length-1 do begin
          w(' ');
          w(Element.Attributes[i].NodeName);
          w('="');
          w(StrToXMLValue(Element.Attributes[i].NodeValue));
          w('"');
        end;
        if Node.FirstChild <> nil then
          w('>')
        else
          w('/>');
        LastNodeWasText:=PreserveSpace;
        if (Node.FirstChild<>nil) then begin
          Traverse(Node,PreserveSpace);
          if not LastNodeWasText then begin
            w(LineEnding);
            w(Space(Node.GetLevel*2-2));
          end;
          w('</');
          w(Element.TagName);
          w('>');
          LastNodeWasText:=PreserveSpace;
        end else if Node is TDOMElement then begin
          TagName:=lowercase(TDOMElement(Node).TagName);
          if (TagName='td') then begin
            // tag needs at least a space
            w('&nbsp;</');
            w(Element.TagName);
            w('>');
          end;
        end;
        PreserveSpace:=OldPreserveSpace;
      end;
    end;
  end;

begin
  LastNodeWasText:=false;
  FirstLine:=true;
  Traverse(Page.XHTML,false);
end;

procedure TWiki2HTMLConverter.SavePage(Page: TW2XHTMLPage);
var
  ms: TMemoryStream;
  Filename: String;
begin
  if OutputDir='' then exit;
  if Page.XHTML=nil then exit;
  Filename:=PageToFilename(Page,true);
  Log('TWiki2HTMLConverter.SavePage '+Filename);
  ms:=TMemoryStream.Create;
  try
    SavePageToStream(Page,ms);
    ms.Position:=0;
    ms.SaveToFile(Filename);
  finally
    ms.Free;
  end;
end;

constructor TWiki2HTMLConverter.Create;
begin
  inherited Create;
  FPageClass:=TW2HTMLPage;
  FOutputDir:='html';
  PageFileExt:='.html';
end;

end.

