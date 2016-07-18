{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

}

unit Laz_XMLWrite;

{$MODE objfpc}{$H+}
{$inline on}

interface

uses Classes, laz2_XMLWrite, laz2_DOM;

const
  xwfOldXMLWrite = [xwfSpecialCharsInAttributeValue];

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text); overload;
procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream); overload;

procedure WriteXML(Element: TDOMNode; const AFileName: String); overload;
procedure WriteXML(Element: TDOMNode; var AFile: Text); overload;
procedure WriteXML(Element: TDOMNode; AStream: TStream); overload;

implementation

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
begin
  laz2_XMLWrite.WriteXMLFile(doc,AFileName,xwfOldXMLWrite);
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
begin
  laz2_XMLWrite.WriteXMLFile(doc,AFile,xwfOldXMLWrite);
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream);
begin
  laz2_XMLWrite.WriteXMLFile(doc,AStream,xwfOldXMLWrite);
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  laz2_XMLWrite.WriteXML(Element,AFileName,xwfOldXMLWrite);
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text);
begin
  laz2_XMLWrite.WriteXML(Element,AFile,xwfOldXMLWrite);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream);
begin
  laz2_XMLWrite.WriteXML(Element,AStream,xwfOldXMLWrite);
end;

end.
