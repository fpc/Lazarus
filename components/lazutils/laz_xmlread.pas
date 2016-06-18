{
 **********************************************************************
  This file is part of LazUtils.
  It is copied from Free Component Library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

}

unit Laz_XMLRead;

{$MODE objfpc}{$H+}
{$inline on}

interface

uses
  Classes, laz2_XMLRead, laz2_DOM;

type
  EXMLReadError = laz2_XMLRead.EXMLReadError;
const
  xrfOldXMLRead = [xrfAllowLowerThanInAttributeValue,xrfAllowSpecialCharsInAttributeValue];

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream; const AFilename: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const AFilename: String); overload;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream; const AFilename: String); overload;

implementation

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
begin
  laz2_XMLRead.ReadXMLFile(ADoc,AFilename,xrfOldXMLRead);
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: File);
begin
  laz2_XMLRead.ReadXMLFile(ADoc,f,xrfOldXMLRead);
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream);
begin
  laz2_XMLRead.ReadXMLFile(ADoc,f,xrfOldXMLRead);
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; f: TStream;
  const AFilename: String);
begin
  laz2_XMLRead.ReadXMLFile(ADoc,f,AFilename,xrfOldXMLRead);
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
begin
  laz2_XMLRead.ReadXMLFragment(AParentNode,AFilename,xrfOldXMLRead);
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File);
begin
  laz2_XMLRead.ReadXMLFragment(AParentNode,f,xrfOldXMLRead);
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  laz2_XMLRead.ReadXMLFragment(AParentNode,f,xrfOldXMLRead);
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream;
  const AFilename: String);
begin
  laz2_XMLRead.ReadXMLFragment(AParentNode,f,AFilename,xrfOldXMLRead);
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
begin
  laz2_XMLRead.ReadDTDFile(ADoc,AFilename);
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File);
begin
  laz2_XMLRead.ReadDTDFile(ADoc,f);
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream);
begin
  laz2_XMLRead.ReadDTDFile(ADoc,f);
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);
begin
  laz2_XMLRead.ReadDTDFile(ADoc,f,AFilename);
end;

end.
