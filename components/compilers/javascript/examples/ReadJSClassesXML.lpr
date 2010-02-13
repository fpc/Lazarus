{ Author: Mattias Gaertner
  Reading a xml file with javascript identifiers

  bugs in extjs3.xml:
     method name: jsname="NativeWindow.getRootHtmlWindow"
     jsname="an element"
     jsname="mst have a center region"
     jsname="Introspector.extend"
     jsname="MultiCombo.Checkable"

}
program ReadJSClassesXML;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, JSClassXMLRead;

var
  t: TJavascriptIdentifierTree;
  Filename: String;
begin
  t:=TJavascriptIdentifierTree.Create;
  Filename:=ExpandFileName(ParamStr(1));
  t.LoadFromFile(Filename);
  t.Free;
end.

