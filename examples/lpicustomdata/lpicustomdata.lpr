{ Copyright (C) 2007 Mattias Gaertner

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    This example shows how to read the custom data of an .lpi file and
    how to change it. The .lpi file is given as first command line parameter.
  
    For example:
      ./lpicustomdata lpicustomdata.lpi

      This will show the old custom data, change a value and save it.
      If there was no custom data in the .lpi file, there will be no output.
      
    Important:
      The IDE does not read the .lpi file if it changed on disk.
      If you add custom data, make sure the IDE has not this project open.
    
}
program LPICustomData;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, AvgLvlTree, XMLCfg;

procedure LoadStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Cnt: LongInt;
  SubPath: String;
  CurName: String;
  CurValue: String;
  i: Integer;
begin
  Tree.Clear;
  Cnt:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to Cnt-1 do begin
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    CurName:=XMLConfig.GetValue(SubPath+'Name','');
    CurValue:=XMLConfig.GetValue(SubPath+'Value','');
    Tree.Values[CurName]:=CurValue;
  end;
end;

procedure SaveStringToStringTree(XMLConfig: TXMLConfig;
  Tree: TStringToStringTree; const Path: string);
var
  Node: TAvgLvlTreeNode;
  Item: PStringToStringItem;
  i: Integer;
  SubPath: String;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Tree.Tree.Count,0);
  Node:=Tree.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    SubPath:=Path+'Item'+IntToStr(i)+'/';
    XMLConfig.SetDeleteValue(SubPath+'Name',Item^.Name,'');
    XMLConfig.SetDeleteValue(SubPath+'Value',Item^.Value,'');
    Node:=Tree.Tree.FindSuccessor(Node);
    inc(i);
  end;
end;

var
  XMLConfig: TXMLConfig;
  LPIFilename: String;
  CustomData: TStringToStringTree;
  Name, Value: string;
begin
  LPIFilename:=ParamStr(1);
  
  // load .lpi file as TXMLConfig
  XMLConfig:=TXMLConfig.Create(nil);
  XMLConfig.Filename:=LPIFilename;
  
  // read custom data
  CustomData:=TStringToStringTree.Create(true);
  LoadStringToStringTree(XMLConfig,CustomData,'ProjectOptions/CustomData/');
  
  // show all custom data
  if CustomData.GetFirst(Name,Value) then begin
    repeat
      writeln(Name,'=',Value);
    until not CustomData.GetNext(Name,Name,Value);
  end;
  
  // change a value
  CustomData.Values['CustomData1']:='LPICustomData example value';
  
  // save custom data
  SaveStringToStringTree(XMLConfig,CustomData,'ProjectOptions/CustomData/');
  
  // save TXMLConfig
  XMLConfig.Flush;

  CustomData.Free;
  XMLConfig.Free;
end.

