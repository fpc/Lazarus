{ Demo package to show the various help types of the IDE.

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
unit MyIDEHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LazHelpHTML,
  LazHelpIntf, HelpIntfs, IDEHelpIntf, IDEDialogs;

type

  { TMyFPCKeywordHelpDatabase
    Help for FPC keywords like 'procedure'
    Note: Do not forget to register }

  TMyFPCKeywordHelpDatabase = class(THTMLHelpDatabase)
  private
    FAllKeywordNode: THelpNode;
  public
    KeywordToText: TStrings; // every line has the format: Keyword=Text
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;

  { TMyFPCDirectiveHelpDatabase
    Help for FPC directives like 'mode'
    Note: Do not forget to register }

  TMyFPCDirectiveHelpDatabase = class(THTMLHelpDatabase)
  private
    FAllDirectiveNode: THelpNode;
  public
    DirectiveToText: TStrings; // every line has the format: Keyword=Text
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForDirective(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;

  TMyHelpSetupDialog = class(TForm)
  private
  public
  end;

var
  MyHelpSetupDialog: TMyHelpSetupDialog;

procedure Register;

implementation

procedure Register;
begin
  HelpDatabases.CreateHelpDatabase('MyFPCKeyWordHelpDB',TMyFPCKeywordHelpDatabase,true);
  HelpDatabases.CreateHelpDatabase('MyFPCDirectiveHelpDB',TMyFPCDirectiveHelpDatabase,true);
end;

{ TMyFPCDirectiveHelpDatabase }

constructor TMyFPCDirectiveHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DirectiveToText:=TStringList.Create;
  DirectiveToText.Add('mode=Set the syntax, e.g. fpc, objfpc, delphi, macpas, tp');
end;

destructor TMyFPCDirectiveHelpDatabase.Destroy;
begin
  FreeAndNil(DirectiveToText);
  inherited Destroy;
end;

function TMyFPCDirectiveHelpDatabase.GetNodesForDirective(
  const HelpKeyword: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Directive: String;
  i: Integer;
begin
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  if (FPCDirectiveHelpPrefix='')
  or (LeftStr(HelpKeyword,length(FPCDirectiveHelpPrefix))<>FPCDirectiveHelpPrefix)
  then exit;
  // HelpKeyword is for example 'FPCDirective_$mode'
  Directive:=copy(HelpKeyword,length(FPCDirectiveHelpPrefix)+2,length(HelpKeyword));
  // directive is now 'mode'
  i:=DirectiveToText.IndexOfName(lowercase(Directive));
  if i<0 then exit;
  // this help database knows this Directive
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllDirectiveNode=nil then
    FAllDirectiveNode:=THelpNode.CreateURL(Self,'','');
  FAllDirectiveNode.Title:='Free Pascal Compiler directive '+Directive;
  CreateNodeQueryListAndAdd(FAllDirectiveNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyFPCDirectiveHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  DirectiveQuery: THelpQueryDirective;
  Directive: String;
  Txt: String;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryDirective) then exit;
  DirectiveQuery:=THelpQueryDirective(Query);
  Directive:=copy(DirectiveQuery.Directive,length(FPCDirectiveHelpPrefix)+2,length(DirectiveQuery.Directive));
  Txt:=DirectiveToText.Values[lowercase(Directive)];
  IDEMessageDialog('My fpc directive help',
    'The compiler directive "'+Directive+'":'#13#13
    +Txt,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{ TMyFPCKeywordHelpDatabase }

constructor TMyFPCKeywordHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  KeywordToText:=TStringList.Create;
  KeywordToText.Add('procedure=Named code block');
end;

destructor TMyFPCKeywordHelpDatabase.Destroy;
begin
  FreeAndNil(KeywordToText);
  inherited Destroy;
end;

function TMyFPCKeywordHelpDatabase.GetNodesForKeyword(
  const HelpKeyword: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  KeyWord: String;
  i: Integer;
begin
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  if (FPCKeyWordHelpPrefix='')
  or (LeftStr(HelpKeyword,length(FPCKeyWordHelpPrefix))<>FPCKeyWordHelpPrefix)
  then exit;
  // HelpKeyword is for example 'FPCKeyword_procedure'
  KeyWord:=copy(HelpKeyword,length(FPCKeyWordHelpPrefix)+1,length(HelpKeyword));
  // keyword is now 'procedure'
  i:=KeywordToText.IndexOfName(lowercase(KeyWord));
  if i<0 then exit;
  // this help database knows this keyword
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllKeywordNode=nil then
    FAllKeywordNode:=THelpNode.CreateURL(Self,'','');
  FAllKeywordNode.Title:='Pascal keyword '+KeyWord;
  CreateNodeQueryListAndAdd(FAllKeywordNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyFPCKeywordHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  KeywordQuery: THelpQueryKeyword;
  KeyWord: String;
  Txt: String;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryKeyword) then exit;
  KeywordQuery:=THelpQueryKeyword(Query);
  KeyWord:=copy(KeywordQuery.Keyword,length(FPCKeyWordHelpPrefix)+1,length(KeywordQuery.Keyword));
  Txt:=KeywordToText.Values[lowercase(KeyWord)];
  IDEMessageDialog('My fpc keyword help',
    'The keyword "'+KeyWord+'":'#13#13
    +Txt,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{$R *.lfm}

end.

