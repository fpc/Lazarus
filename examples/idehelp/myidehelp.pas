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
{
ToDos:
- FPC keyword: context
- FPC Messages, message, id
- Other messages: Linker errors, fpcres errors
- predefined identifiers: cardinal, longint
- predefined procs: exit, break, continue, writeln
- help for sources
- IDE dialogs / wiki:
- custom dialogs / wiki:
- fpc compiler options:

}
unit MyIDEHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  LazHelpHTML, LazHelpIntf, HelpIntfs, IDEHelpIntf, IDEDialogs;

type

  { TMyFPCKeywordHelpDatabase
    Help for FPC keywords like 'procedure'
    Notes: Do not forget to register!
           You can combine all databases into one. }

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

  { TMyDirectiveHelpDatabase
    Help for FPC and Lazarus IDE directives like '$mode' and '%H'
    Notes: Do not forget to register }

  TMyDirectiveHelpDatabase = class(THTMLHelpDatabase)
  private
    FAllDirectiveNode: THelpNode;
  public
    FPCDirectiveToText: TStrings; // every line has the format: Keyword=Text
    IDEDirectiveToText: TStrings; // every line has the format: Keyword=Text
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForDirective(const HelpDirective: string;
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
  HelpDatabases.CreateHelpDatabase('MyFPCDirectiveHelpDB',TMyDirectiveHelpDatabase,true);
end;

{ TMyDirectiveHelpDatabase }

constructor TMyDirectiveHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPCDirectiveToText:=TStringList.Create;
  FPCDirectiveToText.Add('mode=Set the syntax, e.g. fpc, objfpc, delphi, macpas, tp');
  IDEDirectiveToText:=TStringList.Create;
  IDEDirectiveToText.Add('H=Use {%H-} to hide any compiler message at that source position in IDE messages window.');
end;

destructor TMyDirectiveHelpDatabase.Destroy;
begin
  FreeAndNil(FPCDirectiveToText);
  FreeAndNil(IDEDirectiveToText);
  inherited Destroy;
end;

function TMyDirectiveHelpDatabase.GetNodesForDirective(
  const HelpDirective: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Directive: String;
  i: Integer;
  Title: String;
begin
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  debugln(['TMyDirectiveHelpDatabase.GetNodesForDirective HelpDirective="',HelpDirective,'"']);
  if (FPCDirectiveHelpPrefix<>'')
  and (LeftStr(HelpDirective,length(FPCDirectiveHelpPrefix))=FPCDirectiveHelpPrefix)
  then begin
    // HelpDirective is for example 'FPCDirective_$mode'
    Directive:=copy(HelpDirective,length(FPCDirectiveHelpPrefix)+2,length(HelpDirective));
    // directive is now 'mode'
    i:=FPCDirectiveToText.IndexOfName(lowercase(Directive));
    if i<0 then exit;
    Title:='Free Pascal Compiler directive $'+Directive;
  end else if (IDEDirectiveHelpPrefix<>'')
  and (LeftStr(HelpDirective,length(IDEDirectiveHelpPrefix))=IDEDirectiveHelpPrefix)
  then begin
    // IDE directive
    Directive:=copy(HelpDirective,length(IDEDirectiveHelpPrefix)+2,length(HelpDirective));
    // directive is now 'H'
    i:=IDEDirectiveToText.IndexOfName(lowercase(Directive));
    Title:='IDE directive %'+Directive;
  end;
  // this help database knows this Directive
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllDirectiveNode=nil then
    FAllDirectiveNode:=THelpNode.CreateURL(Self,'','');
  FAllDirectiveNode.Title:=Title;
  CreateNodeQueryListAndAdd(FAllDirectiveNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyDirectiveHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
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
  Directive:=DirectiveQuery.Directive;
  if (FPCDirectiveHelpPrefix<>'')
  and (LeftStr(Directive,length(FPCDirectiveHelpPrefix))=FPCDirectiveHelpPrefix)
  then begin
    // Directive is for example 'FPCDirective_$mode'
    Directive:=copy(Directive,length(FPCDirectiveHelpPrefix)+2,length(Directive));
    // directive is now 'mode'
    Txt:=FPCDirectiveToText.Values[lowercase(Directive)];
    IDEMessageDialog('My fpc directive help',
      'Free Pascal compiler directive "$'+Directive+'":'#13#13
      +Txt,mtInformation,[mbOk]);
  end else if (IDEDirectiveHelpPrefix<>'')
  and (LeftStr(Directive,length(IDEDirectiveHelpPrefix))=IDEDirectiveHelpPrefix)
  then begin
    // IDE directive
    Directive:=copy(Directive,length(IDEDirectiveHelpPrefix)+2,length(Directive));
    Txt:=IDEDirectiveToText.Values[lowercase(Directive)];
    IDEMessageDialog('My Lazarus IDE directive help',
      'Lazarus IDE directive "%'+Directive+'":'#13#13
      +Txt,mtInformation,[mbOk]);
  end;
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

