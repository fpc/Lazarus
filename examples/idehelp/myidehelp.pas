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
Examples:
- helpcontext
- predefined identifiers: cardinal, longint
- predefined procs: exit, break, continue, writeln
- help for sources
- HTML help - showing an URL
- IDE dialogs / wiki:
- custom dialogs / wiki:
- fpc compiler options:

Wiki:
- explain all on the wiki

Help for:
- Other messages: Linker errors, fpcres errors
- FPC keyword: context, for example 'var' can be a section or a parameter modifier

}
unit MyIDEHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  LazHelpIntf, HelpIntfs, IDEHelpIntf, IDEDialogs, IDEOptionsIntf;

const
  MyHelpOptionID: integer = 10000; // an arbitrary number, choose a big number
                     // to append your options frame as last / below the others
type
  { TMyFPCKeywordHelpDatabase
    Help for FPC keywords like 'procedure'.
    Actually FPC keywords are a special case. Any LCL TControl can set its
    HelpKeyword property and invoke keyword help.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyFPCKeywordHelpDatabase = class(THelpDatabase)
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
var
  MyFPCKeywordHelpDatabase: TMyFPCKeywordHelpDatabase;

type
  { TMyDirectiveHelpDatabase
    Help for FPC and Lazarus IDE directives like '$mode' and '%H'
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyDirectiveHelpDatabase = class(THelpDatabase)
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
var
  MyDirectiveHelpDatabase: TMyDirectiveHelpDatabase;

type
  { TMyMessagesHelpDatabase
    Help for messages, for example compiler messages like 'identifier not found'.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyMessagesHelpDatabase = class(THelpDatabase)
  private
    FAllMessageNode: THelpNode;
  public
    function GetNodesForMessage(const AMessage: string; MessageParts: TStrings;
                        var ListOfNodes: THelpNodeQueryList; var ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;
var
  MyMessagesHelpDatabase: TMyMessagesHelpDatabase;

type
  { TMyContextHelpDatabase
    Help HelpContext numbers. The IDE does not use this kind itself, because
    there is a risk that two packages have overlapping numbers.
    This help type is useful for your own applications, because a simple number
    is used to identify help and any LCL TControl can set its HelpContext
    property.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyContextHelpDatabase = class(THelpDatabase)
  private
    FAllContextNode: THelpNode;
  public
    ContextToMessage: TStrings;  // every line has the format: DecimalNumber=Text
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForContext(HelpContext: THelpContext;
                         var ListOfNodes: THelpNodeQueryList; var ErrMsg: string
                         ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;
var
  MyContextHelpDatabase: TMyContextHelpDatabase;

type
  { TMyHelpSetupDialog }

  TMyHelpSetupDialog = class(TAbstractIDEOptionsEditor)
  private
  public
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  // register help databases
  // For demonstration purpose one help database per help type
  // Normally you would combine them into one or a few.
  MyFPCKeywordHelpDatabase:=TMyFPCKeywordHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyFPCKeyWordHelpDB',TMyFPCKeywordHelpDatabase,true));
  MyDirectiveHelpDatabase:=TMyDirectiveHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyDirectiveHelpDB',TMyDirectiveHelpDatabase,true));
  MyMessagesHelpDatabase:=TMyMessagesHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyMessagesHelpDB',TMyMessagesHelpDatabase,true));
  MyContextHelpDatabase:=TMyContextHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyContextHelpDB',TMyContextHelpDatabase,true));

  // register frame in the IDE options to setup "My IDE help"
  MyHelpOptionID:=RegisterIDEOptionsEditor(GroupHelp,TMyHelpSetupDialog,MyHelpOptionID)^.Index;
end;

{ TMyHelpSetupDialog }

function TMyHelpSetupDialog.GetTitle: String;
begin
  Result:='My IDE Help';
end;

procedure TMyHelpSetupDialog.ReadSettings(AOptions: TAbstractIDEOptions);
begin

end;

procedure TMyHelpSetupDialog.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

class function TMyHelpSetupDialog.
  SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  // show whenever help options are shown
  Result:=TAbstractIDEHelpOptions;
end;

procedure TMyHelpSetupDialog.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

{ TMyContextHelpDatabase }

constructor TMyContextHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ContextToMessage:=TStringList.Create;
  ContextToMessage.Add('3456=A help text for helpcontext 3456');
end;

destructor TMyContextHelpDatabase.Destroy;
begin
  FreeAndNil(ContextToMessage);
  inherited Destroy;
end;

function TMyContextHelpDatabase.GetNodesForContext(HelpContext: THelpContext;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  Msg: String;
  Title: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;

  Msg:=ContextToMessage.Values[IntToStr(HelpContext)];
  if Msg='' then exit;

  // this help database knows this context
  Title:='Help for context';
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllContextNode=nil then
    FAllContextNode:=THelpNode.CreateURL(Self,'','');
  FAllContextNode.Title:=Title;
  CreateNodeQueryListAndAdd(FAllContextNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyContextHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  Context: THelpQueryContext;
  Msg: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryContext) then exit;
  Context:=THelpQueryContext(Query);
  debugln(['TMyContextHelpDatabase.ShowHelp Context="',Context.Context]);

  Msg:=ContextToMessage.Values[IntToStr(Context.Context)];

  IDEMessageDialog('My context help',
    'The context "'+IntToStr(Context.Context)+'":'#13#13
    +Msg,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{ TMyMessagesHelpDatabase }

function TMyMessagesHelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Title: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  debugln(['TMyMessagesHelpDatabase.GetNodesForDirective AMessage="',AMessage,'" Parts="',MessageParts.Text,'"']);

  // check if the message fits
  if MessageParts=nil then exit;
  if MessageParts.Values['Message']<>'User defined: Test' then exit;

  // this help database knows this message
  Title:='Help for message';
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllMessageNode=nil then
    FAllMessageNode:=THelpNode.CreateURL(Self,'','');
  FAllMessageNode.Title:=Title;
  CreateNodeQueryListAndAdd(FAllMessageNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyMessagesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  Msg: THelpQueryMessage;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryMessage) then exit;
  Msg:=THelpQueryMessage(Query);
  debugln(['TMyMessagesHelpDatabase.ShowHelp Msg="',Msg.WholeMessage,'" Parts="',Msg.MessageParts.Text,'"']);
  // check if the message fits
  if Msg.MessageParts=nil then exit;
  if Msg.MessageParts.Values['Message']<>'User defined: Test' then exit;

  IDEMessageDialog('My message help',
    'The message "$'+Msg.WholeMessage+'":'#13#13
    +'Success. Message recognized by TMyMessagesHelpDatabase',mtInformation,[mbOk]);
  Result:=shrSuccess;
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


