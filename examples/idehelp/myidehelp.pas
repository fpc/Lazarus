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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
{
ToDos:
Examples:
- help for messages: use codetools TFPCMsgFile
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
  Classes, SysUtils,
  // LCL
  LCLProc, Dialogs, ComCtrls, StdCtrls, ExtCtrls, LazHelpIntf, HelpIntfs,
  // LazUtils
  LazConfigStorage,
  // IdeIntf
  IDEDialogs, IDEHelpIntf, IDEOptionsIntf, IDEOptEditorIntf, BaseIDEIntf;

const
  MyHelpConfigFilename = 'demo_myidehelp.xml';
  MyHelpOptionID: integer = 10000; // an arbitrary number, choose a big number
                     // to append your options frame as last / below the others
var
  IDEHelpPkgName: string = 'DemoIDEHelp';

type

  { TMyHelpDatabase
    This is base class for all the demonstrated IDE help databases.
    In your help database you would probably combine all the features you
    want into a single class. }

  TMyHelpDatabase = class(THelpDatabase)
  private
    FEnabled: boolean;
    FModified: boolean;
    procedure SetEnabled(AValue: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure LoadFromConfig(Config: TConfigStorage); virtual; // called in Register
    procedure SaveToConfig(Config: TConfigStorage); virtual; // called by TMyHelpSetupDialog.WriteSettings
    property Enabled: boolean read FEnabled write SetEnabled; // switch to disable single example databases
    property Modified: boolean read FModified write FModified;
  end;

  { TMyFPCKeywordHelpDatabase
    Help for FPC keywords like 'procedure'.
    Actually FPC keywords are a special case. Any LCL TControl can set its
    HelpKeyword property and invoke keyword help.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyFPCKeywordHelpDatabase = class(TMyHelpDatabase)
  private
    FAllKeywordNode: THelpNode;
    FKeywordToText: TStrings;
    procedure SetKeywordToText(AValue: TStrings);
  public
    const
      DefaultKeyWordToText = 'procedure=Named code block';
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
    procedure LoadFromConfig(Config: TConfigStorage); override;
    procedure SaveToConfig(Config: TConfigStorage); override;
    property KeywordToText: TStrings read FKeywordToText write SetKeywordToText; // every line has the format: Keyword=Text
  end;
var
  MyFPCKeywordHelpDatabase: TMyFPCKeywordHelpDatabase;

type
  { TMyDirectiveHelpDatabase
    Help for FPC and Lazarus IDE directives like '$mode' and '%H'
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyDirectiveHelpDatabase = class(TMyHelpDatabase)
  private
    FAllDirectiveNode: THelpNode;
    FFPCDirectiveToText: TStrings;
    FIDEDirectiveToText: TStrings;
    procedure SetFPCDirectiveToText(AValue: TStrings);
    procedure SetIDEDirectiveToText(AValue: TStrings);
  public
    const
      DefaultFPCDirectiveToText = 'mode=Set the syntax, e.g. fpc, objfpc, delphi, macpas, tp';
      DefaultIDEDirectiveToText = 'H=Use {%H-} to hide any compiler message at that source position in IDE messages window.';
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForDirective(const HelpDirective: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
    procedure LoadFromConfig(Config: TConfigStorage); override;
    procedure SaveToConfig(Config: TConfigStorage); override;
    property FPCDirectiveToText: TStrings read FFPCDirectiveToText write SetFPCDirectiveToText; // every line has the format: Keyword=Text
    property IDEDirectiveToText: TStrings read FIDEDirectiveToText write SetIDEDirectiveToText; // every line has the format: Keyword=Text
  end;
var
  MyDirectiveHelpDatabase: TMyDirectiveHelpDatabase;

type
  { TMyMessagesHelpDatabase
    Help for messages, for example compiler messages like 'identifier not found'.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyMessagesHelpDatabase = class(TMyHelpDatabase)
  private
    FAllMessageNode: THelpNode;
  public
    constructor Create(TheOwner: TComponent); override;
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
    Help for HelpContext numbers. The IDE does not use this kind itself, because
    there is a risk that two packages have overlapping numbers.
    This help type is useful for your own applications, because a simple number
    is used to identify help and any LCL TControl can set its HelpContext
    property, so you can set this via the object inspector.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyContextHelpDatabase = class(TMyHelpDatabase)
  private
    FAllContextNode: THelpNode;
    FContextToMessage: TStrings;
    procedure SetContextToMessage(AValue: TStrings);
  public
    const
      DefaultContextToMessage = '3456=A help text for helpcontext 3456';
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodesForContext(HelpContext: THelpContext;
                         var ListOfNodes: THelpNodeQueryList; var ErrMsg: string
                         ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
    procedure LoadFromConfig(Config: TConfigStorage); override;
    procedure SaveToConfig(Config: TConfigStorage); override;
    property ContextToMessage: TStrings read FContextToMessage write SetContextToMessage;  // every line has the format: DecimalNumber=Text
  end;
var
  MyContextHelpDatabase: TMyContextHelpDatabase;

type
  { TMyClassesHelpDatabase
    Help for classes.
    At the moment (0.9.31) the IDE does not use this kind itself.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyClassesHelpDatabase = class(TMyHelpDatabase)
  private
    FAllClassNode: THelpNode;
  public
    function GetNodesForClass(AClass: TClass; var ListOfNodes: THelpNodeQueryList;
                              var ErrMsg: string): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;
var
  MyClassesHelpDatabase: TMyClassesHelpDatabase;

type
  { TMyPascalSourcesHelpDatabase
    Help for pascal sources.
    Notes: Do not forget to register this using HelpDatabases.CreateHelpDatabase!
           You can combine all your databases into one. }

  TMyPascalSourcesHelpDatabase = class(TMyHelpDatabase)
  private
    FAllSourceNode: THelpNode;
  public
    function GetNodesForClass(AClass: TClass; var ListOfNodes: THelpNodeQueryList;
                              var ErrMsg: string): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var {%H-}ErrMsg: string): TShowHelpResult; override;
  end;
var
  MyPascalSourcesHelpDatabase: TMyPascalSourcesHelpDatabase;

type
  { TMyHelpSetupDialog }

  TMyHelpSetupDialog = class(TAbstractIDEOptionsEditor)
    ClassesCheckBox: TCheckBox;
    ClassesNoteLabel: TLabel;
    ContextEnableCheckBox: TCheckBox;
    ContextMemo: TMemo;
    ContextNoteLabel: TLabel;
    DirectivesEnableCheckBox: TCheckBox;
    DirectivesSplitter: TSplitter;
    FPCDirectivesMemo: TMemo;
    DirectivesNoteLabel: TLabel;
    FPCDirectivesGroupBox: TGroupBox;
    FPCKeywordsEnableCheckBox: TCheckBox;
    FPCKeywordsMemo: TMemo;
    FPCKeywordsNoteLabel: TLabel;
    IDEDirectivesGroupBox: TGroupBox;
    IDEDirectivesMemo: TMemo;
    MessagesEnableCheckBox: TCheckBox;
    MessagesNoteLabel: TLabel;
    PageControl1: TPageControl;
    FPCKeywordsTabSheet: TTabSheet;
    DirectivesTabSheet: TTabSheet;
    MessagesTabSheet: TTabSheet;
    ContextTabSheet: TTabSheet;
    PascalSourcesEnabledCheckBox: TCheckBox;
    PascalSourcesNoteLabel: TLabel;
    PascalSourcesTabSheet: TTabSheet;
    ClassesTabSheet: TTabSheet;
  private
  public
    HelpShortCutAsText: string;
    function GetTitle: String; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
  end;

procedure LoadSaveMyIDEOptions(Filename: string; Load: boolean);
procedure TrimStrings(List: TStrings);
function AssignTrimmedStrings(Src, Dest: TStrings): boolean; // true if changed

procedure Register;

implementation

procedure LoadSaveMyIDEOptions(Filename: string; Load: boolean);
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,Load);
  try
    Config.AppendBasePath('FPCKeywords');
    if Load then
      MyFPCKeywordHelpDatabase.LoadFromConfig(Config)
    else
      MyFPCKeywordHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;

    Config.AppendBasePath('Directives');
    if Load then
      MyDirectiveHelpDatabase.LoadFromConfig(Config)
    else
      MyDirectiveHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;

    Config.AppendBasePath('Messages');
    if Load then
      MyMessagesHelpDatabase.LoadFromConfig(Config)
    else
      MyMessagesHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;

    Config.AppendBasePath('Contexts');
    if Load then
      MyContextHelpDatabase.LoadFromConfig(Config)
    else
      MyContextHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;

    Config.AppendBasePath('Classes');
    if Load then
      MyClassesHelpDatabase.LoadFromConfig(Config)
    else
      MyClassesHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;

    Config.AppendBasePath('PascalSources');
    if Load then
      MyPascalSourcesHelpDatabase.LoadFromConfig(Config)
    else
      MyPascalSourcesHelpDatabase.SaveToConfig(Config);
    Config.UndoAppendBasePath;
  finally
    Config.Free;
  end;
end;

procedure TrimStrings(List: TStrings);
var
  i: Integer;
  Line: String;
begin
  if List=nil then exit;
  for i:=List.Count-1 downto 0 do begin
    Line:=Trim(List[i]);
    if Line='' then
      List.Delete(i)
    else if Line<>List[i] then
      List[i]:=Line;
  end;
end;

function AssignTrimmedStrings(Src, Dest: TStrings): boolean;
var
  SrcIndex: Integer;
  DstIndex: Integer;
  Line: String;
begin
  Result:=false;
  SrcIndex:=0;
  DstIndex:=0;
  while SrcIndex<Src.Count-1 do begin
    Line:=Trim(Src[SrcIndex]);
    inc(SrcIndex);
    if Line<>'' then continue;
    if DstIndex=Dest.Count then begin
      Dest.Add(Line);
      Result:=true;
    end else if Dest[DstIndex]<>Line then begin
      Dest[DstIndex]:=Line;
      Result:=true;
    end;
    inc(DstIndex);
  end;
  while Dest.Count>DstIndex do begin
    Result:=true;
    Dest.Delete(Dest.Count-1);
  end;
end;

procedure Register;
begin
  // register help databases
  // For demonstration purpose there is one help database per help type.
  // Normally you would combine them into one database.
  MyFPCKeywordHelpDatabase:=TMyFPCKeywordHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyFPCKeyWordHelpDB',TMyFPCKeywordHelpDatabase,true));
  MyDirectiveHelpDatabase:=TMyDirectiveHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyDirectiveHelpDB',TMyDirectiveHelpDatabase,true));
  MyMessagesHelpDatabase:=TMyMessagesHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyMessagesHelpDB',TMyMessagesHelpDatabase,true));
  MyContextHelpDatabase:=TMyContextHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyContextHelpDB',TMyContextHelpDatabase,true));
  MyClassesHelpDatabase:=TMyClassesHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyClassHelpDB',TMyClassesHelpDatabase,true));
  MyPascalSourcesHelpDatabase:=TMyPascalSourcesHelpDatabase(
    HelpDatabases.CreateHelpDatabase('MyPascalSourcesHelpDB',TMyPascalSourcesHelpDatabase,true));

  // register frame in the IDE options to setup "My IDE help"
  MyHelpOptionID:=RegisterIDEOptionsEditor(GroupHelp,TMyHelpSetupDialog,MyHelpOptionID)^.Index;

  LoadSaveMyIDEOptions(MyHelpConfigFilename,true);
end;

{ TMyPascalSourcesHelpDatabase }

function TMyPascalSourcesHelpDatabase.GetNodesForClass(AClass: TClass;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  Title: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) or (not Enabled) then exit;

  if not AClass.InheritsFrom(TMyHelpDatabase) then exit;

  // this help database knows this context
  Title:='Help for source';
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllSourceNode=nil then
    FAllSourceNode:=THelpNode.CreateURL(Self,'','');
  FAllSourceNode.Title:=Title;
  CreateNodeQueryListAndAdd(FAllSourceNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyPascalSourcesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  Msg: String;
  PascalQuery: THelpQueryPascalContexts;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryClass) then exit;
  PascalQuery:=THelpQueryPascalContexts(Query);
  debugln(['TMyPascalSourcesHelpDatabase.ShowHelp PascalSource="',PascalQuery.ListOfPascalHelpContextList.Count,'"']);

  Msg:='This is a demonstration how to get help for a pascal identifier. See package '+IDEHelpPkgName;

  IDEMessageDialog('My pascal source help',
    'ToDo: implement me:'+LineEnding+LineEnding
    +Msg,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{ TMyClassesHelpDatabase }

function TMyClassesHelpDatabase.GetNodesForClass(AClass: TClass;
  var ListOfNodes: THelpNodeQueryList; var ErrMsg: string): TShowHelpResult;
var
  Title: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) or (not Enabled) then exit;

  if not AClass.InheritsFrom(TMyHelpDatabase) then exit;

  // this help database knows this context
  Title:='Help for class';
  // => add a node, so that if there are several possibilities the IDE can
  //    show the user a dialog to choose
  if FAllClassNode=nil then
    FAllClassNode:=THelpNode.CreateURL(Self,'','');
  FAllClassNode.Title:=Title;
  CreateNodeQueryListAndAdd(FAllClassNode,nil,ListOfNodes,true);
  Result:=shrSuccess;
end;

function TMyClassesHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  ClassQuery: THelpQueryClass;
  Msg: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryClass) then exit;
  ClassQuery:=THelpQueryClass(Query);
  debugln(['TMyClassHelpDatabase.ShowHelp Class="',dbgsname(ClassQuery.TheClass)]);

  Msg:='This is a demonstration how to get help for a class. See package '+IDEHelpPkgName;

  IDEMessageDialog('My class help',
    'The class "'+ClassQuery.TheClass.ClassName+'":'+LineEnding+LineEnding
    +Msg,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{ TMyHelpDatabase }

procedure TMyHelpDatabase.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  Modified:=true;
end;

constructor TMyHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Enabled:=true;
end;

procedure TMyHelpDatabase.LoadFromConfig(Config: TConfigStorage);
begin
  Enabled:=Config.GetValue('Enabled',true);
end;

procedure TMyHelpDatabase.SaveToConfig(Config: TConfigStorage);
begin
  Config.SetDeleteValue('Enabled',Enabled,true);
end;

{ TMyHelpSetupDialog }

function TMyHelpSetupDialog.GetTitle: String;
begin
  Result:='My IDE Help';
end;

procedure TMyHelpSetupDialog.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  // fpc keywords
  FPCKeywordsEnableCheckBox.Checked:=MyFPCKeywordHelpDatabase.Enabled;
  FPCKeywordsMemo.Text:=MyFPCKeywordHelpDatabase.KeywordToText.Text;

  // directives
  DirectivesEnableCheckBox.Checked:=MyDirectiveHelpDatabase.Enabled;
  FPCDirectivesMemo.Text:=MyDirectiveHelpDatabase.FPCDirectiveToText.Text;
  IDEDirectivesMemo.Text:=MyDirectiveHelpDatabase.IDEDirectiveToText.Text;

  // messages
  MessagesEnableCheckBox.Checked:=MyMessagesHelpDatabase.Enabled;

  // context
  ContextEnableCheckBox.Checked:=MyContextHelpDatabase.Enabled;
  ContextMemo.Text:=MyContextHelpDatabase.ContextToMessage.Text;

  // classes
  ClassesCheckBox.Checked:=MyClassesHelpDatabase.Enabled;

  // pascal sources
  PascalSourcesEnabledCheckBox.Checked:=MyPascalSourcesHelpDatabase.Enabled;
end;

procedure TMyHelpSetupDialog.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  HelpShortCutAsText:='F1';
  IDEHelpPkgName:='DemoIDEHelp';

  // fpc keywords
  FPCKeywordsTabSheet.Caption:='FPC keywords';
  FPCKeywordsNoteLabel.Caption:='Simple help for FPC keywords. Installed by package '+IDEHelpPkgName+'.'
  +' You get this help when you press the help key ('+HelpShortCutAsText+') in the source editor and caret is not on an identifier.'
  +' Each line has the format "keyword=text" without the quotes. For example "repeat=This keyword starts a repeat-until loop."';
  FPCKeywordsEnableCheckBox.Caption:='Enable';

  // directives
  DirectivesTabSheet.Caption:='Directives';
  DirectivesNoteLabel.Caption:='Simple help for FPC and IDE directives. Installed by package '+IDEHelpPkgName+'.'
  +' You get this help when you press the help key ('+HelpShortCutAsText+') in the source editor and caret is on a directive (e.g. {$mode objfpc} or {%H-}).'
  +' Each line has the format "directive=text" without the quotes. Here is an example for the fpc directives: "H=$H+ turns ansistring on, $H- turns shortstrings on.". Here is an example for an IDE directive: "H=%H- hides the compiler generated message at this source position."';
  DirectivesEnableCheckBox.Caption:='Enable';
  FPCDirectivesGroupBox.Caption:='FPC directives (e.g. mode=...)';
  IDEDirectivesGroupBox.Caption:='IDE directives (e.g. H=...)';

  // messages
  MessagesTabSheet.Caption:='Messages';
  MessagesNoteLabel.Caption:='Simple help for messages. Installed by package '+IDEHelpPkgName+'.'
  +' You get this help when you click on Help in the context menu of the messages window.'
  +' There is only an example for the message "unitname.pas(line,col) Error: User defined: test", which is created by a {$error test} in the source.';
  MessagesEnableCheckBox.Caption:='Enable';

  // context
  ContextTabSheet.Caption:='HelpContext';
  ContextNoteLabel.Caption:='Simple help for HelpContext numbers. Installed by package '+IDEHelpPkgName+'.'
  +' You get this help when you press the help key ('+HelpShortCutAsText+') on a control which has a value in its property "HelpContext".'
  +' Each line has the format "decimal_number=text" without the quotes. Here is an example for HelpContext=1: "1=Help for button foobar."';
  ContextEnableCheckBox.Caption:='Enable';

  // classes
  ClassesTabSheet.Caption:='Classes';
  ClassesNoteLabel.Caption:='Simple help for messages. Installed by package '+IDEHelpPkgName+'.'
  +' This help type is currently not used in the IDE.';
  ClassesCheckBox.Caption:='Enable';

  // pascal sources
  PascalSourcesTabSheet.Caption:='Pascal Sources';
  PascalSourcesNoteLabel.Caption:='Simple help for pascal sources. Installed by package '+IDEHelpPkgName+'.'
  +' You get this help when you press the help key ('+HelpShortCutAsText+') in the source editor and caret is on an identifier.'
  +' There is currently no example for this in this package.';
  PascalSourcesEnabledCheckBox.Caption:='Enable';
end;

class function TMyHelpSetupDialog.
  SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  // show whenever help options are shown
  Result:=TAbstractIDEHelpOptions;
end;

procedure TMyHelpSetupDialog.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  // fpc keywords
  MyFPCKeywordHelpDatabase.Enabled:=FPCKeywordsEnableCheckBox.Checked;
  MyFPCKeywordHelpDatabase.KeywordToText:=FPCKeywordsMemo.Lines;

  // directives
  MyDirectiveHelpDatabase.Enabled:=DirectivesEnableCheckBox.Checked;
  MyDirectiveHelpDatabase.FPCDirectiveToText:=FPCDirectivesMemo.Lines;

  // messages
  MyMessagesHelpDatabase.Enabled:=MessagesEnableCheckBox.Checked;

  // context
  MyContextHelpDatabase.Enabled:=ContextEnableCheckBox.Checked;
  MyContextHelpDatabase.ContextToMessage:=ContextMemo.Lines;

  // classes
  MyClassesHelpDatabase.Enabled:=ClassesCheckBox.Checked;

  // sources
  MyPascalSourcesHelpDatabase.Enabled:=PascalSourcesEnabledCheckBox.Checked;

  // save if modified
  if MyFPCKeywordHelpDatabase.Modified
  or MyDirectiveHelpDatabase.Modified
  or MyMessagesHelpDatabase.Modified
  or MyContextHelpDatabase.Modified
  or MyClassesHelpDatabase.Modified
  or MyPascalSourcesHelpDatabase.Modified
  then
    LoadSaveMyIDEOptions(MyHelpConfigFilename,false);
end;

{ TMyContextHelpDatabase }

procedure TMyContextHelpDatabase.SetContextToMessage(AValue: TStrings);
begin
  if AssignTrimmedStrings(AValue,FContextToMessage) then
    Modified:=true;
end;

constructor TMyContextHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FContextToMessage:=TStringList.Create;
  ContextToMessage.Text:=DefaultContextToMessage;
end;

destructor TMyContextHelpDatabase.Destroy;
begin
  FreeAndNil(FContextToMessage);
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
  if (csDesigning in ComponentState) or (not Enabled) then exit;

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
    'The context "'+IntToStr(Context.Context)+'":'+LineEnding+LineEnding
    +Msg,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

procedure TMyContextHelpDatabase.LoadFromConfig(Config: TConfigStorage);
begin
  inherited LoadFromConfig(Config);
  Config.GetValue('Contexts',ContextToMessage);
  if ContextToMessage.Count=0 then
    ContextToMessage.Text:=DefaultContextToMessage;
end;

procedure TMyContextHelpDatabase.SaveToConfig(Config: TConfigStorage);
begin
  inherited SaveToConfig(Config);
  Config.SetValue('Contexts',ContextToMessage);
end;

{ TMyMessagesHelpDatabase }

constructor TMyMessagesHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

function TMyMessagesHelpDatabase.GetNodesForMessage(const AMessage: string;
  MessageParts: TStrings; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  Title: String;
begin
  ErrMsg:='';
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) or (not Enabled) then exit;
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
    'The message "$'+Msg.WholeMessage+'":'+LineEnding+LineEnding
    +'Success. Message recognized by TMyMessagesHelpDatabase',mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

{ TMyDirectiveHelpDatabase }

procedure TMyDirectiveHelpDatabase.SetFPCDirectiveToText(AValue: TStrings);
begin
  if AssignTrimmedStrings(AValue,FFPCDirectiveToText) then
    Modified:=true;
end;

procedure TMyDirectiveHelpDatabase.SetIDEDirectiveToText(AValue: TStrings);
begin
  if AssignTrimmedStrings(AValue,FIDEDirectiveToText) then
    Modified:=true;
end;

constructor TMyDirectiveHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Enabled:=true;
  FFPCDirectiveToText:=TStringList.Create;
  FPCDirectiveToText.Text:=DefaultFPCDirectiveToText;
  FIDEDirectiveToText:=TStringList.Create;
  IDEDirectiveToText.Text:=DefaultIDEDirectiveToText;
end;

destructor TMyDirectiveHelpDatabase.Destroy;
begin
  FreeAndNil(FFPCDirectiveToText);
  FreeAndNil(FIDEDirectiveToText);
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
  if (csDesigning in ComponentState) or (not Enabled) then exit;
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
      'Free Pascal compiler directive "$'+Directive+'":'+LineEnding+LineEnding
      +Txt,mtInformation,[mbOk]);
  end else if (IDEDirectiveHelpPrefix<>'')
  and (LeftStr(Directive,length(IDEDirectiveHelpPrefix))=IDEDirectiveHelpPrefix)
  then begin
    // IDE directive
    Directive:=copy(Directive,length(IDEDirectiveHelpPrefix)+2,length(Directive));
    Txt:=IDEDirectiveToText.Values[lowercase(Directive)];
    IDEMessageDialog('My Lazarus IDE directive help',
      'Lazarus IDE directive "%'+Directive+'":'+LineEnding+LineEnding
      +Txt,mtInformation,[mbOk]);
  end;
  Result:=shrSuccess;
end;

procedure TMyDirectiveHelpDatabase.LoadFromConfig(Config: TConfigStorage);
begin
  inherited LoadFromConfig(Config);
  Config.GetValue('FPCDirectives',FPCDirectiveToText);
  if FPCDirectiveToText.Count=0 then
    FPCDirectiveToText.Text:=DefaultFPCDirectiveToText;
  Config.GetValue('IDEDirectives',IDEDirectiveToText);
  if IDEDirectiveToText.Count=0 then
    IDEDirectiveToText.Text:=DefaultIDEDirectiveToText;
end;

procedure TMyDirectiveHelpDatabase.SaveToConfig(Config: TConfigStorage);
begin
  inherited SaveToConfig(Config);
  Config.SetValue('FPCDirectives',FPCDirectiveToText);
  Config.SetValue('IDEDirectives',IDEDirectiveToText);
end;

{ TMyFPCKeywordHelpDatabase }

procedure TMyFPCKeywordHelpDatabase.SetKeywordToText(AValue: TStrings);
begin
  if AssignTrimmedStrings(AValue,FKeywordToText) then
    Modified:=true;
end;

constructor TMyFPCKeywordHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FKeywordToText:=TStringList.Create;
  KeywordToText.Text:=DefaultKeyWordToText;
end;

destructor TMyFPCKeywordHelpDatabase.Destroy;
begin
  FreeAndNil(FKeywordToText);
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
  if (csDesigning in ComponentState) or (not Enabled) then exit;
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
    'The keyword "'+KeyWord+'":'+LineEnding+LineEnding
    +Txt,mtInformation,[mbOk]);
  Result:=shrSuccess;
end;

procedure TMyFPCKeywordHelpDatabase.LoadFromConfig(Config: TConfigStorage);
begin
  inherited LoadFromConfig(Config);
  Config.GetValue('KeywordToText',KeywordToText);
  if KeywordToText.Count=0 then
    KeywordToText.Text:=DefaultKeyWordToText;
end;

procedure TMyFPCKeywordHelpDatabase.SaveToConfig(Config: TConfigStorage);
begin
  inherited SaveToConfig(Config);
  Config.SetValue('KeywordToText',KeywordToText);
end;

{$R *.lfm}

end.


