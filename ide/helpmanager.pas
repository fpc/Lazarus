{  $Id$  }
{
 /***************************************************************************
                            helpmanager.pas
                            ---------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit HelpManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, StdCtrls, Dialogs,
  CodeToolManager, CodeAtom, CodeCache, CustomCodeTool, CodeTree,
  PascalParserTool, FindDeclarationTool,
  PropEdits, ObjectInspector, FormEditingIntf, ProjectIntf,
  HelpIntf, HelpHTML, HelpFPDoc, MacroIntf, IDEWindowIntf, MsgIntf, LazIDEIntf,
  LazarusIDEStrConsts, TransferMacros, DialogProcs, IDEOptionDefs,
  EnvironmentOpts, AboutFrm, MsgView, Project, PackageDefs, MainBar,
  OutputFilter, HelpOptions, MainIntf, LazConf, ExtCtrls, LResources,
  Interfaces;

type
  { TIDEHelpDatabases }

  TIDEHelpDatabases = class(THelpDatabases)
  public
    function ShowHelpSelector(Query: THelpQuery; Nodes: THelpNodeQueryList;
                              var ErrMsg: string;
                              var Selection: THelpNodeQuery
                              ): TShowHelpResult; override;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); override;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; override;
    function ShowHelpForSourcePosition(Query: THelpQuerySourcePosition;
                                       var ErrMsg: string): TShowHelpResult; override;
  end;
  
  
  { THelpManager }

  THelpManager = class(TBaseHelpManager)
    // help menu of the IDE menu bar
    procedure mnuHelpAboutLazarusClicked(Sender: TObject);
    procedure mnuHelpConfigureHelpClicked(Sender: TObject);
    procedure mnuHelpOnlineHelpClicked(Sender: TObject);
  private
    FFCLHelpDBPath: THelpBasePathObject;
    FLCLHelpDBPath: THelpBasePathObject;
    FMainHelpDB: THelpDatabase;
    FMainHelpDBPath: THelpBasePathObject;
    FRTLHelpDB: THelpDatabase;
    FFCLHelpDB: THelpDatabase;
    FLCLHelpDB: THelpDatabase;
    FRTLHelpDBPath: THelpBasePathObject;
    procedure RegisterIDEHelpDatabases;
    procedure RegisterDefaultIDEHelpViewers;
    procedure FindDefaultBrowser(var DefaultBrowser, Params: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure LoadHelpOptions; override;
    procedure SaveHelpOptions; override;

    procedure ShowLazarusHelpStartPage;
    procedure ShowIDEHelpForContext(HelpContext: THelpContext);
    procedure ShowIDEHelpForKeyword(const Keyword: string);

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; override;
    procedure ShowHelpForMessage(Line: integer); override;
    procedure ShowHelpForObjectInspector(Sender: TObject); override;
  public
    property FCLHelpDB: THelpDatabase read FFCLHelpDB;
    property FCLHelpDBPath: THelpBasePathObject read FFCLHelpDBPath;
    property LCLHelpDB: THelpDatabase read FLCLHelpDB;
    property LCLHelpDBPath: THelpBasePathObject read FLCLHelpDBPath;
    property MainHelpDB: THelpDatabase read FMainHelpDB;
    property MainHelpDBPath: THelpBasePathObject read FMainHelpDBPath;
    property RTLHelpDB: THelpDatabase read FRTLHelpDB;
    property RTLHelpDBPath: THelpBasePathObject read FRTLHelpDBPath;
  end;

  { THelpSelectorDialog }
  THelpSelectorDialog = class(TForm)
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    NodesGroupBox: TGroupBox;
    NodesListBox: TListBox;
    procedure HelpSelectorDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
  private
    FNodes: THelpNodeQueryList;
    procedure SetNodes(const AValue: THelpNodeQueryList);
    procedure FillNodesListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    property Nodes: THelpNodeQueryList read FNodes write SetNodes;
  end;

  { Help Contexts for IDE help }
const
  lihcStartPage = 'StartPage';
  lihcRTLUnits = 'RTLUnits';
  lihcFCLUnits = 'FCLUnits';
  lihcLCLUnits = 'LCLUnits';
  
  lihBaseUrl = 'http://lazarus-ccr.sourceforge.net/docs/';

  lihRTLURL = lihBaseUrl+'rtl/';
  lihFCLURL = lihBaseUrl+'fcl/';
  lihLCLURL = lihBaseUrl+'lcl/';

var
  HelpBoss: TBaseHelpManager;
  
implementation

procedure THelpSelectorDialog.HelpSelectorDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure THelpSelectorDialog.SetNodes(const AValue: THelpNodeQueryList);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
  FillNodesListBox;
end;

procedure THelpSelectorDialog.FillNodesListBox;
var
  List: TStringList;
  i: Integer;
  NodeQuery: THelpNodeQuery;
begin
  List:=TStringList.Create;
  if (Nodes<>nil) then begin
    for i:=0 to Nodes.Count-1 do begin
      NodeQuery:=Nodes[i];
      List.Add(NodeQuery.AsString);
    end;
  end;
  NodesListBox.Items.Assign(List);
  List.Free;
end;

constructor THelpSelectorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,500,300);

  NodesGroupBox.Caption:=lisSelectAHelpItem;
  OkButton.Caption:=lisLazBuildOk;
  CancelButton.Caption:=dlgCancel;
end;

{ TIDEHelpDatabases }

function TIDEHelpDatabases.ShowHelpSelector(Query: THelpQuery;
  Nodes: THelpNodeQueryList;
  var ErrMsg: string;
  var Selection: THelpNodeQuery
  ): TShowHelpResult;
var
  Dialog: THelpSelectorDialog;
  i: LongInt;
begin
  Selection:=nil;
  Result:=shrNone;
  Dialog:=THelpSelectorDialog.Create(nil);
  try
    Dialog.Nodes:=Nodes;
    if Dialog.ShowModal=mrOk then begin
      i:=Dialog.NodesListBox.ItemIndex;
      if i>=0 then begin
        Selection:=Nodes[i];
        Result:=shrSuccess;
      end;
    end;
  finally
    Dialog.Free;
  end;
end;

procedure TIDEHelpDatabases.ShowError(ShowResult: TShowHelpResult;
  const ErrMsg: string);
var
  ErrorCaption: String;
begin
  case ShowResult of
  shrNone: ErrorCaption:=lisCodeTemplError;
  shrSuccess: exit;
  shrDatabaseNotFound: ErrorCaption:=lisHelpDatabaseNotFound;
  shrContextNotFound: ErrorCaption:=lisHelpContextNotFound;
  shrViewerNotFound: ErrorCaption:=lisHelpViewerNotFound;
  shrHelpNotFound: ErrorCaption:=lisHelpNotFound;
  shrViewerError: ErrorCaption:=lisHelpViewerError;
  shrSelectorError: ErrorCaption:=lisHelpSelectorError;
  else ErrorCaption:=lisUnknownErrorPleaseReportThisBug;
  end;
  MessageDlg(ErrorCaption,ErrMsg,mtError,[mbCancel],0);
end;

function TIDEHelpDatabases.GetBaseDirectoryForBasePathObject(
  BasePathObject: TObject): string;
begin
  Result:='';
  if (BasePathObject is THelpBasePathObject) then
    Result:=THelpBasePathObject(BasePathObject).BasePath
  else if (BasePathObject=HelpBoss) or (BasePathObject=MainIDEInterface) then
    Result:=EnvironmentOptions.LazarusDirectory
  else if BasePathObject is TProject then
    Result:=TProject(BasePathObject).ProjectDirectory
  else if BasePathObject is TLazPackage then
    Result:=TLazPackage(BasePathObject).Directory;
  if Result<>'' then
    IDEMacros.SubstituteMacros(Result);
end;

function TIDEHelpDatabases.ShowHelpForSourcePosition(
  Query: THelpQuerySourcePosition; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpBoss.ShowHelpForSourcePosition(Query.Filename,
                                             Query.SourcePosition,ErrMsg);
end;

{ THelpManager }

procedure THelpManager.mnuHelpAboutLazarusClicked(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure THelpManager.mnuHelpConfigureHelpClicked(Sender: TObject);
begin
  if ShowHelpOptionsDialog=mrOk then
    SaveHelpOptions;
end;

procedure THelpManager.mnuHelpOnlineHelpClicked(Sender: TObject);
begin
  ShowLazarusHelpStartPage;
end;

procedure THelpManager.RegisterIDEHelpDatabases;

  procedure CreateMainIDEHelpDB;
  var
    StartNode: THelpNode;
    HTMLHelp: THTMLHelpDatabase;
  begin
    FMainHelpDB:=HelpDatabases.CreateHelpDatabase(lihcStartPage,
                                                  THTMLHelpDatabase,true);
    HTMLHelp:=FMainHelpDB as THTMLHelpDatabase;
    FMainHelpDBPath:=THelpBasePathObject.Create('$(LazarusDir)');
    HTMLHelp.BasePathObject:=FMainHelpDBPath;

    // HTML nodes for the IDE
    StartNode:=THelpNode.CreateURLID(HTMLHelp,'Lazarus',
                                     'file://docs/index.html',lihcStartPage);
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,StartNode);
    HTMLHelp.RegisterItemWithNode(StartNode);
  end;
  
  procedure CreateRTLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FRTLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcRTLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FRTLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihRTLURL;
    FRTLHelpDBPath:=THelpBasePathObject.Create;
    HTMLHelp.BasePathObject:=FRTLHelpDBPath;

    // FPDoc nodes for units in the RTL
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'RTL - Free Pascal Run Time Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,'$(FPCSrcDir)/rtl',
                                            '*.pp;*.pas',true);
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateFCLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FFCLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcFCLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FFCLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihFCLURL;
    FFCLHelpDBPath:=THelpBasePathObject.Create;
    HTMLHelp.BasePathObject:=FFCLHelpDBPath;

    // FPDoc nodes for units in the FCL
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,'$(FPCSrcDir)/fcl',
                                            '*.pp;*.pas',true);
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateLCLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FLCLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcLCLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FLCLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihLCLURL;
    FLCLHelpDBPath:=THelpBasePathObject.Create;
    HTMLHelp.BasePathObject:=FLCLHelpDBPath;

    // FPDoc nodes for units in the LCL
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'LCL - Lazarus Component Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,'$(LazarusDir)/lcl',
                                            '*.pp;*.pas',false);
    HTMLHelp.RegisterItem(DirItem);
  end;

begin
  CreateMainIDEHelpDB;
  CreateRTLHelpDB;
  CreateFCLHelpDB;
  CreateLCLHelpDB;
end;

procedure THelpManager.RegisterDefaultIDEHelpViewers;
var
  HelpViewer: THTMLBrowserHelpViewer;
begin
  HelpViewer:= THTMLBrowserHelpViewer.Create;
  HelpViewer.OnFindDefaultBrowser := @FindDefaultBrowser;
  HelpViewers.RegisterViewer(HelpViewer);
end;

procedure THelpManager.FindDefaultBrowser(var DefaultBrowser, Params: string);
begin
  GetDefaultBrowser(DefaultBrowser, Params);
end;

constructor THelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpBoss:=Self;
  HelpOpts:=THelpOptions.Create;
  HelpOpts.SetDefaultFilename;
  HelpDatabases:=TIDEHelpDatabases.Create;
  HelpViewers:=THelpViewers.Create;
  RegisterIDEHelpDatabases;
  RegisterDefaultIDEHelpViewers;

  // register property editors for URL handling
  RegisterPropertyEditor(TypeInfo(AnsiString),
                       THTMLHelpDatabase,'BaseURL',TURLDirectoryPropertyEditor);
end;

destructor THelpManager.Destroy;
begin
  FreeThenNil(HelpDatabases);
  FreeThenNil(HelpViewers);
  FreeThenNil(HelpOpts);
  FreeThenNil(FMainHelpDBPath);
  FreeThenNil(FRTLHelpDBPath);
  FreeThenNil(FFCLHelpDBPath);
  FreeThenNil(FLCLHelpDBPath);
  HelpBoss:=nil;
  inherited Destroy;
end;

procedure THelpManager.ConnectMainBarEvents;
begin
  with MainIDEBar do begin
  itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
  itmHelpOnlineHelp.OnClick := @mnuHelpOnlineHelpClicked;
  itmHelpConfigureHelp.OnClick :=@mnuHelpConfigureHelpClicked;
  end;
end;

procedure THelpManager.LoadHelpOptions;
begin
  HelpOpts.Load;
end;

procedure THelpManager.SaveHelpOptions;
begin
  HelpOpts.Save;
end;

procedure THelpManager.ShowLazarusHelpStartPage;
begin
  ShowIDEHelpForKeyword(lihcStartPage);
end;

procedure THelpManager.ShowIDEHelpForContext(HelpContext: THelpContext);
begin
  ShowHelpOrErrorForContext(MainHelpDB.ID,HelpContext);
end;

procedure THelpManager.ShowIDEHelpForKeyword(const Keyword: string);
begin
  ShowHelpOrErrorForKeyword(MainHelpDB.ID,Keyword);
end;

function THelpManager.ShowHelpForSourcePosition(const Filename: string;
  const CodePos: TPoint; var ErrMsg: string): TShowHelpResult;
  
  function ConvertCodePosToPascalHelpContext(ACodePos: PCodeXYPosition
    ): TPascalHelpContextList;
    
    procedure AddContext(Descriptor: TPascalHelpContextType;
      const Context: string);
    var
      CurContext: TPascalHelpContext;
    begin
      CurContext.Descriptor:=Descriptor;
      CurContext.Context:=Context;
      Result.Add(CurContext);
      debugln('  AddContext Descriptor=',dbgs(ord(Descriptor)),' Context="',Context,'"');
    end;
    
    procedure AddContextsBackwards(Tool: TCodeTool;
      Node: TCodeTreeNode);
    begin
      if Node=nil then exit;
      AddContextsBackwards(Tool,Node.Parent);
      case Node.Desc of
      ctnUnit, ctnPackage, ctnProgram, ctnLibrary:
        AddContext(pihcSourceName,Tool.GetSourceName);
      ctnVarDefinition:
        AddContext(pihcVariable,Tool.ExtractDefinitionName(Node));
      ctnTypeDefinition:
        AddContext(pihcType,Tool.ExtractDefinitionName(Node));
      ctnConstDefinition:
        AddContext(pihcConst,Tool.ExtractDefinitionName(Node));
      ctnProperty:
        AddContext(pihcProperty,Tool.ExtractPropName(Node,false));
      ctnProcedure:
        AddContext(pihcProcedure,Tool.ExtractProcName(Node,
                                                      [phpWithoutClassName]));
      ctnProcedureHead:
        AddContext(pihcParameterList,'');
      end;
    end;
    
  var
    MainCodeBuffer: TCodeBuffer;
    Tool: TCustomCodeTool;
    CleanPos: integer;
    i: Integer;
    Node: TCodeTreeNode;
    IncludeChain: TFPList;
    ConversionResult: LongInt;
  begin
    Result:=nil;
    // find code buffer
    if ACodePos^.Code=nil then begin
      debugln('WARNING: ConvertCodePosToPascalHelpContext ACodePos.Code=nil');
      exit;
    end;
    Result:=TPascalHelpContextList.Create;
    // add filename and all filenames of the include chain
    IncludeChain:=nil;
    try
      CodeToolBoss.GetIncludeCodeChain(ACodePos^.Code,true,IncludeChain);
      if IncludeChain=nil then begin
        debugln('WARNING: ConvertCodePosToPascalHelpContext IncludeChain=nil');
        exit;
      end;
      for i:=0 to IncludeChain.Count-1 do
        AddContext(pihcFilename,TCodeBuffer(IncludeChain[i]).Filename);
      MainCodeBuffer:=TCodeBuffer(IncludeChain[0]);
    finally
      IncludeChain.Free;
    end;
    // find code tool
    Tool:=CodeToolBoss.FindCodeToolForSource(MainCodeBuffer);
    if not (Tool is TCodeTool) then begin
      debugln('WARNING: ConvertCodePosToPascalHelpContext not (Tool is TCodeTool) MainCodeBuffer=',MainCodeBuffer.Filename);
      exit;
    end;
    // convert cursor position to clean position
    ConversionResult:=Tool.CaretToCleanPos(ACodePos^,CleanPos);
    if ConversionResult<>0 then begin
      // position not in clean code, maybe a comment, maybe behind last line
      // => ignore
      exit;
    end;
    // find node
    Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
    if Node=nil then begin
      // position not in a scanned pascal node, maybe in between
      // => ignore
      exit;
    end;
    AddContextsBackwards(TCodeTool(Tool),Node);
  end;
  
var
  CodeBuffer: TCodeBuffer;
  i: Integer;
  CurCodePos: PCodeXYPosition;
  ListOfPCodeXYPosition: TFPList;
  PascalHelpContextLists: TList;
  NewList: TPascalHelpContextList;
begin
  debugln('THelpManager.ShowHelpForSourcePosition A Filename=',Filename,' ',dbgs(CodePos));
  Result:=shrHelpNotFound;
  ErrMsg:='No help found for "'+Filename+'"'
         +' at ('+IntToStr(CodePos.Y)+','+IntToStr(CodePos.X)+')';
  // commit editor changes
  if not CodeToolBoss.GatherExternalChanges then exit;
  // get code buffer for Filename
  if mrOk<>LoadCodeBuffer(CodeBuffer,FileName,[lbfCheckIfText]) then
    exit;
  ListOfPCodeXYPosition:=nil;
  PascalHelpContextLists:=nil;
  try
    // get all possible declarations of this identifier
    if CodeToolBoss.FindDeclarationAndOverload(CodeBuffer,CodePos.X,CodePos.Y,
      ListOfPCodeXYPosition) then
    begin
      debugln('THelpManager.ShowHelpForSourcePosition B Success ',dbgs(ListOfPCodeXYPosition.Count));
      // convert the source positions in pascal help context list
      if ListOfPCodeXYPosition=nil then exit;
      for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
        CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
        debugln('THelpManager.ShowHelpForSourcePosition C ',CurCodePos^.Code.Filename,' X=',dbgs(CurCodePos^.X),' Y=',dbgs(CurCodePos^.Y));
        NewList:=ConvertCodePosToPascalHelpContext(CurCodePos);
        if NewList<>nil then begin
          if PascalHelpContextLists=nil then
            PascalHelpContextLists:=TList.Create;
          PascalHelpContextLists.Add(NewList);
        end;
      end;
      if PascalHelpContextLists=nil then exit;

      // invoke help system
      debugln('THelpManager.ShowHelpForSourcePosition D PascalHelpContextLists.Count=',dbgs(PascalHelpContextLists.Count));
      Result:=ShowHelpForPascalContexts(Filename,CodePos,PascalHelpContextLists,ErrMsg);
    end else begin
      MainIDEInterface.DoJumpToCodeToolBossError;
    end;
  finally
    FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    if PascalHelpContextLists<>nil then begin
      for i:=0 to PascalHelpContextLists.Count-1 do
        TObject(PascalHelpContextLists[i]).Free;
      PascalHelpContextLists.Free;
    end;
  end;
end;

procedure THelpManager.ShowHelpForMessage(Line: integer);

  function ParseMessage(MsgItem: TIDEMessageLine): TStringList;
  begin
    Result:=TStringList.Create;
    Result.Values['Message']:=MsgItem.Msg;
    if MsgItem.Parts<>nil then
      Result.Assign(MsgItem.Parts);
  end;

var
  MsgItem: TIDEMessageLine;
  MessageParts: TStringList;
begin
  debugln('THelpManager.ShowHelpForMessage A Line=',dbgs(Line));
  if MessagesView=nil then exit;
  if Line<0 then
    Line:=MessagesView.SelectedMessageIndex;
  if (Line<0) or (Line>=MessagesView.VisibleItemCount) then exit;
  MsgItem:=MessagesView.VisibleItems[Line];
  if MsgItem=nil then exit;
  if MsgItem.Msg<>'' then begin
    MessageParts:=ParseMessage(MsgItem);
    ShowHelpOrErrorForMessageLine(MsgItem.Msg,MessageParts);
  end;
end;

procedure THelpManager.ShowHelpForObjectInspector(Sender: TObject);
var
  AnInspector: TObjectInspector;
  Row: TOIPropertyGridRow;
  LookupRoot: TPersistent;
  AFile: TLazProjectFile;
begin
  DebugLn('THelpManager.ShowHelpForObjectInspector ',dbgsName(Sender));
  if Sender=nil then Sender:=ObjectInspector1;
  if Sender is TObjectInspector then begin
    AnInspector:=TObjectInspector(Sender);
    Row:=AnInspector.GetActivePropertyRow;
    if Row=nil then begin
      // TODO: show help about object inspector
      DebugLn('THelpManager.ShowHelpForObjectInspector TODO: show help about object inspector');
    end else begin
      // find unit of LookupRoot
      if AnInspector.PropertyEditorHook=nil then exit;
      LookupRoot:=AnInspector.PropertyEditorHook.LookupRoot;
      if not (LookupRoot is TComponent) then exit;
      AFile:=LazarusIDE.GetProjectFileWithRootComponent(TComponent(LookupRoot));
      if AFile=nil then exit;
      
      
      if Row.Editor=nil then exit;
      if Row.Editor.GetPropInfo=nil then exit;
      
    end;
  end;
end;

initialization
  {$i helpmanager.lrs}

end.

