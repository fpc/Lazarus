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
  HelpIntf, HelpHTML, DialogProcs,
  IDEOptionDefs, EnvironmentOpts, AboutFrm, Project, PackageDefs, MainBar,
  HelpOptions, MainIntf;

type
  { TBaseHelpManager }

  TBaseHelpManager = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure ConnectMainBarEvents; virtual;
    procedure LoadHelpOptions; virtual; abstract;
    procedure SaveHelpOptions; virtual; abstract;

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; virtual; abstract;
  end;


  { TIDEHelpDatabases }

  TIDEHelpDatabases = class(THelpDatabases)
  public
    function ShowHelpSelector(Nodes: TList; var ErrMsg: string;
                              var Selection: THelpNode): TShowHelpResult; override;
    procedure ShowError(ShowResult: TShowHelpResult; const ErrMsg: string); override;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; override;
    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; override;
  end;
  
  
  { THelpManager }

  THelpManager = class(TBaseHelpManager)
    // help menu of the IDE menu bar
    procedure mnuHelpAboutLazarusClicked(Sender: TObject);
    procedure mnuHelpConfigureHelpClicked(Sender: TObject);
    procedure mnuHelpOnlineHelpClicked(Sender: TObject);
  private
    FMainHelpDB: THelpDatabase;
    procedure RegisterIDEHelpDatabases;
    procedure RegisterDefaultIDEHelpViewers;
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
  public
    property MainHelpDB: THelpDatabase read FMainHelpDB;
  end;

  { Help Contexts for IDE help }
const
  lihcStartPage = 'StartPage';
  
var
  HelpBoss: TBaseHelpManager;

implementation

{ TBaseHelpManager }

constructor TBaseHelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpBoss:=Self;
end;

destructor TBaseHelpManager.Destroy;
begin
  HelpBoss:=nil;
  inherited Destroy;
end;

procedure TBaseHelpManager.ConnectMainBarEvents;
begin

end;

{ THelpSelectorDialog }

type
  THelpSelectorDialog = class(TForm)
    NodesGroupBox: TGroupBox;
    NodesListBox: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    procedure HelpSelectorDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
  private
    FNodes: TList;
    procedure SetNodes(const AValue: TList);
    procedure FillNodesListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    property Nodes: TList read FNodes write SetNodes;
  end;

procedure THelpSelectorDialog.HelpSelectorDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure THelpSelectorDialog.SetNodes(const AValue: TList);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
  FillNodesListBox;
end;

procedure THelpSelectorDialog.FillNodesListBox;
var
  List: TStringList;
  Node: THelpNode;
  i: Integer;
begin
  List:=TStringList.Create;
  if (Nodes<>nil) then begin
    for i:=0 to Nodes.Count-1 do begin
      Node:=TObject(Nodes[i]) as THelpNode;
      List.Add(Node.Title);
    end;
  end;
  NodesListBox.Items.Assign(List);
  List.Free;
end;

constructor THelpSelectorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  OnClose:=@HelpSelectorDialogClose;

  NodesGroupBox:=TGroupBox.Create(Self);
  with NodesGroupBox do begin
    Name:='NodesGroupBox';
    Parent:=Self;
    Left:=5;
    Top:=5;
    Width:=Self.ClientWidth-10;
    Height:=Self.ClientWidth-40;
    Anchors:=[akLeft,akTop,akRight,akBottom];
    Caption:='Select a help item:';
  end;
  
  NodesListBox:=TListBox.Create(Self);
  with NodesListBox do begin
    Name:='NodesListBox';
    Parent:=NodesGroupBox;
    Align:=alClient;
  end;
  
  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Left:=5;
    Top:=Self.ClientHeight-35;
    Width:=80;
    Anchors:=[akLeft,akBottom];
    Caption:='Ok';
    ModalResult:=mrOk;
  end;
  
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Left:=OkButton.Left+OkButton.Width+10;
    Top:=Self.ClientHeight-35;
    Width:=80;
    Anchors:=[akLeft,akBottom];
    Caption:='Cancel';
    ModalResult:=mrCancel;
  end;
end;

{ TIDEHelpDatabases }

function TIDEHelpDatabases.ShowHelpSelector(Nodes: TList; var ErrMsg: string;
  var Selection: THelpNode): TShowHelpResult;
var
  Dialog: THelpSelectorDialog;
  i: LongInt;
begin
  Selection:=nil;
  Result:=shrNone;
  Dialog:=THelpSelectorDialog.Create(Application);
  try
    Dialog.Nodes:=Nodes;
    if Dialog.ShowModal=mrOk then begin
      i:=Dialog.NodesListBox.ItemIndex;
      if i>=0 then begin
        Selection:=THelpNode(Nodes[i]);
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
  shrNone: ErrorCaption:='Error';
  shrSuccess: exit;
  shrDatabaseNotFound: ErrorCaption:='Help Database not found';
  shrContextNotFound: ErrorCaption:='Help Context not found';
  shrViewerNotFound: ErrorCaption:='Help Viewer not found';
  shrHelpNotFound: ErrorCaption:='Help not found';
  shrViewerError: ErrorCaption:='Help Viewer Error';
  shrSelectorError: ErrorCaption:='Help Selector Error';
  else ErrorCaption:='Unknown Error, please report this bug';
  end;
  MessageDlg(ErrorCaption,ErrMsg,mtError,[mbCancel],0);
end;

function TIDEHelpDatabases.GetBaseDirectoryForBasePathObject(
  BasePathObject: TObject): string;
begin
  Result:='';
  if (BasePathObject=HelpBoss) or (BasePathObject=MainIDEInterface) then
    Result:=EnvironmentOptions.LazarusDirectory
  else if BasePathObject is TProject then
    Result:=TProject(BasePathObject).ProjectDirectory
  else if BasePathObject is TLazPackage then
    Result:=TLazPackage(BasePathObject).Directory;
end;

function TIDEHelpDatabases.ShowHelpForSourcePosition(const Filename: string;
  const CodePos: TPoint; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpBoss.ShowHelpForSourcePosition(Filename,CodePos,ErrMsg);
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
var
  HTMLHelp: THTMLHelpDatabase;
  StartNode: THelpNode;
begin
  FMainHelpDB:=HelpDatabases.CreateHelpDatabase('Lazarus IDE',THTMLHelpDatabase,
                                                true);
  HTMLHelp:=FMainHelpDB as THTMLHelpDatabase;
  HTMLHelp.BasePathObject:=Self;
  // nodes
  StartNode:=THelpNode.CreateURLID(HTMLHelp,'Lazarus',
                                   'file://docs/index.html',lihcStartPage);
  HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,StartNode);
  HTMLHelp.RegisterItemWithNode(StartNode);
end;

procedure THelpManager.RegisterDefaultIDEHelpViewers;
begin
  HelpViewers.RegisterViewer(THTMLBrowserHelpViewer.Create);
end;

constructor THelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpOpts:=THelpOptions.Create;
  HelpOpts.SetDefaultFilename;
  HelpDatabases:=TIDEHelpDatabases.Create;
  HelpViewers:=THelpViewers.Create;
  RegisterIDEHelpDatabases;
  RegisterDefaultIDEHelpViewers;
end;

destructor THelpManager.Destroy;
begin
  FreeThenNil(HelpDatabases);
  FreeThenNil(HelpViewers);
  FreeThenNil(HelpOpts);
  inherited Destroy;
end;

procedure THelpManager.ConnectMainBarEvents;
begin
  with MainIDEBar do begin
    itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
    itmHelpOnlineHelp.OnClick :=@mnuHelpOnlineHelpClicked;
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
        AddContext(pihcProcedure,Tool.ExtractProcHead(Node,
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
    IncludeChain: TList;
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
      CodeToolBoss.GetIncludeCodeChain(ACodePos^.Code,IncludeChain);
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
      debugln('WARNING: ConvertCodePosToPascalHelpContext not (Tool is TCodeTool)');
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
  ListOfPCodeXYPosition: TList;
  PascalHelpContextLists: TList;
  NewList: TPascalHelpContextList;
begin
  debugln('THelpManager.ShowHelpForSourcePosition A Filename=',Filename,' ',dbgs(CodePos));
  Result:=shrSuccess;
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
    // get all possible declarations for this identifier
    if CodeToolBoss.FindDeclarationsAndAncestors(CodeBuffer,CodePos.X,CodePos.Y,
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
      Result:=ShowHelpForPascalContexts(PascalHelpContextLists,ErrMsg);
    end else begin
      MainIDEInterface.DoJumpToCodeToolBossError;
    end;
  finally
    if ListOfPCodeXYPosition<>nil then begin
      for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
        CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
        Dispose(CurCodePos);
      end;
      ListOfPCodeXYPosition.Free;
    end;
    if PascalHelpContextLists<>nil then begin
      for i:=0 to PascalHelpContextLists.Count-1 do
        TObject(PascalHelpContextLists[i]).Free;
      PascalHelpContextLists.Free;
    end;
  end;
end;

end.

