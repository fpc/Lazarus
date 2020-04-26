{
 /***************************************************************************
                            codeexplorer.pas
                            ----------------

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    Window showing the current source as tree structure.
    Normally it shows the codetools nodes of the current unit in the
    source editor. If an include file is open, the corresponding unit is shown.
}
unit CodeExplorer;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  // RTL+FCL
  Classes, SysUtils, types, Laz_AVL_Tree,
  // LazUtils
  LazLoggerBase,
  // LCL
  LCLProc, LCLType, Forms, Controls, Dialogs, Buttons, ComCtrls, Menus, StdCtrls, ExtCtrls,
  // CodeTools
  FileProcs, BasicCodeTools, CustomCodeTool, CodeToolManager, CodeAtom,
  CodeCache, CodeTree, KeywordFuncLists, FindDeclarationTool, DirectivesTree,
  PascalParserTool,
  // IDEIntf
  LazIDEIntf, IDECommands, MenuIntf, SrcEditorIntf, IDEDialogs, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, IDEOptionDefs, CodeExplOpts;

type
  TCodeExplorerView = class;

  TOnGetDirectivesTree =
     procedure(Sender: TObject; var ADirectivesTool: TDirectivesTool) of object;
  TOnJumpToCode = procedure(Sender: TObject; const Filename: string;
                            const Caret: TPoint; TopLine: integer) of object;

  TCodeExplorerViewFlag = (
    cevCodeRefreshNeeded,
    cevDirectivesRefreshNeeded,
    cevRefreshing,
    cevCheckOnIdle // check if a refresh is needed on next idle
    );
  TCodeExplorerViewFlags = set of TCodeExplorerViewFlag;
  
  TCodeObsStackItemType = (
    cositNone,
    cositBegin,
    cositRepeat,
    cositTry,
    cositFinally,
    cositExcept,
    cositCase,
    cositCaseElse,
    cositRoundBracketOpen,
    cositEdgedBracketOpen
    );
  TCodeObsStackItem = record
    StartPos: integer;
    Typ: TCodeObsStackItemType;
    StatementStartPos: integer;
  end;
  TCodeObsStack = ^TCodeObsStackItem;

  { TCodeObserverStatementState }

  TCodeObserverStatementState = class
  private
    function GetStatementStartPos: integer;
    procedure SetStatementStartPos(const AValue: integer);
  public
    Stack: TCodeObsStack;
    StackPtr: integer;
    StackCapacity: integer;
    IgnoreConstLevel: integer;
    TopLvlStatementStartPos: integer;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    procedure Push(Typ: TCodeObsStackItemType; StartPos: integer);
    function Pop: TCodeObsStackItemType;
    procedure PopAll;
    function TopType: TCodeObsStackItemType;
    property StatementStartPos: integer read GetStatementStartPos write SetStatementStartPos;
  end;

  { TCodeExplorerView }

  TCodeExplorerView = class(TForm)
    CodeFilterEdit: TEdit;
    CodePage: TTabSheet;
    CodeTreeview: TTreeView;
    DirectivesFilterEdit: TEdit;
    DirectivesPage: TTabSheet;
    DirectivesTreeView: TTreeView;
    IdleTimer1: TIdleTimer;
    Imagelist1: TImageList;
    MainNotebook: TPageControl;
    MenuItem1: TMenuItem;
    CodeTreeviewButtonPanel: TPanel;
    CodeOptionsSpeedButton: TSpeedButton;
    CodeRefreshSpeedButton: TSpeedButton;
    CodeModeSpeedButton: TSpeedButton;
    DirOptionsSpeedButton: TSpeedButton;
    DirRefreshSpeedButton: TSpeedButton;
    TreePopupmenu: TPopupMenu;
    procedure CodeExplorerViewCreate(Sender: TObject);
    procedure CodeExplorerViewDestroy(Sender: TObject);
    procedure CodeFilterEditChange(Sender: TObject);
    procedure CodeTreeviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DirectivesFilterEditChange(Sender: TObject);
    procedure DirRefreshSpeedButtonClick(Sender: TObject);
    procedure FilterEditEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure JumpToMenuItemClick(Sender: TObject);
    procedure JumpToImplementationMenuItemClick(Sender: TObject);
    procedure CloseIDEHandler(Sender: TObject);
    procedure ShowSrcEditPosMenuItemClick(Sender: TObject);
    procedure MainNotebookPageChanged(Sender: TObject);
    procedure CodeModeSpeedButtonClick(Sender: TObject);
    procedure CodeRefreshSpeedButtonClick(Sender: TObject);
    procedure OptionsSpeedButtonClick(Sender: TObject);
    procedure RefreshMenuItemClick(Sender: TObject);
    procedure RenameMenuItemClick(Sender: TObject);
    procedure TreePopupmenuPopup(Sender: TObject);
    procedure TreeviewDblClick(Sender: TObject);
    procedure TreeviewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeviewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UserInputHandler(Sender: TObject; {%H-}Msg: Cardinal);
  private
    fCategoryNodes: array[TCodeExplorerCategory] of TTreeNode;
    FCodeFilename: string;
    FCodeCmd1, FCodeCmd2, FCodeCmd3: TIDECommand;
    FDirectivesFilename: string;
    FFlags: TCodeExplorerViewFlags;
    FLastCodeChangeStep: integer;
    FLastCodeFilter: string;
    fLastCodeOptionsChangeStep: integer;
    FLastCodeValid: boolean;
    FLastCodeXY: TPoint;
    FLastCode: TCodeBuffer;
    FLastDirectivesChangeStep: integer;
    FLastDirectivesFilter: string;
    FLastMode: TCodeExplorerMode;
    FMode: TCodeExplorerMode;
    fObserverCatNodes: array[TCEObserverCategory] of TTreeNode;
    fObserverCatOverflow: array[TCEObserverCategory] of boolean;
    fObserverNode: TTreeNode;
    fSurroundingNode: TTreeNode;
    FOnGetDirectivesTree: TOnGetDirectivesTree;
    FOnJumpToCode: TOnJumpToCode;
    FOnShowOptions: TNotifyEvent;
    fSortCodeTool: TCodeTool;
    fLastCodeTool: TCodeTool;
    fCodeSortedForStartPos: TAvlTree;// tree of TTreeNode sorted for TViewNodeData(Node.Data).StartPos, secondary EndPos
    fNodesWithPath: TAvlTree; // tree of TViewNodeData sorted for Path and Params
    FUpdateCount: integer;
    ImgIDClass: Integer;
    ImgIDClassInterface: Integer;
    ImgIDRecord: Integer;
    ImgIDEnum: Integer;
    ImgIDHelper: Integer;
    ImgIDConst: Integer;
    ImgIDSection: Integer;
    ImgIDDefault: integer;
    ImgIDFinalization: Integer;
    ImgIDImplementation: Integer;
    ImgIDInitialization: Integer;
    ImgIDInterface: Integer;
    ImgIDProcedure: Integer;
    ImgIDFunction: Integer;
    ImgIDConstructor: Integer;
    ImgIDDestructor: Integer;
    ImgIDProgram: Integer;
    ImgIDProperty: Integer;
    ImgIDPropertyReadOnly: Integer;
    ImgIDType: Integer;
    ImgIDUnit: Integer;
    ImgIDVariable: Integer;
    ImgIDHint: Integer;
    ImgIDLabel: Integer;
    procedure AssignAllImages;
    procedure ClearCodeTreeView;
    procedure ClearDirectivesTreeView;
    function GetCodeFilter: string;
    function GetCurrentPage: TCodeExplorerPage;
    function GetDirectivesFilter: string;
    function GetCodeNodeDescription(ACodeTool: TCodeTool;
                                   CodeNode: TCodeTreeNode): string;
    function GetDirectiveNodeDescription(ADirectivesTool: TDirectivesTool;
                                         Node: TCodeTreeNode): string;
    function GetCodeNodeImage(Tool: TFindDeclarationTool;
                              CodeNode: TCodeTreeNode): integer;
    function GetDirectiveNodeImage(CodeNode: TCodeTreeNode): integer;
    procedure CreateIdentifierNodes(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode;
                          ParentViewNode: TTreeNode);
    function GetCTNodePath(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode): string;
    procedure CreateNodePath(ACodeTool: TCodeTool; aNodeData: TObject);
    procedure AddImplementationNode(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode);
    procedure CreateDirectiveNodes(ADirectivesTool: TDirectivesTool;
      CodeNode: TCodeTreeNode; ParentViewNode: TTreeNode);
    procedure CreateObservations(Tool: TCodeTool);
    function CreateObserverNode(Tool: TCodeTool; f: TCEObserverCategory): TTreeNode;
    procedure CreateObserverNodesForStatement(Tool: TCodeTool;
                            CodeNode: TCodeTreeNode; StartPos, EndPos: integer;
                            ObserverState: TCodeObserverStatementState);
    procedure FindObserverTodos(Tool: TCodeTool);
    procedure CreateSurrounding(Tool: TCodeTool);
    procedure DeleteTVNode(TVNode: TTreeNode);
    procedure SetCodeFilter(const AValue: string);
    procedure SetCurrentPage(const AValue: TCodeExplorerPage);
    procedure SetDirectivesFilter(const AValue: string);
    procedure SetMode(AMode: TCodeExplorerMode);
    procedure UpdateMode;
    procedure UpdateCaption;
    function OnExpandedStateGetNodeText(Node: TTreeNode): string;
    procedure ApplyCodeFilter;
    procedure ApplyDirectivesFilter;
    function CompareCodeNodes(Node1, Node2: TTreeNode): integer;
    function FilterNode(ANode: TTreeNode; const TheFilter: string;
      KeepTopLevel: Boolean): boolean;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CheckOnIdle;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Refresh(OnlyVisible: boolean);
    procedure RefreshCode(OnlyVisible: boolean);
    procedure RefreshDirectives(OnlyVisible: boolean);
    procedure ClearCTNodes(ATreeView: TTreeView);// remove temporary references
    function JumpToSelection(ToImplementation: boolean = false): boolean; // jump in source editor
    function SelectSourceEditorNode: boolean;
    function SelectCodePosition(CodeBuf: TCodeBuffer; X, Y: integer): boolean; // select deepest node
    function FindCodeTVNodeAtCleanPos(CleanPos: integer): TTreeNode;
    procedure BuildCodeSortedForStartPos;
    procedure CurrentCodeBufferChanged;
    procedure CodeFilterChanged;
    procedure DirectivesFilterChanged;
    function FilterFits(const NodeText, TheFilter: string): boolean; virtual;
    function GetCurrentTreeView: TCustomTreeView;
  public
    property OnGetDirectivesTree: TOnGetDirectivesTree read FOnGetDirectivesTree
                                                     write FOnGetDirectivesTree;
    property OnJumpToCode: TOnJumpToCode read FOnJumpToCode write FOnJumpToCode;
    property OnShowOptions: TNotifyEvent read FOnShowOptions write FOnShowOptions;
    property Mode: TCodeExplorerMode read FMode write SetMode;
    property CodeFilename: string read FCodeFilename;
    property CodeFilter: string read GetCodeFilter write SetCodeFilter;
    property DirectivesFilename: string read FDirectivesFilename;
    property DirectivesFilter: string read GetDirectivesFilter
                                      write SetDirectivesFilter;
    property CurrentPage: TCodeExplorerPage read GetCurrentPage
                                            write SetCurrentPage;
  end;

const
  CodeExplorerMenuRootName = 'Code Explorer';
  CodeObserverMaxNodes = 50;

var
  CodeExplorerView: TCodeExplorerView = nil;
  CEJumpToIDEMenuCommand: TIDEMenuCommand;
  CEJumpToImplementationIDEMenuCommand: TIDEMenuCommand;
  CEShowSrcEditPosIDEMenuCommand: TIDEMenuCommand;
  CERefreshIDEMenuCommand: TIDEMenuCommand;
  CERenameIDEMenuCommand: TIDEMenuCommand;

procedure RegisterStandardCodeExplorerMenuItems;

function GetToDoComment(const Src: string;
                       CommentStartPos, CommentEndPos: integer;
                       out MagicStartPos, TextStartPos, TextEndPos: integer): boolean;

implementation

{$R *.lfm}

type

  { TViewNodeData }

  TViewNodeData = class
  public
    CTNode: TCodeTreeNode; // only valid during update, at other times it is nil
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    StartPos, EndPos: integer;
    Path: string;
    Params: string;
    ImplementationNode: TViewNodeData;
    SortChildren: boolean; // sort for TVNode text (optional) and StartPos, EndPos
    constructor Create(CodeNode: TCodeTreeNode; SortTheChildren: boolean = true);
    destructor Destroy; override;
    procedure CreateParams(ACodeTool: TCodeTool);
  end;

function CompareViewNodeDataStartPos(Node1, Node2: TTreeNode): integer;
var
  NodeData1: TViewNodeData;
  NodeData2: TViewNodeData;
begin
  NodeData1:=TViewNodeData(Node1.Data);
  NodeData2:=TViewNodeData(Node2.Data);
  if NodeData1.StartPos>NodeData2.StartPos then
    Result:=1
  else if NodeData1.StartPos<NodeData2.StartPos then
    Result:=-1
  else if NodeData1.EndPos>NodeData2.EndPos then
    Result:=1
  else if NodeData1.EndPos<NodeData2.EndPos then
    Result:=-1
  else
    Result:=0;
end;

function CompareStartPosWithViewNodeData(Key: PInteger; Node: TTreeNode): integer;
var
  NodeData: TViewNodeData;
begin
  NodeData:=TViewNodeData(Node.Data);
  if Key^ > NodeData.StartPos then
    Result:=1
  else if Key^ < NodeData.StartPos then
    Result:=-1
  else
    Result:=0;
end;

function CompareViewNodePathsAndParams(NodeData1, NodeData2: Pointer): integer;
var
  Node1: TViewNodeData absolute NodeData1;
  Node2: TViewNodeData absolute NodeData2;
begin
  Result:=SysUtils.CompareText(Node1.Path,Node2.Path);
  if Result<>0 then exit;
  Result:=SysUtils.CompareText(Node1.Params,Node2.Params);
end;

function CompareViewNodePaths(NodeData1, NodeData2: Pointer): integer;
var
  Node1: TViewNodeData absolute NodeData1;
  Node2: TViewNodeData absolute NodeData2;
begin
  Result:=SysUtils.CompareText(Node1.Path,Node2.Path);
end;

procedure RegisterStandardCodeExplorerMenuItems;
var
  Path: String;
begin
  CodeExplorerMenuRoot:=RegisterIDEMenuRoot(CodeExplorerMenuRootName);
  Path:=CodeExplorerMenuRoot.Name;
  CEJumpToIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Jump to', lisMenuJumpTo);
  CEJumpToImplementationIDEMenuCommand:=RegisterIDEMenuCommand(Path,
    'Jump to implementation', lisMenuJumpToImplementation);
  CEShowSrcEditPosIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Show position of source editor',
    lisShowPositionOfSourceEditor);
  CERefreshIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Refresh', dlgUnitDepRefresh);
  CERenameIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Rename', lisRename);
end;

function GetToDoComment(const Src: string; CommentStartPos,
  CommentEndPos: integer; out MagicStartPos, TextStartPos, TextEndPos: integer
  ): boolean;
var
  StartPos: Integer;
  EndPos: Integer;
  p: Integer;
begin
  if CommentStartPos<1 then exit(false);
  if CommentEndPos-CommentStartPos<5 then exit(false);
  if Src[CommentStartPos]='/' then begin
    StartPos:=CommentStartPos+2;
    EndPos:=CommentEndPos;
  end else if (Src[CommentStartPos]='{') then begin
    StartPos:=CommentStartPos+1;
    EndPos:=CommentEndPos-1;
  end else if (CommentStartPos<length(Src)) and (Src[CommentStartPos]='(')
  and (Src[CommentStartPos+1]='*') then begin
    StartPos:=CommentStartPos+2;
    EndPos:=CommentEndPos-2;
  end else
    exit(false);
  while (StartPos<EndPos) and (Src[StartPos]=' ') do inc(StartPos);
  MagicStartPos:=StartPos;
  if Src[StartPos]='#' then inc(StartPos);
  if CompareIdentifiers('todo',@Src[StartPos])<>0 then exit(false);
  // this is a ToDo
  p:=StartPos+length('todo');
  TextStartPos:=p;
  while (TextStartPos<EndPos) and (Src[TextStartPos]<>':') do inc(TextStartPos);
  if Src[TextStartPos]=':' then
    inc(TextStartPos) // a todo with colon syntax
  else
    TextStartPos:=p; // a todo without syntax
  while (TextStartPos<EndPos) and (Src[TextStartPos]=' ') do inc(TextStartPos);
  TextEndPos:=EndPos;
  while (TextEndPos>TextStartPos) and (Src[TextEndPos-1]=' ') do dec(TextEndPos);
  Result:=true;
end;

{ TViewNodeData }

constructor TViewNodeData.Create(CodeNode: TCodeTreeNode;
  SortTheChildren: boolean);
begin
  CTNode:=CodeNode;
  Desc:=CodeNode.Desc;
  SubDesc:=CodeNode.SubDesc;
  StartPos:=CodeNode.StartPos;
  EndPos:=CodeNode.EndPos;
  SortChildren:=SortTheChildren;
end;

destructor TViewNodeData.Destroy;
begin
  FreeAndNil(ImplementationNode);
  inherited Destroy;
end;

procedure TViewNodeData.CreateParams(ACodeTool: TCodeTool);
begin
  if Params<>'' then exit;
  if CTNode.Desc=ctnProcedure then begin
    try
      Params:=ACodeTool.ExtractProcHead(CTNode,
        [phpWithoutClassKeyword,phpWithoutClassName,phpWithoutName,phpWithoutSemicolon]);
    except
      on E: ECodeToolError do ; // ignore syntax errors
    end;
  end;
  if Params='' then
    Params:=' ';
end;

{ TCodeExplorerView }

procedure TCodeExplorerView.CodeExplorerViewCreate(Sender: TObject);
begin
  FMode := CodeExplorerOptions.Mode;
  UpdateMode;

  Name:=NonModalIDEWindowNames[nmiwCodeExplorer];
  UpdateCaption;

  case CodeExplorerOptions.Page of
  cepDirectives: MainNotebook.ActivePage:=DirectivesPage;
  else MainNotebook.ActivePage:=CodePage;
  end;

  CodePage.Caption:=lisCode;
  CodeRefreshSpeedButton.Hint:=dlgUnitDepRefresh;
  CodeOptionsSpeedButton.Hint:=lisOptions;
  CodeFilterEdit.Text:='';
  DirectivesPage.Caption:=lisDirectives;
  DirectivesFilterEdit.Text:='';
  DirRefreshSpeedButton.Hint:=dlgUnitDepRefresh;
  DirOptionsSpeedButton.Hint:=lisOptions;
  CodeFilterEdit.TextHint:=lisCEFilter;
  DirectivesFilterEdit.TextHint:=lisCEFilter;

  AssignAllImages;
  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  CodeExplorerMenuRoot.MenuItem:=TreePopupMenu.Items;
  //CodeExplorerMenuRoot.Items.WriteDebugReport(' ');

  CEJumpToIDEMenuCommand.OnClick:=@JumpToMenuItemClick;
  CEJumpToImplementationIDEMenuCommand.OnClick:=@JumpToImplementationMenuItemClick;
  CEShowSrcEditPosIDEMenuCommand.OnClick:=@ShowSrcEditPosMenuItemClick;
  CERefreshIDEMenuCommand.OnClick:=@RefreshMenuItemClick;
  CERenameIDEMenuCommand.OnClick:=@RenameMenuItemClick;

  fNodesWithPath:=TAvlTree.Create(@CompareViewNodePathsAndParams);

  Application.AddOnUserInputHandler(@UserInputHandler);
  LazarusIDE.AddHandlerOnIDEClose(@CloseIDEHandler);
end;

procedure TCodeExplorerView.CodeExplorerViewDestroy(Sender: TObject);
begin
  //debugln('TCodeExplorerView.CodeExplorerViewDestroy');
  fLastCodeTool:=nil;
  FreeAndNil(fNodesWithPath);
  FreeAndNil(fCodeSortedForStartPos);
  if CodeExplorerView=Self then
    CodeExplorerView:=nil;
end;

procedure TCodeExplorerView.CodeFilterEditChange(Sender: TObject);
begin
  CodeFilterChanged;
end;

procedure TCodeExplorerView.CodeTreeviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if Button=mbMiddle then begin
    Node:=CodeTreeview.GetNodeAt(X,Y);
    Node.Selected:=true;
    JumpToSelection(true);
  end;
end;

procedure TCodeExplorerView.DirectivesFilterEditChange(Sender: TObject);
begin
  DirectivesFilterChanged;
end;

procedure TCodeExplorerView.DirRefreshSpeedButtonClick(Sender: TObject);
begin
  FLastDirectivesChangeStep:=CTInvalidChangeStamp;
  RefreshDirectives(true);
end;

procedure TCodeExplorerView.FilterEditEnter(Sender: TObject);
begin
  (Sender as TEdit).SelectAll;
end;

procedure TCodeExplorerView.FormActivate(Sender: TObject);
begin
  //DebugLn(['TCodeExplorerView.FormActivate!']);
  FCodeCmd1:=IDECommandList.FindIDECommand(ecFindDeclaration);
  FCodeCmd2:=IDECommandList.FindIDECommand(ecFindProcedureDefinition);
  FCodeCmd3:=IDECommandList.FindIDECommand(ecFindProcedureMethod);
end;

procedure TCodeExplorerView.IdleTimer1Timer(Sender: TObject);
begin
  if not (cevCheckOnIdle in FFlags) then exit;
  if (Screen.ActiveCustomForm<>nil)
  and (fsModal in Screen.ActiveCustomForm.FormState) then
  begin
    // do not update while a modal form is shown, except for clear
    if SourceEditorManagerIntf=nil then exit;
    if SourceEditorManagerIntf.SourceEditorCount=0 then
    begin
      Exclude(FFlags,cevCheckOnIdle);
      FLastCodeValid:=false;
      ClearCodeTreeView;
      FDirectivesFilename:='';
      ClearDirectivesTreeView;
    end;
    exit;
  end;
  if not IsVisible then exit;
  Exclude(FFlags,cevCheckOnIdle);
  case CurrentPage of
  cepNone: ;
  cepCode: if (CurrentPage<>cepCode) or CodeTreeview.Focused then exit;
  cepDirectives: if (CurrentPage<>cepDirectives) or DirectivesTreeView.Focused then exit;
  end;
  Refresh(true);
end;

procedure TCodeExplorerView.JumpToMenuItemClick(Sender: TObject);
begin
  JumpToSelection(false);
end;

procedure TCodeExplorerView.JumpToImplementationMenuItemClick(Sender: TObject);
begin
  JumpToSelection(true);
end;

procedure TCodeExplorerView.CloseIDEHandler(Sender: TObject);
begin
  CodeExplorerOptions.Save;
end;

procedure TCodeExplorerView.ShowSrcEditPosMenuItemClick(Sender: TObject);
begin
  SelectSourceEditorNode;
end;

procedure TCodeExplorerView.MainNotebookPageChanged(Sender: TObject);
begin
  if MainNotebook.ActivePage=DirectivesPage then
    CodeExplorerOptions.Page:=cepDirectives
  else
    CodeExplorerOptions.Page:=cepCode;
  Refresh(true);
end;

procedure TCodeExplorerView.CodeModeSpeedButtonClick(Sender: TObject);
begin
  // Let's Invert Mode of Exibition
  if Mode = cemCategory then
    SetMode(cemSource)
  else
    SetMode(cemCategory);
end;

procedure TCodeExplorerView.CodeRefreshSpeedButtonClick(Sender: TObject);
begin
  FLastCodeChangeStep:=CTInvalidChangeStamp;
  RefreshCode(true);
end;

procedure TCodeExplorerView.OptionsSpeedButtonClick(Sender: TObject);
begin
  if Assigned(FOnShowOptions) then
  begin
    OnShowOptions(Self);
    Refresh(True);
  end;
end;

procedure TCodeExplorerView.RefreshMenuItemClick(Sender: TObject);
begin
  FLastCodeChangeStep:=CTInvalidChangeStamp;
  FLastDirectivesChangeStep:=CTInvalidChangeStamp;
  Refresh(true);
end;

procedure TCodeExplorerView.RenameMenuItemClick(Sender: TObject);
begin
  if not JumpToSelection then begin
    IDEMessageDialog(lisCCOErrorCaption, lisTreeNeedsRefresh, mtError, [mbOk]);
    Refresh(true);
    exit;
  end;
  ExecuteIDECommand(SourceEditorManagerIntf.ActiveSourceWindow, ecRenameIdentifier);
end;

procedure TCodeExplorerView.TreePopupmenuPopup(Sender: TObject);
var
  CurTreeView: TCustomTreeView;
  CurItem: TTreeNode;
  CanRename: boolean;
  CurNode: TViewNodeData;
  HasImplementation: Boolean;
begin
  CanRename:=false;
  HasImplementation:=false;
  CurTreeView:=GetCurrentTreeView;
  if CurTreeView<>nil then begin
    if tvoAllowMultiselect in CurTreeView.Options then
      CurItem:=CurTreeView.GetFirstMultiSelected
    else
      CurItem:=CurTreeView.Selected;
    if CurItem<>nil then begin
      CurNode:=TViewNodeData(CurItem.Data);
      if CurNode.StartPos>0 then begin
        case CurrentPage of
        cepCode:
          if (CurNode.Desc in AllIdentifierDefinitions+[ctnProcedure,ctnProperty])
          and (CurItem.GetNextMultiSelected=nil) then
            CanRename:=true;
        cepDirectives:
          ;
        end;
      end;
      if (CurNode.ImplementationNode<>nil)
      and (CurNode.ImplementationNode.StartPos>0) then
        HasImplementation:=true;
    end;
  end;
  CERenameIDEMenuCommand.Visible:=CanRename;
  CEJumpToImplementationIDEMenuCommand.Visible:=HasImplementation;
  //DebugLn(['TCodeExplorerView.TreePopupmenuPopup ',CERenameIDEMenuCommand.Visible]);
end;

procedure TCodeExplorerView.TreeviewDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.TreeviewDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data<>nil then
    TObject(Node.Data).Free;
end;

procedure TCodeExplorerView.TreeviewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[])
  or ((Key=FCodeCmd1.ShortcutA.Key1) and (Shift=FCodeCmd1.ShortcutA.Shift1))
  or ((Key=FCodeCmd1.ShortcutB.Key1) and (Shift=FCodeCmd1.ShortcutB.Shift1))
  or ((Key=FCodeCmd2.ShortcutA.Key1) and (Shift=FCodeCmd2.ShortcutA.Shift1))
  or ((Key=FCodeCmd2.ShortcutB.Key1) and (Shift=FCodeCmd2.ShortcutB.Shift1))
  or ((Key=FCodeCmd3.ShortcutA.Key1) and (Shift=FCodeCmd3.ShortcutA.Shift1))
  or ((Key=FCodeCmd3.ShortcutB.Key1) and (Shift=FCodeCmd3.ShortcutB.Shift1))
  then begin
    JumpToSelection;
    Key:=0;
  end;
end;

procedure TCodeExplorerView.UserInputHandler(Sender: TObject; Msg: Cardinal);
begin
  if CodeExplorerOptions.Refresh=cerOnIdle then
    CheckOnIdle;
end;

procedure TCodeExplorerView.AssignAllImages;
begin
  IDEImages.AssignImage(CodeRefreshSpeedButton, 'laz_refresh');
  IDEImages.AssignImage(CodeOptionsSpeedButton, 'menu_environment_options');
  IDEImages.AssignImage(DirRefreshSpeedButton, 'laz_refresh');
  IDEImages.AssignImage(DirOptionsSpeedButton, 'menu_environment_options');

  CodeTreeview.Images := IDEImages.Images_16;
  ImgIDDefault := IDEImages.GetImageIndex('ce_default');
  ImgIDProgram := IDEImages.GetImageIndex('ce_program');
  ImgIDUnit := IDEImages.GetImageIndex('cc_unit');
  ImgIDInterface := IDEImages.GetImageIndex('ce_interface');
  ImgIDImplementation := IDEImages.GetImageIndex('ce_implementation');
  ImgIDInitialization := IDEImages.GetImageIndex('ce_initialization');
  ImgIDFinalization := IDEImages.GetImageIndex('ce_finalization');
  ImgIDType := IDEImages.GetImageIndex('cc_type');
  ImgIDVariable := IDEImages.GetImageIndex('cc_variable');
  ImgIDConst := IDEImages.GetImageIndex('cc_constant');
  ImgIDClass := IDEImages.GetImageIndex('cc_class');
  ImgIDClassInterface := IDEImages.GetImageIndex('ce_classinterface');
  ImgIDHelper := IDEImages.GetImageIndex('ce_helper');
  ImgIDRecord := IDEImages.GetImageIndex('cc_record');
  ImgIDEnum := IDEImages.GetImageIndex('cc_enum');
  ImgIDProcedure := IDEImages.GetImageIndex('cc_procedure');
  ImgIDFunction := IDEImages.GetImageIndex('cc_function');
  ImgIDConstructor := IDEImages.GetImageIndex('cc_constructor');
  ImgIDDestructor := IDEImages.GetImageIndex('cc_destructor');
  ImgIDLabel := IDEImages.GetImageIndex('cc_label');
  ImgIDProperty := IDEImages.GetImageIndex('cc_property');
  ImgIDPropertyReadOnly := IDEImages.GetImageIndex('cc_property_ro');
  // sections
  ImgIDSection := IDEImages.GetImageIndex('ce_section');
  ImgIDHint := IDEImages.GetImageIndex('state_hint');
end;

function TCodeExplorerView.GetCodeNodeDescription(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode): string;
var
  ClassIdentNode, HelperForNode, InhNode: TCodeTreeNode;
begin
  Result:='?';
  try
    case CodeNode.Desc of
    ctnUnit, ctnProgram, ctnLibrary, ctnPackage:
      Result:=CodeNode.DescAsString+' '+ACodeTool.ExtractSourceName;
    ctnTypeSection:
      Result:='Type';
    ctnVarSection:
      Result:='Var';
    ctnConstSection:
      Result:='Const';
    ctnLabelSection:
      Result:='Label';
    ctnResStrSection:
      Result:='Resourcestring';
    ctnVarDefinition, ctnConstDefinition, ctnEnumIdentifier, ctnLabel:
      Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);
    ctnUseUnit:
      Result:=ACodeTool.ExtractDottedIdentifier(CodeNode.StartPos);
    ctnTypeDefinition:
      begin
        Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);
        ClassIdentNode := CodeNode.FirstChild;
        if Assigned(ClassIdentNode) then
        begin
          if ClassIdentNode.Desc in [ctnClassHelper, ctnRecordHelper, ctnTypeHelper] then
            HelperForNode := ACodeTool.FindHelperForNode(ClassIdentNode)
          else
            HelperForNode := nil;
          InhNode:=ACodeTool.FindInheritanceNode(ClassIdentNode);
          if InhNode<>nil then
            Result:=Result+ACodeTool.ExtractNode(InhNode,[]);
          if HelperForNode<>nil then
            Result:=Result+' '+ACodeTool.ExtractNode(HelperForNode,[]);
        end;
      end;
    ctnGenericType:
      Result:=ACodeTool.ExtractDefinitionName(CodeNode);
    ctnClass,ctnObject,ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,
    ctnClassInterface,ctnCPPClass:
      Result:='('+ACodeTool.ExtractClassInheritance(CodeNode,[])+')';
    ctnProcedure:
      Result:=ACodeTool.ExtractProcHead(CodeNode,
                    [// phpWithStart is no needed because there are icons
                     phpWithVarModifiers,
                     phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                     phpWithOfObject]);
    ctnProcedureHead:
      Result:='Procedure Header';
    ctnProperty:
      Result:=ACodeTool.ExtractPropName(CodeNode,false); // property keyword is not needed because there are icons
    ctnInterface:
      Result:='Interface';
    ctnBeginBlock:
      Result:='Begin block';
    ctnAsmBlock:
      Result:='Asm block';
    else
      Result:=CodeNode.DescAsString;
    end;
  except
    on E: ECodeToolError do
      Result:=''; // ignore syntax errors
  end;
end;

function TCodeExplorerView.GetDirectiveNodeDescription(
  ADirectivesTool: TDirectivesTool; Node: TCodeTreeNode): string;
begin
  Result:=ADirectivesTool.GetDirective(Node);
end;

function TCodeExplorerView.GetCodeFilter: string;
begin
  Result:=CodeFilterEdit.Text;
end;

procedure TCodeExplorerView.ClearCodeTreeView;
var
  f: TCEObserverCategory;
  c: TCodeExplorerCategory;
begin
  for c:=low(TCodeExplorerCategory) to high(TCodeExplorerCategory) do
    fCategoryNodes[c]:=nil;
  fObserverNode:=nil;
  for f:=low(TCEObserverCategory) to high(TCEObserverCategory) do
    fObserverCatNodes[f]:=nil;
  fSurroundingNode:=nil;
  CodeTreeview.Items.Clear;
end;

procedure TCodeExplorerView.ClearDirectivesTreeView;
begin
  DirectivesTreeView.Items.Clear;
end;

function TCodeExplorerView.GetCurrentPage: TCodeExplorerPage;
begin
  if MainNotebook.ActivePage=CodePage then
    Result:=cepCode
  else if MainNotebook.ActivePage=DirectivesPage then
    Result:=cepDirectives
  else
    Result:=cepNone;
end;

function TCodeExplorerView.GetDirectivesFilter: string;
begin
  Result:=DirectivesFilterEdit.Text;
end;

function TCodeExplorerView.GetCodeNodeImage(Tool: TFindDeclarationTool;
  CodeNode: TCodeTreeNode): integer;
begin
  case CodeNode.Desc of
    ctnProgram,ctnLibrary,ctnPackage: Result:=ImgIDProgram;
    ctnUnit:                          Result:=ImgIDUnit;
    ctnInterface:                     Result:=ImgIDInterface;
    ctnImplementation:                Result:=ImgIDImplementation;
    ctnInitialization:                Result:=ImgIDInitialization;
    ctnFinalization:                  Result:=ImgIDFinalization;
    ctnTypeSection:                   Result:=ImgIDSection;
    ctnTypeDefinition:
      begin
        if (CodeNode.FirstChild <> nil) then
          case CodeNode.FirstChild.Desc of
            ctnClassInterface,ctnDispinterface,ctnObjCProtocol:
              Result := ImgIDClassInterface;
            ctnClass,ctnObjCClass,ctnObjCCategory,ctnCPPClass:
              Result := ImgIDClass;
            ctnObject,ctnRecordType:
              Result := ImgIDRecord;
            ctnEnumerationType,ctnEnumIdentifier:
              Result:=ImgIDEnum;
            ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
              Result := ImgIDHelper;
          else
            Result := ImgIDType;
          end
        else
          Result := ImgIDType;
      end;
    ctnVarSection:                    Result:=ImgIDSection;
    ctnVarDefinition:                 Result:=ImgIDVariable;
    ctnConstSection,ctnResStrSection: Result:=ImgIDSection;
    ctnConstDefinition:               Result:=ImgIDConst;
    ctnClassInterface,ctnDispinterface,ctnObjCProtocol:
      Result := ImgIDClassInterface;
    ctnClass,ctnObject,
    ctnObjCClass,ctnObjCCategory,ctnCPPClass:
                                      Result:=ImgIDClass;
    ctnRecordType:                    Result:=ImgIDRecord;
    ctnEnumerationType,ctnEnumIdentifier:
                                      Result:=ImgIDEnum;
    ctnClassHelper,ctnRecordHelper,ctnTypeHelper:
                                      Result:=ImgIDHelper;
    ctnProcedure:
                                      if Tool.NodeIsConstructor(CodeNode) then
                                        Result:=ImgIDConstructor
                                      else
                                      if Tool.NodeIsDestructor(CodeNode) then
                                        Result:=ImgIDDestructor
                                      else
                                      if Tool.NodeIsFunction(CodeNode) then
                                        Result:=ImgIDFunction
                                      else
                                        Result:=ImgIDProcedure;
    ctnProperty:                      Result:=ImgIDProperty;
    ctnUsesSection:                   Result:=ImgIDSection;
    ctnUseUnit:                       Result:=ImgIDUnit;
    ctnLabelSection:                  Result:=ImgIDSection;
    ctnLabel:                         Result:=ImgIDLabel;
  else
    Result:=ImgIDDefault;
  end;
end;

function TCodeExplorerView.GetDirectiveNodeImage(CodeNode: TCodeTreeNode): integer;
begin
  case CodeNode.SubDesc of
  cdnsInclude:  Result:=ImgIDSection;
  else
    case CodeNode.Desc of
    cdnIf:     Result:=ImgIDSection;
    cdnElseIf: Result:=ImgIDSection;
    cdnElse:   Result:=ImgIDSection;
    cdnEnd:    Result:=ImgIDSection;
    cdnDefine: Result:=ImgIDConst;
    else
      Result:=ImgIDDefault;
    end;
  end;
end;

procedure TCodeExplorerView.CreateIdentifierNodes(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode; ParentViewNode: TTreeNode);
var
  NodeData: TViewNodeData;
  NodeText: String;
  ViewNode, CurParentViewNode, InFrontViewNode: TTreeNode;
  NodeImageIndex: Integer;
  ShowNode: Boolean;
  ShowChilds: Boolean;
  Category: TCodeExplorerCategory;
begin
  InFrontViewNode:=nil;
  while CodeNode<>nil do begin
    ShowNode:=true;
    ShowChilds:=true;
    CurParentViewNode:=ParentViewNode;

    // don't show statements
    if (CodeNode.Desc in AllPascalStatements+[ctnParameterList]-
        [ctnInitialization,ctnFinalization]) then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;
    // don't show parameter lists
    if (CodeNode.Desc in [ctnProcedureHead]) then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;
    // don't show forward class definitions
    if (CodeNode.Desc=ctnTypeDefinition)
    and (CodeNode.FirstChild<>nil)
    and (CodeNode.FirstChild.Desc in AllClasses)
    and ((CodeNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0) then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;
    // don't show class node (the type node is already shown)
    if (CodeNode.Desc in AllClasses) then begin
      ShowNode:=false;
    end;

    //don't show child nodes of ctnUseUnit
    if (CodeNode.Desc=ctnUseUnit)
    then begin
      ShowChilds:=false;
    end;

    // don't show subs
    if CodeNode.Desc in [ctnConstant,ctnIdentifier,ctnRangedArrayType,
      ctnOpenArrayType,ctnOfConstType,ctnRangeType,ctnTypeType,ctnFileType,
      ctnVariantType,ctnSetType,ctnProcedureType]
    then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;

    // show enums, but not the brackets
    if CodeNode.Desc=ctnEnumerationType then
      ShowNode:=false;

    // don't show end node and class modification nodes
    if CodeNode.Desc in [ctnEndPoint,ctnClassInheritance,ctnHelperFor,
                         ctnClassAbstract,ctnClassExternal,ctnClassSealed]
    then
      ShowNode:=false;
      
    // don't show class visibility section nodes
    if (CodeNode.Desc in AllClassSections) then
      ShowNode:=false;

    if Mode=cemCategory then begin
      // don't show method bodies
      if (CodeNode.Desc=ctnProcedure)
      and (ACodeTool.NodeIsMethodBody(CodeNode)) then begin
        ShowNode:=false;
        ShowChilds:=false;
      end;

      // don't show single hint modifiers
      if (CodeNode.Desc = ctnHintModifier) and (CurParentViewNode = nil) then
      begin
        ShowNode:=false;
        ShowChilds:=false;
      end;

      // category mode: put nodes in categories
      Category:=cecNone;
      if ShowNode
      and ((CodeNode.Parent=nil)
        or (CodeNode.Parent.Desc in AllCodeSections)
        or (CodeNode.Parent.Parent=nil)
        or (CodeNode.Parent.Parent.Desc in AllCodeSections)) then
      begin
        // top level definition
        case CodeNode.Desc of
        ctnUseUnit:         Category:=cecUses;
        ctnTypeDefinition,ctnGenericType:  Category:=cecTypes;
        ctnVarDefinition:   Category:=cecVariables;
        ctnConstDefinition,ctnEnumIdentifier: Category:=cecConstants;
        ctnProcedure:       Category:=cecProcedures;
        ctnProperty:        Category:=cecProperties;
        end;
        if Category<>cecNone then begin
          ShowNode:=Category in CodeExplorerOptions.Categories;
          if ShowNode then begin
            if fCategoryNodes[Category]=nil then begin
              // create treenode for new category
              NodeData:=TViewNodeData.Create(CodeNode.Parent);
              NodeText:=CodeExplorerLocalizedString(Category);
              NodeImageIndex:=GetCodeNodeImage(ACodeTool,CodeNode.Parent);
              fCategoryNodes[Category]:=CodeTreeview.Items.AddChildObject(nil,
                                                             NodeText,NodeData);
              fCategoryNodes[Category].ImageIndex:=NodeImageIndex;
              fCategoryNodes[Category].SelectedIndex:=NodeImageIndex;
            end;
            if (CurParentViewNode=nil) then
              CurParentViewNode:=fCategoryNodes[Category];
            InFrontViewNode:=nil;
          end;
        end else begin
          ShowNode:=false;
        end;
      end else begin
        // not a top level node
      end;
      //DebugLn(['TCodeExplorerView.CreateIdentifierNodes ',CodeNode.DescAsString,' ShowNode=',ShowNode,' ShowChilds=',ShowChilds]);
    end;
    
    if ShowNode then begin
      // add a node to the TTreeView
      NodeData:=TViewNodeData.Create(CodeNode);
      CreateNodePath(ACodeTool,NodeData);
      NodeText:=GetCodeNodeDescription(ACodeTool,CodeNode);
      NodeImageIndex:=GetCodeNodeImage(ACodeTool,CodeNode);
      //if NodeText='TCodeExplorerView' then
      //  debugln(['TCodeExplorerView.CreateIdentifierNodes CodeNode=',CodeNode.DescAsString,' NodeText="',NodeText,'" Category=',dbgs(Category),' InFrontViewNode=',InFrontViewNode<>nil,' CurParentViewNode=',CurParentViewNode<>nil]);
      if InFrontViewNode<>nil then
        ViewNode:=CodeTreeview.Items.InsertObjectBehind(InFrontViewNode,NodeText,NodeData)
      else if CurParentViewNode<>nil then
        ViewNode:=CodeTreeview.Items.AddChildObject(CurParentViewNode,NodeText,NodeData)
      else
        ViewNode:=CodeTreeview.Items.AddObject(nil,NodeText,NodeData);
      ViewNode.ImageIndex:=NodeImageIndex;
      ViewNode.SelectedIndex:=NodeImageIndex;
      InFrontViewNode:=ViewNode;
    end else begin
      // do not add a node to the TTreeView
      ViewNode:=CurParentViewNode;
      AddImplementationNode(ACodeTool,CodeNode);
    end;
    if ShowChilds then
      CreateIdentifierNodes(ACodeTool,CodeNode.FirstChild,ViewNode);
    CodeNode:=CodeNode.NextBrother;
  end;
end;

procedure TCodeExplorerView.CreateDirectiveNodes(ADirectivesTool: TDirectivesTool;
  CodeNode: TCodeTreeNode; ParentViewNode: TTreeNode);
var
  NodeData: TViewNodeData;
  NodeText: String;
  ViewNode, InFrontViewNode: TTreeNode;
  NodeImageIndex: Integer;
  ShowNode: Boolean;
  ShowChilds: Boolean;
begin
  InFrontViewNode:=nil;
  while CodeNode<>nil do begin
    ShowNode:=true;
    ShowChilds:=true;
    
    // do not show root node
    if CodeNode.Desc=cdnRoot then begin
      ShowNode:=false;
    end;

    ViewNode:=ParentViewNode;
    if ShowNode then begin
      NodeData:=TViewNodeData.Create(CodeNode,false);
      NodeText:=GetDirectiveNodeDescription(ADirectivesTool,CodeNode);
      NodeImageIndex:=GetDirectiveNodeImage(CodeNode);
      if InFrontViewNode<>nil then
        ViewNode:=DirectivesTreeView.Items.InsertObjectBehind(
                                              InFrontViewNode,NodeText,NodeData)
      else if ParentViewNode<>nil then
        ViewNode:=DirectivesTreeView.Items.AddChildObject(
                                               ParentViewNode,NodeText,NodeData)
      else
        ViewNode:=DirectivesTreeView.Items.AddObject(nil,NodeText,NodeData);
      ViewNode.ImageIndex:=NodeImageIndex;
      ViewNode.SelectedIndex:=NodeImageIndex;
      InFrontViewNode:=ViewNode;
    end;
    if ShowChilds then
      CreateDirectiveNodes(ADirectivesTool,CodeNode.FirstChild,ViewNode);
    CodeNode:=CodeNode.NextBrother;
  end;
end;

procedure TCodeExplorerView.CreateObservations(Tool: TCodeTool);

  function AddCodeNode(f: TCEObserverCategory; CodeNode: TCodeTreeNode): TTreeNode;
  var
    Data: TViewNodeData;
    ObsTVNode: TTreeNode;
    NodeText: String;
    NodeImageIndCex: LongInt;
  begin
    ObsTVNode:=CreateObserverNode(Tool,f);
    if ObsTVNode.Count>=CodeObserverMaxNodes then
    begin
      fObserverCatOverflow[f]:=true;
      exit(nil);
    end;
    Data:=TViewNodeData.Create(CodeNode);
    NodeText:=GetCodeNodeDescription(Tool,CodeNode);
    NodeImageIndCex:=GetCodeNodeImage(Tool,CodeNode);
    Result:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
    Result.Data:=Data;
    Result.Text:=NodeText;
    Result.ImageIndex:=NodeImageIndCex;
    Result.SelectedIndex:=NodeImageIndCex;
  end;

  procedure CheckUnsortedClassMembers(ParentCodeNode: TCodeTreeNode);
  var
    LastNode: TCodeTreeNode;
    LastIdentifier: string;

    function NodeSorted(CodeNode: TCodeTreeNode): boolean;
    var
      p: PChar;
      Identifier: String;
    begin
      Result:=true;
      if (LastNode<>nil)
      //and (not CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.MixMethodsAndProperties)
      and (CodeNode.Desc<>LastNode.Desc) then begin
        // sort variables then methods and properties
        if (LastNode.Desc in [ctnProperty,ctnProcedure])
        and not (CodeNode.Desc in [ctnProperty,ctnProcedure])
        then begin
          Result:=false;
        end;
        if (LastNode.Desc in [ctnProperty])
        and (CodeNode.Desc in [ctnProcedure])
        and (not CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.MixMethodsAndProperties)
        then
          Result:=false;
      end;
      p:=Tool.GetNodeIdentifier(CodeNode);
      if p<>nil then
        Identifier:=GetIdentifier(p)
      else
        Identifier:='';
      if Result and (LastIdentifier<>'') and (Identifier<>'')
      and (CodeNode.Desc=LastNode.Desc) then begin
        // compare identifiers
        if CompareIdentifiers(PChar(Identifier),PChar(LastIdentifier))>0 then
        begin
          Result:=false;
        end;
      end;
      if not Result then begin
        AddCodeNode(cefcUnsortedClassMembers,CodeNode);
      end;
      LastNode:=CodeNode;
      LastIdentifier:=Identifier;
    end;

  var
    CodeNode: TCodeTreeNode;
  begin
    CodeNode:=ParentCodeNode.FirstChild;
    LastNode:=nil;
    while CodeNode<>nil do begin
      if CodeNode.Desc in AllIdentifierDefinitions then begin
        if not NodeSorted(CodeNode) then exit;
        // skip all variables in a group (e.g. Next,Prev:TNode)
        while CodeNode.FirstChild=nil do begin
          CodeNode:=CodeNode.NextBrother;
          if CodeNode=nil then exit;
        end;
      end else if CodeNode.Desc in [ctnProperty,ctnProcedure] then
      begin
        if not NodeSorted(CodeNode) then exit;
      end;
      CodeNode:=CodeNode.NextBrother;
    end;
  end;

var
  CodeNode: TCodeTreeNode;
  LineCnt: LongInt;
  i: integer;
  f: TCEObserverCategory;
  ObserverCats: TCEObserverCategories;
  ProcNode: TCodeTreeNode;
  ObsState: TCodeObserverStatementState;
  TVNode: TTreeNode;
begin
  CodeNode:=Tool.Tree.Root;
  ObserverCats:=CodeExplorerOptions.ObserverCategories;
  ObsState:=TCodeObserverStatementState.Create;
  try
    while CodeNode<>nil do begin
      case CodeNode.Desc of

      ctnBeginBlock:
        begin
          if (CodeNode.SubDesc and ctnsNeedJITParsing)<>0 then
          begin
            try
              Tool.BuildSubTreeForBeginBlock(CodeNode);
            except
            end;
          end;
          if (cefcLongProcs in ObserverCats)
          and (CodeNode.Parent.Desc=ctnProcedure) then begin
            LineCnt:=LineEndCount(Tool.Src,CodeNode.StartPos,CodeNode.EndPos,i);
            if LineCnt>=CodeExplorerOptions.LongProcLineCount then
            begin
              ProcNode:=CodeNode.Parent;
              TVNode:=AddCodeNode(cefcLongProcs,ProcNode);
              if Assigned(TVNode) then
                TVNode.Text:=TVNode.Text+' ['+IntToStr(LineCnt)+']';
            end;
          end;
          if (cefcEmptyProcs in ObserverCats)
          and (CodeNode.Parent.Desc=ctnProcedure) then
          begin
            Tool.MoveCursorToCleanPos(CodeNode.StartPos);
            Tool.ReadNextAtom;// read begin
            Tool.ReadNextAtom;
            if Tool.CurPos.Flag=cafEnd then begin
              // no code, maybe comments and directives (hidden code)
              ProcNode:=CodeNode.Parent;
              AddCodeNode(cefcEmptyProcs,ProcNode);
            end;
          end;
          if not CodeNode.HasParentOfType(ctnBeginBlock) then
          begin
            CreateObserverNodesForStatement(Tool,CodeNode,
                                    CodeNode.StartPos,CodeNode.EndPos,ObsState);
          end;
          if (cefcEmptyBlocks in ObserverCats)
          and CodeIsOnlySpace(Tool.Src,CodeNode.StartPos+length('begin'),
               CodeNode.EndPos-length('end')-1)
          then begin
            AddCodeNode(cefcEmptyBlocks,CodeNode);
          end;
        end;

      ctnAsmBlock:
        begin
          if (cefcEmptyBlocks in ObserverCats)
          and CodeIsOnlySpace(Tool.Src,CodeNode.StartPos+length('asm'),
               CodeNode.EndPos-length('end')-1)
          then begin
            AddCodeNode(cefcEmptyBlocks,CodeNode);
          end;
        end;

      ctnProcedure:
        begin
          if (cefcNestedProcs in ObserverCats) then
          begin
            i:=0;
            ProcNode:=CodeNode.FirstChild;
            while ProcNode<>nil do begin
              if ProcNode.Desc=ctnProcedure then
                inc(i);
              ProcNode:=ProcNode.NextBrother;
            end;
            if i>=CodeExplorerOptions.NestedProcCount then begin
              AddCodeNode(cefcNestedProcs,CodeNode);
            end;
          end;
        end;

      ctnParameterList:
        begin
          if (cefcLongParamLists in ObserverCats)
          and (CodeNode.HasParentOfType(ctnInterface))
          and (CodeNode.ChildCount>CodeExplorerOptions.LongParamListCount) then
          begin
            if (CodeNode.Parent.Desc=ctnProcedureHead)
            and (CodeNode.Parent.Parent.Desc=ctnProcedure) then
            begin
              ProcNode:=CodeNode.Parent.Parent;
              AddCodeNode(cefcLongParamLists,ProcNode);
            end;
          end;
        end;

      ctnProperty:
        begin
          if (cefcPublishedPropWithoutDefault in ObserverCats)
          and (CodeNode.Parent.Desc=ctnClassPublished) then
          begin
            if (not Tool.PropertyHasSpecifier(CodeNode,'default',false))
            and (Tool.PropertyHasSpecifier(CodeNode,'read',false))
            and (Tool.PropertyHasSpecifier(CodeNode,'write',false))
            then
              AddCodeNode(cefcPublishedPropWithoutDefault,CodeNode);
          end;
        end;

      ctnClassClassVar..ctnClassPublished:
        begin
          if (cefcUnsortedClassVisibility in ObserverCats)
          and (CodeNode.PriorBrother<>nil)
          and (CodeNode.PriorBrother.Desc in AllClassBaseSections)
          and (CodeNode.PriorBrother.Desc>CodeNode.Desc)
          then begin
            if (CodeNode.PriorBrother.Desc=ctnClassPublished)
            and ((CodeNode.PriorBrother.PriorBrother=nil)
               or (not (CodeNode.PriorBrother.PriorBrother.Desc in AllClassBaseSections)))
            then begin
              // the first section can be published
            end else begin
              // the prior section was more visible
              AddCodeNode(cefcUnsortedClassVisibility,CodeNode);
            end;
          end;
          if (cefcUnsortedClassMembers in ObserverCats)
          then
            CheckUnsortedClassMembers(CodeNode);
          if (cefcEmptyClassSections in ObserverCats)
          and (CodeNode.FirstChild=nil) then
          begin
            if (CodeNode.Desc=ctnClassPublished)
            and ((CodeNode.PriorBrother=nil)
               or (not (CodeNode.PriorBrother.Desc in AllClassBaseSections)))
            then begin
              // the first section can be empty
            end else begin
              // empty class section
              AddCodeNode(cefcEmptyClassSections,CodeNode);
            end;
          end;
        end;

      end;
      CodeNode:=CodeNode.Next;
    end;

    if cefcToDos in ObserverCats then
      FindObserverTodos(Tool);
  finally
    ObsState.Free;
  end;

  // add numbers
  for f:=low(TCEObserverCategory) to high(TCEObserverCategory) do
  begin
    if fObserverCatNodes[f]=nil then continue;
    if fObserverCatOverflow[f] then
      fObserverCatNodes[f].Text:=
        fObserverCatNodes[f].Text+' ('+IntToStr(fObserverCatNodes[f].Count)+'+)'
    else
      fObserverCatNodes[f].Text:=
        fObserverCatNodes[f].Text+' ('+IntToStr(fObserverCatNodes[f].Count)+')';
  end;
end;

function TCodeExplorerView.CreateObserverNode(Tool: TCodeTool;
  f: TCEObserverCategory): TTreeNode;
var
  Data: TViewNodeData;
begin
  if fObserverCatNodes[f] = nil then
  begin
    if fObserverNode = nil then
    begin
      fObserverNode:=CodeTreeview.Items.Add(nil, lisCodeObserver);
      Data:=TViewNodeData.Create(Tool.Tree.Root);
      Data.Desc:=ctnNone;
      Data.StartPos:=Tool.SrcLen;
      fObserverNode.Data:=Data;
      fObserverNode.ImageIndex:=ImgIDSection;
      fObserverNode.SelectedIndex:=ImgIDSection;
    end;
    fObserverCatNodes[f]:=CodeTreeview.Items.AddChild(fObserverNode,
                            CodeExplorerLocalizedString(f));
    Data:=TViewNodeData.Create(Tool.Tree.Root);
    Data.Desc:=ctnNone;
    Data.StartPos:=Tool.SrcLen;
    fObserverCatNodes[f].Data:=Data;
    fObserverCatNodes[f].ImageIndex:=ImgIDHint;
    fObserverCatNodes[f].SelectedIndex:=ImgIDHint;
  end;
  Result:=fObserverCatNodes[f];
end;

procedure TCodeExplorerView.CreateObserverNodesForStatement(Tool: TCodeTool;
  CodeNode: TCodeTreeNode;
  StartPos, EndPos: integer; ObserverState: TCodeObserverStatementState);
var
  Data: TViewNodeData;
  ObsTVNode: TTreeNode;
  NodeText: String;
  NodeImageIndex: LongInt;
  TVNode: TTreeNode;
  ProcNode: TCodeTreeNode;
  OldPos: LongInt;
  CurAtom, Last1Atom, Last2Atom: TCommonAtomFlag;
  FuncName: string;
  Atom: TAtomPosition;
  c1: Char;
  Typ: TCodeObsStackItemType;
  CheckWrongIndentation: boolean;
  FindUnnamedConstants: boolean;

  procedure CheckSubStatement(CanBeEqual: boolean);
  var
    StatementStartPos: Integer;
    LastIndent: LongInt;
    Indent: LongInt;
    NeedUndo: Boolean;
    LastPos: LongInt;
  begin
    //DebugLn(['CheckSubStatement START=',Tool.GetAtom,' ',CheckWrongIndentation,' ',ObserverState.StatementStartPos,' ',dbgstr(copy(Tool.Src,ObserverState.StatementStartPos,15))]);
    if not CheckWrongIndentation then exit;
    StatementStartPos:=ObserverState.StatementStartPos;
    if StatementStartPos<1 then exit;
    LastPos:=Tool.CurPos.StartPos;
    Tool.ReadNextAtom;
    if PositionsInSameLine(Tool.Src,LastPos,Tool.CurPos.StartPos) then exit;
    NeedUndo:=true;
    //DebugLn(['CheckSubStatement NEXT=',Tool.GetAtom,' NotSameLine=',not PositionsInSameLine(Tool.Src,StatementStartPos,Tool.CurPos.StartPos),' ',dbgstr(copy(Tool.Src,Tool.CurPos.StartPos,15))]);
    if (Tool.CurPos.Flag<>cafNone)
    and (not PositionsInSameLine(Tool.Src,StatementStartPos,Tool.CurPos.StartPos))
    then begin
      LastIndent:=GetLineIndent(Tool.Src,StatementStartPos);
      Indent:=GetLineIndent(Tool.Src,Tool.CurPos.StartPos);
      //DebugLn(['CheckSubStatement OTHER LINE ',Tool.GetAtom,' ',LastIndent,' ',Indent]);
      if (Indent<LastIndent)
      or ((Indent=LastIndent) and (not CanBeEqual) and (not Tool.UpAtomIs('BEGIN')))
      then begin
        //DebugLn(['CheckSubStatement START=',CheckWrongIndentation,' ',ObserverState.StatementStartPos,' ',dbgstr(copy(Tool.Src,ObserverState.StatementStartPos,15))]);
        //DebugLn(['CheckSubStatement NEXT=',Tool.GetAtom,' NotSameLine=',not PositionsInSameLine(Tool.Src,StatementStartPos,Tool.CurPos.StartPos),' ',dbgstr(copy(Tool.Src,Tool.CurPos.StartPos,15))]);
        //DebugLn(['CheckSubStatement OTHER LINE LastIndent=',LastIndent,' Indent=',Indent]);
        // add wrong indentation
        ObsTVNode:=CreateObserverNode(Tool,cefcWrongIndentation);
        if ObsTVNode.Count>=CodeObserverMaxNodes then
        begin
          fObserverCatOverflow[cefcWrongIndentation]:=true;
        end else begin
          Data:=TViewNodeData.Create(CodeNode);
          Data.Desc:=ctnConstant;
          Data.SubDesc:=ctnsNone;
          Data.StartPos:=Tool.CurPos.StartPos;
          Data.EndPos:=Tool.CurPos.EndPos;
          NodeText:=Tool.GetAtom;
          // add some context information
          Tool.UndoReadNextAtom;
          NeedUndo:=false;
          ProcNode:=CodeNode;
          while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
            ProcNode:=ProcNode.Parent;
          if ProcNode<>nil then begin
            OldPos:=Tool.CurPos.EndPos;
            NodeText:=Format(lisCEIn, [NodeText, Tool.ExtractProcName(ProcNode, [
              phpWithoutClassName])]);
            Tool.MoveCursorToCleanPos(OldPos);
          end;
          NodeImageIndex:=ImgIDConst;
          TVNode:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
          TVNode.Data:=Data;
          TVNode.Text:=NodeText;
          TVNode.ImageIndex:=NodeImageIndex;
          TVNode.SelectedIndex:=NodeImageIndex;
        end;
      end;
    end;
    if NeedUndo then
      Tool.UndoReadNextAtom;
  end;

begin
  if EndPos>Tool.SrcLen then EndPos:=Tool.SrcLen+1;
  if (StartPos<1) or (StartPos>=EndPos) then exit;
  CheckWrongIndentation:=cefcWrongIndentation in CodeExplorerOptions.ObserverCategories;
  FindUnnamedConstants:=cefcUnnamedConsts in CodeExplorerOptions.ObserverCategories;
  if (not FindUnnamedConstants) and (not CheckWrongIndentation) then exit;
  Tool.MoveCursorToCleanPos(StartPos);
  Last1Atom:=cafNone;
  Last2Atom:=cafNone;
  ObserverState.Reset;
  while Tool.CurPos.StartPos<EndPos do begin
    CurAtom:=cafNone;
    if ObserverState.StatementStartPos<1 then
    begin
      // start of statement
      ObserverState.StatementStartPos:=Tool.CurPos.StartPos;
    end;

    c1:=Tool.Src[Tool.CurPos.StartPos];
    case c1 of
    ';':
      begin
        // end of statement
        ObserverState.StatementStartPos:=0;
      end;

    '''','#','0'..'9','$','%':
      begin
        // a constant
        if not FindUnnamedConstants then begin
          // ignore
        end else if (ObserverState.IgnoreConstLevel>=0)
        and (ObserverState.IgnoreConstLevel>=ObserverState.StackPtr)
        then begin
          // ignore range
        end else if Tool.AtomIsEmptyStringConstant then begin
          // ignore empty string constant ''
        end else if Tool.AtomIsCharConstant
        and (not CodeExplorerOptions.ObserveCharConst) then
        begin
          // ignore char constants
        end else if CodeExplorerOptions.COIgnoreConstant(@Tool.Src[Tool.CurPos.StartPos])
        then begin
          // ignore user defined constants
        end else begin
          // add constant
          ObsTVNode:=CreateObserverNode(Tool,cefcUnnamedConsts);
          if ObsTVNode.Count>=CodeObserverMaxNodes then
          begin
            fObserverCatOverflow[cefcUnnamedConsts]:=true;
            break;
          end else begin
            Data:=TViewNodeData.Create(CodeNode);
            Data.Desc:=ctnConstant;
            Data.SubDesc:=ctnsNone;
            Data.StartPos:=Tool.CurPos.StartPos;
            Data.EndPos:=Tool.CurPos.EndPos;
            NodeText:=Tool.GetAtom;
            // add some context information
            ProcNode:=CodeNode;
            while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
              ProcNode:=ProcNode.Parent;
            if ProcNode<>nil then begin
              OldPos:=Tool.CurPos.EndPos;
              NodeText:=Format(lisCEIn, [NodeText, Tool.ExtractProcName(ProcNode, [
                phpWithoutClassName])]);
              Tool.MoveCursorToCleanPos(OldPos);
            end;
            NodeImageIndex:=ImgIDConst;
            TVNode:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
            TVNode.Data:=Data;
            TVNode.Text:=NodeText;
            TVNode.ImageIndex:=NodeImageIndex;
            TVNode.SelectedIndex:=NodeImageIndex;
          end;
        end;
      end;

    '.':
      CurAtom:=cafPoint;

    '(','[':
      begin
        if c1='(' then
          ObserverState.Push(cositRoundBracketOpen,Tool.CurPos.StartPos)
        else
          ObserverState.Push(cositEdgedBracketOpen,Tool.CurPos.StartPos);
        if (Last1Atom=cafWord)
        and (ObserverState.IgnoreConstLevel<0) then
        begin
          Atom:=Tool.LastAtoms.GetPriorAtom;
          FuncName:=copy(Tool.Src,Atom.StartPos,Atom.EndPos-Atom.StartPos);
          if Last2Atom=cafPoint then
            FuncName:='.'+FuncName;
          if CodeExplorerOptions.COIgnoreConstInFunc(FuncName) then
          begin
            // skip this function call
            ObserverState.IgnoreConstLevel:=ObserverState.StackPtr;
          end;
        end;
      end;

    ')',']':
      begin
        while ObserverState.StackPtr>0 do
        begin
          Typ:=ObserverState.TopType;
          if Typ in [cositRoundBracketOpen,cositEdgedBracketOpen]
          then begin
            ObserverState.Pop;
            // normally brackets must match () []
            // but during editing often the brackets don't match
            // for example [( ]
            // skip silently
            if (Typ=cositRoundBracketOpen)=(c1='(') then break;
          end else begin
            // missing bracket close
            break;
          end;
        end;
      end;

    ':':
      ObserverState.StatementStartPos:=-1;

    '_','a'..'z','A'..'Z':
      begin
        CurAtom:=cafWord;
        if Tool.UpAtomIs('END') then
        begin
          while ObserverState.StackPtr>0 do
          begin
            Typ:=ObserverState.Pop;
            if Typ in [cositBegin,cositFinally,cositExcept,cositCase,cositCaseElse]
            then
              break;
          end;
          ObserverState.StatementStartPos:=-1;
        end
        else if Tool.UpAtomIs('BEGIN') then
          ObserverState.Push(cositBegin,Tool.CurPos.StartPos)
        else if Tool.UpAtomIs('REPEAT') then
          ObserverState.Push(cositRepeat,Tool.CurPos.StartPos)
        else if Tool.UpAtomIs('TRY') then
          ObserverState.Push(cositTry,Tool.CurPos.StartPos)
        else if Tool.UpAtomIs('FINALLY') or Tool.UpAtomIs('EXCEPT') then
        begin
          while ObserverState.StackPtr>0 do
          begin
            Typ:=ObserverState.Pop;
            if Typ=cositTry then
              break;
          end;
          ObserverState.StatementStartPos:=-1;
          if Tool.UpAtomIs('FINALLY') then
            ObserverState.Push(cositFinally,Tool.CurPos.StartPos)
          else
            ObserverState.Push(cositExcept,Tool.CurPos.StartPos);
        end
        else if Tool.UpAtomIs('CASE') then
        begin
          ObserverState.Push(cositCase,Tool.CurPos.StartPos);
          ObserverState.StatementStartPos:=Tool.CurPos.StartPos;
        end
        else if Tool.UpAtomIs('ELSE') then
        begin
          if ObserverState.TopType=cositCase then
          begin
            ObserverState.Pop;
            ObserverState.Push(cositCaseElse,Tool.CurPos.StartPos);
          end;
          ObserverState.StatementStartPos:=-1;
          CheckSubStatement(false);
        end
        else if Tool.UpAtomIs('DO') or Tool.UpAtomIs('THEN') then
          CheckSubStatement(false)
        else if Tool.UpAtomIs('OF') then
          CheckSubStatement(true);
      end;
    end;
    // read next atom
    Last2Atom:=Last1Atom;
    Last1Atom:=CurAtom;
    Tool.ReadNextAtom;
  end;
end;

procedure TCodeExplorerView.FindObserverTodos(Tool: TCodeTool);
var
  Src: String;
  p: Integer;
  CommentEndPos: LongInt;
  MagicStartPos: integer;
  TextStartPos: integer;
  TextEndPos: integer;
  l: Integer;
  SrcLen: Integer;
  Data: TViewNodeData;
  ObsTVNode: TTreeNode;
  NodeText: String;
  NodeImageIndCex: LongInt;
  TVNode: TTreeNode;
begin
  Src:=Tool.Src;
  SrcLen:=length(Src);
  p:=1;
  repeat
    p:=FindNextComment(Src,p);
    if p>SrcLen then break;
    CommentEndPos:=FindCommentEnd(Src,p,Tool.Scanner.NestedComments);
    if GetToDoComment(Src,p,CommentEndPos,MagicStartPos,TextStartPos,TextEndPos)
    then begin
      // add todo
      ObsTVNode:=CreateObserverNode(Tool,cefcToDos);
      if fObserverNode.Count>=CodeObserverMaxNodes then begin
        fObserverCatOverflow[cefcToDos]:=true;
        break;
      end else begin
        Data:=TViewNodeData.Create(Tool.Tree.Root,false);
        Data.Desc:=ctnConstant;
        Data.SubDesc:=ctnsNone;
        Data.StartPos:=p;
        Data.EndPos:=MagicStartPos;
        l:=TextEndPos-TextStartPos;
        if l>20 then l:=20;
        NodeText:=TrimCodeSpace(copy(Src,TextStartPos,l));
        NodeImageIndCex:=ImgIDConst;
        TVNode:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
        TVNode.Data:=Data;
        TVNode.Text:=NodeText;
        TVNode.ImageIndex:=NodeImageIndCex;
        TVNode.SelectedIndex:=NodeImageIndCex;
      end;
    end;
    p:=CommentEndPos;
  until p>SrcLen;
end;

procedure TCodeExplorerView.CreateSurrounding(Tool: TCodeTool);

  function CTNodeIsEnclosing(CTNode: TCodeTreeNode; p: integer): boolean;
  var
    NextCTNode: TCodeTreeNode;
  begin
    Result:=false;
    if (p<CTNode.StartPos) or (p>CTNode.EndPos) then exit;
    if (p=CTNode.EndPos) then begin
      NextCTNode:=CTNode.NextSkipChilds;
      if (NextCTNode<>nil) and (NextCTNode.StartPos<=p) then exit;
    end;
    Result:=true;
  end;

  procedure CreateSubNodes(ParentTVNode: TTreeNode; CTNode: TCodeTreeNode;
    p: integer);
  var
    ChildCTNode: TCodeTreeNode;
    ChildData: TViewNodeData;
    ChildTVNode: TTreeNode;
    AddChilds: Boolean;
    Add: Boolean;
    CurParentTVNode: TTreeNode;
  begin
    ChildCTNode:=CTNode.FirstChild;
    while ChildCTNode<>nil do
    begin
      AddChilds:=false;
      Add:=false;
      if CTNodeIsEnclosing(ChildCTNode,p) then begin
        AddChilds:=true;
        Add:=true;
        if ChildCTNode.Desc in AllClasses then
          Add:=false;
      end else if (CTNode.Desc=ctnProcedure)
      and (ChildCTNode.Desc<>ctnProcedureHead) then begin
        Add:=true
      end;

      CurParentTVNode:=ParentTVNode;
      if Add then
      begin
        ChildData:=TViewNodeData.Create(ChildCTNode,false);
        ChildTVNode:=CodeTreeview.Items.AddChildObject(
                     ParentTVNode,GetCodeNodeDescription(Tool,ChildCTNode),ChildData);
        ChildTVNode.ImageIndex:=GetCodeNodeImage(Tool,ChildCTNode);
        ChildTVNode.SelectedIndex:=ChildTVNode.ImageIndex;
        CurParentTVNode:=ChildTVNode;
      end else
        ChildTVNode:=nil;
      if AddChilds then
      begin
        CreateSubNodes(CurParentTVNode,ChildCTNode,p);
        if ChildTVNode<>nil then
          ChildTVNode.Expanded:=true;
      end;
      ChildCTNode:=ChildCTNode.NextBrother;
    end;
  end;

var
  CodeNode: TCodeTreeNode;
  Data: TViewNodeData;
  TVNode: TTreeNode;
  CurPos: TCodeXYPosition;
  p: integer;
begin
  if fSurroundingNode = nil then
  begin
    fSurroundingNode:=CodeTreeview.Items.Add(nil, lisCESurrounding);
    Data:=TViewNodeData.Create(Tool.Tree.Root,false);
    Data.Desc:=ctnNone;
    Data.StartPos:=Tool.SrcLen;
    fSurroundingNode.Data:=Data;
    fSurroundingNode.ImageIndex:=ImgIDSection;
    fSurroundingNode.SelectedIndex:=ImgIDSection;
  end;

  CurPos.Code:=FLastCode;
  CurPos.X:=FLastCodeXY.X;
  CurPos.Y:=FLastCodeXY.Y;
  fLastCodeTool.CaretToCleanPos(CurPos,p);

  // add all top lvl sections
  CodeNode:=Tool.Tree.Root;
  while CodeNode<>nil do begin
    Data:=TViewNodeData.Create(CodeNode,false);
    TVNode:=CodeTreeview.Items.AddChildObject(
                       fSurroundingNode,GetCodeNodeDescription(Tool,CodeNode),Data);
    TVNode.ImageIndex:=GetCodeNodeImage(Tool,CodeNode);
    TVNode.SelectedIndex:=TVNode.ImageIndex;
    if CTNodeIsEnclosing(CodeNode,p) then
      CreateSubNodes(TVNode,CodeNode,p);
    TVNode.Expanded:=true;

    CodeNode:=CodeNode.NextBrother;
  end;
  fSurroundingNode.Expanded:=true;
end;

procedure TCodeExplorerView.DeleteTVNode(TVNode: TTreeNode);
var
  c: TCodeExplorerCategory;
  oc: TCEObserverCategory;
begin
  if TVNode=nil then exit;
  if TVNode.Data<>nil then begin
    if (TObject(TVNode.Data) is TViewNodeData) and (fCodeSortedForStartPos<>nil)
    then
      fCodeSortedForStartPos.Remove(TVNode);
    TObject(TVNode.Data).Free;
    TVNode.Data:=nil;
  end;
  if TVNode.Parent=nil then begin
    if TVNode=fObserverNode then
      fObserverNode:=nil
    else if TVNode=fSurroundingNode then
      fSurroundingNode:=nil
    else begin
      for c:=low(fCategoryNodes) to high(fCategoryNodes) do
        if fCategoryNodes[c]=TVNode then
          fCategoryNodes[c]:=nil;
    end;
  end else if TVNode=fObserverNode then begin
    for oc:=low(fObserverCatNodes) to high(fObserverCatNodes) do
      if fObserverCatNodes[oc]=TVNode then
        fObserverCatNodes[oc]:=nil;
  end;
  TVNode.Delete;
end;

procedure TCodeExplorerView.SetCodeFilter(const AValue: string);
begin
  if CodeFilter=AValue then exit;
  CodeFilterEdit.Text:=AValue;
  CodeFilterChanged;
end;

procedure TCodeExplorerView.SetCurrentPage(const AValue: TCodeExplorerPage);
begin
  case AValue of
  cepCode:       MainNotebook.ActivePage:=CodePage;
  cepDirectives: MainNotebook.ActivePage:=DirectivesPage;
  end;
end;

procedure TCodeExplorerView.SetDirectivesFilter(const AValue: string);
begin
  if DirectivesFilter=AValue then exit;
  DirectivesFilterEdit.Text:=AValue;
  DirectivesFilterChanged;
end;

procedure TCodeExplorerView.SetMode(AMode: TCodeExplorerMode);
begin
  if FMode=AMode then exit;
  FMode:=AMode;
  UpdateMode;
end;

procedure TCodeExplorerView.UpdateMode;
begin
  if FMode=cemCategory
  then begin
    IDEImages.AssignImage(CodeModeSpeedButton, 'show_category');
    CodeModeSpeedButton.Hint:=lisCEModeShowSourceNodes;
  end
  else begin
    IDEImages.AssignImage(CodeModeSpeedButton, 'show_source');
    CodeModeSpeedButton.Hint:=lisCEModeShowCategories;
  end;
  Refresh(true);
end;

procedure TCodeExplorerView.UpdateCaption;
var
  s: String;
begin
  s:=lisMenuViewCodeExplorer;
  if (CodeExplorerOptions.Refresh=cerManual) and (FCodeFilename<>'') then
    s+=' - ' + ExtractFileName(FCodeFilename);
  Caption:=s;
end;

function TCodeExplorerView.OnExpandedStateGetNodeText(Node: TTreeNode): string;
var
  p: Integer;
begin
  Result:=Node.Text;
  if Result='' then exit;
  p:=length(Result);
  if Result[p]=')' then begin
    dec(p);
    while (p>1) and (Result[p] in ['+','0'..'9']) do dec(p);
    if (p>1) and (Result[p]='(') then begin
      repeat
        dec(p);
      until (p=0) or (Result[p]<>' ');
      SetLength(Result,p);
    end;
  end;
end;

procedure TCodeExplorerView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

procedure TCodeExplorerView.ApplyCodeFilter;
var
  ANode, NextNode: TTreeNode;
  TheFilter: String;
begin
  TheFilter:=GetCodeFilter;
  //DebugLn(['TCodeExplorerView.ApplyCodeFilter ====================="',TheFilter,'"']);
  FLastCodeFilter:=TheFilter;
  CodeTreeview.BeginUpdate;
  ANode:=CodeTreeview.Items.GetFirstNode;
  while ANode<>nil do begin
    NextNode:=ANode.GetNextSibling;
    FilterNode(ANode,TheFilter,True);
    ANode:=NextNode;
  end;
  CodeTreeview.EndUpdate;
end;

procedure TCodeExplorerView.ApplyDirectivesFilter;
var
  ANode, NextNode: TTreeNode;
  TheFilter: String;
begin
  TheFilter:=GetDirectivesFilter;
  //DebugLn(['TCodeExplorerView.ApplyDirectivesFilter ====================="',TheFilter,'"']);
  FLastDirectivesFilter:=TheFilter;
  DirectivesTreeView.BeginUpdate;
  //DirectivesTreeView.Options:=DirectivesTreeView.Options+[tvoAllowMultiselect];
  ANode:=DirectivesTreeView.Items.GetFirstNode;
  while ANode<>nil do begin
    NextNode:=ANode.GetNextSibling;
    FilterNode(ANode,TheFilter,False);
    ANode:=NextNode;
  end;
  DirectivesTreeView.EndUpdate;
end;

procedure TCodeExplorerView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCodeExplorerView.EndUpdate;
var
  CurPage: TCodeExplorerPage;
begin
  if FUpdateCount<=0 then
    RaiseGDBException('TCodeExplorerView.EndUpdate');
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    CurPage:=CurrentPage;
    if (CurPage=cepCode) and (cevCodeRefreshNeeded in FFlags) then
      RefreshCode(true);
    if (CurPage=cepDirectives) and (cevDirectivesRefreshNeeded in FFlags) then
      RefreshDirectives(true);
  end;
end;

procedure TCodeExplorerView.CheckOnIdle;
begin
  Include(FFlags,cevCheckOnIdle);
end;

procedure TCodeExplorerView.Refresh(OnlyVisible: boolean);
begin
  Exclude(FFlags,cevCheckOnIdle);
  //debugln(['TCodeExplorerView.Refresh ']);
  RefreshCode(OnlyVisible);
  RefreshDirectives(OnlyVisible);
end;

procedure TCodeExplorerView.RefreshCode(OnlyVisible: boolean);

  procedure AutoExpandNodes;
  var
    TVNode: TTreeNode;
    Data: TViewNodeData;
    ShowInterfaceImplementation: Boolean;
  begin
    ShowInterfaceImplementation:=(Mode <> cemCategory)
      or (not (cecSurrounding in CodeExplorerOptions.Categories));
    if not ShowInterfaceImplementation then exit;
    TVNode:=CodeTreeview.Items.GetFirstNode;
    while TVNode<>nil do begin
      Data:=TViewNodeData(TVNode.Data);
      if Data.Desc in [ctnInterface,ctnImplementation] then begin
        // auto expand interface and implementation nodes
        TVNode.Expanded:=true;
      end;
      TVNode:=TVNode.GetNext;
    end;
  end;

  procedure DeleteDuplicates(ACodeTool: TCodeTool);

    function IsForward(Data: TViewNodeData): boolean;
    begin
      if Data.Desc=ctnProcedure then
      begin
        if (Data.CTNode.Parent<>nil) and (Data.CTNode.Parent.Desc=ctnInterface)
        then
          exit(true);
        if ACodeTool.NodeIsForwardProc(Data.CTNode) then
          exit(true);
      end;
      Result:=false;
    end;

  var
    TVNode: TTreeNode;
    NextTVNode: TTreeNode;
    Data: TViewNodeData;
    NextData: TViewNodeData;
    DeleteNode: Boolean;
    DeleteNextNode: Boolean;
  begin
    TVNode:=CodeTreeview.Items.GetFirstNode;
    while TVNode<>nil do begin
      NextTVNode:=TVNode.GetNext;
      if NextTVNode=nil then break;
      if (TVNode.Parent<>nil) and (NextTVNode.Parent=TVNode.Parent) then
      begin
        DeleteNode:=false;
        DeleteNextNode:=false;
        if (CompareTextIgnoringSpace(TVNode.Text,NextTVNode.Text,false)=0) then
        begin
          Data:=TViewNodeData(TVNode.Data);
          NextData:=TViewNodeData(NextTVNode.Data);
          if IsForward(Data) then
            DeleteNode:=true;
          if IsForward(NextData) then
            DeleteNextNode:=true;
        end;
        if DeleteNextNode then begin
          DeleteTVNode(NextTVNode);
          NextTVNode:=TVNode;
        end else if DeleteNode then begin
          NextTVNode:=TVNode.GetNextSkipChildren;
          DeleteTVNode(TVNode);
        end;
      end;
      TVNode:=NextTVNode;
    end;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  ACodeTool: TCodeTool;
  SrcEdit: TSourceEditorInterface;
  Filename: String;
  Code: TCodeBuffer;
  NewXY: TPoint;
  OnlyXYChanged: Boolean;
  CurFollowNode: Boolean;
  TVNode: TTreeNode;
  TheFilter: String;
begin
  if (FUpdateCount>0)
  or (OnlyVisible and ((CurrentPage<>cepCode) or (not IsVisible))) then begin
    Include(FFlags,cevCodeRefreshNeeded);
    exit;
  end;
  Exclude(FFlags,cevCodeRefreshNeeded);
  fLastCodeTool:=nil;
  OldExpanded:=nil;
  try
    Include(FFlags,cevRefreshing);
    
    // get the current editor
    if not LazarusIDE.BeginCodeTools then exit;
    SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit=nil then exit;
    // get the codetool for the current editor
    Filename:=SrcEdit.FileName;
    Code:=CodeToolBoss.FindFile(Filename);
    if Code=nil then exit;
    ACodeTool:=nil;
    // ToDo: check if something changed (file stamp, codebuffer stamp, defines stamp)
    CodeToolBoss.Explore(Code,ACodeTool,false);
    if ACodeTool=nil then exit;

    fLastCodeTool:=ACodeTool;
    FLastCode:=Code;

    // check for changes in the codetool
    TheFilter:=GetCodeFilter;
    OnlyXYChanged:=false;
    if (ACodeTool=nil) then begin
      if (FCodeFilename='') then begin
        // still no tool
        exit;
      end;
      //debugln(['TCodeExplorerView.RefreshCode no tool']);
    end else begin
      if CompareText(FLastCodeFilter,TheFilter)<>0 then begin
        // debugln(['TCodeExplorerView.RefreshCode filter changed']);
      end else if not FLastCodeValid then begin
        //debugln(['TCodeExplorerView.RefreshCode last code not valid'])
      end else if ACodeTool.MainFilename<>FCodeFilename then begin
        //debugln(['TCodeExplorerView.RefreshCode File changed ',ACodeTool.MainFilename,' ',FCodeFilename])
      end else if (ACodeTool.Scanner=nil) then begin
        //debugln(['TCodeExplorerView.RefreshCode Scanner=nil'])
      end else if (ACodeTool.Scanner.ChangeStep<>FLastCodeChangeStep) then begin
        //debugln(['TCodeExplorerView.RefreshCode Scanner changed ',ACodeTool.Scanner.ChangeStep,' ',FLastCodeChangeStep])
      end else if (Mode<>FLastMode) then begin
        //debugln(['TCodeExplorerView.RefreshCode Mode changed ',ord(Mode),' ',ord(FLastMode)])
      end else if (fLastCodeOptionsChangeStep<>CodeExplorerOptions.ChangeStep) then begin
        //debugln(['TCodeExplorerView.RefreshCode Options changed ',fLastCodeOptionsChangeStep,' ',CodeExplorerOptions.ChangeStep])
      end else begin
        // still the same source and options
        OnlyXYChanged:=true;
        if not CodeExplorerOptions.FollowCursor then
          exit;
        NewXY:=SrcEdit.CursorTextXY;
        //debugln(['TCodeExplorerView.RefreshCode ',dbgs(NewXY),' ',dbgs(FLastCodeXY)]);
        if ComparePoints(NewXY,FLastCodeXY)=0 then begin
          // still the same cursor position
          exit;
        end;
        FLastCodeXY:=NewXY;
      end;
    end;

    if OnlyXYChanged then begin
      SelectCodePosition(Code,FLastCodeXY.X,FLastCodeXY.Y);
    end else begin

      FLastCodeValid:=true;
      FLastMode:=Mode;
      fLastCodeOptionsChangeStep:=CodeExplorerOptions.ChangeStep;
      FLastCodeXY:=SrcEdit.CursorTextXY;
      FLastCodeFilter:=TheFilter;
      // remember the codetools ChangeStep
      if ACodeTool<>nil then begin
        FCodeFilename:=ACodeTool.MainFilename;
        if ACodeTool.Scanner<>nil then
          FLastCodeChangeStep:=ACodeTool.Scanner.ChangeStep;
      end else
        FCodeFilename:='';

      if fCodeSortedForStartPos<>nil then
        fCodeSortedForStartPos.Clear;
      fNodesWithPath.Clear;

      //DebugLn(['TCodeExplorerView.RefreshCode ',FCodeFilename]);

      CurFollowNode:=CodeExplorerOptions.FollowCursor and (not Active);

      // start updating the CodeTreeView
      CodeTreeview.BeginUpdate;
      if not CurFollowNode then
        OldExpanded:=TTreeNodeExpandedState.Create(CodeTreeView,@OnExpandedStateGetNodeText);

      ClearCodeTreeView;

      if (ACodeTool<>nil) and (ACodeTool.Tree<>nil) and (ACodeTool.Tree.Root<>nil)
      then begin
        CreateIdentifierNodes(ACodeTool,ACodeTool.Tree.Root,nil);
        if (Mode = cemCategory) then
        begin
          if (cecCodeObserver in CodeExplorerOptions.Categories) then
            CreateObservations(ACodeTool);
          if (cecSurrounding in CodeExplorerOptions.Categories) then
            CreateSurrounding(ACodeTool);
        end;
      end;

      // sort nodes
      fSortCodeTool:=ACodeTool;
      TVNode:=CodeTreeview.Items.GetFirstNode;
      while TVNode<>nil do begin
        if (TVNode.GetFirstChild<>nil)
        and (TObject(TVNode.Data) is TViewNodeData)
        and TViewNodeData(TVNode.Data).SortChildren then begin
          TVNode.CustomSort(@CompareCodeNodes);
        end;
        TVNode:=TVNode.GetNext;
      end;

      DeleteDuplicates(ACodeTool);

      // restore old expanded state
      if not CurFollowNode then
        AutoExpandNodes;

      BuildCodeSortedForStartPos;
      // clear references to the TCodeTreeNode to avoid dangling pointers
      ClearCTNodes(CodeTreeview);

      ApplyCodeFilter;

      if OldExpanded<>nil then
        OldExpanded.Apply(CodeTreeView,false);

      if CurFollowNode then
        SelectCodePosition(Code,FLastCodeXY.X,FLastCodeXY.Y);

      CodeTreeview.EndUpdate;
    end;
    UpdateCaption;
    if HostDockSite <> nil then
      HostDockSite.UpdateDockCaption();
  finally
    Exclude(FFlags,cevRefreshing);
    OldExpanded.Free;
  end;
end;

procedure TCodeExplorerView.RefreshDirectives(OnlyVisible: boolean);
var
  ADirectivesTool: TDirectivesTool;
  OldExpanded: TTreeNodeExpandedState;
begin
  if (FUpdateCount>0)
  or (OnlyVisible and ((CurrentPage<>cepDirectives) or (not IsVisible))) then
  begin
    Include(FFlags,cevDirectivesRefreshNeeded);
    exit;
  end;
  Exclude(FFlags,cevDirectivesRefreshNeeded);

  try
    Include(FFlags,cevRefreshing);

    // get the directivestool with the updated tree
    ADirectivesTool:=nil;
    if Assigned(OnGetDirectivesTree) then
      OnGetDirectivesTree(Self,ADirectivesTool);

    // check for changes in the codetools
    if (ADirectivesTool=nil) then begin
      if (FDirectivesFilename='') then begin
        // still no tool
        exit;
      end;
    end else begin
      if (ADirectivesTool.Code.Filename=FDirectivesFilename)
      and (ADirectivesTool.ChangeStep=FLastDirectivesChangeStep) then begin
        // still the same source
        exit;
      end;
    end;

    // remember the codetools ChangeStep
    if ADirectivesTool<>nil then begin
      FDirectivesFilename:=ADirectivesTool.Code.Filename;
      FLastDirectivesChangeStep:=ADirectivesTool.ChangeStep;
    end else
      FDirectivesFilename:='';
      
    //DebugLn(['TCodeExplorerView.RefreshDirectives ',FDirectivesFilename]);

    // start updating the DirectivesTreeView
    DirectivesTreeView.BeginUpdate;
    OldExpanded:=TTreeNodeExpandedState.Create(DirectivesTreeView);

    ClearDirectivesTreeView;
    if (ADirectivesTool<>nil) and (ADirectivesTool.Tree<>nil)
    and (ADirectivesTool.Tree.Root<>nil) then
    begin
      CreateDirectiveNodes(ADirectivesTool,ADirectivesTool.Tree.Root,nil);
    end;

    // restore old expanded state
    OldExpanded.Apply(DirectivesTreeView);
    OldExpanded.Free;
    ClearCTNodes(DirectivesTreeView);

    ApplyDirectivesFilter;

    DirectivesTreeView.EndUpdate;

  finally
    Exclude(FFlags,cevRefreshing);
  end;
end;

procedure TCodeExplorerView.ClearCTNodes(ATreeView: TTreeView);
var
  TVNode: TTreeNode;
  NodeData: TViewNodeData;
begin
  TVNode:=ATreeView.Items.GetFirstNode;
  while TVNode<>nil do begin
    NodeData:=TViewNodeData(TVNode.Data);
    NodeData.CTNode:=nil;
    TVNode:=TVNode.GetNext;
  end;
end;

function TCodeExplorerView.JumpToSelection(ToImplementation: boolean): boolean;
var
  CurItem: TTreeNode;
  CurNode: TViewNodeData;
  Caret: TCodeXYPosition;
  NewTopLine: integer;
  CodeBuffer: TCodeBuffer;
  ACodeTool: TCodeTool;
  CurTreeView: TCustomTreeView;
  SrcEdit: TSourceEditorInterface;
  NewNode: TCodeTreeNode;
  p: LongInt;
begin
  Result:=false;
  CurTreeView:=GetCurrentTreeView;
  if CurTreeView=nil then exit;
  if tvoAllowMultiselect in CurTreeView.Options then
    CurItem:=CurTreeView.GetFirstMultiSelected
  else
    CurItem:=CurTreeView.Selected;
  if CurItem=nil then exit;
  CurNode:=TViewNodeData(CurItem.Data);
  if ToImplementation then begin
    CurNode:=CurNode.ImplementationNode;
    if CurNode=nil then exit;
  end;
  if CurNode.StartPos<1 then exit;
  CodeBuffer:=nil;
  case CurrentPage of
  cepCode:
    begin
      CodeBuffer:=CodeToolBoss.LoadFile(CodeFilename,false,false);
      if CodeBuffer=nil then exit;
      ACodeTool:=nil;
      CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
      if ACodeTool=nil then exit;
      p:=CurNode.StartPos;
      NewNode:=ACodeTool.FindDeepestNodeAtPos(p,false);
      if NewNode<>nil then begin
        if (NewNode.Desc=ctnProcedure)
        and (NewNode.FirstChild<>nil)
        and (NewNode.FirstChild.Desc=ctnProcedureHead)
        and (NewNode.FirstChild.StartPos>p) then
          p:=NewNode.FirstChild.StartPos;
        if NewNode.Desc=ctnProperty then begin
          if ACodeTool.MoveCursorToPropName(NewNode) then
            p:=ACodeTool.CurPos.StartPos;
        end;
      end;
      if not ACodeTool.CleanPosToCaretAndTopLine(p,Caret,NewTopLine)
      then exit;
    end;
  cepDirectives:
    begin
      CodeBuffer:=CodeToolBoss.LoadFile(DirectivesFilename,false,false);
      if CodeBuffer=nil then exit;
      CodeBuffer.AbsoluteToLineCol(CurNode.StartPos,Caret.Y,Caret.X);
      if Caret.Y<1 then exit;
      Caret.Code:=CodeBuffer;
      NewTopLine:=Caret.Y-(CodeToolBoss.VisibleEditorLines div 2);
      if NewTopLine<1 then NewTopLine:=1;
    end;
  else
    exit;
  end;
  if Assigned(OnJumpToCode) then
    OnJumpToCode(Self,Caret.Code.Filename,Point(Caret.X,Caret.Y),NewTopLine);
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  //DebugLn(['TCodeExplorerView.JumpToSelection  ',SrcEdit.FileName,' ',dbgs(SrcEdit.CursorTextXY),' X=',Caret.X,' Y=',Caret.Y]);
  // check if jump was successful
  if (SrcEdit.CodeToolsBuffer<>CodeBuffer)
  or (SrcEdit.CursorTextXY.X<>Caret.X) or (SrcEdit.CursorTextXY.Y<>Caret.Y) then
    exit;
  Result:=true;
end;

function TCodeExplorerView.SelectSourceEditorNode: boolean;
var
  SrcEdit: TSourceEditorInterface;
  xy: TPoint;
begin
  Result:=false;
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  xy:=SrcEdit.CursorTextXY;
  Result:=SelectCodePosition(TCodeBuffer(SrcEdit.CodeToolsBuffer),xy.x,xy.y);
end;

function TCodeExplorerView.SelectCodePosition(CodeBuf: TCodeBuffer; X,
  Y: integer): boolean;
var
  CodePos: TCodeXYPosition;
  CleanPos: integer;
  TVNode: TTreeNode;
begin
  Result:=false;
  if CurrentPage=cepCode then begin
    if FLastCodeValid and (fLastCodeTool<>nil) then begin
      CodePos:=CodeXYPosition(X,Y,CodeBuf);
      CodeBuf.LineColToPosition(Y,X,CleanPos);
      //debugln(['TCodeExplorerView.SelectCodePosition Code ',ExtractFileName(CodeBuf.Filename),' y=',y,' x=',x,' CleanPos=',CleanPos,' ',dbgstr(copy(CodeBuf.Source,CleanPos-20,20)),'|',dbgstr(copy(CodeBuf.Source,CleanPos,20))]);
      if fLastCodeTool.CaretToCleanPos(CodePos,CleanPos)<>0 then exit;
      //debugln(['TCodeExplorerView.SelectCodePosition CleanSrc ',ExtractFileName(CodeBuf.Filename),' y=',y,' x=',x,' Tool=',ExtractFileName(fLastCodeTool.MainFilename),' ',dbgstr(copy(fLastCodeTool.Src,CleanPos-20,20)),'|',dbgstr(copy(fLastCodeTool.Src,CleanPos,20))]);
      TVNode:=FindCodeTVNodeAtCleanPos(CleanPos);
      if TVNode=nil then exit;
      //debugln(['TCodeExplorerView.SelectCodePosition ',TVNode.Text]);
      CodeTreeview.BeginUpdate;
      CodeTreeview.Options:=CodeTreeview.Options-[tvoAllowMultiselect];
      if not TVNode.IsVisible then begin
        // collapse all other and expand only this
        CodeTreeview.FullCollapse;
        CodeTreeview.Selected:=TVNode;
        //debugln(['TCodeExplorerView.SelectCodePosition ',TVNode.Text]);
      end else begin
        CodeTreeview.Selected:=TVNode;
        //debugln(['TCodeExplorerView.SelectCodePosition ',TVNode.Text]);
      end;
      //debugln(['TCodeExplorerView.SelectCodePosition TVNode=',TVNode.Text,' Selected=',CodeTreeview.Selected=TVNode]);
      CodeTreeview.EndUpdate;
      Result:=true;
    end;
  end;
end;

function TCodeExplorerView.FindCodeTVNodeAtCleanPos(CleanPos: integer): TTreeNode;
// find TTreeNode in CodeTreeView containing the codetools clean position
// if there are several nodes, the one with the shortest range (EndPos-StartPos)
// is returned.
var
  Best: TTreeNode;
  BestStartPos, BestEndPos: integer;

  procedure Check(TVNode: TTreeNode; NodeData: TViewNodeData);
  begin
    if NodeData=nil then exit;
    if (NodeData.StartPos>CleanPos) or (NodeData.EndPos<CleanPos) then exit;
    //debugln(['FindCodeTVNodeAtCleanPos.Check TVNode="',TVNode.Text,'" NodeData="',dbgstr(copy(fLastCodeTool.Src,NodeData.StartPos,40)),'"']);
    if (Best<>nil) then begin
      if (BestEndPos=CleanPos) and (NodeData.EndPos>CleanPos) then begin
        // for example  a,|b  then b is better
      end else if BestEndPos-BestStartPos > NodeData.EndPos-NodeData.StartPos then begin
        // smaller range is better
      end else
        exit;
    end;
    Best:=TVNode;
    BestStartPos:=NodeData.StartPos;
    BestEndPos:=NodeData.EndPos;
  end;

var
  AVLNode: TAvlTreeNode;
  Node: TTreeNode;
  NodeData: TViewNodeData;
begin
  Result:=nil;
  if (fLastCodeTool=nil) or (not FLastCodeValid) or (CodeTreeview=nil)
  or (fCodeSortedForStartPos=nil) then exit;

  // find nearest node in tree
  Best:=nil;
  BestStartPos:=0;
  BestEndPos:=0;
  AVLNode:=fCodeSortedForStartPos.FindLowest;
  while AVLNode<>nil do begin
    Node:=TTreeNode(AVLNode.Data);
    NodeData:=TViewNodeData(Node.Data);
    //debugln(['TCodeExplorerView.FindCodeTVNodeAtCleanPos Node ',NodeData.StartPos,'-',NodeData.EndPos,' ',Node.Text,' ',CleanPos]);
    Check(Node,NodeData);
    Check(Node,NodeData.ImplementationNode);
    AVLNode:=fCodeSortedForStartPos.FindSuccessor(AVLNode);
  end;
  Result:=Best;
end;

procedure TCodeExplorerView.BuildCodeSortedForStartPos;
var
  TVNode: TTreeNode;
  NodeData: TViewNodeData;
begin
  if fCodeSortedForStartPos<>nil then
    fCodeSortedForStartPos.Clear;
  if (CodeTreeview=nil) then exit;
  TVNode:=CodeTreeview.Items.GetFirstNode;
  while TVNode<>nil do begin
    if TVNode.Parent=nil then begin
      if (TVNode=fObserverNode) or (TVNode=fSurroundingNode) then break;
    end;
    NodeData:=TViewNodeData(TVNode.Data);
    if (NodeData<>nil) and (NodeData.StartPos>0)
    and (NodeData.EndPos>=NodeData.StartPos) then begin
      if fCodeSortedForStartPos=nil then
        fCodeSortedForStartPos:=TAvlTree.Create(TListSortCompare(@CompareViewNodeDataStartPos));
      fCodeSortedForStartPos.Add(TVNode);
    end;
    TVNode:=TVNode.GetNext;
  end;
end;

procedure TCodeExplorerView.CurrentCodeBufferChanged;
begin
  if CodeExplorerOptions.Refresh=cerSwitchEditorPage then
    CheckOnIdle;
end;

procedure TCodeExplorerView.CodeFilterChanged;
var
  TheFilter: String;
begin
  TheFilter:=GetCodeFilter;
  if FLastCodeFilter=TheFilter then exit;
  if (FUpdateCount>0) or (CurrentPage<>cepCode) then begin
    Include(FFlags,cevCodeRefreshNeeded);
    exit;
  end;
  if (FLastCodeFilter='')
  or (System.Pos(lowercase(FLastCodeFilter),lowercase(TheFilter))>0)
  then begin
    // longer filter => just delete nodes
    ApplyCodeFilter;
  end else begin
    CheckOnIdle;
  end;
end;

procedure TCodeExplorerView.DirectivesFilterChanged;
var
  TheFilter: String;
begin
  TheFilter:=DirectivesFilterEdit.Text;
  if FLastDirectivesFilter=TheFilter then exit;
  if (FUpdateCount>0) or (CurrentPage<>cepDirectives) then begin
    Include(FFlags,cevDirectivesRefreshNeeded);
    exit;
  end;
  FLastDirectivesChangeStep:=CTInvalidChangeStamp;
  RefreshDirectives(False);
end;

function TCodeExplorerView.FilterNode(ANode: TTreeNode;
  const TheFilter: string; KeepTopLevel: Boolean): boolean;
// Return True if ANode passes the filter. Delete nodes which do not pass.
// Filter recursively all subnodes.
var
  ChildNode, NextNode: TTreeNode;
  ChildPass, ChildrenPassed: Boolean;
begin
  if ANode=nil then exit(false);
  ChildNode:=ANode.GetFirstChild;
  ChildrenPassed:=false;
  while ChildNode<>nil do begin
    NextNode:=ChildNode.GetNextSibling;
    ChildPass:=FilterNode(ChildNode,TheFilter,KeepTopLevel);
    ChildrenPassed:=ChildrenPassed or ChildPass;
    ChildNode:=NextNode;
  end;
  Result:=((ANode.Parent=nil) and KeepTopLevel)
      or ChildrenPassed or FilterFits(ANode.Text,TheFilter);
  //DebugLn(['TCodeExplorerView.FilterNode "',ANode.Text,'" Parent=',ANode.Parent,
  //  ' Child=',ANode.GetFirstChild,' Filter=',FilterFits(ANode.Text,TheFilter),' Result=',Result]);
  if Result then begin
    if ChildrenPassed and (TheFilter<>'') then
      ANode.Expanded:=True;
  end
  else
    DeleteTVNode(ANode);
end;

function TCodeExplorerView.FilterFits(const NodeText, TheFilter: string): boolean;
var
  Src: PChar;
  PFilter: PChar;
  c: Char;
  i: Integer;
begin
  Result:=false;
  if TheFilter='' then
    Result:=true
  else if NodeText<>'' then begin
    Src:=PChar(NodeText);
    PFilter:=PChar(TheFilter);
    repeat
      c:=Src^;
      if c<>#0 then begin
        if UpChars[Src^]=UpChars[PFilter^] then begin
          i:=1;
          while (UpChars[Src[i]]=UpChars[PFilter[i]]) and (PFilter[i]<>#0) do
            inc(i);
          if PFilter[i]=#0 then begin
            //DebugLn(['TCodeExplorerView.FilterFits Fits "',NodeText,'" "',TheFilter,'"']);
            exit(true);
          end;
        end;
      end else
        exit(false);
      inc(Src);
    until false;
  end;
end;

function TCodeExplorerView.GetCurrentTreeView: TCustomTreeView;
begin
  case CurrentPage of
  cepCode: Result:=CodeTreeview;
  cepDirectives: Result:=DirectivesTreeView;
  else  Result:=nil;
  end;
end;

function TCodeExplorerView.GetCTNodePath(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode): string;
var
  CurName: String;
begin
  Result:='';
  try
    while CodeNode<>nil do begin
      CurName:='';
      case CodeNode.Desc of

      ctnTypeDefinition,
      ctnVarDefinition,
      ctnConstDefinition,
      ctnEnumIdentifier:
        CurName:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);

      ctnUseUnit:
        CurName:=ACodeTool.ExtractDottedIdentifier(CodeNode.StartPos);

      ctnGenericType:
        CurName:=ACodeTool.ExtractDefinitionName(CodeNode);

      ctnProcedure:
        CurName:=ACodeTool.ExtractProcName(CodeNode,[]);

      ctnProperty:
        CurName:=ACodeTool.ExtractPropName(CodeNode,false); // property keyword is not needed because there are icons

      end;
      if CurName<>'' then begin
        if Result<>'' then Result:='.'+Result;
        Result:=CurName+Result;
      end;
      CodeNode:=CodeNode.Parent;
    end;
  except
    on E: ECodeToolError do
      Result:=''; // ignore syntax errors
  end;
end;

procedure TCodeExplorerView.CreateNodePath(ACodeTool: TCodeTool;
  aNodeData: TObject);
var
  NodeData: TViewNodeData absolute aNodeData;
  AVLNode: TAvlTreeNode;
begin
  if NodeData.CTNode.Desc=ctnProcedure then
    NodeData.Path:=GetCTNodePath(ACodeTool,NodeData.CTNode);
  if NodeData.Path='' then exit;
  AVLNode:=fNodesWithPath.FindKey(NodeData,@CompareViewNodePaths);
  if AVLNode=nil then begin
    // unique path
    fNodesWithPath.Add(NodeData);
    exit;
  end;
  // there is already a node with this path
  // => add params to distinguish overloads
  NodeData.CreateParams(ACodeTool);
  TViewNodeData(AVLNode.Data).CreateParams(ACodeTool);
  fNodesWithPath.Add(NodeData);
end;

procedure TCodeExplorerView.AddImplementationNode(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode);
var
  NodeData: TViewNodeData;
  AVLNode: TAvlTreeNode;
  DeclData: TViewNodeData;
begin
  if (CodeNode.Desc=ctnProcedure)
  and ((ctnsForwardDeclaration and CodeNode.SubDesc)=0) then begin
    NodeData:=TViewNodeData.Create(CodeNode);
    try
      NodeData.Path:=GetCTNodePath(ACodeTool,NodeData.CTNode);
      if NodeData.Path='' then exit;
      //debugln(['TCodeExplorerView.AddImplementationNode Proc=',NodeData.Path]);
      AVLNode:=fNodesWithPath.FindKey(NodeData,@CompareViewNodePaths);
      if (AVLNode=nil) or (TViewNodeData(AVLNode.Data).ImplementationNode<>nil)
      then begin
        // there is no declaration, or there is already an implementation
        // => ignore
        exit;
      end;
      DeclData:=TViewNodeData(AVLNode.Data);
      if (DeclData.Params<>'') then begin
        // there are several nodes with this Path
        NodeData.CreateParams(ACodeTool);
        AVLNode:=fNodesWithPath.Find(NodeData);
        if (AVLNode=nil) or (TViewNodeData(AVLNode.Data).ImplementationNode<>nil)
        then begin
          // there is no declaration, or there is already an implementation
          // => ignore
          exit;
        end;
        DeclData:=TViewNodeData(AVLNode.Data);
      end;
      // implementation found
      //debugln(['TCodeExplorerView.AddImplementationNode implementation found: ',NodeData.Path,'(',NodeData.Params,')']);
      NodeData.Desc:=CodeNode.Desc;
      NodeData.SubDesc:=CodeNode.SubDesc;
      NodeData.StartPos:=CodeNode.StartPos;
      NodeData.EndPos:=CodeNode.EndPos;
      DeclData.ImplementationNode:=NodeData;
      NodeData:=nil;
    finally
      NodeData.Free;
    end;
  end;
end;

function TCodeExplorerView.CompareCodeNodes(Node1, Node2: TTreeNode): integer;
const
  SortDesc = AllIdentifierDefinitions+[ctnProcedure,ctnProperty];
  
  function DescToLvl(Desc: TCodeTreeNodeDesc): integer;
  begin
    case Desc of
    ctnTypeSection,
    ctnTypeDefinition,ctnGenericType:
      Result:=1;
    ctnConstSection,ctnConstDefinition:
      Result:=2;
    ctnVarSection,ctnClassClassVar,ctnResStrSection,ctnLabelSection,
    ctnVarDefinition:
      Result:=3;
    ctnInterface,ctnImplementation,ctnProgram,ctnPackage,ctnLibrary,
    ctnProcedure:
      Result:=4;
    ctnProperty:
      Result:=5;
    ctnUsesSection:
      Result:=6;

    // class sections
    ctnClassGUID,
    ctnClassPrivate,
    ctnClassProtected,
    ctnClassPublic,
    ctnClassPublished   : Result:=Desc-ctnClassGUID;
    
    else Result:=10000;
    end;
  end;
  
var
  Data1: TViewNodeData;
  Data2: TViewNodeData;
begin
  Data1:=TViewNodeData(Node1.Data);
  Data2:=TViewNodeData(Node2.Data);
  if (Mode=cemCategory) then begin
    if Data1.Desc<>Data2.Desc then begin
      Result:=DescToLvl(Data1.Desc)-DescToLvl(Data2.Desc);
      if Result<>0 then exit;
    end;
    if (Data1.Desc in SortDesc)
    and (Data2.Desc in SortDesc) then begin
      Result:=SysUtils.CompareText(Node1.Text,Node2.Text);
      if Result<>0 then exit;
    end;
    if (Data1.Desc=ctnConstant) and (Data2.Desc=ctnConstant)
    and (fSortCodeTool<>nil) then begin
      //if GetAtomLength(@fSortCodeTool.Src[Data1.StartPos])>50 then
      //  DebugLn(['TCodeExplorerView.CompareCodeNodes ',GetAtomString(@fSortCodeTool.Src[Data1.StartPos],fSortCodeTool.Scanner.NestedComments),' ',round(Now*8640000) mod 10000]);
      //Result:=-CompareAtom(@fSortCodeTool.Src[Data1.StartPos],
      //                     @fSortCodeTool.Src[Data2.StartPos]);
      //if Result<>0 then exit;
    end;
  end;
  if Data1.StartPos<Data2.StartPos then
    Result:=-1
  else if Data1.StartPos>Data2.StartPos then
    Result:=1
  else
    Result:=0;
end;

{ TCodeObserverStatementState }

function TCodeObserverStatementState.GetStatementStartPos: integer;
begin
  if StackPtr=0 then
    Result:=TopLvlStatementStartPos
  else
    Result:=Stack[StackPtr-1].StatementStartPos;
end;

procedure TCodeObserverStatementState.SetStatementStartPos(const AValue: integer);
begin
  if StackPtr=0 then
    TopLvlStatementStartPos:=AValue
  else
    Stack[StackPtr-1].StatementStartPos:=AValue;
end;

destructor TCodeObserverStatementState.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeObserverStatementState.Clear;
begin
  ReAllocMem(Stack,0);
  StackCapacity:=0;
  StackPtr:=0;
end;

procedure TCodeObserverStatementState.Reset;
begin
  PopAll;
  TopLvlStatementStartPos:=0;
  IgnoreConstLevel:=-1;
end;

procedure TCodeObserverStatementState.Push(Typ: TCodeObsStackItemType;
  StartPos: integer);
begin
  if StackPtr=StackCapacity then
  begin
    StackCapacity:=StackCapacity*2+10;
    ReAllocMem(Stack,SizeOf(TCodeObsStackItem)*StackCapacity);
  end;
  Stack[StackPtr].Typ:=Typ;
  Stack[StackPtr].StartPos:=StartPos;
  Stack[StackPtr].StatementStartPos:=0;
  inc(StackPtr);
end;

function TCodeObserverStatementState.Pop: TCodeObsStackItemType;
begin
  if StackPtr=0 then
    RaiseGDBException('inconsistency');
  dec(StackPtr);
  Result:=Stack[StackPtr].Typ;
  if IgnoreConstLevel>StackPtr then
    IgnoreConstLevel:=-1;
end;

procedure TCodeObserverStatementState.PopAll;
begin
  StackPtr:=0;
end;

function TCodeObserverStatementState.TopType: TCodeObsStackItemType;
begin
  if StackPtr>0 then
    Result:=Stack[StackPtr-1].Typ
  else
    Result:=cositNone;
end;

end.

