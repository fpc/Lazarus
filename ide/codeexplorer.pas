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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  // FCL+LCL
  Classes, SysUtils, types, LCLProc, LCLType, Forms, Controls, Graphics,
  Dialogs, Buttons, ComCtrls, Menus, AvgLvlTree, StdCtrls, ExtCtrls,
  // CodeTools
  BasicCodeTools, CustomCodeTool, CodeToolManager, CodeAtom, CodeCache,
  CodeTree, KeywordFuncLists, FindDeclarationTool, DirectivesTree,
  PascalParserTool,
  // IDE Intf
  LazIDEIntf, IDECommands, MenuIntf, SrcEditorIntf,
  // IDE
  KeyMapping, LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs, InputHistory,
  IDEProcs, CodeExplOpts;

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
  
  TCodeExplorerPage = (
    cepNone,
    cepCode,
    cepDirectives
    );

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
    OptionsSpeedButton: TSpeedButton;
    RefreshSpeedButton: TSpeedButton;
    ModeSpeedButton: TSpeedButton;
    TreePopupmenu: TPopupMenu;
    procedure CodeExplorerViewCreate(Sender: TObject);
    procedure CodeExplorerViewDestroy(Sender: TObject);
    procedure CodeTreeviewDblClick(Sender: TObject);
    procedure CodeTreeviewDeletion(Sender: TObject; Node: TTreeNode);
    procedure CodeTreeviewKeyUp(Sender: TObject; var Key: Word;
                                Shift: TShiftState);
    procedure CodeFilterEditChange(Sender: TObject);
    procedure DirectivesFilterEditChange(Sender: TObject);
    procedure DirectivesTreeViewDblClick(Sender: TObject);
    procedure DirectivesTreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure DirectivesTreeViewKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure JumpToMenuitemClick(Sender: TObject);
    procedure MainNotebookPageChanged(Sender: TObject);
    procedure ModeSpeedButtonClick(Sender: TObject);
    procedure OptionsSpeedButtonClick(Sender: TObject);
    procedure RefreshMenuitemClick(Sender: TObject);
    procedure RefreshSpeedButtonClick(Sender: TObject);
    procedure RenameMenuItemClick(Sender: TObject);
    procedure TreePopupmenuPopup(Sender: TObject);
    procedure OnUserInput(Sender: TObject; Msg: Cardinal);
  private
    FCodeFilename: string;
    fCategoryNodes: array[TCodeExplorerCategory] of TTreeNode;
    fObserverNode: TTreeNode;
    fObserverCatNodes: array[TCEObserverCategory] of TTreeNode;
    fObserverCatOverflow: array[TCEObserverCategory] of boolean;
    FDirectivesFilename: string;
    FFlags: TCodeExplorerViewFlags;
    FLastCodeFilter: string;
    FLastCodeChangeStep: integer;
    fLastCodeOptionsChangeStep: integer;
    FLastDirectivesFilter: string;
    FLastDirectivesChangeStep: integer;
    FMode: TCodeExplorerMode;
    FLastMode: TCodeExplorerMode;
    FLastCodeValid: boolean;
    FLastCodeXY: TPoint;
    FOnGetDirectivesTree: TOnGetDirectivesTree;
    FOnJumpToCode: TOnJumpToCode;
    FOnShowOptions: TNotifyEvent;
    FUpdateCount: integer;
    fSortCodeTool: TCodeTool;
    ImgIDClass: Integer;
    ImgIDConst: Integer;
    ImgIDSection: Integer;
    ImgIDDefault: integer;
    ImgIDFinalization: Integer;
    ImgIDImplementation: Integer;
    ImgIDInitialization: Integer;
    ImgIDInterface: Integer;
    ImgIDProcedure: Integer;
    ImgIDFunction: Integer;
    ImgIDProgram: Integer;
    ImgIDProperty: Integer;
    ImgIDPropertyReadOnly: Integer;
    ImgIDType: Integer;
    ImgIDUnit: Integer;
    ImgIDVariable: Integer;
    ImgIDHint: Integer;
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
                          ParentViewNode, InFrontViewNode: TTreeNode;
                          CreateSiblings: boolean);
    procedure CreateDirectiveNodes(ADirectivesTool: TDirectivesTool;
                          CodeNode: TCodeTreeNode;
                          ParentViewNode, InFrontViewNode: TTreeNode;
                          CreateSiblings: boolean);
    procedure CreateObservations(Tool: TCodeTool);
    function CreateObserverNode(Tool: TCodeTool; f: TCEObserverCategory): TTreeNode;
    procedure CreateObserverNodesForStatement(Tool: TCodeTool;
                            CodeNode: TCodeTreeNode; StartPos, EndPos: integer;
                            ObserverState: TCodeObserverStatementState);
    procedure FindObserverTodos(Tool: TCodeTool);
    procedure SetCodeFilter(const AValue: string);
    procedure SetCurrentPage(const AValue: TCodeExplorerPage);
    procedure SetDirectivesFilter(const AValue: string);
    procedure SetMode(AMode: TCodeExplorerMode);
    procedure UpdateMode;
  protected
    fLastCodeTool: TCodeTool;
    fCodeSortedForStartPos: TAvgLvlTree;// tree of TTreeNode sorted for TViewNodeData(Node.Data).StartPos, secondary EndPos
    procedure ApplyCodeFilter;
    procedure ApplyDirectivesFilter;
    function CompareCodeNodes(Node1, Node2: TTreeNode): integer;
  public
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CheckOnIdle;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Refresh(OnlyVisible: boolean);
    procedure RefreshCode(OnlyVisible: boolean);
    procedure RefreshDirectives(OnlyVisible: boolean);
    procedure ClearCTNodes(ATreeView: TTreeView);// remove temporary references
    function JumpToSelection: boolean; // jump in source editor
    function SelectSourceEditorNode: boolean;
    function SelectCodePosition(CodeBuf: TCodeBuffer; X, Y: integer): boolean; // select deepest node
    function FindCodeTVNodeAtCleanPos(CleanPos: integer): TTreeNode;
    procedure BuildCodeSortedForStartPos;
    procedure CurrentCodeBufferChanged;
    procedure CodeFilterChanged;
    procedure DirectivesFilterChanged;
    function FilterNode(ANode: TTreeNode; const TheFilter: string): boolean;
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
  CERefreshIDEMenuCommand: TIDEMenuCommand;
  CERenameIDEMenuCommand: TIDEMenuCommand;

procedure RegisterStandardCodeExplorerMenuItems;

function GetToDoComment(const Src: string;
                       CommentStartPos, CommentEndPos: integer;
                       out MagicStartPos, TextStartPos, TextEndPos: integer): boolean;

implementation

{$R *.lfm}

type
  TViewNodeData = class
  public
    CTNode: TCodeTreeNode; // only valid during update, other times it is nil
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    StartPos, EndPos: integer;
    constructor Create(CodeNode: TCodeTreeNode);
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

procedure RegisterStandardCodeExplorerMenuItems;
var
  Path: String;
begin
  CodeExplorerMenuRoot:=RegisterIDEMenuRoot(CodeExplorerMenuRootName);
  Path:=CodeExplorerMenuRoot.Name;
  CEJumpToIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Jump to', lisMenuJumpTo
    );
  CERefreshIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Refresh',
    dlgUnitDepRefresh);
  CERenameIDEMenuCommand:=RegisterIDEMenuCommand(Path, 'Rename', lisFRIRename);
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

constructor TViewNodeData.Create(CodeNode: TCodeTreeNode);
begin
  CTNode:=CodeNode;
  Desc:=CodeNode.Desc;
  SubDesc:=CodeNode.SubDesc;
  StartPos:=CodeNode.StartPos;
  EndPos:=CodeNode.EndPos;
end;

{ TCodeExplorerView }

procedure TCodeExplorerView.CodeExplorerViewCREATE(Sender: TObject);
begin
  FMode := CodeExplorerOptions.Mode;
  UpdateMode;

  Name:=NonModalIDEWindowNames[nmiwCodeExplorerName];
  Caption := lisMenuViewCodeExplorer;

  MainNotebook.ActivePage:=CodePage;

  RefreshSpeedButton.Hint:=dlgUnitDepRefresh;
  OptionsSpeedButton.Hint:=dlgFROpts;
  CodeFilterEdit.Text:=lisCEFilter;
  CodePage.Caption:=lisCode;
  DirectivesFilterEdit.Text:=lisCEFilter;
  DirectivesPage.Caption:=lisDirectives;
  

  RefreshSpeedButton.LoadGlyphFromLazarusResource('laz_refresh');
  OptionsSpeedButton.LoadGlyphFromLazarusResource('menu_environment_options');

  ImgIDDefault := Imagelist1.AddLazarusResource('ce_default');
  ImgIDProgram := Imagelist1.AddLazarusResource('ce_program');
  ImgIDUnit := Imagelist1.AddLazarusResource('ce_unit');
  ImgIDInterface := Imagelist1.AddLazarusResource('ce_interface');
  ImgIDImplementation := Imagelist1.AddLazarusResource('ce_implementation');
  ImgIDInitialization := Imagelist1.AddLazarusResource('ce_initialization');
  ImgIDFinalization := Imagelist1.AddLazarusResource('ce_finalization');
  ImgIDType := Imagelist1.AddLazarusResource('ce_type');
  ImgIDVariable := Imagelist1.AddLazarusResource('ce_variable');
  ImgIDConst := Imagelist1.AddLazarusResource('ce_const');
  ImgIDClass := Imagelist1.AddLazarusResource('ce_class');
  ImgIDProcedure := Imagelist1.AddLazarusResource('ce_procedure');
  ImgIDFunction := Imagelist1.AddLazarusResource('ce_function');
  ImgIDProperty := Imagelist1.AddLazarusResource('ce_property');
  ImgIDPropertyReadOnly := Imagelist1.AddLazarusResource('ce_property_readonly');
  // sections
  ImgIDSection := Imagelist1.AddLazarusResource('ce_section');
  ImgIDHint := Imagelist1.AddLazarusResource('state_hint');

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  CodeExplorerMenuRoot.MenuItem:=TreePopupMenu.Items;
  //CodeExplorerMenuRoot.Items.WriteDebugReport(' ');

  CEJumpToIDEMenuCommand.OnClick:=@JumpToMenuitemCLICK;
  CERefreshIDEMenuCommand.OnClick:=@RefreshMenuitemCLICK;
  CERenameIDEMenuCommand.OnClick:=@RenameMenuItemClick;

  Application.AddOnUserInputHandler(@OnUserInput);
end;

procedure TCodeExplorerView.CodeExplorerViewDestroy(Sender: TObject);
begin
  //debugln('TCodeExplorerView.CodeExplorerViewDestroy');
end;

procedure TCodeExplorerView.CodeTreeviewDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.CodeTreeviewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data<>nil then
    TViewNodeData(Node.Data).Free;
end;

procedure TCodeExplorerView.CodeTreeviewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[]) then
    JumpToSelection;
end;

procedure TCodeExplorerView.CodeFilterEditChange(Sender: TObject);
begin
  if Sender=nil then ;
  CodeFilterChanged;
end;

procedure TCodeExplorerView.DirectivesFilterEditChange(Sender: TObject);
begin
  if Sender=nil then ;
  DirectivesFilterChanged;
end;

procedure TCodeExplorerView.DirectivesTreeViewDblClick(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.DirectivesTreeViewDeletion(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data<>nil then
    TObject(Node.Data).Free;
end;

procedure TCodeExplorerView.DirectivesTreeViewKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[]) then
    JumpToSelection;
end;

procedure TCodeExplorerView.IdleTimer1Timer(Sender: TObject);
begin
  if not (cevCheckOnIdle in FFlags) then exit;
  if (Screen.ActiveCustomForm<>nil)
  and (fsModal in Screen.ActiveCustomForm.FormState) then
    exit;
  if not IsVisible then exit;
  if Active then exit;
  Refresh(true);
end;

procedure TCodeExplorerView.JumpToMenuitemCLICK(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.MainNotebookPageChanged(Sender: TObject);
begin
  Refresh(true);
end;

procedure TCodeExplorerView.ModeSpeedButtonClick(Sender: TObject);
begin
  // Let's Invert Mode of Exibition
  if Mode = cemCategory then
    SetMode(cemSource)
  else
    SetMode(cemCategory);
end;

procedure TCodeExplorerView.OptionsSpeedButtonClick(Sender: TObject);
begin
  if Assigned(FOnShowOptions) then
  begin
    OnShowOptions(Self);
    Refresh(True);
  end;
end;

procedure TCodeExplorerView.RefreshMenuitemCLICK(Sender: TObject);
begin
  Refresh(true);
end;

procedure TCodeExplorerView.RefreshSpeedButtonClick(Sender: TObject);
begin
  Refresh(true);
end;

procedure TCodeExplorerView.RenameMenuItemClick(Sender: TObject);
begin
  if not JumpToSelection then begin
    MessageDlg(lisCCOErrorCaption, lisTreeNeedsRefresh, mtError, [mbOk], 0);
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
begin
  CanRename:=false;
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
    end;
  end;
  CERenameIDEMenuCommand.Visible:=CanRename;
  DebugLn(['TCodeExplorerView.TreePopupmenuPopup ',CERenameIDEMenuCommand.Visible]);
end;

procedure TCodeExplorerView.OnUserInput(Sender: TObject; Msg: Cardinal);
begin
  if CodeExplorerOptions.Refresh=cerOnIdle then
    CheckOnIdle;
end;

function TCodeExplorerView.GetCodeNodeDescription(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode): string;
begin
  Result:='?';
  try
    case CodeNode.Desc of

    ctnUnit, ctnProgram, ctnLibrary, ctnPackage:
      Result:=CodeNode.DescAsString+' '+ACodeTool.ExtractSourceName;

    ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition,ctnUseUnit:
      Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);

    ctnGenericType:
      Result:=ACodeTool.ExtractDefinitionName(CodeNode);

    ctnClass,ctnObject,ctnObjCClass,ctnObjCCategory,ctnObjCProtocol,
    ctnInterface,ctnCPPClass:
      Result:='('+ACodeTool.ExtractClassInheritance(CodeNode,[])+')';

    ctnEnumIdentifier:
      Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);

    ctnProcedure:
      Result:=ACodeTool.ExtractProcHead(CodeNode,
                    [// phpWithStart is no needed because there are icons
                     phpWithVarModifiers,
                     phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                     phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);

    ctnProperty:
      Result:=ACodeTool.ExtractPropName(CodeNode,false); // property keyword is not needed because there are icons

    else
      Result:=CodeNode.DescAsString;
    end;
  except
    on E: ECodeToolError do ; // ignore syntax errors
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
  if Result=lisCEFilter then Result:='';
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
  if Result=lisCEFilter then Result:='';
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
        if (CodeNode.FirstChild <> nil)
        and (CodeNode.FirstChild.Desc in AllClasses) then
          Result := ImgIDClass
        else
          Result := ImgIDType;
      end;
    ctnVarSection:                    Result:=ImgIDSection;
    ctnVarDefinition:                 Result:=ImgIDVariable;
    ctnConstSection,ctnResStrSection: Result:=ImgIDSection;
    ctnConstDefinition:               Result:=ImgIDConst;
    ctnClass,ctnClassInterface,ctnObject,
    ctnObjCClass,ctnObjCProtocol,ctnObjCCategory,ctnCPPClass:
                                      Result:=ImgIDClass;
    ctnProcedure:                     if Tool.NodeIsFunction(CodeNode) then
                                        Result:=ImgIDFunction
                                      else
                                        Result:=ImgIDProcedure;
    ctnProperty:                      Result:=ImgIDProperty;
    ctnUsesSection:                   Result:=ImgIDSection;
    ctnUseUnit:                       Result:=ImgIDUnit;
  else
    Result:=ImgIDDefault;
  end;
end;

function TCodeExplorerView.GetDirectiveNodeImage(CodeNode: TCodeTreeNode
  ): integer;
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
  CodeNode: TCodeTreeNode;
  ParentViewNode, InFrontViewNode: TTreeNode; CreateSiblings: boolean);
var
  NodeData: TViewNodeData;
  NodeText: String;
  ViewNode: TTreeNode;
  NodeImageIndex: Integer;
  ShowNode: Boolean;
  ShowChilds: Boolean;
  Category: TCodeExplorerCategory;
begin
  while CodeNode<>nil do begin
    ShowNode:=true;
    ShowChilds:=true;

    // don't show statements
    if (CodeNode.Desc in AllPascalStatements+[ctnParameterList]) then begin
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

    // don't show modifier nodes
    if CodeNode.Desc in [ctnIdentifier,ctnRangedArrayType,
      ctnOpenArrayType,ctnOfConstType,ctnRangeType,ctnTypeType,ctnFileType,
      ctnVariantType,ctnEnumerationType,ctnSetType,ctnProcedureType]
    then
      ShowNode:=false;
      
    // don't show End.
    if CodeNode.Desc=ctnEndPoint then
      ShowNode:=false;
      
    // don't show class visibility section nodes
    if (CodeNode.Desc in AllClassSections) then begin
      ShowNode:=false;
    end;

    if Mode=cemCategory then begin
      // don't show method bodies
      if (CodeNode.Desc=ctnProcedure)
      and (ACodeTool.NodeIsMethodBody(CodeNode)) then begin
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
        ctnUsesSection:     Category:=cecUses;
        ctnTypeDefinition,ctnGenericType:  Category:=cecTypes;
        ctnVarDefinition:   Category:=cecVariables;
        ctnConstDefinition: Category:=cecConstants;
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
            ParentViewNode:=fCategoryNodes[Category];
            InFrontViewNode:=nil;
          end;
        end else begin
          ShowNode:=false;
        end;
      end else if (CodeNode.Parent<>nil)
      and (CodeNode.Parent.Desc in (AllClassSections+AllClassInterfaces+[ctnRecordType]))
      then begin
        // show class, interface and record nodes
        ShowNode:=true;
      end else begin
        ShowNode:=false;
      end;
      //DebugLn(['TCodeExplorerView.CreateNodes ',CodeNode.DescAsString,' ShowNode=',ShowNode,' ShowChilds=',ShowChilds]);
    end;
    
    if ShowNode then begin
      NodeData:=TViewNodeData.Create(CodeNode);
      NodeText:=GetCodeNodeDescription(ACodeTool,CodeNode);
      NodeImageIndex:=GetCodeNodeImage(ACodeTool,CodeNode);
      if InFrontViewNode<>nil then
        ViewNode:=CodeTreeview.Items.InsertObjectBehind(
                                              InFrontViewNode,NodeText,NodeData)
      else if ParentViewNode<>nil then
        ViewNode:=CodeTreeview.Items.AddChildObject(
                                               ParentViewNode,NodeText,NodeData)
      else
        ViewNode:=CodeTreeview.Items.AddObject(nil,NodeText,NodeData);
      ViewNode.ImageIndex:=NodeImageIndex;
      ViewNode.SelectedIndex:=NodeImageIndex;
      InFrontViewNode:=ViewNode;
    end else begin
      ViewNode:=ParentViewNode;
    end;
    if ShowChilds then
      CreateIdentifierNodes(ACodeTool,CodeNode.FirstChild,ViewNode,nil,true);
    if not CreateSiblings then break;
    CodeNode:=CodeNode.NextBrother;
  end;
end;

procedure TCodeExplorerView.CreateDirectiveNodes(ADirectivesTool: TDirectivesTool;
  CodeNode: TCodeTreeNode; ParentViewNode, InFrontViewNode: TTreeNode;
  CreateSiblings: boolean);
var
  NodeData: TViewNodeData;
  NodeText: String;
  ViewNode: TTreeNode;
  NodeImageIndex: Integer;
  ShowNode: Boolean;
  ShowChilds: Boolean;
begin
  while CodeNode<>nil do begin
    ShowNode:=true;
    ShowChilds:=true;
    
    // do not show root node
    if CodeNode.Desc=cdnRoot then begin
      ShowNode:=false;
    end;

    ViewNode:=ParentViewNode;
    if ShowNode then begin
      NodeData:=TViewNodeData.Create(CodeNode);
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
      CreateDirectiveNodes(ADirectivesTool,CodeNode.FirstChild,ViewNode,nil,true);
    if not CreateSiblings then break;
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
              AddCodeNode(cefcLongProcs,ProcNode);
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

      ctnClassConst..ctnClassPublished:
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
    fObserverNode.Expanded:=true;
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
  NodeImageIndCex: LongInt;
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
          NodeImageIndCex:=ImgIDConst;
          TVNode:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
          TVNode.Data:=Data;
          TVNode.Text:=NodeText;
          TVNode.ImageIndex:=NodeImageIndCex;
          TVNode.SelectedIndex:=NodeImageIndCex;
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
        if (ObserverState.IgnoreConstLevel>=0)
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
            NodeImageIndCex:=ImgIDConst;
            TVNode:=CodeTreeview.Items.AddChild(ObsTVNode,NodeText);
            TVNode.Data:=Data;
            TVNode.Text:=NodeText;
            TVNode.ImageIndex:=NodeImageIndCex;
            TVNode.SelectedIndex:=NodeImageIndCex;
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
          Atom:=Tool.LastAtoms.GetValueAt(0);
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
        Data:=TViewNodeData.Create(Tool.Tree.Root);
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
    ModeSpeedButton.LoadGlyphFromLazarusResource('show_category');
    ModeSpeedButton.Hint:=lisCEModeShowSourceNodes;
  end
  else begin
    ModeSpeedButton.LoadGlyphFromLazarusResource('show_source');
    ModeSpeedButton.Hint:=lisCEModeShowCategories;
  end;
  Refresh(true);
end;

procedure TCodeExplorerView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

procedure TCodeExplorerView.ApplyCodeFilter;
var
  ANode: TTreeNode;
  TheFilter: String;
begin
  TheFilter:=CodeFilterEdit.Text;
  FLastCodeFilter:=TheFilter;
  CodeTreeview.BeginUpdate;
  CodeTreeview.Options:=CodeTreeview.Options+[tvoAllowMultiselect];
  //DebugLn(['TCodeExplorerView.ApplyCodeFilter =====================']);
  ANode:=CodeTreeview.Items.GetFirstNode;
  while ANode<>nil do begin
    FilterNode(ANode,TheFilter);
    ANode:=ANode.GetNextSibling;
  end;
  CodeTreeview.EndUpdate;
end;

procedure TCodeExplorerView.ApplyDirectivesFilter;
var
  ANode: TTreeNode;
  TheFilter: String;
begin
  TheFilter:=DirectivesFilterEdit.Text;
  FLastDirectivesFilter:=TheFilter;
  DirectivesTreeView.BeginUpdate;
  DirectivesTreeView.Options:=DirectivesTreeView.Options+[tvoAllowMultiselect];
  ANode:=DirectivesTreeView.Items.GetFirstNode;
  while ANode<>nil do begin
    FilterNode(ANode,TheFilter);
    ANode:=ANode.GetNextSibling;
  end;
  DirectivesTreeView.EndUpdate;
end;

destructor TCodeExplorerView.Destroy;
begin
  inherited Destroy;
  fLastCodeTool:=nil;
  FreeAndNil(fCodeSortedForStartPos);
  if CodeExplorerView=Self then
    CodeExplorerView:=nil;
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
    RaiseException('TCodeExplorerView.EndUpdate');
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
  begin
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
      DeleteNode:=false;
      DeleteNextNode:=false;
      if (NextTVNode<>nil)
      and (CompareTextIgnoringSpace(TVNode.Text,NextTVNode.Text,false)=0) then
      begin
        Data:=TViewNodeData(TVNode.Data);
        NextData:=TViewNodeData(NextTVNode.Data);
        if IsForward(Data) then
          DeleteNode:=true;
        if IsForward(NextData) then
          DeleteNextNode:=true;
      end;
      if DeleteNextNode then begin
        TViewNodeData(NextTVNode.Data).Free;
        NextTVNode.Data:=nil;
        NextTVNode.Delete;
        NextTVNode:=TVNode;
      end else if DeleteNode then begin
        TViewNodeData(TVNode.Data).Free;
        TVNode.Data:=nil;
        TVNode.Delete;
      end;
      TVNode:=NextTVNode;
    end;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  ACodeTool: TCodeTool;
  c: TCodeExplorerCategory;
  f: TCEObserverCategory;
  SrcEdit: TSourceEditorInterface;
  Filename: String;
  Code: TCodeBuffer;
  NewXY: TPoint;
  OnlyXYChanged: Boolean;
  CurFollowNode: Boolean;
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

    // check for changes in the codetool
    OnlyXYChanged:=false;
    if (ACodeTool=nil) then begin
      if (FCodeFilename='') then begin
        // still no tool
        exit;
      end;
      //debugln(['TCodeExplorerView.RefreshCode no tool']);
    end else begin
      if not FLastCodeValid then begin
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
      // remember the codetools ChangeStep
      if ACodeTool<>nil then begin
        FCodeFilename:=ACodeTool.MainFilename;
        if ACodeTool.Scanner<>nil then
          FLastCodeChangeStep:=ACodeTool.Scanner.ChangeStep;
      end else
        FCodeFilename:='';

      if fCodeSortedForStartPos<>nil then
        fCodeSortedForStartPos.Clear;

      //DebugLn(['TCodeExplorerView.RefreshCode ',FCodeFilename]);

      CurFollowNode:=CodeExplorerOptions.FollowCursor and (not Active);

      // start updating the CodeTreeView
      CodeTreeview.BeginUpdate;
      if not CurFollowNode then
        OldExpanded:=TTreeNodeExpandedState.Create(CodeTreeView);

      for c:=low(TCodeExplorerCategory) to high(TCodeExplorerCategory) do
        fCategoryNodes[c]:=nil;
      fObserverNode:=nil;
      for f:=low(TCEObserverCategory) to high(TCEObserverCategory) do
        fObserverCatNodes[f]:=nil;

      if (ACodeTool=nil) or (ACodeTool.Tree=nil) or (ACodeTool.Tree.Root=nil) then
      begin
        CodeTreeview.Items.Clear;
      end else begin
        CodeTreeview.Items.Clear;
        CreateIdentifierNodes(ACodeTool,ACodeTool.Tree.Root,nil,nil,true);
        if (Mode = cemCategory) and
           (cecCodeObserver in CodeExplorerOptions.Categories) then
          CreateObservations(ACodeTool);
      end;

      fSortCodeTool:=ACodeTool;
      CodeTreeview.CustomSort(@CompareCodeNodes);

      DeleteDuplicates(ACodeTool);

      // restore old expanded state
      if not CurFollowNode then
        AutoExpandNodes;

      BuildCodeSortedForStartPos;
      ClearCTNodes(CodeTreeview);

      ApplyCodeFilter;

      if OldExpanded<>nil then
        OldExpanded.Apply(CodeTreeView);

      if CurFollowNode then
        SelectCodePosition(Code,FLastCodeXY.X,FLastCodeXY.Y);

      CodeTreeview.EndUpdate;
    end;
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

    if (ADirectivesTool=nil) or (ADirectivesTool.Tree=nil)
    or (ADirectivesTool.Tree.Root=nil) then
    begin
      DirectivesTreeView.Items.Clear;
    end else begin
      DirectivesTreeView.Items.Clear;
      CreateDirectiveNodes(ADirectivesTool,ADirectivesTool.Tree.Root,nil,nil,true);
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

function TCodeExplorerView.JumpToSelection: boolean;
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

function TCodeExplorerView.SelectCodePosition(CodeBuf: TCodeBuffer;
  X, Y: integer): Boolean;
var
  CodePos: TCodeXYPosition;
  CleanPos: integer;
  TVNode: TTreeNode;
begin
  Result:=false;
  if CurrentPage=cepCode then begin
    if FLastCodeValid and (fLastCodeTool<>nil) then begin
      CodePos:=CodeXYPosition(X,Y,CodeBuf);
      if fLastCodeTool.CaretToCleanPos(CodePos,CleanPos)<>0 then exit;
      TVNode:=FindCodeTVNodeAtCleanPos(CleanPos);
      if TVNode=nil then exit;
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
      CodeTreeview.EndUpdate;
      Result:=true;
    end;
  end;
end;

function TCodeExplorerView.FindCodeTVNodeAtCleanPos(CleanPos: integer
  ): TTreeNode;
// find TTreeNode in CodeTreeView containing the codetools clean position
// if there are several nodes, the one with the shortest range (EndPos-StartPos)
// is returned.
var
  AVLNode: TAvgLvlTreeNode;
  Node: TTreeNode;
  NodeData: TViewNodeData;
begin
  Result:=nil;
  if (fLastCodeTool=nil) or (not FLastCodeValid) or (CodeTreeview=nil)
  or (fCodeSortedForStartPos=nil) then exit;
  // find nearest node in tree

  AVLNode:=fCodeSortedForStartPos.FindLowest;
  while AVLNode<>nil do begin
    Node:=TTreeNode(AVLNode.Data);
    NodeData:=TViewNodeData(Node.Data);
    //debugln(['TCodeExplorerView.FindCodeTVNodeAtCleanPos Node ',NodeData.StartPos,'-',NodeData.EndPos,' ',Node.Text,' ',CleanPos]);
    if NodeData.StartPos>CleanPos then exit;
    if NodeData.EndPos>=CleanPos then begin
      if (Result=nil)
      or (NodeData.EndPos-NodeData.StartPos
         < TViewNodeData(Result.Data).EndPos-TViewNodeData(Result.Data).StartPos)
      then
        Result:=Node;
    end;
    AVLNode:=fCodeSortedForStartPos.FindSuccessor(AVLNode);
  end;
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
    NodeData:=TViewNodeData(TVNode.Data);
    if (NodeData<>nil) and (NodeData.StartPos>0)
    and (NodeData.EndPos>=NodeData.StartPos) then begin
      if fCodeSortedForStartPos=nil then
        fCodeSortedForStartPos:=
              TAvgLvlTree.Create(TListSortCompare(@CompareViewNodeDataStartPos));
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
  TheFilter:=CodeFilterEdit.Text;
  if FLastCodeFilter=TheFilter then exit;
  if (FUpdateCount>0) or (CurrentPage<>cepCode) then begin
    Include(FFlags,cevCodeRefreshNeeded);
    exit;
  end;
  ApplyCodeFilter;
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
  ApplyDirectivesFilter;
end;

function TCodeExplorerView.FilterNode(ANode: TTreeNode;
  const TheFilter: string): boolean;
var
  ChildNode: TTreeNode;
  HasVisibleChilds: Boolean;
begin
  if ANode=nil then exit(false);
  ChildNode:=ANode.GetFirstChild;
  HasVisibleChilds:=false;
  while ChildNode<>nil do begin
    if FilterNode(ChildNode,TheFilter) then
      HasVisibleChilds:=true;
    ChildNode:=ChildNode.GetNextSibling;
  end;
  ANode.Expanded:=HasVisibleChilds;
  ANode.MultiSelected:=FilterFits(ANode.Text,TheFilter);
  Result:=ANode.Expanded or ANode.MultiSelected;
end;

function TCodeExplorerView.FilterFits(const NodeText, TheFilter: string
  ): boolean;
var
  Src: PChar;
  PFilter: PChar;
  c: Char;
  i: Integer;
begin
  if TheFilter='' then begin
    Result:=true;
  end else begin
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

function TCodeExplorerView.CompareCodeNodes(Node1, Node2: TTreeNode): integer;
const
  SortDesc = AllIdentifierDefinitions+[ctnProcedure,ctnProperty];
  
  function DescToLvl(Desc: TCodeTreeNodeDesc): integer;
  begin
    case Desc of
    ctnTypeSection,
    ctnTypeDefinition,ctnGenericType:
      Result:=1;
    ctnVarSection,ctnConstSection,ctnResStrSection,ctnLabelSection,
    ctnVarDefinition,ctnConstDefinition,ctnProperty:
      Result:=2;
    ctnInterface,ctnImplementation,ctnProgram,ctnPackage,ctnLibrary,
    ctnProcedure:
      Result:=3;
    ctnUsesSection:
      Result:=4;

    // class sections
    ctnClassConst,
    ctnClassType,
    ctnClassVar,
    ctnClassClassVar,
    ctnClassPrivate,
    ctnClassProtected,
    ctnClassPublic,
    ctnClassPublished   : Result:=Desc-ctnClassConst;
    
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

procedure TCodeObserverStatementState.SetStatementStartPos(const AValue: integer
  );
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

