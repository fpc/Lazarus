unit CodeExplorer; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls,
  CodeToolManager, CodeAtom, CodeCache, CodeTree, PascalParserTool,
  EnvironmentOpts, IDEOptionDefs, LazarusIDEStrConsts, InputHistory, IDEProcs,
  Menus;

type
  TOnGetCodeTree =
    procedure(Sender: TObject; var ACodeTool: TCodeTool) of object;
  TOnJumpToCode = procedure(Sender: TObject; const Filename: string;
    const Caret: TPoint; TopLine: integer) of object;

  TCodeExplorerViewFlag = (
    cevRefreshNeeded,
    cevRefresing
    );
  TCodeExplorerViewFlags = set of TCodeExplorerViewFlag;

  TCodeExplorerView = class(TForm)
    Imagelist1: TIMAGELIST;
    JumpToMenuitem: TMENUITEM;
    RefreshMenuitem: TMENUITEM;
    TreePopupmenu: TPOPUPMENU;
    RefreshButton: TBUTTON;
    OptionsButton: TBUTTON;
    CodeTreeview: TTREEVIEW;
    procedure CodeExplorerViewCLOSE(Sender: TObject; var Action: TCloseAction);
    procedure CodeExplorerViewCREATE(Sender: TObject);
    procedure CodeExplorerViewRESIZE(Sender: TObject);
    procedure CodeTreeviewDBLCLICK(Sender: TObject);
    procedure CodeTreeviewDELETION(Sender: TObject; Node: TTreeNode);
    procedure JumpToMenuitemCLICK(Sender: TObject);
    procedure RefreshButtonCLICK(Sender: TObject);
    procedure RefreshMenuitemCLICK(Sender: TObject);
  private
    FMainFilename: string;
    FFlags: TCodeExplorerViewFlags;
    FOnGetCodeTree: TOnGetCodeTree;
    FOnJumpToCode: TOnJumpToCode;
    FUpdateCount: integer;
    function GetNodeDescription(ACodeTool: TCodeTool;
                                CodeNode: TCodeTreeNode): string;
    procedure CreateNodes(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode;
           ParentViewNode, InFrontViewNode: TTreeNode; CreateSiblings: boolean);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Refresh;
    procedure JumpToSelection;
    property OnGetCodeTree: TOnGetCodeTree read FOnGetCodeTree
                                           write FOnGetCodeTree;
    property OnJumpToCode: TOnJumpToCode read FOnJumpToCode write FOnJumpToCode;
    property MainFilename: string read FMainFilename;
  end;

var
  CodeExplorerView: TCodeExplorerView;

implementation

type
  TViewNodeData = class
  public
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    StartPos, EndPos: integer;
    constructor Create(CodeNode: TCodeTreeNode);
  end;

{ TViewNodeData }

constructor TViewNodeData.Create(CodeNode: TCodeTreeNode);
begin
  Desc:=CodeNode.Desc;
  SubDesc:=CodeNode.SubDesc;
  StartPos:=CodeNode.StartPos;
  EndPos:=CodeNode.EndPos;
end;

{ TCodeExplorerView }

procedure TCodeExplorerView.CodeExplorerViewCREATE(Sender: TObject);

  procedure AddResImg(ImgList: TImageList; const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    if LazarusResources.Find(ResName)=nil then
      writeln('TCodeExplorerView.CodeExplorerViewCREATE: ',
        ' WARNING: icon not found: "',ResName,'"');
    Pixmap.LoadFromLazarusResource(ResName);
    ImgList.Add(Pixmap,nil)
  end;

begin
  Name:=NonModalIDEWindowNames[nmiwCodeExplorerName];
  Caption := lisMenuViewCodeExplorer;
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  
  RefreshButton.Caption:=dlgUnitDepRefresh;
  OptionsButton.Caption:=dlgFROpts;
  
  AddResImg(Imagelist1,'srctype_unknown_22x22');
end;

procedure TCodeExplorerView.CodeExplorerViewRESIZE(Sender: TObject);
var
  y: Integer;
begin
  RefreshButton.Width:=ClientWidth div 2;
  with OptionsButton do
    SetBounds(RefreshButton.Width,Top,
              Parent.ClientWidth-RefreshButton.Width,Height);
  y:=RefreshButton.Top+RefreshButton.Height;
  with CodeTreeview do
    SetBounds(0,y,Parent.ClientWidth,Parent.ClientHeight-y);
end;

procedure TCodeExplorerView.CodeTreeviewDBLCLICK(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.CodeTreeviewDELETION(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data<>nil then
    TViewNodeData(Node.Data).Free;
end;

procedure TCodeExplorerView.CodeExplorerViewCLOSE(Sender: TObject;
  var Action: TCloseAction);
begin
  EnvironmentOptions.IDEWindowLayoutList.ItemByForm(Self).GetCurrentPosition;
end;

procedure TCodeExplorerView.JumpToMenuitemCLICK(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.RefreshButtonCLICK(Sender: TObject);
begin
  Refresh;
end;

procedure TCodeExplorerView.RefreshMenuitemCLICK(Sender: TObject);
begin
  Refresh;
end;

function TCodeExplorerView.GetNodeDescription(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode): string;
begin
  case CodeNode.Desc of
  
  ctnUnit, ctnProgram, ctnLibrary, ctnPackage:
    Result:=CodeNode.DescAsString+' '+ACodeTool.ExtractSourceName;

  ctnTypeDefinition,ctnVarDefinition,ctnConstDefinition:
    Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);

  ctnClass:
    Result:='('+ACodeTool.ExtractClassInheritance(CodeNode,[])+')';
            
  ctnEnumIdentifier:
    Result:=ACodeTool.ExtractIdentifier(CodeNode.StartPos);
    
  ctnProcedure:
    Result:=ACodeTool.ExtractProcHead(CodeNode,
                  [phpWithStart,phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                   phpWithOfObject,phpWithCallingSpecs]);
                   
  ctnProperty:
    Result:='property '+ACodeTool.ExtractPropName(CodeNode,false);

  else
    Result:=CodeNode.DescAsString;
  end;
end;

procedure TCodeExplorerView.CreateNodes(ACodeTool: TCodeTool;
  CodeNode: TCodeTreeNode;
  ParentViewNode, InFrontViewNode: TTreeNode; CreateSiblings: boolean);
var
  NodeData: TViewNodeData;
  NodeText: String;
  ViewNode: TTreeNode;
  NodeImageIndex: Integer;
begin
  if CodeNode=nil then exit;
  
  // don't show statements, parameter lists
  if (CodeNode.Desc in AllPascalStatements)
  or (CodeNode.Desc in [ctnProcedureHead,ctnParameterList]) then exit;
  
  if CodeNode.FirstChild=CodeNode.LastChild then begin
    // node has no childs or one child
    // don't show boring details
    if CodeNode.Desc in [ctnIdentifier,ctnEnumerationType,ctnRangedArrayType,
      ctnOpenArrayType,ctnOfConstType,ctnRangeType,ctnTypeType,ctnFileType,
      ctnVariantType]
    then begin
      CreateNodes(ACodeTool,CodeNode.FirstChild,ParentViewNode,InFrontViewNode,
                  true);
      exit;
    end;
  end;

  NodeData:=TViewNodeData.Create(CodeNode);
  NodeText:=GetNodeDescription(ACodeTool,CodeNode);
  NodeImageIndex:=0;
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
  CreateNodes(ACodeTool,CodeNode.FirstChild,ViewNode,nil,true);
  if CreateSiblings then begin
    CreateNodes(ACodeTool,CodeNode.NextBrother,ParentViewNode,ViewNode,true);
  end;
end;

procedure TCodeExplorerView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TCodeExplorerView.EndUpdate;
begin
  if FUpdateCount<=0 then
    RaiseException('TCodeExplorerView.EndUpdate');
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    if cevRefreshNeeded in FFlags then Refresh;
  end;
end;

procedure TCodeExplorerView.Refresh;
var
  OldExpanded: TTreeNodeExpandedState;
  ACodeTool: TCodeTool;
begin
  if FUpdateCount>0 then begin
    Include(FFlags,cevRefreshNeeded);
    exit;
  end;
  Exclude(FFlags,cevRefreshNeeded);
  
  Include(FFlags,cevRefresing);

  // get the codetool with the updated codetree
  ACodeTool:=nil;
  if Assigned(OnGetCodeTree) then
    OnGetCodeTree(Self,ACodeTool);

  // start updating the CodeTreeView
  CodeTreeview.BeginUpdate;
  OldExpanded:=TTreeNodeExpandedState.Create(CodeTreeView);

  if (ACodeTool=nil) or (ACodeTool.Tree=nil) or (ACodeTool.Tree.Root=nil) then
  begin
    CodeTreeview.Items.Clear;
    FMainFilename:='';
  end else begin
    FMainFilename:=ACodeTool.MainFilename;
    CodeTreeview.Items.Clear;
    CreateNodes(ACodeTool,ACodeTool.Tree.Root,nil,nil,true);
  end;

  // restore old expanded state
  OldExpanded.Apply(CodeTreeView);
  OldExpanded.Free;
  CodeTreeview.EndUpdate;
  
  Exclude(FFlags,cevRefresing);
end;

procedure TCodeExplorerView.JumpToSelection;
var
  CurItem: TTreeNode;
  CurNode: TViewNodeData;
  Caret: TCodeXYPosition;
  NewTopLine: integer;
  CodeBuffer: TCodeBuffer;
  ACodeTool: TCodeTool;
begin
  CurItem:=CodeTreeview.Selected;
  if CurItem=nil then exit;
  CurNode:=TViewNodeData(CurItem.Data);
  if CurNode.StartPos<1 then exit;

  CodeBuffer:=CodeToolBoss.FindFile(MainFilename);
  if CodeBuffer=nil then exit;
  ACodeTool:=nil;
  CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
  if ACodeTool=nil then exit;
  if not ACodeTool.CleanPosToCaretAndTopLine(CurNode.StartPos,Caret,NewTopLine)
  then exit;

  if Assigned(OnJumpToCode) then
    OnJumpToCode(Self,Caret.Code.Filename,Point(Caret.X,Caret.Y),NewTopLine);
end;

initialization
  {$I codeexplorer.lrs}
  CodeExplorerView:=nil;

end.

