{  $Id$  }
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
}
unit CodeExplorer;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LCLType, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, ComCtrls, Menus,
  // CodeTools
  CodeToolManager, CodeAtom, CodeCache, CodeTree, PascalParserTool,
  KeywordFuncLists,
  // IDE Intf
  IDECommands, MenuIntf,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs, InputHistory, IDEProcs,
  CodeExplOpts, StdCtrls;

type
  TCodeExplorerView = class;

  TOnGetCodeTree =
    procedure(Sender: TObject; var ACodeTool: TCodeTool) of object;
  TOnJumpToCode = procedure(Sender: TObject; const Filename: string;
    const Caret: TPoint; TopLine: integer) of object;

  TCodeExplorerViewFlag = (
    cevRefreshNeeded,
    cevRefresing
    );
  TCodeExplorerViewFlags = set of TCodeExplorerViewFlag;

  { TCodeExplorerView }

  TCodeExplorerView = class(TForm)
    FilterEdit: TEdit;
    Imagelist1: TImageList;
    TreePopupmenu: TPopupMenu;
    RefreshButton: TButton;
    OptionsButton: TButton;
    CodeTreeview: TTreeView;
    procedure CodeExplorerViewClose(Sender: TObject;
                                    var CloseAction: TCloseAction);
    procedure CodeExplorerViewCreate(Sender: TObject);
    procedure CodeExplorerViewDestroy(Sender: TObject);
    procedure CodeExplorerViewResize(Sender: TObject);
    procedure CodeTreeviewDblClick(Sender: TObject);
    procedure CodeTreeviewDeletion(Sender: TObject; Node: TTreeNode);
    procedure CodeTreeviewKeyUp(Sender: TObject; var Key: Word;
                                Shift: TShiftState);
    procedure FilterEditChange(Sender: TObject);
    procedure JumpToMenuitemClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure RefreshMenuitemClick(Sender: TObject);
  private
    FMainFilename: string;
    FFlags: TCodeExplorerViewFlags;
    FOnGetCodeTree: TOnGetCodeTree;
    FOnJumpToCode: TOnJumpToCode;
    FUpdateCount: integer;
    ImgIDDefault: integer;
    ImgIDProgram: Integer;
    ImgIDUnit: Integer;
    ImgIDInterfaceSection: Integer;
    ImgIDImplementation: Integer;
    ImgIDInitialization: Integer;
    ImgIDFinalization: Integer;
    ImgIDTypeSection: Integer;
    ImgIDType: Integer;
    ImgIDVarSection: Integer;
    ImgIDVariable: Integer;
    ImgIDConstSection: Integer;
    ImgIDConst: Integer;
    ImgIDClass: Integer;
    ImgIDProc: Integer;
    ImgIDProperty: Integer;
    FLastFilter: string;
    function GetFilter: string;
    function GetNodeDescription(ACodeTool: TCodeTool;
                                CodeNode: TCodeTreeNode): string;
    function GetNodeImage(CodeNode: TCodeTreeNode): integer;
    procedure CreateNodes(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode;
           ParentViewNode, InFrontViewNode: TTreeNode; CreateSiblings: boolean);
    procedure SetFilter(const AValue: string);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Refresh;
    procedure JumpToSelection;
    procedure CurrentCodeBufferChanged;
    procedure FilterChanged;
    function FilterNode(ANode: TTreeNode; const TheFilter: string): boolean;
    function FilterFits(const NodeText, TheFilter: string): boolean; virtual;
  public
    property OnGetCodeTree: TOnGetCodeTree read FOnGetCodeTree
                                           write FOnGetCodeTree;
    property OnJumpToCode: TOnJumpToCode read FOnJumpToCode write FOnJumpToCode;
    property MainFilename: string read FMainFilename;
    property Filter: string read GetFilter write SetFilter;
  end;

const
  CodeExplorerMenuRootName = 'Code Explorer';

var
  CodeExplorerView: TCodeExplorerView;
  CEJumpToIDEMenuCommand: TIDEMenuCommand;
  CERefreshIDEMenuCommand: TIDEMenuCommand;

procedure InitCodeExplorerOptions;
procedure LoadCodeExplorerOptions;
procedure SaveCodeExplorerOptions;

procedure RegisterStandardCodeExplorerMenuItems;


implementation


type
  TViewNodeData = class
  public
    Desc: TCodeTreeNodeDesc;
    SubDesc: TCodeTreeNodeSubDesc;
    StartPos, EndPos: integer;
    constructor Create(CodeNode: TCodeTreeNode);
  end;

procedure InitCodeExplorerOptions;
begin
  if CodeExplorerOptions=nil then
   CodeExplorerOptions:=TCodeExplorerOptions.Create;
end;

procedure LoadCodeExplorerOptions;
begin
  InitCodeExplorerOptions;
  CodeExplorerOptions.Load;
end;

procedure SaveCodeExplorerOptions;
begin
  CodeExplorerOptions.Save;
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

  procedure AddResImg(ImgList: TImageList; const ResName: string;
    out ImgID: integer);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    if LazarusResources.Find(ResName)=nil then
      DebugLn('TCodeExplorerView.CodeExplorerViewCREATE: ',
        ' WARNING: icon not found: "',ResName,'"');
    Pixmap.LoadFromLazarusResource(ResName);
    ImgID:=ImgList.Add(Pixmap, nil);
    Pixmap.Free;
  end;

begin
  LoadCodeExplorerOptions;

  Name:=NonModalIDEWindowNames[nmiwCodeExplorerName];
  Caption := lisMenuViewCodeExplorer;
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  
  RefreshButton.Caption:=dlgUnitDepRefresh;
  OptionsButton.Caption:=dlgFROpts;
  FilterEdit.Text:=lisCEFilter;
  
  AddResImg(Imagelist1,'ce_default',ImgIDDefault);
  AddResImg(Imagelist1,'ce_program',ImgIDProgram);
  AddResImg(Imagelist1,'ce_unit',ImgIDUnit);
  AddResImg(Imagelist1,'ce_interface',ImgIDInterfaceSection);
  AddResImg(Imagelist1,'ce_implementation',ImgIDImplementation);
  AddResImg(Imagelist1,'ce_initialization',ImgIDInitialization);
  AddResImg(Imagelist1,'ce_finalization',ImgIDFinalization);
  AddResImg(Imagelist1,'ce_type',ImgIDTypeSection);
  AddResImg(Imagelist1,'ce_type',ImgIDType);
  AddResImg(Imagelist1,'ce_variable',ImgIDVarSection);
  AddResImg(Imagelist1,'ce_variable',ImgIDVariable);
  AddResImg(Imagelist1,'ce_const',ImgIDConstSection);
  AddResImg(Imagelist1,'ce_const',ImgIDConst);
  AddResImg(Imagelist1,'ce_class',ImgIDClass);
  AddResImg(Imagelist1,'ce_procedure',ImgIDProc);
  AddResImg(Imagelist1,'ce_property',ImgIDProperty);
  
  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  CodeExplorerMenuRoot.MenuItem:=TreePopupMenu.Items;
  //CodeExplorerMenuRoot.Items.WriteDebugReport(' ');

  CEJumpToIDEMenuCommand.OnClick:=@JumpToMenuitemCLICK;
  CERefreshIDEMenuCommand.OnClick:=@RefreshMenuitemCLICK;
end;

procedure TCodeExplorerView.CodeExplorerViewDestroy(Sender: TObject);
begin
  //debugln('TCodeExplorerView.CodeExplorerViewDestroy');
end;

procedure TCodeExplorerView.CodeExplorerViewRESIZE(Sender: TObject);
begin
  RefreshButton.Width:=ClientWidth div 2;
  with OptionsButton do
    SetBounds(RefreshButton.Width,Top,
              Parent.ClientWidth-RefreshButton.Width,Height);
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

procedure TCodeExplorerView.CodeTreeviewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift=[]) then
    JumpToSelection;
end;

procedure TCodeExplorerView.FilterEditChange(Sender: TObject);
begin
  if Sender=nil then ;
  FilterChanged;
end;

procedure TCodeExplorerView.CodeExplorerViewCLOSE(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  EnvironmentOptions.IDEWindowLayoutList.ItemByForm(Self).GetCurrentPosition;
end;

procedure TCodeExplorerView.JumpToMenuitemCLICK(Sender: TObject);
begin
  JumpToSelection;
end;

procedure TCodeExplorerView.OptionsButtonClick(Sender: TObject);
begin
  if ShowCodeExplorerOptions=mrOk then
    SaveCodeExplorerOptions;
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
                   phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);
                   
  ctnProperty:
    Result:='property '+ACodeTool.ExtractPropName(CodeNode,false);

  else
    Result:=CodeNode.DescAsString;
  end;
end;

function TCodeExplorerView.GetFilter: string;
begin
  Result:=FilterEdit.Text;
  if Result=lisCEFilter then Result:='';
end;

function TCodeExplorerView.GetNodeImage(CodeNode: TCodeTreeNode): integer;
begin
  case CodeNode.Desc of
  ctnProgram,ctnLibrary,ctnPackage:
    Result:=ImgIDProgram;
  ctnUnit:
    Result:=ImgIDUnit;
  ctnInterface:
    Result:=ImgIDInterfaceSection;
  ctnImplementation:
    Result:=ImgIDImplementation;
  ctnInitialization:
    Result:=ImgIDInitialization;
  ctnFinalization:
    Result:=ImgIDFinalization;
  ctnTypeSection:
    Result:=ImgIDTypeSection;
  ctnTypeDefinition:
    Result:=ImgIDType;
  ctnVarSection:
    Result:=ImgIDVarSection;
  ctnVarDefinition:
    Result:=ImgIDVariable;
  ctnConstSection,ctnResStrSection:
    Result:=ImgIDConstSection;
  ctnConstDefinition:
    Result:=ImgIDConst;
  ctnClass:
    Result:=ImgIDClass;
  ctnProcedure:
    Result:=ImgIDProc;
  ctnProperty:
    Result:=ImgIDProperty;
  else
    Result:=ImgIDDefault;
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
  ShowNode: Boolean;
  ShowChilds: Boolean;
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
    and (CodeNode.FirstChild<>nil) and (CodeNode.FirstChild.Desc=ctnClass)
    and ((CodeNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0) then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;
    // don't show class node (the type node is already shown)
    if (CodeNode.Desc in [ctnClass,ctnClassInterface]) then begin
      ShowNode:=false;
      ShowChilds:=true;
    end;

    // don't show keyword nodes
    if CodeNode.Desc in [ctnIdentifier,ctnRangedArrayType,
      ctnOpenArrayType,ctnOfConstType,ctnRangeType,ctnTypeType,ctnFileType,
      ctnVariantType]
    then
      ShowNode:=false;
      
    // don't show End.
    if CodeNode.Desc=ctnEndPoint then
      ShowNode:=false;

    ViewNode:=ParentViewNode;
    if ShowNode then begin
      NodeData:=TViewNodeData.Create(CodeNode);
      NodeText:=GetNodeDescription(ACodeTool,CodeNode);
      NodeImageIndex:=GetNodeImage(CodeNode);
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
    end;
    if ShowChilds then
      CreateNodes(ACodeTool,CodeNode.FirstChild,ViewNode,nil,true);
    if not CreateSiblings then break;
    CodeNode:=CodeNode.NextBrother;
  end;
end;

procedure TCodeExplorerView.SetFilter(const AValue: string);
begin
  if Filter=AValue then exit;
  FilterEdit.Text:=AValue;
  FilterChanged;
end;

procedure TCodeExplorerView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

destructor TCodeExplorerView.Destroy;
begin
  inherited Destroy;
  if CodeExplorerView=Self then
    CodeExplorerView:=nil;
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
  
  FilterEdit.Text:=lisCEFilter;

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
  if tvoAllowMultiselect in CodeTreeview.Options then
    CurItem:=CodeTreeView.GetFirstMultiSelected
  else
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

procedure TCodeExplorerView.CurrentCodeBufferChanged;
begin
  if CodeExplorerOptions.Refresh=cerSwitchEditorPage then
    Refresh;
end;

procedure TCodeExplorerView.FilterChanged;
var
  TheFilter: String;
  ANode: TTreeNode;
begin
  if FUpdateCount>0 then begin
    Include(FFlags,cevRefreshNeeded);
    exit;
  end;
  TheFilter:=FilterEdit.Text;
  if FLastFilter=TheFilter then exit;
  FLastFilter:=TheFilter;
  CodeTreeview.BeginUpdate;
  CodeTreeview.Options:=CodeTreeview.Options+[tvoAllowMultiselect];
  ANode:=CodeTreeview.Items.GetFirstNode;
  while ANode<>nil do begin
    FilterNode(ANode,TheFilter);
    ANode:=ANode.GetNextSibling;
  end;
  CodeTreeview.EndUpdate;
end;

function TCodeExplorerView.FilterNode(ANode: TTreeNode; const TheFilter: string
  ): boolean;
var
  ChildNode: TTreeNode;
  HasVisibleChilds: Boolean;
begin
  if ANode=nil then exit;
  ChildNode:=ANode.GetFirstChild;
  HasVisibleChilds:=false;
  while ChildNode<>nil do begin
    if FilterNode(ChildNode,TheFilter) then
      HasVisibleChilds:=true;
    ChildNode:=ChildNode.GetNextSibling;
  end;
  ANode.Expanded:=HasVisibleChilds;
  ANode.Selected:=FilterFits(ANode.Text,TheFilter);
  Result:=ANode.Expanded or ANode.Selected;
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
            exit(true);
          end;
        end;
      end else
        exit(false);
      inc(Src);
    until false;
  end;
end;

initialization
  {$I codeexplorer.lrs}
  CodeExplorerView:=nil;

end.

