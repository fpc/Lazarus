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

  Abstract:
    Window showing the current source as tree structure.
    Normally it shows the codetools nodes of the current unit in the
    source editor. If an include file is open, the corresponding unit is shown.
}
unit CodeExplorer;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, LCLProc, LCLType, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, ComCtrls, Menus,
  // CodeTools
  CodeToolManager, CodeAtom, CodeCache, CodeTree, KeywordFuncLists,
  FindDeclarationTool, DirectivesTree, PascalParserTool,
  // IDE Intf
  IDECommands, MenuIntf,
  // IDE
  LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs, InputHistory, IDEProcs,
  CodeExplOpts, StdCtrls, ExtCtrls;

type
  TCodeExplorerView = class;

  TOnGetCodeTree =
                 procedure(Sender: TObject; var ACodeTool: TCodeTool) of object;
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

  { TCodeExplorerView }

  TCodeExplorerView = class(TForm)
    CodeFilterEdit: TEdit;
    CodePage: TPage;
    CodeTreeview: TTreeView;
    DirectivesFilterEdit: TEdit;
    DirectivesPage: TPage;
    DirectivesTreeView: TTreeView;
    Imagelist1: TImageList;
    MainNotebook: TNotebook;
    MenuItem1: TMenuItem;
    CodeTreeviewButtonPanel: TPanel;
    OptionsSpeedButton: TSpeedButton;
    RefreshSpeedButton: TSpeedButton;
    ModeSpeedButton: TSpeedButton;
    TreePopupmenu: TPopupMenu;
    procedure CodeExplorerViewClose(Sender: TObject;
                                    var CloseAction: TCloseAction);
    procedure CodeExplorerViewCreate(Sender: TObject);
    procedure CodeExplorerViewDestroy(Sender: TObject);
    procedure CodeExplorerViewResize(Sender: TObject);
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
    procedure JumpToMenuitemClick(Sender: TObject);
    procedure MainNotebookPageChanged(Sender: TObject);
    procedure ModeSpeedButtonClick(Sender: TObject);
    procedure OptionsSpeedButtonClick(Sender: TObject);
    procedure RefreshMenuitemClick(Sender: TObject);
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure RefreshSpeedButtonClick(Sender: TObject);
  private
    FCodeFilename: string;
    fCategoryNodes: array[TCodeExplorerCategory] of TTreeNode;
    FDirectivesFilename: string;
    FFlags: TCodeExplorerViewFlags;
    FLastCodeFilter: string;
    FLastCodeChangeStep: integer;
    FLastDirectivesFilter: string;
    FLastDirectivesChangeStep: integer;
    FMode: TCodeExplorerMode;
    FLastMode: TCodeExplorerMode;
    FOnGetCodeTree: TOnGetCodeTree;
    FOnGetDirectivesTree: TOnGetDirectivesTree;
    FOnJumpToCode: TOnJumpToCode;
    FUpdateCount: integer;
    ImgIDClass: Integer;
    ImgIDConst: Integer;
    ImgIDConstSection: Integer;
    ImgIDDefault: integer;
    ImgIDFinalization: Integer;
    ImgIDImplementation: Integer;
    ImgIDInitialization: Integer;
    ImgIDInterfaceSection: Integer;
    ImgIDProcedure: Integer;
    ImgIDFunction: Integer;
    ImgIDProgram: Integer;
    ImgIDProperty: Integer;
    ImgIDType: Integer;
    ImgIDTypeSection: Integer;
    ImgIDUnit: Integer;
    ImgIDVariable: Integer;
    ImgIDVarSection: Integer;
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
    procedure CreateNodes(ACodeTool: TCodeTool; CodeNode: TCodeTreeNode;
                          ParentViewNode, InFrontViewNode: TTreeNode;
                          CreateSiblings: boolean);
    procedure CreateNodes(ADirectivesTool: TDirectivesTool;
                          CodeNode: TCodeTreeNode;
                          ParentViewNode, InFrontViewNode: TTreeNode;
                          CreateSiblings: boolean);
    procedure SetCodeFilter(const AValue: string);
    procedure SetCurrentPage(const AValue: TCodeExplorerPage);
    procedure SetDirectivesFilter(const AValue: string);
    procedure SetMode(AMode: TCodeExplorerMode);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure ApplyCodeFilter;
    procedure ApplyDirectivesFilter;
    function CompareCodeNodes(Node1, Node2: TTreeNode): integer;
  public
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CheckOnIdle;
    procedure Refresh(OnlyVisible: boolean);
    procedure RefreshCode(OnlyVisible: boolean);
    procedure RefreshDirectives(OnlyVisible: boolean);
    procedure ClearCTNodes(ATreeView: TTreeView);// remove temporary references
    procedure JumpToSelection;
    procedure CurrentCodeBufferChanged;
    procedure CodeFilterChanged;
    procedure DirectivesFilterChanged;
    function FilterNode(ANode: TTreeNode; const TheFilter: string): boolean;
    function FilterFits(const NodeText, TheFilter: string): boolean; virtual;
    function GetCurrentTreeView: TCustomTreeView;
  public
    property OnGetCodeTree: TOnGetCodeTree read FOnGetCodeTree
                                           write FOnGetCodeTree;
    property OnGetDirectivesTree: TOnGetDirectivesTree read FOnGetDirectivesTree
                                                     write FOnGetDirectivesTree;
    property OnJumpToCode: TOnJumpToCode read FOnJumpToCode write FOnJumpToCode;
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
    CTNode: TCodeTreeNode;
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
  CTNode:=CodeNode;
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
  
  Mode:=CodeExplorerOptions.Mode;

  Name:=NonModalIDEWindowNames[nmiwCodeExplorerName];
  Caption := lisMenuViewCodeExplorer;
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  
  RefreshSpeedButton.Hint:=dlgUnitDepRefresh;
  OptionsSpeedButton.Hint:=dlgFROpts;
  CodeFilterEdit.Text:=lisCEFilter;
  CodePage.Caption:=dlgCodeGeneration;
  DirectivesFilterEdit.Text:=lisCEFilter;
  DirectivesPage.Caption:=lisDirectives;

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
  AddResImg(Imagelist1,'ce_procedure',ImgIDProcedure);
  AddResImg(Imagelist1,'ce_function',ImgIDFunction);
  AddResImg(Imagelist1,'ce_property',ImgIDProperty);
  
  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  CodeExplorerMenuRoot.MenuItem:=TreePopupMenu.Items;
  //CodeExplorerMenuRoot.Items.WriteDebugReport(' ');

  CEJumpToIDEMenuCommand.OnClick:=@JumpToMenuitemCLICK;
  CERefreshIDEMenuCommand.OnClick:=@RefreshMenuitemCLICK;
  
  Application.AddOnIdleHandler(@OnApplicationIdle);
end;

procedure TCodeExplorerView.CodeExplorerViewDestroy(Sender: TObject);
begin
  //debugln('TCodeExplorerView.CodeExplorerViewDestroy');
end;

procedure TCodeExplorerView.CodeExplorerViewRESIZE(Sender: TObject);
begin

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

procedure TCodeExplorerView.CodeExplorerViewCLOSE(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  EnvironmentOptions.IDEWindowLayoutList.ItemByForm(Self).GetCurrentPosition;
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
  else SetMode(cemCategory);
end;

procedure TCodeExplorerView.OptionsSpeedButtonClick(Sender: TObject);
begin
  if ShowCodeExplorerOptions=mrOk then
    SaveCodeExplorerOptions;
end;

procedure TCodeExplorerView.RefreshMenuitemCLICK(Sender: TObject);
begin
  Refresh(true);
end;

procedure TCodeExplorerView.OnApplicationIdle(Sender: TObject; var Done: Boolean
  );
begin
  if (cevCheckOnIdle in FFlags) or (CodeExplorerOptions.Refresh=cerOnIdle) then
    Refresh(true);
end;

procedure TCodeExplorerView.RefreshSpeedButtonClick(Sender: TObject);
begin
  Refresh(true);
end;

function TCodeExplorerView.GetCodeNodeDescription(ACodeTool: TCodeTool;
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
                  [// phpWithStart is no needed because there are icons
                   phpWithVarModifiers,
                   phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
                   phpWithOfObject,phpWithCallingSpecs,phpWithProcModifiers]);
                   
  ctnProperty:
    Result:=ACodeTool.ExtractPropName(CodeNode,false); // property keyword is not needed because there are icons

  else
    Result:=CodeNode.DescAsString;
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
  if MainNotebook.ActivePageComponent=CodePage then
    Result:=cepCode
  else if MainNotebook.ActivePageComponent=DirectivesPage then
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
  ctnProgram,ctnLibrary,ctnPackage:   Result:=ImgIDProgram;
  ctnUnit:                            Result:=ImgIDInterfaceSection;
  ctnImplementation:                  Result:=ImgIDImplementation;
  ctnInitialization:                  Result:=ImgIDInitialization;
  ctnFinalization:                    Result:=ImgIDFinalization;
  ctnTypeSection:                     Result:=ImgIDTypeSection;
  ctnTypeDefinition:                  Result:=ImgIDType;
  ctnVarSection:                      Result:=ImgIDVarSection;
  ctnVarDefinition:                   Result:=ImgIDVariable;
  ctnConstSection,ctnResStrSection:   Result:=ImgIDConstSection;
  ctnConstDefinition:                 Result:=ImgIDConst;
  ctnClass:                           Result:=ImgIDClass;
  ctnProcedure:                       if Tool.NodeIsFunction(CodeNode) then
                                        Result:=ImgIDFunction
                                      else
                                        Result:=ImgIDProcedure;
  ctnProperty:                        Result:=ImgIDProperty;
  else
    Result:=ImgIDDefault;
  end;
end;

function TCodeExplorerView.GetDirectiveNodeImage(CodeNode: TCodeTreeNode
  ): integer;
begin
  case CodeNode.SubDesc of
  cdnsInclude:  Result:=ImgIDVarSection;
  else
    case CodeNode.Desc of
    cdnIf:     Result:=ImgIDTypeSection;
    cdnElseIf: Result:=ImgIDTypeSection;
    cdnElse:   Result:=ImgIDTypeSection;
    cdnEnd:    Result:=ImgIDTypeSection;
    cdnDefine: Result:=ImgIDConst;
    else
      Result:=ImgIDDefault;
    end;
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
    and (CodeNode.FirstChild<>nil) and (CodeNode.FirstChild.Desc=ctnClass)
    and ((CodeNode.FirstChild.SubDesc and ctnsForwardDeclaration)>0) then begin
      ShowNode:=false;
      ShowChilds:=false;
    end;
    // don't show class node (the type node is already shown)
    if (CodeNode.Desc in [ctnClass,ctnClassInterface]) then begin
      ShowNode:=false;
    end;

    // don't show keyword nodes
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
      // category mode: do not show sections
      if (CodeNode.Desc in AllDefinitionSections) then begin
        ShowNode:=false;
      end;
      
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
      or (CodeNode.Parent.Desc in [ctnInterface,ctnImplementation])
      or (CodeNode.Parent.Parent=nil)
      or (CodeNode.Parent.Parent.Desc in [ctnInterface,ctnImplementation])) then
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
      end;
      if Category<>cecNone then begin
        ShowNode:=Category in CodeExplorerOptions.Categories;
        ShowChilds:=CodeNode.Desc in [ctnTypeDefinition,ctnGenericType];
        if ShowNode then begin
          if fCategoryNodes[Category]=nil then begin
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
      end;

      // category mode: do not show code sections
      if (CodeNode.Desc in AllCodeSections) then begin
        ShowNode:=false;
      end;
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
      CreateNodes(ACodeTool,CodeNode.FirstChild,ViewNode,nil,true);
    if not CreateSiblings then break;
    CodeNode:=CodeNode.NextBrother;
  end;
end;

procedure TCodeExplorerView.CreateNodes(ADirectivesTool: TDirectivesTool;
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
      CreateNodes(ADirectivesTool,CodeNode.FirstChild,ViewNode,nil,true);
    if not CreateSiblings then break;
    CodeNode:=CodeNode.NextBrother;
  end;
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
  cepCode:       MainNotebook.ActivePageComponent:=CodePage;
  cepDirectives: MainNotebook.ActivePageComponent:=DirectivesPage;
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
  if FMode=cemCategory then
  begin
    ModeSpeedButton.Caption:='C'; // To-Do: Change it to use image instead of 'C'.
    ModeSpeedButton.Hint:=lisCEModeShowSourceNodes;
  end
  else begin
    ModeSpeedButton.Caption:='S'; // To-Do: Change it to use image instead of 'S'.
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

var
  OldExpanded: TTreeNodeExpandedState;
  ACodeTool: TCodeTool;
  c: TCodeExplorerCategory;
begin
  if (FUpdateCount>0)
  or (OnlyVisible and ((CurrentPage<>cepCode) or (not IsVisible))) then begin
    Include(FFlags,cevCodeRefreshNeeded);
    exit;
  end;
  Exclude(FFlags,cevCodeRefreshNeeded);

  try
    Include(FFlags,cevRefreshing);
    
    CodeFilterEdit.Text:=lisCEFilter;

    // get the codetool with the updated codetree
    ACodeTool:=nil;
    if Assigned(OnGetCodeTree) then
      OnGetCodeTree(Self,ACodeTool);

    // check for changes in the codetools
    if (ACodeTool=nil) then begin
      if (FCodeFilename='') then begin
        // still no tool
        exit;
      end;
    end else begin
      if (ACodeTool.MainFilename=FCodeFilename)
      and (ACodeTool.Scanner<>nil)
      and (ACodeTool.Scanner.ChangeStep=FLastCodeChangeStep)
      and (Mode=FLastMode) then begin
        // still the same source
        exit;
      end;
    end;

    FLastMode:=Mode;
    // remember the codetools ChangeStep
    if ACodeTool<>nil then begin
      FCodeFilename:=ACodeTool.MainFilename;
      if ACodeTool.Scanner<>nil then
        FLastCodeChangeStep:=ACodeTool.Scanner.ChangeStep;
    end else
      FCodeFilename:='';
      
    //DebugLn(['TCodeExplorerView.RefreshCode ',FCodeFilename]);

    // start updating the CodeTreeView
    CodeTreeview.BeginUpdate;
    OldExpanded:=TTreeNodeExpandedState.Create(CodeTreeView);

    for c:=low(TCodeExplorerCategory) to high(TCodeExplorerCategory) do
      fCategoryNodes[c]:=nil;
    if (ACodeTool=nil) or (ACodeTool.Tree=nil) or (ACodeTool.Tree.Root=nil) then
    begin
      CodeTreeview.Items.Clear;
    end else begin
      CodeTreeview.Items.Clear;
      CreateNodes(ACodeTool,ACodeTool.Tree.Root,nil,nil,true);
    end;

    // restore old expanded state
    OldExpanded.Apply(CodeTreeView);
    OldExpanded.Free;
    CodeTreeview.CustomSort(@CompareCodeNodes);
    
    AutoExpandNodes;
    
    ClearCTNodes(CodeTreeview);
    CodeTreeview.EndUpdate;

  finally
    Exclude(FFlags,cevRefreshing);
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

    DirectivesFilterEdit.Text:=lisCEFilter;

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
      CreateNodes(ADirectivesTool,ADirectivesTool.Tree.Root,nil,nil,true);
    end;

    // restore old expanded state
    OldExpanded.Apply(DirectivesTreeView);
    OldExpanded.Free;
    ClearCTNodes(DirectivesTreeView);
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

procedure TCodeExplorerView.JumpToSelection;
var
  CurItem: TTreeNode;
  CurNode: TViewNodeData;
  Caret: TCodeXYPosition;
  NewTopLine: integer;
  CodeBuffer: TCodeBuffer;
  ACodeTool: TCodeTool;
  CurTreeView: TCustomTreeView;
begin
  CurTreeView:=GetCurrentTreeView;
  if CurTreeView=nil then exit;
  if tvoAllowMultiselect in CurTreeView.Options then
    CurItem:=CurTreeView.GetFirstMultiSelected
  else
    CurItem:=CurTreeView.Selected;
  if CurItem=nil then exit;
  CurNode:=TViewNodeData(CurItem.Data);
  if CurNode.StartPos<1 then exit;

  case CurrentPage of
  cepCode:
    begin
      CodeBuffer:=CodeToolBoss.FindFile(CodeFilename);
      if CodeBuffer=nil then exit;
      ACodeTool:=nil;
      CodeToolBoss.Explore(CodeBuffer,ACodeTool,false);
      if ACodeTool=nil then exit;
      if not ACodeTool.CleanPosToCaretAndTopLine(CurNode.StartPos,Caret,NewTopLine)
      then exit;
    end;
  cepDirectives:
    begin
      CodeBuffer:=CodeToolBoss.FindFile(DirectivesFilename);
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
var
  Data1: TViewNodeData;
  Data2: TViewNodeData;
begin
  Data1:=TViewNodeData(Node1.Data);
  Data2:=TViewNodeData(Node2.Data);
  if (Mode=cemCategory) then begin
    if (Data1.Desc in SortDesc)
    and (Data2.Desc in SortDesc) then begin
      Result:=SysUtils.CompareText(Node1.Text,Node2.Text);
      if Result<>0 then exit;
    end;
  end;
  if Data1.StartPos<Data2.StartPos then
    Result:=-1
  else if Data1.StartPos>Data2.StartPos then
    Result:=1
  else
    Result:=0;
end;

initialization
  {$I codeexplorer.lrs}
  CodeExplorerView:=nil;

end.

