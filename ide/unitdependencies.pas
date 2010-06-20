{
/***************************************************************************
                             unitdependencies.pas
                             --------------------

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
 
 
  Author: Mattias Gaertner
 
  Abstract:
    Defines the TUnitDependenciesView form.
    The Unit Dependencies shows the used units in a treeview.
 
}
unit UnitDependencies;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Controls, Forms, Dialogs, Buttons, ComCtrls, StdCtrls,
  Graphics, LCLType, FileUtil, LCLProc,
  CodeToolManager, CodeCache,
  IDECommands, IDEImagesIntf,
  EnvironmentOpts, IDEOptionDefs, LazarusIDEStrConsts, InputHistory;
  
type

  { TUnitNode }
  
  TUnitNodeFlag = (
    unfImplementation, // this unit was used in an implementation uses section
    unfCircle,         // this unit is the parent of itself
    unfForbiddenCircle,// forbidden circle
    unfFileNotFound,   // this unit file was not found
    unfParseError      // error parsing the source
    );
  TUnitNodeFlags = set of TUnitNodeFlag;
  
  TUnitNodeSourceType = (
    unstUnknown,
    unstUnit,
    unstProgram,
    unstLibrary,
    unstPackage
    );
    
const
  UnitNodeSourceTypeNames: array[TUnitNodeSourceType] of string = (
    '?',
    'Unit',
    'Program',
    'Library',
    'Package'
    );

type
  TUnitDependenciesView = class;


  { TUnitNode }

  TUnitNode = class
  private
    FChildCount: integer;
    FCodeBuffer: TCodeBuffer;
    FFilename: string;
    FFirstChild: TUnitNode;
    FFlags: TUnitNodeFlags;
    FLastChild: TUnitNode;
    FNextSibling: TUnitNode;
    FParent: TUnitNode;
    FPrevSibling: TUnitNode;
    FShortFilename: string;
    FSourceType: TUnitNodeSourceType;
    FTreeNode: TTreeNode;
    procedure SetCodeBuffer(const AValue: TCodeBuffer);
    procedure SetFilename(const AValue: string);
    procedure SetParent(const AValue: TUnitNode);
    procedure SetShortFilename(const AValue: string);
    procedure SetTreeNode(const AValue: TTreeNode);
    procedure CreateShortFilename;
    procedure UnbindFromParent;
    procedure AddToParent;
    procedure AddChild(const AFilename: string; ACodeBuffer: TCodeBuffer;
      InImplementation: boolean);
    procedure UpdateSourceType;
    function ForbiddenCircle: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearChildren;
    procedure CreateChildren;
    procedure ClearGrandChildren;
    procedure CreateGrandChildren;
    function FindParentWithCodeBuffer(ACodeBuffer: TCodeBuffer): TUnitNode;
    function HasChildren: boolean;
    function ImageIndex: integer;
    function IsFirstImplementationNode: boolean;
    function IsImplementationNode: boolean;
    property ChildCount: integer read FChildCount;
    property CodeBuffer: TCodeBuffer read FCodeBuffer write SetCodeBuffer;
    property Filename: string read FFilename write SetFilename;
    property FirstChild: TUnitNode read FFirstChild;
    property Flags: TUnitNodeFlags read FFlags;
    property LastChild: TUnitNode read FLastChild;
    property NextSibling: TUnitNode read FNextSibling;
    property PrevSibling: TUnitNode read FPrevSibling;
    property Parent: TUnitNode read FParent write SetParent;
    property ShortFilename: string read FShortFilename write SetShortFilename;
    property SourceType: TUnitNodeSourceType read FSourceType;
    property TreeNode: TTreeNode read FTreeNode write SetTreeNode;
  end;
  
  
  { TUnitDependenciesView }
  
  TOnGetProjectMainFilename = function(Sender: TObject): string of object;
  TOnOpenFile = procedure(Sender: TObject; const Filename: string) of object;

  TUnitDependenciesView = class(TForm)
    CloseButton: TBitBtn;
    ToolBar: TToolBar;
    RefreshButton: TToolButton;
    ShowProjectButton: TToolButton;
    SelectUnitButton: TToolButton;
    UnitHistoryList: TComboBox;
    UnitTreeView: TTreeView;
    procedure RefreshButtonClick(Sender: TObject);
    procedure SelectUnitButtonClick(Sender: TObject);
    procedure ShowProjectButtonClick(Sender: TObject);
    procedure UnitHistoryListEditingDone(Sender: TObject);
    procedure UnitTreeViewAdvancedCustomDrawItem(Sender: TCustomTreeView;
          Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
          var PaintImages, DefaultDraw: Boolean);
    procedure UnitTreeViewCollapsing(Sender: TObject; Node: TTreeNode;
          var AllowCollapse: Boolean);
    procedure UnitTreeViewExpanding(Sender: TObject; Node: TTreeNode;
          var AllowExpansion: Boolean);
    procedure UnitTreeViewMouseDown(Sender: TOBject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
  private
    FCommitUnitHistoryListSelectionNeeded: boolean;
    FOnAccessingSources: TNotifyEvent;
    FOnGetProjectMainFilename: TOnGetProjectMainFilename;
    FOnOpenFile: TOnOpenFile;
    FRefreshNeeded: boolean;
    FRebuildTreeNeeded: boolean;
    FRefreshHistoryListNeeded: boolean;
    FRootCodeBuffer: TCodeBuffer;
    FRootFilename: string;
    FRootNode: TUnitNode;
    FRootShortFilename: string;
    FRootValid: boolean;
    FUpdateCount: integer;
    procedure ClearTree;
    procedure RebuildTree;
    procedure SetRootFilename(const AValue: string);
    procedure SetRootShortFilename(const AValue: string);
    procedure CommitUnitHistoryListSelection;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Refresh;
    procedure RefreshHistoryList;
    function RootValid: boolean;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    property OnAccessingSources: TNotifyEvent
      read FOnAccessingSources write FOnAccessingSources;
    property OnGetProjectMainFilename: TOnGetProjectMainFilename
      read FOnGetProjectMainFilename write FOnGetProjectMainFilename;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property RootFilename: string read FRootFilename write SetRootFilename;
    property RootShortFilename: string read FRootShortFilename write SetRootShortFilename;
    property RefreshNeeded: boolean read FRefreshNeeded write FRefreshNeeded;
  end;
  
var
  UnitDependenciesView: TUnitDependenciesView = nil;

implementation

{$R *.lfm}

type
  { TExpandedUnitNodeState
    Used to save which TUnitNodes are expanded during a Refresh }

  TExpandedUnitNodeState = class
  private
    FPaths: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(ANode: TUnitNode);
    procedure AssignTo(ANode: TUnitNode);
  end;

{ TUnitDependenciesView }

procedure TUnitDependenciesView.RefreshButtonClick(Sender: TObject);
begin
  Refresh;
end;

procedure TUnitDependenciesView.SelectUnitButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist];
    if OpenDialog.Execute then begin
      RootFilename:=ExpandFileNameUTF8(OpenDialog.Filename);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TUnitDependenciesView.ShowProjectButtonClick(Sender: TObject);
var
  NewFilename: string;
begin
  if Assigned(OnGetProjectMainFilename) then begin
    NewFilename:=OnGetProjectMainFilename(Self);
    if NewFilename<>'' then
      RootFilename:=NewFilename;
  end;
end;

procedure TUnitDependenciesView.UnitHistoryListEditingDone(Sender: TObject);
begin
  //DebugLn('TUnitDependenciesView.UnitHistoryListEditingDone ',UnitHistoryList.Text,' ',dbgs(UnitHistoryList.Items.IndexOf(UnitHistoryList.Text)));
  if UnitHistoryList.Items.IndexOf(UnitHistoryList.Text)<0 then exit;
  CommitUnitHistoryListSelection;
end;

procedure TUnitDependenciesView.UnitTreeViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  UnitNode: TUnitNode;
  NodeRect: TRect;
begin
  if Stage<>cdPostPaint then exit;
  UnitNode:=TUnitNode(Node.Data);
  if (UnitNode<>nil) and UnitNode.IsFirstImplementationNode then begin
    NodeRect:=Node.DisplayRect(false);
    NodeRect.Left:=Node.DisplayStateIconLeft;
    with Node.TreeView.Canvas do begin
      Pen.Color:=clRed;
      MoveTo(NodeRect.Left,NodeRect.Top);
      LineTo(NodeRect.Right,NodeRect.Top);
    end;
  end;
end;

procedure TUnitDependenciesView.UnitTreeViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
var
  UnitNode: TUnitNode;
begin
  AllowCollapse:=true;
  UnitNode:=TUnitNode(Node.Data);
  UnitNode.ClearGrandChildren;
end;

procedure TUnitDependenciesView.UnitTreeViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  UnitNode: TUnitNode;
begin
  UnitNode:=TUnitNode(Node.Data);
  if UnitNode.HasChildren then begin
    UnitTreeView.BeginUpdate;
    try
      AllowExpansion:=true;
      UnitNode.CreateGrandChildren;
    finally
      UnitTreeView.EndUpdate;
    end;
  end else begin
    AllowExpansion:=false;
  end;
end;

procedure TUnitDependenciesView.UnitTreeViewMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ATreeNode: TTreeNode;
  CurNode: TUnitNode;
begin
  if ssDouble in Shift then begin
    ATreeNode:=UnitTreeView.GetNodeAt(X,Y);
    if ATreeNode=nil then exit;
    CurNode:=TUnitNode(ATreeNode.Data);
    if Assigned(OnOpenFile) then OnOpenFile(Self,CurNode.Filename);;
  end;
end;

procedure TUnitDependenciesView.ClearTree;
begin
  FreeThenNil(FRootNode);
end;

procedure TUnitDependenciesView.RebuildTree;
var
  ExpandState: TExpandedUnitNodeState;
begin
  if FUpdateCount>0 then begin
    FRebuildTreeNeeded:=true;
    exit;
  end;
  FRebuildTreeNeeded:=false;

  CodeToolBoss.ActivateWriteLock;
  ExpandState:=TExpandedUnitNodeState.Create;
  BeginUpdate;
  try
    ExpandState.Assign(FRootNode);
    ClearTree;
    if RootFilename='' then exit;
    FRootNode:=TUnitNode.Create;
    FRootNode.CodeBuffer:=FRootCodeBuffer;
    FRootNode.Filename:=RootFilename;
    //debugln('TUnitDependenciesView.RebuildTree RootFilename=',RootFilename);
    FRootNode.ShortFilename:=FRootShortFilename;
    UnitTreeView.Items.Clear;
    FRootNode.TreeNode:=UnitTreeView.Items.Add(nil,'');
    FRootNode.CreateChildren;
    ExpandState.AssignTo(FRootNode);
  finally
    FRebuildTreeNeeded:=false;
    EndUpdate;
    ExpandState.Free;
    CodeToolBoss.DeActivateWriteLock;
  end;
end;

procedure TUnitDependenciesView.SetRootFilename(const AValue: string);
begin
  //DebugLn('TUnitDependenciesView.SetRootFilename Old=',FRootFilename,' New',AValue);
  if FRootFilename=AValue then exit;
  FRootFilename:=AValue;
  FRootCodeBuffer:=CodeToolBoss.LoadFile(FRootFilename,false,false);
  FRootShortFilename:=FRootFilename;
  FRootValid:=FRootCodeBuffer<>nil;
  RebuildTree;
  RefreshHistoryList;
end;

procedure TUnitDependenciesView.SetRootShortFilename(const AValue: string);
begin
  if FRootShortFilename=AValue then exit;
  FRootShortFilename:=AValue;
  if FRootNode<>nil then
    FRootNode.ShortFilename:=AValue;
end;

procedure TUnitDependenciesView.CommitUnitHistoryListSelection;
begin
  //DebugLn('TUnitDependenciesView.CommitUnitHistoryListSelection Old=',FRootFilename,' New=',UnitHistoryList.Text,' FUpdateCount=',dbgs(FUpdateCount));
  if FUpdateCount>0 then begin
    FCommitUnitHistoryListSelectionNeeded:=true;
    exit;
  end;
  FCommitUnitHistoryListSelectionNeeded:=false;
  if UnitHistoryList.Items.IndexOf(UnitHistoryList.Text)<0 then exit;
  RootFilename:=ExpandFileNameUTF8(UnitHistoryList.Text);
end;

procedure TUnitDependenciesView.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

function TUnitDependenciesView.RootValid: boolean;
begin
  Result:=FRootValid;
end;

constructor TUnitDependenciesView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name:=NonModalIDEWindowNames[nmiwUnitDependenciesName];
  Caption := dlgUnitDepCaption;

  RefreshHistoryList;

  SelectUnitButton.Caption:=dlgUnitDepBrowse;
  RefreshButton.Caption:=dlgUnitDepRefresh;
  ShowProjectButton.Caption:=dlgEnvProject;
  CloseButton.Caption:=lisClose;
  UnitTreeView.Images := IDEImages.Images_16;
  ToolBar.Images := IDEImages.Images_16;
  ShowProjectButton.ImageIndex := IDEImages.LoadImage(16, 'item_project');
  SelectUnitButton.ImageIndex := IDEImages.LoadImage(16, 'laz_open');
  RefreshButton.ImageIndex := IDEImages.LoadImage(16, 'laz_refresh');
end;

destructor TUnitDependenciesView.Destroy;
begin
  ClearTree;
  inherited Destroy;
end;

procedure TUnitDependenciesView.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TUnitDependenciesView.EndUpdate;
begin
  if FUpdateCount<0 then RaiseGDBException('TUnitDependenciesView.EndUpdate');
  dec(FUpdateCount);
  if FUpdateCount=0 then begin
    if FCommitUnitHistoryListSelectionNeeded then
      CommitUnitHistoryListSelection;
    if FRefreshNeeded then
      Refresh;
    if FRebuildTreeNeeded then
      RebuildTree;
    if FRefreshHistoryListNeeded then
      RefreshHistoryList;
  end;
end;

procedure TUnitDependenciesView.Refresh;
begin
  if FUpdateCount>0 then begin
    FRefreshNeeded:=true;
    exit;
  end;
  FRefreshNeeded:=false;
  BeginUpdate;
  try
    if Assigned(OnAccessingSources) then OnAccessingSources(Self);
    RebuildTree;
  finally
    FRefreshNeeded:=false;
    EndUpdate;
  end;
end;

procedure TUnitDependenciesView.RefreshHistoryList;
begin
  if FUpdateCount>0 then begin
    FRefreshHistoryListNeeded:=true;
    exit;
  end;
  FRefreshHistoryListNeeded:=false;
  BeginUpdate;
  try
    if RootFilename<>'' then
      if not InputHistories.AddToUnitDependenciesHistory(RootFilename) then
        exit;
    UnitHistoryList.Items.Assign(InputHistories.UnitDependenciesHistory);
    if UnitHistoryList.Items.Count>0 then
      UnitHistoryList.Text:=UnitHistoryList.Items[0]
    else
      UnitHistoryList.Text:=RootFilename;
  finally
    FRefreshHistoryListNeeded:=false;
    EndUpdate;
  end;
end;

{ TUnitNode }

procedure TUnitNode.SetCodeBuffer(const AValue: TCodeBuffer);
begin
  if CodeBuffer=AValue then exit;
  FCodeBuffer:=AValue;
  if CodeBuffer<>nil then
    Filename:=CodeBuffer.Filename;
end;

procedure TUnitNode.SetFilename(const AValue: string);
begin
  if Filename=AValue then exit;
  FFilename:=AValue;
  FSourceType:=unstUnknown;
  CreateShortFilename;
end;

procedure TUnitNode.SetParent(const AValue: TUnitNode);
begin
  if Parent=AValue then exit;
  UnbindFromParent;
  FParent:=AValue;
  if Parent<>nil then AddToParent;
end;

procedure TUnitNode.SetShortFilename(const AValue: string);
begin
  if ShortFilename=AValue then exit;
  FShortFilename:=AValue;
  if TreeNode<>nil then
    TreeNode.Text:=FShortFilename;
end;

procedure TUnitNode.SetTreeNode(const AValue: TTreeNode);
begin
  if TreeNode=AValue then exit;
  FTreeNode:=AValue;
  if TreeNode<>nil then begin
    TreeNode.Text:=ShortFilename;
    TreeNode.Data:=Self;
    TreeNode.HasChildren:=HasChildren;
    TreeNode.ImageIndex:=ImageIndex;
    TreeNode.SelectedIndex:=ImageIndex;
  end;
end;

procedure TUnitNode.CreateShortFilename;
begin
  ShortFilename:=Filename;
  if (Parent<>nil) and (FilenameIsAbsolute(Parent.Filename))
  and (FilenameIsAbsolute(Filename)) then begin
    ShortFilename:=ExtractRelativePath(ExtractFilePath(Parent.Filename),
                                       Filename);
  end;
end;

procedure TUnitNode.UnbindFromParent;
begin
  if TreeNode<>nil then begin
    TreeNode.Free;
    TreeNode:=nil;
  end;
  if Parent<>nil then begin
    if Parent.FirstChild=Self then Parent.FFirstChild:=NextSibling;
    if Parent.LastChild=Self then Parent.FLastChild:=PrevSibling;
    Dec(Parent.FChildCount);
  end;
  if NextSibling<>nil then NextSibling.FPrevSibling:=PrevSibling;
  if PrevSibling<>nil then PrevSibling.FNextSibling:=NextSibling;
  FNextSibling:=nil;
  FPrevSibling:=nil;
  FParent:=nil;
end;

procedure TUnitNode.AddToParent;
begin
  if Parent=nil then exit;
  
  FPrevSibling:=Parent.LastChild;
  FNextSibling:=nil;
  Parent.FLastChild:=Self;
  if Parent.FirstChild=nil then Parent.FFirstChild:=Self;
  if PrevSibling<>nil then PrevSibling.FNextSibling:=Self;
  Inc(Parent.FChildCount);
  CreateShortFilename;

  if FindParentWithCodeBuffer(CodeBuffer)<>nil then begin
    Include(FFlags,unfCircle);
    if ForbiddenCircle then
      Include(FFlags,unfForbiddenCircle);
  end;

  if Parent.TreeNode<>nil then begin
    Parent.TreeNode.HasChildren:=true;
    TreeNode:=Parent.TreeNode.TreeNodes.AddChild(Parent.TreeNode,'');
    if Parent.TreeNode.Expanded then begin
      CreateChildren;
    end;
  end;
end;

procedure TUnitNode.AddChild(const AFilename: string; ACodeBuffer: TCodeBuffer;
  InImplementation: boolean);
var
  NewNode: TUnitNode;
begin
  NewNode:=TUnitNode.Create;
  NewNode.CodeBuffer:=ACodeBuffer;
  NewNode.Filename:=AFilename;
  if InImplementation then
    Include(NewNode.FFlags,unfImplementation);
  if ACodeBuffer=nil then begin
    Include(NewNode.FFlags,unfFileNotFound);
  end;
  NewNode.Parent:=Self;
end;

procedure TUnitNode.UpdateSourceType;
var
  SourceKeyWord: string;
  ASrcType: TUnitNodeSourceType;
begin
  FSourceType:=unstUnknown;
  if CodeBuffer=nil then exit;
  SourceKeyWord:=CodeToolBoss.GetSourceType(CodeBuffer,false);
  for ASrcType:=Low(TUnitNodeSourceType) to High(TUnitNodeSourceType) do
    if CompareText(SourceKeyWord,UnitNodeSourceTypeNames[ASrcType])=0
    then
      FSourceType:=ASrcType;
  if TreeNode<>nil then begin
    TreeNode.ImageIndex:=ImageIndex;
    TreeNode.SelectedIndex:=ImageIndex;
  end;
end;

function TUnitNode.ForbiddenCircle: boolean;
var
  ParentNode, CurNode: TUnitNode;
begin
  Result:=false;
  if CodeBuffer=nil then exit;
  CurNode:=Self;
  ParentNode:=Parent;
  if (ParentNode<>nil) and (ParentNode.CodeBuffer=CodeBuffer) then begin
    // unit includes itself -> forbidden
    Result:=true;
    exit;
  end;
  while ParentNode<>nil do begin
    if (unfImplementation in CurNode.Flags) then begin
      // pascal allows to use nearly anything in the implementation section
      exit;
    end;
    if ParentNode.CodeBuffer=CodeBuffer then begin
      // interface circle detected
      Result:=true;
      exit;
    end;
    CurNode:=ParentNode;
    ParentNode:=ParentNode.Parent;
  end;
end;

constructor TUnitNode.Create;
begin
  inherited Create;
  FSourceType:=unstUnknown;
end;

destructor TUnitNode.Destroy;
begin
  ClearChildren;
  Parent:=nil;
  inherited Destroy;
end;

procedure TUnitNode.ClearChildren;
begin
  while LastChild<>nil do
    LastChild.Free;
end;

procedure TUnitNode.CreateChildren;
var
  UsedInterfaceFilenames, UsedImplementationFilenames: TStrings;
  i: integer;
begin
  ClearChildren;
  UpdateSourceType;
  if CodeBuffer=nil then exit;
  if CodeToolBoss.FindUsedUnitFiles(CodeBuffer,
                                    UsedInterfaceFilenames,
                                    UsedImplementationFilenames) then
  begin
    Exclude(FFlags,unfParseError);
    for i:=0 to UsedInterfaceFilenames.Count-1 do
      AddChild(UsedInterfaceFilenames[i],
               TCodeBuffer(UsedInterfaceFilenames.Objects[i]),false);
    UsedInterfaceFilenames.Free;
    for i:=0 to UsedImplementationFilenames.Count-1 do
      AddChild(UsedImplementationFilenames[i],
               TCodeBuffer(UsedImplementationFilenames.Objects[i]),true);
    UsedImplementationFilenames.Free;
  end else begin
    Include(FFlags,unfParseError);
  end;
end;

procedure TUnitNode.ClearGrandChildren;
var
  AChildNode: TUnitNode;
begin
  AChildNode:=FirstChild;
  while AChildNode<>nil do begin
    AChildNode.ClearChildren;
    AChildNode:=AChildNode.NextSibling;
  end;
end;

procedure TUnitNode.CreateGrandChildren;
var
  AChildNode: TUnitNode;
begin
  AChildNode:=FirstChild;
  while AChildNode<>nil do begin
    AChildNode.CreateChildren;
    AChildNode:=AChildNode.NextSibling;
  end;
end;

function TUnitNode.FindParentWithCodeBuffer(ACodeBuffer: TCodeBuffer
  ): TUnitNode;
begin
  Result:=Parent;
  while (Result<>nil) and (Result.CodeBuffer<>ACodeBuffer) do begin
    Result:=Result.Parent;
  end;
end;

function TUnitNode.HasChildren: boolean;
begin
  Result:=FChildCount>0;
end;

function TUnitNode.ImageIndex: integer;
begin
  if not (unfCircle in FFlags) then 
  begin
    case SourceType of
      unstUnit:    Result := IDEImages.LoadImage(16, 'item_unit');
      unstProgram: Result := IDEImages.LoadImage(16, 'item_project');
      unstLibrary: Result := IDEImages.LoadImage(16, 'item_library');
      unstPackage: Result := IDEImages.LoadImage(16, 'item_package');
    else
      begin
        if unfFileNotFound in Flags then
          Result := IDEImages.LoadImage(16, 'state_not_found')
        else if unfParseError in Flags then
          Result := IDEImages.LoadImage(16, 'state_error')
        else
          Result := IDEImages.LoadImage(16, 'state_unknown');
      end;
    end;
  end else 
  begin
    if unfForbiddenCircle in Flags then 
      Result := IDEImages.LoadImage(16, 'state_circular_reference')
    else 
      Result := IDEImages.LoadImage(16, 'state_unit_circular_reference');
  end;
end;

function TUnitNode.IsFirstImplementationNode: boolean;
begin
  Result:=IsImplementationNode
    and ((PrevSibling=nil) or (not PrevSibling.IsImplementationNode));
end;

function TUnitNode.IsImplementationNode: boolean;
begin
  Result:=unfImplementation in FFlags;
end;

//-----------------------------------------------------------------------------

{ TExpandedUnitNodeState }

constructor TExpandedUnitNodeState.Create;
begin
  inherited Create;
end;

destructor TExpandedUnitNodeState.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TExpandedUnitNodeState.Clear;

  procedure ClearPathTree(StringListNode: TStringList);
  var
    i: integer;
  begin
    if StringListNode=nil then exit;
    for i:=0 to StringListNode.Count-1 do
      ClearPathTree(TStringList(StringListNode.Objects[i]));
    FreeAndNil(StringListNode);
  end;
  
begin
  ClearPathTree(FPaths);
end;

procedure TExpandedUnitNodeState.Assign(ANode: TUnitNode);

  procedure AssignRecursive(var CurPathList: TStringList; CurNode: TUnitNode);
  var
    ChildNode: TUnitNode;
    CurChildList: TStringList;
  begin
    if (CurNode=nil) or (not CurNode.HasChildren) or (CurNode.TreeNode=nil) then
      exit;
    if not CurNode.TreeNode.Expanded then exit;
    if CurPathList=nil then
      CurPathList:=TStringList.Create;

    CurPathList.Add(CurNode.Filename);
    CurChildList:=nil;
    ChildNode:=CurNode.FirstChild;
    while ChildNode<>nil do begin
      AssignRecursive(CurChildList,ChildNode);
      ChildNode:=ChildNode.NextSibling;
    end;
    CurPathList.Objects[CurPathList.Count-1]:=CurChildList;
  end;

begin
  Clear;
  AssignRecursive(FPaths,ANode);
end;

procedure TExpandedUnitNodeState.AssignTo(ANode: TUnitNode);

  procedure AssignToRecursive(CurPathList: TStringList; CurNode: TUnitNode);
  var
    ChildNode: TUnitNode;
    CurChildList: TStringList;
    i: integer;
  begin
    if (CurNode=nil) or (not CurNode.HasChildren) or (CurNode.TreeNode=nil) then
      exit;
    if CurPathList=nil then exit;
    i:=CurPathList.IndexOf(CurNode.Filename);
    if i<0 then exit;

    CurNode.TreeNode.Expand(false);
    CurChildList:=TStringList(CurPathList.Objects[i]);
    if CurChildList=nil then exit;

    ChildNode:=CurNode.FirstChild;
    while ChildNode<>nil do begin
      AssignToRecursive(CurChildList,ChildNode);
      ChildNode:=ChildNode.NextSibling;
    end;
  end;

begin
  AssignToRecursive(FPaths,ANode);
end;

end.

