{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TComponentTreeView is a component to show the child components of a
    TComponent. TControls are shown in a hierachic view.
    It supports
      - multi selecting components
      - editing the creation order
      - editing the TControl.Parent hierachy
    For an usage example, see the object inspector.
}
unit ComponentTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, AvgLvlTree, Dialogs, Controls, ComCtrls,
  ExtCtrls, LResources,
  ObjInspStrConsts, PropEdits;
  
type
  { TComponentTreeView }

  TComponentTreeView = class(TCustomTreeView)
  private
    FComponentList: TBackupComponentList;
    FPropertyEditorHook: TPropertyEditorHook;
    FImageList: TImageList;
    function GetSelection: TPersistentSelectionList;
    procedure SetPropertyEditorHook(const AValue: TPropertyEditorHook);
    procedure SetSelection(const NewSelection: TPersistentSelectionList);
  protected
    procedure DoSelectionChanged; override;
    function GetImageFor(AComponent: TComponent):integer;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure MouseLeave; override;
    procedure GetComponentInsertMarkAt(X, Y: Integer;
                              out AnInsertMarkNode: TTreeNode;
                              out AnInsertMarkType: TTreeViewInsertMarkType);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X,Y: Integer); override;
    procedure RebuildComponentNodes; virtual;
    procedure UpdateComponentNodesValues; virtual;
    function CreateNodeCaption(APersistent: TPersistent): string; virtual;
  public
    property Selection: TPersistentSelectionList read GetSelection
                                                 write SetSelection;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
    property OnSelectionChanged;
  end;

implementation

type
  TComponentCandidate = class
  public
    APersistent: TPersistent;
    Parent: TComponent;
    Added: boolean;
  end;

  { TComponentWalker }

  TComponentWalker = class
    FTreeView: TComponentTreeView;
    FCandidates: TAvgLvlTree;
    FRootComponent: TComponent;
    FNode: TTreeNode;
  public
    constructor Create(
      ATreeView: TComponentTreeView; ACandidates: TAvgLvlTree;
      ARootComponent: TComponent; ANode: TTreeNode);

    procedure Walk(AComponent: TComponent);
  end;

  TComponentAccessor = class(TComponent);

function CompareComponentCandidates(
  Candidate1, Candidate2: TComponentCandidate): integer;
begin
  Result := ComparePointers(Candidate1.APersistent, Candidate2.APersistent);
end;

function ComparePersistentWithComponentCandidate(
  APersistent: TPersistent; Candidate: TComponentCandidate): integer;
begin
  Result := ComparePointers(APersistent, Candidate.APersistent);
end;

{ TComponentWalker }

constructor TComponentWalker.Create(ATreeView: TComponentTreeView;
  ACandidates: TAvgLvlTree; ARootComponent: TComponent; ANode: TTreeNode);
begin
  FTreeView := ATreeView;
  FCandidates := ACandidates;
  FRootComponent := ARootComponent;
  FNode := ANode;
end;

procedure TComponentWalker.Walk(AComponent: TComponent);
var
  oldNode: TTreeNode;
  candidate: TComponentCandidate;
  avlNode: TAvgLvlTreeNode;
begin
  if GetLookupRootForComponent(AComponent) <> FRootComponent then exit;
  avlNode := FCandidates.FindKey(
    AComponent, TListSortCompare(@ComparePersistentWithComponentCandidate));
  if avlNode = nil then exit;
  candidate := TComponentCandidate(avlNode.Data);
  if candidate.Added then exit;
  candidate.Added := true;

  oldNode := FNode;
  FNode := FTreeView.Items.AddChild(FNode, FTreeView.CreateNodeCaption(AComponent));
  FNode.Data := AComponent;
  FNode.ImageIndex := FTreeView.GetImageFor(AComponent);
  FNode.SelectedIndex := FNode.ImageIndex;
  FNode.MultiSelected := FTreeView.Selection.IndexOf(AComponent) >= 0;
  TComponentAccessor(AComponent).GetChildren(@Walk, FRootComponent);
  FNode := oldNode;
  FNode.Expanded := true;
end;
  
{ TComponentTreeView }

procedure TComponentTreeView.SetSelection(const NewSelection: TPersistentSelectionList);
begin
  if (PropertyEditorHook = nil) then
  begin
    if (FComponentList.LookupRoot = nil) then
      Exit;
    FComponentList.Clear;
  end
  else
  if not NewSelection.ForceUpdate and FComponentList.IsEqual(PropertyEditorHook.LookupRoot, NewSelection) then
  begin
    // nodes ok, but maybe node values need update
    UpdateComponentNodesValues;
    Exit;
  end;
  FComponentList.LookupRoot := PropertyEditorHook.LookupRoot;
  FComponentList.Selection.Assign(NewSelection);
  RebuildComponentNodes;
end;

procedure TComponentTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  AComponent: TComponent;
  NewSelection: TPersistentSelectionList;
begin
  NewSelection:=TPersistentSelectionList.Create;
  try
    if (PropertyEditorHook<>nil)
    and (PropertyEditorHook.LookupRoot<>nil)
    and (not (csDestroying in ComponentState)) then begin
      ANode:=GetFirstMultiSelected;
      while ANode<>nil do begin
        AComponent:=TComponent(ANode.Data);
        if AComponent=nil then
          RaiseGDBException('TComponentTreeView.DoSelectionChanged ANode.Data=nil');
        if GetLookupRootForComponent(AComponent)=PropertyEditorHook.LookupRoot
        then
          NewSelection.Add(AComponent);
        ANode:=ANode.GetNextMultiSelected;
      end;
      NewSelection.SortLike(FComponentList.Selection);
    end;
    if NewSelection.IsEqual(FComponentList.Selection) then exit;
    FComponentList.Selection.Assign(NewSelection);
    if (NewSelection.Count=1)
    and (NewSelection[0] is TCustomPage)
    and (TCustomPage(NewSelection[0]).Parent is TCustomNotebook)
    then begin
      TCustomNotebook(TCustomPage(NewSelection[0]).Parent).PageIndex:=
        TCustomPage(NewSelection[0]).PageIndex;
    end;
    inherited DoSelectionChanged;
  finally
    NewSelection.Free;
  end;
end;

procedure TComponentTreeView.DragDrop(Source: TObject; X, Y: Integer);
var
  Node, SelNode: TTreeNode;
  AContainer: TWinControl;
  AControl: TControl;
  ParentNode: TTreeNode;
  InsertType: TTreeViewInsertMarkType;
  ok: Boolean;
begin
  GetComponentInsertMarkAt(X,Y,Node,InsertType);
  SetInsertMark(nil,tvimNone);
  ParentNode:=Node;
  if InsertType in [tvimAsNextSibling,tvimAsPrevSibling] then
    ParentNode:=ParentNode.Parent;
  if Assigned(ParentNode) then
  begin
    if TObject(ParentNode.Data) is TWinControl then
    begin
      AContainer := TWinControl(ParentNode.Data);
      SelNode := GetFirstMultiSelected;
      while Assigned(SelNode) do
      begin
        if TObject(SelNode.Data) is TControl then
        begin
          AControl := TControl(SelNode.Data);
          ok:=false;
          try
            AControl.Parent := AContainer;
            ok:=true;
          except
            on E: Exception do begin
              MessageDlg(oisError,
                Format(oisUnableToChangeParentOfControlToNewParent, ['"',
                  DbgSName(AControl), '"', '"', DbgSName(AContainer), '"', #13,
                  E.Message]), mtError, [mbCancel], 0);
            end;
          end;
          if not ok then break;
        end;
        SelNode := SelNode.GetNextMultiSelected;
      end;
    end;
    RebuildComponentNodes;
  end;
  inherited DragDrop(Source, X, Y);
end;

procedure TComponentTreeView.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Node: TTreeNode;
  AnObject: TObject;
  AContainer,AControl: TControl;
  AcceptControl, AcceptContainer: Boolean;
  InsertType: TTreeViewInsertMarkType;
  ParentNode: TTreeNode;
begin
  //debugln('TComponentTreeView.DragOver START ',dbgs(Accept));

  AcceptContainer := False;
  AcceptControl := True;

  GetComponentInsertMarkAt(X,Y,Node,InsertType);
  SetInsertMark(Node,InsertType);

  // check new parent
  ParentNode:=Node;
  if InsertType in [tvimAsNextSibling,tvimAsPrevSibling] then
    ParentNode:=ParentNode.Parent;
  if Assigned(ParentNode) and Assigned(ParentNode.Data) then
  begin
    AnObject := TObject(ParentNode.Data);
    if (AnObject is TWinControl) then
    begin
      if (csAcceptsControls in TWinControl(AnObject).ControlStyle) and
         not (csInline in TWinControl(AnObject).ComponentState) and // Because of TWriter, you can not put a control onto an csInline control (e.g. on a frame).
         ( // TReader/TWriter only supports this
           (TWinControl(AnObject).Owner = nil) or // root
           (TWinControl(AnObject).Owner.Owner = nil) // child of a root
         )
         then
      begin
        AContainer := TWinControl(AnObject);
        //DebugLn(['TComponentTreeView.DragOver AContainer=',DbgSName(AContainer)]);
        AcceptContainer := True;
      end;
    end;
  end;

  if AcceptContainer then 
  begin
    Node := GetFirstMultiSelected;
    while Assigned(Node) and AcceptControl do
    begin
      AnObject := TObject(Node.Data);
      if AnObject is TControl then
      begin
        AControl := TControl(AnObject);
        if AControl=AContainer then break;
        //DebugLn(['TComponentTreeView.DragOver AControl=',DbgSName(AControl),' Parent=',DbgSName(AControl.Parent),' OldAccepts=',csAcceptsControls in AControl.Parent.ControlStyle]);
        // check if new parent allows this control class
        if not AContainer.CheckChildClassAllowed(AnObject.ClassType, False) then
          break;
        // check if one of the parent of the container is the control itself
        if AControl.IsParentOf(AContainer) then break;
        // do not move childs of a restricted parent to another parent
        // e.g. TPage of TPageControl
        if (AControl.Parent<>nil) and (AControl.Parent<>AContainer)
        and (not (csAcceptsControls in AControl.Parent.ControlStyle)) then
          break;
      end;
      Node := Node.GetNextMultiSelected;
    end;
    AcceptControl:=(Node=nil);
  end;

  Accept := AcceptContainer and AcceptControl;
  //debugln('TComponentTreeView.DragOver A ',dbgs(Accept));
  inherited DragOver(Source, X, Y, State, Accept);
  //debugln('TComponentTreeView.DragOver B ',dbgs(Accept));

  Accept := AcceptContainer and AcceptControl
            and ((OnDragOver=nil) or Accept);
end;

procedure TComponentTreeView.DragCanceled;
begin
  SetInsertMark(nil,tvimNone);
  inherited DragCanceled;
end;

procedure TComponentTreeView.MouseLeave;
begin
  SetInsertMark(nil,tvimNone);
  inherited MouseLeave;
end;

procedure TComponentTreeView.GetComponentInsertMarkAt(X, Y: Integer; out
  AnInsertMarkNode: TTreeNode; out AnInsertMarkType: TTreeViewInsertMarkType);
var
  Node: TTreeNode;
begin
  Node:=GetFirstMultiSelected;
  if (Node<>nil) and (TObject(Node.Data) is TControl) then
  begin
    // TWinControl allows only to add/remove childs, but not at a specific position
    AnInsertMarkNode:=GetNodeAt(X,Y);
    AnInsertMarkType:=tvimAsFirstChild;
  end else begin
    GetInsertMarkAt(X,Y,AnInsertMarkNode,AnInsertMarkType);
  end;
end;

function TComponentTreeView.GetImageFor(AComponent: TComponent): integer;
begin
  if Assigned(AComponent) then begin
    if (AComponent is TControl)
    and (csAcceptsControls in TControl(AComponent).ControlStyle) then
      Result := 3
    else if (AComponent is TControl) then
      Result := 2
    else
      Result := 1;
  end else 
    Result := -1;
end;

procedure TComponentTreeView.SetPropertyEditorHook(
  const AValue: TPropertyEditorHook);
begin
  if FPropertyEditorHook=AValue then exit;
  FPropertyEditorHook:=AValue;
  RebuildComponentNodes;
end;

function TComponentTreeView.GetSelection: TPersistentSelectionList;
begin
  Result:=FComponentList.Selection;
end;

constructor TComponentTreeView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  DragMode := dmAutomatic;
  FComponentList:=TBackupComponentList.Create;
  Options := Options + [tvoAllowMultiselect, tvoAutoItemHeight, tvoKeepCollapsedNodes, tvoReadOnly];
  FImageList := TImageList.Create(nil);
  FImageList.AddLazarusResource('oi_form');
  FImageList.AddLazarusResource('oi_comp');
  FImageList.AddLazarusResource('oi_control');
  FImageList.AddLazarusResource('oi_box');
  Images := FImageList;
end;

destructor TComponentTreeView.Destroy;
begin
  FreeThenNil(FComponentList);
  FreeThenNil(FImageList);
  inherited Destroy;
end;

procedure TComponentTreeView.RebuildComponentNodes;
var
  Candidates: TAvgLvlTree;
  RootComponent: TComponent;

  procedure AddChildren(AComponent: TComponent; ANode: TTreeNode);
  var
    walker: TComponentWalker;
  begin
    walker := TComponentWalker.Create(Self, Candidates, RootComponent, ANode);
    try
      TComponentAccessor(AComponent).GetChildren(@walker.Walk, RootComponent);
    finally
      walker.Free;
    end;
  end;

  procedure AddCandidates(OwnerComponent: TComponent);
  var
    AComponent: TComponent;
    Candidate: TComponentCandidate;
    i: Integer;
  begin
    if OwnerComponent=nil then exit;
    for i:=0 to OwnerComponent.ComponentCount-1 do begin
      AComponent:=OwnerComponent.Components[i];
      Candidate:=TComponentCandidate.Create;
      Candidate.APersistent:=AComponent;
      if Candidates.Find(Candidate)<>nil then begin
        DebugLn('WARNING: TComponentTreeView.RebuildComponentNodes doppelganger found ',AComponent.Name);
        Candidate.Free;
      end else begin
        Candidates.Add(Candidate);
        if csInline in AComponent.ComponentState then
          AddCandidates(AComponent);
      end;
    end;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  NewNode: TTreeNode;
  RootObject: TPersistent;
  i: Integer;
  AComponent: TComponent;
  RootNode: TTreeNode;
  AVLNode: TAvgLvlTreeNode;
  Candidate: TComponentCandidate;
begin
  BeginUpdate;
  // save old expanded state and clear
  OldExpanded:=TTreeNodeExpandedState.Create(Self);
  Items.Clear;

  RootObject:=PropertyEditorHook.LookupRoot;
  if RootObject<>nil then begin
    Candidates:=TAvgLvlTree.Create(TListSortCompare(@CompareComponentCandidates));
    try
      // first add the lookup root
      RootNode:=Items.Add(nil,CreateNodeCaption(RootObject));
      RootNode.Data:=RootObject;
      RootNode.ImageIndex:=0;
      RootNode.SelectedIndex:=RootNode.ImageIndex;
      RootNode.MultiSelected:=Selection.IndexOf(RootObject)>=0;
    
      // create candidate nodes for every child
      Candidate:=TComponentCandidate.Create;
      Candidate.APersistent:=RootObject;
      Candidate.Added:=true;
      Candidates.Add(Candidate);

      // add components in creation order and TControl.Parent relationship
      if RootObject is TComponent then begin
        RootComponent:=TComponent(RootObject);
        AddCandidates(RootComponent);
        
        for i:=0 to RootComponent.ComponentCount-1 do begin
          AComponent:=RootComponent.Components[i];
          AVLNode:=Candidates.FindKey(AComponent,
                     TListSortCompare(@ComparePersistentWithComponentCandidate));
          Candidate:=TComponentCandidate(AVLNode.Data);
          if Candidate.Added or
             AComponent.HasParent and
             (AComponent.GetParentComponent <> nil) and
             (AComponent.GetParentComponent <> RootComponent) then
            continue;
          Candidate.Added:=true;
          NewNode:=Items.AddChild(RootNode,CreateNodeCaption(AComponent));
          NewNode.Data:=AComponent;
          NewNode.ImageIndex:=GetImageFor(AComponent);
          NewNode.SelectedIndex:=NewNode.ImageIndex;
          NewNode.MultiSelected:=Selection.IndexOf(AComponent)>=0;
          AddChildren(AComponent, NewNode);
        end;
      end;
    finally
      Candidates.FreeAndClear;
      Candidates.Free;
    end;

    RootNode.Expand(true);
  end;

  // restore old expanded state
  OldExpanded.Apply(Self);
  OldExpanded.Free;
  MakeSelectionVisible;
  EndUpdate;
end;

procedure TComponentTreeView.UpdateComponentNodesValues;

  procedure UpdateComponentNode(ANode: TTreeNode);
  var
    AComponent: TComponent;
  begin
    if ANode=nil then exit;
    AComponent:=TComponent(ANode.Data);
    ANode.Text:=CreateNodeCaption(AComponent);
    UpdateComponentNode(ANode.GetFirstChild);
    UpdateComponentNode(ANode.GetNextSibling);
  end;

begin
  UpdateComponentNode(Items.GetFirstNode);
end;

function TComponentTreeView.CreateNodeCaption(APersistent: TPersistent): string;
begin
  Result:=APersistent.ClassName;
  if APersistent is TComponent then
    Result:=TComponent(APersistent).Name+': '+Result;
end;

initialization
  {$I ../images/componenttreeview.lrs}

end.

