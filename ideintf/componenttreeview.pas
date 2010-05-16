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
  Classes, SysUtils, TypInfo, LCLProc, AvgLvlTree, Dialogs, Controls, ComCtrls,
  ExtCtrls, LResources,
  ObjInspStrConsts, PropEdits, PropEditUtils;
  
type
  { TComponentTreeView }

  TComponentTreeView = class(TCustomTreeView)
  private
    FComponentList: TBackupComponentList;
    FOnModified: TNotifyEvent;
    FPropertyEditorHook: TPropertyEditorHook;
    FImageList: TImageList;
    function GetSelection: TPersistentSelectionList;
    procedure SetPropertyEditorHook(const AValue: TPropertyEditorHook);
    procedure SetSelection(const NewSelection: TPersistentSelectionList);
  protected
    procedure DoSelectionChanged; override;
    function GetImageFor(APersistent: TPersistent):integer;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
                       var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure MouseLeave; override;
    procedure GetComponentInsertMarkAt(X, Y: Integer;
                              out AnInsertMarkNode: TTreeNode;
                              out AnInsertMarkType: TTreeViewInsertMarkType);
    procedure DoModified;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure RebuildComponentNodes; virtual;
    procedure UpdateComponentNodesValues; virtual;
    function CreateNodeCaption(APersistent: TPersistent): string; virtual;
  public
    property Selection: TPersistentSelectionList read GetSelection
                                                 write SetSelection;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
    property OnSelectionChanged;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation

type
  TCollectionAccess = class(TCollection);

  TComponentCandidate = class
  public
    APersistent: TPersistent;
    Parent: TComponent;
    Added: boolean;
  end;

  TGetCollectionProc = procedure(ACollection: TCollection) of object;

  { TComponentWalker }

  TComponentWalker = class
    FTreeView: TComponentTreeView;
    FCandidates: TAvgLvlTree;
    FRootComponent: TComponent;
    FNode: TTreeNode;
  protected
    procedure GetCollections(AComponent: TComponent; AProc: TGetCollectionProc);
  public
    constructor Create(
      ATreeView: TComponentTreeView; ACandidates: TAvgLvlTree;
      ARootComponent: TComponent; ANode: TTreeNode);

    procedure Walk(AComponent: TComponent);
    procedure AddCollection(ACollection: TCollection);
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

procedure TComponentWalker.GetCollections(AComponent: TComponent; AProc: TGetCollectionProc);
var
  PropList: PPropList;
  i, PropCount: Integer;
  Obj: TObject;
begin
  PropCount := GetPropList(AComponent, PropList);
  try
    for i := 0 to PropCount - 1 do
      if (PropList^[i]^.PropType^.Kind = tkClass) then
      begin
        Obj := GetObjectProp(AComponent, PropList^[i], TCollection);
        if Assigned(Obj) then
          AProc(TCollection(Obj));
      end;
  finally
    FreeMem(PropList);
  end;
end;

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
  OldNode: TTreeNode;
  Candidate: TComponentCandidate;
  AVLNode: TAvgLvlTreeNode;
  Root: TComponent;
begin
  if csDestroying in AComponent.ComponentState then exit;
  if GetLookupRootForComponent(AComponent) <> FRootComponent then Exit;

  AVLNode := FCandidates.FindKey(AComponent, TListSortCompare(@ComparePersistentWithComponentCandidate));
  if AVLNode = nil then Exit;

  Candidate := TComponentCandidate(AVLNode.Data);
  if Candidate.Added then Exit;
  Candidate.Added := True;

  OldNode := FNode;
  FNode := FTreeView.Items.AddChild(FNode, FTreeView.CreateNodeCaption(AComponent));
  FNode.Data := AComponent;
  FNode.ImageIndex := FTreeView.GetImageFor(AComponent);
  FNode.SelectedIndex := FNode.ImageIndex;
  FNode.MultiSelected := FTreeView.Selection.IndexOf(AComponent) >= 0;

  GetCollections(AComponent, @AddCollection);

  if (csInline in AComponent.ComponentState) or (AComponent.Owner = nil) then
    Root := AComponent
  else
    Root := AComponent.Owner;

  if not ((Root is TControl) and (csOwnedChildrenNotSelectable in TControl(Root).ControlStyle)) then
    TComponentAccessor(AComponent).GetChildren(@Walk, Root);
  FNode := OldNode;
  FNode.Expanded := True;
end;

procedure TComponentWalker.AddCollection(ACollection: TCollection);
var
  CollectionNode, ItemNode: TTreeNode;
  i: integer;
  Item: TCollectionItem;
begin
  if GetLookupRootForComponent(ACollection) <> FRootComponent then Exit;

  CollectionNode := FTreeView.Items.AddChild(FNode, FTreeView.CreateNodeCaption(ACollection));
  CollectionNode.Data := ACollection;
  CollectionNode.ImageIndex := FTreeView.GetImageFor(ACollection);
  CollectionNode.SelectedIndex := CollectionNode.ImageIndex;
  CollectionNode.MultiSelected := FTreeView.Selection.IndexOf(ACollection) >= 0;

  for i := 0 to ACollection.Count - 1 do
  begin
    Item := ACollection.Items[i];
    ItemNode := FTreeView.Items.AddChild(CollectionNode, FTreeView.CreateNodeCaption(Item));
    ItemNode.Data := Item;
    ItemNode.ImageIndex := FTreeView.GetImageFor(Item);
    ItemNode.SelectedIndex := ItemNode.ImageIndex;
    ItemNode.MultiSelected := FTreeView.Selection.IndexOf(Item) >= 0;
  end;
  
  FNode.Expanded := True;
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
  if not NewSelection.ForceUpdate
    and FComponentList.IsEqual(PropertyEditorHook.LookupRoot, NewSelection) then
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
  APersistent: TPersistent;
  NewSelection: TPersistentSelectionList;
begin
  NewSelection := TPersistentSelectionList.Create;
  try
    if (PropertyEditorHook<>nil) and
       (PropertyEditorHook.LookupRoot<>nil) and
       (not (csDestroying in ComponentState)) then
    begin
      ANode := GetFirstMultiSelected;
      while ANode <> nil do
      begin
        APersistent := TPersistent(ANode.Data);
        if APersistent = nil then
          RaiseGDBException('TComponentTreeView.DoSelectionChanged ANode.Data=nil');
        if GetLookupRootForComponent(APersistent) = PropertyEditorHook.LookupRoot then
          NewSelection.Add(APersistent);
        ANode := ANode.GetNextMultiSelected;
      end;
      NewSelection.SortLike(FComponentList.Selection);
    end;
    if NewSelection.IsEqual(FComponentList.Selection) then
      Exit;
    FComponentList.Selection.Assign(NewSelection);
    if (NewSelection.Count=1) and
       (NewSelection[0] is TCustomPage) and
       (TCustomPage(NewSelection[0]).Parent is TCustomNotebook) then
    begin
      TCustomNotebook(TCustomPage(NewSelection[0]).Parent).PageIndex :=
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
  ACollection: TCollection;
  AContainer: TWinControl;
  AControl: TControl;
  ParentNode: TTreeNode;
  InsertType: TTreeViewInsertMarkType;
  NewIndex, AIndex: Integer;
  ok: Boolean;
begin
  GetComponentInsertMarkAt(X, Y, Node, InsertType);
  SetInsertMark(nil, tvimNone);
  ParentNode := Node;
  if InsertType in [tvimAsNextSibling, tvimAsPrevSibling] then
    ParentNode := ParentNode.Parent;
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
            DoModified;
          except
            on E: Exception do
              MessageDlg(oisError,
                Format(oisUnableToChangeParentOfControlToNewParent, ['"',
                  DbgSName(AControl), '"', '"', DbgSName(AContainer), '"', #13,
                  E.Message]), mtError, [mbOk], 0);
          end;
          if not ok then break;
        end;
        SelNode := SelNode.GetNextMultiSelected;
      end;
    end
    else
    if TObject(Node.Data) is TCollectionItem then
    begin
      ACollection := TCollectionItem(Node.Data).Collection;
      ACollection.BeginUpdate;
      case InsertType of
        tvimAsNextSibling:
          NewIndex := TCollectionItem(Node.Data).Index + 1;
        tvimAsPrevSibling:
          NewIndex := TCollectionItem(Node.Data).Index;
      end;
      SelNode := GetLastMultiSelected;
      while Assigned(SelNode) do
      begin
        if (TObject(SelNode.Data) is TCollectionItem) and
           (TCollectionItem(SelNode.Data).Collection = ACollection) then
        begin
          ok := False;
          try
            AIndex := TCollectionItem(SelNode.Data).Index;
            if AIndex < NewIndex then
              TCollectionItem(SelNode.Data).Index := NewIndex - 1
            else
              TCollectionItem(SelNode.Data).Index := NewIndex;
            ok := True;
            DoModified;
          except
            on E: Exception do
              MessageDlg(E.Message, mtError, [mbOk], 0);
          end;
          if not ok then break;
        end;
        SelNode := SelNode.GetPrevMultiSelected;
      end;
      ACollection.EndUpdate;
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
  AControl: TControl absolute AnObject;
  AContainer: TPersistent;
  AcceptControl, AcceptContainer: Boolean;
  InsertType: TTreeViewInsertMarkType;
  ParentNode: TTreeNode;
begin
  //debugln('TComponentTreeView.DragOver START ',dbgs(Accept));

  AcceptContainer := False;
  AcceptControl := True;

  GetComponentInsertMarkAt(X, Y, Node, InsertType);
  SetInsertMark(Node, InsertType);

  // check new parent
  ParentNode := Node;
  if InsertType in [tvimAsNextSibling, tvimAsPrevSibling] then
    ParentNode := ParentNode.Parent;
  if Assigned(ParentNode) and Assigned(ParentNode.Data) then
  begin
    AnObject := TObject(ParentNode.Data);
    if (AnObject is TWinControl) then
    begin
      if (csAcceptsControls in AControl.ControlStyle) and
         // Because of TWriter, you can not put a control onto an csInline, csAncestor controls (e.g. on a frame or it child).
         ([csInline, csAncestor] * AControl.ComponentState = []) and
         ( // TReader/TWriter only supports this
           (AControl.Owner = nil) or // root
           (AControl.Owner.Owner = nil) // child of a root
         ) then
      begin
        AContainer := TPersistent(AnObject);
        //DebugLn(['TComponentTreeView.DragOver AContainer=',DbgSName(AContainer)]);
        AcceptContainer := True;
      end;
    end
    else
    if (AnObject is TCollection) then
    begin
      // it is allowed to move container items inside the container
      AContainer := TPersistent(AnObject);
      AcceptContainer := True;
    end;
  end;

  if AcceptContainer then 
  begin
    Node := GetFirstMultiSelected;
    while Assigned(Node) and AcceptControl do
    begin
      AnObject := TObject(Node.Data);
      // don't allow to move ancestor components
      if (AnObject is TComponent) and
         (csAncestor in TComponent(AnObject).ComponentState) then break;
      if (AnObject is TControl) then
      begin
        if AnObject = AContainer then break;
        if not (AContainer is TWinControl) then break;
        //DebugLn(['TComponentTreeView.DragOver AControl=',DbgSName(AControl),' Parent=',DbgSName(AControl.Parent),' OldAccepts=',csAcceptsControls in AControl.Parent.ControlStyle]);
        // check if new parent allows this control class
        if not TWinControl(AContainer).CheckChildClassAllowed(AnObject.ClassType, False) then
          break;
        // check if one of the parent of the container is the control itself
        if AControl.IsParentOf(TWinControl(AContainer)) then break;
        // do not move children of a restricted parent to another parent
        // e.g. TPage of TPageControl
        if (AControl.Parent <> nil) and (AControl.Parent <> AContainer) and
            (not (csAcceptsControls in AControl.Parent.ControlStyle)) then
          break;
      end
      else
      if (AnObject is TCollectionItem) then
      begin
        if AnObject = AContainer then break;
        if not (AContainer is TCollection) then
          break;
        if TCollectionItem(AnObject).Collection <> TCollection(AContainer) then
          break;
      end;
      Node := Node.GetNextMultiSelected;
    end;
    AcceptControl := (Node = nil);
  end;

  Accept := AcceptContainer and AcceptControl;
  //debugln('TComponentTreeView.DragOver A ',dbgs(Accept));
  inherited DragOver(Source, X, Y, State, Accept);
  //debugln('TComponentTreeView.DragOver B ',dbgs(Accept));

  Accept := AcceptContainer and AcceptControl and ((OnDragOver=nil) or Accept);
end;

procedure TComponentTreeView.DragCanceled;
begin
  SetInsertMark(nil, tvimNone);
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
  Node := GetFirstMultiSelected;
  if (Node <> nil) and (TObject(Node.Data) is TControl) then
  begin
    // TWinControl allows only to add/remove children, but not at a specific position
    AnInsertMarkNode := GetNodeAt(X,Y);
    AnInsertMarkType := tvimAsFirstChild;
  end
  else
  begin
    GetInsertMarkAt(X, Y, AnInsertMarkNode, AnInsertMarkType);
    if (Node <> nil) and (TObject(Node.Data) is TCollectionItem) then
      if AnInsertMarkType = tvimAsFirstChild then
        AnInsertMarkType := tvimAsPrevSibling;
  end;
end;

procedure TComponentTreeView.DoModified;
begin
  if Assigned(PropertyEditorHook) then
    PropertyEditorHook.RefreshPropertyValues;
  if Assigned(FOnModified) then
    OnModified(Self);
end;

function TComponentTreeView.GetImageFor(APersistent: TPersistent): integer;
begin
  if Assigned(APersistent) then
  begin
    if (APersistent is TControl) and (csAcceptsControls in TControl(APersistent).ControlStyle) then
      Result := 3
    else
    if (APersistent is TControl) then
      Result := 2
    else
    if (APersistent is TComponent) then
      Result := 1
    else
    if (APersistent is TCollection) then
      Result := 4
    else
    if (APersistent is TCollectionItem) then
      Result := 5;
  end
  else
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
  FImageList.AddLazarusResource('oi_collection');
  FImageList.AddLazarusResource('oi_item');
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
  RootObject: TPersistent;
  RootComponent: TComponent absolute RootObject;

  procedure AddChildren(AComponent: TComponent; ANode: TTreeNode);
  var
    walker: TComponentWalker;
    Root: TComponent;
  begin
    if csDestroying in AComponent.ComponentState then exit;
    //debugln(['AddChildren ',DbgSName(AComponent),' ',AComponent.ComponentCount]);
    walker := TComponentWalker.Create(Self, Candidates, RootComponent, ANode);
    try
      // add inline components children
      if csInline in AComponent.ComponentState then
        Root := AComponent
      else
        Root := RootComponent;
      TComponentAccessor(AComponent).GetChildren(@walker.Walk, Root);
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
    //debugln(['AddCandidates OwnerComponent=',DbgSName(OwnerComponent)]);
    if OwnerComponent = nil then Exit;
    if csDestroying in OwnerComponent.ComponentState then exit;
    for i := 0 to OwnerComponent.ComponentCount - 1 do
    begin
      AComponent := OwnerComponent.Components[i];
      Candidate := TComponentCandidate.Create;
      Candidate.APersistent := AComponent;
      if Candidates.Find(Candidate)<>nil then
      begin
        DebugLn('WARNING: TComponentTreeView.RebuildComponentNodes doppelganger found ', AComponent.Name);
        Candidate.Free;
      end
      else
      begin
        Candidates.Add(Candidate);
        if csInline in AComponent.ComponentState then
          AddCandidates(AComponent);
      end;
    end;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  RootNode: TTreeNode;
  Candidate: TComponentCandidate;
begin
  BeginUpdate;
  // save old expanded state and clear
  OldExpanded:=TTreeNodeExpandedState.Create(Self);
  Items.Clear;

  RootObject := nil;
  if PropertyEditorHook<>nil then
    RootObject := PropertyEditorHook.LookupRoot;
  if (RootObject is TComponent)
  and (csDestroying in TComponent(RootObject).ComponentState) then
    RootObject:=nil;
  if RootObject <> nil then
  begin
    Candidates:=TAvgLvlTree.Create(TListSortCompare(@CompareComponentCandidates));
    try
      // first add the lookup root
      RootNode := Items.Add(nil, CreateNodeCaption(RootObject));
      RootNode.Data := RootObject;
      RootNode.ImageIndex := 0;
      RootNode.SelectedIndex := RootNode.ImageIndex;
      RootNode.MultiSelected := Selection.IndexOf(RootObject) >= 0;

      // create candidate nodes for every child
      Candidate := TComponentCandidate.Create;
      Candidate.APersistent := RootObject;
      Candidate.Added := True;
      Candidates.Add(Candidate);

      // add components in creation order and TControl.Parent relationship
      if RootObject is TComponent then
      begin
        AddCandidates(RootComponent);
        AddChildren(RootComponent,RootNode);
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
    APersistent: TPersistent;
  begin
    if ANode = nil then Exit;
    APersistent := TPersistent(ANode.Data);
    ANode.Text := CreateNodeCaption(APersistent);
    UpdateComponentNode(ANode.GetFirstChild);
    UpdateComponentNode(ANode.GetNextSibling);
  end;

begin
  UpdateComponentNode(Items.GetFirstNode);
end;

function TComponentTreeView.CreateNodeCaption(APersistent: TPersistent): string;

  function GetCollectionName(ACollection: TCollection): String;
  var
    PropList: PPropList;
    i, PropCount: Integer;
  begin
    Result := TCollectionAccess(ACollection).PropName;
    if Result <> '' then
      Exit;

    Result := '<unknown>';
    if ACollection.Owner = nil then
      Exit;

    PropCount := GetPropList(ACollection.Owner, PropList);
    try
      for i := 0 to PropCount - 1 do
        if (PropList^[i]^.PropType^.Kind = tkClass) and
           (GetObjectProp(ACollection.Owner, PropList^[i], ACollection.ClassType) = ACollection) then
          Exit(PropList^[i]^.Name);
    finally
      FreeMem(PropList);
    end;
  end;

begin
  Result := APersistent.ClassName;
  if APersistent is TComponent then
    Result := TComponent(APersistent).Name + ': ' + Result
  else
  if APersistent is TCollection then
    Result := GetCollectionName(TCollection(APersistent)) + ': ' + Result
  else
  if APersistent is TCollectionItem then
    Result := IntToStr(TCollectionItem(APersistent).Index) + ' - ' + TCollectionItem(APersistent).DisplayName;
end;

initialization
  {$I ../images/componenttreeview.lrs}

end.

