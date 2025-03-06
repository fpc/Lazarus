{ TreeFilterEdit

  Copyright (C) 2012 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit TreeFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl,
  // LCL
  LCLType, Graphics, ComCtrls, EditBtn,
  // LazUtils
  LazFileUtils, LazUTF8, LazLoggerBase, AvgLvlTree, LazClasses, LazUtilities;

type
  TImageIndexEvent = function (Str: String; Data: TObject;
                               var AIsEnabled: Boolean): Integer of object;
  TFilterNodeEvent = function (ItemNode: TTreeNode; out Done: Boolean): Boolean of object;

  TTreeFilterEdit = class;

  { TTreeFilterBranch }

  // A branch in the tree which can be sorted and filtered
  TTreeFilterBranch = class
  private type

    TTreeFilterBranchStringListItem = class(TRefCountedObject)
      Name, FullName, ComparableName: String;
      Data: TObject;
    end;
    PTreeFilterBranchStringListItem = ^TTreeFilterBranchStringListItem;

    { TTreeFilterBranchStringListItemList }

    TTreeFilterBranchStringListItemList = class(specialize TFPGList<TTreeFilterBranchStringListItem>)
    protected
      procedure Deref(Item: Pointer); override;
    end;

    { TTreeFilterBranchStringList }

    TTreeFilterBranchStringList = class(TStrings)
    private
      FList: TTreeFilterBranchStringListItemList;
      FModified: boolean;
      function GetComparableName(AnIndex: Integer): string;
      function GetFullName(AnIndex: Integer): string;
      function GetInternalItem(AnIndex: Integer): TTreeFilterBranchStringListItem;
      procedure SetComparableName(AnIndex: Integer; AValue: string);
      procedure SetFullName(AnIndex: Integer; AValue: string);
    protected
      function Get(Index: Integer): string; override;
      function GetCount: Integer; override;
      function GetCapacity: Integer; override;
      function GetObject(Index: Integer): TObject; override;
      procedure Put(Index: Integer; const S: string); override;
      procedure PutObject(Index: Integer; AObject: TObject); override;
      procedure SetCapacity(NewCapacity: Integer); override;
      function Add(const AnInternalItem: TTreeFilterBranchStringListItem): Integer;
      property InternalItem[AnIndex: Integer]: TTreeFilterBranchStringListItem read GetInternalItem;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(const S: string): Integer; override; overload;
      function AddObject(const S: string; AObject: TObject): Integer; override; overload;
      function AddObject(const S, FullName: string; AObject: TObject): Integer; overload;
      procedure Assign(Source: TPersistent); override;
      procedure Clear; override;
      procedure Delete(Index: Integer); override;
      procedure Insert(Index: Integer; const S: string); override;
      procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
      procedure InsertObject(Index: Integer; const S, FullName: string; AObject: TObject);
      property  FullName[AnIndex: Integer]: string read GetFullName write SetFullName;
      property  ComparableName[AnIndex: Integer]: string read GetComparableName write SetComparableName;
      property  Modified: boolean read FModified write FModified;
    end;

  private
    fOwner: TTreeFilterEdit;
    fRootNode: TTreeNode;
    fDataFlags: set of (fbDataSorted, fbDataShowDirHierarchy);
    fOriginalData: TTreeFilterBranchStringList;       // Data supplied by caller.
    fSortedData: TTreeFilterBranchStringList;     // Data sorted for viewing.
    fFilteredData: TTreeFilterBranchStringList;   // Data sorted and filtered for viewing. (only if filter is set)
    fDisplayedData: TTreeFilterBranchStringList;  // reference to either Sorted or Filtered data (depends on filter is set or not)
    fImgIndex: Integer;
    fTVNodeStack: TList;
    function GetOriginalData: TStrings;
    procedure SortAndFilter;
    procedure ApplyFilter;
    procedure TVDeleteUnneededNodes(p: integer);
    procedure TVClearUnneededAndCreateHierachy(Filename: string);
    procedure RemoveChildrenData(ARootNode : TTreeNode);
  public
    constructor Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
    destructor Destroy; override;
    procedure AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string = '');
    procedure DeleteData(ANode: TTreeNode);
    procedure FreeNodeData(ANode: TTreeNode);
    function GetData(AIndex: integer): TObject;
    procedure ClearNodeData;
    procedure InvalidateBranch;
    procedure Move(CurIndex, NewIndex: integer);
  public
    property Items: TStrings read GetOriginalData;
  end;

  TBranchList = specialize TFPGObjectList<TTreeFilterBranch>;

  { TTreeFilterEdit }

  TTreeFilterEdit = class(TCustomControlFilterEdit)
  private
    fFilteredTreeview: TCustomTreeview; // A control showing the (filtered) data.
    fImageIndexDirectory: integer;  // Needed if directory structure is shown.
    FScrolledPos: TPoint;
    fSelectionList: TStringList;    // Store/restore the old selections here.
    fShowDirHierarchy: Boolean;     // Show directories / files as a tree structure.
    fBranches: TBranchList;         // Items under these nodes can be sorted.
    fExpandAllInitially: Boolean;   // Expand all levels when searched for the first time.
    // First node matching the filter. Will be selected if old selection is hidden.
    fFirstPassedNode: TTreeNode;
    fOnGetImageIndex: TImageIndexEvent;
    fOnFilterNode: TFilterNodeEvent;
    procedure SetFilteredTreeview(AValue: TCustomTreeview);
    procedure SetShowDirHierarchy(AValue: Boolean);
    function FilterTree(Node: TTreeNode): Boolean;
    procedure OnBeforeTreeDestroy(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MoveNext(ASelect: Boolean = False); override;
    procedure MovePrev(ASelect: Boolean = False); override;
    procedure MovePageUp(ASelect: Boolean = False); override;
    procedure MovePageDown(ASelect: Boolean = False); override;
    procedure MoveHome(ASelect: Boolean = False); override;
    procedure MoveEnd(ASelect: Boolean = False); override;
    function ReturnKeyHandled: Boolean; override;
    procedure EditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyphName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTreeFilterSilently(ATree: TCustomTreeview; AFilter: string);
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
    function GetExistingBranch(ARootNode: TTreeNode): TTreeFilterBranch;
    function GetCleanBranch(ARootNode: TTreeNode): TTreeFilterBranch;
    function DeleteBranch(ARootNode: TTreeNode): Boolean;
  public
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property ScrolledPos: TPoint read FScrolledPos; // only valid if positive
    property SelectionList: TStringList read fSelectionList;
    property ShowDirHierarchy: Boolean read fShowDirHierarchy write SetShowDirHierarchy;
  published
    property FilteredTreeview: TCustomTreeview read fFilteredTreeview write SetFilteredTreeview;
    property ExpandAllInitially: Boolean read fExpandAllInitially write fExpandAllInitially default False;
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex;
    property OnFilterNode: TFilterNodeEvent read fOnFilterNode write fOnFilterNode;
  end;

  { TTFENodeData - TreeFilterEditNodeData }

  TTFENodeData = class
  public
    Node: TTreeNode;
    Branch: TTreeFilterBranch;
  end;

  { TFileNameItem }

  TFileNameItem = class(TTFENodeData)
  public
    Data: Pointer;
    Filename: string;
    constructor Create(AFilename: string; aData: Pointer);
  end;

implementation

{ TTreeFilterBranch }

constructor TTreeFilterBranch.Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
begin
  inherited Create;
  fOwner:=AOwner;
  fRootNode:=ARootNode; // RootNode can also be Nil. Then all items are at top level.
  fOriginalData:=TTreeFilterBranchStringList.Create;
  fSortedData:=TTreeFilterBranchStringList.Create;
  fFilteredData:=TTreeFilterBranchStringList.Create;
  fDisplayedData:=fSortedData;
  fImgIndex:=-1;
end;

destructor TTreeFilterBranch.Destroy;
begin
  ClearNodeData;
  FreeAndNil(fSortedData);
  FreeAndNil(fOriginalData);
  FreeAndNil(fFilteredData);
  inherited Destroy;
end;

procedure TTreeFilterBranch.AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string);
begin
  fOriginalData.AddObject(ANodeText, AFullFilename, AData);
end;

function TTreeFilterBranch.GetData(AIndex: integer): TObject;
begin
  if AIndex<fDisplayedData.Count then
    Result:=fDisplayedData.Objects[AIndex]
  else
    Result:=Nil;
end;

function TTreeFilterBranch.GetOriginalData: TStrings;
begin
  Result := fOriginalData;
end;

function DoCompFN(Item1, Item2: Pointer): Integer;
var
  I1: TTreeFilterBranch.TTreeFilterBranchStringListItem absolute Item1;
  I2: TTreeFilterBranch.TTreeFilterBranchStringListItem absolute Item2;
begin
  Result := CompareStr(I1.ComparableName, I2.ComparableName);
end;

procedure TTreeFilterBranch.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  i: Integer;
  o: TTreeFilterBranchStringListItem;
  NewFlags: set of low(fDataFlags)..high(fDataFlags);
begin
  fFilteredData.Clear;
  NewFlags := [];
  if fOwner.SortData then Include(NewFlags, fbDataSorted);
  if fOwner.fShowDirHierarchy then Include(NewFlags, fbDataShowDirHierarchy);

  if fOriginalData.Modified or (fDataFlags <> NewFlags) then begin
    fSortedData.Assign(fOriginalData);
    if fOwner.SortData then begin
      for i := 0 to fSortedData.Count - 1 do
        fSortedData.ComparableName[i] := GetLazNormalizedFilename(fSortedData[i]);
      MergeSortWithLen(PPointer(fSortedData.FList.List^), fSortedData.Count, @DoCompFN);
    end
    else
    if fOwner.fShowDirHierarchy then begin
      for i := 0 to fSortedData.Count - 1 do
        fSortedData.ComparableName[i] := GetLazNormalizedFilename(ExtractFilePath(fSortedData[i]));
      MergeSortWithLen(PPointer(fSortedData.FList.List^), fSortedData.Count, @DoCompFN);
    end;
    fOriginalData.Modified := False; // fSortedData is up to date
  end;
  fDisplayedData:=fSortedData;
  fDataFlags := NewFlags;

  if (fOwner.Filter<>'') then begin
    fFilteredData.Capacity := fSortedData.Count div 2;
    for i := 0 to fSortedData.Count - 1 do begin
      o := fSortedData.InternalItem[i];
      if fOwner.DoFilterItem(o.Name, nil) then
        fFilteredData.Add(o);
    end;
    fDisplayedData:=fFilteredData;
  end;
end;

procedure TTreeFilterBranch.ApplyFilter;
var
  TVNode: TTreeNode;
  i: Integer;
  FileN, s: string;
  ena: Boolean;
  AObject: TObject;
begin
  if Assigned(fRootNode) then Begin
    ClearNodeData;
    fRootNode.DeleteChildren;      // Delete old tree nodes.
  end
  else
    fOwner.fFilteredTreeview.Items.Clear;
  if fOwner.ShowDirHierarchy then
    fTVNodeStack:=TList.Create;
  for i:=0 to fDisplayedData.Count-1 do begin
    FileN:=fDisplayedData[i];
    if fOwner.ShowDirHierarchy then begin
      TVClearUnneededAndCreateHierachy(FileN);
      TVNode:=TTreeNode(fTVNodeStack[fTVNodeStack.Count-1]);
    end
    else
      TVNode:=fOwner.fFilteredTreeview.Items.AddChild(fRootNode,FileN);
    // Save the long filename to Node.Data
    AObject := fDisplayedData.Objects[i];
    s := fDisplayedData.FullName[i];
    if s = '' then
      s:=FileN
    else
      AObject := TFileNameItem.Create(s, AObject);
    If AObject is TTFENodeData then Begin
      TTFENodeData(AObject).Node := TVNode;
      TTFENodeData(AObject).Branch := Self;
    end;
    TVNode.Data:=AObject;
    // Get ImageIndex for Node
    ena := True;
    if Assigned(fOwner.OnGetImageIndex) then
      fImgIndex:=fOwner.OnGetImageIndex(FileN, fDisplayedData.Objects[i], ena);
    TVNode.ImageIndex:=fImgIndex;
    TVNode.SelectedIndex:=fImgIndex;
  end;
  if fOwner.ShowDirHierarchy then
    fTVNodeStack.Free;
  if Assigned(fRootNode) then
    fRootNode.Expanded:=True;
end;

procedure TTreeFilterBranch.TVDeleteUnneededNodes(p: integer);
// delete all nodes behind the nodes in the stack, and depth>=p
var
  i: Integer;
  Node: TTreeNode;
begin
  for i:=fTVNodeStack.Count-1 downto p do begin
    Node:=TTreeNode(fTVNodeStack[i]);
    while Node.GetNextSibling<>nil do
      Node.GetNextSibling.Free;
  end;
  fTVNodeStack.Count:=p;
end;

procedure TTreeFilterBranch.TVClearUnneededAndCreateHierachy(Filename: string);
// TVNodeStack contains a path of TTreeNode for the filename
var
  DelimPos: Integer;
  FilePart: String;
  Node: TTreeNode;
  p: Integer;
begin
  p:=0;
  while Filename<>'' do begin
    // get the next file name part
    DelimPos:=Pos(PathDelim,Filename);
    if DelimPos>0 then begin
      FilePart:=copy(Filename,1,DelimPos-1);
      delete(Filename,1,DelimPos);
    end else begin
      FilePart:=Filename;
      Filename:='';
    end;
    //debugln(['ClearUnneededAndCreateHierachy FilePart=',FilePart,' Filename=',Filename,' p=',p]);
    if p < fTVNodeStack.Count then begin
      Node:=TTreeNode(fTVNodeStack[p]);
      if (FilePart=Node.Text) and (Node.Data=nil) then begin
        // same sub directory
      end
      else begin
        // change directory => last directory is complete
        // => delete unneeded nodes after last path
        TVDeleteUnneededNodes(p+1);
        if Node.GetNextSibling<>nil then begin
          Node:=Node.GetNextSibling;
          Node.Text:=FilePart;
        end
        else
          Node:=fOwner.fFilteredTreeview.Items.Add(Node,FilePart);
        fTVNodeStack[p]:=Node;
      end;
    end else begin
      // new sub node
      Assert(p=fTVNodeStack.Count, Format('TVClearUnneededAndCreateHierachy: p (%d) > fTVNodeStack.Count (%d).',
                                          [p, fTVNodeStack.Count]));
      if p>0 then
        Node:=TTreeNode(fTVNodeStack[p-1])
      else
        Node:=fRootNode;
      Assert(Assigned(Node), Format('TVClearUnneededAndCreateHierachy: Node=nil, p=%d', [p]));
      if Node.GetFirstChild<>nil then begin
        Node:=Node.GetFirstChild;
        Node.Text:=FilePart;
      end
      else
        Node:=fOwner.fFilteredTreeview.Items.AddChild(Node,FilePart);
      fTVNodeStack.Add(Node);
    end;
    if (Filename<>'') then begin
      Node.ImageIndex:=fOwner.ImageIndexDirectory;
      Node.SelectedIndex:=Node.ImageIndex;
    end;
    inc(p);
  end;
end;

procedure TTreeFilterBranch.DeleteData(ANode : TTreeNode);

  procedure DeleteFromList(List: TStrings);
  var
    i: Integer;
  begin
    i := List.IndexOf(ANode.Text);
    if i > -1 then
      List.Delete(i);
  end;

begin
  FreeNodeData(ANode);
  DeleteFromList(fOriginalData);
  DeleteFromList(fSortedData);
  DeleteFromList(fFilteredData);
  fOwner.FilteredTreeview.Items.Delete(ANode);
end;

procedure TTreeFilterBranch.FreeNodeData(ANode : TTreeNode);
Var
  AObject : TObject;
Begin
  If Assigned(ANode) And Assigned(ANode.Data) Then Begin
    AObject := TObject(ANode.Data);
    If AObject.InheritsFrom(TTFENodeData) Then Begin
      TTFENodeData(AObject).Node := NIL;
      TTFENodeData(AObject).Branch := NIL;
    end;
    If AObject is TFileNameItem Then
      AObject.Free;
    ANode.Data := NIL;
  end;
end;

procedure TTreeFilterBranch.RemoveChildrenData(ARootNode : TTreeNode);

  procedure ProcessSubNodes(ANode : TTreeNode);
  Var
    BNode : TTreeNode;
  begin
    FreeNodeData(ANode);
    BNode := ANode.GetFirstChild;
    While Assigned(BNode) Do Begin
      ProcessSubNodes(BNode);
      BNode := BNode.GetNextSibling;
    end;
  end;

Begin
  if ARootNode=nil then exit;
  ProcessSubNodes(ARootNode);
end;

procedure TTreeFilterBranch.ClearNodeData;
Begin
  RemoveChildrenData(fRootNode);
end;

procedure TTreeFilterBranch.InvalidateBranch;
begin
  SortAndFilter;
  ApplyFilter;
end;

procedure TTreeFilterBranch.Move(CurIndex, NewIndex: integer);
var
  item: TTreeNode;
begin
  item := fRootNode.Items[CurIndex];
  item.Index := NewIndex;
  item.MakeVisible;
  fOriginalData.Move(CurIndex, NewIndex);
end;

{ TTreeFilterBranch.TTreeFilterBranchStringListItemList }

procedure TTreeFilterBranch.TTreeFilterBranchStringListItemList.Deref(Item: Pointer);
begin
  TTreeFilterBranchStringListItem(Item^).ReleaseReference;
end;

{ TTreeFilterBranch.TTreeFilterBranchStringList }

function TTreeFilterBranch.TTreeFilterBranchStringList.GetComparableName(AnIndex: Integer): string;
begin
  Result := FList[AnIndex].ComparableName;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.GetFullName(AnIndex: Integer): string;
begin
  Result := FList[AnIndex].FullName;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.GetInternalItem(AnIndex: Integer
  ): TTreeFilterBranchStringListItem;
begin
  Result := FList[AnIndex];
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.SetComparableName(AnIndex: Integer;
  AValue: string);
begin
  FList[AnIndex].ComparableName := AValue;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.SetFullName(AnIndex: Integer;
  AValue: string);
begin
  FList[AnIndex].FullName := AValue;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.Get(Index: Integer): string;
begin
  Result := FList[Index].Name;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.GetObject(Index: Integer): TObject;
begin
  Result := FList[Index].Data;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.Put(Index: Integer; const S: string);
begin
  FList[Index].Name := S;
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.PutObject(Index: Integer; AObject: TObject
  );
begin
  FList[Index].Data := AObject;
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.SetCapacity(NewCapacity: Integer);
begin
  FList.SetCapacity(NewCapacity);
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.Add(
  const AnInternalItem: TTreeFilterBranchStringListItem): Integer;
begin
  FList.Add(AnInternalItem);
  AnInternalItem.AddReference;
  FModified := True;
end;

constructor TTreeFilterBranch.TTreeFilterBranchStringList.Create;
begin
  FList := TTreeFilterBranchStringListItemList.Create;
  inherited Create;
end;

destructor TTreeFilterBranch.TTreeFilterBranchStringList.Destroy;
begin
  inherited Destroy;
  FList.Free;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.Add(const S: string): Integer;
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  FList.Add(o);
  FModified := True;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.AddObject(const S: string; AObject: TObject
  ): Integer;
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  o.Data := AObject;
  FList.Add(o);
  FModified := True;
end;

function TTreeFilterBranch.TTreeFilterBranchStringList.AddObject(const S, FullName: string;
  AObject: TObject): Integer;
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  o.FullName := FullName;
  o.Data := AObject;
  FList.Add(o);
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.Assign(Source: TPersistent);
var
  Other: TTreeFilterBranch.TTreeFilterBranchStringList absolute Source;
  o: TTreeFilterBranchStringListItem;
  i: Integer;
begin
  if Source is TTreeFilterBranchStringList then begin
    FList.Clear;
    FList.Capacity := Other.FList.Count;
    for i := 0 to Other.FList.Count - 1 do begin
      o := Other.FList[i];
      o.AddReference;
      FList.Add(o);
    end;
  end
  else
    inherited Assign(Source);
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.Clear;
begin
  if FList.Count > 0 then
    FModified := True;
  FList.Clear;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.Delete(Index: Integer);
begin
  FList.Delete(Index);
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.Insert(Index: Integer; const S: string);
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  FList.Insert(Index, o);
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.InsertObject(Index: Integer;
  const S: string; AObject: TObject);
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  o.Data := AObject;
  FList.Insert(Index, o);
  FModified := True;
end;

procedure TTreeFilterBranch.TTreeFilterBranchStringList.InsertObject(Index: Integer; const S,
  FullName: string; AObject: TObject);
var
  o: TTreeFilterBranchStringListItem;
begin
  o := TTreeFilterBranch.TTreeFilterBranchStringListItem.Create;
  o.AddReference;
  o.Name := S;
  o.FullName := FullName;
  o.Data := AObject;
  FList.Insert(Index, o);
  FModified := True;
end;

{ TFileNameItem }

constructor TFileNameItem.Create(AFilename: string; aData: Pointer);
begin
  Filename:=AFilename;
  Data:=aData;
end;

{ TTreeFilterEdit }

constructor TTreeFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSelectionList:=TStringList.Create;
  fImageIndexDirectory := -1;
  fExpandAllInitially := False;
end;

destructor TTreeFilterEdit.Destroy;
begin
  FilteredTreeview:=nil;
  FreeAndNil(fBranches);
  FreeAndNil(fSelectionList);
  inherited Destroy;
end;

function TTreeFilterEdit.GetDefaultGlyphName: string;
begin
  Result := 'btnfiltercancel';
end;

procedure TTreeFilterEdit.OnBeforeTreeDestroy(Sender: TObject);
begin
  FreeAndNil(fBranches);
end;

procedure TTreeFilterEdit.SetFilteredTreeview(AValue: TCustomTreeview);
begin
  if fFilteredTreeview = AValue then Exit;
  if fFilteredTreeview <> nil then
  begin
    fFilteredTreeview.RemoveFreeNotification(Self);
    fFilteredTreeview.RemoveHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
    InternalSetFilter('');
    ApplyFilter(True);
  end;
  fFilteredTreeview := AValue;
  if fFilteredTreeview <> nil then
  begin
    InternalSetFilter(Text);
    fFilteredTreeview.FreeNotification(Self);
    fFilteredTreeview.AddHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
  end;
end;

procedure TTreeFilterEdit.SetTreeFilterSilently(ATree: TCustomTreeview; AFilter: string);
begin  // The tree is already filtered, the same FilterEdit is used for many.
  fAlreadyFiltered:=True;
  SetFilteredTreeview(ATree);
  InternalSetFilter(AFilter);
  fAlreadyFiltered:=False;
  Text:=AFilter;     // This will trigger filtering if Text differs from AFilter.
end;

procedure TTreeFilterEdit.SetShowDirHierarchy(AValue: Boolean);
begin
  if fShowDirHierarchy=AValue then exit;
  if not Assigned(fFilteredTreeview) then
    raise Exception.Create('Showing directory hierarchy requires Treeview.');
  fShowDirHierarchy:=AValue;
  InvalidateFilter;
end;

function TTreeFilterEdit.FilterTree(Node: TTreeNode): Boolean;
// Filter all tree branches recursively, setting Node.Visible as needed.
// Returns True if Node or its siblings or child nodes have visible items.
var
  Pass, Done: Boolean;
begin
  Result := False;
  Pass := False;
  Done := False;
  while Node<>nil do
  begin
    // Filter with event handler if there is one.
    if Assigned(fOnFilterNode) then
      Pass := fOnFilterNode(Node, Done);
    if not Done then
      Pass := {Pass or} DoFilterItem(Node.Text, Node.Data);
    if Pass and (fFirstPassedNode=Nil) then
      fFirstPassedNode:=Node;
    // Recursive call for child nodes.
    Node.Visible := FilterTree(Node.GetFirstChild) or Pass;
    if Node.Visible then begin                 // Collapse all when Filter=''.
      if (Filter<>'') or (fExpandAllInitially and fIsFirstUpdate) then
        Node.Expanded := True;
      Result := True;
    end;
    Node := Node.GetNextSibling;
  end;
end;

procedure TTreeFilterEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (FilteredTreeview=AComponent) then
  begin
    IdleConnected:=False;
    fNeedFiltering:=False;
    fFilteredTreeview.RemoveHandlerOnBeforeDestruction(@OnBeforeTreeDestroy);
    fFilteredTreeview:=nil;
    FreeAndNil(fBranches);
  end;
end;

procedure TTreeFilterEdit.SortAndFilter;
// Sort and filter either branches or the whole tree depending on operation mode.
var
  i: Integer;
begin
  if Assigned(fBranches) then begin      // Filter the brances
    for i:=0 to fBranches.Count-1 do
      fBranches[i].SortAndFilter;
  end
  else begin                  // Filter the whole tree (done in ApplyFilterCore).
    //
  end;
end;

procedure TTreeFilterEdit.ApplyFilterCore;
var
  i: Integer;
begin
  if fFilteredTreeview = nil then Exit;
  fFilteredTreeview.BeginUpdate;
  if Assigned(fBranches) then begin      // Apply filter for the branches
    for i:=0 to fBranches.Count-1 do
      fBranches[i].ApplyFilter;
  end
  else begin                             // Apply filter for the whole tree.
    if fExpandAllInitially and fIsFirstUpdate then
      fFilteredTreeview.FullExpand;
    FilterTree(fFilteredTreeview.Items.GetFirstNode);
  end;
  fFilteredTreeview.ClearInvisibleSelection;
  fFilteredTreeview.EndUpdate;
end;

procedure TTreeFilterEdit.StoreSelection;
var
  ANode: TTreeNode;
begin
  if fFilteredTreeview = nil then Exit;
  fFirstPassedNode:=Nil;
  ANode:=fFilteredTreeview.Selected;
  if (ANode=nil) then Exit;
  if (not ANode.IsVisible) and (fFilteredTreeview is TTreeView) then
  begin
    // selected node is not in scrolled view -> store scroll position
    FScrolledPos.X:=TTreeView(fFilteredTreeview).ScrolledLeft;
    FScrolledPos.Y:=TTreeView(fFilteredTreeview).ScrolledTop;
  end else begin
    FScrolledPos.X:=-1;
    FScrolledPos.Y:=-1;
  end;
  if ANode=fFilteredTreeview.Items.GetFirstVisibleNode then Exit;
  fSelectionList.Clear;       // Clear old selection only if there is new one.
  fSelectionList.Add(ANode.Text);
end;

procedure TTreeFilterEdit.RestoreSelection;
var
  ANode, SelectNode: TTreeNode;
  CurText: string;
begin
  if fFilteredTreeview=nil then Exit;

  SelectNode:=Nil;
  // ToDo: support more than one items or otherwise clean the code.
  Assert(fSelectionList.Count < 2,
    'TTreeFilterEdit.RestoreSelection: fSelectionList has more than one item.');
  if fSelectionList.Count > 0 then
  begin
    CurText:=fSelectionList[0];
    ANode:=fFilteredTreeview.Items.GetFirstVisibleNode;
    while (ANode<>nil) and (ANode.Text<>CurText) do
      ANode:=ANode.GetNextVisible;
    if Assigned(ANode) then                 // Selection found
    begin
      SelectNode:=ANode;
      fSelectionList.Delete(0);
    end;
  end;
  if Assigned(SelectNode) then
    ANode:=SelectNode                       // Stored selection
  else if Assigned(fFirstPassedNode) then
    ANode:=fFirstPassedNode                 // Node matching the filter
  else
    ANode:=fFilteredTreeview.Items.GetFirstVisibleNode; // Otherwise first node
  fFilteredTreeview.Selected:=ANode;
  if fFilteredTreeview is TTreeView then
  begin
    if FScrolledPos.Y>=0 then
      TTreeView(fFilteredTreeview).ScrolledTop:=FScrolledPos.Y;
    if FScrolledPos.X>=0 then
      TTreeView(fFilteredTreeview).ScrolledLeft:=FScrolledPos.X;
  end;
end;

function TTreeFilterEdit.GetExistingBranch(ARootNode: TTreeNode): TTreeFilterBranch;
// Get an existing branch for a given tree-node, or Nil if there is none.
var
  i: Integer;
begin
  Result := Nil;
  if not Assigned(fBranches) then Exit;
  for i := 0 to fBranches.Count-1 do
    if fBranches[i].fRootNode = ARootNode then
    begin
      Result := fBranches[i];
      Break;
    end;
end;

procedure TTreeFilterEdit.MoveEnd(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
    fFilteredTreeview.MoveEnd(ASelect);
end;

procedure TTreeFilterEdit.MoveHome(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
    fFilteredTreeview.MoveHome(ASelect);
end;

function TTreeFilterEdit.GetCleanBranch(ARootNode: TTreeNode): TTreeFilterBranch;
// Get a new or existing branch with data cleared for a given tree-node.
begin
  if not Assigned(fBranches) then
    fBranches := TBranchList.Create;
  Result := GetExistingBranch(ARootNode);
  if Assigned(Result) then
    Result.fOriginalData.Clear
  else begin
    Result := TTreeFilterBranch.Create(Self, ARootNode);
    fBranches.Add(Result);
  end;
end;

function TTreeFilterEdit.DeleteBranch(ARootNode: TTreeNode): Boolean;
// Delete the branch connected to a given root node. Returns True if found and deleted.
var
  i: Integer;
begin
  Result := False;
  if not Assigned(fBranches) then Exit;
  for i := 0 to fBranches.Count-1 do
    if fBranches[i].fRootNode = ARootNode then begin
      fBranches.Delete(i);
      Result := True;
      InvalidateFilter;
      Break;
    end;
end;

procedure TTreeFilterEdit.MoveNext(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
  begin
    ASelect := ASelect
      and (tvoAllowMultiSelect in fFilteredTreeview.Options)
      and (msShiftSelect in fFilteredTreeview.MultiSelectStyle);
    fFilteredTreeview.MoveToNextNode(ASelect);
  end;
end;

procedure TTreeFilterEdit.MovePageDown(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
    fFilteredTreeview.MovePageDown(ASelect);
end;

procedure TTreeFilterEdit.MovePageUp(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
    fFilteredTreeview.MovePageUp(ASelect);
end;

procedure TTreeFilterEdit.MovePrev(ASelect: Boolean);
begin
  if Assigned(fFilteredTreeview) then
  begin
    ASelect := ASelect
      and (tvoAllowMultiSelect in fFilteredTreeview.Options)
      and (msShiftSelect in fFilteredTreeview.MultiSelectStyle);
    fFilteredTreeview.MoveToPrevNode(ASelect);
  end;
end;

function TTreeFilterEdit.ReturnKeyHandled: Boolean;
// Retuns true if the Return press was forwarded to the Tree
var
  Key: Char;
begin
  if fFilteredTreeview = nil then
    exit(false);
  Result:=Assigned(fFilteredTreeview.OnKeyPress);
  if Result then
  begin
    Key:=Char(VK_RETURN);
    fFilteredTreeview.OnKeyPress(fFilteredTreeview, Key);
  end;
end;

procedure TTreeFilterEdit.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited EditKeyDown(Key, Shift);
  if Key = 0 then exit;

  if fFilteredTreeview <> nil then
  begin
    // expand/collapse current node
    if (Key = VK_LEFT) and (Shift = [ssAlt]) then
    begin
      fFilteredTreeview.MoveLeft;
      Key := 0;
    end
    else if (Key = VK_RIGHT) and (Shift = [ssAlt]) then
    begin
      fFilteredTreeview.MoveRight;
      Key := 0;
    end

    // expand/collapse full tree
    else if (Key = VK_LEFT) and (Shift = [ssShift, ssAlt]) then
    begin
      fFilteredTreeview.FullCollapse;
      Key := 0;
    end
    else if (Key = VK_RIGHT) and (Shift = [ssShift, ssAlt]) then
    begin
      fFilteredTreeview.FullExpand;
      Key := 0;
    end
  end;
end;

end.

