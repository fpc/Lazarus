unit TreeFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, Graphics,
  Controls, ComCtrls, EditBtn, FileUtil, AvgLvlTree, fgl;

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var AIsEnabled: Boolean): Integer of object;

  TTreeFilterEdit = class;
  TTreeNodeList = specialize TFPGList<TTreeNode>;

  { TBranch }

  // A branch in the tree which can be sorted
  TBranch = class
  private
    fOwner: TTreeFilterEdit;
    fRootNode: TTreeNode;
    fOriginalData: TStringList;     // Data supplied by caller.
    fSortedData: TStringList;       // Data sorted for viewing.
    fImgIndex: Integer;
    // Full filename in node data is needed when showing the directory hierarchy.
    // It is stored automatically if AFullFilename is passed to contructor.
    fFilenameMap: TStringToStringTree;
    fTVNodeStack: TTreeNodeList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SortAndFilter;
    procedure ApplyFilter;
    procedure FreeTVNodeData(Node: TTreeNode);
    procedure TVDeleteUnneededNodes(p: integer);
    procedure TVClearUnneededAndCreateHierachy(Filename: string);
  public
    constructor Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
    destructor Destroy; override;
    procedure AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string = '');
  end;

  TBranchList = specialize TFPGObjectList<TBranch>;

  { TTreeFilterEdit }

  TTreeFilterEdit = class(TCustomControlFilterEdit)
  private
    fFilteredTreeview: TTreeview;   // A control showing the (filtered) data.
    fImageIndexDirectory: integer;  // Needed if directory structure is shown.
    fSelectionList: TStringList;    // Store/restore the old selections here.
    fShowDirHierarchy: Boolean;     // Show direcories / files as a tree structure.
    fBranches: TBranchList;         // Items are under these nodes can be sorted.
    fOnGetImageIndex: TImageIndexEvent;
    procedure SetFilteredTreeview(const AValue: TTreeview);
    procedure SetShowDirHierarchy(const AValue: Boolean);
  protected
    procedure MoveNext; override;
    procedure MovePrev; override;
    procedure SortAndFilter; override;
    procedure ApplyFilterCore; override;
    function GetDefaultGlyph: TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
    function GetBranch(ARootNode: TTreeNode): TBranch;
  public
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property SelectionList: TStringList read fSelectionList;
    property ShowDirHierarchy: Boolean read fShowDirHierarchy write SetShowDirHierarchy;
    property Branches: TBranchList read fBranches write fBranches;
  published
    property FilteredTreeview: TTreeview read fFilteredTreeview write SetFilteredTreeview;
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex;
//                                  deprecated 'use OnGetImageIndex handler in FilteredTreeview';
  end;

  { TFileNameItem }

  TFileNameItem = class
  public
    Filename: string;
    constructor Create(AFilename: string);
  end;

var
  TreeFilterGlyph: TBitmap;

procedure Register;

implementation

procedure Register;
begin
  {$I treefilteredit_icon.lrs}
  RegisterComponents('LazControls',[TTreeFilterEdit]);
end;

{ TBranch }

constructor TBranch.Create(AOwner: TTreeFilterEdit; ARootNode: TTreeNode);
begin
  inherited Create;
  fOwner:=AOwner;
  fRootNode:=ARootNode; // RootNode can also be Nil. Then all items are at top level.
  fOriginalData:=TStringList.Create;
  fSortedData:=TStringList.Create;
  fFilenameMap:=TStringToStringTree.Create(True);
  fImgIndex:=-1;
end;

destructor TBranch.Destroy;
begin
  fFilenameMap.Free;
  fSortedData.Free;
  fOriginalData.Free;
  FreeTVNodeData(fRootNode);
  inherited Destroy;
end;

procedure TBranch.AddNodeData(ANodeText: string; AData: TObject; AFullFilename: string);
begin
  fOriginalData.AddObject(ANodeText, AData);
  if AFullFilename <> '' then
    fFilenameMap[ANodeText]:=AFullFilename;
end;

function TBranch.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if fOwner.SortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else if fOwner.fShowDirHierarchy then
    Result:=CompareFilenames(ExtractFilePath(AFilename1), ExtractFilePath(AFilename2))
  else
    Result:=0;
end;

procedure TBranch.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  FileN: string;
begin
  fSortedData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    FileN:=fOriginalData[Origi];
    if (fOwner.Filter='') or (Pos(fOwner.Filter,lowercase(FileN))>0) then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(FileN,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1,FileN, fOriginalData.Objects[Origi]);
    end;
  end;
end;

procedure TBranch.ApplyFilter;
var
  TVNode: TTreeNode;
  i: Integer;
  FileN, s: string;
  ena: Boolean;
begin
  if fFilenameMap.Count > 0 then
    FreeTVNodeData(fRootNode);    // Free node data now, it will be filled later.
  if Assigned(fRootNode) then
    fRootNode.DeleteChildren      // Delete old tree nodes.
  else
    fOwner.FilteredTreeview.Items.Clear;
  if fOwner.ShowDirHierarchy then
    fTVNodeStack:=TTreeNodeList.Create;
  for i:=0 to fSortedData.Count-1 do begin
    FileN:=fSortedData[i];
    if fOwner.ShowDirHierarchy then begin
      TVClearUnneededAndCreateHierachy(FileN);
      TVNode:=fTVNodeStack[fTVNodeStack.Count-1];
    end
    else
      TVNode:=fOwner.FilteredTreeview.Items.AddChild(fRootNode,FileN);
//    else  TVNode:=fFilteredTreeview.Items.Add(Nil,FileN);
    if fFilenameMap.Count > 0 then begin
      s:=FileN;
      if fFilenameMap.Contains(FileN) then
        s:=fFilenameMap[FileN];           // Full file name.
      TVNode.Data:=TFileNameItem.Create(s);
    end;
    ena := True;
    if Assigned(fOwner.OnGetImageIndex) then
      fImgIndex:=fOwner.OnGetImageIndex(FileN, fSortedData.Objects[i], ena);
    TVNode.ImageIndex:=fImgIndex;
    TVNode.SelectedIndex:=fImgIndex;
//    if Assigned(fSelectedPart) then
//      TVNode.Selected:=fSelectedPart=fSortedData.Objects[i];
  end;
  if fOwner.ShowDirHierarchy then      // TVDeleteUnneededNodes(0); ?
    fTVNodeStack.Free;
  if Assigned(fRootNode) then
    fRootNode.Expanded:=True;
end;

procedure TBranch.FreeTVNodeData(Node: TTreeNode);
var
  Child: TTreeNode;
begin
  if Node=nil then exit;
  if (Node.Data<>nil) then begin
    TObject(Node.Data).Free;
    Node.Data:=nil;
  end;
  Child:=Node.GetFirstChild;
  while Child<>nil do
  begin
    FreeTVNodeData(Child);                 // Recursive call.
    Child:=Child.GetNextSibling;
  end;
end;

procedure TBranch.TVDeleteUnneededNodes(p: integer);
// delete all nodes behind the nodes in the stack, and depth>=p
var
  i: Integer;
  Node: TTreeNode;
begin
  for i:=fTVNodeStack.Count-1 downto p do begin
    Node:=fTVNodeStack[i];
    while Node.GetNextSibling<>nil do
      Node.GetNextSibling.Free;
  end;
  fTVNodeStack.Count:=p;
end;

procedure TBranch.TVClearUnneededAndCreateHierachy(Filename: string);
// TVNodeStack contains a path of TTreeNode for the last filename
var
  DelimPos: Integer;
  FilePart: String;
  Node: TTreeNode;
  p: Integer;
begin
  p:=0;
  while Filename<>'' do begin
    // get the next file name part
    DelimPos:=System.Pos(PathDelim,Filename);
    if DelimPos>0 then begin
      FilePart:=copy(Filename,1,DelimPos-1);
      Filename:=copy(Filename,DelimPos+1,length(Filename));
    end else begin
      FilePart:=Filename;
      Filename:='';
    end;
    //debugln(['ClearUnneededAndCreateHierachy FilePart=',FilePart,' Filename=',Filename,' p=',p]);
    if p < fTVNodeStack.Count then begin
      Node:=fTVNodeStack[p];
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
          Node:=fOwner.FilteredTreeview.Items.Add(Node,FilePart);
        fTVNodeStack[p]:=Node;
      end;
    end else begin
      // new sub node
      if p>0 then
        Node:=fTVNodeStack[p-1]
      else
        Node:=fRootNode;
      if Node.GetFirstChild<>nil then begin
        Node:=Node.GetFirstChild;
        Node.Text:=FilePart;
      end
      else
        Node:=fOwner.FilteredTreeview.Items.AddChild(Node,FilePart);
      fTVNodeStack.Add(Node);
    end;
    if (Filename<>'') then begin
      Node.ImageIndex:=fOwner.ImageIndexDirectory;
      Node.SelectedIndex:=Node.ImageIndex;
    end;
    inc(p);
  end;
end;

{ TFileNameItem }

constructor TFileNameItem.Create(AFilename: string);
begin
  Filename:=AFilename;
end;

{ TTreeFilterEdit }

constructor TTreeFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSelectionList:=TStringList.Create;
  fImageIndexDirectory := -1;
end;

destructor TTreeFilterEdit.Destroy;
begin
  fBranches.Free;
  fSelectionList.Free;
  inherited Destroy;
end;

function TTreeFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := TreeFilterGlyph;
end;

procedure TTreeFilterEdit.SetFilteredTreeview(const AValue: TTreeview);
begin
  if fFilteredTreeview = AValue then Exit;
  fFilteredTreeview:=AValue;
  if AValue = nil then Exit;
end;

procedure TTreeFilterEdit.SetShowDirHierarchy(const AValue: Boolean);
begin
  if fShowDirHierarchy=AValue then exit;
  if not Assigned(fFilteredTreeview) then
    raise Exception.Create('Showing directory hierarchy requires Treeview.');
  fShowDirHierarchy:=AValue;
end;

procedure TTreeFilterEdit.ApplyFilterCore;
var
  i: Integer;
begin
  fFilteredTreeview.BeginUpdate;
  if Assigned(fBranches) then begin      // Filter the brances
    for i:=0 to fBranches.Count-1 do
      fBranches[i].ApplyFilter;
  end
  else begin                             // Filter the whole tree.
    fFilteredTreeview.Items.Clear;
    // ToDo ...
  end;
  fFilteredTreeview.EndUpdate;
end;

procedure TTreeFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  i: Integer;
begin
  if Assigned(fBranches) then begin      // Filter the brances
    for i:=0 to fBranches.Count-1 do
      fBranches[i].SortAndFilter;
  end
  else begin                             // Filter the whole tree.
    // ToDo ...
  end;
end;

procedure TTreeFilterEdit.StoreSelection;
var
  ANode: TTreeNode;
begin
  fSelectionList.Clear;
  ANode:=fFilteredTreeview.Selected;
  while ANode<>nil do begin
    fSelectionList.Insert(0,ANode.Text);
    ANode:=ANode.Parent;
  end;
end;

procedure TTreeFilterEdit.RestoreSelection;
var
  ANode: TTreeNode;
  CurText: string;
begin
  ANode:=nil;
  while fSelectionList.Count>0 do begin
    CurText:=fSelectionList[0];
    if ANode=nil then
      ANode:=fFilteredTreeview.Items.GetFirstNode
    else
      ANode:=ANode.GetFirstChild;
    while (ANode<>nil) and (ANode.Text<>CurText) do
      ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    fSelectionList.Delete(0);
  end;
  if ANode<>nil then
    fFilteredTreeview.Selected:=ANode;
end;

function TTreeFilterEdit.GetBranch(ARootNode: TTreeNode): TBranch;
// Get a new or existing branch for a node.
var
  i: Integer;
begin
  if not Assigned(fBranches) then
    fBranches := TBranchList.Create;
  Result := Nil;
  for i := 0 to fBranches.Count-1 do
    if fBranches[i].fRootNode = ARootNode then begin
      Result := fBranches[i];
      Result.fOriginalData.Clear;
      Break;
    end;
  if Result = Nil then begin
    Result := TBranch.Create(Self, ARootNode);
    fBranches.Add(Result);
  end;
end;

procedure TTreeFilterEdit.MoveNext;
begin
  fFilteredTreeview.MoveToNextNode;
end;

procedure TTreeFilterEdit.MovePrev;
begin
  fFilteredTreeview.MoveToPrevNode;
end;

end.

