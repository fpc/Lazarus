unit TreeFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, LCLType, Graphics,
  Controls, ComCtrls, EditBtn, FileUtil, AvgLvlTree;

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var AIsEnabled: Boolean): Integer of object;

  { TTreeFilterEdit }

  TTreeFilterEdit = class(TCustomControlFilterEdit)
  private
    fFilteredTreeview: TTreeview;   // A control showing the (filtered) data.
    fImageIndexDirectory: integer;  // Needed if directory structure is shown.
    fSelectionList: TStringList;    // Store/restore the old selections here.
    fShowDirHierarchy: Boolean;     // Show direcories / files as a tree structure.
    // Full filename in node data is needed when showing the directory hierarchy.
    // It is stored automatically if the map is populated by MapShortToFullFilename.
    fFilenameMap: TStringToStringTree;
    // Data supplied by caller through Data property.
    fOriginalData: TStringList;
    // Data sorted for viewing.
    fSortedData: TStringList;
    fRootNode: TTreeNode;           // The filtered items are under this node.
    fOnGetImageIndex: TImageIndexEvent;
    fImgIndex: Integer;
    fTVNodeStack: TFPList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SetFilteredTreeview(const AValue: TTreeview);
    procedure SetShowDirHierarchy(const AValue: Boolean);
    procedure FreeTVNodeData(Node: TTreeNode);
    procedure TVDeleteUnneededNodes(p: integer);
    procedure TVClearUnneededAndCreateHierachy(Filename: string);
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
    procedure MapShortToFullFilename(ShortFilename, FullFilename: string);
  public
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property SelectionList: TStringList read fSelectionList;
    property ShowDirHierarchy: Boolean read fShowDirHierarchy write SetShowDirHierarchy;
    property Data: TStringList read fOriginalData;
    property RootNode: TTreeNode read fRootNode write fRootNode;
  published
    property FilteredTreeview: TTreeview read fFilteredTreeview write SetFilteredTreeview;
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex; deprecated 'use OnDrawItem handler in FilteredListbox';
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

{ TFileNameItem }

constructor TFileNameItem.Create(AFilename: string);
begin
  Filename:=AFilename;
end;

{ TTreeFilterEdit }

constructor TTreeFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TStringList.Create;
  fSelectionList:=TStringList.Create;
  fFilenameMap:=TStringToStringTree.Create(True);
  fSortedData:=TStringList.Create;
  fImageIndexDirectory := -1;
  fImgIndex:=-1;
end;

destructor TTreeFilterEdit.Destroy;
begin
  fSortedData.Free;
  fFilenameMap.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  FreeTVNodeData(fRootNode);
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

procedure TTreeFilterEdit.MapShortToFullFilename(ShortFilename, FullFilename: string);
begin
  fFilenameMap[ShortFilename]:=FullFilename;
end;

procedure TTreeFilterEdit.ApplyFilterCore;
var
  TVNode: TTreeNode;
  i: Integer;
  FileN, s: string;
  ena: Boolean;
begin
  if fFilenameMap.Count > 0 then
    FreeTVNodeData(fRootNode);   // Free node data now, it will be filled later.
  if Assigned(fRootNode) then    // Delete old tree nodes.
    fRootNode.DeleteChildren
  else
    fFilteredTreeview.Items.Clear;
  if fShowDirHierarchy then
    fTVNodeStack:=TFPList.Create;
  fFilteredTreeview.BeginUpdate;
  for i:=0 to fSortedData.Count-1 do begin
    FileN:=fSortedData[i];
    if fShowDirHierarchy then begin
      TVClearUnneededAndCreateHierachy(FileN);
      TVNode:=TTreeNode(fTVNodeStack[fTVNodeStack.Count-1]);
    end
    else if Assigned(fRootNode) then
      TVNode:=fFilteredTreeview.Items.AddChild(fRootNode,FileN)
    else
      TVNode:=fFilteredTreeview.Items.Add(Nil,FileN);
    if fFilenameMap.Count > 0 then begin
      s:=FileN;
      if fFilenameMap.Contains(FileN) then
        s:=fFilenameMap[FileN];           // Full file name.
      TVNode.Data:=TFileNameItem.Create(s);
    end;
    ena := True;
    if Assigned(OnGetImageIndex) then
      fImgIndex:=OnGetImageIndex(FileN, fSortedData.Objects[i], ena);
    TVNode.ImageIndex:=fImgIndex;
    TVNode.SelectedIndex:=fImgIndex;
    if Assigned(fSelectedPart) then
      TVNode.Selected:=fSelectedPart=fSortedData.Objects[i];
  end;
  if fShowDirHierarchy then      // TVDeleteUnneededNodes(0); ?
    fTVNodeStack.Free;
  if Assigned(fRootNode) then
    fRootNode.Expanded:=True;
  fFilteredTreeview.EndUpdate;
end;

function TTreeFilterEdit.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if SortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else if fShowDirHierarchy then
    Result:=CompareFilenames(ExtractFilePath(AFilename1), ExtractFilePath(AFilename2))
  else
    Result:=0;
end;

procedure TTreeFilterEdit.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  FileN: string;
begin
  fSortedData.Clear;
  for Origi:=0 to fOriginalData.Count-1 do begin
    FileN:=fOriginalData[Origi];
    if (Filter='') or (Pos(Filter,lowercase(FileN))>0) then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(FileN,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1,FileN, fOriginalData.Objects[Origi]);
    end;
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

procedure TTreeFilterEdit.MoveNext;
var tn: TTreeNode;
begin
  tn := fFilteredTreeview.Selected;
  if not Assigned(tn) then
  begin
    tn := fFilteredTreeview.TopItem;
    if Assigned(tn) then
      fFilteredTreeview.Selected := tn;
    Exit;
  end;
  tn := tn.GetNext;
  if Assigned(tn) then
    fFilteredTreeview.Selected := tn;
end;

procedure TTreeFilterEdit.MovePrev;
var tn: TTreeNode;
begin
  tn := fFilteredTreeview.Selected;
  if not Assigned(tn) then Exit;
  tn := tn.GetPrev;
  if Assigned(tn) then
    fFilteredTreeview.Selected := tn;
end;

procedure TTreeFilterEdit.FreeTVNodeData(Node: TTreeNode);
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
    FreeTVNodeData(Child);
    Child:=Child.GetNextSibling;
  end;
end;

procedure TTreeFilterEdit.TVDeleteUnneededNodes(p: integer);
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

procedure TTreeFilterEdit.TVClearUnneededAndCreateHierachy(Filename: string);
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
          Node:=fFilteredTreeview.Items.Add(Node,FilePart);
        fTVNodeStack[p]:=Node;
      end;
    end else begin
      // new sub node
      if p>0 then
        Node:=TTreeNode(fTVNodeStack[p-1])
      else
        Node:=fRootNode;
      if Node.GetFirstChild<>nil then begin
        Node:=Node.GetFirstChild;
        Node.Text:=FilePart;
      end
      else
        Node:=fFilteredTreeview.Items.AddChild(Node,FilePart);
      fTVNodeStack.Add(Node);
    end;
    if (Filename<>'') then begin
      Node.ImageIndex:=fImageIndexDirectory;
      Node.SelectedIndex:=Node.ImageIndex;
    end;
    inc(p);
  end;
end;

end.

