unit ListFilterEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LResources, LCLType, Graphics,
  Controls, StdCtrls, ComCtrls, EditBtn, FileUtil, AvgLvlTree, ImgList;

resourcestring
  lisCEFilter = '(Filter)';

type

  TImageIndexEvent = function (Str: String; Data: TObject;
                               var IsEnabled: Boolean): Integer of object;

  TListFilterEdit = class;

  { TViewControl }

  // A wrapper for the visual control displaying data
  TViewControl = class
  private
    fOwner: TListFilterEdit;
    fImgIndex: Integer;
    fSortedData: TStringList;
    function CompareFNs(AFilename1,AFilename2: string): integer;
    procedure SortAndFilter;
    procedure ApplyFilter; virtual; abstract;
    procedure StoreSelection; virtual; abstract;
    procedure RestoreSelection; virtual; abstract;
    procedure MoveNext; virtual; abstract;
    procedure MovePrev; virtual; abstract;
  public
    constructor Create(AOwner: TListFilterEdit);
    destructor Destroy; override;
  end;

  { TViewControlTreeview }

  TViewControlTreeview = class(TViewControl)
  private
    fTreeview: TTreeview;
    fTVNodeStack: TFPList;
    procedure ApplyFilter; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
    procedure MoveNext; override;
    procedure MovePrev; override;
    procedure FreeTVNodeData(Node: TTreeNode);
    procedure TVDeleteUnneededNodes(p: integer);
    procedure TVClearUnneededAndCreateHierachy(Filename: string);
  public
    constructor Create(AOwner: TListFilterEdit; aTreeview: TTreeview);
    destructor Destroy; override;
  end;

  { TViewControlListbox }

  TViewControlListbox = class(TViewControl)
  private
    fListbox: TListbox;
    procedure ApplyFilter; override;
    procedure StoreSelection; override;
    procedure RestoreSelection; override;
    procedure MoveNext; override;
    procedure MovePrev; override;
  public
    constructor Create(AOwner: TListFilterEdit; aListbox: TListbox);
    destructor Destroy; override;
  end;

  { TListFilterEdit }

  TListFilterEdit = class(TCustomEditButton)
    procedure ListBoxDrawItem(Control: TWinControl;
      Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    fFilter: string;
    // A control showing the (filtered) data. These are exclusive, only one is used.
    fFilteredTreeview: TTreeview;
    fFilteredListbox: TListbox;
    fViewControlWrapper: TViewControl; // Wraps either TTreeview or TListbox.
    fImageIndexDirectory: integer;     // Needed if directory structure is shown.
    fImages4Listbox: TCustomImageList; // Listbox does not have ImageList of its own.
    fNeedUpdate: Boolean;
    fIdleConnected: Boolean;
    fIsFirstUpdate: Boolean;
    fSelectedPart: TObject;         // Select this node on next update
    fSelectionList: TStringList;    // or store/restore the old selections here.
    fShowDirHierarchy: Boolean;     // Show direcories / files as a tree structure.
    fSortData: Boolean;             // Data needs to be sorted.
    // Full filename in node data is needed when showing the directory hierarchy.
    // It is stored automatically if the map is populated by MapShortToFullFilename.
    fFilenameMap: TStringToStringTree;
    // Data supplied by caller through Data property.
    // Objects property is passed to OnGetImageIndex.
    fOriginalData: TStringList;
    // Data sorted for viewing.
    fRootNode: TTreeNode;           // The filtered items are under this node.
    fOnGetImageIndex: TImageIndexEvent;
    procedure SetFilter(const AValue: string);
    procedure SetFilteredTreeview(const AValue: TTreeview);
    procedure SetFilteredListbox(const AValue: TListBox);
    procedure SetShowDirHierarchy(const AValue: Boolean);
    procedure SetIdleConnected(const AValue: Boolean);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure Change; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoButtonClick (Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MapShortToFullFilename(ShortFilename, FullFilename: string);
    procedure ApplyFilter(Immediately: Boolean = False);
    procedure InvalidateFilter;
    procedure StoreSelection; // Calls the ViewControlWrapper method
  public
    property Filter: string read fFilter write SetFilter;
    property ImageIndexDirectory: integer read fImageIndexDirectory write fImageIndexDirectory;
    property IdleConnected: Boolean read fIdleConnected write SetIdleConnected;
    property SelectedPart: TObject read fSelectedPart write fSelectedPart;
    property SelectionList: TStringList read fSelectionList;
    property ShowDirHierarchy: Boolean read fShowDirHierarchy write SetShowDirHierarchy;
    property SortData: Boolean read fSortData write fSortData;
    property Data: TStringList read fOriginalData;
    property RootNode: TTreeNode read fRootNode write fRootNode;
  published
    // TListFilterEdit properties.
    property FilteredTreeview: TTreeview read fFilteredTreeview write SetFilteredTreeview;
    property FilteredListbox: TListBox read fFilteredListbox write SetFilteredListbox;
    property Images4Listbox: TCustomImageList read fImages4Listbox write fImages4Listbox; deprecated 'use OnDrawItem handler in FilteredListbox';
    property OnGetImageIndex: TImageIndexEvent read fOnGetImageIndex write fOnGetImageIndex; deprecated 'use OnDrawItem handler in FilteredListbox';
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property AutoSelect;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

  { TFileNameItem }

  TFileNameItem = class
  public
    Filename: string;
    constructor Create(AFilename: string);
  end;

var
  ListFilterGlyph: TBitmap;

const
  ResBtnListFilter = 'btnfiltercancel';

procedure Register;

implementation

procedure Register;
begin
  {$I listfilteredit_icon.lrs}
  RegisterComponents('LazControls',[TListFilterEdit]);
end;

{ TFileNameItem }

constructor TFileNameItem.Create(AFilename: string);
begin
  Filename:=AFilename;
end;

{ TViewControl }

constructor TViewControl.Create(AOwner: TListFilterEdit);
begin
  inherited Create;
  fOwner:=AOwner;
  fImgIndex:=-1;
  fSortedData:=TStringList.Create;
end;

destructor TViewControl.Destroy;
begin
  fSortedData.Free;
  inherited Destroy;
end;

function TViewControl.CompareFNs(AFilename1,AFilename2: string): integer;
begin
  if fOwner.fSortData then
    Result:=CompareFilenames(AFilename1, AFilename2)
  else if fOwner.fShowDirHierarchy then
    Result:=CompareFilenames(ExtractFilePath(AFilename1), ExtractFilePath(AFilename2))
  else
    Result:=0;
end;

procedure TViewControl.SortAndFilter;
// Copy data from fOriginalData to fSortedData in sorted order
var
  Origi, i: Integer;
  FileN: string;
begin
  fSortedData.Clear;
  for Origi:=0 to fOwner.fOriginalData.Count-1 do begin
    FileN:=fOwner.fOriginalData[Origi];
    if (fOwner.Filter='') or (Pos(fOwner.Filter,lowercase(FileN))>0) then begin
      i:=fSortedData.Count-1;
      while i>=0 do begin
        if CompareFNs(FileN,fSortedData[i])>=0 then break;
        dec(i);
      end;
      fSortedData.InsertObject(i+1,FileN, fOwner.fOriginalData.Objects[Origi]);
    end;
  end;
end;

{ TViewControlTreeview }

constructor TViewControlTreeview.Create(AOwner: TListFilterEdit; aTreeview: TTreeview);
begin
  inherited Create(AOwner);
  fTreeview:=aTreeview;
end;

destructor TViewControlTreeview.Destroy;
begin
  FreeTVNodeData(fOwner.fRootNode);
  inherited Destroy;
end;

procedure TViewControlTreeview.ApplyFilter;
var
  TVNode: TTreeNode;
  i: Integer;
  FileN, s: string;
  ena: Boolean;
begin
  with fOwner do begin
    if fFilenameMap.Count > 0 then
      FreeTVNodeData(fRootNode);   // Free node data now, it will be filled later.
    if Assigned(fRootNode) then    // Delete old tree nodes.
      fRootNode.DeleteChildren
    else
      fTreeview.Items.Clear;
    if fShowDirHierarchy then
      fTVNodeStack:=TFPList.Create;
    fTreeview.BeginUpdate;
    for i:=0 to fSortedData.Count-1 do begin
      FileN:=fSortedData[i];
      if fShowDirHierarchy then begin
        TVClearUnneededAndCreateHierachy(FileN);
        TVNode:=TTreeNode(fTVNodeStack[fTVNodeStack.Count-1]);
      end
      else if Assigned(fRootNode) then
        TVNode:=fTreeview.Items.AddChild(fRootNode,FileN)
      else
        TVNode:=fTreeview.Items.Add(Nil,FileN);
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
    fTreeview.EndUpdate;
  end;
end;

procedure TViewControlTreeview.StoreSelection;
var
  ANode: TTreeNode;
begin
  fOwner.fSelectionList.Clear;
  ANode:=fTreeview.Selected;
  while ANode<>nil do begin
    fOwner.fSelectionList.Insert(0,ANode.Text);
    ANode:=ANode.Parent;
  end;
end;

procedure TViewControlTreeview.RestoreSelection;
var
  ANode: TTreeNode;
  CurText: string;
begin
  ANode:=nil;
  with fOwner do begin
    while fSelectionList.Count>0 do begin
      CurText:=fSelectionList[0];
      if ANode=nil then
        ANode:=fTreeview.Items.GetFirstNode
      else
        ANode:=ANode.GetFirstChild;
      while (ANode<>nil) and (ANode.Text<>CurText) do
        ANode:=ANode.GetNextSibling;
      if ANode=nil then break;
      fSelectionList.Delete(0);
    end;
    if ANode<>nil then
      fTreeview.Selected:=ANode;
  end;
end;

procedure TViewControlTreeview.MoveNext;
var tn: TTreeNode;
begin
  tn := fTreeview.Selected;
  if not Assigned(tn) then
  begin
    tn := fTreeview.TopItem;
    if Assigned(tn) then fTreeview.Selected := tn;
    Exit;
  end;
  tn := tn.GetNext;
  if Assigned(tn) then fTreeview.Selected := tn;
end;

procedure TViewControlTreeview.MovePrev;
var tn: TTreeNode;
begin
  tn := fTreeview.Selected;
  if not Assigned(tn) then Exit;
  tn := tn.GetPrev;
  if Assigned(tn) then fTreeview.Selected := tn;
end;

procedure TViewControlTreeview.FreeTVNodeData(Node: TTreeNode);
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

procedure TViewControlTreeview.TVDeleteUnneededNodes(p: integer);
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

procedure TViewControlTreeview.TVClearUnneededAndCreateHierachy(Filename: string);
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
          Node:=fTreeview.Items.Add(Node,FilePart);
        fTVNodeStack[p]:=Node;
      end;
    end else begin
      // new sub node
      if p>0 then
        Node:=TTreeNode(fTVNodeStack[p-1])
      else
        Node:=fOwner.fRootNode;
      if Node.GetFirstChild<>nil then begin
        Node:=Node.GetFirstChild;
        Node.Text:=FilePart;
      end
      else
        Node:=fTreeview.Items.AddChild(Node,FilePart);
      fTVNodeStack.Add(Node);
    end;
    if (Filename<>'') then begin
      Node.ImageIndex:=fOwner.fImageIndexDirectory;
      Node.SelectedIndex:=Node.ImageIndex;
    end;
    inc(p);
  end;
end;


{ TViewControlListbox }

constructor TViewControlListbox.Create(AOwner: TListFilterEdit; aListbox: TListbox);
begin
  inherited Create(AOwner);
  fListbox:=aListbox;
end;

destructor TViewControlListbox.Destroy;
begin
  inherited Destroy;
end;

procedure TViewControlListbox.ApplyFilter;
var
  i: Integer;
  FileN: string;
begin
  fListbox.Clear;
  fListbox.Items.BeginUpdate;
  with fOwner do
    for i:=0 to fSortedData.Count-1 do begin
      FileN:=fSortedData[i];
      fListbox.Items.AddObject(FileN, fSortedData.Objects[i]);
      if Assigned(fSelectedPart) then
        fListbox.Selected[i]:=fSelectedPart=fSortedData.Objects[i];
    end;
  fListbox.Items.EndUpdate;
end;

procedure TViewControlListbox.StoreSelection;
var
  i: Integer;
begin
  fOwner.fSelectionList.Clear;
  for i := 0 to fListbox.Count-1 do begin
    if fListbox.Selected[i] then
      fOwner.fSelectionList.Add(fListbox.Items[i]);
  end;
end;

procedure TViewControlListbox.RestoreSelection;
var
  i: Integer;
begin
  for i := 0 to fListbox.Count-1 do begin
    if fOwner.fSelectionList.IndexOf(fListbox.Items[i])>0 then
      fListbox.Selected[i]:=True;
  end;
end;

procedure TViewControlListbox.MoveNext;
var i: Integer;
begin
  i := fListbox.ItemIndex + 1;
  if i < fListbox.Count then fListbox.ItemIndex := i;
end;

procedure TViewControlListbox.MovePrev;
var i: Integer;
begin
  i := fListbox.ItemIndex - 1;
  if i >= 0 then fListbox.ItemIndex := i;
end;


{ TListBoxFilterEdit }

constructor TListFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fOriginalData:=TStringList.Create;
  fSelectionList:=TStringList.Create;
  fFilenameMap:=TStringToStringTree.Create(True);
  fImageIndexDirectory := -1;
  Button.Enabled:=False;
end;

destructor TListFilterEdit.Destroy;
begin
  fFilenameMap.Free;
  fSelectionList.Free;
  fOriginalData.Free;
  fViewControlWrapper.Free;
  inherited Destroy;
end;

procedure TListFilterEdit.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  ImgIndex: Integer;
  ena: Boolean;
begin
  if Index < 0 then Exit;
  ena:=True;
  ImgIndex:=-1;
  if Assigned(fImages4Listbox) and Assigned(OnGetImageIndex) then begin
    ImgIndex:=OnGetImageIndex(fFilteredListbox.Items[Index],
                              fFilteredListbox.Items.Objects[Index], ena);
  end;
  fFilteredListbox.Canvas.FillRect(ARect);
  if ImgIndex<>-1 then
  begin
    if not (ena or (odSelected in State)) then
      fFilteredListbox.Canvas.Font.Color := clGreen;
    fImages4Listbox.Draw(fFilteredListbox.Canvas, 1, ARect.Top, ImgIndex, ena);
  end;
  fFilteredListbox.Canvas.TextRect(ARect, ARect.Left + 20,ARect.Top,
                                fFilteredListbox.Items[Index]);
end;

procedure TListFilterEdit.Change;
begin
  Filter:=Text;
  inherited;
end;

procedure TListFilterEdit.DoEnter;
begin
  if Text=lisCEFilter then
    Text:='';
  inherited;
end;

procedure TListFilterEdit.DoExit;
begin
  Filter:=Text;
  inherited;
end;

procedure TListFilterEdit.DoButtonClick(Sender: TObject);
begin
  Filter:='';
end;

procedure TListFilterEdit.SetFilter(const AValue: string);
var
  NewValue: String;
begin
  if AValue=lisCEFilter then
    NewValue:=''
  else
    NewValue:=LowerCase(AValue);
  Button.Enabled:=NewValue<>'';
  if (NewValue='') and not Focused then begin
    Text:=lisCEFilter;
    Font.Color:=clBtnShadow;
  end
  else begin
    Text:=NewValue;
    Font.Color:=clDefault;
  end;
  if fFilter=NewValue then exit;
  fFilter:=NewValue;
  ApplyFilter;
end;

procedure TListFilterEdit.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if fNeedUpdate then
    ApplyFilter(true);
  IdleConnected:=false;
end;

procedure TListFilterEdit.SetIdleConnected(const AValue: boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TListFilterEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(fViewControlWrapper)
    and (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
  begin
    case Key of
      VK_UP:   fViewControlWrapper.MovePrev;
      VK_DOWN: fViewControlWrapper.MoveNext;
    end;
    Key:=0;
  end
  else inherited KeyDown(Key, Shift);
end;

function TListFilterEdit.GetDefaultGlyph: TBitmap;
begin
  Result := ListFilterGlyph;
end;

function TListFilterEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnListFilter;
end;

procedure TListFilterEdit.SetFilteredTreeview(const AValue: TTreeview);
begin
  if fFilteredTreeview = AValue then Exit;
  if Assigned(fFilteredListbox) then
    FilteredListbox := nil;
  fFilteredTreeview:=AValue;
  if Assigned(fViewControlWrapper) then FreeAndNil(fViewControlWrapper);
  if AValue = nil then Exit;
  fViewControlWrapper:=TViewControlTreeview.Create(Self, fFilteredTreeview);
  fIsFirstUpdate := True;
end;

procedure TListFilterEdit.SetFilteredListbox(const AValue: TListBox);
begin
  if fFilteredListbox = AValue then Exit;
  if Assigned(fFilteredTreeview) then
    FilteredTreeview := nil;
  if Assigned(fFilteredListbox)
    and (fFilteredListbox.OnDrawItem = @ListBoxDrawItem) then
      fFilteredListbox.OnDrawItem := nil;
  fFilteredListbox:=AValue;
  if Assigned(fViewControlWrapper) then FreeAndNil(fViewControlWrapper);
  if AValue = nil then Exit;
  if fFilteredListbox.Style<>lbStandard then begin
    // if Listbox.Style<>Standard then it is OwnerDraw,
    // and OnDrawItem should be defined, so the following code is useless
    if not (csDesigning in fFilteredListbox.ComponentState)
      and not Assigned(fFilteredListbox.OnDrawItem) then
        // AP: I propose not to use ListBoxDrawItem at all, DrawItem handler of FilteredListbox must be used
        fFilteredListbox.OnDrawItem:=@ListBoxDrawItem;
  end;
  fViewControlWrapper:=TViewControlListbox.Create(Self, fFilteredListbox);
  fOriginalData.Assign(fFilteredListbox.Items);
  fIsFirstUpdate := True;
end;

procedure TListFilterEdit.SetShowDirHierarchy(const AValue: Boolean);
begin
  if fShowDirHierarchy=AValue then exit;
  if not Assigned(fFilteredTreeview) then
    raise Exception.Create('Showing directory hierarchy requires Treeview.');
  fShowDirHierarchy:=AValue;
end;

procedure TListFilterEdit.MapShortToFullFilename(ShortFilename, FullFilename: string);
begin
  fFilenameMap[ShortFilename]:=FullFilename;
end;

procedure TListFilterEdit.ApplyFilter(Immediately: Boolean);
begin
  if Immediately then begin
    fNeedUpdate := False;
    if not Assigned(fViewControlWrapper) then Exit;
    fViewControlWrapper.SortAndFilter;
    if (fSelectedPart=Nil) and not fIsFirstUpdate then
      fViewControlWrapper.StoreSelection; // At first round the selection is from caller
    fIsFirstUpdate:=False;
    fViewControlWrapper.ApplyFilter;
    fSelectedPart:=Nil;
    fViewControlWrapper.RestoreSelection;
  end
  else begin
    if csDestroying in ComponentState then exit;
    InvalidateFilter;
  end;
end;

procedure TListFilterEdit.InvalidateFilter;
begin
  fNeedUpdate:=true;
  IdleConnected:=true;
end;

procedure TListFilterEdit.StoreSelection;
begin
  fViewControlWrapper.StoreSelection;
end;

end.

