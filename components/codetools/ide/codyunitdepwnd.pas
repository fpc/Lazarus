unit CodyUnitDepWnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, AVL_Tree, contnrs, FileUtil, lazutf8classes,
  LazLogger, TreeFilterEdit, CTUnitGraph, CodeToolManager, DefineTemplates,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls,
  LCLType, LazIDEIntf, ProjectIntf, IDEWindowIntf;

resourcestring
  a='';
  rsSelectAUnit = 'Select a unit';
  rsClose = 'Close';

const
  DefaultCategoryGap = 0.04;
  DefaultFirstCategory = 0;
  DefaultCategoryMinSize = 1.0;
  DefaultItemSize = 1.0;
type
  TCustomCircleDiagramControl = class;
  TCircleDiagramCategory = class;

  { TCircleDiagramItem }

  TCircleDiagramItem = class(TPersistent)
  private
    FCaption: TCaption;
    FCategory: TCircleDiagramCategory;
    FEndDegree: single;
    FSize: single;
    FStartDegree: single;
    procedure SetCaption(AValue: TCaption);
    procedure SetSize(AValue: single);
    procedure UpdateLayout;
  public
    constructor Create(TheCategory: TCircleDiagramCategory);
    destructor Destroy; override;
    property Category: TCircleDiagramCategory read FCategory;
    property Caption: TCaption read FCaption write SetCaption;
    property Size: single read FSize write SetSize default DefaultItemSize; // scaled to fit
    property StartDegree: single read FStartDegree;
    property EndDegree: single read FEndDegree;
  end;

  { TCircleDiagramCategory }

  TCircleDiagramCategory = class(TPersistent)
  private
    FCaption: TCaption;
    FColor: TColor;
    FDiagram: TCustomCircleDiagramControl;
    FEndDegree: single;
    FMinSize: single;
    fItems: TObjectList; // list of TCircleDiagramItem
    FSize: single;
    FStartDegree: single;
    function GetItems(Index: integer): TCircleDiagramItem;
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetMinSize(AValue: single);
    procedure UpdateLayout;
    procedure Invalidate;
    procedure InternalRemoveItem(Item: TCircleDiagramItem);
  public
    constructor Create(TheDiagram: TCustomCircleDiagramControl);
    destructor Destroy; override;
    function InsertItem(Index: integer; aCaption: string): TCircleDiagramItem;
    function AddItem(aCaption: string): TCircleDiagramItem;
    property Diagram: TCustomCircleDiagramControl read FDiagram;
    property Caption: TCaption read FCaption write SetCaption;
    property MinSize: single read FMinSize write SetMinSize default DefaultCategoryMinSize; // scaled to fit
    function Count: integer;
    property Items[Index: integer]: TCircleDiagramItem read GetItems; default;
    property Color: TColor read FColor write SetColor;
    property Size: single read FSize;
    property StartDegree: single read FStartDegree;
    property EndDegree: single read FEndDegree;
  end;

  TCircleDiagramCtrlFlag = (
    cdcNeedUpdateLayout
    );
  TCircleDiagramCtrlFlags = set of TCircleDiagramCtrlFlag;

  { TCustomCircleDiagramControl }

  TCustomCircleDiagramControl = class(TCustomControl)
  private
    FCategoryGap: single;
    FCenter: TPoint;
    FCenterCaption: TCaption;
    FCenterCaptionRect: TRect;
    FFirstCategory: single;
    fCategories: TObjectList; // list of TCircleDiagramCategory
    FInnerRadius: single;
    FOuterRadius: single;
    fUpdateLock: integer;
    fFlags: TCircleDiagramCtrlFlags;
    function GetCategories(Index: integer): TCircleDiagramCategory;
    procedure SetCategoryGap(AValue: single);
    procedure SetCenterCaption(AValue: TCaption);
    procedure SetFirstCategory(AValue: single);
    procedure InternalRemoveCategory(Category: TCircleDiagramCategory);
  protected
    //procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    //procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure CreateWnd; override;
    procedure UpdateScrollBar;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

    //procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    //procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    //procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;

    //procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    //procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure Paint; override;
    procedure DrawCategory(i: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CenterCaption: TCaption read FCenterCaption write SetCenterCaption;
    procedure Clear;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure UpdateLayout;
    procedure EraseBackground({%H-}DC: HDC); override;
    function InsertCategory(Index: integer; aCaption: TCaption): TCircleDiagramCategory;
    function AddCategory(aCaption: TCaption): TCircleDiagramCategory;
    function IndexOfCategory(aCaption: TCaption): integer;
    function FindCategory(aCaption: TCaption): TCircleDiagramCategory;
    property CategoryGap: single read FCategoryGap write SetCategoryGap default DefaultCategoryGap; // 0..0.3 part of a full circle
    property FirstCategoryDeg: single read FFirstCategory write SetFirstCategory default DefaultFirstCategory; // 0..1 part of a full circle starting at top
    function CategoryCount: integer;
    property Categories[Index: integer]: TCircleDiagramCategory read GetCategories; default;
    property Color default clWhite;
    // computed values
    property CenterCaptionRect: TRect read FCenterCaptionRect;
    property Center: TPoint read FCenter;
    property InnerRadius: single read FInnerRadius;
    property OuterRadius: single read FOuterRadius;
  end;

  { TCircleDiagramControl }

  TCircleDiagramControl = class(TCustomCircleDiagramControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

  TUDDUsesType = (
    uddutInterfaceUses,
    uddutImplementationUses,
    uddutUsedByInterface,
    uddutUsedByImplementation
    );
  TUDDUsesTypes = set of TUDDUsesType;

  { TUnitDependenciesDialog }

  TUnitDependenciesDialog = class(TForm)
    BtnPanel: TPanel;
    CloseBitBtn: TBitBtn;
    CurUnitPanel: TPanel;
    CurUnitSplitter: TSplitter;
    CurUnitTreeView: TTreeView;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    CurUnitTreeFilterEdit: TTreeFilterEdit;
    procedure CloseBitBtnClick(Sender: TObject);
    procedure CurUnitTreeViewSelectionChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentUnit: TUGUnit;
    FIdleConnected: boolean;
    FUsesGraph: TUsesGraph;
    fCircleCategories: array[TUDDUsesType] of TCircleDiagramCategory;
    procedure SetCurrentUnit(AValue: TUGUnit);
    procedure SetIdleConnected(AValue: boolean);
    procedure AddStartAndTargetUnits;
    procedure UpdateAll;
    procedure UpdateCurUnitDiagram;
    procedure UpdateCurUnitTreeView;
    function NodeTextToUnit(NodeText: string): TUGUnit;
    function UGUnitToNodeText(UGUnit: TUGUnit): string;
  public
    CurUnitDiagram: TCircleDiagramControl;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property UsesGraph: TUsesGraph read FUsesGraph;
    property CurrentUnit: TUGUnit read FCurrentUnit write SetCurrentUnit;
  end;

var
  UnitDependenciesDialog: TUnitDependenciesDialog;

procedure ShowUnitDependenciesDialog(Sender: TObject);

implementation

procedure ShowUnitDependenciesDialog(Sender: TObject);
var
  Dlg: TUnitDependenciesDialog;
begin
  Dlg:=TUnitDependenciesDialog.Create(nil);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TCustomCircleDiagramControl }

procedure TCustomCircleDiagramControl.SetCategoryGap(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if AValue>0.3 then AValue:=0.3;
  if FCategoryGap=AValue then Exit;
  FCategoryGap:=AValue;
  UpdateLayout;
end;

function TCustomCircleDiagramControl.GetCategories(Index: integer
  ): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory(fCategories[Index]);
end;

procedure TCustomCircleDiagramControl.SetCenterCaption(AValue: TCaption);
begin
  if FCenterCaption=AValue then Exit;
  FCenterCaption:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.SetFirstCategory(AValue: single);
begin
  if FFirstCategory=AValue then Exit;
  FFirstCategory:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.InternalRemoveCategory(
  Category: TCircleDiagramCategory);
begin
  fCategories.Remove(Category);
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.UpdateScrollBar;
begin

end;

procedure TCustomCircleDiagramControl.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.Paint;
var
  i: Integer;
begin
  inherited Paint;
  if cdcNeedUpdateLayout in fFlags then
    UpdateLayout;

  // background
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color:=clRed;
  //Canvas.Chord(FCenter.X,FCenter.Y,FCenter.X+50,FCenter.Y+50,0,3000);

  // draw categories
  for i:=0 to CategoryCount-1 do
    DrawCategory(i);

  // center caption
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clNone;
  Canvas.TextOut(FCenterCaptionRect.Left,FCenterCaptionRect.Top,CenterCaption);
end;

procedure TCustomCircleDiagramControl.DrawCategory(i: integer);
var
  Cat: TCircleDiagramCategory;
begin
  Cat:=Categories[i];
  //Canvas.Chord();
end;

constructor TCustomCircleDiagramControl.Create(AOwner: TComponent);
begin
  BeginUpdate;
  try
    inherited Create(AOwner);
    fCategories:=TObjectList.create(true);
    FFirstCategory:=DefaultFirstCategory;
    fCategoryGap:=DefaultCategoryGap;
    Color:=clWhite;
  finally
    EndUpdate;
  end;
end;

destructor TCustomCircleDiagramControl.Destroy;
begin
  BeginUpdate; // disable updates
  FreeAndNil(fCategories);
  inherited Destroy;
end;

procedure TCustomCircleDiagramControl.Clear;
begin
  BeginUpdate;
  try
    while CategoryCount>0 do
      Categories[0].Free;
  finally
    EndUpdate;
  end;
end;

procedure TCustomCircleDiagramControl.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TCustomCircleDiagramControl.EndUpdate;
begin
  if fUpdateLock=0 then
    raise Exception.Create('TCustomCircleDiagramControl.EndUpdate');
  dec(fUpdateLock);
  if fUpdateLock=0 then begin
    if cdcNeedUpdateLayout in fFlags then
      UpdateLayout;
  end;
end;

procedure TCustomCircleDiagramControl.UpdateLayout;
var
  aSize: TSize;
  aCategory: TCircleDiagramCategory;
  i: Integer;
  j: Integer;
  TotalSize: Single;
  CurCategoryDegree: Single;
  GapDegree: Single;
  TotalItemDegree: Single;
  Item: TCircleDiagramItem;
  CurItemDegree: Single;
begin
  if (fUpdateLock>0) or (not IsVisible) or (not HandleAllocated) then begin
    Include(fFlags,cdcNeedUpdateLayout);
    exit;
  end;
  Exclude(fFlags,cdcNeedUpdateLayout);

  // center caption
  FCenter:=Point(ClientWidth div 2,ClientHeight div 2);
  aSize:=Canvas.TextExtent(CenterCaption);
  FCenterCaptionRect:=Bounds(FCenter.X-(aSize.cx div 2),FCenter.Y-(aSize.cy div 2)
    ,aSize.cx,aSize.cy);

  // radius
  fInnerRadius:=0.5*Min(ClientWidth,ClientHeight);
  fOuterRadius:=1.1*InnerRadius;

  // degrees
  TotalSize:=0.0;
  CurCategoryDegree:=FirstCategoryDeg;
  if CategoryCount>0 then begin
    // calculate TotalSize
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FSize:=0;
      for j:=0 to aCategory.Count-1 do
        aCategory.FSize+=aCategory[j].Size;
      aCategory.FSize:=Max(aCategory.FSize,aCategory.MinSize);
      TotalSize+=aCategory.FSize;
    end;

    // calculate degrees
    GapDegree:=Min(CategoryGap,0.8/CategoryCount);
    TotalItemDegree:=1.0-(GapDegree*CategoryCount);
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FStartDegree:=CurCategoryDegree;
      if TotalSize>0 then
        CurCategoryDegree+=TotalItemDegree*aCategory.Size/TotalSize;
      aCategory.FEndDegree:=CurCategoryDegree;

      // item degrees
      CurItemDegree:=aCategory.StartDegree;
      for j:=0 to aCategory.Count-1 do begin
        Item:=aCategory[j];

        Item.FStartDegree:=CurItemDegree;
        if aCategory.Size>0 then
          CurItemDegree+=(aCategory.EndDegree-aCategory.StartDegree)*Item.Size/aCategory.Size;
        Item.FEndDegree:=CurItemDegree;
      end;

      CurCategoryDegree+=GapDegree;
    end;
  end;

end;

procedure TCustomCircleDiagramControl.EraseBackground(DC: HDC);
begin
  // do not erase background, Paint will paint the whole area
end;

function TCustomCircleDiagramControl.InsertCategory(Index: integer;
  aCaption: TCaption): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory.Create(Self);
  Result.Caption:=aCaption;
  fCategories.Insert(Index,Result);
end;

function TCustomCircleDiagramControl.AddCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
begin
  Result:=InsertCategory(CategoryCount,aCaption);
end;

function TCustomCircleDiagramControl.IndexOfCategory(aCaption: TCaption
  ): integer;
begin
  Result:=CategoryCount-1;
  while Result>=0 do begin
    if Categories[Result].Caption=aCaption then exit;
    dec(Result);
  end;
end;

function TCustomCircleDiagramControl.FindCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
var
  i: Integer;
begin
  i:=IndexOfCategory(aCaption);
  if i>=0 then
    Result:=Categories[i]
  else
    Result:=nil;
end;

function TCustomCircleDiagramControl.CategoryCount: integer;
begin
  Result:=fCategories.Count;
end;

{ TCircleDiagramCategory }

procedure TCircleDiagramCategory.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCircleDiagramCategory.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

function TCircleDiagramCategory.GetItems(Index: integer): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem(fItems[Index]);
end;

procedure TCircleDiagramCategory.SetMinSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FMinSize=AValue then Exit;
  FMinSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramCategory.UpdateLayout;
begin
  if Diagram<>nil then
    Diagram.UpdateLayout;
end;

procedure TCircleDiagramCategory.Invalidate;
begin
  if Diagram<>nil then
    Diagram.Invalidate;
end;

procedure TCircleDiagramCategory.InternalRemoveItem(Item: TCircleDiagramItem);
begin
  fItems.Remove(Item);
  UpdateLayout;
end;

constructor TCircleDiagramCategory.Create(
  TheDiagram: TCustomCircleDiagramControl);
begin
  FDiagram:=TheDiagram;
  fItems:=TObjectList.Create(true);
  FMinSize:=DefaultCategoryMinSize;
end;

destructor TCircleDiagramCategory.Destroy;
begin
  if Diagram<>nil then
    Diagram.InternalRemoveCategory(Self);
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TCircleDiagramCategory.InsertItem(Index: integer; aCaption: string
  ): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem.Create(Self);
  Result.Caption:=aCaption;
  fItems.Insert(Index,Result);
end;

function TCircleDiagramCategory.AddItem(aCaption: string): TCircleDiagramItem;
begin
  Result:=InsertItem(Count,aCaption);
end;

function TCircleDiagramCategory.Count: integer;
begin
  Result:=fItems.Count;
end;

{ TCircleDiagramItem }

procedure TCircleDiagramItem.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.SetSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FSize=AValue then Exit;
  FSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.UpdateLayout;
begin
  if Category<>nil then
    Category.UpdateLayout;
end;

constructor TCircleDiagramItem.Create(TheCategory: TCircleDiagramCategory);
begin
  FCategory:=TheCategory;
  FSize:=DefaultItemSize;
end;

destructor TCircleDiagramItem.Destroy;
begin
  if Category<>nil then
    Category.InternalRemoveItem(Self);
  inherited Destroy;
end;

{ TUnitDependenciesDialog }

procedure TUnitDependenciesDialog.CloseBitBtnClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TUnitDependenciesDialog.CurUnitTreeViewSelectionChanged(
  Sender: TObject);
var
  CurUnit: TUGUnit;
begin
  if CurUnitTreeView.Selected=nil then exit;
  CurUnit:=NodeTextToUnit(CurUnitTreeView.Selected.Text);
  if CurUnit=nil then exit;
  CurrentUnit:=CurUnit;
end;

procedure TUnitDependenciesDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TUnitDependenciesDialog.FormCreate(Sender: TObject);
begin
  FUsesGraph:=CodeToolBoss.CreateUsesGraph;
  ProgressBar1.Style:=pbstMarquee;
  AddStartAndTargetUnits;

  Caption:='Unit Dependencies';
  CloseBitBtn.Caption:=rsClose;

  IDEDialogLayoutList.ApplyLayout(Self,600,400);

  CurUnitDiagram:=TCircleDiagramControl.Create(Self);
  with CurUnitDiagram do begin
    Name:='CurUnitDiagram';
    Align:=alClient;
    fCircleCategories[uddutInterfaceUses]:=AddCategory('Interface uses');
    fCircleCategories[uddutImplementationUses]:=AddCategory('Implementation uses');
    fCircleCategories[uddutUsedByInterface]:=AddCategory('Used by interfaces');
    fCircleCategories[uddutUsedByImplementation]:=AddCategory('Used by implementations');
    CenterCaption:=rsSelectAUnit;
    Parent:=Self;
  end;

  IdleConnected:=true;
end;

procedure TUnitDependenciesDialog.FormDestroy(Sender: TObject);
begin
  IdleConnected:=false;
  FreeAndNil(FUsesGraph);
end;

procedure TUnitDependenciesDialog.OnIdle(Sender: TObject; var Done: Boolean);
var
  Completed: boolean;
begin
  UsesGraph.Parse(true,Completed,200);
  if Completed then begin
    IdleConnected:=false;
    ProgressBar1.Visible:=false;
    ProgressBar1.Style:=pbstNormal;
    Timer1.Enabled:=false;
    UpdateAll;
  end;
end;

procedure TUnitDependenciesDialog.Timer1Timer(Sender: TObject);
begin
  UpdateAll;
end;

procedure TUnitDependenciesDialog.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TUnitDependenciesDialog.SetCurrentUnit(AValue: TUGUnit);
begin
  if FCurrentUnit=AValue then Exit;
  FCurrentUnit:=AValue;
  UpdateCurUnitDiagram;
end;

procedure TUnitDependenciesDialog.AddStartAndTargetUnits;
var
  aProject: TLazProject;
begin
  UsesGraph.TargetAll:=true;

  // project lpr
  aProject:=LazarusIDE.ActiveProject;
  if (aProject<>nil) and (aProject.MainFile<>nil) then
    UsesGraph.AddStartUnit(aProject.MainFile.Filename);

  // ToDo: add all open packages

end;

procedure TUnitDependenciesDialog.UpdateAll;
begin
  UpdateCurUnitTreeView;
end;

procedure TUnitDependenciesDialog.UpdateCurUnitDiagram;

  procedure UpdateCircleCategory(List: TFPList; t: TUDDUsesType);
  // List is CurrentUnit.UsesUnits or CurrentUnit.UsedByUnits
  var
    i: Integer;
    CurUses: TUGUses;
    Item: TCircleDiagramItem;
    CurUnit: TUGUnit;
    Cnt: Integer;
    s: String;
  begin
    Cnt:=0;
    for i:=0 to List.Count-1 do begin
      CurUses:=TUGUses(List[i]);
      if CurUses.InImplementation<>(t in [uddutImplementationUses,uddutUsedByImplementation])
      then continue;
      if t in [uddutInterfaceUses,uddutImplementationUses] then
        CurUnit:=CurUses.Owner
      else
        CurUnit:=CurUses.UsesUnit;
      s:=ExtractFileName(CurUnit.Filename);
      if fCircleCategories[t].Count>Cnt then begin
        Item:=fCircleCategories[t].Items[Cnt];
        Item.Caption:=s
      end else
        Item:=fCircleCategories[t].AddItem(s);
      inc(Cnt);
    end;
    while fCircleCategories[t].Count>Cnt do
      fCircleCategories[t].Items[Cnt].Free;
  end;

begin
  CurUnitDiagram.BeginUpdate;
  try
    if CurrentUnit<>nil then begin
      CurUnitDiagram.CenterCaption:=ExtractFilename(CurrentUnit.Filename);
      UpdateCircleCategory(CurrentUnit.UsesUnits,uddutInterfaceUses);
      UpdateCircleCategory(CurrentUnit.UsesUnits,uddutImplementationUses);
      UpdateCircleCategory(CurrentUnit.UsedByUnits,uddutUsedByInterface);
      UpdateCircleCategory(CurrentUnit.UsedByUnits,uddutUsedByImplementation);
    end else begin
      CurUnitDiagram.CenterCaption:=rsSelectAUnit;
    end;
  finally
    CurUnitDiagram.EndUpdate;
  end;
end;

procedure TUnitDependenciesDialog.UpdateCurUnitTreeView;
var
  AVLNode: TAVLTreeNode;
  CurUnit: TUGUnit;
  sl: TStringListUTF8;
  i: Integer;
begin
  CurUnitTreeView.BeginUpdate;
  sl:=TStringListUTF8.Create;
  try
    CurUnitTreeView.Items.Clear;

    AVLNode:=UsesGraph.FilesTree.FindLowest;
    while AVLNode<>nil do begin
      CurUnit:=TUGUnit(AVLNode.Data);
      sl.Add(UGUnitToNodeText(CurUnit));
      AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
    end;

    sl.CustomSort(@CompareStringListItemsUTF8LowerCase);
    for i:=0 to sl.Count-1 do begin
      CurUnitTreeView.Items.Add(nil,sl[i]);
    end;
  finally
    sl.Free;
    CurUnitTreeView.EndUpdate;
  end;
end;

function TUnitDependenciesDialog.NodeTextToUnit(NodeText: string): TUGUnit;
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode:=UsesGraph.FilesTree.FindLowest;
  while AVLNode<>nil do begin
    Result:=TUGUnit(AVLNode.Data);
    if NodeText=UGUnitToNodeText(Result) then exit;
    AVLNode:=UsesGraph.FilesTree.FindSuccessor(AVLNode);
  end;
  Result:=nil;
end;

function TUnitDependenciesDialog.UGUnitToNodeText(UGUnit: TUGUnit): string;
begin
  Result:=ExtractFileName(UGUnit.Filename);
end;

{$R *.lfm}

end.

