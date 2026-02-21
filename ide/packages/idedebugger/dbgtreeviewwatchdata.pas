unit DbgTreeViewWatchData;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, Math, fgl, IdeDebuggerBase, DebuggerTreeView, IdeDebuggerWatchResult,
  ArrayNavigationFrame, BaseDebugManager, IdeDebuggerWatchResPrinter, Debugger, laz.VirtualTrees,
  DbgIntfDebuggerBase, IdeDebuggerWatchValueIntf, LazIDEIntf, ProjectIntf, Controls, Forms,
  LazDebuggerIntf, LazDebuggerIntfBaseTypes;

type

  TTreeViewDataScope       = (vdsFocus, vdsSelection, vdsSelectionOrFocus, vdsAll);
  TTreeViewDataToTextField = (vdfName, vdfDataAddress, vdfValue);
  TTreeViewDataToTextOption = (
    vdoUnQuoted, vdoAllowMultiLine
  );
  TTreeViewDataToTextFields = set of TTreeViewDataToTextField;
  TTreeViewDataToTextOptions = set of TTreeViewDataToTextOption;

  { TDbgTreeViewTrackedData }

  TDbgTreeViewTrackedData = record
    Expanded: Boolean;
    class operator =(a,b: TDbgTreeViewTrackedData): boolean;
  end;

  TDbgTreeViewTrackedDataMap = specialize TFPGMap<string, TDbgTreeViewTrackedData>;

  { TDbgTreeViewWatchDataMgr }

  TDbgTreeViewWatchDataMgr = class
  private
    FCancelUpdate: Boolean;
    FDisplayFormatResolver: TDisplayFormatResolver;
    FTreeView: TDbgTreeView;
    FDbgTreeViewTrackedDataMap: TDbgTreeViewTrackedDataMap;
    FCurrentTrackId: String;
    FExpandingWatchAbleResult: TObject;

    function DoProjectChanged(Sender: TObject; AProject: TLazProject): TModalResult;
    function GetDbgTreeViewTrackedData(AWatchAble: TObject; out AData: TDbgTreeViewTrackedData): Boolean;
    function GetDbgTreeViewTrackedData(AWatchAble: TObject; out AnIndex: Integer): TDbgTreeViewTrackedData;
    procedure UpdateDbgTreeViewTrackedData(AnIndex: Integer; AData: TDbgTreeViewTrackedData);

    procedure TreeNodeRightButtonClicked(Sender: TDbgTreeView; ANode: PVirtualNode);
    procedure TreeViewCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewExpanded(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeViewInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);

    procedure DoItemRemovedFromView(Sender: TDbgTreeView; AWatchAble: TObject; ANode: PVirtualNode);
    procedure DoWatchAbleFreed(Sender: TObject);
    procedure WatchNavChanged(Sender: TArrayNavigationBar; AValue: Int64);
  protected
    function  WatchAbleResultFromNode(AVNode: PVirtualNode): IWatchAbleResultIntf; virtual; abstract;
    function  WatchAbleResultFromObject(AWatchAble: TObject): IWatchAbleResultIntf; virtual; abstract;
    function  GetTrackingIdFor(AWatchAble: TObject): string; virtual;
    function  HasWatchAbleForTrackingId(ATrackingId: string): boolean; virtual;

    function GetFieldAsText(Nd: PVirtualNode;
      AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; //AVNode: PVirtualNode
      AField: TTreeViewDataToTextField;
      AnOpts: TTreeViewDataToTextOptions): String; virtual;
    procedure UpdateColumnsText(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode); virtual; abstract;
    procedure ConfigureNewSubItem(AWatchAble: TObject); virtual;

    procedure UpdateSubItemsLocked(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out ChildCount: LongWord); virtual;
    procedure UpdateSubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out ChildCount: LongWord); virtual;
    procedure DoUpdateArraySubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out ChildCount: LongWord);
    procedure DoUpdateStructSubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out ChildCount: LongWord);
    procedure DoUpdateOldSubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out ChildCount: LongWord);
  public
    constructor Create(ATreeView: TDbgTreeView);
    destructor Destroy; override;

    function AddWatchData(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf = nil; AVNode: PVirtualNode = nil): PVirtualNode;
    procedure UpdateWatchData(AWatchAble: TObject; AVNode: PVirtualNode; AWatchAbleResult: IWatchAbleResultIntf = nil; AnIgnoreNodeVisible: Boolean = False);

    procedure ClearTrackingIdData;
    procedure ClearOutdatedTrackingIdData;

    function GetAsText(AScope: TTreeViewDataScope;
      AFields: TTreeViewDataToTextFields;
      AnOpts: TTreeViewDataToTextOptions): String;

    property DisplayFormatResolver: TDisplayFormatResolver read FDisplayFormatResolver write FDisplayFormatResolver;
    property CancelUpdate: Boolean read FCancelUpdate write FCancelUpdate;
    property TreeView: TDbgTreeView read FTreeView;
  end;

implementation

{ TDbgTreeViewTrackedData }

class operator TDbgTreeViewTrackedData. = (a, b: TDbgTreeViewTrackedData): boolean;
begin
  Result := (a.Expanded = b.Expanded);
end;

{ TDbgTreeViewWatchDataMgr }

procedure TDbgTreeViewWatchDataMgr.DoWatchAbleFreed(Sender: TObject);
var
  VNode: PVirtualNode;
  Nav: TControl;
begin
  VNode := FTreeView.FindNodeForItem(Sender);
  if VNode = nil then
    exit;

  FTreeView.OnItemRemoved := nil;
  FTreeView.NodeItem[VNode] := nil;

  Nav := FTreeView.NodeControl[VNode];
  if (Nav <> nil) and (Nav is TArrayNavigationBar) then
    TArrayNavigationBar(Nav).OwnerData := nil;

  FTreeView.OnItemRemoved := @DoItemRemovedFromView;
end;

procedure TDbgTreeViewWatchDataMgr.WatchNavChanged(Sender: TArrayNavigationBar;
  AValue: Int64);
var
  VNode: PVirtualNode;
  AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf;
  c: LongWord;
begin
  if Sender.OwnerData = nil then
    exit;

  AWatchAble :=  TObject(Sender.OwnerData);
  AWatchAbleResult :=  WatchAbleResultFromObject(AWatchAble);
  if (AWatchAbleResult <> nil) and AWatchAbleResult.Enabled and
     (AWatchAbleResult.Validity = ddsValid)
  then begin
    VNode := FTreeView.FindNodeForItem(AWatchAble);
    if VNode = nil then
      exit;

    UpdateSubItems(AWatchAble, AWatchAbleResult, VNode, c);
  end;
end;

function TDbgTreeViewWatchDataMgr.GetTrackingIdFor(AWatchAble: TObject): string;
begin
  Result := '';
end;

function TDbgTreeViewWatchDataMgr.HasWatchAbleForTrackingId(ATrackingId: string): boolean;
begin
  Result := True;
end;

function TDbgTreeViewWatchDataMgr.GetFieldAsText(Nd: PVirtualNode;
  AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
  AField: TTreeViewDataToTextField; AnOpts: TTreeViewDataToTextOptions): String;
begin
  Result := '';
  case AField of
    vdfName:        Result := TreeView.NodeText[Nd, 0];
    vdfDataAddress: Result := TreeView.NodeText[Nd, 1];
    vdfValue:       Result := TreeView.NodeText[Nd, 2];
  end;
end;

procedure TDbgTreeViewWatchDataMgr.ConfigureNewSubItem(AWatchAble: TObject);
begin
  //
end;

procedure TDbgTreeViewWatchDataMgr.UpdateSubItemsLocked(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
begin
  UpdateSubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount);
end;

procedure TDbgTreeViewWatchDataMgr.UpdateSubItems(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
var
  ResData: TWatchResultData;
begin
  ChildCount := 0;
  if (AWatchAble <> nil) or (AWatchAbleResult = nil) then
    AWatchAbleResult := WatchAbleResultFromObject(AWatchAble);
  if (AWatchAble = nil) or (AWatchAbleResult = nil) then begin
    FTreeView.ChildCount[AVNode] := 0;
    exit;
  end;

  FTreeView.BeginUpdate;
  try
    ResData := AWatchAbleResult.ResultData;
    while (ResData <> nil) and (ResData.ValueKind = rdkPointerVal) do
      ResData := ResData.DerefData;

    if (ResData <> nil) and
       (ResData.FieldCount > 0) and
       (ResData.ValueKind <> rdkConvertRes)
    then
      DoUpdateStructSubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount)
    else
    if (ResData <> nil) and
       //(ResData.ValueKind = rdkArray) and
       (ResData.ArrayLength > 0)
    then
      DoUpdateArraySubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount)
    else
    if (AWatchAbleResult.TypeInfo <> nil) and (AWatchAbleResult.TypeInfo.Fields <> nil) then
      // Old Interface
      DoUpdateOldSubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount);

    FTreeView.ChildCount[AVNode] := ChildCount;
  finally
    FTreeView.EndUpdate;
  end;
  FTreeView.Invalidate;
end;

procedure TDbgTreeViewWatchDataMgr.DoUpdateArraySubItems(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
var
  NewWatchAble: TObject;
  i, TotalCount, DerefCount: Integer;
  ResData: TWatchResultData;
  ExistingNode, nd: PVirtualNode;
  Nav: TArrayNavigationBar;
  Offs, KeepCnt, KeepBelow: Int64;
  ForceIdx: Boolean;
  intfArrayNav: IArrayNavSettings;
begin
  ChildCount := 0;
  ResData := AWatchAbleResult.ResultData;
  DerefCount := 0;
  while (ResData <> nil) and (ResData.ValueKind = rdkPointerVal) do begin
    ResData := ResData.DerefData;
    inc(DerefCount);
  end;
  if (ResData = nil) then
    exit;

  TotalCount := ResData.ArrayLength;
  if (ResData.ValueKind <> rdkArray) or (TotalCount = 0) then
    TotalCount := ResData.Count;

  Nav := TArrayNavigationBar(FTreeView.NodeControl[AVNode]);
  if Nav = nil then begin
    Nav := TArrayNavigationBar.Create(nil, FTreeView, AVNode);
    Nav.ParentColor := False;
    Nav.ParentBackground := False;
    Nav.Color := FTreeView.Colors.BackGroundColor;
    Nav.LowBound := ResData.LowBound;
    Nav.HighBound := ResData.LowBound + TotalCount - 1;
    Nav.ShowBoundInfo := True;
    Nav.Index := ResData.LowBound;
    Nav.PageSize := 10;
    Nav.OnIndexChanged := @WatchNavChanged;
    Nav.OnPageSize := @WatchNavChanged;
    Nav.HardLimits := not(ResData.ValueKind = rdkArray);
    FTreeView.NodeControl[AVNode] := Nav;
    FTreeView.NodeControlHeight[AVNode] := Nav.PreferredHeight;
  end
  else begin
    ForceIdx := (Nav.LowBound <> ResData.LowBound) or
                ( (Nav.Index <= Nav.HighBound) and
                  (Nav.Index >  ResData.LowBound + TotalCount - 1)
                );
    Nav.LowBound := ResData.LowBound;
    Nav.HighBound := ResData.LowBound + TotalCount - 1;
    if ForceIdx then
      Nav.Index := ResData.LowBound;
    Nav.HardLimits := not(ResData.ValueKind = rdkArray);
  end;
  FTreeView.NodeControlVisible[AVNode] := True;
  Nav.OwnerData := AWatchAble;
  Nav.DisplayFormatResolver := FDisplayFormatResolver;
  AWatchAble.GetInterface(IArrayNavSettings, intfArrayNav);
  Nav.ArrayNavConfig := intfArrayNav;
  Nav.UpdateForNewBounds;
  ChildCount := Nav.LimitedPageSize;

  if ChildCount > 0 then begin
    ExistingNode := FTreeView.GetFirstChildNoInit(AVNode);
    if ExistingNode = nil then
      ExistingNode := FTreeView.AddChild(AVNode, nil)
    else
      FTreeView.NodeItem[ExistingNode] := nil;
  end;

  Offs := Nav.Index;
  for i := 0 to ChildCount - 1 do begin
    NewWatchAble := AWatchAbleResult.ChildrenByNameAsArrayEntry[Offs +  i, DerefCount];
    if NewWatchAble = nil then begin
      dec(ChildCount);
      continue;
    end;

    ConfigureNewSubItem(NewWatchAble);

    if ExistingNode <> nil then begin
      FTreeView.NodeItem[ExistingNode] := NewWatchAble;
      nd := ExistingNode;
      ExistingNode := FTreeView.GetNextSiblingNoInit(ExistingNode);
    end
    else begin
      nd := FTreeView.AddChild(AVNode, NewWatchAble);
    end;
    (NewWatchAble as IFreeNotifyingIntf).AddFreeNotification(@DoWatchAbleFreed);
    UpdateWatchData(NewWatchAble, nd, nil, True);
  end;

  KeepCnt := Nav.PageSize;
  KeepBelow := KeepCnt;
  KeepCnt := max(max(50, KeepCnt+10),
           Min(KeepCnt*10, 500) );
  KeepBelow := Min(KeepBelow, KeepCnt - Nav.PageSize);
  (AWatchAble as IWatchAbleDataIntf).LimitChildWatchCount(KeepCnt, ResData.LowBound + KeepBelow);
end;

procedure TDbgTreeViewWatchDataMgr.DoUpdateStructSubItems(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
var
  ResData: TWatchResultData;
  ExistingNode, nd: PVirtualNode;
  AnchClass: String;
  NewWatchAble: TObject;
  ChildInfo: TWatchResultDataFieldInfo;
  DerefCount: Integer;
begin
  ChildCount := 0;
  ResData := AWatchAbleResult.ResultData;
  DerefCount := 0;
  while (ResData <> nil) and (ResData.ValueKind = rdkPointerVal) do begin
    ResData := ResData.DerefData;
    inc(DerefCount);
  end;
  if ResData = nil then
    exit;

  ExistingNode := FTreeView.GetFirstChildNoInit(AVNode);

  AnchClass := '';
  if ResData.StructType <> dstRecord then
    AnchClass := ResData.TypeName;
  for ChildInfo in ResData do begin
    NewWatchAble := AWatchAbleResult.ChildrenByNameAsField[ChildInfo.FieldName, AnchClass, DerefCount];
    if NewWatchAble = nil then begin
      continue;
    end;
    inc(ChildCount);

    ConfigureNewSubItem(NewWatchAble);

    if ExistingNode <> nil then begin
      FTreeView.NodeItem[ExistingNode] := NewWatchAble;
      nd := ExistingNode;
      ExistingNode := FTreeView.GetNextSiblingNoInit(ExistingNode);
    end
    else begin
      nd := FTreeView.AddChild(AVNode, NewWatchAble);
    end;
    (NewWatchAble as IFreeNotifyingIntf).AddFreeNotification(@DoWatchAbleFreed);
    UpdateWatchData(NewWatchAble, nd, nil, True);
  end;
end;

procedure TDbgTreeViewWatchDataMgr.DoUpdateOldSubItems(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
var
  TypInfo: TDBGType;
  IsGdbmiArray: Boolean;
  ExistingNode, nd: PVirtualNode;
  AnchClass: String;
  i: Integer;
  NewWatchAble: TObject;
begin
  TypInfo := AWatchAbleResult.TypeInfo;

  if (TypInfo <> nil) and (TypInfo.Fields <> nil) then begin
    IsGdbmiArray := TypInfo.Attributes * [saDynArray, saArray] <> [];
    ChildCount := TypInfo.Fields.Count;
    ExistingNode := FTreeView.GetFirstChildNoInit(AVNode);

    AnchClass := TypInfo.TypeName;
    for i := 0 to TypInfo.Fields.Count-1 do begin
      if IsGdbmiArray then
        NewWatchAble := AWatchAbleResult.ChildrenByNameAsArrayEntry[StrToInt64Def(TypInfo.Fields[i].Name, 0), 0]
      else
        NewWatchAble := AWatchAbleResult.ChildrenByNameAsField[TypInfo.Fields[i].Name, AnchClass, 0];
      if NewWatchAble = nil then begin
        dec(ChildCount);
        continue;
      end;
      ConfigureNewSubItem(NewWatchAble);

      if ExistingNode <> nil then begin
        FTreeView.NodeItem[ExistingNode] := NewWatchAble;
        nd := ExistingNode;
        ExistingNode := FTreeView.GetNextSiblingNoInit(ExistingNode);
      end
      else begin
        nd := FTreeView.AddChild(AVNode, NewWatchAble);
      end;
      (NewWatchAble as IFreeNotifyingIntf).AddFreeNotification(@DoWatchAbleFreed);
      UpdateWatchData(NewWatchAble, nd, nil, True);
    end;
  end;
end;

procedure TDbgTreeViewWatchDataMgr.TreeViewExpanded(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Nav: TArrayNavigationBar;
  AWatchAble: TObject;
  i: Integer;
  TrckData: TDbgTreeViewTrackedData;
begin
  Nav := TArrayNavigationBar(FTreeView.NodeControl[Node]);
  if Nav <> nil then
    Nav.UpdateCollapsedExpanded;

  AWatchAble := FTreeView.NodeItem[Node];
  TrckData := GetDbgTreeViewTrackedData(AWatchAble, i);
  TrckData.Expanded := True;
  UpdateDbgTreeViewTrackedData(i, TrckData);

  Node := FTreeView.GetFirstChildNoInit(Node);
  while Node <> nil do begin
    AWatchAble := FTreeView.NodeItem[Node];
    if AWatchAble <> nil then
      UpdateWatchData(AWatchAble, Node);
    Node := FTreeView.GetNextSiblingNoInit(Node);
  end;
end;

procedure TDbgTreeViewWatchDataMgr.TreeViewCollapsed(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Nav: TArrayNavigationBar;
  AWatchAble: TObject;
  i: Integer;
  TrckData: TDbgTreeViewTrackedData;
begin
  FTreeView.NodeControlVisible[Node] := False;
  Nav := TArrayNavigationBar(FTreeView.NodeControl[Node]);
  if Nav <> nil then
    Nav.UpdateCollapsedExpanded;

  AWatchAble := FTreeView.NodeItem[Node];
  TrckData := GetDbgTreeViewTrackedData(AWatchAble, i);
  TrckData.Expanded := False;
  UpdateDbgTreeViewTrackedData(i, TrckData);
end;

function TDbgTreeViewWatchDataMgr.GetDbgTreeViewTrackedData(AWatchAble: TObject; out
  AData: TDbgTreeViewTrackedData): Boolean;
var
  i: Integer;
begin
  AData := GetDbgTreeViewTrackedData(AWatchAble, i);
  Result := i >= 0;
end;

function TDbgTreeViewWatchDataMgr.DoProjectChanged(Sender: TObject; AProject: TLazProject
  ): TModalResult;
begin
  ClearTrackingIdData;
  Result := mrOK;
end;

function TDbgTreeViewWatchDataMgr.GetDbgTreeViewTrackedData(AWatchAble: TObject; out
  AnIndex: Integer): TDbgTreeViewTrackedData;
begin
  Result := Default(TDbgTreeViewTrackedData);
  AnIndex := -1;
  FCurrentTrackId := GetTrackingIdFor(AWatchAble);
  if FCurrentTrackId <> '' then begin
    AnIndex := FDbgTreeViewTrackedDataMap.IndexOf(FCurrentTrackId);
    if AnIndex >= 0 then
      Result := FDbgTreeViewTrackedDataMap.Data[AnIndex];
  end;
end;

procedure TDbgTreeViewWatchDataMgr.UpdateDbgTreeViewTrackedData(AnIndex: Integer;
  AData: TDbgTreeViewTrackedData);
begin
  if AData = Default(TDbgTreeViewTrackedData) then begin
    if AnIndex >= 0 then
      FDbgTreeViewTrackedDataMap.Delete(AnIndex);
  end
  else
  if AnIndex >= 0 then
    FDbgTreeViewTrackedDataMap.Data[AnIndex] := AData
  else
    AnIndex := FDbgTreeViewTrackedDataMap.Add(FCurrentTrackId, AData);
end;

procedure TDbgTreeViewWatchDataMgr.TreeNodeRightButtonClicked(Sender: TDbgTreeView;
  ANode: PVirtualNode);
var
  Nav: TControl;
begin
  Nav := FTreeView.NodeControl[ANode];
  if Nav <> nil then TArrayNavigationBar(Nav).ShowNavBar;
end;

procedure TDbgTreeViewWatchDataMgr.TreeViewInitChildren(
  Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
var
  AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf;
begin
  ChildCount := 0;
  AWatchAble := FTreeView.NodeItem[Node];
  if (AWatchAble <> nil) then
    AWatchAbleResult := WatchAbleResultFromObject(AWatchAble);
  if (AWatchAble = nil) or (AWatchAbleResult = nil) then begin
    FTreeView.ChildCount[Node] := 0;
    exit;
  end;

  FExpandingWatchAbleResult := AWatchAble;
  FTreeView.BeginUpdate;
  UpdateSubItemsLocked(AWatchAble, AWatchAbleResult, Node, ChildCount);
  FTreeView.EndUpdate;
  FExpandingWatchAbleResult := nil;
end;

procedure TDbgTreeViewWatchDataMgr.DoItemRemovedFromView(Sender: TDbgTreeView;
  AWatchAble: TObject; ANode: PVirtualNode);
begin
  if AWatchAble <> nil then
    with (AWatchAble as IWatchAbleDataIntf) do begin
      ClearDisplayData;
      RemoveFreeNotification(@DoWatchAbleFreed);
    end;
end;

constructor TDbgTreeViewWatchDataMgr.Create(ATreeView: TDbgTreeView);
begin
  FDbgTreeViewTrackedDataMap := TDbgTreeViewTrackedDataMap.Create;
  FTreeView := ATreeView;
  FTreeView.OnItemRemoved  := @DoItemRemovedFromView;
  FTreeView.OnExpanded     := @TreeViewExpanded;
  FTreeView.OnCollapsed    := @TreeViewCollapsed;
  FTreeView.OnInitChildren := @TreeViewInitChildren;
  FTreeView.OnNodeRightButtonClick:= @TreeNodeRightButtonClicked;
  LazarusIDE.AddHandlerOnProjectClose(@DoProjectChanged);
  LazarusIDE.AddHandlerOnProjectOpening(@DoProjectChanged);
end;

destructor TDbgTreeViewWatchDataMgr.Destroy;
begin
  LazarusIDE.RemoveHandlerOnProjectClose(@DoProjectChanged);
  LazarusIDE.RemoveHandlerOnProjectOpening(@DoProjectChanged);
  inherited Destroy;
  FDbgTreeViewTrackedDataMap.Destroy;
end;

function TDbgTreeViewWatchDataMgr.AddWatchData(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode): PVirtualNode;
begin
  if AWatchAble = nil then
    exit;

  FTreeView.BeginUpdate;
  try
    if (AVNode <> nil) then begin
      FTreeView.NodeItem[AVNode] := AWatchAble;
      FTreeView.SelectNode(AVNode);
      (AWatchAble as IFreeNotifyingIntf).AddFreeNotification(@DoWatchAbleFreed);
    end
    else begin
      AVNode := FTreeView.FindNodeForItem(AWatchAble);
      if AVNode = nil then begin
        AVNode := FTreeView.AddChild(nil, AWatchAble);
        FTreeView.SelectNode(AVNode);
        (AWatchAble as IFreeNotifyingIntf).AddFreeNotification(@DoWatchAbleFreed);
      end;
    end;
    Result := AVNode;

    UpdateWatchData(AWatchAble, AVNode, AWatchAbleResult);
  finally
    FTreeView.EndUpdate;
  end;
end;

procedure TDbgTreeViewWatchDataMgr.UpdateWatchData(AWatchAble: TObject;
  AVNode: PVirtualNode; AWatchAbleResult: IWatchAbleResultIntf;
  AnIgnoreNodeVisible: Boolean);
var
  TypInfo: TDBGType;
  ResData: TWatchResultData;
  HasChildren: Boolean;
  c: LongWord;
  TrckData: TDbgTreeViewTrackedData;
begin
  if not (FTreeView.FullyVisible[AVNode] or AnIgnoreNodeVisible) then
    exit;

  if AWatchAbleResult = nil then
    AWatchAbleResult := WatchAbleResultFromObject(AWatchAble);

  FTreeView.BeginUpdate;
  try
    UpdateColumnsText(AWatchAble, AWatchAbleResult, AVNode);
    FTreeView.Invalidate;

    if AWatchAbleResult = nil then
      exit;

    // some debuggers may have run Application.ProcessMessages
    if CancelUpdate then
      exit;

    (* If the watch is ddsRequested or ddsEvaluating => keep any expanded tree-nodes. (Avoid flicker)
       > ddsEvaluating includes "not HasAllValidParents"
       If the debugger is running => keey any expanded tree-nodes
       > ddsInvalid happens when the debugger changes from pause to run, and the watch had not been evaluated
    *)

    if (not(AWatchAbleResult.Validity in [ddsRequested, ddsEvaluating, ddsInvalid])) and
       ((DebugBoss = nil) or (DebugBoss.State <> dsRun))
    then begin
      TypInfo := AWatchAbleResult.TypeInfo;
      ResData := AWatchAbleResult.ResultData;
      while (ResData <> nil) and (ResData.ValueKind = rdkPointerVal) do
        ResData := ResData.DerefData;
      HasChildren := ( (TypInfo <> nil) and (TypInfo.Fields <> nil) and (TypInfo.Fields.Count > 0) ) or
                     ( (ResData <> nil) and
                       ( ( (ResData.FieldCount > 0) and (ResData.ValueKind <> rdkConvertRes) )
                         or
                         //( (ResData.ValueKind = rdkArray) and (ResData.ArrayLength > 0) )
                         (ResData.ArrayLength > 0)
                     ) );
      FTreeView.HasChildren[AVNode] := HasChildren;

      if HasChildren and FTreeView.Expanded[AVNode] then begin
        if (AWatchAbleResult.Validity = ddsValid) then begin
          (* The current "AWatchAbleResult" should be done. Allow UpdateItem for nested entries *)

          UpdateSubItems(AWatchAble, AWatchAbleResult, AVNode, c);
        end;
      end
      else begin
        if HasChildren and
          GetDbgTreeViewTrackedData(AWatchAble, TrckData) and
          TrckData.Expanded
        then begin
          FTreeView.Expanded[AVNode] := True;
        end
        else
        if AWatchAble <> FExpandingWatchAbleResult then begin
          FTreeView.DeleteChildren(AVNode, False);
          FTreeView.NodeControl[AVNode] := nil
        end;
      end;
    end
  finally
    FTreeView.EndUpdate;
  end;
end;

procedure TDbgTreeViewWatchDataMgr.ClearTrackingIdData;
begin
  FDbgTreeViewTrackedDataMap.Clear;
end;

procedure TDbgTreeViewWatchDataMgr.ClearOutdatedTrackingIdData;
var
  i: Integer;
begin
  i := FDbgTreeViewTrackedDataMap.Count - 1;
  while i >= 0 do begin
    if not HasWatchAbleForTrackingId(FDbgTreeViewTrackedDataMap.Keys[i]) then
      FDbgTreeViewTrackedDataMap.Delete(i);
    dec(i);
  end;
end;

function TDbgTreeViewWatchDataMgr.GetAsText(AScope: TTreeViewDataScope;
  AFields: TTreeViewDataToTextFields; AnOpts: TTreeViewDataToTextOptions
  ): String;

  function GetEntryText(Nd: PVirtualNode): String;
  var
    AWatchAbleResult: IWatchAbleResultIntf;
    AWatchAble: TObject;
    r: String;
  begin
    AWatchAble := TreeView.NodeItem[Nd];
    AWatchAbleResult := WatchAbleResultFromObject(AWatchAble);
    Result := '';
    if vdfName in AFields then
      Result := GetFieldAsText(Nd, AWatchAble, AWatchAbleResult, vdfName, AnOpts);
    if vdfDataAddress in AFields then begin
      r := GetFieldAsText(Nd, AWatchAble, AWatchAbleResult, vdfDataAddress, AnOpts);
      if r <> '' then begin
        if Result <> '' then
          Result := Result + ' ';
        if (AFields - [vdfDataAddress]) <> [] then
          Result := Result + '@';
        Result := Result + r;
      end;
    end;
    if vdfValue in AFields then begin
      r := GetFieldAsText(Nd, AWatchAble, AWatchAbleResult, vdfValue, AnOpts);
      if r <> '' then begin
        if Result <> '' then
          Result := Result + ' = ';
        Result := Result + r;
      end;
    end;
  end;

var
  Nd: PVirtualNode;
  Itr: TVTVirtualNodeEnumeration;
begin
  Result := '';
  if (AScope = vdsFocus) or
     ( (AScope = vdsSelectionOrFocus) and (TreeView.SelectedCount=0) )
  then begin
    Nd := TreeView.FocusedNode;
    Result := GetEntryText(Nd);
    exit;
  end;

  case AScope of
    vdsSelection, vdsSelectionOrFocus: Itr := TreeView.SelectedNodes;
    vdsAll: Itr := TreeView.NoInitNodes;
  end;
  for Nd in Itr do begin
    Result := Result + GetEntryText(Nd) + LineEnding;
  end;
end;

end.

