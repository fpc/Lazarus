{ $Id$ }
{               ----------------------------------------------
                 breakpointsdlg.pp  -  Overview of breakpoints
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the Breakpoint dialog.


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit BreakPointsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Dialogs, IDEWindowIntf,
  Menus, ComCtrls, Debugger, DebuggerDlg, ActnList, ExtCtrls, IDEImagesIntf,
  {$ifdef Windows} ActiveX, {$else} laz.FakeActiveX, {$endif}
  DbgIntfDebuggerBase, DbgIntfMiscClasses, BaseDebugManager, IdeDebuggerStringConstants,
  DebuggerTreeView, breakpointgroupframe, IdeDebuggerOpts, EnvDebuggerOptions, IdeIntfStrConsts,
  SrcEditorIntf, IDEDialogs, laz.VirtualTrees, LazDebuggerIntf;

type
  TBreakPointsDlgState = (
    bpdsItemsNeedUpdate,
    bpdsInEndUpdate
    );
  TBreakPointsDlgStates = set of TBreakPointsDlgState;

  { TBreakPointsDlg }

  TBreakPointsDlg = class(TBreakPointsDlgBase, IFPObserver)
    actAddSourceBP: TAction;
    actAddAddressBP: TAction;
    actAddWatchPoint: TAction;
    actGroupSetNone: TAction;
    actGroupSetNew: TAction;
    actShow: TAction;
    actProperties: TAction;
    actToggleCurrentEnable: TAction;
    actDeleteAllInSrc: TAction;
    actEnableSelected: TAction;
    actDisableSelected: TAction;
    actDeleteSelected: TAction;
    actEnableAll: TAction;
    actDisableAll: TAction;
    actDeleteAll: TAction;
    actEnableAllInSrc: TAction;
    actDisableAllInSrc: TAction;
    ActionList1: TActionList;
    tbGroupByBrkGroup: TToolButton;
    ToolButton1: TToolButton;
    tvBreakPoints: TDbgTreeView;
    popGroupSep: TMenuItem;
    popGroupSetNew: TMenuItem;
    popGroupSetNone: TMenuItem;
    popGroup: TMenuItem;
    popAddWatchPoint: TMenuItem;
    popAddAddressBP: TMenuItem;
    N0: TMenuItem;
    popShow: TMenuItem;
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    popAddSourceBP: TMenuItem;
    N1: TMenuItem; //--------------
    popProperties: TMenuItem;
    popEnabled: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem; //--------------
    popDisableAll: TMenuItem;
    popEnableAll: TMenuItem;
    popDeleteAll: TMenuItem;
    N3: TMenuItem; //--------------
    popDisableAllSameSource: TMenuItem;
    popEnableAllSameSource: TMenuItem;
    popDeleteAllSameSource: TMenuItem;
    ToolBar1: TToolBar;
    ToolButtonAdd: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolSep2: TToolButton;
    ToolButtonEnable: TToolButton;
    ToolButtonDisable: TToolButton;
    ToolButtonTrash: TToolButton;
    ToolSep1: TToolButton;
    ToolButtonEnableAll: TToolButton;
    ToolButtonDisableAll: TToolButton;
    ToolButtonTrashAll: TToolButton;
    procedure actAddAddressBPExecute(Sender: TObject);
    procedure actAddSourceBPExecute(Sender: TObject);
    procedure actAddWatchPointExecute(Sender: TObject);
    procedure actDisableSelectedExecute(Sender: TObject);
    procedure actEnableSelectedExecute(Sender: TObject);
    procedure actGroupSetNoneExecute(Sender: TObject);
    procedure actGroupSetNewExecute(Sender: TObject);
    procedure actShowExecute(Sender: TObject);
    procedure BreakpointsDlgCREATE(Sender: TObject);
    procedure mnuPopupPopup(Sender: TObject);
    procedure popDeleteAllSameSourceCLICK(Sender: TObject);
    procedure popDisableAllSameSourceCLICK(Sender: TObject);
    procedure popEnableAllSameSourceCLICK(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
    procedure tbGroupByBrkGroupClick(Sender: TObject);
    procedure tvBreakPointsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvBreakPointsCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure tvBreakPointsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure tvBreakPointsDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure tvBreakPointsStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure tvBreakPointsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tvBreakPointsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure tvBreakPointsHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure tvBreakPointsNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
  private
    FSecondarySortCol, FCurrentSortCol: TColumnIndex;
    FSecondarySortDir: TSortDirection;
    FBaseDirectory: string;
    FStates: TBreakPointsDlgStates;
    FUngroupedHeader, FAddGroupedHeader: TBreakpointGroupFrame;
    FDragSource: Boolean;
    FLastTargetHeader: TBreakpointGroupFrame;

    function GetDropTargetGroup(ANode: PVirtualNode): TBreakpointGroupFrame;
    procedure DoDetermineDropMode(const P: TPoint; var HitInfo: THitInfo;
      var NodeRect: TRect; var DropMode: TDropMode);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BreakPointAdd(const {%H-}ASender: TIDEBreakPoints;
                            const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointUpdate(const ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointRemove(const {%H-}ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure DoGroupDeleteBtnClicked(Sender: TBreakpointGroupFrame;
      BrkGroup: TIDEBreakPointGroup);
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
    procedure ClearTree;
    procedure DoItemRemoved(Sender: TDbgTreeView; AnItem: TObject;
      ANode: PVirtualNode);
    function  GetNodeForBrkGroup(const ABrkGroup: TIDEBreakPointGroup; AOnlyExisting: Boolean = False): PVirtualNode;
    procedure SetBaseDirectory(const AValue: string);
    procedure popSetGroupItemClick(Sender: TObject);
    procedure SetGroup(const NewGroup: TIDEBreakPointGroup);

    function  FindParentNode(const ABreakpoint: TIDEBreakPoint): PVirtualNode;
    procedure ChangeParentNode(const ANode, ANewParentNode: PVirtualNode);
    function  GetGroupFrame(const ANode: PVirtualNode): TBreakpointGroupFrame; inline;

    procedure UpdateItem(const AVNode: PVirtualNode;
                         const ABreakpoint: TIDEBreakPoint);
    procedure UpdateAll;
    
    procedure DeleteSelectedBreakpoints;
    procedure JumpToCurrentBreakPoint;
    procedure ShowProperties;
  protected
    procedure AcceptGroupHeaderDrop(ADroppedGroupFrame: TBreakpointGroupFrame; ATargetNode: PVirtualNode); override;
    procedure DoBreakPointsChanged; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DisableAllActions;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property BreakPoints;
  end;
  
function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;


implementation

{$R *.lfm}

var
  BreakPointDlgWindowCreator: TIDEWindowCreator;

const
  COL_BREAK_STATE     = 1;
  COL_BREAK_FILE      = 2;
  COL_BREAK_LINE      = 3;
  COL_BREAK_CONDITION = 4;
  COL_BREAK_ACTION    = 5;
  COL_BREAK_PASS      = 6;
  COL_BREAK_GROUP     = 7;
  COL_WIDTHS: Array[0..6] of integer = ( 60, 150, 100,  75, 150, 100, 80);

function BreakPointDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TBreakPointsDlg;
  if Result then
    Result := TBreakPointsDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure BreakPointDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TBreakPointsDlg then
    TBreakPointsDlg(AForm).ColSizeSetter(AColId, ASize);
end;

function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
var
  DEBUG_STATE: array[Boolean, TValidState] of ShortString;
begin
  DEBUG_STATE[false, vsUnknown]:=lisOff;
  DEBUG_STATE[false, vsValid]:=lisBPSDisabled;
  DEBUG_STATE[false, vsInvalid]:=lisInvalidOff;
  DEBUG_STATE[false, vsPending]:=lisInvalidOff;
  DEBUG_STATE[true, vsUnknown]:=lisOn;
  DEBUG_STATE[true, vsValid]:=lisBPSEnabled;
  DEBUG_STATE[true, vsInvalid]:=lisInvalidOn;
  DEBUG_STATE[true, vsPending]:=lisPendingOn;
  Result:=DEBUG_STATE[ABreakpoint.Enabled,ABreakpoint.Valid];
end;

function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;
var
  DEBUG_ACTION: array[TIDEBreakPointAction] of ShortString;
  CurBreakPoint: TIDEBreakPoint;
  Action: TIDEBreakPointAction;
begin
  Result := '';

  DEBUG_ACTION[bpaStop]:=lisBreak;
  DEBUG_ACTION[bpaEnableGroup]:=lisEnableGroups;
  DEBUG_ACTION[bpaDisableGroup]:=lisDisableGroups;
  DEBUG_ACTION[bpaLogMessage]:=lisLogMessage;
  DEBUG_ACTION[bpaEValExpression]:=lisLogEvalExpression;
  DEBUG_ACTION[bpaLogCallStack]:=lisLogCallStack;
  DEBUG_ACTION[bpaTakeSnapshot]:=lisTakeSnapshot;

  if ABreakpoint is TIDEBreakPoint then begin
    CurBreakPoint:=TIDEBreakPoint(ABreakpoint);
    for Action := Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
      if Action in CurBreakpoint.Actions
      then begin
        if Result <> '' then Result := Result + ', ';
        Result := Result + DEBUG_ACTION[Action]
      end;
  end;
end;

procedure TBreakPointsDlg.BreakPointAdd(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  VNode, p: PVirtualNode;
  g: TBreakpointGroupFrame;
begin
  BeginUpdate;
  try
    VNode := tvBreakPoints.FindNodeForItem(ABreakpoint);
    if VNode = nil
    then begin
      p := FindParentNode(ABreakpoint);
      VNode := tvBreakPoints.AddChild(p, ABreakpoint);
      g := GetGroupFrame(p);
      if g <> nil then g.UpdateDisplay;
      tvBreakPoints.SelectNode(VNode);
    end;

    UpdateItem(VNode, ABreakPoint);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.BreakPointUpdate(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  VNode: PVirtualNode;
begin
  if ABreakpoint = nil then Exit;

  BeginUpdate;
  try
    VNode := tvBreakPoints.FindNodeForItem(ABreakpoint);
    if VNode = nil
    then BreakPointAdd(ASender, ABreakPoint)
    else begin
      if UpdateCount>0 then begin
        Include(FStates,bpdsItemsNeedUpdate);
        exit;
      end;
      UpdateItem(VNode, ABreakPoint);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.BreakPointRemove(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  BeginUpdate;
  try
    tvBreakPoints.DeleteNode(tvBreakPoints.FindNodeForItem(ABreakpoint));
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DoGroupDeleteBtnClicked(
  Sender: TBreakpointGroupFrame; BrkGroup: TIDEBreakPointGroup);
var
  ConfirmDeleteDlg: TTaskDialog;
  DlgResult: TModalResult;
  i: Integer;
begin
  if BrkGroup.Count = 0 then begin
    BrkGroup.Free;
    exit;
  end;

  ConfirmDeleteDlg := TTaskDialog.Create(Self);
  try
    ConfirmDeleteDlg.Flags := [tfAllowDialogCancellation];
    ConfirmDeleteDlg.Caption := 'Delete breakpoint group';
    ConfirmDeleteDlg.Title := Format('The breakpoint group "%s" will be deleted.', [BrkGroup.Name]);
    ConfirmDeleteDlg.Text := 'Please choose what to do with the breakpoints in the group:';
    ConfirmDeleteDlg.MainIcon := tdiWarning;

    with ConfirmDeleteDlg.RadioButtons.Add do begin
      ModalResult := 1;
      Default := True;
      Caption := 'Keep Breakpoints';
    end;
    with ConfirmDeleteDlg.RadioButtons.Add do begin
      ModalResult := 2;
      Caption := 'Delete Breakpoints';
    end;

    if not ConfirmDeleteDlg.Execute() then
       exit;
    DlgResult := ConfirmDeleteDlg.ModalResult;
    if (DlgResult = mrOK) and (nil <> ConfirmDeleteDlg.RadioButton) then begin
      if ConfirmDeleteDlg.RadioButton.ModalResult = 2 then begin
        for i := BrkGroup.Count - 1 downto 0 do
          BrkGroup.Breakpoints[i].ReleaseReference;
      end;
      BrkGroup.Free;
    end;
  finally
    ConfirmDeleteDlg.free
  end;
end;

procedure TBreakPointsDlg.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  GrpHeader: TBreakpointGroupFrame;
  VNode: PVirtualNode;
begin
  if ASender <> DebugBoss.BreakPointGroups then
    exit;
  BeginUpdate;
  try
    case Operation of
      ooChange: begin
          if Data = nil then begin // several items where updated
            UpdateAll;
          end
          else begin
            GrpHeader := GetGroupFrame(GetNodeForBrkGroup(TIDEBreakPointGroup(Data)));
            if GrpHeader <> nil then begin
              GrpHeader.UpdateDisplay;
              tvBreakPoints.Invalidate;
            end;
          end;
        end;
      ooFree: ;
      ooAddItem: begin
          GetNodeForBrkGroup(TIDEBreakPointGroup(Data));
        end;
      ooDeleteItem: begin
          VNode := GetNodeForBrkGroup(TIDEBreakPointGroup(Data), True);
          if VNode <> nil then begin
            tvBreakPoints.DeleteNode(VNode);
          end;
        end;
      ooCustom: ;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.ClearTree;
var
  PVNode: PVirtualNode;
begin
  BeginUpdate;
  try
    tvBreakPoints.Clear;

    PVNode := tvBreakPoints.AddChild(nil, nil);
    FUngroupedHeader := TBreakpointGroupFrame.Create(Self, tvBreakPoints, PVNode, nil);
    FUngroupedHeader.NodeVisible:= tbGroupByBrkGroup.Down;
    tvBreakPoints.NodeControl[PVNode] := FUngroupedHeader;

    PVNode := tvBreakPoints.AddChild(nil, nil);
    FAddGroupedHeader := TBreakpointGroupFrame.Create(Self, tvBreakPoints, PVNode, nil, bgfAddNewGroup);
    FAddGroupedHeader.NodeVisible := False;
    tvBreakPoints.NodeControl[PVNode] := FAddGroupedHeader;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DoItemRemoved(Sender: TDbgTreeView; AnItem: TObject;
  ANode: PVirtualNode);
begin
  tvBreakPointsChange(nil, nil);
end;

function TBreakPointsDlg.GetNodeForBrkGroup(
  const ABrkGroup: TIDEBreakPointGroup; AOnlyExisting: Boolean): PVirtualNode;
var
  PVNode: PVirtualNode;
  GrpHeader: TBreakpointGroupFrame;
begin
  if ABrkGroup = nil then
    exit(FUngroupedHeader.Node);

  for PVNode in tvBreakPoints.ControlNodes do
    if GetGroupFrame(PVNode).BrkGroup = ABrkGroup then
      exit(PVNode);

  if AOnlyExisting then
    exit(nil);

  tvBreakPoints.BeginUpdate;
  try
    Result := tvBreakPoints.AddChild(nil, nil);
    GrpHeader := TBreakpointGroupFrame.Create(Self, tvBreakPoints, Result, ABrkGroup);
    GrpHeader.NodeVisible := tbGroupByBrkGroup.Down;
    GrpHeader.OnDeleteGroup := @DoGroupDeleteBtnClicked;
    tvBreakPoints.NodeControl[Result] := GrpHeader;
  finally
    tvBreakPoints.EndUpdate;
  end;
end;

procedure TBreakPointsDlg.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  UpdateAll;
end;

procedure TBreakPointsDlg.SetGroup(const NewGroup: TIDEBreakPointGroup);
var
  VNode: PVirtualNode;
  Brk: TIDEBreakPoint;
  OldGroup: TIDEBreakPointGroup;
  OldGroups: TList;
  PrevChoice: TModalResult;
begin
  BeginUpdate;
  try
    PrevChoice := mrNone;
    OldGroups := TList.Create;
    try
      for VNode in tvBreakPoints.SelectedItemNodes do
      begin
        Brk := TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
        OldGroup := Brk.Group;
        Brk.Group := NewGroup;
        if (OldGroup <> nil) and (OldGroup.Count = 0) and (OldGroups.IndexOf(OldGroup) < 0) then
          OldGroups.Add(OldGroup);
      end;
    finally
      while OldGroups.Count > 0 do begin
        OldGroup := TIDEBreakPointGroup(OldGroups[0]);
        OldGroups.Delete(0);
        if not (PrevChoice in [mrYesToAll, mrNoToAll]) then
        begin
          if OldGroups.Count > 0 then
            PrevChoice := MessageDlg(Caption, Format(lisGroupEmptyDelete + lisGroupEmptyDeleteMore,
              [OldGroup.Name, LineEnding, OldGroups.Count]),
              mtConfirmation, mbYesNo + [mbYesToAll, mbNoToAll], 0)
          else
            PrevChoice := MessageDlg(Caption, Format(lisGroupEmptyDelete,
              [OldGroup.Name]), mtConfirmation, mbYesNo, 0);
        end;
        if PrevChoice in [mrYes, mrYesToAll] then
          OldGroup.Free;
      end;
      OldGroups.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TBreakPointsDlg.FindParentNode(const ABreakpoint: TIDEBreakPoint
  ): PVirtualNode;
var
  BrkGroup: TIDEBreakPointGroup;
  GrpHeader: TBreakpointGroupFrame;
begin
  Result := tvBreakPoints.RootNode;

  if tbGroupByBrkGroup.Down then begin
    BrkGroup := ABreakpoint.Group;
    Result := GetNodeForBrkGroup(BrkGroup);
  end;
end;

procedure TBreakPointsDlg.ChangeParentNode(const ANode,
  ANewParentNode: PVirtualNode);
var
  CurParent: PVirtualNode;
  f: TBreakpointGroupFrame;
begin
  CurParent := tvBreakPoints.NodeParent[ANode];
  if CurParent = ANewParentNode then
    exit;
  tvBreakPoints.NodeParent[ANode] := ANewParentNode;

  f := GetGroupFrame(CurParent);
  if f <> nil then f.UpdateDisplay;
  f := GetGroupFrame(ANewParentNode);
  if f <> nil then f.UpdateDisplay;
end;

function TBreakPointsDlg.GetGroupFrame(const ANode: PVirtualNode
  ): TBreakpointGroupFrame;
begin
  if ANode = nil then
    exit(nil);
  Result := TBreakpointGroupFrame(tvBreakPoints.NodeControl[ANode]);
  assert((Result = nil) or (TObject(Result) is TBreakpointGroupFrame), 'TBreakPointsDlg.GetGroupFrame: (Result = nil) or (TObject(Result) is TBreakpointGroupFrame)');
end;

constructor TBreakPointsDlg.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  Name:='BreakPointsDlg';
  BreakpointsNotification.OnAdd    := @BreakPointAdd;
  BreakpointsNotification.OnUpdate := @BreakPointUpdate;
  BreakpointsNotification.OnRemove := @BreakPointRemove;
  tvBreakPoints.OnItemRemoved := @DoItemRemoved;
  tvBreakPoints.OnDetermineDropMode := @DoDetermineDropMode;

  DebugBoss.BreakPointGroups.FPOAttachObserver(Self);

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;
  mnuPopup.Images := IDEImages.Images_16;
  tvBreakPoints.Images := IDEImages.Images_16;

  ToolButtonAdd.ImageIndex := IDEImages.LoadImage('laz_add');

  actEnableSelected.Caption := lisDbgItemEnable;
  actEnableSelected.Hint    := lisDbgItemEnableHint;
  actEnableSelected.ImageIndex := IDEImages.LoadImage('debugger_enable');

  actDisableSelected.Caption := lisDbgItemDisable;
  actDisableSelected.Hint    := lisDbgItemDisableHint;
  actDisableSelected.ImageIndex := IDEImages.LoadImage('debugger_disable');

  actDeleteSelected.Caption := lisBtnDelete;
  actDeleteSelected.Hint    := lisDbgItemDeleteHint;
  actDeleteSelected.ImageIndex := IDEImages.LoadImage('laz_delete');

  actEnableAll.Caption := lisEnableAll;
  actEnableAll.Hint    := lisDbgAllItemEnableHint;
  actEnableAll.ImageIndex := IDEImages.LoadImage('debugger_enable_all');

  actDisableAll.Caption := liswlDIsableAll;
  actDisableAll.Hint    := lisDbgAllItemDisableHint;
  actDisableAll.ImageIndex := IDEImages.LoadImage('debugger_disable_all');

  actDeleteAll.Caption := lisDeleteAll;
  actDeleteAll.Hint    := lisDbgAllItemDeleteHint;
  actDeleteAll.ImageIndex := IDEImages.LoadImage('delete_all_in_list');

  actProperties.Caption:= liswlProperties;
  actProperties.Hint := lisDbgBreakpointPropertiesHint;
  actProperties.ImageIndex := IDEImages.LoadImage('menu_environment_options');

  tbGroupByBrkGroup.ImageIndex := IDEImages.LoadImage('ttreeview');
  tbGroupByBrkGroup.Hint := lisDbgBreakpointGroupsHint;
  case DebuggerOptions.BreakpointsDialogShowTree of
    bstNone:     tbGroupByBrkGroup.Down := False;
    bstBrkGroup: tbGroupByBrkGroup.Down := True;
  end;

  actToggleCurrentEnable.Caption:= lisBtnEnabled;

  actEnableAllInSrc.Caption:= lisEnableAllInSameSource;
  actDisableAllInSrc.Caption:= lisDisableAllInSameSource;
  actDeleteAllInSrc.Caption:= lisDeleteAllInSameSource;
  for i := low(COL_WIDTHS) to high(COL_WIDTHS) do
    tvBreakPoints.Header.Columns[i].Width := COL_WIDTHS[i];

  ClearTree;
end;

destructor TBreakPointsDlg.Destroy;
begin
  if (DebugBoss <> nil) and (DebugBoss.BreakPointGroups <> nil) then
    DebugBoss.BreakPointGroups.FPODetachObserver(Self);
  if FLastTargetHeader <> nil then
    FLastTargetHeader.RemoveFreeNotification(Self);
  inherited Destroy;
end;

procedure TBreakPointsDlg.BreakpointsDlgCREATE(Sender: TObject);
begin
  Caption:= lisMenuViewBreakPoints;
  tvBreakPoints.Header.Columns[0].Text:= lisBrkPointState;
  tvBreakPoints.Header.Columns[1].Text:= lisFilenameAddress;
  tvBreakPoints.Header.Columns[2].Text:= lisLineLength;
  tvBreakPoints.Header.Columns[3].Text:= lisCondition;
  tvBreakPoints.Header.Columns[4].Text:= lisBrkPointAction;
  tvBreakPoints.Header.Columns[5].Text:= lisPassCount;
  tvBreakPoints.Header.Columns[6].Text:= lisGroup;
  actShow.Caption := lisViewSource;
  popAdd.Caption:= lisAdd;
  actAddSourceBP.Caption := lisSourceBreakpoint;
  actAddAddressBP.Caption := lisAddressBreakpoint;
  actAddWatchPoint.Caption := lisWatchPoint;
  popGroup.Caption := lisGroup;
  actGroupSetNew.Caption := lisGroupSetNew;
  actGroupSetNone.Caption := lisGroupSetNone;
end;

procedure TBreakPointsDlg.actEnableSelectedExecute(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  BeginUpdate;
  try
    for VNode in tvBreakPoints.SelectedItemNodes do
      TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]).Enabled := True;
  finally
    EndUpdate
  end;
end;

procedure TBreakPointsDlg.actGroupSetNewExecute(Sender: TObject);
var
  GroupName: String;
  NewGroup: TIDEBreakPointGroup;
begin
  BeginUpdate;
  try
    GroupName := '';
    if not InputQuery(Caption, lisGroupNameInput, GroupName) then Exit;
    if GroupName = '' then
    begin
      if MessageDlg(Caption, lisGroupNameEmptyClearInstead,
        mtConfirmation, mbYesNo, 0) = mrYes then Exit;
      NewGroup := nil;
    end
    else begin
      NewGroup := DebugBoss.BreakPointGroups.GetGroupByName(GroupName);
      if NewGroup = nil then
      begin
        if not TIDEBreakPointGroup.CheckName(GroupName) then
        begin
          MessageDlg(Caption, lisGroupNameInvalid, mtError, [mbOk], 0);
          Exit;
        end;
        NewGroup := TIDEBreakPointGroup(DebugBoss.BreakPointGroups.Add);
        try
          NewGroup.Name := GroupName;
        except
          NewGroup.Free;
          raise;
        end;
      end
      else if MessageDlg(Caption, Format(lisGroupAssignExisting,
          [GroupName]), mtConfirmation, mbYesNo, 0) <> mrYes
        then
          Exit;
    end;

    SetGroup(NewGroup);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.actGroupSetNoneExecute(Sender: TObject);
begin
  SetGroup(nil);
end;

procedure TBreakPointsDlg.popSetGroupItemClick(Sender: TObject);
var
  Group: TIDEBreakPointGroup;
begin
  Group := DebugBoss.BreakPointGroups.GetGroupByName((Sender as TMenuItem).Caption);
  if Group = nil then
    raise Exception.CreateFmt('Group %s not found', [(Sender as TMenuItem).Caption]);
  SetGroup(Group);
end;

procedure TBreakPointsDlg.actShowExecute(Sender: TObject);
begin
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.actDisableSelectedExecute(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  BeginUpdate;
  try
    for VNode in tvBreakPoints.SelectedItemNodes do
      TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]).Enabled := False;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.actAddSourceBPExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
  SrcEdit: TSourceEditorInterface;
begin
  BeginUpdate;
  try
    SrcEdit := SourceEditorManagerIntf.ActiveEditor;
    if SrcEdit <> nil then
      NewBreakpoint := BreakPoints.Add(SrcEdit.FileName, SrcEdit.CursorTextXY.Y, True)
    else
      NewBreakpoint := BreakPoints.Add('', 0, True);
    if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
      NewBreakpoint.EndUpdate;
      UpdateAll;
    end
    else
      ReleaseRefAndNil(NewBreakpoint);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.actAddWatchPointExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
begin
  BeginUpdate;
  try
    NewBreakpoint := BreakPoints.Add('', wpsGlobal, wpkWrite, True);
    if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
      NewBreakpoint.EndUpdate;
      UpdateAll;
    end
    else
      ReleaseRefAndNil(NewBreakpoint);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.actAddAddressBPExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
begin
  BeginUpdate;
  try
    NewBreakpoint := BreakPoints.Add(0, True);
    if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
      NewBreakpoint.EndUpdate;
      UpdateAll;
    end
    else
      ReleaseRefAndNil(NewBreakpoint);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.mnuPopupPopup(Sender: TObject);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  for i := popGroup.Count - 1 downto popGroup.IndexOf(popGroupSep) +1 do
    popGroup.Items[i].Free;
  for i := 0 to DebugBoss.BreakPointGroups.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(popGroup);
    MenuItem.Caption := DebugBoss.BreakPointGroups[i].Name;
    MenuItem.OnClick := @popSetGroupItemClick;
    popGroup.Add(MenuItem);
  end;
  popGroupSep.Visible := DebugBoss.BreakPointGroups.Count <> 0;
end;

procedure TBreakPointsDlg.popDeleteAllSameSourceCLICK(Sender: TObject);
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
  MsgResult: Integer;
  NotAgain: Boolean;
begin
  BeginUpdate;
  try
    VNode := tvBreakPoints.FocusedNode;
    if VNode = nil then
      exit;
    Filename:=TIDEBreakpoint(tvBreakPoints.NodeItem[VNode]).Source;

    if EnvironmentDebugOpts.ConfirmDeleteFileBreakPoints then begin
      MsgResult:=TaskDlg(lisDeleteAllBreakpoints, lisDeleteAllBreakpoints2, '', tdiQuestion,
                 [mbYes, mbNo], dbgDoNotShowThisMessageAgain, NotAgain);
      if MsgResult <> mrYes then
        exit;
      if NotAgain then
        EnvironmentDebugOpts.ConfirmDeleteFileBreakPoints:= False;
    end;

    for VNode in tvBreakPoints.NoInitItemNodes do
    begin
      CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then ReleaseRefAndNil(CurBreakPoint);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popDisableAllSameSourceCLICK(Sender: TObject);
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  BeginUpdate;
  try
    VNode := tvBreakPoints.FocusedNode;
    if VNode = nil then
      exit;
    Filename:=TIDEBreakpoint(tvBreakPoints.NodeItem[VNode]).Source;

    for VNode in tvBreakPoints.NoInitItemNodes do
    begin
      CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then CurBreakPoint.Enabled := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popEnableAllSameSourceCLICK(Sender: TObject);
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  BeginUpdate;
  try
    VNode := tvBreakPoints.FocusedNode;
    if VNode = nil then
      exit;
    Filename:=TIDEBreakpoint(tvBreakPoints.NodeItem[VNode]).Source;

    for VNode in tvBreakPoints.NoInitItemNodes do
    begin
      CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then CurBreakPoint.Enabled := True;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popDeleteAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  MsgResult: Integer;
  NotAgain: Boolean;
begin
  if EnvironmentDebugOpts.ConfirmDeleteAllBreakPoints then begin
    MsgResult:=TaskDlg(lisDeleteAllBreakpoints, lisDeleteAllBreakpoints, '', tdiQuestion,
               [mbYes, mbNo], dbgDoNotShowThisMessageAgain, NotAgain);
    if MsgResult <> mrYes then
      exit;
    if NotAgain then
      EnvironmentDebugOpts.ConfirmDeleteAllBreakPoints := False;
  end;

  BeginUpdate;
  try
    for VNode in tvBreakPoints.NoInitItemNodes do
    begin
      CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
      CurBreakPoint.ReleaseReference;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.tbGroupByBrkGroupClick(Sender: TObject);
begin
  UpdateAll;

  case tbGroupByBrkGroup.Down of
    True:  DebuggerOptions.BreakpointsDialogShowTree := bstBrkGroup;
    False: DebuggerOptions.BreakpointsDialogShowTree := bstNone;
  end;
  DebuggerOptions.Save;
end;

procedure TBreakPointsDlg.tvBreakPointsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  VNode: PVirtualNode;
  ItemSelected: Boolean;
  SelCanEnable, SelCanDisable: Boolean;
  AllCanEnable, AllCanDisable: Boolean;
  CurBreakPoint: TIDEBreakPoint;
  TotalCnt: Integer;
  g: TBreakpointGroupFrame;
begin
  if UpdateCount > 0 then exit;

  SelCanEnable := False;
  SelCanDisable := False;
  AllCanEnable := False;
  allCanDisable := False;
  TotalCnt := 0;
  for VNode in tvBreakPoints.NoInitItemNodes do begin
    CurBreakPoint := TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
    if CurBreakPoint = nil then
      continue;
    inc(TotalCnt);
    if tvBreakPoints.Selected[VNode] then begin
      SelCanEnable  := SelCanEnable  or not CurBreakPoint.Enabled;
      SelCanDisable := SelCanDisable or CurBreakPoint.Enabled;
    end;
    AllCanEnable  := AllCanEnable  or not CurBreakPoint.Enabled;
    AllCanDisable := AllCanDisable or CurBreakPoint.Enabled;
  end;

  CurBreakPoint := TIDEBreakPoint(tvBreakPoints.FocusedItem);
  ItemSelected := CurBreakPoint <> nil;

  actToggleCurrentEnable.Enabled := ItemSelected;
  actToggleCurrentEnable.Checked := (CurBreakPoint <> nil) and CurBreakPoint.Enabled;

  actEnableSelected.Enabled := SelCanEnable;
  actDisableSelected.Enabled := SelCanDisable;
  actDeleteSelected.Enabled := ItemSelected;

  actEnableAll.Enabled := AllCanEnable;
  actDisableAll.Enabled := AllCanDisable;
  actDeleteAll.Enabled := TotalCnt > 0;

  actEnableAllInSrc.Enabled := ItemSelected;
  actDisableAllInSrc.Enabled := ItemSelected;
  actDeleteAllInSrc.Enabled := ItemSelected;

  actProperties.Enabled := ItemSelected;
  actShow.Enabled := ItemSelected;

  popGroup.Enabled := ItemSelected;
  actGroupSetNew.Enabled := ItemSelected;
  actGroupSetNone.Enabled := ItemSelected;

    for VNode in tvBreakPoints.ControlNodes do begin
      g := GetGroupFrame(VNode);
      if g <> nil then
        g.UpdateDisplay;
    end;

  tvBreakPoints.Invalidate;
end;

procedure TBreakPointsDlg.tvBreakPointsCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  c1, c2: TBreakpointGroupFrame;
  b1, b2: TIDEBreakPoint;
  Desc: integer;
begin
  Result := 0;
  if tvBreakPoints.Header.SortDirection = sdDescending then
    Desc := -1
  else
    Desc := 1;

  c1 := GetGroupFrame(Node1);
  c2 := GetGroupFrame(Node2);

  if (c1 <> nil) and (c2 <> nil) then begin
    Result := Desc * c1.Compare(c2);
    exit;
  end
  else
  if (c1 <> nil) and (c2 = nil) then begin
    Result := Desc;
    exit;
  end
  else
  if (c1 = nil) and (c2 <> nil) then begin
    Result := -Desc;
    exit;
  end;

  b1 := TIDEBreakPoint(tvBreakPoints.NodeItem[Node1]);
  b2 := TIDEBreakPoint(tvBreakPoints.NodeItem[Node2]);

  case Column of
    0: Result := (ord(b1.Enabled)*256 + ord(b1.Valid)) - (ord(b2.Enabled)*256 + ord(b2.Valid));
    1: begin
        if (b1.Kind = bpkAddress) and (b2.Kind = bpkAddress) then begin
          if b1.Address = b2.Address then Result := 0
          else
          if b1.Address > b2.Address then Result := 1
          else
          Result := -1;
        end
        else begin
          Result := ord(b1.Kind) - ord(b2.Kind);
          if Result = 0 then
            Result := CompareStr(tvBreakPoints.Text[Node1,1], tvBreakPoints.Text[Node2,1]);
        end;
      end;
    2: begin
        if (b1.Kind = bpkSource) and (b2.Kind = bpkSource) then begin
          Result := b1.Line - b2.Line;
        end
        else begin
          Result := CompareStr(tvBreakPoints.Text[Node1,1], tvBreakPoints.Text[Node2,1]);
        end;
      end;
    else
      if (Column >= 0) and (Column <= 6) then
        Result := CompareStr(tvBreakPoints.Text[Node1, Column], tvBreakPoints.Text[Node2, Column]);
  end;

  if (Result = 0) and (Column >= 0) and (FSecondarySortCol >= 0) and (FSecondarySortCol <> Column) then begin
    case FSecondarySortCol of
      0: Result := (ord(b1.Enabled)*256 + ord(b1.Valid)) - (ord(b2.Enabled)*256 + ord(b2.Valid));
      1: begin
          if (b1.Kind = bpkAddress) and (b2.Kind = bpkAddress) then begin
            if b1.Address = b2.Address then Result := 0
            else
            if b1.Address > b2.Address then Result := 1
            else
            Result := -1;
          end
          else begin
            Result := ord(b1.Kind) - ord(b2.Kind);
            if Result = 0 then
              Result := CompareStr(tvBreakPoints.Text[Node1,1], tvBreakPoints.Text[Node2,1]);
          end;
        end;
      2: begin
          if (b1.Kind = bpkSource) and (b2.Kind = bpkSource) then begin
            Result := b1.Line - b2.Line;
          end
          else begin
            Result := CompareStr(tvBreakPoints.Text[Node1,1], tvBreakPoints.Text[Node2,1]);
          end;
        end;
      else
        if (FSecondarySortCol >= 0) and (FSecondarySortCol <= 6) then
          Result := CompareStr(tvBreakPoints.Text[Node1, FSecondarySortCol], tvBreakPoints.Text[Node2, FSecondarySortCol]);
    end;
    if FSecondarySortDir = sdDescending then
      Result := -Result;
  end;

  if Result = 0 then
    Result := b1.Index - b2.Index;
end;

procedure TBreakPointsDlg.AcceptGroupHeaderDrop(
  ADroppedGroupFrame: TBreakpointGroupFrame; ATargetNode: PVirtualNode);
var
  TargetGroupFrame: TBreakpointGroupFrame;
  idx: Integer;
begin
  TargetGroupFrame := GetDropTargetGroup(ATargetNode);
  if (TargetGroupFrame = nil) or (TargetGroupFrame = ADroppedGroupFrame) then
    exit;

  if TargetGroupFrame.GroupKind <> bgfGroup then begin
    ADroppedGroupFrame.BrkGroup.Index := 0;
  end
  else begin
    if TargetGroupFrame.BrkGroup = nil then
      exit;

    idx := TargetGroupFrame.BrkGroup.Index;
    if ADroppedGroupFrame.BrkGroup.Index > idx then
      ADroppedGroupFrame.BrkGroup.Index := idx + 1
    else
      ADroppedGroupFrame.BrkGroup.Index := idx;
  end;
end;


procedure TBreakPointsDlg.tvBreakPointsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  TargetNd, N: PVirtualNode;
  TargetHeader: TBreakpointGroupFrame;
  Brk: TIDEBreakPoint;
  idx: Integer;
begin
  BeginUpdate;
  try
    TargetNd := tvBreakPoints.GetNodeAt(Pt);
    if (TargetNd <> nil) and (Source = tvBreakPoints) and (tvBreakPoints.SelectedCount > 0) then begin
      TargetHeader := GetDropTargetGroup(TargetNd);
      Brk := TIDEBreakPoint(tvBreakPoints.NodeItem[TargetNd]);
      if (tvBreakPoints.Header.SortColumn < 0) and (Brk <> nil) then begin
        idx := Brk.Index;
        //  inc(idx);
        for N in tvBreakPoints.SelectedItemNodes do begin
          Brk := TIDEBreakPoint(tvBreakPoints.NodeItem[N]);
          if Mode = dmAbove then begin
            if Brk.Index < idx then
              dec(idx);
            Brk.Index := idx;
            inc(idx);
          end
          else begin
            if Brk.Index > idx then
              inc(idx);
            Brk.Index := idx;
          end;
        end;
      end;

      if TargetHeader <> nil then
        TargetHeader.FrameDragDrop(Sender, Source, Pt.X, Pt.Y);
    end;

    if (Source is TToolBar) and (TToolBar(Source).Owner is TBreakpointGroupFrame) then begin
      AcceptGroupHeaderDrop(TBreakpointGroupFrame(TToolBar(Source).Owner), TargetNd);
    end;

    FAddGroupedHeader.NodeVisible:= False;
    FDragSource := False;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.tvBreakPointsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
var
  TargetNd: PVirtualNode;
  TargetHeader: TBreakpointGroupFrame;
  dummy: Boolean;
begin
  Accept := False;
  if FDragSource and tbGroupByBrkGroup.Down then
    FAddGroupedHeader.NodeVisible := True;

  TargetNd := tvBreakPoints.GetNodeAt(Pt);
  if (TargetNd <> nil) and (Source = tvBreakPoints) and (tvBreakPoints.SelectedCount > 0) then begin
    TargetHeader := GetDropTargetGroup(TargetNd);

    if (FLastTargetHeader <> nil) and (FLastTargetHeader <> TargetHeader) then begin
      FLastTargetHeader.FrameDragOver(Sender, Source, Pt.X, Pt.Y, dsDragLeave, dummy);
      FLastTargetHeader.RemoveFreeNotification(Self);
      FLastTargetHeader := nil;
    end;

    FLastTargetHeader := TargetHeader;
    if (TargetHeader <> nil) then begin
      FLastTargetHeader.FreeNotification(Self);
      TargetHeader.FrameDragOver(Sender, Source, Pt.X, Pt.Y, State, Accept);
    end;

    if tvBreakPoints.Header.SortColumn < 0 then
      Accept := True;
  end;

  if (State = dsDragLeave) and (FLastTargetHeader <> nil) then begin
    FLastTargetHeader.FrameDragOver(Sender, Source, Pt.X, Pt.Y, dsDragLeave, dummy);
    FLastTargetHeader.RemoveFreeNotification(Self);
    FLastTargetHeader := nil;
  end;

  if (Source is TToolBar) and (TToolBar(Source).Owner is TBreakpointGroupFrame) then begin
    TargetHeader := GetDropTargetGroup(TargetNd);
    Accept := (TToolBar(Source).Owner <> TargetHeader) and
              (TargetHeader.GroupKind in [bgfGroup, bgfUngrouped]);
  end;
end;

procedure TBreakPointsDlg.tvBreakPointsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragSource := True;
  FDraggingGroupHeader := False;
end;

procedure TBreakPointsDlg.tvBreakPointsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  FAddGroupedHeader.NodeVisible := False;
  FDragSource := False;
end;

function TBreakPointsDlg.GetDropTargetGroup(ANode: PVirtualNode
  ): TBreakpointGroupFrame;
begin
  Result := GetGroupFrame(ANode);
  if Result = nil then begin
    ANode := tvBreakPoints.NodeParent[ANode];
    if ANode <> nil then
      Result := GetGroupFrame(ANode);
  end;
end;

procedure TBreakPointsDlg.DoDetermineDropMode(const P: TPoint;
  var HitInfo: THitInfo; var NodeRect: TRect; var DropMode: TDropMode);
begin
  if (tvBreakPoints.Header.SortColumn >= 0) or
     (FDraggingGroupHeader)
  then
    DropMode := dmNowhere;

  if (HitInfo.HitNode <> nil) and (GetGroupFrame(HitInfo.HitNode) <> nil) then // header node
    DropMode := dmNowhere
end;

procedure TBreakPointsDlg.tvBreakPointsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  tvBreakPointsChange(nil, nil);
end;

procedure TBreakPointsDlg.tvBreakPointsHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  tvBreakPoints.BeginUpdate;
  try
    if tvBreakPoints.Header.SortColumn <> HitInfo.Column then begin
      if tvBreakPoints.Header.SortColumn >= 0 then begin
        FSecondarySortCol := tvBreakPoints.Header.SortColumn;
        FSecondarySortDir := tvBreakPoints.Header.SortDirection;
      end
      else
      if tvBreakPoints.Header.SortColumn <> FCurrentSortCol then begin
        FSecondarySortCol := -1;
      end;

      tvBreakPoints.Header.SortColumn := HitInfo.Column;
      tvBreakPoints.Header.SortDirection := sdAscending;
      FCurrentSortCol := HitInfo.Column;
    end
    else
    if tvBreakPoints.Header.SortDirection = sdAscending then
      tvBreakPoints.Header.SortDirection := sdDescending
    else
    begin
      tvBreakPoints.Header.SortColumn := -1;
      tvBreakPoints.Header.SortDirection := sdAscending;
    end;
  finally
    tvBreakPoints.EndUpdate;
  end;
end;

procedure TBreakPointsDlg.tvBreakPointsNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  tvBreakPointsChange(nil, nil);
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FLastTargetHeader) then
    FLastTargetHeader := nil;
end;

procedure TBreakPointsDlg.popDeleteClick(Sender: TObject);
begin
  BeginUpdate;
  try
    DeleteSelectedBreakpoints;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popDisableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  BeginUpdate;
  try
    for VNode in tvBreakPoints.NoInitItemNodes do
      TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]).Enabled := False;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popEnableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  BeginUpdate;
  try
    for VNode in tvBreakPoints.NoInitItemNodes do
      TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]).Enabled := True;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popEnabledClick(Sender: TObject);
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  Enable: Boolean;
begin
  BeginUpdate;
  try
    VNode := tvBreakPoints.FocusedNode;
    if VNode = nil then
      exit;

    CurBreakPoint:=TIDEBreakpoint(tvBreakPoints.NodeItem[VNode]);
    Enable := not CurBreakPoint.Enabled;

    if tvBreakPoints.SelectedCount > 1
    then begin
      for VNode in tvBreakPoints.SelectedItemNodes do
        TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]).Enabled := Enable;
    end
    else begin
      CurBreakPoint.Enabled:= Enable;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.popPropertiesClick(Sender: TObject);
begin
  BeginUpdate;
  try
    ShowProperties;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;

  if bpdsInEndUpdate in FStates then
    exit;

  Include(FStates, bpdsInEndUpdate);
  try
    if bpdsItemsNeedUpdate in FStates then
      UpdateAll; // calls begin/end udpate

    tvBreakPointsChange(nil, nil);
    tvBreakPoints.EndUpdate;
  finally
    Exclude(FStates, bpdsInEndUpdate);
  end;
end;

procedure TBreakPointsDlg.DisableAllActions;
var
  i: Integer;
begin
  for i := 0 to ActionList1.ActionCount - 1 do
    (ActionList1.Actions[i] as TAction).Enabled := False;
  actAddSourceBP.Enabled := True;
  actAddAddressBP.Enabled := True;
  actAddWatchPoint.Enabled := True;
end;

function TBreakPointsDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if (AColId - 1 >= 0) and (AColId - 1 < tvBreakPoints.Header.Columns.Count) then begin
    ASize := tvBreakPoints.Header.Columns[AColId - 1].Width;
    Result := ASize <> COL_WIDTHS[AColId - 1];
  end
  else
    Result := False;
end;

procedure TBreakPointsDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_BREAK_STATE:     tvBreakPoints.Header.Columns[0].Width := ASize;
    COL_BREAK_FILE:      tvBreakPoints.Header.Columns[1].Width := ASize;
    COL_BREAK_LINE:      tvBreakPoints.Header.Columns[2].Width := ASize;
    COL_BREAK_CONDITION: tvBreakPoints.Header.Columns[3].Width := ASize;
    COL_BREAK_ACTION:    tvBreakPoints.Header.Columns[4].Width := ASize;
    COL_BREAK_PASS:      tvBreakPoints.Header.Columns[5].Width := ASize;
    COL_BREAK_GROUP:     tvBreakPoints.Header.Columns[6].Width := ASize;
  end;
end;

procedure TBreakPointsDlg.UpdateItem(const AVNode: PVirtualNode;
  const ABreakpoint: TIDEBreakPoint);
var
  ParentVNode: PVirtualNode;
  s, Filename: String;
begin
  BeginUpdate;
  try
    ParentVNode := FindParentNode(ABreakpoint);
    ChangeParentNode(AVNode, ParentVNode);

    if tvBreakPoints.ChildCount[ParentVNode] = 1 then
      tvBreakPoints.Expanded[ParentVNode] := True;

    // state
    tvBreakPoints.NodeText[AVNode, 0] := GetBreakPointStateDescription(ABreakpoint);
    tvBreakPoints.NodeImageIndex[AVNode, 0] := GetBreakPointImageIndex(ABreakpoint);

    // filename/address
    case ABreakpoint.Kind of
      bpkSource:
        begin
          Filename:=ABreakpoint.Source;
          if BaseDirectory<>'' then
            Filename:=CreateRelativePath(Filename,BaseDirectory);
          tvBreakPoints.NodeText[AVNode, 1] := Filename;
          // line
          if ABreakpoint.Line > 0
          then tvBreakPoints.NodeText[AVNode, 2] := IntToStr(ABreakpoint.Line)
          else tvBreakPoints.NodeText[AVNode, 2] := '';
        end;
      bpkAddress:
        begin
          // todo: how to define digits count? 8 or 16 depends on gdb pointer size for platform
          tvBreakPoints.NodeText[AVNode, 1] := '$' + IntToHex(ABreakpoint.Address, 8);
        end;
      bpkData:
        begin
          tvBreakPoints.NodeText[AVNode, 1] := ABreakpoint.WatchData;
          case ABreakpoint.WatchScope of
            wpsGlobal: s:= lisWatchScopeGlobal;
            wpsLocal:  s:= lisWatchScopeLocal;
            else s := '';
          end;
          s := s +' / ';
          case ABreakpoint.WatchKind of
            wpkRead:      s := s + lisWatchKindRead;
            wpkReadWrite: s := s + lisWatchKindReadWrite;
            wpkWrite:     s := s + lisWatchKindWrite;
          end;
          tvBreakPoints.NodeText[AVNode, 2] := s;
        end;
    end;

    // expression
    tvBreakPoints.NodeText[AVNode, 3] := ABreakpoint.Expression;

    // actions
    tvBreakPoints.NodeText[AVNode, 4]  := GetBreakPointActionsDescription(ABreakpoint);

    // hitcount
    tvBreakPoints.NodeText[AVNode, 5] := IntToStr(ABreakpoint.HitCount);

    // group
    if ABreakpoint.Group = nil
    then tvBreakPoints.NodeText[AVNode, 6] := ''
    else tvBreakPoints.NodeText[AVNode, 6] := ABreakpoint.Group.Name;

  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.UpdateAll;
var
  VNode, LastAbandoned: PVirtualNode;
  GrpHeader: TBreakpointGroupFrame;
begin
  if UpdateCount>0 then begin
    Include(FStates,bpdsItemsNeedUpdate);
    exit;
  end;

  BeginUpdate;
  try
    Exclude(FStates,bpdsItemsNeedUpdate);

    LastAbandoned := nil;
    for VNode in tvBreakPoints.NoInitItemNodes(False, True) do begin
      GrpHeader := GetGroupFrame(VNode);
      if GrpHeader <> nil then begin
        if LastAbandoned <> nil then begin
          tvBreakPoints.DeleteNode(LastAbandoned);
          LastAbandoned := nil;
        end;
        if GrpHeader.GroupKind in [bgfGroup, bgfUngrouped] then
          GrpHeader.NodeVisible := tbGroupByBrkGroup.Down;
        if GrpHeader.GroupKind = bgfAbandoned then
          LastAbandoned := VNode;
      end
      else
        UpdateItem(VNode, TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]));
    end;
    if LastAbandoned <> nil then begin
      tvBreakPoints.DeleteNode(LastAbandoned);
      LastAbandoned := nil;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DeleteSelectedBreakpoints;
var
  VNode: PVirtualNode;
  CurBreakPoint: TIDEBreakPoint;
  Msg: String;
begin
  BeginUpdate;
  try
    CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.FocusedItem);
    if CurBreakPoint = nil then exit;

    if tvBreakPoints.SelectedCount <= 1 then
    begin
      case CurBreakPoint.Kind of
        bpkSource: Msg := Format(lisDeleteBreakpointAtLine,
                             [LineEnding, CurBreakPoint.Source, CurBreakPoint.Line]);
        bpkAddress: Msg := Format(lisDeleteBreakpointForAddress, ['$' + IntToHex(CurBreakPoint.Address, 8)]);
        bpkData: Msg := Format(lisDeleteBreakpointForWatch, [CurBreakPoint.WatchData]);
      end;
    end
    else
      Msg := lisDeleteAllSelectedBreakpoints;
    if MessageDlg(Msg, mtConfirmation, [mbYes,mbCancel],0) <> mrYes then exit;

    if tvBreakPoints.SelectedCount <= 1
    then begin
      CurBreakPoint.ReleaseReference;
    end
    else begin
      tvBreakPoints.BeginUpdate;
      try
        for VNode in tvBreakPoints.SelectedItemNodes do
        begin
          CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.NodeItem[VNode]);
          CurBreakPoint.ReleaseReference;
        end;
      finally
        tvBreakPoints.EndUpdate;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.JumpToCurrentBreakPoint;
var
  CurBreakPoint: TIDEBreakPoint;
begin
  CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.FocusedItem);
  if CurBreakPoint = nil then exit;

  if CurBreakPoint.Kind = bpkSource then
    DebugBoss.JumpToUnitSource(CurBreakPoint.Source, CurBreakPoint.Line, False);
end;

procedure TBreakPointsDlg.ShowProperties;
var
  CurBreakPoint: TIDEBreakPoint;
begin
  CurBreakPoint:=TIDEBreakPoint(tvBreakPoints.FocusedItem);
  if CurBreakPoint = nil then exit;

  BeginUpdate;
  try
    DebugBoss.ShowBreakPointProperties(CurBreakPoint);
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DoBreakPointsChanged;
var
  i: Integer;
  VNode: PVirtualNode;
begin
  BeginUpdate;
  try
    for VNode in tvBreakPoints.NoInitItemNodes(False, True) do
      if tvBreakPoints.NodeControl[VNode] = nil then
        tvBreakPoints.DeleteNode(VNode);

    if BreakPoints <> nil
    then begin
      for i:=0 to BreakPoints.Count-1 do
        BreakPointUpdate(BreakPoints, BreakPoints.Items[i]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TBreakPointsDlg.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
  if not (bpdsInEndUpdate in FStates) then begin
    tvBreakPoints.BeginUpdate;
    DisableAllActions;
  end;
end;

initialization

  BreakPointDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtBreakpoints]);
  BreakPointDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  BreakPointDlgWindowCreator.OnSetDividerSize := @BreakPointDlgColSizeSetter;
  BreakPointDlgWindowCreator.OnGetDividerSize := @BreakPointDlgColSizeGetter;
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakState',     COL_BREAK_STATE,     @drsColWidthState);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakFile',      COL_BREAK_FILE,      @drsBreakPointColWidthFile);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakLine',      COL_BREAK_LINE,      @drsBreakPointColWidthLine);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakCondition', COL_BREAK_CONDITION, @drsBreakPointColWidthCondition);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakAction',    COL_BREAK_ACTION,    @drsBreakPointColWidthAction);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakPassCnt',   COL_BREAK_PASS,      @drsBreakPointColWidthPassCount);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakGroup',     COL_BREAK_GROUP,     @drsBreakPointColWidthGroup);
  BreakPointDlgWindowCreator.CreateSimpleLayout;

end.

