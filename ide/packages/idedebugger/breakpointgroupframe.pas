unit BreakpointGroupFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  LCLType, Buttons, Graphics, Dialogs, laz.VirtualTrees, IDEImagesIntf,
  DebuggerTreeView, Debugger, IdeDebuggerStringConstants, BaseDebugManager,
  DebuggerDlg;

type

  TBreakpointGroupFrame = class;

  TBreakPointsDlgBase = class(TDebuggerDlg)
  protected
    FDraggingGroupHeader: Boolean;
    procedure AcceptGroupHeaderDrop(ADroppedGroupFrame: TBreakpointGroupFrame; ATargetNode: PVirtualNode); virtual; abstract;
  end;

  TOnDeleteGroup = procedure(Sender: TBreakpointGroupFrame; BrkGroup: TIDEBreakPointGroup) of object;

  TBreakpointGroupFrameKind = (bgfUngrouped, bgfGroup, bgfAddNewGroup, bgfAbandoned);

  { TBreakpointGroupFrame }

  TBreakpointGroupFrame = class(TFrame)
    BtnDelete: TSpeedButton;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    ToolBar1: TToolBar;
    procedure BtnDeleteClick(Sender: TObject);
    procedure FrameDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FrameDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure StaticText1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ToolBar1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ToolBar1StartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    FGroupKind: TBreakpointGroupFrameKind;
    FOnDeleteGroup: TOnDeleteGroup;
    FOwner: TBreakPointsDlgBase;
    FTree: TDbgTreeView;
    FNode: PVirtualNode;
    FBrkGroup: TIDEBreakPointGroup;
    procedure DoBrkGroupFreed(Sender: TObject);
    function GetCount: Integer;
    function GetName: String;
    function GetVisible: boolean;
  protected
    procedure SetVisible(Value: Boolean); reintroduce;
    procedure VisibleChanged; override;
  public
    constructor Create(TheOwner: TBreakPointsDlgBase; ATree: TDbgTreeView; ANode: PVirtualNode;
      ABrkGroup: TIDEBreakPointGroup;
      AGroupKind: TBreakpointGroupFrameKind = bgfGroup); reintroduce;
    destructor Destroy; override;
    procedure UpdateDisplay;
    function Compare(AnOther: TBreakpointGroupFrame): integer;

    property Visible: boolean read GetVisible write SetVisible;
    property GroupKind: TBreakpointGroupFrameKind read FGroupKind;
    property BrkGroup: TIDEBreakPointGroup read FBrkGroup;
    property Tree: TDbgTreeView read FTree;
    property Node: PVirtualNode read FNode;
    property Name: String read GetName;
    property Count: Integer read GetCount;
    property OnDeleteGroup: TOnDeleteGroup read FOnDeleteGroup write FOnDeleteGroup;
  end;

implementation

{$R *.lfm}

{ TBreakpointGroupFrame }

procedure TBreakpointGroupFrame.StaticText1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ToolBar1.DragMode = dmAutomatic then
    ToolBar1.BeginDrag(DragManager.DragImmediate, DragManager.DragThreshold);
end;

procedure TBreakpointGroupFrame.ToolBar1EndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  FOwner.FDraggingGroupHeader := False;
end;

procedure TBreakpointGroupFrame.ToolBar1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FOwner.FDraggingGroupHeader := True;
end;

procedure TBreakpointGroupFrame.BtnDeleteClick(Sender: TObject);
begin
  if assigned(FOnDeleteGroup) then
    FOnDeleteGroup(Self, FBrkGroup);
end;

procedure TBreakpointGroupFrame.FrameDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  NewGroup: TIDEBreakPointGroup;
  VNode: PVirtualNode;
  Brk: TIDEBreakPoint;
  GroupName: String;
begin
  NewGroup := FBrkGroup;

  FOwner.BeginUpdate;
  try
    if FGroupKind = bgfAddNewGroup then begin
      GroupName := '';
      repeat
        repeat
          if not InputQuery(Caption, lisGroupNameInput, GroupName) then
            exit;

          if (GroupName = '') or (not TIDEBreakPointGroup.CheckName(GroupName)) then begin
            if MessageDlg(Caption, lisGroupNameInvalid, mtError, [mbCancel, mbRetry], 0) <> mrRetry then
              exit;
          end
          else
            break;
        until false;

        NewGroup := DebugBoss.BreakPointGroups.GetGroupByName(GroupName);
        if NewGroup = nil then begin
          NewGroup := TIDEBreakPointGroup(DebugBoss.BreakPointGroups.Add);
          NewGroup.Name := GroupName;
          break
        end
        else begin
          case MessageDlg(Caption, Format(lisGroupAssignExisting,
            [GroupName]), mtConfirmation, [mbYes, mbCancel, mbRetry], 0)
          of
            mrYes, mrOK: Break;
            mrNo, mrCancel: exit;
            mrRetry: ;
          end;
        end;
      until false;
    end;

    if (Source = FTree) and (FTree.SelectedCount > 0) then begin
      for VNode in FTree.SelectedNodes(True) do begin
        Brk := TIDEBreakPoint(FTree.NodeItem[VNode]);
        Brk.Group := NewGroup;
      end;
    end;

    if (Source is TToolBar) and (TToolBar(Source).Owner is TBreakpointGroupFrame) then begin
      FOwner.AcceptGroupHeaderDrop(TBreakpointGroupFrame(TToolBar(Source).Owner), FNode);
    end;

  finally
    if FGroupKind = bgfAddNewGroup then
      Visible := False;
    FOwner.EndUpdate;
  end;
end;

procedure TBreakpointGroupFrame.FrameDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  VNode: PVirtualNode;
  Brk: TIDEBreakPoint;
begin
  Accept := False;

  if (Source = FTree) and (FTree.SelectedCount > 0) then begin
    Accept := FGroupKind = bgfAddNewGroup;
    if not Accept then begin
      for VNode in FTree.SelectedNodes(True) do begin
        Brk := TIDEBreakPoint(FTree.NodeItem[VNode]);
        if Brk.Group <> FBrkGroup then begin
          Accept := True;
          Break;
        end;
      end;
    end;
  end;
  if Accept and (State = dsDragMove) then begin
    ToolBar1.Color := clHighlight;
    ToolBar1.Font.Color := clHighlightText;
  end
  else begin
    ToolBar1.Color := clBtnFace;
    ToolBar1.Font.Color := clDefault;
  end;

  if (Source is TToolBar) and (TToolBar(Source).Owner is TBreakpointGroupFrame) then begin
    Accept := (TToolBar(Source).Owner <> Self) and
              (GroupKind in [bgfGroup, bgfUngrouped]);
  end;
end;

function TBreakpointGroupFrame.GetCount: Integer;
begin
  Result := FTree.ChildCount[FNode];
end;

procedure TBreakpointGroupFrame.DoBrkGroupFreed(Sender: TObject);
begin
  FBrkGroup := nil;
  FGroupKind := bgfAbandoned;
  Visible := False;
end;

function TBreakpointGroupFrame.GetName: String;
begin
  Result := StaticText1.Caption;
end;

function TBreakpointGroupFrame.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TBreakpointGroupFrame.SetVisible(Value: Boolean);
begin
  if (Value = Visible) or
     (Value and (FGroupKind = bgfAbandoned))
  then
    exit;

  if not Value then
    inherited SetVisible(Value);
  FTree.IsVisible[FNode] := Value;
end;

procedure TBreakpointGroupFrame.VisibleChanged;
begin
  inherited VisibleChanged;
  if IsVisible then
    FTree.NodeHeight[FNode] := min(40, Max(15, ToolBar1.Height));
end;

constructor TBreakpointGroupFrame.Create(TheOwner: TBreakPointsDlgBase;
  ATree: TDbgTreeView; ANode: PVirtualNode; ABrkGroup: TIDEBreakPointGroup;
  AGroupKind: TBreakpointGroupFrameKind);
begin
  inherited Create(nil);
  FOwner    := TheOwner;
  FTree     := ATree;
  FNode     := ANode;
  FBrkGroup := ABrkGroup;
  FGroupKind := AGroupKind;
  if FBrkGroup <> nil then
    FBrkGroup.AddFreeNotification(@DoBrkGroupFreed)
  else
  if AGroupKind = bgfGroup then
    FGroupKind := bgfUngrouped;

  BtnDelete.Visible := FBrkGroup <> nil;
  BtnDelete.Images := IDEImages.Images_16;
  BtnDelete.ImageIndex := IDEImages.LoadImage('menu_close');

  UpdateDisplay;
end;

destructor TBreakpointGroupFrame.Destroy;
begin
  if FBrkGroup <> nil then
    FBrkGroup.RemoveFreeNotification(@DoBrkGroupFreed);
  inherited Destroy;
end;

procedure TBreakpointGroupFrame.UpdateDisplay;
begin
  case FGroupKind of
    bgfUngrouped: begin
        StaticText1.Caption := BreakViewHeaderNoGroup;
        StaticText2.Caption := Format(' (%d)', [FTree.ChildCount[FNode]]);
      end;
    bgfGroup: begin
        if FBrkGroup <> nil then
          StaticText1.Caption := FBrkGroup.Name;
        StaticText2.Caption := Format(' (%d)', [Count]);

        if (Count > 0) then
          ToolBar1.DragMode := dmAutomatic
        else
          ToolBar1.DragMode := dmManual;
      end;
    bgfAddNewGroup: begin
        StaticText1.Caption := BreakViewHeaderAddGroup;
        StaticText2.Caption := '';
        ToolBar1.Font.Style := [fsItalic];
      end;
    bgfAbandoned: ;
  end;

end;

function TBreakpointGroupFrame.Compare(AnOther: TBreakpointGroupFrame): integer;
begin
  Result := ord(FGroupKind) - ord(AnOther.FGroupKind);
  if Result <> 0 then
    exit;

  if FBrkGroup = nil then
    exit(-1);
  if AnOther.FBrkGroup = nil then
    exit(1);

  if (Count = 0) and (AnOther.Count > 0) then
    exit(1);
  if (Count > 0) and (AnOther.Count = 0) then
    exit(-1);

  Result := FBrkGroup.Index - AnOther.FBrkGroup.Index;
end;

end.

