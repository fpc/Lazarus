unit BreakprointGroupFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  LCLType, Buttons, laz.VirtualTrees, IDEImagesIntf, DebuggerTreeView, Debugger;

type

  TBreakpointGroupFrame = class;

  TOnDeleteGroup = procedure(Sender: TBreakpointGroupFrame; BrkGroup: TIDEBreakPointGroup) of object;

  TBreakpointGroupFrameKind = (bgfUngrouped, bgfGroup, bgfAbandoned);

  { TBreakpointGroupFrame }

  TBreakpointGroupFrame = class(TFrame)
    BtnDelete: TSpeedButton;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    ToolBar1: TToolBar;
    procedure BtnDeleteClick(Sender: TObject);
  private
    FGroupKind: TBreakpointGroupFrameKind;
    FOnDeleteGroup: TOnDeleteGroup;
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
    constructor Create(TheOwner: TComponent; ATree: TDbgTreeView; ANode: PVirtualNode;
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

procedure TBreakpointGroupFrame.BtnDeleteClick(Sender: TObject);
begin
  if assigned(FOnDeleteGroup) then
    FOnDeleteGroup(Self, FBrkGroup);
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

constructor TBreakpointGroupFrame.Create(TheOwner: TComponent;
  ATree: TDbgTreeView; ANode: PVirtualNode; ABrkGroup: TIDEBreakPointGroup;
  AGroupKind: TBreakpointGroupFrameKind);
begin
  inherited Create(nil);
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
  if FBrkGroup <> nil then begin
    StaticText1.Caption := FBrkGroup.Name;
    StaticText2.Caption := Format(' (%d)', [Count]);
  end
  else begin
    StaticText1.Caption := 'No group';
    StaticText2.Caption := Format(' (%d)', [FTree.ChildCount[FNode]]);
  end;
end;

function TBreakpointGroupFrame.Compare(AnOther: TBreakpointGroupFrame): integer;
begin
  if FBrkGroup = nil then
    exit(-1);
  if AnOther.FBrkGroup = nil then
    exit(1);

  if (Count = 0) and (AnOther.Count > 0) then
    exit(1);
  if (Count > 0) and (AnOther.Count = 0) then
    exit(-1);

  //Result := CompareText(Name, AnOther.Name);
  Result := FBrkGroup.Index - AnOther.FBrkGroup.Index;
end;

end.

