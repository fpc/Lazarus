unit DebuggerTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz.VirtualTrees, LMessages;

type

  TDbgTreeNodeData = record
    Item: TObject;
    CachedText: Array of String;
  end;
  PDbgTreeNodeData = ^TDbgTreeNodeData;

  { TDbgTreeView }

  TDbgTreeView = class(TLazVirtualStringTree)
  private
    function GetNodeItem(Node: PVirtualNode): TObject;
    function GetNodeText(Node: PVirtualNode; AColumn: integer): String;
    procedure SetNodeItem(Node: PVirtualNode; AValue: TObject);
    procedure SetNodeText(Node: PVirtualNode; AColumn: integer; AValue: String);
  protected
    procedure ValidateNodeDataSize(var Size: Integer); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DetermineLineImageAndSelectLevel(Node: PVirtualNode;
      var LineImage: TLineImage): Integer; override;
    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var AText: String); override;
  public
    function GetNodeData(Node: PVirtualNode): PDbgTreeNodeData; reintroduce;

    function GetFocusedNode(OnlySelected: Boolean = True): PVirtualNode;
    function FocusedData(OnlySelected: Boolean = True): PDbgTreeNodeData;
    function FocusedItem(OnlySelected: Boolean = True): TObject;

    procedure SelectNode(Node: PVirtualNode; ASetFocus: boolean = True);
    function FindNodeForItem(AnItem: TObject): PVirtualNode;
    procedure DeleteNodeEx(Node: PVirtualNode; FreeItem: Boolean; Reindex: Boolean = True);

    property NodeItem[Node: PVirtualNode]: TObject read GetNodeItem write SetNodeItem;
    property NodeText[Node: PVirtualNode; AColumn: integer]: String read GetNodeText write SetNodeText;
  end;

implementation

{ TDbgTreeView }

function TDbgTreeView.GetNodeItem(Node: PVirtualNode): TObject;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := GetNodeData(Node);
  if Data <> nil then
    Result := Data^.Item;
end;

function TDbgTreeView.GetNodeText(Node: PVirtualNode; AColumn: integer): String;
var
  Data: PDbgTreeNodeData;
begin
  Result := '';
  Data := GetNodeData(Node);
  if (Data <> nil) and (AColumn < Length(Data^.CachedText)) then
    Result := Data^.CachedText[AColumn];
end;

procedure TDbgTreeView.SetNodeItem(Node: PVirtualNode; AValue: TObject);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then
    Data^.Item := AValue;
end;

procedure TDbgTreeView.SetNodeText(Node: PVirtualNode; AColumn: integer;
  AValue: String);
var
  Data: PDbgTreeNodeData;
begin
  Data := GetNodeData(Node);
  if Data <> nil then begin
    if AColumn >= Length(Data^.CachedText) then
      SetLength(Data^.CachedText, AColumn + 1);
    Data^.CachedText[AColumn] := AValue;
  end;
end;

procedure TDbgTreeView.ValidateNodeDataSize(var Size: Integer);
begin
  Size := SizeOf(TDbgTreeNodeData);
end;

procedure TDbgTreeView.DoFreeNode(Node: PVirtualNode);
begin
  PDbgTreeNodeData(GetNodeData(Node))^ := Default(TDbgTreeNodeData);
  inherited DoFreeNode(Node);
end;

function TDbgTreeView.DetermineLineImageAndSelectLevel(Node: PVirtualNode;
  var LineImage: TLineImage): Integer;
begin
  Result := inherited DetermineLineImageAndSelectLevel(Node, LineImage);
  if Length(LineImage) > 1 then
    LineImage[0] := ltNone
  else
  if (Length(LineImage) > 0) and (LineImage[0] <> ltNone) then
    LineImage[0] := ltRight;
end;

procedure TDbgTreeView.HandleMouseDblClick(var Message: TLMMouse;
  const HitInfo: THitInfo);
begin
  inherited HandleMouseDblClick(Message, HitInfo);
  if (HitInfo.HitNode = nil) and (HitInfo.HitColumn < 0) then
    DoNodeDblClick(HitInfo);

end;

procedure TDbgTreeView.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var AText: String);
begin
  if Assigned(OnGetText) then begin
    inherited DoGetText(Node, Column, TextType, AText);
  end
  else begin
    AText := NodeText[Node, Column];
  end;
end;

function TDbgTreeView.GetNodeData(Node: PVirtualNode): PDbgTreeNodeData;
begin
  Result := PDbgTreeNodeData(inherited GetNodeData(Node));
end;

function TDbgTreeView.GetFocusedNode(OnlySelected: Boolean): PVirtualNode;
begin
  Result := FocusedNode;
  if (Result = nil) or (OnlySelected and not Selected[Result]) then
    Result := nil;
end;

function TDbgTreeView.FocusedData(OnlySelected: Boolean): PDbgTreeNodeData;
var
  VNode: PVirtualNode;
begin
  Result := nil;
  VNode := FocusedNode;
  if (VNode = nil) or (OnlySelected and not Selected[VNode]) then
    exit;
  Result := PDbgTreeNodeData(GetNodeData(VNode));
end;

function TDbgTreeView.FocusedItem(OnlySelected: Boolean): TObject;
var
  Data: PDbgTreeNodeData;
begin
  Result := nil;
  Data := FocusedData(OnlySelected);
  if Data <> nil then
    Result := Data^.Item;
end;

procedure TDbgTreeView.SelectNode(Node: PVirtualNode; ASetFocus: boolean);
begin
  ClearSelection;
  FocusedNode := Node;
  Selected[Node] := True;
end;

function TDbgTreeView.FindNodeForItem(AnItem: TObject): PVirtualNode;
var
  VNode: PVirtualNode;
begin
  for VNode in NoInitNodes do begin
    if GetNodeItem(VNode) = AnItem then
      exit(VNode);
  end;
  Result := nil;
end;

procedure TDbgTreeView.DeleteNodeEx(Node: PVirtualNode; FreeItem: Boolean;
  Reindex: Boolean);
var
  Item: TObject;
begin
  if Node = nil then
    exit;
  Item := GetNodeItem(Node);
  DeleteNode(Node, Reindex);
  if FreeItem then
    Item.Free;
end;

initialization
  RegisterClass(TDbgTreeView);

end.

