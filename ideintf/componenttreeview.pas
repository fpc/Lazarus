{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    TComponentTreeView is a component to show the child components of a
    TComponent. TControls are shown in a hierachic view.
    It supports
      - multi selecting components
      - ToDo: editing the creation order
      - ToDo: editing the TControl.Parent hierachy
    For an usage example, see the object inspector.

  ToDo:
    - icons
    - pass keys to form editor
    - drag&drop: change parent and position
}
unit ComponentTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, ComCtrls, PropEdits;
  
type
  { TComponentTreeView }

  TComponentTreeView = class(TCustomTreeView)
  private
    FComponentList: TBackupComponentList;
    FPropertyEditorHook: TPropertyEditorHook;
    function GetSelection: TPersistentSelectionList;
    procedure SetPropertyEditorHook(const AValue: TPropertyEditorHook);
    procedure SetSelection(const NewSelection: TPersistentSelectionList);
  protected
    procedure DoSelectionChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildComponentNodes; virtual;
    procedure UpdateComponentNodesValues; virtual;
    function CreateNodeCaption(APersistent: TPersistent): string; virtual;
  public
    property Selection: TPersistentSelectionList read GetSelection
                                                 write SetSelection;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
    property OnSelectionChanged;
  end;

implementation

{ TComponentTreeView }

procedure TComponentTreeView.SetSelection(
  const NewSelection: TPersistentSelectionList);
begin
  if (PropertyEditorHook=nil) then begin
    if (FComponentList.LookupRoot=nil) then
      exit;
    FComponentList.Clear;
  end else begin
    if FComponentList.IsEqual(PropertyEditorHook.LookupRoot,NewSelection) then
    begin
      // nodes ok, but maybe node values needs update
      UpdateComponentNodesValues;
      exit;
    end;
  end;
  FComponentList.LookupRoot:=PropertyEditorHook.LookupRoot;
  FComponentList.Selection.Assign(NewSelection);
  RebuildComponentNodes;
end;

procedure TComponentTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  AComponent: TComponent;
  NewSelection: TPersistentSelectionList;
begin
  NewSelection:=TPersistentSelectionList.Create;
  try
    if (PropertyEditorHook<>nil)
    and (PropertyEditorHook.LookupRoot<>nil)
    and (not (csDestroying in ComponentState)) then begin
      ANode:=GetFirstMultiSelected;
      while ANode<>nil do begin
        AComponent:=TComponent(ANode.Data);
        if AComponent=nil then
          RaiseGDBException('TComponentTreeView.DoSelectionChanged ANode.Data=nil');
        if (AComponent=PropertyEditorHook.LookupRoot)
        or (AComponent.Owner=PropertyEditorHook.LookupRoot)
        then
          NewSelection.Add(AComponent);
        ANode:=ANode.GetNextMultiSelected;
      end;
    end;
    if NewSelection.IsEqual(FComponentList.Selection) then exit;
    FComponentList.Selection.Assign(NewSelection);
    inherited DoSelectionChanged;
  finally
    NewSelection.Free;
  end;
end;

procedure TComponentTreeView.SetPropertyEditorHook(
  const AValue: TPropertyEditorHook);
begin
  if FPropertyEditorHook=AValue then exit;
  FPropertyEditorHook:=AValue;
  RebuildComponentNodes;
end;

function TComponentTreeView.GetSelection: TPersistentSelectionList;
begin
  Result:=FComponentList.Selection;
end;

constructor TComponentTreeView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FComponentList:=TBackupComponentList.Create;
  Options:=Options+[tvoAllowMultiselect,tvoAutoItemHeight,tvoKeepCollapsedNodes];
end;

destructor TComponentTreeView.Destroy;
begin
  FreeThenNil(FComponentList);
  inherited Destroy;
end;

procedure TComponentTreeView.RebuildComponentNodes;

  procedure AddChildControls(AControl: TWinControl; ANode: TTreeNode);
  var
    i: Integer;
    CurControl: TControl;
    NewNode: TTreeNode;
  begin
    if AControl=nil then exit;
    for i:=0 to AControl.ControlCount-1 do begin
      CurControl:=AControl.Controls[i];
      if CurControl.Owner<>AControl.Owner then continue;
      NewNode:=Items.AddChild(ANode,CreateNodeCaption(CurControl));
      NewNode.Data:=CurControl;
      NewNode.ImageIndex:=-1;
      NewNode.MultiSelected:=Selection.IndexOf(CurControl)>=0;
      if CurControl is TWinControl then
        AddChildControls(TWinControl(CurControl),NewNode);
    end;
    ANode.Expanded:=true;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  NewNode: TTreeNode;
  RootObject: TPersistent;
  i: Integer;
  AComponent: TComponent;
  RootNode: TTreeNode;
  AControl: TControl;
  RootComponent: TComponent;
begin
  BeginUpdate;
  // save old expanded state and clear
  OldExpanded:=TTreeNodeExpandedState.Create(Self);
  Items.Clear;

  RootObject:=PropertyEditorHook.LookupRoot;
  if RootObject<>nil then begin
    // first add the lookup root
    RootNode:=Items.Add(nil,CreateNodeCaption(RootObject));
    RootNode.Data:=RootObject;
    RootNode.ImageIndex:=-1;
    RootNode.MultiSelected:=Selection.IndexOf(RootObject)>=0;

    // add components in creation order and TControl.Parent relationship
    if RootObject is TComponent then begin
      RootComponent:=TComponent(RootObject);
      for i:=0 to RootComponent.ComponentCount-1 do begin
        AComponent:=RootComponent.Components[i];
        if AComponent is TControl then begin
          AControl:=TControl(AComponent);
          if (AControl.Parent<>nil) and (AControl.Parent<>RootComponent) then
            // child controls will be added recursively, not here
            continue;
        end;
        NewNode:=Items.AddChild(RootNode,CreateNodeCaption(AComponent));
        NewNode.Data:=AComponent;
        NewNode.ImageIndex:=-1;
        NewNode.MultiSelected:=Selection.IndexOf(AComponent)>=0;
        if AComponent is TWinControl then
          AddChildControls(TWinControl(AComponent),NewNode);
      end;
    end;
    RootNode.Expand(true);
  end;

  // restore old expanded state
  OldExpanded.Apply(Self);
  OldExpanded.Free;
  EndUpdate;
end;

procedure TComponentTreeView.UpdateComponentNodesValues;

  procedure UpdateComponentNode(ANode: TTreeNode);
  var
    AComponent: TComponent;
  begin
    if ANode=nil then exit;
    AComponent:=TComponent(ANode.Data);
    ANode.Text:=CreateNodeCaption(AComponent);
    UpdateComponentNode(ANode.GetFirstChild);
    UpdateComponentNode(ANode.GetNextSibling);
  end;

begin
  UpdateComponentNode(Items.GetFirstNode);
end;

function TComponentTreeView.CreateNodeCaption(APersistent: TPersistent): string;
begin
  Result:=APersistent.ClassName;
  if APersistent is TComponent then
    Result:=TComponent(APersistent).Name+': '+Result;
end;

end.

