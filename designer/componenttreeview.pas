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
      - editing the creation order
      - editing the TControl.Parent hierachy
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
    function GetSelections: TComponentSelectionList;
    procedure SetPropertyEditorHook(const AValue: TPropertyEditorHook);
    procedure SetSelections(const NewSelections: TComponentSelectionList);
  protected
    procedure DoSelectionChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildComponentNodes; virtual;
    function CreateNodeCaption(AComponent: TComponent): string; virtual;
  public
    property Selections: TComponentSelectionList read GetSelections
                                                 write SetSelections;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
  end;

implementation

{ TComponentTreeView }

procedure TComponentTreeView.SetSelections(
  const NewSelections: TComponentSelectionList);
begin
  if (PropertyEditorHook=nil) then begin
    if (FComponentList.LookupRoot=nil) then
      exit;
    FComponentList.Clear;
  end else begin
    if FComponentList.IsEqual(PropertyEditorHook.LookupRoot,NewSelections) then
      exit;
  end;
  FComponentList.LookupRoot:=PropertyEditorHook.LookupRoot;
  FComponentList.Selection.Assign(NewSelections);
  RebuildComponentNodes;
end;

procedure TComponentTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  AComponent: TComponent;
  NewSelection: TComponentSelectionList;
begin
  NewSelection:=TComponentSelectionList.Create;
  try
    if (PropertyEditorHook<>nil)
    and (PropertyEditorHook.LookupRoot<>nil)
    and (not (csDestroying in ComponentState)) then begin
      ANode:=GetFirstMultiSelected;
      while ANode<>nil do begin
        AComponent:=TComponent(ANode.Data);
        if AComponent=nil then
          RaiseGDBException('TComponentTreeView.DoSelectionChanged ANode.Data=nil');
        if ((AComponent.Owner=nil)
        and (AComponent=PropertyEditorHook.LookupRoot))
        or (AComponent.Owner=PropertyEditorHook.LookupRoot)
        then
          NewSelection.Add(AComponent);
        ANode:=ANode.GetNextMultiSelected;
      end;
    end;
    if NewSelection.IsEqual(FComponentList.Selection) then exit;
    FComponentList.Selection.Assign(NewSelection);
  finally
    NewSelection.Free;
  end;
  inherited DoSelectionChanged;
end;

procedure TComponentTreeView.SetPropertyEditorHook(
  const AValue: TPropertyEditorHook);
begin
  if FPropertyEditorHook=AValue then exit;
  FPropertyEditorHook:=AValue;
  RebuildComponentNodes;
end;

function TComponentTreeView.GetSelections: TComponentSelectionList;
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
      NewNode.MultiSelected:=Selections.IndexOf(CurControl)>=0;
      if CurControl is TWinControl then
        AddChildControls(TWinControl(CurControl),NewNode);
    end;
  end;

var
  OldExpanded: TTreeNodeExpandedState;
  NewNode: TTreeNode;
  RootComponent: TComponent;
  i: Integer;
  AComponent: TComponent;
  RootNode: TTreeNode;
  AControl: TControl;
begin
  BeginUpdate;
  // save old expanded state and clear
  OldExpanded:=TTreeNodeExpandedState.Create(Self);
  Items.Clear;

  RootComponent:=PropertyEditorHook.LookupRoot;
  if RootComponent<>nil then begin
    // first add the lookup root
    RootNode:=Items.Add(nil,CreateNodeCaption(RootComponent));
    RootNode.Data:=RootComponent;
    RootNode.ImageIndex:=-1;
    RootNode.MultiSelected:=Selections.IndexOf(RootComponent)>=0;

    // add components in creation order and TControl.Parent relationship
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
      NewNode.MultiSelected:=Selections.IndexOf(AComponent)>=0;
      if AComponent is TWinControl then
        AddChildControls(TWinControl(AComponent),NewNode);
    end;
    
    RootNode.Expand(true);
  end;

  // restore old expanded state
  OldExpanded.Apply(Self);
  OldExpanded.Free;
  EndUpdate;
end;

function TComponentTreeView.CreateNodeCaption(AComponent: TComponent): string;
begin
  Result:=AComponent.Name+': '+AComponent.ClassName;
end;

end.

