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
    TComponent. TControls can be shown in a hierachic view.
    It supports
      - multi selecting components
      - editing the creation order
      - editing the TControl.Parent hierachy
    For example of the usage, see the object inspector.

  ToDo:
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
    FComponentList: TComponentSelectionList;
    FPropertyEditorHook: TPropertyEditorHook;
    procedure SetPropertyEditorHook(const AValue: TPropertyEditorHook);
    procedure SetSelections(const NewSelections: TComponentSelectionList);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildComponentNodes; virtual;
    function CreateNodeCaption(AComponent: TComponent): string; virtual;
  public
    property Selections: TComponentSelectionList read FComponentList
                                                 write SetSelections;
    property PropertyEditorHook: TPropertyEditorHook
                           read FPropertyEditorHook write SetPropertyEditorHook;
  end;

implementation

{ TComponentTreeView }

procedure TComponentTreeView.SetSelections(
  const NewSelections: TComponentSelectionList);
begin
  FComponentList.Assign(NewSelections);
  RebuildComponentNodes;
end;

procedure TComponentTreeView.SetPropertyEditorHook(
  const AValue: TPropertyEditorHook);
begin
  if FPropertyEditorHook=AValue then exit;
  FPropertyEditorHook:=AValue;
  RebuildComponentNodes;
end;

constructor TComponentTreeView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FComponentList:=TComponentSelectionList.Create;
  Options:=Options+[tvoAllowMultiselect,tvoAutoItemHeight,tvoKeepCollapsedNodes];
end;

destructor TComponentTreeView.Destroy;
begin
  FreeThenNil(FComponentList);
  inherited Destroy;
end;

procedure TComponentTreeView.RebuildComponentNodes;
var
  OldExpanded: TTreeNodeExpandedState;
  NewNode: TTreeNode;
  RootComponent: TComponent;
  i: Integer;
  AComponent: TComponent;
  RootNode: TTreeNode;
begin
  BeginUpdate;
  // save old expanded state and clear
  OldExpanded:=TTreeNodeExpandedState.Create(Self);
  Items.Clear;

  RootComponent:=PropertyEditorHook.LookupRoot;
  if RootComponent<>nil then begin
    // first add the lookup root
    RootNode:=Items.Add(nil,CreateNodeCaption(RootComponent));
    RootNode.ImageIndex:=-1;
    RootNode.Selected:=Selections.IndexOf(RootComponent)>=0;

    // add components in creation order
    for i:=0 to RootComponent.ComponentCount-1 do begin
      AComponent:=RootComponent.Components[i];
      NewNode:=Items.AddChild(RootNode,CreateNodeCaption(AComponent));
      NewNode.ImageIndex:=-1;
      NewNode.Selected:=Selections.IndexOf(AComponent)>=0;
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

