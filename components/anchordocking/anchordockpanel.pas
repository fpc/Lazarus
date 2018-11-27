{ For license see anchordocking.pas
}
unit AnchorDockPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  AnchorDockStorage;

type
  TAnchorDockPanel = class(TPanel)
  protected
    procedure DragOver({%H-}Source: TObject; {%H-}X, {%H-}Y: Integer; {%H-}State: TDragState;
      var Accept: Boolean); override;
  public
    procedure SaveLayout(LayoutTree: TAnchorDockLayoutTree;
                         LayoutNode: TAnchorDockLayoutTreeNode);
    function GetOneControl: TControl;
  published
  end;

procedure Register;

implementation

uses AnchorDocking;

procedure TAnchorDockPanel.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept:=true;
end;

function TAnchorDockPanel.GetOneControl: TControl;
var
  i: Integer;
begin
  for i:=0 to ControlCount-1 do begin
    Result:=Controls[i];
    if Result.Owner<>Self then exit;
  end;
  Result:=nil;
end;

procedure TAnchorDockPanel.SaveLayout(
  LayoutTree: TAnchorDockLayoutTree; LayoutNode: TAnchorDockLayoutTreeNode);
var
  OneControl: TControl;
begin
  OneControl:=GetOneControl;
  if OneControl is TAnchorDockHostSite then
  begin

    LayoutNode.NodeType:=adltnControl;
    LayoutNode.Assign(Self,false,false);
    LayoutNode.Name:={OneControl.}Name;

    TAnchorDockHostSite(OneControl).SaveLayout(LayoutTree,LayoutNode);
  end;
end;

procedure Register;
begin
  {$I anchordockpanel_icon.lrs}
  RegisterComponents('Additional',[TAnchorDockPanel]);
end;

end.
