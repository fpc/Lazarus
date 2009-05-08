unit fMain;
(* EasyDockSite dock test form by DoDi.
  Demonstrates docking of various controls, and related bugs in the LCL.
*)

(* A dock site should contain no other (not docked) controls.
  Delphi docks all controls in a dock site, when a docking manager is assigned.
  This does not (yet) work with the LCL :-(
  When a docking manager is replaced, the controls should be undocked again?
  (depends on the lists, where docked and undocked controls reside in the dock site)

  LCL does not notify the docking manager of a resized dock site?
*)

//some defines, to demonstrate LCL flaws
{$DEFINE docker}    //using control (undef: entire form) as dock site
{$DEFINE easy}      //using EasyDockSite (undef: default LDockTree)
{.$DEFINE dragForm}  //create a form from the draggable images (or drag images)
  //dragging forms is not supported on all platforms!


interface

uses
  LCLIntf,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls, LResources;

type

  { TEasyDockMain }

  TEasyDockMain = class(TForm)
    Docker: TPanel;
    edDock: TEdit;
    lbDock: TLabel;
    sb: TStatusBar;
    ToolBar1: TToolBar;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    buDump: TButton;
    procedure DockerResize(Sender: TObject);
    procedure Shape1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DockerDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure DockerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DockerDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure buDumpClick(Sender: TObject);
  private
  {$IFDEF docker}
  {$ELSE}
    Docker: TForm;
  {$ENDIF}
    ShapeCount: integer;
  public
    { Public declarations }
  end;

var
  EasyDockMain: TEasyDockMain;

implementation

uses
  fDockable,
{$IFDEF easy}
  EasyDockSite,
{$ELSE}
{$ENDIF}
  Interfacebase,
  fTree;


procedure TEasyDockMain.Shape1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  df: TDockable;
  shp: TShape;
  c: TColor;
  ctl: TShape;
  r: TRect;
begin
  if sender is TShape then begin
    shp := Sender as TShape;
  {$IFDEF dragForm}
    df := TDockable.Create(self);
    sb.SimpleText := df.Name;
    c := shp.Brush.Color;
    //df.Color := c; - not all widgetsets support TForm.Color!?
    //c := df.Color;
    df.Shape1.Brush.Color := c;

  { TODO -cdocking : form is not dockable with some widgetsets? }
  // all this doesn't help
    df.DragKind := dkDock;
    df.DragMode := dmAutomatic;
    //df.ManualFloat(df.BoundsRect);

    df.Visible := True;

    if WidgetSet.GetLCLCapability(lcDragDockStartOnTitleClick) <> 0 then begin
      sb.SimpleText := 'should dock'
    end else begin
      sb.SimpleText := 'cannot dock'
    end;
  {$ELSE}
    ctl := TShape.Create(self);
    //ctl.Assign(shp);
    ctl.Name := 'test' + IntToStr(ShapeCount);
    inc(ShapeCount);
    ctl.Brush.Color := shp.Brush.Color;
    ctl.DragMode := dmAutomatic;
    ctl.DragKind := dkDock;
    r.TopLeft := self.BoundsRect.TopLeft;
    //r.Left := x; r.Top := y;
    r.Right := r.Left + 100;
    r.Bottom := r.Top + 100;
    ctl.ManualFloat(r); //(ctl.BoundsRect);
  {$ENDIF}
    //df.Name := shp.Name;
  end;
end;

procedure TEasyDockMain.FormCreate(Sender: TObject);
begin
{$IFDEF docker}
{$ELSE}
  Docker := self;
{$ENDIF}
{$IFDEF easy}
  Docker.DockManager := TEasyTree.Create(Docker);
{$ELSE}
{$ENDIF}
  Docker.DockSite := True;
  Docker.UseDockManager := True;
  Mouse.DragImmediate := False;
end;

procedure TEasyDockMain.DockerDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
  sb.SimpleText := 'drop!';
end;

procedure TEasyDockMain.DockerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  //sb.SimpleText := 'move';
  //Docker.DockManager.
end;

procedure TEasyDockMain.DockerResize(Sender: TObject);
begin
(* workaround:
  Delphi notifies the docking manager of a changed dock site size,
  Lazarus doesn't :-(
*)
{ TODO -cdocking : DockManager should receive resize notification from the dock site.
Fix this in the LCL! }
//check: already fixed?
  //Docker.DockManager.ResetBounds(False);
end;

procedure TEasyDockMain.DockerDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  s: string;
begin
{$IFDEF easy}
  if DropOn = nil then
    sb.SimpleText := '<drop nowhere>'
  else begin
    s := Format('drop onto %s[%d,%d - %d,%d]', [
      DropOn.Name, DropOn.Top, DropOn.Left, DropOn.Width, DropOn.Height]);
    sb.SimpleText := s;
  end;
{$ELSE}
{$ENDIF}
end;

procedure TEasyDockMain.buDumpClick(Sender: TObject);
var
  s: TStringStream;
begin
  s := TStringStream.Create('');
  try
    Docker.DockManager.SaveToStream(s);
    DumpBox.Memo1.Text := s.DataString;
  finally
    s.Free;
  end;
  DumpBox.Visible := True;
  sb.SimpleText := lbDock.Name;
end;

initialization
  {$i fMain.lrs}
end.

