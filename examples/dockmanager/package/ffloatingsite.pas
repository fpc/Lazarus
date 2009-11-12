unit fFloatingSite;
(* Floating dock host.

ToDo:
- show summary caption
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TFloatingSite = class(TForm)
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  private
    procedure AdjustCaption(without: TControl);
  protected
    procedure Loaded; override;
  public
    { public declarations }
  end; 

var
  FloatingSite: TFloatingSite;

implementation

uses
  EasyDockSite,
  LCLproc;

{ TFloatingSite }

procedure TFloatingSite.AdjustCaption(without: TControl);
var
  i: integer;
  s: string;
  ctl: TControl;
begin
  s := '';
  for i := 0 to DockClientCount - 1 do begin
    ctl := DockClients[i];
    if ctl <> without then
      s := s + GetDockCaption(ctl) + ', ';
  end;
  SetLength(s, Length(s) - 2); //strip trailing ", "
  Caption := s; //GetDockCaption(self);
end;

procedure TFloatingSite.FormDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  AdjustCaption(nil);
end;

procedure TFloatingSite.FormDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  //DebugLn('DockOver');
end;

procedure TFloatingSite.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
(* Check for undock last client:
    - disallow to Nil (move window?)
    - if allowed, kill empty docksite.
  Refresh caption after undock.

Shit: in both cases the docking management does the opposite of what it should do :-(

When the last control is dragged away, it's hosted in a *new* site.
When a second control is dragged away, the entire site is moved.
*)
  if DockClientCount <= 1 then begin
    if NewTarget = nil then begin
      //Allow := False; //deny
      Allow := True;    //move form - where???
      Release;
    end else
      Release;
  end else begin
  //allow float - action required?
  (* strange behaviour: client is undocked, but stays in the site.
      The site is moved to the drop location.
  *)
    //Allow := NewTarget <> nil;  //simply disallow undock to floating state (for now)
    Allow := True; //bug!!!
    //DragManager. - not helpful - where is the DockObject???
  end;
  if Allow then begin
    AdjustCaption(Client);
  end;
end;

procedure TFloatingSite.Loaded;
begin
  inherited Loaded;
  if DockManager = nil then
    DockManager := TEasyTree.Create(self);
  if DockManager is TEasyTree then begin
    TEasyTree(DockManager).HideSingleCaption := True;
    TEasyTree(DockManager).SetStyle(hsForm);
  end;
end;

initialization
  {$I ffloatingsite.lrs}

end.

