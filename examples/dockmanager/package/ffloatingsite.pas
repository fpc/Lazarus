unit fFloatingSite;
(* Floating dock host.
  Host one or more docked clients.
  To distinguish multiple clients, use the form header style (named caption).
  Destroy the site on the last undock.

  Handle flaws of the Delphi docking model (improper undock).
  - Disallow TControls to float (else nothing but trouble).
  - For the IDE, floating client forms must wrap themselves into a new
    host site, to allow for continued docking of other clients.

Problems:
As with DockBook, closing docked forms results in Exceptions :-(

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls;

type
  TFloatingSite = class(TForm)
    Image1: TImage;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  protected
    procedure Loaded; override;
  public
    procedure UpdateCaption(without: TControl);
  end;

var
  FloatingSite: TFloatingSite;

implementation

uses
  LCLproc,  //debugging only
  EasyDockSite; //our DockManager

{ TFloatingSite }

procedure TFloatingSite.UpdateCaption(without: TControl);
var
  i: integer;
  s: string;
  ctl: TControl;
begin
(* Show the combined captions of all clients.
  Exclude client to be undocked.
*)
  s := '';
  for i := 0 to DockClientCount - 1 do begin
    ctl := DockClients[i];
    if ctl <> without then
      s := s + GetDockCaption(ctl) + ', ';
  end;
  SetLength(s, Length(s) - 2); //strip trailing ", "
  Caption := s;
end;

procedure TFloatingSite.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: integer;
  ctl: TControl;
  frm: TCustomForm absolute ctl;
begin
(* When an empty site is closed, it shall be freed.
  Otherwise the clients must be handled (close forms).

  Currently closing docked forms leads to exceptions :-(
*)
{$IFDEF new}
  //BeginFormUpdate;
  for i := DockClientCount - 1 downto 0 do begin
    ctl := DockClients[i];
    ctl.ManualDock(nil);
    //Application.ReleaseComponent(ctl); --- Exception!
    if ctl <> nil then begin
    //verify that both Parent and HostDockSite are cleared
      DebugLn('Undocked %s P=%p H=%p', [ctl.Name,
        pointer(ctl.Parent), pointer(ctl.HostDockSite)]);
      //DebugLn('P=%p H=%p', [ctl.Parent, ctl.HostDockSite]);
      //DebugLn('%x', [self]);
    end;
    if ctl is TCustomForm then begin
      //frm.Close; --- Exception!
      //frm.Release; --- also Exception!
      //frm.Hide;
    end;
  end;
  //EndFormUpdate;
{$ELSE}
  //not required?
{$ENDIF}
  CloseAction := caFree;
end;

procedure TFloatingSite.FormDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
(* Update the caption.
*)
  UpdateCaption(nil);
end;

procedure TFloatingSite.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
(* Check for undock last client, if allowed kill empty docksite.
  Refresh caption after undock.

Shit: in both cases the docking management does the opposite of what it should do :-(

When the last control is dragged away, it's hosted in a *new* site.
When a second control is dragged away, the entire site is moved.

Fix: disallow TControls to become floating.
*)
//try to distinguish between TControl and TWinControl (TCustomForm?)
  Allow := (NewTarget <> nil) or (Client is TWinControl); //seems to be safe
  if not Allow then
    exit; //all done

  if DockClientCount <= 1 then begin
    Release; //destroy empty site
  end else begin
    UpdateCaption(Client); //update caption, excluding removed client
    DockManager.ResetBounds(True); //required with gtk2!?
  end;
end;

procedure TFloatingSite.Loaded;
begin
(* select and configure the docking manager.
*)
  inherited Loaded;
  if DockManager = nil then
    DockManager := TEasyTree.Create(self);
  if DockManager is TEasyTree then begin
  //adjust as desired (order required!?)
    TEasyTree(DockManager).HideSingleCaption := True; //only show headers for multiple clients
    TEasyTree(DockManager).SetStyle(hsForm);  //show client name in the header
  end;
end;

initialization
  {$I ffloatingsite.lrs}

end.

