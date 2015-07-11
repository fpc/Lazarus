unit fClientForm;
(* A form for easy docking into multi-client floating sites.

  When the form becomes floating, it docks itself into a new floating dockhost
  site, that can accept further clients.

  Since not all window managers allow to dock forms,
  docking is allowed only from a dedicated (pin) icon.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TViewWindow = class(TForm)
    Label1: TLabel;
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

//var ViewWindow: TViewWindow; //useless

implementation

uses
  //LCLProc,  //debugging only
  fFloatingSite;

{ TViewWindow }

procedure TViewWindow.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var
  Site: TFloatingSite;
begin
(* When we become floating, dock immediately into a new floating host docksite.
*)
  if HostDockSite = nil then begin
    //DebugLn('--- floating');
    Site := TFloatingSite.Create(Application); //the new site
    Site.BoundsRect := self.BoundsRect; //the new position and extension
    ManualDock(Site);
  end else begin
    //DebugLn('--- in ' + HostDockSite.Name);
  end;
end;

procedure TViewWindow.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
(* The mouse is moved over our docking gadget.
  When the left button is pressed, start dragging (for docking).
*)
  if ssLeft in Shift then begin
    BeginDrag(False);
  end;
end;

{$R *.lfm}

end.

