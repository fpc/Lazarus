unit fFloatingSite;
(* Floating dock host.
  Host one or more docked clients.
  Destroy the site on the last undock.

  To allow for un/docking forms without widgetset support, use dock headers.
  To distinguish multiple clients, use the form header style (named caption).
    Default are unnamed headers, override app specifc with hsForm.

  Handle flaws of the Delphi docking model (improper undock).
  - Disallow TControls to float (else nothing but trouble).
  - For the IDE, floating client forms must wrap themselves into a new
    host site, to allow for continued docking of other clients.

Problems:
As with DockBook, closing docked forms may result in Exceptions :-(

*)

{$mode objfpc}{$H+}

{$DEFINE appdock} //using DockMaster/AppDockManager?

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls;

type
  TFloatingSite = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  protected
    destructor Destroy; override;
    procedure Loaded; override;
  {$IFDEF appdock}
  {$ELSE}
    procedure ReloadDockedControl(const AControlName: string;
                                  var AControl: TControl); override;
  {$ENDIF}
  public
    procedure UpdateCaption(without: TControl);
  end;

implementation

uses
  LCLproc,  //debugging only
  EasyDockSite, //EasyTree DockManager
  uMakeSite;    //AppDockManager

// ----------- config --------------
const
  HideSingleHeader = False; //always show dockheader, for undocking forms
  HeaderStyle = hsMinimal;  //default to small headers (no caption bar)

type
{$IFDEF appdock}
  TOurDockManager = TAppDockManager;
{$ELSE}
  TOurDockManager = TEasyTree;
{$ENDIF}

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

destructor TFloatingSite.Destroy;
begin
  DebugLn('destroying ', Name);
  inherited Destroy;
end;

procedure TFloatingSite.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: integer;
  ctl: TControl;
  //frm: TCustomForm absolute ctl;
begin
(* When an empty site is closed, it shall be freed.
  Otherwise the clients must be handled (close forms?).
*)
  for i := DockClientCount - 1 downto 0 do begin
    ctl := DockClients[i];
    ctl.Visible := False; //handle rest invisibly
    ctl.ManualDock(nil);
    //if ctl.Owner = nil then ctl.Destroy; //seems to work, but is this okay???
{
//Application.ReleaseComponent(ctl); --- Exception!
    if ctl <> nil then begin
    //verify that both Parent and HostDockSite are cleared
      DebugLn('Undocked %s P=%p H=%p', [ctl.Name,
        pointer(ctl.Parent), pointer(ctl.HostDockSite)]);
    end;
    if ctl is TCustomForm then begin
      frm.Close; //--- Exception!
      //frm.Release; --- also Exception!
      //frm.Hide;
    end;
}
  end;
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

Shit: in both cases the docking management does the opposite of what it should do:
  When the last control is dragged away, it's hosted in a *new* site.
  When a second control is dragged away, the entire site is moved.
:-(

Fix: disallow TControls to become floating.
*)
//try to distinguish between TControl and TWinControl (TCustomForm?)
  Allow := (NewTarget <> nil) or (Client is TWinControl); //seems to be safe
  DebugLn('TFloatingSite undodock, allow ', DbgS(Allow));
  if not Allow then
    exit; //all done

  if DockClientCount <= 1 then begin
    DebugLn('release ', Name);
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
    DockManager := TOurDockManager.Create(self);
  if DockManager is TEasyTree then begin
  //adjust as desired, in config section above (order required!?)
    TEasyTree(DockManager).HideSingleCaption := HideSingleHeader;  // True; //only show headers for multiple clients
    TEasyTree(DockManager).SetStyle(HeaderStyle);  //show client name in the header
  end;
end;

{$IFDEF appdock}
{$ELSE}
procedure TFloatingSite.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  inherited ReloadDockedControl(AControlName, AControl);
  if AControl = nil then begin
    AControl := TForm.Create(Application);
    //make dock client
    //if uMakeSite...
  end;
end;
{$ENDIF}

{$R *.lfm}

initialization
  {.$I ffloatingsite.lrs}

end.


