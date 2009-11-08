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
    function  DoUnDock(NewTarget: TWinControl; Client: TControl;
                       KeepDockSiteSize: Boolean = true): Boolean; override;
  public
    { public declarations }
  end; 

var
  FloatingSite: TFloatingSite;

implementation

uses
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

function TFloatingSite.DoUnDock(NewTarget: TWinControl; Client: TControl;
  KeepDockSiteSize: Boolean): Boolean;
begin
  //Result:=inherited DoUnDock(NewTarget, Client, KeepDockSiteSize);
  Result := True;
  if Assigned(OnUnDock) then begin
    OnUnDock(Self, Client, NewTarget, Result);
    if not Result then
      Exit;
  end;
{ TODO -cdocking : Also ask docking manager!
In case of a drop into the old location the undock operation should be aborted,
because then the docking manager (DragDockObject) would refer to an invalid (no more
existing) target.
 }
{$IFDEF old}
  if not KeepDockSiteSize then
  begin
    NewBounds := BoundsRect;
    case Client.Align of
      alLeft:
        inc(NewBounds.Left, Client.Width);
      alTop:
        inc(NewBounds.Top, Client.Height);
      alRight:
        dec(NewBounds.Right, Client.Width);
      alBottom:
        dec(NewBounds.Bottom, Client.Height);
    end;
    SetBoundsKeepBase(NewBounds.Left, NewBounds.Top,
                      NewBounds.Right - NewBounds.Left,
                      NewBounds.Bottom - NewBounds.Top);
  end;
{$ELSE}
(* There exists a bug in the floating logic :-(
  When we are the FloatingDockSiteClass, the control isundocked,
  BUT then becomes a normal child control,
  and the docksite (we!) is moved to the new location.
*)
{$ENDIF}
  Result := Result and DoUndockClientMsg(NewTarget, Client);
  if Result and (NewTarget = nil) then begin
    //ManualFloat(???)
  end;
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
*)
  if DockClientCount <= 1 then begin
    if NewTarget = nil then begin
      Allow := False;
      //move form?
    end else
      Application.ReleaseComponent(Self); //Close;
  end else begin
  //allow float - action required?
  (* strange behaviour: client is undocked, but stays in the site.
      The site is moved to the drop location.
  *)
    Allow := True;
  end;
  if Allow then
    AdjustCaption(Client);
end;

initialization
  {$I ffloatingsite.lrs}

end.

