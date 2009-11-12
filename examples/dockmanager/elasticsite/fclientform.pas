unit fClientForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TViewWindow = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure FormEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ViewWindow: TViewWindow;

implementation

uses
  LCLProc, fFloatingSite;

{ TViewWindow }

procedure TViewWindow.FormEndDock(Sender, Target: TObject; X, Y: Integer);
var
  Site: TFloatingSite;
begin
  if HostDockSite = nil then begin
    DebugLn('--- floating');
    Site := TFloatingSite.Create(Application);
    Site.BoundsRect := self.BoundsRect;
    ManualDock(Site);
  end else
    DebugLn('--- in ' + HostDockSite.Name);
end;

procedure TViewWindow.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then begin
    BeginDrag(False);
  end;
end;

initialization
  {$I fclientform.lrs}

end.

