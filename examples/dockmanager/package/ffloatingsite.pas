unit fFloatingSite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TFloatingSite = class(TForm)
    procedure FormUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FloatingSite: TFloatingSite;

implementation

{ TFloatingSite }

procedure TFloatingSite.FormUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
begin
  if NewTarget = nil then
    Allow := False
  else if DockClientCount <= 1 then
    Application.ReleaseComponent(Self); //Close;
end;

initialization
  {$I ffloatingsite.lrs}

end.

