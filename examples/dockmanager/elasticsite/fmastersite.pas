unit fMasterSite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,
  uMakeSite;

type
  TMasterSite = class(TForm)
    buDump: TButton;
    procedure buDumpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    DockMaster: TDockMaster;
  end; 

var
  MasterSite: TMasterSite;

implementation

uses
  fClientForm;

{ TMasterSite }

procedure TMasterSite.buDumpClick(Sender: TObject);
begin
  DockMaster.DumpSites;
end;

procedure TMasterSite.FormCreate(Sender: TObject);
begin
  DockMaster := TDockMaster.Create(self); //(Application)?
  DockMaster.AddElasticSites(self, [alBottom]);
  DockMaster.CreateDockable('', True);
  DockMaster.CreateDockable('', True);
//all specific classes must be registered!
{
  RegisterClass(TViewWindow);
  DockMaster.CreateDockable('ViewWindow1', True);
}
end;

initialization
  {$I fmastersite.lrs}

end.

