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
    buCreateForm: TButton;
    buSave: TButton;
    buRestore: TButton;
    procedure buCreateFormClick(Sender: TObject);
    procedure buDumpClick(Sender: TObject);
    procedure buRestoreClick(Sender: TObject);
    procedure buSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ms: TMemoryStream;
  public
    DockMaster: TDockMaster;
  end; 

var
  MasterSite: TMasterSite;

implementation

uses
  fClientForm;

{ TMasterSite }

procedure TMasterSite.buCreateFormClick(Sender: TObject);
var
  AControl: TWinControl;
begin
  AControl:=DockMaster.CreateDockable('', True, True);
  AControl.EnableAlign;
end;

procedure TMasterSite.buDumpClick(Sender: TObject);
begin
  DockMaster.DumpSites;
end;

procedure TMasterSite.buRestoreClick(Sender: TObject);
begin
  if ms <> nil then
    DockMaster.LoadFromStream(ms);
end;

procedure TMasterSite.buSaveClick(Sender: TObject);
begin
  if ms = nil then
    ms := TMemoryStream.Create
  else
    ms.Clear;
  DockMaster.SaveToStream(ms);
end;

procedure TMasterSite.FormCreate(Sender: TObject);
begin
  DockMaster := TDockMaster.Create(self); //(Application)?
  DockMaster.AddElasticSites(self, [alBottom]);
  DockMaster.CreateDockable('', True, False);
  DockMaster.CreateDockable('', True, False);
//all specific classes must be registered!
{
  RegisterClass(TViewWindow);
  DockMaster.CreateDockable('ViewWindow1', True);
}
end;

initialization
  {$I fmastersite.lrs}

end.

