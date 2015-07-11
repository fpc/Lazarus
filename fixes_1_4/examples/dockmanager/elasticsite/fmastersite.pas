unit fMasterSite;
(* Test dockable forms and elastic sites, and save/restore layout.

ToDo: restore layout does not re-use existing forms?
*)

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
{$IFDEF TestUnwrapped}
  EasyDockSite, //CreateUniqueComponentName
{$ENDIF}
  LCLProc;

{ TMasterSite }

procedure TMasterSite.buCreateFormClick(Sender: TObject);
begin
  DockMaster.CreateDockable('', True, True, True);
end;

procedure TMasterSite.buDumpClick(Sender: TObject);
begin
  DockMaster.DumpSites;
end;

procedure TMasterSite.buRestoreClick(Sender: TObject);
begin
  if ms <> nil then begin
    ms.Position := 0; //rewind
    DockMaster.LoadFromStream(ms);
  end;
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
var
  f: TWinControl;
begin
  DockMaster := TDockMaster.Create(self); //(Application)?
  DockMaster.AddElasticSites(self, [alBottom]);
  f := DockMaster.CreateDockable('', True);
  //f.Visible := True; //???
  if f.HostDockSite <> nil then
    f := f.HostDockSite;
  f.Top := 300;
  f.Show;
  DebugLn('Clients: %d', [f.ControlCount]);
{$IFDEF TestUnwrapped}
  f := DockMaster.CreateDockable('', True, False);
  if f.Name = '' then //name it - for unique caption
    f.Name := CreateUniqueComponentName(f.ClassName, f.Owner);
{$ELSE}
  f := DockMaster.CreateDockable('', False);
  if f.HostDockSite <> nil then
    f := f.HostDockSite;
  f.Top := 600;
{$ENDIF}
  f.Show;
end;

{$R *.lfm}

initialization
  {.$I fmastersite.lrs}

end.

