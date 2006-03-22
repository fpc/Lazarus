unit DockForm1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  DockForm2Unit, Buttons, Menus, LDockCtrl;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateNewFormButton: TButton;
    MainPopupMenu: TPopupMenu;
    procedure CreateNewFormButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    function CreateNewForm: TCustomForm;
  public
    DockerForm1: TLazControlDocker;
    DockingManager: TLazDockingManager;
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormPaint(Sender: TObject);
begin
  if Sender=nil then ;
  PaintBoundaries(Self,clBlue);
end;

function TMainForm.CreateNewForm: TCustomForm;
var
  DockForm: TDockFormX;
begin
  DockForm:=TDockFormX.Create(Self);
  Result:=DockForm;
  DockForm.Docker.Manager:=DockingManager;
  DockForm.Name:=DockForm.Docker.DockerName;
  DockForm.Docker.Name:='Docker'+DockForm.Name;
  DebugLn('TMainForm.CreateNewForm ',DockForm.Name,' ',DockingManager.FindDockerByControl(DockForm,nil).DockerName,' ',DockingManager.GetControlConfigName(DockForm));
end;

procedure TMainForm.CreateNewFormButtonClick(Sender: TObject);
begin
  if Sender=nil then ;
  CreateNewForm;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Form2: TCustomForm;
  Form3: TCustomForm;
begin
  if Sender=nil then ;
  DockingManager:=TLazDockingManager.Create(Self);
  DockerForm1:=TLazControlDocker.Create(Self);
  DockerForm1.Name:='DockerForm1';
  DockerForm1.Manager:=DockingManager;
  
  Form2:=CreateNewForm;
  DockingManager.Manager.InsertControl(Form2,alLeft,Self);

  Form3:=CreateNewForm;
  DockingManager.Manager.InsertControl(Form3,alBottom,Self);
  
  DockingManager.WriteDebugReport;
  
  DockerForm1.GetLayoutFromControl;
  DockerForm1.WriteConfigTreeDebugReport;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  DockingManager.Free;
end;

initialization
  {$I dockform1unit.lrs}

end.

