unit DockForm1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
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
    Docker: TLazControlDocker;
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
begin
  Result:=TDockFormX.Create(Self);
  TDockFormX(Result).Docker.Manager:=DockingManager;
  TDockFormX(Result).Caption:=TDockFormX(Result).Docker.DockerName;
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
  Docker:=TLazControlDocker.Create(Self);
  Docker.Manager:=DockingManager;
  
  Form2:=CreateNewForm;
  DockingManager.Manager.InsertControl(Form2,alLeft,Self);

  Form3:=CreateNewForm;
  DockingManager.Manager.InsertControl(Form3,alBottom,Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  DockingManager.Free;
end;

initialization
  {$I dockform1unit.lrs}

end.

