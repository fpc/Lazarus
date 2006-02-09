unit DockForm1Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  DockForm2Unit, Buttons, LDockTree, Menus, LDockCtrl;

type

  { TDockForm1 }

  TDockForm1 = class(TForm)
    CreateNewFormButton: TButton;
    MainPopupMenu: TPopupMenu;
    HideMenuItem: TMenuItem;
    procedure CreateNewFormButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure HideMenuItemClick(Sender: TObject);
  private
    function CreateNewForm: TCustomForm;
  public
    Docker: TLazControlDocker;
    DockingManager: TLazDockingManager;
  end;

var
  DockForm1: TDockForm1;

implementation

{ TDockForm1 }

procedure TDockForm1.FormPaint(Sender: TObject);
begin
  if Sender=nil then ;
  PaintBoundaries(Self,clBlue);
end;

procedure TDockForm1.HideMenuItemClick(Sender: TObject);
begin
  Hide;
end;

function TDockForm1.CreateNewForm: TCustomForm;
begin
  Result:=TDockFormX.Create(Self);
  TDockFormX(Result).Docker.Manager:=DockingManager;
  TDockFormX(Result).Caption:=TDockFormX(Result).Docker.DockerName;
end;

procedure TDockForm1.CreateNewFormButtonClick(Sender: TObject);
begin
  if Sender=nil then ;
  CreateNewForm;
end;

procedure TDockForm1.FormCreate(Sender: TObject);
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

procedure TDockForm1.FormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  DockingManager.Free;
end;

initialization
  {$I dockform1unit.lrs}

end.

