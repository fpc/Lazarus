unit MenuEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, ExtCtrls, LMessages, DesignerMenu, Menus, GraphType,
  ComponentEditors;

type

  TMainMenuEditorForm = class(TForm)
  private
    FDesignerMainMenu: TDesignerMainMenu;
    FCanvas: TCanvas;
  public
    constructor CreateWithMenu(TheOwner: TComponent; AMenu: TMainMenu);
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDownClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
    property DesignerMainMenu: TDesignerMainMenu read FDesignerMainMenu
                                                 write FDesignerMainMenu;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TDefaultComponentEditor)
  private
    FMenu: TMainMenu;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    property Menu: TMainMenu read FMenu write FMenu;
  end;

implementation

{ TMainMenuEditorForm }

constructor TMainMenuEditorForm.CreateWithMenu(TheOwner: TComponent;
  AMenu: TMainMenu);
begin
  inherited Create(TheOwner);
  Canvas:=inherited Canvas;
  self.width:=800;
  self.height:=600;
  self.position:=poDesktopCenter;
  self.OnMouseDown:=@MouseDownClick;
  
  DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self,AMenu);
  with DesignerMainMenu do
  begin    
    LoadMainMenu;
    SetCoordinates(1,1,DesignerMainMenu.Root);
  end;
end;

destructor TMainMenuEditorForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMainMenuEditorForm.Paint;
begin
  inherited Paint;
  DesignerMainMenu.Erase(DesignerMainmenu.Root,Canvas);
  DesignerMainMenu.Draw(DesignerMainMenu.Root,Canvas);
end;

procedure TMainMenuEditorForm.MouseDownClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  DesignerMainMenu.MouseDownClick(DesignerMainMenu.Root,X,Y);
  Paint;
end;

{ TMainMenuComponentEditor}

constructor TMainMenuComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  m1: TMenuItem;
  m2: TMenuItem;
  m3: TMenuItem;
  m4: TMenuItem;
  m5: TMenuItem;
  m6: TMenuItem;
  m7: TMenuItem;
  m8: TMenuItem;
begin
  inherited Create(AComponent,ADesigner);
  Menu:=TMainMenu.Create(AComponent);
  m1:=TMenuItem.Create(AComponent);
  m1.Caption:='File';
  Menu.Items.Add(m1);
  m2:=TMenuItem.Create(AComponent);
  m2.Caption:='Power';
  Menu.Items.Add(m2);
  m3:=TMenuItem.Create(AComponent);
  m3.Caption:='Settings';
  Menu.Items.Add(m3);
  m4:=TMenuItem.Create(AComponent);
  m4.Caption:='New';
  m1.Add(m4);
  m5:=TMenuItem.Create(AComponent);
  m5.Caption:='Wizard';
  m1.Add(m5);
  m6:=TMenuItem.Create(AComponent);
  m6.Caption:='Project';
  m5.Add(m6);
  m7:=TMenuItem.Create(AComponent);
  m7.Caption:='Power On';
  m2.Add(m7);
  m8:=TMenuItem.Create(AComponent);
  m8.Caption:='Another Caption';
  m6.Add(m8);
end;

procedure TMainMenuComponentEditor.Edit;
var
  MainMenuEditorForm: TMainMenuEditorForm;
begin
  MainMenuEditorForm:=TMainMenuEditorForm.CreateWithMenu(Application,Menu);
  MainMenuEditorForm.ShowModal;
  MainMenuEditorForm.Free;
end;
{ //TMainMenuComponentEditor}

initialization
  RegisterComponentEditor(TMainMenu,TMainMenuComponentEditor);

end.
