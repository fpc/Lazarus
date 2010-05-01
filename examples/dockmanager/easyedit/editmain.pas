unit EditMain;
(* Multi-window editor example by DoDi <DrDiettrich1@aol.com>.

This example application only demonstrates the implementation and use of
multiple editor windows. It does not demonstrate any editing capabilities.

1. Open files from the menu.
2. Drag file tabs to dock edit pages somewhere else.

This is the first working version of dockable SynEdit components.

Done:
+ maintain a list of open editor windows (active window first)
+ open files in the (last) active editor window

*)

{$mode objfpc}{$H+}
{$DEFINE EditBook}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
{$IFDEF EditBook}
  fEditBook,
{$ELSE}
{$ENDIF}
  Menus, fDockBook, fEditForm, SynEdit;

type
{$IFDEF EditBook}
  TEditForm = TEditBook;
{$ELSE}
  TEditForm = TEasyDockBook;
{$ENDIF}
  TEasyEdit = TEditPage;

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnClose: TMenuItem;
    mnOpen: TMenuItem;
    mnuFile: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure mnCloseClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
  private
    //MyEdit: TEasyPages;
    CurForm: TEditForm;
    CurEdit: TEasyEdit;
  public
    function OpenFile(const AName: string): TObject;
  end; 

var
  MainForm: TMainForm;

implementation

uses
  uMakeSite;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TDockMaster.Create(self);
{
  MyEdit := TEasyPages.Create(self);
  MyEdit.Align := alClient;
  MyEdit.Parent := self;
}
end;

procedure TMainForm.mnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

function TMainForm.OpenFile(const AName: string): TObject;
begin
{$IFDEF EditBook}
  if MRUEdit = nil then
    MRUEdit := TEditForm.Create(self);
  CurForm := MRUEdit;
{$ELSE}
  if Editors = nil then
    Editors := TList.Create;
  if Editors.Count = 0 then begin
    CurForm := TEasyDockBook.Create(self);
    Editors.Add(CurForm);
  end;
  if CurForm = nil then
    pointer(CurForm) := Editors.Items[0];
{$ENDIF}
//todo: load the file
  CurEdit := TEasyEdit.Create(self);
  //CurEdit.FloatingDockSiteClass := TEasyDockBook; //provisions there???
  //if false then ManualFloat();
  //CurEdit.Lines.LoadFromFile(AName);
  CurEdit.LoadFile(AName);
  //CurEdit.ManualDock(nil);
  //CurEdit.ManualDock(CurForm, CurForm.pnlDock);
  CurEdit.ManualDock(CurForm);
//make it visible
  CurForm.Show;
  Result := CurEdit; //or what?
end;

initialization
  {$I editmain.lrs}

end.

