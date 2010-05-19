unit fEditorSite;
(* EditorSite by DoDi <DrDiettrich1@aol.com>
Mimics an Delphi editor window, that allows to dock other windows to it,
with several extensions:
- optionally enlarging the window
- detach a page into a new editor window
- move a page into a different editor window


Some quirks should be handled properly in a true IDE implementation:

For simplicity an IDE main menu has been added to the main window,
that allows to create several project (View) window dummies,
which can be docked to each other, or to the editor window.

Mixed docking of editor pages and View windows (currently) is not blocked,
so that you can have multiple edit views within the editor window.

Secondary editor windows should have the same docking capabilities.
(not yet)

Known bugs:
- The IDE suspects dangling references - KEEP these references!
  Please report if you know how to fix this issue.

+ Problems with non-form project windows.
  Since the IDE windows are all forms, we only handle this case now.
*)

{$mode objfpc}{$H+}

{$DEFINE minimize} //test application minimize/restore

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus,
  //fElasticSite,
  fEditBook,
  fEditForm;

type
  //TEditorSite = class(TDockingSite)
  TEditorSite = class(TForm)
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    mnRestore: TMenuItem;
    mnMinimize: TMenuItem;
    mnWindowDump: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnView: TMenuItem;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnExit: TMenuItem;
    mnOpen: TMenuItem;
    mnFile: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure mnMinimizeClick(Sender: TObject);
    procedure mnRestoreClick(Sender: TObject);
    procedure mnWindowDumpClick(Sender: TObject);
    procedure ViewMenuClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
  private
    FEdit: TEditBook; //to become a frame!
    CurEdit: TEditPage;
  public
    function CreateDockable(const cap: string): TWinControl;
    function OpenFile(const FileName: string): TObject;
  end;

var
  EditorSite: TEditorSite;

implementation

uses
  LCLProc,
  uMiniRestore,
  fClientForm,
  fFloatingSite;

{ TEditorSite }

procedure TEditorSite.FormCreate(Sender: TObject);
begin
(* EditBook should be a frame!
*)
  FEdit := TEditBook.Create(self);
  FEdit.Align := alClient;
  FEdit.BorderStyle := bsNone;
  FEdit.Parent := self;
  FEdit.Visible := True;
  FEdit.DragMode := dmManual; //disallow undocking
  FEdit.StayDocked := True;
end;

function TEditorSite.CreateDockable(const cap: string): TWinControl;
var
  Site: TFloatingSite;
  Client: TViewWindow;
begin
(* Create a client form, and dock it into a floating dock host site.
  We must force docking here, later the client will dock itself into
  a float host site, when it becomes floating.
*)
//create the form
  Client := TViewWindow.Create(Application);
  Client.Label1.Caption := cap;
  Client.Visible := True;
//name it
  Client.Caption := cap;
  try
    Client.Name := StringReplace(cap, ' ', '', [rfReplaceAll]);
  except
    //here: simply ignore duplicate name
  end;
  //Client.FloatingDockSiteClass := TFloatingSite;
  Site := TFloatingSite.Create(Application);
  Client.ManualDock(Site, nil, alClient);
  Site.Show;
  Result := Client;
end;

procedure TEditorSite.ViewMenuClick(Sender: TObject);
var
  item: TMenuItem absolute Sender;
begin
(* Create a dummy window from the menu item name.
*)
  CreateDockable(item.Caption);
end;

function TEditorSite.OpenFile(const FileName: string): TObject;
begin
//todo: load the file
  CurEdit := TEditPage.Create(self);
  CurEdit.LoadFile(FileName);
  CurEdit.ManualDock(FEdit);
//make it visible
  Result := CurEdit; //or what?
end;

procedure TEditorSite.mnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TEditorSite.mnExitClick(Sender: TObject);
begin
  Close;
end;


// ----------- application window handling -------------

procedure TEditorSite.FormActivate(Sender: TObject);
begin
  //DebugLn('--- Activate');
end;

procedure TEditorSite.FormDeactivate(Sender: TObject);
begin
  //DebugLn('--- Deactivate');
end;

procedure TEditorSite.FormHide(Sender: TObject);
begin
  //DebugLn('--- FormHide'); //not when minimized manually (win32)
  //mnMinimizeClick(Sender);
end;

procedure TEditorSite.FormResize(Sender: TObject);
begin
  //DebugLn('--- Resize');
end;

procedure TEditorSite.FormWindowStateChange(Sender: TObject);
begin
{$IFDEF minimize}
  //test manual Hide/Show
{$ELSE}
  DoMiniRestore;
{$ENDIF}
end;

procedure TEditorSite.mnMinimizeClick(Sender: TObject);
var
  i: integer;
  f: TForm;
begin
{$IFDEF minimize}
  for i := 0 to Screen.FormCount - 1 do begin
    f := Screen.Forms[i];
    //if f = self then f.WindowState := wsMinimized else
    if (f <> self) and f.Visible and (f.WindowState = wsNormal) then begin
    //both changes together crash on Linux/gtk2 :-(
      f.Hide;
      //f.WindowState := wsMinimized;
    end;
  end;
{$ELSE}
{$ENDIF}
end;

procedure TEditorSite.mnRestoreClick(Sender: TObject);
var
  i: integer;
  f: TForm;
begin
{$IFDEF minimize}
  for i := 0 to Screen.FormCount - 1 do begin
    f := Screen.Forms[i];
    //if f = self then f.WindowState := wsMinimized else
    if not f.Visible and (f.WindowState = wsNormal) then begin
    //both changes together crash on Linux/gtk2 :-(
      f.Show;
      //f.WindowState := wsNormal;
    end;
  end;
{$ELSE}
{$ENDIF}
end;

procedure TEditorSite.mnWindowDumpClick(Sender: TObject);
var
  i: integer;
  f: TWinControl;
  s: string;
begin
  DebugLn('--- CustomForms ---');
  for i := 0 to Screen.CustomFormCount - 1 do begin
    f := Screen.CustomForms[i];
    s := f.Name;
    while f.HostDockSite <> nil do begin
      f := f.HostDockSite;
      s := s + ' in ' + f.Name;
    end;
    DebugLn(s);
  end;
{ Seems to be the same list
}
  DebugLn('--- Forms ---');
  for i := 0 to Screen.FormCount - 1 do begin
    f := Screen.Forms[i];
    s := f.Name;
    while f.Parent <> nil do begin
      f := f.Parent;
      s := s + ' in ' + f.Name;
    end;
    DebugLn(s);
  end;
{}
  DebugLn('---');
end;

initialization
  {$I feditorsite.lrs}

end.

