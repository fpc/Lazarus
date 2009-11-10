unit fEditorSite;
(* EditorSite by DoDi <DrDiettrich1@aol.com>
mimics an Delphi editor window, that allows to
- dock other windows to it
- optionally enlarging the window
- detach a page into a new editor window
- move a page into a different editor window


Some quirks should be handled properly in a true IDE implementation:

For simplicity an IDE main menu has been added to the main window,
that allows to create several project window dummies,
which can be docked to the editor window.

Mixed docking of editor pages and View windows is not blocked, so that
you can have multiple edit views within the editor window.

Secondary editor windows should have the same docking capabilities.


Known bugs:
- The IDE suspects dangling references - KEEP these references!
  Please report if you know how to fix this issue.
*)

{$mode objfpc}{$H+}

{.$DEFINE stdfloat} //using standard floating host?

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus,
  fElasticSite, fEditBook, fEditForm;

type
  TEditorSite = class(TDockingSite)
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
    procedure FormCreate(Sender: TObject);
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
    function CreateDockable(const cap: string): TPanel;
    function OpenFile(const FileName: string): TObject;
  end;

var
  EditorSite: TEditorSite;

implementation

uses
  LCLProc,
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
  //FEdit.pnlDock.DragMode := dmManual;
  FAutoExpand := True;
end;

function TEditorSite.CreateDockable(const cap: string): TPanel;
var
  Site: TFloatingSite;
  Client: TPanel;
begin
  Client := TPanel.Create(self);
  Client.DragMode := dmAutomatic;
  Client.DragKind := dkDock;
  Client.Visible := True;
//name it
  Client.Caption := cap;
  try
    Client.Name := StringReplace(cap, ' ', '', [rfReplaceAll]);
  except
    //here: simply ignore duplicate name
  end;
{$IFDEF stdfloat}
  Client.ManualDock(nil);
{$ELSE}
  Client.FloatingDockSiteClass := TFloatingSite;
  {$IFDEF old}
  //ManualFloat doesn't work as expected :-(
    //Client.Align := alClient; //required for proper docking
    Client.ManualFloat(Rect(200,200, 400,400));
  {$ELSE}
    Site := TFloatingSite.Create(Application);
    Client.ManualDock(Site, nil, alClient);
  {$ENDIF}
{$ENDIF}
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

procedure TEditorSite.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TEditorSite.mnMinimizeClick(Sender: TObject);
begin
  self.WindowState := wsMinimized;
end;

procedure TEditorSite.mnRestoreClick(Sender: TObject);
begin
  WindowState := wsNormal;
end;

procedure TEditorSite.mnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
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

function TEditorSite.OpenFile(const FileName: string): TObject;
begin
//todo: load the file
  CurEdit := TEditPage.Create(self);
  CurEdit.LoadFile(FileName);
  CurEdit.ManualDock(FEdit);
//make it visible
  Result := CurEdit; //or what?
end;

initialization
  {$I feditorsite.lrs}

end.

