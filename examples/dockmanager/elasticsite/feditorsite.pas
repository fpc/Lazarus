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

The View menu windows should be dockable to each other.
(Done, but the first dock clobbers the dock site - please redock)


Known bugs:
- The IDE suspects dangling references - KEEP these references!
  Please report if you know how to fix this issue.

- Initial docking is allowed only near the top/left edges of the window.
  This is a widgetset bug, that doesn't reflect the current extent of the window.
  This bug becomes more obvious when you enlarge the initially invisible docking
  panels at the sides of the EditorSite, using the splitters. These panels accept
  an first drop only in the original (designed) area. Later drops are accepted
  into the full extent of the panel.
*)

{$mode objfpc}{$H+}

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
  AutoExpand := True;
end;

function TEditorSite.CreateDockable(const cap: string): TPanel;
var
  Site: TFloatingSite;
  Client: TPanel;
begin
  //TDockingClient.Create(self);
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
{$IFnDEF old}
  Client.FloatingDockSiteClass := TFloatingSite;
  Client.ManualFloat(Rect(200,200, 400,400));
{$ELSE}
  Site := TFloatingSite.Create(Application);
  //Site.Visible := True;
  Client.ManualDock(Site, nil, alClient);
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

procedure TEditorSite.mnOpenClick(Sender: TObject);
begin
  //OpenFile('fMain.pas');
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
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

