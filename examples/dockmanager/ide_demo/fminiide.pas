unit fMiniIde;
(* IDE main bar (rudimentary)

Problems:
  View window names are not derived from class name.
  EditBooks are not managed sites(?)
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, SynEdit;

type
  TMainBar = class(TForm)
    buSave: TButton;
    buRestore: TButton;
    cbLayouts: TComboBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnExit: TMenuItem;
    mnFile: TMenuItem;
    mnMinimize: TMenuItem;
    mnOpen: TMenuItem;
    mnRestore: TMenuItem;
    mnView: TMenuItem;
    mnWindowDump: TMenuItem;
    OpenDialog1: TOpenDialog;
    dlgLayout: TSaveDialog;
    procedure buRestoreClick(Sender: TObject);
    procedure buSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnOpenClick(Sender: TObject);
    procedure mnWindowDumpClick(Sender: TObject);
    procedure ViewMenuClick(Sender: TObject);
  private
    procedure GetLayouts;
    procedure OpenFile(const AName: string);
  private //DockMaster callbacks
    function  OnReloadControl(const CtrlName: string; ASite: TWinControl): TControl;
    function  OnSaveControl(ACtrl: TControl): string;
  public
    function CreateDockable(const cap: string; fWrap: boolean = True): TWinControl;
  end;

var
  MainBar: TMainBar;

implementation

uses
  LCLProc,
  EasyDockSite,
  uMakeSite, fEditBook, fClientForm;

{ TMainBar }

procedure TMainBar.FormCreate(Sender: TObject);
begin
  TDockMaster.Create(self);
  DockMaster.OnSave := @OnSaveControl;
  DockMaster.OnRestore := @OnReloadControl;
  DockMaster.AddElasticSites(self, [alBottom]);
  GetLayouts;
end;

procedure TMainBar.GetLayouts;
var
  sr: TSearchRec;
begin
  if FindFirst('*.lyt', faAnyFile, sr) = 0 then begin
    repeat
      cbLayouts.Items.Add(sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure TMainBar.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainBar.mnOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TMainBar.mnWindowDumpClick(Sender: TObject);

  procedure DumpOwner(fo: TComponent);
  var
    i: integer;
    cmp: TComponent;
  begin
    for i := 0 to fo.ComponentCount - 1 do begin
      cmp := fo.Components[i];
      //if cmp.Name <> '' then
      begin
        DebugLn(cmp.Name, ': ', cmp.ClassName);
      end;
    end;
  end;

  procedure DumpForms;
  var
    i: integer;
    cmp: TComponent;
  begin
    DebugLn('- Forms');
    for i := 0 to Screen.FormCount - 1 do begin
      cmp := Screen.Forms[i];
      //if cmp.Name <> '' then
      begin
        DebugLn(cmp.Name, ': ', cmp.ClassName);
      end;
    end;
  end;

begin
//debug only
  DebugLn('--- Screen');
  DumpOwner(Screen);
  DumpForms;
{
  DebugLn('--- Owner');
  DumpOwner(DockMaster.Owner);
}
  DebugLn('--- end dump');
end;

procedure TMainBar.OpenFile(const AName: string);
var
  frm: TEditBook;
  ctl: TControl;
begin
(* Translate into layout Reload format
*)
//get editor form
  frm := nil;
  if MRUEdit <> nil then begin
  //use parent of last edit page
    ctl := MRUEdit.Parent;
    while ctl.Parent <> nil do
      ctl := ctl.Parent;
    if ctl is TEditBook then
      frm := TEditBook(ctl);
  end;
  if frm = nil then begin
    frm := TEditBook.Create(Application);
    frm.Visible := True;
  end;
  frm.OpenFile(AName);
end;

procedure TMainBar.ViewMenuClick(Sender: TObject);
var
  item: TMenuItem absolute Sender;
begin
(* Create a dummy window from the menu item name.
*)
  CreateDockable(item.Caption);
end;

procedure TMainBar.buRestoreClick(Sender: TObject);
var
  i: integer;
  s: string;
  f: TFileStream;
begin
  i := cbLayouts.ItemIndex;
  if i < 0 then begin
    beep;
    exit;
  end;
  s := cbLayouts.Items[i];
  f := TFileStream.Create(s, fmOpenRead);
  try
    DockMaster.LoadFromStream(f);
  finally
    f.Free;
  end;
end;

procedure TMainBar.buSaveClick(Sender: TObject);
var
  strm: TFileStream;
begin
(* Save layout.
  This should inlude ALL forms, not only the dockable ones!
*)
  if dlgLayout.Execute then begin
    strm := TFileStream.Create(dlgLayout.FileName, fmCreate);
    try
    //save non-dockable forms
    //save dockable forms and sites
      DockMaster.SaveToStream(strm);
    finally
      strm.Free;
    end;
    cbLayouts.AddItem(dlgLayout.FileName, nil); //Extract?
  end;
end;

function TMainBar.CreateDockable(const cap: string; fWrap: boolean): TWinControl;
var
  Client: TViewWindow;
  n: string;
begin
(* Create a client form, and dock it into a floating dock host site.
  We must force docking here, later the client will dock itself into
  a float host site, when it becomes floating.

  Should use: DockMaster.CreateDockable(TViewWindow) - RegisterClass!
*)
//lookup existing (assume single instance forms)
  n := StringReplace(cap, ' ', '', [rfReplaceAll]);
  Result := Screen.FindForm(n);
  if Result = nil then begin
  //create the form
    Client := TViewWindow.Create(Self);
    Client.Label1.Caption := cap;
  //name it
    Client.Caption := cap;
    TryRename(Client, n);
    DockMaster.MakeDockable(Client, fWrap, True);
    Client.Visible := True;
    Result := Client;
  end else begin
  //activate the existing form
    Result.Show; //might be invisible
    Result.SetFocus;
  end;
end;

(* Special load/store cases:
  ViewWindow: @<caption>
  EditForm/Book: special load/save
  EditPage?
*)
function TMainBar.OnReloadControl(const CtrlName: string;
  ASite: TWinControl): TControl;
var
  i: integer;
  lst: TStringList;
  s: string;
  eb: TEditBook absolute Result;
  ss: TStringStream;
begin
(* Handle special cases:
  ViewWindow
  EditBook
*)
  if CtrlName[1] = '@' then begin
    s := copy(CtrlName, 2, Length(CtrlName));
    Result := CreateDockable(s, False);
    //if Result <> nil then Result.Visible := True; //edit forms without pages are hidden?
  end else if ord(CtrlName[1]) = EditBookID then begin
    eb := TEditBook.Create(Application);
    ss := TStringStream.Create(CtrlName);
    try
      eb.LoadFromStream(ss);
    finally
      ss.Free;
    end;
  end else
    Result := nil; //for now
end;

function TMainBar.OnSaveControl(ACtrl: TControl): string;
var
  i: integer;
  ep: TEditPage;
  ss: TStringStream;
begin
(* handle special cases:
  ViewWindow
  EditBook
*)
  if ACtrl is TViewWindow then begin
    Result := '@' + ACtrl.Caption
  end else if ACtrl is TEditBook then begin
    ss := TStringStream.Create('');
    try
      TEditBook(ACtrl).SaveToStream(ss);
      Result := ss.DataString;
    finally
      ss.Free;
    end;
    //Result := ',' + TWinControl(ACtrl).GetDockCaption(ACtrl);
  end else
    Result := ''; //unhandled
    //Result := ACtrl.HostDockSite.GetDockCaption(ACtrl);
end;

{$R *.lfm}

end.

