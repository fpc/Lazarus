unit fMain;
(* Documentation manager GUI.
  View/Edit configuration, packages
  Add/Import packages
  Create/Update skeletons
  Create documentation (final, test)
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms,
  Controls, Graphics, Dialogs, Menus, StdCtrls, ComCtrls,
  uManager;

type

  { TMain }

  TMain = class(TForm)
    buSkel: TButton;
    buUpdate: TButton;
    buRefresh: TButton;
    buShowLog: TButton;
    buTest: TButton;
    dlgSelLpk: TOpenDialog;
    swShowUpdate: TCheckBox;
    edLog: TEdit;
    lbPackages: TComboBox;
    edUnit: TEdit;
    GroupBox1: TGroupBox;
    lbUnits: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    mnImportLpk: TMenuItem;
    mnPackage: TMenuItem;
    mnConfig: TMenuItem;
    MenuItem3: TMenuItem;
    mnExit: TMenuItem;
    dlgSelRoot: TSelectDirectoryDialog;
    Units: TPageControl;
    swAll: TRadioButton;
    swSingle: TRadioButton;
    edXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    ViewXML: TTabSheet;
    TabSheet2: TTabSheet;
    procedure buRefreshClick(Sender: TObject);
    procedure buTestClick(Sender: TObject);
    procedure edLogChange(Sender: TObject);
    procedure edXMLExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbPackagesClick(Sender: TObject);
    procedure lbUnitsClick(Sender: TObject);
    procedure mnConfigClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnImportLpkClick(Sender: TObject);
    procedure swShowUpdateChange(Sender: TObject);
    procedure swSingleClick(Sender: TObject);
  private
    LogName: string;
    LogFile: TStream;
    procedure ProjectsChanged(Sender: TObject);
    procedure LogToFile(Sender: TObject; const msg: string);
    procedure LogToMsgBox(Sender: TObject; const msg: string);
    procedure LogStart;
    procedure LogDone;
    procedure ShowUpdate;
    procedure OnParseImport(Sender: TObject; var ASource, ALink: string);
  public
    CurPkg: TDocPackage;
    CurUnit: string;
    procedure UpdateDocs;
  end;

var
  Main: TMain;

implementation

uses
  fConfig, fLogView, fUpdateView,
  //dw_HTML, //more writers?
  uLpk;

{$R *.lfm}

{ TMain }

procedure TMain.FormCreate(Sender: TObject);
var
  r: TRect;
  s: string;
  l: TStringList;
begin
  Manager := TFPDocManager.Create(self);
  Manager.OnChange := @ProjectsChanged;
  if not Manager.LoadConfig(GetCurrentDir) then begin
  //query root directory
    if not dlgSelRoot.Execute then
      exit;
    if not Manager.LoadConfig(dlgSelRoot.FileName, True) then begin
      //InitConfig(dlgSelRoot.FileName); ?
      mnConfigClick(self); //does an UpdateDocs
      exit; //nothing to init from?
    end;
  end;
//init...
  if Manager.Config.SectionExists('GUI') then begin
    s := Manager.Config.ReadString('GUI', 'position', '');
    if s <> '' then begin
      l := TStringList.Create;
      try
        l.DelimitedText := s;
        if l.Count = 4 then begin
          //SetBounds(l[0], l[1], );
          r.Left := StrToInt(l[0]);
          r.Top := StrToInt(l[1]);
          r.Right := StrToInt(l[2]);
          r.Bottom:= StrToInt(l[3]);
          BoundsRect := r;
        end;
      finally
        l.Free;
      end;
    end;
  end;
end;

procedure TMain.FormResize(Sender: TObject);
var
  r: TRect;
begin
  r := BoundsRect;
  Manager.Config.WriteString('GUI', 'position', Format('%d,%d,%d,%d',[r.Left, r.Top, r.Right, r.Bottom]));
end;

// --------------- events ------------------

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
//is this really required?
  //CanClose :=
  Manager.SaveConfig; //what if fails?
end;

procedure TMain.mnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMain.swShowUpdateChange(Sender: TObject);
begin
  if swShowUpdate.Checked then
    ShowUpdate;
end;

procedure TMain.edLogChange(Sender: TObject);
begin
  LogName:=edLog.Text;
end;

procedure TMain.LogStart;
begin
  if LogName = '' then
    edLog.Text := Manager.RootDir + 'doclog.txt';
  LogFile.Free;
  LogFile := TFileStream.Create(LogName, fmCreate); //fmWrite
  Manager.OnLog := @LogToFile;
  //Manager.OnImport:=@OnParseImport;
end;

procedure TMain.LogDone;
begin
  if not assigned(LogFile) then
    exit;
  FreeAndNil(LogFile);
  Manager.OnLog := @LogToMsgBox;
//view messages
  LogView.Caption := 'View ' + LogName;
  LogView.edLog.Lines.LoadFromFile(LogName); //direct log???
  LogView.Show;
end;

procedure TMain.ShowUpdate;
var
  v: TUpdateView;
  fn: string;
begin
  if not swShowUpdate.Checked then
    exit;
  if lbUnits.ItemIndex < 0 then
    exit;
  fn := 'upd.' + edUnit.Text + '.xml';
  if not FileExists(fn) then
    exit;
//problem with files kept open???
  if UpdateView = nil then
    v := TUpdateView.Create(self)
  else
    v := UpdateView;
  v.Caption := 'Update of ' + edUnit.Text;
  v.edUpdate.Lines.LoadFromFile(fn);
  v.Show;
end;

procedure TMain.LogToFile(Sender: TObject; const msg: string);
var
  s: string;
begin
  if assigned(LogFile) then begin
    s := msg + LineEnding;
    LogFile.WriteBuffer(s[1], Length(s))
  end
  else
    LogToMsgBox(Sender, msg);
end;

procedure TMain.LogToMsgBox(Sender: TObject; const msg: string);
begin
  ShowMessage(msg);
end;

procedure TMain.OnParseImport(Sender: TObject; var ASource, ALink: string);
var
  i: integer;
  pn: string;
begin
(* Provide ASource-->content file, ALink depends on format.
  ASource can be a package name (only), or a CSV spec.
  Sender is Manager.
An extended model could store a list of required packages,
and provide the list of imports.
*)
  i := Pos(',', ASource);
  if i > 0 then
    exit; //default format, handled by caller
  pn := ExtractFileNameOnly(ASource);
  ASource := Manager.RootDir + pn + '.xct';
  //if Manager.Options.??? - where's the output format?
  ALink := '../' + pn + '/';
end;

procedure TMain.ProjectsChanged(Sender: TObject);
begin
  UpdateDocs; //immediately or delayed (OnIdle?)
end;

procedure TMain.UpdateDocs;
var
  i: integer;
begin
  lbPackages.Clear;
  for i := 0 to Manager.Projects.Count - 1 do begin
    lbPackages.AddItem(Manager.Projects.Names[i], Manager.Projects.Objects[i]);
  end;
end;

procedure TMain.mnConfigClick(Sender: TObject);
begin
  Manager.BeginUpdate;
  if CfgWizard.ShowModal <> mrCancel then begin
    //UpdateDocs; - by Manager!?
    //Manager.Config.;
  end;
  Manager.EndUpdate;
end;

// ---------------- pages ------------------

procedure TMain.lbPackagesClick(Sender: TObject);
var
  i: integer;
  pkg: TDocPackage;
  fn: string;
begin
  i := lbPackages.ItemIndex; //clicked?
  pkg := lbPackages.Items.Objects[i] as TDocPackage;
  if pkg = nil then
    exit; //not really created?
  fn := pkg.ProjectFile; //initialized where?
  if fn <> '' then
    edXML.Lines.LoadFromFile(fn);
  //load units...
  lbUnits.Items.BeginUpdate;
  lbUnits.Clear;
  for i := 0 to pkg.Units.Count - 1 do begin
    //fn := Manager.UnitName(pkg.Inputs, i);
    fn := pkg.Units.Names[i];
    lbUnits.AddItem(fn, nil);
  end;
  lbUnits.Sorted := True;
  lbUnits.Items.EndUpdate;
//remember selection
  CurPkg := pkg;
  CurUnit:='';
end;

procedure TMain.mnImportLpkClick(Sender: TObject);
var
  pkName, pkPrj: string;
begin
  if not dlgSelLpk.Execute then
    exit;
  pkName:=dlgSelLpk.FileName;
  Manager.ImportLpk(pkName);
{
  Manager.BeginUpdate;
  try
    if not uLpk.ImportLpk(pkName) then begin
      LogToMsgBox(self, 'Import failed on ' + pkName);
      exit;
    end;
  //create project file - preprocess options!?
    pkPrj:=ChangeFileExt(pkName, '.xml');
    Manager.CreateProject(pkPrj, Manager.SelectedPackage);
  finally
    Manager.EndUpdate; //not modified???
  end;
//
}
end;

procedure TMain.edXMLExit(Sender: TObject);
begin
  if edXML.Modified then begin
    case MessageDlg('Project was changed', 'Save changes?',
      mtConfirmation, mbYesNoCancel, 0) of
    mrYes: edXML.Lines.SaveToFile(CurPkg.ProjectFile);
    mrNo: exit;
    else
      edXML.SetFocus;
    end;
  end;
end;

// ------------------- actions ----------------------

procedure TMain.lbUnitsClick(Sender: TObject);
var
  i: integer;
begin
  i := lbUnits.ItemIndex;
  if i < 0 then
    CurUnit := ''
  else
    CurUnit := lbUnits.Items[i];
  edUnit.Text := CurUnit; //further depends on swSingle
  swSingle.Checked := True; //assume test single unit
  ShowUpdate;
end;

procedure TMain.swSingleClick(Sender: TObject);
begin
  edUnit.Enabled := swSingle.Checked;
end;

(* FPDoc dry run, with logfile
*)
procedure TMain.buTestClick(Sender: TObject);
var
  u: string;
begin
  LogStart;
  if swSingle.Checked then
    u := CurUnit
  else
    u := '';
  Manager.TestRun(CurPkg, u);
  LogDone;
end;

procedure TMain.buRefreshClick(Sender: TObject);
var
  u: string;
begin
  LogStart;
  if swSingle.Checked then
    u := CurUnit
  else
    u := '';
  Manager.Update(CurPkg, u);
  LogDone;
end;

end.

