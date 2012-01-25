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
  Classes, SysUtils, FileUtil, SynHighlighterXML, SynEdit, Forms, Controls,
  Graphics, Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls,
  uManager;

type

  { TMain }

  TMain = class(TForm)
    buRefresh: TButton;
    buShowLog: TButton;
    buTest: TButton;
    buMakeDoc: TButton;
    buNewProfile: TButton;
    cbFormat: TComboBox;
    cbProfile: TComboBox;
    edOutput: TEdit;
    edOS: TEdit;
    edCPU: TEdit;
    edLang: TEdit;
    edMoDir: TEdit;
    edDefOut: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edBackend: TMemo;
    StatusBar1: TStatusBar;
    swOutput: TRadioButton;
    swDefOut: TRadioButton;
    swDocOpts: TCheckGroup;
    Label1: TLabel;
    swSortNodes: TCheckBox;
    optUpd: TCheckGroup;
    dlgSelLpk: TOpenDialog;
    edINI: TMemo;
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
    ViewFinal: TTabSheet;
    ViewINI: TTabSheet;
    Units: TPageControl;
    swAll: TRadioButton;
    swSingle: TRadioButton;
    edXML: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    ViewXML: TTabSheet;
    ViewUnits: TTabSheet;
    procedure buMakeDocClick(Sender: TObject);
    procedure buNewProfileClick(Sender: TObject);
    procedure buRefreshClick(Sender: TObject);
    procedure buTestClick(Sender: TObject);
    procedure cbFormatSelect(Sender: TObject);
    procedure cbProfileSelect(Sender: TObject);
    procedure edLogChange(Sender: TObject);
    procedure edOSExit(Sender: TObject);
    procedure edXMLExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbBackendExit(Sender: TObject);
    procedure lbPackagesClick(Sender: TObject);
    procedure lbUnitsClick(Sender: TObject);
    procedure mnConfigClick(Sender: TObject);
    procedure mnExitClick(Sender: TObject);
    procedure mnImportLpkClick(Sender: TObject);
    procedure optUpdItemClick(Sender: TObject; Index: integer);
    procedure swShowUpdateChange(Sender: TObject);
    procedure swSingleClick(Sender: TObject);
  private
    LogName: string;
    LogFile: TStream;
    Profile: string;
    procedure ProjectsChanged(Sender: TObject);
    procedure LogToFile(Sender: TObject; const msg: string);
    procedure LogToMsgBox(Sender: TObject; const msg: string);
    procedure LogStart;
    procedure LogDone;
    procedure ShowUpdate;
  {$IFDEF v0}
    procedure OnParseImport(Sender: TObject; var ASource, ALink: string);
  {$ELSE}
  {$ENDIF}
    procedure SaveOptions;
    procedure GetOptions;
    procedure GetEngines;
    procedure GetProfile;
    procedure SelectFormat(AFmt: string);
    procedure FormatSelected;
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
  dwlinear,
  dWriter;

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
  //UpdateDocs; //package objects seem to be missing?
  GetEngines;
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
  CanClose := True; //make compiler happy
  //Manager.SaveConfig; //what if fails?
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

procedure TMain.LogStart;
begin
  Screen.Cursor := crHourGlass;
  if LogName = '' then
    edLog.Text := Manager.RootDir + 'doclog.txt';
  LogFile.Free;
  LogFile := TFileStream.Create(LogName, fmCreate); //fmWrite
  Manager.OnLog := @LogToFile;
  StatusBar1.SimpleText := 'Starting...';
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
  Screen.Cursor := crDefault;
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
    LogFile.WriteBuffer(s[1], Length(s));
  //give immediate feedback
    StatusBar1.SimpleText := msg;
    Application.ProcessMessages;
  end
  else
    LogToMsgBox(Sender, msg);
end;

procedure TMain.LogToMsgBox(Sender: TObject; const msg: string);
begin
  ShowMessage(msg);
end;

{$IFDEF v0}
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
{$ELSE}
{$ENDIF}

type
  SkelOpts = (
    soDecl, soOverrides, soErrors, soSeeAlso, soArgs, soResults,
    soPriv, soProt, soClassSep
  );
procedure TMain.SaveOptions;
begin
  Manager.Options.WriteDeclaration := not optUpd.Checked[ord(soDecl)];
  Manager.Options.DisableOverride := optUpd.Checked[ord(soOverrides)];
  Manager.Options.DisableErrors := optUpd.Checked[ord(soErrors)];
  Manager.Options.DisableSeealso := optUpd.Checked[ord(soSeeAlso)];
  Manager.Options.DisableArguments := optUpd.Checked[ord(soArgs)];
  Manager.Options.DisableFunctionResults := optUpd.Checked[ord(soResults)];
  Manager.Options.ShowPrivate := not optUpd.Checked[ord(soPriv)];
  Manager.Options.DisableProtected := optUpd.Checked[ord(soProt)];
  Manager.Options.EmitClassSeparator := not optUpd.Checked[ord(soClassSep)];
  Manager.Options.SortNodes := swSortNodes.Checked;
  Manager.Options.Modified := True; //assume!
//Backend
  //if cbFormat.Caption = '' then exit; //no valid options
  if cbFormat.ItemIndex < 0 then
    SelectFormat('html');
  Manager.Options.Backend := cbFormat.Items.Names[cbFormat.ItemIndex];
  Manager.Options.StopOnParseError := swDocOpts.Checked[0];
  Manager.Options.WarnNoNode := swDocOpts.Checked[1];
  Manager.Options.HideProtected := swDocOpts.Checked[2];
  Manager.Options.ShowPrivate := swDocOpts.Checked[3];
  Manager.Options.InterfaceOnly := swDocOpts.Checked[4];
  Manager.Options.DontTrim := swDocOpts.Checked[5];
  Manager.Options.Verbose := swDocOpts.Checked[6];
  Manager.Options.OSTarget := edOS.Text;
  Manager.Options.CPUTarget := edCPU.Text;
  Manager.Options.Language := edLang.Text;
  Manager.Options.MoDir := edMoDir.Text;
  Manager.Options.BackendFromPairs(edBackend.Lines);
end;

procedure TMain.GetOptions;
begin
  optUpd.Checked[ord(soDecl)] := not Manager.Options.WriteDeclaration;
  optUpd.Checked[ord(soOverrides)] := Manager.Options.DisableOverride;
  optUpd.Checked[ord(soErrors)] := Manager.Options.DisableErrors;
  optUpd.Checked[ord(soSeeAlso)] := Manager.Options.DisableSeealso;
  optUpd.Checked[ord(soArgs)] := Manager.Options.DisableArguments;
  optUpd.Checked[ord(soResults)] := Manager.Options.DisableFunctionResults;
  optUpd.Checked[ord(soPriv)] := not Manager.Options.ShowPrivate;
  optUpd.Checked[ord(soProt)] := Manager.Options.DisableProtected;
  optUpd.Checked[ord(soClassSep)] := not Manager.Options.EmitClassSeparator;
  swSortNodes.Checked := Manager.Options.SortNodes;
//backend
  if Profile = '' then begin
    Profile:=Manager.Profile;
    cbProfile.Items.CommaText := Manager.Profiles;
    cbProfile.Caption := Profile;
  end;
  GetProfile;
end;

procedure TMain.GetProfile;
begin
  //if Profile = AName then exit; //nothing changed?
  //Manager.Profile := AName;
  //cbFormat.Caption := Manager.Options.Backend; //select from CB?
  SelectFormat(Manager.Options.Backend);
  swDocOpts.Checked[0] := Manager.Options.StopOnParseError;
  swDocOpts.Checked[1] := Manager.Options.WarnNoNode;
  swDocOpts.Checked[2] := Manager.Options.HideProtected;
  swDocOpts.Checked[3] := Manager.Options.ShowPrivate;
  swDocOpts.Checked[4] := Manager.Options.InterfaceOnly;
  swDocOpts.Checked[5] := Manager.Options.DontTrim;
//these should be global options?
  edOS.Text := Manager.Options.OSTarget;
  edCPU.Text := Manager.Options.CPUTarget;
  edLang.Text := Manager.Options.Language;
  edMoDir.Text := Manager.Options.MoDir;
//backend options
  Manager.Options.BackendToPairs(edBackend.Lines);
end;

procedure TMain.edLogChange(Sender: TObject);
begin
  LogName:=edLog.Text;
end;

procedure TMain.edOSExit(Sender: TObject);
var
  ed: TEdit absolute Sender;
begin
  if ed.Modified then begin
    SaveOptions;
    ed.Modified := False;
  end;
end;

procedure TMain.lbBackendExit(Sender: TObject);
begin
//Modified never True???
  if edBackend.Modified then begin
    SaveOptions;
    edBackend.Modified := False;
  end;
end;

procedure TMain.cbFormatSelect(Sender: TObject);
begin
  SaveOptions;
  FormatSelected;
end;

procedure TMain.SelectFormat(AFmt: string);
var
  i: integer;
begin
  i := cbFormat.Items.IndexOfName(AFmt);
  if i < 0 then
    i := cbFormat.Items.Count - 1;
  cbFormat.ItemIndex := i;
  FormatSelected;
end;

procedure TMain.FormatSelected;
var
  s: string;
begin
  if assigned(Manager.Package) then
    s := Manager.RootDir + Manager.Package.Name
  else
    s := '';
  edDefOut.Text := s;
end;

procedure TMain.GetEngines;
begin
//should separate: writers (format) and settings!
  dWriter.EnumWriters(cbFormat.Items);
  cbProfile.Items.CommaText := Manager.Profiles;
  cbProfile.Caption := Manager.Profile; //select???
  cbProfileSelect(cbProfile);
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
  for i := 0 to Manager.Packages.Count - 1 do begin
    if (Manager.Packages.ValueFromIndex[i] <> '')
    and (Manager.Packages.Objects[i] <> nil) then
      lbPackages.AddItem(Manager.Packages.Names[i], Manager.Packages.Objects[i]);
  end;
end;

procedure TMain.mnConfigClick(Sender: TObject);
begin
  Manager.BeginUpdate;
  if not assigned(CfgWizard) then
    CfgWizard := TCfgWizard.Create(self);
  if CfgWizard.ShowModal <> mrCancel then begin
    //all done
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
  Manager.Package := pkg;
  edDefOut.Text := Manager.RootDir + pkg.Name;
  fn := pkg.ProjectFile; //initialized where?
  if fn <> '' then begin
    if FileExists(fn) then
      edXML.Lines.LoadFromFile(fn)
    else
      edXML.Lines.Clear;
  end;
  fn := pkg.IniFileName;
  if FileExists(fn) then
    edINI.Lines.LoadFromFile(fn);
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
  pkName: string;
begin
  if not dlgSelLpk.Execute then
    exit;
  pkName:=dlgSelLpk.FileName;
  Manager.ImportLpk(pkName);
end;

procedure TMain.optUpdItemClick(Sender: TObject; Index: integer);
begin
  SaveOptions;
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

procedure TMain.cbProfileSelect(Sender: TObject);
begin
  Profile:=cbProfile.Caption;
  Manager.Profile := Profile;
  GetProfile;
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

procedure TMain.buNewProfileClick(Sender: TObject);
begin
  Profile := cbProfile.Caption;
  if Profile = '' then
    exit; //need name
  if cbProfile.Items.IndexOf(Profile) < 0 then begin
    cbProfile.AddItem(Profile, nil);
    Manager.AddProfile(Profile);
  end;
end;

procedure TMain.buMakeDocClick(Sender: TObject);
var
  res: boolean;
begin
  LogStart;
  if swDefOut.Checked then
    res := Manager.MakeDoc(Manager.Package, '', edDefOut.Text)
  else
    res := Manager.MakeDoc(Manager.Package, '', edOutput.Text);
  LogDone;
  if res then
    StatusBar1.SimpleText := 'Done :-)'
  else
    StatusBar1.SimpleText := 'There were errors, see log';
end;

end.

