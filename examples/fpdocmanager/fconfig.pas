unit fConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, CheckLst;

type

  { TCfgWizard }

  TCfgWizard = class(TForm)
    buBack: TButton;
    buNext: TButton;
    buFclBat: TButton;
    buSelFpcDocs: TButton;
    buSelRoot: TButton;
    buDownload: TButton;
    buSelFpc: TButton;
    Button1: TButton;
    buCancel: TButton;
    buRtlBat: TButton;
    buLazDir: TButton;
    buScanFcl: TButton;
    swDirs: TCheckListBox;
    swFCLads: TCheckBox;
    edFpcDir: TEdit;
    edFpcDocs: TEdit;
    edLazDir: TEdit;
    edRtlBat: TEdit;
    edRoot: TEdit;
    edFclBat: TEdit;
    Label1: TLabel;
    dlgSelRoot: TSelectDirectoryDialog;
    Label2: TLabel;
    Label3: TLabel;
    dlgOpen: TOpenDialog;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    Steps: TPageControl;
    sb: TStatusBar;
    SelRoot: TTabSheet;
    SelFPDir: TTabSheet;
    MkRTL: TTabSheet;
    MkLCL: TTabSheet;
    MkFCL: TTabSheet;
    procedure buBackClick(Sender: TObject);
    procedure buFclBatClick(Sender: TObject);
    procedure buLazDirClick(Sender: TObject);
    procedure buNextClick(Sender: TObject);
    procedure buRtlBatClick(Sender: TObject);
    procedure buScanFclClick(Sender: TObject);
    procedure buSelFpcClick(Sender: TObject);
    procedure buSelFpcDocsClick(Sender: TObject);
    procedure buSelRootClick(Sender: TObject);
    procedure edFpcDirChange(Sender: TObject);
    procedure edFpcDocsChange(Sender: TObject);
    procedure edLazDirChange(Sender: TObject);
    procedure edRootChange(Sender: TObject);
    procedure edRtlBatChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MkFCLShow(Sender: TObject);
    procedure MkLCLShow(Sender: TObject);
    procedure MkRTLShow(Sender: TObject);
    procedure SelFPDirShow(Sender: TObject);
    procedure SelRootShow(Sender: TObject);
    procedure swDirsExit(Sender: TObject);
    procedure swFCLadsChange(Sender: TObject);
  private
    NoRun: boolean;
    procedure GetFclDirs;
    procedure PutFclDirs;
  public
    { public declarations }
  end; 

var
  CfgWizard: TCfgWizard;

implementation

uses
  uManager;
var
  FclPkg: TDocPackage;

{$R *.lfm}

{ TCfgWizard }

procedure TCfgWizard.buSelRootClick(Sender: TObject);
begin
  dlgSelRoot.Title := 'Documentation Root Directory';
  if not dlgSelRoot.Execute then
    exit;
  edRoot.Text := AppendPathDelim(dlgSelRoot.FileName);
  //buNext.Enabled := True;
end;

procedure TCfgWizard.edFpcDirChange(Sender: TObject);
begin
  if NoRun then exit;
  Manager.FpcDir := edFpcDir.Text;
end;

procedure TCfgWizard.edFpcDocsChange(Sender: TObject);
begin
  if NoRun then exit;
  Manager.FpcDocDir := edFpcDocs.Text;
  buNext.Enabled := edFpcDocs.Text <> '';
end;

procedure TCfgWizard.edLazDirChange(Sender: TObject);
begin
  if NoRun then exit;
  if edLazDir.Text = '' then
    exit;
  Manager.LazarusDir := edLazDir.Text;
//import LazUtils
  Manager.ImportLpk(Manager.LazarusDir + 'components/lazutils/lazutils.lpk');
//import LCLBase
  Manager.ImportLpk(Manager.LazarusDir + 'lcl/lclbase.lpk');
end;

procedure TCfgWizard.edRootChange(Sender: TObject);
begin
  if NoRun then exit;
  Manager.RootDir:=edRoot.Text;
  buNext.Enabled := Manager.RootDir <> '';
end;

procedure TCfgWizard.FormShow(Sender: TObject);
begin
  //ModalResult:=mrOK; exits!!!
  Steps.ActivePage := SelRoot;
//init edits
  NoRun:=True;
    edRoot.Text := Manager.RootDir;

    edFpcDocs.Text := Manager.FpcDocDir;
    edFpcDir.Text := Manager.FpcDir;

    edRtlBat.Text := Manager.Packages.Values['rtl'];
    edFclBat.Text := Manager.Packages.Values['fcl'];

    edLazDir.Text := Manager.LazarusDir;

    swFCLads.Checked := Manager.IsExtended('fcl') <> '';
  NoRun:=False;
end;

procedure TCfgWizard.MkFCLShow(Sender: TObject);
var
  ok: boolean;
begin
  ok := (edFpcDir.Text <> '') and (edLazDir.Text <> '');
  ok := ok and DirectoryExists(edFpcDir.Text);
  ok := ok and DirectoryExists(edLazDir.Text);
  swFCLads.Enabled := ok;
  if not ok then begin
    swFCLads.Checked := False;
    ShowMessage('Please select FPC and Lazarus directories first!');
  end;
  GetFclDirs;
end;

procedure TCfgWizard.MkLCLShow(Sender: TObject);
begin
end;

procedure TCfgWizard.MkRTLShow(Sender: TObject);
begin
end;

procedure TCfgWizard.SelFPDirShow(Sender: TObject);
begin
  edFpcDir.Text := Manager.FpcDocDir;
  buBack.Enabled := True;
  //buNext.Enabled := FpcDocDir <> '';
end;

procedure TCfgWizard.buBackClick(Sender: TObject);
begin
  Steps.SelectNextPage(False);
end;

procedure TCfgWizard.buNextClick(Sender: TObject);
begin
  Steps.SelectNextPage(True);
end;

procedure TCfgWizard.edRtlBatChange(Sender: TObject);
var
  fn: string;
  ed: TEdit absolute Sender;
begin
  fn := ed.Text;
  if fn = '' then
    exit;
  //uCmdLine.CmdToPrj(fn);
  if not NoRun then
    Manager.ImportCmd(fn);
end;

procedure TCfgWizard.buFclBatClick(Sender: TObject);
begin
  dlgOpen.InitialDir := Manager.FpcDocDir;
  dlgOpen.Title := 'FCL.bat command file';
  if dlgOpen.Execute then
    edFclBat.Text := dlgOpen.FileName;
end;

procedure TCfgWizard.buLazDirClick(Sender: TObject);
begin
  dlgSelRoot.InitialDir := Manager.LazarusDir;
  dlgSelRoot.Title := 'Lazarus Directory';
  if dlgSelRoot.Execute then
    edLazDir.Text := AppendPathDelim(dlgSelRoot.FileName);
end;

procedure TCfgWizard.buRtlBatClick(Sender: TObject);
begin
  dlgOpen.InitialDir := Manager.FpcDocDir;
  dlgOpen.Title := 'RTL.bat command file';
  if dlgOpen.Execute then
    edRtlBat.Text := dlgOpen.FileName;
end;

procedure TCfgWizard.buSelFpcClick(Sender: TObject);
begin
  dlgSelRoot.Title := 'FPC Source Directory';
  if not dlgSelRoot.Execute then
    exit;
  edFpcDir.Text := AppendPathDelim(dlgSelRoot.FileName);
end;

procedure TCfgWizard.buSelFpcDocsClick(Sender: TObject);
begin
  dlgSelRoot.Title := 'FPC Documentation Source Directory';
  dlgSelRoot.InitialDir := edFpcDir.Text;
  if not dlgSelRoot.Execute then
    exit;
  edFpcDocs.Text := AppendPathDelim(dlgSelRoot.FileName);
end;

procedure TCfgWizard.SelRootShow(Sender: TObject);
begin
  edRoot.Text := Manager.RootDir;
  buBack.Enabled := False;
  buNext.Enabled := Manager.RootDir <> '';
end;

procedure TCfgWizard.swDirsExit(Sender: TObject);
begin
  if swDirs.Count > 0 then
    PutFclDirs;
end;

procedure TCfgWizard.swFCLadsChange(Sender: TObject);
begin
  Manager.UpdateFCL(swFCLads.Checked);
end;

procedure TCfgWizard.buScanFclClick(Sender: TObject);
var
  s: string;
begin
  s := Manager.FpcDir + 'packages' + DirectorySeparator;
  ListDirs(s, swDirs.Items); //dupes suppressed
  swDirs.Sorted := True; //sort?
  PutFclDirs;
end;

procedure TCfgWizard.GetFclDirs;
var
  i, il: integer;
  s: string;
  b: boolean;
  lst: TStrings;
begin
//read fcl.SrcDirs
  FclPkg := Manager.AddPackage('fcl');
  if FclPkg = nil then
    exit;
  lst := FclPkg.SrcDirs;
  if (lst = nil) or (lst.Count = 0) then begin
  //scan FCL
    buScanFclClick(nil);
    exit;
  end;
//read from config
  swDirs.Clear;
  for i := 0 to lst.Count - 1 do begin
    s := lst.Names[i];
    b := lst.ValueFromIndex[i] > '0';
    il := swDirs.Items.Add(s); //dupes?
    swDirs.Checked[il] := b;
  end;
end;

procedure TCfgWizard.PutFclDirs;
var
  i, il: integer;
  s: string;
  b: boolean;
  lst: TStrings;
const
  tf: array[boolean] of string = ('0','1');
begin
  if FclPkg = nil then
    exit;
  lst := FclPkg.SrcDirs;
  if (lst = nil) then
    exit;
  for i := 0 to swDirs.Count - 1 do begin
    s := swDirs.Items[i];
    b := swDirs.Checked[i];
    lst.Values[s] := tf[b];
  end;
  FclPkg.UpdateConfig;
end;

end.

