unit fConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls;

type

  { TCfgWizard }

  TCfgWizard = class(TForm)
    buBack: TButton;
    buNext: TButton;
    buFclBat: TButton;
    buSelRoot: TButton;
    buDownload: TButton;
    buSelFpc: TButton;
    Button1: TButton;
    Button2: TButton;
    buCancel: TButton;
    buRtlBat: TButton;
    edFpcDir: TEdit;
    edRtlBat: TEdit;
    edRoot: TEdit;
    edFclBat: TEdit;
    Label1: TLabel;
    dlgSelRoot: TSelectDirectoryDialog;
    Label2: TLabel;
    Label3: TLabel;
    dlgOpen: TOpenDialog;
    Panel1: TPanel;
    Steps: TPageControl;
    sb: TStatusBar;
    SelRoot: TTabSheet;
    SelFPDir: TTabSheet;
    MkRTL: TTabSheet;
    procedure buBackClick(Sender: TObject);
    procedure buFclBatClick(Sender: TObject);
    procedure buNextClick(Sender: TObject);
    procedure buRtlBatClick(Sender: TObject);
    procedure buSelFpcClick(Sender: TObject);
    procedure buSelRootClick(Sender: TObject);
    procedure edFpcDirChange(Sender: TObject);
    procedure edRootChange(Sender: TObject);
    procedure edRtlBatChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelFPDirShow(Sender: TObject);
    procedure SelRootShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  CfgWizard: TCfgWizard;

implementation

uses
  uManager, uCmdLine;

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
  Manager.FpcDocDir := edFpcDir.Text;
  buNext.Enabled := edFpcDir.Text <> '';
end;

procedure TCfgWizard.edRootChange(Sender: TObject);
begin
  Manager.RootDir:=edRoot.Text;
  buNext.Enabled := Manager.RootDir <> '';
end;

procedure TCfgWizard.FormShow(Sender: TObject);
begin
  //ModalResult:=mrOK; exits!!!
  Steps.ActivePage := SelRoot;
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
  uCmdLine.CmdToPrj(fn);
end;

procedure TCfgWizard.buFclBatClick(Sender: TObject);
begin
  dlgOpen.InitialDir := Manager.FpcDocDir;
  dlgOpen.Title := 'FCL.bat command file';
  if dlgOpen.Execute then
    edFclBat.Text := dlgOpen.FileName;
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
  dlgSelRoot.Title := 'FPC Documentation Source Directory';
  if not dlgSelRoot.Execute then
    exit;
  edFpcDir.Text := AppendPathDelim(dlgSelRoot.FileName);
end;

procedure TCfgWizard.SelRootShow(Sender: TObject);
begin
  edRoot.Text := Manager.RootDir;
  buBack.Enabled := False;
  buNext.Enabled := Manager.RootDir <> '';
end;

end.

