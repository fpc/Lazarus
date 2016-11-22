unit ManifestDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, LazarusIDEStrConsts, W32Manifest, LCLIntf;

type

  { TManifestForm }

  TManifestForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DpiAwareComboBox: TComboBox;
    DpiAwareLabel: TLabel;
    DescEdit: TEdit;
    DescLabel: TLabel;
    NameEdit: TEdit;
    ExecutionLevelComboBox: TComboBox;
    ExecutionLevelLabel: TLabel;
    NameLabel: TLabel;
    UIAccessCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private

  public

  end;

var
  ManifestForm: TManifestForm;

implementation

{$R *.lfm}

{ TManifestForm }

procedure TManifestForm.FormCreate(Sender: TObject);
var
  DpiLevel: TXPManifestDpiAware;
  DpiLevelNames: array[TXPManifestDpiAware] of string;
  ExecLevel: TXPManifestExecutionLevel;
  ExecLevelNames: array[TXPManifestExecutionLevel] of string;
begin
  Caption := dlgPOManifestOptions;

  DpiLevelNames[xmdaFalse] := dlgPODpiAwarenessOff;
  DpiLevelNames[xmdaTrue] := dlgPODpiAwarenessOn;
  DpiLevelNames[xmdaPerMonitor] := dlgPODpiAwarenessOldOffNewPerMonitor;
  DpiLevelNames[xmdaTruePM] := dlgPODpiAwarenessOldOnNewPerMonitor;

  ExecLevelNames[xmelAsInvoker] := dlgPOAsInvoker;
  ExecLevelNames[xmelHighestAvailable] := dlgPOHighestAvailable;
  ExecLevelNames[xmelRequireAdministrator] := dlgPORequireAdministrator;

  DpiAwareLabel.Caption := dlgPODpiAwareness;
  ExecutionLevelLabel.Caption := dlgPOExecutionLevel;
  UIAccessCheckBox.Caption := dlgPOUIAccess;
  NameLabel.Caption := lisName;
  DescLabel.Caption := lisCodeHelpDescrTag;

  ExecutionLevelComboBox.Items.Clear;
  for ExecLevel in TXPManifestExecutionLevel do
    ExecutionLevelComboBox.Items.Add(ExecLevelNames[ExecLevel]);

  DpiAwareComboBox.Items.Clear;
  for DpiLevel in TXPManifestDpiAware do
    DpiAwareComboBox.Items.Add(DpiLevelNames[DpiLevel] + ' (' + ManifestDpiAwareValues[DpiLevel] + ')');

  ButtonPanel1.OKButton.Caption := lisOk;
  ButtonPanel1.CancelButton.Caption := lisCancel;
  ButtonPanel1.HelpButton.Caption := lisHelp;
end;

procedure TManifestForm.HelpButtonClick(Sender: TObject);
begin
  OpenURL('http://wiki.lazarus.freepascal.org/IDE_Window:__Project_Options');
end;

end.

