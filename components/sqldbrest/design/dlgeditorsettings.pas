unit dlgeditorsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TSchemaEditorSettingsDialog }

  TSchemaEditorSettingsDialog = class(TForm)
    BPSettings: TButtonPanel;
    cbDisableVersionCheck: TCheckBox;
    GBConnections: TGroupBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private

  public
    Procedure LoadSettings;
    Procedure SaveSettings;
  end;

var
  SchemaEditorSettingsDialog: TSchemaEditorSettingsDialog;

implementation

uses schemaeditorconf;

{$R *.lfm}

{ TSchemaEditorSettingsDialog }

procedure TSchemaEditorSettingsDialog.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TSchemaEditorSettingsDialog.LoadSettings;
begin
  cbDisableVersionCheck.Checked:=SchemaSettings.DisableMySQLVersionCheck;
end;

procedure TSchemaEditorSettingsDialog.SaveSettings;
begin
  SchemaSettings.DisableMySQLVersionCheck:=cbDisableVersionCheck.Checked;
  if SchemaSettings.CurrentFile<>'' then
    SchemaSettings.SaveToFile(SchemaSettings.CurrentFile);
end;

procedure TSchemaEditorSettingsDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrOK then
    SaveSettings;
end;

end.

