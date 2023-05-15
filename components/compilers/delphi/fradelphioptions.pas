unit fradelphioptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  // LazUtils
  LazFileCache, LazFileUtils, LazStringUtils, FileUtil,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, IDEDialogs,
  DelphiOptions;

type

  { TDelphiOptionsFrame }

  TDelphiOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbConfigFileExtension: TComboBox;
    cbAdditionalOptions: TComboBox;
    cbGenConfig: TCheckBox;
    cbConvertDosToUnix: TCheckBox;
    lblConfigFileExtension: TLabel;
    lblAdditionalOptions: TLabel;
    Pas2jsPathBrowseButton: TButton;
    cbDelphiPath: TComboBox;
    lblDelphiPath: TLabel;
  private

  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

uses strdelphitool;

{$R *.lfm}

{ TDelphiOptionsFrame }

function TDelphiOptionsFrame.GetTitle: String;
begin
  Result:=SDelphiToolOptionsTitle;
end;

procedure TDelphiOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lblDelphiPath.Caption:=SDelphiCompilerFileNameCaption;
  lblConfigFileExtension.Caption:=SConfigFileExtensionCaption;
  cbGenConfig.Caption:=SGenerateConfigFileCaption;
  cbConvertDosToUnix.Caption:=SConvertDosToUnixCaption;
  cbConvertDosToUnix.Enabled:={$IFDEF UNIX}True{$ELSE}False{$ENDIF};
  lblAdditionalOptions.Caption:=SDelphiCompilerArgs;
end;

procedure TDelphiOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

var
  Opts : TDelphiToolOptions;

begin
  Opts:=DelphiToolOptions;
  cbGenConfig.Checked:=Opts.GenerateConfigFile;
  cbConvertDosToUnix.Checked:=Opts.ConvertPathsToUnix;
  cbDelphiPath.Text:=Opts.CompilerFileName;
  cbConfigFileExtension.Text:=Opts.ConfigFileExtension;
  cbAdditionalOptions.Text:=Opts.AdditionalOptions;
end;

procedure TDelphiOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Opts : TDelphiToolOptions;

begin
  Opts:=DelphiToolOptions;
  Opts.GenerateConfigFile:=cbGenConfig.Checked;
  Opts.ConvertPathsToUnix:=cbConvertDosToUnix.Checked;
  Opts.CompilerFileName:=cbDelphiPath.Text;
  Opts.ConfigFileExtension:=cbConfigFileExtension.Text;
  Opts.AdditionalOptions:=cbAdditionalOptions.Text;
  Opts.Save;
end;

class function TDelphiOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

