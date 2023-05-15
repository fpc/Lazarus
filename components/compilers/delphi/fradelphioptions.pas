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
    cbConvertDosToUnix: TCheckBox;
    lblConfigFileExtension: TLabel;
    lblAdditionalOptions: TLabel;
    DelphiPathBrowseButton: TButton;
    cbDelphiPath: TComboBox;
    lblDelphiPath: TLabel;
    procedure DelphiPathBrowseButtonClick(Sender: TObject);
  private

  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

uses dialogs, strdelphitool;

{$R *.lfm}

{ TDelphiOptionsFrame }

procedure TDelphiOptionsFrame.DelphiPathBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=SSelectDelphiExecutable;
    OpenDialog.FileName:=cbDelphiPath.Text;
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(cbDelphiPath,AFilename,cstFilename,30);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TDelphiOptionsFrame.GetTitle: String;
begin
  Result:=SDelphiLocalizedParserName;
end;

procedure TDelphiOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lblDelphiPath.Caption:=SDelphiCompilerFileNameCaption;
  lblConfigFileExtension.Caption:=SConfigFileExtensionCaption;
  cbConvertDosToUnix.Caption:=SConvertDosToUnixCaption;
  cbConvertDosToUnix.Enabled:={$IFDEF UNIX}True{$ELSE}False{$ENDIF};
  lblAdditionalOptions.Caption:=SDelphiCompilerArgs;
end;

procedure TDelphiOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

var
  Opts : TDelphiToolOptions;

begin
  Opts:=DelphiToolOptions;
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

