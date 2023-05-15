unit fraprojectdelphioptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,
  LazLoggerBase, LazFileUtils, LazUTF8,
  // IdeIntf
  LazIDEIntf, ProjectIntf, CompOptsIntf, IDEOptionsIntf, IDEOptEditorIntf,
  delphioptions, strdelphitool, delphitool;

type

  { TProjectDelphiOptionsFrame }

  TProjectDelphiOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbAdditionalOptions: TComboBox;
    cbGenConfigFile: TCheckBox;
    lblAdditionalOptions: TLabel;
  private

  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectDelphiOptionsFrame }

function TProjectDelphiOptionsFrame.GetTitle: string;
begin
  Result:=SDelphiLocalizedParserName;
end;

procedure TProjectDelphiOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  lblAdditionalOptions.Caption:=SDelphiCompilerArgs;
  cbGenConfigFile.Caption:=SGenerateConfigFileCaption;
end;

procedure TProjectDelphiOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
Var
  Prj : TLazProject;

begin
  if AOptions=nil then ;
  Prj:=LazarusIDE.ActiveProject;
  cbGenConfigFile.Checked:=Prj.GenerateDelphiConfigFile;
  cbAdditionalOptions.Text:=Prj.AdditionalDelphiOptions;
end;

procedure TProjectDelphiOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
Var
  Prj : TLazProject;

begin
  if AOptions=nil then ;
  Prj:=LazarusIDE.ActiveProject;
  Prj.GenerateDelphiConfigFile:=cbGenConfigFile.Checked;
  Prj.AdditionalDelphiOptions:=cbAdditionalOptions.Text;
end;

class function TProjectDelphiOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEProjectOptions;
end;

end.

