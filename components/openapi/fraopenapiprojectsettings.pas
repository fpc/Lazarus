unit fraopenapiprojectsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, EditBtn,
  // IdeIntf
  LazIDEIntf, ProjectIntf, CompOptsIntf, IDEOptionsIntf, IDEOptEditorIntf,
  // Openapi
  lazopenapistr, lazopenapictrl
  ;

type

  { TLazOpenAPIProjectOptions }

  TLazOpenAPIProjectOptions = class(TAbstractIDEOptionsEditor)
    FEOpenAPI: TFileNameEdit;
    FEBaseUnitFile: TFileNameEdit;
    FEConfig: TFileNameEdit;
    lblBaseUnitFile: TLabel;
    lblConfig: TLabel;
    lblOpenAPIFile: TLabel;
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

{ TLazOpenAPIProjectOptions }

function TLazOpenAPIProjectOptions.GetTitle: string;
begin
  Result:=SOpenAPIProjectOptionsCaption;
end;

procedure TLazOpenAPIProjectOptions.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  lPath : String;
begin
  lPath:=ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
  FEConfig.InitialDir:=lPath;
  FEBaseUnitFile.InitialDir:=lPath;
  FEOpenAPI.InitialDir:=lPath;
end;

procedure TLazOpenAPIProjectOptions.ReadSettings(AOptions: TAbstractIDEOptions);
var
  lOpenAPI,lConfig,lBaseFile : String;
begin
  OpenAPIHandler.GetProjectData(LazarusIDE.ActiveProject,lConfig,lOpenAPI,lBaseFile);
  FEConfig.FileName:=lConfig;
  FEOpenAPI.FileName:=lOpenAPI;
  FEBaseUnitFile.FileName:=lBaseFile;
end;

procedure TLazOpenAPIProjectOptions.WriteSettings(AOptions: TAbstractIDEOptions);
var
  lOpenAPI,lConfig,lBaseFile : String;
begin
  lConfig:=FEConfig.FileName;
  lOpenAPI:=FEOpenAPI.FileName;
  lBaseFile:=FEBaseUnitFile.FileName;
  OpenAPIHandler.SetProjectData(LazarusIDE.ActiveProject,lConfig,lOpenAPI,lBaseFile);

end;

class function TLazOpenAPIProjectOptions.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEProjectOptions;
end;

end.

