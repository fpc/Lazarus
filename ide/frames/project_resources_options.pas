unit project_resources_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls,
  IDEOptionsIntf, Project, LazarusIDEStrConsts;

type

  { TResourcesOptionsFrame }

  TResourcesOptionsFrame = class(TAbstractIDEOptionsEditor)
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TResourcesOptionsFrame }

function TResourcesOptionsFrame.GetTitle: string;
begin
  Result := dlgPOResources;
end;

procedure TResourcesOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

procedure TResourcesOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin

end;

procedure TResourcesOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TResourcesOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TResourcesOptionsFrame, ProjectOptionsResources);

end.

