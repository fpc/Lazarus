unit frmtemplatesettings; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ProjectTemplates;

type

  { TTemplateSettingsForm }

  TTemplateSettingsForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    DETemplates: TDirectoryEdit;
    Label1: TLabel;
    procedure BOKClick(Sender: TObject);
  private
    { private declarations }
    FTemplates : TProjectTemplates;
    procedure SetTemplates(const AValue: TProjectTemplates);
  public
    { public declarations }
    Property Templates : TProjectTemplates Read FTemplates Write SetTemplates;
  end;

var
  TemplateSettingsForm: TTemplateSettingsForm;

implementation

{ TTemplateSettingsForm }

procedure TTemplateSettingsForm.BOKClick(Sender: TObject);

begin
  If (Templates.TemplateDir<>DETemplates.Directory) then
    FTemplates.Initialize(DETemplates.Directory);
end;

procedure TTemplateSettingsForm.SetTemplates(const AValue: TProjectTemplates);
begin
  FTemplates:=AValue;
  DEtemplates.Directory:=Ftemplates.TemplateDir;
end;

initialization
  {$I frmtemplatesettings.lrs}

end.

