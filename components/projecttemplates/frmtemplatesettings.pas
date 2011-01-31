unit frmtemplatesettings; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ButtonPanel, ProjectTemplates;

type

  { TTemplateSettingsForm }

  TTemplateSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DETemplates: TDirectoryEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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

resourcestring
  STitle = 'Project templates settings';
  SDirect= '&Directory with templates:';

{$R *.lfm}

{ TTemplateSettingsForm }

procedure TTemplateSettingsForm.FormCreate(Sender: TObject);
begin
  Caption := STitle;
  Label1.Caption := SDirect;
end;

procedure TTemplateSettingsForm.OKButtonClick(Sender: TObject);
begin
  if (Templates.TemplateDir<>DETemplates.Directory) then
    FTemplates.Initialize(DETemplates.Directory);
end;

procedure TTemplateSettingsForm.SetTemplates(const AValue: TProjectTemplates);
begin
  FTemplates:=AValue;
  DEtemplates.Directory:=Ftemplates.TemplateDir;
end;

end.

