unit frmtemplatesettings; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, ProjectTemplates;

type

  { TTemplateSettingsForm }

  TTemplateSettingsForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    DETemplates: TDirectoryEdit;
    Label1: TLabel;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  SbtnCancel = 'Cancel';
  SbtnOk = 'OK';

{$R *.lfm}

{ TTemplateSettingsForm }

procedure TTemplateSettingsForm.BOKClick(Sender: TObject);

begin
  If (Templates.TemplateDir<>DETemplates.Directory) then
    FTemplates.Initialize(DETemplates.Directory);
end;

procedure TTemplateSettingsForm.FormCreate(Sender: TObject);
begin
  Caption := STitle;
  Label1.Caption := SDirect;
  BCancel.Caption:= SbtnCancel;
  BOK.Caption:= SbtnOk;
end;

procedure TTemplateSettingsForm.SetTemplates(const AValue: TProjectTemplates);
begin
  FTemplates:=AValue;
  DEtemplates.Directory:=Ftemplates.TemplateDir;
end;

end.

