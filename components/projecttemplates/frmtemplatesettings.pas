unit frmtemplatesettings; 

{$mode objfpc}{$H+}

interface

uses
  // LCL
  LclIntf, Forms, StdCtrls, EditBtn, ButtonPanel,
  // ProjectTemplates
  ProjectTemplates, ptstrconst;

type

  { TTemplateSettingsForm }

  TTemplateSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DETemplates: TDirectoryEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FTemplates : TProjectTemplates;
    procedure SetTemplates(const AValue: TProjectTemplates);
  public
    Property Templates : TProjectTemplates Read FTemplates Write SetTemplates;
  end;

var
  TemplateSettingsForm: TTemplateSettingsForm;

implementation

{$R *.lfm}

{ TTemplateSettingsForm }

procedure TTemplateSettingsForm.FormCreate(Sender: TObject);
begin
  Caption := STitle;
  Label1.Caption := SDirect;
end;

procedure TTemplateSettingsForm.HelpButtonClick(Sender: TObject);
begin
  OpenUrl('http://wiki.lazarus.freepascal.org/Project_Templates');
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

