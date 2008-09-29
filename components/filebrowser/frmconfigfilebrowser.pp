unit frmconfigfilebrowser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  EditBtn,
  ButtonPanel;

type
  TStartDir = (sdProjectDir, sdLastOpened, sdCustomDir);


  TFileBrowserConfigForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DEInitial: TDirectoryEdit;
    GBStartDir: TGroupBox;
    RBThisDir: TRadioButton;
    RBLastDir: TRadioButton;
    RBUseProjectDir: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure SelectInitialDir(Sender: TObject);
  private
    function GetCustomDir: string;
    function GetStartDir: TStartDir;
    procedure SetCustomDir(const AValue: string);
    procedure SetStartDir(const AValue: TStartDir);
  public
    property StartDir: TStartDir read GetStartDir write SetStartDir;
    property CustomDir: string read GetCustomDir write SetCustomDir;
  end;

var
  FileBrowserConfigForm: TFileBrowserConfigForm;

implementation

{ TFileBrowserConfigForm }

procedure TFileBrowserConfigForm.SelectInitialDir(Sender: TObject);
begin
  DEinitial.Enabled := RBThisDir.Checked;
end;

procedure TFileBrowserConfigForm.FormShow(Sender: TObject);
begin
  SelectInitialDir(nil);
end;

function TFileBrowserConfigForm.GetCustomDir: string;
begin
  Result := DEinitial.Directory;
end;

function TFileBrowserConfigForm.GetStartDir: TStartDir;
begin
  if RBUseProjectDir.Checked then
    Result := sdProjectDir
  else if RBLastDir.Checked then
    Result := sdLastOpened
  else
    Result := sdCustomDir;
end;

procedure TFileBrowserConfigForm.SetCustomDir(const AValue: string);
begin
  DEinitial.Directory := AValue;
end;

procedure TFileBrowserConfigForm.SetStartDir(const AValue: TStartDir);
var
  RB: TRadioButton;
begin
  case AValue of
    sdProjectDir: RB := RBUseProjectDir;
    sdLastOpened: RB := RBLastDir;
    sdCustomDir: RB  := RBThisDir;
  end;
  RB.Checked := True;
end;

initialization
  {$I frmconfigfilebrowser.lrs}

end.

