{ IDE options frame for pas2js options

  Author: Mattias Gaertner
}
unit PJSDsgnOptsFrame;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, LazFileCache, LazConfigStorage, LazFileUtils, FileUtil,
  Forms, Controls, StdCtrls, Dialogs,
  IDEOptionsIntf, MacroIntf, BaseIDEIntf, IDEUtils,
  PJSDsgnOptions;

type
  { TPas2jsOptionsFrame }

  TPas2jsOptionsFrame = class(TAbstractIDEOptionsEditor)
    Pas2jsPathBrowseButton: TButton;
    Pas2jsPathComboBox: TComboBox;
    Pas2jsPathLabel: TLabel;
    procedure Pas2jsPathBrowseButtonClick(Sender: TObject);
  private
    function CheckCompiler(Buttons: TMsgDlgButtons): boolean;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TPas2jsOptionsFrame }

procedure TPas2jsOptionsFrame.Pas2jsPathBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: String;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    //InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:='Select pas2js executable';
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(Pas2jsPathComboBox,AFilename,cstFilename,30);
      CheckCompiler([mbOk]);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TPas2jsOptionsFrame.CheckCompiler(Buttons: TMsgDlgButtons): boolean;
var
  NewExe: string;
begin
  NewExe:=Pas2jsPathComboBox.Text;
  if NewExe=PJSOptions.CompilerFilename then exit(true);
  Result:=false;
  PJSOptions.CompilerFilename:=NewExe;
  // ToDo: check file
  //NewExe:=PJSOptions.GetParsedCompilerExe;
end;

function TPas2jsOptionsFrame.GetTitle: String;
begin
  Result:='Pas2JS';
end;

procedure TPas2jsOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  ExeName: String;
begin
  ExeName:=GetStandardPas2jsExe;
  Pas2jsPathLabel.Caption:='Path of '+ExeName;
  Pas2jsPathLabel.Hint:='You can use IDE macros like $MakeExe(). Without a full path, '+ExeName+' is searched in PATH.';
  Pas2jsPathBrowseButton.Caption:='...';
  Pas2jsPathBrowseButton.Hint:='Browse';
end;

procedure TPas2jsOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  SetComboBoxText(Pas2jsPathComboBox,PJSOptions.CompilerFilename,cstFilename,30);
end;

procedure TPas2jsOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  PJSOptions.CompilerFilename:=Pas2jsPathComboBox.Text;
end;

class function TPas2jsOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

