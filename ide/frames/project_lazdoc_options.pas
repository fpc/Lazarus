unit project_lazdoc_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, Project, IDEOptionsIntf, LazarusIDEStrConsts, IDEProcs,
  FPDocEditWindow, CodeHelp;

type

  { TProjectFPDocOptionsFrame }

  TProjectFPDocOptionsFrame = class(TAbstractIDEOptionsEditor)
    AddPathButton: TBitBtn;
    BrowseButton: TButton;
    DeletePathButton: TBitBtn;
    FPDocPackageNameEdit: TEdit;
    FPDocPackageNameLabel: TLabel;
    PathsListBox: TListBox;
    PathEdit: TEdit;
    SearchPathsGroupBox: TGroupBox;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure AddPathButtonClick(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure DeletePathButtonClick(Sender: TObject);
    procedure FPDocPackageNameEditEnter(Sender: TObject);
    procedure FPDocPackageNameEditExit(Sender: TObject);
    procedure PathsListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure PathEditChange(Sender: TObject);
  private
    function GetFPDocPkgNameEditValue: string;
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectFPDocOptionsFrame }

function TProjectFPDocOptionsFrame.GetTitle: string;
begin
  Result := lisFPDocEditor;
end;

procedure TProjectFPDocOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FPDocPackageNameEdit.Hint:=lisFPDocPackageNameDefaultIsProjectFileName;
  FPDocPackageNameLabel.Caption:=lisFPDocPackageName;
  AddPathButton.Caption := lisCodeHelpAddPathButton;
  DeletePathButton.Caption := lisCodeHelpDeletePathButton;
  DeletePathButton.LoadGlyphFromLazarusResource('laz_delete');
  AddPathButton.LoadGlyphFromLazarusResource('laz_add');

  PathEdit.Clear;
end;

procedure TProjectFPDocOptionsFrame.BrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    PathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TProjectFPDocOptionsFrame.AddPathButtonClick(Sender: TObject);
begin
  if PathEdit.Text <> '' then begin
    PathsListBox.Items.Add(PathEdit.Text);
    PathEdit.Text := '';
  end;
end;

procedure TProjectFPDocOptionsFrame.DeletePathButtonClick(Sender: TObject);
begin
  if (PathsListBox.ItemIndex >= 0) then begin
    PathsListBox.Items.Delete(PathsListBox.ItemIndex);
    PathsListBoxSelectionChange(PathsListBox, True);
  end;
end;

procedure TProjectFPDocOptionsFrame.FPDocPackageNameEditEnter(Sender: TObject);
begin
  if FPDocPackageNameEdit.Text=lisDefaultPlaceholder then
    FPDocPackageNameEdit.Text:='';
end;

procedure TProjectFPDocOptionsFrame.FPDocPackageNameEditExit(Sender: TObject);
begin
  if GetFPDocPkgNameEditValue='' then
    FPDocPackageNameEdit.Text:=lisDefaultPlaceholder
  else
    FPDocPackageNameEdit.Text:=GetFPDocPkgNameEditValue;
end;

procedure TProjectFPDocOptionsFrame.PathsListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  DeletePathButton.Enabled:=(Sender as TListBox).ItemIndex <> -1;
end;

procedure TProjectFPDocOptionsFrame.PathEditChange(Sender: TObject);
begin
  AddPathButton.Enabled:=(Sender as TEdit).Text <> '';
end;

function TProjectFPDocOptionsFrame.GetFPDocPkgNameEditValue: string;
begin
  if FPDocPackageNameEdit.Text=lisDefaultPlaceholder then
    Result:=''
  else
    Result:=MakeValidFPDocPackageName(FPDocPackageNameEdit.Text);
end;

procedure TProjectFPDocOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do begin
    SplitString(FPDocPaths, ';', PathsListBox.Items, True);
    if FPDocPackageName='' then
      FPDocPackageNameEdit.Text:=lisDefaultPlaceholder
    else
      FPDocPackageNameEdit.Text:=FPDocPackageName;
  end;
end;

procedure TProjectFPDocOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do begin
    FPDocPaths := StringListToText(PathsListBox.Items, ';', True);
    FPDocPackageName:=GetFPDocPkgNameEditValue;
  end;
end;

class function TProjectFPDocOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectFPDocOptionsFrame, ProjectOptionsLazDoc);

end.

