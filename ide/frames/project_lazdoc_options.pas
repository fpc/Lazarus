unit project_lazdoc_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, Dialogs, StdCtrls, Buttons, EditBtn,
  // LazUtils
  LazStringUtils,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf,
  // IDE
  Project, LazarusIDEStrConsts, CodeHelp;

type

  { TProjectFPDocOptionsFrame }

  TProjectFPDocOptionsFrame = class(TAbstractIDEOptionsEditor)
    AddPathButton: TBitBtn;
    DeletePathButton: TBitBtn;
    FPDocPackageNameEdit: TEdit;
    FPDocPackageNameLabel: TLabel;
    PathEdit: TDirectoryEdit;
    PathsListBox: TListBox;
    SearchPathsGroupBox: TGroupBox;
    procedure AddPathButtonClick(Sender: TObject);
    procedure DeletePathButtonClick(Sender: TObject);
    procedure PathsListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure PathEditChange(Sender: TObject);
  private
    function GetFPDocPkgNameEditValue: string;
  public
    function GetTitle: string; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
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
  FPDocPackageNameEdit.Hint := lisFPDocPackageNameDefaultIsProjectFileName;
  FPDocPackageNameLabel.Caption := lisFPDocPackageName;
  SearchPathsGroupBox.Caption := lisSearchPaths2;
  AddPathButton.Caption := lisCodeHelpAddPathButton;
  DeletePathButton.Caption := lisCodeHelpDeletePathButton;
  IDEImages.AssignImage(DeletePathButton, 'laz_delete');
  IDEImages.AssignImage(AddPathButton, 'laz_add');

  PathEdit.Clear;
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

procedure TProjectFPDocOptionsFrame.PathsListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  DeletePathButton.Enabled:=(Sender as TListBox).ItemIndex <> -1;
end;

procedure TProjectFPDocOptionsFrame.PathEditChange(Sender: TObject);
begin
  AddPathButton.Enabled:=PathEdit.Text<>'';
end;

function TProjectFPDocOptionsFrame.GetFPDocPkgNameEditValue: string;
begin
  Result:=MakeValidFPDocPackageName(FPDocPackageNameEdit.Text);
end;

procedure TProjectFPDocOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TProjectIDEOptions).Project do begin
    SplitString(FPDocPaths, ';', PathsListBox.Items, True);
    FPDocPackageNameEdit.TextHint:=lisDefaultPlaceholder;
    FPDocPackageNameEdit.Text:=FPDocPackageName;
  end;
end;

procedure TProjectFPDocOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with (AOptions as TProjectIDEOptions).Project do begin
    FPDocPaths := StringListToText(PathsListBox.Items, ';', True);
    FPDocPackageName:=GetFPDocPkgNameEditValue;
  end;
end;

class function TProjectFPDocOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectIDEOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectFPDocOptionsFrame, ProjectOptionsLazDoc);

end.

