unit project_lazdoc_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, Project, IDEOptionsIntf, LazarusIDEStrConsts, IDEProcs;

type

  { TProjectLazDocOptionsFrame }

  TProjectLazDocOptionsFrame = class(TAbstractIDEOptionsEditor)
    LazDocAddPathButton: TBitBtn;
    LazDocBrowseButton: TButton;
    LazDocDeletePathButton: TBitBtn;
    LazDocListBox: TListBox;
    LazDocPathEdit: TEdit;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocBrowseButtonClick(Sender: TObject);
    procedure LazDocDeletePathButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TProjectLazDocOptionsFrame }

procedure TProjectLazDocOptionsFrame.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TProjectLazDocOptionsFrame.LazDocDeletePathButtonClick(Sender: TObject);
begin
  if (LazDocListBox.ItemIndex >= 0) then
    LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
end;

procedure TProjectLazDocOptionsFrame.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
end;

function TProjectLazDocOptionsFrame.GetTitle: string;
begin
  Result := lisFPDocEditor;
end;

procedure TProjectLazDocOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  LazDocAddPathButton.Caption := lisCodeHelpAddPathButton;
  LazDocDeletePathButton.Caption := lisCodeHelpDeletePathButton;
  LazDocDeletePathButton.LoadGlyphFromLazarusResource('laz_delete');
  LazDocAddPathButton.LoadGlyphFromLazarusResource('laz_add');

  LazDocPathEdit.Clear;
end;

procedure TProjectLazDocOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do
    SplitString(LazDocPaths, ';', LazDocListBox.Items, True);
end;

procedure TProjectLazDocOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TProject do
    LazDocPaths := StringListToText(LazDocListBox.Items, ';', True);
end;

class function TProjectLazDocOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProject;
end;

initialization
  RegisterIDEOptionsEditor(GroupProject, TProjectLazDocOptionsFrame, ProjectOptionsLazDoc);

end.

