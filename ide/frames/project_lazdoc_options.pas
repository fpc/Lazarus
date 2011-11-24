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
    procedure LazDocListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure LazDocPathEditChange(Sender: TObject);
  private
    fLoaded: Boolean;
    FSaved: Boolean;
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

procedure TProjectLazDocOptionsFrame.LazDocBrowseButtonClick(Sender: TObject);
begin
  if SelectDirectoryDialog.Execute then
    LazDocPathEdit.Text := SelectDirectoryDialog.FileName;
end;

procedure TProjectLazDocOptionsFrame.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then begin
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
    LazDocPathEdit.Text := '';
  end;
end;

procedure TProjectLazDocOptionsFrame.LazDocDeletePathButtonClick(Sender: TObject);
begin
  if (LazDocListBox.ItemIndex >= 0) then begin
    LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
    LazDocListBoxSelectionChange(LazDocListBox, True);
  end;
end;

procedure TProjectLazDocOptionsFrame.LazDocListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  LazDocDeletePathButton.Enabled:=(Sender as TListBox).ItemIndex <> -1;
end;

procedure TProjectLazDocOptionsFrame.LazDocPathEditChange(Sender: TObject);
begin
  LazDocAddPathButton.Enabled:=(Sender as TEdit).Text <> '';
end;

procedure TProjectLazDocOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if fLoaded then exit;
  fLoaded:=true;
  with AOptions as TProject do
    SplitString(LazDocPaths, ';', LazDocListBox.Items, True);
end;

procedure TProjectLazDocOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if FSaved then exit;
  FSaved:=true;
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

