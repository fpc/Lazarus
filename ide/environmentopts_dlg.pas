{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    This unit defines a dialog for the lazarus environment options.

}
unit EnvironmentOpts_Dlg;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Controls, Forms, ExtCtrls, LResources, ButtonPanel, 
  EnvironmentOpts, LazarusIDEStrConsts, IDEWindowIntf,
  options_files, options_desktop, options_window, options_formed, options_oi,
  options_backup, options_naming, options_fpdoc;

type
  { TEnvironmentOptionsDialog: EnvironmentOptionsDialog for environment options }
  
  TEnvOptsDialogPage = (
    eodpLanguage,
    eodpAutoSave,
    eodpDesktop,
    eodpMainHints,
    eodpWindowPositions,
    eodpFormEditor,
    eodpObjectInspector,
    eodpFiles,
    eodpBackup,
    eodpNaming
  );
  
  { TEnvironmentOptionsDialog }

  TEnvironmentOptionsDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    NoteBook: TNoteBook;
    FilesPage: TPage;
    DesktopPage: TPage;
    WindowsPage: TPage;
    FormEditorPage: TPage;
    ObjectInspectorPage: TPage;
    BackupPage: TPage;
    NamingPage: TPage;
    LazDocPage: TPage;

    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadEnvironmentSettings: TOnLoadEnvironmentSettings;
    FOnSaveEnvironmentSettings: TOnSaveEnvironmentSettings;
    FilesOptionsFrame: TFilesOptionsFrame;
    DesktopOptionsFrame: TDesktopOptionsFrame;
    WindowOptionsFrame: TWindowOptionsFrame;
    FormEditorOptionsFrame: TFormEditorOptionsFrame;
    OIOptionsFrame: TOIOptionsFrame;
    BackupOptionsFrame: TBackupOptionsFrame;
    NamingOptionsFrame: TNamingOptionsFrame;
    FpDocOptionsFrame: TFpDocOptionsFrame;

    procedure SetupFrame(AFrame: TAbstractOptionsFrame; APage: TPage);

    procedure SetCategoryPage(const AValue: TEnvOptsDialogPage);
    procedure SetupFilesPage(Page: integer);
    procedure SetupDesktopPage(Page: integer);
    procedure SetupWindowsPage(Page: integer);
    procedure SetupFormEditorPage(Page: integer);
    procedure SetupObjectInspectorPage(Page: integer);
    procedure SetupBackupPage(Page: integer);
    procedure SetupNamingPage(Page: integer);
    procedure SetupLazDocPage(Page: integer);
    function CheckValues: boolean;

    procedure LoadEnvironmentSettings(Sender: TObject; AOptions: TEnvironmentOptions);
    procedure SaveEnvironmentSettings(Sender: TObject; AOptions: TEnvironmentOptions);
  published
    property OnSaveEnvironmentSettings: TOnSaveEnvironmentSettings
      read FOnSaveEnvironmentSettings write FOnSaveEnvironmentSettings;
    property OnLoadEnvironmentSettings: TOnLoadEnvironmentSettings
      read FOnLoadEnvironmentSettings write FOnLoadEnvironmentSettings;
    property CategoryPage: TEnvOptsDialogPage write SetCategoryPage;
  public
    procedure ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
    procedure WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
    constructor Create(TheOwner: TComponent); override;
  end;

 
implementation

uses
  IDEContextHelpEdit;

{ TEnvironmentOptionsDialog }

constructor TEnvironmentOptionsDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,Width,Height);
  Caption := lisMenuGeneralOptions;

  NoteBook.PageIndex:=0;

  SetupFilesPage(0);
  SetupDesktopPage(1);
  SetupWindowsPage(2);
  SetupFormEditorPage(3);
  SetupObjectInspectorPage(4);
  SetupBackupPage(5);
  SetupNamingPage(6);
  SetupLazDocPage(7);

  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
end;

procedure TEnvironmentOptionsDialog.SetupDesktopPage(Page: integer);
begin
  DesktopOptionsFrame := TDesktopOptionsFrame.Create(Self);
  SetupFrame(DesktopOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupWindowsPage(Page: integer);
begin
  WindowOptionsFrame := TWindowOptionsFrame.Create(Self);
  SetupFrame(WindowOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupBackupPage(Page: integer);
begin
  BackupOptionsFrame := TBackupOptionsFrame.Create(Self);
  SetupFrame(BackupOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupFrame(AFrame: TAbstractOptionsFrame; APage: TPage);
begin
  AFrame.Parent := APage;

  AFrame.Anchors := [akLeft, akTop, akRight, akBottom];
  AFrame.AnchorSideLeft.Control := APage;
  AFrame.AnchorSideTop.Control := APage;
  AFrame.AnchorSideRight.Side := asrBottom;
  AFrame.AnchorSideRight.Control := APage;
  AFrame.AnchorSideBottom.Side := asrBottom;
  AFrame.AnchorSideBottom.Control := APage;
  AFrame.BorderSpacing.Around := 6;

  AFrame.OnLoadEnvironmentSettings := @LoadEnvironmentSettings;
  AFrame.OnSaveEnvironmentSettings := @SaveEnvironmentSettings;

  AFrame.Setup;
  AFrame.Visible := True;

  APage.Caption := AFrame.GetTitle;
end;

procedure TEnvironmentOptionsDialog.SetupFilesPage(Page: integer);
begin
  FilesOptionsFrame := TFilesOptionsFrame.Create(Self);
  SetupFrame(FilesOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetCategoryPage(const AValue: TEnvOptsDialogPage);
var
  p: Integer;
begin
  case AValue of
    eodpFiles: p:=0;
    eodpLanguage, eodpAutoSave, eodpDesktop, eodpMainHints,
    eodpWindowPositions: p:=2;
    eodpFormEditor: p:=3;
    eodpObjectInspector: p:=4;
    eodpBackup: p:=5;
    eodpNaming: p:=6;
  end;
  Notebook.PageIndex:=p;
end;

procedure TEnvironmentOptionsDialog.SetupFormEditorPage(Page: integer);
begin
  FormEditorOptionsFrame := TFormEditorOptionsFrame.Create(Self);
  SetupFrame(FormEditorOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupNamingPage(Page: integer);
begin
  NamingOptionsFrame := TNamingOptionsFrame.Create(Self);
  SetupFrame(NamingOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.SetupLazDocPage(Page: integer);
begin
  FpDocOptionsFrame := TFpDocOptionsFrame .Create(Self);
  SetupFrame(FpDocOptionsFrame, NoteBook.Page[Page]);
end;

procedure TEnvironmentOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TEnvironmentOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then
    Exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOk;
end;

procedure TEnvironmentOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TEnvironmentOptionsDialog.ReadSettings(AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do 
  begin
    // Files
    FilesOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Desktop
    DesktopOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Window
    WindowOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Object inspector
    OIOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Form editor
    FormEditorOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // Backup
    BackupOptionsFrame.ReadSettings(AnEnvironmentOptions);

    // naming
    NamingOptionsFrame.ReadSettings(AnEnvironmentOptions);

    //lazdoc
    FpDocOptionsFrame.ReadSettings(AnEnvironmentOptions);
  end;
end;

procedure TEnvironmentOptionsDialog.WriteSettings(AnEnvironmentOptions: TEnvironmentOptions);
begin
  with AnEnvironmentOptions do 
  begin
    // Files
    FilesOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Desktop
    DesktopOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Window
    WindowOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Object inspector
    OIOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Form editor
    FormEditorOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // Backup
    BackupOptionsFrame.WriteSettings(AnEnvironmentOptions);

    // naming
    NamingOptionsFrame.WriteSettings(AnEnvironmentOptions);

    //lazdoc
    FpDocOptionsFrame.WriteSettings(AnEnvironmentOptions);
  end;
end;

procedure TEnvironmentOptionsDialog.SetupObjectInspectorPage(Page: integer);
begin
  OIOptionsFrame := TOIOptionsFrame.Create(Self);
  SetupFrame(OIOptionsFrame, NoteBook.Page[Page]);
end;

function TEnvironmentOptionsDialog.CheckValues: boolean;
begin
  Result := FilesOptionsFrame.Check;
end;

procedure TEnvironmentOptionsDialog.LoadEnvironmentSettings(Sender: TObject;
  AOptions: TEnvironmentOptions);
begin
  if Assigned(OnLoadEnvironmentSettings) then
    OnLoadEnvironmentSettings(Self, AOptions);
  ReadSettings(AOptions);
end;

procedure TEnvironmentOptionsDialog.SaveEnvironmentSettings(Sender: TObject;
  AOptions: TEnvironmentOptions);
begin
  WriteSettings(AOptions);
  if Assigned(OnSaveEnvironmentSettings) then
    OnSaveEnvironmentSettings(Self, AOptions);
end;

initialization
  {$I environmentopts_dlg.lrs}

end.

