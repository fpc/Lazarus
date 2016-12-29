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

 Author: Balázs Székely
 Abstract:
   Implementation of the options dialog.
}

unit opkman_optionsfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, opkman_VirtualTrees, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Spin, ComCtrls, Buttons, EditBtn, LazFileUtils, Math;

type

  { TOptionsFrm }

  TOptionsFrm = class(TForm)
    bCancel: TButton;
    bFoldersAdd: TButton;
    bFilesEdit: TButton;
    bFilesDelete: TButton;
    bFoldersDelete: TButton;
    bFoldersEdit: TButton;
    bOk: TButton;
    bRestore: TButton;
    bFilesAdd: TButton;
    cbProxy: TCheckBox;
    cbForceDownloadExtract: TCheckBox;
    cbDeleteZipAfterInstall: TCheckBox;
    cbCheckForUpdates: TComboBox;
    cbSelectProfile: TComboBox;
    edLocalRepositoryUpdate: TDirectoryEdit;
    edLocalRepositoryPackages: TDirectoryEdit;
    edLocalRepositoryArchive: TDirectoryEdit;
    edProxyPassword: TEdit;
    edProxyServer: TEdit;
    edProxyUser: TEdit;
    edRemoteRepository: TEdit;
    gbProxySettings: TGroupBox;
    lbFilterFiles: TLabel;
    lbFilterDirs: TLabel;
    lbLastUpdate: TLabel;
    lbSelectProfile: TLabel;
    lbUpdates: TLabel;
    lbLocalRepositoryArchive: TLabel;
    lbLocalRepositoryUpdate: TLabel;
    lbRemoteRepository: TLabel;
    lbServer: TLabel;
    lbLocalRepositoryPackages: TLabel;
    lbUserName: TLabel;
    lbPort: TLabel;
    lbPassword: TLabel;
    lbExcludeFiles: TListBox;
    lbExcludeFolders: TListBox;
    pnProfilesCaptionLeft: TPanel;
    pnProfilesCaptionLeft1: TPanel;
    pnProfilesLeftButtons: TPanel;
    pnProfilesRightButtons: TPanel;
    pnProfilesRight: TPanel;
    pnProfilesTop: TPanel;
    pnProfiles: TPanel;
    pnFolders: TPanel;
    pnProxy: TPanel;
    pnGeneral: TPanel;
    pgOptions: TPageControl;
    pnBottom: TPanel;
    pnProfilesMain: TPanel;
    pnProfilesLeft: TPanel;
    SDD: TSelectDirectoryDialog;
    seProxyPort: TSpinEdit;
    tsFolders: TTabSheet;
    tsProfiles: TTabSheet;
    tsGeneral: TTabSheet;
    tsProxy: TTabSheet;
    procedure bFilesAddClick(Sender: TObject);
    procedure bFilesDeleteClick(Sender: TObject);
    procedure bFilesEditClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure bRestoreClick(Sender: TObject);
    procedure cbProxyChange(Sender: TObject);
    procedure cbSelectProfileChange(Sender: TObject);
    procedure edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
    procedure pnProfilesMainResize(Sender: TObject);
    procedure pnProfilesTopResize(Sender: TObject);
  private
    function GetSelectedText(AListBox: TListBox; var AIndex: Integer): String;
  public
    procedure SetupControls(const AActivePageIndex: Integer = 0);
  end;

var
  OptionsFrm: TOptionsFrm;

implementation
uses opkman_options, opkman_common, opkman_const;
{$R *.lfm}

{ TOptionsFrm }

procedure TOptionsFrm.bOkClick(Sender: TObject);
var
  I: Integer;
begin
  if Trim(edRemoteRepository.Text)  = '' then
  begin
    MessageDlgEx(rsOptions_RemoteRepository_Information, mtInformation, [mbOk], Self);
    edRemoteRepository.SetFocus;
    Exit;
  end;

  if cbProxy.Checked then
  begin
    if Trim(edProxyServer.Text)  = '' then
    begin
      MessageDlgEx(rsOptions_ProxyServer_Info, mtInformation, [mbOk], Self);
      edProxyServer.SetFocus;
      Exit;
    end;
    if seProxyPort.Value = 0 then
    begin
      MessageDlgEx(rsOptions_ProxyPort_Info, mtInformation, [mbOk], Self);
      seProxyPort.SetFocus;
      Exit;
    end;
  end;

  if Trim(edLocalRepositoryPackages.Text)  = '' then
  begin
    MessageDlgEx(rsOptions_InvalidDirectory_Info, mtInformation, [mbOk], Self);
    edLocalRepositoryPackages.SetFocus;
    Exit;
  end;
  if Trim(edLocalRepositoryArchive.Text)  = '' then
  begin
    MessageDlgEx(rsOptions_InvalidDirectory_Info, mtInformation, [mbOk], Self);
    edLocalRepositoryArchive.SetFocus;
    Exit;
  end;
  if Trim(edLocalRepositoryUpdate.Text)  = '' then
  begin
    MessageDlgEx(rsOptions_InvalidDirectory_Info, mtInformation, [mbOk], Self);
    edLocalRepositoryUpdate.SetFocus;
    Exit;
  end;
  Options.RemoteRepository := edRemoteRepository.Text;
  Options.ForceDownloadAndExtract := cbForceDownloadExtract.Checked;
  Options.DeleteZipAfterInstall := cbDeleteZipAfterInstall.Checked;
  Options.CheckForUpdates := cbCheckForUpdates.ItemIndex;

  Options.ProxyEnabled := cbProxy.Checked;
  Options.ProxyServer := edProxyServer.Text;
  Options.ProxyPort := seProxyPort.Value;
  Options.ProxyUser := edProxyUser.Text;
  Options.ProxyPassword := edProxyPassword.Text;

  Options.LocalRepositoryPackages := AppendPathDelim(edLocalRepositoryPackages.Text);
  Options.LocalRepositoryArchive := AppendPathDelim(edLocalRepositoryArchive.Text);
  Options.LocalRepositoryUpdate := AppendPathDelim(edLocalRepositoryUpdate.Text);

  Options.UserProfile := cbSelectProfile.ItemIndex;
  for I := 0 to lbExcludeFiles.Items.Count - 1 do
  begin
    if I = 0 then
      Options.ExcludedFiles := lbExcludeFiles.Items[I]
    else
      Options.ExcludedFiles := Options.ExcludedFiles + ',' + lbExcludeFiles.Items[I];
  end;
  for I := 0 to lbExcludeFolders.Items.Count - 1 do
  begin
    if I = 0 then
      Options.ExcludedFolders := lbExcludeFolders.Items[I]
    else
      Options.ExcludedFolders := Options.ExcludedFolders + ',' + lbExcludeFolders.Items[I];
  end;

  Options.Save;
  ModalResult := mrOk;
end;

function TOptionsFrm.GetSelectedText(AListBox: TListBox; var AIndex: Integer): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AListBox.Count - 1 do
  begin
    if AListBox.Selected[I] then
    begin
      Result := AListBox.Items.Strings[I];
      AIndex := I;
      Break;
    end;
  end;
end;

procedure TOptionsFrm.bFilesAddClick(Sender: TObject);
var
  Value: String;
  Index: Integer;
  ListBox: TListBox;
begin
  case (Sender as TButton).Tag of
    0: begin
         ListBox := lbExcludeFiles;
         Value := InputBox(rsOptions_InputBox_Caption, rsOptions_InputBox_Text0, '*.');
       end;
    1: begin
         ListBox := lbExcludeFolders;
         Value := InputBox(rsOptions_InputBox_Caption, rsOptions_InputBox_Text1, '');
       end;
  end;
  if Value <> '' then
  begin
    Index := ListBox.Items.Add(Value);
    ListBox.Selected[Index] := True;
  end;
end;

procedure TOptionsFrm.bFilesEditClick(Sender: TObject);
var
  Value: String;
  Index: Integer;
  MsgInfo: String;
  MsgTxt: String;
  ListBox: TListBox;
begin
  case (Sender as TButton).Tag of
    0: begin
         MsgInfo := rsOptions_InputBox_Info0;
         MsgTxt := rsOptions_InputBox_Text0;
         Listbox := lbExcludeFiles;
       end;
    1: begin
         MsgInfo := rsOptions_InputBox_Info1;
         MsgTxt := rsOptions_InputBox_Text1;
         ListBox := lbExcludeFolders;
       end;
  end;
  if ListBox.SelCount = 0 then
  begin
    MessageDlgEx(MsgInfo, mtInformation, [mbOk], Self);
    ListBox.SetFocus;
    Exit;
  end;
  Index := -1;
  Value := InputBox(rsOptions_InputBox_Caption, MsgTxt, GetSelectedText(ListBox, Index));
  if (Value <> '') and (Index <> -1) then
  begin
    ListBox.Sorted := False;
    ListBox.Items[Index] := Value;
    ListBox.Sorted := True;
    Index := ListBox.Items.IndexOf(Value);
    ListBox.Selected[Index] := True;
  end;
end;


procedure TOptionsFrm.bFilesDeleteClick(Sender: TObject);
var
  MsgConf: String;
  MsgInfo: String;
  ListBox: TListBox;
  Value: String;
  Index: Integer;
begin
  case (Sender as TButton).Tag of
     0: begin
          MsgInfo := rsOptions_InputBox_Info0;
          MsgConf := rsOptions_InputBox_Conf0;
          ListBox := lbExcludeFiles;
        end;
     1: begin
          MsgInfo := rsOptions_InputBox_Info1;
          MsgConf := rsOptions_InputBox_Conf1;
          ListBox := lbExcludeFolders;
        end;
   end;
  if ListBox.SelCount = 0 then
  begin
    MessageDlgEx(MsgInfo, mtInformation, [mbOk], Self);
    ListBox.SetFocus;
    Exit;
  end;
  Index := -1;
  Value := GetSelectedText(ListBox, Index);
  if (Value <> '') and (Index <> -1) then
    if MessageDlgEx(Format(MsgConf, [Value]), mtConfirmation, [mbYes, mbNo], Self) = mrYes then
      ListBox.Items.Delete(Index);
end;

procedure TOptionsFrm.bRestoreClick(Sender: TObject);
begin
  if MessageDlgEx(rsOptions_RestoreDefaults_Conf, mtInformation, [mbYes, mbNo], Self) = mrYes then
  begin
    Options.LoadDefault;
    Options.CreateMissingPaths;
    SetupControls;
  end;
end;

procedure TOptionsFrm.cbProxyChange(Sender: TObject);
begin
  gbProxySettings.Enabled:= cbProxy.Checked;
end;

procedure TOptionsFrm.cbSelectProfileChange(Sender: TObject);
begin
  pnProfilesMain.Visible := cbSelectProfile.ItemIndex = 1;
end;

procedure TOptionsFrm.edRemoteRepositoryKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    bOkClick(bOk);
end;

procedure TOptionsFrm.SetupControls(const AActivePageIndex: Integer = 0);
begin
  Self.DoubleBuffered := True;
  Caption := rsOptions_FrmCaption;
  pgOptions.ActivePageIndex := AActivePageIndex;
  tsGeneral.Caption := rsOptions_tsGeneral_Caption;
  lbRemoteRepository.Caption := rsOptions_lbRemoteRepository_Caption;
  edRemoteRepository.Text := Options.RemoteRepository;
  cbForceDownloadExtract.Checked := Options.ForceDownloadAndExtract;
  cbDeleteZipAfterInstall.Checked := Options.DeleteZipAfterInstall;
  cbForceDownloadExtract.Caption := rsOptions_cbForceDownloadExtract_Caption;
  cbForceDownloadExtract.Hint := rsOptions_cbForceDownloadExtract_Hint;
  cbDeleteZipAfterInstall.Caption := rsOptions_cbDelete_Caption;
  cbDeleteZipAfterInstall.Hint := rsOptions_cbDelete_Hint;
  lbUpdates.Caption := rsOptions_lbCheckForUpdates_Caption;
  cbCheckForUpdates.Clear;
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item0);
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item1);
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item2);
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item3);
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item4);
  cbCheckForUpdates.Items.Add(rsOptions_cbCheckForUpdates_Item5);
  cbCheckForUpdates.ItemIndex := Options.CheckForUpdates;
  if CompareValue(Options.LastUpdate, 0.0, 0.1) <= 0 then
    lbLastUpdate.Caption := rsOptions_lbLastUpdate_Caption + rsOptions_LastUpdate_Never
  else
    lbLastUpdate.Caption := rsOptions_lbLastUpdate_Caption + FormatDateTime('YYYY.MM.DD  hh:mm:ss', Options.LastUpdate);
  tsProxy.Caption := rsOptions_tsProxy_Caption;
  cbProxy.Caption := rsOptions_cbProxy_Caption;
  gbProxySettings.Caption := rsOptions_gbProxySettings_Caption;
  lbServer.Caption := rsOptions_lbServer_Caption;
  lbPort.Caption := rsOptions_lbPort_Caption;
  lbUserName.Caption := rsOptions_lbUsername_Caption;
  lbPassword.Caption := rsOptions_lbPassword_Caption;
  cbProxy.Checked := Options.ProxyEnabled;
  gbProxySettings.Enabled := Options.ProxyEnabled;
  edProxyServer.Text := Options.ProxyServer;
  seProxyPort.Value := Options.ProxyPort;
  edProxyUser.Text := Options.ProxyUser;
  edProxyPassword.Text := Options.ProxyPassword;

  tsFolders.Caption := rsOptions_tsFolders_Caption;
  lbLocalRepositoryPackages.Caption := rsOptions_lbLocalRepositoryPackages_Caption;
  edLocalRepositoryPackages.Hint := rsOptions_edLocalRepositoryPackages_Hint;
  lbLocalRepositoryArchive.Caption := rsOptions_lbLocalRepositoryArchive_Caption;
  edLocalRepositoryArchive.Hint := rsOptions_edLocalRepositoryArchive_Hint;
  lbLocalRepositoryUpdate.Caption := rsOptions_lbLocalRepositoryUpdate_Caption;
  edLocalRepositoryUpdate.Hint := rsOptions_edLocalRepositoryUpdate_Hint;
  edLocalRepositoryPackages.Text := Options.LocalRepositoryPackages;
  edLocalRepositoryArchive.Text := Options.LocalRepositoryArchive;
  edLocalRepositoryUpdate.Text := Options.LocalRepositoryUpdate;

  tsProfiles.Caption := rsOptions_tsProfiles_Caption;
  lbSelectProfile.Caption := rsOptions_lbSelectProfile_Caption;
  pnProfilesMain.DoubleBuffered := True;
  pnProfilesLeft.DoubleBuffered := True;
  pnProfilesTop.DoubleBuffered := True;
  cbSelectProfile.Clear;
  cbSelectProfile.Items.Add(rsOptions_cbSelectProfile_Item0);
  cbSelectProfile.Items.Add(rsOptions_cbSelectProfile_Item1);
  cbSelectProfile.ItemIndex := Options.UserProfile;
  cbSelectProfile.Hint := rsOptions_cbSelectProfile_Hint;
  lbFilterFiles.Caption := rsOptions_lbFilterFiles_Caption;
  lbFilterDirs.Caption := rsOptions_lbFilterDirs_Caption;
  bFilesAdd.Caption := rsOptions_bAdd_Caption;
  bFilesEdit.Caption := rsOptions_bEdit_Caption;
  bFilesDelete.Caption := rsOptions_bDelete_Caption;
  bFoldersAdd.Caption := rsOptions_bAdd_Caption;
  bFoldersEdit.Caption := rsOptions_bEdit_Caption;
  bFoldersDelete.Caption := rsOptions_bDelete_Caption;
  lbExcludeFiles.Hint := rsOptions_lbExcludeFiles_Hint;
  lbExcludeFiles.Items.Delimiter := ',';
  lbExcludeFiles.Items.StrictDelimiter := True;
  lbExcludeFiles.Items.DelimitedText := Options.ExcludedFiles;
  lbExcludeFolders.Hint := rsOptions_lbExcludeFolders_Hint;
  lbExcludeFolders.Items.Delimiter := ',';
  lbExcludeFolders.Items.StrictDelimiter := True;
  lbExcludeFolders.Items.DelimitedText := Options.ExcludedFolders;
  pnProfilesMain.Visible := Options.UserProfile = 1;
end;

procedure TOptionsFrm.pnProfilesMainResize(Sender: TObject);
begin
  pnProfilesLeft.Width := pnProfilesMain.Width div 2;
end;

procedure TOptionsFrm.pnProfilesTopResize(Sender: TObject);
begin
  cbSelectProfile.Left := lbSelectProfile.Left + lbSelectProfile.Width + 10;
end;

end.

