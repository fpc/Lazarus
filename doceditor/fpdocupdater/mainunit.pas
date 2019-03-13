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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Tom Gregorovic
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FPDocFiles, StdCtrls, ComCtrls, FileUtil, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Process, UTF8Process, EditBtn, Laz_XMLCfg,
  LazUtf8, LazFileUtils;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonRefresh: TButton;
    ButtonUpdateNew: TButton;
    ButtonUpdate: TButton;
    ButtonUpdateAll: TButton;
    CheckBoxShowSummary: TCheckBox;
    CheckBoxBackup: TCheckBox;
    EditInclude: TDirectoryEdit;
    EditMakeSkel: TFileNameEdit;
    EditPackage: TEdit;
    EditBackup: TEdit;
    EditUnits: TDirectoryEdit;
    EditDocs: TDirectoryEdit;
    Label1: TLabel;
    LabelInclude: TLabel;
    LabelMakeSkel: TLabel;
    LabelPackage: TLabel;
    LabelBackup: TLabel;
    LabelUnits: TLabel;
    LabelDocs: TLabel;
    ListBox: TListBox;
    OpenDialog: TOpenDialog;
    StatusBar: TStatusBar;
    procedure ButtonRefreshClick(Sender: TObject);
    procedure ButtonUpdateAllClick(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure ButtonUpdateNewClick(Sender: TObject);
    procedure EditDocsChange(Sender: TObject);
    procedure EditUnitsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateList;
    procedure UpdateFile(const AFileName: String);
    procedure BackupFile(const AFileName: String);
    procedure WriteStatus(const S: String);
    
    procedure MoveElement(const SrcPackage: TFPDocPackage;
      const SrcModule: TFPDocModule; const Src: TFPDocElement;
      const DestList: TStrings; var Dest: Integer);
  end; 

var
  FormMain: TFormMain;
  XMLConfig: TXMLConfig;
  BackupList: TStringList;

implementation

{$R *.lfm}

uses
  UnitMove, UnitSummary;

function FindFiles(const Path, Mask: String; OnlyFileName: Boolean = False): TStringList;
var
  I: Integer;
begin
  Result := FindAllFiles(Path, Mask, False);
  if OnlyFileName then
    for I := 0 to Result.Count - 1 do
      Result[I] := ExtractFileNameOnly(Result[I]);
end;

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin
  UpdateList;
end;

procedure TFormMain.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  if (Index < 0) or (Index >= ListBox.Items.Count) then Exit;
  
  with ListBox.Canvas do
  begin
    if odSelected in State then
      Brush.Color := clHighlight
    else
    begin
      Brush.Color := ListBox.Color;
      case PtrInt(ListBox.Items.Objects[Index]) of
      0: SetTextColor(ListBox.Canvas.Handle, ListBox.Canvas.Font.Color); // normal
      1: SetTextColor(ListBox.Canvas.Handle, clRed);                     // new
      end;
    end;
      
    FillRect(ARect);
    TextRect(ARect, ARect.Left + 8, ARect.Top + 2, ExtractFileNameOnly(ListBox.Items[Index]));
  end;
end;

procedure TFormMain.BeginUpdate;
begin
  BackupList := TStringList.Create;
  BackupList.Sorted := True;
  WriteStatus('Updating started.');
end;

procedure TFormMain.EndUpdate;
begin
  BackupList.Free;
  UpdateList;
  WriteStatus('Updating done.');
  Sleep(5000);
  WriteStatus('');
end;

procedure TFormMain.ButtonRefreshClick(Sender: TObject);
begin
  UpdateList;
end;

procedure TFormMain.ButtonUpdateAllClick(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      UpdateFile(ListBox.Items[I]);
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ButtonUpdateClick(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      if ListBox.Selected[I] then UpdateFile(ListBox.Items[I]);
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.ButtonUpdateNewClick(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      if PtrInt(ListBox.items.Objects[I]) = 1 then UpdateFile(ListBox.Items[I]);
  finally
    EndUpdate;
  end;
end;

procedure TFormMain.EditDocsChange(Sender: TObject);
begin
  if DirectoryExistsUTF8(EditDocs.Text) then UpdateList;
end;

procedure TFormMain.EditUnitsChange(Sender: TObject);
begin
  if DirectoryExistsUTF8(EditUnits.Text) then UpdateList;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  XMLConfig := TXMLConfig.Create('FPDocUpdater.xml');

 EditDocs.Directory := XMLConfig.GetValue('FPDocsPath/Value', 'Please set path to <Docs\xml\lcl\>');
  EditUnits.Directory := XMLConfig.GetValue('UnitsPath/Value', 'Please set path to <LCL\>');
  EditInclude.Directory := XMLConfig.GetValue('IncludePath/Value', 'Please set path to <LCL\Include\>');
  EditMakeSkel.FileName := XMLConfig.GetValue('MakeSkelPath/Value', 'Please set path to <makeskel.exe>');
  CheckBoxBackup.Checked := XMLConfig.GetValue('BackupFPDocs/Value', True);
  EditBackup.Text := XMLConfig.GetValue('BackupExt/Value', 'bak');
  EditPackage.Text := XMLConfig.GetValue('Package/Value', 'lcl');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  XMLConfig.Clear;
  
  XMLConfig.SetValue('FPDocsPath/Value', EditDocs.Directory);
  XMLConfig.SetValue('UnitsPath/Value', EditUnits.Directory);
  XMLConfig.SetValue('IncludePath/Value', EditInclude.Directory);
  XMLConfig.SetValue('MakeSkelPath/Value', EditMakeSkel.FileName);
  XMLConfig.SetValue('BackupFPDocs/Value', CheckBoxBackup.Checked);
  XMLConfig.SetValue('BackupExt/Value', EditBackup.Text);
  XMLConfig.SetValue('Package/Value', EditPackage.Text);
  
  XMLConfig.Free;
end;

procedure TFormMain.UpdateList;
var
  Docs, Units: TStringList;
  DocsPath, UnitsPath: String;
  I: Integer;
  N: String;
  State: PtrInt;
begin
  ListBox.Items.BeginUpdate;
  try
    ListBox.Items.Clear;
    
    DocsPath := AppendPathDelim(EditDocs.Directory);
    UnitsPath := AppendPathDelim(EditUnits.Directory);
    
    Docs := FindFiles(DocsPath, '*.xml', True);
    Units := FindFiles(UnitsPath, '*.pas;*.pp');
    try
      Units.Sorted := True;
      for I := 0 to Units.Count - 1 do
      begin
        N := ExtractFileNameOnly(Units[I]);

        if Docs.IndexOf(N) = -1 then
          State := 1
        else
          State := 0;
            
        ListBox.Items.AddObject(Units[I], TObject(State));
      end;
    finally
      Units.Free;
      Docs.Free;
    end;
  finally
    ListBox.Items.EndUpdate;
  end;
end;

procedure RaiseError(const S: String);
begin
  DebugLn(S);
  raise Exception.Create(S);
end;

procedure TFormMain.UpdateFile(const AFileName: String);
var
  DocFileName: String;
  MakeSkelPath: String;
  AProcess: TProcessUTF8;
  AStringList, AErrorList: TStringList;
  M: TMemoryStream;
  N, BytesRead: LongInt;
  OldDoc, NewDoc: TFPDocFile;
const
   READ_BYTES = 2048;
   
begin
  try
    if not FileExistsUTF8(AFileName) then
    begin
      RaiseError('Update ' + AFileName + ' failed!');
    end;

    MakeSkelPath := FindDefaultExecutablePath(EditMakeSkel.FileName);

    if not FileIsExecutable(MakeSkelPath) then
      RaiseError('Unable to find MakeSkel tool executable "' + EditMakeSkel.Text +'"!');

    DocFileName := AppendPathDelim(EditDocs.Directory) + ExtractFileNameOnly(AFileName) + '.xml';

    if CheckBoxBackup.Checked then BackupFile(DocFileName);

    WriteStatus('Updating ' + AFileName);

    AProcess := TProcessUTF8.Create(nil);
    AStringList := TStringList.Create;
    AErrorList := TStringList.Create;
    M := TMemoryStream.Create;
    try
      AProcess.CommandLine :=
        Format(MakeSkelPath + ' --package="%s" --input="%s -Fi%s"',
          [EditPackage.Text, AFileName, EditInclude.Directory]);
      AProcess.Options := AProcess.Options + [poUsePipes, poNoConsole, poStderrToOutPut];
      AProcess.Execute;

      BytesRead := 0;
      while AProcess.Running do
      begin
        M.SetSize(BytesRead + READ_BYTES);
        N := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
        if N > 0 then Inc(BytesRead, N)
        else Sleep(100);
      end;

      repeat
        M.SetSize(BytesRead + READ_BYTES);
        N := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
        if N > 0 then Inc(BytesRead, N);
      until N <= 0;
      M.SetSize(BytesRead);

      AStringList.LoadFromStream(M);

      while (AStringList.Count > 0) and
        (AStringList.Strings[AStringList.Count - 1] = '') do
        AStringList.Delete(AStringList.Count - 1);

      while (AStringList.Count > 0) and
        (AStringList.Strings[AStringList.Count - 1] <> '') and
        (AStringList.Strings[AStringList.Count - 1] <> '</fpdoc-descriptions>') do
      begin
        AErrorList.Add(AStringList.Strings[AStringList.Count - 1]);
        AStringList.Delete(AStringList.Count - 1);
      end;

      while (AStringList.Count > 0) and
        (AStringList.Strings[AStringList.Count - 1] <> '</fpdoc-descriptions>') do
        AStringList.Delete(AStringList.Count - 1);

      if AStringList.Count = 0 then
        RaiseError('Update ' + AFileName + ' failed, because' + AErrorList.Text);

      M.Clear;
      AStringList.SaveToStream(M);
      M.Position := 0;
      NewDoc := TFPDocFile.Create(M);
      if FileExistsUTF8(DocFileName) then OldDoc := TFPDocFile.Create(DocFileName)
      else OldDoc := nil;

      try
        if OldDoc <> nil then
        begin
          FormSummary.OldInfo := OldDoc.GetInfo;
          OldDoc.AssignToSkeleton(NewDoc, @MoveElement);
          FormSummary.NewInfo := NewDoc.GetInfo;
        end
        else
        begin
          FormSummary.OldInfo := EmptyFPDocInfo;
          FormSummary.NewInfo := NewDoc.GetInfo;
        end;

        if CheckBoxShowSummary.Checked then
        begin
          FormSummary.LabelFileName.Caption := DocFileName;
          if FormSummary.ShowModal = mrOk then
          begin
            NewDoc.SaveToFile(DocFileName);
            WriteStatus('Update ' + AFileName + ' to ' + DocFileName + ' succeeds!');
          end
          else
            WriteStatus('Update ' + AFileName + ' to ' + DocFileName + ' cancelled!');
        end
        else
        begin
          NewDoc.SaveToFile(DocFileName);
          WriteStatus('Update ' + AFileName + ' to ' + DocFileName + ' succeeds!');
        end;

      finally
        if OldDoc <> nil then OldDoc.Free;
        NewDoc.Free;
      end;

    finally
      M.Free;
      AStringList.Free;
      AErrorList.Free;
      AProcess.Free;
    end;
  except
    on E: Exception do
      MessageDlg('Error', E.Message, mtError, [mbOK], '');
  end;
end;

procedure TFormMain.BackupFile(const AFileName: String);
var
  BackupFileName: String;
begin
  if not FileExistsUTF8(AFileName) then Exit;

  if BackupList.IndexOf(AFileName) = -1 then
  begin
    BackupFileName := ChangeFileExt(AFileName, '.' + EditBackup.Text);

    if CopyFile(AFileName, BackupFileName, True) then
    begin
      WriteStatus('Backup ' + AFileName + ' to ' + BackupFileName + ' succeeds.');
      BackupList.Add(AFileName);
    end
    else
      RaiseError('Backup ' + AFileName + ' to ' + BackupFileName + ' failed!');
  end;
end;

procedure TFormMain.WriteStatus(const S: String);
begin
  DebugLn(S);
  StatusBar.SimpleText := S;
  StatusBar.Update;
end;

procedure TFormMain.MoveElement(const SrcPackage: TFPDocPackage;
  const SrcModule: TFPDocModule; const Src: TFPDocElement;
  const DestList: TStrings; var Dest: Integer);
var
  JumpList: TStringList;
  I: Integer;
  Prefix: String;
begin
  FormMove.LabelSrc.Caption := Format('Package: %sModule: %s',
    [SrcPackage.Name + LineEnding, SrcModule.Name]);
  FormMove.LabelSrcElement.Caption := 'Element: ' + Src.Name;

  JumpList := TStringList.Create;
  try
    for I := 0 to DestList.Count - 1 do
    begin
      if Pos('.', DestList[I]) = 0 then Continue;
      Prefix := Copy(DestList[I], 1, LastDelimiter('.', DestList[I]) - 1);
      if JumpList.IndexOf(Prefix) = -1 then JumpList.Add(Prefix);
    end;
    
    FormMove.ComboBoxJump.Items.Assign(JumpList);
  finally
    JumpList.Free;
  end;
  
  FormMove.ListBoxDest.Items.Assign(DestList);
  
  case FormMove.ShowModal of
    mrYes: Dest := FormMove.ListBoxDest.ItemIndex;
  end;
  
  if Dest <> -1 then
    WriteStatus('Move Element: ' + SrcPackage.Name + '\' + SrcModule.Name + '\' + Src.Name +
      ' Dest: ' + DestList[Dest]);
end;

end.

