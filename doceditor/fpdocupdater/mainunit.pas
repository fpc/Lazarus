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

  Author: Tom Gregorovic
}
unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FPDocFiles, StdCtrls, ComCtrls, Masks, FileUtil, ExtCtrls,
  LCLIntf, LCLType, LCLProc, Process, EditBtn, XMLCfg;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonRefresh: TButton;
    ButtonUpdateNew: TButton;
    ButtonUpdate: TButton;
    ButtonUpdateAll: TButton;
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
    procedure EditDocsAcceptDirectory(Sender: TObject; var Value: String);
    procedure EditDocsEditingDone(Sender: TObject);
    procedure EditIncludeAcceptDirectory(Sender: TObject; var Value: String);
    procedure EditUnitsAcceptDirectory(Sender: TObject; var Value: String);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateList;
    procedure UpdateFile(const AFileName: String);
    procedure BackupFile(const AFileName: String);
    procedure WriteStatus(const S: String);
    
    procedure MoveElement(const SrcPackage: TFPDocPackage;
      const SrcModule: TFPDocModule; const Src: TFPDocElement;
      const DestList: TStrings; var Dest: String);
  end; 

var
  Form1: TForm1; 
  XMLConfig: TXMLConfig;
  BackupList: TStringList;

implementation

uses
  UnitMove;

function FindFiles(const Path, Mask: String; WithPath: Boolean = True;
  WithExt: Boolean = True): TStringList;
var
  MaskList: TMaskList;
  Info: TSearchRec;
  S: String;
begin
  Result := TStringList.Create;
  MaskList := TMaskList.Create(Mask);
  try
    if SysUtils.FindFirst(Path + GetAllFilesMask, faAnyFile, Info) = 0 then
    repeat
      if MaskList.Matches(Info.Name) then
      begin
        if WithPath then S := Path
        else S := '';
        if WithExt then S := S + Info.Name
        else S := S + ExtractFileNameOnly(Info.Name);
        
        Result.Add(S);
      end;
    until SysUtils.FindNext(Info) <> 0;

    SysUtils.FindClose(Info);
  finally
    MaskList.Free;
  end;
end;

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateList;
end;

procedure TForm1.ListBoxDrawItem(Control: TWinControl; Index: Integer;
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
      case Integer(ListBox.Items.Objects[Index]) of
      0: SetTextColor(ListBox.Canvas.Handle, ListBox.Canvas.Font.Color); // normal
      1: SetTextColor(ListBox.Canvas.Handle, clRed);                     // new
      end;
    end;
      
    FillRect(ARect);
    TextRect(ARect, ARect.Left + 8, ARect.Top, ExtractFileNameOnly(ListBox.Items[Index]));
  end;
end;

procedure TForm1.BeginUpdate;
begin
  BackupList := TStringList.Create;
  BackupList.Sorted := True;
  WriteStatus('Updating started.');
end;

procedure TForm1.EndUpdate;
begin
  BackupList.Free;
  UpdateList;
  WriteStatus('Updating done.');
  Sleep(1000);
  WriteStatus('');
end;

procedure TForm1.ButtonRefreshClick(Sender: TObject);
begin
  UpdateList;
end;

procedure TForm1.ButtonUpdateAllClick(Sender: TObject);
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

procedure TForm1.ButtonUpdateClick(Sender: TObject);
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

procedure TForm1.ButtonUpdateNewClick(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ListBox.Items.Count - 1 do
      if Integer(ListBox.items.Objects[I]) = 1 then UpdateFile(ListBox.Items[I]);
  finally
    EndUpdate;
  end;
end;

procedure TForm1.EditDocsAcceptDirectory(Sender: TObject; var Value: String);
begin
  EditDocs.Directory := AppendPathDelim(Value);
  EditDocs.SetFocus;
  UpdateList;
  Value := '';
end;

procedure TForm1.EditDocsEditingDone(Sender: TObject);
begin
  UpdateList;
end;

procedure TForm1.EditIncludeAcceptDirectory(Sender: TObject; var Value: String);
begin
  Value := AppendPathDelim(Value);
end;

procedure TForm1.EditUnitsAcceptDirectory(Sender: TObject; var Value: String);
begin
  EditUnits.Directory := AppendPathDelim(Value);
  EditUnits.SetFocus;
  UpdateList;
  Value := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  XMLConfig := TXMLConfig.Create(nil);
  XMLConfig.RootName := 'Config';
  XMLConfig.Filename := 'FPDocUpdater.xml';

  EditDocs.Directory := XMLConfig.GetValue('FPDocsPath/Value', 'D:\Projects\lazarus\docs\xml\lcl\');
  EditUnits.Directory := XMLConfig.GetValue('UnitsPath/Value', 'D:\Projects\lazarus\lcl\');
  EditInclude.Directory := XMLConfig.GetValue('IncludePath/Value', 'D:\Projects\lazarus\lcl\include\');
  EditMakeSkel.FileName := XMLConfig.GetValue('MakeSkelPath/Value', 'D:\Projects\fpcbeta\bin\i386-win32\makeskel.exe');
  CheckBoxBackup.Checked := XMLConfig.GetValue('BackupFPDocs/Value', True);
  EditBackup.Text := XMLConfig.GetValue('BackupExt/Value', 'bak');
  EditPackage.Text := XMLConfig.GetValue('Package/Value', 'LCL');
end;

procedure TForm1.FormDestroy(Sender: TObject);
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

procedure TForm1.UpdateList;
var
  Docs, Units: TStringList;
  I: Integer;
  N: String;
  State: Integer;
begin
  ListBox.Items.BeginUpdate;
  try
    ListBox.Items.Clear;

    Docs := FindFiles(EditDocs.Directory, '*.xml', False, False);
    Units := FindFiles(EditUnits.Directory, '*.pas;*.pp');
    try
      Units.Sorted := True;
      for I := 0 to Units.Count - 1 do
      begin
        N := ExtractFileNameOnly(Units[I]);

        if Docs.IndexOf(N) = -1 then State := 1
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
  ListBox.SetFocus;
end;

procedure ShowError(const S: String);
begin
  DebugLn(S);
  raise Exception.Create(S);
end;

procedure TForm1.UpdateFile(const AFileName: String);
var
  DocFileName: String;
  MakeSkelPath: String;
  AProcess: TProcess;
  AStringList: TStringList;
  M: TMemoryStream;
  N, BytesRead: LongInt;
  OldDoc, NewDoc: TFPDocFile;
const
   READ_BYTES = 2048;
   
begin
  if not FileExists(AFileName) then
  begin
    ShowError('Update ' + AFileName + ' failed!');
    Exit;
  end;
  
  MakeSkelPath := FindDefaultExecutablePath(EditMakeSkel.FileName);
  
  if not FileIsExecutable(MakeSkelPath) then
    ShowError('Unable to find MakeSkel tool executable "' + EditMakeSkel.Text +'"!');
  
  DocFileName := EditDocs.Directory + ExtractFileNameOnly(AFileName) + '.xml';
  
  if CheckBoxBackup.Checked then BackupFile(DocFileName);
  
  WriteStatus('Updating ' + AFileName);

  AProcess := TProcess.Create(nil);
  AStringList := TStringList.Create;
  M := TMemoryStream.Create;
  try
    AProcess.CommandLine :=
      Format(MakeSkelPath + ' --package="%s" --input="%s -Fi%s"',
        [EditPackage.Text, AFileName, EditInclude.Directory]);
    AProcess.Options := AProcess.Options + [poUsePipes];
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
    if AStringList.Strings[AStringList.Count - 1] <> 'Done.' then
    begin
      ShowError('Update ' + AFileName + ' failed! ' + AStringList.Strings[AStringList.Count - 1]);
      Exit;
    end;
      
    while (AStringList.Count > 0) and
      (AStringList.Strings[AStringList.Count - 1] <> '</fpdoc-descriptions>') do
      AStringList.Delete(AStringList.Count - 1);

    M.Clear;
    AStringList.SaveToStream(M);
    M.Position := 0;
    NewDoc := TFPDocFile.Create(M);
    if FileExists(DocFileName) then OldDoc := TFPDocFile.Create(DocFileName)
    else OldDoc := nil;
    try
      if OldDoc <> nil then OldDoc.AssignToSkeleton(NewDoc, @MoveElement);
      NewDoc.SaveToFile(DocFileName);
    finally
      if OldDoc <> nil then OldDoc.Free;
      NewDoc.Free;
    end;

    WriteStatus('Update ' + AFileName + ' in ' + DocFileName + ' succeeds!');
  finally
    M.Free;
    AStringList.Free;
    AProcess.Free;
  end;
end;

procedure TForm1.BackupFile(const AFileName: String);
var
  BackupFileName: String;
begin
  if not FileExists(AFileName) then Exit;

  if BackupList.IndexOf(AFileName) = -1 then
  begin
    BackupFileName := ChangeFileExt(AFileName, '.' + EditBackup.Text);

    if CopyFile(AFileName, BackupFileName, True) then
    begin
      WriteStatus('Backup ' + AFileName + ' to ' + BackupFileName + ' succeeds.');
      BackupList.Add(AFileName);
    end
    else
      ShowError('Backup ' + AFileName + ' to ' + BackupFileName + ' failed!');
  end;
end;

procedure TForm1.WriteStatus(const S: String);
begin
  DebugLn(S);
  StatusBar.SimpleText := S;
end;

procedure TForm1.MoveElement(const SrcPackage: TFPDocPackage;
  const SrcModule: TFPDocModule; const Src: TFPDocElement;
  const DestList: TStrings; var Dest: String);
var
  F: TFPDocFile;
begin
  FormMove.LabelSrc.Caption := Format('Package: %sModule: %s',
    [SrcPackage.Name + LineEnding, SrcModule.Name]);
  FormMove.LabelSrcElement.Caption := 'Element: ' + Src.Name;

  FormMove.ComboBoxDest.Items.Assign(DestList);
  FormMove.ComboBoxDest.Sorted := True;
  
  case FormMove.ShowModal of
  mrYes:
    Dest := FormMove.ComboBoxDest.Text;
  mrCancel:
    begin // Move to another file
      OpenDialog.InitialDir := ExtractFileDir(EditDocs.Directory);
      if OpenDialog.Execute then
      begin
        if CheckBoxBackup.Checked then BackupFile(OpenDialog.FileName);
        
        F := TFPDocFile.Create(OpenDialog.FileName);
        try
          F.PackagesByName[EditPackage.Text].Modules[0].Add(Src);
          F.SaveToFile(OpenDialog.FileName);
          
          WriteStatus('Move Element: ' + SrcPackage.Name + '\' + SrcModule.Name +
            '\' + Src.Name + ' Dest file: ' + OpenDialog.FileName);
          Exit;
        finally
          F.Free;
        end;
      end;
    end;
  end;
    
  WriteStatus('Move Element: ' + SrcPackage.Name + '\' + SrcModule.Name + '\' + Src.Name +
    ' Dest: ' + Dest);
end;

initialization
  {$I mainunit.lrs}

end.

