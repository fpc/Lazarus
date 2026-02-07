{ For license see registeranchordocking.pas

  Author: liao bozhi, integrated in AnchorDockingDsgn by Juha Manninen
}
unit AnchorBackupTool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, XMLPropStorage,
  FileUtil, LazFileUtils, LazLoggerBase,
  IDEOptionsIntf,
  IDECommands, IDEWindowIntf, MenuIntf, IDEImagesIntf, LazIDEIntf,
  AnchorDocking, AnchorDockStr;

type

  { TDockBackupFrm }

  TDockBackupFrm = class(TForm)
    btnBackup: TBitBtn;
    btnRestore: TBitBtn;
    btnRestoreDefault: TBitBtn;
    lblStatus: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure btnBackupClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure btnRestoreDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    FBackupDir: string;
    procedure LoadBackupList;
    procedure RestoreLayout(aBackupFile: string);
    procedure UpdateStatus(const Msg: string; IsError: Boolean = False);
  public

  end;

var
  DockBackupFrm: TDockBackupFrm;

procedure ShowDockbkFrm(Sender: TObject);

implementation

{$R *.lfm}

procedure ShowDockbkFrm(Sender: TObject);
begin
  DockBackupFrm:=TDockBackupFrm.Create(nil);
  DockBackupFrm.ShowModal;
  DockBackupFrm.Free;
end;

{ TDockBackupFrm }

procedure TDockBackupFrm.FormCreate(Sender: TObject);
begin
  self.Caption := adrsAnchorDockBackupRecovery;
  btnBackup.Caption := adrsBackupCurrentLayout;
  btnRestore.Caption := adrsRestoreSelectedLayout;
  btnRestore.Enabled := False;  // Until a filename is selected.
  btnRestoreDefault.Caption := adrsRestoreDefaultLayout;
  IDEImages.AssignImage(btnBackup, 'menu_saveas');
  IDEImages.AssignImage(btnRestore, 'restore_default');
  IDEImages.AssignImage(btnRestoreDefault, 'ce_default');
  // 设置默认路径
  FBackupDir := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + 'backups' + PathDelim;
  ForceDirectories(FBackupDir);
  // 加载备份列表
  LoadBackupList;
  UpdateStatus(adrsReady);
end;

procedure TDockBackupFrm.btnBackupClick(Sender: TObject);
var
  BackupFile: string;
  XMLConfig: TXMLConfigStorage;
begin
  BackupFile := FBackupDir + 'dockbackup_'
              + FormatDateTime('yyyy-mm-dd_hhnnss',Now) + '.xml';
  try
    XMLConfig:=TXMLConfigStorage.Create(BackupFile,false);
    try
      DockMaster.SaveLayoutToConfig(XMLConfig);
      DockMaster.SaveSettingsToConfig(XMLConfig);
      XMLConfig.WriteToDisk;
    finally
      XMLConfig.Free;
      UpdateStatus(adrsBackupSuccessful+': ' + ExtractFileName(BackupFile));
      LoadBackupList;
    end;
  except
    on E: Exception do
      UpdateStatus(adrsBackupFailed, True);
  end;
end;

procedure TDockBackupFrm.btnRestoreClick(Sender: TObject);
begin
  Assert(ListBox1.ItemIndex>=0, 'TDockBackupFrm.btnRestoreClick: ListBox1.ItemIndex');
  RestoreLayout(FBackupDir + ListBox1.Items[ListBox1.ItemIndex]);
end;

procedure TDockBackupFrm.btnRestoreDefaultClick(Sender: TObject);
begin
  //debugln(['TDockBackupFrm.btnRestoreDefaultClick LazarusDirectory=',
  //          IDEEnvironmentOptions.GetParsedLazarusDirectory]);
  RestoreLayout(IDEEnvironmentOptions.GetParsedLazarusDirectory
               +'components/anchordocking/design/ADLayoutDefault.xml');
end;

procedure TDockBackupFrm.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  btnRestore.Enabled := ListBox1.ItemIndex >= 0;
  Memo1.Clear;
  if ListBox1.ItemIndex = -1 then exit;
  try
    Memo1.Lines.LoadFromFile(FBackupDir+ListBox1.Items[ListBox1.ItemIndex]);
    UpdateStatus(ListBox1.Items[ListBox1.ItemIndex],False);
  except
    on E: Exception do
      Memo1.Lines.Add(adrsUnableToReadFile+': ' + E.Message);
  end;
end;

procedure TDockBackupFrm.LoadBackupList;
var
  Files: TStringList;
  i: Integer;
begin
  ListBox1.Clear;
  if DirectoryExists(FBackupDir) then
  begin
    Files := TStringList.Create;
    try
      FindAllFiles(Files, FBackupDir, 'dockbackup_*.xml', False);
      Files.Sort;
      for i := Files.Count - 1 downto 0 do  // 从最新到最旧排序
        ListBox1.Items.Add(ExtractFileName(Files[i]));
    finally
      Files.Free;
    end;
  end;
end;

procedure TDockBackupFrm.RestoreLayout(aBackupFile: string);
var
  XMLConfig: TXMLConfigStorage;
begin
  if MessageDlg(adrsConfirm, adrsAreYouSure,
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      XMLConfig := TXMLConfigStorage.Create(aBackupFile,True);
      DockMaster.LoadLayoutFromConfig(XMLConfig,true);
      DockMaster.LoadSettingsFromConfig(XMLConfig);
      UpdateStatus(adrsLayoutRestoredSuccessfully, False);
    finally
      XMLConfig.Free;
    end;
  end;
end;

procedure TDockBackupFrm.UpdateStatus(const Msg: string; IsError: Boolean);
begin
  lblStatus.Caption := Msg;
  if IsError then
  begin
    lblStatus.Font.Color := clRed;
    lblStatus.Font.Style := [fsBold];
  end
  else
  begin
    lblStatus.Font.Color := clGreen;
    lblStatus.Font.Style := [];
  end;
  //Application.ProcessMessages;
end;

end.
