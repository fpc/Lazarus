unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, FileUtil,
  LazUTF8, LazFileUtils, LCLIntf, Buttons, Menus, IniFiles,
  SynEdit, SynHighlighterHTML;

type

  { TMainForm }

  TMainForm = class(TForm)
    bbtnClose: TBitBtn;
    bbtnCreateHTML: TBitBtn;
    bbtnSave: TBitBtn;
    bbtnShow: TBitBtn;
    cbDarkMode: TCheckBox;
    DirectoryEdit: TDirectoryEdit;
    ImageList: TImageList;
    popLastDirs: TPopupMenu;
    sbtnLastDirs: TSpeedButton;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TaskDialog: TTaskDialog;
    procedure bbtnCloseClick(Sender: TObject);
    procedure bbtnCreateHTMLClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure bbtnShowClick(Sender: TObject);
    procedure cbDarkModeChange(Sender: TObject);
    procedure DirectoryEditChange(Sender: TObject);
    procedure DirectoryEditEditingDone(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastDirClick(Sender: TObject);
    procedure sbtnLastDirsClick(Sender: TObject);
  private
    fn: String;
    ImgDir: String;
    Config: TIniFile;
    LastDirsMax: Integer;
    procedure InfoMsg(const AMsg: String);
    procedure ErrorMsg(const AMsg: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  MenItem: TMenuItem;
begin
  LastDirsMax := 9;
  for i := 0 to LastDirsMax do
  begin
    MenItem := TMenuItem.Create(popLastDirs);
    MenItem.OnClick := @LastDirClick;
    MenItem.ImageIndex := 6;
    popLastDirs.Items.Add(MenItem);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  Config := TIniFile.Create('IconTableConfig.ini');
  try
    Top := Config.ReadInteger('Position', 'Top', 100);
    Left := Config.ReadInteger('Position', 'Left', 100);
    Width := Config.ReadInteger('Position', 'Width', 100);
    Height := Config.ReadInteger('Position', 'Height', 100);
    WindowState := wsNormal;
    Application.ProcessMessages;
    WindowState := TWindowState(Config.ReadInteger('Position', 'WindowState', 0));

    for i := 0 to LastDirsMax do
    begin
      popLastDirs.Items[i].Caption := Config.ReadString('LastDirs', 'LastDir' + i.ToString, '');
      popLastDirs.Items[i].Visible := popLastDirs.Items[i].Caption > '';
    end;

    cbDarkMode.Checked := Config.ReadBool('Options', 'DarkMode', False);
  finally
    Config.Free;
  end;

  if (popLastDirs.Items[0].Caption = '') and (DirectoryExists(CleanAndExpandDirectory('../../images/general_purpose/'))) then
  begin
    popLastDirs.Items[0].Caption := CleanAndExpandDirectory('../../images/general_purpose/');
    popLastDirs.Items[0].Visible := popLastDirs.Items[0].Caption > '';
  end;

  if DirectoryExists(popLastDirs.Items[0].Caption) then
    DirectoryEdit.Directory := popLastDirs.Items[0].Caption;

  sbtnLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;

  Config := TIniFile.Create('IconTableConfig.ini');
  try
    Config.WriteInteger('Position', 'Top', RestoredTop);
    Config.WriteInteger('Position', 'Left', RestoredLeft);
    Config.WriteInteger('Position', 'Width', RestoredWidth);
    Config.WriteInteger('Position', 'Height', RestoredHeight);
    Config.WriteInteger('Position', 'WindowState', Integer(WindowState));

    for i := 0 to LastDirsMax do
      Config.WriteString('LastDirs', 'LastDir' + i.ToString, popLastDirs.Items[i].Caption);

    Config.WriteBool('Options', 'DarkMode', cbDarkMode.Checked);
  finally
    Config.Free;
  end;
end;

procedure TMainForm.cbDarkModeChange(Sender: TObject);
begin
  bbtnSave.Enabled := False;
  bbtnShow.Enabled := False;
end;

procedure TMainForm.bbtnCreateHTMLClick(Sender: TObject);
var
  AllFileList: TStringList;
  IcoFileList: TStringList;
  IcoNameList: TStringList;
  IcoSizeList: TStringList;
  PixSizeList: TStringList;
  InfoTxtList: TStringList;
  LineStr: String;
  SizeStr: String;
  TempStr: String;
  DPos: Integer;
  IntDummy: Integer;
  i: Integer;
  ips: Integer;
  isl: Integer;
  StartIdx: Integer = 0;
  IconGroups: Integer = 0;
  BodyFontColor: String = ' color: #000000;';
  BodyBackColor: String = ' background-color: #ffffff;';
  InfoFontColor: String = ' color: #000000;';
  InfoBackColor: String = ' background-color: #ffffe0;';
  HoverFontColor: String = ' color: #ffffff;';
  HoverBackColor: String = ' background-color: #303030;';
begin
  try
    AllFileList := TStringList.Create;
    IcoFileList := TStringList.Create;
    IcoNameList := TStringList.Create;
    IcoSizeList := TStringList.Create;
    PixSizeList := TStringList.Create;
    Screen.BeginWaitCursor;
    SynEdit.Lines.BeginUpdate;

    FindAllFiles(AllFileList, ImgDir, '*.png', False);

    if AllFileList.Count = 0 then
    begin
      ErrorMsg('No png image files found in ' + ImgDir);
      Exit;
    end;

    AllFileList.Sort;
    for i := 0 to AllFileList.Count - 1 do
    begin
      TempStr := ChangeFileExt(ExtractFileName(AllFileList.Strings[i]), '');
      DPos := LastDelimiter('_', TempStr);
      if DPos > 0 then
      begin
        SizeStr := RightStr(TempStr, Utf8Length(TempStr) - DPos);
        if TryStrToInt(SizeStr, IntDummy) then
        begin
          IcoFileList.Add(TempStr);
          IcoNameList.Add(Utf8Copy(TempStr, 1, DPos - 1));
          IcoSizeList.Add(SizeStr);
          if PixSizeList.IndexOf(SizeStr) = -1 then
            PixSizeList.Add(SizeStr);
        end;
      end;
    end;
    PixSizeList.Sort;

    if IcoFileList.Count = 0 then
    begin
      ErrorMsg('No matching png image files found in ' + ImgDir);
      Exit;
    end;

    if cbDarkMode.Checked then
    begin
      BodyFontColor := ' color: #ffffff;';
      BodyBackColor := ' background-color: #303030;';
      InfoFontColor := ' color: #ffffff;';
      InfoBackColor := ' background-color: #000000;';
      HoverFontColor := ' color: #000000;';
      HoverBackColor := ' background-color: #ffffff;';
    end;

    SynEdit.Lines.Clear;
    SynEdit.Lines.Add('<!DOCTYPE html>');
    SynEdit.Lines.Add('<html>');
    SynEdit.Lines.Add('<head>');
    SynEdit.Lines.Add('<title>Icons</title>');
    SynEdit.Lines.Add('<meta charset="UTF-8">');
    SynEdit.Lines.Add('<style media="all">');
    SynEdit.Lines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px;' + BodyBackColor + BodyFontColor + '}');
    SynEdit.Lines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    SynEdit.Lines.Add('  tr:hover {' + HoverBackColor + HoverFontColor + '}');
    SynEdit.Lines.Add('  td {border-bottom: 1px solid #ddd; padding: 15px; text-align: left;}');
    SynEdit.Lines.Add('  td.topleft {border-bottom: 5px solid #ddd; padding: 15px; text-align: left;' + InfoBackColor + InfoFontColor + '}');
    SynEdit.Lines.Add('  td.topcenter {border-bottom: 5px solid #ddd; padding: 15px; text-align: center;' + InfoBackColor + InfoFontColor + '}');
    SynEdit.Lines.Add('  .info_container {margin: 0 auto; width: 500px; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 15px; margin-top: 30px;' + InfoBackColor + InfoFontColor + '}');
    SynEdit.Lines.Add('</style>');
    SynEdit.Lines.Add('</head>');
    SynEdit.Lines.Add('<body>');
    SynEdit.Lines.Add('<table>');
    SynEdit.Lines.Add('  <tr>');
    SynEdit.Lines.Add('    <td class="topleft">Name</td>');
    for i := 0 to PixSizeList.Count - 1 do
      SynEdit.Lines.Add('    <td class="topcenter">' + PixSizeList[i] + '</td>');
    SynEdit.Lines.Add('  </tr>');

    for i := 0 to IcoFileList.Count - 1 do
    begin
      if (i = IcoFileList.Count - 1) or (IcoNameList[i + 1] <> IcoNameList[i]) then
      begin
        SynEdit.Lines.Add('  <tr>');
        SynEdit.Lines.Add('    <td>' + IcoNameList[i] + '</td>');
        for ips := 0 to PixSizeList.Count - 1 do
        begin
          LineStr := '';
          for isl := StartIdx to i do
            if IcoSizeList[isl] = PixSizeList[ips] then
              LineStr := '    <td><img src="' + IcoFileList.Strings[isl] + '.png" alt=""></td>';
          if LineStr > '' then
            SynEdit.Lines.Add(LineStr)
          else
            SynEdit.Lines.Add('    <td></td>');
        end;
        SynEdit.Lines.Add('  </tr>');
        StartIdx := i + 1;
        IconGroups := IconGroups + 1;
      end;
    end;

    SynEdit.Lines.Add('</table>');

    SynEdit.Lines.Add('<div class="info_container">');
    SynEdit.Lines.Add('This folder contains ' + IntToStr(IcoFileList.Count) + ' icons in ' + IntToStr(IconGroups) + ' icon groups with ' + IntToStr(PixSizeList.Count) + LineEnding + ' icon sizes.');
    if FileExists(ImgDir + 'lazarus_general_purpose_images.txt') then
    begin
      try
        InfoTxtList := TStringList.Create;
        InfoTxtList.LoadFromFile(ImgDir + 'lazarus_general_purpose_images.txt');
        SynEdit.Lines.Add('<hr>');
        for i := 0 to InfoTxtList.Count - 1 do
          SynEdit.Lines.Add(InfoTxtList[i] + '<br>');
      finally
        InfoTxtList.Free;
      end;
    end;
    SynEdit.Lines.Add('</div>');

    SynEdit.Lines.Add('</body>');
    SynEdit.Lines.Add('</html>');

    bbtnSave.Enabled := True;
    bbtnSave.SetFocus;
    bbtnShow.Enabled := False;
  finally
    AllFileList.Free;
    IcoFileList.Free;
    IcoNameList.Free;
    IcoSizeList.Free;
    PixSizeList.Free;
    SynEdit.Lines.EndUpdate;
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.bbtnSaveClick(Sender: TObject);
begin
  fn := ImgDir + 'IconTable.html';
  try
    SynEdit.Lines.SaveToFile(fn);
    InfoMsg('Saved as: ' + fn);
    bbtnShow.Enabled := True;
    bbtnShow.SetFocus;
  except
    ErrorMsg('The file could not be saved as: ' + fn);
  end;
end;

procedure TMainForm.bbtnShowClick(Sender: TObject);
begin
  fn := ImgDir + 'IconTable.html';
  if FileExists(fn) then
    OpenURL(fn);
  bbtnCreateHTML.SetFocus;
end;

procedure TMainForm.bbtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.DirectoryEditEditingDone(Sender: TObject);
var
  i: Integer;
begin
  if DirectoryExists(DirectoryEdit.Directory) then
  begin
    ImgDir := CleanAndExpandDirectory(DirectoryEdit.Directory);
    bbtnCreateHTML.Enabled := True;

    for i := 0 to LastDirsMax do
      if ImgDir = popLastDirs.Items[i].Caption then
      begin
        popLastDirs.Items[i].MenuIndex := 0;
        Exit;
      end;

    popLastDirs.Items[LastDirsMax].Caption := ImgDir;
    popLastDirs.Items[LastDirsMax].MenuIndex := 0;
  end;
end;

procedure TMainForm.DirectoryEditChange(Sender: TObject);
begin
  bbtnCreateHTML.Enabled := DirectoryExists(DirectoryEdit.Directory);
end;

procedure TMainForm.LastDirClick(Sender: TObject);
begin
  if DirectoryExists(TMenuItem(Sender).Caption) then
  begin
    TMenuItem(Sender).MenuIndex := 0;
    DirectoryEdit.Directory := TMenuItem(Sender).Caption;
  end;
end;

procedure TMainForm.sbtnLastDirsClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := sbtnLastDirs.ClientToScreen(Point(sbtnLastDirs.Width, sbtnLastDirs.Height));
  popLastDirs.PopUp(pt.X, pt.Y);
end;

procedure TMainForm.InfoMsg(const AMsg: String);
begin
  TaskDialog.Caption := 'Information';
  TaskDialog.MainIcon := tdiInformation;
  TaskDialog.Title := 'Information';
  TaskDialog.CommonButtons := [tcbOk];
  TaskDialog.DefaultButton := tcbOk;
  TaskDialog.Text := AMsg;
  TaskDialog.Execute;
end;

procedure TMainForm.ErrorMsg(const AMsg: String);
begin
  TaskDialog.Caption := 'Error';
  TaskDialog.MainIcon := tdiError;
  TaskDialog.Title := 'Error';
  TaskDialog.CommonButtons := [tcbOk];
  TaskDialog.DefaultButton := tcbOk;
  TaskDialog.Text := AMsg;
  TaskDialog.Execute;
end;

end.
