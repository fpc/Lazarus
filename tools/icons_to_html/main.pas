unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, FileUtil,
  LazUTF8, LazFileUtils, LCLIntf, LCLType, Buttons, Menus, IniFiles,
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
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastDirClick(Sender: TObject);
    procedure sbtnLastDirsClick(Sender: TObject);
  private
    ImgDir: String;
    LastDirsMax: Integer;
    function GetIniFileName: String;
    procedure InfoMsg(const AMsg: String);
    procedure ErrorMsg(const AMsg: String);
    procedure UpdateLastDirs(D: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  LE2 = LineEnding + LineEnding;
  
{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  MenItem: TMenuItem;
begin
  Position := poScreenCenter;
  
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
  L, T, W, H: Integer;
  R: TRect;
  Config: TIniFile;
begin
  Constraints.MinWidth := cbDarkMode.Left + cbDarkMode.Width + cbDarkMode.BorderSpacing.Right + 
    bbtnClose.Left + bbtnClose.Width - bbtnCreateHTML.Left + bbtnClose.BorderSpacing.Right;
  Constraints.MinHeight := Constraints.MinWidth * 2 div 3;
  
  Position := poDesigned;
  
  Config := TIniFile.Create(GetIniFileName);
  try
    T := Config.ReadInteger('Position', 'Top', Top);
    L := Config.ReadInteger('Position', 'Left', Left);
    W := Config.ReadInteger('Position', 'Width', Width);
    H := Config.ReadInteger('Position', 'Height', Height);
    R := Screen.WorkAreaRect;
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
    if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
    SetBounds(L, T, W, H);
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
    popLastDirs.Items[0].Visible := True;
  end;

  if DirectoryExists(popLastDirs.Items[0].Caption) then
    DirectoryEdit.Directory := popLastDirs.Items[0].Caption;

  sbtnLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
  Config: TIniFile;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;

  Config := TIniFile.Create(GetIniFileName);
  try
    try
      Config.WriteInteger('Position', 'Top', RestoredTop);
      Config.WriteInteger('Position', 'Left', RestoredLeft);
      Config.WriteInteger('Position', 'Width', RestoredWidth);
      Config.WriteInteger('Position', 'Height', RestoredHeight);
      Config.WriteInteger('Position', 'WindowState', Integer(WindowState));

      for i := 0 to LastDirsMax do
        Config.WriteString('LastDirs', 'LastDir' + i.ToString, popLastDirs.Items[i].Caption);

      Config.WriteBool('Options', 'DarkMode', cbDarkMode.Checked);
    except
      on E: Exception do
        ErrorMsg('The configuration could not be saved.' + LE2 + E.Message);
    end;
  finally
    Config.Free;
  end;
end;

function TMainForm.GetIniFileName: String;
begin
  Result := Application.Location + 'IconTableConfig.ini';
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
  IcoFile: String;
  IcoSize: String;
  IcoName: String;
  DPos: Integer;
  IntDummy: Integer;
  i: Integer;
  ips: Integer;
  isl: Integer;
  StartIdx: Integer = 0;
  IconGroups: Integer = 0;
  ColorSet1: String = 'color: #000000; background-color: #ffffe0;}';
  ColorSet2: String = 'color: #000000; background-color: #fbfba8;}';
  BodyColors: String = 'color: #000000; background-color: #ffffff;}';
  HoverColors: String = 'color: #ffffff; background-color: #303030;}';
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
      IcoFile := ChangeFileExt(ExtractFileName(AllFileList.Strings[i]), '');
      DPos := LastDelimiter('_', IcoFile);
      IcoSize := RightStr(IcoFile, Utf8Length(IcoFile) - DPos);

      if not TryStrToInt(IcoSize, IntDummy) then
        IcoSize := '';

      if IcoSize = '' then
        IcoName := IcoFile
      else
        IcoName := Utf8Copy(IcoFile, 1, DPos - 1);

      IcoFileList.Add(IcoFile);
      IcoNameList.Add(IcoName);
      IcoSizeList.Add(IcoSize);
      if PixSizeList.IndexOf(IcoSize) = -1 then
        PixSizeList.Add(IcoSize);
    end;
    PixSizeList.Sort;

    if IcoFileList.Count = 0 then
    begin
      ErrorMsg('No matching png image files found in ' + ImgDir);
      Exit;
    end;

    if cbDarkMode.Checked then
    begin
      ColorSet1 := 'color: #ffffff; background-color: #5c0000;}';
      ColorSet2 := 'color: #ffffff; background-color: #000057;}';
      BodyColors := 'color: #ffffff; background-color: #303030;}';
      HoverColors := 'color: #000000; background-color: #ffffff;}';
    end;

    SynEdit.Lines.Clear;
    SynEdit.Lines.Add('<!DOCTYPE html>');
    SynEdit.Lines.Add('<html>');
    SynEdit.Lines.Add('<head>');
    SynEdit.Lines.Add('<title>Icons</title>');
    SynEdit.Lines.Add('<meta charset="UTF-8">');
    SynEdit.Lines.Add('<style media="all">');
    SynEdit.Lines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px; ' + BodyColors);
    SynEdit.Lines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    SynEdit.Lines.Add('  tr {border-bottom: 1px solid #ddd;}');
    SynEdit.Lines.Add('  tr:hover {' + HoverColors);
    SynEdit.Lines.Add('  td {padding: 10px 15px 10px 15px;}');
    SynEdit.Lines.Add('  .colorset1 {' + ColorSet1);
    SynEdit.Lines.Add('  .colorset2 {' + ColorSet2);
    SynEdit.Lines.Add('  .text_center {text-align: center;}');
    SynEdit.Lines.Add('  .right_border {border-right: 1px solid #ddd;}');
    SynEdit.Lines.Add('  .no_border {border: 0;}');
    SynEdit.Lines.Add('  .infobox {margin: 0 auto; width: 500px; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 10px 15px 10px 15px; margin-top: 30px;}');
    SynEdit.Lines.Add('</style>');
    SynEdit.Lines.Add('</head>');
    SynEdit.Lines.Add('<body>');
    SynEdit.Lines.Add('<table>');
    SynEdit.Lines.Add('  <tr class="no_border">');
    SynEdit.Lines.Add('    <td class="colorset1 right_border"></td>');
    SynEdit.Lines.Add('    <td class="colorset2 text_center" colspan="' + IntToStr(PixSizeList.Count) + '">Appendix</td>');
    SynEdit.Lines.Add('  </tr>');
    SynEdit.Lines.Add('  <tr>');
    SynEdit.Lines.Add('    <td class="colorset1 right_border">Name</td>');
    for i := 0 to PixSizeList.Count - 1 do
      SynEdit.Lines.Add('    <td class="colorset2 text_center">' + PixSizeList[i] + '</td>');
    SynEdit.Lines.Add('  </tr>');

    for i := 0 to IcoFileList.Count - 1 do
    begin
      if (i = IcoFileList.Count - 1) or (IcoNameList[i + 1] <> IcoNameList[i]) then
      begin
        SynEdit.Lines.Add('  <tr>');
        SynEdit.Lines.Add('    <td class="right_border">' + IcoNameList[i] + '</td>');
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

    SynEdit.Lines.Add('<div class="infobox colorset2">');
    SynEdit.Lines.Add('This folder contains ' + IntToStr(IcoFileList.Count) + ' icons in ' + IntToStr(IconGroups) + ' icon groups with ' + IntToStr(PixSizeList.Count) + ' icon sizes.');
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
    UpdateLastDirs(ImgDir);
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
var
  fn: String;
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
var
  fn: String;
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

procedure TMainForm.DirectoryEditChange(Sender: TObject);
begin
  if DirectoryExists(DirectoryEdit.Directory) then
  begin
    ImgDir := CleanAndExpandDirectory(DirectoryEdit.Directory);
    bbtnCreateHTML.Enabled := True;
  end
  else
    bbtnCreateHTML.Enabled := False;
  bbtnSave.Enabled := False;
  bbtnShow.Enabled := False;
end;

procedure TMainForm.LastDirClick(Sender: TObject);
begin
  if DirectoryExists(TMenuItem(Sender).Caption) then
  begin
    ImgDir := TMenuItem(Sender).Caption;
    DirectoryEdit.Directory := ImgDir;
    TMenuItem(Sender).MenuIndex := 0;
    bbtnSave.Enabled := False;
    bbtnShow.Enabled := False;
    SynEdit.Clear;
  end;
end;

procedure TMainForm.sbtnLastDirsClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := sbtnLastDirs.ClientToScreen(Point(sbtnLastDirs.Width, sbtnLastDirs.Height));
  popLastDirs.PopUp(pt.X, pt.Y);
end;

procedure TMainForm.UpdateLastDirs(D: String);
var
  i: Integer;
begin
  i := popLastDirs.Items.IndexOfCaption(D);
  if i > -1 then
    popLastDirs.Items[i].MenuIndex := 0
  else
  begin
    popLastDirs.Items[LastDirsMax].Caption := D;
    popLastDirs.Items[LastDirsMax].Visible := True;
    popLastDirs.Items[LastDirsMax].MenuIndex := 0;
  end;
  sbtnLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
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
