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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure LastDirClick(Sender: TObject);
    procedure sbtnLastDirsClick(Sender: TObject);
  private
    ImgDirectory: String;
    function GetImgDirectory(P: String): String;
    procedure ErrorMsg(const AMsg: String);
    procedure InfoMsg(const AMsg: String);
    procedure UpdateLastDirs(ImgDir: String; Delete: Boolean);
  public

  end;

var
  MainForm: TMainForm;

function CustomSortProc(List: TStringList; X1, X2: Integer): Integer;

implementation

{$R *.lfm}

const
  ConfigFileName = 'IconTableConfig.ini';
  IconTableFileName = 'IconTable.html';
  InfoTextFileName = 'lazarus_general_purpose_images.txt';
  DefaultDirectory = '../../images/general_purpose/';
  LastDirsMax = 9;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  MenItem: TMenuItem;
begin
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
  Config: TIniFile;
  StartDirectory: String;
begin
  Config := TIniFile.Create(Application.Location + ConfigFileName);
  try
    Top := Config.ReadInteger('Position', 'Top', (Screen.Height - Constraints.MinHeight) div 2);
    Left := Config.ReadInteger('Position', 'Left', (Screen.Width - Constraints.MinWidth) div 2);
    Width := Config.ReadInteger('Position', 'Width', Constraints.MinWidth);
    Height := Config.ReadInteger('Position', 'Height', Constraints.MinHeight);

    if (Left < -Width div 2) or (Top < -Height div 2) or (Left + Width div 2 > Screen.DesktopWidth) or (Top + Height div 2 > Screen.DesktopHeight) then
    begin
      Top := (Screen.Height - Constraints.MinHeight) div 2;
      Left := (Screen.Width - Constraints.MinWidth) div 2;
      Width := Constraints.MinWidth;
      Height := Constraints.MinHeight;
    end;

    for i := 0 to LastDirsMax do
    begin
      popLastDirs.Items[i].Caption := Config.ReadString('LastDirs', 'LastDir' + i.ToString, '');
      popLastDirs.Items[i].Visible := popLastDirs.Items[i].Caption > '';
    end;

    cbDarkMode.Checked := Config.ReadBool('Options', 'DarkMode', False);
  finally
    Config.Free;
  end;

  if ParamCount > 0 then
  begin
    StartDirectory := GetImgDirectory(ParamStr(1));
    if StartDirectory > '' then
      UpdateLastDirs(StartDirectory, False);
  end;

  if (popLastDirs.Items[0].Caption = '') and (DirectoryExists(CleanAndExpandDirectory(DefaultDirectory))) then
    UpdateLastDirs(CleanAndExpandDirectory(DefaultDirectory), False);

  if DirectoryExists(popLastDirs.Items[0].Caption) then
    DirectoryEdit.Directory := popLastDirs.Items[0].Caption;

  sbtnLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  DropDirectory: String;
begin
  DropDirectory := GetImgDirectory(FileNames[0]);
  if DropDirectory > '' then
  begin
    DirectoryEdit.Directory := DropDirectory;
    UpdateLastDirs(DropDirectory, False);
    MainForm.BringToFront;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
  Config: TIniFile;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;

  Config := TIniFile.Create(Application.Location + ConfigFileName);
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
      ErrorMsg('The configuration could not be saved.');
    end;
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

    FindAllFiles(AllFileList, ImgDirectory, '*.png', False);

    if AllFileList.Count = 0 then
    begin
      ErrorMsg('No png image files found in ' + ImgDirectory);
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
    PixSizeList.CustomSort(@CustomSortProc);

    if IcoFileList.Count = 0 then
    begin
      ErrorMsg('No matching png image files found in ' + ImgDirectory);
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
    SynEdit.Lines.Add('    <td class="colorset2 text_center" colspan="' + PixSizeList.Count.ToString + '">Appendix</td>');
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
              LineStr := '    <td><img src="' + IcoFileList.Strings[isl] + '.png" loading="lazy" alt=""></td>';
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
    SynEdit.Lines.Add('This folder contains ' + IcoFileList.Count.ToString + ' icons in ' + IconGroups.ToString + ' icon groups with ' + PixSizeList.Count.ToString + ' icon sizes.');
    if FileExists(ImgDirectory + InfoTextFileName) then
    begin
      try
        InfoTxtList := TStringList.Create;
        InfoTxtList.LoadFromFile(ImgDirectory + InfoTextFileName);
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
    UpdateLastDirs(ImgDirectory, False);
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
  try
    SynEdit.Lines.SaveToFile(ImgDirectory + IconTableFileName);
    InfoMsg('Saved as: ' + ImgDirectory + IconTableFileName);
    bbtnShow.Enabled := True;
    bbtnShow.SetFocus;
  except
    ErrorMsg('The file could not be saved as: ' + ImgDirectory + IconTableFileName);
  end;
end;

procedure TMainForm.bbtnShowClick(Sender: TObject);
begin
  if FileExists(ImgDirectory + IconTableFileName) then
    OpenURL(ImgDirectory + IconTableFileName);
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
    ImgDirectory := CleanAndExpandDirectory(DirectoryEdit.Directory);
    SynEdit.Clear;
    bbtnCreateHTML.Enabled := True;
    bbtnSave.Enabled := False;
    bbtnShow.Enabled := False;
    bbtnCreateHTML.SetFocus;
  end
  else
    bbtnCreateHTML.Enabled := False;
end;

procedure TMainForm.LastDirClick(Sender: TObject);
begin
  if DirectoryExists(TMenuItem(Sender).Caption) then
  begin
    DirectoryEdit.Directory := TMenuItem(Sender).Caption;
    TMenuItem(Sender).MenuIndex := 0;
  end
  else
  begin
    TaskDialog.Caption := 'Information';
    TaskDialog.MainIcon := tdiInformation;
    TaskDialog.Title := 'Information';
    TaskDialog.CommonButtons := [tcbYes, tcbNo];
    TaskDialog.DefaultButton := tcbNo;
    TaskDialog.Text := 'The folder [' + TMenuItem(Sender).Caption + '] does not exist or is currently not available.' +
      LineEnding + LineEnding + 'Should it be removed from the list?';
    TaskDialog.Execute;
    if TaskDialog.ModalResult = mrYes then
      UpdateLastDirs(TMenuItem(Sender).Caption, True);
  end;
end;

procedure TMainForm.sbtnLastDirsClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := sbtnLastDirs.ClientToScreen(Point(sbtnLastDirs.Width, sbtnLastDirs.Height));
  popLastDirs.PopUp(pt.X, pt.Y);
end;

procedure TMainForm.UpdateLastDirs(ImgDir: String; Delete: Boolean);
var
  i: Integer;
begin
  i := popLastDirs.Items.IndexOfCaption(ImgDir);
  if i > -1 then
  begin
    popLastDirs.Items[i].MenuIndex := 0;
    if Delete then
    begin
      popLastDirs.Items[0].MenuIndex := LastDirsMax;
      popLastDirs.Items[LastDirsMax].Caption := '';
      popLastDirs.Items[LastDirsMax].Visible := False;
    end;
  end
  else
  begin
    popLastDirs.Items[LastDirsMax].Caption := ImgDir;
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

function TMainForm.GetImgDirectory(P: String): String;
begin
  if FileExists(P) then
    Exit(CleanAndExpandDirectory(ExtractFilePath(P)));

  if DirectoryExists(P) then
    Exit(CleanAndExpandDirectory(P));

  Result := '';
end;

function CustomSortProc(List: TStringList; X1, X2: Integer): Integer;
var
  P1, P2: Integer;
begin
  if not TryStrToInt(List[X1], P1) then
    P1 := 0;
  if not TryStrToInt(List[X2], P2) then
    P2 := 0;

  //CustomSort sorts the stringlist with a custom comparison function.
  //The function should compare 2 elements in the list, and return a negative number
  //if the first item is before the second. It should return 0 if the elements are equal,
  //and a positive result indicates that the second elements should be before the first.
  Result := P1 - P2;
end;

end.
