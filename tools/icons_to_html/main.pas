unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, FPImage, Controls, Dialogs, EditBtn, FileUtil,
  LazUTF8, LazFileUtils, LCLIntf, LCLType, Buttons, Menus, IniFiles,
  SynEdit, SynHighlighterHTML, DefaultTranslator, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    bbtnClose: TBitBtn;
    bbtnCreateHTML: TBitBtn;
    bbtnSave: TBitBtn;
    bbtnPreview: TBitBtn;
    DirectoryEdit: TDirectoryEdit;
    ImageList: TImageList;
    menuDarkHTMLpage: TMenuItem;
    menuHTMLpageEnglish: TMenuItem;
    popLastDirs: TPopupMenu;
    popMenu: TPopupMenu;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TaskDialog: TTaskDialog;
    ToolBar: TToolBar;
    tbLastDirs: TToolButton;
    tbMenu: TToolButton;
    procedure bbtnCloseClick(Sender: TObject);
    procedure bbtnCreateHTMLClick(Sender: TObject);
    procedure bbtnPreviewClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure cbDarkModeChange(Sender: TObject);
    procedure DirectoryEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure LastDirClick(Sender: TObject);
    procedure menuDarkHTMLpageClick(Sender: TObject);
    procedure menuHTMLpageEnglishClick(Sender: TObject);
  private
    ImgDirectory: String;
    function GetImgDirectory(P: String): String;
    procedure CreateHTML(HTMLLines: TStrings; Preview: Boolean);
    procedure ShowMsg(const AMsgCaption: String; const AMsg: String);
    procedure UpdateLastDirs(ImgDir: String; Delete: Boolean);
    procedure GetPixSize(FileName: String; var PixWidth: Integer; var PixHeight: Integer);
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
  InfoTextFileName = 'readme.txt';
  TempFileName = 'IconTableTemp.html';
  DefaultDirectory = '../../images/general_purpose/';
  LastDirsMax = 9;
  URL_RolandHahn = 'https://www.rhsoft.de/';
  URL_CC0 = 'https://creativecommons.org/publicdomain/zero/1.0/';
  License_CC0 = 'Creative Commons CC0 1.0 Universal';
  ThisFolderContains = 'This folder contains %0:d icons in %1:d icon groups with %2:d icon sizes.';

resourcestring
  rsInformation = 'Information';
  rsError = 'Error';
  rsTheConfigurationCouldNotBeSaved = 'Configuration file could not be saved.';
  rsTheTempFileCouldNotBeDeleted = 'Temporary file could not be deleted.';
  rsTheFileCouldNotBeSavedAs = 'The file could not be saved as: %s';
  rsSavedAs = 'Saved as: %s';
  rsNoPngImageFilesFoundIn = 'No PNG image files found in %s';
  rsNoMatchingPngImageFilesFoundIn = 'No matching PNG image files found in %s';
  rsTheFolderDoesNotExist = 'Folder "%s" does not exist or is currently not available.' + LineEnding + LineEnding + 'Should it be removed from the list?';
  rsThisFolderContains = ThisFolderContains;
  rsSize = 'Size';
  rsName = 'Name';
  rsInfoText1 = 'The images in this folder can be used in Lazarus applications as toolbar or button icons.';
  rsInfoText2 = 'The different sizes as required by LCL scaling for Hi-DPI screens can be used like this, for example:';
  rsSizeInfoSmall = '16x16, 24x24 and 32x32 pixels for "small" images,';
  rsSizeInfoMedium = '24x24, 36x36 and 48x48 pixels for "medium" sized images,';
  rsSizeInfoLarge = '32x32, 48x48 and 64x64 pixels for "large" images.';
  rsImagesWereProvidedBy = 'The images were kindly provided by Roland Hahn (%s).';
  rsLicense = 'License:' + LineEnding + '%s';
  rsFreelyAvailable = '(freely available, no restrictions in usage)';

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
    MenItem.ImageIndex := 0;
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

    menuDarkHTMLpage.Checked := Config.ReadBool('Options', 'DarkMode', menuDarkHTMLpage.Checked);
    menuHTMLpageEnglish.Checked := Config.ReadBool('Options', 'HTMLpageEnglish', menuHTMLpageEnglish.Checked);
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

  tbLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
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

      Config.WriteBool('Options', 'DarkMode', menuDarkHTMLpage.Checked);
      Config.WriteBool('Options', 'HTMLpageEnglish', menuHTMLpageEnglish.Checked);
    except
      ShowMsg(rsError, rsTheConfigurationCouldNotBeSaved);
    end;
  finally
    Config.Free;
  end;

  try
    if FileExists(Application.Location + TempFileName) then
      DeleteFile(Application.Location + TempFileName);
  except
    ShowMsg(rsError, rsTheTempFileCouldNotBeDeleted);
  end;
end;

procedure TMainForm.cbDarkModeChange(Sender: TObject);
begin
  bbtnPreview.Enabled := False;
  bbtnSave.Enabled := False;
end;

procedure TMainForm.bbtnCreateHTMLClick(Sender: TObject);
begin
  SynEdit.Lines.Clear;
  CreateHTML(SynEdit.Lines, False);

  if SynEdit.Text > '' then
  begin
    bbtnPreview.Enabled := True;
    bbtnSave.Enabled := True;
    bbtnPreview.SetFocus;
  end;
  UpdateLastDirs(ImgDirectory, False);
end;

procedure TMainForm.bbtnPreviewClick(Sender: TObject);
var
  HTMLLines: TStrings;
begin
  HTMLLines := TStringList.Create;
  CreateHTML(HTMLLines, True);
  try
    HTMLLines.SaveToFile(Application.Location + TempFileName);
  except
    ShowMsg(rsError, Format(rsTheFileCouldNotBeSavedAs, [Application.Location + TempFileName]));
  end;
  HTMLLines.Free;

  if FileExists(Application.Location + TempFileName) then
    OpenURL(Application.Location + TempFileName);

  bbtnSave.SetFocus;
end;

procedure TMainForm.bbtnSaveClick(Sender: TObject);
begin
  try
    SynEdit.Lines.SaveToFile(ImgDirectory + IconTableFileName);
    ShowMsg(rsInformation, Format(rsSavedAs, [ImgDirectory + IconTableFileName]));
  except
    ShowMsg(rsError, Format(rsTheFileCouldNotBeSavedAs, [ImgDirectory + IconTableFileName]));
  end;
end;

procedure TMainForm.bbtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.CreateHTML(HTMLLines: TStrings; Preview: Boolean);

  function Link(URL, AText: String): String;
  begin
    Result := Format('<a href="%s">%s</a>', [URL, AText]);
  end;

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
  translated: Boolean;
  IcoWidth: Integer = 0;
  IcoHeight: Integer = 0;
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
  Screen.BeginWaitCursor;
  AllFileList := TStringList.Create;
  IcoFileList := TStringList.Create;
  IcoNameList := TStringList.Create;
  IcoSizeList := TStringList.Create;
  PixSizeList := TStringList.Create;
  try
    translated := not menuHTMLpageEnglish.Checked;

    FindAllFiles(AllFileList, ImgDirectory, '*.png', False);

    if AllFileList.Count = 0 then
    begin
      ShowMsg(rsError, Format(rsNoPngImageFilesFoundIn, [ImgDirectory]));
      Exit;
    end;

    AllFileList.Sort;
    for i := 0 to AllFileList.Count - 1 do
    begin
      try
        GetPixSize(AllFileList.Strings[i], IcoWidth, IcoHeight);
        IcoSize := IntToStr(IcoWidth);
      except
        IcoSize := '';
      end;

      if IcoSize <> '' then
      begin
        IcoFile := ChangeFileExt(ExtractFileName(AllFileList.Strings[i]), '');
        DPos := LastDelimiter('_', IcoFile);
        if TryStrToInt(RightStr(IcoFile, Utf8Length(IcoFile) - DPos), IntDummy) then
          IcoName := Utf8Copy(IcoFile, 1, DPos - 1)
        else
          IcoName := IcoFile;

        if Preview then
          IcoFileList.Add('file:///' + ImgDirectory + IcoFile)
        else
          IcoFileList.Add(IcoFile);

        IcoNameList.Add(IcoName);
        IcoSizeList.Add(IcoSize);
        if PixSizeList.IndexOf(IcoSize) = -1 then
          PixSizeList.Add(IcoSize);
      end;
    end;
    PixSizeList.CustomSort(@CustomSortProc);

    if IcoFileList.Count = 0 then
    begin
      ShowMsg(rsError, Format(rsNoMatchingPngImageFilesFoundIn, [ImgDirectory]));
      Exit;
    end;

    if menuDarkHTMLpage.Checked then
    begin
      ColorSet1 := 'color: #ffffff; background-color: #5c0000;}';
      ColorSet2 := 'color: #ffffff; background-color: #000057;}';
      BodyColors := 'color: #ffffff; background-color: #303030;}';
      HoverColors := 'color: #000000; background-color: #ffffff;}';
    end;

    HTMLLines.Clear;
    HTMLLines.Add('<!DOCTYPE html>');
    HTMLLines.Add('<html>');
    HTMLLines.Add('<head>');
    HTMLLines.Add('<title>Icons</title>');
    HTMLLines.Add('<meta charset="UTF-8">');
    HTMLLines.Add('<style media="all">');
    HTMLLines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px; ' + BodyColors);
    HTMLLines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    HTMLLines.Add('  tr {border-bottom: 1px solid #ddd;}');
    HTMLLines.Add('  tr:hover {' + HoverColors);
    HTMLLines.Add('  td {padding: 10px 15px 10px 15px;}');
    HTMLLines.Add('  .colorset1 {' + ColorSet1);
    HTMLLines.Add('  .colorset2 {' + ColorSet2);
    HTMLLines.Add('  .text_center {text-align: center;}');
    HTMLLines.Add('  .right_border {border-right: 1px solid #ddd;}');
    HTMLLines.Add('  .no_border {border: 0;}');
    HTMLLines.Add('  .infobox {margin: 0 auto; width: 500px; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 10px 15px 10px 15px; margin-top: 30px;}');
    HTMLLines.Add('</style>');
    HTMLLines.Add('</head>');
    HTMLLines.Add('<body>');
    HTMLLines.Add('<table>');
    HTMLLines.Add('  <tr class="no_border">');
    HTMLLines.Add('    <td class="colorset1 right_border"></td>');
    HTMLLines.Add('    <td class="colorset2 text_center" colspan="' + PixSizeList.Count.ToString + '">' + IfThen(translated, rsSize, 'Size') + '</td>');
    HTMLLines.Add('  </tr>');
    HTMLLines.Add('  <tr>');
    HTMLLines.Add('    <td class="colorset1 right_border">' + IfThen(translated, rsName, 'Name') + '</td>');
    for i := 0 to PixSizeList.Count - 1 do
      HTMLLines.Add('    <td class="colorset2 text_center">' + PixSizeList[i] + '</td>');
    HTMLLines.Add('  </tr>');

    for i := 0 to IcoFileList.Count - 1 do
    begin
      if (i = IcoFileList.Count - 1) or (IcoNameList[i + 1] <> IcoNameList[i]) then
      begin
        HTMLLines.Add('  <tr>');
        HTMLLines.Add('    <td class="right_border">' + IcoNameList[i] + '</td>');
        for ips := 0 to PixSizeList.Count - 1 do
        begin
          LineStr := '';
          for isl := StartIdx to i do
            if IcoSizeList[isl] = PixSizeList[ips] then
              LineStr := '    <td><img src="' + IcoFileList.Strings[isl] + '.png" loading="lazy" alt=""></td>';
          if LineStr > '' then
            HTMLLines.Add(LineStr)
          else
            HTMLLines.Add('    <td></td>');
        end;
        HTMLLines.Add('  </tr>');
        StartIdx := i + 1;
        IconGroups := IconGroups + 1;
      end;
    end;

    HTMLLines.Add('</table>');

    HTMLLines.Add('<div class="infobox colorset2">');
    HTMLLines.Add(Format(IfThen(translated, rsThisFolderContains, ThisFolderContains), [IcoFileList.Count, IconGroups, PixSizeList.Count]));

    if FileExists(ImgDirectory + InfoTextFileName) then
    begin
      if not menuHTMLpageEnglish.Checked then
      begin
        HTMLLines.Add('<hr>');
        HTMLLines.Add('<p>' + rsInfoText1 + '</p>');
        HTMLLines.Add('<p>' + rsInfoText2 + '</p>');
        HTMLLines.Add('<ul>');
        HTMLLines.Add('  <li>' + rsSizeInfoSmall + '</li>');
        HTMLLines.Add('  <li>' + rsSizeInfoMedium + '</li>');
        HTMLLines.Add('  <li>' + rsSizeInfoLarge + '</li>');
        HTMLLines.Add('</ul>');
        HTMLLines.Add('<p>' + Format(rsImagesWereProvidedBy, [Link(URL_RolandHahn, URL_RolandHahn)]) + '</p>');
        HTMLLines.Add('<p>' + Format(rsLicense, [License_CC0]) + '<br>');
        HTMLLines.Add(Link(URL_CC0, URL_CC0) + '<br>');
        HTMLLines.Add(rsFreelyAvailable + '</p>');
      end
      else
      begin
        InfoTxtList := TStringList.Create;
        try
          InfoTxtList.LoadFromFile(ImgDirectory + InfoTextFileName);
          HTMLLines.Add('<hr>');
          for i := 0 to InfoTxtList.Count - 1 do
            HTMLLines.Add(InfoTxtList[i] + '<br>');
        finally
          InfoTxtList.Free;
        end;
      end;
    end;
    HTMLLines.Add('</div>');

    HTMLLines.Add('</body>');
    HTMLLines.Add('</html>');
  finally
    AllFileList.Free;
    IcoFileList.Free;
    IcoNameList.Free;
    IcoSizeList.Free;
    PixSizeList.Free;
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.DirectoryEditChange(Sender: TObject);
begin
  if DirectoryExists(DirectoryEdit.Directory) then
  begin
    ImgDirectory := CleanAndExpandDirectory(DirectoryEdit.Directory);
    SynEdit.Clear;
    bbtnCreateHTML.Enabled := True;
    bbtnPreview.Enabled := False;
    bbtnSave.Enabled := False;
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
    TaskDialog.Caption := rsInformation;
    TaskDialog.MainIcon := tdiInformation;
    TaskDialog.Title := rsInformation;
    TaskDialog.CommonButtons := [tcbYes, tcbNo];
    TaskDialog.DefaultButton := tcbNo;
    TaskDialog.Text := Format(rsTheFolderDoesNotExist, [TMenuItem(Sender).Caption]);
    TaskDialog.Execute;
    if TaskDialog.ModalResult = mrYes then
      UpdateLastDirs(TMenuItem(Sender).Caption, True);
  end;
end;

procedure TMainForm.menuDarkHTMLpageClick(Sender: TObject);
begin
  menuDarkHTMLpage.Checked := not menuDarkHTMLpage.Checked;
  if (bbtnCreateHTML.Enabled) and (SynEdit.Text <> '') then
    bbtnCreateHTML.Click;
end;

procedure TMainForm.menuHTMLpageEnglishClick(Sender: TObject);
begin
  menuHTMLpageEnglish.Checked := not menuHTMLpageEnglish.Checked;
  if (bbtnCreateHTML.Enabled) and (SynEdit.Text <> '') then
    bbtnCreateHTML.Click;
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
  tbLastDirs.Enabled := popLastDirs.Items[0].Caption > '';
end;

procedure TMainForm.ShowMsg(const AMsgCaption: String; const AMsg: String);
begin
  if AMsgCaption = rsError then
    TaskDialog.MainIcon := tdiError
  else
    TaskDialog.MainIcon := tdiInformation;
  TaskDialog.Caption := AMsgCaption;
  TaskDialog.Title := AMsgCaption;
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

procedure TMainForm.GetPixSize(FileName: String; var PixWidth: Integer; var PixHeight: Integer);
var
  stream: TStream;
  reader: TFPCustomImageReaderClass;
begin
  stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    reader := TFPCustomImage.FindReaderFromStream(stream);
    with reader.ImageSize(stream) do
    begin
      PixWidth := X;
      PixHeight := Y;
    end;
  finally
    stream.Free;
  end;
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
