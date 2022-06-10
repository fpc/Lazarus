unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, FileUtil,
  LazUTF8, LazFileUtils, LCLIntf, SynEdit, SynHighlighterHTML;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnCreateHTML: TButton;
    btnSave: TButton;
    btnShow: TButton;
    btnClose: TButton;
    cbDarkMode: TCheckBox;
    DirectoryEdit: TDirectoryEdit;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TaskDialog: TTaskDialog;
    procedure cbDarkModeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DirectoryEditChange(Sender: TObject);
    procedure btnCreateHTMLClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    fn: String;
    ImgDir: String;
    function GetDestFileName: String;
    procedure InfoMsg(const AMsg: String);
    procedure ErrorMsg(const AMsg: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  ImgDir := CleanAndExpandDirectory('../../images/general_purpose/');
  if DirectoryExists(ImgDir) then
    DirectoryEdit.Text := ImgDir
  else
    DirectoryEdit.Text := CleanAndExpandDirectory(GetCurrentDir);
end;

procedure TMainForm.cbDarkModeChange(Sender: TObject);
begin
  btnSave.Enabled := False;
  btnShow.Enabled := False;
end;

procedure TMainForm.DirectoryEditChange(Sender: TObject);
begin
  btnCreateHTML.Enabled := (DirectoryEdit.Text <> '') and DirectoryExists(DirectoryEdit.Text);
  if btnCreateHtml.Enabled then
    btnCreateHTML.SetFocus;
end;

procedure TMainForm.btnCreateHTMLClick(Sender: TObject);
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
  InfoBackColor: String = ' background-color: #ffffe0;';
begin
  try
    AllFileList := TStringList.Create;
    IcoFileList := TStringList.Create;
    IcoNameList := TStringList.Create;
    IcoSizeList := TStringList.Create;
    PixSizeList := TStringList.Create;
    Screen.BeginWaitCursor;
    SynEdit.Lines.BeginUpdate;

    ImgDir := CleanAndExpandDirectory(DirectoryEdit.Text);

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
      InfoBackColor := ' background-color: #000000;';
    end;

    SynEdit.Lines.Clear;
    SynEdit.Lines.Add('<!DOCTYPE html>');
    SynEdit.Lines.Add('<html>');
    SynEdit.Lines.Add('<head>');
    SynEdit.Lines.Add('<title>Icons</title>');
    SynEdit.Lines.Add('<meta charset="UTF-8">');
    SynEdit.Lines.Add('<style media="all">');
    SynEdit.Lines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px;' + BodyBackColor + BodyFontColor + '}');
    SynEdit.Lines.Add('  td.topleft {border-bottom: 5px solid #ddd; padding: 15px; text-align: left;' + InfoBackColor + '}');
    SynEdit.Lines.Add('  td.topcenter {border-bottom: 5px solid #ddd; padding: 15px; text-align: center;' + InfoBackColor + '}');
    SynEdit.Lines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    SynEdit.Lines.Add('  td {border-bottom: 1px solid #ddd; padding: 15px; text-align: left;}');
    SynEdit.Lines.Add('  .info_container {margin: 0 auto; width: 500px; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 15px; margin-top: 30px;' + InfoBackColor + '}');
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

    btnSave.Enabled := True;
    btnSave.SetFocus;
    btnShow.Enabled := False;
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

procedure TMainForm.btnSaveClick(Sender: TObject);
begin
  fn := GetDestFileName;
  try
    SynEdit.Lines.SaveToFile(fn);
    InfoMsg('Saved as: ' + fn);
    btnShow.Enabled := True;
    btnShow.SetFocus;
  except
    ErrorMsg('The file could not be saved as: ' + fn);
  end;
end;

procedure TMainForm.btnShowClick(Sender: TObject);
begin
  fn := GetDestFileName;
  if FileExists(fn) then
    OpenURL(fn);
  btnClose.SetFocus;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.GetDestFileName: String;
begin
  Result := AppendPathDelim(DirectoryEdit.Text) + 'IconTable.html';
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
