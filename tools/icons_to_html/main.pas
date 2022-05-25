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
    DirectoryEdit: TDirectoryEdit;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TaskDialog: TTaskDialog;
    procedure FormShow(Sender: TObject);
    procedure DirectoryEditChange(Sender: TObject);
    procedure btnCreateHTMLClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure ErrorMsg(const AMsg: String);
    procedure InfoMsg(const AMsg: String);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  if DirectoryExists(CleanAndExpandDirectory('../../images/general_purpose/')) then
    DirectoryEdit.Text := CleanAndExpandDirectory('../../images/general_purpose/')
  else
    DirectoryEdit.Text := CleanAndExpandDirectory(GetCurrentDir);
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
  LineStr: String;
  SizeStr: String;
  TempStr: String;
  DPos: Integer;
  IntDummy: Integer;
  i: Integer;
  ips: Integer;
  isl: Integer;
  StartIdx: Integer = 0;
begin
  try
    AllFileList := TStringList.Create;
    IcoFileList := TStringList.Create;
    IcoNameList := TStringList.Create;
    IcoSizeList := TStringList.Create;
    PixSizeList := TStringList.Create;
    Screen.BeginWaitCursor;
    SynEdit.Lines.BeginUpdate;

    FindAllFiles(AllFileList, DirectoryEdit.Text, '*.png', False);

    if AllFileList.Count = 0 then
    begin
      ErrorMsg('No png image files found in ' + DirectoryEdit.Text);
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
      ErrorMsg('No matching png image files found in ' + DirectoryEdit.Text);
      Exit;
    end;

    SynEdit.Lines.Clear;
    SynEdit.Lines.Add('<!DOCTYPE html>');
    SynEdit.Lines.Add('<html>');
    SynEdit.Lines.Add('<head>');
    SynEdit.Lines.Add('<title>Icons</title>');
    SynEdit.Lines.Add('<meta charset="UTF-8">');
    SynEdit.Lines.Add('<style media="all">');
    SynEdit.Lines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px;}');
    SynEdit.Lines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    SynEdit.Lines.Add('  td {border-bottom: 1px solid #ddd; padding: 15px; text-align: left;}');
    SynEdit.Lines.Add('  td.topleft {border-bottom: 5px solid #ddd; padding: 15px; text-align: left; background-color: #ffffe0;}');
    SynEdit.Lines.Add('  td.topcenter {border-bottom: 5px solid #ddd; padding: 15px; text-align: center; background-color: #ffffe0;}');
    SynEdit.Lines.Add('  .info_container {margin: 0 auto; width: 500px; background-color: #f7f7f7; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 15px; margin-top: 30px;}');
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
      end;
    end;

    SynEdit.Lines.Add('</table>');

    if RightStr(DirectoryEdit.Text, 16) = 'general_purpose' + PathDelim then
    begin
      SynEdit.Lines.Add('<div class="info_container">');
      SynEdit.Lines.Add('The images in this folder can be used in Lazarus applications as toolbar or button icons.<br><br>');
      SynEdit.Lines.Add('The different sizes as required by LCL scaling for high-dpi screens can be used like this, for example:<br><br>');
      SynEdit.Lines.Add('- 16x16, 24x24 and 32x32 pixels for "small" images, and<br>');
      SynEdit.Lines.Add('- 24x24, 36x36 and 48x48 pixels for "medium" sized images, and<br>');
      SynEdit.Lines.Add('- 32x32, 48x48 and 64x64 pixels for "large" images.<br><br>');
      SynEdit.Lines.Add('The images were kindly provided by Roland Hahn.<br><br>');
      SynEdit.Lines.Add('License:<br>');
      SynEdit.Lines.Add('Creative Commons CC0 1.0 Universal<br>');
      SynEdit.Lines.Add('(freely available, no restrictions in usage)');
      SynEdit.Lines.Add('</div>');
    end;

    SynEdit.Lines.Add('</body>');
    SynEdit.Lines.Add('</html>');
    
    btnSave.Enabled := true;
    btnSave.SetFocus;
    btnShow.Enabled := false;
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
var
  fn: String = '';
begin
  fn := AppendPathDelim(DirectoryEdit.Text) + 'IconTable.html';
  try
    SynEdit.Lines.SaveToFile(fn);
    InfoMsg('Saved as: ' + fn);
    btnShow.Enabled := true;
    btnShow.SetFocus;
  except
    ErrorMsg('The file could not be saved as: ' + fn);
  end;
end;

procedure TMainForm.btnShowClick(Sender: TObject);
var
  fn: String = '';
begin
  fn := AppendPathDelim(DirectoryEdit.Text) + 'IconTable.html';
  if FileExists(fn) then
    OpenURL(fn);
  btnClose.SetFocus;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
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

end.
