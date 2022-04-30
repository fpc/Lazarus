unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, EditBtn, FileUtil,
  LazUTF8, LazFileUtils, UITypes, SynEdit, SynHighlighterHTML;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnCreateHTML: TButton;
    btnSave: TButton;
    btnClose: TButton;
    DirectoryEdit: TDirectoryEdit;
    SynEdit: TSynEdit;
    SynHTMLSyn: TSynHTMLSyn;
    TaskDialog: TTaskDialog;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCreateHTMLClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  DirectoryEdit.Text := CleanAndExpandDirectory('../../images/general_purpose/');
end;

procedure TMainForm.btnCreateHTMLClick(Sender: TObject);
var
  FileList: TStringList;
  PixSizeStr: String;
  i: Integer;
begin
  SynEdit.Lines.BeginUpdate;
  try
    SynEdit.Lines.Clear;
    SynEdit.Lines.Add('<!DOCTYPE html>');
    SynEdit.Lines.Add('<html lang="de">');
    SynEdit.Lines.Add('<head>');
    SynEdit.Lines.Add('<title>Icons</title>');
    SynEdit.Lines.Add('<meta charset="UTF-8">');
    SynEdit.Lines.Add('<style media="all">');
    SynEdit.Lines.Add('  body {font-family: sans-serif; font-size: 16px; font-weight: 400; margin: 0 auto; padding: 30px 0px 80px 0px;}');
    SynEdit.Lines.Add('  table {border-collapse: collapse; margin-left: auto; margin-right: auto;}');
    SynEdit.Lines.Add('  td {border-bottom: 1px solid #ddd; padding: 15px; text-align: left;}');
    SynEdit.Lines.Add('  .top_container {margin: 0 auto; width: 500px; background-color: #f7f7f7; box-shadow: 0px 0px 5px 3px rgba(192, 192, 192, 0.37); padding: 15px; margin-bottom: 30px;}');
    SynEdit.Lines.Add('</style>');
    SynEdit.Lines.Add('</head>');
    SynEdit.Lines.Add('<body>');
    SynEdit.Lines.Add('<div class="top_container">');
    SynEdit.Lines.Add('The images in this folder can be used in Lazarus applications as toolbar or button icons.<br><br>');
    SynEdit.Lines.Add('They come in several sizes as required by the LCL scaling for high-dpi screens:<br><br>');
    SynEdit.Lines.Add('- 16x16, 24x24 and 32x32 pixels for "small" images, and<br>');
    SynEdit.Lines.Add('- 24x24, 36x36 and 48x48 pixels for "medium" sized images, and<br>');
    SynEdit.Lines.Add('- 32x32, 48x48 and 64x64 pixels for "large" images.<br><br>');
    SynEdit.Lines.Add('The images were kindly provided by Roland Hahn.<br><br>');
    SynEdit.Lines.Add('License:<br>');
    SynEdit.Lines.Add('Creative Commons CC0 1.0 Universal<br>');
    SynEdit.Lines.Add('(freely available, no restrictions in usage)');
    SynEdit.Lines.Add('</div>');
    SynEdit.Lines.Add('<table>');

    Screen.BeginWaitCursor;
    FileList := TStringList.Create;
    try
      FindAllFiles(FileList, DirectoryEdit.Text, '*.png', False);
      if FileList.Count > 0 then
      begin
        FileList.Sort;
        for i := 0 to FileList.Count - 1 do
        begin
          FileList.Strings[i] := ChangeFileExt(ExtractFileName(FileList.Strings[i]), '');
          PixSizeStr := RightStr(FileList.Strings[i], 3);
          if PixSizeStr = '_16' then
          begin
            SynEdit.Lines.Add('  <tr>');
            SynEdit.Lines.Add('    <td>' + Utf8Copy(FileList.Strings[i], 1, UTF8Length(FileList.Strings[i]) - 3) + '</td>');
          end;
          SynEdit.Lines.Add('    <td><img src="' + FileList.Strings[i] + '.png" alt=""></td>');
          if PixSizeStr = '_64' then
            SynEdit.Lines.Add('  </tr>');
        end;
      end else
      begin                   
        SynEdit.Lines.Clear;
        TaskDialog.Caption := 'Error';
        TaskDialog.MainIcon := tdiError;
        TaskDialog.Title := 'Error';
        TaskDialog.CommonButtons := [tcbOk];
        TaskDialog.DefaultButton := tcbOk;
        TaskDialog.Text := 'No png image files found in ' + DirectoryEdit.Text;
        TaskDialog.Execute;
        exit;
      end;
        
    finally
      FileList.Free;
      Screen.EndWaitCursor;
    end;
  
    SynEdit.Lines.Add('</table>');
    SynEdit.Lines.Add('</body>');
    SynEdit.Lines.Add('</html>');
    
  finally
    SynEdit.Lines.EndUpdate;
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  fn: String;
begin
  fn := AppendPathDelim(DirectoryEdit.Text) + 'IconTable.html';
  try
    SynEdit.Lines.SaveToFile(fn);
  except
    TaskDialog.Caption := 'Error';
    TaskDialog.MainIcon := tdiError;
    TaskDialog.Title := 'Error';
    TaskDialog.CommonButtons := [tcbOk];
    TaskDialog.DefaultButton := tcbOk;
    TaskDialog.Text := 'The file could not be saved as: ' + fn;
    TaskDialog.Execute;
    Exit;
  end;
  TaskDialog.Caption := 'Information';
  TaskDialog.MainIcon := tdiInformation;
  TaskDialog.Title := 'Information';
  TaskDialog.CommonButtons := [tcbOk];
  TaskDialog.DefaultButton := tcbOk;
  TaskDialog.Text := 'Saved as: ' + fn;
  TaskDialog.Execute;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
