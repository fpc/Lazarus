unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons, FileCtrl,
  lmf, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    cmbFileName: TComboBox;
    lblFileName: TLabel;
    OpenDialog: TOpenDialog;
    PaintBox: TPaintBox;
    FileSelectorPanel: TPanel;
    sbBrowse: TSpeedButton;
    sbOpenFile: TSpeedButton;
    procedure cmbFileNameDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cmbFileNameSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure sbBrowseClick(Sender: TObject);
    procedure sbOpenFileClick(Sender: TObject);
  private
    FLmfImg: TlmfImage;
    procedure AddToHistory(const AFileName: String);

    function CreateIni: TCustomIniFile;
    procedure ReadIni;
    procedure WriteIni;

  public
    procedure OpenFile(const AFileName: String);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  MAX_HISTORY = 20;

{ TMainForm }

procedure TMainForm.AddToHistory(const AFileName: String);
begin
  cmbFileName.AddHistoryItem(cmbFileName.Text, nil, MAX_HISTORY, true, false);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FLmfImg.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ReadIni;
  if ParamCount > 0 then
  begin
    cmbFileName.Text := ParamStr(1);
    OpenFile(cmbFileName.Text);
  end;
end;

procedure TMainForm.cmbFileNameSelect(Sender: TObject);
begin
  OpenFile(cmbFileName.Text);
end;

procedure TMainForm.cmbFileNameDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  fn: String;
begin
  fn := ExpandFileName(cmbFileName.Items[Index]);
  fn := MinimizeName(fn, cmbFileName.Canvas, cmbFileName.ClientWidth - Scale96ToFont(12));
  cmbFileName.Canvas.TextOut(ARect.Left, ARect.Top, fn);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    WriteIni;
end;

function TMainForm.CreateIni: TCustomIniFile;
begin
  Result := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TMainForm.OpenFile(const AFileName: String);
begin
  if AFileName = '' then
  begin
    MessageDlg('No filename specified.', mtError, [mbOk], 0);
    exit;
  end;
  if not FileExists(AFileName) then
  begin
    MessageDlg('File "' + AFileName + '" does not exist.', mtError, [mbOK], 0);
    exit;
  end;
  if FLmfImg <> nil then
    FLmfImg.Free;

  FLmfImg := TlmfImage.Create;
  FLmfImg.LoadFromLMFFile(AFileName);

  AddToHistory(AFileName);
  Caption := 'WMF File Reader [' + ExtractfileName(AFileName) + ']';
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
begin
  if (FLmfImg = nil) then //or FLmfImg.Empty then
    exit;
  PaintBox.Canvas.StretchDraw(Rect(0, 0, PaintBox.Width, PaintBox.Height), FLmfImg);
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L: TStrings;
  i: Integer;
  fn: String;
begin
  ini := CreateIni;
  try
    L := TStringList.Create;
    ini.ReadSection('History', L);
    for i := 0 to L.Count-1 do
    begin
      fn := ini.ReadString('History', L[i], '');
      if (fn <> '') and FileExists(fn) then
        cmbFileName.Items.Add(fn);
    end;
    L.Free;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.sbBrowseClick(Sender: TObject);
begin
  if cmbFileName.Text <> '' then
  begin
    OpenDialog.InitialDir := ExtractFileDir(cmbFileName.Text);
    OpenDialog.FileName := cmbFileName.Text;
  end;
  if OpenDialog.Execute then
  begin
    cmbFileName.Text := OpenDialog.FileName;
  end;
end;

procedure TMainForm.sbOpenFileClick(Sender: TObject);
begin
  OpenFile(cmbFileName.Text);
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := CreateIni;
  try
    for i := 0 to cmbFileName.Items.Count-1 do
      ini.WriteString('History', Format('Item %d', [i+1]), cmbFileName.Items[i]);
  finally
    ini.Free;
  end;
end;

end.

