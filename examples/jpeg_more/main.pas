unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Spin, EditBtn, FPReadJpeg;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnWrite: TButton;
    BtnRead: TButton;
    CbApplyMinSize: TCheckBox;
    CbGrayscale: TCheckBox;
    LblProgressive: TLabel;
    RbOtherFile: TRadioButton;
    CbProgressive: TCheckBox;
    CbSmoothing: TCheckBox;
    EdFileName: TFileNameEdit;
    RbUseCheetah: TRadioButton;
    EdMinWidth: TSpinEdit;
    EdMinHeight: TSpinEdit;
    FilenamePanel: TPanel;
    Image1: TImage;
    Image2: TImage;
    LblMinHeight: TLabel;
    LblQualityLevel: TLabel;
    LblSpeed: TLabel;
    LblQuality: TLabel;
    LblSizeInfo: TLabel;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    RgScale: TRadioGroup;
    RgPerformance: TRadioGroup;
    SbQuality: TScrollBar;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    PgRead: TTabSheet;
    PgWrite: TTabSheet;
    procedure BtnReadClick(Sender: TObject);
    procedure BtnWriteClick(Sender: TObject);
    procedure EdFileNameAcceptFileName(Sender: TObject; var Value: String);
    procedure EdFileNameButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SbQualityChange(Sender: TObject);
  private
    function GetFileName: String;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  CHEETAH_FILENAME = '../../images\splash_source/cheetah.jpg';

{ TForm1 }

procedure TForm1.BtnReadClick(Sender: TObject);
var
  filename: String;
  jpeg: TJpegImage;
  t: TDateTime;
begin
  fileName := GetFileName;
  if fileName = '' then begin
    MessageDlg('No filename', mtError, [mbOK], 0);
    exit;
  end;
  if not FileExists(filename) then begin
    MessageDlg(Format('File "%s" not found.', [filename]), mtError, [mbOK], 0);
    exit;
  end;

  LblSpeed.Caption := 'Time to load: ...';
  LblSpeed.Refresh;

  jpeg := TJpegImage.Create;
  try
    t := now;
    jpeg.Scale := TJPEGScale(RgScale.ItemIndex);
    jpeg.Smoothing := CbSmoothing.Checked;
    jpeg.Performance := TJPEGPerformance(RgPerformance.ItemIndex);
    if CbApplyMinSize.Checked then begin
      jpeg.MinWidth := EdMinWidth.Value;
      jpeg.MinHeight := EdMinHeight.Value;
    end;
    jpeg.LoadFromFile(FILENAME);
    t := now - t;
    Image1.Picture.Assign(jpeg);
    Image1.Width := Image1.Picture.Width;
    Image1.Height := Image1.Picture.Height;
    LblSizeInfo.Caption := Format('Size %d x %d', [jpeg.Width, jpeg.Height]);
    LblSpeed.Caption := 'Time to load: ' + FormatDateTime('s.zzz', t) + ' s';
    if jpeg.ProgressiveEncoding then
      LblProgressive.Caption := 'Progressive encoding'
    else
      LblProgressive.Caption := '';
  finally
    jpeg.Free;
  end;
end;

procedure TForm1.BtnWriteClick(Sender: TObject);
var
  jpeg: TJpegImage;
  fileName: String;
  newFileName: String;
begin
  fileName := GetFileName;
  if fileName = '' then begin
    MessageDlg('No filename', mtError, [mbOK], 0);
    exit;
  end;
  if not FileExists(filename) then begin
    MessageDlg(Format('File "%s" not found.', [filename]), mtError, [mbOK], 0);
    exit;
  end;

  newFileName := ChangeFileExt(filename, '') + '_mod.jpg';

  jpeg := TJpegImage.Create;
  try
    jpeg.LoadFromFile(filename);
    jpeg.CompressionQuality := SbQuality.Position;
    jpeg.GrayScale := CbGrayScale.Checked;
    jpeg.ProgressiveEncoding := CbProgressive.Checked;
    jpeg.SaveToFile(newFileName);
  finally
    jpeg.Free;
  end;

  jpeg := TJpegImage.Create;
  try
    jpeg.LoadFromFile(newFileName);
    Image2.Picture.Assign(jpeg);
    Image2.Width := Image2.Picture.Width;
    Image2.Height := Image2.Picture.Height;
  finally
    jpeg.Free;
  end;
end;

procedure TForm1.EdFileNameAcceptFileName(Sender: TObject; var Value: String);
begin
  RbOtherFile.Checked := true;
end;

procedure TForm1.EdFileNameButtonClick(Sender: TObject);
begin
  if EdFileName.FileName <> '' then
    EdFileName.InitialDir := ExtractFileDir(EdFilename.Filename);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LblProgressive.Caption := '';
  BtnReadClick(nil);
  SbQualityChange(nil);
end;

function TForm1.GetFileName: String;
begin
  if RbUseCheetah.Checked then
    Result := CHEETAH_FILENAME
  else
    Result := EdFileName.FileName;
end;

procedure TForm1.SbQualityChange(Sender: TObject);
begin
  LblQualityLevel.Caption := IntToStr(SbQuality.Position);
end;

end.

