unit screenshotunit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, ExtCtrls, Buttons, LCLType, LCLIntf;

type

  { TfrmScreenshot }

  TfrmScreenshot = class(TForm)
    btnScreenshot: TBitBtn;
    imgScreenshot: TImage;
    procedure btnScreenshotClick(Sender: TObject);
  private

  public

  end; 

var
  frmScreenshot: TfrmScreenshot;

implementation

{$R *.lfm}

{ TfrmScreenshot }

procedure TfrmScreenshot.btnScreenshotClick(Sender: TObject);
var
  bmp: TBitmap;
  DC: HDC;
begin
  bmp := TBitmap.Create;
  DC := GetDC(0);
  try
    bmp.LoadFromDevice(DC);
  finally
    ReleaseDC(0, DC);
  end;
  imgScreenshot.Picture.Bitmap.Assign(bmp);
  FreeAndNil(bmp);
end;

end.

