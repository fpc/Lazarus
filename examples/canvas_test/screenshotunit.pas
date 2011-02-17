unit screenshotunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons;

type

  { TfrmScreenshot }

  TfrmScreenshot = class(TForm)
    btnScreenshot: TBitBtn;
    imgScreenshot: TImage;
    procedure btnScreenshotClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmScreenshot: TfrmScreenshot;

implementation

{ TfrmScreenshot }

procedure TfrmScreenshot.btnScreenshotClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromDevice(0);
  imgScreenshot.Picture.Bitmap.Assign(bmp);
  FreeAndNil(bmp);
end;

initialization
  {$I screenshotunit.lrs}

end.

