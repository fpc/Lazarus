unit shapedwindowtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntf, LCLType;

type

  { TfrmShapedWindow }

  TfrmShapedWindow = class(TForm)
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmShapedWindow: TfrmShapedWindow;

implementation

{ TfrmShapedWindow }

procedure TfrmShapedWindow.FormShow(Sender: TObject);
{var
  Rgn: HRGN;
begin
  Rgn := LCLIntf.CreateEllipticRgn(0, 0, 200, 200);
  LCLIntf.SetWindowRgn(Handle, Rgn, False);
  LCLIntf.DeleteObject(Rgn);}
var
  Shape: TBitmap;
begin
  Shape := TBitmap.Create;
  try
    Shape.Width := 200;
    Shape.Height := 200;
    Shape.Canvas.Ellipse(0, 0, 200, 200);
    SetShape(Shape);
  finally
    Shape.Free;
  end;
end;

initialization
  {$I shapedwindowtest.lrs}

end.

