unit shapedwindowtest;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, StdCtrls, LCLIntf;

type

  { TfrmShapedWindow }

  TfrmShapedWindow = class(TForm)
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end; 

var
  frmShapedWindow: TfrmShapedWindow;

implementation

{$R *.lfm}

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
    {$IFDEF LCLGtk2}
    Shape.Monochrome := True;
    {$ENDIF}
    Shape.Width := Width;
    Shape.Height := Height;
    Shape.Canvas.Ellipse(0, 0, Width, Height);
    SetShape(Shape);
  finally
    Shape.Free;
  end;
end;

procedure TfrmShapedWindow.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

