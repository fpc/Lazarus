unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs; 

type

  { TSubControl }

  TSubControl = class(TCustomControl)
  public
    procedure Paint; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    SubControl: TSubControl;
    ClickCounter: Integer;
  end; 

var
  Form1: TForm1; 

implementation

{ TSubControl }

procedure TSubControl.Paint;
begin
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(0, 0, Width, Height);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin
  Caption := Format('Form click #%d', [ClickCounter]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SubControl := TSubControl.Create(Self);
  SubControl.Left := 100;
  SubControl.Top := 100;
  SubControl.Width := 100;
  SubControl.Height := 100;
  SubControl.Parent := Self;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clRed;
  Canvas.Rectangle(10, 10, 100, 100);
  Canvas.Brush.Color := clGreen;
  Canvas.Rectangle(100, 100, 200, 200);
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(200, 200, 300, 300);
end;

end.

