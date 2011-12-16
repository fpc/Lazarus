unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLProc, Arrow, StdCtrls, ComCtrls, LCLType, LCLIntf;

type
  TSubControl = class;

  { TForm1 }

  TForm1 = class(TForm)
    Arrow1: TArrow;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    procedure Arrow1Click(Sender: TObject);
    procedure Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Button1KeyPress(Sender: TObject; var Key: char);
    procedure Button1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    SubControl: TSubControl;
    ClickCounter: Integer;
    procedure HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
  end; 

  { TSubControl }

  TSubControl = class(TCustomControl)
  public
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
  end;

var
  Form1: TForm1; 

implementation

uses secondform;

{ TSubControl }

procedure TSubControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Up X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseEnter;
begin
  DebugLn('TSubControl.Mouse Enter');
end;

procedure TSubControl.MouseLeave;
begin
  DebugLn('TSubControl.Mouse Leave');
end;

procedure TSubControl.Paint;
begin
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(0, 0, Width, Height);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin
  DebugLn(Format('Form click #%d', [ClickCounter]));
  Inc(ClickCounter);
//  Invalidate;
end;

procedure TForm1.Arrow1Click(Sender: TObject);
begin
  Caption := 'Clicked Arrow';
  DebugLn('Clicked Arrow');
end;

procedure TForm1.Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Up X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DebugLn('Button1Click');
  ProgressBar1.Position := ProgressBar1.Position + 10;
end;

procedure TForm1.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DebugLn('[TForm1.Button1KeyDown] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyDown ' + LCLProc.DbgsVKCode(Key);
end;

procedure TForm1.Button1KeyPress(Sender: TObject; var Key: char);
begin
  DebugLn('KeyPress: ' + Key);
end;

procedure TForm1.Button1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  DebugLn('[TForm1.Button1KeyUp] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyUp ' + LCLProc.DbgsVKCode(Key);
end;

procedure TForm1.Button1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  DebugLn('UTF8KeyPress: ' + UTF8Key);
  Caption := UTF8Key;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.OnMessageDialogFinished := @HandleMessageDialogFinished;
  DebugLn('Button2Click A');
//  LCLIntf.MessageBox(0, 'Text', 'Title', MB_ABORTRETRYIGNORE);
  Application.MessageBox('Text', 'Title', MB_ABORTRETRYIGNORE);
  DebugLn('Button2Click B');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Form2.Show;
  DebugLn('Button3Click');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SubControl := TSubControl.Create(Self);
  SubControl.Left := 40;
  SubControl.Top := 160;
  SubControl.Width := 50;
  SubControl.Height := 50;
  SubControl.Parent := Self;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('MouseMove x=%d y=%d', [x, y]));
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  lPoints: array[0..2] of TPoint;
begin
  Canvas.Brush.Color := clRed;
  lPoints[0] := Point(67,57);
  lPoints[1] := Point(11,29);
  lPoints[2] := Point(67,1);
  Canvas.Polygon(lPoints);

{  Canvas.Brush.Color := clRed;
  Canvas.Rectangle(10, 10, 100, 100);
  Canvas.Brush.Color := clGreen;
  Canvas.Rectangle(100, 100, 200, 200);
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(200, 200, 300, 300);}
end;

procedure TForm1.HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
begin
  DebugLn(Format('[TForm1.HandleMessageDialogFinished] AResult=%d', [AResult]));
end;

end.

