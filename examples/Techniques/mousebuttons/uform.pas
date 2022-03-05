unit uform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ShapeButtonExtra2: TShape;
    ShapeButtonLeft: TShape;
    ShapeButtonExtra1: TShape;
    ShapeButtonRight: TShape;
    ShapeWheel: TShape;
    ShapeWheelUp: TShape;
    ShapeWheelDown: TShape;
    ShapeWheelRight: TShape;
    ShapeWheelLeft: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelLeft(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelRight(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  color_On = clGreen;
  color_Off = clWhite;
  cDelay = 100;

procedure DoBlink(C: TShape);
begin
  C.Brush.Color:= color_On;
  Application.ProcessMessages;
  Sleep(cDelay);
  C.Brush.Color:= color_Off;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to ControlCount-1 do
    if Controls[i] is TShape then
    begin
      (Controls[i] as TShape).OnMouseDown:= Self.OnMouseDown;
      (Controls[i] as TShape).OnMouseUp:= Self.OnMouseUp;
      (Controls[i] as TShape).OnMouseWheelUp:= Self.OnMouseWheelUp;
      (Controls[i] as TShape).OnMouseWheelDown:= Self.OnMouseWheelDown;
      (Controls[i] as TShape).OnMouseWheelLeft:= Self.OnMouseWheelLeft;
      (Controls[i] as TShape).OnMouseWheelRight:= Self.OnMouseWheelRight;
    end;
end;

procedure TFormMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
      ShapeButtonLeft.Brush.Color:= color_On;
    mbRight:
      ShapeButtonRight.Brush.Color:= color_On;
    mbMiddle:
      ShapeWheel.Brush.Color:= color_On;
    mbExtra1:
      ShapeButtonExtra1.Brush.Color:= color_On;
    mbExtra2:
      ShapeButtonExtra2.Brush.Color:= color_On;
  end;
end;

procedure TFormMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:
      ShapeButtonLeft.Brush.Color:= color_Off;
    mbRight:
      ShapeButtonRight.Brush.Color:= color_Off;
    mbMiddle:
      ShapeWheel.Brush.Color:= color_Off;
    mbExtra1:
      ShapeButtonExtra1.Brush.Color:= color_Off;
    mbExtra2:
      ShapeButtonExtra2.Brush.Color:= color_Off;
  end;
end;

procedure TFormMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  DoBlink(ShapeWheelDown);
end;

procedure TFormMain.FormMouseWheelLeft(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  DoBlink(ShapeWheelLeft);
end;

procedure TFormMain.FormMouseWheelRight(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  DoBlink(ShapeWheelRight);
end;

procedure TFormMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  DoBlink(ShapeWheelUp);
end;

end.

