unit DockForm2Unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  Buttons, LDockCtrl, StdCtrls, Menus;

type

  { TDockFormX }

  TDockFormX = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    HideMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure HideMenuItemClick(Sender: TObject);
  private
  public
    Docker: TLazControlDocker;
  end;
  
procedure PaintBoundaries(AForm: TCustomForm; const AColor: TColor);

implementation

procedure PaintBoundaries(AForm: TCustomForm; const AColor: TColor);
begin
  AForm.Canvas.Brush.Color:=AColor;
  AForm.Canvas.FrameRect(Rect(0,0,AForm.ClientWidth-1,AForm.ClientHeight-1));
  AForm.Canvas.Pen.Color:=AColor;
  AForm.Canvas.Line(0,0,AForm.ClientWidth-1,AForm.ClientHeight-1);
  AForm.Canvas.Line(0,AForm.ClientHeight-1,AForm.ClientWidth-1,0);
  AForm.Canvas.TextOut(10,10,DbgSName(AForm));
end;

{ TDockFormX }

procedure TDockFormX.FormPaint(Sender: TObject);
begin
  if Sender=nil then ;
  PaintBoundaries(Self,clRed);
end;

procedure TDockFormX.HideMenuItemClick(Sender: TObject);
begin
  if Sender=nil then ;
  Hide;
end;

procedure TDockFormX.FormCreate(Sender: TObject);
begin
  if Sender=nil then ;
  Docker:=TLazControlDocker.Create(Self);
end;

initialization
  {$I dockform2unit.lrs}

end.

