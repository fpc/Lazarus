{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  LCL Test 2_3

  Showing a form at 100,90,350,200
}
program Test2_3TwoSimpleForms1;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages;

type

  { TForm1And2 }

  TForm1And2 = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormEnter(Sender: TObject);
    procedure FormExit(Sender: TObject);
    function FormHelp(Command: Word; Data: Longint; var CallHelp: Boolean
      ): Boolean;
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure FormMouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShortcut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  Form1: TForm1And2 = nil;
  Form2: TForm1And2 = nil;

{ TForm1And2 }

procedure TForm1And2.FormActivate(Sender: TObject);
begin
  debugln('TForm1And2.FormActivate ',DbgSName(Sender));
end;

procedure TForm1And2.FormChangeBounds(Sender: TObject);
begin
  debugln('TForm1And2.FormChangeBounds ',DbgSName(Sender),' Bounds=',dbgs(BoundsRect));
end;

procedure TForm1And2.FormClick(Sender: TObject);
begin
  debugln('TForm1And2.FormClick ',DbgSName(Sender));
end;

procedure TForm1And2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  debugln('TForm1And2.FormClose ',DbgSName(Sender),' CloseAction=',
          GetEnumName(TypeInfo(TCloseAction),ord(CloseAction)));
end;

procedure TForm1And2.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  debugln('TForm1And2.FormCloseQuery ',DbgSName(Sender),' CanClose',dbgs(CanClose));
end;

procedure TForm1And2.FormCreate(Sender: TObject);
begin
  debugln('TForm1And2.FormCreate ',DbgSName(Sender));
end;

procedure TForm1And2.FormDeactivate(Sender: TObject);
begin
  debugln('TForm1And2.FormDeactivate ',DbgSName(Sender));
end;

procedure TForm1And2.FormEnter(Sender: TObject);
begin
  debugln('TForm1And2.FormEnter ',DbgSName(Sender));
end;

procedure TForm1And2.FormExit(Sender: TObject);
begin
  debugln('TForm1And2.FormExit ',DbgSName(Sender));
end;

function TForm1And2.FormHelp(Command: Word; Data: Longint; var CallHelp: Boolean
  ): Boolean;
begin
  debugln('TForm1And2.FormHelp Command=',dbgs(Command),' Data=',HexStr(Cardinal(Data),8),' CallHelp=',dbgs(CallHelp));
  Result:=false;
end;

procedure TForm1And2.FormHide(Sender: TObject);
begin
  debugln('TForm1And2.FormHide ',DbgSName(Sender));
end;

procedure TForm1And2.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  Debugln('TForm1And2.FormKeyDown ',DbgSName(Sender),' Key=',dbgs(Key),
          ' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormKeyPress(Sender: TObject; var Key: char);
begin
  debugln('TForm1And2.FormKeyPress ',DbgSName(Sender),' Key=',DbgStr(Key));
end;

procedure TForm1And2.FormMouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1And2.FormMouseDown ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormMouseEnter(Sender: TObject);
begin
  debugln('TForm1And2.FormMouseEnter ',DbgSName(Sender));
end;

procedure TForm1And2.FormMouseLeave(Sender: TObject);
begin
  debugln('TForm1And2.FormMouseLeave ',DbgSName(Sender));
end;

procedure TForm1And2.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  debugln('TForm1And2.FormMouseMove ',DbgSName(Sender),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormMouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1And2.FormMouseUp ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1And2.FormMouseWheel ',DbgSName(Sender),
    ' WheelDelta=',dbgs(WheelDelta),' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1And2.FormMouseWheelDown ',DbgSName(Sender),
    ' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1And2.FormMouseWheelUp ',DbgSName(Sender),
    ' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1And2.FormPaint(Sender: TObject);
begin
  debugln('TForm1And2.FormPaint ',DbgSName(Sender));
end;

procedure TForm1And2.FormResize(Sender: TObject);
begin
  debugln('TForm1And2.FormResize ',DbgSName(Sender),' Bounds=',dbgs(BoundsRect));
end;

procedure TForm1And2.FormShortcut(var Msg: TLMKey; var Handled: Boolean);
begin
  debugln('TForm1And2.FormShortcut Msg.CharCode=',dbgs(Msg.CharCode),
          ' Handled=',dbgs(Handled));
end;

procedure TForm1And2.FormShow(Sender: TObject);
begin
  debugln('TForm1And2.FormShow ',DbgSName(Sender));
end;

procedure TForm1And2.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  debugln('TForm1And2.FormUTF8KeyPress ',DbgSName(Sender),' UTF8Key="',DbgStr(UTF8Key),'"');
end;

constructor TForm1And2.Create(TheOwner: TComponent);
begin
  OnActivate:=@FormActivate;
  OnChangeBounds:=@FormChangeBounds;
  OnClick:=@FormClick;
  OnClose:=@FormClose;
  OnCloseQuery:=@FormCloseQuery;
  OnCreate:=@FormCreate;
  OnDeactivate:=@FormDeactivate;
  OnEnter:=@FormEnter;
  OnExit:=@FormExit;
  OnHelp:=@FormHelp;
  OnHide:=@FormHide;
  OnKeyDown:=@FormKeyDown;
  OnKeyDown:=@FormKeyDown;
  OnKeyPress:=@FormKeyPress;
  OnMouseDown:=@FormMouseDown;
  OnMouseEnter:=@FormMouseEnter;
  OnMouseLeave:=@FormMouseLeave;
  OnMouseMove:=@FormMouseMove;
  OnMouseUp:=@FormMouseUp;
  OnMouseWheel:=@FormMouseWheel;
  OnMouseWheelDown:=@FormMouseWheelDown;
  OnMouseWheelUp:=@FormMouseWheelUp;
  OnPaint:=@FormPaint;
  OnResize:=@FormResize;
  OnShortcut:=@FormShortcut;
  OnShow:=@FormShow;
  OnUTF8KeyPress:=@FormUTF8KeyPress;
  inherited Create(TheOwner);

  if Self=Form1 then
    Name:='Form1'
  else
    Name:='Form2';
  Caption:='Title '+Name;
  if Name='Form1' then
    SetBounds(100,90,350,200)
  else
    SetBounds(470,90,380,190);
end;

begin
  Application.Initialize;
  Application.CreateForm(TForm1And2,Form1);
  debugln('Form1.Bounds=',dbgs(Form1.BoundsRect));
  Application.CreateForm(TForm1And2,Form2);
  debugln('Form2.Bounds=',dbgs(Form2.BoundsRect));
  Form2.Show;
  Application.Run;
end.

