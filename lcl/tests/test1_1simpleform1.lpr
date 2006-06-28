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
 
  LCL Test 1_1

  Showing a form at 0,0,320,240
}
program Test1_1SimpleForm1;

{$mode objfpc}{$H+}

uses
  Interfaces, FPCAdds, LCLProc, LCLType, Classes, Controls, Forms, TypInfo,
  LMessages;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure Form1Activate(Sender: TObject);
    procedure Form1ChangeBounds(Sender: TObject);
    procedure Form1Click(Sender: TObject);
    procedure Form1Close(Sender: TObject; var CloseAction: TCloseAction);
    procedure Form1CloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Form1Create(Sender: TObject);
    procedure Form1Deactivate(Sender: TObject);
    procedure Form1Enter(Sender: TObject);
    procedure Form1Exit(Sender: TObject);
    function Form1Help(Command: Word; Data: Longint; var CallHelp: Boolean
      ): Boolean;
    procedure Form1Hide(Sender: TObject);
    procedure Form1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Form1KeyPress(Sender: TObject; var Key: char);
    procedure Form1MouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Form1MouseEnter(Sender: TObject);
    procedure Form1MouseLeave(Sender: TObject);
    procedure Form1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Form1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Form1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Form1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Form1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Form1Paint(Sender: TObject);
    procedure Form1Resize(Sender: TObject);
    procedure Form1Shortcut(var Msg: TLMKey; var Handled: Boolean);
    procedure Form1Show(Sender: TObject);
    procedure Form1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TForm1 }

procedure TForm1.Form1Activate(Sender: TObject);
begin
  debugln('TForm1.Form1Activate ',DbgSName(Sender));
end;

procedure TForm1.Form1ChangeBounds(Sender: TObject);
begin
  debugln('TForm1.Form1ChangeBounds ',DbgSName(Sender),' Bounds=',dbgs(BoundsRect));
end;

procedure TForm1.Form1Click(Sender: TObject);
begin
  debugln('TForm1.Form1Click ',DbgSName(Sender));
end;

procedure TForm1.Form1Close(Sender: TObject; var CloseAction: TCloseAction);
begin
  debugln('TForm1.Form1Close ',DbgSName(Sender),' CloseAction=',
          GetEnumName(TypeInfo(TCloseAction),ord(CloseAction)));
end;

procedure TForm1.Form1CloseQuery(Sender: TObject; var CanClose: boolean);
begin
  debugln('TForm1.Form1CloseQuery ',DbgSName(Sender),' CanClose',dbgs(CanClose));
end;

procedure TForm1.Form1Create(Sender: TObject);
begin
  debugln('TForm1.Form1Create ',DbgSName(Sender));
end;

procedure TForm1.Form1Deactivate(Sender: TObject);
begin
  debugln('TForm1.Form1Deactivate ',DbgSName(Sender));
end;

procedure TForm1.Form1Enter(Sender: TObject);
begin
  debugln('TForm1.Form1Enter ',DbgSName(Sender));
end;

procedure TForm1.Form1Exit(Sender: TObject);
begin
  debugln('TForm1.Form1Exit ',DbgSName(Sender));
end;

function TForm1.Form1Help(Command: Word; Data: Longint; var CallHelp: Boolean
  ): Boolean;
begin
  debugln('TForm1.Form1Help Command=',dbgs(Command),' Data=',HexStr(Cardinal(Data),8),' CallHelp=',dbgs(CallHelp));
  Result:=false;
end;

procedure TForm1.Form1Hide(Sender: TObject);
begin
  debugln('TForm1.Form1Hide ',DbgSName(Sender));
end;

procedure TForm1.Form1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  Debugln('TForm1.Form1KeyDown ',DbgSName(Sender),' Key=',dbgs(Key),
          ' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1KeyPress(Sender: TObject; var Key: char);
begin
  debugln('TForm1.Form1KeyPress ',DbgSName(Sender),' Key=',DbgStr(Key));
end;

procedure TForm1.Form1MouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1.Form1MouseDown ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1MouseEnter(Sender: TObject);
begin
  debugln('TForm1.Form1MouseEnter ',DbgSName(Sender));
end;

procedure TForm1.Form1MouseLeave(Sender: TObject);
begin
  debugln('TForm1.Form1MouseLeave ',DbgSName(Sender));
end;

procedure TForm1.Form1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  exit;
  debugln('TForm1.Form1MouseMove ',DbgSName(Sender),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1MouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1.Form1MouseUp ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1.Form1MouseWheel ',DbgSName(Sender),
    ' WheelDelta=',dbgs(WheelDelta),' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1.Form1MouseWheelDown ',DbgSName(Sender),
    ' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  debugln('TForm1.Form1MouseWheelUp ',DbgSName(Sender),
    ' MousePos=',dbgs(MousePos),
    ' Handled=',dbgs(Handled),' Shift=',dbgs(Shift));
end;

procedure TForm1.Form1Paint(Sender: TObject);
begin
  debugln('TForm1.Form1Paint ',DbgSName(Sender));
end;

procedure TForm1.Form1Resize(Sender: TObject);
begin
  debugln('TForm1.Form1Resize ',DbgSName(Sender),' Bounds=',dbgs(BoundsRect));
end;

procedure TForm1.Form1Shortcut(var Msg: TLMKey; var Handled: Boolean);
begin
  debugln('TForm1.Form1Shortcut Msg.CharCode=',dbgs(Msg.CharCode),
          ' Handled=',dbgs(Handled));
end;

procedure TForm1.Form1Show(Sender: TObject);
begin
  debugln('TForm1.Form1Show ',DbgSName(Sender));
end;

procedure TForm1.Form1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  debugln('TForm1.Form1UTF8KeyPress ',DbgSName(Sender),' UTF8Key="',DbgStr(UTF8Key),'"');
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  OnActivate:=@Form1Activate;
  OnChangeBounds:=@Form1ChangeBounds;
  OnClick:=@Form1Click;
  OnClose:=@Form1Close;
  OnCloseQuery:=@Form1CloseQuery;
  OnCreate:=@Form1Create;
  OnDeactivate:=@Form1Deactivate;
  OnEnter:=@Form1Enter;
  OnExit:=@Form1Exit;
  OnHelp:=@Form1Help;
  OnHide:=@Form1Hide;
  OnKeyDown:=@Form1KeyDown;
  OnKeyDown:=@Form1KeyDown;
  OnKeyPress:=@Form1KeyPress;
  OnMouseDown:=@Form1MouseDown;
  OnMouseEnter:=@Form1MouseEnter;
  OnMouseLeave:=@Form1MouseLeave;
  OnMouseMove:=@Form1MouseMove;
  OnMouseUp:=@Form1MouseUp;
  OnMouseWheel:=@Form1MouseWheel;
  OnMouseWheelDown:=@Form1MouseWheelDown;
  OnMouseWheelUp:=@Form1MouseWheelUp;
  OnPaint:=@Form1Paint;
  OnResize:=@Form1Resize;
  OnShortcut:=@Form1Shortcut;
  OnShow:=@Form1Show;
  OnUTF8KeyPress:=@Form1UTF8KeyPress;
  inherited Create(TheOwner);
  
  Name:='Form1';
  Caption:='Title Form1';
  SetBounds(100,90,350,200);
end;

var
  Form1: TForm1 = nil;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  debugln('Form1.Bounds=',dbgs(Form1.BoundsRect));
  Application.Run;
end.

