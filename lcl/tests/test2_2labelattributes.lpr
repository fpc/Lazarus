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
 
  LCL Test 2_2

  Showing a form at 0,0,320,240 with a label and several controls to test the
  label properties.
}
program test2_2labelattributes;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, FPCAdds, LCLProc, LCLType, Controls, Forms,
  TypInfo, LMessages, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    WidthRadioGroup: TRadioGroup;
    AutoSizeCheckBox: TCheckBox;
    procedure AutoSizeCheckBoxClick(Sender: TObject);
    procedure Label1ChangeBounds(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Label1DblClick(Sender: TObject);
    procedure Label1Enter(Sender: TObject);
    procedure Label1Exit(Sender: TObject);
    procedure Label1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label1MouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Label1MouseUp(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label1Resize(Sender: TObject);
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
    procedure WidthRadioGroupClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ TForm1 }

procedure TForm1.Label1ChangeBounds(Sender: TObject);
begin
  debugln('TForm1.Label1ChangeBounds ',DbgSName(Sender),' Bounds=',dbgs(TControl(Sender).BoundsRect));
end;

procedure TForm1.AutoSizeCheckBoxClick(Sender: TObject);
begin
  debugln('TForm1.AutoSizeCheckBoxClick ',DbgSName(Sender),' AutoSizeCheckBox.Checked=',dbgs(AutoSizeCheckBox.Checked));
  Label1.AutoSize:=AutoSizeCheckBox.Checked;
  AutoSizeCheckBox.Checked:=Label1.AutoSize;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  debugln('TForm1.Label1Click ',DbgSName(Sender));
end;

procedure TForm1.Label1DblClick(Sender: TObject);
begin
  debugln('TForm1.Label1DblClick ',DbgSName(Sender));
end;

procedure TForm1.Label1Enter(Sender: TObject);
begin
  debugln('TForm1.Label1Enter ',DbgSName(Sender));
end;

procedure TForm1.Label1Exit(Sender: TObject);
begin
  debugln('TForm1.Label1Exit ',DbgSName(Sender));
end;

procedure TForm1.Label1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Debugln('TForm1.Label1KeyDown ',DbgSName(Sender),' Key=',dbgs(Key),
          ' Shift=',dbgs(Shift));
end;

procedure TForm1.Label1MouseDown(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1.Label1MouseDown ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Label1MouseEnter(Sender: TObject);
begin
  debugln('TForm1.Label1MouseEnter ',DbgSName(Sender));
end;

procedure TForm1.Label1MouseLeave(Sender: TObject);
begin
  debugln('TForm1.Label1MouseLeave ',DbgSName(Sender));
end;

procedure TForm1.Label1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  debugln('TForm1.Label1MouseMove ',DbgSName(Sender),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Label1MouseUp(Sender: TOBject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  debugln('TForm1.Label1MouseUp ',DbgSName(Sender),
    ' Button=',GetEnumName(TypeInfo(TMouseButton),ord(Button)),
    ' X=',dbgs(X),' Y=',dbgs(Y),' Shift=',dbgs(Shift));
end;

procedure TForm1.Label1Resize(Sender: TObject);
begin         ;
  debugln('TForm1.Label1Resize ',DbgSName(Sender),' Bounds=',dbgs(TControl(Sender).BoundsRect));
end;

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
  Label1:=TLabel.Create(Self);
  with Label1 do begin
    Name:='Label1';
    Caption:='This is a &label with a long caption';
    SetBounds(10,10,75,25);
    Parent:=Self;
    OnChangeBounds:=@Label1ChangeBounds;
    OnClick:=@Label1Click;
    OnDblClick:=@Label1DblClick;
    OnEnter:=@Label1Enter;
    OnExit:=@Label1Exit;
    OnMouseDown:=@Label1MouseDown;
    OnMouseEnter:=@Label1MouseEnter;
    OnMouseLeave:=@Label1MouseLeave;
    OnMouseMove:=@Label1MouseMove;
    OnMouseUp:=@Label1MouseUp;
    OnResize:=@Label1Resize;
    OnKeyDown:=@Label1KeyDown;
  end;
  
  WidthRadioGroup:=TRadioGroup.Create(Self);
  with WidthRadioGroup do begin
    Name:='WidthRadioGroup';
    SetBounds(10,100,100,100);
    Caption:='Label.Width';
    Parent:=Self;
    Items.Add('20');
    Items.Add('80');
    Items.Add('200');
    OnClick:=@WidthRadioGroupClick;
  end;
  
  AutoSizeCheckBox:=TCheckBox.Create(Self);
  with AutoSizeCheckBox do begin
    Name:='AutoSizeCheckBox';
    SetBounds(120,100,100,25);
    Caption:='Label.AutoSize';
    Parent:=Self;
    Checked:=Label1.AutoSize;
    OnClick:=@AutoSizeCheckBoxClick;
  end;
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
  debugln('TForm1.Form1Resize ',DbgSName(Sender),' Bounds=',dbgs(TControl(Sender).BoundsRect));
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

procedure TForm1.WidthRadioGroupClick(Sender: TObject);
begin
  debugln('TForm1.WidthRadioGroupClick ',DbgSName(Sender),' WidthRadioGroup.ItemIndex=',dbgs(WidthRadioGroup.ItemIndex));
  if WidthRadioGroup.ItemIndex>=0 then begin
    Label1.Width:=StrToIntDef(WidthRadioGroup.Items[WidthRadioGroup.ItemIndex],
                              Label1.Width);
  end;
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
end;

var
  Form1: TForm1 = nil;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

