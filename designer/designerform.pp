{ /***************************************************************************
                   designerform.pp  -  Designer Form
                             -------------------
                 Implements a widget list created by TDesigner.


                 Initial Revision  : Sat May 10 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit designerform;

{$mode objfpc}

interface

uses
   classes, controls, forms, graphics, sysutils, lmessages, Buttons,
   extctrls, ControlSelection;

type
  TGridPoint = record
      x: integer;
      y: integer;
    end;

  TMoveControl = record
    Control : TControl;
    X : Integer;
    Y : Integer;
  end;

  TDesignerForm = class(TForm)
  private
    FMoveControl: TMoveControl;
    FControlSelection: TControlSelection;
  protected
    procedure Paint; override;
  public
    constructor Create (aowner : TComponent); override;
    procedure AddControl(AControl: TControl; X, Y: Integer);
//    procedure AddDesignerWinControl(wcontrol: TControl);
//    procedure AddDesignerControl(control: TControl);
    procedure DrawGrid;
//    procedure Adjusttogrid(control : TControl);
    procedure ControlClick(Sender : TObject);
    procedure ControlDblClick(Sender : TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Sender : TOBject; Button: TMouseButton; Shift : TShiftState; X, Y: Integer);
    procedure Mouseup(Sender : TOBject; Button: TMouseButton; Shift : TShiftState; X, Y: Integer);
  
    function GetNewName(Sender : TObject): String;
  
    property Selection: TControlSelection read FControlSelection;
//  property MoveControl : TMoveControl read FMoveControl write SetMoveControl;
 end;

var
GridPoints : TGridPoint;

implementation

uses Project,global,idecomp;

constructor TDesignerForm.create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  Height := Screen.height div 2;
  Width := Screen.width div 2;
  
  FControlSelection := TControlSelection.Create(Self);
end;


procedure TDesignerForm.Paint;
begin
  inherited Paint;
  Writeln('TDesignerform.paint');
  Drawgrid;
end;

procedure TDesignerForm.DrawGrid;
var
  x,y : integer;
begin
  canvas.Pen.Color := clGray;

//go from the left to the right and top to the bottom.

  X := left;
  while X <= left + width do
  begin
    Y := Top;
    while y <= top+height do
    begin
      Canvas.Rectangle(x-left,y-top,x-left+1,y-top+1);
      Inc(Y, GridPoints.Y);
    end;
    Inc(x, GridPoints.X);
  end;
end;

(*
procedure TDesignerForm.Adjusttogrid(control : TControl);
var
  NewLEft : Integer;
  NewTop : Integer;
begin
  //Makeit part of Selection
  
  if Control = nil then exit;
{Writeln(format('Old control left and top %d,%d',[Control.left, Control.top]));
NewLeft := (Control.left div GridPoints.X) * gridPoints.x+1;
NewTop := (Control.Top div GridPoints.Y) * gridPoints.y+1;
Writeln(format('New control left and top %d,%d',[Newleft, Newtop]));
Control.Left := NewLeft;
Control.Top := NewTop;
 }
end;
*)

(*
{add a designer TWincontrol to the form}
procedure TDesignerForm.AddDesignerWinControl(wControl: TControl);
Var
MousePos : TPoint;
begin
MousePos := Mouse.CursorPos;
Writeln(format('Mousepos is %d,%d',[Mousepos.x,MousePos.y]));
Writeln(Format('Left,Top = %d,%d',[Left,Top]));

wControl.Parent := Self;
wControl.Left := MousePos.x-left;
wControl.Top := MousePos.y-top-5;  //-5 is for the captiopn bar
wControl.Name := Getname(wControl);
wControl.Text := wControl.Name;
Writeln('1');
wControl.Visible := True;
Writeln('2');
wControl.SetDesigning(true);
Writeln('3');
wControl.OnClick := @ControlClick;
wControl.OnDblClick := @ControlDblClick;
wControl.OnMouseMove := @MOuseMove;
wControl.OnMouseup := @MOuseUp;
wControl.OnMouseDown := @MOuseDown;
Adjusttogrid(wControl);
ActiveControl := wControl;
if wcontrol.ClassNameIs('TBitBtn') then
   TBitBtn(wControl).kind := bkOK;
{if wcontrol.ClassNameIs('TNotebook') then
  Begin
   TNotebook(wControl).Height := 300;
   TNotebook(wControl).Width := 300;
  end;
 }

end;

{---------------------------------------------------------------------}
{add a designer Tcontrol to the form}
{---------------------------------------------------------------------}
procedure TDesignerForm.AddDesignerControl(Control: TControl);
Var
MousePos : TPoint;
begin
MousePos := Mouse.CursorPos;
Writeln(format('Mousepos is %d,%d',[Mousepos.x,MousePos.y]));
Writeln(Format('Left,Top = %d,%d',[Left,Top]));

Control.Parent := Self;
Control.Left := MousePos.x-left;
Control.Top := MousePos.y-top-5;  //-5 is for the captiopn bar
Control.Name := Getname(Control);
//Control.Text := Control.Name;
Control.Visible := True;
Control.SetDesigning(true);
Control.OnClick := @ControlClick;
Control.OnDblClick := @ControlDblClick;
Control.OnMouseMove := @MOuseMove;
Control.OnMouseup := @MOuseUp;
Control.OnMouseDown := @MOuseDown;
Adjusttogrid(Control);

ActiveControl := Control;

end;

*)

procedure TDesignerForm.AddControl(AControl: TControl; X, Y: Integer);
var
  MousePos : TPoint;
begin
  if AControl <> nil 
  then with AControl do 
  begin
    FControlSelection.Clear;
    
    Parent := Self;
    Left := (X div Gridpoints.X) * Gridpoints.X;
    Top := (Y div Gridpoints.Y) * Gridpoints.Y;
    Name := GetNewName(AControl);
    Visible := True;
    SetDesigning(True);
    OnClick := @ControlClick;
    OnDblClick := @ControlDblClick;
    
    FControlSelection.Add(AControl);
  end;
end;

function TDesignerForm.GetNewName(Sender : TObject): String;
var
  I: Integer;
  Num: Integer;
  Stop: Boolean;
begin
  Writeln('Getname');
  Result := LowerCase(Sender.ClassName);
  Delete(Result, 1, 1);
  Num := 1;
  repeat
    Stop := True;
    for i := 0 to ComponentCount - 1 do
    begin
      if (LowerCase(Components[i].name) = Format('%s%d', [Result, Num])) 
      then begin
        Inc(num);
        Stop := False;
        Break;
      end;
    end;
  until Stop;

  //make the name pretty
  Result := Format('%s%d', [Result, Num]);
  Result[1] := Uppercase(Result[1])[1];
end;

Procedure TDesignerForm.ControlClick(Sender : TObject);
var
I : Integer;
Begin
(*
ActiveControl := TControl(Sender);

for I := 0 to idecomplist.count-1 do
   if (TideComponent(ideComplist.items[I])).ClassNameis(sender.classname) then
      Break;

if I < ideCompList.Count then
TideComponent(ideComplist.items[i]).ClickMethod(Sender);
*)
End;

Procedure TDesignerForm.ControlDblClick(Sender : TObject);
var
I : Integer;
Begin
(*
for I := 0 to idecomplist.count-1 do
   if (TideComponent(ideComplist.items[I])).ClassNameis(sender.classname) then
      Break;

if I < ideCompList.Count then
TideComponent(ideComplist.items[i]).DblClickMethod(Sender);
*)

End;

procedure TDesignerForm.MouseDown(Sender : TOBject; Button: TMouseButton; Shift : TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) 
  then begin
    FMoveControl.Control := TControl(Sender);
    FMoveControl.X := Mouse.CursorPos.X;
    FMoveControl.Y := Mouse.CursorPos.Y;
  end;
end;

Procedure TDesignerForm.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  //if TCOntrol(sender).dragging then
    WriteLn(Format('[TDesignerForm.MouseMove] Start(%d, %d) Stop(%d, %d)', [FMoveCOntrol.X, FMoveCOntrol.Y, X, Y]));
    Writeln(format('Sender classname is %s',[Sender.classname]));
(*  if (FMoveControl.Control = Sender)
  then begin
    Writeln('-------');
    dx := X - FMoveControl.X;
    dy := Y - FMoveCOntrol.Y;
    FMoveControl.Control.Left := FMoveControl.Control.Left + dX;
    FMoveControl.Control.Top  := FMoveControl.Control.Top + dY;
  end;
*)
end;

procedure TDesignerForm.Mouseup(Sender : TOBject; Button: TMouseButton; Shift : TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
  MousePos : TPoint;
begin
  if (Button=mbLeft) 
  then begin
    if (FMoveCOntrol.Control = Sender) 
    then begin
{      WriteLn(Format('[TDesignerForm.Mouseup] Start(%d, %d) Stop(%d, %d)', [FMoveCOntrol.X, FMoveCOntrol.Y, X, Y]));
      Writeln('------------------');
      Writeln('------------------');
      Writeln('------------------');
      Writeln(format('Parent handle = %d',[longint(FMoveControl.Control.parent.handle)]));
      Writeln(format('Parent coords = %d,%d',[FMoveControl.Control.parent.left,FMoveControl.Control.parent.Top]));
      Writeln('------------------');
      Writeln('------------------');
      Writeln('------------------');
 }
     MousePos := MOuse.CursorPos;
      if (MousePos.X < FMoveControl.Control.parent.left) then
        dx := FMoveControl.Control.left
      else
      if (MousePos.X > FMoveControl.Control.parent.left + FMoveControl.Control.parent.Width) then
        dx := FMoveControl.Control.Parent.Width - FMoveControl.Control.Width
      else
      dx := MousePos.X - FMoveControl.X;

      if (MousePos.Y < FMoveControl.Control.parent.Top) then
        dy := FMoveControl.Control.Top
      else
      if (MousePos.Y > FMoveControl.Control.parent.Top + FMoveControl.Control.parent.Height) then
        dy := FMoveControl.Control.Parent.Height - FMoveControl.Control.height
      else
        dy := MousePos.Y - FMoveControl.Y;

      FMoveControl.Control.Left := FMoveControl.Control.Left + dX;
      FMoveControl.Control.Top  := FMoveControl.Control.Top + dY;

    end;

    FMoveControl.Control := nil;
  end;
end;

initialization
  Gridpoints.x := 15;
  GridPoints.Y := 15;


end.
