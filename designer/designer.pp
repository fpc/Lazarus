{ /***************************************************************************
                   widgetstack.pp  -  Designer Widget Stack
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
unit designer;

{$mode objfpc}

interface

uses
  classes, Forms, controls, lmessages, graphics, ControlSelection, CustomFormEditor,FormEditor, UnitEditor,Main;

type
  TGridPoint = record
      x: integer;
      y: integer;
    end;

  TDesigner = class(TIDesigner)
  private
    FCustomForm: TCustomForm;
    FFormEditor : TFormEditor;
    FSourceEditor : TSourceEditor;
    FMainIDE      : TMainIDE;
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
  protected
    MouseDownControl : TObject;
    MouseDownPos, MouseUpPos, LastMouseMovePos : TPoint;

    Procedure MouseDownOnControl(Sender : TControl; Message : TLMessage);
    procedure MouseMoveOnControl(Sender : TControl; var Message : TLMessage);
    Procedure MouseUpOnControl(Sender : TControl; Message:TLMessage);

    Procedure KeyDown(Sender : TControl; Message:TLMKEY);
    Procedure KeyUP(Sender : TControl; Message:TLMKEY);

    Procedure RemoveControl(Control : TComponent);
    Procedure NudgeControl(Value1,Value2 : Integer);
    Procedure NudgeSize(Value1,Value2 : Integer);

  public
    ControlSelection : TControlSelection;
    constructor Create(customform : TCustomform);
    destructor Destroy; override;
    Procedure AddControlCode(Control : TComponent);

    procedure CreateNew(FileName : string);
    procedure LoadFile(FileName: string);

    function IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean; override;
    procedure Modified; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string); override;
    Procedure SelectOnlyThisComponent(AComponent:TComponent);


    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read FCustomForm write FCustomForm;
    property FormEditor : TFormEditor read FFormEditor write FFormEditor;
    property SourceEditor : TSourceEditor read FSourceEditor write FSourceEditor;
    property MainIDE : TMainIDE read FMainIDE write FMainIDE;
 end;

 implementation

uses
  Sysutils, Typinfo,Math;


const
  mk_lbutton = 1;
  mk_rbutton = 2;
  mk_shift = 4;
  mk_control = 8;
  mk_mbutton = $10;

var
  GridPoints : TGridPoint;

constructor TDesigner.Create(CustomForm : TCustomForm);
begin
  inherited Create;
  FCustomForm := CustomForm;
  ControlSelection := TControlSelection.Create(CustomForm);

  //the source is created when the form is created.
  //the TDesigner is created in Main.pp and then TDesigner.SourceEditor := SourceNotebook.CreateFormFromUnit(CustomForm);


end;

destructor TDesigner.Destroy;
Begin
  ControlSelection.free;
  Inherited;
end;


procedure TDesigner.CreateNew(FileName : string);
begin

end;


Procedure TDesigner.RemoveControl(Control : TComponent);
Begin
    Writeln('RemoveControl called');
    FSourceEditor.RemoveControlCode(Control);
    Writeln('1');
    FCustomForm.RemoveControl(TCOntrol(Control));  //this send a message to notification and removes it from the controlselection
    Writeln('2');
    FFormEditor.DeleteControl(Control);
    Writeln('3');
end;

Procedure TDesigner.NudgeControl(Value1,Value2 : Integer);
Begin
Writeln('NudgeControl');
 ControlSelection.MoveSelection(Value1,Value2);
end;

Procedure TDesigner.NudgeSize(Value1,Value2 : Integer);
Begin
  Writeln('NudgeSize');
  ControlSelection.SizeSelection(Value1,Value2);
end;

procedure TDesigner.SelectOnlyThisComponent(AComponent:TComponent);
begin
Writeln('Control Added '+TCOntrol(aComponent).name);
    ControlSelection.Clear;
    ControlSelection.Add(TControl(AComponent));

  FFormEditor.ClearSelected;
  // this will automatically inform the object inspector
  FFormEditor.AddSelected(AComponent);
end;


procedure TDesigner.MouseDownOnControl(Sender : TControl; Message : TLMessage);
Begin
//  if assigned(MouseDownControl) and (MOuseDownControl <> Sender) then Exit;
  Writeln('Left is '+Inttostr(TCOntrol(Sender).left));
  Writeln('Top is '+Inttostr(TCOntrol(Sender).Top));
  Writeln('***************************');
  Writeln('TDesigner.MouseDownOnControl');
  Writeln(Format('X,Y = %d,%d',[TLMMOuse(Message).pos.x,TLMMOuse(Message).pos.Y]));
  Writeln(Format('Control left and top are %d,%d',[TCOntrol(sender).Left,TCOntrol(sender).Top]));
  Writeln('***************************');


  if GetCaptureGrabber<>nil then exit;

  if not assigned(MouseDownControl) then
     Begin
       MouseDownPos.X := TLMMOuse(Message).pos.X;
       MouseDownPos.Y := TLMMOuse(Message).pos.Y;
       //adjust X and Y by adding the Control corners.
       MouseDownControl:=Sender;
       if not (Sender is TCustomForm) then
         begin
         inc(MouseDownPos.X,TControl(Sender).Left);
         inc(MouseDownPos.Y,TControl(Sender).Top);
         end;
       LastMouseMovePos:=MouseDownPos;
     end;

  if (TLMMouse(Message).keys and MK_Shift) = MK_Shift then
    Writeln('Shift down')
    else
    Writeln('No Shift down');

  if (TLMMouse(Message).keys and MK_Control) = MK_Control then
    Writeln('CTRL down')
    else
    Writeln('No CTRL down');




  Writeln('Sender is '+sender.name);
  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.
    if not (Sender is TCustomForm) then begin
    if (TLMMouse(Message).keys and MK_Shift) = MK_Shift then
      ControlSelection.Add(sender)
      else
      SelectOnlyThisComponent(TComponent(Sender));
    end;
  end;
End;

procedure TDesigner.MouseUpOnControl(Sender : TControl; Message:TLMessage);
var
  ParentCI, NewCI : TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight : Integer;
//  CInterface : TComponentInterface;
  CaptureGrabber:TGrabber;
  Button : TMouseButton;
  Shift : TShiftState;
Begin


  Writeln('***************************');
   Writeln('In TDesigner.UpOnControl');
  Writeln(Format('X,Y = %d,%d',[TLMMOuse(Message).pos.x,TLMMOuse(Message).pos.Y]));
  Writeln('***************************');
  if (TLMMouse(Message).keys and MK_LButton) = MK_LButton then
    Button := mbLEft
   else
  if (TLMMouse(Message).keys and MK_LButton) = MK_RButton then
    Button := mbRight;

    Shift := [];
  if (TLMMouse(Message).keys and MK_Shift) = MK_Shift then
    shift := [ssShift];

  if (TLMMouse(Message).keys and MK_Control) = MK_Control then
    shift := shift +[ssCTRL];


  CaptureGrabber:=GetCaptureGrabber;
  if CaptureGrabber<>nil then begin
    CaptureGrabber.CaptureMouseUp(TControl(Sender),Button,Shift,TLMMouse(Message).pos.X,TLMMouse(Message).pos.Y);
    exit;
  end;

  MouseUpPos.X := TLMMouse(Message).pos.X;
  MouseUpPos.Y := TLMMouse(Message).pos.Y;
  if not (Sender is TCustomForm) then begin
    inc(MouseUpPos.X,TControl(Sender).Left);
    inc(MouseUpPos.Y,TControl(Sender).Top);
  end;

  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.

  end
  else
  Begin  //add a new control

    FMainIDE.SetDesigning(FCustomForm,False);
    ParentCI:=TComponentInterface(FFormEditor.FindComponent(TComponent(Sender)));
    if (TComponent(Sender) is TWinControl)
      and (not (csAcceptsControls in TWinControl(Sender).ControlStyle)) then
    begin
      ParentCI:=TComponentInterface(
        FFormEditor.FindComponent(TWinControl(Sender).Parent));
    end;
    if Assigned(ParentCI) then begin
      NewLeft:=Min(MouseDownPos.X,MouseUpPos.X);
      NewWidth:=Abs(MouseUpPos.X-MouseDownPos.X);
      NewTop:=Min(MouseDownPos.Y,MouseUpPos.Y);
      NewHeight:=Abs(MouseUpPos.Y-MouseDownPos.Y);
      if Abs(NewWidth+NewHeight)<7 then begin
        // this very small component is probably only a wag, take default size
        NewWidth:=0;
        NewHeight:=0;
      end;
      NewCI := TComponentInterface(FFormEditor.CreateComponent(ParentCI,FMainIDE.SelectedComponent.ComponentClass
        ,NewLeft,NewTop,NewWidth,NewHeight));
      NewCI.SetPropByName('Visible',True);
      NewCI.SetPropByName('Designing',True);
      FMainIDE.SetDesigning(NewCI.Control,True);

      ObjectInspector1.FillComponentComboBox;
      AddControlCode(NewCI.Control);

        SelectOnlyThisComponent(TComponent(NewCI.Control));
        Writeln('Calling ControlClick with nil from MouseUpOnControl');
        FMainIDE.ControlClick(FMainIDE.Notebook1);  //this resets it to the mouse.
        FMainIDE.SetDesigning(FCustomForm,True);

    end;
  end;

  MouseDownControl:=nil;
end;



Procedure TDesigner.MouseMoveOnControl(Sender : TControl; var Message : TLMessage);
const
  mk_lbutton = 1;
  mk_rbutton = 2;
  mk_shift = 4;
  mk_control = 8;
  mk_mbutton = $10;
var
  CaptureGrabber : TGrabber;
  Shift : TShiftState;
  X,Y : Integer;
Begin

//  if assigned(MouseDownControl) and (MOuseDownControl <> Sender) then Exit;
  Writeln('MouseMoveOnControl');
  X :=TLMMouse(Message).Pos.x;
  Y := TLMMouse(Message).Pos.Y;
  Writeln('MousePos');
  Writeln(Format('X,y = %d,%d',[Mouse.CursorPos.X,MOuse.CursorPos.Y]));
  Writeln('X and Y are '+inttostr(x)+','+inttostr(y));
  If (sender is TControl) then Begin
      Writeln('Sender is '+TControl(sender).Name);
      Writeln('Left is '+Inttostr(TControl(sender).Left));
      Writeln('Width is '+Inttostr(TControl(sender).Width));
      Writeln('Top is '+Inttostr(TControl(sender).Top));
      Writeln('Height is '+Inttostr(TControl(sender).Height));
      end;
  if Assigned(MouseDownControl) then Writeln('MouseDownControl is '+TCOntrol(MouseDownControl).Name);

  Shift := [];
  if (TLMMouse(Message).keys and MK_Shift) = MK_Shift then
    shift := [ssShift];

  if (TLMMouse(Message).keys and MK_Control) = MK_Control then
    shift := Shift + [ssCTRL];

  CaptureGrabber:=GetCaptureGrabber;
  if CaptureGrabber<>nil then begin
    CaptureGrabber.CaptureMouseMove(TControl(Sender),Shift,X,Y);
  end else begin
    if Assigned(MouseDownControl) then begin
      if FMainIDE.SelectedComponent = nil then begin
        // mouse pointer button pressed
       { if not (Sender is TCustomForm) then} begin
          // move selection
             Writeln('moving stuff');
            {  if not(X in ([0 ..(TControl(sender).Width)])) or
              not(Y in ([0 ..(TControl(sender).Height)])) then
              exit;    }
             ControlSelection.MoveSelection(X-LastMouseMovePos.X, Y-LastMouseMovePos.Y);
//             ControlSelection.MoveContent(X-LastMouseMovePos.X, Y-LastMouseMovePos.Y);

            LastMouseMovePos:=Point(X,Y);
          end;
        end;
      end;
    end;
end;

{
-----------------------------K E Y D O W N -------------------
}
{
 Handles the keydown messages.  DEL deletes the selected controls, CTRL-UPARROR/DOWNARROW
 moves the selction up one, etc.
}
Procedure TDesigner.KeyDown(Sender : TControl; Message:TLMKEY);
var
  I : Integer;
  Continue : Boolean;
  Shift : TShiftState;
Begin
Writeln('KEYDOWN');
with MEssage do
  Begin
  Writeln('CHARCODE = '+inttostr(charcode));
  Writeln('KEYDATA = '+inttostr(KeyData));
  end;


Shift := KeyDataToShiftState(Message.KeyData);

if Message.CharCode = 46 then //DEL KEY
   begin
   Continue := True;
   While Continue do
    Begin
    Continue := False;
    for  I := 0 to FCustomForm.ComponentCount-1 do
      Begin
        Writeln('I = '+inttostr(i));
        if (FCustomForm.Components[i] is TControl) and
           ControlSelection.IsSelected(TControl(FCustomForm.Components[i])) then
           Begin
              Continue := True;
              RemoveControl(TControl(FCustomForm.Components[i]));
              Break;
           end;
      end;
      End;
     SelectOnlythisComponent(FCustomForm);

   end
   else
if Message.CharCode = 38 then //UP ARROW
   Begin
   if (ssCtrl in Shift) then
   NudgeControl(0,-1)
   else
   if (ssShift in Shift) then
   NudgeSize(0,-1);
   end
   else
if Message.CharCode = 40 then //DOWN ARROW
   Begin
   if (ssCtrl in Shift) then
   NudgeControl(0,1)
   else
   if (ssShift in Shift) then
   NudgeSize(0,1);
   end
   else
if Message.CharCode = 39 then //RIGHT ARROW
   Begin
   if (ssCtrl in Shift) then
   NudgeControl(1,0)
   else
   if (ssShift in Shift) then
   NudgeSize(1,0);
   end
   else
if Message.CharCode = 37 then //LEFT ARROW
   Begin
   if (ssCtrl in Shift) then
   NudgeControl(-1,0)
   else
   if (ssShift in Shift) then
   NudgeSize(-1,0);
   end;




end;


{-----------------------------------------K E Y U P --------------------------------}
Procedure TDesigner.KeyUp(Sender : TControl; Message:TLMKEY);
Begin
Writeln('KEYUp');
with MEssage do
  Begin
  Writeln('CHARCODE = '+inttostr(charcode));
  Writeln('KEYDATA = '+inttostr(KeyData));
  end;

end;

function TDesigner.IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean;
Begin
result := false;
if ((Message.msg >= LM_MOUSEFIRST) and (Message.msg <= LM_MOUSELAST)) then
Result := true
else
if ((Message.msg >= LM_KeyFIRST) and (Message.msg <= LM_KeyLAST)) then
    Begin
     Writeln('KEY MESSAGE in IsDesignMsg');
     if MEssage.MSG = LM_KEYDOWN then KeyDown(Sender,TLMKey(Message))
     else
     if MEssage.MSG = LM_KEYUP then KeyUP(Sender,TLMKey(Message));
     Result := true;
    end;


if (Message.msg=LM_LBUTTONDOWN) then
   begin
   MouseDownonControl(sender,message);
   end
else
if (Message.msg=LM_LBUTTONUP) then
   begin
   MouseUPONControl(sender,message);
   end
else
if Message.msg = LM_MOUSEMOVE then
   MouseMoveonCOntrol(Sender, Message)



{if Result then Writeln('It IS a design message')
else
Writeln('It IS NOT a design message');
 }
end;

procedure TDesigner.LoadFile(FileName: string);
begin

end;

procedure TDesigner.Modified;
Begin

end;


Procedure TDesigner.AddControlCode(Control : TComponent);
Begin
FSourceEditor.AddControlCode(Control);
end;


procedure TDesigner.Notification(AComponent: TComponent; Operation: TOperation);
Begin
 if Operation = opInsert then
   begin
   end
  else
  if Operation = opRemove then
    begin
     writeln('[TDesigner.Notification] opRemove '+
       ''''+AComponent.ClassName+'.'+AComponent.Name+'''');
      if (AComponent is TControl) then
      if ControlSelection.IsSelected(TControl(AComponent)) then
          ControlSelection.Remove(TControl(AComponent));
    end;
end;

procedure TDesigner.PaintGrid;
var
  x,y : integer;
begin
  with FCustomForm do
    Begin
      canvas.Pen.Color := clGray;
      x := left;
      while x <= left + width do
        begin
          y := Top;
          while y <= top+height do
            begin
              if Controlatpos(TPOINT([x,y]),True) = nil then
              Canvas.Rectangle(x-left,y-top,x-left+1,y-top);
              Inc(y, GridPoints.Y);
            end;
          Inc(x, GridPoints.X);
        end;
    end;
end;

procedure TDesigner.ValidateRename(AComponent: TComponent; const CurName, NewName: string);
Begin

end;

function TDesigner.GetIsControl: Boolean;
Begin
Result := True;
end;

procedure TDesigner.SetIsControl(Value: Boolean);
Begin

end;

initialization
  GridPoints.x := 10;
  GridPoints.Y := 10;

end.

