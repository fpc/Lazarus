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
    Procedure RemoveControl(Control : TComponent);

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

var
  GridPoints : TGridPoint;

constructor TDesigner.Create(CustomForm : TCustomForm);
var
  nmUnit : String;
  I : Integer;
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
    Control.Destroy;
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
  if assigned(MouseDownControl) and (MOuseDownControl <> Sender) then Exit;
  Writeln('Left is '+Inttostr(TCOntrol(Sender).left));
  Writeln('Top is '+Inttostr(TCOntrol(Sender).Top));
  Writeln('***************************');
  Writeln('TDesigner.MouseDownOnControl');
  Writeln(Format('X,Y = %d,%d',[TLMMOuse(Message).pos.x,TLMMOuse(Message).pos.Y]));
  Writeln(Format('Control left and top are %d,%d',[TCOntrol(sender).Left,TCOntrol(sender).Top]));
  Writeln('***************************');


  if GetCaptureGrabber<>nil then exit;

  MouseDownPos.X := TLMMOuse(Message).pos.X;
  MouseDownPos.Y := TLMMOuse(Message).pos.Y;


  //adjust X and Y by adding the Control corners.
  if not (Sender is TCustomForm) then begin
    inc(MouseDownPos.X,TControl(Sender).Left);
    inc(MouseDownPos.Y,TControl(Sender).Top);
  end;

  MouseDownControl:=Sender;
  LastMouseMovePos:=MouseDownPos;

  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.
    if not (Sender is TCustomForm) then begin
      SelectOnlyThisComponent(TComponent(Sender));
    end;
  end;
End;

procedure TDesigner.MouseUpOnControl(Sender : TControl; Message:TLMessage);
const
  mk_lbutton = 1;
  mk_rbutton = 2;
  mk_shift = 4;
  mk_control = 8;
  mk_mbutton = $10;
var
  ParentCI, NewCI : TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight : Integer;
//  CInterface : TComponentInterface;
  CaptureGrabber:TGrabber;
  Button : TMouseButton;
  Shift : TShiftState;
  X,Y : Integer;
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


  X := TLMMOuse(Message).pos.X;
  Y := TLMMOuse(Message).pos.Y;
  CaptureGrabber:=GetCaptureGrabber;
  if CaptureGrabber<>nil then begin
    CaptureGrabber.CaptureMouseUp(TControl(Sender),Button,Shift,TLMMouse(Message).pos.X,TLMMouse(Message).pos.Y);
    exit;
  end;

  if   ((not (Sender is TCustomForm))
        or (( X < TControl(sender).left) or ( X > (TControl(sender).left+TControl(sender).Width)))
        or (( Y < TControl(sender).Top) or ( Y > (TControl(sender).Top+TControl(sender).Height)))) then begin
    inc(X,TControl(Sender).Left);
    inc(Y,TControl(Sender).Top);
    end;


  if MouseDownControl = Sender then
    Begin
    Writeln('***************');
    Writeln(Format('MouseLAstPos.X,Y= %d,%d',[LastMOuseMovePos.X,LastMouseMovePos.Y]));
    Writeln(Format('MouseDownPos.X,Y= %d,%d',[MOuseDownPos.X,MouseDownPos.Y]));

    ControlSelection.MoveSelection(X-LastMouseMovePos.X, Y-LastMouseMovePos.Y);
    //do something like ControlSelection.Sizecontent but move x and y from where
    // the grabber started to where it finished.
    ControlSelection.MoveContent(X-MouseDownPos.X,Y-MouseDownPos.Y);
    end;

  MouseUpPos.X := TLMMouse(Message).pos.X;
  MouseUpPos.Y := TLMMouse(Message).pos.Y;
  if not (Sender is TCustomForm) then begin
    inc(MouseUpPos.X,TControl(Sender).Left);
    inc(MouseUpPos.Y,TControl(Sender).Top);
  end;

  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.
    if Sender is TCustomForm then
      SelectOnlyThisComponent(TComponent(Sender));
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
      NewCI.SetPropByName('Visible',True); //Control).Visible := True;
{      if (NewCI.Control is TCOntrol) then Begin
          Writeln('Setting visbile 2');
          TCOntrol(NewCI.Control).Visible := True;
          end;
}      ObjectInspector1.FillComponentComboBox;
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
  Button : TMouseButton;
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

  if (TLMMouse(Message).keys and MK_LButton) = MK_LButton then
    Button := mbLEft
   else
  if (TLMMouse(Message).keys and MK_LButton) = MK_RButton then
    Button := mbRight;
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
        if not (Sender is TCustomForm) then begin
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
Begin
Writeln('KEYDOWN');
with MEssage do
  Begin
  Writeln('CHARCODE = '+inttostr(charcode));
  Writeln('KEYDATA = '+inttostr(KeyData));
  end;

if Message.CharCode = 46 then //DEL KEY
   begin
    for  I := 0 to FCustomForm.ComponentCount-1 do
      Begin
        Writeln('I = '+inttostr(i));
        if (FCustomForm.Components[i] is TControl) and
           ControlSelection.IsSelected(TControl(FCustomForm.Components[i])) then
           Begin
              RemoveControl(TControl(FCustomForm.Components[i]));
           end;
      end;
    FFormEditor.ClearSelected;
  // this will automatically inform the object inspector
    ControlSelection.Add(FCustomForm);
    FFormEditor.AddSelected(FCustomForm);


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
     KeyDown(Sender,TLMKey(Message));
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

end;

procedure TDesigner.SetIsControl(Value: Boolean);
Begin

end;

initialization
  GridPoints.x := 10;
  GridPoints.Y := 10;

end.

