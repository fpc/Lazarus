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

    procedure MouseUpOnForm(Sender : TObject; Button: TMouseButton;
                            Shift : TShiftState; X, Y: Integer);
    procedure MouseDownOnForm(Sender : TObject; Button: TMouseButton;
                              Shift : TShiftState; X, Y: Integer);
    procedure MouseMoveOnForm(Sender : TObject; Shift : TShiftState; X, Y: Integer);

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

procedure TDesigner.SelectOnlyThisComponent(AComponent:TComponent);
begin
    ControlSelection.Clear;
    ControlSelection.Add(TControl(AComponent));

  FFormEditor.ClearSelected;
  // this will automatically inform the object inspector
  FFormEditor.AddSelected(AComponent);
end;


procedure TDesigner.MouseDownOnForm(Sender : TObject; Button: TMouseButton;
Shift : TShiftState; X, Y: Integer);
Begin
Writeln('MOUSEDOWNONFORM');
  if GetCaptureGrabber<>nil then exit;

  MouseDownPos.X := X;
  MouseDownPos.Y := Y;
  MouseDownControl:=Sender;
  LastMouseMovePos:=MouseDownPos;

End;

procedure TDesigner.MouseMoveOnForm(Sender : TObject;
Shift : TShiftState; X, Y: Integer);
var
  CurDesigner: TDesigner;
  CaptureGrabber:TGrabber;
Begin
  Writeln('MouseMoveOnForm');
  CaptureGrabber:=GetCaptureGrabber;
  if CaptureGrabber<>nil then begin
    CaptureGrabber.CaptureMouseMove(TControl(Sender),Shift,X,Y);
  end else begin

    if Assigned(MouseDownControl) then begin
       LastMouseMovePos:=Point(X,Y);
          end;
        end;
End;

procedure TDesigner.MouseUpOnForm(Sender : TObject; Button: TMouseButton;
Shift : TShiftState; X, Y: Integer);
var
  ParentCI, NewCI : TComponentInterface;
  NewLeft, NewTop, NewWidth, NewHeight : Integer;
//  CInterface : TComponentInterface;
  CaptureGrabber:TGrabber;
Begin
  Writeln('MouseUpOnForm');
  CaptureGrabber:=GetCaptureGrabber;
  if CaptureGrabber<>nil then begin
    CaptureGrabber.CaptureMouseUp(TControl(Sender),Button,Shift,X,Y);
    exit;
  end;
  MouseUpPos.X := X;
  MouseUpPos.Y := Y;


  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.
      SelectOnlyThisComponent(TComponent(Sender));
  end
  else
  Begin  //add a new control
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

      ObjectInspector1.FillComponentComboBox;
      AddControlCode(NewCI.Control);
      Writeln('2222');
      if NewCI.Control is TControl then begin
        // set the OnMouseDown and OnMouseUp event so we know when the control
        // is selected or a new control is dropped
          FMainIDE.SetDesigning(NewCI.Control);
//        NewCI.SetPropByName('OnMouseUp',@MouseUpOnControl);
//        NewCI.SetPropByName('OnMouseDown',@MouseDownOnControl);
//        NewCI.SetPropByName('OnMouseMove',@MouseMoveOnControl);
        SelectOnlyThisComponent(TComponent(NewCI.Control));
      end;
      Writeln('Calling ControlClick with Nil from MouseUponForm');
      FMainIDE.ControlClick(FMainIDE.Notebook1);  //this resets it to the mouse.
    end;
  end;

  MouseDownControl:=nil;
  Writeln('Exiting MouseUPOnForm');
end;

procedure TDesigner.MouseDownOnControl(Sender : TControl; Message : TLMessage);
Begin
  Writeln('MouseDownOnControl');
  if GetCaptureGrabber<>nil then exit;

  MouseDownPos.X := TLMMOuse(Message).pos.X;
  MouseDownPos.Y := TLMMOuse(Message).pos.Y;
  if not (Sender is TCustomForm) then begin
    inc(MouseDownPos.X,TControl(Sender).Left);
    inc(MouseDownPos.Y,TControl(Sender).Top);
  end;
  MouseDownControl:=Sender;
  LastMouseMovePos:=MouseDownPos;
  Writeln(TComponent(Sender).Name+'.OnMouseDown at '+inttostr(MouseDownPos.x)
    +','+inttostr(MouseDownPos.Y));

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
Begin
   Writeln('In UpOnControl');
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
  Writeln(TComponent(Sender).Name+'.OnMouseUp at '+inttostr(TLMMouse(Message).pos.x)+','+inttostr(TLMMouse(Message).pos.y));

  if FMainIDE.SelectedComponent = nil then
  Begin //mouse pointer button pressed.
    if Sender is TCustomForm then
      SelectOnlyThisComponent(TComponent(Sender));
  end
  else
  Begin  //add a new control
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

      ObjectInspector1.FillComponentComboBox;
      AddControlCode(NewCI.Control);

      if NewCI.Control is TControl then begin
        // set the OnMouseDown and OnMouseUp event so we know when the control
        // is selected or a new control is dropped
//why cant I do this here???          NewCI.Control.SetDesigning(True);
//        NewCI.SetPropByName('OnMouseUp',@MouseUpOnControl);
//        NewCI.SetPropByName('OnMouseDown',@MouseDownOnControl);
//        NewCI.SetPropByName('OnMouseMove',@MouseMoveOnControl);
        SelectOnlyThisComponent(TComponent(NewCI.Control));
      end;
        Writeln('Calling ControlClick with ni from MouseUpOnControl');
        FMainIDE.ControlClick(FMainIDE.Notebook1);  //this resets it to the mouse.
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
  Writeln('MouseMoveOnControl');

  X :=TLMMouse(Message).Pos.x;
  Y := TLMMouse(Message).Pos.Y;
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
    Writeln('CaptureGrabber is <> nil');
    CaptureGrabber.CaptureMouseMove(TControl(Sender),Shift,X,Y);
  end else begin
    if Assigned(MouseDownControl) then begin
    Writeln('MouseDownControl is assigned');
      if FMainIDE.SelectedComponent = nil then begin
        // mouse pointer button pressed
    Writeln('SelectedComponent = nil');

        if not (Sender is TCustomForm) then begin
          // move selection
             ControlSelection.MoveSelection(
              X-LastMouseMovePos.X, Y-LastMouseMovePos.Y);
            LastMouseMovePos:=Point(X,Y);
          end;
        end;
      end;
    end;
end;

function TDesigner.IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean;
Begin
result := false;
Writeln('In ISDESIGNMSG');
if ((Message.msg >= LM_MOUSEFIRST) and (Message.msg <= LM_MOUSELAST)) then
Result := true;

if (Message.msg=LM_LBUTTONDOWN) then
   begin  //select the control
   ControlSelection.Clear;
   ControlSelection.Add(TControl(Sender));
   end
else
if Message.msg = LM_MOUSEMOVE then
   MouseMoveonCOntrol(Sender, Message);


if Result then Writeln('It IS a design message')
else
Writeln('It IS NOT a design message');

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

