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
  classes,Forms,controls,lmessages,graphics,ControlSelection, FormEditor;

type

  TGridPoint = record
      x: integer;
      y: integer;
    end;



 TDesigner = class(TIDesigner)
  private
    FCustomForm: TCustomForm;
    FFormEditor : TFormEditor;
    FControlSelection : TControlSelection;
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
  protected
    ControlSelection : TControlSelection;

  public
    constructor Create(customform : TCustomform);
    destructor Destroy; override;
    procedure CreateNew(FileName : string);
    procedure LoadFile(FileName: string);

    function IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean; override;
    procedure Modified; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent; const CurName, NewName: string); override;
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read FCustomForm write FCustomForm;
    property FormEditor : TFormEditor read FFormEditor write FFormEditor;
 end;

 implementation

var
GridPoints : TGridPoint;

constructor TDesigner.Create(CustomForm : TCustomForm);
begin
inherited Create;


FCustomForm := CustomForm;
//The controlselection should NOT be owned by the form.  When it is it shows up in the OI
ControlSelection := TControlSelection.Create(CustomForm);
end;

destructor TDesigner.Destroy;
Begin
Inherited;
ControlSelection.free;
end;

procedure TDesigner.CreateNew(FileName : string);
begin

end;

procedure TDesigner.LoadFile(FileName: string);
begin

end;


function TDesigner.IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean;
Begin

end;

procedure TDesigner.Modified;
Begin

end;

procedure TDesigner.Notification(AComponent: TComponent; Operation: TOperation);
Begin
 if Operation = opInsert then
    begin
     //AComponent.SetDesigning(True);
     if (AComponent is TCOntrol) then
        Begin
//        TControl(AComponent).Visible := True;
        ControlSelection.Clear;
        Controlselection.Add(TCOntrol(AComponent));
        end;
    end
    else
 if Operation = opRemove then
    begin
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
       X := left;
       while X <= left + width do
         begin
           Y := Top;
           while y <= top+height do
              begin
                 Canvas.Rectangle(x-left,y-top,x-left+1,y-top);
                   Inc(Y, GridPoints.Y);
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
  Gridpoints.x := 10;
  GridPoints.Y := 10;

end.

