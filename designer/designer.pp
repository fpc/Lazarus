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
  classes, Forms, controls, lmessages, graphics, ControlSelection, FormEditor, UnitEditor;

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
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
  protected
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
    property IsControl: Boolean read GetIsControl write SetIsControl;
    property Form: TCustomForm read FCustomForm write FCustomForm;
    property FormEditor : TFormEditor read FFormEditor write FFormEditor;
    property SourceEditor : TSourceEditor read FSourceEditor write FSourceEditor;
 end;

 implementation

uses
  Sysutils, Typinfo;

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

function TDesigner.IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean;
Begin
result := false;
Writeln('In ISDESIGNMSG');
if ((Message.msg >= LM_MOUSEFIRST) and (Message.msg <= LM_MOUSELAST)) then
Result := true;
if Result then Writeln('It IS a design message')
else
Writeln('It IS NOT a design message')
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

