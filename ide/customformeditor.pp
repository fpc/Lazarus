{
 /***************************************************************************
                               CustomFormEditor.pp
                             -------------------




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
{$H+}
unit CustomFormEditor;

{$mode objfpc}

interface

uses
  classes, abstractformeditor, controls;

type

{
TCustomFormEditor
  One is created whenever a "NEw Form" is created.  The Form is contained in the MainControl
  property.  FComponentClass tells whether this container is a TFORM or a TDataModule, or
  something else new.
}

 TControlClass = class of TControl;

 TCustomFormEditor = class(TAbstractFormEditor)
  private
    FMainControl : TControl;
    FControlClass : TControlClass;
    Function GetMainControl : TControl;
  protected

  public

    constructor Create; virtual;
    destructor Destroy; override;

    Function Filename : String; override;

    property ControlClass : TControlClass read FControlClass write FControlClass;
    property MainControl : TControl read GetMainControl;
  end;


implementation

constructor TCustomFormEditor.Create;
begin
inherited;
end;

destructor TCustomFormEditor.Destroy;
begin
inherited;
end;

function TCustomFormEditor.GetMainControl: TControl;
begin
if not Assigned(FMainControl) then
   Begin
        FMainControl := FControlClass.Create(nil);
        FMainControl.Parent := nil;
   end;

result := FMainControl;
end;


Function TCustomFormEditor.Filename : String; override;
begin
Result := 'testing.pp';
end;


end.
