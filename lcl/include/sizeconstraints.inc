// included by controls.pp

{******************************************************************************
                                  TSizeConstraints
				
  Simple class to hold size constraints for a control.
 ******************************************************************************

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
}
{------------------------------------------------------------------------------
  Method: TSizeConstraints.Create
  Params:  AControl: the owner of the class
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TSizeConstraints.Create(AControl : TControl);
begin
  inherited Create;
  FControl:= AControl;

  FMaxWidth:= 0;
  FMaxHeight:= 0;
  FMinWidth:= 0;
  FMinHeight:= 0;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.SetMaxWidth
  Params:  Value: the new value of the property
  Returns: Nothing

  Sets a new value of its property.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.SetMaxWidth(Value: TConstraintSize);
begin
  if Value <> FMaxWidth then begin
    FMaxWidth:= Value;
    if (FMinWidth > 0) and (FMaxWidth < FMinWidth) then FMinWidth:= FMaxWidth;
    Change;
    FControl.RequestAlign;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.SetMaxHeight
  Params:  Value: the new value of the property
  Returns: Nothing

  Sets a new value of its property.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.SetMaxHeight(Value: TConstraintSize);
begin
  if Value <> FMaxHeight then begin
    FMaxHeight:= Value;
    if (FMinHeight > 0) and (FMaxHeight < FMinHeight) then FMinHeight:= FMaxHeight;
    Change;
    FControl.RequestAlign;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.SetMinWidth
  Params:  Value: the new value of the property
  Returns: Nothing

  Sets a new value of its property.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.SetMinWidth(Value: TConstraintSize);
begin
  if Value <> FMinWidth then begin
    FMinWidth:= Value;
    if (FMaxWidth > 0) and (FMinWidth > FMaxWidth) then FMaxWidth:= FMinWidth;
    Change;
    FControl.RequestAlign;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.SetMinHeight
  Params:  Value: the new value of the property
  Returns: Nothing

  Sets a new value of its property.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.SetMinHeight(Value: TConstraintSize);
begin
  if Value <> FMinHeight then begin
    FMinHeight:= Value;
    if (FMaxHeight > 0) and (FMinHeight > FMaxHeight) then FMaxHeight:= FMinHeight;
    Change;
    FControl.RequestAlign;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.Change
  Params:  none
  Returns: Nothing

  Calls a change handler if assigned.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.AssignTo
  Params:  Dest: Destination constraints to be assigned
  Returns: Nothing

  Calls a change handler if assigned.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.AssignTo(Dest: TPersistent);
begin
  if Dest is TSizeConstraints then begin
    with TSizeConstraints(Dest) do begin
      FMinWidth:= Self.FMinWidth;
      FMaxWidth:= Self.FMaxWidth;
      FMinHeight:= Self.FMinHeight;
      FMaxHeight:= Self.FMaxHeight;
      Change;
    end;  
  end else begin
    inherited AssignTo(Dest);
  end;    
end;

// included by controls.pp

