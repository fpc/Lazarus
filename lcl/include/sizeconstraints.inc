{%MainUnit ../controls.pp}

{******************************************************************************
                                  TSizeConstraints
				
  Simple class to hold size constraints for a control.
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  UpdateInterfaceConstraints;
end;

{------------------------------------------------------------------------------
  procedure TSizeConstraints.UpdateInterfaceConstraints;

  Asks interface for constraints.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.UpdateInterfaceConstraints;
begin
  if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    TWSControlClass(Control.WidgetSetClass).GetConstraints(Control, Self);
end;

{------------------------------------------------------------------------------
  procedure TSizeConstraints.SetInterfaceConstraints(MinW, MinH,
    MaxW, MaxH: integer);

  Used by the interface to set the interface constraints.
  Should only be used by custom components, not by applications.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.SetInterfaceConstraints(MinW, MinH,
  MaxW, MaxH: integer);
begin
  if (FMinInterfaceWidth=MinW)
  and (FMinInterfaceHeight=MinH)
  and (FMaxInterfaceWidth=MaxW)
  and (FMaxInterfaceHeight=MaxH) then exit;
  
  FMinInterfaceWidth:=MinW;
  FMinInterfaceHeight:=MinH;
  FMaxInterfaceWidth:=MaxW;
  FMaxInterfaceHeight:=MaxH;
  
  if (FControl.Width<>MinMaxWidth(FControl.Width))
  or (FControl.Height<>MinMaxHeight(FControl.Height)) then
    FControl.RequestAlign;
end;

function TSizeConstraints.EffectiveMinWidth: integer;
begin
  if csLoading in Control.ComponentState then
    exit(0);
  if (MinWidth>MinInterfaceWidth) then begin
    Result:=MinWidth;
    if (MaxInterfaceWidth>0) and (MaxInterfaceWidth<MinWidth) then
      Result:=MaxInterfaceWidth;
  end else
    Result:=MinInterfaceWidth;
end;

function TSizeConstraints.EffectiveMinHeight: integer;
begin
  if csLoading in Control.ComponentState then
    exit(0);
  if (MinHeight>MinInterfaceHeight) then begin
    Result:=MinHeight;
    if (MaxInterfaceHeight>0) and (Result>MaxInterfaceHeight) then
      Result:=MaxInterfaceHeight;
  end else
    Result:=MinInterfaceHeight;
end;

// The EffectiveMaxWidth is the minumum of MaxWidth and MaxInterfaceWidth,
// but it is at least MinInterfaceWidth.
// A zero value is interpreted as unconstraint.
function TSizeConstraints.EffectiveMaxWidth: integer;
begin
  if csLoading in Control.ComponentState then
    exit(0);
  if (MaxInterfaceWidth>0) and
    ((MaxWidth=0) or (MaxInterfaceWidth<MaxWidth)) then
    Result := MaxInterfaceWidth
  else
    Result:=MaxWidth;
  if (Result>0) and (MinInterfaceWidth>0) and (Result<MinInterfaceWidth) then
    Result:=MinInterfaceWidth;
end;

// The EffectiveMaxHeight is the minumum of MaxHeight and MaxInterfaceHeight,
// but it is at least the MinInterfaceHeight.
// A zero value is interpreted as unconstraint.
function TSizeConstraints.EffectiveMaxHeight: integer;
begin
  if csLoading in Control.ComponentState then
    exit(0);
  if (MaxInterfaceHeight>0) and
    ((MaxHeight=0) or (MaxInterfaceHeight<MaxHeight)) then
    Result := MaxInterfaceHeight
  else
    Result:=MaxHeight;
  if (Result>0) and (MinInterfaceHeight>0) and (Result<MinInterfaceHeight) then
    Result:=MinInterfaceHeight;
end;

function TSizeConstraints.MinMaxWidth(Width: integer): integer;
var
  MinW: LongInt;
  MaxW: LongInt;
begin
  Result:=Width;
  MinW:=EffectiveMinWidth;
  if (Result<MinW) then Result:=MinW;
  MaxW:=EffectiveMaxWidth;
  if (MaxW>0) and (Result>MaxW) then Result:=MaxW;
  if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    TWSControlClass(Control.WidgetSetClass).ConstraintWidth(Control, Self, Result);
end;

function TSizeConstraints.MinMaxHeight(Height: integer): integer;
var
  MinH: LongInt;
  MaxH: LongInt;
begin
  Result:=Height;
  MinH:=EffectiveMinHeight;
  if (Result<MinH) then Result:=MinH;
  MaxH:=EffectiveMaxHeight;
  if (MaxH>0) and (Result>MaxH) then Result:=MaxH;
  if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    TWSControlClass(Control.WidgetSetClass).ConstraintHeight(Control, Self, Result);
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
    if (FMinWidth > 0) and (FMaxWidth>0) and (FMaxWidth < FMinWidth) then
      FMinWidth:= FMaxWidth;
    Change;
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
    if (FMinHeight > 0) and (FMaxHeight>0) and (FMaxHeight < FMinHeight) then
      FMinHeight:= FMaxHeight;
    Change;
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
  end;
end;

procedure TSizeConstraints.SetOptions(const AValue: TSizeConstraintsOptions);
begin
  if FOptions=AValue then exit;
  FOptions:=AValue;
end;

{------------------------------------------------------------------------------
  Method:  TSizeConstraints.Change
  Params:  none
  Returns: Nothing

  Calls a change handler if assigned.
 ------------------------------------------------------------------------------}
procedure TSizeConstraints.Change;
begin
  FControl.DoConstraintsChange(Self);
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
      if (FMinWidth<>Self.FMinWidth)
      or (FMaxWidth<>Self.FMaxWidth)
      or (FMinHeight<>Self.FMinHeight)
      or (FMaxHeight<>Self.FMaxHeight) then begin
        FMinWidth:= Self.FMinWidth;
        FMaxWidth:= Self.FMaxWidth;
        FMinHeight:= Self.FMinHeight;
        FMaxHeight:= Self.FMaxHeight;
        Change;
      end;
    end;
  end else begin
    inherited AssignTo(Dest);
  end;    
end;

// included by controls.pp
