{%MainUnit ../controls.pp}

{******************************************************************************
                                  TBaseDragControlObject
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

constructor TBaseDragControlObject.Create(AControl : TControl);
begin
  FControl := AControl;
end;

procedure TBaseDragControlObject.Assign(Source: TDragObject);
begin
  inherited Assign(Source);
  if Source is TBaseDragControlObject then
    FControl := TBaseDragControlObject(Source).Control;
end;

Procedure TBaseDragControlObject.Finished(Target: TObject; X, Y: Integer;
  Accepted: Boolean);
Begin
  if Not Accepted then
  Begin
    fControl.DragCanceled;
    Target := nil;
  end;
  EndDrag(Target,X,Y);
end;

procedure TBaseDragControlObject.EndDrag(Target: TObject; X,Y : Integer);
Begin
  {$IFDEF VerboseDrag}
  DebugLn('TBaseDragControlObject.EndDrag ',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
  {$ENDIF}
  FControl.DoEndDrag(Target,X,Y);
end;

// included by controls.pp
