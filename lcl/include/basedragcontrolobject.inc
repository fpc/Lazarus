{******************************************************************************
                                  TBaseDragControlObject
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

constructor TBaseDragControlObject.Create(AControl : TControl);
begin
FControl := AControl;
end;

Procedure TBaseDragControlObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
Begin
   Assert(False, 'Trace:5-1');
if Not Accepted then
   Begin
   fControl.DragCanceled;
   Target := nil;
   end;
   Assert(False, 'Trace:5-2');
   EndDrag(Target,X,Y);
end;

procedure TBaseDragControlObject.EndDrag(Target : TObject; X,Y : Integer);
Begin
  FControl.DoEndDrag(Target,X,Y);
end;



