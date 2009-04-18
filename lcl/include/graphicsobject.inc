{%MainUnit ../controls.pp}
{******************************************************************************
                               TGraphicsObject
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

procedure TGraphicsObject.DoChange(var msg);
begin
  Changed;
end;

procedure TGraphicsObject.Changing;
begin
  Assert(False, Format('Trace:[TgraphicsObject.Changing] %s', [ClassName]));
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TGraphicsObject.Changed;
begin
  Assert(False, Format('Trace:[TgraphicsObject.Changed] %s', [ClassName]));
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphicsObject.Lock;
begin

end;

procedure TGraphicsObject.UnLock;
begin

end;
