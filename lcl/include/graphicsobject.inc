{******************************************************************************
                               TGraphicsObject
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

procedure TGraphicsObject.DoChange(var msg);
begin
  Changed;
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

{ =============================================================================

  $Log$
  Revision 1.3  2002/09/30 14:01:06  lazarus
  MG: undid the TBinaryObjectWriter Buffersize

  Revision 1.2  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.3  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.2  2000/05/08 15:56:59  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

}
