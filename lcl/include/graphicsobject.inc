(******************************************************************************
                               TGraphicsObject
 ******************************************************************************)

procedure TGraphicsObject.DoChange(var msg);
begin
  Changed;
end;

procedure TgraphicsObject.Changed;
begin
  Assert(False, Format('Trace:[TgraphicsObject.Changed] %s', [ClassName]));
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TgraphicsObject.Lock;
begin

end;

procedure TgraphicsObject.UnLock;
begin

end;

{ =============================================================================

  $Log$
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
