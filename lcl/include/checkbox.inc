// included by stdctrls.pp

{------------------------------------------------------------------------------}

constructor TCheckbox.Create(AOwner : TComponent);
begin
   if assigned(AOwner) then
   begin
      inherited Create(AOwner);
      fCompStyle := csCHECKBOX;
   end;
end;

// included by stdctrls.pp

{
  $Log$
  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

}

