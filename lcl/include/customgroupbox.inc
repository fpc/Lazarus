{------------------------------------------------------------------------------}
{  function TCustomGroupBox.Create                                             }
{------------------------------------------------------------------------------}
constructor TCustomGroupBox.Create(AOwner: TComponent);
begin
   inherited Create (AOwner);
   fCompStyle := csGroupBox;
   ControlStyle := ControlStyle + [csAcceptsControls];
end;
 
{
  $Log$
  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.1  2000/04/02 20:49:56  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.3  2000/01/02 00:25:12  lazarus
  Stoppok:
    - enhanced TCustomradiogroup & TCustomgroupbox

}
