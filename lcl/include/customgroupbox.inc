// included by stdctrls.pp

{------------------------------------------------------------------------------}
{  function TCustomGroupBox.AdjustClientRect                                   }
{------------------------------------------------------------------------------}
procedure TCustomGroupBox.AdjustClientRect(var Rect: TRect);
begin
  if HandleAllocated then
    LCLLinux.GetClientRect(Handle,Rect)
  else
    inherited;
end;

{------------------------------------------------------------------------------}
{  function TCustomGroupBox.Create                                             }
{------------------------------------------------------------------------------}
constructor TCustomGroupBox.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  fCompStyle := csGroupBox;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;
 
// included by stdctrls.pp

{
  $Log$
  Revision 1.4  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.3  2002/04/21 06:53:55  lazarus
  MG: fixed save lrs to test dir

  Revision 1.2  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

}
