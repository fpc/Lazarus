{------------------------------------------------------------------------------}
{  function TScrollBar.Create                                                      }
{------------------------------------------------------------------------------}

constructor TScrollBar.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
//   FCompStlye := csVScrollBar;
   fOwner := TControl(AOwner);
end;
