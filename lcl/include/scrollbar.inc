{------------------------------------------------------------------------------}
{  function TScrollBar.Create                                                      }
{------------------------------------------------------------------------------}

constructor TScrollBar.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   fOwner := TControl(AOwner);
end;
