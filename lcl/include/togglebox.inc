// included by stdctrls.pp

{------------------------------------------------------------------------------}

constructor TToggleBox.Create(AOwner : TComponent);
begin
   if assigned(AOwner) then
   begin
      inherited Create(AOwner);
      fCompStyle := csToggleBox;
   end;
end;

{------------------------------------------------------------------------------}

// included by stdctrls.pp

