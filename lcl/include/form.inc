{------------------------------------------------------------------------------}
{       TForm Destructor                                                       }
{------------------------------------------------------------------------------}
destructor TForm.destroy;
begin
  Assert(False, 'Trace:Destroying signals for TForm');
  FCanvas.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------}
{       TForm Constructor                                                      }
{------------------------------------------------------------------------------}
constructor TForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {setup default size properties}
  Top := 1;
  Left := 1;
  Width := 120;
  Height := 120;

end;


