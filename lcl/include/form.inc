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
end;


