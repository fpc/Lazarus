{------------------------------------------------------------------------------}
{  TScrolledWindow Constructor                                                 }
{------------------------------------------------------------------------------}
constructor TScrolledWindow.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  fCompStyle := csScrolledWindow;
  CreateComponent(AOwner);

  Left := 1;
  Top := 1;
  Height := 500;
  Width := 300;

AddControl;
end;

{------------------------------------------------------------------------------}
{  TScrolledWindow Destructor                                                  }
{------------------------------------------------------------------------------}
destructor TScrolledWindow.Destroy;
begin
 inherited Destroy;
end;
