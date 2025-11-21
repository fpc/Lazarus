program tclass_search_order.pas;
{$Mode objfpc}

(*
  - Find own class name, before identifier in base class

*)

type

  TBar = class
  public type
    TFoo = word;
  end;

  TFoo = class(TBar)
  public type
    TMe  = TFoo{declaration:TFoo};
    TOrd = TBar.TFoo{declaration:TBar.TFoo};
  public
    FMe: TMe;
    FVal: TOrd;
    procedure n;
  end;

{ TFoo }

procedure TFoo.n;
begin
  FMe := nil;
  FVal := 1;
end;


begin

end.

