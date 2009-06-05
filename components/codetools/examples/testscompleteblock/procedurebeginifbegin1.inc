{%MainUnit unit1.pas}
procedure Do1;
  procedure BeginBlock(Typ: TFABBlockType);
  var
    ParentBlock: PBlock;
  begin
    if (Stack.Top>=0) then begin
      ParentBlock:=@Stack.Stack[Stack.Top];
      if (ParentBlock^.InnerStartPos=AtomStart)
      and (Policies<>nil) then begin
        Policies.AddIndent(ParentBlock^.Typ,Typ,ParentBlock^.InnerIdent);

    end;
    Stack.BeginBlock(Typ,AtomStart);
    {$IFDEF ShowCodeBeautifierParser}
    DebugLn([GetIndentStr(Stack.Top*2),'BeginBlock ',FABBlockTypeNames[Typ],' ',GetAtomString(@Src[AtomStart],NestedComments),' at ',PosToStr(p)]);
    {$ENDIF}
  end;
begin

end;

