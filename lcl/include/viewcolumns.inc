constructor TViewColumns.Create(Aowner : TCustomListView);
Begin
   inherited Create;
   ListView := AOwner;
end;

Function TViewColumns.Add(const S : String): Integer;
Begin

Writeln('1');
   Result := inherited;
   ListView.ColumnsChanged;
end;

Procedure TViewColumns.Delete(Index : Integer);
Begin
Writeln('2');
   inherited;
   ListView.ColumnsChanged;
end;

Procedure TViewColumns.Assign(source : TPersistent);
Begin
Writeln('3');
   inherited;
   ListView.ColumnsChanged;
end;


