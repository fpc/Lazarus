{------------------------------------------------------------------------------}
{   TCustomListView Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TCustomListView.Create(Aowner: TComponent);
begin
 Inherited Create(AOwner);

 ParentWindow := TScrolledwindow.Create(AOwner);
 ParentWindow.Show;
 {fix this call it is a hack}

 fCompStyle := csListView;
 CreateComponent(AOwner);

 Left := 2;
 Top := 2;
 Width := 300;
 Height := 300;

// ******************************
// the following line has been commented out so that gtk dependency can be
// removed from all files. The following function needs to be moved.   MAH
//
// gtk_container_add(PgtkContainer(ParentWindow.fComponent),fComponent);

end;

{------------------------------------------------------------------------------}
{   TCustomListView SetItems                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItems(Value : TListItems);
begin
end;

{------------------------------------------------------------------------------}
{   TCustomListView Delete                                                     }
{------------------------------------------------------------------------------}
procedure TCustomListView.Delete(Item : TListItem);
begin
end;

{------------------------------------------------------------------------------}
{   TCustomListView InsertItem                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.InsertItem(Item : TListItem);
begin
end;


{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
begin
 inherited Destroy;
end;
