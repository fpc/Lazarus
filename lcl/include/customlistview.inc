{------------------------------------------------------------------------------}
{   TCustomListView Constructor                                                           }
{------------------------------------------------------------------------------}
constructor TCustomListView.Create(Aowner: TComponent);
begin
 Inherited Create(AOwner);

 ParentWindow := TScrolledwindow.Create(AOwner);
 ParentWindow.Show;
 {fix this call it is a hack}

 Columns := TViewColumns.Create(self);
 Columns.OnChange := @ColumnsChanged;
 FListItems := TListITems.Create(self);
 fCompStyle := csListView;
 fViewStyle := vsList;
 fSorted := False;
 fSortColumn := 0;
 Setbounds(2,2,300,300);

end;


{------------------------------------------------------------------------------}
{   TCustomListView ColumnsChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ColumnsChanged(Sender : TObject);
Begin
  //notify the interface....
  RecreateWnd;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemChanged(Index : Integer);  //called by TListItems
Begin
  //notify the interface....
  CNSendMessage(LM_LV_CHANGEITEM,self,@Index);
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemDeleted(Index : Integer);  //called by TListItems
Begin
  //notify the interface....
  CNSendMessage(LM_LV_DELETEITEM,self,@Index);
End;



{------------------------------------------------------------------------------}
{   TCustomListView ItemAdded                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemAdded;
Begin
  //notify the interface....
  CNSendMessage(LM_LV_ADDITEM,self,nil);
End;

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
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(Value : TViewStyle);
begin
  if FViewStyle = Value then Exit;
  FViewStyle := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetSorted                                                  }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSorted(Value : Boolean);
begin
  if FSorted = Value then Exit;
  FSorted := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(Value : Integer);
begin
  if FSortColumn = Value then Exit;
  FSortColumn := Value;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;




{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
begin
  Columns.Free;
 inherited Destroy;
end;

procedure TCustomListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect <> AValue then
     Begin
       FMultiSelect := AValue;
       CNSendMessage(LM_SETPROPERTIES,self,nil);
     end;
     
end;
