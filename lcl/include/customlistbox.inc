{------------------------------------------------------------------------------}
{ procedure TCustomListBox.CreateHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.CreateHandle;
var NewStrings : TStrings;
begin
  inherited CreateHandle;
  NewStrings:= TStrings(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
  NewStrings.Assign(Items);
  FItems.Free;
  FItems:= NewStrings;
  { Sync other properties }
  CNSendMessage(LM_SETBORDER, Self, nil);
  UpdateSelectionMode;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.DestroyHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.DestroyHandle;
var NewStrings : TStrings;
begin
  NewStrings:= TStringList.Create;
  NewStrings.Assign(Items);
  FItems.Free;
  FItems:= NewStrings;
  inherited DestroyHandle;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetBorderStyle                                      }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetBorderStyle(Val : TBorderStyle);
begin
  if FBorderStyle <> Val then begin
    FBorderStyle:= Val;
    if HandleAllocated then CNSendMessage(LM_SETBORDER, Self, nil);
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.UpdateSelectionMode                                 }
{------------------------------------------------------------------------------}
procedure TCustomListBox.UpdateSelectionMode;
var Message : TLMSetSelMode;
begin
  Message.ExtendedSelect:= ExtendedSelect;
  Message.MultiSelect:= MultiSelect;
  if HandleAllocated then CNSendMessage(LM_SETSELMODE, Self, @Message);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetExtendedSelect                                   }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetExtendedSelect(Val : boolean);
begin
  if Val <> FExtendedSelect then begin
    FExtendedSelect:= Val;
    UpdateSelectionMode;
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetMultiSelect                                      }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetMultiSelect(Val : boolean);
begin
  if Val <> FMultiSelect then begin
    FMultiSelect:= Val;
    UpdateSelectionMode;
  end;
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetSelected                                         }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetSelected(Index : integer; Val : boolean);
var Message : TLMSetSel;
begin
  if (Index < 0) or (Index >= Items.Count) then raise Exception.Create('List index out of bounds');
  HandleNeeded;
  Message.Index:= Index;
  Message.Selected:= Val;
  CNSendMessage(LM_SETSEL, Self, @Message);
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelected                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelected(Index : integer) : boolean;
begin
  if (Index < 0) or (Index >= Items.Count) then raise Exception.Create('List index out of bound.');
  Result:= (CNSendMessage(LM_GETSEL, Self, @Index) >= 0);
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelCount                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelCount : integer;
begin
  Result:= CNSendMessage(LM_GETSELCOUNT, Self, nil);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.SetSorted                                          }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetSorted(Val : boolean);
var AMessage : TLMSort;
begin
  if Val <> FSorted then begin
    with AMessage do begin
      Msg:= LM_SORT;
      List:= Items;
      IsSorted:= Val;
    end;
    CNSendMessage(LM_SORT, Self, @AMessage);
    FSorted:= Val;
  end;
end;

{------------------------------------------------------------------------------}
{  procedure TCustomListBox.SetStyle                                }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetStyle(Val : TListBoxStyle);
begin
  if Val <> FStyle then begin
    FStyle:= Val;
  end;
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.SetItems                                   }
{------------------------------------------------------------------------------}
procedure TCustomListBox.SetItems(Value : TStrings);
begin
  if Value <> FItems then begin
    FItems.Assign(Value);
  end;
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.Create                                      }
{------------------------------------------------------------------------------}
constructor TCustomListBox.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);
   fCompStyle := csListBox;

   FBorderStyle:= bsSingle;
   SetBounds(1, 1, 100, 25);
   FItems := TStringList.Create;
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.Destroy                                      }
{------------------------------------------------------------------------------}
destructor TCustomListBox.Destroy;
begin
  if FItems <> nil then FItems.Free;
  inherited Destroy;
end;

function TCustomListBox.GetItemIndex : integer;
begin
  Result:= CNSendMessage(LM_GETITEMINDEX, Self, nil);
end;

procedure TCustomListBox.SetItemIndex(Val : integer);
begin
  if (Val < 0) or (Val >= FItems.Count) then raise Exception.Create('Out of bounds');
  CNSendMessage(LM_SETITEMINDEX, Self, Pointer(Val));
end;


