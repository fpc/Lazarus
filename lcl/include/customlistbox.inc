// included by stdctrls.pp
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{ if not HandleAllocated then
    FItems contains a normal TStringList
  else
    FItems contains an interface specific TStrings descendent
}


{------------------------------------------------------------------------------}
{ procedure TCustomListBox.CreateHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.CreateHandle;
var NewStrings : TStrings;
begin
//writeln('[TCustomListBox.CreateHandle] A ',FItems.ClassName);
  inherited CreateHandle;
//writeln('[TCustomListBox.CreateHandle] B ',FItems.ClassName);
  // create
  NewStrings:= TStrings(Pointer(CNSendMessage(LM_GETITEMS, Self, nil)));
  NewStrings.Assign(Items);
  FItems.Free;
  FItems:= NewStrings;
  { Sync other properties }
  CNSendMessage(LM_SETBORDER, Self, nil);
  UpdateSelectionMode;
//writeln('[TCustomListBox.CreateHandle] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{ procedure TCustomListBox.DestroyHandle                                        }
{------------------------------------------------------------------------------}
procedure TCustomListBox.DestroyHandle;
var NewStrings : TStrings;
begin
//writeln('[TCustomListBox.DestroyHandle] A ',FItems.ClassName);
  NewStrings:= TStringList.Create;
  NewStrings.Assign(Items);
  FItems.Free;
  FItems:= NewStrings;
//writeln('[TCustomListBox.DestroyHandle] B ',FItems.ClassName);
  inherited DestroyHandle;
//writeln('[TCustomListBox.DestroyHandle] END ',FItems.ClassName);
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
  if (Index < 0) or (Index >= Items.Count) then
    raise Exception.Create('List index out of bounds');
//writeln('TCustomListBox.SetSelected A ',Items.Count);
  HandleNeeded;
//writeln('TCustomListBox.SetSelected B ',Items.Count);
  Message.Index:= Index;
  Message.Selected:= Val;
  CNSendMessage(LM_SETSEL, Self, @Message);
//writeln('TCustomListBox.SetSelected END ',Items.Count);
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelected                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelected(Index : integer) : boolean;
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise Exception.Create('TCustomListBox.GetSelected: index '+IntToStr(Index)
      +' out of bound. Count='+IntToStr(Items.Count));
  Result:= (CNSendMessage(LM_GETSEL, Self, @Index) >= 0);
end;

{------------------------------------------------------------------------------}
{ function TCustomListBox.GetSelCount                                          }
{------------------------------------------------------------------------------}
function TCustomListBox.GetSelCount : integer;
begin
  Result:= CNSendMessage(LM_GETSELCOUNT, Self, nil);
end;

function TCustomListBox.GetItemHeight: Integer;
begin
  Result := FItemHeight;
end;

procedure TCustomListBox.SetItemHeight(Value: Integer);
begin
  if (FItemHeight <> Value) and (Value > 0) then
    FItemHeight := Value;
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
  if (Value <> FItems) then begin
//writeln('[TCustomListBox.SetItems] A FItems=',FItems.ClassName,' Value=',Value.ClassName);
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
  FItems := TStringList.Create;
  SetBounds(1, 1, 100, 25);
end;

{------------------------------------------------------------------------------}
{  function TCustomListBox.Destroy                                      }
{------------------------------------------------------------------------------}
destructor TCustomListBox.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

function TCustomListBox.GetItemIndex : integer;
begin
//writeln('[TCustomListBox.GetItemIndex] A ',FItems.ClassName);
  Result:= CNSendMessage(LM_GETITEMINDEX, Self, nil);
//writeln('[TCustomListBox.GetItemIndex] END ');
end;

procedure TCustomListBox.SetItemIndex(Val : integer);
begin
  if (Val < 0) or (Val >= FItems.Count) then raise Exception.Create('Out of bounds');
//writeln('[TCustomListBox.SetItemIndex] A ',FItems.ClassName,' ',Val);
  CNSendMessage(LM_SETITEMINDEX, Self, Pointer(Val));
//writeln('[TCustomListBox.SetItemIndex] END ',FItems.ClassName);
end;

{------------------------------------------------------------------------------}
{  procedure TCustomListBox.Clear                                            }
{------------------------------------------------------------------------------}
procedure TCustomListBox.Clear;
begin
  FItems.Clear;
end;


// back to stdctrls.pp
