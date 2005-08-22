{%MainUnit gtkint.pp}
{******************************************************************************
                                 gtklistsl.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************
 
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

const
  GtkListItemGtkListTag = 'GtkList';
  GtkListItemLCLListTag = 'LCLList';

{*************************************************************}
{                      Default compare functions              }
{*************************************************************}

{function DefaultCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PGtkLabel(PGtkBin(a)^.child), @AStr);
  gtk_label_get(PGtkLabel(PGtkBin(b)^.child), @BStr);
  Result:= strcomp(AStr, BStr);
end;}

{function DefaultCheckCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PPointer(PGTKBox(PGtkBin(a)^.child)^.Children^.Next^.Data)^, @AStr);
  gtk_label_get(PPointer(PGTKBox(PGtkBin(b)^.child)^.Children^.Next^.Data)^, @BStr);
  Result:= strcomp(AStr, BStr);
end;}

{------------------------------------------------------------------------------
  function gtkListItemDrawCB(Widget: PGtkWidget; area: PGDKRectangle;
    data: gPointer) : GBoolean; cdecl;

  Handler for draw events of every item in a TGtkListStringList.
------------------------------------------------------------------------------}
function gtkListItemDrawAfterCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer): GBoolean; cdecl;
var
  Msg: TLMDrawListItem;
  ItemIndex: integer;
  GtkList: PGtkList;
  AreaRect: TRect;
  State: TOwnerDrawState;
  LCLList: TGtkListStringList;
begin
  Result:=true;

  //DebugLn('gtkListItemDrawCB ');

  // get context
  GtkList:=PGtkList(gtk_object_get_data(PGtkObject(Data),GtkListItemGtkListTag));
  if GtkList=nil then
    RaiseException('gtkListItemDrawAfterCB GtkList=nil');
  LCLList:=TGtkListStringList(gtk_object_get_data(PGtkObject(Data),
                              GtkListItemLCLListTag));
  if LCLList=nil then
    RaiseException('gtkListItemDrawAfterCB LCLList=nil');
  if [csDestroying,csLoading]*LCLList.Owner.ComponentState<>[] then exit;

  // get itemindex and area
  ItemIndex:=g_list_index(GtkList^.children,Data);
  AreaRect:=Bounds(Area^.x,Area^.y,Area^.Width,Area^.Height);
  
  // collect state flags
  State:=[odPainted];
  if g_list_index(GtkList^.selection,Widget)>=0 then
    Include(State,odSelected);
  if not GTK_WIDGET_SENSITIVE(Widget) then
    Include(State,odInactive);
  if GTK_WIDGET_HAS_DEFAULT(Widget) then
    Include(State,odDefault);
  if GTK_WIDGET_HAS_FOCUS(Widget) then
    Include(State,odFocused);

  // create message and deliver
  FillChar(Msg,SizeOf(Msg),0);
  Msg.Msg:=LM_DrawListItem;
  New(Msg.DrawListItemStruct);
  try
    FillChar(Msg.DrawListItemStruct^,SizeOf(TDrawListItemStruct),0);
    with Msg.DrawListItemStruct^ do begin
      ItemID:=ItemIndex;
      Area:=AreaRect;
      DC:=GetDC(HWnd(Widget));
      ItemState:=State;
    end;
    //DebugLn('gtkListItemDrawCB A LCLList=',DbgS(LCLList),' Widget=',DbgS(Widget),' ',DbgS(Data));
    //DebugLn('gtkListItemDrawCB B ',LCLList.ClassName,' ',DbgS(LCLList.Owner);
    //DebugLn('gtkListItemDrawCB C ',LCLList.Owner.ClassName);
    Result := DeliverMessage(LCLList.Owner, Msg)=0;
    ReleaseDC(HWnd(Widget),Msg.DrawListItemStruct^.DC);
  finally
    Dispose(Msg.DrawListItemStruct);
  end;
end;

{------------------------------------------------------------------------------
function gtkListItemExposeEvent(Widget: PGtkWidget;
  Event : PGdkEventExpose; data: gPointer): GBoolean; cdecl;

  GTK2 helper for drawing every item in a TGtkListStringList.
------------------------------------------------------------------------------}
function gtkListItemExposeEvent(Widget: PGtkWidget;
  Event : PGdkEventExpose; data: gPointer): GBoolean; cdecl;
begin
  Result := gtkListItemDrawAfterCB(Widget, @Event^.Area, data);
end;

{*************************************************************}
{                      TGtkListStringList methods             }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkListStringList.Create(List: PGtkList; TheOwner: TWinControl;
  const AWithCheckBox: Boolean);
begin
  inherited Create;
  if List = nil then RaiseException(
    'TGtkListStringList.Create Unspecified list widget');
  FGtkList:= List;
  if TheOwner = nil then RaiseException(
    'TGtkListStringList.Create Unspecified owner');
  FOwner:=TheOwner;
  FWithCheckBox := AWithCheckBox;
  //DebugLn('TGtkListStringList.Create Self=',DbgS(Self),' List=',DbgS(List),' Owner=',DbgS(Owner));
  Include(FStates,glsItemCacheNeedsUpdate);
  ConnectAllCallbacks;
  {$IFDEF CheckGtkList}
  ConsistencyCheck;
  {$ENDIF}
end;

destructor TGtkListStringList.Destroy;
begin
  // don't destroy the widgets
  RemoveAllCallbacks;
  ReAllocMem(FCachedItems,0);
  FCachedItems:=nil;
  FCachedCount:=0;
  FCachedCapacity:=0;
  //DebugLn('TGtkListStringList.Destroy Self=',DbgS(Self),' List=',DbgS(FGtkList),' Owner=',DbgS(Owner));
  inherited Destroy;
end;

function TGtkListStringList.Add(const S: string): Integer;
begin
  if FSorted then
    Result := GetInsertPosition(S)
  else
    Result:=Count;
  Insert(Count,S);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.SetSorted
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    if FSorted then Sort;
  end;
end;

procedure TGtkListStringList.CheckForInvalidFocus;
var
  Window: PGtkWindow;
begin
  { This procedure works round a gtk problem - a deleted item may have the focus
    according to an enclosing window, but the enclosing window does not notice that
    the item has gone. }
  Window := PGtkWindow(gtk_widget_get_ancestor(PGtkWidget(FGtkList),gtk_window_get_type));
  if (Window <> nil) and (Window^.focus_widget <> nil)
     and (gtk_widget_get_ancestor(Window^.focus_widget, gtk_list_get_type) = PGtkWidget(FGtkList)) then
    Window^.focus_widget := nil;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.ConnectItemCallbacks(Index: integer);

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.ConnectItemCallbacks(Index: integer);
var
  ListItem: PGtkListItem;
begin
  UpdateItemCache;
  ListItem:=FCachedItems[Index];
  ConnectItemCallbacks(ListItem);
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.ConnectItemCallbacks(Li: PGtkListItem);

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.ConnectItemCallbacks(Li: PGtkListItem);
begin
  gtk_object_set_data(PGtkObject(li),GtkListItemLCLListTag,Self);
  gtk_object_set_data(PGtkObject(li),GtkListItemGtkListTag,FGtkList);
  //DebugLn('TGtkListStringList.ConnectItemCallbacks Self=',DbgS(Self),
  //' GtkList=',DbgS(FGtkList),
  //' Owner=',DbgS(Owner),'=',Owner.ClassName,
  //' LI=',DbgS(LI),
  //' ');
  {$ifdef GTK2}
  g_signal_connect_after(G_OBJECT(li), 'expose_event',
                         G_CALLBACK(@gtkListItemExposeEvent), li);
  {$else}
  gtk_signal_connect_after(PGtkObject(li), 'draw',
                           TGTKSignalFunc(@gtkListItemDrawAfterCB),li);
  {$endif}		    
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.ConnectAllCallbacks;
 ------------------------------------------------------------------------------}
procedure TGtkListStringList.ConnectAllCallbacks;
var
  i, Cnt: integer;
begin
  BeginUpdate;
  Cnt:=Count-1;
  for i:=0 to Cnt-1 do
    ConnectItemCallbacks(i);
  EndUpdate;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.RemoveItemCallbacks(Index: integer);

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.RemoveItemCallbacks(Index: integer);
var
  ListItem: PGtkListItem;
begin
  UpdateItemCache;
  ListItem:=FCachedItems[Index];
  gtk_object_set_data(PGtkObject(ListItem),GtkListItemLCLListTag,nil);
  gtk_object_set_data(PGtkObject(ListItem),GtkListItemGtkListTag,nil);
  {$ifdef GTK2}
  g_signal_handlers_disconnect_by_func(
    G_OBJECT(ListItem),G_CALLBACK(@gtkListItemExposeEvent),ListItem);
  {$else}
  gtk_signal_disconnect_by_func(
    PGtkObject(ListItem),TGTKSignalFunc(@gtkListItemDrawAfterCB),ListItem);
  {$endif}
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.RemoveAllCallbacks;

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.RemoveAllCallbacks;
var
  i: integer;
begin
  BeginUpdate;
  for i:=0 to Count-1 do
    RemoveItemCallbacks(i);
  EndUpdate;
end;

procedure TGtkListStringList.UpdateItemCache;
var
  CurListItem: PGList;
  i: integer;
begin
  if not (glsItemCacheNeedsUpdate in FStates) then exit;
  if (FGtkList<>nil) and (FGtkList^.children<>nil) then
    FCachedCount:=g_list_length(FGtkList^.children)
  else
    FCachedCount:=0;
  if FCachedCount=0 then
    FCachedCapacity:=0
  else begin
    FCachedCapacity:=1;
    while FCachedCapacity<FCachedCount do
      FCachedCapacity:=FCachedCapacity shl 1;
    FCachedCapacity:=FCachedCapacity shl 1;
  end;
  ReAllocMem(FCachedItems,SizeOf(PGtkListItem)*FCachedCapacity);
  if FGtkList<>nil then begin
    CurListItem:=FGtkList^.children;
    i:=0;
    while CurListItem<>nil do begin
      FCachedItems[i]:=PGtkListItem(CurListItem^.Data);
      inc(i);
      CurListItem:=CurListItem^.Next;
    end;
  end;
  Exclude(FStates,glsItemCacheNeedsUpdate);
end;

function TGtkListStringList.CacheValid: boolean;
begin
  Result:=not (glsItemCacheNeedsUpdate in FStates);
end;

procedure TGtkListStringList.PutObject(Index: Integer; AnObject: TObject);
var
  ListItem : PGtkListItem;
begin
  //DebugLn('[TGtkListStringList.PutObject] Index=',Index,' Count=',Count);
  ListItem:=GetListItem(Index);
  if ListItem <> nil then
    gtk_object_set_data(PGtkObject(ListItem),'LCLStringsObject',AnObject);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Sort;
var
  sl: TStringList;
begin
  BeginUpdate;
  // sort internally (sorting in the widget would be slow and unpretty ;)
  sl:=TStringList.Create;
  sl.Assign(Self);
  sl.Sort; // currently this is quicksort -> 
             // Disadvantages: - worst case on sorted list
             //                - not keeping order
             // ToDo: replace by mergesort and add customsort
             //       remember selected items
  Assign(sl);
  sl.Free;
  EndUpdate;
end;

function TGtkListStringList.IsEqual(List: TStrings;
  CompareObjects: boolean): boolean;
var
  i, Cnt: integer;
  CmpList: TStringList;
begin
  if List=Self then begin
    Result:=true;
    exit;
  end;
  Result:=false;
  if List=nil then exit;
  Cnt:=Count;
  if (Cnt<>List.Count) then exit;
  BeginUpdate;
  CmpList:=TStringList.Create;
  try
    CmpList.Assign(List);
    CmpList.Sorted:=FSorted;
    for i:=0 to Cnt-1 do begin
      if (Strings[i]<>CmpList[i])
      or (CompareObjects and (Objects[i]<>CmpList.Objects[i])) then
        exit;
    end;
  finally
    CmpList.Free;
    EndUpdate;
  end;
  Result:=true;
end;

procedure TGtkListStringList.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TGtkListStringList.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount=0) and (glsItemCacheNeedsUpdate in FStates) then
    UpdateItemCache;
end;

procedure TGtkListStringList.ConsistencyCheck;
var
  CurListItem: PGList;
  i: integer;
  RealCachedCount: Integer;
  Str1: string;
  Str2: string;
begin
  if FCachedCount>FCachedCapacity then RaiseException('');
  if (FCachedItems=nil) and (FCachedCapacity>0) then RaiseException('');
  if (FCachedItems<>nil) and (FCachedCapacity=0) then RaiseException('');

  UpdateItemCache;
  if (FGtkList<>nil) and (FGtkList^.children<>nil) then
    RealCachedCount:=g_list_length(FGtkList^.children)
  else
    RealCachedCount:=0;
  if RealCachedCount<>FCachedCount then
    RaiseException('RealCachedCount='+IntToStr(RealCachedCount)
      +' FCachedCount='+IntToStr(FCachedCount));
  if FGtkList<>nil then begin
    CurListItem:=FGtkList^.children;
    i:=0;
    while CurListItem<>nil do begin
      if FCachedItems[i]<>PGtkListItem(CurListItem^.Data) then
        RaiseException(IntToStr(i));
      inc(i);
      CurListItem:=CurListItem^.Next;
    end;
  end;

  if Sorted then begin
    for i:=0 to FCachedCount-2 do begin
      Str1:=Strings[i];
      Str2:=Strings[i+1];
      if (AnsiCompareText(Str1,Str2)>0) then
        RaiseException(IntToStr(i)+':'+Str1+'>'+IntToStr(i+1)+':'+Str2);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Assign(Source : TPersistent);
var
  i, Cnt: integer;
  SrcStrings: TStrings;
begin
  if (Source=Self) or (Source=nil) then exit;
  if ((Source is TGtkListStringList)
    and (TGtkListStringList(Source).FGtkList=FGtkList))
  then
    RaiseException('TGtkListStringList.Assign: There 2 lists with the same FGtkList');
  BeginUpdate;
//DebugLn('[TGtkListStringList.Assign] A ',Source.Classname,' Self=',DbgS(Self),' Source=',DbgS(Source));
  try
    if Source is TStrings then begin
      // clearing and resetting can change other properties of the widget,
      // => don't change if the content is already the same
      SrcStrings:=TStrings(Source);
      if IsEqual(SrcStrings,true) then exit;
      Clear;
      Cnt:=SrcStrings.Count;
      for i:=0 to Cnt - 1 do begin
        AddObject(SrcStrings[i],SrcStrings.Objects[i]);
      end;
      // ToDo: restore other settings

      // Do not call inherited Assign as it does things we do not want to happen
    end else
      inherited Assign(Source);
  finally
    EndUpdate;
  end;
  {$IFDEF CheckGtkList}
  ConsistencyCheck;
  {$ENDIF}
//DebugLn('[TGtkListStringList.Assign] END ',Source.Classname);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkListStringList.Get(Index : integer) : string;
var
  Item : PChar;
  ALabel : PGtkLabel;
begin
  //DebugLn('[TGtkListStringList.Get] Index=',Index,' Count=',Count);
  ALabel:=GetLabel(Index);

  if ALabel = nil then
    Result:= ''
  else begin
    Item:=nil;
    gtk_label_get(ALabel, @Item);
    Result:= StrPas(Item);
  end;  
end;

function TGtkListStringList.GetObject(Index: Integer): TObject;
var
  ListItem : PGtkListItem;
begin
  //DebugLn('[TGtkListStringList.GetObject] Index=',Index,' Count=',Count);
  Result:=nil;
  ListItem:=GetListItem(Index);
  if ListItem<>nil then
    Result:=TObject(gtk_object_get_data(PGtkObject(ListItem),'LCLStringsObject'));
end;

procedure TGtkListStringList.Put(Index: Integer; const S: string);
var
  ALabel: PGtkLabel;
  NewText: PChar;
  SortedIndex: Integer;
begin
  //DebugLn('[TGtkListStringList.Put] Index=',Index,' Count=',Count);
  if Sorted then begin
    SortedIndex:=GetInsertPosition(S);
    // we move instead of insert => adjust position
    if SortedIndex>Index then dec(SortedIndex);
  end else
    SortedIndex:=Index;
  
  // change label
  ALabel:=GetLabel(Index);
  if ALabel = nil then
    RaiseException('TGtkListStringList.Put');
  if S<>'' then
    NewText:=PChar(S)
  else
    NewText:=#0;
  gtk_label_set_text(ALabel, NewText);
  
  // repair sorting
  if Sorted and (SortedIndex<>Index) then begin
    Move(Index,SortedIndex);
  end;
end;

function TGtkListStringList.GetListItem(Index: integer): PGtkListItem;
begin
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStringList.Get Out of bounds.')
  else begin
    UpdateItemCache;
    Result:=FCachedItems[Index];
  end;
end;

function TGtkListStringList.GetLabel(Index: integer): PGtkLabel;
var
  ListItem: PGtkListItem;
begin
  ListItem:=GetListItem(Index);

  if FWithCheckBox then
    Result := PPointer(PGTKBox(PGtkBin(ListItem)^.child)^.Children^.Next^.Data)^
  else
    Result := PGTKLabel(PGtkBin(ListItem)^.child);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.GetCount
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkListStringList.GetCount: integer;
begin
  if (FGtkList<>nil) and (FGtkList^.children <> nil) then begin
    UpdateItemCache;
    Result:=FCachedCount;
  end else begin
    Result:= 0
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Clear;
begin
  BeginUpdate;
  RemoveAllCallbacks;
  Include(FStates,glsItemCacheNeedsUpdate);
  CheckForInvalidFocus;
  gtk_list_clear_items(FGtkList, 0, Count);
  FCachedCount:=0;
  EndUpdate;
  {$IFDEF CheckGtkList}
  ConsistencyCheck;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Delete(Index: integer);
{$IFDEF Gtk1}
var
  Next: PGList;
{$ENDIF}
begin
  UpdateItemCache;
  RemoveItemCallbacks(Index);
  // remove item from cache
  if (Index<FCachedCount-1) then begin
    System.Move(FCachedItems[Index+1],FCachedItems[Index],
      SizeOf(Pointer)*(FCachedCount-1-Index));
  end;
  // shrink cache (lazy)
  dec(FCachedCount);
  if (FCachedCount<(FCachedCapacity shr 2)) then begin
    FCachedCapacity:=FCachedCapacity shr 1;
    ReAllocMem(FCachedItems,SizeOf(PGtkListItem)*FCachedCapacity);
  end;
  // remove item from gtk list
  if Count = 0 then
    CheckForInvalidFocus
  else begin
    {$IFDEF Gtk1}
    { Work round gtk bug - crashes if deleting first item in list
      and item has focus and there are remaining items }
    if (Index = 0) and (PGTKContainer(FGtkList)^.focus_child <> nil)
       and (gtk_list_child_position(FGtkList,PGTKContainer(FGtkList)^.focus_child) = 0) then begin
      Next := FGtkList^.children^.next;
      if Next <> nil then
       gtk_widget_grab_focus(Next^.data);
    end;
    {$ENDIF}
  end;
  gtk_list_clear_items(FGtkList, Index, Index + 1);
  {$IFDEF CheckGtkList}
  ConsistencyCheck;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function TGtkListStringList.IndexOf(const S: string): Integer;

  Returns index of item with string.
 ------------------------------------------------------------------------------}
function TGtkListStringList.IndexOf(const S: string): Integer;
var
  l, m, r, cmp: integer;
begin
  BeginUpdate;
  if FSorted then begin
    l:=0;
    r:=Count-1;
    m:=l;
    while (l<=r) do begin
      m:=(l+r) shr 1;
      cmp:=AnsiCompareText(S,Strings[m]);

      if cmp<0 then
        r:=m-1
      else if cmp>0 then
        l:=m+1
      else begin
        Result:=m;
        exit;
      end;
    end;
    Result:=-1;
  end else begin
    Result:=inherited IndexOf(S);
  end;
  EndUpdate;
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Insert(Index : integer; const S : string);
var
  li, cb, box: PGtkWidget;
  item_requisition: TGtkRequisition;
  OldCount: LongInt;
  
  procedure RaiseIndexOutOfBounds;
  begin
    RaiseException('TGtkListStringList.Insert: Index '+IntToStr(Index)
      +' out of bounds. Count='+IntToStr(OldCount));
  end;
  
begin
  OldCount:=Count;
  BeginUpdate;
  try
    if FSorted then begin
      Index:=GetInsertPosition(S);
    end;
    if (Index < 0) or (Index > OldCount) then
      RaiseIndexOutOfBounds;
    if Owner = nil then RaiseException(
      'TGtkListStringList.Insert Unspecified owner');

    // ToDo:
    //  - Icons
    //  - measure item

    if FWithCheckBox
    then begin
      li := gtk_list_item_new;
      box := gtk_hbox_new(False, 0); //^Pointer(PGTKBox(box)^.children^.Next^.Data)^
      gtk_container_add(PGTKContainer(li), box);
      cb := gtk_check_button_new;
      gtk_box_pack_start(PGTKBox(box), cb, False, False, 0);
      gtk_box_pack_start(PGTKBox(box), gtk_label_new(PChar(S)), False, False, 0);
    end
    else begin
      li:=gtk_list_item_new_with_label(PChar(S));
    end;
    ConnectItemCallbacks(PGtkListItem(li));
    // grow capacity
    UpdateItemCache;
    if (FCachedCapacity<=OldCount) then begin
      if FCachedCapacity=0 then FCachedCapacity:=1;
      while (FCachedCapacity<=OldCount) do
        FCachedCapacity:=FCachedCapacity shl 1;
      ReAllocMem(FCachedItems,SizeOf(PGtkListItem)*FCachedCapacity);
    end;
    // insert item in cache
    inc(FCachedCount);
    if Index<OldCount then
      System.Move(FCachedItems[Index],FCachedItems[Index+1],
                  SizeOf(PGtkListItem)*(OldCount-Index));
    FCachedItems[Index]:=PGtkListItem(li);
    // insert in gtk
    gtk_widget_show_all(li);
    gtk_list_insert_items(FGtkList, g_list_append(nil, li), Index);
    // adjust gtk height
    if (Owner is TCustomListBox)
    and (TListBox(Owner).ItemHeight>1) then begin
      if li^.Allocation.Width>1 then
        item_requisition.Width:=li^.Allocation.Width
      else
        gtk_widget_size_request(li,@item_requisition);
      gtk_widget_set_usize(li,Max(li^.Allocation.Width,item_requisition.Width),
                           TListBox(Owner).ItemHeight);
    end;
  finally
    EndUpdate;
    {$IFDEF CheckGtkList}
    ConsistencyCheck;
    {$ENDIF}
  end;
//DebugLn('[TGtkListStringList.Insert] END Index=',Index,' Count=',Count,' ',S,',',Count);
end;

function TGtkListStringList.GetInsertPosition(const S: string): integer;
var
  l: Integer;
  Cnt: LongInt;
  r: Integer;
  m: LongInt;
  cmp: LongInt;
begin
  Cnt:=Count;
  if FSorted then begin
    l:=0;
    r:=Cnt-1;
    m:=l;
    while (l<=r) do begin
      m:=(l+r) shr 1;
      cmp:=AnsiCompareText(S,Strings[m]);
      if cmp<0 then
        r:=m-1
      else if cmp>0 then
        l:=m+1
      else
        break;
    end;
    if (m<Cnt) and (AnsiCompareText(S,Strings[m])>0) then
      inc(m);
    Result:=m;
  end else begin
    Result:=Cnt;
  end;
end;

procedure TGtkListStringList.Move(FromIndex, ToIndex: Integer);
var
  Item: PGtkListItem;
begin
  if (FromIndex=ToIndex) then exit;
  
  //debugln('TGtkListStringList.Move From=',dbgs(FromIndex),' To=',dbgs(ToIndex));
  Item:=GetListItem(FromIndex);
  
  // move in gtk
  MoveGListLink(FGtkList^.children,FromIndex,ToIndex);
  if (GTK_WIDGET_VISIBLE (PGtkWidget(FGtkList))) then
    gtk_widget_queue_resize (PGtkWidget(FGtkList));

  // move in cache
  if CacheValid then begin
    if FromIndex<ToIndex then begin
      System.Move(FCachedItems[FromIndex+1],FCachedItems[FromIndex],
                  SizeOf(PGtkListItem)*(ToIndex-FromIndex));
    end else begin
      System.Move(FCachedItems[ToIndex],FCachedItems[ToIndex+1],
                  SizeOf(PGtkListItem)*(FromIndex-ToIndex));
    end;
    FCachedItems[ToIndex]:=Item;
  end;
end;

{*************************************************************}
{                      TGtkCListStringList methods            }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkCListStringList.Create(List : PGtkCList);
begin
  inherited Create;
  if List = nil then
    RaiseException('TGtkCListStringList.Create: Unspecified list widget');
  FGtkCList:= List;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.SetSorted
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.SetSorted(Val : boolean);
begin
  if Val <> FSorted then begin
    FSorted:= Val;
    gtk_clist_set_auto_sort(FGtkCList, Val);
    if Val then Sort;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Sort
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Sort;
begin
  gtk_clist_sort(FGtkCList);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Assign(Source : TPersistent);
var
  Counter : integer;
begin
  { Do not call inherited Assign as it does things we do not want to happen }
  if Source is TStrings 
  then begin
    Clear;
    for Counter:= TStrings(Source).Count - 1 downto 0 do 
      InsertObject(0, TStrings(Source)[Counter], TStrings(Source).Objects[Counter]);
  end 
  else inherited Assign(Source);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Clear
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Clear;
begin
  gtk_clist_clear(FGtkCList);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Delete(Index : integer);
begin
  gtk_clist_remove(FGtkCList, Index);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.Get(Index : integer) : string;
var 
  Item : PChar;
begin
  if (Index < 0) or (Index >= Count) 
  then RaiseException('TGtkCListStringList.Get  Out of bounds.')
  else begin
    Item := nil;
    gtk_clist_get_text(FGtkCList, Index, 0, @Item);
    Result:= StrPas(Item);
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.GetCount
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.GetCount : integer;
begin
  Result:= FGtkCList^.rows;
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.GetObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkCListStringList.GetObject(Index: Integer): TObject;
begin
  pointer(Result) := gtk_clist_get_row_data(FGtkCList, Index);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.Insert(Index : integer; const S : string);
type
  TCSArr = record  
    Arr: array[0..15] of PChar;
    Str: array[0..0] of Char;
  end;
var
  CS: ^TCSArr;
  CSize: integer;
  K:     integer;
begin
  CSize := sizeof(TCSArr)+length(S)+1;
  GetMem(CS, CSize);
  FillChar(CS^, sizeof(TCSArr), 0);
  StrPCopy(CS^.Str, S);
  CS^.Arr[0] := @CS^.Str;
  for K := 1 to 15 do begin
    CS^.Arr[K] := StrScan(CS^.Arr[K-1], #9);
    if Assigned(CS^.Arr[K])
    then begin
      CS^.Arr[K][0] := #0;
      Inc(CS^.Arr[K]);
    end else
      break;
  end;
  gtk_clist_insert(FGtkCList, Index, PPGChar(CS));
  FreeMem(CS);
end;

{------------------------------------------------------------------------------
  Method: TGtkCListStringList.PutObject
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkCListStringList.PutObject(Index: Integer; AObject: TObject);
begin
  gtk_clist_set_row_data(FGtkCList, Index, AObject);
end;

{
  $Log$
  Revision 1.33  2005/07/23 09:35:00  mattias
  fixed gtk2 intf compilation

  Revision 1.32  2005/06/10 18:57:12  vincents
  fixed objects with a sorted listbox   from Jesus

  Revision 1.31  2005/03/21 08:12:10  mattias
  fixed removing focus of a gtk listbox on delete item  from Collin Western

  Revision 1.30  2005/03/13 22:35:17  mattias
  fixed deleting selected TListBox item under gtk1  from Collin

  Revision 1.29  2005/03/07 21:59:45  vincents
  changed hexstr(cardinal()) for pointers to dbgs() and other 64-bits fixes   from Peter Vreman

  Revision 1.28  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.27  2004/07/17 14:24:31  mattias
  fixed freeing synedit PrimarySelection and sending gtklist DrawItem msg for destroing controls

  Revision 1.26  2004/07/03 11:11:08  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.25  2004/06/27 12:37:18  mattias
  fixed gtk lists sorted

  Revision 1.24  2004/06/27 09:34:24  mattias
  fixed TStringGrid goEditing   from Jesus

  Revision 1.23  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.22  2004/04/24 20:57:18  mattias
  fixed gtk2 for drawing list item  from Ladislav

  Revision 1.21  2003/09/18 12:15:01  mattias
  fixed is checks for TCustomXXX controls

  Revision 1.20  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.19  2002/08/18 08:54:36  marc
  * Fixed chrash on saving checklistboxitems

  Revision 1.18  2003/06/20 01:37:47  marc
  + Added TCheckListBox component

  Revision 1.17  2003/05/15 13:00:21  mattias
  fixed find declaration intf cache

  Revision 1.16  2003/05/14 13:06:00  mattias
  fixed setting TListBox.Selected before createhandle

  Revision 1.15  2003/04/29 13:35:39  mattias
  improved configure build lazarus dialog

  Revision 1.14  2003/04/11 12:48:07  mattias
  fixed gtk warning on setting item height

  Revision 1.13  2003/04/05 16:39:47  mattias
  implemented showing files in opened package

  Revision 1.12  2003/03/09 21:13:32  mattias
  localized gtk interface

  Revision 1.11  2002/11/17 11:10:04  mattias
  TComboBox and TListBox accelerated and now supports objects

  Revision 1.10  2002/10/04 14:24:15  lazarus
  MG: added DrawItem to TComboBox/TListBox

  Revision 1.9  2002/10/03 18:04:46  lazarus
  MG: started customdrawitem

  Revision 1.8  2002/10/03 14:47:31  lazarus
  MG: added TComboBox.OnPopup+OnCloseUp+ItemWidth

  Revision 1.7  2002/09/07 20:30:50  lazarus
  Make TComboboxes sort again, including in OI

  Revision 1.6  2002/08/29 00:07:02  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.5  2002/05/10 06:05:57  lazarus
  MG: changed license to LGPL

  Revision 1.4  2001/11/27 15:06:13  lazarus
  MG: added multi language syntax hilighting

  Revision 1.3  2001/10/31 10:38:26  lazarus
  MG: fixed sorted customlistbox

  Revision 1.2  2001/09/30 08:34:52  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.1  2000/07/13 10:28:29  michael
  + Initial import

  Revision 1.2  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.1  2000/03/30 22:51:42  lazarus
  MWE:
    Moved from ../../lcl

  Revision 1.3  2000/03/04 00:05:21  lazarus
  MWE: added changes from Hans (HJO)

}
