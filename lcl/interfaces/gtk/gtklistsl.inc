{%MainUnit gtkint.pp}
{******************************************************************************
                                 gtklistsl.inc
                 TGtkListStringList and TGtkCListStringList
                 
 ******************************************************************************
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


{*************************************************************
                      Default compare functions
*************************************************************}

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
    RaiseGDBException('gtkListItemDrawAfterCB GtkList=nil');
  LCLList:=TGtkListStringList(gtk_object_get_data(PGtkObject(Data),
                              GtkListItemLCLListTag));
  if LCLList=nil then
    RaiseGDBException('gtkListItemDrawAfterCB LCLList=nil');
  if [csDestroying,csLoading]*LCLList.Owner.ComponentState<>[] then exit;
  
  // only owner draw lists are interested in drawing items themselves.
  if LclList.Owner is TCustomListbox then
    if TCustomListbox(LCLList.Owner).Style = lbStandard then
      exit;
  if LclList.Owner is TCustomCombobox then
    if not TCustomCombobox(LclList.Owner).Style.IsOwnerDrawn then
      exit;

  // get itemindex and area
  ItemIndex:=g_list_index(GtkList^.children,Data);
  {$IFDEF GTK1}
  AreaRect:=Bounds(Area^.x,Area^.y,Area^.Width,Area^.Height);
  {$ELSE}
  with Widget^.allocation do begin
    AreaRect:=Bounds(x,y,width,height);
  end;
  {$ENDIF}
  
  // collect state flags
  State:=[odBackgroundPainted];
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
      DC:=GetDC(HWnd(PtrUInt(Widget)));
      ItemState:=State;
    end;
    //DebugLn('gtkListItemDrawAfterCB ',DbgSName(LCLList.Owner),' Widget=',DbgS(Widget));
    Result := DeliverMessage(LCLList.Owner, Msg)=0;
    ReleaseDC(HWnd(PtrUInt(Widget)),Msg.DrawListItemStruct^.DC);
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

{------------------------------------------------------------------------------
  function gtkListItemToggledCB(Widget: PGtkWidget; Data: gPointer): GBoolean; cdecl;

  Called when a toggle button has change in a
  TGtkListStringList (TCustomCheckListBox).
------------------------------------------------------------------------------}
function gtkListItemToggledCB(Widget: PGtkWidget; Data: gPointer): GBoolean; cdecl;
var
  GtkList: PGtkList;
  LCLList: TGtkListStringList;
  Mess: TLMessage;
  ItemIndex: LongInt;
begin
  Result:=true;

  //DebugLn('gtkListItemDrawCB ');

  // get context
  GtkList:=PGtkList(gtk_object_get_data(PGtkObject(Data),GtkListItemGtkListTag));
  if GtkList=nil then
    RaiseGDBException('gtkListItemToggledCB GtkList=nil');
  LCLList:=TGtkListStringList(gtk_object_get_data(PGtkObject(Data),
                              GtkListItemLCLListTag));
  if LCLList=nil then
    RaiseGDBException('gtkListItemToggledCB LCLList=nil');
  if [csDestroying,csLoading]*LCLList.Owner.ComponentState<>[] then exit;

  // get itemindex and area
  ItemIndex:=g_list_index(GtkList^.children,Data);
  
  if LockOnChange(PgtkObject(LCLList.Owner.Handle),0) > 0 then Exit;

  if GtkWidgetIsA(Widget,gtk_toggle_button_get_type) then begin
    gtk_object_set_data(PgtkObject(Widget), 'Grayed', nil);
  end;

  Mess.Msg := LM_CHANGED;
  Mess.Result := 0;
  Mess.WParam := ItemIndex;
  //DebugLn(['gtkListItemToggledCB ',ItemIndex]);
  DeliverMessage(LCLList.Owner, Mess);
end;

procedure GtkListItemSelectAfterCB(Widget: PGtkWidget; data: gpointer); cdecl;

  procedure RaiseGTKListNotFound;
  var
    s: String;
    ChildWidget: PGtkWidget;
    BoxWidget: PGtkBox;
    LabelWidget: PGtkLabel;
    LabelText: PChar;
  begin
    s:='gtkListItemSelectAfterCB GtkList=nil li='+dbgs(Widget);
    ChildWidget:=PGtkBin(Widget)^.child;
    LabelWidget:=nil;
    if GtkWidgetIsA(ChildWidget,gtk_label_get_type) then
      LabelWidget:=PGTKLabel(ChildWidget)
    else if GtkWidgetIsA(ChildWidget,gtk_box_get_type) then begin
      BoxWidget:=PGTKBox(ChildWidget);
      if (BoxWidget^.Children<>nil)
      and (BoxWidget^.Children^.Next<>nil) then begin
        LabelWidget:=BoxWidget^.Children^.Next^.Data;
        if not GtkWidgetIsA(PGtkWidget(LabelWidget),gtk_label_get_type) then
          LabelWidget:=nil;
      end;
    end;
    if LabelWidget<>nil then begin
      LabelText:=nil;
      gtk_label_get(LabelWidget, @LabelText);
      s:=s+' Text="'+DbgStr(StrPas(LabelText))+'"';
    end;
    RaiseGDBException(s);
  end;

var
  GtkList: PGtkList;
  LCLList: TGtkListStringList;
  //ItemIndex: LongInt;
  Mess: TLMessage;
begin
  {$IFDEF EventTrace}
  Debugln('gtkListItemSelectAfterCB');
  {$ENDIF}
  // get context
  GtkList:=PGtkList(gtk_object_get_data(PGtkObject(Data),GtkListItemGtkListTag));
  if GtkList=nil then
    RaiseGTKListNotFound;
  LCLList:=TGtkListStringList(gtk_object_get_data(PGtkObject(Data),
                              GtkListItemLCLListTag));
  if LCLList=nil then
    RaiseGDBException('gtkListItemSelectAfterCB LCLList=nil');
  if [csDestroying,csLoading]*LCLList.Owner.ComponentState<>[] then exit;

  // get itemindex and area
  //ItemIndex:=g_list_index(GtkList^.children,Data);

  if LockOnChange(PGtkObject(LCLList.Owner.Handle),0) > 0 then Exit;

  Mess.Msg := LM_SELCHANGE;
  Mess.Result := 0;
  DeliverMessage(LCLList.Owner, Mess);
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
  if List = nil then RaiseGDBException(
    'TGtkListStringList.Create Unspecified list widget');
  FGtkList:= List;
  if TheOwner = nil then RaiseGDBException(
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
  if FSorted then begin
    Result := GetInsertPosition(S);
    Insert(Count,S);
  end else begin
    Result:=Count;
    Insert(Result,S);
  end;
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
    according to an enclosing window, but the enclosing window does not notice
    that the item has gone. }
  Window := PGtkWindow(gtk_widget_get_ancestor(PGtkWidget(FGtkList),
                                               gtk_window_get_type));
  if (Window <> nil) and (Window^.focus_widget <> nil)
     and (gtk_widget_get_ancestor(Window^.focus_widget, gtk_list_get_type)
          = PGtkWidget(FGtkList))
  then
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
  {$IFDEF EventTrace}
  Debugln( 'connect ',strings[index]);
  {$ENDIF}
  ListItem:=FCachedItems[Index];
  ConnectItemCallbacks(ListItem);
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.ConnectItemCallbacks(Li: PGtkListItem);

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.ConnectItemCallbacks(Li: PGtkListItem);
var
  ChildWidget: Pointer;
begin
  {$IFDEF EventTrace}
   Debugln('connect itemCallback');
  {$ENDIF}
  gtk_object_set_data(PGtkObject(li),GtkListItemLCLListTag,Self);
  gtk_object_set_data(PGtkObject(li),GtkListItemGtkListTag,FGtkList);
  //DebugLn('TGtkListStringList.ConnectItemCallbacks Self=',DbgS(Self),
  //' GtkList=',DbgS(FGtkList),
  //' Owner=',DbgS(Owner),'=',Owner.ClassName,
  //' LI=',DbgS(LI),
  //' ');
  {$ifdef GTK2}
  //DebugLn(['TGtkListStringList.ConnectItemCallbacks ',DbgSName(Owner)]);
  g_signal_connect_after(G_OBJECT(li), 'expose_event',
                         G_CALLBACK(@gtkListItemExposeEvent), li);
  {$else}
  gtk_signal_connect_after(PGtkObject(li), 'draw',
                           TGTKSignalFunc(@gtkListItemDrawAfterCB),li);
  {$endif}
  if FWithCheckBox then begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(Li)^.child)^.Children^.Data)^;
    g_signal_connect_after(G_OBJECT(ChildWidget), 'toggled',
                           G_CALLBACK(@gtkListItemToggledCB), li);
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.ConnectAllCallbacks;
 ------------------------------------------------------------------------------}
procedure TGtkListStringList.ConnectAllCallbacks;
var
  i: Integer;
begin
  BeginUpdate;
  UpdateItemCache;
  i := FCachedCount - 1;
  while i >= 0 do
  begin
    {$IFDEF EventTrace}
    DebugLn('connect ',strings[i]);
    {$ENDIF}
    ConnectItemCallbacks(FCachedItems[i]);
    Dec(i);
  end;
  EndUpdate;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.RemoveItemCallbacks(Index: integer);

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.RemoveItemCallbacks(Index: integer);
begin
  UpdateItemCache;
  RemoveItemCallbacks(FCachedItems[Index]);
end;

procedure TGtkListStringList.RemoveItemCallbacks(AItem: PGtkListItem);
var
  ChildWidget: Pointer;
begin
  {$IFDEF EventTrace}
   Debugln('connect itemCallback');
  {$ENDIF}
  gtk_object_set_data(PGtkObject(AItem), GtkListItemLCLListTag, nil);
  gtk_object_set_data(PGtkObject(AItem), GtkListItemGtkListTag, nil);
  {$ifdef GTK2}
  g_signal_handlers_disconnect_by_func(
    G_OBJECT(AItem), G_CALLBACK(@gtkListItemExposeEvent), AItem);
  {$else}
  gtk_signal_disconnect_by_func(
    PGtkObject(AItem), TGTKSignalFunc(@gtkListItemDrawAfterCB), AItem);
  {$endif}
  if FWithCheckBox
  then begin
    ChildWidget := PPointer(PGTKBox(PGtkBin(AItem)^.child)^.Children^.Data)^;
    gtk_signal_disconnect_by_func(
      PGtkObject(ChildWidget), TGTKSignalFunc(@gtkListItemToggledCB), AItem);
    FreeWidgetInfo(ChildWidget);
  end;
end;

{------------------------------------------------------------------------------
  procedure TGtkListStringList.RemoveAllCallbacks;

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.RemoveAllCallbacks;
var
  i: integer;
begin
  BeginUpdate;
  UpdateItemCache;
  i := FCachedCount - 1;
  while i >= 0 do
  begin
    RemoveItemCallbacks(FCachedItems[i]);
    Dec(i);
  end;
  EndUpdate;
end;

procedure TGtkListStringList.UpdateItemCache;
var
  CurListItem: PGList;
  i: integer;
begin
  if not (glsItemCacheNeedsUpdate in FStates) then exit;

  {$IFDEF DebugLCLComponents}
  // if items were removed => mark them as destroyed
  for i:=0 to FCachedCount-1 do begin
    if (FGtkList=nil)
    or (g_list_find(FGtkList^.children,FCachedItems[i])=nil) then begin
      if DebugGtkWidgets.IsCreated(FCachedItems[i]) then begin
        DebugLn(['TGtkListStringList.UpdateItemCache item vanished: ',i]);
        DebugGtkWidgets.MarkDestroyed(FCachedItems[i]);
      end;
    end;
  end;
  {$ENDIF}

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
      {$IFDEF DebugLCLComponents}
      if not DebugGtkWidgets.IsCreated(PGtkListItem(CurListItem^.Data)) then
      begin
        DebugLn(['TGtkListStringList.UpdateItemCache unknown item ',i,' ',DbgSName(Owner)]);
        DumpStack;
      end;
      {$ENDIF}
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
  //NOTE: in TComboBox, event handling is done inside the 'changed' event
  //  of the entry widget. Here we are locking the main combo widget.
  //  Currently, there's no know bug origined from this flaw.
  inc(FUpdateCount);
  if (FUpdateCount=1) and (Owner<>nil) and (Owner.HandleAllocated) then
    LockOnChange(PGtkObject(Owner.Handle),+1);
end;

procedure TGtkListStringList.EndUpdate;
begin
  dec(FUpdateCount);
  if (FUpdateCount=0) then begin
    if (Owner<>nil) and (Owner.HandleAllocated) then
      LockOnChange(PGtkObject(Owner.Handle),-1);
    if (glsItemCacheNeedsUpdate in FStates) then
      UpdateItemCache;
  end;
end;

procedure TGtkListStringList.ConsistencyCheck;
var
  CurListItem: PGList;
  i: integer;
  RealCachedCount: Integer;
  Str1: string;
  Str2: string;
begin
  if FCachedCount>FCachedCapacity then RaiseGDBException('');
  if (FCachedItems=nil) and (FCachedCapacity>0) then RaiseGDBException('');
  if (FCachedItems<>nil) and (FCachedCapacity=0) then RaiseGDBException('');

  UpdateItemCache;
  if (FGtkList<>nil) and (FGtkList^.children<>nil) then
    RealCachedCount:=g_list_length(FGtkList^.children)
  else
    RealCachedCount:=0;
  if RealCachedCount<>FCachedCount then
    RaiseGDBException('RealCachedCount='+IntToStr(RealCachedCount)
      +' FCachedCount='+IntToStr(FCachedCount));
  if FGtkList<>nil then begin
    CurListItem:=FGtkList^.children;
    i:=0;
    while CurListItem<>nil do begin
      if FCachedItems[i]<>PGtkListItem(CurListItem^.Data) then
        RaiseGDBException(IntToStr(i));
      inc(i);
      CurListItem:=CurListItem^.Next;
    end;
  end;

  if Sorted then begin
    for i:=0 to FCachedCount-2 do begin
      Str1:=Strings[i];
      Str2:=Strings[i+1];
      if (AnsiCompareText(Str1,Str2)>0) then
        RaiseGDBException(IntToStr(i)+':'+Str1+'>'+IntToStr(i+1)+':'+Str2);
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
    RaiseGDBException('TGtkListStringList.Assign: There 2 lists with the same FGtkList');
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
    RaiseGDBException('TGtkListStringList.Put');
  if S<>'' then
    NewText:=PChar(S)
  else
    NewText:=#0;
  gtk_label_set_text(ALabel, NewText);
  //set default font
  
  // repair sorting
  if Sorted and (SortedIndex<>Index) then begin
    Move(Index,SortedIndex);
  end;
end;

function TGtkListStringList.GetListItem(Index: integer): PGtkListItem;
begin
  if (Index < 0) or (Index >= Count) then
    RaiseGDBException('TGtkListStringList.Get Out of bounds.')
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
var
  i: integer;
begin
  BeginUpdate;
  RemoveAllCallbacks;
  for i:=0 to FCachedCount-1 do begin
    {$IFDEF DebugLCLComponents}
    DebugGtkWidgets.MarkDestroyed(FCachedItems[i]);
    {$ENDIF}
  end;
  Include(FStates,glsItemCacheNeedsUpdate);
  CheckForInvalidFocus;
  if gtkListGetSelectionMode(FGtkList)=GTK_SELECTION_BROWSE then begin
    // GTK_SELECTION_BROWSE always auto selects one child
    // -> disable it and enable it when a selection is needed
    gtk_list_set_selection_mode(FGtkList,GTK_SELECTION_SINGLE);
  end;
  gtk_list_clear_items(FGtkList, 0, Count);
  FCachedCount:=0;
  //Update the internal Item Index cache
  if FOwner.HandleAllocated and (FOwner is TCustomComboBox) then
    PInteger(GetWidgetInfo(Pointer(FOwner.Handle))^.UserData)^ := -1;
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
  {$IFDEF DebugLCLComponents}
  DebugGtkWidgets.MarkDestroyed(FCachedItems[Index]);
  {$ENDIF}
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
  // change selection mode if needed
  if (gtkListGetSelectionMode(FGtkList)=GTK_SELECTION_BROWSE)
  and (FGtkList^.selection<>nil)
  and (g_list_nth_data(FGtkList^.children, Index)=FGtkList^.selection^.data) then begin
    // item is selected and BROWSE mode is enabled
    // -> change selection mode to prevent, that gtk auto selects another child
    gtk_list_set_selection_mode(FGtkList,GTK_SELECTION_SINGLE);
  end;

  // remove item from gtk list
  if Count = 0 then
    CheckForInvalidFocus
  else begin
    {$IFDEF Gtk1}
    { Work round gtk bug - crashes if deleting first item in list
      and item has focus and there are remaining items }
    if (Index = 0) and (PGTKContainer(FGtkList)^.focus_child <> nil)
       and (gtk_list_child_position(FGtkList,PGTKContainer(FGtkList)^.focus_child) = 0)
    then begin
      Next := FGtkList^.children^.next;
      if Next <> nil then
        gtk_widget_grab_focus(Next^.data);
    end;
    {$ENDIF}
  end;
  gtk_list_clear_items(FGtkList, Index, Index + 1);
  Include(FStates,glsItemCacheNeedsUpdate);
  
  //Clear the combobox text and set item index to -1
  if FOwner is TCustomComboBox then
    TGtkWSCustomComboBox.SetItemIndex(TCustomComboBox(FOwner), -1);
  
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
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Insert
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Insert(Index : integer; const S : string);
var
  li, cb, box,aLabel: PGtkWidget;
  item_requisition: TGtkRequisition;
  OldCount: LongInt;
  LCLIndex: PInteger;
  
  procedure RaiseIndexOutOfBounds;
  begin
    RaiseGDBException('TGtkListStringList.Insert: Index '+IntToStr(Index)
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
    if Owner = nil then RaiseGDBException(
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

      aLabel:=gtk_label_new(PChar(S));
      if not TListBox(Owner).Font.IsDefault then begin
        GtkWidgetSet.SetWidgetColor(aLabel, TListBox(Owner).Font.Color, clNone,
              [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
        GtkWidgetSet.SetWidgetFont(aLabel, TListBox(Owner).Font);
      end;
      gtk_box_pack_start(PGTKBox(box), aLabel, False, False, 0);
    end
    else begin
      li:=gtk_list_item_new_with_label(PChar(S));
      aLabel:=PGtkBin(li)^.child;
      if not TListBox(Owner).Font.IsDefault then begin
        GtkWidgetSet.SetWidgetColor(aLabel, TListBox(Owner).Font.Color, clNone,
            [GTK_STATE_NORMAL,GTK_STATE_ACTIVE,GTK_STATE_PRELIGHT,GTK_STATE_SELECTED]);
        GtkWidgetSet.SetWidgetFont(aLabel, TListBox(Owner).Font);
      end;
    end;
    {$IFDEF DebugLCLComponents}
    DebugGtkWidgets.MarkCreated(li,dbgsName(Owner)+' Index='+dbgs(Index)+' Count='+dbgs(Count));
    {$ENDIF}
    {$IFDEF EventTrace}
    Debugln('insertListItem',s);
    {$ENDIF}
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
    //if the item is inserted before the selected item the
    //internal index cache becomes out of sync
    if (FOwner is TCustomComboBox) and FOwner.HandleAllocated then
    begin
      LCLIndex := PInteger(GetWidgetInfo(Pointer(FOwner.Handle))^.UserData);
      if Index <= LCLIndex^ then
        Inc(LCLIndex^);
    end;
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

