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

{$IFOPT H+} 
  {$DEFINE H_PLUS} 
{$ELSE}
  {$H+}
  {$UNDEF H_PLUS}
{$ENDIF} 

const
  GtkListItemGtkListTag = 'GtkList';
  GtkListItemLCLListTag = 'LCLList';

{*************************************************************}
{                      Default compare function               }
{*************************************************************}

function DefaultCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PGtkLabel(PGtkBin(a)^.child), @AStr);
  gtk_label_get(PGtkLabel(PGtkBin(b)^.child), @BStr);
  Result:= strcomp(AStr, BStr);
end;

function DefaultCheckCompareFunc(a, b : gpointer) : gint; cdecl;
var AStr, BStr : PChar;
begin
  gtk_label_get(PGtkLabel(PGTKBox(a)^.Children^.Next^.Data), @AStr);
  gtk_label_get(PGtkLabel(PGTKBox(b)^.Children^.Next^.Data), @BStr);
  Result:= strcomp(AStr, BStr);
end;

{------------------------------------------------------------------------------
  function gtkListItemDrawCB(Widget: PGtkWidget; area: PGDKRectangle;
    data: gPointer) : GBoolean; cdecl;

  Handler for draw events of every item in a TGtkListStringList.
------------------------------------------------------------------------------}
function gtkListItemDrawAfterCB(Widget: PGtkWidget; area: PGDKRectangle;
  data: gPointer) : GBoolean; cdecl;
var
  Msg: TLMDrawListItem;
  ItemIndex: integer;
  GtkList: PGtkList;
  AreaRect: TRect;
  State: TOwnerDrawState;
  LCLList: TGtkListStringList;
begin
  Result:=true;
  
  //writeln('gtkListItemDrawCB ');

  // get context
  GtkList:=PGtkList(gtk_object_get_data(PGtkObject(Data),GtkListItemGtkListTag));
  if GtkList=nil then
    RaiseException('gtkListItemDrawAfterCB GtkList=nil');
  LCLList:=TGtkListStringList(gtk_object_get_data(PGtkObject(Data),
                              GtkListItemLCLListTag));
  if LCLList=nil then
    RaiseException('gtkListItemDrawAfterCB LCLList=nil');

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
    //writeln('gtkListItemDrawCB A LCLList=',HexStr(Cardinal(LCLList),8),' Widget=',HexStr(Cardinal(Widget),8),' ',HexStr(Cardinal(Data),8));
    //writeln('gtkListItemDrawCB B ',LCLList.ClassName,' ',HexStr(Cardinal(LCLList.Owner),8));
    //writeln('gtkListItemDrawCB C ',LCLList.Owner.ClassName);
    Result := DeliverMessage(LCLList.Owner, Msg)=0;
    ReleaseDC(HWnd(Widget),Msg.DrawListItemStruct^.DC);
  finally
    Dispose(Msg.DrawListItemStruct);
  end;
end;


{*************************************************************}
{                      TGtkListStringList methods             }
{*************************************************************}

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Create
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
constructor TGtkListStringList.Create(List : PGtkList; TheOwner: TWinControl; const AWithCheckBox: Boolean);
begin
  inherited Create;
  if List = nil then RaiseException(
    'TGtkListStringList.Create Unspecified list widget');
  FGtkList:= List;
  if TheOwner = nil then RaiseException(
    'TGtkListStringList.Create Unspecified owner');
  FOwner:=TheOwner;
  FWithCheckBox := AWithCheckBox;
  //writeln('TGtkListStringList.Create Self=',HexStr(Cardinal(Self),8),' List=',HexStr(Cardinal(List),8),' Owner=',HexStr(Cardinal(Owner),8));
  Include(FStates,glsItemCacheNeedsUpdate);
  ConnectAllCallbacks;
end;

destructor TGtkListStringList.Destroy;
begin
  // don't destroy the widgets
  RemoveAllCallbacks;
  ReAllocMem(FCachedItems,0);
  //writeln('TGtkListStringList.Destroy Self=',HexStr(Cardinal(Self),8),' List=',HexStr(Cardinal(FGtkList),8),' Owner=',HexStr(Cardinal(Owner),8));
  inherited Destroy;
end;

function TGtkListStringList.Add(const S: string): Integer;
begin
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
  //writeln('TGtkListStringList.ConnectItemCallbacks Self=',HexStr(Cardinal(Self),8),
  //' GtkList=',HexStr(Cardinal(FGtkList),8),
  //' Owner=',HexStr(Cardinal(Owner),8),'=',Owner.ClassName,
  //' LI=',HexStr(Cardinal(LI),8),
  //' ');
  gtk_signal_connect_after(PGtkObject(li), 'draw',
                           TGTKSignalFunc(@gtkListItemDrawAfterCB),li);
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
  gtk_signal_disconnect_by_func(
    PGtkObject(ListItem), TGTKSignalFunc(@gtkListItemDrawAfterCB),ListItem);
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
  ReAllocMem(FCachedItems,SizeOf(PGtkListItem)*FCachedCount);
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

procedure TGtkListStringList.PutObject(Index: Integer; AnObject: TObject);
var
  ListItem : PGtkListItem;
begin
//writeln('[TGtkListStringList.Get] Index=',Index,' Count=',Count);
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStringList.PutObject Out of bounds.')
  else if FGtkList<>nil then begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];
    if ListItem <> nil then begin
      gtk_object_set_data(PGtkObject(ListItem),'LCLStringsObject',AnObject);
    end;
  end;
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
  Assign(sl);
  sl.Free;
  EndUpdate;
end;

function TGtkListStringList.IsEqual(List: TStrings): boolean;
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
  BeginUpdate;
  Cnt:=Count;
  if (Cnt<>List.Count) then exit;
  CmpList:=TStringList.Create;
  try
    CmpList.Assign(List);
    CmpList.Sorted:=FSorted;
    for i:=0 to Cnt-1 do begin
      if (Strings[i]<>CmpList[i]) or (Objects[i]<>CmpList.Objects[i]) then exit;
    end;
  finally
    CmpList.Free;
  end;
  Result:=true;
  EndUpdate;
end;

procedure TGtkListStringList.BeginUpdate;
begin
  if FUpdateCount=0 then Include(FStates,glsItemCacheNeedsUpdate);
  inc(FUpdateCount);
end;

procedure TGtkListStringList.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount=0 then Include(FStates,glsItemCacheNeedsUpdate);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Assign
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Assign(Source : TPersistent);
var
  i, Cnt: integer;
begin
  if (Source=Self) or (Source=nil) then exit;
  if ((Source is TGtkListStringList)
    and (TGtkListStringList(Source).FGtkList=FGtkList))
  then
    RaiseException('TGtkListStringList.Assign: There 2 lists with the same FGtkList');
  BeginUpdate;
//writeln('[TGtkListStringList.Assign] A ',Source.Classname,' Self=',HexStr(Cardinal(Self),8),' Source=',HexStr(Cardinal(Source),8));
  try
    if Source is TStrings then begin
      // clearing and resetting can change other properties of the widget,
      // => don't change if the content is already the same
      if IsEqual(TStrings(Source)) then exit;
      Clear;
      Cnt:=TStrings(Source).Count;
      for i:=0 to Cnt - 1 do begin
        AddObject(TStrings(Source)[i],TStrings(Source).Objects[i]);
      end;
      // ToDo: restore other settings

      // Do not call inherited Assign as it does things we do not want to happen
    end else
      inherited Assign(Source);
  finally
    EndUpdate;
  end;
//writeln('[TGtkListStringList.Assign] END ',Source.Classname);
end;

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Get
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
function TGtkListStringList.Get(Index : integer) : string;
var
  Item : PChar;
  ALabel : PGtkWidget;
  ListItem : PGtkListItem;
begin
//writeln('[TGtkListStringList.Get] Index=',Index,' Count=',Count);
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStringList.Get Out of bounds.')
  else begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];

    if FWithCheckBox
    then ALabel:= PGTKBox(ListItem)^.Children^.Next^.Data
    else ALabel:= PGtkBin(ListItem)^.child;

    if ALabel = nil then
      Result:= ''
    else begin
      Item:=nil;
      gtk_label_get (PGtkLabel(ALabel), @Item);
      Result:= StrPas(Item);
    end;  
  end;      
end;

function TGtkListStringList.GetObject(Index: Integer): TObject;
var
  ListItem : PGtkListItem;
begin
//writeln('[TGtkListStringList.Get] Index=',Index,' Count=',Count);
  Result:=nil;
  if (Index < 0) or (Index >= Count) then
    RaiseException('TGtkListStringList.GetObject Out of bounds.')
  else if FGtkList<>nil then begin
    UpdateItemCache;
    ListItem:=FCachedItems[Index];
    if ListItem<>nil then begin
      Result:=TObject(gtk_object_get_data(PGtkObject(ListItem),'LCLStringsObject'));
    end;
  end;
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
  RemoveAllCallbacks;
  Include(FStates,glsItemCacheNeedsUpdate);
  gtk_list_clear_items(FGtkList, 0, Count);
end;  

{------------------------------------------------------------------------------
  Method: TGtkListStringList.Delete
  Params: 
  Returns: 

 ------------------------------------------------------------------------------}
procedure TGtkListStringList.Delete(Index : integer);
begin
  RemoveItemCallbacks(Index);
  Include(FStates,glsItemCacheNeedsUpdate);
  gtk_list_clear_items(FGtkList, Index, Index + 1);
end;

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
  l, m, r, cmp: integer;
  item_requisition: TGtkRequisition;
begin
  BeginUpdate;
  try
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
        else
          break;
      end;
      if (m<Count) and (AnsiCompareText(S,Strings[m])>0) then
        inc(m);
      Index:=m;
    end;
    if (Index < 0) or (Index > Count) then
      RaiseException('TGtkListStringList.Insert: Index '+IntToStr(Index)
        +' out of bounds. Count='+IntToStr(Count));
    if Owner = nil then RaiseException(
      'TGtkListStringList.Insert Unspecified owner');

    // ToDo:
    //  - Icons
    //  - measure item

    if FWithCheckBox
    then begin
      li := gtk_list_item_new;
      box := gtk_hbox_new(False, 0);
      gtk_container_add(PGTKContainer(li), box);
      cb := gtk_check_button_new;
      gtk_box_pack_start(PGTKBox(box), cb, False, False, 0);
      gtk_box_pack_start(PGTKBox(box), gtk_label_new(PChar(S)), False, False, 0);
    end
    else begin
      li:=gtk_list_item_new_with_label(PChar(S));
    end;
    ConnectItemCallbacks(PGtkListItem(li));
    Include(FStates,glsItemCacheNeedsUpdate);
    gtk_widget_show_all(li);
    gtk_list_insert_items(FGtkList, g_list_append(nil, li), Index);
    if (Owner is TListBox)
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
  end;
//writeln('[TGtkListStringList.Insert] END Index=',Index,' Count=',Count,' ',S,',',Count);
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
  if List = nil then RaiseException('TGtkCListStringList.Create: Unspecified list widget');
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
    if Assigned(CS^.Arr[K]) then begin
      CS^.Arr[K][0] := #0;
      inc(integer(CS^.Arr[K]));
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

{$IFDEF H_PLUS}
  {$UNDEF H_PLUS}
{$ELSE}
  {$H-}
{$ENDIF}

{ =============================================================================

  $Log$
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
