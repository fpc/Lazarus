{%MainUnit ../comctrls.pp}
{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{------------------------------------------------------------------------------
   TCustomListView Constructor
------------------------------------------------------------------------------}
constructor TCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse];
  FColumns := TListColumns.Create(self);
  FListItems := TListItems.Create(self);
  BorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FCompStyle := csListView;
  FViewStyle := vsList;
  FSortType := stNone;

// MWE: 
// A class is initalized to 0 when created
// so no need to initialize these here again   
(*
  FSortColumn := 0;
  FOnCompare := nil;
  FOnCustomDraw := nil;
  FOnCustomDrawItem := nil;
  FOnCustomDrawSubItem := nil;
  FOnAdvancedCustomDraw := nil;
  FOnAdvancedCustomDrawItem := nil;
  FOnAdvancedCustomDrawSubItem := nil;
  FSelected := nil;
  FFocused := nil;
*)  
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageChanged;
  FHoverTime := -1;
  TabStop := true;
  SetInitialBounds(0,0,100,90);
  ParentColor := False;
  Color := clWindow;  
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  FProperties := [lvpColumnClick, lvpHideSelection, lvpShowColumnHeaders];
end;

{------------------------------------------------------------------------------}
{   TCustomListView Change                                                     }
{------------------------------------------------------------------------------}
procedure TCustomListView.Change(AItem: TListItem; AChange: Integer);
var
  ItemChange: TItemChange;
begin
  case AChange of
    LVIF_TEXT: ItemChange := ctText;
    LVIF_IMAGE: ItemChange := ctImage;
    LVIF_STATE: ItemChange := ctState;
  else
    Exit;
  end;
  if Assigned(FOnChange)
  then FOnChange(Self, AItem, ItemChange);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ColClick                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.ColClick(AColumn: TListColumn);
begin
  if Assigned(FOnColumnClick) and ColumnClick then FOnColumnClick(Self, AColumn)
end;

{------------------------------------------------------------------------------}
{   TCustomListView CNNotify                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.CNNotify(var AMessage: TLMNotify);
var
  nm: PNMListView;
  Item: TListItem;
  n: Integer;
begin
  nm := PNMListView(AMessage.NMHdr);
  if nm^.iItem>=Items.Count then exit;
  //remark: NMHdr^.code is normally unhanged by the win32 interface, so the others
  //        maps there codes to the of win32
  case AMessage.NMHdr^.code of 
//  HDN_TRACK:
//  NM_CUSTOMDRAW: //Custom Drawing is handled direct from the interfaces 
                   //(at least win32 does so)
//  LVN_BEGINDRAG:
    LVN_DELETEITEM: begin
      Item := FListItems[nm^.iItem];
      if FSelected = Item then
        InvalidateSelected;
      if Item = nil then Exit; //? nm^.iItem > Items.Count ?
      Exclude(Item.FFlags, lifCreated);
      if not (lifDestroying in Item.FFlags)
      then Item.Delete;
    end;
    LVN_DELETEALLITEMS: begin
      InvalidateSelected;
      for n := FListItems.Count - 1 downto 0 do
      begin
        Item := FListItems[n];
        Exclude(Item.FFlags, lifCreated);
        if not (lifDestroying in Item.FFlags)
        then Item.Delete;
      end;
    end;
//    LVN_GETDISPINFO:
//    LVN_ODCACHEHINT:
//    LVN_ODFINDITEM:
//    LVN_ODSTATECHANGED:
//    LVN_BEGINLABELEDIT:
//    LVN_ENDLABELEDIT:
    LVN_COLUMNCLICK: begin
      ColClick(Columns[nm^.iSubItem]);
    end;
    LVN_INSERTITEM: begin
      // don't call insert yet,
      // there is no solution available when we have inserted the item first
      // see delete
      // besides... who's inserting items
    end;
    LVN_ITEMCHANGING: begin
      //Check
    end;
    LVN_ITEMCHANGED: begin
      Item := Items[nm^.iItem];
      //DebugLn('TCustomListView.CNNotify Count=',dbgs(Items.Count),' nm^.iItem=',dbgs(nm^.iItem),' destroying=',dbgs(lifDestroying in Item.FFlags));
      if (lifDestroying in Item.FFlags) then begin
        if Item=FFocused then
          FFocused:=nil;
        if Item=FSelected then
          InvalidateSelected;
      end else begin
        Change(Item, nm^.uChanged);
        if (nm^.uChanged = LVIF_STATE)
        then begin
          // focus
          if (nm^.uOldState and LVIS_FOCUSED) <> (nm^.uNewState and LVIS_FOCUSED)
          then begin
            // focus state changed
            if (nm^.uNewState and LVIS_FOCUSED) = 0
            then begin
              if FFocused = Item
              then FFocused := nil;
            end
            else begin
              FFocused := Item;
            end;
          end;
          // select
          if (nm^.uOldState and LVIS_SELECTED) <> (nm^.uNewState and LVIS_SELECTED)
          then begin
            // select state changed
            if (nm^.uNewState and LVIS_SELECTED) = 0
            then begin
              if FSelected = Item then
                InvalidateSelected;
              DoSelectItem(Item, False);
            end
            else begin
              FSelected := Item;
              Include(FFlags,lffSelectedValid);
              //DebugLn('TCustomListView.CNNotify FSelected=',dbgs(FSelected));
              DoSelectItem(Item, True);
            end;
          end;
        end;
      end;
    end;
//    LVN_GETINFOTIP:
//    NM_CLICK:
//    NM_RCLICK:
  end;
end;

procedure TCustomListView.InvalidateSelected;
begin
  FSelected:=nil;
  Exclude(FFlags,lffSelectedValid);
end;

{------------------------------------------------------------------------------}
{   TCustomListView InitializeWnd                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.InitializeWnd;
var
  LVC: TWSCustomListViewClass;
  lvil: TListViewImageList;
begin
  inherited InitializeWnd;
  
  LVC := TWSCustomListViewClass(WidgetSetClass);

  // set the style first
  LVC.SetViewStyle(Self, FViewStyle);
  
  // add columns
  FColumns.WSCreateColumns;
  
  // set imagelists and item depending properties
  for lvil := Low(TListViewImageList) to High(TListViewImageList) do
  begin
    if FImages[lvil] <> nil
    then LVC.SetImageList(Self, lvil, FImages[lvil]);
  end;
  LVC.SetScrollBars(Self, FScrollBars);
  LVC.SetScrolledLeft(Self, FScrolledLeft);
  LVC.SetScrolledTop(Self, FScrolledTop);
  LVC.SetSort(Self, FSortType, FSortColumn);

  // add items
  FListItems.WSCreateItems;
  
  // set other properties
  LVC.SetAllocBy(Self, FAllocBy);
  LVC.SetDefaultItemHeight(Self, FDefaultItemHeight);
  LVC.SetHotTrackStyles(Self, FHotTrackStyles);
  LVC.SetHoverTime(Self, FHoverTime);
  LVC.SetProperties(Self, FProperties);

  if FSelected <> nil 
  then LVC.ItemSetState(Self, FSelected.Index, FSelected, lisSelected, True);
  if FFocused <> nil
  then LVC.ItemSetState(Self, FFocused.Index, FFocused, lisFocused, True);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoDeletion                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoDeletion(AItem: TListItem);
begin
  if Assigned(FOnDeletion) then FOnDeletion(Self, AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoInsert                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoInsert(AItem: TListItem);
begin
  if Assigned(FOnInsert) then FOnInsert(Self, AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoSelectItem                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoSelectItem(AItem: TListItem; ASelected: Boolean);
begin
  AItem.Selected:=ASelected;
  if Assigned(FOnSelectItem) then FOnSelectItem(Self, AItem, ASelected);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ItemDeleted                                                }
{------------------------------------------------------------------------------}
procedure TCustomListView.ItemDeleted(const AItem: TListItem);  //called by TListItems
begin
  //DebugLn('TCustomListView.ItemDeleted ',dbgs(AItem),' FSelected=',dbgs(FSelected));
  if FSelected = AItem then InvalidateSelected;
  if FFocused = AItem then FFocused := nil;
  if csDestroying in Componentstate then Exit;
  DoDeletion(AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ItemInserted                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.ItemInserted(const AItem: TListItem);
begin
  if csDestroying in Componentstate then Exit;
  DoInsert(AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetItems                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItems(const AValue : TListItems);
begin
end; 

{------------------------------------------------------------------------------}
{   TCustomListView SetItemVisible                                             }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItemVisible(const AValue : TListItem;
                                                     const APartialOK: Boolean);
begin
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  TWSCustomListViewClass(WidgetSetClass).ItemShow(
                                        Self, AValue.Index, AValue, APartialOK);
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
{   TCustomListView SetColumns                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetColumns(const AValue: TListColumns);
begin
  if AValue=FColumns then exit;
  BeginUpdate;
  FColumns.Assign(AValue);
  EndUpdate;
  if ([csDesigning,csLoading,csReading]*ComponentState=[csDesigning]) then
    OwnerFormDesignerModified(Self);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(const AValue: TViewStyle);
begin
  if FViewStyle = AValue then Exit;
  FViewStyle := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetViewStyle(Self, AValue);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetSortType                                                }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortType(const AValue: TSortType);
begin
  if FSortType = AValue then Exit;
  FSortType := AValue;
  if not(AValue in [stNone]) then Sort;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetSort(Self, AValue, FSortColumn);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(const AValue : Integer);
begin
  if FSortColumn = AValue then Exit;
  FSortColumn := AValue;
  if not(FSortType in [stNone]) then Sort;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetSort(Self, FSortType, AValue);
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  Str1: String;
  Str2: String;
  ListView: TCustomListView;
begin
  Result := 0;
  ListView := TListItem(Item1).Owner.Owner;
  if Assigned(ListView.FOnCompare) then begin
    ListView.FOnCompare(ListView, TListItem(Item1), TListItem(Item2),0 ,Result);
  end
  else begin
    if ListView.FSortColumn = 0 then begin
      Str1 := TListItem(Item1).Caption;
      Str2 := TListItem(Item2).Caption;
    end
    else begin
      if ListView.FSortColumn <= TListItem(Item1).SubItems.Count then
        Str1 := TListItem(Item1).SubItems.Strings[ListView.FSortColumn-1]
      else Str1 := '';
      if ListView.FSortColumn <= TListItem(Item2).SubItems.Count then
        Str2 := TListItem(Item2).SubItems.Strings[ListView.FSortColumn-1]
      else Str2 := '';
    end;
    Result := AnsiCompareText(Str1, Str2);
  end;
end;

{------------------------------------------------------------------------------}
{   TCustomListView Sort                                                       }
{------------------------------------------------------------------------------}
procedure TCustomListView.Sort;
begin
  if FListItems.Count < 2 then Exit;
  FListItems.FItems.Sort(@CompareItems);
end;

{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
begin
  // Better destroy the wincontrol (=widget) first. So wo don't have to delete
  // all items/columns and we won't get notifications for each.
  FreeAndNil(FCanvas);
  inherited Destroy;
  FreeAndNil(FColumns);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FListItems);
end;

{------------------------------------------------------------------------------
   TCustomListView DestroyWnd
   Params: None
   Result: none

   Frees the canvas
 ------------------------------------------------------------------------------}
procedure TCustomListView.DestroyWnd; 
begin
  if FCanvas<>nil then
    TControlCanvas(FCanvas).FreeHandle;
  inherited DestroyWnd;
end;

{------------------------------------------------------------------------------
   TCustomListView BeginUpdate
   Params: None
   Result: none

   Increases the update count. Use this procedure before any big change, so that
   the interface will not show any single step.
 ------------------------------------------------------------------------------}
procedure TCustomListView.BeginUpdate;
begin
  Inc(FUpdateCount);
  if (FUpdateCount = 1) and HandleAllocated
  then TWSCustomListViewClass(WidgetSetClass).BeginUpdate(Self);
end;

procedure TCustomListView.Clear;
begin
  FListItems.Clear;
end;

{------------------------------------------------------------------------------}
{   TCustomListView EndUpdate                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.EndUpdate;
begin
  if FUpdateCount <= 0
  then RaiseGDBException('TCustomListView.EndUpdate FUpdateCount=0');
  
  Dec(FUpdateCount);
  if (FUpdateCount = 0) and HandleAllocated
  then TWSCustomListViewClass(WidgetSetClass).EndUpdate(Self);
end;

function TCustomListView.FindCaption(StartIndex: Integer; Value: string;
  Partial, Inclusive, Wrap: Boolean; PartStart: Boolean = True): TListItem;
begin
  Result := FListItems.FindCaption(StartIndex, Value, Partial, Inclusive, Wrap);
end;

function TCustomListView.GetBoundingRect: TRect;
begin
  if not HandleAllocated
  then Result := Rect(0,0,0,0)
  else Result := TWSCustomListViewClass(WidgetSetClass).GetBoundingRect(Self);
end;

function TCustomListView.GetColumnFromIndex(AIndex: Integer): TListColumn;
begin
  Result := FColumns[AIndex];
end;

function TCustomListView.GetDropTarget: TListItem;
var
  idx: Integer;
begin
  if not HandleAllocated
  then idx := -1
  else idx := TWSCustomListViewClass(WidgetSetClass).GetDropTarget(Self);
  if idx = -1
  then Result := nil
  else Result := FListItems[idx];
end;

function TCustomListView.GetFocused: TListItem;
begin
  Result := FFocused;
end;

function TCustomListView.GetImageList(const ALvilOrd: Integer): TCustomImageList;
begin
  Result := FImages[TListViewImageList(ALvilOrd)];
end;

function TCustomListView.GetHoverTime: Integer;
begin
  if HandleAllocated
  then Result := TWSCustomListViewClass(WidgetSetClass).GetHoverTime(Self)
  else Result := FHoverTime;
end;

function TCustomListView.GetItemAt(x,y: Integer): TListItem;
var 
  Item: Integer;
begin      
  Result := nil; 
  if HandleAllocated
  then begin
    Item := TWSCustomListViewClass(WidgetSetClass).GetItemAt(Self,x,y);
    if Item <> -1 
    then Result := Items[Item];
  end;
end;

function TCustomListView.GetProperty(const ALvpOrd: Integer): Boolean;
begin
  Result := (TListViewProperty(ALvpOrd) in FProperties);
end;

function TCustomListView.GetSelCount: Integer;
begin
  if HandleAllocated
  then Result := TWSCustomListViewClass(WidgetSetClass).GetSelCount(Self)
  else Result := 0;
end;

{------------------------------------------------------------------------------
   TCustomListView GetSelection
------------------------------------------------------------------------------}
function TCustomListView.GetSelection: TListItem;
var
  i: Integer;
begin
  if not (lffSelectedValid in FFlags) then begin
    FSelected:=nil;
    for i:=0 to Items.Count-1 do begin
      if Items[i].Selected then begin
        FSelected:=Items[i];
        DebugLn('TCustomListView.GetSelection ',dbgs(FSelected));
        break;
      end;
    end;
    Include(FFlags,lffSelectedValid);
  end;
  Result := FSelected;
end;

function TCustomListView.GetTopItem: TListItem;
var
  idx: Integer;
begin
  if ViewStyle in [vsSmallIcon, vsIcon]
  then idx := -1
  else idx := TWSCustomListViewClass(WidgetSetClass).GetTopItem(Self);
  if idx = -1
  then Result := nil
  else Result := FListItems[idx];
end;

function TCustomListView.GetViewOrigin: TPoint;
begin
  Result := Point(FScrolledLeft, FScrolledTop);
end;

function TCustomListView.GetVisibleRowCount: Integer;
begin
  if ViewStyle in [vsReport, vsList]
  then Result := TWSCustomListViewClass(WidgetSetClass).GetVisibleRowCount(Self)
  else Result := 0;
end;

procedure TCustomListView.SetAllocBy(const AValue: Integer);
begin
  if FAllocBy = AValue then Exit;
  FAllocBy := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetAllocBy(Self, AValue);
end;

procedure TCustomListView.SetDefaultItemHeight(AValue: Integer);
begin
  if AValue <=0 then AValue := 20;
  if AValue = FDefaultItemHeight then Exit;
  FDefaultItemHeight := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetDefaultItemHeight(Self, AValue);
end;

procedure TCustomListView.SetDropTarget(const AValue: TListItem);
begin
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, AValue.Index, AValue, lisDropTarget, True);
end;

procedure TCustomListView.SetFocused(const AValue: TListItem);
begin
  if FFocused = AValue then exit;
  FFocused := AValue;
  if not HandleAllocated then exit;
  TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, FFocused.Index, FFocused, lisFocused, True);
end;

procedure TCustomListView.SetHotTrackStyles(const AValue: TListHotTrackStyles);
begin
  if FHotTrackStyles = AValue then Exit;
  FHotTrackStyles := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetHotTrackStyles(Self, AValue);
end;

procedure TCustomListView.SetHoverTime(const AValue: Integer);
begin
  if FHoverTime = AValue then Exit;
  FHoverTime := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetHoverTime(Self, FHoverTime);
end;

procedure TCustomListView.SetImageList(const ALvilOrd: Integer; const AValue: TCustomImageList);
var
  lvil: TListViewImageList;
begin
  lvil := TListViewImageList(ALvilOrd);

  if FImages[lvil] = AValue then Exit;

  if FImages[lvil] <> nil
  then FImages[lvil].UnregisterChanges(FImageChangeLink);

  FImages[lvil] := AValue;

  if FImages[lvil] <> nil
  then begin
    FImages[lvil].RegisterChanges(FImageChangeLink);
    FImages[lvil].FreeNotification(self);
  end;

  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetImageList(Self, lvil, AValue);
end;

{------------------------------------------------------------------------------
   TCustomListView SetSelection
------------------------------------------------------------------------------}
procedure TCustomListView.SetSelection(const AValue: TListItem);
begin
  if (AValue<>nil) and (AValue.ListView<>Self) then
    raise Exception.Create('item does not belong to this listview');
  if FSelected = AValue then Exit;
  FSelected := AValue;
  //DebugLn('TCustomListView.SetSelection FSelected=',dbgs(FSelected));
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, FSelected.Index,
                                                  FSelected, lisSelected, True);
end;

procedure TCustomListView.SetOwnerData(const AValue: Boolean);
begin
  if FOwnerData=AValue then exit;
  FOwnerData:=AValue;
end;

procedure TCustomListView.SetProperty(const ALvpOrd: Integer;
  const AIsSet: Boolean);
var
  AProp: TListViewProperty;
begin
  AProp := TListViewProperty(ALvpOrd);
  if (AProp in FProperties) = AIsSet then Exit;

  if AIsSet
  then Include(FProperties, AProp)
  else Exclude(FProperties, AProp);

  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetProperty(Self, AProp, AIsSet);
end;

procedure TCustomListView.ImageChanged(Sender : TObject);
begin
  if csDestroying in ComponentState Then Exit;
// TODO: move Imagelist to interface, image changes can be update there
//  if FUpdateCount>0 then
//    Include(FStates,lvUpdateNeeded)
//  else begin
//    //image changed so redraw it all....
//    UpdateProperties;
//  end;
end;

procedure TCustomListView.Loaded;
begin
  // create interface columns if needed
  FColumns.WSCreateColumns;
  inherited Loaded;
end;

procedure TCustomListView.SetScrollBars(const AValue: TScrollStyle);
begin
  if (FScrollBars = AValue) then exit;
  FScrollBars := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetScrollBars(Self, AValue);
  UpdateScrollBars;
end;

procedure TCustomListView.SetScrolledLeft(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
//  if AValue>GetMaxScrolledLeft then AValue:=GetMaxScrolledLeft;
//  if AValue=FScrolledLeft then exit;
  FScrolledLeft:=AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetScrolledLeft(Self, AValue);
end;

procedure TCustomListView.SetScrolledTop(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if FScrolledTop=AValue then Exit;
//  if AValue>GetMaxScrolledTop then AValue:=GetMaxScrolledTop;
//  if AValue=FScrolledTop then Exit;
  FScrolledTop:=AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).SetScrolledTop(Self, AValue);
end;

procedure TCustomListView.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  DebugLn('TODO: TCustomListView.UpdateScrollbars');
  exit;

  if not HandleAllocated then exit
  else
  begin
  //  Exclude(FStates,tvsScrollbarChanged);
    if fScrollBars <> ssNone then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
      ScrollInfo.nTrackPos := 0;
      if fScrollBars in [ssBoth, ssHorizontal] then begin
        // horizontal scrollbar
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := (ClientWidth-ScrollBarWidth)-2*BorderWidth;
        if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
        ScrollInfo.nMax := {GetMaxScrollLeft+}ScrollInfo.nPage;
        if ScrollInfo.nMax<1 then ScrollInfo.nMax:=1;
        ScrollInfo.nPos := FScrolledLeft;
        if not CompareMem(@ScrollInfo,@FLastHorzScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastHorzScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
          ShowScrollBar(Handle,SB_HORZ,True);
        end;
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        // vertical scrollbar
        ScrollInfo.nMin := 0;
        ScrollInfo.nPage := (ClientHeight-ScrollBarWidth)-FDefaultItemHeight;
        if ScrollInfo.nPage<1 then ScrollInfo.nPage:=1;
        ScrollInfo.nMax := {GetMaxScrollTop+}ScrollInfo.nPage;
        if ScrollInfo.nMax<1 then ScrollInfo.nMax:=1;
        ScrollInfo.nTrackPos := 0;
        ScrollInfo.nPos := FScrolledTop;
        if not CompareMem(@ScrollInfo,@FLastVertScrollInfo,SizeOf(TScrollInfo))
        then begin
          FLastVertScrollInfo:=ScrollInfo;
          SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
          ShowScrollBar(Handle,SB_VERT,True);
        end;
      end;
    end;
  end;
end;

procedure TCustomListView.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_LEFT:       ScrolledLeft := 0;
    SB_RIGHT:      ScrolledLeft := width{GetMaxScrollLeft};
      // Scrolls one line left / right
    SB_LINERIGHT:  ScrolledLeft := ScrolledLeft + FDefaultItemHeight div 2;
    SB_LINELEFT:   ScrolledLeft := ScrolledLeft - FDefaultItemHeight div 2;
      // Scrolls one page of lines left / right
    SB_PAGERIGHT:  ScrolledLeft := ScrolledLeft + (ClientHeight-ScrollBarWidth)
                                       - FDefaultItemHeight;
    SB_PAGELEFT:   ScrolledLeft := ScrolledLeft - (ClientHeight-ScrollBarWidth)
                                       + FDefaultItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledLeft := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

procedure TCustomListView.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrolledTop := 0;
    SB_BOTTOM:     ScrolledTop := Height{GetMaxScrollTop};
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefaultItemHeight div 2;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefaultItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + (ClientHeight-ScrollBarWidth)
                                     - FDefaultItemHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - (ClientHeight-ScrollBarWidth)
                                     + FDefaultItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledTop := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL: ;
  end;
end;

Function TCustomListView.GetMaxScrolledLeft : Integer;
begin
  Result := 0;
end;

Function TCustomListView.GetMaxScrolledTop : Integer;
begin
  Result := 0;
end;

// included by comctrls.pp



