{%MainUnit ../comctrls.pp}
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
{------------------------------------------------------------------------------
   TCustomListView Constructor
------------------------------------------------------------------------------}
constructor TCustomListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TListColumns.Create(self);
  FListItems := TListItems.Create(self);
  BorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FCompStyle := csListView;
  FViewStyle := vsList;
  FSortType := stNone;
  FSortColumn := 0;
  FOnCompare := nil;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageChanged;
  FSelected := nil;
  FFocused := nil;
  FHoverTime := -1;
  TabStop := true;
  SetInitialBounds(0,0,100,90);
  ParentColor := False;
  Color := clWindow;  
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
  if Assigned(FOnColumnClick) then FOnColumnClick(Self, AColumn)
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

  case AMessage.NMHdr^.code of
//    HDN_TRACK:
//    NM_CUSTOMDRAW:
//    LVN_BEGINDRAG:
    LVN_DELETEITEM: begin
      Item := Items[nm^.iItem];
      Exclude(Item.FFlags, lifCreated);
      if not (lifDestroying in Item.FFlags)
      then Item.Delete;
    end;
    LVN_DELETEALLITEMS: begin
      for n := Items.Count - 1 downto 0 do
      begin
        Item := Items[n];
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
            if FSelected = Item
            then FSelected := nil;
            DoSelectItem(Item, False);
          end
          else begin
            FSelected := Item;
            DoSelectItem(Item, True);
          end;
        end;
      end;
    end;
//    LVN_GETINFOTIP:
//    NM_CLICK:
//    NM_RCLICK:
  end;
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
  FColumns.IntfCreateColumns;
  
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
  FListItems.IntfCreateItems;
  
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
  if FSelected = AItem then FSelected := nil;
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
  {$IFDEF VER1_0}
  // we need the csDestroying flag. 
  // FPC 1.0.x doesnt call BeforeDestruction
  BeforeDestruction;
  {$ENDIF}
  
  FreeAndNil(FColumns);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FListItems);
  inherited Destroy;
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

{------------------------------------------------------------------------------}
{   TCustomListView GetSelection                                               }
{------------------------------------------------------------------------------}
function TCustomListView.GetSelection: TListItem;
begin
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

{------------------------------------------------------------------------------}
{   TCustomListView SetSelection                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSelection(const AValue: TListItem);
begin
  if FSelected = AValue then Exit;
  FSelected := AValue;
  if not HandleAllocated then Exit;
  TWSCustomListViewClass(WidgetSetClass).ItemSetState(Self, FSelected.Index, FSelected, lisSelected, True);
end;

procedure TCustomListView.SetOwnerData(const AValue: Boolean);
begin
  if FOwnerData=AValue then exit;
  FOwnerData:=AValue;
end;

procedure TCustomListView.SetProperty(const ALvpOrd: Integer; const AIsSet: Boolean);
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

{ =============================================================================

  $Log$
  Revision 1.47  2005/06/25 15:34:03  mattias
  fixed a few fpc over warnings  from Andrew Haines

  Revision 1.46  2005/04/22 08:19:39  mattias
  added TListItems.FindCaption partstart parameter

  Revision 1.45  2005/04/21 16:15:52  mattias
  added TListView.FindCaption  from Matthijs Willemstein

  Revision 1.44  2005/02/26 18:01:19  mattias
  fixed warnings

  Revision 1.43  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.42  2004/11/25 14:28:45  vincents
  disabled TCustomListView.Begin/EndUpdate on win32.

  Revision 1.41  2004/09/22 18:06:32  micha
  convert LM_SETPROPERTIES message to interface methods for TCustomListView

  Revision 1.40  2004/09/08 23:05:35  mattias
  improved TListView.SetItemVisible  from Andrew Haines

  Revision 1.39  2004/07/24 00:00:33  mattias
  started TCollectionPropertyEditor

  Revision 1.38  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.37  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.36  2004/05/20 21:28:54  marc
  * Fixed win32 listview

  Revision 1.35  2004/05/18 23:10:41  marc
  * Started to move TListview to the WS interface

  Revision 1.34  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.33  2004/04/10 17:58:57  mattias
  implemented mainunit hints for include files

  Revision 1.32  2004/04/04 17:10:05  marc
  Patch from Andrew Haines

  Revision 1.31  2004/03/18 22:35:52  mattias
  improved TCustomListView.ItemAdded with an Index param  from Andrew

  Revision 1.30  2004/03/06 18:44:06  mattias
  workaround for fpc bug 2859

  Revision 1.29  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.27  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.26  2003/02/28 15:49:43  mattias
  fixed initial size

  Revision 1.25  2002/11/25 11:37:18  mattias
  applied patch from Vasily

  Revision 1.24  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.23  2002/10/09 11:46:04  lazarus
  MG: fixed loading TListView from stream

  Revision 1.22  2002/09/14 14:47:41  lazarus
  MG: fixed icons

  Revision 1.21  2002/09/10 10:00:27  lazarus
  MG: TListView now works handleless and SetSelection implemented

  Revision 1.20  2002/08/28 10:44:45  lazarus
  MG: implemented run param environment variables

  Revision 1.19  2002/05/28 14:58:30  lazarus
  MG: added scrollbars for TListView

  Revision 1.18  2002/05/10 06:05:52  lazarus
  MG: changed license to LGPL

  Revision 1.17  2002/03/27 08:57:17  lazarus
  MG: reduced compiler warnings

  Revision 1.16  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.15  2002/03/23 15:49:22  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.14  2002/03/14 23:25:52  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.13  2002/03/12 23:55:37  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

}



