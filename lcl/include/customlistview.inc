// included by comctrls.pp
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
constructor TCustomListView.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TListColumns.Create(self);
  FListItems := TListItems.Create(self);
  FBorderStyle := bsSingle;
  FScrollBars := ssBoth;
  FSmallImages := nil;
  FCompStyle := csListView;
  FViewStyle := vsList;
  FSortType := stNone;
  FSortColumn := 0;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageChanged;
  FSelected := nil;
  Setbounds(2,2,300,300);
end;


{------------------------------------------------------------------------------}
{   TCustomListView ColumnsChanged                                             }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ColumnsChanged;
Begin
  Include(FStates,lvUpdateNeeded);
  DoUpdate;
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
begin
  nm := PNMListView(AMessage.NMHdr);
  
  case AMessage.NMHdr^.code of
//    HDN_TRACK:
//    NM_CUSTOMDRAW:
//    LVN_BEGINDRAG: 
    LVN_DELETEITEM: begin
      //don'tcall delete yet, there is no sulution available when we have deleted the item first
      Item := Items[nm^.iItem];
      DoDeletion(Item)
    end;
    LVN_DELETEALLITEMS: begin
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
      // see delete comment
    end;
    LVN_ITEMCHANGING: begin
      //Check
    end;
    LVN_ITEMCHANGED: begin
      Item := Items[nm^.iItem];
      Change(Item, nm^.uChanged);
      if (nm^.uChanged = LVIF_STATE) 
      then begin
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

{------------------------------------------------------------------------------
   TCustomListView DoUpdate
 ------------------------------------------------------------------------------}
procedure TCustomListView.DoUpdate;
begin
  if FUpdateCount>0 then exit;
  if not (lvUpdateNeeded in FStates) then exit;
  if csDestroying in Componentstate Then Exit;
  //TODO: Optimize implementation by invoking individual updates instead of
  //      recreating window
  //notify the interface....
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  Exclude(FStates,lvUpdateNeeded);
  RecreateWnd;
end;

procedure TCustomListView.InitializeWnd;
begin
  inherited InitializeWnd;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
  if FSelected<>nil then
    CNSendMessage(LM_LV_SELECTITEM,self,FSelected);
end;

procedure TCustomListView.Loaded;
begin
  inherited Loaded;
  DoUpdate;
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoDeletion                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoDeletion(AItem: TListItem);
begin
  if Assigned(FOnDeletion) then FOnDeletion(Self, AItem);
end;

{------------------------------------------------------------------------------}
{   TCustomListView DoSelectItem                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.DoSelectItem(AItem: TListItem; ASelected: Boolean); 
begin                                   
  if Assigned(FOnSelectItem) then FOnSelectItem(Self, AItem, ASelected);
end;

{------------------------------------------------------------------------------}
{   TCustomListView ItemChanged                                                }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemChanged(Index : Integer);  //called by TListItems
Begin
  if csDestroying in Componentstate Then Exit;
  if FUpdateCount>0 then
    Include(FStates,lvUpdateNeeded)
  else begin
    //notify the interface....
    if (not HandleAllocated) or (csLoading in ComponentState) then exit;
    CNSendMessage(LM_LV_CHANGEITEM,self,@Index);
  end;
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemDeleted                                                }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemDeleted(Index : Integer);  //called by TListItems
Begin
  if csDestroying in Componentstate Then Exit;
  if FSelected= FListItems[Index] then FSelected:=nil;
  DoDeletion(FListItems[Index]);
  if FUpdateCount>0 then
    Include(FStates,lvUpdateNeeded)
  else begin
    //notify the interface....
    if (not HandleAllocated) or (csLoading in ComponentState) then exit;
    CNSendMessage(LM_LV_DELETEITEM,self,@Index);
  end;
End;

{------------------------------------------------------------------------------}
{   TCustomListView ItemAdded                                                  }
{------------------------------------------------------------------------------}
Procedure TCustomListView.ItemAdded;
Begin
  if csDestroying in Componentstate Then Exit;
  if FUpdateCount>0 then
    Include(FStates,lvUpdateNeeded)
  else begin
    //notify the interface....
    if (not HandleAllocated) or (csLoading in ComponentState) then exit;
    CNSendMessage(LM_LV_ADDITEM,self,nil);
  end;
End;

{------------------------------------------------------------------------------}
{   TCustomListView SetItems                                                   }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetItems(const AValue : TListItems);
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
{   TCustomListView SetColumns                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetColumns(const AValue: TListColumns);
begin
  FColumns.Assign(AValue);
end;
         
{------------------------------------------------------------------------------}
{   TCustomListView SetViewStyle                                               }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetViewStyle(const AValue: TViewStyle);
begin
  if FViewStyle = AValue then Exit;
  FViewStyle := AValue;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;


{------------------------------------------------------------------------------}
{   TCustomListView SetSortType                                                  }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortType(const AValue: TSortType);
begin
  if FSortType = AValue then Exit;
  FSortType := AValue;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSortColumn                                              }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSortColumn(const AValue : Integer);
begin
  if FSortColumn = AValue then Exit;
  FSortColumn := AValue;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_SETPROPERTIES,self,nil);
end;

{------------------------------------------------------------------------------}
{   TCustomListView Destructor                                                 }
{------------------------------------------------------------------------------}
destructor TCustomListView.Destroy;
begin
  Columns.Free;
  FImageChangeLink.Free;
  FListItems.Free;
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
  inc(FUpdateCount);
end;

{------------------------------------------------------------------------------}
{   TCustomListView GetSelection                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.EndUpdate;
begin
  if FUpdateCount=0 then exit;
  dec(FUpdateCount);
  if FUpdateCount>0 then exit;
  DoUpdate;
end;

function TCustomListView.GetMultiSelect: Boolean;
begin
  Result:=lvMultiSelect in FStates;
end;

{------------------------------------------------------------------------------}
{   TCustomListView GetSelection                                                 }
{------------------------------------------------------------------------------}
function TCustomListView.GetSelection: TListItem;
begin
  Result := FSelected;
end;

{------------------------------------------------------------------------------}
{   TCustomListView SetSelection                                                 }
{------------------------------------------------------------------------------}
procedure TCustomListView.SetSelection(const AValue: TListItem);
begin
  if FSelected=AValue then exit;
  FSelected := AValue;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  CNSendMessage(LM_LV_SELECTITEM,self,FSelected);
  //DoSelectItem(FSelected, True);
end;

procedure TCustomListView.SetMultiSelect(const AValue: Boolean);
begin
  if MultiSelect <> AValue then
  Begin
    if AValue then
      Include(FStates,lvMultiSelect)
    else
      Exclude(FStates,lvMultiSelect);
    if (not HandleAllocated) or (csLoading in ComponentState) then
      Include(FStates,lvUpdateNeeded);
    CNSendMessage(LM_SETPROPERTIES,self,nil);
  end;
end;

procedure TCustomListView.SetSmallImages(const AValue: TCustomImageList);
begin
    if AValue <> FSmallImages then
     Begin
        if FSmallImages <> nil then
           FSmallImages.UnregisterChanges(FImageChangeLink);

           FSmallImages := AValue;

        if FSmallImages <> nil then
           Begin
             FSmallImages.RegisterChanges(FImageChangeLink);
             FSmallImages.FreeNotification(self);
           end;
        if (not HandleAllocated) or (csLoading in ComponentState) then exit;
        CNSendMessage(LM_SETPROPERTIES,self,nil);
     end;
end;

Procedure TCustomListView.ImageChanged(Sender : TObject);
begin
  if csDestroying in Componentstate Then Exit;
  if FUpdateCount>0 then
    Include(FStates,lvUpdateNeeded)
  else begin
    //image changed so redraw it all....
    if (not HandleAllocated) or (csLoading in ComponentState) then exit;
    CNSendMessage(LM_SETPROPERTIES,self,nil);
  end;
end;

procedure TCustomListView.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then
  begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateScrollBars;
  end;
end;

procedure TCustomListView.SetScrolledLeft(AValue: integer);
begin
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
//temp  if AValue>GetMaxScrollLeft then AValue:=GetMaxScrollLeft;
  if AValue=FScrolledLeft then exit;
  FScrolledLeft:=AValue;
//  Include(FStates,tvsScrollbarChanged);
  Invalidate;
end;

procedure TCustomListView.SetScrolledTop(AValue: integer);
begin
  if FScrolledTop=AValue then exit;
  if AValue<0 then AValue:=0;
//temp  if AValue>GetMaxScrollTop then AValue:=GetMaxScrollTop;
  if AValue=FScrolledTop then exit;
  FScrolledTop:=AValue;
//  FStates:=FStates+[tvsTopItemNeedsUpdate,tvsBottomItemNeedsUpdate,
//                    tvsScrollbarChanged];
  Invalidate;
end;

procedure TCustomListView.UpdateScrollbars;
var
  ScrollInfo: TScrollInfo;
begin
  writeln('TODO: TCustomListView.UpdateScrollbars');
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
        ScrollInfo.nPage := (ClientHeight-ScrollBarWidth)-FDefItemHeight;
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
    SB_LINERIGHT:  ScrolledLeft := ScrolledLeft + FDefItemHeight div 2;
    SB_LINELEFT:   ScrolledLeft := ScrolledLeft - FDefItemHeight div 2;
      // Scrolls one page of lines left / right
    SB_PAGERIGHT:  ScrolledLeft := ScrolledLeft + (ClientHeight-ScrollBarWidth)
                                       - FDefItemHeight;
    SB_PAGELEFT:   ScrolledLeft := ScrolledLeft - (ClientHeight-ScrollBarWidth)
                                       + FDefItemHeight;
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
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefItemHeight div 2;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + (ClientHeight-ScrollBarWidth)
                                     - FDefItemHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - (ClientHeight-ScrollBarWidth)
                                     + FDefItemHeight;
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

procedure TCustomListView.SetDefaultItemHeight(AValue: integer);
begin
  if AValue<=0 then AValue:=20;
  if AValue=FDefItemHeight then exit;
  FDefItemHeight:=AValue;
//  Include(FStates,tvsTopsNeedsUpdate);
  Invalidate;
end;

// included by comctrls.pp

{ =============================================================================

  $Log$
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

 

