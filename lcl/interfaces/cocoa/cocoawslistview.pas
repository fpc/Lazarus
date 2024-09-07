unit CocoaWSListView;

{$mode delphi}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils, LCLType,
  Controls, ComCtrls, Types, StdCtrls, LCLProc, Graphics, ImgList, Forms,
  WSComCtrls,
  CocoaAll, CocoaPrivate, CocoaWSCommon,
  CocoaListControl, CocoaListView, CocoaTables, CocoaCollectionView;

type
  { TCocoaWSCustomListView }

  TCocoaWSCustomListView = class(TWSCustomListView)
  private
    class var _settingLCLDirectly: Boolean;
  private
    class function getWSHandler( const lclListView: TCustomListView ):
      TCocoaWSListViewHandler;
    class function getCallback( const lclListView: TCustomListView ):
      TLCLListViewCallback;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;
    class procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); override;

    // Item
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    //carbon//class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;*)
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    // LV
    //available in 10.7 only//class procedure BeginUpdate(const ALV: TCustomListView); override;
    //available in 10.7 only//class procedure EndUpdate(const ALV: TCustomListView); override;

    //class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    //carbon//class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    //carbon//class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    //class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); override;
    //carbon//class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    //carbon//class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    //carbon//class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetImageList(const ALV: TCustomListView; const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const {%H-}AValue: Boolean); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    //class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
    class function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean; override;
    (*class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;*)
    class procedure SetViewStyle(const ALV: TCustomListView; const AValue: TViewStyle); override;
  end;

implementation

type
  TCustomListViewAccess = class(TCustomListView);

procedure CocoaListViewAllocFuncImpl(const listView: NSView; const viewStyle: TViewStyle; out backendControl: NSView; out WSHandler: TCocoaWSListViewHandler );
var
  cocoaListView: TCocoaListView Absolute listView;
  processor: TCocoaTableViewProcessor;
begin
  if viewStyle = vsReport then begin
    backendControl:= AllocCocoaTableListView;
    processor:= TCocoaTableListViewProcessor.Create;
    TCocoaTableListView(backendControl).lclSetProcessor( processor );
    WSHandler:= TCocoaWSListView_TableViewHandler.Create( cocoaListView );
  end else begin
    backendControl:= AllocCocoaCollectionView( viewStyle );
    WSHandler:= TCocoaWSListView_CollectionViewHandler.Create( cocoaListView );
  end;
end;

{ TCocoaWSCustomListView }

class function TCocoaWSCustomListView.getWSHandler(
  const lclListView: TCustomListView): TCocoaWSListViewHandler;
var
  cocoaListView: TCocoaListView;
begin
  Result:= nil;
  if NOT Assigned(lclListView) or NOT lclListView.HandleAllocated then
    Exit;
  cocoaListView:= TCocoaListView( lclListView.Handle );
  if NOT Assigned(cocoaListView) then
    Exit;
  Result:= cocoaListView.WSHandler;
end;

class function TCocoaWSCustomListView.getCallback(
  const lclListView: TCustomListView): TLCLListViewCallback;
var
  cocoaListView: TCocoaListView;
begin
  Result:= nil;
  if NOT Assigned(lclListView) or NOT lclListView.HandleAllocated then
    Exit;
  cocoaListView:= TCocoaListView( lclListView.Handle );
  if NOT Assigned(cocoaListView) then
    Exit;
  Result:= cocoaListView.callback;
end;

class function TCocoaWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  cocoaListView: TCocoaListView;
  lclListView: TCustomListViewAccess Absolute AWinControl;
  lclcb: TLCLListViewCallback;
begin
  cocoaListView:= TCocoaListView.alloc.lclInitWithCreateParams(AParams);
  cocoaListView.setAutoresizesSubviews( True );
  cocoaListView.setAllocFunc( CocoaListViewAllocFuncImpl );
  lclcb := TLCLListViewCallback.Create( cocoaListView, lclListView, cocoaListView );
  lclcb.listView := lclListView;
  cocoaListView.callback:= lclcb;
  Result:= TLCLHandle( cocoaListView );
end;

class procedure TCocoaWSCustomListView.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  cocoaListView: TCocoaListView;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  cocoaListView:= TCocoaListView(AWinControl.Handle);
  ScrollViewSetBorderStyle(cocoaListView.scrollView, ABorderStyle);
  UpdateControlFocusRing(cocoaListView.documentView, AWinControl);
end;

class procedure TCocoaWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnDelete( AIndex );
end;

class function TCocoaWSCustomListView.ColumnGetWidth(
  const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn
  ): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ColumnGetWidth( AIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnInsert( AIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnMove( AOldIndex, ANewIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnSetAlignment(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAlignment: TAlignment);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetAlignment( AIndex, AColumn, AAlignment );
end;

class procedure TCocoaWSCustomListView.ColumnSetAutoSize(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAutoSize: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetAutoSize( AIndex, AColumn, AAutoSize );
end;

class procedure TCocoaWSCustomListView.ColumnSetCaption(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ACaption: String);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetCaption( AIndex, AColumn, ACaption );
end;

class procedure TCocoaWSCustomListView.ColumnSetMaxWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMaxWidth: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetMaxWidth( AIndex, AColumn, AMaxWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetMinWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMinWidth: integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetMinWidth( AIndex, AColumn, AMinWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AWidth: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetWidth( AIndex, AColumn, AWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetVisible(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AVisible: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetVisible( AIndex, AColumn, AVisible );
end;

class procedure TCocoaWSCustomListView.ColumnSetSortIndicator(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetSortIndicator( AIndex, AColumn, ASortIndicator );
end;

class procedure TCocoaWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemDelete( AIndex );
end;

class function TCocoaWSCustomListView.ItemDisplayRect(
  const ALV: TCustomListView; const AIndex, ASubItem: Integer;
  ACode: TDisplayCode): TRect;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= Bounds(0,0,0,0);
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemDisplayRect( AIndex, ASubItem, ACode );
end;

class procedure TCocoaWSCustomListView.ItemExchange(const ALV: TCustomListView;
  AItem: TListItem; const AIndex1, AIndex2: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemExchange(ALV, AItem, AIndex1, AIndex2);
end;

class function TCocoaWSCustomListView.ItemGetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem
  ): Boolean;
var
  lclcb : TLCLListViewCallback;
begin
  Result:= False;
  lclcb:= getCallback( ALV );
  if NOT Assigned(lclcb) then
    Exit;

  Result := lclcb.checkedIndexSet.containsIndex(AIndex);
end;

class function TCocoaWSCustomListView.ItemGetPosition(
  const ALV: TCustomListView; const AIndex: Integer): TPoint;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= TPoint.Create( 0, 0 );
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemGetPosition( AIndex );
end;

class function TCocoaWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= False;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemGetState( AIndex, AItem, AState, AIsSet );
end;

class procedure TCocoaWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemInsert( AIndex, AItem );
end;

class procedure TCocoaWSCustomListView.ItemSetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem;
  const AChecked: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
  lclcb: TLCLListViewCallback;
  cocoaListView: TCocoaListView;
  needsUpdate: Boolean = False;
begin
  if _settingLCLDirectly then
    Exit;

  lclcb:= self.getCallback( ALV );
  if NOT Assigned(lclcb) then
    Exit;

  if AChecked and not lclcb.checkedIndexSet.containsIndex(AIndex) then begin
    lclcb.checkedIndexSet.addIndex(AIndex);
    needsUpdate:= True;
  end else if not AChecked and lclcb.checkedIndexSet.containsIndex(AIndex) then begin
    lclcb.checkedIndexSet.removeIndex(AIndex);
    needsUpdate:= True;
  end;

  if needsUpdate then begin
    WSHandler:= getWSHandler( ALV );
    if Assigned(WSHandler) then
      WSHandler.ItemSetChecked( AIndex, AItem, AChecked );
  end;

  _settingLCLDirectly:= True;
  AItem.Checked:= AChecked;
  _settingLCLDirectly:= False;
end;

class procedure TCocoaWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetImage( AIndex, AItem, ASubIndex, AImageIndex );
end;

class procedure TCocoaWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetState( AIndex, AItem, AState, AIsSet );
end;

class procedure TCocoaWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetText( AIndex, AItem, ASubIndex, AText );
end;

class procedure TCocoaWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemShow( AIndex, AItem, PartialOK );
end;

class function TCocoaWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetFocused();
end;

class function TCocoaWSCustomListView.GetItemAt(const ALV: TCustomListView; x,
  y: integer): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetItemAt( x, y );
end;

class function TCocoaWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetSelCount();
end;

class function TCocoaWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetSelection();
end;

class function TCocoaWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetTopItem();
end;

class function TCocoaWSCustomListView.GetVisibleRowCount(
  const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetVisibleRowCount();
end;

class procedure TCocoaWSCustomListView.SelectAll(const ALV: TCustomListView;
  const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SelectAll( AIsSet );
end;

class procedure TCocoaWSCustomListView.SetDefaultItemHeight(
  const ALV: TCustomListView; const AValue: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetDefaultItemHeight( AValue );
end;

class procedure TCocoaWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetImageList( AList, AValue );
end;

class procedure TCocoaWSCustomListView.SetItemsCount(
  const ALV: TCustomListView; const Avalue: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetItemsCount( AValue );
end;

class procedure TCocoaWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback( ALV );
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.ownerData := AValue;
  if lclcb.ownerData then begin
    lclcb.checkedIndexSet.removeAllIndexes; // releasing memory
    lclcb.mixedCheckedIndexSet.removeAllIndexes; // releasing memory
  end;
end;

class procedure TCocoaWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetProperty( AProp, AIsSet );
end;

class procedure TCocoaWSCustomListView.SetScrollBars(
  const ALV: TCustomListView; const AValue: TScrollStyle);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetScrollBars( AValue );
end;

class procedure TCocoaWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
var
  WSHandler: TCocoaWSListViewHandler;
  lclcb: TLCLListViewCallback;
begin
  lclcb:= getCallback( ALV );
  if NOT Assigned(lclcb) then
    Exit;

  if TCocoaListView(lclcb.Owner).initializing then
    Exit;

  lclcb.checkedIndexSet.removeAllIndexes;
  lclcb.mixedCheckedIndexSet.removeAllIndexes;
  lclcb.selectionIndexSet.removeAllIndexes;

  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetSort( AType, AColumn, ASortDirection );
end;

class function TCocoaWSCustomListView.RestoreItemCheckedAfterSort(
  const ALV: TCustomListView): Boolean;
begin
  Result:= True;
end;

class procedure TCocoaWSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const AValue: TViewStyle);
var
  cocoalistView: TCocoaListView;
begin
  cocoalistView:= TCocoaListView( ALV.Handle );
  cocoalistView.setViewStyle( AValue );
end;

end.

