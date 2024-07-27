unit CocoaCollectionView;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  // rtl+ftl
  Classes, SysUtils,
  // Libs
  MacOSAll, CocoaAll,
  CocoaPrivate, Cocoa_Extra, CocoaCallback, CocoaConfig, CocoaUtils,
  CocoaListView, CocoaTextEdits,
  // LCL
  LCLType, Controls, ComCtrls, StdCtrls, ImgList, Forms;

type

  { TCocoaListView_CollectionView_StyleHandler }
  TCocoaListView_CollectionView_StyleHandler = class
  private
    _collectionView: NSCollectionView;
  public
    constructor Create( collectionView: NSCollectionView ); virtual;
  public
    procedure onInit; virtual; abstract;
    procedure onUpdateItemValue( indexPath:NSIndexPath;
      cocoaItem:NSCollectionViewItem ); virtual; abstract;
    procedure onUpdateItemSize( baseSize: NSSize ); virtual; abstract;
    procedure onUpdateItemLayout( item:NSCollectionViewItem ); virtual; abstract;
    procedure onAdjustTextEditorRect( var aFrame: NSRect ); virtual; abstract;
  end;

  { TCocoaCollectionItem }
  TCocoaCollectionItem = objcclass(NSCollectionViewItem)
  public
    procedure loadView; override;
    procedure prepareForReuse; message 'prepareForReuse';
    procedure dealloc; override;
  end;

  { TCocoaCollectionItemView }
  TCocoaCollectionItemView = objcclass(NSView)
  private
    item: TCocoaCollectionItem;
  public
    procedure drawRect(dirtyRect: NSRect); override;
  end;

  { TCocoaCollectionView }
  TCocoaCollectionView = objcclass(
    NSCollectionView,
    NSCollectionViewDataSourceProtocol,
    NSCollectionViewDelegateProtocol_1011,
    TCocoaListViewBackendControlProtocol )
  public
    styleHandler: TCocoaListView_CollectionView_StyleHandler;
    iconSize: NSSize;
    itemSize: NSSize;
  public
    callback: TLCLListViewCallback;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure lclExpectedKeys(var wantTabs, wantKeys, wantReturn, wantAllKeys: Boolean); override;

    procedure backend_setCallback( cb: TLCLListViewCallback );
    procedure backend_reloadData;
    procedure backend_onInit;
  public
    procedure dealloc; override;
    procedure addSubview(aView: NSView); override;
  public
    procedure updateItemValue( indexPath:NSIndexPath; cocoaItem:TCocoaCollectionItem );
      message 'updateItemValue:cocoaItem:';
    procedure updateItemSize( baseSize: NSSize );
      message 'updateItemSize:';
    procedure updateItemLayout( item:TCocoaCollectionItem );
      message 'updateItemLayout:';

    procedure redrawVisibleItems; message 'redrawVisibleItems';
    procedure restoreFromStableSelection; message 'restoreFromStableSelection';
    procedure reloadData; override;

    procedure selectOneItemByIndex( index: Integer; isSelected: Boolean );
      message 'selectOneItemByIndex:isSelected:';
  public
    function collectionView_numberOfItemsInSection(
      collectionView: NSCollectionView; section: NSInteger ): NSInteger;
    function collectionView_itemForRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView; indexPath: NSIndexPath
      ): NSCollectionViewItem;

    function collectionView_shouldChangeItemsAtIndexPaths_toHighlightState(
      collectionView: NSCollectionView;
      indexPaths: NSSet;
      highlightState: NSCollectionViewItemHighlightState): NSSet;
    procedure collectionView_didChangeItemsAtIndexPaths_toHighlightState(
      collectionView: NSCollectionView;
      indexPaths: NSSet;
      highlightState: NSCollectionViewItemHighlightState );

    procedure collectionView_willDisplayItem_forRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView;
      item:NSCollectionViewItem;
      indexPath:NSIndexPath );
    procedure collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath(
      collectionView: NSCollectionView;
      item:NSCollectionViewItem;
      indexPath:NSIndexPath );

    function collectionView_shouldSelectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet ): NSSet;
      message 'collectionView:shouldSelectItemsAtIndexPaths:';
    procedure collectionView_didSelectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet );
      message 'collectionView:didSelectItemsAtIndexPaths:';
    function collectionView_shouldDeselectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet ): NSSet;
      message 'collectionView:shouldDeselectItemsAtIndexPaths:';
    procedure collectionView_didDeselectItemsAtIndexPaths(
      collectionView: NSCollectionView; indexPaths:NSSet );
      message 'collectionView:didDeselectItemsAtIndexPaths:';

    procedure mouseDown(theEvent: NSEvent); override;
  end;

  { TCocoaWSListView_CollectionViewHandler }

  TCocoaWSListView_CollectionViewHandler = class(TCocoaWSListViewHandler)
  private
    _listView: TCocoaListView;
    _collectionView: TCocoaCollectionView;
  private
    function getCallback: TLCLListViewCallback;
    procedure doReloadDataAfterDelete( AIndex: PtrInt );
  public
    constructor Create( listView: TCocoaListView );
  public
    // Column
    procedure ColumnDelete( const AIndex: Integer ); override;
    function  ColumnGetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    procedure ColumnInsert( const AIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnMove( const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnSetAlignment( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    procedure ColumnSetAutoSize( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    procedure ColumnSetCaption( const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    procedure ColumnSetMaxWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    procedure ColumnSetMinWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    procedure ColumnSetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    procedure ColumnSetVisible( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;
    procedure ColumnSetSortIndicator( const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); override;

    // Item
    procedure ItemDelete( const AIndex: Integer); override;
    function  ItemDisplayRect( const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    function  ItemGetChecked( const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    function  ItemGetPosition( const AIndex: Integer): TPoint; override;
    function  ItemGetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    procedure ItemInsert( const AIndex: Integer; const {%H-}AItem: TListItem); override;
    procedure ItemSetChecked( const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    procedure ItemSetImage( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    procedure ItemSetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    procedure ItemSetText( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    procedure ItemShow( const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    function GetFocused: Integer; override;
    function GetItemAt( x,y: integer): Integer; override;
    function GetSelCount: Integer; override;
    function GetSelection: Integer; override;
    function GetTopItem: Integer; override;
    function GetVisibleRowCount: Integer; override;

    procedure SelectAll( const AIsSet: Boolean); override;
    procedure SetDefaultItemHeight( const AValue: Integer); override;
    procedure SetImageList( const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); override;
    procedure SetItemsCount( const Avalue: Integer); override;
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); override;
    procedure SetScrollBars( const AValue: TScrollStyle); override;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
  end;

function AllocCocoaCollectionView( style: TViewStyle ): TCocoaCollectionView;
function indexPathsWithOneIndex( cv: NSCollectionView; AIndex: Integer ): NSSet;
function realVisibleItems( cv: NSCollectionView ): NSArray;

implementation

type
  TCustomListViewAccess = class(TCustomListView);

  { TCocoaListView_CollectionView_LargeIconHandler }
  TCocoaListView_CollectionView_LargeIconHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure onInit; override;
    procedure onUpdateItemValue(indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem); override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout(item: NSCollectionViewItem); override;
    procedure onAdjustTextEditorRect( var aFrame: NSRect ); override;
  end;

  { TCocoaListView_CollectionView_SmallIconHandler }
  TCocoaListView_CollectionView_SmallIconHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure onInit; override;
    procedure onUpdateItemValue(indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem); override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout(item: NSCollectionViewItem); override;
    procedure onAdjustTextEditorRect( var aFrame: NSRect ); override;
  end;

  { TCocoaListView_CollectionView_ListHandler }
  TCocoaListView_CollectionView_ListHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure onInit; override;
    procedure onUpdateItemValue(indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem); override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout(item: NSCollectionViewItem); override;
    procedure onAdjustTextEditorRect( var aFrame: NSRect ); override;
  end;

function AllocCocoaCollectionView( style: TViewStyle ): TCocoaCollectionView;
var
  styleHandler: TCocoaListView_CollectionView_StyleHandler;
begin
  Result:= TCocoaCollectionView.new;

  case style of
    vsIcon:
      styleHandler:= TCocoaListView_CollectionView_LargeIconHandler.Create( Result );
    vsSmallIcon:
      styleHandler:= TCocoaListView_CollectionView_SmallIconHandler.Create( Result );
    vsList:
      styleHandler:= TCocoaListView_CollectionView_ListHandler.Create( Result );
  end;

  Result.styleHandler:= styleHandler;
  Result.setDataSource( Result );
  Result.setDelegate( NSCollectionViewDelegateProtocol_1011(Result) );
  Result.setCollectionViewLayout( NSCollectionViewFlowLayout.new.autorelease );
  Result.registerClass_forItemWithIdentifier( TCocoaCollectionItem, NSSTR('Cell') );
  Result.setSelectable( True );
end;

function indexPathsWithOneIndex( cv: NSCollectionView; AIndex: Integer ): NSSet;
var
  indexPath: NSIndexPath;
begin
  indexPath:= NSIndexPath.indexPathForItem_inSection( AIndex, 0 );
  Result:= NSSet.setWithObject( indexPath );
end;

// get the Real Visible Items.
// NSCollectionView returns the items that have been displayed.
function realVisibleItems( cv: NSCollectionView ): NSArray;
var
  visibleRect: NSRect;
  item: NSCollectionViewItem;
  items: NSMutableArray Absolute Result;
begin
  Result:= NSMutableArray.new.autorelease;
  visibleRect:= cv.visibleRect;
  for item in cv.visibleItems do begin
    if NSIntersectsRect( item.view.frame, visibleRect ) then
      items.addObject( item );
  end;
end;

{ TCocoaListView_CollectionView_StyleHandler }

constructor TCocoaListView_CollectionView_StyleHandler.Create(
  collectionView: NSCollectionView);
begin
  _collectionView:= collectionView;
end;

{ TCocoaListView_CollectionView_LargeIconHandler }

procedure TCocoaListView_CollectionView_LargeIconHandler.onInit;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= 64;
  minSize.height:= 68;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( 4 );
  layout.setMinimumLineSpacing( 4 );
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onUpdateItemValue(
  indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  cocoaImage: NSImage;
  lclImageIndex: Integer;
  lclText: String;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.callback.GetItemImageAt( indexPath.item, 0, lclImageIndex );
  cocoaImage:= cv.callback.GetImageFromIndex( lclImageIndex );
  cocoaItem.imageView.setImage( cocoaImage );

  cv.callback.GetItemTextAt( indexPath.item, 0, lclText );
  cocoaItem.textField.setStringValue( StrToNSString(lclText) );
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onUpdateItemSize(
  baseSize: NSSize);
var
  cv: TCocoaCollectionView;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.iconSize:= baseSize;
  if cv.iconSize.Width < 32 then
    cv.iconSize.Width:= 32;
  if cv.iconSize.Height < 32 then
    cv.iconSize.Height:= 32;

  cv.itemSize.Width:= 10 + baseSize.Width + 10;
  cv.itemSize.Height:= 10 + baseSize.Height + 2 + 14 + 10;
  if cv.itemSize.Width < 64 then
    cv.itemSize.Width:= 64;
  if cv.itemSize.Height < 68 then
    cv.itemSize.Height:= 68;
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onUpdateItemLayout(
  item: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
begin
  cv:= TCocoaCollectionView(_collectionView);
  aFrame.origin.x:= (cv.itemSize.Width - cv.iconSize.Width) / 2;
  aFrame.origin.y:= cv.itemSize.Height - cv.iconSize.Height - 10;
  aFrame.size:= cv.iconSize;
  item.imageView.setFrame( aFrame );

  aFrame.origin.x:= 0;
  aFrame.origin.y:= 9;
  aFrame.size.width:= cv.itemSize.Width;
  aFrame.size.height:= 15;
  item.textField.setAlignment( NSTextAlignmentCenter );
  item.textField.setFrame( aFrame );
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onAdjustTextEditorRect(
  var aFrame: NSRect);
begin
  aFrame.origin.x:= aFrame.origin.x + 2;
  aFrame.size.width:= aFrame.size.width - 4;
end;

{ TCocoaListView_CollectionView_SmallIconHandler }

procedure TCocoaListView_CollectionView_SmallIconHandler.onInit;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= 150;
  minSize.height:= 28;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( 10 );
  layout.setMinimumLineSpacing( 0 );
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onUpdateItemValue(
  indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  cocoaImage: NSImage;
  lclImageIndex: Integer;
  lclText: String;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.callback.GetItemImageAt( indexPath.item, 0, lclImageIndex );
  cocoaImage:= cv.callback.GetImageFromIndex( lclImageIndex );
  cocoaItem.imageView.setImage( cocoaImage );

  cv.callback.GetItemTextAt( indexPath.item, 0, lclText );
  cocoaItem.textField.setStringValue( StrToNSString(lclText) );
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onUpdateItemSize(
  baseSize: NSSize);
var
  cv: TCocoaCollectionView;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.iconSize:= baseSize;
  if cv.iconSize.Width < 16 then
    cv.iconSize.Width:= 16;
  if cv.iconSize.Height < 16 then
    cv.iconSize.Height:= 16;

  cv.itemSize.Width:= 6 + baseSize.Width + 2 + 120 + 6;
  cv.itemSize.Height:= 4 + baseSize.Height + 4;
  if cv.itemSize.Height < 28 then
    cv.itemSize.Height:= 28;
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onUpdateItemLayout(
  item: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
begin
  cv:= TCocoaCollectionView(_collectionView);
  aFrame.origin.x:= 6;
  aFrame.origin.y:= (cv.itemSize.Height - cv.iconSize.Height) / 2;
  aFrame.size:= cv.iconSize;
  item.imageView.setFrame( aFrame );

  aFrame.origin.x:= aFrame.origin.x + aFrame.size.width + 2;
  aFrame.origin.y:= (cv.itemSize.Height - 15) / 2;
  aFrame.size.width:= 120;
  aFrame.size.height:= 15;
  item.textField.setFrame( aFrame );
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onAdjustTextEditorRect(
  var aFrame: NSRect);
begin
  aFrame.size.width:= aFrame.size.width + 4;
end;

{ TCocoaListView_CollectionView_ListHandler }

procedure TCocoaListView_CollectionView_ListHandler.onInit;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= 146;
  minSize.height:= 24;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( 0 );
  layout.setMinimumLineSpacing( 10 );

  layout.setScrollDirection( NSCollectionViewScrollDirectionHorizontal );
end;

procedure TCocoaListView_CollectionView_ListHandler.onUpdateItemValue(
  indexPath: NSIndexPath; cocoaItem: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  cocoaImage: NSImage;
  lclImageIndex: Integer;
  lclText: String;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.callback.GetItemTextAt( indexPath.item, 0, lclText );
  cocoaItem.textField.setStringValue( StrToNSString(lclText) );
end;

procedure TCocoaListView_CollectionView_ListHandler.onUpdateItemSize(
  baseSize: NSSize);
var
  cv: TCocoaCollectionView;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.itemSize.Width:= 146;
  cv.itemSize.Height:= 24;
end;

procedure TCocoaListView_CollectionView_ListHandler.onUpdateItemLayout(
  item: NSCollectionViewItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
begin
  cv:= TCocoaCollectionView(_collectionView);
  aFrame.origin.x:= 4;
  aFrame.origin.y:= (cv.itemSize.Height - 15) / 2;
  aFrame.size.width:= 138;
  aFrame.size.height:= 15;
  item.textField.setFrame( aFrame );
end;

procedure TCocoaListView_CollectionView_ListHandler.onAdjustTextEditorRect(
  var aFrame: NSRect);
begin
  aFrame.origin.x:= aFrame.origin.x - 2;
  aFrame.origin.y:= aFrame.origin.y - 2;
  aFrame.size.width:= aFrame.size.width + 4;
end;

{ TCocoaCollectionItem }

procedure TCocoaCollectionItem.loadView;
var
  itemView: TCocoaCollectionItemView;
  fieldControl: NSTextField;
  imageControl: NSImageView;
begin
  itemView:= TCocoaCollectionItemView.alloc.initWithFrame( NSZeroRect);
  itemView.item:= self;

  imageControl:= NSImageView.alloc.initWithFrame( NSZeroRect );
  imageControl.cell.setImageScaling( NSImageScaleProportionallyUpOrDown );
  self.setImageView( imageControl );
  itemView.addSubview( imageControl );

  fieldControl:= NSTextField.alloc.initWithFrame(NSZeroRect);
  fieldControl.setBordered( False );
  fieldControl.setDrawsBackground( False );
  fieldControl.setEditable( False );
  fieldControl.setLineBreakMode( NSLineBreakByTruncatingTail );
  self.setTextField( fieldControl );
  itemView.addSubview( fieldControl );

  self.setView( itemView );
end;

procedure TCocoaCollectionItem.prepareForReuse;
begin
  self.view.removeFromSuperview;
end;

procedure TCocoaCollectionItem.dealloc;
begin
  self.imageView.removeFromSuperview;
  self.textField.removeFromSuperview;
  self.view.removeFromSuperview;
  self.imageView.release;
  self.textField.release;
  self.view.release;
  inherited dealloc;
end;

{ TCocoaCollectionItemView }

procedure TCocoaCollectionItemView.drawRect(dirtyRect: NSRect);
begin
  inherited;
  if self.item.isSelected then begin
    NSColor.selectedControlColor.set_;
    NSRectFill( dirtyRect );
  end;
end;

{ TCocoaCollectionView }

function TCocoaCollectionView.lclGetCallback: ICommonCallback;
begin
  Result:= callback;
end;

procedure TCocoaCollectionView.lclClearCallback;
begin
  callback:= nil;
end;

procedure TCocoaCollectionView.lclExpectedKeys(var wantTabs, wantKeys,
  wantReturn, wantAllKeys: Boolean);
begin
  wantTabs := false;
  wantKeys := true;
  wantReturn := false; // todo: this should be "true" for editting purposes.
                       //       or false, to let LCL handle editting
  wantAllKeys := false;
end;

procedure TCocoaCollectionView.backend_setCallback(cb: TLCLListViewCallback);
begin
  self.callback:= cb;
end;

procedure TCocoaCollectionView.backend_reloadData;
begin
  self.reloadData;
end;

procedure TCocoaCollectionView.backend_onInit;
begin
  self.styleHandler.onInit;
end;

procedure TCocoaCollectionView.dealloc;
begin
  inherited dealloc;
  FreeAndNil( self.styleHandler );
end;

procedure TCocoaCollectionView.addSubview(aView: NSView);
var
  field: TCocoaTextField;
begin
  if aView.isKindOfClass(TCocoaTextField) then begin
    field:= TCocoaTextField( aView );
    field.setBezeled( False );
    field.fixedBorderStyle:= True;
  end;
  inherited addSubview(aView);
end;

procedure TCocoaCollectionView.updateItemValue(
  indexPath:NSIndexPath; cocoaItem: TCocoaCollectionItem );
var
  isSelected: Boolean;
begin
  if NOT Assigned(self.callback) then
    Exit;

  self.styleHandler.onUpdateItemValue( indexPath, cocoaItem );

  isSelected:= self.callback.getItemStableSelection(indexPath.item);
  cocoaItem.setSelected( isSelected );
end;

procedure TCocoaCollectionView.updateItemSize( baseSize: NSSize );
begin
  self.styleHandler.onUpdateItemSize( baseSize );
  NSCollectionViewFlowLayout(self.collectionViewLayout).setItemSize( self.itemSize );
end;

procedure TCocoaCollectionView.updateItemLayout(item: TCocoaCollectionItem);
begin
  self.styleHandler.onUpdateItemLayout( item );
end;

procedure TCocoaCollectionView.redrawVisibleItems;
var
  item: NSCollectionViewItem;
begin
  for item in realVisibleItems(self) do begin
    item.view.setNeedsDisplay_( True );
  end;
end;

procedure TCocoaCollectionView.restoreFromStableSelection;
begin
  if Assigned(self.callback) then
    self.setSelectionIndexes( self.callback.selectionIndexSet );
end;

procedure TCocoaCollectionView.reloadData;
begin
  if NOT Assigned(self.callback) then
    Exit;

  if NOT TCocoaListView(self.callback.Owner).initializing then begin
    inherited reloadData;
    self.cancelPreviousPerformRequestsWithTarget_selector_object(
      self, ObjcSelector('restoreFromStableSelection'), nil );
    self.performSelector_withObject_afterDelay(
      ObjcSelector('restoreFromStableSelection'), nil, 0 );
  end;
end;

procedure TCocoaCollectionView.selectOneItemByIndex(
  index: Integer; isSelected: Boolean );
var
  indexPath: NSIndexPath;
  indexPaths: NSSet;
begin
  indexPath:= NSIndexPath.indexPathForItem_inSection( index, 0 );
  indexPaths:= NSSet.setWithObject( indexPath );

  if isSelected then begin
    if NOT self.allowsMultipleSelection then begin
      if NOT self.selectionIndexPaths.containsObject(indexPath) then begin
        self.deselectAll( self );
        self.collectionView_didDeselectItemsAtIndexPaths( self, self.selectionIndexPaths );
      end;
    end;
    if NOT self.selectionIndexPaths.containsObject(indexPath) then begin
      self.selectItemsAtIndexPaths_scrollPosition(
        indexPaths, NSCollectionViewScrollPositionNone );
      self.collectionView_didSelectItemsAtIndexPaths( self, indexPaths );
    end;
  end else begin
    if self.selectionIndexPaths.containsObject(indexPath) then begin
      self.deselectItemsAtIndexPaths( indexPaths );
      self.collectionView_didDeselectItemsAtIndexPaths( self, indexPaths );
    end;
  end;
end;

function TCocoaCollectionView.collectionView_numberOfItemsInSection(
  collectionView: NSCollectionView; section: NSInteger): NSInteger;
begin
  Result:= 0;
  if Assigned(self.callback) then
    Result:= self.callback.ItemsCount;
end;

function TCocoaCollectionView.collectionView_itemForRepresentedObjectAtIndexPath
  (collectionView: NSCollectionView; indexPath: NSIndexPath
  ): NSCollectionViewItem;
var
  item: TCocoaCollectionItem Absolute Result;
begin
  Result:= collectionView.makeItemWithIdentifier_forIndexPath(
    NSSTR('Cell'), indexPath );
  self.updateItemValue( indexPath, item );
  self.updateItemLayout( item );
end;

function TCocoaCollectionView.collectionView_shouldChangeItemsAtIndexPaths_toHighlightState
  (collectionView: NSCollectionView; indexPaths: NSSet;
  highlightState: NSCollectionViewItemHighlightState): NSSet;
begin
  Result:= indexPaths;
end;

// don't remove
procedure TCocoaCollectionView.collectionView_didChangeItemsAtIndexPaths_toHighlightState
  (collectionView: NSCollectionView; indexPaths: NSSet;
  highlightState: NSCollectionViewItemHighlightState);
begin
end;

// don't remove
procedure TCocoaCollectionView.collectionView_willDisplayItem_forRepresentedObjectAtIndexPath
  (collectionView: NSCollectionView; item: NSCollectionViewItem;
  indexPath: NSIndexPath);
begin
end;

// don't remove
procedure TCocoaCollectionView.collectionView_didEndDisplayingItem_forRepresentedObjectAtIndexPath
  (collectionView: NSCollectionView; item: NSCollectionViewItem;
  indexPath: NSIndexPath);
begin
end;

// don't remove
function TCocoaCollectionView.collectionView_shouldSelectItemsAtIndexPaths(
  collectionView: NSCollectionView; indexPaths: NSSet): NSSet;
begin
  Result:= indexPaths;
end;

procedure TCocoaCollectionView.collectionView_didSelectItemsAtIndexPaths(
  collectionView: NSCollectionView; indexPaths: NSSet);
var
  indexPath: NSIndexPath;
  item: TCocoaCollectionItem;
begin
  for indexPath in indexPaths do begin
    item:= TCocoaCollectionItem( self.itemAtIndexPath(indexPath) );
    if Assigned(item) then begin
      item.setSelected( True );
      item.textField.setToolTip( item.textField.stringValue );
      item.view.setNeedsDisplay_(True);
    end;
    if Assigned(self.callback) then
      self.callback.selectOne( indexPath.item, True );
  end;
end;

function TCocoaCollectionView.collectionView_shouldDeselectItemsAtIndexPaths(
  collectionView: NSCollectionView; indexPaths: NSSet): NSSet;
begin
  Result:= indexPaths;
end;

procedure TCocoaCollectionView.collectionView_didDeselectItemsAtIndexPaths(
  collectionView: NSCollectionView; indexPaths: NSSet);
var
  indexPath: NSIndexPath;
  item: TCocoaCollectionItem;
begin
  for indexPath in indexPaths do begin
    item:= TCocoaCollectionItem( self.itemAtIndexPath(indexPath) );
    if Assigned(item) then begin
      item.setSelected( False );
      item.textField.setToolTip( nil );
      item.view.setNeedsDisplay_(True);
    end;
    if Assigned(self.callback) then
      self.callback.selectOne( indexPath.item, False );
  end;
end;

procedure TCocoaCollectionView.mouseDown(theEvent: NSEvent);
begin
  inherited mouseDown(theEvent);
end;

{ TCocoaWSListView_CollectionViewHandler }

constructor TCocoaWSListView_CollectionViewHandler.Create(
   listView: TCocoaListView );
begin
  _listView:= listView;
  _collectionView:= TCocoaCollectionView(listView.documentView);
end;

function TCocoaWSListView_CollectionViewHandler.getCallback: TLCLListViewCallback;
begin
  Result:= _collectionView.callback;
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnDelete(
  const AIndex: Integer);
begin
end;

function TCocoaWSListView_CollectionViewHandler.ColumnGetWidth(
  const AIndex: Integer; const AColumn: TListColumn): Integer;
begin
  Result:= -1;
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnInsert(
  const AIndex: Integer; const AColumn: TListColumn);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnMove(const AOldIndex,
  ANewIndex: Integer; const AColumn: TListColumn);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetAlignment(
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetAutoSize(
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetCaption(
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetMaxWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetMinWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetVisible(
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetSortIndicator(
  const AIndex: Integer; const AColumn: TListColumn;
  const ASortIndicator: TSortIndicator);
begin
end;

// when LCL call ItemDelete, the Item isn't Deleted at LCL
// delayed reload is necessary
procedure TCocoaWSListView_CollectionViewHandler.ItemDelete(
  const AIndex: Integer);
begin
  Application.QueueAsyncCall( @doReloadDataAfterDelete, AIndex );
end;

procedure TCocoaWSListView_CollectionViewHandler.doReloadDataAfterDelete( AIndex: PtrInt );
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1 );
  _collectionView.reloadData;
end;

function TCocoaWSListView_CollectionViewHandler.ItemDisplayRect(const AIndex,
  ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  item: NSCollectionViewItem;
  frame: NSRect;
begin
  Result:= Bounds(0,0,0,0);
  item:= _collectionView.itemAtIndex( AIndex );
  if NOT Assigned(item) then
    Exit;

  case ACode of
    drLabel:
      begin
        frame:= item.textField.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
        _collectionView.styleHandler.onAdjustTextEditorRect( frame );
      end;
    drIcon:
      begin
        frame:= item.imageView.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
      end
    else
      frame:= item.view.frame;
  end;

  Result:= NSRectToRect( frame );
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetChecked(
  const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result:= False;
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetPosition(
  const AIndex: Integer): TPoint;
var
  rect: TRect;
begin
  rect:= self.ItemDisplayRect( AIndex, 0, drBounds );
  Result:= rect.TopLeft;
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  lclcb : TLCLListViewCallback;
begin
  Result:= false;
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisSelected: begin
      Result:= (AIndex>=0) and (AIndex < _collectionView.numberOfItemsInSection(0));
      AIsSet:= lclcb.getItemStableSelection( AIndex );
    end;
  end;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemInsert(
  const AIndex: Integer; const AItem: TListItem);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if TCocoaListView(lclcb.Owner).initializing then
    Exit;

  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetChecked(
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetImage(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisSelected: begin
      if lclcb.getItemStableSelection(AIndex) <> AIsSet then begin
        _collectionView.selectOneItemByIndex( AIndex, AIsSet );
        _collectionView.redrawVisibleItems;
      end;
    end;
  end;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetText(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemShow(
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  indexPaths: NSSet;
begin
  indexPaths:= CocoaCollectionView.indexPathsWithOneIndex( _collectionView, AIndex );
  _collectionView.scrollToItemsAtIndexPaths_scrollPosition(
    indexPaths, NSCollectionViewScrollPositionTop );
end;

// what is the function?
// never be called ???
function TCocoaWSListView_CollectionViewHandler.GetFocused: Integer;
begin
  Result:= self.GetSelection;
end;

function TCocoaWSListView_CollectionViewHandler.GetItemAt(x, y: integer
  ): Integer;
var
  cocoaPoint: NSPoint;
  indexPath: NSIndexPath;
begin
  Result:= -1;
  cocoaPoint.x:= x;
  cocoaPoint.y:= y;
  indexPath:= _collectionView.indexPathForItemAtPoint( cocoaPoint );
  if Assigned(indexPath) then
    Result:= indexPath.item;
end;

function TCocoaWSListView_CollectionViewHandler.GetSelCount: Integer;
begin
  Result:= _collectionView.selectionIndexPaths.count;
end;

function TCocoaWSListView_CollectionViewHandler.GetSelection: Integer;
var
  lclListView: TCustomListView;
  lclItem: TListItem;
begin
  Result:= -1;
  lclListView:= TCustomListView(_collectionView.lclGetTarget);
  if Assigned(lclListView) then begin
    lclItem:= lclListView.LastSelected;
    if Assigned(lclItem) then
      Result:= lclItem.Index;
  end;
end;

function TCocoaWSListView_CollectionViewHandler.GetTopItem: Integer;
var
  items: NSArray;
  item: NSCollectionViewItem;
begin
  Result:= -1;
  items:= CocoaCollectionView.realVisibleItems( _collectionView );
  if items.count > 0 then begin
    item:= NSCollectionViewItem(items.firstObject);
    Result:= _collectionView.indexPathForItem(item).item;
  end;
end;

function TCocoaWSListView_CollectionViewHandler.GetVisibleRowCount: Integer;
begin
  Result:= CocoaCollectionView.realVisibleItems(_collectionView).count;
end;

procedure TCocoaWSListView_CollectionViewHandler.SelectAll(const AIsSet: Boolean
  );
begin
  if AIsSet then
    _collectionView.selectAll( _collectionView )
  else
    _collectionView.deselectAll( _collectionView );
end;

procedure TCocoaWSListView_CollectionViewHandler.SetDefaultItemHeight(
  const AValue: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.SetImageList(
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  lclcb: TLCLListViewCallback;
  lvil: TListViewImageList;
  iconSize: NSSize;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if NOT lclcb.GetImageListType(lvil) then
    Exit;

  if AList <> lvil then
    Exit;

  iconSize.Width:= AValue.Width;
  iconSize.Height:= AValue.Height;
  _collectionView.updateItemSize( iconSize );
end;

procedure TCocoaWSListView_CollectionViewHandler.SetItemsCount(
  const Avalue: Integer);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.SetProperty(
  const AProp: TListViewProperty; const AIsSet: Boolean);
begin
  case AProp of
    {lvpHideSelection,
    lvpHotTrack,}
    lvpMultiSelect: _collectionView.setAllowsMultipleSelection(AIsSet);
    {lvpOwnerDraw,
    lvpReadOnly:
    lvpShowWorkAreas,
    lvpWrapText,
    lvpToolTips}
  end;
end;

// scrollBars auto handled by NSCollectionView
procedure TCocoaWSListView_CollectionViewHandler.SetScrollBars(
  const AValue: TScrollStyle);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.SetSort(
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if TCocoaListView(lclcb.Owner).initializing then
    Exit;

  lclcb.selectionIndexSet.removeAllIndexes;
  _collectionView.reloadData();
end;

end.
