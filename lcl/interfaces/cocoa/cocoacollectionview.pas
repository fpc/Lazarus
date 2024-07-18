unit CocoaCollectionView;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls,
  MacOSAll, CocoaAll, CocoaPrivate, Cocoa_Extra, CocoaUtils,
  CocoaWSComCtrls, CocoaTextEdits;

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
  end;

function AllocCocoaCollectionView( style: TViewStyle ): TCocoaCollectionView;
function indexPathsWithOneIndex( cv: NSCollectionView; AIndex: Integer ): NSSet;
function realVisibleItems( cv: NSCollectionView ): NSArray;

implementation

type
  
  { TCocoaListView_CollectionView_LargeIconHandler }
  TCocoaListView_CollectionView_LargeIconHandler = class(TCocoaListView_CollectionView_StyleHandler)
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
      styleHandler:= TCocoaListView_CollectionView_LargeIconHandler.Create( Result );
    vsList:
      styleHandler:= TCocoaListView_CollectionView_LargeIconHandler.Create( Result );
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
  aFrame.origin.x:= aFrame.origin.x + 1;
  aFrame.size.width:= aFrame.size.width - 2;
  aFrame.size.height:= aFrame.size.height + 8;
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

end.
