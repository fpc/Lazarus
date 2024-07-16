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

  { TCocoaCollectionItem }
  TCocoaCollectionItem = objcclass(NSCollectionViewItem)
  public
    procedure loadView; override;
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
    iconSize: NSSize;
    itemSize: NSSize;
  public
    callback: TLCLListViewCallback;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure lclExpectedKeys(var wantTabs, wantKeys, wantReturn, wantAllKeys: Boolean); override;

    procedure backend_setCallback( cb: TLCLListViewCallback );
    procedure backend_reloadData;
    procedure backend_onInit( lclListView: TCustomListView );
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

function AllocCocoaCollectionView: TCocoaCollectionView;
function indexPathsWithOneIndex( cv: NSCollectionView; AIndex: Integer ): NSSet;
function realVisibleItems( cv: NSCollectionView ): NSArray;

implementation

function AllocCocoaCollectionView: TCocoaCollectionView;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout.new;

  minSize.width:= 64;
  minSize.height:= 68;
  layout.setItemSize( minSize );

  layout.setMinimumInteritemSpacing( 4 );
  layout.setMinimumLineSpacing( 4 );

  Result:= TCocoaCollectionView.new;
  Result.setDataSource( Result );
  Result.setDelegate( NSCollectionViewDelegateProtocol_1011(Result) );
  Result.setCollectionViewLayout( layout );
  Result.registerClass_forItemWithIdentifier( TCocoaCollectionItem, NSSTR('Cell') );
  Result.setSelectable( True );
  layout.release;
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
  Result:= NSMutableArray.new;
  visibleRect:= cv.visibleRect;
  for item in cv.visibleItems do begin
    if NSIntersectsRect( item.view.frame, visibleRect ) then
      items.addObject( item );
  end;
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
  fieldControl.setAlignment( NSTextAlignmentCenter );
  fieldControl.setBordered( False );
  fieldControl.setDrawsBackground( False );
  fieldControl.setEditable( False );
  fieldControl.setLineBreakMode( NSLineBreakByTruncatingTail );
  self.setTextField( fieldControl );
  itemView.addSubview( fieldControl );

  self.setView( itemView );
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

procedure TCocoaCollectionView.backend_onInit( lclListView: TCustomListView );
begin
end;

procedure TCocoaCollectionView.updateItemValue(
  indexPath:NSIndexPath; cocoaItem: TCocoaCollectionItem );
var
  lclListView: TCustomListView;
  lclItem: TListItem;
  cocoaImage: NSImage;
  lclText: String;
  isSelected: Boolean;
begin
  lclListView:= TCustomListView( self.lclGetTarget );
  if NOT Assigned(lclListView) then
    Exit;
  if NOT Assigned(self.callback) then
    Exit;

  lclItem:= lclListView.Items[indexPath.item];
  cocoaImage:= self.callback.GetImageFromIndex( lclItem.ImageIndex );
  cocoaItem.imageView.setImage( cocoaImage );

  self.callback.GetItemTextAt( indexPath.item, 0, lclText );
  cocoaItem.textField.setStringValue( StrToNSString(lclText) );

  isSelected:= self.callback.getItemStableSelection(indexPath.item);
  cocoaItem.setSelected( isSelected );
end;

procedure TCocoaCollectionView.updateItemSize( baseSize: NSSize );
begin
  self.iconSize:= baseSize;
  if self.iconSize.Width < 32 then
    self.iconSize.Width:= 32;
  if self.iconSize.Height < 32 then
    self.iconSize.Height:= 32;

  self.itemSize.Width:= 10 + baseSize.Width + 10;
  self.itemSize.Height:= 10 + baseSize.Height + 2 + 14 + 10;
  if self.itemSize.Width < 64 then
    self.itemSize.Width:= 64;
  if self.itemSize.Height < 68 then
    self.itemSize.Height:= 68;

  NSCollectionViewFlowLayout(self.collectionViewLayout).setItemSize( self.itemSize );
end;

procedure TCocoaCollectionView.updateItemLayout(item: TCocoaCollectionItem);
var
  aFrame: NSRect;
  newView: NSView;
  fieldControl: NSTextField;
  imageControl: NSImageView;
begin
  aFrame.origin.x:= (self.itemSize.Width - self.iconSize.Width) / 2;
  aFrame.origin.y:= self.itemSize.Height - self.iconSize.Height - 10;
  aFrame.size:= self.iconSize;
  item.imageView.setFrame( aFrame );

  aFrame.origin.x:= 0;
  aFrame.origin.y:= 9;
  aFrame.size.width:= self.itemSize.Width;
  aFrame.size.height:= 15;
  item.textField.setFrame( aFrame );
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
