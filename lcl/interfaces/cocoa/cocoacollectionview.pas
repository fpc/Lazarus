unit CocoaCollectionView;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaPrivate, Cocoa_Extra, CocoaCallback, CocoaConfig, CocoaConst, CocoaUtils,
  CocoaListView, CocoaTextEdits,
  LCLType, Controls, ComCtrls, StdCtrls, ImgList, Forms;

type

  { TCocoaCollectionItem }
  TCocoaCollectionItem = objcclass(NSCollectionViewItem)
  private
    _checkBox: NSButton;
  private
    procedure checkboxAction(sender: NSButton); message 'checkboxAction:';
  public
    procedure loadView; override;
    function checkBox: NSButton; message 'checkBox';
    procedure createCheckBox; message 'createCheckBox';
    procedure prepareForReuse; message 'prepareForReuse';
    procedure dealloc; override;
  end;

  { TCocoaListView_CollectionView_StyleHandler }
  TCocoaListView_CollectionView_StyleHandler = class
  private
    _collectionView: NSCollectionView;
  public
    constructor Create( collectionView: NSCollectionView ); virtual;
    function hasCheckBoxes: Boolean;
  public
    procedure resetSize; virtual; abstract;
    procedure onInit; virtual;
    function isSupportIcon: Boolean; virtual;
    procedure onUpdateItemSize( baseSize: NSSize ); virtual; abstract;
    procedure onUpdateItemLayout( cocoaItem: TCocoaCollectionItem ); virtual; abstract;
    procedure onAdjustTextEditor( lv: TCocoaListView; var aFrame: NSRect ); virtual; abstract;
  end;

  { TCocoaCollectionItemView }
  TCocoaCollectionItemView = objcclass(NSView)
  private
    item: TCocoaCollectionItem;
    trackingArea: NSTrackingArea;
  public
    procedure updateTrackingAreas; override;
    procedure mouseEntered(theEvent: NSEvent); override;
    procedure mouseExited(theEvent: NSEvent); override;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
  end;

  { TCocoaCollectionView }
  TCocoaCollectionView = objcclass(
    NSCollectionView,
    NSCollectionViewDataSourceProtocol,
    NSCollectionViewDelegateProtocol_1011,
    TCocoaListViewBackendControlProtocol )
  private
    _checkBoxes: Boolean;
  public
    styleHandler: TCocoaListView_CollectionView_StyleHandler;
    iconSize: NSSize;
    itemSize: NSSize;

    procedure lclSetCheckBoxes( checkBoxes: Boolean); message 'lclSetCheckBoxes:';
    function lclHasCheckBoxes: Boolean; message 'lclHasCheckBoxes';
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

    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
  end;

  { TCocoaWSListView_CollectionViewHandler }

  TCocoaWSListView_CollectionViewHandler = class(TCocoaWSListViewHandler)
  private
    _listView: TCocoaListView;
    _collectionView: TCocoaCollectionView;
  private
    function getCallback: TLCLListViewCallback;
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
    procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
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
  { TCocoaListView_CollectionView_LargeIconHandler }
  TCocoaListView_CollectionView_LargeIconHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure resetSize; override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout( cocoaItem: TCocoaCollectionItem ); override;
    procedure onAdjustTextEditor( lv: TCocoaListView; var aFrame: NSRect ); override;
  end;

  { TCocoaListView_CollectionView_SmallIconHandler }
  TCocoaListView_CollectionView_SmallIconHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure resetSize; override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout( cocoaItem: TCocoaCollectionItem ); override;
    procedure onAdjustTextEditor( lv: TCocoaListView; var aFrame: NSRect ); override;
  end;

  { TCocoaListView_CollectionView_ListHandler }
  TCocoaListView_CollectionView_ListHandler = class(TCocoaListView_CollectionView_StyleHandler)
    procedure resetSize; override;
    function isSupportIcon: Boolean; override;
    procedure onUpdateItemSize( baseSize: NSSize ); override;
    procedure onUpdateItemLayout( cocoaItem: TCocoaCollectionItem ); override;
    procedure onAdjustTextEditor( lv: TCocoaListView; var aFrame: NSRect ); override;
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

function TCocoaListView_CollectionView_StyleHandler.hasCheckBoxes: Boolean;
begin
  Result:= TCocoaCollectionView(_collectionView).lclHasCheckBoxes;
end;

procedure TCocoaListView_CollectionView_StyleHandler.onInit;
begin
  self.resetSize;
end;

function TCocoaListView_CollectionView_StyleHandler.isSupportIcon: Boolean;
begin
  Result:= True;
end;

{ TCocoaListView_CollectionView_LargeIconHandler }

procedure TCocoaListView_CollectionView_LargeIconHandler.resetSize;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= CocoaConfigListView.vsIcon.item.minSize.width;
  minSize.height:= CocoaConfigListView.vsIcon.item.minSize.height;

  if self.hasCheckBoxes then
    minSize.width:= minSize.width +
                    CocoaConfigListView.vsIcon.item.checkBoxOccupiedWidth;

  TCocoaCollectionView(_collectionView).itemSize:= minSize;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( CocoaConfigListView.vsIcon.interitemSpacing );
  layout.setMinimumLineSpacing( CocoaConfigListView.vsIcon.lineSpacing );
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onUpdateItemSize(
  baseSize: NSSize);
var
  cv: TCocoaCollectionView;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.iconSize:= baseSize;
  if cv.iconSize.Width < CocoaConfigListView.vsIcon.imageView.minSize.width then
    cv.iconSize.Width:= CocoaConfigListView.vsIcon.imageView.minSize.width;
  if cv.iconSize.Height < CocoaConfigListView.vsIcon.imageView.minSize.height then
    cv.iconSize.Height:= CocoaConfigListView.vsIcon.imageView.minSize.height;

  cv.itemSize.Width:= cv.iconSize.Width +
                      CocoaConfigListView.vsIcon.imageView.padding * 2;
  cv.itemSize.Height:= cv.iconSize.Height +
                       CocoaConfigListView.vsIcon.imageView.padding * 2 +
                       CocoaConfigListView.vsIcon.item.controlSpacing +
                       CocoaConfigListView.vsIcon.textField.defaultHeight;
  if cv.itemSize.Width < CocoaConfigListView.vsIcon.item.minSize.width then
    cv.itemSize.Width:= CocoaConfigListView.vsIcon.item.minSize.width;
  if cv.itemSize.Height < CocoaConfigListView.vsIcon.item.minSize.height then
    cv.itemSize.Height:= CocoaConfigListView.vsIcon.item.minSize.height;

  if self.hasCheckBoxes then
    cv.itemSize.Width:= cv.itemSize.Width +
                        CocoaConfigListView.vsIcon.item.checkBoxOccupiedWidth;
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onUpdateItemLayout(
  cocoaItem: TCocoaCollectionItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
  textField: NSTextField;
  imageView: NSImageView;
  checkBox: NSButton;
begin
  textField:= cocoaItem.textField;
  imageView:= cocoaItem.imageView;
  checkBox:= cocoaItem.checkBox;
  cv:= TCocoaCollectionView(_collectionView);

  aFrame.origin.x:= round( (cv.itemSize.Width - cv.iconSize.Width) / 2 );
  aFrame.origin.y:= cv.itemSize.Height - cv.iconSize.Height -
                    CocoaConfigListView.vsIcon.imageView.padding;
  aFrame.size:= cv.iconSize;
  if Assigned(checkBox) then
    aFrame.origin.x:= aFrame.origin.x +
                      CocoaConfigListView.vsIcon.item.checkBoxOccupiedWidth / 2;
  imageView.setFrame( aFrame );

  textField.sizeToFit;
  aFrame.size.height:= textField.frame.size.height;
  aFrame.origin.y:= round( (aFrame.origin.y-aFrame.size.height) / 2 );
  aFrame.origin.x:= 0;
  if Assigned(checkBox) then
    aFrame.origin.x:= aFrame.origin.x + CocoaConfigListView.vsIcon.item.checkBoxOccupiedWidth;
  aFrame.size.width:= cv.itemSize.Width - aFrame.origin.x;
  textField.setFrame( aFrame );
  textField.setAlignment( CocoaConfigListView.vsIcon.item.textFieldAlignment );

  if Assigned(checkBox) then begin
    checkBox.sizeToFit;
    aFrame.size:= checkBox.frame.size;
    aFrame.origin.x:= CocoaConfigListView.vsIcon.item.checkBoxOccupiedWidth -
                      checkBox.frame.size.width;
    aFrame.origin.y:= round( (cv.itemSize.Height-aFrame.size.height) / 2 );
    checkBox.setFrame( aFrame );
  end;
end;

procedure TCocoaListView_CollectionView_LargeIconHandler.onAdjustTextEditor(
  lv: TCocoaListView; var aFrame: NSRect );
var
  offset: Double;   // FocusRing Occupied
begin
  lv.setCaptionAlignment( CocoaConfigListView.vsIcon.item.textFieldAlignment );

  offset:= 1;
  if NOT hasCheckBoxes then
    offset:= offset + 1;
  aFrame.origin.x:= aFrame.origin.x + offset;
  aFrame.size.Width:= aFrame.size.Width - offset*2;
end;

{ TCocoaListView_CollectionView_SmallIconHandler }

procedure TCocoaListView_CollectionView_SmallIconHandler.resetSize;
var
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= CocoaConfigListView.vsSmallIcon.item.minSize.width;
  minSize.height:= CocoaConfigListView.vsSmallIcon.item.minSize.height;
  if self.hasCheckBoxes then
    minSize.width:= minSize.width + CocoaConfigListView.vsSmallIcon.item.checkBoxOccupiedWidth;

  TCocoaCollectionView(_collectionView).itemSize:= minSize;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( CocoaConfigListView.vsSmallIcon.interitemSpacing );
  layout.setMinimumLineSpacing( CocoaConfigListView.vsSmallIcon.lineSpacing );
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onUpdateItemSize(
  baseSize: NSSize);
var
  cv: TCocoaCollectionView;
  textWidth: CGFloat;
begin
  cv:= TCocoaCollectionView(_collectionView);
  cv.iconSize:= baseSize;
  if cv.iconSize.Width < CocoaConfigListView.vsSmallIcon.imageView.minSize.width then
    cv.iconSize.Width:= CocoaConfigListView.vsSmallIcon.imageView.minSize.width;
  if cv.iconSize.Height < CocoaConfigListView.vsSmallIcon.imageView.minSize.height then
    cv.iconSize.Height:= CocoaConfigListView.vsSmallIcon.imageView.minSize.height;

  textWidth:= Round( cv.iconSize.Width *
                     CocoaConfigListView.vsSmallIcon.textField.iconWidthFactor +
                     CocoaConfigListView.vsSmallIcon.textField.baseWidth );
  if textWidth < CocoaConfigListView.vsSmallIcon.textField.minWidth then
    textWidth:= CocoaConfigListView.vsSmallIcon.textField.minWidth;
  cv.itemSize.Width:= cv.iconSize.Width +
                      CocoaConfigListView.vsSmallIcon.item.controlSpacing +
                      textWidth +
                      CocoaConfigListView.vsSmallIcon.imageView.padding * 2;
  cv.itemSize.Height:= cv.iconSize.Height +
                       CocoaConfigListView.vsSmallIcon.imageView.padding * 2;
  if cv.itemSize.Height < CocoaConfigListView.vsSmallIcon.item.minSize.height then
    cv.itemSize.Height:= CocoaConfigListView.vsSmallIcon.item.minSize.height;

  if self.hasCheckBoxes then
    cv.itemSize.width:= cv.itemSize.width +
                        CocoaConfigListView.vsSmallIcon.item.checkBoxOccupiedWidth;
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onUpdateItemLayout(
  cocoaItem: TCocoaCollectionItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
  textField: NSTextField;
  imageView: NSImageView;
  checkBox: NSButton;
begin
  textField:= cocoaItem.textField;
  imageView:= cocoaItem.imageView;
  checkBox:= cocoaItem.checkBox;
  cv:= TCocoaCollectionView(_collectionView);

  aFrame:= NSZeroRect;

  if Assigned(checkBox) then begin
    checkBox.sizeToFit;
    aFrame.size:= checkBox.frame.size;
    aFrame.origin.x:= CocoaConfigListView.vsSmallIcon.item.checkBoxOccupiedWidth -
                      checkBox.frame.size.width;
    aFrame.origin.y:= round( (cv.itemSize.Height - aFrame.size.height ) / 2 );
    checkBox.setFrame( aFrame );
  end;

  aFrame.origin.x:= aFrame.origin.x + aFrame.size.width +
                    CocoaConfigListView.vsSmallIcon.imageView.padding +
                    CocoaConfigListView.vsSmallIcon.item.controlSpacing;
  aFrame.origin.y:= round( (cv.itemSize.Height - cv.iconSize.height) / 2 );
  aFrame.size:= cv.iconSize;
  imageView.setFrame( aFrame );

  textField.sizeToFit;
  aFrame.origin.x:= aFrame.origin.x + aFrame.size.width +
                    CocoaConfigListView.vsSmallIcon.item.controlSpacing;
  aFrame.origin.y:= round( (cv.itemSize.Height - textField.frame.size.height) / 2 );
  aFrame.size.width:= cv.itemSize.Width - aFrame.origin.x -
                      CocoaConfigListView.vsSmallIcon.item.controlSpacing;
  aFrame.size.height:= textField.frame.size.height;
  textField.setFrame( aFrame );
  textField.setAlignment( CocoaConfigListView.vsSmallIcon.item.textFieldAlignment );
end;

procedure TCocoaListView_CollectionView_SmallIconHandler.onAdjustTextEditor(
  lv: TCocoaListView; var aFrame: NSRect );
begin
  lv.setCaptionAlignment( CocoaConfigListView.vsSmallIcon.item.textFieldAlignment );
end;

{ TCocoaListView_CollectionView_ListHandler }

procedure TCocoaListView_CollectionView_ListHandler.resetSize;
var
  cv: TCocoaCollectionView;
  layout: NSCollectionViewFlowLayout;
  minSize: NSSize;
begin
  layout:= NSCollectionViewFlowLayout(_collectionView.collectionViewLayout);
  minSize.width:= CocoaConfigListView.vsList.item.minSize.width;
  minSize.height:= CocoaConfigListView.vsList.item.minSize.height;
  if self.hasCheckBoxes then
    minSize.width:= minSize.width + CocoaConfigListView.vsList.item.checkBoxOccupiedWidth;

  TCocoaCollectionView(_collectionView).itemSize:= minSize;
  layout.setItemSize( minSize );
  layout.setMinimumInteritemSpacing( CocoaConfigListView.vsList.interitemSpacing );
  layout.setMinimumLineSpacing( CocoaConfigListView.vsList.lineSpacing );

  layout.setScrollDirection( NSCollectionViewScrollDirectionHorizontal );
end;

function TCocoaListView_CollectionView_ListHandler.isSupportIcon: Boolean;
begin
  Result:= False;
end;

procedure TCocoaListView_CollectionView_ListHandler.onUpdateItemSize(
  baseSize: NSSize);
begin
end;

procedure TCocoaListView_CollectionView_ListHandler.onUpdateItemLayout(
  cocoaItem: TCocoaCollectionItem);
var
  cv: TCocoaCollectionView;
  aFrame: NSRect;
  textField: NSTextField;
  checkBox: NSButton;
begin
  textField:= cocoaItem.textField;
  checkBox:= cocoaItem.checkBox;
  cv:= TCocoaCollectionView(_collectionView);

  aFrame:= NSZeroRect;

  if Assigned(checkBox) then begin
    checkBox.sizeToFit;
    aFrame.size:= checkBox.frame.size;
    aFrame.origin.x:= CocoaConfigListView.vsList.item.checkBoxOccupiedWidth -
                      checkBox.frame.size.width;
    aFrame.origin.y:= round( (cv.itemSize.Height - aFrame.size.height ) / 2 );
    checkBox.setFrame( aFrame );
  end;

  textField.sizeToFit;
  aFrame.origin.x:= aFrame.origin.x + aFrame.size.width +
                    CocoaConfigListView.vsList.item.controlSpacing;
  aFrame.origin.y:= round( (cv.itemSize.Height - textField.frame.size.height) / 2 );
  aFrame.size.width:= cv.itemSize.Width - aFrame.origin.x -
                      CocoaConfigListView.vsList.item.controlSpacing;
  aFrame.size.height:= textField.frame.size.height;
  textField.setFrame( aFrame );
  textField.setAlignment( CocoaConfigListView.vsList.item.textFieldAlignment );
end;

procedure TCocoaListView_CollectionView_ListHandler.onAdjustTextEditor(
  lv: TCocoaListView; var aFrame: NSRect );
begin
  lv.setCaptionAlignment( CocoaConfigListView.vsList.item.textFieldAlignment );
end;

{ TCocoaCollectionItem }

procedure TCocoaCollectionItem.createCheckBox;
begin
  if NOT Assigned(_checkBox) then begin
    _checkBox:= NSButton.alloc.init;
    _checkBox.setHidden( True );
    _checkBox.setButtonType( NSSwitchButton );
    _checkBox.setTitle( CocoaConst.NSSTR_EMPTY );
    _checkBox.setTarget( self );
    _checkBox.setAction( ObjCSelector('checkboxAction:') );
    self.View.addSubview( _checkBox );
  end;
end;

procedure TCocoaCollectionItem.checkboxAction(sender: NSButton);
var
  row: Integer;
  cv: TCocoaCollectionView;
  indexPath: NSIndexPath;
begin
  cv:= TCocoaCollectionView( self.collectionView );
  indexPath:= cv.indexPathForItem( self );
  row:= indexPath.item;
  cv.callback.SetItemCheckedAt( row, sender.state );
  if sender.state = NSOnState then begin
    cv.selectOneItemByIndex( row, True );
    self.view.window.makeFirstResponder( self.collectionView );
  end;
end;

procedure TCocoaCollectionItem.loadView;
var
  itemView: TCocoaCollectionItemView;
  fieldControl: NSTextField;
  imageControl: NSImageView;
  cell: NSTextFieldCell;
begin
  itemView:= TCocoaCollectionItemView.alloc.initWithFrame( NSZeroRect);
  itemView.item:= self;

  imageControl:= NSImageView.alloc.initWithFrame( NSZeroRect );
  imageControl.setImageScaling( NSImageScaleProportionallyUpOrDown );
  self.setImageView( imageControl );
  itemView.addSubview( imageControl );

  fieldControl:= NSTextField.alloc.initWithFrame(NSZeroRect);
  fieldControl.setBordered( False );
  fieldControl.setDrawsBackground( False );
  fieldControl.setEditable( False );
  fieldControl.setLineBreakMode( NSLineBreakByTruncatingTail );
  fieldControl.setBezeled( False );
  cell := NSTextFieldCell(fieldControl.cell);
  cell.setWraps(false);
  cell.setScrollable(false);
  self.setTextField( fieldControl );
  itemView.addSubview( fieldControl );

  self.setView( itemView );
end;

function TCocoaCollectionItem.checkBox: NSButton;
begin
  Result:= _checkBox;
end;

procedure TCocoaCollectionItem.prepareForReuse;
begin
  if Assigned(_checkBox) then begin
    _checkBox.removeFromSuperview;
    _checkBox.release;
    _checkBox:= nil;
  end;

  self.imageView.setImage( nil );
  self.view.removeFromSuperview;
end;

procedure TCocoaCollectionItem.dealloc;
begin
  self.imageView.removeFromSuperview;
  self.imageView.release;

  self.textField.removeFromSuperview;
  self.textField.release;

  if Assigned(_checkBox) then begin
    _checkBox.removeFromSuperview;
    _checkBox.release;
  end;

  self.view.removeFromSuperview;
  self.view.release;
  inherited dealloc;
end;

{ TCocoaCollectionItemView }

procedure TCocoaCollectionItemView.updateTrackingAreas;
const
  options: NSTrackingAreaOptions = NSTrackingMouseEnteredAndExited
                                or NSTrackingActiveAlways;
begin
  if Assigned(self.trackingArea) then begin
    removeTrackingArea(self.trackingArea);
    self.trackingArea.release;
  end;

  self.trackingArea:= NSTrackingArea.alloc.initWithRect_options_owner_userInfo(
    self.bounds,
    options,
    self,
    nil );
  self.addTrackingArea( self.trackingArea );
end;

procedure TCocoaCollectionItemView.mouseEntered(theEvent: NSEvent);
begin
  if Assigned(self.item.checkBox) then
    self.item.checkBox.setHidden( False );
end;

procedure TCocoaCollectionItemView.mouseExited(theEvent: NSEvent);
var
  checkBox: NSButton;
begin
  checkBox:= self.item.checkBox;
  if NOT Assigned(checkBox) then
    Exit;
  if checkBox.state = NSOnState then
    Exit;
  if self.item.isSelected then
    Exit;
  checkBox.setHidden( True );
end;

procedure TCocoaCollectionItemView.drawRect(dirtyRect: NSRect);
begin
  inherited;
  if self.item.isSelected then begin
    NSColor.selectedControlColor.set_;
    NSRectFill( dirtyRect );
  end;
end;

procedure TCocoaCollectionItemView.dealloc;
begin
  if Assigned(self.trackingArea) then begin
    removeTrackingArea(self.trackingArea);
    self.trackingArea.release;
  end;
end;

{ TCocoaCollectionView }

procedure TCocoaCollectionView.lclSetCheckBoxes(checkBoxes: Boolean);
begin
  if _checkBoxes = checkBoxes then
    Exit;

  _checkBoxes:= checkBoxes;
  self.styleHandler.resetSize;
  self.updateItemSize( self.iconSize );
  self.reloadData;
end;

function TCocoaCollectionView.lclHasCheckBoxes: Boolean;
begin
  Result:= _checkBoxes;
end;

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
  textField: TCocoaTextField Absolute aView;
begin
  inherited;
  if NOT aView.isKindOfClass(TCocoaTextField) then
    Exit;
  if NOT Assigned(self.callback) then
    Exit;
  TCocoaListView(self.callback.Owner).setCaptionEditor( textField );
end;

procedure TCocoaCollectionView.updateItemValue(
  indexPath:NSIndexPath; cocoaItem: TCocoaCollectionItem );
var
  row: Integer;
  checkBox: NSButton;
  checkedValue: Integer;
  cocoaImage: NSImage;
  lclImageIndex: Integer;
  lclText: String;
  isSelected: Boolean;
begin
  if NOT Assigned(self.callback) then
    Exit;

  row:= indexPath.item;

  isSelected:= self.callback.getItemStableSelection( row );
  cocoaItem.setSelected( isSelected );

  if _checkBoxes then
    cocoaItem.createCheckBox;
  checkBox:= cocoaItem.checkBox;
  if Assigned(checkBox) then begin
    self.callback.GetItemCheckedAt( row, checkedValue{%H-} );
    checkBox.setState( checkedValue );
    checkBox.setHidden( NOT ((checkedValue=NSOnState) or isSelected) );
  end;

  if self.styleHandler.isSupportIcon then begin
    self.callback.GetItemImageAt( row, 0, lclImageIndex{%H-} );
    cocoaImage:= self.callback.GetImageFromIndex( lclImageIndex );
    cocoaItem.imageView.setImage( cocoaImage );
  end;

  self.callback.GetItemTextAt( row, 0, lclText{%H-} );
  cocoaItem.textField.setStringValue( StrToNSString(lclText) );
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
      if Assigned(item.checkBox) then
        item.checkBox.setHidden( False );
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
      if Assigned(item.checkBox) then
        item.checkBox.setHidden( item.checkBox.state<>NSOnState );
      item.view.setNeedsDisplay_(True);
    end;
    if Assigned(self.callback) then
      self.callback.selectOne( indexPath.item, False );
  end;
end;

procedure TCocoaCollectionView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaCollectionView.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaCollectionView.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaCollectionView.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
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

  frame:= item.view.frame;
  case ACode of
    drLabel:
      begin
        _listView.setCaptionFont( item.textField.font );
        // to do: completely restore TFont
        _listView.getLCLControlCanvas.Font.Height:= Round(item.textField.font.pointSize);
        frame:= item.textField.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
        _collectionView.styleHandler.onAdjustTextEditor( _listView, frame );
      end;
    drIcon:
      begin
        frame:= item.imageView.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
      end
  end;

  Result:= NSRectToRect( frame );
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemExchange(
  const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer
  );
begin
  // not supported yet
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
  _collectionView.reloadData;
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
    lisFocused,
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
var
  lclListView: TCustomListView;
  index: Integer;
begin
  case AProp of
    {lvpAutoArrange,}
    lvpCheckboxes: _collectionView.lclSetCheckBoxes(AIsSet);
    {lvpHideSelection,
    lvpHotTrack,}
    lvpMultiSelect: begin
      _collectionView.setAllowsMultipleSelection( AIsSet );
      if NOT AIsSet and (_collectionView.selectionIndexPaths.count>1) then begin
        lclListView:= TCustomListView( _listView.lclGetTarget );
        if Assigned(lclListView.ItemFocused) then begin
          index:= lclListView.ItemFocused.Index;
          _collectionView.deselectAll( nil );
          _collectionView.selectOneItemByIndex( index, True );
        end;
      end;
    end;
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
begin
  _collectionView.reloadData();
end;

end.
