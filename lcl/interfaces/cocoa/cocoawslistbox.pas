unit CocoaWSListBox;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  // Libs
  CocoaAll, Classes, sysutils,
  // LCL
  Graphics, Controls, StdCtrls, ComCtrls, LCLType, LCLMessageGlue, LMessages,
  // Widgetset
  WSStdCtrls, WSLCLClasses,
  // LCL Cocoa
  CocoaPrivate, CocoaGDIObjects,
  CocoaListControl, CocoaTables, CocoaScrollers, CocoaWSScrollers;

type

  { TLCLListBoxCallback }

  TLCLListBoxCallback = class(TLCLListControlCallback)
  protected
    function AllocStrings(ATable: NSTableView): TCocoaListControlStringList; virtual;
  public
    listview : TCocoaTableListView;
    strings  : TCocoaListControlStringList;
    constructor CreateWithView(AOwner: TCocoaTableListView; ATarget: TWinControl);
    destructor Destroy; override;
    function ItemsCount: Integer; override;
    function GetImageListType(out lvil: TListViewImageList): Boolean; override;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean; override;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean; override;
    function GetImageFromIndex(imgIdx: Integer): NSImage; override;
    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String); override;
    function shouldSelectionChange(NewSel: Integer): Boolean; override;
    procedure ColumnClicked(ACol: Integer); override;
    function drawItem( row: Integer; ctx: TCocoaContext; const r: TRect;
      state: TOwnerDrawState ): Boolean; override;
    function customDraw( row: Integer; col: Integer;
      ctx: TCocoaContext; state: TCustomDrawState ): Boolean; override;
    function isCustomDrawSupported: Boolean; override;
    procedure GetRowHeight(rowidx: integer; var h: Integer); override;
    function GetBorderStyle: TBorderStyle; override;
  end;
  TLCLListBoxCallBackClass = class of TLCLListBoxCallBack;

  { TCocoaListBoxStringList }

  TCocoaListBoxStringList = class( TCocoaListControlStringList )
  protected
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Clear; override;
  end;

  { TCocoaTableListBoxProcessor }

  TCocoaTableListBoxProcessor = class( TCocoaTableListControlProcessor )
    function isInitializing( tv: NSTableView ): Boolean; override;
    function getLCLControlCanvas(tv: NSTableView): TCanvas; override;
    procedure onSelectionChanged(tv: NSTableView); override;
  end;

  { TCocoaWSCustomListBox }

  TCocoaWSCustomListBox = class(TWSCustomListBox)
  published
    class procedure DragStart(const ACustomListBox: TCustomListBox); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont ); override;

    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer; override;
    class function GetItemIndex(const ACustomListBox: TCustomListBox): integer; override;
    class function GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean; override;
    class function GetScrollWidth(const ACustomListBox: TCustomListBox): Integer; override;
    class function GetSelCount(const ACustomListBox: TCustomListBox): integer; override;
    class function GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean; override;
    class function GetStrings(const ACustomListBox: TCustomListBox): TStrings; override;
    class function GetTopIndex(const ACustomListBox: TCustomListBox): integer; override;

    class procedure SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    //class procedure SetBorder(const ACustomListBox: TCustomListBox); override;
    class procedure SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer); override;
    class procedure SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer); override;
    class procedure SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean); override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
    {class procedure SetSorted(const ACustomListBox: TCustomListBox; AList: TStrings; ASorted: boolean); override;}
    class procedure SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer); override;
  end;

  { TCocoaWSListBoxUtil }

  TCocoaWSListBoxUtil = class
  public
    class function getCallback(
      const AListBox: TCustomListBox ): TLCLListBoxCallback;
    class function getTableListView(
      const AListBox: TCustomListBox ): TCocoaTableListView;
    class procedure setStyle(
      const list: TCocoaTableListView;
      const AStyle: TListBoxStyle);
  end;

implementation

{ TCocoaWSListBoxUtil }

class function TCocoaWSListBoxUtil.getTableListView(
  const AListBox: TCustomListBox ): TCocoaTableListView;
var
  scrollView: NSScrollView;
begin
  Result:= nil;
  if NOT Assigned(AListBox) or NOT AListBox.HandleAllocated then
    Exit;
  scrollView:= NSSCrollView( AListBox.Handle );
  Result:= TCocoaTableListView( scrollView.documentView );
end;

class function TCocoaWSListBoxUtil.getCallback(
  const AListBox: TCustomListBox ): TLCLListBoxCallback;
var
  cocoaListView: TCocoaTableListView;
begin
  Result:= nil;
  cocoaListView:= getTableListView( AListBox );
  if Assigned(cocoaListView) then
    Result:= TLCLListBoxCallback( cocoaListView.callback );
end;

class procedure TCocoaWSListBoxUtil.setStyle(
  const list: TCocoaTableListView;
  const AStyle: TListBoxStyle );
begin
  if not Assigned(list) then Exit;
  list.isOwnerDraw := AStyle in [lbOwnerDrawFixed, lbOwnerDrawVariable];
  list.isDynamicRowHeight := AStyle = lbOwnerDrawVariable;
  //todo: if flag isCustomRowHeight changes in runtime
  //      noteHeightOfRowsWithIndexesChanged, should be sent to listview
end;

{ TLCLListBoxCallback }

function TLCLListBoxCallback.AllocStrings(ATable: NSTableView
  ): TCocoaListControlStringList;
begin
  Result := TCocoaListBoxStringList.Create(ATable);
end;

constructor TLCLListBoxCallback.CreateWithView(AOwner: TCocoaTableListView;
  ATarget: TWinControl);
begin
  Create(AOwner, ATarget);

  listview := AOwner;
  strings := AllocStrings(AOwner);
end;

destructor TLCLListBoxCallback.Destroy;
begin
  // "strings" are released with FreeStrings call
  inherited Destroy;
end;

function TLCLListBoxCallback.ItemsCount: Integer;
begin
  Result := strings.Count;
end;

function TLCLListBoxCallback.GetImageListType(out lvil: TListViewImageList
  ): Boolean;
begin
  Result:= False;
end;

function TLCLListBoxCallback.GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean;
begin
  Result := (ARow>=0) and (ARow < strings.Count);
  if Result then Text := strings[ARow];
end;

function TLCLListBoxCallback.GetItemImageAt(ARow, ACol: Integer;
  var imgIdx: Integer): Boolean;
begin
  Result := false;
end;

function TLCLListBoxCallback.GetImageFromIndex(imgIdx: Integer): NSImage;
begin
  Result := nil;
end;

procedure TLCLListBoxCallback.SetItemTextAt(ARow, ACol: Integer;
  const Text: String);
begin
  // todo:
end;

function TLCLListBoxCallback.shouldSelectionChange(NewSel: Integer
  ): Boolean;
begin
  Result:= true;
end;

procedure TLCLListBoxCallback.ColumnClicked(ACol: Integer);
begin
  // not needed
end;

function TLCLListBoxCallback.drawItem( row: Integer; ctx: TCocoaContext;
  const r: TRect; state: TOwnerDrawState ): Boolean;
var
  DrawStruct: TDrawListItemStruct;
begin
  Result:= False;
  if NOT listview.isOwnerDraw then
    Exit;

  DrawStruct.ItemState := state;
  DrawStruct.Area := r;
  DrawStruct.DC := HDC(ctx);
  DrawStruct.ItemID := row;
  LCLSendDrawListItemMsg(Target, @DrawStruct);
  Result:= True;
end;

function TLCLListBoxCallback.customDraw(row: Integer; col: Integer;
  ctx: TCocoaContext; state: TCustomDrawState ): Boolean;
begin
  Result:= False;
end;

function TLCLListBoxCallback.isCustomDrawSupported: Boolean;
begin
  Result:= False;
end;

procedure TLCLListBoxCallback.GetRowHeight(rowidx: integer; var h: Integer);
begin
  if TCustomListBox(Target).Style = lbOwnerDrawVariable then
    TCustomListBox(Target).MeasureItem(rowidx, h);
end;

function TLCLListBoxCallback.GetBorderStyle: TBorderStyle;
begin
  Result:= TCustomListBox(Target).BorderStyle;
end;

{ TCocoaListBoxStringList }

procedure TCocoaListBoxStringList.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited;
  TCocoaTableListView(self.Owner).lclInsertItem(Index);
end;

procedure TCocoaListBoxStringList.Delete(Index: Integer);
begin
  inherited;
  TCocoaTableListView(self.Owner).lclDeleteItem(Index);
end;

procedure TCocoaListBoxStringList.Exchange(Index1, Index2: Integer);
begin
  inherited;
  TCocoaTableListView(self.Owner).lclExchangeItem(Index1, Index2);
end;

procedure TCocoaListBoxStringList.Clear;
begin
  inherited;
  TCocoaTableListView(self.Owner).lclClearItem();
end;

{ TCocoaWSCustomListBox }

class procedure TCocoaWSCustomListBox.DragStart(
  const ACustomListBox: TCustomListBox);
var
  lclcb : TLCLListBoxCallback;
begin
  lclcb:= TCocoaWSListBoxUtil.getCallback( ACustomListBox );
  if NOT Assigned(lclcb) then
    Exit;
  lclcb.BlockCocoaMouseMove:=true;
end;

function TCocoaTableListBoxProcessor.isInitializing(tv: NSTableView): Boolean;
begin
  Result:= False;
end;

function TCocoaTableListBoxProcessor.getLCLControlCanvas(tv: NSTableView
  ): TCanvas;
begin
  Result:= TCustomListBox(tv.lclGetTarget).Canvas;
end;

procedure TCocoaTableListBoxProcessor.onSelectionChanged(tv: NSTableView);
var
  lclListBox: TCustomListBox;
  cocoaTLV: TCocoaTableListView Absolute tv;
  lclcb: TLCLListBoxCallback;
begin
  if NOT Assigned(cocoaTLV.callback) then
    Exit;

  lclcb:= TLCLListBoxCallback( cocoaTLV.callback );
  lclListBox:= TCustomListBox( lclcb.Target );
  if lclListBox = nil then
    Exit;

  if NOT cocoaTLV.selectingByProgram then begin
    lclcb.selectionIndexSet.removeAllIndexes;
    lclcb.selectionIndexSet.addIndexes( tv.selectedRowIndexes );
  end;

  if cocoaTLV.dontSendOnChangeMessage then
    Exit;

  // do not notify about selection changes while clearing
  if Assigned(lclcb.strings) and (lclcb.strings.isClearing) then Exit;
  SendSimpleMessage(lclListBox, LM_SELCHANGE);
end;

class function TCocoaWSCustomListBox.CreateHandle(const AWinControl:TWinControl;
  const AParams:TCreateParams):TLCLHandle;
var
  list    : TCocoaTableListView;
  processor: TCocoaTableViewProcessor;
  column  : NSTableColumn;
  scroll  : TCocoaScrollView;
  lclListBox: TCustomListBox absolute AWinControl;
  cb  : TLCLListBoxCallback;
begin
  list := AllocCocoaTableListView.lclInitWithCreateParams(AParams);
  if not Assigned(list) then
  begin
    Result := 0;
    Exit;
  end;
  processor:= TCocoaTableListBoxProcessor.Create;
  list.lclSetProcessor( processor );
  cb := TLCLListBoxCallback.CreateWithView(list, AWinControl);
  list.callback := cb;

  column := NSTableColumn.alloc.init.autorelease;
  // if column.ResizingMask is set when column.Width < TableView.Width,
  // it is not only meaningless, but also causes problems in the
  // vertical direction of the TableView. we can scroll the TableView until
  // the last row appears at the top.
  // Note: It is not clear whether this is a feature of Cocoa or a bug.
  if lclListBox.ScrollWidth > lclListBox.Width then
  begin
    column.setResizingMask(NSTableColumnNoResizing);
    column.setWidth(lclListBox.ScrollWidth);
  end;
  list.addTableColumn(column);

  list.setHeaderView(nil);
  list.setDataSource(list);
  list.setDelegate(list);
  list.setAllowsMultipleSelection(lclListBox.MultiSelect);
  list.CustomRowHeight:= lclListBox.ItemHeight;
  list.readOnly := true;
  // LCL ItemHeight for TListBox can only be set during Recreation of Handle
  if TCustomListBox(AWinControl).ItemHeight>0 then
  begin
    // Cocoa default is 16.
    // Note that it might be different of Retina monitors
    list.CustomRowHeight := TCustomListBox(AWinControl).ItemHeight;
    list.setRowHeight(list.CustomRowHeight);
  end;

  TCocoaWSListBoxUtil.setStyle(list, TCustomListBox(AWinControl).Style);

  scroll := TCocoaWSScrollerUtil.embedInScrollView(list);
  if not Assigned(scroll) then
  begin
    Result := 0;
    Exit;
  end;
  cb.SetHandleFrame(scroll);
  scroll.callback := list.callback;
  scroll.setHasVerticalScroller(true);
  scroll.setHasHorizontalScroller(true);
  scroll.setAutohidesScrollers(true);
  TCocoaScrollUtil.setBorderStyle(scroll, lclListBox.BorderStyle);
  TCocoaViewUtil.updateFocusRing(list, lclListBox);

  Result := TLCLHandle(scroll);
end;

class function TCocoaWSCustomListBox.GetIndexAtXY(const ACustomListBox: TCustomListBox; X, Y: integer): integer;
var
  list: TCocoaTableListView;
  lPoint: NSPoint;
begin
  list := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(list) then
  begin
    Result:=-1;
    Exit();
  end;

  Result := LCLCoordToRow(list, x,y);
end;

class function TCocoaWSCustomListBox.GetItemRect(const ACustomListBox: TCustomListBox; Index: integer; var ARect: TRect): boolean;
var
  view: TCocoaTableListView;
  r:NSRect;
begin
  Result := False;

  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(False);
  Result := LCLGetItemRect(view, Index, 0, ARect);
end;

class function TCocoaWSCustomListBox.GetScrollWidth(const ACustomListBox: TCustomListBox): Integer;
var
  view: TCocoaTableListView;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(0);
  Result := view.ScrollWidth;
end;

class function TCocoaWSCustomListBox.GetItemIndex(const ACustomListBox: TCustomListBox): integer;
var
  view: TCocoaTableListView;
  indexset: NSIndexSet;
begin
  view:=TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(-1);

  indexset:=view.selectedRowIndexes();
  if indexset.count = 0 then
    Result := -1
  else
    Result := indexset.firstIndex;
end;

class function TCocoaWSCustomListBox.GetSelCount(const ACustomListBox: TCustomListBox): integer;
var
  view: TCocoaTableListView;
  selection: NSIndexSet;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(0);
  selection := view.selectedRowIndexes();
  Result := selection.count();
end;


class function TCocoaWSCustomListBox.GetSelected(const ACustomListBox: TCustomListBox; const AIndex: integer): boolean;
var
  view: TCocoaTableListView;
  selection: NSIndexSet;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(False);
  if AIndex < 0 then Exit(False);
  selection := view.selectedRowIndexes();
  Result := selection.containsIndex(AIndex);
end;

class function TCocoaWSCustomListBox.GetStrings(const ACustomListBox: TCustomListBox):TStrings;
var
  lclcb : TLCLListBoxCallback;
begin
  lclcb:= TCocoaWSListBoxUtil.getCallback( ACustomListBox );
  if NOT Assigned(lclcb) then
    Exit;
  Result:= lclcb.strings;
end;

class function TCocoaWSCustomListBox.GetTopIndex(const ACustomListBox: TCustomListBox): integer;
var
  view: TCocoaTableListView;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit(-1);
  Result := LCLGetTopRow(view);
end;

class procedure TCocoaWSCustomListBox.SelectItem(const ACustomListBox: TCustomListBox; AIndex: integer; ASelected: boolean);
var
  cocoaTLV:TCocoaTableListView;
  lclcb: TLCLListBoxCallback;
begin
  lclcb:= TCocoaWSListBoxUtil.getCallback( ACustomListBox );
  if NOT Assigned(lclcb) then
    Exit;

  if lclcb.getItemStableSelection(AIndex) <> ASelected then begin
    cocoaTLV:= TCocoaWSListBoxUtil.getTableListView( ACustomListBox );
    cocoaTLV.selectOneItemByIndex( AIndex, ASelected );
  end;
end;

class procedure TCocoaWSCustomListBox.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  list: TCocoaTableListView;
begin
  list := TCocoaWSListBoxUtil.getTableListView( TCustomListBox(AWinControl) );
  if not Assigned(list) then Exit;

  TCocoaScrollUtil.setBorderStyle(list.enclosingScrollView, ABorderStyle);
  TCocoaViewUtil.updateFocusRing(list, AWinControl);
end;

class procedure TCocoaWSCustomListBox.SetFont(const AWinControl: TWinControl;
  const AFont: TFont);
var
  ACustomListBox: TCustomListBox absolute AWinControl;
  list: TCocoaTableListView;
begin
  list:= TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(list) then
    Exit;
  list.reloadData;
end;

class procedure TCocoaWSCustomListBox.SetItemIndex(const ACustomListBox: TCustomListBox; const AIndex: integer);
var
  list: TCocoaTableListView;
begin
  list := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(list) then Exit();

  if (AIndex < 0) then
    list.deselectAll(nil)
  else
  begin
    list.selectRowIndexes_byExtendingSelection(NSIndexSet.indexSetWithIndex(AIndex), false);
    list.scrollRowToVisible(AIndex);
  end;
end;

class procedure TCocoaWSCustomListBox.SetScrollWidth(const ACustomListBox: TCustomListBox; const AScrollWidth: Integer);
var
  view: TCocoaTableListView;
  column: NSTableColumn;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit;
  view.ScrollWidth := AScrollWidth;
  column := NSTableColumn(view.tableColumns.objectAtIndex(0));
  if AScrollWidth > ACustomListBox.Width then begin
    column.setResizingMask(NSTableColumnNoResizing);
    column.setWidth(AScrollWidth);
  end else begin
    column.setResizingMask(NSTableColumnAutoresizingMask or NSTableColumnUserResizingMask);
    view.sizeLastColumnToFit;
  end;
end;

class procedure TCocoaWSCustomListBox.SetSelectionMode(const ACustomListBox: TCustomListBox; const AExtendedSelect, AMultiSelect: boolean);
var
  list: TCocoaTableListView;
begin
  list := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(list) then Exit();
  list.setAllowsMultipleSelection(AMultiSelect);
end;

class procedure TCocoaWSCustomListBox.SetStyle(const ACustomListBox: TCustomListBox);
var
  view: TCocoaTableListView;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  TCocoaWSListBoxUtil.setStyle(view, ACustomListBox.Style);
  view.reloadData;
end;

class procedure TCocoaWSCustomListBox.SetTopIndex(const ACustomListBox: TCustomListBox; const NewTopIndex: integer);
var
  view: TCocoaTableListView;
begin
  view := TCocoaWSListBoxUtil.getTableListView(ACustomListBox);
  if not Assigned(view) then Exit();
  view.scrollRowToVisible(NewTopIndex);
end;

end.
