unit CocoaWSComCtrls;

interface

{$mode delphi}
{$modeswitch objectivec1}

{.$DEFINE COCOA_DEBUG_TABCONTROL}
{$DEFINE COCOA_DEBUG_LISTVIEW}

uses
  // RTL, FCL, LCL
  MacOSAll, CocoaAll,
  Classes, LCLType, SysUtils, Contnrs, LCLMessageGlue, LMessages,
  Controls, ComCtrls, Types, StdCtrls, LCLProc, Graphics, ImgList,
  // WS
  WSComCtrls,
  // Cocoa WS
  CocoaPrivate, CocoaUtils, CocoaProc, CocoaWSCommon;

type

  { TCocoaWSStatusBar }

  TCocoaWSStatusBar = class(TWSStatusBar)
  published
    //class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    //class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    //class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    //class procedure Update(const AStatusBar: TStatusBar); override;
  end;

  { TCocoaWSTabSheet }

  TCocoaWSTabSheet = class(TWSTabSheet)
  published
  end;


  { TCocoaWSCustomPage }

  TCocoaWSCustomPage = class(TWSCustomPage)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetProperties(const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
  end;

  { TCocoaWSCustomTabControl }

  TCocoaWSCustomTabControl = class(TWSCustomTabControl)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;

    class procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ATabControl: TCustomTabControl; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    //class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    //class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
  end;

  { TCocoaWSPageControl }

  TCocoaWSPageControl = class(TWSPageControl)
  published
  end;

  { TCocoaWSCustomListView }

  TCocoaWSCustomListView = class(TWSCustomListView)
  private
    class function CheckParams(out ATableControl: TCocoaTableListView; const ALV: TCustomListView): Boolean;
    class function CheckColumnParams(out ATableControl: TCocoaTableListView;
      out ANSColumn: NSTableColumn; const ALV: TCustomListView; const AIndex: Integer; ASecondIndex: Integer = -1): Boolean;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AImageIndex: Integer); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;

    // Item
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem); override;
    (*class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    //class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    //carbon//class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;*)
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    // LV
    //available in 10.7 only//class procedure BeginUpdate(const ALV: TCustomListView); override;
    //available in 10.7 only//class procedure EndUpdate(const ALV: TCustomListView); override;

    (*class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    //carbon//class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    //carbon//class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    //carbon//class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    //carbon//class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    //carbon//class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    //carbon//class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetImageList(const ALV: TCustomListView; const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageList); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const {%H-}AValue: Boolean); override;*)
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    (*class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
    class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;
    class procedure SetViewStyle(const ALV: TCustomListView; const AValue: TViewStyle); override;*)
  end;

  { TCocoaWSProgressBar }

  TCocoaWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TLCLListViewCallback }

  TLCLListViewCallback = class(TLCLCommonCallback, IListViewCallback)
  public
    procedure SelectionChanged; virtual;
  end;
  TLCLListViewCallBackClass = class of TLCLListViewCallback;

implementation

type
  { TCocoaProgressIndicator }

  TCocoaProgressIndicator = objcclass(NSProgressIndicator)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
  end;


function AllocProgressIndicator(ATarget: TWinControl; const AParams: TCreateParams): TCocoaProgressIndicator;
begin
  Result := TCocoaProgressIndicator.alloc.lclInitWithCreateParams(AParams);
  if Assigned(Result) then
  begin
    Result.callback := TLCLCommonCallback.Create(Result, ATarget);
    Result.startAnimation(nil);
    //small constrol size looks like carbon
    //Result.setControlSize(NSSmallControlSize);
  end;
end;

{ TCocoaWSCustomPage }

class function TCocoaWSCustomPage.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lControl: TCocoaTabPage;
  tv: TCocoaTabPageView;
  tabview: TCocoaTabControl;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomPage.CreateHandle]');
  {$ENDIF}
  lControl := TCocoaTabPage.alloc().init();
  Result := TLCLIntfHandle(lControl);
  if Result <> 0 then
  begin
    lControl.callback := TLCLCommonCallback.Create(lControl, AWinControl);
    lControl.LCLPage := TCustomPage(AWinControl);
    SetProperties(TCustomPage(AWinControl), lControl);

    // Set a special view for the page
    tabview := TCocoaTabControl(AWinControl.Parent.Handle);
    tv := TCocoaTabPageView.alloc.initWithFrame(
      tabview.contentRect);
    {tv.setHasVerticalScroller(True);
    tv.setHasHorizontalScroller(True);
    tv.setAutohidesScrollers(True);
    tv.setBorderType(NSNoBorder);}
    tv.tabview := tabview;
    tv.callback := TLCLCommonCallback.Create(tv, AWinControl);
    lControl.setView(tv);
  end;
end;

class procedure TCocoaWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.UpdateProperties] ACustomPage='+IntToStr(PtrInt(ACustomPage)));
  {$ENDIF}
  if not Assigned(ACustomPage) or not ACustomPage.HandleAllocated then Exit;
  lTabPage := TCocoaTabPage(ACustomPage.Handle);
  SetProperties(ACustomPage, lTabPage);
end;

class procedure TCocoaWSCustomPage.SetProperties(
  const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
var
  lHintStr: string;
begin
  // title
  ACocoaControl.setLabel(NSStringUTF8(ACustomPage.Caption));

  // hint
  if ACustomPage.ShowHint then lHintStr := ACustomPage.Hint
  else lHintStr := '';
  ACocoaControl.setToolTip(NSStringUTF8(lHintStr));
end;

{ TCocoaWSCustomTabControl }

class function TCocoaWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lControl: TCocoaTabControl;
begin
  lControl := TCocoaTabControl.alloc.lclInitWithCreateParams(AParams);
  Result := TLCLIntfHandle(lControl);
  if Result <> 0 then
  begin
    lControl.callback := TLCLCommonCallback.Create(lControl, AWinControl);
    lControl.LCLPageControl := TCustomTabControl(AWinControl);
    lControl.setDelegate(lControl);
  end;
end;

class procedure TCocoaWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] AChild='+IntToStr(PtrInt(AChild)));
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaTabPage(AChild.Handle);

  lTabControl.insertTabViewItem_atIndex(lTabPage, AIndex);
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] END');
  {$ENDIF}
end;

class procedure TCocoaWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaTabPage(AChild.Handle);

  lTabControl.removeTabViewItem(lTabPage);
  lTabControl.insertTabViewItem_atIndex(lTabPage, NewIndex);
end;

class procedure TCocoaWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lTabPage := lTabControl.tabViewItemAtIndex(AIndex);
  lTabControl.removeTabViewItem(lTabPage);
end;

class function TCocoaWSCustomTabControl.GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
  lClientPos: NSPoint;
begin
  Result := 0;
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lClientPos := GetNSPoint(AClientPos.X, AClientPos.Y); // ToDo: Check if it doesn't need y invertion
  lTabPage := lTabControl.tabViewItemAtPoint(lClientPos);
  Result := lTabControl.indexOfTabViewItem(lTabPage);
end;

class procedure TCocoaWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabCount: NSInteger;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.SetPageIndex]');
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaWSCustomTabControl.SetPageIndex] lTabControl=%d', [PtrUInt(lTabControl)]));
  {$ENDIF}
  lTabCount := lTabControl.numberOfTabViewItems();
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaWSCustomTabControl.SetPageIndex] lTabCount=%d', [lTabCount]));
  {$ENDIF}
  if (AIndex < 0) or (AIndex >= lTabCount) then Exit;

  lTabControl.selectTabViewItemAtIndex(AIndex);
end;

{ TCocoaWSCustomListView }

class function TCocoaWSCustomListView.CheckParams(out
  ATableControl: TCocoaTableListView; const ALV: TCustomListView): Boolean;
var
  lObject: NSObject;
begin
  Result := False;
  ATableControl := nil;
  ALV.HandleNeeded();
  if not Assigned(ALV) or not ALV.HandleAllocated then Exit;
  lObject := NSObject(ALV.Handle);

  // ToDo: Implement for other styles
  if lObject.isKindOfClass(TCocoaTableListView) then
  begin
    ATableControl := TCocoaTableListView(lObject);
    Result := True;
  end;
end;

class function TCocoaWSCustomListView.CheckColumnParams(
  out ATableControl: TCocoaTableListView; out ANSColumn: NSTableColumn;
  const ALV: TCustomListView; const AIndex: Integer; ASecondIndex: Integer
  ): Boolean;
var
  lObject: NSObject;
begin
  Result := False;
  ALV.HandleNeeded();
  if not Assigned(ALV) or not ALV.HandleAllocated then Exit;
  lObject := NSObject(ALV.Handle);
  if not lObject.isKindOfClass(TCocoaTableListView) then Exit; // in styles other than Report, column have no meaning

  ATableControl := TCocoaTableListView(lObject);
  if (AIndex < 0) or (AIndex >= ATableControl.tableColumns.count()) then Exit;
  ANSColumn := NSTableColumn(ATableControl.tableColumns.objectAtIndex(AIndex));

  if ASecondIndex >= 0 then
  begin
    if (ASecondIndex < 0) or (ASecondIndex >= ATableControl.tableColumns.count()) then Exit;
  end;
  Result := True;
end;

class function TCocoaWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lTableLV: TCocoaTableListView;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn('[TCocoaWSCustomListView.CreateHandle] AWinControl='+IntToStr(PtrInt(AWinControl)));
  {$ENDIF}
  lTableLV := TCocoaTableListView.alloc.lclInitWithCreateParams(AParams);
  Result := TLCLIntfHandle(lTableLV);
  if Result <> 0 then
  begin
    lTableLV.callback := TLCLListViewCallback.Create(lTableLV, AWinControl);
    lTableLV.Items := TStringList.Create;

    lTableLV.ListView := TCustomListView(AWinControl);
    lTableLV.setDataSource(lTableLV);
    lTableLV.setDelegate(lTableLV);
  end;
end;

class procedure TCocoaWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnDelete] AIndex=%d', [AIndex]));
  {$ENDIF}
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  if lNSColumn = nil then Exit;
  lTableLV.removeTableColumn(lNSColumn);
  lNSColumn.release;
end;

class function TCocoaWSCustomListView.ColumnGetWidth(
  const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn
  ): Integer;
var
  lTableLV: TCocoaTableListView;
  lColumn: NSTableColumn;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnGetWidth] AIndex=%d', [AIndex]));
  {$ENDIF}
  if not Assigned(ALV) or not ALV.HandleAllocated then Exit;
  lTableLV := TCocoaTableListView(ALV.Handle);
  if (AIndex < 0) or (AIndex >= lTableLV.tableColumns.count()) then Exit;

  lColumn := lTableLV.tableColumns.objectAtIndex(AIndex);
  Result := Round(lColumn.width());
end;

class procedure TCocoaWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
  lTitle: NSString;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnInsert] ALV=%x AIndex=%d', [PtrInt(ALV), AIndex]));
  {$ENDIF}
  ALV.HandleNeeded();
  if not Assigned(ALV) or not ALV.HandleAllocated then Exit;
  lTableLV := TCocoaTableListView(ALV.Handle);
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnInsert]=> tableColumns.count=%d', [lTableLV.tableColumns.count()]));
  {$ENDIF}
  if (AIndex < 0) or (AIndex >= lTableLV.tableColumns.count()+1) then Exit;
  lTitle := NSStringUTF8(AColumn.Caption);
  lNSColumn := NSTableColumn.alloc.initWithIdentifier(lTitle);
  lTableLV.addTableColumn(lNSColumn);
  lTitle.release;
end;

class procedure TCocoaWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AOldINdex, ANewIndex) then Exit;

  lTableLV.moveColumn_toColumn(AOldIndex, ANewIndex);
end;

class procedure TCocoaWSCustomListView.ColumnSetAlignment(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAlignment: TAlignment);
begin
  inherited ColumnSetAlignment(ALV, AIndex, AColumn, AAlignment);
end;

class procedure TCocoaWSCustomListView.ColumnSetAutoSize(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAutoSize: Boolean);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
  lResizeMask: NSUInteger;
begin
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  if AAutoSize then lResizeMask := NSTableColumnAutoresizingMask
  else lResizeMask := NSTableColumnUserResizingMask;
  lNSColumn.setResizingMask(lResizeMask);
end;

class procedure TCocoaWSCustomListView.ColumnSetCaption(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ACaption: String);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
  lNSCaption: NSString;
begin
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  lNSCaption := NSStringUtf8(ACaption);
  lNSColumn.setIdentifier(lNSCaption);
end;

class procedure TCocoaWSCustomListView.ColumnSetImage(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AImageIndex: Integer);
begin
  inherited ColumnSetImage(ALV, AIndex, AColumn, AImageIndex);
end;

class procedure TCocoaWSCustomListView.ColumnSetMaxWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMaxWidth: Integer);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnSetMaxWidth] AMaxWidth=%d', [AMaxWidth]));
  {$ENDIF}
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  if AMaxWidth <= 0 then lNSColumn.setMaxWidth($FFFFFFFF)
  else lNSColumn.setMaxWidth(AMaxWidth);
end;

class procedure TCocoaWSCustomListView.ColumnSetMinWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMinWidth: integer);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnSetMinWidth] AMinWidth=%d', [AMinWidth]));
  {$ENDIF}
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  lNSColumn.setMinWidth(AMinWidth);
end;

class procedure TCocoaWSCustomListView.ColumnSetWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AWidth: Integer);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TCocoaWSCustomListView.ColumnSetWidth] AWidth=%d', [AWidth]));
  {$ENDIF}
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  lNSColumn.setWidth(AWidth);
end;

class procedure TCocoaWSCustomListView.ColumnSetVisible(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AVisible: Boolean);
var
  lTableLV: TCocoaTableListView;
  lNSColumn: NSTableColumn;
begin
  if not CheckColumnParams(lTableLV, lNSColumn, ALV, AIndex) then Exit;

  lNSColumn.setHidden(not AVisible);
end;

class procedure TCocoaWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaWSCustomListView.ItemDelete] AIndex=%d', [AIndex]));
  {$ENDIF}
  inherited ItemDelete(ALV, AIndex);
end;

class function TCocoaWSCustomListView.ItemDisplayRect(
  const ALV: TCustomListView; const AIndex, ASubItem: Integer;
  ACode: TDisplayCode): TRect;
begin
  Result:=inherited ItemDisplayRect(ALV, AIndex, ASubItem, ACode);
end;

class function TCocoaWSCustomListView.ItemGetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem
  ): Boolean;
begin
  Result:=inherited ItemGetChecked(ALV, AIndex, AItem);
end;

class function TCocoaWSCustomListView.ItemGetPosition(
  const ALV: TCustomListView; const AIndex: Integer): TPoint;
begin
  Result:=inherited ItemGetPosition(ALV, AIndex);
end;

class function TCocoaWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
begin
  Result:=inherited ItemGetState(ALV, AIndex, AItem, AState, AIsSet);
end;

class procedure TCocoaWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  lTableLV: TCocoaTableListView;
  i, lColumnCount: Integer;
  lColumn: NSTableColumn;
  lStr: string;
  lNSStr: NSString;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaWSCustomListView.ItemInsert] AIndex=%d', [AIndex]));
  {$ENDIF}
  if not Assigned(ALV) or not ALV.HandleAllocated then Exit;
  lTableLV := TCocoaTableListView(ALV.Handle);
  lColumnCount := lTableLV.tableColumns.count();
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaWSCustomListView.ItemInsert]=> lColumnCount=%d', [lColumnCount]));
  {$ENDIF}
  for i := 0 to lColumnCount-1 do
  begin
    lColumn := lTableLV.tableColumns.objectAtIndex(i);
    if i = 0 then
      lStr := AItem.Caption
    else if (i-1 < AItem.SubItems.Count) then
      lStr := AItem.SubItems.Strings[i-1]
    else
      lStr := '';
    lNSStr := NSStringUTF8(lStr);
    lTableLV.setStringValue_forTableColumn_row(lNSStr, i, AIndex);
    lNSStr.release;
  end;
  lTableLV.reloadData();
end;

class procedure TCocoaWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
  inherited ItemSetText(ALV, AIndex, AItem, ASubIndex, AText);
end;

class procedure TCocoaWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
begin
  inherited ItemShow(ALV, AIndex, AItem, PartialOK);
end;

class procedure TCocoaWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  lTableLV: TCocoaTableListView;
begin
  CheckParams(lTableLV, ALV);
  case AProp of
  {lvpAutoArrange,
  lvpCheckboxes,}
  lvpColumnClick: lTableLV.setAllowsColumnSelection(AIsSet);
{  lvpFlatScrollBars,
  lvpFullDrag,
  lvpGridLines,
  lvpHideSelection,
  lvpHotTrack,}
  lvpMultiSelect: lTableLV.setAllowsMultipleSelection(AIsSet);
  {lvpOwnerDraw,
  //lvpReadOnly, implemented elsewhere, no need for this here
  lvpRowSelect,
  lvpShowColumnHeaders,
  lvpShowWorkAreas,
  lvpWrapText,
  lvpToolTips}
  end;
end;

class procedure TCocoaWSCustomListView.SetProperties(
  const ALV: TCustomListView; const AProps: TListViewProperties);
begin
  inherited SetProperties(ALV, AProps);
end;

{ TCocoaWSProgressBar }

class function TCocoaWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  Result:=TLCLIntfHandle(AllocProgressIndicator(AWinControl, AParams));
end;

class procedure TCocoaWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  ind : NSProgressIndicator;
begin
  if not Assigned(AProgressBar) or not AProgressBar.HandleAllocated then Exit;
  ind:=NSProgressIndicator(AProgressBAr.Handle);
  ind.setMaxValue(AProgressBar.Max);
  ind.setMinValue(AProgressBar.Min);
  ind.setDoubleValue(AProgressBar.Position);
  ind.setIndeterminate(AProgressBar.Style = pbstMarquee);
end;

class procedure TCocoaWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if AProgressBar.HandleAllocated then
    NSProgressIndicator(AProgressBar.Handle).setDoubleValue(NewPosition);
end;

class procedure TCocoaWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if AProgressBar.HandleAllocated then
    NSProgressIndicator(AProgressBar.Handle).setIndeterminate(NewStyle = pbstMarquee);
end;

{ TCocoaTabPage }

(*function TCocoaTabPage.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabPage.lclClearCallback;
begin
  callback:=nil;
end;

{ TCocoaTabControl }

function TCocoaTabControl.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabControl.lclClearCallback;
begin
  callback:=nil;
end; *)

{ TCocoaProgressIndicator }

function TCocoaProgressIndicator.acceptsFirstResponder: Boolean;
begin
  Result:=True;
end;

function TCocoaProgressIndicator.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaProgressIndicator.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaProgressIndicator.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaProgressIndicator.lclClearCallback;
begin
  callback:=nil;
end;

procedure TCocoaProgressIndicator.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TLCLListViewCallback }

procedure TLCLListViewCallback.SelectionChanged;
begin
  SendSimpleMessage(Target, LM_SELCHANGE);
end;

end.
