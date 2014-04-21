unit CocoaWSComCtrls;

interface

{$mode delphi}
{$modeswitch objectivec1}

uses
  // RTL, FCL, LCL
  CocoaAll,
  Classes, LCLType, SysUtils,
  Controls, ComCtrls, Types, StdCtrls, LCLProc, Graphics, ImgList,
  // WS
  WSComCtrls,
  // Cocoa WS
  CocoaPrivate, CocoaUtils, CocoaWSCommon;

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
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
(*    // Column
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
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    //class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    //carbon//class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    // LV
    //carbon//class procedure BeginUpdate(const ALV: TCustomListView); override;
    //carbon//class procedure EndUpdate(const ALV: TCustomListView); override;

    class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
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
    class procedure SetOwnerData(const ALV: TCustomListView; const {%H-}AValue: Boolean); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
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


implementation

type

  TCocoaTabPage = objcclass(NSTabViewItem)
    callback: ICommonCallback;
    LCLPage: TCustomPage;
    //function lclGetCallback: ICommonCallback; override;
    //procedure lclClearCallback; override;
  end;

  TCocoaTabControl = objcclass(NSTabView)
    callback: ICommonCallback;
    //function lclGetCallback: ICommonCallback; override;
    //procedure lclClearCallback; override;
  end;

  TCocoaTableListView = objcclass(NSTableView)
    callback: ICommonCallback;
    //function lclGetCallback: ICommonCallback; override;
    //procedure lclClearCallback; override;
  end;

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
begin
  lControl := TCocoaTabPage.alloc().init();
  Result := TLCLIntfHandle(lControl);
  if Result <> 0 then
  begin
    //lControl.callback := TLCLCommonCallback.Create(Result, ATarget);
    lControl.LCLPage := TCustomPage(AWinControl);
  end;
end;

class procedure TCocoaWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
end;

{ TCocoaWSCustomTabControl }

class function TCocoaWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  lControl: TCocoaTabPage;
begin
  lControl := TCocoaTabControl.alloc.lclInitWithCreateParams(AParams);
  Result := TLCLIntfHandle(lControl);
  if Result <> 0 then
  begin
    //Result.callback := TLCLCommonCallback.Create(Result, ATarget);
  end;
end;

class procedure TCocoaWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaTabPage(AChild.Handle);

  lTabControl.insertTabViewItem_atIndex(lTabPage, AIndex);
end;

class procedure TCocoaWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
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
  WriteLn('[TCocoaWSCustomTabControl.SetPageIndex]');
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  WriteLn(Format('[TCocoaWSCustomTabControl.SetPageIndex] lTabControl=%d', [PtrUInt(lTabControl)]));
  lTabCount := lTabControl.numberOfTabViewItems();
  WriteLn(Format('[TCocoaWSCustomTabControl.SetPageIndex] lTabCount=%d', [lTabCount]));
  if (AIndex < 0) or (AIndex >= lTabCount) then Exit;

  lTabControl.selectTabViewItemAtIndex(AIndex);
end;

{ TCocoaWSCustomListView }

class function TCocoaWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
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

end.
