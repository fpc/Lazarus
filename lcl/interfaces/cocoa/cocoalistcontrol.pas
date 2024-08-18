unit CocoaListControl;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Graphics, Controls, ComCtrls,
  CocoaAll, CocoaPrivate, CocoaCallback, CocoaWSCommon, CocoaGDIObjects;

type
  {
    currently the following callbacks implement IListViewCallBack,
    need to be considered before modification:
    1. TLCLListViewCallback
    2. TLCLListBoxCallback
    3. TLCLCheckboxListCallback
  }

  { IListViewCallBack }

  IListViewCallBack = interface(ICommonCallback)
    function ItemsCount: Integer;
    procedure GetRowHeight(rowidx: Integer; var height: Integer);
    function GetBorderStyle: TBorderStyle;

    function GetImageListType( out lvil: TListViewImageList ): Boolean;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean;
    function GetItemCheckedAt( row: Integer; var CheckState: Integer): Boolean;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean;
    function GetImageFromIndex(imgIdx: Integer): NSImage;

    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String);
    procedure SetItemCheckedAt( row: Integer; CheckState: Integer);

    function selectionIndexSet: NSMutableIndexSet;
    function checkedIndexSet: NSMutableIndexSet;
    function shouldSelectionChange(NewSel: Integer): Boolean;
    function getItemStableSelection(ARow: Integer): Boolean;

    procedure ColumnClicked(ACol: Integer);
    function onAddSubview( aView:NSView ): Boolean;

    function drawItem( row: Integer; ctx: TCocoaContext; const r: TRect; state: TOwnerDrawState ): Boolean;
    function customDraw( row: Integer; col: Integer; ctx: TCocoaContext; state: TCustomDrawState ): Boolean;
    function isCustomDrawSupported: Boolean;
  end;

  { TLCLListControlCallback }

  TLCLListControlCallback = class abstract(TLCLCommonCallback, IListViewCallback)
  private
    _selectionIndexSet: NSMutableIndexSet;
    _checkedIndexSet: NSMutableIndexSet;
  public
    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleFrame: NSView = nil); override;
    destructor Destroy; override;
    function selectionIndexSet: NSMutableIndexSet; virtual;
    function checkedIndexSet: NSMutableIndexSet; virtual;
    function GetItemCheckedAt( row: Integer; var CheckState: Integer): Boolean; virtual;
    procedure SetItemCheckedAt( row: Integer; CheckState: Integer); virtual;
    function getItemStableSelection(ARow: Integer): Boolean; virtual;
  public
    function ItemsCount: Integer; virtual; abstract;
    procedure GetRowHeight(rowidx: Integer; var height: Integer); virtual; abstract;
    function GetBorderStyle: TBorderStyle; virtual; abstract;

    function GetImageListType( out lvil: TListViewImageList ): Boolean; virtual; abstract;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean; virtual; abstract;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean; virtual; abstract;
    function GetImageFromIndex(imgIdx: Integer): NSImage; virtual; abstract;
    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String); virtual; abstract;

    function shouldSelectionChange(NewSel: Integer): Boolean; virtual; abstract;
    procedure ColumnClicked(ACol: Integer); virtual; abstract;
    function onAddSubview( aView:NSView ): Boolean; virtual; abstract;

    function drawItem( row: Integer; ctx: TCocoaContext; const r: TRect; state: TOwnerDrawState ): Boolean; virtual; abstract;
    function customDraw( row: Integer; col: Integer; ctx: TCocoaContext; state: TCustomDrawState ): Boolean; virtual; abstract;
    function isCustomDrawSupported: Boolean; virtual; abstract;
  end;

  {
    1. TCocoaTableListView related need to support
       TListView/TListBox/TCheckListBox, etc.
    2. the differences between these controls can be considered to be
       implemented in the callback.
    3. however, after careful consideration, we tried to keep the original
       intention of the callback, and added TCocoaTableViewProcessor to
       isolate these differences.
  }
  { TCocoaTableViewProcessor }

  TCocoaTableViewProcessor = class
    function isInitializing( tv: NSTableView ): Boolean; virtual; abstract;
    function getLCLControlCanvas( tv:NSTableView ): TCanvas; virtual; abstract;
    procedure onReloadData( tv: NSTableView ); virtual; abstract;
    procedure onSelectOneItem( tv: NSTableView; selection: NSIndexSet ); virtual; abstract;
    procedure onSelectionChanged( tv: NSTableView ); virtual; abstract;
  end;

  { TCocoaTableListControlProcessor }

  TCocoaTableListControlProcessor = class( TCocoaTableViewProcessor )
  protected
    function getCallback( tv: NSTableView ): TLCLListControlCallback;
  public
    procedure onReloadData( tv: NSTableView ); override;
    procedure onSelectOneItem( tv: NSTableView; selection: NSIndexSet ); override;
  end;

  { TCocoaListControlStringList }

  TCocoaListControlStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner: NSTableView;
    // some notificaitons (i.e. selection change)
    // should not be passed to LCL while clearing
    isClearing: Boolean;
    constructor Create(AOwner: NSTableView);
    procedure Clear; override;
  end;

implementation

{ TLCLListControlCallback }

constructor TLCLListControlCallback.Create(AOwner: NSObject;
  ATarget: TWinControl; AHandleFrame: NSView);
begin
  inherited;
  _selectionIndexSet:= NSMutableIndexSet.new;
  _checkedIndexSet:= NSMutableIndexSet.new;
end;

destructor TLCLListControlCallback.Destroy;
begin
  _selectionIndexSet.release;
  _checkedIndexSet.release;
  inherited Destroy;
end;

function TLCLListControlCallback.selectionIndexSet: NSMutableIndexSet;
begin
  Result:= _selectionIndexSet;
end;

function TLCLListControlCallback.checkedIndexSet: NSMutableIndexSet;
begin
  Result:= _checkedIndexSet;
end;

function TLCLListControlCallback.GetItemCheckedAt(row: Integer;
  var CheckState: Integer): Boolean;
var
  BoolState : array [Boolean] of Integer = (NSOffState, NSOnState);
begin
  CheckState := BoolState[self.checkedIndexSet.containsIndex(row)];
  Result := true;
end;

procedure TLCLListControlCallback.SetItemCheckedAt(row: Integer;
  CheckState: Integer);
begin
  if CheckState = NSOnState then
    self.checkedIndexSet.addIndex( row )
  else
    self.checkedIndexSet.removeIndex( row );
end;

function TLCLListControlCallback.getItemStableSelection(ARow: Integer): Boolean;
begin
  Result:= selectionIndexSet.containsIndex( ARow );
end;

{ TCocoaTableListControlProcessor }

function TCocoaTableListControlProcessor.getCallback(tv: NSTableView
  ): TLCLListControlCallback;
begin
  Result:= TLCLListControlCallback( tv.lclGetCallback.GetCallbackObject );
end;

procedure TCocoaTableListControlProcessor.onReloadData( tv: NSTableView );
begin
  tv.cancelPreviousPerformRequestsWithTarget_selector_object(
    tv, ObjcSelector('restoreFromStableSelection'), nil );
  tv.performSelector_withObject_afterDelay(
    ObjcSelector('restoreFromStableSelection'), nil, 0 );
end;

procedure TCocoaTableListControlProcessor.onSelectOneItem(tv: NSTableView;
  selection: NSIndexSet);
var
  lclcb: TLCLListControlCallback;
begin
  lclcb:= self.getCallback(tv);
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.selectionIndexSet.removeAllIndexes;
  lclcb.selectionIndexSet.addIndexes( selection );
end;

{ TCocoaListControlStringList }

procedure TCocoaListControlStringList.Changed;
begin
  inherited Changed;
  Owner.reloadData;
end;

constructor TCocoaListControlStringList.Create(AOwner: NSTableView);
begin
  Owner:=AOwner;
  inherited Create;
end;

procedure TCocoaListControlStringList.Clear;
begin
  isClearing := true;
  try
    inherited Clear;
  finally
    isClearing := false;
  end;
end;

end.

