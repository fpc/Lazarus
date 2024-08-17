unit CocoaListControl;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LclType, Controls, ComCtrls,
  CocoaAll, CocoaCallback, CocoaWSCommon, CocoaGDIObjects;

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
    function selectionIndexSet: NSMutableIndexSet;
    function checkedIndexSet: NSMutableIndexSet;
  public
    function ItemsCount: Integer; virtual; abstract;
    procedure GetRowHeight(rowidx: Integer; var height: Integer); virtual; abstract;
    function GetBorderStyle: TBorderStyle; virtual; abstract;

    function GetImageListType( out lvil: TListViewImageList ): Boolean; virtual; abstract;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean; virtual; abstract;
    function GetItemCheckedAt( row: Integer; var CheckState: Integer): Boolean; virtual; abstract;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean; virtual; abstract;
    function GetImageFromIndex(imgIdx: Integer): NSImage; virtual; abstract;

    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String); virtual; abstract;
    procedure SetItemCheckedAt( row: Integer; CheckState: Integer); virtual; abstract;

    function shouldSelectionChange(NewSel: Integer): Boolean; virtual; abstract;

    procedure ColumnClicked(ACol: Integer); virtual; abstract;
    function onAddSubview( aView:NSView ): Boolean; virtual; abstract;

    function drawItem( row: Integer; ctx: TCocoaContext; const r: TRect; state: TOwnerDrawState ): Boolean; virtual; abstract;
    function customDraw( row: Integer; col: Integer; ctx: TCocoaContext; state: TCustomDrawState ): Boolean; virtual; abstract;
    function isCustomDrawSupported: Boolean; virtual; abstract;
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
  inherited;// Create(AOwner, ATarget, AHandleView);
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

