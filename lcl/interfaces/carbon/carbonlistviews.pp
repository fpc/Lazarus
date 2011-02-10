{
                  ------------------------------------------------
                  carbonlistviews.pp  -  Carbon list-like controls
                  ------------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonListViews;

{$mode objfpc}{$H+}

interface

// defines
{$I carbondefines.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  MacOSAll,
 // LCL
  LMessages, LCLMessageGlue, LCLType, LCLProc, Controls, StdCtrls, ComCtrls,
  ImgList, Graphics,
 // LCL Carbon
  CarbonDef, CarbonPrivate, CarbonGDIObjects;

type
  TCarbonDataBrowser = class;

  { TCarbonListColumn }

  TCarbonListColumn = class
  private
    FOwner: TCarbonDataBrowser;
    FListColumn: TListColumn;
    FDesc: DataBrowserListViewColumnDesc;
    FVisible: Boolean;
    FWidth: Integer;
    FIndex: Integer; // index of TListColumn
    fTextWithIcon: Boolean;

    procedure UpdateHeader;
    function GetHeaderWidth: UInt16;
    procedure SetHeaderWidth(AWidth: Integer);
    
    function PropertyID: DataBrowserPropertyID;
  protected
    function GetHeaderPropertyType: DataBrowserPropertyType; virtual;
    function GetHeaderPropertyFlags: Integer; virtual;
  public
    constructor Create(AOwner: TCarbonDataBrowser; APropertyID: DataBrowserPropertyID;
      AListColumn: TListColumn);
    destructor Destroy; override;

    procedure Add;
    procedure Remove;
    procedure ReCreate;
    
    procedure UpdateIndex;
  public
    function GetWidth: Integer;
    procedure SetAlignment(AAlignment: TAlignment);
    // TODO: SetAutoSize
    procedure SetCaption(const ACaption: String);
    procedure SetImageIndex(AImageIndex: Integer);
    procedure SetMinWidth(AMinWidth: Integer);
    procedure SetMaxWidth(AMaxWidth: Integer);
    procedure SetVisible(AVisible: Boolean);
    procedure SetWidth(AWidth: Integer; AAutoSize: Boolean);
    property TextWithIcon: Boolean read fTextWithIcon write fTextWithIcon;
  end;

  { TCarbonCheckListColumn }

  TCarbonCheckListColumn = class(TCarbonListColumn)
  protected
    function GetHeaderPropertyType: DataBrowserPropertyType; override;
  public
    constructor Create(AOwner: TCarbonDataBrowser);
    destructor Destroy; override;
  end;
  
  { TCarbonCaptionListColumn }

  TCarbonCaptionListColumn = class(TCarbonListColumn)
  protected
    function GetHeaderPropertyType: DataBrowserPropertyType; override;
  public
    constructor Create(AOwner: TCarbonDataBrowser);
    destructor Destroy; override;
  end;

  { TCarbonDataBrowser }

  // TODO: images
  TCarbonDataBrowser = class(TCarbonControl)
  private
    FColumns: TObjectList; // of TCarbonListColumn, indexed by col PropertyIDs
    FCheckListColumn: TCarbonCheckListColumn;
    FCaptionListColumn: TCarbonCaptionListColumn;
    FHeaderHeight: UInt16;
    FScrollBars: TScrollStyle;
    FItemIndex: Integer; // focused item
    FItemsCheck: TList;
    NotifySelectionChange: Boolean;

    function SetItemIndexQuiet(AIndex: Integer): Boolean;
    function GetHeaderHeight: UInt16;
    function GetItemsHeight: UInt16;
    function GetBorderSize: Integer;
    function GetInsertIndex(AColumn: TCarbonListColumn): Integer;
    procedure RegisterOwnerDrawEvent;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    procedure RegisterEvents; override;
  protected
    function GetItemCaption(AIndex, ASubIndex: Integer): String; virtual; abstract;
    function GetItemIcon(AIndex, ASubIndex: Integer): IconRef; virtual;
    function GetReadOnly: Boolean; virtual; abstract;
    function MultiSelect: Boolean; virtual; abstract;
    function IsOwnerDrawn: Boolean; virtual; abstract;
  protected  
    function DataCallBack(ID: DataBrowserItemId; PropID: DataBrowserPropertyID; 
      Data: DataBrowserItemDataRef; ASetValue: Boolean): OSStatus; virtual;
    procedure NotificationCallBack(ID: DataBrowserItemId; 
      AMessage: DataBrowserItemNotification); virtual;

  public
    procedure BoundsChanged; override;
    function GetClientRect(var ARect: TRect): Boolean; override;
    procedure DrawItem(AIndex: Integer; AState: DataBrowserItemState); virtual; abstract;
    procedure CheckChanged(AIndex: Integer; AChecked: Boolean); virtual;
    procedure SelectionChanged(AIndex: Integer; ASelect: Boolean); virtual;
    procedure FocusedChanged(AIndex: Integer); virtual; abstract;
  public
    procedure CheckNeedsScrollBars;
    
    function GetItemChecked(AIndex: Integer): Boolean;
    function GetItemIndex: Integer;
    function GetItemsRect: TRect;
    function GetItemsCount: Integer;
    function GetItemRect(AIndex: Integer): TRect;
    function GetItemRect(AIndex, ASubIndex: Integer; ACode: TDisplayCode): TRect;
    function GetItemSelected(AIndex: Integer): Boolean;
    function GetItemState(AIndex: Integer; AState: TListItemState; out AIsSet: Boolean): Boolean;
    function GetItemAt(X, Y: Integer): Integer;
    function GetTopItem: Integer;
    function GetSelCount: UInt32;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;
    
    procedure SelectItem(AIndex: Integer; ASelect: Boolean);
    procedure SetBorderStyle(ABorderStyle : TBorderStyle);
    procedure SetItemChecked(AIndex: Integer; AChecked: Boolean);
    procedure SetItemIndex(AItemIndex: Integer);
    procedure SetItemState(AIndex: Integer; AState: TListItemState; AIsSet: Boolean);
    procedure SetItemsHeight(AHeight: Integer);
    procedure SetOwnerDraw(AOwnerDraw: Boolean);
    procedure SetRowSelect(ARowSelect: Boolean);
    procedure SetScrollBars(AScrollBars: TScrollStyle);
    procedure SetSelectionMode(AExtendedSelect, AMultiSelect: Boolean);
    procedure SetTopItem(AIndex: Integer);
    procedure SetViewOrigin(const AOrigin: TPoint);
    procedure ShowAsList(AShow: Boolean);
    procedure ShowCheckboxes(AShow: Boolean);
    procedure ShowItem(AIndex: Integer; Partial: Boolean);
    procedure ShowColumnHeaders(AShow: Boolean);
    
    function GetColumn(AIndex: Integer): TCarbonListColumn;
    procedure DeleteColumn(AIndex: Integer);
    procedure InsertColumn(AIndex: Integer; const AColumn: TListColumn);
    procedure MoveColumn(AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
    procedure UpdateColumnIndex;
    procedure UpdateColumnView; virtual;

    procedure ClearItems;
    procedure DeleteItem(AIndex: Integer);
    procedure InsertItem(AIndex: Integer);
    procedure UpdateItem(AIndex: Integer);
    procedure UpdateItems;
  end;

  TCarbonListView = class;

  TViewMode = class(TObject)
  protected
    class procedure Apply(View: TCarbonListView); virtual; abstract;
    class procedure Resized(View: TCarbonListView); virtual; abstract;
    class function DataCallBack(View: TCarbonListView; ID: DataBrowserItemId; 
      PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef; 
      ASetValue: Boolean): OSStatus; virtual; abstract;
  end;
  TViewModeClass = class of TViewMode;

  { TReportViewMode }

  TReportViewMode = class(TViewMode)
  protected
    class procedure Apply(View: TCarbonListView); override;
    class procedure Resized(View: TCarbonListView); override;
    class function DataCallBack(View: TCarbonListView; ID: DataBrowserItemId; 
      PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef; 
      ASetValue: Boolean): OSStatus; override;
  end;
  
 
  { TCarbonListView }
  
  TCarbonListView = class(TCarbonDataBrowser)
  private
    FIcons  : TFPList;
    FStyle  : TViewStyle;
    FOwnerData  : Boolean;
    FDestroying : Boolean;
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
    procedure DestroyWidget; override;
    
    function DataCallBack(ID: DataBrowserItemId; PropID: DataBrowserPropertyID; 
      Data: DataBrowserItemDataRef; ASetValue: Boolean): OSStatus; override;
  protected
    function GetItemCaption(AIndex, ASubIndex: Integer): String; override;
    function GetItemIcon(AIndex, ASubIndex: Integer): IconRef; override;
    function GetReadOnly: Boolean; override;
    function MultiSelect: Boolean; override;
    function IsOwnerDrawn: Boolean; override;
  public
    constructor Create(const AObject: TWinControl; const AParams: TCreateParams);
    destructor Destroy; override;

    procedure DrawItem(AIndex: Integer; AState: DataBrowserItemState); override;
    procedure SelectionChanged(AIndex: Integer; ASelect: Boolean); override;
    procedure FocusedChanged(AIndex: Integer); override;

    procedure UpdateColumnView; override;

    procedure ClearIconCache;
    
    procedure SetViewStyle(AStyle: TViewStyle);

    procedure DoColumnClicked(MouseX,MouseY: Integer);
    procedure SetItemsCount(ACount: Integer); 
    function NeedDeliverMouseEvent(Msg: Integer; const AMessage): Boolean; override;
    property OwnerData: Boolean read FOwnerData write FOwnerData;
  end;
  
  { TCarbonListBox }

  TCarbonListBox = class(TCarbonDataBrowser)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  protected
    function GetItemCaption(AIndex, ASubIndex: Integer): String; override;
    function GetReadOnly: Boolean; override;
    function MultiSelect: Boolean; override;
    function IsOwnerDrawn: Boolean; override;
  public
    procedure DrawItem(AIndex: Integer; AState: DataBrowserItemState); override;
    procedure SelectionChanged(AIndex: Integer; ASelect: Boolean); override;
    procedure FocusedChanged(AIndex: Integer); override;
  end;
  
  { TCarbonCheckListBox }

  TCarbonCheckListBox = class(TCarbonListBox)
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure CheckChanged(AIndex: Integer; AChecked: Boolean); override;
  end;
  
const
  CheckPropertyID   = 1024;
  CaptionPropertyID = 1025;

implementation

uses InterfaceBase, CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonStrings,
     CarbonCanvas;
     
var CarbonItemDataCallBackUPP        : DataBrowserItemDataUPP;
    CarbonItemNotificationCallBackUPP: DataBrowserItemNotificationUPP;
    CarbonDrawItemCallBackUPP        : DataBrowserDrawItemUPP;
    
const 
  ListViewModes : array [TViewStyle] of TViewModeClass = 
    (TReportViewMode, TReportViewMode, TReportViewMode, TReportViewMode);    
    

function GetIconRefFromBitmap(bmp: TBitmap; IconSize: Integer): IconRef;
var
  image     : TCarbonBitmap;
  context   : CGContextRef;
  ColSpace  : CGColorSpaceRef;
  data      : array of byte;
  iconHnd   : IconFamilyHandle;
  tmpHnd    : MacOSAll.Handle;
  i,c,sz    : Integer;
  dataType  : Integer;
  maskType  : Integer;

begin
  Result := nil;
  if not Assigned(bmp) then Exit;
  if not CheckBitmap(bmp.Handle, 'GetIconRefFromBitmap', 'bmp') then Exit;

  image := TCarbonBitmap(bmp.Handle);
  sz := IconSize;
  case sz of
    16: begin
      dataType := kSmall32BitData;
      maskType := kSmall8BitMask;
    end;
    32: begin
      dataType := kLarge32BitData;
      maskType := kLarge8BitMask;
    end;
    128: begin
      dataType := kThumbnail32BitData;
      maskType := kThumbnail8BitMask;
    end;
  else
    dataType := kHuge32BitData;
    maskType := kHuge8BitMask;
  end;
  SetLength(data,IconSize*IconSize*4);

  ColSpace := CGColorSpaceCreateDeviceRGB;
  if (ColSpace = nil) then Exit;

  // intel-order bitmap
  context := CGBitmapContextCreate(@data[0], sz, sz, 8, sz * 4, ColSpace, kCGImageAlphaPremultipliedFirst);
  if not Assigned(context) then Exit;

  if not Assigned(image.CGImage) then image.UpdateImage;
  CGContextDrawImage(context, GetCGRect(0, 0, sz, sz), image.CGImage );
  CGContextRelease(context);
  CGColorSpaceRelease(ColSpace);

  //samples, stated, that NewHandle() must be called with size set to zero
  //rather than 8, on 10.4 or higher. dunno why,
  //but calling with any specified size fails icon creation.
  //The code "iconHnd^^.resourceType" looks dangerous , but works.
  iconHnd := IconFamilyHandle(NewHandle(0));
  if (iconHnd = nil) then Exit;
  iconHnd^^.resourceType := kIconFamilyType;
  iconHnd^^.resourceSize := sizeof(OSType) + sizeof(Size);

  if PtrToHand(@data[0], tmpHnd, length(data)) = noErr then
  begin
    OSError(
       SetIconFamilyData(iconHnd, dataType, tmpHnd),
       'GetIconRefFromBitmap', 'SetIconFamilyData');
    DisposeHandle(tmpHnd);
  end;

  //it's the following code is Intel only? or is it fine for PowerPC too?
  //combining alpha into single mask array based on byte of BGRA
  //{$ifdef LITTLE_ENDIAN} c:=0;{$else}c:=3;{$endif}
  c := 0;
  for i := 0 to sz*sz - 1 do begin
    data[i] := data[c];
    inc(c, 4);
  end;

  if PtrToHand(@data[0], tmpHnd, sz*sz) = noErr then
  begin
    OSError(
      SetIconFamilyData(iconHnd, maskType, tmpHnd),
      'GetIconRefFromBitmap', 'SetIconFamilyData');
    DisposeHandle(tmpHnd);
  end;

  OSError(
    GetIconRefFromIconFamilyPtr( iconHnd^^, GetHandleSize(MacOSAll.Handle(iconHnd)), Result),
    'GetIconRefFromBitmap', 'GetIconRefFromIconFamilyPtr');
  DisposeHandle(MacOSAll.Handle(iconHnd));
end;

function GetIconRefFromImageList(Images: TCustomImageList; AIndex: Integer; WantedIconSize: Integer): IconRef;
var
  iconbmp : TBitmap;
begin
  if not Assigned(Images) or (AIndex < 0) or (AIndex >= Images.Count) or
    (Images.Width = 0) or (Images.Height = 0) then
  begin
    Result := nil;
    Exit;
  end;

  iconbmp := TBitmap.Create;
  iconbmp.PixelFormat := pf32bit;
  iconbmp.SetSize(Images.Width, Images.Height);

  Images.GetBitmap(AIndex, iconbmp);
  Result := GetIconRefFromBitmap(iconbmp, WantedIconSize);

  iconbmp.Free;
end;

{ TCarbonListColumn }

procedure TCarbonListColumn.UpdateHeader;
begin
  OSError(SetDataBrowserListViewHeaderDesc(FOwner.Widget, PropertyID, FDesc.headerBtnDesc),
    Self, 'SetHeaderDesc', 'SetDataBrowserListViewHeaderDesc');
end;

function TCarbonListColumn.GetHeaderWidth: UInt16;
begin
  OSError(GetDataBrowserTableViewNamedColumnWidth(FOwner.Widget, PropertyID, Result),
    Self, 'GetHeaderWidth', 'GetDataBrowserTableViewNamedColumnWidth');
end;

procedure TCarbonListColumn.SetHeaderWidth(AWidth: Integer);
begin
  OSError(SetDataBrowserTableViewNamedColumnWidth(FOwner.Widget, PropertyID, AWidth),
    Self, 'SetHeaderWidth', 'SetDataBrowserTableViewNamedColumnWidth');

  FOwner.CheckNeedsScrollBars;
end;

function TCarbonListColumn.PropertyID: DataBrowserPropertyID;
begin
  Result := FDesc.propertyDesc.propertyID;
end;

function TCarbonListColumn.GetHeaderPropertyType: DataBrowserPropertyType;
begin
  if FOwner.IsOwnerDrawn then
    Result := kDataBrowserCustomType
  else begin
    if fTextWithIcon
      then Result := kDataBrowserIconAndTextType
      else Result := kDataBrowserTextType;
  end;
end;

function TCarbonListColumn.GetHeaderPropertyFlags: Integer;
begin
  Result := kDataBrowserPropertyIsMutable or
            kDataBrowserListViewSelectionColumn or
            kDataBrowserListViewTypeSelectColumn{or
            kDataBrowserListViewSortableColumn or
            kDataBrowserListViewMovableColumn};
end;

constructor TCarbonListColumn.Create(AOwner: TCarbonDataBrowser;
  APropertyID: DataBrowserPropertyID; AListColumn: TListColumn);
begin
  FOwner := AOwner;
  FListColumn := AListColumn;
  FVisible := False;
  FWidth := 0;
  FIndex := -1;
  
  FDesc.propertyDesc.propertyID := APropertyID;
  FDesc.propertyDesc.propertyType := GetHeaderPropertyType;
  FDesc.propertyDesc.propertyFlags := GetHeaderPropertyFlags;

  FDesc.headerBtnDesc.version := kDataBrowserListViewLatestHeaderDesc;
  FDesc.headerBtnDesc.minimumWidth := 0;
  FDesc.headerBtnDesc.maximumWidth := $FFFF;
  FDesc.headerBtnDesc.titleOffset := 0;
  FDesc.headerBtnDesc.titleString := nil;
  FDesc.headerBtnDesc.initialOrder := kDataBrowserOrderIncreasing;
  FDesc.headerBtnDesc.btnFontStyle.flags := 0;
  FDesc.headerBtnDesc.btnContentInfo.contentType := kControlContentTextOnly;
end;

destructor TCarbonListColumn.Destroy;
begin
  SetVisible(False);
  
  FreeCFString(FDesc.headerBtnDesc.titleString);
    
  inherited Destroy;
end;

procedure TCarbonListColumn.Add;
begin
  FDesc.propertyDesc.propertyType := GetHeaderPropertyType;
    
  OSError(
    AddDataBrowserListViewColumn(FOwner.Widget, FDesc, FOwner.GetInsertIndex(Self)),
    Self, 'Add', 'AddDataBrowserListViewColumn');
    
  SetHeaderWidth(FWidth)
end;

procedure TCarbonListColumn.Remove;
begin
  OSError(RemoveDataBrowserTableViewColumn(FOwner.Widget, FDesc.propertyDesc.propertyID),
    Self, 'Remove', 'RemoveDataBrowserTableViewColumn');

  FOwner.CheckNeedsScrollBars;
end;

procedure TCarbonListColumn.ReCreate;
begin
  if FVisible then
  begin
    Remove;
    Add;
  end;
end;

procedure TCarbonListColumn.UpdateIndex;
begin
  FIndex := FListColumn.Index;
end;

function TCarbonListColumn.GetWidth: Integer;
begin
  if FVisible then Result := GetHeaderWidth
  else Result := FWidth;
end;

procedure TCarbonListColumn.SetAlignment(AAlignment: TAlignment);
begin
  FDesc.headerBtnDesc.btnFontStyle.flags := FDesc.headerBtnDesc.btnFontStyle.flags or kControlUseJustMask;
  case AAlignment of
    taLeftJustify:  FDesc.headerBtnDesc.btnFontStyle.just := teFlushLeft;
    taRightJustify: FDesc.headerBtnDesc.btnFontStyle.just := teFlushRight;
    taCenter:       FDesc.headerBtnDesc.btnFontStyle.just := teCenter;
  end;
  
  if FVisible then UpdateHeader;
end;

procedure TCarbonListColumn.SetCaption(const ACaption: String);
begin
  FreeCFString(FDesc.headerBtnDesc.titleString);
  
  CreateCFString(ACaption, FDesc.headerBtnDesc.titleString);
  if FVisible then UpdateHeader;
end;

procedure TCarbonListColumn.SetImageIndex(AImageIndex: Integer);
begin
  
  // TODO

  if FVisible then UpdateHeader;
end;

procedure TCarbonListColumn.SetMinWidth(AMinWidth: Integer);
begin
  FDesc.headerBtnDesc.minimumWidth := AMinWidth;
  if FVisible then UpdateHeader;
end;

procedure TCarbonListColumn.SetMaxWidth(AMaxWidth: Integer);
begin
  if AMaxWidth <= 0 then
    FDesc.headerBtnDesc.maximumWidth := $FFFF
  else
    FDesc.headerBtnDesc.maximumWidth := AMaxWidth;
  if FVisible then UpdateHeader;
end;

procedure TCarbonListColumn.SetVisible(AVisible: Boolean);
begin
  if AVisible = FVisible then Exit;
  FVisible := AVisible;
  
  if FVisible then Add
  else Remove;
end;

procedure TCarbonListColumn.SetWidth(AWidth: Integer; AAutoSize: Boolean);
var
  lBmp: TBitmap;
begin
  // Implements Column Autosizing
  if AAutoSize then
  begin
    lBmp := TBitmap.Create;
    // The standard Mac listview font is quite bigger then the standard TCanvas font
    // plus, we also need an extra spacing
    FWidth := lBmp.Canvas.TextWidth(FListColumn.Caption) * 2;
    lBmp.Free;
  end
  else
    FWidth := AWidth;

  if FVisible then SetHeaderWidth(FWidth);
end;

{ TCarbonCheckListColumn }

function TCarbonCheckListColumn.GetHeaderPropertyType: DataBrowserPropertyType;
begin
  Result := kDataBrowserCheckboxType;
end;

constructor TCarbonCheckListColumn.Create(AOwner: TCarbonDataBrowser);
begin
  FListColumn := TListColumn.Create(nil);
  FListColumn.Width := GetCarbonThemeMetric(kThemeMetricCheckBoxWidth, 18) + 4;
  
  inherited Create(AOwner, CheckPropertyID, FListColumn);
  
  FDesc.headerBtnDesc.minimumWidth := FListColumn.Width;
  FDesc.headerBtnDesc.maximumWidth := FListColumn.Width;
  FDesc.headerBtnDesc.btnContentInfo.contentType := kControlContentTextOnly;
  
  FWidth := FListColumn.Width;
  
  SetVisible(True);
end;

destructor TCarbonCheckListColumn.Destroy;
begin
  FListColumn.Free;
  
  inherited Destroy;
end;

{ TCarbonCaptionListColumn }

function TCarbonCaptionListColumn.GetHeaderPropertyType: DataBrowserPropertyType;
begin
  if FOwner.IsOwnerDrawn then
    Result := kDataBrowserCustomType
  else
    Result := kDataBrowserTextType;
end;

constructor TCarbonCaptionListColumn.Create(AOwner: TCarbonDataBrowser);
begin
  FListColumn := TListColumn.Create(nil);
  FListColumn.Width := $FFFF;
  
  inherited Create(AOwner, CaptionPropertyID, FListColumn);
  
  FWidth := FListColumn.Width;
  
  SetVisible(True);
end;

destructor TCarbonCaptionListColumn.Destroy;
begin
  FListColumn.Free;
  
  inherited Destroy;
end;

{ TCarbonDataBrowser }

{------------------------------------------------------------------------------
  Function: CarbonItemDataCallBack

  Responds to Data Browser requests for data and states
 ------------------------------------------------------------------------------}
function CarbonItemDataCallBack(AControl: ControlRef; ID: DataBrowserItemId;
  PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef;
  SetValue: Boolean): OSStatus; {$IFDEF darwin} mwpascal;{$ENDIF}
var
  ACarbonDataBrowser: TCarbonDataBrowser;
begin
  Result := noErr;
  // DebugLn('CarbonItemDataCallBack ID: ' + DbgS(ID));
  ACarbonDataBrowser := TCarbonDataBrowser(GetCarbonControl(AControl));
  if ACarbonDataBrowser = nil then Exit;
  
  Result := ACarbonDataBrowser.DataCallBack(ID, PropID, Data, SetValue)
end;

{------------------------------------------------------------------------------
  Function: CarbonItemNotificationCallBack

  Called by DataBrowser when items are selected/deselected
 ------------------------------------------------------------------------------}
procedure CarbonItemNotificationCallBack(AControl: ControlRef;
  ID: DataBrowserItemId; AMessage: DataBrowserItemNotification);
  {$IFDEF darwin} mwpascal;{$ENDIF}
var
  ACarbonDataBrowser: TCarbonDataBrowser;
begin
  // DebugLn('CarbonItemNotificationCallBack ID: ' + DbgS(ID));
  ACarbonDataBrowser := TCarbonDataBrowser(GetCarbonControl(AControl));
  if ACarbonDataBrowser = nil then Exit;
  ACarbonDataBrowser.NotificationCallBack(ID, AMessage);
end;

{------------------------------------------------------------------------------
  Function: CarbonDrawItemCallBack

  Handles draw requests from DataBrowser when in ownerdrawn style
 ------------------------------------------------------------------------------}
procedure CarbonDrawItemCallBack(AControl: ControlRef;
  ID: DataBrowserItemID; PropID: DataBrowserPropertyID;
  State: DataBrowserItemState; const R: Rect; Depth: SInt16;
  ColorDevice: Boolean); {$IFDEF darwin} mwpascal;{$ENDIF}
var
  ACarbonDataBrowser: TCarbonDataBrowser;
begin
  // DebugLn('CarbonDrawItemCallBack ID: ' + DbgS(ID) + ' R: ' + DbgS(R));
  ACarbonDataBrowser := TCarbonDataBrowser(GetCarbonControl(AControl));
  if ACarbonDataBrowser = nil then Exit;

  ACarbonDataBrowser.DrawItem(ID - 1, State);
end;

function TCarbonDataBrowser.SetItemIndexQuiet(AIndex: Integer): Boolean;
begin
  if (AIndex >= -1) and (AIndex < GetItemsCount) then
  begin
    FItemIndex := AIndex;
    Result := AIndex <> -1;
  end
  else
    Result := False;
end;

function TCarbonDataBrowser.GetHeaderHeight: UInt16;
begin
  OSError(GetDataBrowserListViewHeaderBtnHeight(Widget, Result),
    Self, 'GetHeaderHeight', 'GetDataBrowserListViewHeaderBtnHeight');
end;

function TCarbonDataBrowser.GetItemsHeight: UInt16;
begin
  OSError(GetDataBrowserTableViewRowHeight(Widget, Result),
      Self, 'GetItemsHeight', 'GetDataBrowserTableViewRowHeight');
end;

function TCarbonDataBrowser.GetBorderSize: Integer;
var
  HasFrame: Boolean;
begin
  Result := 0;

  OSError(GetControlData(Widget, kControlNoPart,
      kControlDataBrowserIncludesFrameAndFocusTag, SizeOf(HasFrame), @HasFrame, nil),
    Self, 'GetBorderSize', SGetData);
    
  if HasFrame then Result := 3;
end;

function TCarbonDataBrowser.GetInsertIndex(AColumn: TCarbonListColumn): Integer;
var
  I, Index: Integer;
begin
  Result := 0;
  
  if AColumn is TCarbonCheckListColumn then Exit;
  if FCheckListColumn <> nil then Inc(Result);
  if AColumn is TCarbonCaptionListColumn then Exit;

  Index := AColumn.FListColumn.Index;
  
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I] = nil then Continue;
        
    with TCarbonListColumn(FColumns[I]) do
      if (FIndex < Index) and FVisible then
        Inc(Result);
  end;
end;

procedure TCarbonDataBrowser.RegisterOwnerDrawEvent;
var
  CustomCallbacks: DataBrowserCustomCallbacks;
const
  SName = 'RegisterOwnerDrawEvent';
begin
  CustomCallbacks.version := kDataBrowserLatestCustomCallbacks;
  // Init data browser custom callbacks
  OSError(
    InitDataBrowserCustomCallbacks(CustomCallbacks),
    Self, SName, 'InitDataBrowserCustomCallbacks');

  if IsOwnerDrawn then
    CustomCallbacks.drawItemCallback := CarbonDrawItemCallBackUPP;

  // Set data browser custom callbacks
  OSError(
    SetDataBrowserCustomCallbacks(Widget, CustomCallbacks),
    Self, SName, 'SetDataBrowserCustomCallbacks');
end;

procedure TCarbonDataBrowser.CreateWidget(const AParams: TCreateParams);
begin
  if OSError(CreateDataBrowserControl(
      GetTopParentWindow, ParamsToCarbonRect(AParams), kDataBrowserListView, Widget),
    Self, SCreateWidget, 'CreateDataBrowserControl') then RaiseCreateWidgetError(LCLObject);

  // set 2px inset for each cell
  OSError(DataBrowserSetMetric(Widget, kDataBrowserMetricCellContentInset, False, 2.0),
    Self, SCreateWidget, 'DataBrowserSetMetric');
    
  // get initial header height
  FHeaderHeight := GetHeaderHeight;
    
  // set variable columns and fixed rows
  OSError(SetDataBrowserTableViewGeometry(Widget, True, False),
    Self, SCreateWidget, 'SetDataBrowserTableViewGeometry');
    
  FCheckListColumn := nil;
  FCaptionListColumn := nil;
  
  FColumns := TObjectList.Create(True);
  FItemIndex := -1;
  
  FItemsCheck := TList.Create;
  
  inherited;
end;

procedure TCarbonDataBrowser.DestroyWidget;
begin
  FCaptionListColumn.Free;
  FCheckListColumn.Free;
  FColumns.Free;
  
  inherited DestroyWidget;

  FItemsCheck.Free;
end;

procedure TCarbonDataBrowser.RegisterEvents;
var
  Callbacks: DataBrowserCallbacks;
const
  SName = 'RegisterEvents';
begin
  inherited RegisterEvents;

  Callbacks.version := kDataBrowserLatestCallbacks;
  // init data browser callbacks
  OSError(
    InitDataBrowserCallbacks(Callbacks),
    Self, SName, 'InitDataBrowserCallbacks');
    
  Callbacks.itemDataCallback := CarbonItemDataCallBackUPP;
  Callbacks.itemNotificationCallback := CarbonItemNotificationCallBackUPP;
  // Set data browser callbacks
  OSError(
    SetDataBrowserCallbacks(Widget, Callbacks),
    Self, SName, 'SetDataBrowserCallbacks');

  RegisterOwnerDrawEvent;
end;

procedure TCarbonDataBrowser.BoundsChanged;
begin
  inherited BoundsChanged;
  
  CheckNeedsScrollBars;
end;

function TCarbonDataBrowser.GetClientRect(var ARect: TRect): Boolean;
begin
  Result := inherited GetClientRect(ARect);
  
  InflateRect(ARect, -GetBorderSize, -GetBorderSize); // without border
  Inc(ARect.Top, GetHeaderHeight); // without header
  // without scroll bars?
end;

procedure TCarbonDataBrowser.CheckChanged(AIndex: Integer; AChecked: Boolean);
begin
  SetItemChecked(AIndex, AChecked);
end;

procedure TCarbonDataBrowser.SelectionChanged(AIndex: Integer; ASelect: Boolean);
begin
  if ASelect then
  begin
    if (AIndex <> FItemIndex) and SetItemIndexQuiet(AIndex) then
      FocusedChanged(AIndex);
  end
  else
    FocusedChanged(FItemIndex);
end;

procedure TCarbonDataBrowser.CheckNeedsScrollBars;
var
  ShowHorz, ShowVert, Horz, Vert: Boolean;
  R, C: TRect;
  SX, SY: UInt32;
const
  SName = 'SetScrollBars';
begin
  GetClientRect(C);
  R := GetItemsRect;
  
  Horz := (C.Right - C.Left) < (R.Right - R.Left);
  Vert := (C.Bottom - C.Top) < (R.Bottom - R.Top);
  ShowHorz := (FScrollBars in [ssHorizontal, ssBoth]) or
    ((FScrollBars in [ssAutoHorizontal, ssAutoBoth]) and Horz);
  ShowVert := (FScrollBars in [ssVertical, ssBoth]) or
    ((FScrollBars in [ssAutoVertical, ssAutoBoth]) and Vert);

  OSError(GetDataBrowserScrollPosition(Widget, SY, SX), // !!! top, left
    Self, SName, 'GetDataBrowserScrollPosition');

  OSError(SetDataBrowserHasScrollBars(Widget, ShowHorz, ShowVert),
    Self, SName, 'SetDataBrowserHasScrollBars');

  // adjust scroll pos
  if not Horz then SX := 0
  else
    if SX > UInt32(R.Right - (C.Right - C.Left)) then
      SX := UInt32(R.Right - (C.Right - C.Left));

  if not Vert then SY := 0
  else
    if SY > UInt32(R.Bottom - (C.Bottom - C.Top)) then
      SY := UInt32(R.Bottom - (C.Bottom - C.Top));

  OSError(SetDataBrowserScrollPosition(Widget, SY, SX), // !!! top, left
    Self, SName, 'SetDataBrowserScrollPosition');
end;

procedure TCarbonDataBrowser.UpdateItems;
var
  i : Integer;
begin
  {// removes all and adds new count of items starting with index 1
  OSError(RemoveDataBrowserItems(Widget, kDataBrowserNoItem, 0, nil, kDataBrowserItemNoProperty),
    Self, 'UpdateItems', 'RemoveDataBrowserItems');

  if GetItemsCount <> 0 then
    OSError(AddDataBrowserItems(Widget, kDataBrowserNoItem, GetItemsCount, nil, kDataBrowserItemNoProperty),
      Self, 'UpdateItems', 'AddDataBrowserItems');}
  for i:=0 to GetItemsCount-1 do UpdateItem(i);

  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.ClearItems;
begin
  FItemsCheck.Clear;
  
  OSError(RemoveDataBrowserItems(Widget, kDataBrowserNoItem, 0, nil, kDataBrowserItemNoProperty),
    Self, 'ClearItems', 'RemoveDataBrowserItems');

  CheckNeedsScrollBars;
end;

function TCarbonDataBrowser.GetItemChecked(AIndex: Integer): Boolean;
begin
  if (AIndex >= 0) and (AIndex < GetItemsCount) then
    Result := Assigned(FItemsCheck[AIndex])
  else
    Result := False;
end;

function TCarbonDataBrowser.GetItemIndex: Integer;
begin
  if (FItemIndex < 0) or (FItemIndex >= GetItemsCount) then
    FItemIndex := -1;
    
  Result := FItemIndex;
end;

function TCarbonDataBrowser.GetItemsRect: TRect;
var
  R: TRect;
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  
  if FCaptionListColumn <> nil then
  begin
    GetClientRect(R);
    Result.Right := R.Right - R.Left;
  end
  else
  begin
    Result.Right := 0;
    
    if FCheckListColumn <> nil then
      Inc(Result.Right, FCheckListColumn.FListColumn.Width);
    
    for I := 0 to FColumns.Count - 1 do
    begin
      if FColumns[I] = nil then Exit;
      if TCarbonListColumn(FColumns[I]).FVisible then
        Inc(Result.Right, TCarbonListColumn(FColumns[I]).FListColumn.Width);
    end;
  end;
  
  Result.Bottom := GetItemsCount * GetItemsHeight;
end;

function TCarbonDataBrowser.GetItemsCount: Integer;
begin
  Result := FItemsCheck.Count;
end;

function TCarbonDataBrowser.GetItemRect(AIndex: Integer): TRect;
begin
  Result := GetItemRect(AIndex, 0, drBounds);
end;

function TCarbonDataBrowser.GetItemRect(AIndex, ASubIndex: Integer; ACode: TDisplayCode): TRect;
var
  P: TPoint;
  R: TRect;
begin
  P := GetViewOrigin;
  GetClientRect(R);
  
  // TODO: ASubIndex, ACode
  
  Result.Left := R.Left - P.X;
  Result.Top := R.Top + GetHeaderHeight - P.Y + AIndex * GetItemsHeight;
  Result.Right := R.Right;
  Result.Bottom := Result.Top + GetItemsHeight;
end;

function TCarbonDataBrowser.GetItemSelected(AIndex: Integer): Boolean;
begin
  GetItemState(AIndex, lisSelected, Result);
end;

function TCarbonDataBrowser.GetItemState(AIndex: Integer;
  AState: TListItemState; out AIsSet: Boolean): Boolean;
var
  S: DataBrowserItemState;
begin
  Result := False;
  OSError(GetDataBrowserItemState(Widget, AIndex + 1, S),
    Self, 'GetItemState', 'GetDataBrowserItemState');
    
  Result := True;
  case AState of
    lisDropTarget: AIsSet := (S and kDataBrowserItemIsDragTarget) <> 0;
    lisFocused:    AIsSet := AIndex = FItemIndex;
    lisSelected:   AIsSet := (S and kDataBrowserItemIsSelected) <> 0;
  else
    Result := False;
  end;
end;

function TCarbonDataBrowser.GetItemAt(X, Y: Integer): Integer;
var
  P: TPoint;
  R: TRect;
begin
  Result := 0;
    
  P := GetViewOrigin;
  GetClientRect(R);
  
  Result := (Y - R.Top - GetHeaderHeight + P.Y) div GetItemsHeight;
  if (Result < 0) or (Result >= GetitemsCount) then Result := -1;
end;

function TCarbonDataBrowser.GetItemIcon(AIndex, ASubIndex: Integer): IconRef;
begin
  Result := nil;
end;

function TCarbonDataBrowser.DataCallBack(ID: DataBrowserItemId;  
  PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef; ASetValue: Boolean): OSStatus;
var
  CheckboxValue: ThemeButtonValue;
  CheckboxState: ThemeDrawState;
  CFString: CFStringRef;
  ItemIcon: IconRef;
  SubIndex: Integer;
 
begin
  if (ID < 1) or (ID > DataBrowserItemId(GetItemsCount)) then
  begin
    Result := errDataBrowserItemNotFound;
    Exit;
  end;
  
  if ASetValue then
  begin
    if PropID = CheckPropertyID then // check has changed
    begin
      Result := GetDataBrowserItemDataButtonValue(Data, CheckboxValue);
      if Result <> noErr then Exit;

      CheckChanged(ID - 1, CheckboxValue = kThemeButtonOn);
    end;
    Exit;
  end;

  case PropID of
    kDataBrowserItemIsActiveProperty:
      Result := SetDataBrowserItemDataBooleanValue(Data, LCLObject.Enabled);
    kDataBrowserItemIsSelectableProperty:
      Result := SetDataBrowserItemDataBooleanValue(Data, LCLObject.Enabled);
    kDataBrowserItemIsEditableProperty:
      Result := SetDataBrowserItemDataBooleanValue(Data, not GetReadOnly);
    kDataBrowserItemIsContainerProperty:
      Result := SetDataBrowserItemDataBooleanValue(Data, False);
    CheckPropertyID:
      begin
        if GetItemChecked(ID - 1) then CheckboxValue := kThemeButtonOn
        else CheckboxValue := kThemeButtonOff;
        
        Result := SetDataBrowserItemDataButtonValue(Data, CheckboxValue);
        if Result <> noErr then Exit;
        
        if LCLObject.Enabled then CheckboxState := kThemeStateActive
        else CheckboxState := kThemeStateInactive;
        
        Result := SetDataBrowserItemDataDrawState(Data, CheckboxState);
      end;
    else
      begin
        if (PropID >= CaptionPropertyID) and
          (PropID <= CaptionPropertyID + DataBrowserPropertyID(FColumns.Count)) then
        begin
          if PropID = CaptionPropertyID then
          begin
            SubIndex := 0;
          end else begin
            SubIndex :=
              TCarbonListColumn(FColumns[PropID - CaptionPropertyID - 1]).FListColumn.Index;
          end;
          
          CFString:=nil;
          CreateCFString(GetItemCaption(ID - 1, SubIndex), CFString);
          try
            SetDataBrowserItemDataText(Data, CFString);
          finally
            FreeCFString(CFString);
          end;

          ItemIcon := GetItemIcon(ID-1, SubIndex);
          if Assigned(ItemIcon) then
            OSError(
              SetDataBrowserItemDataIcon(Data, ItemIcon),
              'CarbonItemDataCallBack', 'SetDataBrowserItemDataIcon');

        end
        else
          Result := errDataBrowserPropertyNotFound;
      end;
  end;

end;

procedure TCarbonDataBrowser.NotificationCallBack(ID: DataBrowserItemId; 
  AMessage: DataBrowserItemNotification); 
begin
  if (ID < 1) or (ID > DataBrowserItemId(GetItemsCount)) then Exit;

  if NotifySelectionChange then
    case AMessage of
      kDataBrowserItemSelected:
        SelectionChanged(ID - 1, True);
      kDataBrowserItemDeselected:
        SelectionChanged(ID - 1, False);
      kDataBrowserSelectionSetChanged: // the selection order has changed
        SelectionChanged(ID - 1, True);
      // kDataBrowserItemDoubleClicked:;
    end;

end;

function TCarbonDataBrowser.GetTopItem: Integer;
begin
  Result := GetItemAt(0, GetHeaderHeight);
  if Result < 0 then Result := 0;
end;

function TCarbonDataBrowser.GetSelCount: UInt32;
begin
  Result := 0;

  OSError(GetDataBrowserItemCount(Widget, kDataBrowserNoItem, False,
      kDataBrowserItemIsSelected, Result),
    Self, 'GetSelCount', 'GetDataBrowserItemCount');
end;

function TCarbonDataBrowser.GetViewOrigin: TPoint;
var
  Top, Left: UInt32;
begin
  Result.X := 0;
  Result.Y := 0;
  
  if OSError(GetDataBrowserScrollPosition(Widget, Top, Left),
      Self, 'GetViewOrigin', 'GetDataBrowserScrollPosition') then Exit;

  Result.X := Left;
  Result.Y := Top;
end;

function TCarbonDataBrowser.GetVisibleRowCount: Integer;
var
  R: TRect;
begin
  GetClientRect(R);
  Result := (R.Bottom - R.Top - GetHeaderHeight) div GetItemsHeight;
  if Result < 0 then Result := 0;
end;

procedure TCarbonDataBrowser.SelectItem(AIndex: Integer; ASelect: Boolean);
var
  Option: DataBrowserSetOption;
  ID: DataBrowserItemID;
begin
  if (AIndex < 0) or (AIndex >= GetItemsCount) then Exit;
  if GetItemSelected(AIndex) = ASelect then Exit;

  //DebugLn('TCarbonDataBrowser.SelectItem Index: ' + DbgS(AIndex) + ' Select: ' + DbgS(ASelect));
  
  ID := AIndex + 1; // items in Carbon start with index 1
  if not ASelect then
    Option := kDataBrowserItemsRemove
  else
    if MultiSelect then Option := kDataBrowserItemsAdd
    else Option := kDataBrowserItemsAssign;
    
  OSError(
    SetDataBrowserSelectedItems(Widget, 1, @ID, Option),
    Self, 'SelectItem' , 'SetDataBrowserSelectedItems');
end;

procedure TCarbonDataBrowser.SetBorderStyle(ABorderStyle: TBorderStyle);
var
  HasFrame: Boolean;
begin
  HasFrame := ABorderStyle = bsSingle;
  
  OSError(SetControlData(Widget, kControlNoPart,
      kControlDataBrowserIncludesFrameAndFocusTag, SizeOf(HasFrame), @HasFrame),
    Self, 'SetBorderStyle', SSetData);
end;

procedure TCarbonDataBrowser.SetItemChecked(AIndex: Integer; AChecked: Boolean);
begin
  if (AIndex >= 0) and (AIndex < GetItemsCount) then
  begin
    FItemsCheck[AIndex] := Pointer(Integer(AChecked));
    UpdateItem(AIndex);
  end;
end;

procedure TCarbonDataBrowser.SetItemIndex(AItemIndex: Integer);
begin
  // If we are not in multiselect mode, -1 clears selection
  // otherwise -1 does nothing
  if (AItemIndex = -1) and (not MultiSelect) then SelectItem(AItemIndex, False);
    
  if SetItemIndexQuiet(AItemIndex) then
  begin
    SelectItem(AItemIndex, True);
  end;
end;

procedure TCarbonDataBrowser.SetItemState(AIndex: Integer;
  AState: TListItemState; AIsSet: BooleaN);
begin
  case AState of
    lisFocused:
      if AIsSet then SetItemIndex(AIndex)
      else if AIndex = FItemIndex then SetItemIndex(-1);
    lisSelected:
      SelectItem(AIndex, AIsSet);
  end;
end;

procedure TCarbonDataBrowser.SetItemsHeight(AHeight: Integer);
begin
  if AHeight <> 0 then
    OSError(
      SetDataBrowserTableViewRowHeight(Widget, AHeight),
      Self, 'SetItemsHeight', 'SetDataBrowserTableViewRowHeight');
end;

procedure TCarbonDataBrowser.SetOwnerDraw(AOwnerDraw: Boolean);
var
  I: Integer;
begin
  if FCheckListColumn <> nil then FCheckListColumn.Recreate;
  if FCaptionListColumn <> nil then FCaptionListColumn.Recreate;
  
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I] = nil then Continue;
    TCarbonListColumn(FColumns[I]).Recreate;
  end;
  
  RegisterOwnerDrawEvent;
end;

procedure TCarbonDataBrowser.SetRowSelect(ARowSelect: Boolean);
var
  Style: DataBrowserTableViewHiliteStyle;
begin
  if ARowSelect then
    Style := kDataBrowserTableViewFillHilite
  else
    Style := kDataBrowserTableViewMinimalHilite;

  OSError(SetDataBrowserTableViewHiliteStyle(Widget, Style),
    Self, 'SetRowSelect', 'SetDataBrowserTableViewHiliteStyle');
end;

procedure TCarbonDataBrowser.SetScrollBars(AScrollBars: TScrollStyle);
begin
  FScrollBars := AScrollBars;
  
  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.SetSelectionMode(AExtendedSelect,
  AMultiSelect: Boolean);
var
  Flags: DataBrowserSelectionFlags;
begin
  Flags := kDataBrowserCmdTogglesSelection;
  if not AMultiSelect then
    Flags := Flags or kDataBrowserSelectOnlyOne or kDataBrowserResetSelection;
  if AExtendedSelect then
    Flags := Flags{; or kDataBrowserAlwaysExtendSelection};
  
  OSError(
    SetDataBrowserSelectionFlags(Widget, Flags),
    Self, 'SetSelectionMode', 'SetDataBrowserSelectionFlags');
end;

procedure TCarbonDataBrowser.SetTopItem(AIndex: Integer);
begin
  SetViewOrigin(Classes.Point(0, AIndex * GetItemsHeight));
end;

procedure TCarbonDataBrowser.SetViewOrigin(const AOrigin: TPoint);
begin
  OSError(SetDataBrowserScrollPosition(Widget, AOrigin.Y, AOrigin.X),
    Self, 'SetViewOrigin', 'SetDataBrowserScrollPosition');
end;

procedure TCarbonDataBrowser.ShowAsList(AShow: Boolean);
var
  I: Integer;
begin
  if AShow = (FCaptionListColumn <> nil) then Exit;

  if AShow then
  begin
    if FCaptionListColumn = nil then
      FCaptionListColumn := TCarbonCaptionListColumn.Create(Self);
  end
  else
    FreeAndNil(FCaptionListColumn);
  
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I] = nil then Continue;
    if not AShow then
      TCarbonListColumn(FColumns[I]).SetVisible(TCarbonListColumn(FColumns[I]).FListColumn.Visible)
    else
      TCarbonListColumn(FColumns[I]).SetVisible(False);
  end;

  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.ShowCheckboxes(AShow: Boolean);
begin
  if AShow then
  begin
    if FCheckListColumn = nil then FCheckListColumn := TCarbonCheckListColumn.Create(Self);
  end
  else
    FreeAndNil(FCheckListColumn);

  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.ShowItem(AIndex: Integer; Partial: Boolean);
begin
  // TODO: partial show
  OSError(RevealDataBrowserItem(Widget, AIndex, kDataBrowserNoItem,
    kDataBrowserRevealWithoutSelecting), Self, 'ShowItem', 'RevealDataBrowserItem');
end;

procedure TCarbonDataBrowser.ShowColumnHeaders(AShow: Boolean);
var
  H: UInt16;
begin
  if AShow then
    H := FHeaderHeight
  else
    H := 0;

  OSError(SetDataBrowserListViewHeaderBtnHeight(Widget, H),
    Self, 'ShowColumnHeaders', 'SetDataBrowserListViewHeaderBtnHeight');

  CheckNeedsScrollBars;
end;

function TCarbonDataBrowser.GetColumn(AIndex: Integer): TCarbonListColumn;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I] = nil then Continue;
    //DebugLn('Get Column ' + DbgS(TCarbonListColumn(FColumns[I]).FIndex));
    if TCarbonListColumn(FColumns[I]).FIndex = AIndex then
    begin
      Result := TCarbonListColumn(FColumns[I]);
      Break;
    end;
  end;
end;

procedure TCarbonDataBrowser.DeleteColumn(AIndex: Integer);
var
  C: TCarbonListColumn;
begin
  C := GetColumn(AIndex);
  if C <> nil then FColumns.Remove(C);
  UpdateColumnIndex;
end;

procedure TCarbonDataBrowser.InsertColumn(AIndex: Integer;
  const AColumn: TListColumn);
var
  Pos: Integer;
  C: TCarbonListColumn;
begin
  // find empty item in FColumns list
  Pos := FColumns.IndexOf(nil);
  if Pos < 0 then Pos := FColumns.Count;

  C := TCarbonListColumn.Create(Self, Pos + CaptionPropertyID + 1, AColumn);
  if Pos < FColumns.Count then
    FColumns[Pos] := C
  else
    FColumns.Add(C);
    
  UpdateColumnIndex;
  UpdateColumnView;
end;

procedure TCarbonDataBrowser.MoveColumn(AOldIndex, ANewIndex: Integer;
  const AColumn: TListColumn);
begin
  // TODO
end;

procedure TCarbonDataBrowser.UpdateColumnIndex;
var
  I: Integer;
begin
  for I := 0 to FColumns.Count - 1 do
  begin
    if FColumns[I] = nil then Continue;
    TCarbonListColumn(FColumns[I]).UpdateIndex;
  end;
end;

procedure TCarbonDataBrowser.UpdateColumnView;
begin

end;

procedure TCarbonDataBrowser.DeleteItem(AIndex: Integer);
var
  Item  : DataBrowserItemID;
  i     : Integer;
begin
  Item:=GetItemsCount;

  NotifySelectionChange:=False;
  for i:=AIndex to Item-2 do
    SelectItem(i, IsDataBrowserItemSelected(Widget, i+2));
  NotifySelectionChange:=True;

  FItemsCheck.Delete(AIndex);

  OSError( RemoveDataBrowserItems(Widget, kDataBrowserNoItem, 1, @Item, kDataBrowserItemNoProperty),
    Self, 'DeleteItem', 'RemoveDataBrowserItems');

  for i:=AIndex to GetItemsCount - 1 do UpdateItem(i);

  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.InsertItem(AIndex: Integer);
var
  Item  : DataBrowserItemID;
  i     : Integer;
  oper  : DataBrowserSetOption;
begin
  Item := GetItemsCount+1;
  FItemsCheck.Insert(AIndex, Pointer(False));
  OSError( AddDataBrowserItems(Widget, kDataBrowserNoItem, 1, @Item, kDataBrowserItemNoProperty),
    Self, 'InsertItem', 'AddDataBrowserItems');

  NotifySelectionChange:=False;
  for i := GetItemsCount downto AIndex+2 do begin
    if IsDataBrowserItemSelected(Widget, i-1) then
      oper:=kDataBrowserItemsAdd
    else
      oper:=kDataBrowserItemsRemove;
    SetDataBrowserSelectedItems( Widget, 1, @i, oper);
  end;
  i:=AIndex+1;
  SetDataBrowserSelectedItems( Widget, 1, @i, kDataBrowserItemsRemove);
  NotifySelectionChange:=True;

  for i := AIndex to GetItemsCount-1 do UpdateItem(i);

  CheckNeedsScrollBars;
end;

procedure TCarbonDataBrowser.UpdateItem(AIndex: Integer);
var
  Item: DataBrowserItemID;
begin
  Item := AIndex + 1;
  OSError(UpdateDataBrowserItems(Widget, kDataBrowserNoItem, 1, @Item,
      kDataBrowserItemNoProperty, kDataBrowserNoItem),
    Self, 'UpdateItem', 'UpdateDataBrowserItems');
end;

{ TCarbonListView }

procedure TCarbonListView.CreateWidget(const AParams: TCreateParams);
begin
  inherited;
end;

procedure TCarbonListView.DestroyWidget;  
begin
  FDestroying := True; 
  inherited DestroyWidget;  
end;

function TCarbonListView.DataCallBack(ID: DataBrowserItemId;  
  PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef;  
  ASetValue: Boolean): OSStatus;  
begin
  Result := ListViewModes[FStyle].DataCallBack(Self, ID, PropID, Data, ASetValue);
end;

function TCarbonListView.GetItemCaption(AIndex, ASubIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < (LCLObject as TCustomListView).Items.Count) then
  begin
    if ASubIndex = 0 then
      Result := (LCLObject as TCustomListView).Items[AIndex].Caption
    else
    begin
      if (ASubIndex > 0) and
        (ASubIndex <= (LCLObject as TCustomListView).Items[AIndex].SubItems.Count) then
        Result := (LCLObject as TCustomListView).Items[AIndex].SubItems[ASubIndex - 1]
      else
        Result := '';
    end;
  end
  else
    Result := '';
end;

function TCarbonListView.GetReadOnly: Boolean;
begin
  Result := (LCLObject as TCustomListView).ReadOnly;
end;

function TCarbonListView.MultiSelect: Boolean;
begin
  Result := (LCLObject as TCustomListView).MultiSelect;
end;

function TCarbonListView.IsOwnerDrawn: Boolean;
begin
  Result := False; // TODO
end;

constructor TCarbonListView.Create(const AObject: TWinControl;
  const AParams: TCreateParams);
begin
  inherited Create(AObject, AParams);
  FIcons:=TFPList.Create;
end;

destructor TCarbonListView.Destroy;
begin
  ClearIconCache;
  FIcons.Free;
  inherited Destroy;
end;

procedure TCarbonListView.DrawItem(AIndex: Integer; AState: DataBrowserItemState);
begin
  // TODO
  //DebugLn('TCarbonListView.DrawItem Index: ' + DbgS(AIndex) + ' AState: ' +  DbgS(Integer(AState)));
end;

procedure TCarbonListView.SelectionChanged(AIndex: Integer; ASelect: Boolean);
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  inherited;
  if FDestroying then Exit;
  //DebugLn('TCarbonListView.SelectionChanged Index: ' + DbgS(AIndex) + ' Select: ' +  DbgS(ASelect));
  
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;

  NMLV.iItem := AIndex;

  NMLV.iSubItem := 0;
  if ASelect then
    NMLV.uNewState := LVIS_SELECTED
  else
    NMLV.uOldState := LVIS_SELECTED;

  NMLV.uChanged := LVIF_STATE;

  Msg.NMHdr := @NMLV.hdr;

  DeliverMessage(LCLObject, Msg);
end;

procedure TCarbonListView.FocusedChanged(AIndex: Integer);
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  if FDestroying then Exit;
  FillChar(Msg, SizeOf(Msg), #0);
  FillChar(NMLV, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := LCLObject.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;

  NMLV.iItem := AIndex;

  NMLV.iSubItem := 0;
  NMLV.uNewState := LVIS_FOCUSED;
  NMLV.uChanged := LVIF_STATE;
  
  Msg.NMHdr := @NMLV.hdr;

  DeliverMessage(LCLObject, Msg);
end;

function TCarbonListView.GetItemIcon(AIndex, ASubIndex: Integer): IconRef;
var
  idx   : Integer;
  view  : TListView;
  imgs  : TCustomImageList;
  size  : Integer;
begin
  Result := nil;
  if not Assigned(LCLObject) or not (LCLObject is TListView) or (ASubIndex > 0) then
    Exit;

  view := TListView(LCLObject);
  idx := view.Items[AIndex].ImageIndex;
  if view.ViewStyle <> vsIcon then begin
    imgs := view.SmallImages;
    size := 16;
  end else begin
    imgs := view.LargeImages;
    size := 32; // larger icons?
  end;
  if not Assigned(imgs) or (idx < 0) or (idx >= imgs.Count) then Exit;

  if FIcons.Count < imgs.Count then FIcons.Count := imgs.Count;

  if not Assigned(FIcons[idx]) then
  begin
    Result := GetIconRefFromImageList(imgs, idx, size);
    FIcons[idx] := Result;
  end
  else
    Result := IconRef(FIcons[idx]);
end;

procedure TCarbonListView.UpdateColumnView;
var
  view: TListView;
  firstIconed  : Boolean;
  c : TCarbonListColumn;
begin
  view := TListView(LCLObject);
  if not Assigned(view) then Exit;

  if (view.ViewStyle = vsReport) and (FColumns.Count > 0) then
  begin
    firstIconed := Assigned(view.SmallImages);
    C := TCarbonListColumn(FColumns[0]);
    if C.TextWithIcon <> firstIconed then
    begin
      C.TextWithIcon := firstIconed;
      C.ReCreate;
    end;
  end;
end;

procedure TCarbonListView.ClearIconCache;
var
  i : Integer;
begin
  for i := 0 to FIcons.Count - 1 do begin
    if Assigned(FIcons[i]) then
      ReleaseIconRef(FIcons[i]);
  end;
  FIcons.Clear;
end;

procedure TCarbonListView.SetViewStyle(AStyle: TViewStyle); 
begin
  FStyle:=AStyle;
  ListViewModes[FStyle].Apply(Self);
end;

procedure TCarbonListView.DoColumnClicked(MouseX, MouseY: Integer);
type
  TColumnInfo = record
    Index : Integer;
    Width : Integer;
  end;
var
  cx, cl  : Integer;
  ci      : DataBrowserTableViewColumnIndex;
  order   : array of TColumnInfo;
  i       : Integer;

  msg     : TLMNotify;
  NM      : TNMListView;
begin
  SetLength(order, FColumns.Count);
  for i := 0 to FColumns.Count - 1 do
  begin
    if GetDataBrowserTableViewColumnPosition(Content, GetColumn(i).PropertyID, ci) = noErr then
      if (ci >= 0) and (ci<FColumns.Count) then
      begin
        order[ci].Index := i;
        order[ci].Width := GetColumn(i).GetWidth;
      end;
  end;

  cx := 0;
  cl := -1;
  for i := 0 to length(order) - 1 do
  begin
    inc(cx, order[i].Width);
    if MouseX < cx then
    begin
      cl := order[i].Index;
      Break;
    end;
  end;

  if (cl>=0) and (cl < FColumns.Count) then
  begin
    msg.Msg := CN_NOTIFY;
    FillChar(NM, SizeOf(NM), 0);
    NM.hdr.hwndfrom := PtrUInt(Self);
    NM.hdr.code := LVN_COLUMNCLICK;
    NM.iItem := -1;
    NM.iSubItem := cl;
    msg.NMHdr := @NM.hdr;
    DeliverMessage(Self.LCLObject, msg);
  end;
end;

procedure TCarbonListView.SetItemsCount(ACount: Integer); 
begin
  if not FOwnerData then Exit;
  
  RemoveDataBrowserItems(Widget, kDataBrowserNoItem, 0, nil, kDataBrowserItemNoProperty);
  OSError(
     AddDataBrowserItems( Widget, kDataBrowserNoItem, ACount, nil, kDataBrowserItemNoProperty),
     Self, 'SetItemsCount', 'AddDataBrowserItems');
  UpdateDataBrowserItems( Widget, kDataBrowserNoItem, ACount, nil, kDataBrowserItemNoProperty, kDataBrowserNoItem);
end;

function TCarbonListView.NeedDeliverMouseEvent(Msg: Integer; const AMessage): Boolean;
type
  PLMMouse = ^TLMMouse;
  PLMMouseMove = ^TLMMouseMove;
var
  h: UInt16;
  x, y: Integer;
  scrolltop, scrollleft : UInt32;

// for some unknown reason, HIViewConvertPoint does return inaccurate x,y position for ListView
// (because of focus ring?)
const
  OfsY = -2;
  OfsX = -4;

begin
  if Assigned(LCLObject) and (TListView(LCLObject).Columns.Count > 0) and
    (TListView(LCLObject).ViewStyle = vsReport) then
  begin
    case Msg of
      LM_LBUTTONDOWN..LM_MBUTTONDBLCLK:
      begin
        y := PLMMouse(@AMessage)^.YPos;
        x := PLMMouse(@AMessage)^.XPos;
      end;
      LM_MOUSEMOVE:
      begin
        y := PLMMouseMove(@AMessage)^.YPos;
        x := PLMMouseMove(@AMessage)^.XPos;
      end;

      LM_MOUSEWHEEL:
      begin
        y := PLMMouseEvent(@AMessage)^.Y;
        x := PLMMouseEvent(@AMessage)^.X;
      end;
    else
      Result := inherited NeedDeliverMouseEvent(msg, AMessage);
      Exit;
    end;

    GetDataBrowserListViewHeaderBtnHeight(Content, h);
    inc(y, OfsY);
    Result := y > h;

    if not Result and (Msg = LM_LBUTTONUP) then
    begin
      GetDataBrowserScrollPosition(Content, scrolltop, scrollleft );
      inc(x, Integer(scrollleft) + OfsX);

      DoColumnClicked(x,y);
    end;

  end
  else
    Result := inherited NeedDeliverMouseEvent(msg, AMessage);
end;

{ TCarbonListBox }

procedure TCarbonListBox.CreateWidget(const AParams: TCreateParams);
begin
  inherited CreateWidget(AParams);
  
  ShowColumnHeaders(False);
  ShowCheckboxes(False);
  ShowAsList(True);
  SetItemsHeight((LCLObject as TCustomListBox).ItemHeight);
  SetRowSelect(True);
  SetScrollBars(ssAutoVertical);
  SetSelectionMode((LCLObject as TCustomListBox).ExtendedSelect,
    (LCLObject as TCustomListBox).MultiSelect);
  //Set BorderStyle according to the provided Params
  if (AParams.ExStyle and WS_EX_CLIENTEDGE) > 0 then
    SetBorderStyle(bsSingle)
  else
    SetBorderStyle(bsNone);
end;

function TCarbonListBox.GetItemCaption(AIndex, ASubIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < (LCLObject as TCustomListBox).Items.Count) then
    Result := (LCLObject as TCustomListBox).Items[AIndex]
  else
    Result := '';
end;

function TCarbonListBox.GetReadOnly: Boolean;
begin
  Result := True;
end;

function TCarbonListBox.MultiSelect: Boolean;
begin
  Result := (LCLObject as TCustomListBox).MultiSelect;
end;

function TCarbonListBox.IsOwnerDrawn: Boolean;
begin
  Result := (LCLObject as TCustomListBox).Style <> lbStandard;
end;

procedure TCarbonListBox.DrawItem(AIndex: Integer; AState: DataBrowserItemState);
var
  DrawStruct: TDrawListItemStruct;
begin
  DrawStruct.Area := GetItemRect(AIndex);
  DrawStruct.DC := HDC(Context);
  DrawStruct.ItemID := AIndex;
  
  DrawStruct.ItemState := [];
  if AState = kDataBrowserItemIsSelected then
    Include(DrawStruct.ItemState, odSelected);
  if not LCLObject.Enabled then
    Include(DrawStruct.ItemState, odDisabled);
  if (LCLObject.Focused) and (FItemIndex = AIndex) then
    Include(DrawStruct.ItemState, odFocused);

  LCLSendDrawListItemMsg(LCLObject, @DrawStruct);
end;

procedure TCarbonListBox.SelectionChanged(AIndex: Integer; ASelect: Boolean);
begin
  // handled in focus changed
  inherited;
end;

procedure TCarbonListBox.FocusedChanged(AIndex: Integer);
begin
  LCLSendSelectionChangedMsg(LCLObject);
end;

{ TCarbonCheckListBox }

procedure TCarbonCheckListBox.CreateWidget(const AParams: TCreateParams);
begin
  inherited CreateWidget(AParams);
  
  ShowCheckboxes(True);
end;

procedure TCarbonCheckListBox.CheckChanged(AIndex: Integer; AChecked: Boolean);
begin
  inherited;
  LCLSendChangedMsg(LCLObject, AIndex);
end;

{ TReportViewMode }

class procedure TReportViewMode.Apply(View: TCarbonListView);  
var
  C : TCarbonListColumn;
  firstIconed : Boolean;
begin
  with View do 
    if (FColumns.Count > 0) then
    begin
      firstIconed := Assigned(TListView(view.LCLObject).SmallImages);
      C := TCarbonListColumn(FColumns[0]);
      if C.TextWithIcon <> firstIconed then
      begin
        C.TextWithIcon := firstIconed;
        C.ReCreate;
      end;
    end;                                
end;

class procedure TReportViewMode.Resized(View: TCarbonListView);  
begin
  // nothing needs to be done here
end;

class function TReportViewMode.DataCallBack(View: TCarbonListView; ID: DataBrowserItemId;  
  PropID: DataBrowserPropertyID; Data: DataBrowserItemDataRef;  
  ASetValue: Boolean): OSStatus;  
var
  CheckboxValue: ThemeButtonValue;
  CheckboxState: ThemeDrawState;
  CFString: CFStringRef;
  ItemIcon: IconRef;
  SubIndex: Integer;
  ItemsCnt : integer; 
begin
  with View do begin
    
    if FOwnerData 
    then ItemsCnt := TListView(View.LCLObject).Items.Count+1
    else ItemsCnt := GetItemsCount + 1;

    if (ID < 1) or (ID > ItemsCnt) then
    begin
      Result := errDataBrowserItemNotFound;
      Exit;
    end;
    
    if ASetValue then
    begin
      if PropID = CheckPropertyID then // check has changed
      begin
        Result := GetDataBrowserItemDataButtonValue(Data, CheckboxValue);
        if Result <> noErr then Exit;
  
        CheckChanged(ID - 1, CheckboxValue = kThemeButtonOn);
      end;
      Exit;
    end;
  
    case PropID of
      kDataBrowserItemIsActiveProperty:
        Result := SetDataBrowserItemDataBooleanValue(Data, LCLObject.Enabled);
      kDataBrowserItemIsSelectableProperty:
        Result := SetDataBrowserItemDataBooleanValue(Data, LCLObject.Enabled);
      kDataBrowserItemIsEditableProperty:
        Result := SetDataBrowserItemDataBooleanValue(Data, not GetReadOnly);
      kDataBrowserItemIsContainerProperty:
        Result := SetDataBrowserItemDataBooleanValue(Data, False);
      CheckPropertyID:
        begin
          if GetItemChecked(ID - 1) then CheckboxValue := kThemeButtonOn
          else CheckboxValue := kThemeButtonOff;
          
          Result := SetDataBrowserItemDataButtonValue(Data, CheckboxValue);
          if Result <> noErr then Exit;
          
          if LCLObject.Enabled then CheckboxState := kThemeStateActive
          else CheckboxState := kThemeStateInactive;
          
          Result := SetDataBrowserItemDataDrawState(Data, CheckboxState);
        end;
      else
        begin
          if (PropID >= CaptionPropertyID) and
            (PropID <= CaptionPropertyID + DataBrowserPropertyID(FColumns.Count)) then
          begin
            if PropID = CaptionPropertyID then
            begin
              SubIndex := 0;
            end else begin
              SubIndex :=
                TCarbonListColumn(FColumns[PropID - CaptionPropertyID - 1]).FListColumn.Index;
            end;
            
            CFString:=nil;
            CreateCFString(GetItemCaption(ID - 1, SubIndex), CFString);
            try
              SetDataBrowserItemDataText(Data, CFString);
            finally
              FreeCFString(CFString);
            end;
  
            ItemIcon := GetItemIcon(ID-1, SubIndex);
            if Assigned(ItemIcon) then
              OSError(
                SetDataBrowserItemDataIcon(Data, ItemIcon),
                'CarbonItemDataCallBack', 'SetDataBrowserItemDataIcon');
  
          end
          else
            Result := errDataBrowserPropertyNotFound;
        end;
    end;
  end;
end;

initialization

  CarbonItemDataCallBackUPP := NewDataBrowserItemDataUPP(@CarbonItemDataCallBack);
  CarbonItemNotificationCallBackUPP := NewDataBrowserItemNotificationUPP(@CarbonItemNotificationCallBack);
  CarbonDrawItemCallBackUPP := NewDataBrowserDrawItemUPP(@CarbonDrawItemCallBack);

finalization

   DisposeDataBrowserItemDataUPP(CarbonItemDataCallBackUPP);
   DisposeDataBrowserItemNotificationUPP(CarbonItemNotificationCallBackUPP);
   DisposeDataBrowserDrawItemUPP(CarbonDrawItemCallBackUPP);


end.

