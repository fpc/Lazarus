{
                  --------------------------------------------
                  carbonlists.pp  -  Carbon list-like controls
                  --------------------------------------------

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CarbonLists;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
 // rtl+ftl
  Types, Classes, SysUtils, Math, Contnrs,
 // carbon bindings
  FPCMacOSAll,
 // LCL
  LMessages, LCLMessageGlue, LCLType, Controls, StdCtrls,
 // LCL Carbon
  CarbonDef, CarbonPrivate;

var ListBoxColumnTextPropID : DataBrowserPropertyID;
    ListBoxColumnCheckPropID : DataBrowserPropertyID;

type

  { TCarbonListBox }

  TCarbonListBox = class(TCarbonControl)
  private
    FItemIndex: Integer;
  protected
    procedure RegisterEvents; override;
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure BoundsChanged; override;
    procedure CheckNeedsScrollbar;
    function GetSelCount : integer;
    function GetSelected(AIndex : integer) : boolean;
    function GetItemIndex: Integer;
    function GetTopIndex : integer;
    procedure SelectItem(AIndex: integer; ASelected: boolean);
    procedure SetBorderStyle(ABorderStyle : TBorderStyle);
    function SetItemIndexQuiet(AIndex: Integer) : boolean;
    procedure SetItemIndex(AIndex: Integer);
    procedure SetSelectionMode(const AExtendedSelect, AMultiSelect: boolean);
    procedure SetStyle; overload;
    procedure SetStyle(AWidth : integer); overload;
    procedure SetTopIndex(const NewTopIndex: integer);
  end;

  { TCarbonCheckListBox }

  TCarbonCheckListBox = class(TCarbonListBox)
  private
  protected
    procedure CreateWidget(const AParams: TCreateParams); override;
  public
    procedure Changed(const AIndex : integer; AChecked : boolean);
    function GetChecked(const AIndex: integer): boolean;
    procedure SetChecked(const AIndex: integer; const AChecked: boolean);
  end;


implementation

uses InterfaceBase, CarbonProc, CarbonDbgConsts, CarbonUtils, CarbonStrings,
     CarbonCanvas;

{ TCarbonListBox }

var CarbonListBox_ItemDataCallBack_UPP         : DataBrowserItemDataUPP;
    CarbonListBox_ItemNotificationCallBack_UPP : DataBrowserItemNotificationUPP;
    CarbonListBox_DrawItemCallBack_UPP         : DataBrowserDrawItemUPP;

{------------------------------------------------------------------------------
  Function: CarbonListBox_ItemDataCallBack

  Responds to Data Browser requests for data and states
 ------------------------------------------------------------------------------}
function CarbonListBox_ItemDataCallBack(aControl : ControlRef; idx : DataBrowserItemId;
         propID : DataBrowserPropertyID; data : DataBrowserItemDataRef;
         setvalue : boolean) : OSStatus; {$IFDEF darwin} mwpascal;{$ENDIF}
var aCarbonListBox : TCarbonListBox;
    aListBox : TCustomListBox;
    aCarbonCheckListBox : TCarbonCheckListBox;
    checkboxvalue : ThemeButtonValue;
    checkboxstate : ThemeDrawState;
    checklist : boolean;
begin
  Result:=noErr;
  aCarbonListBox:=TCarbonListBox(GetCarbonControl(aControl));
  if aCarbonListBox=nil then exit;
  aListBox:=TCustomListBox(aCarbonListBox.LCLObject);
  if aCarbonListBox is TCarbonCheckListBox
    then aCarbonCheckListBox:=TCarbonCheckListBox(aCarbonListBox)
  else aCarbonCheckListBox:=nil;
  checklist:=aCarbonCheckListBox<>nil;
//  writeln('  idx = ',idx,' propid = ',propid,' data = ',longword(data),' setvalue = ',setvalue);

  if (idx<1) or (idx>aListBox.Items.Count) then
  begin
    Result:=errDataBrowserItemNotFound;
    exit;
  end;
  if setvalue then
  begin
    //we mind setvalue only when our control is a CheckListBox...
    if not checklist then exit;
    //...and a checkbox state has changed
    if propID = ListBoxColumnCheckPropID then
    begin
      Result:=GetDataBrowserItemDataButtonValue(data,checkboxvalue);
      if Result<>noErr then exit;
      aCarbonCheckListBox.Changed(idx-1,checkboxvalue = kThemeButtonOn);
    end;
    exit;
  end;

  case propId of
    kDataBrowserItemIsActiveProperty : Result:=SetDataBrowserItemDataBooleanValue(data,aListBox.Enabled);
    kDataBrowserItemIsSelectableProperty : Result:=SetDataBrowserItemDataBooleanValue(data,aListBox.Enabled);
    kDataBrowserItemIsEditableProperty : Result:=SetDataBrowserItemDataBooleanValue(data,false);
    kDataBrowserItemIsContainerProperty : Result:=SetDataBrowserItemDataBooleanValue(data,false)
    else if propId = ListBoxColumnTextPropID then
      Result:=SetDataBrowserItemDataText(data,TCarbonListBoxStrings(aListBox.Items).GetCFString(idx))
    else if ((propId = ListBoxColumnCheckPropID) and checklist) then
    begin
      if aCarbonCheckListBox.GetChecked(idx-1) then checkboxvalue:=kThemeButtonOn
      else checkboxvalue:=kThemeButtonOff;
      Result:=SetDataBrowserItemDataButtonValue(data,checkboxvalue);
      if Result<>noErr then exit;
      if aListBox.Enabled then checkboxstate:=kThemeStateActive
      else checkboxstate:=kThemeStateInactive;
      Result:=SetDataBrowserItemDataDrawState(data,checkboxstate);
      if Result<>noErr then exit;
    end
    else Result:=errDataBrowserPropertyNotFound;
  end;
end;

{------------------------------------------------------------------------------
  Function: CarbonListBox_ItemNotificationCallBack

  Called by DataBrowser when items are selected/deselected
 ------------------------------------------------------------------------------}
procedure CarbonListBox_ItemNotificationCallBack(aControl : ControlRef;
         idx : DataBrowserItemId; message : DataBrowserItemNotification);
         {$IFDEF darwin} mwpascal;{$ENDIF}
var aCarbonListBox : TCarbonListBox;
    aListBox : TCustomListBox;
    msg : TLMessage;
begin
  aCarbonListBox:=TCarbonListBox(GetCarbonControl(aControl));
  if aCarbonListBox=nil then exit;
  aListBox:=TCustomListBox(aCarbonListBox.LCLObject);
  if (idx<1) or (idx>aListBox.Items.Count) then exit;

  FillChar(msg,sizeof(msg),0);
  msg.Msg:=LM_SELCHANGE;
  case message of
    kDataBrowserItemSelected :
    begin
      aCarbonListBox.SetItemIndexQuiet(idx-1);
      DeliverMessage(aListBox,msg);
    end;
    kDataBrowserItemDeselected :
    begin
      if aCarbonListBox.GetSelCount=0 then aCarbonListBox.SetItemIndexQuiet(-1);
      DeliverMessage(aListBox,msg);
    end;
//    kDataBrowserItemDoubleClicked : ;
    kDataBrowserSelectionSetChanged : DeliverMessage(aListBox,msg);
  end;

end;

{------------------------------------------------------------------------------
  Function: CarbonListBox_DrawItemCallBack

  Handles draw requests from DataBrowser when in ownerdrawn style
 ------------------------------------------------------------------------------}
procedure CarbonListBox_DrawItemCallBack(aControl : ControlRef;
  idx: DataBrowserItemID; propID : DataBrowserPropertyID;
  state :DataBrowserItemState; const theRect : Rect; gdDepth: SInt16;
  colorDevice: Boolean ); {$IFDEF darwin} mwpascal;{$ENDIF}
var aCarbonListBox : TCarbonListBox;
    aListBox : TCustomListBox;
    msg : TLMDrawListItem;
    drawstruct : TDrawListItemStruct;
    checklist : boolean;
    arect : TRect;
    delta : uint16;
begin
//  writeln(Format('idx = %d propid = %d rect = %d,%d,%d,%d gdDepth = %d ',[idx,propid,therect.left,therect.top,therect.right,therect.bottom,gddepth]));

  aCarbonListBox:=TCarbonListBox(GetCarbonControl(aControl));
  if aCarbonListBox=nil then exit;
  aListBox:=TCustomListBox(aCarbonListBox.LCLObject);
  checklist:=aCarbonListBox is TCarbonCheckListBox;

//get the client rect (the carbon provided one usually is smaller than it
//should be)
  idx:=idx-1;
  if not WidgetSet.GetListBoxItemRect(aListBox,idx,arect) then exit;
  if checklist then
  begin
    //shrink the rect so that it doesn't include the area used by checkbox
    if OSError(
      GetDataBrowserTableViewNamedColumnWidth(aControl,ListBoxColumnCheckPropID,delta),
      'CarbonListBox_DrawItemCallBack','GetDataBrowserTableViewNamedColumnWidth')
    then exit;
    inc(arect.Left,delta);
  end;

  drawstruct.Area:=arect;
  drawstruct.DC:=HDC(aCarbonListBox.Context);
  drawstruct.ItemID:=idx;
  drawstruct.ItemState:=[];
  if state = kDataBrowserItemIsSelected then Include(drawstruct.ItemState,odSelected);
  if not aListBox.Enabled then Include(drawstruct.ItemState,odDisabled);
  if ((aListBox.Focused) and (aListBox.ItemIndex=idx)) then
    Include(drawstruct.ItemState,odFocused);

  FillChar(msg,sizeof(msg),0);
  msg.msg:=LM_DRAWLISTITEM;
  msg.DrawListItemStruct:=@drawstruct;
  DeliverMessage(aListBox,msg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.RegisterEvents

  Registers event handlers for list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.RegisterEvents;
const SRegisterEvents = 'RegisterEvents';
var callbacks : DataBrowserCallbacks;
    custcallbacks : DataBrowserCustomCallbacks;
    isownerdrawn : boolean;
begin
  inherited RegisterEvents;

  callbacks.version:=kDataBrowserLatestCallbacks;
  //init data browser callbacks
  OSError(
    InitDataBrowserCallbacks(callbacks),
    Self, SRegisterEvents, 'InitDataBrowserCallbacks');
  callbacks.itemDataCallback:=CarbonListBox_ItemDataCallBack_UPP;
  callbacks.itemNotificationCallback:=CarbonListBox_ItemNotificationCallBack_UPP;
  //set data browser callbacks
  OSError(
    SetDataBrowserCallbacks(widget,callbacks),
    Self, SRegisterEvents, 'SetDataBrowserCallbacks');


  custcallbacks.version:=kDataBrowserLatestCustomCallbacks;
  //init data browser custom callbacks
  OSError(
    InitDataBrowserCustomCallbacks(custcallbacks),
    Self, SRegisterEvents, 'InitDataBrowserCustomCallbacks');

  isownerdrawn:=TCustomListBox(LCLObject).Style<>lbStandard;
  if isownerdrawn then
    custcallbacks.drawItemCallback:=CarbonListBox_DrawItemCallBack_UPP;

  //set data browser custom callbacks
  OSError(
    SetDataBrowserCustomCallbacks(widget,custcallbacks),
    Self, SRegisterEvents, 'SetDataBrowserCustomCallbacks');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.CreateWidget(const AParams: TCreateParams);
begin
  //Create DataBrowser Control
  if OSError(
    CreateDataBrowserControl(GetTopParentWindow,ParamsToCarbonRect(aParams),
                             kdatabrowserListView,widget),
    Self, SCreateWidget, 'CreateDataBrowserControl') then RaiseCreateWidgetError(LCLObject);

  //Hide column headers
  OSError(
    SetDataBrowserListViewHeaderBtnHeight(widget,0),
    Self,SCreateWidget, 'SetDataBrowserListViewHeaderBtnHeight');

  //This adds and configures the main column
  SetStyle(AParams.Width);

  //The whole selected item line must be highlighted, not only the "text area"
  OSError(
    SetDataBrowserTableViewHiliteStyle(widget,kDataBrowserTableViewFillHilite),
    Self, SCreateWidget, 'SetDataBrowserTableViewHiliteStyle');

  //Text starts at 2px from the left instead of using the default indentation
  OSError(
    DataBrowserSetMetric(widget,kDataBrowserMetricCellContentInset,false,2.0),
    Self, SCreateWidget, 'DataBrowserSetMetric');

  //Set rowheight if ItemHeight is set.
  if TCustomListBox(LCLObject).ItemHeight>0 then
    OSError(
      SetDataBrowserTableViewRowHeight(widget,TCustomListBox(LCLObject).ItemHeight),
      Self, SCreateWidget, 'SetDataBrowserTableViewRowHeight');

  //Hide scrollbars
  OSError(
    SetDataBrowserHasScrollBars(widget,false,true),
    Self, SCreateWidget, 'SetDataBrowserHasScrollBars');

  //Set selection mode
  SetSelectionMode(TCustomListBox(LCLObject).ExtendedSelect,TCustomListBox(LCLObject).MultiSelect);

  FItemIndex := -1;

  inherited;

end;

procedure TCarbonListBox.BoundsChanged;
begin
  inherited;
  CheckNeedsScrollbar;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.CheckNeedsScrollbar;
  Params:  -

  Enables or disables vertical scroolbar if data doesn't fit in the listbox
  This method is called from TCarbonListBoxStrings when control needs update
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.CheckNeedsScrollbar;
const SCheckNeedsScrollbar = 'CheckNeedsScrollbar';
var rowheight : Uint16;
    scrollbarvisible : boolean;
begin
  if OSError(
    GetDataBrowserTableViewRowHeight(widget,rowheight),
    Self,SCheckNeedsScrollbar,'GetDataBrowserTableViewRowHeight')
  then exit;

  with LCLObject as TCustomListBox do
    scrollbarvisible:=(Items.Count*rowheight)>Height;

  OSError(
    SetDataBrowserHasScrollBars(widget,false,scrollbarvisible),
    Self,SCheckNeedsScrollbar,'SetDataBrowserHasScrollBars');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetSelCount
  Params:  -

  Returns: Count of selected items in list box
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetSelCount : integer;
const SGetSelCount = 'GetSelCount';
var acount : uint32;
begin
  if OSError(
    GetDataBrowserItemCount(widget,kDataBrowserNoItem,false,
    kDataBrowserItemIsSelected,acount),
    Self,SGetSelCount,'GetDataBrowserItemCount')
  then Result:=0
  else Result:=acount;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetSelected
  Params:  AIndex         - Item index

  Returns: If the specified item in list box is selected
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetSelected(AIndex : integer) : boolean;
const SGetSelected = 'GetSelected';
var astate : DataBrowserItemState;
begin
  Result:=false;
  if ((AIndex<0) or (AIndex>=TCustomListBox(LCLObject).Items.Count)) then exit;
  if OSError(
    GetDataBrowserItemState(widget,AIndex+1,astate),
    Self,SGetSelected,'GetDataBrowserItemState')
  then exit;
  Result:= (astate and kDataBrowserItemIsSelected) = kDataBrowserItemIsSelected;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetItemIndex
  Returns: The index of selected item in list box
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetItemIndex: Integer;
begin
  if FItemIndex>=TCustomListBox(LCLObject).Items.Count then
    FItemIndex:=-1;

  Result := FItemIndex;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.GetTopIndex
  Params:  -

  Returns: Index of top visible item of list box
 ------------------------------------------------------------------------------}
function TCarbonListBox.GetTopIndex : integer;
const SGetTopIndex = 'GetTopIndex';
var rowheight : UInt16;
    atop, aleft : UInt32;
begin
  Result:=0;
  if OSError(
    GetDataBrowserTableViewRowHeight(widget,rowheight),
    Self,SGetTopIndex,'GetDataBrowserTableViewRowHeight')
  then exit;

  if OSError(
    GetDataBrowserScrollPosition(widget,atop,aleft),
    Self,SGetTopIndex,'GetDataBrowserScrollPosition')
  then exit;
  Result:=atop div rowheight;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomListBox.SelectItem
  Params:  AIndex         - Item index to change selection
           ASelected      - New selection value

  Changes selection of item with the specified index of list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SelectItem(AIndex: integer; ASelected: boolean);
const SSelectItem = 'SelectItem';
var aoperation : DataBrowserSetOption;
    aid : DataBrowserItemID;
begin
  if ((AIndex<0) or (AIndex>=TCustomListBox(LCLObject).Items.Count)) then exit;
  aid:=AIndex+1;
  if not ASelected then aoperation:=kDataBrowserItemsRemove
  else if TCustomListBox(LCLObject).MultiSelect then aoperation:=kDataBrowserItemsAdd
  else aoperation:=kDataBrowserItemsAssign;
  OSError(
    SetDataBrowserSelectedItems(widget,1,@aid,aoperation),
    Self,SSelectItem,'SetDataBrowserSelectedItems');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetBorderStyle
  Params:  ABorderStyle - Border style to set

  Changes border style of list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetBorderStyle(ABorderStyle : TBorderStyle);
const SSetBorderStyle = 'SetBorderStyle';
var oldstate : boolean;
    newstate : boolean;
begin
  newstate:=ABorderStyle = bsSingle;
  if OSError(
    GetControlData(Widget,kControlNoPart,
    kControlDataBrowserIncludesFrameAndFocusTag,sizeof(oldstate),@oldstate,nil),
    Self, SSetBorderStyle, 'GetControlData')
  then exit;

  if oldstate = newstate then exit;

  OSError(
    SetControlData(widget,kControlNoPart,
    kControlDataBrowserIncludesFrameAndFocusTag,sizeof(newstate),@newstate),
    Self,SSetBorderStyle,'SetControlData');
end;

function TCarbonListBox.SetItemIndexQuiet(AIndex: Integer) : boolean;
begin
  Result:=((AIndex>=-1) and (AIndex<TCustomListBox(LCLObject).Items.Count));
  if Result then FItemIndex:=AIndex;
  Result:=Result and (AIndex<>-1);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetItemIndex
  Params:  AIndex - Index of item to select

  Sets the index of item to select
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetItemIndex(AIndex: Integer);
begin
//If we are not in multiselect mode, -1 clears selection
//In multiselect mode, -1 does nothing
  if (AIndex=-1) and (not TCustomListBox(LCLObject).MultiSelect) then
    SelectItem(FItemIndex,false);
  if SetItemIndexQuiet(AIndex) then
    SelectItem(AIndex,true);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetSelectionMode
  Params:  AExtendedSelect - New extended selection value
           AMultiSelect    - New mutliple selection value

  Changes selection mode of list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetSelectionMode(const AExtendedSelect, AMultiSelect: boolean);
const SSetSelectionMode = 'SetSelectionMode';
var aflags : DataBrowserSelectionFlags;
begin
//  aflags:=kDataBrowserDragSelect or kDataBrowserResetSelection or kDataBrowserCmdTogglesSelection;
  aflags:=kDataBrowserCmdTogglesSelection;
  if not AMultiSelect then aflags:=aflags or kDataBrowserSelectOnlyOne or kDataBrowserResetSelection;
  if not AExtendedSelect then aflags:=aflags or kDataBrowserAlwaysExtendSelection;
  OSError(
    SetDataBrowserSelectionFlags(widget,aflags),
    Self,SSetSelectionMode,'SetDataBrowserSelectionFlags');
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetStyle
  Params:  -

  Changes style (standard,ownerdrawn...) of list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetStyle;
begin
  SetStyle(LCLObject.Width);
end;

procedure TCarbonListBox.SetStyle(AWidth : integer);
const SSetStyle = 'SetStyle';
var position : DataBrowserTableViewColumnIndex;
    err : OSStatus;
    desc : DataBrowserListViewColumnDesc;
    proptype : DataBrowserPropertyType;
    atcreation,isownerdrawn : boolean;
    custcallbacks : DataBrowserCustomCallbacks;
begin
  err:=GetDataBrowserTableViewColumnPosition(widget,ListBoxColumnTextPropID,
       position);
  if ((err<>noErr) and (err<>errDataBrowserPropertyNotFound)) then
  begin
    OSError(err,Self,SSetStyle,'GetDataBrowserTableViewColumnPosition');
    exit;
  end;

//if there was no column, we were called from createwidget
  atcreation:=err=errDataBrowserPropertyNotFound;

  if err=noErr then
    if OSError(
      RemoveDataBrowserTableViewColumn(widget,ListBoxColumnTextPropID),
      Self,SSetStyle,'RemoveDataBrowserTableViewColumn') then exit;

  isownerdrawn:=TCustomListBox(LCLObject).Style<>lbStandard;
  if isownerdrawn then proptype:=kDataBrowserCustomType
  else proptype:=kDataBrowserTextType;

  //Set properties for the main column
  desc.propertyDesc.propertyID:=ListBoxColumnTextPropID;
  desc.propertyDesc.propertyType:=proptype;
  desc.propertyDesc.propertyFlags:=kDataBrowserListViewSelectionColumn;
  desc.headerBtnDesc.version:=kDataBrowserListViewLatestHeaderDesc;
  desc.headerBtnDesc.minimumWidth:=AWidth;
  desc.headerBtnDesc.maximumWidth:=AWidth;
  desc.headerBtnDesc.titleOffset:=0;
  desc.headerBtnDesc.titleString:=nil;
  desc.headerBtnDesc.initialOrder:=kDataBrowserOrderIncreasing;
  desc.headerBtnDesc.btnFontStyle.flags:=0;
  desc.headerBtnDesc.btnContentInfo.contentType:=kControlContentTextOnly;

  //Add the main column
  OSError(
    AddDataBrowserListViewColumn(widget,desc,kDataBrowserListViewAppendColumn),
    Self, SSetStyle, 'AddDataBrowserListViewColumn');

  //if user changed style after creation, change callbacks
  if not atcreation then
  begin
    if OSError(
      GetDataBrowserCustomCallbacks(widget,custcallbacks),
      Self, SSetStyle, 'GetDataBrowserCustomCallbacks')
    then exit;

    if isownerdrawn then
      custcallbacks.drawItemCallback:=CarbonListBox_DrawItemCallBack_UPP
    else custcallbacks.drawItemCallback:=nil;

    OSError(
      SetDataBrowserCustomCallbacks(widget,custcallbacks),
      Self, SSetStyle, 'SetDataBrowserCustomCallbacks');
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCarbonListBox.SetTopIndex
  Params:  NewTopIndex    - New top index

  Sets top visible item of list box
 ------------------------------------------------------------------------------}
procedure TCarbonListBox.SetTopIndex(const NewTopIndex: integer);
const SSetTopIndex = 'SetTopIndex';
var rowheight : UInt16;
    atop, aleft : UInt32;
begin
  if OSError(
    GetDataBrowserTableViewRowHeight(widget,rowheight),
    Self,SSetTopIndex,'GetDataBrowserTableViewRowHeight')
  then exit;

  aleft:=0;
  atop:=NewTopIndex*rowheight;

  if OSError(
    SetDataBrowserScrollPosition(widget,atop,aleft),
    Self,SSetTopIndex,'SetDataBrowserScrollPosition')
  then exit;
end;

{ TCarbonCheckListBox }

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBox.CreateWidget
  Params:  AParams - Creation parameters

  Creates Carbon check list box
 ------------------------------------------------------------------------------}
procedure TCarbonCheckListBox.CreateWidget(const AParams: TCreateParams);
var desc : DataBrowserListViewColumnDesc;
    cbwidth : Sint32;
begin
  //create the listbox and set all properties
  inherited;
  //then add our checkbox column

  if OSError(
    GetThemeMetric(kThemeMetricCheckBoxWidth,cbwidth),
    Self,SCreateWidget,'GetThemeMetric')
  then exit;

  //Set properties for the checkbox column
  desc.propertyDesc.propertyID:=ListBoxColumnCheckPropID;
  desc.propertyDesc.propertyType:=kDataBrowserCheckboxType;
  desc.propertyDesc.propertyFlags:=kDataBrowserPropertyIsMutable or
                                   kDataBrowserListViewSelectionColumn;
  desc.headerBtnDesc.version:=kDataBrowserListViewLatestHeaderDesc;
  desc.headerBtnDesc.minimumWidth:=cbwidth+4; //2 pixel space on both sides
  desc.headerBtnDesc.maximumWidth:=cbwidth+4;
  desc.headerBtnDesc.titleOffset:=0;
  desc.headerBtnDesc.titleString:=nil;
  desc.headerBtnDesc.initialOrder:=kDataBrowserOrderIncreasing;
  desc.headerBtnDesc.btnFontStyle.flags:=0;
  desc.headerBtnDesc.btnContentInfo.contentType:=kControlContentTextOnly;

  //Add this column to the left
  OSError(
    AddDataBrowserListViewColumn(widget,desc,0),
    Self, SCreateWidget, 'AddDataBrowserListViewColumn');

end;

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBox.Changed
  Params:  AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with specified index and notifies the LCL
  object of the change.
  Note: this method is called from the databrowser event handler to tell LCL
  that user selected/deselected a checkbox of the check list box
 ------------------------------------------------------------------------------}
procedure TCarbonCheckListBox.Changed(const AIndex : integer; AChecked : boolean);
var msg : TLMessage;
begin
  if GetChecked(AIndex)=AChecked then exit;
  SetChecked(AIndex,AChecked);
  FillChar(msg,sizeof(msg),0);
  msg.msg:=LM_CHANGED;
  msg.WParam:=AIndex;
  DeliverMessage(LCLObject,msg);
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBox.GetChecked
  Params:  AIndex              - Item index
  Returns: If the specified item in check list box is checked
 ------------------------------------------------------------------------------}
function TCarbonCheckListBox.GetChecked(const AIndex: integer): boolean;
begin
  Result:=TCarbonCheckListBoxStrings(TCustomListBox(LCLObject).Items).Checked[AIndex];
end;

{------------------------------------------------------------------------------
  Method:  TCarbonCheckListBox.SetChecked
  Params:  AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with the specified index of check list box
 ------------------------------------------------------------------------------}
procedure TCarbonCheckListBox.SetChecked(const AIndex: integer; const AChecked: boolean);
begin
  TCarbonCheckListBoxStrings(TCustomListBox(LCLObject).Items).Checked[AIndex]:=AChecked;
end;

initialization
  ListBoxColumnTextPropID:=MakeFourCC('LBTX');    //Text column
  ListBoxColumnCheckPropID:=MakeFourCC('LBCK');   //Check box column
  CarbonListBox_ItemDataCallBack_UPP:=NewDataBrowserItemDataUPP(@CarbonListBox_ItemDataCallBack);
  CarbonListBox_ItemNotificationCallBack_UPP:=NewDataBrowserItemNotificationUPP(@CarbonListBox_ItemNotificationCallBack);
  CarbonListBox_DrawItemCallBack_UPP:=NewDataBrowserDrawItemUPP(@CarbonListBox_DrawItemCallBack);

finalization
   DisposeDataBrowserItemDataUPP(CarbonListBox_ItemDataCallBack_UPP);
   DisposeDataBrowserItemNotificationUPP(CarbonListBox_ItemNotificationCallBack_UPP);
   DisposeDataBrowserDrawItemUPP(CarbonListBox_DrawItemCallBack_UPP);

end.

