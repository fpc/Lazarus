{
 *****************************************************************************
 *                              CocoaWSCheckLst.pp                           *
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWSCheckLst;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}

interface

uses
  // Libs
  MacOSAll, CocoaAll, Classes, sysutils,
  // LCL
  Controls, StdCtrls, CheckLst, LCLType, LCLMessageGlue,
  // Widgetset
  WSCheckLst, WSLCLClasses,
  // LCL Cocoa
  CocoaWSCommon, CocoaPrivate, CocoaConfig, CocoaGDIObjects,
  CocoaWSStdCtrls, CocoaListControl, CocoaTables, CocoaScrollers, CocoaWSScrollers;

type

  { TLCLCheckboxListCallback }

  TLCLCheckboxListCallback = class(TLCLListBoxCallback)
  protected
    function AllocStrings(ATable: NSTableView): TCocoaListControlStringList; override;
  public
    checklist: TCustomCheckListBox;
    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView); override;
    procedure SetItemCheckedAt( row: Integer; CheckState: Integer); override;
    function drawItem(row: Integer; ctx: TCocoaContext; const r: TRect;
      state: TOwnerDrawState): Boolean; override;
  end;

  { TCocoaTableCheckListBoxProcessor }

  TCocoaTableCheckListBoxProcessor = class( TCocoaTableListBoxProcessor )
    procedure onOwnerDrawItem(rowView: NSView); override;
  end;

  { TCocoaWSCustomCheckListBox }

  TCocoaWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AState: TCheckBoxState); override;
    class function GetCheckWidth(const ACheckListBox: TCustomCheckListBox): integer; override;
    class procedure SetStyle(const ACustomListBox: TCustomListBox); override;
  end;

implementation

{ TLCLCheckboxListCallback }

function TLCLCheckboxListCallback.AllocStrings(ATable: NSTableView): TCocoaListControlStringList;
begin
  Result:=TCocoaListBoxStringList.Create(ATable);
end;

constructor TLCLCheckboxListCallback.Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView);
begin
  inherited Create(AOwner, ATarget, AHandleView);
  if ATarget is TCustomCheckListBox then
    checklist := TCustomCheckListBox(ATarget);
end;

procedure TLCLCheckboxListCallback.SetItemCheckedAt( row: Integer;
  CheckState: Integer);
begin
  Inherited;
  LCLSendChangedMsg( self.Target, row );
end;

function TLCLCheckboxListCallback.drawItem(row: Integer; ctx: TCocoaContext;
  const r: TRect; state: TOwnerDrawState): Boolean;
var
  rectReducedLeftSpacing: TRect;
  lv: TCocoaTableListView;
  itemView: NSView;
begin
  rectReducedLeftSpacing:= r;
  lv:= TCocoaTableListView( self.Owner );
  if Assigned(lv) then begin
    itemView:= lv.viewAtColumn_row_makeIfNecessary( 0, row, false );
    if Assigned(itemView) then
      rectReducedLeftSpacing.Left:= Round( itemView.frame.origin.x );
  end;
  Result:= inherited drawItem(row, ctx, rectReducedLeftSpacing, state);
end;

{ TCocoaTableCheckListBoxProcessor }

procedure TCocoaTableCheckListBoxProcessor.onOwnerDrawItem(rowView: NSView);
var
  itemView: TCocoaTableListItem;
begin
  itemView:= TCocoaTableListItem( rowView.subviews.objectAtIndex(0) );
  if NOT Assigned(itemView) then
    Exit;
  if Assigned(itemView.imageView) then
    itemView.imageView.setHidden( True );
  if Assigned(itemView.textField) then
    itemView.textField.setHidden( True );
end;

{ TCocoaWSCustomCheckListBox }

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Cocoa interface

  Creates new check list box in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  list: TCocoaTableListView;
  scroll: TCocoaScrollView;
  processor: TCocoaTableViewProcessor;
  lclCheckListBox: TCustomCheckListBox absolute AWinControl;
begin
  list := AllocCocoaTableListView.lclInitWithCreateParams(AParams);
  if not Assigned(list) then
  begin
    Result := 0;
    Exit;
  end;
  processor:= TCocoaTableCheckListBoxProcessor.Create;
  list.lclSetProcessor( processor );
  list.callback := TLCLCheckboxListCallback.CreateWithView(list, AWinControl);
  list.lclSetCheckboxes(true);
  list.lclSetCheckBoxAllowsMixed( lclCheckListBox.AllowGrayed );
  //list.list := TCocoaStringList.Create(list);
  list.addTableColumn(NSTableColumn.alloc.init.autorelease);
  list.setHeaderView(nil);
  list.setDataSource(list);
  list.setDelegate(list);
  list.setAllowsMultipleSelection(lclCheckListBox.MultiSelect);
  list.CustomRowHeight:= lclCheckListBox.ItemHeight;
  list.readOnly := true;
  //todo:
  //list.AllowMixedState := TCustomCheckListBox(AWinControl).AllowGrayed;
  list.isOwnerDraw := lclCheckListBox.Style in [lbOwnerDrawFixed, lbOwnerDrawVariable];

  ListBoxSetStyle(list, TCustomListBox(AWinControl).Style);

  scroll := EmbedInScrollView(list);
  if not Assigned(scroll) then
  begin
    Result := 0;
    Exit;
  end;
  scroll.callback := list.callback;
  scroll.setHasVerticalScroller(true);
  scroll.setAutohidesScrollers(true);

  ScrollViewSetBorderStyle(scroll, lclCheckListBox.BorderStyle);
  UpdateControlFocusRing(list, AWinControl);

  Result := TLCLHandle(scroll);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.GetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index
  Returns: If the specified item in check list box in Cocoa interface is
           checked, grayed or unchecked
 ------------------------------------------------------------------------------}
class function TCocoaWSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState;
const
  checkStateArray: Array [NSMixedState..NSOnState] of TCheckBoxState =
    ( cbGrayed, cbUnchecked, cbChecked );
var
  lclcb : TLCLCheckboxListCallback;
  checkState: Integer;
begin
  Result:= cbUnchecked;

  lclcb:= TLCLCheckboxListCallback( getCallbackFromLCLListBox(ACheckListBox) );
  if NOT Assigned(lclcb) then
    Exit;

  if lclcb.GetItemCheckedAt(AIndex, checkState) then
    Result:= checkStateArray[checkState];
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSCustomCheckListBox.SetState
  Params:  ACustomCheckListBox - LCL custom check list box
           AIndex              - Item index to change checked value
           AChecked            - New checked value

  Changes checked value of item with the specified index of check list box in
  Cocoa interface
 ------------------------------------------------------------------------------}
class procedure TCocoaWSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
const
  checkStateArray: Array [TCheckBoxState] of Integer =
    ( NSOffState, NSOnState, NSMixedState );
var
  cocoaTLV: TCocoaTableListView;
  lclcb : TLCLCheckboxListCallback;
  checkState: Integer;
begin
  lclcb:= TLCLCheckboxListCallback( getCallbackFromLCLListBox(ACheckListBox) );
  if NOT Assigned(lclcb) then
    Exit;

  checkState:= checkStateArray[AState];
  lclcb.SetItemCheckedAt( AIndex, checkState );

  cocoaTLV:= getTableViewFromLCLListBox( ACheckListBox );
  cocoaTLV.reloadDataForRow_column( AIndex, 0 );
end;

class function TCocoaWSCustomCheckListBox.GetCheckWidth(
  const ACheckListBox: TCustomCheckListBox): integer;
begin
  Result:= Round( CocoaConfigListView.vsList.item.checkBoxOccupiedWidth );
end;

class procedure TCocoaWSCustomCheckListBox.SetStyle(
  const ACustomListBox: TCustomListBox);
begin
  TCocoaWSCustomListBox.SetStyle( ACustomListBox );
end;

end.
