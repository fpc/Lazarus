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
  Controls, StdCtrls, CheckLst, LCLType,
  // Widgetset
  WSCheckLst, WSLCLClasses,
  // LCL Cocoa
  CocoaWSCommon, CocoaPrivate, CocoaUtils, CocoaWSStdCtrls, CocoaTables, CocoaGDIObjects,
  CocoaScrollers
  ,LCLMessageGlue;

type

  { TLCLCheckboxListCallback }

  TLCLCheckboxListCallback = class(TLCLListBoxCallback, IListViewCallback)
  public
    checklist: TCustomCheckListBox;
    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView); override;
    function GetItemCheckedAt(ARow, ACol: Integer; var CheckState: Integer): Boolean; override;
    procedure SetItemCheckedAt(ARow, ACol: Integer; CheckState: Integer); override;

    function GetCheckState(Index: Integer; var AState: Integer): Boolean;
    function SetCheckState(Index: Integer; AState: Integer; InvalidateCocoa: Boolean = true): Boolean;
  end;


  { TCocoaWSCustomCheckListBox }

  TCocoaWSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer): TCheckBoxState; override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox; const AIndex: integer; const AState: TCheckBoxState); override;
  end;

function CtrlToCheckList(ctrl: TWinControl; out tbl: TCocoaTableListView; out cb: TLCLCheckboxListCallback): Boolean;

implementation

function CtrlToCheckList(ctrl: TWinControl; out tbl: TCocoaTableListView; out cb: TLCLCheckboxListCallback): Boolean;
begin
  Result := Assigned(ctrl) and (ctrl.HandleAllocated) and (ctrl.Handle <> 0);
  if not Result then begin
    tbl := nil;
    cb := nil;
    Exit;
  end;
  tbl:=TCocoaTableListView(NSSCrollView(ctrl.Handle).documentView);
  Result := Assigned(tbl);
  if Result then
    cb := TLCLCheckboxListCallback(tbl.lclGetCallback.GetCallbackObject)
  else
    cb := nil;
end;

{ TLCLCheckboxListCallback }

constructor TLCLCheckboxListCallback.Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView);
begin
  inherited Create(AOwner, ATarget, AHandleView);
  if ATarget is TCustomCheckListBox then
    checklist := TCustomCheckListBox(ATarget);
end;

function TLCLCheckboxListCallback.GetItemCheckedAt(ARow, ACol: Integer;
  var CheckState: Integer): Boolean;
begin
  Result := GetCheckState(Arow, CheckState);
end;

procedure TLCLCheckboxListCallback.SetItemCheckedAt(ARow, ACol: Integer;
  CheckState: Integer);
var
  changed : Boolean;
begin
  changed := SetCheckState(ARow, CheckState, false); // returns true, if changed!s
  if changed then LCLSendChangedMsg(Target, ARow);
end;

function TLCLCheckboxListCallback.GetCheckState(Index: Integer; var AState: Integer): Boolean;
begin
  Result := Assigned(strings) and (Index>=0) and (Index<strings.Count);
  if Result then
    AState := Integer(PtrInt(strings.Objects[Index]))
  else
    ASTate := 0;
end;

function TLCLCheckboxListCallback.SetCheckState(Index: Integer; AState: Integer;
  InvalidateCocoa: Boolean = true): Boolean;
begin
  Result := Assigned(Strings) and (Index>=0) and (Index<strings.Count);
  if not Result then Exit;
  Result := strings.Objects[Index] <> TObject(PtrInt(AState));
  if Result then
  begin
    strings.Objects[Index] := TObject(PtrInt(AState));
    if InvalidateCocoa and Assigned(listview) then
      listview.reloadDataForRow_column(Index, 0);
  end;
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
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  list: TCocoaTableListView;
  scroll: TCocoaScrollView;
begin
  list := AllocCocoaTableListView.lclInitWithCreateParams(AParams);
  if not Assigned(list) then
  begin
    Result := 0;
    Exit;
  end;
  list.callback := TLCLCheckboxListCallback.CreateWithView(list, AWinControl);
  list.lclSetFirstColumCheckboxes(true);
  //list.list := TCocoaStringList.Create(list);
  list.addTableColumn(NSTableColumn.alloc.init);
  list.setHeaderView(nil);
  list.setDataSource(list);
  list.setDelegate(list);
  list.readOnly := true;
  //todo:
  //list.AllowMixedState := TCustomCheckListBox(AWinControl).AllowGrayed;
  list.isCustomDraw := TCustomCheckListBox(AWinControl).Style in [lbOwnerDrawFixed, lbOwnerDrawVariable];

  scroll := EmbedInScrollView(list);
  if not Assigned(scroll) then
  begin
    list.dealloc;
    Result := 0;
    Exit;
  end;
  scroll.callback := list.callback;
  scroll.setHasVerticalScroller(true);
  scroll.setAutohidesScrollers(true);
  Result := TLCLIntfHandle(scroll);
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
var
  tbl: TCocoaTableListView;
  cb : TLCLCheckboxListCallback;
  cocoaSt: Integer;
begin
  if not CtrlToCheckList(ACheckListBox, tbl, cb) then begin
    Result := cbUnchecked;
    Exit;
  end;
  if cb.GetCheckState(AIndex, cocoaSt) then
    case cocoaSt of
      NSOnState : Result := cbChecked;
      NSMixedState : Result := cbGrayed;
    else
      Result := cbUnchecked;
    end
  else
    Result := cbUnchecked;
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
var
  tbl: TCocoaTableListView;
  cb : TLCLCheckboxListCallback;
  cocoaSt: Integer;
begin
  if not CtrlToCheckList(ACheckListBox, tbl, cb) then Exit;

  case AState of
    cbChecked: cocoaSt := NSOnState;
    cbGrayed:  cocoaSt := NSMixedState;
  else
    cocoaSt := NSOffState;
  end;
  cb.SetCheckState(AIndex, cocoaSt, true);
end;

end.
