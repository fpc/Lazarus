{ $Id$ }
{               ----------------------------------------------  
                 localsdlg.pp  -  Overview of local variables 
                ---------------------------------------------- 
 
 @created(Thu Mar 14st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Locals debugger dialog.
 
 
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit LocalsDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, StrUtils,
  // LCL
  Forms, ClipBrd, ComCtrls, ActnList, Menus, Controls,
  laz.VirtualTrees,
  // LazUtils
  LazLoggerBase, LazStringUtils, LazUTF8,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // LazDebuggerIntf
  LazDebuggerIntf, LazDebuggerIntfBaseTypes,
  // IDE Debugger
  IdeDebuggerStringConstants, BaseDebugManager, Debugger,
  DebuggerDlg, IdeDebuggerWatchResPrinter, IdeDebuggerUtils, DebuggerTreeView,
  IdeDebuggerWatchResult, IdeDebuggerBase, DbgTreeViewWatchData, EnvDebuggerOptions,
  {$ifdef Windows}ActiveX{$else}laz.FakeActiveX{$endif};

type

    TDbgTreeViewLocalsValueMgr = class;

  { TLocalsDlg }

  TLocalsDlg = class(TDebuggerDlg)
    actInspect: TAction;
    actEvaluate: TAction;
    actCopyName: TAction;
    actCopyValue: TAction;
    actCopyRAWValue: TAction;
    actCopyAddr: TAction;
    actCopyEntry: TAction;
    actCopyAll: TAction;
    actWath: TAction;
    ActionList1: TActionList;
    ToolBar1: TToolBar;
    ToolButtonPower: TToolButton;
    ToolButton2: TToolButton;
    btnShowDataAddr: TToolButton;
    vtLocals: TDbgTreeView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    menuCopyName: TMenuItem;
    menuCopyValue: TMenuItem;
    menuCopyRawValue: TMenuItem;
    menuCopyAddr: TMenuItem;
    menuCopyEntry: TMenuItem;
    menuCopyAll: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actCopyRAWValueExecute(Sender: TObject);
    procedure actCopyAddrExecute(Sender: TObject);
    procedure actCopyEntryExecute(Sender: TObject);
    procedure actCopyAllExecute(Sender: TObject);
    procedure actCopyAllUpdate(Sender: TObject);
    procedure actEvaluateExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actInspectUpdate(Sender: TObject);
    procedure actWathExecute(Sender: TObject);
    procedure btnShowDataAddrClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButtonPowerClick(Sender: TObject);
    procedure vtLocalsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtLocalsDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure vtLocalsDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure vtLocalsFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtLocalsNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
  private
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FWatchPrinter: TWatchResultPrinter;
    FLocolsTreeMgr: TDbgTreeViewLocalsValueMgr;

    FUpdateFlags: set of (ufNeedUpdating);
    function GetSelected: TLocalsValue; // The focused Selected Node
    procedure CopyRAWValueEvaluateCallback(Sender: TObject; ASuccess: Boolean;
      ResultText: String; ResultDBGType: TDBGType);
    procedure CopyValueEvaluateCallback(Sender: TObject; ASuccess: Boolean;
      ResultText: String; ResultDBGType: TDBGType);

    procedure ClearTree(OnlyClearNodeData: boolean = False);
    procedure LocalsChanged(Sender: TObject);
    procedure SubLocalChanged(Sender: TObject);
    function  GetThreadId: Integer;
    function  GetSelectedThreads(Snap: TSnapshot): TIdeThreads;
    function GetStackframe: Integer;
    function  GetSelectedSnapshot: TSnapshot;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LocalsMonitor;
    property ThreadsMonitor;
    property CallStackMonitor;
    property SnapshotManager;
  end;

  { TDbgTreeViewLocalsValueMgr }

  TDbgTreeViewLocalsValueMgr = class(TDbgTreeViewWatchDataMgr)
  private
    FLocalsDlg: TLocalsDlg;
  protected
    function WatchAbleResultFromNode(AVNode: PVirtualNode): IWatchAbleResultIntf; override;
    function WatchAbleResultFromObject(AWatchAble: TObject): IWatchAbleResultIntf; override;

    function GetFieldAsText(Nd: PVirtualNode; AWatchAble: TObject;
      AWatchAbleResult: IWatchAbleResultIntf; AField: TTreeViewDataToTextField;
      AnOpts: TTreeViewDataToTextOptions): String; override;
    procedure UpdateColumnsText(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode); override;
    procedure ConfigureNewSubItem(AWatchAble: TObject); override;
    //procedure UpdateSubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
    //  AVNode: PVirtualNode; out ChildCount: LongWord); override;
    //procedure UpdateSubItemsLocked(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
    //  AVNode: PVirtualNode; out ChildCount: LongWord); override;
  end;

function ValueToRAW(const AValue: string): string;

implementation

{$R *.lfm}

var
  DBG_DATA_MONITORS: PLazLoggerLogGroup;
  LocalsDlgWindowCreator: TIDEWindowCreator;

const
  COL_LOCALS_NAME   = 1;
  COL_LOCALS_VALUE  = 2;
  COL_ADDR_VALUE    = 3;
  COL_WIDTHS: Array[0..2] of integer = (100, 250, 80);

function LocalsDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TLocalsDlg;
  if Result then
    Result := TLocalsDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure LocalsDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TLocalsDlg then
    TLocalsDlg(AForm).ColSizeSetter(AColId, ASize);
end;

function ValueToRAW(const AValue: string): string;
var
  I: Integer; //current char in AValue
  M: Integer; //max char in AValue
  L: Integer; //current char in Result

  procedure ProcessCharConsts;
  var
    xNum: string;
    xCharOrd: Integer;
  begin
    while (I <= M) and (AValue[I] = '#') do
    begin
      Inc(I);
      xNum := '';
      if (I <= M) and (AValue[I]='$') then
      begin // hex
        xNum := xNum + AValue[I];
        Inc(I);
        while (I <= M) and (AValue[I] in ['0'..'9', 'A'..'F', 'a'..'f']) do
        begin
          xNum := xNum + AValue[I]; // not really fast, but OK for this purpose
          Inc(I);
        end;
      end else
      begin // dec
        while (I <= M) and (AValue[I] in ['0'..'9']) do
        begin
          xNum := xNum + AValue[I]; // not really fast, but OK for this purpose
          Inc(I);
        end;
      end;
      if TryStrToInt(xNum, xCharOrd) then
      begin
        Result[L] := Char(xCharOrd);
        Inc(L);
      end;
    end;
  end;

  procedure ProcessQuote;
  begin
    Inc(I);
    if AValue[I] = '''' then // "''" => "'"
    begin
      Result[L] := AValue[I];
      Inc(L);
    end else
    if AValue[I] = '#' then // "'#13#10'" => [CRLF]
      ProcessCharConsts;
  end;

  procedure ProcessString;
  begin
    I := 2;
    L := 1;
    M := Length(AValue);
    if AValue[M] = '''' then
      Dec(M);
    SetLength(Result, Length(AValue)-2);
    while I <= M do
    begin
      if AValue[I] = '''' then
      begin
        ProcessQuote;
      end else
      begin
        Result[L] := AValue[I];
        Inc(L);
      end;
      Inc(I);
    end;
    SetLength(Result, L-1);
  end;

  procedure ProcessOther;
  begin
    I := Pos('(', AValue);
    if I > 0 then
    begin
      // Invalid enum value: "true (85)" => "85"
      L := PosEx(')', AValue, I+1);
      Result := Copy(AValue, I+1, L-I-1);
    end else
    begin
      //no formatting
      Result := AValue;
    end;
  end;

begin
  // try to guess and format value back to raw data, e.g.
  //   "'value'" => "value"
  //   "true (85)" => "85"
  Result := '';
  if AValue='' then
    Exit;

  if AValue[1] = '''' then
    //string "'val''ue'" => "val'ue"
    ProcessString
  else
    ProcessOther;
end;

function ExtractValue(const AValue: string; AType: string): string;
var
  StringStart: SizeInt;
begin
  Result := AValue;
  if (AType='') and (AValue<>'') and CharInSet(AValue[1], ['a'..'z', 'A'..'Z']) then
  begin                                            // no type - guess from AValue
    StringStart := Pos('(', AValue);
    if StringStart>0 then
      AType := Copy(AValue, 1, StringStart-1);
  end;
  if (PosI('char',AType)>0) or (PosI('string',AType)>0) then // extract string value
  begin
    StringStart := Pos('''', Result);
    if StringStart>0 then
      Delete(Result, 1, StringStart-1);
  end;

  Result := StringReplace(Result, LineEnding, ' ', [rfReplaceAll]);
end;

{ TLocalsDlg }

constructor TLocalsDlg.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  LocalsNotification.OnChange     := @LocalsChanged;
  ThreadsNotification.OnCurrent   := @LocalsChanged;
  CallstackNotification.OnCurrent := @LocalsChanged;
  SnapshotNotification.OnCurrent  := @LocalsChanged;
  FWatchPrinter := TWatchResultPrinter.Create;
  FWatchPrinter.FormatFlags := [rpfClearMultiLine];
  FLocolsTreeMgr := TDbgTreeViewLocalsValueMgr.Create(vtLocals);
  FLocolsTreeMgr.FLocalsDlg := Self;
  ToolBar1.Images := IDEImages.Images_16;

  Caption:= lisLocals;
  vtLocals.Header.Columns[0].Text:= lisName;
  vtLocals.Header.Columns[1].Text:= lisValue;
  vtLocals.Header.Columns[2].Text := dlgValueDataAddr;
  actInspect.Caption := lisInspect;
  actWath.Caption := lisWatch;
  actEvaluate.Caption     := lisEvaluateModify;
  actCopyName.Caption     := lisLocalsDlgCopyName;
  actCopyValue.Caption    := lisLocalsDlgCopyValue;
  actCopyRAWValue.Caption := lisLocalsDlgCopyRAWValue;
  actCopyAddr.Caption     := lisLocalsDlgCopyAddr;
  actCopyEntry.Caption    := lisLocalsDlgCopyEntry;
  actCopyAll.Caption      := lisLocalsDlgCopyAll;
  btnShowDataAddr.ImageIndex := IDEImages.LoadImage('ce_implementation');

  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  ToolButtonPower.ImageIndex := FPowerImgIdx;
  ToolButtonPower.Caption := lisDbgWinPower;
  ToolButtonPower.Hint := lisDbgWinPowerHint;


  for i := low(COL_WIDTHS) to high(COL_WIDTHS) do
    vtLocals.Header.Columns[i].Width := COL_WIDTHS[i];
end;

destructor TLocalsDlg.Destroy;
begin
  ClearTree;
  inherited Destroy;
  FWatchPrinter.free;
  FLocolsTreeMgr.Free;
end;

procedure TLocalsDlg.actInspectUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(GetSelected);
end;

procedure TLocalsDlg.actWathExecute(Sender: TObject);
var
  S: String;
  Watch: TCurrentWatch;
  LVal: TLocalsValue;
begin
  LVal := GetSelected;
  if LVal = nil then
    exit;

  S := LVal.Name;
  if s = '' then
    exit;
  if DebugBoss.Watches.CurrentWatches.Find(S) = nil then
  begin
    DebugBoss.Watches.CurrentWatches.BeginUpdate;
    try
      Watch := DebugBoss.Watches.CurrentWatches.Add(S);
      Watch.Enabled := True;
      if EnvironmentDebugOpts.DebuggerAutoSetInstanceFromClass then
        Watch.EvaluateFlags := Watch.EvaluateFlags + [defClassAutoCast];
    finally
      DebugBoss.Watches.CurrentWatches.EndUpdate;
    end;
  end;
  DebugBoss.ViewDebugDialog(ddtWatches);
end;

procedure TLocalsDlg.btnShowDataAddrClick(Sender: TObject);
begin
  if btnShowDataAddr.Down then
    vtLocals.Header.Columns[2].Options := vtLocals.Header.Columns[2].Options + [coVisible]
  else
    vtLocals.Header.Columns[2].Options := vtLocals.Header.Columns[2].Options - [coVisible];
end;

procedure TLocalsDlg.FormShow(Sender: TObject);
begin
  LocalsChanged(nil);
end;

procedure TLocalsDlg.ToolButtonPowerClick(Sender: TObject);
begin
  if ToolButtonPower.Down
  then begin
    ToolButtonPower.ImageIndex := FPowerImgIdx;
    LocalsChanged(nil);
  end
  else begin
    ToolButtonPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TLocalsDlg.vtLocalsChange(Sender: TBaseVirtualTree; Node: PVirtualNode
  );
begin

end;

procedure TLocalsDlg.vtLocalsDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
begin

end;

procedure TLocalsDlg.vtLocalsDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin

end;

procedure TLocalsDlg.vtLocalsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin

end;

procedure TLocalsDlg.vtLocalsNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin

end;

function TLocalsDlg.GetSelected: TLocalsValue;
begin
  Result := TLocalsValue(vtLocals.FocusedItem(True));
end;

procedure TLocalsDlg.actInspectExecute(Sender: TObject);
begin
  if GetSelected <> nil then
    DebugBoss.Inspect(GetSelected.Name);
end;

procedure TLocalsDlg.actEvaluateExecute(Sender: TObject);
begin
  if GetSelected <> nil then
  DebugBoss.EvaluateModify(GetSelected.Name);
end;

procedure TLocalsDlg.actCopyNameExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfName], []);
  Clipboard.Close;
end;

procedure TLocalsDlg.actCopyValueExecute(Sender: TObject);
var
  LVal: TLocalsValue;
  ResVal: TWatchResultData;
begin
  LVal := GetSelected;
  if LVal = nil then
    exit;

  ResVal := LVal.ResultData;
  if ResVal = nil then
    exit;

  if (ResVal.ValueKind <> rdkPrePrinted) then begin
    Clipboard.Open;
    Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfValue], []);
    Clipboard.Close;
    exit;
  end;

  if not DebugBoss.Evaluate(LVal.Name, @CopyValueEvaluateCallback, []) then
  begin
    Clipboard.Open;
    Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfValue], []);
    Clipboard.Close;
  end
end;

procedure TLocalsDlg.actCopyRAWValueExecute(Sender: TObject);
var
  LVal: TLocalsValue;
  ResVal: TWatchResultData;
begin
  LVal := GetSelected;
  if LVal = nil then
    exit;

  ResVal := LVal.ResultData;
  if ResVal = nil then
    exit;

  if (ResVal.ValueKind <> rdkPrePrinted) then begin
    Clipboard.Open;
    Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfValue], [vdoUnQuoted]);
    Clipboard.Close;
    exit;
  end;

  if not DebugBoss.Evaluate(LVal.Name, @CopyRAWValueEvaluateCallback, []) then
  begin
    Clipboard.Open;
    Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfValue], [vdoUnQuoted]);
    Clipboard.Close;
  end;
end;

procedure TLocalsDlg.actCopyAddrExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfDataAddress], []);
  Clipboard.Close;
end;

procedure TLocalsDlg.actCopyEntryExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsSelectionOrFocus, [vdfName, vdfDataAddress, vdfValue], []);
  Clipboard.Close;
end;

procedure TLocalsDlg.actCopyAllExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := FLocolsTreeMgr.GetAsText(vdsAll, [vdfName, vdfDataAddress, vdfValue], []);
  Clipboard.Close;
end;

procedure TLocalsDlg.actCopyAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := vtLocals.ChildCount[nil] > 0;
end;

procedure TLocalsDlg.LocalsChanged(Sender: TObject);
var
  n: Integer;
  Locals: TIDELocals;
  Snap: TSnapshot;
  LVal: TIdeLocalsValue;
  VNode, VN2: PVirtualNode;
begin
  if (not ToolButtonPower.Down) or (not Visible) then exit;

  if (DebugBoss = nil) or (ThreadsMonitor = nil) or (CallStackMonitor = nil) or (LocalsMonitor=nil) then begin
    ClearTree;
    exit;
  end;

  if IsUpdating then begin
    DebugLn(DBG_DATA_MONITORS, ['DebugDataWindow: TLocalsDlg.LocalsChanged  in IsUpdating']);
    Include(FUpdateFlags, ufNeedUpdating);
    exit;
  end;
  Exclude(FUpdateFlags, ufNeedUpdating);
  DebugLn(DBG_DATA_MONITORS, ['DebugDataMonitor: TLocalsDlg.LocalsChanged']);

  if GetStackframe < 0 then begin // TODO need dedicated validity property
    ClearTree;
    exit;
  end;

  Snap := GetSelectedSnapshot;

  if (Snap = nil) then begin
    if (DebugBoss.State in [dsInit, dsIdle, dsStop]) then begin
      ClearTree;
      exit;
    end;

    if not (DebugBoss.State in [dsPause, dsInternalPause]) then begin
      ClearTree(True);
      exit;
    end;
  end;

  if (Snap <> nil)
  then begin
    Locals := LocalsMonitor.Snapshots[Snap][GetThreadId, GetStackframe];
    Caption:= lisLocals + ' ('+ Snap.LocationAsText +')';
  end
  else begin
    Locals := LocalsMonitor.CurrentLocalsList[GetThreadId, GetStackframe];
    Caption:= lisLocals;
  end;

  if (Locals = nil) then begin
    ClearTree;
    Exit;
  end;

  if (Locals is TCurrentLocals) and (TCurrentLocals(Locals).Validity in [ddsUnknown, ddsRequested, ddsEvaluating])
  then begin
    Locals.Count; // trigger
    ClearTree(True);
    Exit;
  end;

  BeginUpdate;
  try
    ClearTree(True);
    VN2 := nil;
    for n := 0 to Locals.Count - 1 do begin
      LVal := TIdeLocalsValue(Locals.Entries[n]);
      VNode := vtLocals.FindNodeForText(LVal.DisplayName, 0, True);
      if (VNode <> nil) and (VNode^.PrevSibling <> VN2) then
        vtLocals.LazMoveTo(VNode, VN2, amInsertAfter);
      VNode := FLocolsTreeMgr.AddWatchData(LVal, LVal, VNode);
      VN2 := VNode;
    end;


    VNode := vtLocals.GetFirstNoInit;
    while VNode <> nil do begin
      if (vtLocals.NodeItem[VNode] = nil) and (vtLocals.NodeControl[VNode] = nil) then begin
        VN2 := VNode;
        VNode := vtLocals.GetNextVisibleSiblingNoInit(VNode);
        vtLocals.DeleteNode(VN2);
      end
      else
        VNode := vtLocals.GetNextVisibleNoInit(VNode);
    end;
  finally
    EndUpdate;
    vtLocals.Invalidate;
  end;
end;

procedure TLocalsDlg.SubLocalChanged(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  VNode := vtLocals.FindNodeForItem(Sender);
  if VNode <> nil then
    FLocolsTreeMgr.UpdateWatchData(TSubLocalsValue(Sender), VNode);
end;

function TLocalsDlg.GetThreadId: Integer;
var
  Threads: TIdeThreads;
begin
  Result := -1;
  if (ThreadsMonitor = nil) then exit;
  Threads := GetSelectedThreads(GetSelectedSnapshot);
  if Threads <> nil
  then Result := Threads.CurrentThreadId
  else Result := 1;
end;

function TLocalsDlg.GetSelectedThreads(Snap: TSnapshot): TIdeThreads;
begin
  if ThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

function TLocalsDlg.GetStackframe: Integer;
var
  Snap: TSnapshot;
  Threads: TIdeThreads;
  tid: LongInt;
  Stack: TIdeCallStack;
begin
  if (CallStackMonitor = nil) or (ThreadsMonitor = nil)
  then begin
    Result := 0;
    exit;
  end;

  Snap := GetSelectedSnapshot;
  Threads := GetSelectedThreads(Snap);
  if Threads <> nil
  then tid := Threads.CurrentThreadId
  else tid := 1;

  if (Snap <> nil)
  then Stack := CallStackMonitor.Snapshots[Snap].EntriesForThreads[tid]
  else Stack := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];

  if Stack <> nil
  then Result := Stack.CurrentIndex
  else Result := 0;
end;

function TLocalsDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.SelectedEntry <> nil)
  then Result := SnapshotManager.SelectedEntry;
end;

procedure TLocalsDlg.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
  vtLocals.BeginUpdate;
end;

procedure TLocalsDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if ufNeedUpdating in FUpdateFlags then LocalsChanged(nil);
  vtLocals.EndUpdate;
  vtLocalsChange(nil, nil);
end;

function TLocalsDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if (AColId - 1 >= 0) and (AColId - 1 < vtLocals.Header.Columns.Count) then begin
    ASize := vtLocals.Header.Columns[AColId - 1].Width;
    Result := (ASize <> COL_WIDTHS[AColId - 1]);
  end
  else
    Result := False;
end;

procedure TLocalsDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_LOCALS_NAME:   vtLocals.Header.Columns[0].Width := ASize;
    COL_LOCALS_VALUE:  vtLocals.Header.Columns[1].Width := ASize;
    COL_ADDR_VALUE:    vtLocals.Header.Columns[2].Width := ASize;
  end;
end;

procedure TLocalsDlg.CopyRAWValueEvaluateCallback(Sender: TObject;
  ASuccess: Boolean; ResultText: String; ResultDBGType: TDBGType);
var
  LVal: TLocalsValue;
begin
  Clipboard.Open;
  if ASuccess then
    Clipboard.AsText := ValueToRAW(ExtractValue(ResultText, ResultDBGType.TypeName))
  else
  begin
    LVal := GetSelected;
    if (LVal <> nil) and (LVal.ResultData <> nil) then begin
      Clipboard.Open;
      Clipboard.AsText := ValueToRAW(FWatchPrinter.PrintWatchValue(LVal.ResultData, wdfDefault));
      Clipboard.Close;
    end;
  end;
  Clipboard.Close;
  FreeAndNil(ResultDBGType);
end;

procedure TLocalsDlg.CopyValueEvaluateCallback(Sender: TObject;
  ASuccess: Boolean; ResultText: String; ResultDBGType: TDBGType);
var
  LVal: TLocalsValue;
begin
  Clipboard.Open;
  if ASuccess then
    Clipboard.AsText := ExtractValue(ResultText, ResultDBGType.TypeName)
  else begin
    LVal := GetSelected;
    if (LVal <> nil) and (LVal.ResultData <> nil) then begin
      Clipboard.Open;
      Clipboard.AsText := FWatchPrinter.PrintWatchValue(LVal.ResultData, wdfDefault);
      Clipboard.Close;
    end;
  end;
  Clipboard.Close;
  FreeAndNil(ResultDBGType);
end;

procedure TLocalsDlg.ClearTree(OnlyClearNodeData: boolean);
var
  VNode: PVirtualNode;
begin
  for VNode in vtLocals.NoInitNodes do begin
    vtLocals.NodeItem[VNode] := nil;
    if OnlyClearNodeData then
      vtLocals.NodeText[VNode, 1] := '<not evaluated>';
  end;

  if not OnlyClearNodeData then
    vtLocals.Clear;
end;

{ TDbgTreeViewLocalsValueMgr }

function TDbgTreeViewLocalsValueMgr.WatchAbleResultFromNode(AVNode: PVirtualNode
  ): IWatchAbleResultIntf;
begin
  Result := TIdeLocalsValue(TreeView.NodeItem[AVNode]);
end;

function TDbgTreeViewLocalsValueMgr.WatchAbleResultFromObject(
  AWatchAble: TObject): IWatchAbleResultIntf;
begin
  Result := TIdeLocalsValue(AWatchAble);
end;

function TDbgTreeViewLocalsValueMgr.GetFieldAsText(Nd: PVirtualNode;
  AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
  AField: TTreeViewDataToTextField; AnOpts: TTreeViewDataToTextOptions): String;
var
  ResVal: TWatchResultData;
  da: TDBGPtr;
begin
  if AWatchAbleResult = nil then
    exit(inherited);

  Result := '';
  case AField of
    vdfName:
      if AWatchAble <> nil then
        Result := TIdeLocalsValue(AWatchAble).Name;
    vdfValue: begin
        if AWatchAbleResult = nil then begin
          Result := '<not evaluated>';
          exit;
        end;
        if not AWatchAbleResult.Enabled then begin
          Result := '<disabled>';
          exit;
        end;
        if vdoUnQuoted in AnOpts then begin
          ResVal := AWatchAbleResult.ResultData;
          while ResVal <> nil do begin
            case ResVal.ValueKind of
              rdkVariant: ResVal := ResVal.DerefData;
              rdkConvertRes: ResVal := ResVal.ConvertedRes;
              //rdkPCharOrString:
              else break;
            end;
          end;
          if (ResVal <> nil) and (ResVal.ValueKind in [rdkString, rdkWideString, rdkChar]) then begin
            Result := ResVal.AsString;
            exit;
          end;
        end;

        if vdoAllowMultiLine in AnOpts then
          FLocalsDlg.FWatchPrinter.FormatFlags := [rpfIndent, rpfMultiLine];
        try
          Result := FLocalsDlg.FWatchPrinter.PrintWatchValue(AWatchAbleResult.ResultData, AWatchAbleResult.DisplayFormat);
        finally
          FLocalsDlg.FWatchPrinter.FormatFlags := [rpfClearMultiLine];
        end;
      end;
    vdfDataAddress: begin
      if AWatchAbleResult.ResultData.HasDataAddress then begin
        da := AWatchAbleResult.ResultData.DataAddress;
        if da = 0
        then Result := 'nil'
        else Result := '$' + IntToHex(da, HexDigicCount(da, 4, True));
      end;
    end;
  end;
end;

procedure TDbgTreeViewLocalsValueMgr.UpdateColumnsText(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode);
var
  s: String;
  ResData: TWatchResultData;
  da: TDBGPtr;
begin
  ResData :=  AWatchAbleResult.ResultData;
  if ResData = nil then
    s := ClearMultiline(AWatchAbleResult.Value)
  else
    s := FLocalsDlg.FWatchPrinter.PrintWatchValue(ResData, wdfDefault);
  TreeView.NodeText[AVNode, 0] := TIdeLocalsValue(AWatchAble).DisplayName;
  TreeView.NodeText[AVNode, 1] := s;

  if (ResData <> nil) and (ResData.HasDataAddress) then begin
    da := ResData.DataAddress;
    if da = 0
    then TreeView.NodeText[AVNode, 2] := 'nil'
    else TreeView.NodeText[AVNode, 2] := '$' + IntToHex(da, HexDigicCount(da, 4, True));
  end

end;

procedure TDbgTreeViewLocalsValueMgr.ConfigureNewSubItem(AWatchAble: TObject);
begin
  TSubLocalsValue(AWatchAble).OnChange := @FLocalsDlg.SubLocalChanged;
end;

initialization

  LocalsDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtLocals]);
  LocalsDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  LocalsDlgWindowCreator.OnSetDividerSize := @LocalsDlgColSizeSetter;
  LocalsDlgWindowCreator.OnGetDividerSize := @LocalsDlgColSizeGetter;
  LocalsDlgWindowCreator.DividerTemplate.Add('LocalsName',  COL_LOCALS_NAME,  @drsColWidthName);
  LocalsDlgWindowCreator.DividerTemplate.Add('LocalsValue', COL_LOCALS_VALUE, @drsColWidthValue);
  LocalsDlgWindowCreator.DividerTemplate.Add('LocalsAddr', COL_ADDR_VALUE, @drsColWidthAddr);
  LocalsDlgWindowCreator.CreateSimpleLayout;

  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );

end.

