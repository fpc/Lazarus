{               ----------------------------------------------
                     inspectdlg.pas  -  Inspect Dialog
                ----------------------------------------------

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
unit InspectDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLProc, LCLType, Grids, StdCtrls, Menus, Forms, Controls, Graphics, ComCtrls,
  ExtCtrls, Buttons, Spin, Clipbrd,
  // IdeIntf
  IDEWindowIntf, IDEImagesIntf, ObjectInspector, PropEdits,
  // DebuggerIntf
  DbgIntfDebuggerBase, DbgIntfBaseTypes, LazClasses, SpinEx, LazDebuggerIntf,
  LazDebuggerIntfBaseTypes,
  // IDE
  LazarusIDEStrConsts, BaseDebugManager, InputHistory, IDEProcs, Debugger,
  IdeDebuggerWatchResPrinter, IdeDebuggerWatchResult, IdeDebuggerWatchResUtils,
  IdeDebuggerBase, ArrayNavigationFrame, IdeDebuggerOpts,
  IdeDebuggerBackendValueConv, WatchInspectToolbar, DebuggerDlg,
  DebuggerStrConst, EnvironmentOpts, RecentListProcs;

type

  { TOIDBGGrid }

  TOIDBGGrid=class(TOIPropertyGrid)
  end;

  { TIDEInspectDlg }

  TIDEInspectDlg = class(TDebuggerDlg)
    ErrorLabel: TLabel;
    menuCopyValue: TMenuItem;
    PageControl: TPageControl;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    DataPage: TTabSheet;
    PropertiesPage: TTabSheet;
    MethodsPage: TTabSheet;
    ErrorPage: TTabSheet;
    TimerClearData: TTimer;
    WatchInspectNav1: TWatchInspectNav;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure DataGridDoubleClick(Sender: TObject);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X,
      {%H-}Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure menuCopyValueClick(Sender: TObject);
    procedure TimerClearDataTimer(Sender: TObject);
  private
    //FDataGridHook,
    //FPropertiesGridHook,
    //FMethodsGridHook: TPropertyEditorHook;
    //FDataGrid,
    //FPropertiesGrid,
    //FMethodsGrid: TOIDBGGrid;
    FAlternateExpression: ansistring;
    FUpdatedData: Boolean;
    FWatchPrinter: TWatchResultPrinter;
    FCurrentResData: TWatchResultData;
    FHumanReadable: ansistring;
    FGridData: TStringGrid;
    FGridMethods: TStringGrid;
    FExpressionWasEvaluated: Boolean;

    procedure ArrayNavChanged(Sender: TArrayNavigationBar; AValue: Int64);
    procedure DoAddEval(Sender: TObject);
    procedure DoAddWatch(Sender: TObject);
    function DoBeforeUpdate(ASender: TObject): boolean;
    procedure DoColumnsChanged(Sender: TObject);
    procedure DoDebuggerState(ADebugger: TDebuggerIntf; AnOldState: TDBGState);
    procedure DoEnvOptChanged(Sender: TObject; Restore: boolean);
    procedure DoWatchesInvalidated(Sender: TObject);
    procedure DoWatchUpdated(const ASender: TIdeWatches; const AWatch: TIdeWatch);
    procedure Localize;
    function  ShortenedExpression: String;
    procedure ContextChanged(Sender: TObject);
    procedure InspectResDataSimple;
    procedure InspectResDataPointer;
    procedure InspectResDataEnum;
    procedure InspectResDataSet;
    procedure InspectResDataArray;
    procedure InspectResDataStruct;
    procedure InspectClass;
    procedure InspectRecord;
    procedure InspectVariant;
    procedure InspectSimple;
    procedure InspectEnum;
    procedure InspectSet;
    procedure InspectPointer;
    procedure GridDataSetup(Initial: Boolean = False);
    procedure GridMethodsSetup(Initial: Boolean = False);
    procedure ShowDataFields;
    procedure ShowMethodsFields;
    //procedure ShowError;
    procedure Clear(Sender: TObject = nil);
  protected
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const AExpression: ansistring; AWatch: TWatch = nil);
  end;

implementation

{$R *.lfm}

var
  InspectDlgWindowCreator: TIDEWindowCreator;

const
  MAX_HISTORY = 1000;
  COL_INSPECT_DNAME       = 1;
  COL_INSPECT_DTYPE       = 2;
  COL_INSPECT_DVALUE      = 3;
  COL_INSPECT_DCLASS      = 4;
  COL_INSPECT_DVISIBILITY = 5;
  COL_INSPECT_MNAME       = 11;
  COL_INSPECT_MTYPE       = 12;
  COL_INSPECT_MRETURNS    = 13;
  COL_INSPECT_MADDRESS    = 14;

function InspectDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TIDEInspectDlg;
  if Result then
    Result := TIDEInspectDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure InspectDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TIDEInspectDlg then
    TIDEInspectDlg(AForm).ColSizeSetter(AColId, ASize);
end;

{ TIDEInspectDlg }

procedure TIDEInspectDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TIDEInspectDlg.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,300,400);
end;

procedure TIDEInspectDlg.ContextChanged(Sender: TObject);
begin
  FExpressionWasEvaluated := False;
  WatchInspectNav1.DoContextChanged;
end;

procedure TIDEInspectDlg.InspectResDataSimple;
var
  Res: TWatchResultData;
  v: String;
begin
  Res := FCurrentResData;

  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=Res.TypeName;
  FGridData.Cells[3,1]:=v;
end;

procedure TIDEInspectDlg.InspectResDataPointer;
var
  Res: TWatchResultData;
  v: String;
begin
  Res := FCurrentResData;
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  v := FWatchPrinter.PrintWatchValue(Res, wdfPointer);
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=Res.TypeName;
  FGridData.Cells[3,1]:=v;

  Res := Res.DerefData;
  if Res <> nil then begin
    FGridData.RowCount := 3;
    FGridData.Cells[1,2]:=Format(lisInspectPointerTo, ['']);
    FGridData.Cells[2,2]:=Res.TypeName;
    FGridData.Cells[3,2]:=FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  end;
end;

procedure TIDEInspectDlg.InspectResDataEnum;
var
  Res: TWatchResultData;
  v: String;
begin
  Res := FCurrentResData;

  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False; // anchestor
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown; // typename
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=Res.TypeName;
  // TODO: show declaration (all elements)
  FGridData.Cells[3,1]:=v;
end;

procedure TIDEInspectDlg.InspectResDataSet;
begin
  InspectEnum;
end;

procedure TIDEInspectDlg.InspectResDataArray;
var
  Res, Entry: TWatchResultData;
  LowBnd: Int64;
  i, SubStart: Integer;
  WVal: TWatchValue;
  CurIndexOffs, ResIdxOffs: Int64;
  CurPageCount: Integer;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  Res := FCurrentResData;
  if Res = nil then begin
    TimerClearData.Enabled := True;
    exit;
  end;

  StatusBar1.SimpleText:=ShortenedExpression+': '+Res.TypeName + '  Len: ' + IntToStr(Res.ArrayLength);

  LowBnd := Res.LowBound;
  if FUpdatedData then begin
    WatchInspectNav1.ArrayNavigationBar1.LowBound := LowBnd;
    WatchInspectNav1.ArrayNavigationBar1.HighBound := LowBnd + Res.ArrayLength - 1;
    WatchInspectNav1.ArrayNavigationBar1.Index := LowBnd;
    FUpdatedData := False;
  end;

  CurIndexOffs := WatchInspectNav1.ArrayNavigationBar1.Index - LowBnd;
  CurPageCount := WatchInspectNav1.ArrayNavigationBar1.PageSize;
  if (CurIndexOffs >= 0) and (CurIndexOffs < res.ArrayLength) then
    CurPageCount := Max(1, Min(CurPageCount, res.ArrayLength - CurIndexOffs));

  WVal:= WatchInspectNav1.CurrentWatchValue.Watch.ValueList.GetEntriesForRange(
    WatchInspectNav1.CurrentWatchValue.ThreadId,
    WatchInspectNav1.CurrentWatchValue.StackFrame,
    CurIndexOffs,
    CurPageCount
  );
  WVal.Value;
  if WVal.Validity <> ddsValid then begin
    TimerClearData.Enabled := True;
    exit;
  end;
  WatchInspectNav1.CurrentWatchValue.Watch.ValueList.ClearRangeEntries(5);

  Res := WVal.ResultData;

  GridDataSetup;
  if Res.Count > 0 then begin
    ResIdxOffs := WVal.FirstIndexOffs;
    SubStart := CurIndexOffs - ResIdxOffs;
    CurPageCount := Min(CurPageCount, Res.Count - SubStart);

    FGridData.RowCount:= CurPageCount + 1;
    for i := SubStart to SubStart+CurPageCount-1 do begin
      Res.SetSelectedIndex(i);
      Entry := Res.SelectedEntry;
      FGridData.Cells[1,i+1-SubStart] := IntToStr(LowBnd + ResIdxOffs + i);
      FGridData.Cells[2,i+1-SubStart] := Entry.TypeName;
      FGridData.Cells[3,i+1-SubStart] := FWatchPrinter.PrintWatchValue(Entry, wdfDefault);
    end;
  end;
end;

procedure TIDEInspectDlg.InspectResDataStruct;
const
  FieldLocationNames: array[TLzDbgFieldVisibility] of string = //(dfvUnknown, dfvPrivate, dfvProtected, dfvPublic, dfvPublished);
    ('', 'Private', 'Protected', 'Public', 'Published');
var
  Res, Fld, Fld2: TWatchResultData;
  FldCnt, MethCnt, f, m: Integer;
  FldInfo: TWatchResultDataFieldInfo;
  AnchType: String;
begin
  Res := FCurrentResData;

  FGridData.Columns[0].Visible := (Res.StructType in [dstClass, dstObject]) and WatchInspectNav1.ColClassIsDown; // anchestor
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown; // typename
  FGridData.Columns[4].Visible := (Res.StructType in [dstClass, dstObject]) and WatchInspectNav1.ColVisibilityIsDown; // class-visibility
  WatchInspectNav1.ColClassEnabled      := Res.StructType in [dstClass, dstObject];
  WatchInspectNav1.ColTypeEnabled       := True;
  WatchInspectNav1.ColVisibilityEnabled := Res.StructType in [dstClass, dstObject];

  AnchType := '';
  if Res.Anchestor <> nil then
    AnchType := Res.Anchestor.TypeName;
  if (Res.ValueKind = rdkStruct) and (AnchType <> '') then
    StatusBar1.SimpleText:=Format(lisInspectClassInherit, [ShortenedExpression, Res.TypeName, AnchType])
  else
    StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + FHumanReadable;

  GridDataSetup;
  FldCnt := 0;
  MethCnt := 0;

  if Res.StructType in [dstClass, dstObject] then begin
    for FldInfo in res do begin
      if (FldInfo.Field <> nil) and
         ( (FldInfo.Field.ValueKind in [rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef]) or
           (ExtractProcResFromMethod(FldInfo.Field) <> nil)
         )
      then
        inc(MethCnt)
      else
        inc(FldCnt);
    end;
  end
  else
  for FldInfo in res do
    inc(FldCnt);

  DataPage.TabVisible := FldCnt > 0;
  PropertiesPage.TabVisible :=false;
  MethodsPage.TabVisible := MethCnt > 0;
  if not (PageControl.ActivePage = MethodsPage) then
    PageControl.ActivePage := DataPage;

  FGridData.RowCount    := max(FldCnt+1, 2);
  FGridMethods.RowCount := max(MethCnt+1, 2);
  f := 1;
  m := 1;
  for FldInfo in res do begin
    Fld := FldInfo.Field;
    Fld2 := ExtractProcResFromMethod(Fld);
    if (MethCnt > 0) and
       (Fld <> nil) and
       ( (Fld.ValueKind in [rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef]) or
         (Fld2 <> nil)
       )
    then begin
      if Fld2 = nil then Fld2 := Fld;

      FGridMethods.Cells[0,m] := FldInfo.FieldName;

      if Fld <> nil then begin
        if Fld2.ValueKind in [rdkFunction, rdkProcedure] then begin
          if dffConstructor in FldInfo.FieldFlags
          then FGridMethods.Cells[1,m] := 'Constructor'
          else if dffDestructor in FldInfo.FieldFlags
          then FGridMethods.Cells[1,m] := 'Destructor'
          else if Fld2.ValueKind = rdkFunction
          then FGridMethods.Cells[1,m] := 'Function'
          else if Fld2.ValueKind = rdkPCharOrString
          then FGridMethods.Cells[1,m] := 'Procedure'
          else FGridMethods.Cells[1,m] := '';
        end
        else
          FGridMethods.Cells[1,m] := Fld.TypeName;
      end
      else
        FGridMethods.Cells[1,m] := '';

      FGridMethods.Cells[2,m] := '';

      if Fld2 = nil then Fld2 := Fld;
      if Fld2 <> nil
      then FGridMethods.Cells[3,m] := IntToHex(Fld2.AsQWord, 16)
      else FGridMethods.Cells[3,m] := '';

      inc(m);
    end
    else begin
      if FldInfo.Owner <> nil
      then FGridData.Cells[0,f] := FldInfo.Owner.TypeName
      else FGridData.Cells[0,f] := '';

      FGridData.Cells[1,f] := FldInfo.FieldName;

      if Fld <> nil
      then FGridData.Cells[2,f] := Fld.TypeName
      else FGridData.Cells[2,f] := '';

      if Fld <> nil
      then FGridData.Cells[3,f] := FWatchPrinter.PrintWatchValue(Fld, wdfDefault)
      else FGridData.Cells[3,f] := '<error>';

      FGridData.Cells[4,f] := FieldLocationNames[FldInfo.FieldVisibility];

      inc(f);
    end;
  end;
end;

procedure TIDEInspectDlg.DataGridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  g: TStringGrid;
  Cur: TPoint;
begin
  if Button = mbExtra1 then WatchInspectNav1.GoPrevBrowseEntry
  else
  if Button = mbExtra2 then WatchInspectNav1.GoNextBrowseEntry
  else
  if Button = mbRight then begin
    if (PageControl.ActivePage = DataPage) then
      g := FGridData
    else
    if (PageControl.ActivePage = MethodsPage) then
      g := FGridMethods
    else
      exit;

    Cur:= g.MouseToCell(Point(x,y));
    if (Cur.Y > 0) and (Cur.Y < g.RowCount) then
      g.Row := Cur.Y;
  end;
end;

procedure TIDEInspectDlg.FormShow(Sender: TObject);
begin
  FCurrentResData := nil;
  WatchInspectNav1.UpdateData(True);
  WatchInspectNav1.FocusEnterExpression;
end;

procedure TIDEInspectDlg.menuCopyValueClick(Sender: TObject);
var
  i: Integer;
begin
  if (PageControl.ActivePage = DataPage) then begin
    i := FGridData.Row;
    if (i < 1) or (i >= FGridData.RowCount) then exit;
    Clipboard.AsText := FGridData.Cells[3, i];
  end
  else
  if (PageControl.ActivePage = MethodsPage) then begin
    i := FGridMethods.Row;
    if (i < 1) or (i >= FGridMethods.RowCount) then exit;
    Clipboard.AsText := FGridMethods.Cells[3, i];
  end
  else
  if (PageControl.ActivePage = ErrorPage) then begin
    Clipboard.AsText := ErrorLabel.Caption;
  end;
end;

procedure TIDEInspectDlg.TimerClearDataTimer(Sender: TObject);
begin
  if not TimerClearData.Enabled then
    exit;
  TimerClearData.Enabled := False;
  Clear;
end;

procedure TIDEInspectDlg.DataGridDoubleClick(Sender: TObject);
var
  i: Integer;
  s, t: String;
begin
  if (WatchInspectNav1.CurrentWatchValue = nil) or (WatchInspectNav1.Expression = '') then exit;

  if WatchInspectNav1.CurrentWatchValue.TypeInfo <> nil then begin

    if (WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind in [skClass, skRecord, skObject]) then begin
      i := FGridData.Row;
      if (i < 1) or (i >= FGridData.RowCount) then exit;
      s := FGridData.Cells[1, i];

      if WatchInspectNav1.UseInstanceIsDown and (WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind = skClass) then
        Execute(FGridData.Cells[0, i] + '(' + WatchInspectNav1.Expression + ').' + s)
      else
        Execute(WatchInspectNav1.Expression + '.' + s);
      exit;
    end;

    if (WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind in [skPointer]) then begin
      i := FGridData.Row;
      if (i < 1) or (i >= FGridData.RowCount) then exit;
      s := FGridData.Cells[1, i];
      t := FGridData.Cells[2, i];
      Execute('(' + WatchInspectNav1.Expression + ')^');
      if not FExpressionWasEvaluated then
        FAlternateExpression := t + '(' + WatchInspectNav1.Expression + ')[0]';
      exit;
    end;

    if (WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind in [skSimple]) and (WatchInspectNav1.CurrentWatchValue.TypeInfo.Attributes*[saArray,saDynArray] <> []) then begin
      if WatchInspectNav1.CurrentWatchValue.TypeInfo.Len < 1 then exit;
      if WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count > 0 then begin
        i := FGridData.Row;
        if (i < 1) or (i >= FGridData.RowCount) then exit;
        s := FGridData.Cells[1, i];
        Execute(WatchInspectNav1.Expression + '[' + s + ']');
      end
      else begin
        //
      end;
    end;

  end
  else
  if FCurrentResData <> nil then begin
    case FCurrentResData.ValueKind of
      rdkPointerVal: begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;
          s := FGridData.Cells[1, i];
          t := FGridData.Cells[2, i];
          Execute('(' + WatchInspectNav1.Expression + ')^');
          if not FExpressionWasEvaluated then
            FAlternateExpression := t + '(' + WatchInspectNav1.Expression + ')[0]';
        end;
      rdkArray: begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;
          s := FGridData.Cells[1, i];
          Execute(WatchInspectNav1.Expression + '[' + s + ']');
        end;
      rdkStruct: begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;
          s := FGridData.Cells[1, i];

          if WatchInspectNav1.UseInstanceIsDown and (FCurrentResData.StructType in [dstClass, dstObject]) then
            Execute(FGridData.Cells[0, i] + '(' + WatchInspectNav1.Expression + ').' + s)
          else
            Execute(WatchInspectNav1.Expression + '.' + s);
        end;

      otherwise begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;

          if FCurrentResData.ArrayLength > 0 then begin
            s := WatchInspectNav1.CurrentWatchValue.ExpressionForChildEntry(FGridData.Cells[1, i]);
            if s <> '' then
              Execute(s);
          end
          else
          if FCurrentResData.FieldCount > 0 then begin
            s := WatchInspectNav1.CurrentWatchValue.ExpressionForChildField(FGridData.Cells[1, i]);
            if s <> '' then
              Execute(s);
          end;
        end;
    end;
  end;

end;

procedure TIDEInspectDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (not Docked) and
     (not WatchInspectNav1.DropDownOpen)
  then
    Close;
end;

procedure TIDEInspectDlg.Localize;
begin
  Caption := lisInspectDialog;
  DataPage.Caption := lisInspectData;
  PropertiesPage.Caption := lisInspectProperties;
  MethodsPage.Caption := lisInspectMethods;
end;

function TIDEInspectDlg.ShortenedExpression: String;
const
  MAX_SHORT_EXPR_LEN = 25;
begin
  Result := WatchInspectNav1.Expression;
  if Length(Result) > MAX_SHORT_EXPR_LEN then
    Result := copy(Result, 1, MAX_SHORT_EXPR_LEN-3) + '...';
end;

procedure TIDEInspectDlg.InspectClass;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=true;
  if not (PageControl.ActivePage = MethodsPage) then
    PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := WatchInspectNav1.ColClassIsDown;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := WatchInspectNav1.ColVisibilityIsDown;
  WatchInspectNav1.ColClassEnabled := True;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := True;


  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields) then exit;
  StatusBar1.SimpleText:=Format(lisInspectClassInherit, [ShortenedExpression, WatchInspectNav1.CurrentWatchValue.TypeInfo.
    TypeName, WatchInspectNav1.CurrentWatchValue.TypeInfo.Ancestor]);
  GridDataSetup;
  ShowDataFields;
  //FGridData.AutoSizeColumn(1);
  //FGridData.AutoSizeColumn(2);
  GridMethodsSetup;
  ShowMethodsFields;
  //FGridMethods.AutoSizeColumn(1);
  //FGridMethods.AutoSizeColumn(3);
end;

procedure TIDEInspectDlg.InspectVariant;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : Variant';
  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:='Variant';
  FGridData.Cells[3,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(1);
end;

procedure TIDEInspectDlg.InspectRecord;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName;
  GridDataSetup;
  ShowDataFields;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectSimple;
var
  j: Integer;
  fld: TDBGField;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  GridDataSetup;

  if WatchInspectNav1.CurrentWatchValue.TypeInfo.Attributes*[saArray,saDynArray] <> [] then begin
    if WatchInspectNav1.CurrentWatchValue.TypeInfo.Len >= 0 then
      StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = Len:' + IntToStr(WatchInspectNav1.CurrentWatchValue.TypeInfo.Len) + ' ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString
    else
      StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;

    if WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count > 0 then begin
      FGridData.RowCount:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count+1;
      for j := 0 to WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count-1 do begin
        fld := WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j];
        FGridData.Cells[1,j+1]:=fld.Name; // index
        FGridData.Cells[2,j+1]:=fld.DBGType.TypeName;
        FGridData.Cells[3,j+1]:=fld.DBGType.Value.AsString;
      end;
      exit;
    end;
  end
  else
    StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;

  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName;
  FGridData.Cells[3,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectEnum;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName;
  if (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName <> '') and (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeDeclaration <> '')
  then FGridData.Cells[2,1] := FGridData.Cells[2,1] + ' = ';
  FGridData.Cells[2,1] := FGridData.Cells[2,1] + WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeDeclaration;
  FGridData.Cells[3,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectSet;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  FGridData.Cells[2,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName;
  if (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName <> '') and (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeDeclaration <> '')
  then FGridData.Cells[2,1] := FGridData.Cells[2,1] + ' = ';
  FGridData.Cells[2,1] := FGridData.Cells[2,1] + WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeDeclaration;
  FGridData.Cells[3,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectPointer;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
  FGridData.Columns[4].Visible := False;
  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := True;
  WatchInspectNav1.ColVisibilityEnabled := False;

  if not Assigned(WatchInspectNav1.CurrentWatchValue) then exit;
  if not Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName + ' = ' + WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=WatchInspectNav1.Expression;
  if (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName <> '') and (WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName[1] = '^')
  then FGridData.Cells[2, 1]:=Format(lisInspectPointerTo, [copy(WatchInspectNav1.CurrentWatchValue.TypeInfo.
    TypeName, 2, length(WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName))])
  else FGridData.Cells[2,1]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.TypeName;
  {$PUSH}{$RANGECHECKS OFF}
  FGridData.Cells[3,1]:=format('$%x',[{%H-}PtrUInt(WatchInspectNav1.CurrentWatchValue.TypeInfo.Value.AsPointer)]);
  {$POP}
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.GridDataSetup(Initial: Boolean = False);
begin
  if Initial then
    with FGridData do begin
      Clear;
      BorderStyle:=bsNone;
      BorderWidth:=0;
      DefaultColWidth:=100;
      Options:=[goColSizing,goDblClickAutoSize,goDrawFocusSelected, goThumbTracking,
                          goVertLine,goHorzLine,goFixedHorzLine,goSmoothScroll,
                          goTabs,goRowSelect];
      MouseWheelOption := mwGrid;
      Align:=alClient;
      TitleFont.Style:=[fsBold];
      ExtendedSelect:=false;
      RowCount:=2;
      FixedRows:=1;
      FixedCols:=0;
      ColCount:=5;
      //Cols[0].Text:='Class';
      //Cols[1].Text:='Name';
      //Cols[2].Text:='Type';
      //Cols[3].Text:='Value';
      //Cols[4].Text:='Visibility';
      Color:=clBtnFace;
      Columns.Add.Title.Caption:=lisColClass;
      Columns.Add.Title.Caption:=lisName;
      Columns.Add.Title.Caption:=dlgEnvType;
      Columns.Add.Title.Caption:=lisValue;
      Columns.Add.Title.Caption:=lisColVisibility;
    end;
  FGridData.RowCount:=1;
  FGridData.RowCount:=2;
  FGridData.FixedRows:=1;
  FGridData.Visible := True;
end;

procedure TIDEInspectDlg.GridMethodsSetup(Initial: Boolean = False);
begin
  if Initial then
    with FGridMethods do begin
      Clear;
      BorderStyle:=bsNone;
      BorderWidth:=0;
      DefaultColWidth:=100;
      Options:=[goColSizing,goDblClickAutoSize,goDrawFocusSelected, goThumbTracking,
                          goVertLine,goHorzLine,goFixedHorzLine,goSmoothScroll,
                          goTabs,goRowSelect];
      MouseWheelOption := mwGrid;
      Align:=alClient;
      TitleFont.Style:=[fsBold];
      ExtendedSelect:=false;
      RowCount:=2;
      FixedRows:=1;
      FixedCols:=0;
      ColCount:=4;
      Cols[0].Text:=lisName;
      Cols[1].Text:=dlgEnvType;
      Cols[2].Text:=lisColReturns;
      Cols[3].Text:=lisColAddress;
      Color:=clBtnFace;
    end;
  FGridMethods.RowCount:=1;
  FGridMethods.RowCount:=2;
  FGridMethods.FixedRows:=1;
end;

procedure TIDEInspectDlg.ShowDataFields;
const
  FieldLocationNames: array[TDBGFieldLocation] of string = //(flPrivate, flProtected, flPublic, flPublished);
    ('Private', 'Protected', 'Public', 'Published');
var
  j,k: SizeInt;
  fld: TDBGField;
begin
  k:=0;
  for j := 0 to WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skSimple,skRecord,skVariant,skPointer: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridData.RowCount:=k;
  k:=0;
  for j := 0 to WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    fld := WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j];
    case fld.DBGType.Kind of
      skSimple:
        begin
          inc(k);
          FGridData.Cells[1,k]:=fld.Name;
          FGridData.Cells[2,k]:=fld.DBGType.TypeName;
          if fld.DBGType.Value.AsString='$0' then begin
            if fld.DBGType.TypeName='ANSISTRING' then begin
              FGridData.Cells[3,k]:='''''';
            end else begin
              FGridData.Cells[3,k]:='nil';
            end;
          end else begin
            FGridData.Cells[3,k]:=fld.DBGType.Value.AsString;
          end;
          FGridData.Cells[0,k]:=fld.ClassName;
          FGridData.Cells[4,k]:=FieldLocationNames[fld.Location];
        end;
      skRecord:
        begin
          inc(k);
          FGridData.Cells[1,k]:=fld.Name;
          FGridData.Cells[2,k]:='Record '+fld.DBGType.TypeName;
          FGridData.Cells[3,k]:=fld.DBGType.Value.AsString;
          FGridData.Cells[0,k]:=fld.ClassName;
          FGridData.Cells[4,k]:=FieldLocationNames[fld.Location];
        end;
      skVariant:
        begin
          inc(k);
          FGridData.Cells[1,k]:=fld.Name;
          FGridData.Cells[2,k]:='Variant';
          FGridData.Cells[3,k]:=fld.DBGType.Value.AsString;
          FGridData.Cells[0,k]:=fld.ClassName;
          FGridData.Cells[4,k]:=FieldLocationNames[fld.Location];
        end;
      skProcedure,skProcedureRef:
        begin
        end;
      skFunction,skFunctionRef:
        begin
        end;
       skPointer:
        begin
          inc(k);
          FGridData.Cells[1,k]:=fld.Name;
          FGridData.Cells[2,k]:='Pointer '+fld.DBGType.TypeName;
          FGridData.Cells[3,k]:=fld.DBGType.Value.AsString;
          FGridData.Cells[0,k]:=fld.ClassName;
          FGridData.Cells[4,k]:=FieldLocationNames[fld.Location];
        end;
      else
        raise Exception.Create('Inspect: Unknown type in record ->'+inttostr(ord(fld.DBGType.Kind)));
    end;
  end;
end;

procedure TIDEInspectDlg.ShowMethodsFields;
var
  j,k: SizeInt;
begin
  k:=0;
  for j := 0 to WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skProcedure,skFunction,skProcedureRef, skFunctionRef: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridMethods.RowCount:=k;
  k:=0;
  for j := 0 to WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skProcedure, skProcedureRef:
        begin
          inc(k);
          FGridMethods.Cells[0,k]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].Name;
          if ffDestructor in WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].Flags then begin
            FGridMethods.Cells[1,k]:='Destructor';
          end else begin
            FGridMethods.Cells[1,k]:='Procedure';
          end;
          FGridMethods.Cells[2,k]:='';
          FGridMethods.Cells[3,k]:='???';
        end;
      skFunction, skFunctionRef:
        begin
          inc(k);
          FGridMethods.Cells[0,k]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].Name;
          if ffConstructor in WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].Flags then begin
            FGridMethods.Cells[1,k]:='Constructor';
          end else begin
            FGridMethods.Cells[1,k]:='Function';
          end;
          if Assigned(WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].DBGType.Result) then begin
            FGridMethods.Cells[2,k]:=WatchInspectNav1.CurrentWatchValue.TypeInfo.Fields[j].DBGType.Result.TypeName;
          end else begin
            FGridMethods.Cells[2,k]:='';
          end;
          FGridMethods.Cells[3,k]:='???';
        end;
    end;
  end;
end;

procedure TIDEInspectDlg.Clear(Sender: TObject);
begin
  DataPage.TabVisible:=false;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  ErrorPage.TabVisible:=false;
  GridDataSetup;
  FGridData.Visible := False;
  StatusBar1.SimpleText:='';
end;

function TIDEInspectDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  ASize := -1;
  case AColId of
    COL_INSPECT_DNAME:    ASize := FGridData.ColWidths[1];
    COL_INSPECT_DTYPE:    ASize := FGridData.ColWidths[2];
    COL_INSPECT_DVALUE:   ASize := FGridData.ColWidths[3];
    COL_INSPECT_DCLASS:   ASize := FGridData.ColWidths[0];
    COL_INSPECT_DVISIBILITY:   ASize := FGridData.ColWidths[4];
    COL_INSPECT_MNAME:    ASize := FGridMethods.ColWidths[0];
    COL_INSPECT_MTYPE:    ASize := FGridMethods.ColWidths[1];
    COL_INSPECT_MRETURNS: ASize := FGridMethods.ColWidths[2];
    COL_INSPECT_MADDRESS: ASize := FGridMethods.ColWidths[3];
  end;
  Result := (ASize > 0) and (ASize <> 100); // The default for all
end;

procedure TIDEInspectDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_INSPECT_DNAME:    FGridData.ColWidths[1]:= ASize;
    COL_INSPECT_DTYPE:    FGridData.ColWidths[2]:= ASize;
    COL_INSPECT_DVALUE:   FGridData.ColWidths[3]:= ASize;
    COL_INSPECT_DCLASS:   FGridData.ColWidths[0]:= ASize;
    COL_INSPECT_DVISIBILITY:   FGridData.ColWidths[4]:= ASize;
    COL_INSPECT_MNAME:    FGridMethods.ColWidths[0]:= ASize;
    COL_INSPECT_MTYPE:    FGridMethods.ColWidths[1]:= ASize;
    COL_INSPECT_MRETURNS: FGridMethods.ColWidths[2]:= ASize;
    COL_INSPECT_MADDRESS: FGridMethods.ColWidths[3]:= ASize;
  end;
end;

constructor TIDEInspectDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Localize;

  ThreadsMonitor := DebugBoss.Threads;
  CallStackMonitor := DebugBoss.CallStack;
  WatchesMonitor := DebugBoss.Watches;
  WatchesNotification.OnUpdate    := @DoWatchUpdated;

  FWatchPrinter := TWatchResultPrinter.Create;

  ThreadsNotification.OnCurrent := @ContextChanged;
  CallstackNotification.OnCurrent := @ContextChanged;

  DebugBoss.RegisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.RegisterWatchesInvalidatedHandler(@DoWatchesInvalidated);


  WatchInspectNav1.Init(WatchesMonitor, ThreadsMonitor, CallStackMonitor, [defExtraDepth, defFullTypeInfo]);
  WatchInspectNav1.HistoryList := InputHistories.HistoryLists.
    GetList(ClassName,True,rltCaseSensitive);

  WatchInspectNav1.OnArrayIndexChanged := @ArrayNavChanged;
  WatchInspectNav1.OnArrayPageSize := @ArrayNavChanged;
  WatchInspectNav1.ShowArrayNav := False;

  FGridData:=TStringGrid.Create(DataPage);
  DataPage.InsertControl(FGridData);
  GridDataSetup(True);

  FGridMethods:=TStringGrid.Create(MethodsPage);
  MethodsPage.InsertControl(FGridMethods);
  GridMethodsSetup(True);

  FGridData.OnDblClick := @DataGridDoubleClick;
  FGridData.OnMouseDown := @DataGridMouseDown;
  FGridData.PopupMenu := PopupMenu1;
  FGridMethods.OnMouseDown := @DataGridMouseDown;
  FGridMethods.PopupMenu := PopupMenu1;

  WatchInspectNav1.btnUseInstance.Down := EnvironmentOptions.DebuggerAutoSetInstanceFromClass;

  WatchInspectNav1.ColClassEnabled := False;
  WatchInspectNav1.ColTypeEnabled := False;
  WatchInspectNav1.ColVisibilityEnabled := False;

  WatchInspectNav1.ShowEvalHist := False;
  WatchInspectNav1.ShowAddInspect := False;
  WatchInspectNav1.ShowDisplayFormat := False;

  WatchInspectNav1.OnAddWatchClicked := @DoAddWatch;
  WatchInspectNav1.OnAddEvaluateClicked := @DoAddEval;

  menuCopyValue.Caption := lisLocalsDlgCopyValue;

  Clear;

  WatchInspectNav1.OnClear := @Clear;
  WatchInspectNav1.OnBeforeEvaluate := @DoBeforeUpdate;
  WatchInspectNav1.OnWatchUpdated := @DoWatchUpdated;
  WatchInspectNav1.OnColumnsChanged := @DoColumnsChanged;

  EnvironmentOptions.AddHandlerAfterWrite(@DoEnvOptChanged);
  DoEnvOptChanged(nil, False);
end;

destructor TIDEInspectDlg.Destroy;
begin
  DebugBoss.UnregisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.UnregisterWatchesInvalidatedHandler(@DoWatchesInvalidated);
  EnvironmentOptions.RemoveHandlerAfterWrite(@DoEnvOptChanged);
  FCurrentResData := nil;
  FreeAndNil(FWatchPrinter);
  inherited Destroy;

end;

procedure TIDEInspectDlg.Execute(const AExpression: ansistring; AWatch: TWatch);
begin
  if AWatch <> nil then
    WatchInspectNav1.ReadFromWatch(AWatch, AExpression)
  else
    WatchInspectNav1.Execute(AExpression);
end;

procedure TIDEInspectDlg.DoWatchUpdated(const ASender: TIdeWatches;
  const AWatch: TIdeWatch);
begin
  if (WatchInspectNav1.CurrentWatchValue = nil) or
     not (WatchInspectNav1.CurrentWatchValue.Validity in [ddsError, ddsInvalid, ddsValid])
  then
    exit;
  if (AWatch <> WatchInspectNav1.CurrentWatchValue.Watch) or
     (ASender <> WatchInspectNav1.Watches)
  then
    exit;

  if (WatchInspectNav1.CurrentWatchValue.Validity in [ddsError, ddsInvalid]) and
     (FAlternateExpression <> '')
  then begin
    WatchInspectNav1.DeleteLastHistoryIf(WatchInspectNav1.Expression);
    Execute(FAlternateExpression);
    FAlternateExpression := '';
    exit;
  end;

  TimerClearData.Enabled := False;

  FAlternateExpression := '';
  FExpressionWasEvaluated := True;
  FCurrentResData := WatchInspectNav1.CurrentWatchValue.ResultData;
  FHumanReadable := FWatchPrinter.PrintWatchValue(FCurrentResData, wdfStructure);

  if WatchInspectNav1.CurrentWatchValue.Validity = ddsValid then begin
    if WatchInspectNav1.CurrentWatchValue.TypeInfo <> nil then begin
      WatchInspectNav1.ShowArrayNav := False;
      case WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind of
        skClass, skObject, skInterface: InspectClass();
        skRecord: InspectRecord();
        skVariant: InspectVariant();
        skEnum: InspectEnum;
        skSet: InspectSet;
        skProcedure, skProcedureRef: InspectSimple;
        skFunction, skFunctionRef: InspectSimple;
        skSimple,
        skInteger,
        skCardinal, skBoolean, skChar, skFloat: InspectSimple();
        skArray: InspectSimple();
        skPointer: InspectPointer();
        skString, skAnsiString, skWideString: InspectSimple;
      //  skDecomposable: ;
        else begin
            Clear;
            StatusBar1.SimpleText:=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
            ErrorLabel.Caption :=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
            PageControl.ActivePage := ErrorPage;
          end;
      end;
    end
    else begin
    // resultdata

      if (FCurrentResData.ValueKind = rdkConvertRes)
      then begin
        if (FCurrentResData.FieldCount > 0) then
        //if (FCurrentResData.FieldCount = 1) then
          FCurrentResData := FCurrentResData.Fields[0].Field;

        if (FCurrentResData.FieldCount > 1) and
           ( (FCurrentResData.Fields[0].Field = nil) or
             (FCurrentResData.Fields[0].Field.ValueKind = rdkError)
           )
        then
          FCurrentResData := FCurrentResData.Fields[1].Field;
      end;


      WatchInspectNav1.ShowArrayNav := (FCurrentResData.ValueKind = rdkArray) or
        (FCurrentResData.ArrayLength > 0);
      WatchInspectNav1.ArrayNavigationBar1.HardLimits := (FCurrentResData.ValueKind <> rdkArray);

      if FCurrentResData.ArrayLength > 0 then
        InspectResDataArray

      else
      if FCurrentResData.FieldCount > 0 then
        InspectResDataStruct

      else
      case FCurrentResData.ValueKind of
        //rdkError: ;
        rdkPrePrinted,
        rdkString,
        rdkWideString,
        rdkChar,
        rdkSignedNumVal,
        rdkUnsignedNumVal,
        rdkFloatVal,
        rdkBool,
        rdkPCharOrString,
        rdkFunction,
        rdkProcedure,
        rdkFunctionRef,
        rdkProcedureRef:  InspectResDataSimple;
        rdkPointerVal:    InspectResDataPointer;
        rdkEnum:          InspectResDataEnum;
        rdkEnumVal:       InspectResDataEnum;
        rdkSet:           InspectResDataSet;
        rdkArray:         InspectResDataArray;
        rdkStruct:        InspectResDataStruct;
//        rdkConvertRes:    InspectResDataStruct;
        else begin
            Clear;
            StatusBar1.SimpleText:=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
            ErrorLabel.Caption :=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
            PageControl.ActivePage := ErrorPage;
          end;
      end;
    end;

    exit
  end;

  Clear;
  StatusBar1.SimpleText:=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
  ErrorLabel.Caption :=Format(lisInspectUnavailableError, [ShortenedExpression, FHumanReadable]);
  PageControl.ActivePage := ErrorPage;
end;

procedure TIDEInspectDlg.DoDebuggerState(ADebugger: TDebuggerIntf;
  AnOldState: TDBGState);
begin
  if (not WatchInspectNav1.PowerIsDown) or (not Visible) then exit;
  if (ADebugger.State = dsPause) and (AnOldState <> dsPause) then begin
    FCurrentResData := nil;
    WatchInspectNav1.UpdateData(True);
  end;
end;

procedure TIDEInspectDlg.DoEnvOptChanged(Sender: TObject; Restore: boolean);
begin
  WatchInspectNav1.ShowCallFunction := EnvironmentOptions.DebuggerAllowFunctionCalls;
  WatchInspectNav1.EdInspect.DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TIDEInspectDlg.ArrayNavChanged(Sender: TArrayNavigationBar;
  AValue: Int64);
begin
  if (FCurrentResData = nil) or (FCurrentResData.ValueKind <> rdkArray) then
    exit;
  InspectResDataArray;
end;

procedure TIDEInspectDlg.DoAddEval(Sender: TObject);
var
  w: TIdeWatch;
begin
  w := nil;
  if WatchInspectNav1.CurrentWatchValue <> nil then
    w := WatchInspectNav1.CurrentWatchValue.Watch;
  DebugBoss.EvaluateModify(WatchInspectNav1.Expression, w);
end;

procedure TIDEInspectDlg.DoAddWatch(Sender: TObject);
var
  w: TCurrentWatch;
begin
  if DebugBoss = nil then
    exit;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  try
    w := DebugBoss.Watches.CurrentWatches.Find(WatchInspectNav1.Expression);
    if w = nil then
      w := DebugBoss.Watches.CurrentWatches.Add(WatchInspectNav1.Expression);
    if (w <> nil) then begin
      WatchInspectNav1.InitWatch(w);
      w.Enabled := True;
      DebugBoss.ViewDebugDialog(ddtWatches, False);
    end;
  finally
    DebugBoss.Watches.CurrentWatches.EndUpdate;
  end;
end;

function TIDEInspectDlg.DoBeforeUpdate(ASender: TObject): boolean;
begin
  FExpressionWasEvaluated := False;
  FAlternateExpression := '';
  FUpdatedData := True;

  Result := DebugBoss.State = dsPause;

  if Result then
    FCurrentResData := nil;
end;

procedure TIDEInspectDlg.DoColumnsChanged(Sender: TObject);
begin
  if (WatchInspectNav1.CurrentWatchValue = nil) then exit;

  if ( (WatchInspectNav1.CurrentWatchValue.TypeInfo <> nil) and
       (WatchInspectNav1.CurrentWatchValue.TypeInfo.Kind = skClass)
     ) or
     ( FCurrentResData.StructType in [dstClass, dstObject] )
  then begin
    FGridData.Columns[0].Visible := WatchInspectNav1.ColClassIsDown;
    FGridData.Columns[4].Visible := WatchInspectNav1.ColVisibilityIsDown;
  end;

  FGridData.Columns[2].Visible := WatchInspectNav1.ColTypeIsDown;
end;

procedure TIDEInspectDlg.DoWatchesInvalidated(Sender: TObject);
begin
  if (not WatchInspectNav1.PowerIsDown) or (not Visible) then exit;
  FCurrentResData := nil;
  WatchInspectNav1.UpdateData(True);
end;

initialization

  InspectDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtInspect]);
  InspectDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  InspectDlgWindowCreator.OnSetDividerSize := @InspectDlgColSizeSetter;
  InspectDlgWindowCreator.OnGetDividerSize := @InspectDlgColSizeGetter;
  InspectDlgWindowCreator.DividerTemplate.Add('InspectDataName',       COL_INSPECT_DNAME, @drsInspectColWidthDataName);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectDataType',       COL_INSPECT_DTYPE, @drsInspectColWidthDataType);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectDataValue',      COL_INSPECT_DVALUE, @drsInspectColWidthDataValue);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectDataClass',      COL_INSPECT_DCLASS, @drsInspectColWidthDataClass);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectDataVisibility', COL_INSPECT_DVISIBILITY, @drsInspectColWidthDataVisibility);

  InspectDlgWindowCreator.DividerTemplate.Add('InspectMethName',    COL_INSPECT_MNAME,    @drsInspectColWidthMethName);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectMethType',    COL_INSPECT_MTYPE,    @drsInspectColWidthMethType);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectMethReturns', COL_INSPECT_MRETURNS, @drsInspectColWidthMethReturns);
  InspectDlgWindowCreator.DividerTemplate.Add('InspectMethAddress', COL_INSPECT_MADDRESS, @drsInspectColWidthMethAddress);
  InspectDlgWindowCreator.CreateSimpleLayout;

end.

