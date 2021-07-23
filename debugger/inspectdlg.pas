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
  IdeDebuggerBase, ArrayNavigationFrame, DebuggerDlg, DebuggerStrConst,
  EnvironmentOpts;

type

  { TOIDBGGrid }

  TOIDBGGrid=class(TOIPropertyGrid)
  end;

  { TIDEInspectDlg }

  TIDEInspectDlg = class(TDebuggerDlg)
    ArrayNavigationBar1: TArrayNavigationBar;
    btnUseConverter: TToolButton;
    EdInspect: TComboBox;
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
    ToolBar1: TToolBar;
    btnUseInstance: TToolButton;
    btnBackward: TToolButton;
    BtnAddWatch: TToolButton;
    btnPower: TToolButton;
    tbDiv1: TToolButton;
    tbDiv5: TToolButton;
    tbDiv3: TToolButton;
    btnColClass: TToolButton;
    btnColType: TToolButton;
    btnColVisibility: TToolButton;
    btnForward: TToolButton;
    tbDiv4: TToolButton;
    tbDiv2: TToolButton;
    procedure BtnAddWatchClick(Sender: TObject);
    procedure btnBackwardClick(Sender: TObject);
    procedure btnColClassClick(Sender: TObject);
    procedure btnForwardClick(Sender: TObject);
    procedure btnPowerClick(Sender: TObject);
    procedure btnUseConverterClick(Sender: TObject);
    procedure btnUseInstanceClick(Sender: TObject);
    procedure EdInspectEditingDone(Sender: TObject);
    procedure EdInspectKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
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
    FExpression, FAlternateExpression: ansistring;
    FUpdatedData: Boolean;
    FWatchPrinter: TWatchResultPrinter;
    FInspectWatches: TCurrentWatches;
    FCurrentWatchValue: TIdeWatchValue;
    FCurrentResData: TWatchResultData;
    FHumanReadable: ansistring;
    FGridData: TStringGrid;
    FGridMethods: TStringGrid;
    FExpressionWasEvaluated: Boolean;
    FHistory: TStringList;
    FHistoryIndex: Integer;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;

    procedure ArrayNavChanged(Sender: TArrayNavigationBar; AValue: Int64);
    procedure DoDebuggerState(ADebugger: TDebuggerIntf; AnOldState: TDBGState);
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
    procedure Clear;
    procedure GotoHistory(AIndex: Integer);
  protected
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
    procedure InternalExecute(const AExpression: ansistring);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const AExpression: ansistring);
    procedure UpdateData;
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

procedure TIDEInspectDlg.EdInspectKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    EdInspectEditingDone(nil);
  end;
end;

procedure TIDEInspectDlg.EdInspectEditingDone(Sender: TObject);
begin
  if FExpression = EdInspect.Text then
    exit;
  Execute(EdInspect.Text);
end;

procedure TIDEInspectDlg.btnUseInstanceClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TIDEInspectDlg.ContextChanged(Sender: TObject);
begin
  FExpressionWasEvaluated := False;
  if (not btnPower.Down) or (not Visible) then exit;

  UpdateData;
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
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
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
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  v := FWatchPrinter.PrintWatchValue(Res, wdfPointer);
  FGridData.Cells[1,1]:=FExpression;
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
  FGridData.Columns[2].Visible := btnColType.Down; // typename
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  v := FWatchPrinter.PrintWatchValue(Res, wdfDefault);
  StatusBar1.SimpleText:=ShortenedExpression+' : '+Res.TypeName + ' = ' + v;

  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
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
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  Res := FCurrentResData;
  if Res = nil then begin
    TimerClearData.Enabled := True;
    exit;
  end;

  StatusBar1.SimpleText:=ShortenedExpression+': '+Res.TypeName + '  Len: ' + IntToStr(Res.ArrayLength);

  LowBnd := Res.LowBound;
  if FUpdatedData then begin
    ArrayNavigationBar1.LowBound := LowBnd;
    ArrayNavigationBar1.HighBound := LowBnd + Res.ArrayLength - 1;
    ArrayNavigationBar1.Index := LowBnd;
    FUpdatedData := False;
  end;

  CurIndexOffs := ArrayNavigationBar1.Index - LowBnd;
  CurPageCount := ArrayNavigationBar1.PageSize;
  if (CurIndexOffs >= 0) and (CurIndexOffs < res.ArrayLength) then
    CurPageCount := Max(1, Min(CurPageCount, res.ArrayLength - CurIndexOffs));

  WVal:= FCurrentWatchValue.Watch.ValueList.GetEntriesForRange(
    FCurrentWatchValue.ThreadId,
    FCurrentWatchValue.StackFrame,
    CurIndexOffs,
    CurPageCount
  );
  WVal.Value;
  if WVal.Validity <> ddsValid then begin
    TimerClearData.Enabled := True;
    exit;
  end;
  FCurrentWatchValue.Watch.ValueList.ClearRangeEntries(5);

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

  FGridData.Columns[0].Visible := (Res.StructType in [dstClass, dstObject]) and btnColClass.Down; // anchestor
  FGridData.Columns[2].Visible := btnColType.Down; // typename
  FGridData.Columns[4].Visible := (Res.StructType in [dstClass, dstObject]) and btnColVisibility.Down; // class-visibility
  btnUseInstance.Enabled   := Res.StructType in [dstClass];
  btnColClass.Enabled      := Res.StructType in [dstClass, dstObject];
  btnColType.Enabled       := True;
  btnColVisibility.Enabled := Res.StructType in [dstClass, dstObject];

  AnchType := '';
  if Res.Anchestor <> nil then
    AnchType := Res.Anchestor.TypeName;
  StatusBar1.SimpleText:=Format(lisInspectClassInherit, [ShortenedExpression, Res.TypeName, AnchType]);

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
  if Button = mbExtra1 then btnBackwardClick(nil)
  else
  if Button = mbExtra2 then btnForwardClick(nil)
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
  ReleaseRefAndNil(FCurrentWatchValue);
  FCurrentResData := nil;
  FInspectWatches.Clear;
  UpdateData;
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
  if (FCurrentWatchValue = nil) or (FExpression = '') then exit;

  if FCurrentWatchValue.TypeInfo <> nil then begin

    if (FCurrentWatchValue.TypeInfo.Kind in [skClass, skRecord, skObject]) then begin
      i := FGridData.Row;
      if (i < 1) or (i >= FGridData.RowCount) then exit;
      s := FGridData.Cells[1, i];

      if btnUseInstance.Down and (FCurrentWatchValue.TypeInfo.Kind = skClass) then
        Execute(FGridData.Cells[0, i] + '(' + FExpression + ').' + s)
      else
        Execute(FExpression + '.' + s);
      exit;
    end;

    if (FCurrentWatchValue.TypeInfo.Kind in [skPointer]) then begin
      i := FGridData.Row;
      if (i < 1) or (i >= FGridData.RowCount) then exit;
      s := FGridData.Cells[1, i];
      t := FGridData.Cells[2, i];
      Execute('(' + FExpression + ')^');
      if not FExpressionWasEvaluated then
        FAlternateExpression := t + '(' + FExpression + ')[0]';
      exit;
    end;

    if (FCurrentWatchValue.TypeInfo.Kind in [skSimple]) and (FCurrentWatchValue.TypeInfo.Attributes*[saArray,saDynArray] <> []) then begin
      if FCurrentWatchValue.TypeInfo.Len < 1 then exit;
      if FCurrentWatchValue.TypeInfo.Fields.Count > 0 then begin
        i := FGridData.Row;
        if (i < 1) or (i >= FGridData.RowCount) then exit;
        s := FGridData.Cells[1, i];
        Execute(FExpression + '[' + s + ']');
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
          Execute('(' + FExpression + ')^');
          if not FExpressionWasEvaluated then
            FAlternateExpression := t + '(' + FExpression + ')[0]';
        end;
      rdkArray: begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;
          s := FGridData.Cells[1, i];
          Execute(FExpression + '[' + s + ']');
        end;
      rdkStruct: begin
          i := FGridData.Row;
          if (i < 1) or (i >= FGridData.RowCount) then exit;
          s := FGridData.Cells[1, i];

          if btnUseInstance.Down and (FCurrentResData.StructType in [dstClass, dstObject]) then
            Execute(FGridData.Cells[0, i] + '(' + FExpression + ').' + s)
          else
            Execute(FExpression + '.' + s);
        end;
    end;
  end;

end;

procedure TIDEInspectDlg.btnColClassClick(Sender: TObject);
begin
  if (FCurrentWatchValue = nil) then exit;

  if ( (FCurrentWatchValue.TypeInfo <> nil) and
       (FCurrentWatchValue.TypeInfo.Kind = skClass)
     ) or
     ( FCurrentResData.StructType in [dstClass, dstObject] )
  then begin
    FGridData.Columns[0].Visible := btnColClass.Down;
    FGridData.Columns[4].Visible := btnColVisibility.Down;
  end;

  FGridData.Columns[2].Visible := btnColType.Down;
end;

procedure TIDEInspectDlg.btnForwardClick(Sender: TObject);
begin
  GotoHistory(FHistoryIndex + 1);
end;

procedure TIDEInspectDlg.btnPowerClick(Sender: TObject);
begin
  if btnPower.Down
  then begin
    btnPower.ImageIndex := FPowerImgIdx;
    ReleaseRefAndNil(FCurrentWatchValue);
    FCurrentResData := nil;
    FInspectWatches.Clear;
    UpdateData;
  end
  else begin
    btnPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TIDEInspectDlg.btnUseConverterClick(Sender: TObject);
begin
  UpdateData;
end;

procedure TIDEInspectDlg.btnBackwardClick(Sender: TObject);
begin
  GotoHistory(FHistoryIndex - 1);
end;

procedure TIDEInspectDlg.BtnAddWatchClick(Sender: TObject);
var
  w: TCurrentWatch;
begin
  if DebugBoss = nil then
    exit;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  try
    w := DebugBoss.Watches.CurrentWatches.Find(FExpression);
    if w = nil then
      w := DebugBoss.Watches.CurrentWatches.Add(FExpression);
    if (w <> nil) then begin
      w.Enabled := True;
      if EnvironmentOptions.DebuggerAutoSetInstanceFromClass or
         btnUseInstance.Down
      then
        w.EvaluateFlags := w.EvaluateFlags + [defClassAutoCast];
      if not btnUseConverter.Down then
        w.EvaluateFlags := w.EvaluateFlags + [defSkipValConv];
      DebugBoss.ViewDebugDialog(ddtWatches, False);
    end;
  finally
    DebugBoss.Watches.CurrentWatches.EndUpdate;
  end;

end;

procedure TIDEInspectDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and not Docked then
    Close;
end;

procedure TIDEInspectDlg.Localize;
begin
  Caption := lisInspectDialog;
  DataPage.Caption := lisInspectData;
  PropertiesPage.Caption := lisInspectProperties;
  MethodsPage.Caption := lisInspectMethods;

  btnUseInstance.Caption := lisInspectUseInstance;
  btnUseInstance.Hint    := lisInspectUseInstanceHint;
  btnUseConverter.Caption := dlgFpConvOptFpConverter;
  btnUseConverter.Hint    := dsrEvalUseFpDebugConverter;
  btnColClass.Hint       := lisInspectShowColClass;
  btnColType.Hint        := lisInspectShowColType;
  btnColVisibility.Hint  := lisInspectShowColVisibility;
end;

function TIDEInspectDlg.ShortenedExpression: String;
const
  MAX_SHORT_EXPR_LEN = 25;
begin
  Result := FExpression;
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
  FGridData.Columns[0].Visible := btnColClass.Down;
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := btnColVisibility.Down;
  btnUseInstance.Enabled := True;
  btnColClass.Enabled := True;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := True;


  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo.Fields) then exit;
  StatusBar1.SimpleText:=Format(lisInspectClassInherit, [ShortenedExpression, FCurrentWatchValue.TypeInfo.
    TypeName, FCurrentWatchValue.TypeInfo.Ancestor]);
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
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : Variant';
  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
  FGridData.Cells[2,1]:='Variant';
  FGridData.Cells[3,1]:=FCurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(1);
end;

procedure TIDEInspectDlg.InspectRecord;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo.Fields) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName;
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
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  GridDataSetup;

  if FCurrentWatchValue.TypeInfo.Attributes*[saArray,saDynArray] <> [] then begin
    if FCurrentWatchValue.TypeInfo.Len >= 0 then
      StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = Len:' + IntToStr(FCurrentWatchValue.TypeInfo.Len) + ' ' + FCurrentWatchValue.TypeInfo.Value.AsString
    else
      StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = ' + FCurrentWatchValue.TypeInfo.Value.AsString;

    if FCurrentWatchValue.TypeInfo.Fields.Count > 0 then begin
      FGridData.RowCount:=FCurrentWatchValue.TypeInfo.Fields.Count+1;
      for j := 0 to FCurrentWatchValue.TypeInfo.Fields.Count-1 do begin
        fld := FCurrentWatchValue.TypeInfo.Fields[j];
        FGridData.Cells[1,j+1]:=fld.Name; // index
        FGridData.Cells[2,j+1]:=fld.DBGType.TypeName;
        FGridData.Cells[3,j+1]:=fld.DBGType.Value.AsString;
      end;
      exit;
    end;
  end
  else
    StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = ' + FCurrentWatchValue.TypeInfo.Value.AsString;

  FGridData.Cells[1,1]:=FExpression;
  FGridData.Cells[2,1]:=FCurrentWatchValue.TypeInfo.TypeName;
  FGridData.Cells[3,1]:=FCurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectEnum;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = ' + FCurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
  FGridData.Cells[2,1]:=FCurrentWatchValue.TypeInfo.TypeName;
  if (FCurrentWatchValue.TypeInfo.TypeName <> '') and (FCurrentWatchValue.TypeInfo.TypeDeclaration <> '')
  then FGridData.Cells[2,1] := FGridData.Cells[2,1] + ' = ';
  FGridData.Cells[2,1] := FGridData.Cells[2,1] + FCurrentWatchValue.TypeInfo.TypeDeclaration;
  FGridData.Cells[3,1]:=FCurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectSet;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = ' + FCurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
  FGridData.Cells[2,1]:=FCurrentWatchValue.TypeInfo.TypeName;
  if (FCurrentWatchValue.TypeInfo.TypeName <> '') and (FCurrentWatchValue.TypeInfo.TypeDeclaration <> '')
  then FGridData.Cells[2,1] := FGridData.Cells[2,1] + ' = ';
  FGridData.Cells[2,1] := FGridData.Cells[2,1] + FCurrentWatchValue.TypeInfo.TypeDeclaration;
  FGridData.Cells[3,1]:=FCurrentWatchValue.TypeInfo.Value.AsString;
  //FGridData.AutoSizeColumn(2);
end;

procedure TIDEInspectDlg.InspectPointer;
begin
  DataPage.TabVisible:=true;
  PropertiesPage.TabVisible:=false;
  MethodsPage.TabVisible:=false;
  PageControl.ActivePage := DataPage;
  FGridData.Columns[0].Visible := False;
  FGridData.Columns[2].Visible := btnColType.Down;
  FGridData.Columns[4].Visible := False;
  btnUseInstance.Enabled := False;
  btnColClass.Enabled := False;
  btnColType.Enabled := True;
  btnColVisibility.Enabled := False;

  if not Assigned(FCurrentWatchValue) then exit;
  if not Assigned(FCurrentWatchValue.TypeInfo) then exit;
  StatusBar1.SimpleText:=ShortenedExpression+' : '+FCurrentWatchValue.TypeInfo.TypeName + ' = ' + FCurrentWatchValue.TypeInfo.Value.AsString;
  GridDataSetup;
  FGridData.Cells[1,1]:=FExpression;
  if (FCurrentWatchValue.TypeInfo.TypeName <> '') and (FCurrentWatchValue.TypeInfo.TypeName[1] = '^')
  then FGridData.Cells[2, 1]:=Format(lisInspectPointerTo, [copy(FCurrentWatchValue.TypeInfo.
    TypeName, 2, length(FCurrentWatchValue.TypeInfo.TypeName))])
  else FGridData.Cells[2,1]:=FCurrentWatchValue.TypeInfo.TypeName;
  {$PUSH}{$RANGECHECKS OFF}
  FGridData.Cells[3,1]:=format('$%x',[{%H-}PtrUInt(FCurrentWatchValue.TypeInfo.Value.AsPointer)]);
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
  for j := 0 to FCurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case FCurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skSimple,skRecord,skVariant,skPointer: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridData.RowCount:=k;
  k:=0;
  for j := 0 to FCurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    fld := FCurrentWatchValue.TypeInfo.Fields[j];
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
  for j := 0 to FCurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case FCurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skProcedure,skFunction,skProcedureRef, skFunctionRef: inc(k);
    end;
  end;
  k:=k+1;
  if k<2 Then k:=2;
  FGridMethods.RowCount:=k;
  k:=0;
  for j := 0 to FCurrentWatchValue.TypeInfo.Fields.Count-1 do begin
    case FCurrentWatchValue.TypeInfo.Fields[j].DBGType.Kind of
      skProcedure, skProcedureRef:
        begin
          inc(k);
          FGridMethods.Cells[0,k]:=FCurrentWatchValue.TypeInfo.Fields[j].Name;
          if ffDestructor in FCurrentWatchValue.TypeInfo.Fields[j].Flags then begin
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
          FGridMethods.Cells[0,k]:=FCurrentWatchValue.TypeInfo.Fields[j].Name;
          if ffConstructor in FCurrentWatchValue.TypeInfo.Fields[j].Flags then begin
            FGridMethods.Cells[1,k]:='Constructor';
          end else begin
            FGridMethods.Cells[1,k]:='Function';
          end;
          if Assigned(FCurrentWatchValue.TypeInfo.Fields[j].DBGType.Result) then begin
            FGridMethods.Cells[2,k]:=FCurrentWatchValue.TypeInfo.Fields[j].DBGType.Result.TypeName;
          end else begin
            FGridMethods.Cells[2,k]:='';
          end;
          FGridMethods.Cells[3,k]:='???';
        end;
    end;
  end;
end;

procedure TIDEInspectDlg.Clear;
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
  FInspectWatches := TCurrentWatches.Create(WatchesMonitor);

  ThreadsNotification.OnCurrent := @ContextChanged;
  CallstackNotification.OnCurrent := @ContextChanged;

  DebugBoss.RegisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.RegisterWatchesInvalidatedHandler(@DoWatchesInvalidated);

  ArrayNavigationBar1.OnIndexChanged := @ArrayNavChanged;
  ArrayNavigationBar1.OnPageSize := @ArrayNavChanged;
  ArrayNavigationBar1.Visible := False;

  FHistory := TStringList.Create;

  FGridData:=TStringGrid.Create(DataPage);
  DataPage.InsertControl(FGridData);
  GridDataSetup(True);

  FGridMethods:=TStringGrid.Create(MethodsPage);
  MethodsPage.InsertControl(FGridMethods);
  GridMethodsSetup(True);

  EdInspect.Items.Assign(InputHistories.HistoryLists.
    GetList(ClassName,True,rltCaseSensitive));

  FGridData.OnDblClick := @DataGridDoubleClick;
  FGridData.OnMouseDown := @DataGridMouseDown;
  FGridData.PopupMenu := PopupMenu1;
  FGridMethods.OnMouseDown := @DataGridMouseDown;
  FGridMethods.PopupMenu := PopupMenu1;

  ToolBar1.Images := IDEImages.Images_16;
  btnBackward.ImageIndex := IDEImages.LoadImage('arrow_left');
  btnBackward.Caption := '';
  btnForward.ImageIndex := IDEImages.LoadImage('arrow_right');
  btnForward.Caption := '';
  BtnAddWatch.Caption:=lisInspectAddWatch;

  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  btnPower.ImageIndex := FPowerImgIdx;
  btnPower.Caption := '';
  btnPower.Hint := lisDbgWinPowerHint;

  btnUseInstance.Enabled := False;
  btnUseInstance.Down := EnvironmentOptions.DebuggerAutoSetInstanceFromClass;
  btnColClass.Enabled := False;
  btnColType.Enabled := False;
  btnColVisibility.Enabled := False;
  btnBackward.Enabled := FHistoryIndex > 0;
  btnForward.Enabled := FHistoryIndex < FHistory.Count - 1;

  menuCopyValue.Caption := lisLocalsDlgCopyValue;

  Clear;
end;

destructor TIDEInspectDlg.Destroy;
begin
  DebugBoss.UnregisterStateChangeHandler(@DoDebuggerState);
  DebugBoss.UnregisterWatchesInvalidatedHandler(@DoWatchesInvalidated);
  ReleaseRefAndNil(FCurrentWatchValue);
  FCurrentResData := nil;
  FreeAndNil(FHistory);
  FreeAndNil(FWatchPrinter);
  //FreeAndNil(FDataGridHook);
  //FreeAndNil(FPropertiesGridHook);
  //FreeAndNil(FMethodsGridHook);
  inherited Destroy;

  FreeAndNil(FInspectWatches);
end;

procedure TIDEInspectDlg.InternalExecute(const AExpression: ansistring);
begin
  if FHistoryIndex >= FHistory.Count then
    FHistoryIndex := FHistory.Count - 1;
  inc(FHistoryIndex);
  while FHistory.Count > FHistoryIndex do
    FHistory.Delete(FHistoryIndex);

  FHistoryIndex := FHistory.Add(AExpression);

  while FHistory.Count > MAX_HISTORY do
    FHistory.Delete(0);

  GotoHistory(FHistoryIndex);
end;

procedure TIDEInspectDlg.Execute(const AExpression: ansistring);
begin
  InternalExecute(AExpression);
end;

procedure TIDEInspectDlg.FormActivate(Sender: TObject);
begin
  EdInspect.DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TIDEInspectDlg.GotoHistory(AIndex: Integer);
begin
  FHistoryIndex := AIndex;
  if FHistory.Count = 0 then exit;
  if FHistoryIndex >= FHistory.Count then
    FHistoryIndex := FHistory.Count - 1;
  if FHistoryIndex < 0 then
    FHistoryIndex := 0;

  btnBackward.Enabled := FHistoryIndex > 0;
  btnForward.Enabled := FHistoryIndex < FHistory.Count - 1;

  if (FExpression=FHistory[FHistoryIndex]) and FExpressionWasEvaluated then
    exit;

  FExpression:=FHistory[FHistoryIndex];
  ArrayNavigationBar1.Index := 0;
  EdInspect.Text := FExpression;
  UpdateData;
end;


procedure TIDEInspectDlg.DoWatchUpdated(const ASender: TIdeWatches;
  const AWatch: TIdeWatch);
begin
  if (FCurrentWatchValue = nil) or
     not (FCurrentWatchValue.Validity in [ddsError, ddsInvalid, ddsValid])
  then
    exit;
  if (AWatch <> FCurrentWatchValue.Watch) or
     (ASender <> FInspectWatches)
  then
    exit;

  if (FCurrentWatchValue.Validity in [ddsError, ddsInvalid]) and
     (FAlternateExpression <> '')
  then begin
    if (FHistoryIndex = FHistory.Count - 1) and
       (FHistory[FHistoryIndex] = FExpression)
    then begin
      FHistory.Delete(FHistoryIndex);
      dec(FHistoryIndex);
    end;
    Execute(FAlternateExpression);
    FAlternateExpression := '';
    exit;
  end;

  TimerClearData.Enabled := False;

  FAlternateExpression := '';
  FExpressionWasEvaluated := True;
  FCurrentResData := FCurrentWatchValue.ResultData;
  FHumanReadable := FWatchPrinter.PrintWatchValue(FCurrentResData, wdfStructure);

  if FCurrentWatchValue.Validity = ddsValid then begin
    if FCurrentWatchValue.TypeInfo <> nil then begin
      ArrayNavigationBar1.Visible := False;
      case FCurrentWatchValue.TypeInfo.Kind of
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

      if (FCurrentResData.ValueKind = rdkStruct) and
         (FCurrentResData.StructType = dstInternal)
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


      ArrayNavigationBar1.Visible := FCurrentResData.ValueKind = rdkArray;
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
  if (not btnPower.Down) or (not Visible) then exit;
  if (ADebugger.State = dsPause) and (AnOldState <> dsPause) then begin
    ReleaseRefAndNil(FCurrentWatchValue);
    FCurrentResData := nil;
    FInspectWatches.Clear;
    UpdateData;
  end;
end;

procedure TIDEInspectDlg.ArrayNavChanged(Sender: TArrayNavigationBar;
  AValue: Int64);
begin
  if (FCurrentResData = nil) or (FCurrentResData.ValueKind <> rdkArray) then
    exit;
  InspectResDataArray;
end;

procedure TIDEInspectDlg.DoWatchesInvalidated(Sender: TObject);
begin
  if (not btnPower.Down) or (not Visible) then exit;
  ReleaseRefAndNil(FCurrentWatchValue);
  FCurrentResData := nil;
  FInspectWatches.Clear;
  UpdateData;
end;

procedure TIDEInspectDlg.UpdateData;
var
  Opts: TWatcheEvaluateFlags;
  AWatch: TCurrentWatch;
  tid, idx: Integer;
  stack: TIdeCallStack;
  expr: String;
begin
  FExpressionWasEvaluated := False;
  FAlternateExpression := '';
  FUpdatedData := True;

  expr := trim(FExpression);
  if expr = '' then begin
    ReleaseRefAndNil(FCurrentWatchValue);
    FCurrentResData := nil;
    Clear;
    StatusBar1.SimpleText := '';
    exit;
  end;

  InputHistories.HistoryLists.Add(ClassName, FExpression,rltCaseSensitive);
  if EdInspect.Items.IndexOf(FExpression) = -1
  then EdInspect.Items.Insert(0, FExpression);


  if (CallStackMonitor = nil) or (ThreadsMonitor = nil) or
     (DebugBoss.State <> dsPause)
  then
    exit;

  tid    := ThreadsMonitor.CurrentThreads.CurrentThreadId;
  stack  := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];
  idx := 0;
  if stack <> nil then
    idx := stack.CurrentIndex;

  Opts := [defExtraDepth, defFullTypeInfo];
  if btnUseInstance.Down then
    include(Opts, defClassAutoCast);
  if not btnUseConverter.Down then
    include(Opts, defSkipValConv);

  if (FCurrentWatchValue <> nil) and
     (FCurrentWatchValue.Expression = expr) and
     (FCurrentWatchValue.EvaluateFlags = Opts) and
     (FCurrentWatchValue.ThreadId = tid) and
     (FCurrentWatchValue.StackFrame = idx)
  then begin
    FCurrentWatchValue.Value;
    DoWatchUpdated(nil, FCurrentWatchValue.Watch);
    exit;
  end;

  ReleaseRefAndNil(FCurrentWatchValue);
  FCurrentResData := nil;

  FInspectWatches.BeginUpdate;
  AWatch := FInspectWatches.Find(expr);
  if AWatch = nil then begin
    FInspectWatches.Clear;
    AWatch := FInspectWatches.Add(expr);
    ArrayNavigationBar1.Index := 0;
  end;
  AWatch.EvaluateFlags := Opts;
  AWatch.Enabled := True;
  AWatch.RepeatCount := ArrayNavigationBar1.PageSize;
  FInspectWatches.EndUpdate;
  FCurrentWatchValue := AWatch.Values[tid, idx];
  if FCurrentWatchValue <> nil then begin
    FCurrentResData := FCurrentWatchValue.ResultData;
    FCurrentWatchValue.AddReference;
    FCurrentWatchValue.Value;
  end;
  DoWatchUpdated(FInspectWatches, AWatch);
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

