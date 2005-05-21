{ $Id$}
{
 /***************************************************************************
                               DBGrids.pas
                               -----------
                     An interface to DB aware Controls
                     Initial Revision : Sun Sep 14 2003


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
{
TDBGrid and TComponentDataLink for Lazarus
Copyright (C) 2003  Jesus Reyes Aguilar.
email: jesusrmx@yahoo.com.mx

todo: credit who created the TComponentDatalink idea (Johana ...)

}
unit DBGrids;

{$mode objfpc}{$H+}
{$define EnableIsSeq}
interface

uses
  Classes, LCLIntf, LCLProc, Graphics, SysUtils, LCLType, stdctrls, DB,
  LMessages, Grids, Dialogs, Controls;

type
  TCustomDbGrid = class;
  TColumn = class;
  TDataSetScrolledEvent = procedure(DataSet: TDataSet; Distance: Integer) of object;
  TDBGridClickEvent = procedure(Column: TColumn) of object;
  TMovedEvent = procedure(Sender: TObject; FromIndex, ToIndex: Integer) of object;
  TDrawColumnCellEvent = procedure(Sender: TObject; const Rect: TRect;
    DataCol: Integer; Column: TColumn; State: TGridDrawState) of object;
  TGetDbEditMaskEvent =
    procedure (Sender: TObject; const Field: TField; var Value: string) of object;

  TDBGridOption = (
    dgEditing,                          // Ya
    dgTitles,                           // Ya
    dgIndicator,                        // Ya
    dgColumnResize,                     // Ya
    dgColLines,                         // Ya
    dgRowLines,                         // Ya
    dgTabs,                             // Ya
    dgAlwaysShowEditor,                 // Ya
    dgRowSelect,                        // Ya
    dgAlwaysShowSelection,              // Ya
    dgConfirmDelete,
    dgCancelOnExit,                     // Ya
    dgMultiselect
  );
  TDbGridOptions = set of TDbGridOption;
  TDbGridStatusItem = (gsVisibleMove, gsUpdatingData);
  TDbGridStatus = set of TDbGridStatusItem;

type

  { TComponentDataLink }

  TComponentDataLink=class(TDatalink)
  private
    FDataSet: TDataSet;
    FDataSetName: string;
    FModified: Boolean;
    FOnDatasetChanged: TDatasetNotifyEvent;
    fOnDataSetClose: TDataSetNotifyEvent;
    fOnDataSetOpen: TDataSetNotifyEvent;
    FOnDataSetScrolled: TDataSetScrolledEvent;
    FOnEditingChanged: TDataSetNotifyEvent;
    fOnInvalidDataSet: TDataSetNotifyEvent;
    fOnInvalidDataSource: TDataSetNotifyEvent;
    FOnLayoutChanged: TDataSetNotifyEvent;
    fOnNewDataSet: TDataSetNotifyEvent;
    FOnRecordChanged: TFieldNotifyEvent;
    FOnUpdateData: TDataSetNotifyEvent;
    
    function GetDataSetName: string;
    function GetFields(Index: Integer): TField;
    procedure SetDataSetName(const AValue: string);
  protected
    procedure RecordChanged(Field: TField); override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure LayoutChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FocusControl(Field: TFieldRef); override;
    // Testing Events
    procedure CheckBrowseMode; override;
    procedure EditingChanged; override;
    procedure UpdateData; override;
    function  MoveBy(Distance: Integer): Integer; override;
    property  Modified: Boolean read FModified write FModified;
  public
    Property OnRecordChanged: TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    Property OnDataSetChanged: TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
    property OnNewDataSet: TDataSetNotifyEvent read fOnNewDataSet write fOnNewDataSet;
    property OnDataSetOpen: TDataSetNotifyEvent read fOnDataSetOpen write fOnDataSetOpen;
    property OnInvalidDataSet: TDataSetNotifyEvent read fOnInvalidDataSet write fOnInvalidDataSet;
    property OnInvalidDataSource: TDataSetNotifyEvent read fOnInvalidDataSource write fOnInvalidDataSource;
    property OnLayoutChanged: TDataSetNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnDataSetClose: TDataSetNotifyEvent read fOnDataSetClose write fOnDataSetClose;
    property OnDataSetScrolled: TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    property OnEditingChanged: TDataSetNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnUpdateData: TDataSetNotifyEvent read FOnUpdateData write FOnUpdateData;
    property DataSetName:string read GetDataSetName write SetDataSetName;
    Property Fields[Index: Integer]: TField read GetFields;
    Property VisualControl;
  end;

  { TColumn }

  TColumnTitle = class(TGridColumnTitle)
  protected
    function  GetDefaultCaption: string; override;
  end;

  { TColumn }

  TColumn = class(TGridColumn)
  private
    FDisplayFormat: String;
    FDisplayFormatChanged: boolean;
    FFieldName: String;
    FField: TField;
    procedure ApplyDisplayFormat;
    function  GetDisplayFormat: string;
    function  GetField: TField;
    function  IsDisplayFormatStored: boolean;
    procedure SetDisplayFormat(const AValue: string);
    procedure SetField(const AValue: TField);
    procedure SetFieldName(const AValue: String);
    function  GetDataSet: TDataSet;
  protected
    procedure LinkField;
    function  GetDefaultDisplayFormat: string;
    function  GetDisplayName: string; override;
    // FPC 1.0 has TAlignment in the DB unit too
    function  GetDefaultAlignment: {$IFDEF VER1_0}Classes.{$ENDIF}TAlignment; override;
    function  InternalDefaultReadOnly: boolean; override;
    function  GetDefaultVisible: boolean; override;
    function  InternalDefaultWidth: Integer; override;
    function  CreateTitle: TGridColumnTitle; override;
  public
    function  IsDefault: boolean; override;
    property Field: TField read GetField write SetField;
  published
    property FieldName: String read FFieldName write SetFieldName;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
      stored IsDisplayFormatStored;
  end;

  TDbGridColumns = class(TGridColumns)
  private
    function GetColumn(Index: Integer): TColumn;
    procedure SetColumn(Index: Integer; Value: TColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
    function ColumnFromField(Field: TField): TColumn;
  public
    constructor Create(TheGrid: TCustomDBGrid);
    function  Add: TColumn;
    procedure LinkFields;
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
  end;

  { TCustomDbGrid }

  TCustomDbGrid=class(TCustomGrid)
  private
    FDataLink: TComponentDataLink;
    FOnCellClick: TDBGridClickEvent;
    FOnColEnter,FOnColExit: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnFieldEditMask: TGetDbEditMaskEvent;
    FOnTitleClick: TDBGridClickEvent;
    FOptions: TDbGridOptions;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    FLayoutChangedCount: integer;
    FVisualChangeCount: Integer;
    FSelectionLock: Boolean;
    FTempText : string;
    FDrawingActiveRecord: Boolean;
    FEditingColumn: Integer;
    FOldPosition: Integer;
    FDefaultColWidths: boolean;
    FGridStatus: TDbGridStatus;
    FOldControlStyle: TControlStyle;
    procedure EmptyGrid;
    function GetCurrentField: TField;
    function GetDataSource: TDataSource;
    function GetRecordCount: Integer;
    function GetThumbTracking: boolean;
    procedure OnRecordChanged(Field:TField);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnEditingChanged(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnLayoutChanged(aDataSet: TDataSet);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure OnUpdateData(aDataSet: TDataSet);
    //procedure ReadColumns(Reader: TReader);
    //procedure SetColumns(const AValue: TDBGridColumns);
    procedure SetCurrentField(const AValue: TField);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetOptions(const AValue: TDbGridOptions);
    procedure SetThumbTracking(const AValue: boolean);
    procedure UpdateBufferCount;
    procedure UpdateData;
    
    // Temporal
    function GetColumnCount: Integer;

    function DefaultFieldColWidth(F: TField): Integer;
    function GetDsFieldFromGridColumn(Column: Integer): TField;
    function GetFieldFromGridColumn(Column: Integer): TField;
    function GetGridColumnFromField(F: TField): Integer;
    function FieldIndexFromGridColumn(Column: Integer): Integer;

    procedure UpdateGridColumnSizes;
    procedure UpdateScrollbarRange;
    procedure BeginVisualChange;
    procedure EndVisualChange;
    procedure DoLayoutChanged;
    //procedure WriteColumns(Writer: TWriter);
    
    procedure RestoreEditor;
    function  ISEOF: boolean;
    function  ValidDataSet: boolean;
    function  InsertCancelable: boolean;
    procedure StartUpdating;
    procedure EndUpdating;
    function  UpdatingData: boolean;
  protected
  {$ifdef ver1_0}
    property FixedColor;
  {$endif}
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure BeginLayout;
    procedure CellClick(const aCol,aRow: Integer); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    function  CreateColumns: TGridColumns; override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefaultDrawCell(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DoExit; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoOnChangeBounds; override;
    procedure DrawByRows; override;
    procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect); override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure EditingColumn(aCol: Integer; Ok: boolean);
    procedure EditorCancelEditing;
    procedure EditorDoGetValue; override;
    function  EditorCanAcceptKey(const ch: Char): boolean; override;
    function  EditorIsReadOnly: boolean; override;
    procedure EndLayout;
    // FPC 1.0 has TAlignment in the DB unit too
    function  GetDefaultAlignment(Column: Integer): {$IFDEF VER1_0}Classes.{$ENDIF}TAlignment; override;
    function  GetDefaultColumnWidth(Column: Integer): Integer; override;
    function  GetDefaultReadOnly(Column: Integer): boolean; override;
    function  GetDefaultTitle(Column: Integer): string; override;
    
    function  GetEditMask(aCol, aRow: Longint): string; override;
    function  GetEditText(aCol, aRow: Longint): string; override;
    function  GridCanModify: boolean;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure MoveSelection; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure SelectEditor; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function  ScrollBarAutomatic(Which: TScrollStyle): boolean; override;
    function  SelectCell(aCol, aRow: Integer): boolean; override;
    procedure UpdateActive;
    function  UpdateGridCounts: Integer;
    procedure UpdateVertScrollbar(const aVisible: boolean; const aRange,aPage: Integer); override;
    procedure VisualChange; override;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure WndProc(var TheMessage : TLMessage); override;
    
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Options: TDbGridOptions read FOptions write SetOptions;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    
    property OnCellClick: TDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
    property OnFieldEditMask: TGetDbEditMaskEvent read FOnFieldEditMask write FOnFieldEditMask;
    property OnTitleClick: TDBGridClickEvent read FOnTitleClick write FOnTitleClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitiateAction; override;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure ResetColWidths;
    destructor Destroy; override;
    property SelectedField: TField read GetCurrentField write SetCurrentField;
    property ThumbTracking: boolean read GetThumbTracking write SetThumbTracking;
  end;
  
  
  TdbGrid=class(TCustomDbGrid)
  public
    property Canvas;
    property DefaultTextStyle;
    property EditorBorderStyle;
    property ExtendedColSizing;
    property FastEditing;
    property FocusColor;
    property FocusRectVisible;
    property GridLineColor;
    property GridLineStyle;
    property SelectedColor;
    //property SelectedRows;
  published
    property Align;
    property Anchors;
    property AutoAdvance;
    property AutoFillColumns;
    //property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Columns; // stored false;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight default 18;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property FixedColor;
    property Flat;
    property Font;
    //property ImeMode;
    //property ImeName;
    property Options;
    //property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Scrollbars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleStyle;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnDrawColumnCell;
    property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    property OnEditButtonClick;
    //property OnEndDock;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPrepareCanvas;
    //property OnStartDock;
    //property OnStartDrag;
    property OnTitleClick;
  end;
  
procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect; Opt: TDataSetState);
var
  dx,dy, x, y: Integer;
  procedure DrawEdit(clr: Tcolor);
  begin
    Canvas.Pen.Color := clr;
    y := R.Top + (R.Bottom-R.Top) div 2 - 1;
    X := R.Left + (R.Right-R.Left) div 2 - 1;
    Canvas.MoveTo(X-2, Y-Dy-1);
    Canvas.LineTo(X+3, Y-Dy-1);
    Canvas.MoveTo(X, Y-Dy);
    Canvas.LineTo(X, Y+Dy);
    Canvas.MoveTo(X-2, Y+Dy);
    Canvas.LineTo(X+3, Y+Dy);
  end;
begin
  dx := 6;
  dy := 6;
  case Opt of
    dsBrowse:
      begin //
        Canvas.Brush.Color:=clBlack;
        Canvas.Pen.Color:=clBlack;
        y:= R.top+ (R.Bottom-R.Top) div 2-1;
        x:= R.Left+2;
        Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsEdit:
      DrawEdit(clBlack);
    dsInsert:
      DrawEdit(clGreen);
   end;
end;

function CalcCanvasCharWidth(Canvas:TCanvas): integer;
begin
  //result := Canvas.TextWidth('W l') div 3;
  if Canvas.HandleAllocated then
    result := Canvas.TextWidth('MX') div 2
  else
    Result := 8;
end;

{ TCustomdbGrid }

procedure TCustomDbGrid.OnRecordChanged(Field: TField);
var
  c: Integer;
begin
  {$IfDef dbgdbgrid}
  DBGOut('('+name+') ','TCustomDBGrid.OnRecordChanged(Field=');
  if Field=nil then DebugLn('nil)')
  else              DebugLn(Field.FieldName,')');
  {$Endif}
  if Field=nil then
    UpdateActive
  else begin
    c := GetGridColumnFromField(Field);
    if c>0 then
      InvalidateCell(C, Row)
    else
      UpdateActive;
  end;
end;

function TCustomDbGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

function TCustomDbGrid.GetRecordCount: Integer;
begin
  result := FDataLink.DataSet.RecordCount;
end;

function TCustomDbGrid.GetThumbTracking: boolean;
begin
  Result :=  goThumbTracking in inherited Options;
end;

procedure TCustomDbGrid.EmptyGrid;
begin
  ColCount := 2;
  RowCount := 2;
  FixedCols := 1;
  FixedRows := 1;
  ColWidths[0]:=12;
end;

function TCustomDbGrid.GetCurrentField: TField;
begin
  result := GetFieldFromGridColumn( Col );
end;

procedure TCustomDbGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DBGOut('('+name+') ','TCustomDBDrid.OnDataSetChanged(aDataSet=');
  if aDataSet=nil then DebugLn('nil)')
  else DebugLn(aDataSet.Name,')');
  {$endif}
  if not (gsVisibleMove in FGridStatus) then
    LayoutChanged
  else
    UpdateScrollBarRange;

  UpdateActive;
  //RestoreEditor;
end;

procedure TCustomDbGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetOpen');
  {$endif}
  FDefaultColWidths := True;
  LinkActive(True);
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetClose');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnEditingChanged');
  if aDataSet<>nil then begin
    DebugLn('Editing=', BoolToStr(dsEdit = aDataSet.State));
    DebugLn('Inserting=',BoolToStr(dsInsert = aDataSet.State));
  end else
    DebugLn('Dataset=nil');
  {$endif}
  FDataLink.Modified := False;
  UpdateActive;
end;

procedure TCustomDbGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSet');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSource');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDbGrid.OnLayoutChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnLayoutChanged');
  {$endif}
  LayoutChanged;
end;

procedure TCustomDbGrid.OnNewDataSet(aDataSet: TDataset);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnNewDataSet');
  {$endif}
  FDefaultColWidths := True;
  LinkActive(True);
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetScrolled(aDataset: TDataSet; Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName, ' (',name,')', '.OnDataSetScrolled(',IntToStr(Distance),')');
  Debugln('Dataset.RecordCount=',IntToStr(aDataSet.RecordCount));
  {$endif}
  UpdateScrollBarRange;
  // todo: Use a fast interface method to scroll a rectangular section of window
  //       if distance=+, Row[Distance] to Row[RowCount-2] UP
  //       if distance=-, Row[FixedRows+1] to Row[RowCount+Distance] DOWN
  if Distance<>0 then
    Invalidate
  else
  UpdateActive;
end;

procedure TCustomDbGrid.OnUpdateData(aDataSet: TDataSet);
begin
  UpdateData;
end;
{
procedure TCustomDbGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;
procedure TCustomDbGrid.SetColumns(const AValue: TDBGridColumns);
begin
  Columns.Assign(AValue);
end;
}
procedure TCustomDbGrid.SetCurrentField(const AValue: TField);
var
  i: Integer;
begin
  if Avalue<>SelectedField then begin
    i := GetGridColumnFromField( AValue );
    if i>FixedCols then
      Col := i;
  end;
end;

procedure TCustomDbGrid.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
  FDefaultColWidths := True;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDbGrid.SetOptions(const AValue: TDbGridOptions);
var
  OldOptions: TGridOptions;
begin
  if FOptions<>AValue then begin
    FOptions:=AValue;
    OldOptions := inherited Options;
    
   if dgRowSelect in FOptions then
    FOptions := FOptions - [dgEditing, dgAlwaysShowEditor];
    
    BeginLayout;
    
    if dgRowLines in fOptions then
      Include(OldOptions, goHorzLine)
    else
      Exclude(OldOptions, goHorzLine);
      
    if dgColLines in fOptions then
      Include(OldOptions, goVertLine)
    else
      Exclude(OldOptions, goVertLine);
    
    if dgColumnResize in fOptions then
      Include(OldOptions, goColSizing)
    else
      Exclude(OldOptions, goColSizing);

    if dgAlwaysShowEditor in FOptions then
      Include(OldOptions, goAlwaysShowEditor)
    else
      Exclude(OldOptions, goAlwaysShowEditor);

    if dgRowSelect in FOptions then
      Include(OldOptions, goRowSelect)
    else
      Exclude(OldOptions, goRowSelect);

    if dgEditing in FOptions then
      Include(OldOptions, goEditing)
    else
      Exclude(OldOptions, goediting);

    if dgTabs in FOptions then
      Include(OldOptions, goTabs)
    else
      Exclude(OldOptions, goTabs);
      
    inherited Options := OldOptions;
    
    EndLayout;
  end;
end;

procedure TCustomDbGrid.SetThumbTracking(const AValue: boolean);
begin
  BeginUpdate;
  if Avalue then
    inherited Options := Inherited Options + [goThumbTracking]
  else
    inherited Options := Inherited Options - [goThumbTracking];
  EndUpdate(uoNone);
end;

procedure TCustomDbGrid.UpdateBufferCount;
var
  BuffCount: Integer;
begin
  if FDataLink.Active then begin
    BuffCount := GCache.ClientHeight div DefaultRowHeight;
    if dgTitles in Options then Dec(BuffCount, 1);
    FDataLink.BufferCount:= BuffCount;
    {$ifdef dbgdbgrid}
    DebugLn(ClassName, ' (',name,')', ' FdataLink.BufferCount=' + IntToStr(Fdatalink.BufferCount));
    {$endif}
  end;
end;

procedure TCustomDbGrid.UpdateData;
var
  selField,edField: TField;
begin
  // get Editor text and update field content
  if not UpdatingData and (FEditingColumn>-1) and FDatalink.Editing then begin
    SelField := SelectedField;
    edField := GetFieldFromGridColumn(FEditingColumn);
    if (edField<>nil) and (edField = SelField) then begin
      {$ifdef dbgdbgrid}
      DebugLn('---> UpdateData: Field[', edField.Fieldname, ']=', FTempText,' INIT');
      {$endif}
      StartUpdating;
      edField.AsString := FTempText;
      EndUpdating;
      EditingColumn(FEditingColumn, False);
      {$ifdef dbgdbgrid}
      DebugLn('<--- UpdateData: Chk: Field:=',edField.ASString,' END');
      {$endif}
    end;
  end;
end;

{$ifdef dbgdbgrid}
function SBCodeToStr(Code: Integer): String;
begin
  Case Code of
    SB_LINEUP : result := 'SB_LINEUP';
    SB_LINEDOWN: result := 'SB_LINEDOWN';
    SB_PAGEUP: result := 'SB_PAGEUP';
    SB_PAGEDOWN: result := 'SB_PAGEDOWN';
    SB_THUMBTRACK: result := 'SB_THUMBTRACK';
    SB_THUMBPOSITION: result := 'SB_THUMBPOSITION';
    SB_ENDSCROLL: result := 'SB_SCROLLEND';
    SB_TOP: result := 'SB_TOP';
    SB_BOTTOM: result := 'SB_BOTTOM';
    else result :=IntToStr(Code)+ ' -> ?';
  end;
end;
{$endif}

procedure TCustomDbGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos: Integer;
  DeltaRec: integer;
  
  function MaxPos: Integer;
  begin
    if IsSeq then
      result := GetRecordCount
    else
      result := 4;
  end;

  procedure CalcPos(Delta: Integer);
  begin
    if FDataLink.Dataset.BOF then
      aPos := 0
    else if FDatalink.DataSet.EOF then
      aPos := MaxPos
    else if IsSeq then
      aPos := FOldPosition + Delta
    else
      aPos := 2;
  end;
  
  procedure DsMoveBy(Delta: Integer);
  begin
    FDataLink.MoveBy(Delta);
    CalcPos(Delta);
  end;
  
  procedure DsGoto(BOF: boolean);
  begin
    if BOF then FDatalink.DataSet.First
    else        FDataLink.DataSet.Last;
    CalcPos(0);
  end;

begin
  if not FDatalink.Active then exit;
  
  {$ifdef dbgdbgrid}
  DebugLn('VSCROLL: Code=',SbCodeToStr(Message.ScrollCode),
          ' Position=', dbgs(Message.Pos),' OldPos=',Dbgs(FOldPosition));
  {$endif}

  IsSeq := FDatalink.DataSet.IsSequenced {$ifndef EnableIsSeq} and false {$endif};
  case Message.ScrollCode of
    SB_TOP:
      DsGoto(True);
    SB_BOTTOM:
      DsGoto(False);
    SB_PAGEUP:
      DsMoveBy(-VisibleRowCount);
    SB_LINEUP:
      DsMoveBy(-1);
    SB_LINEDOWN:
      DsMoveBy(1);
    SB_PAGEDOWN:
      DsMoveBy(VisibleRowCount);
    SB_THUMBPOSITION:
      begin
        aPos := Message.Pos;
        if aPos>=MaxPos then
          dsGoto(False)
        else if aPos=0 then
          dsGoto(True)
        else if IsSeq then
          FDatalink.DataSet.RecNo := aPos
        else begin
          DeltaRec := Message.Pos - FOldPosition;
          if DeltaRec=0 then
            exit
          else if DeltaRec<-1 then
            DsMoveBy(-VisibleRowCount)
          else if DeltaRec>1 then
            DsMoveBy(VisibleRowCount)
          else
            DsMoveBy(DeltaRec);
        end;
      end;
    else
      Exit;
  end;

  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;
  {$ifdef dbgdbgrid}
  DebugLn('---- Diff=',dbgs(DeltaRec), ' FinalPos=',dbgs(aPos));
  {$endif}
end;

procedure TCustomDbGrid.WndProc(var TheMessage: TLMessage);
begin
  if (TheMessage.Msg=LM_SETFOCUS) and (gsUpdatingData in FGridStatus) then begin
    {$ifdef dbgGrid}DebugLn('dbgrid.LM_SETFOCUS while updating');{$endif}
    if EditorMode then begin
      LCLIntf.SetFocus(Editor.Handle);
      EditorSelectAll;
    end;
    exit;
  end;
  inherited WndProc(TheMessage);
end;


function TCustomDbGrid.DefaultFieldColWidth(F: TField): Integer;
begin
  if not HandleAllocated or (F=nil) then
    result:=DefaultColWidth
  else begin
    if F.DisplayWidth = 0 then
      if Canvas.HandleAllocated then
        result := Canvas.TextWidth( F.DisplayName ) + 4
      else
        Result := DefaultColWidth
    else
      result := F.DisplayWidth * CalcCanvasCharWidth(Canvas);
  end;
end;

function TCustomDbGrid.GetColumnCount: Integer;
var
  i: integer;
  F: TField;
begin
  result := 0;
  if Columns.Enabled then
    result := Columns.VisibleCount
  else
    if FDataLink.Active then
      for i:=0 to FDataLink.DataSet.FieldCount-1 do begin
        F:= FDataLink.DataSet.Fields[i];
        if (F<>nil) and F.Visible then
          Inc(Result);
      end;
end;

// Get the visible field (from dataset fields) that corresponds to given column
function TCustomDbGrid.GetDsFieldFromGridColumn(Column: Integer): TField;
var
  i: Integer;
begin
  i := FieldIndexFromGridColumn( Column );
  if i>=0 then
    Result := FDataLink.DataSet.Fields[i]
  else
    Result := nil;
end;

// obtain the field either from a Db column or directly from dataset fields
function TCustomDbGrid.GetFieldFromGridColumn(Column: Integer): TField;
var
  i: integer;
begin
  if Columns.Enabled then begin
    i := ColumnIndexFromGridColumn( Column );
    if i>=0 then
      result := TDbGridColumns(Columns)[i].FField
    else
      result := nil;
  end else
    result := GetDsFieldFromGridColumn(Column);
end;

// obtain the corresponding grid column for the given field
function TCustomDbGrid.GetGridColumnFromField(F: TField): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=FixedCols to ColCount-1 do begin
    if GetFieldFromGridColumn(i) = F then begin
      result := i;
      break;
    end;
  end;
end;

// obtain the visible field index corresponding to the grid column index
function TCustomDbGrid.FieldIndexFromGridColumn(Column: Integer): Integer;
var
  i: Integer;
begin
  column := column - FixedCols;
  i := 0;
  result := -1;
  if FDataLink.Active then
  while (i<FDataLink.DataSet.FieldCount)and(Column>=0) do begin
    if FDataLink.Fields[i].Visible then begin
        Dec(Column);
        if Column<0 then begin
          result := i;
          break;
        end;
    end;
    inc(i);
  end;
end;

procedure TCustomDbGrid.UpdateGridColumnSizes;
var
  i: Integer;
begin
  if FDefaultColWidths then begin
    if dgIndicator in Options then
      ColWidths[0]:=12;
    for i:=FixedCols to ColCount-1 do
      ColWidths[i] := GetColumnWidth(i);
  end;
end;

procedure TCustomDbGrid.UpdateScrollbarRange;
var
  aRange, aPage: Integer;
  aPos: Integer;
  isSeq: boolean;
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then exit;
  if FDatalink.Active then begin
    IsSeq := FDatalink.dataset.IsSequenced{$ifndef EnableIsSeq}and false{$endif};
    if IsSeq then begin
      aRange := GetRecordCount + VisibleRowCount - 1;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      if FDatalink.BOF then aPos := 0 else
      if FDatalink.EOF then aPos := aRange
      else
        aPos := FDataLink.DataSet.RecNo - 1; // RecNo is 1 based
      if aPos<0 then aPos:=0;
    end else begin
      aRange := 6;
      aPage := 2;
      if FDatalink.EOF then aPos := 4 else
      if FDatalink.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  {$ifdef WIN32}
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.ntrackPos := 0;
  {$else}
  ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
  //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
  ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
  {$endif}
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := aRange;
  ScrollInfo.nPos := aPos;
  ScrollInfo.nPage := aPage;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
  FOldPosition := aPos;
  {$ifdef dbgdbgrid}
  DebugLn('UpdateScrollBarRange: Handle=',IntToStr(Handle),
   ' aRange=', IntToStr(aRange),
   ' aPage=', IntToStr(aPage), ' aPos=', IntToStr(aPos));
  {$endif}
end;
procedure TCustomDbGrid.BeginVisualChange;
begin
  inc(FVisualChangeCount);
end;

procedure TCustomDbGrid.EndVisualChange;
begin
  dec(FVisualChangecount);
  if FVisualChangeCount = 0 then
    VisualChange;
end;

procedure TCustomDbGrid.doLayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  {$ifdef dbgdbgrid} DebugLn('doLayoutChanged INIT'); {$endif}
  if UpdateGridCounts=0 then
    EmptyGrid;
  UpdateScrollBarRange;
  {$ifdef dbgdbgrid} DebugLn('doLayoutChanged FIN'); {$endif}
end;
{
procedure TCustomDbGrid.WriteColumns(Writer: TWriter);
begin
  if Columns.IsDefault then
    Writer.WriteCollection(nil)
  else
    Writer.WriteCollection(Columns);
end;
}
procedure TCustomDbGrid.RestoreEditor;
begin
  if EditorMode then begin
    EditorMode := False;
    EditorMode := True;
  end;
end;

function TCustomDbGrid.IsEOF: boolean;
begin
  with FDatalink do
    result :=
      Active and DataSet.EOF;
end;

function TCustomDbGrid.ValidDataSet: boolean;
begin
  result := FDatalink.Active And (FDatalink.DataSet<>nil)
end;

function TCustomDbGrid.InsertCancelable: boolean;
begin
  with FDatalink.DataSet do
  Result := (State=dsInsert) and not (Modified or FDataLink.FModified);
end;

procedure TCustomDbGrid.StartUpdating;
begin
  if not UpdatingData then begin
    {$ifdef dbgdbgrid} DebugLn('dbgrid.StartUpdating');{$endif}
    Include(FGridStatus, gsUpdatingData);
    FOldControlStyle := ControlStyle;
    ControlStyle := ControlStyle + [csActionClient];
    LockEditor;
  end
  else
    {$ifdef dbgdbgrid} DebugLn('WARNING: multiple call to StartUpdating');{$endif}
end;

procedure TCustomDbGrid.EndUpdating;
begin
  {$ifdef dbgdbgrid} DebugLn('dbGrid.EndUpdating');{$endif}
  Exclude(FGridStatus, gsUpdatingData);
  ControlStyle := FOldControlStyle;
  UnLockEditor;
  if csActionClient in ControlStyle then
    DebugLn('WARNING: still got csActionClient');
end;

function TCustomDbGrid.UpdatingData: boolean;
begin
  result := gsUpdatingData in FGridStatus;
end;

procedure TCustomDbGrid.LinkActive(Value: Boolean);
begin
  LayoutChanged;
end;

procedure TCustomDBGrid.LayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  if FLayoutChangedCount=0 then begin
    BeginLayout;
    if Columns.Count>0 then
      TDbGridColumns(Columns).LinkFields
    else if not FDataLink.Active then
      FDataLink.BufferCount := 0;
    EndLayout;
  end;
end;

procedure TCustomDbGrid.Loaded;
begin
  LayoutChanged;
  inherited Loaded;
end;

type
  TProtFields=class(TFields)
  {$ifdef ver1_0}
   // workaround to access protected procedure in base class
   Procedure SetFieldIndex (Field : TField;Value : Integer);
  {$endif}
  end;

{$ifdef ver1_0}
{ TProtFields }

procedure TProtFields.SetFieldIndex(Field: TField; Value: Integer);
begin
  inherited SetFieldIndex(Field, Value);
end;
{$endif}

procedure TCustomDbGrid.ColRowMoved(IsColumn: Boolean; FromIndex,
  ToIndex: Integer);
var
  F,CurField: TField;
  i,j: Integer;
begin
  if IsColumn then begin
    CurField := SelectedField;
    if Columns.Enabled then begin
      i := ColumnIndexFromGridColumn( FromIndex );
      j := ColumnIndexFromGridColumn( ToIndex );
      if (i>=0)and(j>=0) then
        Columns[i].Index := J;
    end
    else if FDatalink.Active and (FDataLink.DataSet<>nil) then begin
      F := GetDsFieldFromGridColumn(FromIndex);
      if F<>nil then begin
        {$IFNDEF VER1_0}
        TProtFields(FDatalink.DataSet.Fields).SetFieldIndex( F, ToIndex - FixedCols );
        {$ENDIF}
      end;
    end;
    i := GetGridColumnFromField(CurField);
    if (i>FixedCols) and (i<>Col) then begin
      // todo: buscar alguna otra forma de hacer esto
      FSelectionLock := true;
      try
        Col := i;
      finally
        FSelectionLock := False;
      end;
    end;
    if Assigned(OnColumnMoved) then
      OnColumnMoved(Self, FromIndex, ToIndex);
  end;
end;

function TCustomDbGrid.CreateColumns: TGridColumns;
begin
  result := TDbGridColumns.Create(Self);
end;

procedure TCustomDbGrid.CreateWnd;
begin
  inherited CreateWnd;
  LayoutChanged;
  if Scrollbars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical] then
    ScrollBarShow(SB_VERT, True);
end;

procedure TCustomDbGrid.DefineProperties(Filer: TFiler);
  {
  function HasColumns: boolean;
  var
    C: TGridColumns;
  begin
    if Filer.Ancestor <> nil then
      C := TCustomGrid(Filer.Ancestor).Columns
    else
      C := Columns;
    if C<>nil then
      result := not C.IsDefault
    else
      result := false;
  end;
  }
begin
  // simply avoid to call TCustomGrid.DefineProperties method
  // which defines ColWidths,Rowheights,Cells
  //Filer.DefineProperty('Columns',  @ReadColumns,  @WriteColumns,  HasColumns);
end;

procedure TCustomDbGrid.DefaultDrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
  function GetDatasetState: TDataSetState;
  begin
    if FDatalink.Active then
      result := FDataLink.DataSet.State
    else
      result := dsInactive;
  end;
  procedure FixRectangle;
  begin
    case Canvas.TextStyle.Alignment of
      Classes.taLeftJustify: Inc(aRect.Left, 3);
      Classes.taRightJustify: Dec(aRect.Right, 3);
    end;
    case Canvas.TextStyle.Layout of
      tlTop: Inc(aRect.Top, 3);
      tlBottom: Dec(aRect.Bottom, 3);
    end;
  end;
var
  S: string;
  F: TField;
begin
  if gdFixed in aState then begin
    if (ACol=0) and FDrawingActiveRecord then begin
      DrawArrow(Canvas, aRect, GetDataSetState);
      {$ifdef dbgGridPaint}
      dbgOut('>');
      {$endif}
    end else
    if (aRow=0)and(ACol>=FixedCols) then begin
      FixRectangle;
      Canvas.TextRect(ARect,ARect.Left,ARect.Top,GeTGridColumnTitle(aCol));
    end;
  end else begin
    F := GetFieldFromGridColumn(aCol);
    if F<>nil then begin
      if F.dataType <> ftBlob then
        S := F.DisplayText
      else
        S := '(blob)';
    end else
      S := '';
    FixRectangle;
    Canvas.TextRect(Arect,ARect.Left,ARect.Top, S);
  end;
end;


procedure TCustomDbGrid.DoOnChangeBounds;
begin
  BeginUpdate;
  inherited DoOnChangeBounds;
  if HandleAllocated then
    LayoutChanged;
  EndUpdate(False);
end;

procedure TCustomDbGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  if FSelectionLock then
    exit;
  {$ifdef dbgdbgrid}DebugLn('dbgrid.BefMovSel INIT');{$endif}
  inherited BeforeMoveSelection(DCol, DRow);
  if DCol<>Col then begin
    if assigned(OnColExit) then
      OnColExit(Self);
    FColEnterPending:=True;
  end;
{$ifdef dbgdbgrid}DebugLn('dbgrid.BefMovSel END');{$endif}
end;

procedure TCustomDbGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  if IsColumn and Assigned(OnTitleClick) then
    OnTitleClick(TColumn(ColumnFromGridColumn(Index)));
end;

procedure TCustomDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
  procedure DoOnKeyDown;
  begin
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
  end;
  procedure DoMoveBy(amount: Integer);
begin
    {$IfDef dbgGrid}DebugLn('KeyDown.DoMoveBy(',IntToStr(Amount),')');{$Endif}
    FDatalink.MoveBy(Amount);
    {$IfDef dbgGrid}DebugLn('KeyDown.DoMoveBy FIN');{$Endif}
  end;
  procedure DoMoveBySmall(amount: Integer);
  begin
    {$IfDef dbgGrid}DebugLn('KeyDown.DoMoveBySmall(',IntToStr(Amount),')');{$Endif}
    Include(FGridStatus, gsVisibleMove);
    FDatalink.MoveBy(Amount);
    Exclude(FGridStatus, gsVisibleMove);
    {$IfDef dbgGrid}DebugLn('KeyDown.doMoveBySmall FIN');{$Endif}
  end;
  procedure DoCancel;
  begin
    {$IfDef dbgGrid}DebugLn('KeyDown.doCancel INIT');{$Endif}
    if EditorMode then
      EditorCancelEditing;
    FDatalink.Dataset.cancel;
    {$IfDef dbgGrid}DebugLn('KeyDown.doCancel FIN');{$Endif}
  end;
  procedure DoDelete;
  begin
    {$IfDef dbgGrid}DebugLn('KeyDown.doDelete INIT');{$Endif}
    FDatalink.Dataset.Delete;
    {$IfDef dbgGrid}DebugLn('KeyDown.doDelete FIN');{$Endif}
  end;
  procedure DoAppend;
  begin
    {$IfDef dbgGrid}DebugLn('KeyDown.doAppend INIT');{$Endif}
    FDatalink.Dataset.Append;
    {$IfDef dbgGrid}DebugLn('KeyDown.doAppend FIN');{$Endif}
  end;
  procedure DoInsert;
begin
    {$IfDef dbgGrid}DebugLn('KeyDown.doInsert INIT');{$Endif}
    FDatalink.Dataset.Insert;
    {$IfDef dbgGrid}DebugLn('KeyDown.doInsert FIN');{$Endif}
  end;

begin
  {$IfDef dbgGrid}DebugLn('DbGrid.KeyDown INIT Key= ',IntToStr(Key));{$Endif}
  case Key of
    VK_DELETE:
      if (ssCtrl in Shift) and GridCanModify then begin
        if not (dgConfirmDelete in Options) or
          (MessageDlg('Delete record?',mtConfirmation,mbOKCancel,0)<>mrCancel)
        then
          doDelete;
      end;
    VK_DOWN:
      if ValidDataSet then
      with FDatalink.Dataset do begin
        DoOnKeyDown;
        Key := 0;
        if InsertCancelable then
        begin
          if IsEOF then
            //exit
          else
            doCancel;
        end else begin
          doMoveBySmall(1);
          if GridCanModify and FDataLink.EOF then
            doAppend;
        end;
      end;
      
    VK_UP:
      if ValidDataSet then
      with FDataLink.DataSet do begin
        doOnKeyDown;
        if InsertCancelable and IsEOF then
          doCancel
        else
          doMoveBySmall(-1);
        key := 0;
      end;
      
    VK_NEXT:
      begin
        doOnKeyDown;
        doMoveBy( VisibleRowCount );
        Key := 0;
      end;
      
    VK_PRIOR:
      begin
        doOnKeyDown;
        doMoveBy( -VisibleRowCount );
        key := 0;
      end;
      
    VK_ESCAPE:
      begin
        doOnKeyDown;
        if EditorMode then begin
          EditorCancelEditing;
          if FDatalink.Active and not FDatalink.Dataset.Modified then
            FDatalink.Modified := False;
        end else
          if FDataLink.Active then
            doCancel;
        Key:=0;
      end;
      
    VK_INSERT:
      begin
        doOnKeyDown;
        if GridCanModify then
          doInsert;
        Key:=0;
      end;
      
    VK_HOME:
      begin
        doOnKeyDown;
        if FDatalink.Active then begin
          if ssCTRL in Shift then
            FDataLink.DataSet.First
          else
            MoveNextSelectable(False, FixedCols, Row);
        end;
        Key:=0;
      end;
      
    VK_END:
      begin
        doOnKeyDown;
        if FDatalink.Active then begin
          if ssCTRL in shift then
            FDatalink.DataSet.Last
          else
            MoveNextSelectable(False, ColCount-1, Row);
        end;
        Key:=0;
      end;

    else
      inherited KeyDown(Key, Shift);
  end;
  {$IfDef dbgGrid}DebugLn('DbGrid.KeyDown END Key= ',IntToStr(Key));{$Endif}
end;

procedure TCustomDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
  procedure doMouseDown;
  begin
    if assigned(OnMouseDown) then
      OnMouseDown(Self, Button, Shift, X, Y);
  end;
  procedure doInherited;
  begin
    inherited MouseDown(Button, Shift, X, Y);
  end;
  procedure doMoveBy;
  begin
    {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown MoveBy INIT'); {$Endif}
    Include(FGridStatus, gsVisibleMove);
    FDatalink.MoveBy(P.Y - Row);
    Exclude(FGridStatus, gsVisibleMove);
    {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown MoveBy END'); {$Endif}
  end;
  procedure doMoveToColumn;
  begin
    {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown MoveToCol INIT Col=', IntToStr(P.X)); {$Endif}
    Col := P.X;
    {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown MoveToCol END'); {$Endif}
  end;
  procedure DoCancel;
  begin
    {$IfDef dbgGrid}DebugLn('DbGrid.MouseDown Dataset.CANCEL INIT');{$Endif}
    if EditorMode then
      EditorCancelEditing;
    FDatalink.Dataset.cancel;
    {$IfDef dbgGrid}DebugLn('DbGrid.MouseDown Dataset.CANCEL FIN');{$Endif}
  end;
  procedure DoAcceptValue;
  begin
    if EditorMode and FDatalink.FModified then
      EditorMode := False;
  end;
begin
  {$ifdef dbgdbgrid}DebugLn('dbgrid.mousedown - INIT');{$endif}
  if (csDesigning in componentState) or not GCache.ValidGrid then
    exit;

  if UpdatingData then begin
    {$ifdef dbgdbgrid}DebugLn('DbGrid.MouseDown - UpdatingData');{$endif}
    exit;
  end;
  
  if button<>mbLeft then begin
    doInherited;
    exit;
  end;
  
  {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown INIT'); {$Endif}
  Gz:=MouseToGridZone(X,Y);
  case Gz of
    gzFixedRows, gzFixedCols:
      doInherited;
    else
      begin
        P:=MouseToCell(Point(X,Y));
        if P.Y=Row then begin
          //doAcceptValue;
          doInherited
        end else begin
          doMouseDown;
          if ValidDataSet then begin
            if InsertCancelable and IsEOF then
              doCancel;
            doMoveBy;
        end;
          doMoveToColumn;
      end;
  end;
end;
  {$IfDef dbgGrid} DebugLn('DbGrid.MouseDown END'); {$Endif}
end;

procedure TCustomDbGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);
  if (not FDatalink.Active) and ((gdSelected in aState) or
    (gdFocused in aState)) then
    Canvas.Brush.Color := Self.Color;
end;

procedure TCustomDbGrid.SelectEditor;
begin
  if FDatalink.Active then
    inherited SelectEditor
  else
    Editor := nil;
end;

procedure TCustomDbGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  //SelectedField.AsString := AValue; // Delayed to avoid frequent updates
  FTempText := Value;
end;

function TCustomDbGrid.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  if Which=ssHorizontal then
    Result:= true
  else
    Result:=inherited ScrollBarAutomatic(Which);
end;

function TCustomDbGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:= (ColWidths[aCol] > 0) and (RowHeights[aRow] > 0);
end;

procedure TCustomDbGrid.BeginLayout;
begin
  inc(FLayoutChangedCount);
end;

procedure TCustomDbGrid.EditingColumn(aCol: Integer; Ok: Boolean);
begin
  {$ifdef dbgdbgrid} DebugLn('Dbgrid.EditingColumn INIT aCol=', InttoStr(aCol), ' Ok=', BoolToStr(ok)); {$endif}
  if Ok then begin
    FEditingColumn := aCol;
    FDatalink.Modified := True;
  end
  else
    FEditingColumn := -1;
  {$ifdef dbgdbgrid} DebugLn('Dbgrid.EditingColumn END'); {$endif}
end;

procedure TCustomDbGrid.EditorCancelEditing;
begin
  EditingColumn(FEditingColumn, False); // prevents updating the value
  if EditorMode then begin
    EditorMode := False;
    if dgAlwaysShowEditor in Options then
      EditorMode := True;
  end;
end;

procedure TCustomDbGrid.EditorDoGetValue;
begin
  {$ifdef dbgdbgrid}DebugLn('dbgrid.EditorDoGetValue INIT');{$endif}
  inherited EditordoGetValue;
  UpdateData;
  {$ifdef dbgdbgrid}DebugLn('dbgrid.EditorDoGetValue FIN');{$endif}
end;

procedure TCustomDbGrid.CellClick(const aCol, aRow: Integer);
begin
  if Assigned(OnCellClick) then
    OnCellClick(TColumn(ColumnFromGridColumn(aCol)));
end;

procedure TCustomDbGrid.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TCustomDbGrid.EndLayout;
begin
  dec(FLayoutChangedCount);
  if FLayoutChangedCount = 0 then
    DoLayoutChanged;
end;

function TCustomDbGrid.GetDefaultAlignment(Column: Integer): {$IFDEF VER1_0}Classes.{$ENDIF}TAlignment;
var
  F: TField;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    result := {$IFNDEF VER1_0}F.Alignment{$ELSE}Classes.TAlignment(F.Alignment){$ENDIF}
  else
    result := {$IFNDEF VER1_0}taLeftJustify{$ELSE}Classes.TAlignment(taLeftJustify){$ENDIF};
end;

function TCustomDbGrid.GetDefaultColumnWidth(Column: Integer): Integer;
begin
  Result := DefaultFieldColWidth(GetDsFieldFromGridColumn(Column));
end;

function TCustomDbGrid.GetDefaultReadOnly(Column: Integer): boolean;
var
  F: Tfield;
begin
  result := true;
  if not Self.ReadOnly and (FDataLink.Active and not FDatalink.ReadOnly) then begin
    F := GetDsFieldFromGridColumn(Column);
    result := (F=nil) or F.ReadOnly;
  end;
end;

function TCustomDbGrid.GetDefaultTitle(Column: Integer): string;
var
  F: Tfield;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    Result := F.DisplayName
  else
    Result := '';
end;

procedure TCustomDbGrid.DoExit;
begin
  {$ifdef dbgdbgrid}DebugLn('DbGrid.DoExit INIT');{$Endif}
  if not EditorShowing then begin
    if ValidDataSet and (dgCancelOnExit in Options) and
      InsertCancelable then
    begin
        FDataLink.DataSet.Cancel;
        EditorCancelEditing;
      end;
    end;
  inherited DoExit;
  {$ifdef dbgdbgrid}DebugLn('DbGrid.DoExit FIN');{$Endif}
end;

function TCustomDbGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    Include(FGridStatus, gsVisibleMove);
    FDatalink.MoveBy(1);
    Exclude(FGridStatus, gsVisibleMove);
    Result := True;
  end;
end;

function TCustomDbGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    Include(FGridStatus, gsVisibleMove);
    FDatalink.MoveBy(-1);
    Exclude(FGridStatus, gsVisibleMove);
    Result := True;
  end;
end;

function TCustomDbGrid.GetEditMask(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  Result := '';
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if (aField<>nil) then begin
      // enable following line if TField gets in the future a MaskEdit property
      //Result := aField.EditMask;
      if assigned(OnFieldEditMask) then
        OnFieldEditMask(Self, AField, Result);
    end;
  end;
end;

function TCustomDbGrid.GetEditText(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if aField<>nil then begin
      Result := aField.AsString;
    end;
  end;
end;

function TCustomDbGrid.GridCanModify: boolean;
begin
  result := not ReadOnly and (dgEditing in Options) and not FDataLink.ReadOnly
    and FDataLink.Active and FDatalink.DataSet.CanModify;
end;

procedure TCustomDbGrid.MoveSelection;
begin
  if FSelectionLock then
    exit;
  {$ifdef dbgdbgrid}DebugLn('DbGrid.MoveSelection INIT');{$Endif}
  inherited MoveSelection;
  if FColEnterPending and Assigned(OnColEnter) then begin
    OnColEnter(Self);
  end;
  FColEnterPending:=False;
  UpdateActive;
  {$ifdef dbgdbgrid}DebugLn('DbGrid.MoveSelection FIN');{$Endif}
end;

procedure TCustomDbGrid.DrawByRows;
var
  CurActiveRecord: Integer;
begin
  if FDataLink.Active then begin
    CurActiveRecord:=FDataLink.ActiveRecord;
  end;
  try
    inherited DrawByRows;
  finally
    if FDataLink.Active then
      FDataLink.ActiveRecord:=CurActiveRecord;
  end;
end;

procedure TCustomDbGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect);
begin
  // Draw focused cell if we have the focus
  if {Self.Focused and }(dgAlwaysShowSelection in Options) and
    FDatalink.Active then
  begin
    CalcFocusRect(aRect);
    DrawRubberRect(Canvas, aRect, FocusColor);
  end;
end;

//
procedure TCustomDbGrid.DrawRow(ARow: Integer);
begin
  if (ARow>=FixedRows) and FDataLink.Active then begin
    //if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
    FDrawingActiveRecord := ARow = Row;
  end else
    FDrawingActiveRecord := False;
  {$ifdef dbgGridPaint}
  DbgOut('DrawRow Row=', IntToStr(ARow), ' Act=', Copy(BoolToStr(FDrawingActiveRecord),1,1));
  {$endif}
  inherited DrawRow(ARow);
  {$ifdef dbgGridPaint}
  DebugLn('End Row')
  {$endif}
end;

procedure TCustomDbGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  {$ifdef dbgGridPaint}
  DbgOut(' ',IntToStr(aCol));
  if gdSelected in aState then DbgOut('S');
  if gdFocused in aState then DbgOut('*');
  if gdFixed in aState then DbgOut('F');
  {$endif dbgGridPaint}
  if Assigned(OnDrawColumnCell) and not(CsDesigning in ComponentState) then
    OnDrawColumnCell(Self, aRect, aCol, TColumn(ColumnFromGridColumn(aCol)), aState)
  else
    DefaultDrawCell(aCol, aRow, aRect, aState);
end;

function TCustomDbGrid.EditorCanAcceptKey(const ch: Char): boolean;
var
  aField: TField;
begin
  result := False;
  if FDataLink.Active then begin
    aField := SelectedField;
    if aField<>nil then begin
      Result := aField.IsValidChar(Ch) and not aField.Calculated and
        (aField.DataType<>ftAutoInc);
    end;
  end;
end;

function TCustomDbGrid.EditorIsReadOnly: boolean;
begin
  Result := inherited EditorIsReadOnly;
  if not Result then begin
    Result := not FDataLink.Edit;
    EditingColumn(Col, not Result);
  end;
end;

procedure TCustomDbGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
var
  i: Integer;
begin
  if IsColumn then begin
    if Columns.Enabled then begin
      i := ColumnIndexFromGridColumn(Index);
      if i>=0 then
        Columns[i].Width := ColWidths[Index];
    end;
    FDefaultColWidths := False;
  end;
end;

procedure TCustomDbGrid.UpdateActive;
var
  PrevRow: Integer;
begin
  if (csDestroying in ComponentState) or not FDatalink.Active then
    exit;
    {$IfDef dbgdbgrid}
  DebugLn(Name,'.UpdateActive: ActiveRecord=', dbgs(FDataLink.ActiveRecord),
            ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
    {$endif}
  PrevRow := Row;
  Row:= FixedRows + FDataLink.ActiveRecord;
  if PrevRow<>Row then
    InvalidateCell(0, PrevRow);//(InvalidateRow(PrevRow);
  InvalidateRow(Row);
end;

function TCustomDbGrid.UpdateGridCounts: Integer;
var
  RecCount: Integer;
  FRCount, FCCount: Integer;
begin
  // find out the column count, if result=0 then
  // there are no visible columns defined or dataset is inactive
  // or there are no visible fields, ie the grid is blank
  Result := GetColumnCount;
  if Result > 0 then begin
    BeginVisualChange;
    if dgTitles in Options then FRCount := 1 else FRCount := 0;
    if dgIndicator in Options then FCCount := 1 else FCCount := 0;
    
    InternalSetColCount(Result + FCCount);
    //ColCount := Result + FCCount;
    if FDataLink.Active then begin
      UpdateBufferCount;
      RecCount := FDataLink.RecordCount + FRCount;
      if RecCount<2 then RecCount:=2;
    end else
      RecCount := 2;

    RowCount := RecCount;
    FixedRows := FRCount;
    FixedCols := FCCount;
    UpdateGridColumnSizes;
    EndVisualChange;
  end;
end;

procedure TCustomDbGrid.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage: Integer);
begin
    if (Scrollbars in [ssAutoVertical, ssAutoBoth]) then begin
      // ssAutovertical and ssAutoBoth would get the scrollbar hidden
      // but this case should be handled as if the scrollbar where
      // ssVertical or ssBoth
      ScrollBarShow(SB_VERT, True)
    end else
      ScrollBarShow(SB_VERT, AVisible);
end;

procedure TCustomDbGrid.VisualChange;
begin
  if FVisualChangeCount=0 then begin
    inherited VisualChange;
  end;
end;

constructor TCustomDbGrid.Create(AOwner: TComponent);
begin
  FEditingColumn:=-1;
  DragDx:=5;
  inherited Create(AOwner);

  FDataLink := TComponentDataLink.Create;//(Self);
  FDataLink.OnRecordChanged:=@OnRecordChanged;
  FDataLink.OnDatasetChanged:=@OnDataSetChanged;
  FDataLink.OnDataSetOpen:=@OnDataSetOpen;
  FDataLink.OnDataSetClose:=@OnDataSetClose;
  FDataLink.OnNewDataSet:=@OnNewDataSet;
  FDataLink.OnInvalidDataSet:=@OnInvalidDataset;
  FDataLink.OnInvalidDataSource:=@OnInvalidDataSource;
  FDataLink.OnDataSetScrolled:=@OnDataSetScrolled;
  FDataLink.OnLayoutChanged:=@OnLayoutChanged;
  FDataLink.OnEditingChanged:=@OnEditingChanged;
  FDataLink.OnUpdateData:=@OnUpdateData;
  FDataLink.VisualControl:= True;

  FDefaultColWidths := True;

  FOptions := [dgColumnResize, dgTitles, dgIndicator, dgRowLines, dgColLines,
    dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing, dgAlwaysShowSelection];
    
  inherited Options :=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect,
     goSmoothScroll, goColMoving, goTabs, goEditing, goDrawFocusSelected,
     goColSizing ];

  
  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrolLBars:=ssBoth;
  DefaultTextStyle.Wordbreak := False;

  DefaultRowHeight := 18;
end;

procedure TCustomDbGrid.InitiateAction;
begin
  {$ifdef dbgdbgrid}DebugLn('===> dbgrid.InitiateAction INIT');{$endif}
  inherited InitiateAction;
  if (gsUpdatingData in FGridStatus) then begin
    EndUpdating;
    {
    if EditorMode then begin
      Editor.SetFocus;
      EditorSelectAll;
    end;
    }
  end;
  {$ifdef dbgdbgrid}DebugLn('<=== dbgrid.InitiateAction FIN');{$endif}
end;

procedure TCustomDbGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
  function GetDatasetState: TDataSetState;
  begin
    if FDatalink.Active then
      result := FDataLink.DataSet.State
    else
      result := dsInactive;
  end;
  function FixRectangle: TRect;
  begin
    result := Rect;
    case Canvas.TextStyle.Alignment of
      Classes.taLeftJustify: Inc(Result.Left, 3);
      Classes.taRightJustify: Dec(Result.Right, 3);
    end;
    case Canvas.TextStyle.Layout of
      tlTop: Inc(Result.Top, 3);
      tlBottom: Dec(Result.Bottom, 3);
    end;
  end;
var
  S: string;
  F: TField;
  R: TRect;
begin
  if gdFixed in State then begin
    if (DataCol=0)and FDrawingActiveRecord then
      DrawArrow(Canvas, Rect, GetDataSetState)
    else
    if (DataCol>=FixedCols) then begin
      R := FixRectangle();
      Canvas.TextRect(R,R.Left,R.Top,GeTGridColumnTitle(DataCol));
    end;
  end else begin
    F := GetFieldFromGridColumn(DataCol);
    if F<>nil then begin
      if F.dataType <> ftBlob then
        S := F.DisplayText
      else
        S := '(blob)';
    end else
      S := '';
    R := FixRectangle();
    Canvas.TextRect(R,R.Left,R.Top,S);
  end;
end;

procedure TCustomDbGrid.ResetColWidths;
begin
  if not FDefaultColWidths then begin
    FDefaultColWidths := True;
    LayoutChanged;
  end;
end;

destructor TCustomDbGrid.Destroy;
begin
  FDataLink.OnDataSetChanged:=nil;
  FDataLink.OnRecordChanged:=nil;
  FDataLink.Free;
  inherited Destroy;
end;

{ TComponentDataLink }

function TComponentDataLink.GetFields(Index: Integer): TField;
begin
  if (index>=0)and(index<DataSet.FieldCount) then result:=DataSet.Fields[index];
end;

function TComponentDataLink.GetDataSetName: string;
begin
  Result:=FDataSetName;
  if DataSet<>nil then Result:=DataSet.Name;
end;

procedure TComponentDataLink.SetDataSetName(const AValue: string);
begin
  if FDataSetName<>AValue then FDataSetName:=AValue;
end;

procedure TComponentDataLink.RecordChanged(Field: TField);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.RecordChanged');
  {$endif}
  if Assigned(OnRecordChanged) then
    OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetChanged');
  {$Endif}
  // todo: improve this routine, for example: OnDatasetInserted
  if Assigned(OnDataSetChanged) then
    OnDataSetChanged(DataSet);
end;

procedure TComponentDataLink.ActiveChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.ActiveChanged');
  {$endif}
  if Active then begin
    fDataSet := DataSet;
    if DataSetName <> fDataSetName then begin
      fDataSetName := DataSetName;
      if Assigned(fOnNewDataSet) then fOnNewDataSet(DataSet);
    end else
      if Assigned(fOnDataSetOpen) then fOnDataSetOpen(DataSet);
  end else begin
    BufferCount := 0;
    if (DataSource = nil)then begin
      if Assigned(fOnInvalidDataSource) then fOnInvalidDataSource(fDataSet);
      fDataSet := nil;
      fDataSetName := '[???]';
    end else begin
      if (DataSet=nil)or(csDestroying in DataSet.ComponentState) then begin
        if Assigned(fOnInvalidDataSet) then fOnInvalidDataSet(fDataSet);
        fDataSet := nil;
        fDataSetName := '[???]';
      end else begin
        if Assigned(fOnDataSetClose) then fOnDataSetClose(DataSet);
        if DataSet <> nil then FDataSetName := DataSetName;
      end;
    end;
  end;
end;

procedure TComponentDataLink.LayoutChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.LayoutChanged');
  {$Endif}
  if Assigned(OnLayoutChanged) then
    OnLayoutChanged(DataSet);
end;

procedure TComponentDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetScrolled(',IntToStr(Distance),')');
  {$endif}
  if Assigned(OnDataSetScrolled) then
    OnDataSetScrolled(DataSet, Distance);
end;

procedure TComponentDataLink.FocusControl(Field: TFieldRef);
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.FocusControl');
  {$endif}
end;

procedure TComponentDataLink.CheckBrowseMode;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.CheckBrowseMode');
  {$endif}
  inherited CheckBrowseMode;
end;

procedure TComponentDataLink.EditingChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.EditingChanged');
  {$endif}
  if Assigned(OnEditingChanged) then
    OnEditingChanged(DataSet);
end;

procedure TComponentDataLink.UpdateData;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.UpdateData');
  {$endif}
  if Assigned(OnUpdatedata) then
    OnUpdateData(DataSet);
end;

function TComponentDataLink.MoveBy(Distance: Integer): Integer;
begin
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.MoveBy  INIT: Distance=',Distance);
  {$endif}
  *)
  Result:=inherited MoveBy(Distance);
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.MoveBy  END: Distance=',Distance);
  {$endif}
  *)
end;

{ TDbGridColumns }

function TDbGridColumns.GetColumn(Index: Integer): TColumn;
begin
  result := TColumn( inherited Items[Index] );
end;

procedure TDbGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign( Value );
end;

procedure TDbGridColumns.Update(Item: TCollectionItem);
begin
  if (Grid<>nil) and not (csLoading in Grid.ComponentState) then
    TCustomDBGrid(Grid).LayoutChanged;
end;

function TDbGridColumns.ColumnFromField(Field: TField): TColumn;
var
  i: Integer;
begin
  if Field<>nil then
  for i:=0 to Count-1 do begin
    result := Items[i];
    if (result<>nil)and(result.Field=Field) then
      exit;
  end;
  result:=nil;
end;

constructor TDbGridColumns.Create(TheGrid: TCustomDBGrid);
begin
  inherited Create( TheGrid, TColumn );
  //FGrid := TheGrid;
end;

function TDbGridColumns.Add: TColumn;
begin
  result := TColumn( inherited add );
end;

procedure TDbGridColumns.LinkFields;
var
  i: Integer;
  G: TCustomDBGrid;
begin
  G := TCustomDBGrid(Grid);
  if G<>nil then
    G.BeginLayout;
  for i:=0 to Count-1 do
    Items[i].LinkField;
  if G<>nil then
    G.EndLayout;
end;

{ TColumn }

function TColumn.GetField: TField;
begin
  if (FFieldName<>'') and (FField<>nil) then
    LinkField;
  result := FField;
end;

procedure TColumn.ApplyDisplayFormat;
begin
  if (FField <> nil) and FDisplayFormatChanged then begin
    if (FField is TNumericField) then
      TNumericField(FField).DisplayFormat := DisplayFormat
    else if (FField is TDateTimeField) then
      TDateTimeField(FField).DisplayFormat := DisplayFormat;
  end;
end;

function TColumn.GetDisplayFormat: string;
begin
  if not FDisplayFormatChanged then
    Result := GetDefaultDisplayFormat
  else
    result := FDisplayFormat;
end;

function TColumn.IsDisplayFormatStored: boolean;
begin
  Result := FDisplayFormatChanged;
end;

procedure TColumn.SetDisplayFormat(const AValue: string);
begin
  if (not FDisplayFormatChanged)or(CompareText(AValue, FDisplayFormat)<>0) then begin
    FDisplayFormat := AValue;
    FDisplayFormatChanged:=True;
    ColumnChanged;
  end;
end;

procedure TColumn.SetField(const AValue: TField);
begin
  if FField <> AValue then begin
    FField := AValue;
    if FField<>nil then
      FFieldName := FField.FieldName;
    ColumnChanged;
  end;
end;

procedure TColumn.SetFieldName(const AValue: String);
begin
  if FFieldName=AValue then exit;
  FFieldName:=AValue;
  LinkField;
  ColumnChanged;
end;

function TColumn.GetDataSet: TDataSet;
var
  TheGrid: TCustomDBGrid;
begin
  TheGrid := TCustomDBGrid(Grid);
  if (TheGrid<>nil) then
    result := TheGrid.FDataLink.DataSet
  else
    result :=nil;
end;

function TColumn.InternalDefaultWidth: Integer;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid := TCustomDBGrid(Grid);
  if (theGrid<>nil)and(TheGrid.HandleAllocated)and(FField<>nil) then
    result := FField.DisplayWidth * CalcCanvasCharWidth(TheGrid.Canvas)
  else
    result := 64;
end;

function TColumn.CreateTitle: TGridColumnTitle;
begin
  Result := TColumnTitle.Create(Self);
end;

function TColumn.IsDefault: boolean;
begin
  result := not FDisplayFormatChanged and (inherited IsDefault());
end;

procedure TColumn.LinkField;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid:= TCustomDBGrid(Grid);
  if (TheGrid<>nil) and TheGrid.FDatalink.Active then begin
    Field := TheGrid.FDataLink.DataSet.FindField(FFieldName);
    ApplyDisplayFormat;
  end else
    Field := nil;
end;

function TColumn.GetDefaultDisplayFormat: string;
begin
  Result := '';
  if FField<>nil then begin
    if FField is TNumericField then
      Result := TNumericField(FField).DisplayFormat
    else if FField is TDateTimeField then
      Result := TDateTimeField(FField).DisplayFormat
  end;
end;

function TColumn.InternalDefaultReadOnly: boolean;
var
  TheGrid: TCustomDBGrid;
begin
  TheGrid := TCustomDBGrid(Grid);
  Result := ((TheGrid<>nil)and(TheGrid.ReadOnly)) or ((FField<>nil)and(FField.ReadOnly))
end;

function TColumn.GetDefaultVisible: boolean;
begin
  if FField<>nil then
    result := FField.Visible
  else
    result := True;
end;

function TColumn.GetDisplayName: string;
begin
  if FFieldName='' then
    result := 'column'
  else
    Result:=FFieldName;
end;

function TColumn.GetDefaultAlignment: {$IFDEF VER1_0}Classes.{$ENDIF}TAlignment;
begin
  if FField<>nil then
    result := {$IFNDEF VER1_0}FField.Alignment{$ELSE}Classes.TAlignment(FField.Alignment){$ENDIF}
  else
    Result := {$IFDEF VER1_0}Classes.{$ENDIF}taLeftJustify;
end;

{ TColumnTitle }

function TColumnTitle.GetDefaultCaption: string;
begin
  with (Column as TColumn) do begin
    if FieldName<>'' then begin
      if FField<>nil then
        Result := Field.DisplayName
      else
        Result := Fieldname;
    end else
      Result := inherited GetDefaultCaption;
  end;
end;

end.

{
  $Log$
  Revision 1.41  2005/05/21 13:24:58  mattias
  TextRect clipping patch  from Jesus

  Revision 1.40  2005/05/02 08:35:42  mattias
  fix bug 878 (can not paste from clipboard) new property TitleStyle  from Jesus

  Revision 1.39  2005/04/03 10:13:34  mattias
  dbgrids: Stops propagating ENTER key when modifying a field  from Jesus

  Revision 1.38  2005/03/29 21:56:02  marc
  * patch from Jesus Reyes

  Revision 1.37  2005/03/24 09:50:03  mattias
  checking HandleAllocated for dbgrid   from Jesus

  Revision 1.36  2005/03/23 15:59:33  mattias
  blob fields for TDBGrid  from Jose A. Rimon

  Revision 1.35  2005/03/08 10:32:47  mattias
  BorderStyle for TCustomEdit in win32 intf  from Jesus

  Revision 1.34  2005/03/07 23:21:36  mattias
  fixed DefaultRowHeight=18 and various published properties  from Jesus

  Revision 1.33  2005/03/04 07:06:18  vincents
  added initialization of FEditingColumn   from Luiz Americo

  Revision 1.32  2005/02/08 11:33:16  mattias
  fixes for empty dbgrid  from Jesus

  Revision 1.31  2005/02/06 22:43:38  mattias
  dbgrid.ThumbTracking and fixes  from Jesus

  Revision 1.30  2005/01/16 13:16:31  mattias
  added DoCompareCells, changed OnCompareCell  from Jesus

  Revision 1.29  2005/01/10 15:59:43  vincents
  some ugly hacks to fix 1.0.x compilation

  Revision 1.28  2005/01/09 14:11:03  mattias
  registered andadded icon for TTIGrid

  Revision 1.27  2005/01/08 15:06:06  mattias
  fixed TabOrder dialog for new TabOrder

  Revision 1.26  2005/01/05 15:20:10  mattias
  Columns property in TCustomGrid  by Jesus

  Revision 1.24  2004/12/23 19:24:46  mattias
  added auto column sizing  from Jesus

  Revision 1.22  2004/12/21 22:49:29  mattias
  implemented scrollbar codes for gtk intf  from Jesus

  Revision 1.21  2004/10/12 08:23:20  mattias
  fixed compiler options interface double variables

  Revision 1.20  2004/10/09 18:08:52  vincents
  Fix fpc 1.0.x compilation.

  Revision 1.19  2004/10/09 12:16:20  mattias
  From Jesus: Ctrl+HOME does dataset.first, ctrl+END does dataSet.Last and fixes

  Revision 1.18  2004/09/24 13:45:31  mattias
  fixed TCanvas.TextRect Delphi compatible Rect and added TBarChart from Michael VC

  Revision 1.17  2004/09/23 12:59:37  vincents
  fix fpc 1.0.x. compilation

  Revision 1.16  2004/09/08 07:21:10  vincents
  - removed some debug output
  - removed wmsize event handler
  - changed case of some keywords
  - improved DrawFocusRect in case there are not gridlines

  Revision 1.15  2004/09/02 17:42:38  mattias
  fixed changing CNCHar.CharCode when key changed

  Revision 1.14  2004/09/01 20:18:03  micha
  from jesus reyes:
  grids:
   + bug 388 fixed
   + focused cell rectangle redesign
   + many internal changes

  dbgrids:
   + font support, still the object inspector doesnt allow main/title
  font changes but it works in the column/column.title font.
   + column/column.title text layout (tltop, tlcenter, tlbottom)
   + editing support
   + fixed resize designtime problems
   + options

  Revision 1.12  2004/08/07 07:03:29  mattias
  implemented virtual temporary ct files

  Revision 1.11  2004/08/06 06:51:15  mattias
  fixed compilation for fpc 1.0.10

  Revision 1.10  2004/08/04 09:35:38  mattias
  implemented setting TTabSheet.TabIndex

  Revision 1.9  2004/08/03 15:38:53  vincents
  fix 1.0.x compilation.

}
