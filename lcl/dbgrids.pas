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
interface

uses
  Classes, LCLProc, Graphics, SysUtils, LCLType, stdctrls, DB, LMessages, Grids,
  Controls;

type
  TDataSetScrolledEvent = procedure(DataSet: TDataSet; Distance: Integer) of object;
  TColumnNotifyEvent = procedure(Sender:TObject; Field: TField) of object;
  TCustomDbGrid = class;
  TColumn = class;
type
  TComponentDataLink=class(TDatalink)
  private
    FDataSet: TDataSet;
    FDataSetName: string;
    FModified: Boolean;
    FOnDatasetChanged: TDatasetNotifyEvent;
    fOnDataSetClose: TDataSetNotifyEvent;
    fOnDataSetOpen: TDataSetNotifyEvent;
    FOnDataSetScrolled: TDataSetScrolledEvent;
    fOnInvalidDataSet: TDataSetNotifyEvent;
    fOnInvalidDataSource: TDataSetNotifyEvent;
    FOnLayoutChanged: TDataSetNotifyEvent;
    fOnNewDataSet: TDataSetNotifyEvent;
    FOnRecordChanged: TFieldNotifyEvent;
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
  public
    procedure Modified;
    Property OnRecordChanged: TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    Property OnDataSetChanged: TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
    property OnNewDataSet: TDataSetNotifyEvent read fOnNewDataSet write fOnNewDataSet;
    property OnDataSetOpen: TDataSetNotifyEvent read fOnDataSetOpen write fOnDataSetOpen;
    property OnInvalidDataSet: TDataSetNotifyEvent read fOnInvalidDataSet write fOnInvalidDataSet;
    property OnInvalidDataSource: TDataSetNotifyEvent read fOnInvalidDataSource write fOnInvalidDataSource;
    property OnLayoutChanged: TDataSetNotifyEvent read FOnLayoutChanged write FOnLayoutChanged;
    property OnDataSetClose: TDataSetNotifyEvent read fOnDataSetClose write fOnDataSetClose;
    Property OnDataSetScrolled: TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    Property DataSetName:string read GetDataSetName write SetDataSetName;
    Property Fields[Index: Integer]: TField read GetFields;
    Property VisualControl;
  end;

  TColumnTitle = class(TPersistent)
  private
    FColumn: TColumn;
    FCaption: PString;
    FColor: ^TColor;
    FAlignment: ^TAlignment;
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetColor: TColor;
    function IsAlignmentStored: boolean;
    function IsCaptionStored: boolean;
    function IsColorStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
  public
    constructor Create(TheColumn: TColumn); virtual;
    destructor Destroy; override;
    property Column: TColumn read FColumn;
    function IsDefault: boolean;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
  end;

  TColumn = class(TCollectionItem)
  private
    FFieldName: String;
    FTitle: TColumnTitle;
    FField: TField;
    
    FAlignment: ^TAlignment;
    FColor: ^TColor;
    FVisible: ^Boolean;
    FReadOnly: ^Boolean;
    FWidth: ^Integer;
    
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetField: TField;
    function GetGrid: TCustomDBGrid;
    function GetReadOnly: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsAlignmentStored: boolean;
    function IsColorStored: boolean;
    function IsReadOnlyStored: boolean;
    function IsVisibleStored: boolean;
    function IsWidthStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetColor(const AValue: TColor);
    procedure SetField(const AValue: TField);
    procedure SetFieldName(const AValue: String);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetTitle(const AValue: TColumnTitle);
    procedure SetVisible(const AValue: Boolean);
    procedure SetWidth(const AValue: Integer);
    
    function GetDataSet: TDataSet;
    function GetDefaultReadOnly: boolean;
    function GetDefaultWidth: Integer;
  protected
  {$ifdef ver1_0}
  // workaround to access protected procedure in base class
    procedure Changed(AllItems: Boolean);
  {$endif}
    function  CreateTitle: TColumnTitle; virtual;
    function  GetDisplayName: string; override;
    procedure FieldChanged;
    procedure LinkField;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function  IsDefault: boolean;
    property  Grid: TCustomDBGrid read GetGrid;
    property  Field: TField read GetField write SetField;
  published
    property  Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property  Color: TColor read GetColor write SetColor stored IsColorStored;
    property  FieldName: String read FFieldName write SetFieldName;
    property  ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property  Title: TColumnTitle read FTitle write SetTitle;
    property  Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property  Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored;
  end;

  TDbGridColumns = class(TCollection)
  private
    FGrid: TCustomDBGrid;
    function GetColumn(Index: Integer): TColumn;
    function GetEnabled: Boolean;
    procedure SetColumn(Index: Integer; Value: TColumn);
    function GetVisibleCount: Integer;
  protected
    procedure Update(Item: TCollectionItem); override;
    function ColumnFromField(Field: TField): TColumn;
  public
    constructor Create(TheGrid: TCustomDBGrid);
    function  Add: TColumn;
    procedure LinkFields;
    function RealIndex(Index: Integer): Integer;
    function IsDefault: boolean;
    property Grid: TCustomDBGrid read FGrid;
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
    property VisibleCount: Integer read GetVisibleCount;
    property Enabled: Boolean read GetEnabled;
  end;

  TCustomDbGrid=class(TCustomGrid)
  private
    FDataLink: TComponentDataLink;
    FOnColEnter,FOnColExit: TColumnNotifyEvent;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    FNumRecords: Integer;
    FColumns: TDbGridColumns;
    FLayoutChangedCount: integer;
    FVisualChangeCount: Integer;
    FSelectionLock: Boolean;
    FNormalViewLocked: Boolean;
    FCanBrowse: Boolean;
    function GetCurrentField: TField;
    function GetDataSource: TDataSource;
    procedure OnRecordChanged(Field:TField);
    procedure OnDataSetChanged(aDataSet: TDataSet);
    procedure OnDataSetOpen(aDataSet: TDataSet);
    procedure OnDataSetClose(aDataSet: TDataSet);
    procedure OnInvalidDataSet(aDataSet: TDataSet);
    procedure OnInvalidDataSource(aDataSet: TDataset);
    procedure OnLayoutChanged(aDataSet: TDataSet);
    procedure OnNewDataSet(aDataSet: TDataset);
    procedure OnDataSetScrolled(aDataSet:TDataSet; Distance: Integer);
    procedure ReadColumns(Reader: TReader);
    procedure SetColumns(const AValue: TDBGridColumns);
    procedure SetCurrentField(const AValue: TField);
    procedure SetDataSource(const AValue: TDataSource);
    procedure UpdateBufferCount;
    
    // Temporal
    function GetColumnAlignment(Column: Integer; ForTitle: Boolean): TAlignment;
    function GetColumnColor(Column: Integer; ForTitle: Boolean): TColor;
    function GetColumnCount: Integer;
    function GetColumnTitle(Column: Integer): string;
    function GetColumnWidth(Column: Integer): Integer;
    function DefaultFieldColWidth(F: TField): Integer;
    
    function GetDsFieldFromGridColumn(Column: Integer): TField;
    function GetFieldFromGridColumn(Column: Integer): TField;
    function GetGridColumnFromField(F: TField): Integer;
    function FieldIndexFromGridColumn(Column: Integer): Integer;

    procedure UpdateGridColumnSizes;
    procedure BeginVisualChange;
    procedure EndVisualChange;
    procedure DoLayoutChanged;
    procedure WriteColumns(Writer: TWriter);
  protected
  {$ifdef ver1_0}
    property FixedColor;
  {$endif}
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure CheckBrowse;
    procedure ClearGrid;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    function  CreateColumns: TDBGridColumns;
    function  ColumnIndexFromGridColumn(Column: Integer): Integer;
    function  ColumnFromGridColumn(Column: Integer): TColumn;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DrawByRows; override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
    
    procedure MoveSelection; override;
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    function  ScrollBarAutomatic(Which: TScrollStyle): boolean; override;
    function  SelectCell(aCol, aRow: Integer): boolean; override;
    procedure BeginLayout;
    procedure EndLayout;
    procedure VisualChange; override;

    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;

    procedure UpdateActive;
    function  UpdateGridCounts(const ViewLocking: boolean ): Integer;
    property Columns: TDBGridColumns read FColumns write SetColumns;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    Property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property OnColEnter: TColumnNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TColumnNotifyEvent read FOnColExit write FOnColExit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedField: TField read GetCurrentField write SetCurrentField;
  end;
  
  
  TdbGrid=class(TCustomDbGrid)
  public
    property Canvas;
    //property SelectedRows;
  published
  
    property Align;
    property Anchors;
    //property BiDiMode;
    //property BorderStyle;
    property Color;
    property Columns stored false;
    property Constraints;
    //property Ctl3D;
    property DataSource;
    property DefaultDrawing;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property FixedColor;
    property Font;
    //property ImeMode;
    //property ImeName;
    //property Options;
    //property ParentBiDiMode;
    property ParentColor;
    //property ParentCtl3D;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property TitleFont;
    property Visible;
    //property OnCellClick;
    property OnColEnter;
    property OnColExit;
    //property OnColumnMoved;
    //property OnDrawDataCell;  { obsolete }
    //property OnDrawColumnCell;
    property OnDblClick;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnEditButtonClick;
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
    //property OnStartDock;
    //property OnStartDrag;
    //property OnTitleClick;
  end;

procedure Register;
  
implementation

{$ifndef ver1_0}
const
  NoValidColor = TColor(-791);
  NoValidAlignment = TAlignment(-791);
{$endif}
  
procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;

function CalcCanvasCharWidth(Canvas:TCanvas): integer;
begin
  result := Canvas.TextWidth('W l') div 3;
end;

{ TCustomdbGrid }

procedure TCustomDbGrid.OnRecordChanged(Field: TField);
begin
  {$IfDef dbgdbgrid}
  DBGOut('('+name+') ','TCustomDBGrid.OnRecordChanged(Field=');
  if Field=nil then DebugLn('nil)')
  else              DebugLn(Field.FieldName,')');
  {$Endif}
end;

function TCustomDbGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
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
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$Ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetOpen');
  {$endif}
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
  LinkActive(True);
  UpdateActive;
end;

procedure TCustomDbGrid.OnDataSetScrolled(aDataset: TDataSet; Distance: Integer);
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName, ' (',name,')', '.OnDataSetScrolled(',IntToStr(Distance),'), Invalidating');
  {$endif}
  UpdateActive;
  if Distance<>0 then Invalidate;
end;

procedure TCustomDbGrid.ReadColumns(Reader: TReader);
begin
  FColumns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(FColumns);
end;

procedure TCustomDbGrid.SetColumns(const AValue: TDBGridColumns);
begin
  FColumns.Assign(AValue);
end;

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
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDbGrid.UpdateBufferCount;
begin
  if FDataLink.Active then begin
    //if FGCache.ValidGrid then
      FDataLink.BufferCount:= ClientHeight div DefaultRowHeight - 1;
    //else
    //  FDataLink.BufferCount:=0;
    {$ifdef dbgdbgrid}
    DebugLn(ClassName, ' (',name,')', ' FdataLink.BufferCount=' + IntToStr(Fdatalink.BufferCount));
    {$endif}
  end;
end;

procedure TCustomDbGrid.WMVScroll(var Message: TLMVScroll);
var
  Num: Integer;
  C, TL: Integer;
begin
  inherited;
  if not GCache.ValidGrid then Exit;
  {$ifdef dbgdbgrid}
  DebugLn('VSCROLL: Code=',dbgs(Message.ScrollCode),' Position=', dbgs(Message.Pos));
  {$endif}
  exit;
  C:=Message.Pos+GCache.Fixedheight;
  Num:=(FNumRecords + FixedRows) * DefaultRowHeight;
  TL:= Num div C;
  GCache.TLRowOff:= C - TL*DefaultRowHeight;
  DebugLn('---- Offset=',dbgs(C), ' ScrollTo=> TL=',dbgs(TL), ' TLRowOFf=', dbgs(GCache.TLRowOff));
end;


function TCustomDbGrid.DefaultFieldColWidth(F: TField): Integer;
begin
  if not HandleAllocated or (F=nil) then
    result:=DefaultColWidth
  else begin
    if F.DisplayWidth = 0 then
      result := Canvas.TextWidth( F.DisplayName ) + 4
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
  if FColumns.Enabled then
    result := FColumns.VisibleCount
  else
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
  if FColumns.Enabled then begin
    i := ColumnIndexFromGridColumn( Column );
    if i>=0 then
      result := FColumns[i].FField
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
  while (i<FDataLink.DataSet.FieldCount) do begin
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

function TCustomDbGrid.GetColumnWidth(Column: Integer): Integer;
var
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    Result := C.Width
  else
    Result := DefaultFieldColWidth(GetDsFieldFromGridColumn(Column));
end;

procedure TCustomDbGrid.UpdateGridColumnSizes;
var
  i: Integer;
begin
  ColWidths[0]:=12;
  for i:=1 to ColCount-1 do
    ColWidths[i] := GetColumnWidth(i);
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
var
  Count: Integer;
begin
  if csDestroying in ComponentState then
    exit;
  if FDataLink.Active then begin
    FNumRecords:= FDataLink.DataSet.RecordCount;
    UpdateBufferCount;
    Count := UpdateGridCounts(false);
    if Count>0 then begin
      ScrollBarRange(SB_HORZ, GridWidth + 2);
      ScrollBarRange(SB_VERT, (FNumRecords + FixedRows) * DefaultRowHeight + 2);
      Exit;
    end;
  end;
  ClearGrid;
end;

procedure TCustomDbGrid.WriteColumns(Writer: TWriter);
begin
  if FColumns.IsDefault then
    Writer.WriteCollection(nil)
  else
    Writer.WriteCollection(FColumns);
end;

function TCustomDbGrid.GetColumnTitle(Column: Integer): string;
var
  F: Tfield;
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    Result := C.Title.Caption
  else begin
    F := GetDsFieldFromGridColumn(Column);
    if F<>nil then
      Result := F.DisplayName
    else
      Result := '';
  end;
end;

function TCustomDbGrid.GetColumnColor(Column: Integer; ForTitle: Boolean
  ): TColor;
var
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    if ForTitle then
      result := C.Title.Color
    else
      result := C.Color
  else
    if ForTitle then
      result := FixedColor
    else
      result := clWhite;
end;

// Get the grid alignment for the given column
function TCustomDbGrid.GetColumnAlignment(Column: Integer; ForTitle: Boolean
  ): TAlignment;
var
  F: Tfield;
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    if ForTitle then
      Result := C.Title.Alignment
    else
      Result := C.Alignment
  else begin
    F := GetDsFieldFromGridColumn(Column);
    if F<>nil then
      result := F.Alignment
    else
      result := taLeftJustify;
  end;
end;

procedure TCustomDbGrid.LinkActive(Value: Boolean);
begin
  LayoutChanged;
end;

procedure TCustomDBGrid.LayoutChanged;
begin
  if FLayoutChangedCount = 0 then begin
    BeginLayout;
    if FColumns.Count>0 then
      FColumns.LinkFields
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

procedure TCustomDbGrid.CheckBrowse;
begin
  FCanBrowse := FDataLink.Active and not FNormalViewLocked;
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
    if FColumns.Enabled then begin
      i := ColumnIndexFromGridColumn( FromIndex );
      j := ColumnIndexFromGridColumn( ToIndex );
      if (i>=0)and(j>=0) then
        FColumns[i].Index := J;
    end
    else if (FDataLink.DataSet<>nil)and FDatalink.Active then begin
      F := GetDsFieldFromGridColumn(FromIndex);
      if F<>nil then
        TProtFields(FDatalink.DataSet.Fields).SetFieldIndex( F, ToIndex - FixedCols );
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
  end;
end;

function TCustomDbGrid.CreateColumns: TDBGridColumns;
begin
  result := TDbGridColumns.Create(Self);
end;

function TCustomDbGrid.ColumnIndexFromGridColumn(Column: Integer): Integer;
begin
  Result := FColumns.RealIndex( Column - FixedCols );
end;

function TCustomDbGrid.ColumnFromGridColumn(Column: Integer): TColumn;
var
  ColIndex: Integer;
begin
  ColIndex := FColumns.RealIndex( Column - FixedCols );
  if ColIndex>=0 then
    result := FColumns[ColIndex]
  else
    result := nil;
end;

procedure TCustomDbGrid.DefineProperties(Filer: TFiler);
  function HasColumns: boolean;
  var
    C: TDbGridColumns;
  begin
    if Filer.Ancestor <> nil then
      C := TCustomDBGrid(Filer.Ancestor).Columns
    else
      C := FColumns;
    if C<>nil then
      result := not C.IsDefault
    else
      result := false;
  end;
begin
  with Filer do begin
    DefineProperty('Columns',  @ReadColumns,  @WriteColumns,  HasColumns);
  end;
end;

procedure TCustomDbGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  if FSelectionLock then
    exit;
  inherited BeforeMoveSelection(DCol, DRow);
  FDatalink.UpdateData;
  if DCol<>Col then begin
     if assigned(OnColExit) then
      OnColExit(Self, GetFieldFromGridColumn(Col));
     FColEnterPending:=True;
  end;
end;

procedure TCustomDbGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  inherited HeaderClick(IsColumn, index);
end;

procedure TCustomDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
  procedure MoveBy(Delta: Integer);
  begin
    FDatalink.MoveBy(Delta);
  end;
begin
  case Key of
    VK_DOWN: MoveBy(1);
    VK_UP: MoveBy(-1);
    VK_NEXT: MoveBy( VisibleRowCount );
    VK_PRIOR: MoveBy( -VisibleRowCount );
    else inherited;
  end;
end;

procedure TCustomDbGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Gz: TGridZone;
  P: TPoint;
begin
  if csDesigning in componentState then Exit;
  if not GCache.ValidGrid then Exit;
  
  Gz:=MouseToGridZone(X,Y, False);
  case Gz of
    gzFixedRows, gzFixedCols: inherited MouseDown(Button, Shift, X, Y);
    else
      begin
        P:=MouseToCell(Point(X,Y));
        if P.Y=Row then inherited MouseDown(Button, Shift, X, Y)
        else begin
          BeginUpdate;
          FDatalink.MoveBy(P.Y - Row);
          Col:=P.X;
          EndUpdate(uoQuick);
        end;
      end;
  end;
end;

procedure TCustomDbGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var
  OldOnEvent: TOnPrepareCanvasEvent;
  ForTitle: boolean;
  TheAlignment: TAlignment;
begin
  OldOnEvent := OnPrepareCanvas;
  OnPrepareCanvas := nil;
  inherited PrepareCanvas(aCol, aRow, aState);
  // we get the default canvas values
  // now, modify canvas according to column values
  if gdSelected in aState then begin
    // what to do in selected state?
    if FNormalViewLocked then
      Canvas.Brush.Color := GetColumnColor(ACol, false);
  end else begin
    ForTitle := gdFixed in aState;
    Canvas.Brush.Color := GetColumnColor(ACol, ForTitle);
    TheAlignment := GetColumnAlignment(ACol, ForTitle);
    case TheAlignment of
      taRightJustify: Canvas.TextStyle.Alignment := Classes.taRightJustify;
      taCenter: Canvas.TextStyle.Alignment := Classes.taCenter;
      taLeftJustify: Canvas.TextStyle.Alignment := classes.taLeftJustify;
    end;
  end;
  OnPrepareCanvas := OldOnEvent;
  if Assigned(OnPrepareCanvas) then
    OnPrepareCanvas(Self, aCol, aRow, aState);
end;

function TCustomDbGrid.ScrollBarAutomatic(Which: TScrollStyle): boolean;
begin
  if Which=ssHorizontal then
    Result:=True
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

procedure TCustomDbGrid.EndLayout;
begin
  dec(FLayoutChangedCount);
  if FLayoutChangedCount = 0 then
    DoLayoutChanged;
end;

procedure TCustomDbGrid.MoveSelection;
begin
  if FSelectionLock then
    exit;
  inherited MoveSelection;
  if FColEnterPending and Assigned(OnColEnter) then begin
    OnColEnter(Self, GetFieldFromGridColumn(Col));
  end;
  FColEnterPending:=False;
  UpdateActive;
end;

procedure TCustomDbGrid.DrawByRows;
var
  CurActiveRecord: Integer;
begin
  CheckBrowse;
  if FCanBrowse then begin
    CurActiveRecord:=FDataLink.ActiveRecord;
    //PrimerRecord:=FDataLink.FirstRecord;
  end;
  try
    inherited DrawByRows;
  finally
    if FCanBrowse then
      FDataLink.ActiveRecord:=CurActiveRecord;
  end;
end;
// 33 31 21 29 80 90 4 3
procedure TCustomDbGrid.DrawRow(ARow: Integer);
begin
  if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
  inherited DrawRow(ARow);
end;

procedure DrawArrow(Canvas: TCanvas; R: TRect; Opt: TDataSetState);
var
  dx,dy, x, y: Integer;
begin
  case Opt of
    dsBrowse:
      begin //
          Canvas.Brush.Color:=clBlack;
          Canvas.Pen.Color:=clBlack;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsEdit:
      begin // Normal
          Canvas.Brush.Color:=clRed;
          Canvas.Pen.Color:=clRed;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsInsert:
      begin // Normal
          Canvas.Brush.Color:=clGreen;
          Canvas.Pen.Color:=clGreen;
          Dx:=6;
          Dy:=6;
          y:= R.top+ (R.Bottom-R.Top) div 2;
          x:= R.Left+2;
          Canvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
   end;
end;

procedure TCustomDbGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
  function GetDatasetState: TDataSetState;
  begin
    if FDatalink.Active then
      result := FDataLink.DataSet.State
    else
      result := dsInactive;
  end;
var
  S: string;
  F: TField;
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  
  if gdFixed in aState then begin
    if (aRow=0)and(ACol>=FixedCols) then begin
      Canvas.TextRect(ARect, 2, 2,  GetColumnTitle(aCol));
    end else
    if (aCol=0)and(aRow=Row) then
      DrawArrow(Canvas, aRect, GetDataSetState)
  end else begin
    F := GetFieldFromGridColumn(aCol);
    if F<>nil then begin
      if not F.Visible then exit;
      S := F.AsString;
    end else
      S := '';
    Canvas.TextRect(Arect, 2, 2, S);
    //Canvas.TextOut(aRect.Left+2,ARect.Top+2, S);
  end;
end;

procedure TCustomDbGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
var
  i: Integer;
begin
  if IsColumn then
    if FColumns.Enabled then begin
      i := ColumnIndexFromGridColumn(Index);
      if i>=0 then
        FColumns[i].Width := ColWidths[Index];
    end;
end;

procedure TCustomDbGrid.UpdateActive;
begin
  with FDataLink do begin
    if not Active then exit;
    //if not GCache.ValidGrid then Exit;
    //if DataSource=nil then Exit;
    DebugLn('(',Name,') ActiveRecord=', dbgs(ActiveRecord), ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
    Row:= FixedRows + ActiveRecord;
  end;
  Invalidate;
end;

function TCustomDbGrid.UpdateGridCounts(const ViewLocking: boolean): Integer;
begin
  Result := GetColumnCount;
  if Result >0 then begin
    BeginVisualChange;
    ColCount := Result + 1;
    if ViewLocking then
      RowCount:= 2
    else
      RowCount := FDataLink.RecordCount+1;
    FixedRows:=1;
    FixedCols:=1;
    UpdateGridColumnSizes;
    EndVisualChange;
    //Invalidate;
    exit;
  end;
end;

procedure TCustomDbGrid.VisualChange;
begin
  if FVisualChangeCount=0 then
    inherited VisualChange;
end;

constructor TCustomDbGrid.Create(AOwner: TComponent);
begin
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
  FDataLink.VisualControl:= True;

  FColumns := CreateColumns;

  FReadOnly:=True;
  Options:=Options + [goColMoving, goColSizing, goDrawFocusSelected];
  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrolLBars:=ssBoth;
  DefaultTextStyle.Wordbreak := false;
  
  ClearGrid;
end;

destructor TCustomDbGrid.Destroy;
begin
  FColumns.Free;
  FDataLink.OnDataSetChanged:=nil;
  FDataLink.OnRecordChanged:=nil;
  FDataLink.Free;
  inherited Destroy;
end;

procedure TCustomDbGrid.ClearGrid;
begin
  FNormalViewLocked := False;
  if FColumns.Enabled then
    FNormalViewLocked := UpdateGridCounts(true) > 0
  else
  if FDataLink.Active then
    FNormalViewLocked := UpdateGridCounts(true) > 0
  else
    Clear;
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
  if Assigned(OnRecordChanged) then OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn('TComponentDataLink.DataSetChanged');
  {$Endif}
  if Assigned(OnDataSetChanged) then OnDataSetChanged(DataSet);
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
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.CheckBrowseMode');
  {$endif}
  *)
  inherited CheckBrowseMode;
end;

procedure TComponentDataLink.EditingChanged;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.EditingChanged');
  {$endif}
  inherited EditingChanged;
end;

procedure TComponentDataLink.UpdateData;
begin
  (*
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.UpdateData');
  {$endif}
  *)
  inherited UpdateData;
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

procedure TComponentDataLink.Modified;
begin
  {$ifdef dbgdbgrid}
  DebugLn(ClassName,'.Modified');
  {$Endif}
  FModified:=True;
end;

{ TDbGridColumns }

function TDbGridColumns.GetColumn(Index: Integer): TColumn;
begin
  result := TColumn( inherited Items[Index] );
end;

function TDbGridColumns.GetEnabled: Boolean;
begin
  result := VisibleCount > 0;
end;

procedure TDbGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign( Value );
end;

procedure TDbGridColumns.Update(Item: TCollectionItem);
begin
  if (FGrid<>nil) and not (csLoading in FGrid.ComponentState) then
    FGrid.LayoutChanged;
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
  inherited Create( TColumn );
  FGrid := TheGrid;
end;

function TDbGridColumns.Add: TColumn;
begin
  result := TColumn( inherited add );
end;

function TDbGridColumns.GetVisibleCount: Integer;
var
  i: Integer;
begin
  result := 0;
  for i:=0 to Count-1 do
    if Items[i].Visible then
      inc(result);
end;

procedure TDbGridColumns.LinkFields;
var
  i: Integer;
begin
  if FGrid<>nil then
    FGrid.BeginLAyout;
  for i:=0 to Count-1 do
    Items[i].LinkField;
  if FGrid<>nil then
    FGrid.EndLayout;
end;

function TDbGridColumns.RealIndex(Index: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  if Index>=0 then
    for i:=0 to Count-1 do begin
      if Items[i].Visible then begin
        Dec(index);
        if Index<0 then begin
          result := i;
          exit;
        end;
      end;
    end;
end;

function TDbGridColumns.IsDefault: boolean;
var
  i: Integer;
begin
  result := True;
  for i:=0 to Count-1 do
    result := Result and Items[i].IsDefault;
end;

{ TColumn }

function TColumn.GetAlignment: TAlignment;
begin
  if FAlignment=nil then
    if FField<>nil then
      result := FField.Alignment
    else
      result := taLeftJustify
  else
    result := FAlignment^;
end;

function TColumn.GetColor: TColor;
var
  TmpGrid: TCustomDBGrid;
begin
  TmpGrid := Grid;
  if FColor=nil then
    if TmpGrid<>nil then
      result := TmpGrid.Color
    else
      result := clWhite
  else
    result := FColor^
end;

function TColumn.GetField: TField;
begin
  if (FFieldName<>'') and (FField<>nil) then
    LinkField;
  result := FField;
end;

function TColumn.GetGrid: TCustomDBGrid;
begin
  if Collection is TDbGridColumns then
    result := (Collection as TDbGridColumns).Grid
  else
    result := nil;
end;

function TColumn.GetReadOnly: Boolean;
begin
  if FReadOnly=nil then
    result := GetDefaultReadOnly
  else
    result := FReadOnly^;
end;

function TColumn.GetVisible: Boolean;
begin
  if FVisible=nil then
    if FField<>nil then
      result := FField.Visible
    else
      result := True
  else
    result := FVisible^;
end;

function TColumn.GetWidth: Integer;
begin
  if FWidth=nil then
    result := GetDefaultWidth
  else
    result := FWidth^;
end;

function TColumn.IsAlignmentStored: boolean;
begin
  result := FAlignment <> nil;
end;

function TColumn.IsColorStored: boolean;
begin
  result := FColor <> nil;
end;

function TColumn.IsReadOnlyStored: boolean;
begin
  result := FReadOnly <> nil;
end;

function TColumn.IsVisibleStored: boolean;
begin
  result := (FVisible<>nil) and not FVisible^;
end;

function TColumn.IsWidthStored: boolean;
begin
  result := FWidth <> nil;
end;

procedure TColumn.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment = nil then
    New(FAlignment)
  else if FAlignment^ = AValue then
    exit;
  FAlignment^ := AValue;
  FieldChanged;
end;

procedure TColumn.SetColor(const AValue: TColor);
begin
  if FColor = nil then
    New(FColor)
  else if FColor^ = AValue then
   exit;
  FColor^ := AValue;
  FieldChanged;
end;

procedure TColumn.SetField(const AValue: TField);
begin
  if FField <> AValue then begin
    FField := AValue;
    if FField<>nil then
      FFieldName := FField.FieldName;
    FieldChanged;
  end;
  
end;

procedure TColumn.SetFieldName(const AValue: String);
begin
  if FFieldName=AValue then exit;
  FFieldName:=AValue;
  LinkField;
  FieldChanged;
end;

procedure TColumn.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = nil then
    New(FReadOnly)
  else if FReadOnly^ = AValue then
    exit;
  FReadOnly^ := Avalue;
  FieldChanged;
end;

procedure TColumn.SetTitle(const AValue: TColumnTitle);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

procedure TColumn.SetVisible(const AValue: Boolean);
begin
  if FVisible = nil then
    New(FVisible)
  else if FVisible^ = AValue then
    exit;
  FVisible^ := AValue;
  FieldChanged;
end;

procedure TColumn.SetWidth(const AValue: Integer);
begin
  if FWidth = nil then
    New(FWidth)
  else if FWidth^ = AVAlue then
    exit;
  FWidth^ := AValue;
  FieldChanged;
end;

function TColumn.GetDataSet: TDataSet;
var
  TheGrid: TCustomDBGrid;
begin
  TheGrid := Grid;
  if (TheGrid<>nil) then
    result := TheGrid.FDataLink.DataSet
  else
    result :=nil;
end;

function TColumn.GetDefaultReadOnly: boolean;
var
  TheGrid: TCustomDBGrid;
begin
  TheGrid := Grid;
  Result := ((TheGrid<>nil)and(TheGrid.ReadOnly)) or ((FField<>nil)and(FField.ReadOnly))
end;

function TColumn.GetDefaultWidth: Integer;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid := Grid;
  if (theGrid<>nil)and(TheGrid.HAndleAllocated)And(FField<>nil) then
    result := FField.DisplayWidth * CalcCanvasCharWidth(TheGrid.Canvas)
  else
    result := 64;
end;

{$ifdef ver1_0}
procedure TColumn.Changed(AllItems: Boolean);
begin
  inherited Changed(AllItems);
end;
{$endif}

procedure TColumn.LinkField;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid:= Grid;
  if (TheGrid<>nil) and TheGrid.FDatalink.Active then
    Field := TheGrid.FDataLink.DataSet.FindField(FFieldName);
end;

procedure TColumn.FieldChanged;
begin
  Changed(False);
end;

function TColumn.CreateTitle: TColumnTitle;
begin
  result := TColumnTitle.Create(Self);
end;

function TColumn.GetDisplayName: string;
begin
  if FFieldName='' then
    Result := Inherited GetDisplayName
  else
    Result:=FFieldName;
end;

constructor TColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTitle := CreateTitle;
end;

destructor TColumn.Destroy;
begin
  if FAlignment<>nil then Dispose(FAlignment);
  if FColor<>nil then Dispose(FColor);
  if FVisible<>nil then Dispose(FVisible);
  if FReadOnly<>nil then Dispose(FReadOnly);
  if FWidth<>nil then Dispose(FWidth);
  FTitle.Free;
  inherited Destroy;
end;

function TColumn.IsDefault: boolean;
begin
  result := FTitle.IsDefault and (FAlignment=nil) and (FColor=nil)
    and (FVisible=nil) and (FReadOnly=nil) and (FWidth=nil);
end;

{ TColumnTitle }

function TColumnTitle.GetAlignment: TAlignment;
begin
  if FAlignment = nil then
    result := taLeftJustify
  else
    result := FAlignment^;
end;

function TColumnTitle.GetCaption: string;
begin
  if FCaption = nil then
    if FColumn.Field <> nil then
      result := FColumn.Field.DisplayName
    else
      result := FColumn.FieldName
  else
    result := FCaption^;
end;

function TColumnTitle.GetColor: TColor;
begin
  if FColor = nil then
    if FColumn.Grid <> nil then
      result := FColumn.Grid.FixedColor
    else
      result := clBtnFace
  else
    result := FColor^;
end;

function TColumnTitle.IsAlignmentStored: boolean;
begin
  result := FAlignment <> nil;
end;

function TColumnTitle.IsCaptionStored: boolean;
begin
  result := FCaption <> nil;
end;

function TColumnTitle.IsColorStored: boolean;
begin
  result := FColor <> nil;
end;

procedure TColumnTitle.SetAlignment(const AValue: TAlignment);
begin
  if Falignment = nil then
    New(Falignment)
  else if FAlignment^ = AValue then
    exit;
  FAlignment^ := AValue;
  FColumn.Changed(False);
end;

procedure TColumnTitle.SetCaption(const AValue: string);
begin
  if (FCaption=nil)or(CompareText(AValue, FCaption^)<>0) then begin
    if FCaption<>nil then
      DisposeStr(FCaption);
    FCaption := NewStr(AValue);
    FColumn.Changed(False);
  end;
end;

procedure TColumnTitle.SetColor(const AValue: TColor);
begin
  if FColor=nil then
    New(FColor)
  else if FColor^=AValue then
    exit;
  FColor^ := AValue;
  FColumn.FieldChanged;
end;

constructor TColumnTitle.Create(TheColumn: TColumn);
begin
  inherited Create;
  FColumn := TheColumn;
end;

destructor TColumnTitle.Destroy;
begin
  if FAlignment<>nil then Dispose(FAlignment);
  if FColor<>nil then Dispose(FColor);
  if FCaption<>nil then DisposeStr(FCaption);
  inherited Destroy;
end;

function TColumnTitle.IsDefault: boolean;
begin
  result :=  (FAlignment=nil) and (FColor=nil) and (FCaption=nil);
end;

end.

{
  $Log$
  Revision 1.10  2004/08/04 09:35:38  mattias
  implemented setting TTabSheet.TabIndex

  Revision 1.9  2004/08/03 15:38:53  vincents
  fix 1.0.x compilation.

}
