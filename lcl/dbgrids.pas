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
  Classes, LCLIntf, LCLProc, Graphics, SysUtils, LCLType, stdctrls, DB, LMessages, Grids,
  Controls, Buttons;

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



  TColumnButtonStyle = (cbsAuto, cbsEllipsis, cbsNone);
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

type
  TCellButton = class(TButton)
  private
    FGrid: TCustomGrid;
  protected
    Procedure msg_SetGrid(Var Msg: TGridMessage); Message GM_SETGRID;
    Procedure msg_SetPos(Var Msg: TGridMessage); Message GM_SETPOS;
  end;
  
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
    property OnDataSetScrolled: TDataSetScrolledEvent read FOnDataSetScrolled write FOnDataSetScrolled;
    property OnEditingChanged: TDataSetNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnUpdateData: TDataSetNotifyEvent read FOnUpdateData write FOnUpdateData;
    property DataSetName:string read GetDataSetName write SetDataSetName;
    Property Fields[Index: Integer]: TField read GetFields;
    Property VisualControl;
  end;

  TColumnTitle = class(TPersistent)
  private
    FColumn: TColumn;
    FCaption: PChar;
    FColor: ^TColor;
    FAlignment: ^TAlignment;
    FFont: TFont;
    FIsDefaultTitleFont: boolean;
    FLayout: ^TTextLayout;
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetCaption: string;
    function GetColor: TColor;
    function GetFont: TFont;
    function GetLayout: TTextLayout;
    function IsAlignmentStored: boolean;
    function IsCaptionStored: boolean;
    function IsColorStored: boolean;
    function IsFontStored: boolean;
    function IsLayoutStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
    procedure SetFont(const AValue: TFont);
    procedure SetLayout(const AValue: TTextLayout);
    property IsDefaultFont: boolean read FIsDefaultTitleFont;
  public
    constructor Create(TheColumn: TColumn); virtual;
    destructor Destroy; override;
    procedure FillTitleDefaultFont;
    function IsDefault: boolean;
    property Column: TColumn read FColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
  end;

  TColumn = class(TCollectionItem)
  private
    FButtonStyle: TColumnButtonStyle;
    FDropDownRows: Longint;
    FFieldName: String;
    FTitle: TColumnTitle;
    FField: TField;
    
    FAlignment: ^TAlignment;
    FColor: ^TColor;
    FLayout: ^TTextLayout;
    FVisible: ^Boolean;
    FReadOnly: ^Boolean;
    FWidth: ^Integer;
    FFont: TFont;
    FisDefaultFont: Boolean;
    FPickList: TStrings;
    
    procedure FontChanged(Sender: TObject);
    function GetAlignment: TAlignment;
    function GetColor: TColor;
    function GetExpanded: Boolean;
    function GetField: TField;
    function GetFont: TFont;
    function GetGrid: TCustomDBGrid;
    function GetLayout: TTextLayout;
    function GetPickList: TStrings;
    function GetReadOnly: Boolean;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    function IsAlignmentStored: boolean;
    function IsColorStored: boolean;
    function IsFontStored: boolean;
    function IsLayoutStored: boolean;
    function IsReadOnlyStored: boolean;
    function IsVisibleStored: boolean;
    function IsWidthStored: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetButtonStyle(const AValue: TColumnButtonStyle);
    procedure SetColor(const AValue: TColor);
    procedure SetExpanded(const AValue: Boolean);
    procedure SetField(const AValue: TField);
    procedure SetFieldName(const AValue: String);
    procedure SetFont(const AValue: TFont);
    procedure SetLayout(const AValue: TTextLayout);
    procedure SetPickList(const AValue: TStrings);
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
    property IsDefaultFont: boolean read FIsDefaultFont;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure FillDefaultFont;
    function  IsDefault: boolean;
    property Grid: TCustomDBGrid read GetGrid;
    property Field: TField read GetField write SetField;

  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property DropDownRows: Longint read FDropDownRows write FDropDownRows default 7;
    property Expanded: Boolean read GetExpanded write SetExpanded default True;
    property FieldName: String read FFieldName write SetFieldName;
    property Font: TFont read GetFont write SetFont stored IsFontStored;
    property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
    property PickList: TStrings read GetPickList write SetPickList;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
    property Title: TColumnTitle read FTitle write SetTitle;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
    property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored;
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
    procedure TitleFontChanged;
    procedure FontChanged;
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
    FOnCellClick: TDBGridClickEvent;
    FOnColEnter,FOnColExit: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnEditButtonClick: TNotifyEvent;
    FOnFieldEditMask: TGetDbEditMaskEvent;
    FOnTitleClick: TDBGridClickEvent;
    FOptions: TDbGridOptions;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    //FNumRecords: Integer;
    FColumns: TDbGridColumns;
    FLayoutChangedCount: integer;
    FVisualChangeCount: Integer;
    FSelectionLock: Boolean;
    //FNormalViewLocked: Boolean;
    FTitleFont,FLastFont: TFont;
    FButtonEditor: TCellButton;
    FStringEditor: TStringCellEditor;
    FTempText : string;
    FDrawingActiveRecord: Boolean;
    FEditingColumn: Integer;
    //FScrolling,FScrollCalc: boolean;
    FOldPosition: Integer;
    function GetCurrentField: TField;
    function GetDataSource: TDataSource;
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
    procedure ReadColumns(Reader: TReader);
    procedure SetColumns(const AValue: TDBGridColumns);
    procedure SetCurrentField(const AValue: TField);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetOptions(const AValue: TDbGridOptions);
    procedure SetTitleFont(const AValue: TFont);
    procedure UpdateBufferCount;
    procedure UpdateData;
    
    // Temporal
    function GetColumnAlignment(Column: Integer; ForTitle: Boolean): TAlignment;
    function GetColumnColor(Column: Integer; ForTitle: Boolean): TColor;
    function GetColumnCount: Integer;
    function GetColumnFont(Column: Integer; ForTitle: Boolean): TFont;
    function GetColumnLayout(Column: Integer; ForTitle: boolean): TTextLayout;
    function GetColumnTitle(Column: Integer): string;
    function GetColumnWidth(Column: Integer): Integer;
    function GetColumnReadOnly(Column: Integer): boolean;
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
    procedure WriteColumns(Writer: TWriter);
    
    procedure OnTitleFontChanged(Sender: TObject);
    procedure RestoreEditor;
    function  ISEOF: boolean;
  protected
  {$ifdef ver1_0}
    property FixedColor;
  {$endif}
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure BeginLayout;
    procedure CellClick(const aCol,aRow: Integer); override;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    function  CreateColumns: TDBGridColumns;
    function  ColumnIndexFromGridColumn(Column: Integer): Integer;
    function  ColumnFromGridColumn(Column: Integer): TColumn;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefaultDrawCell(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DoExit; override;
    procedure DoOnChangeBounds; override;
    procedure DrawByRows; override;
    procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect); override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure EditButtonClicked(Sender: TObject);
    procedure EditingColumn(aCol: Integer; Ok: boolean);
    procedure EditorCancelEditing;
    procedure EditordoGetValue; override;
    function  EditorCanAcceptKey(const ch: Char): boolean; override;
    function  EditorIsReadOnly: boolean; override;
    procedure EndLayout;
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
    
    property Columns: TDBGridColumns read FColumns write SetColumns;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Options: TDbGridOptions read FOptions write SetOptions;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    
    property OnCellClick: TDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;
    property OnFieldEditMask: TGetDbEditMaskEvent read FOnFieldEditMask write FOnFieldEditMask;
    property OnTitleClick: TDBGridClickEvent read FOnTitleClick write FOnTitleClick;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
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
    property AutoAdvance;
    //property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns stored false;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight;
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
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
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
        y:= R.top+ (R.Bottom-R.Top) div 2;
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
  result := Canvas.TextWidth('MX') div 2;
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
  LayoutChanged;
  UpdateActive;
  RestoreEditor;
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

procedure TCustomDbGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgdbgrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnEditingChanged');
  DebugLn('Editing=', BoolToStr(dsEdit = aDataSet.State));
  DebugLn('Inserting=',BoolToStr(dsInsert = aDataSet.State));
  {$endif}
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
  UpdateActive;
  if Distance<>0 then Invalidate;
end;

procedure TCustomDbGrid.OnUpdateData(aDataSet: TDataSet);
begin
  UpdateData;
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

    {
    if dgAlwaysShowSelection in FOptions then
      Include(OldOptions, goDrawFocusSelected)
    else
      Exclude(OldOptions, goDrawFocusSelected);
    }

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

procedure TCustomDbGrid.SetTitleFont(const AValue: TFont);
begin
  FTitleFont.Assign(AValue);
  LayoutChanged;
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
  if FDatalink.Editing then begin
    SelField := SelectedField;
    edField := GetFieldFromGridColumn(FEditingColumn);
    if (edField<>nil) and (edField = SelField) then
    //if (edField<>nil) and (FEditingField=SelField) then
      edField.AsString := FTempText;
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
  IsSeq, IsFirst, IsLast: boolean;
  aPos: Integer;
  DeltaRec: integer;
begin
  if not FDatalink.Active then exit;
  
  {$ifdef dbgdbgrid}
  DebugLn('VSCROLL: Code=',SbCodeToStr(Message.ScrollCode),
          ' Position=', dbgs(Message.Pos),' OldPos=',Dbgs(FOldPosition));
  {$endif}

  IsSeq := FDatalink.DataSet.IsSequenced {$ifndef EnableIsSeq} and false {$endif};
  if IsSeq then begin
    aPos := Message.Pos;
    FDatalink.DataSet.RecNo := aPos;
  end else begin
    IsFirst := false;
    IsLast := false;
    case Message.ScrollCode of
      SB_PAGEUP: DeltaRec := -VisibleRowCount;
      SB_LINEUP: DeltaRec := -1;
      SB_LINEDOWN: DeltaRec := 1;
      SB_PAGEDOWN: DeltaRec := VisibleRowCount;
      SB_THUMBPOSITION:
        begin
          if Message.Pos=4 then IsLast := True
          else if Message.Pos=0 then IsFirst := True
          else begin
            DeltaRec := Message.Pos - FOldPosition;
            if DeltaRec=0 then exit; // no movement at all
            if DeltaRec<-1 then DeltaRec := -VisibleRowCount
            else if DeltaRec>1 then DeltaRec := VisibleRowCount;
          end;
        end;
      else
        exit; // SB_THUMPOSITION, SB_ENDSCROLL
    end;
    if IsFirst then FDataLink.DataSet.First
    else if IsLast then FDatalink.DataSet.Last
    else if (DeltaRec<>0) then FDatalink.Moveby(DeltaRec);

    if FDatalink.Dataset.BOF then aPos := 0
    else if FDataLink.DataSet.EOF then aPos := 4
    else aPos := 2;
  end;
  
  ScrollBarPosition(SB_VERT, aPos);
  FOldPosition:=aPos;
  
  
  {$ifdef dbgdbgrid}
  DebugLn('---- Diff=',dbgs(DeltaRec), ' FinalPos=',dbgs(aPos));
  {$endif}
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
    if FDataLink.Active then
      for i:=0 to FDataLink.DataSet.FieldCount-1 do begin
        F:= FDataLink.DataSet.Fields[i];
        if (F<>nil) and F.Visible then
          Inc(Result);
      end;
end;

function TCustomDbGrid.GetColumnFont(Column: Integer; ForTitle: Boolean
  ): TFont;
var
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    if ForTitle then
      Result := C.Title.Font
    else
      Result := C.Font
  else begin
    if ForTitle then
      Result := TitleFont
    else
      Result := Self.Font;
  end;
end;

function TCustomDbGrid.GetColumnLayout(Column: Integer; ForTitle: boolean
  ): TTextLayout;
var
  C: TColumn;
begin
  C := ColumnFromGridColumn(Column);
  if C<>nil then
    if ForTitle then
      Result := C.Title.Layout
    else
      Result := C.Layout
  else
    result := tlCenter;
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

function TCustomDbGrid.GetColumnReadOnly(Column: Integer): boolean;
var
  F: Tfield;
  C: TColumn;
begin
  result := true;
  if not Self.ReadOnly and (FDataLink.Active and not FDatalink.ReadOnly) then begin
    C := ColumnFromGridColumn(Column);
    if c<>nil then
      result := C.ReadOnly
    else begin
      F := GetDsFieldFromGridColumn(Column);
      result := (F<>nil) and F.ReadOnly;
    end;
  end;
end;

procedure TCustomDbGrid.UpdateGridColumnSizes;
var
  i: Integer;
begin
  if dgIndicator in Options then
    ColWidths[0]:=12;
  for i:=FixedCols to ColCount-1 do
    ColWidths[i] := GetColumnWidth(i);
end;

procedure TCustomDbGrid.UpdateScrollbarRange;
var
  aRange, aPage: Integer;
  aPos: Integer;
  isSeq: boolean;
  ScrollInfo: TScrollInfo;
begin
  if FDatalink.Active then begin
    IsSeq := FDatalink.dataset.IsSequenced{$ifndef EnableIsSeq}and false{$endif};
    if IsSeq then begin
      aRange := FDatalink.DataSet.RecordCount + 2;
      aPage := VisibleRowCount;
      if aPage<1 then aPage := 1;
      aPos := FDataLink.DataSet.RecNo;
    end else begin
      aRange := 6;
      aPage := 2;
      if FDatalink.EOF then aPos := 5 else
      if FDatalink.BOF then aPos := 0
      else aPos := 2;
    end;
  end else begin
    aRange := 0;
    aPage := 0;
    aPos := 0;
  end;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
  //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
  ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := aRange;
  ScrollInfo.nPos := aPos;
  ScrollInfo.nPage := aPage;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
  FOldPosition := aPos;
  {$ifdef dbgdbgrid}
  DebugLn('UpdateScrollBarRange: aRange=' + IntToStr(aRange)+
    ' aPage=' + IntToStr(aPage) + ' aPos='+IntToStr(aPos));
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
  if UpdateGridCounts=0 then
    Clear;
  UpdateScrollBarRange;
end;

procedure TCustomDbGrid.WriteColumns(Writer: TWriter);
begin
  if FColumns.IsDefault then
    Writer.WriteCollection(nil)
  else
    Writer.WriteCollection(FColumns);
end;

procedure TCustomDbGrid.OnTitleFontChanged(Sender: TObject);
begin
  if FColumns.Enabled then
    FColumns.TitleFontChanged
  else
    LayoutChanged;
end;

procedure TCustomDbGrid.RestoreEditor;
begin
  if EditorMode then begin
    EditorMode := False;
    EditorMode := True;
  end;
end;

(*
// Workaround: dataset is not EOF after Append!
type
  TMyDs=class(TDataSet)
  {$IFDEF VER1_0}
    //fpc 1.0 can't call the protected method GetBookmarkFlag outside the class.
    function DoGetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
  {$ENDIF}
  end;

{$IFDEF VER1_0}
function TMyDs.DoGetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := GetBookmarkFlag(Buffer);
end;
{$ENDIF}
*)
function TCustomDbGrid.IsEOF: boolean;
begin
  with FDatalink do
    result :=
      Active and DataSet.EOF;
(*

{$IFNDEF VER1_0}
      (TMyDS(Dataset).GetBookmarkFlag(DataSet.ActiveBuffer) = bfEOF);
{$ELSE}
      (TMyDS(Dataset).DoGetBookmarkFlag(DataSet.ActiveBuffer) = bfEOF);
{$ENDIF}
*)
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
  if FLayoutChangedCount=0 then begin
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

procedure TCustomDbGrid.CreateWnd;
begin
  inherited CreateWnd;
  LayoutChanged;
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
    Inc(aRect.Top, 2);
  end;
var
  S: string;
  F: TField;
begin
  if gdFixed in aState then begin
    if (ACol=0) and FDrawingActiveRecord then
      DrawArrow(Canvas, aRect, GetDataSetState)
    else
    if (aRow=0)and(ACol>=FixedCols) then begin
      FixRectangle;
      Canvas.TextRect(ARect,ARect.Left,ARect.Top,GetColumnTitle(aCol));
    end;
  end else begin
    F := GetFieldFromGridColumn(aCol);
    if F<>nil then begin
      S := F.DisplayText;
    end else
      S := '';
    FixRectangle;
    Canvas.TextRect(Arect,ARect.Left,ARect.Top, S);
  end;
end;


procedure TCustomDbGrid.DoOnChangeBounds;
begin
  BeginVisualChange;
  inherited DoOnChangeBounds;
  if HandleAllocated then
    LayoutChanged;
  EndVisualChange;
end;

procedure TCustomDbGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  if FSelectionLock then
    exit;
  inherited BeforeMoveSelection(DCol, DRow);
  if FDataLink.Active then begin
    if FDataLink.Editing then
      FDataLink.UpdateData;
    {
    if dgCancelOnExit in Options then
      FDataLink.DataSet.Cancel
    else
      FDatalink.UpdateData;
    }
  end;
  if DCol<>Col then begin
    if assigned(OnColExit) then
      OnColExit(Self);
    FColEnterPending:=True;
  end;
end;

procedure TCustomDbGrid.HeaderClick(IsColumn: Boolean; index: Integer);
begin
  if IsColumn and Assigned(OnTitleClick) then
    OnTitleClick(ColumnFromGridColumn(Index));
end;

procedure TCustomDbGrid.KeyDown(var Key: Word; Shift: TShiftState);
  procedure DoOnKeyDown;
  begin
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
  end;
begin
  case Key of
    VK_DOWN:
      if FDatalink.DataSet <> nil then
      with FDatalink.Dataset do begin
        DoOnKeyDown;
        Key := 0;
        if (State=dsInsert)and not (Modified or FDataLink.FModified) then begin
          if IsEOF then
            exit
          else
            cancel;
        end else begin
          FDatalink.MoveBy(1);
          if GridCanModify and FDataLink.EOF then
            Append;
        end;
      end;
      
    VK_UP:
      if FDataLink.DataSet <> nil then
      with FDataLink.DataSet do begin
        doOnKeyDown;
        if (State=dsInsert) and IsEOF and not (Modified or FDataLink.FModified) then
          cancel
        else
          FDatalink.MoveBy(-1);
        key := 0;
      end;
      
    VK_NEXT:
      begin
        doOnKeyDown;
        FDatalink.MoveBy( VisibleRowCount );
        Key := 0;
      end;
      
    VK_PRIOR:
      begin
        doOnKeyDown;
        FDatalink.MoveBy( -VisibleRowCount );
        key := 0;
      end;
      
    VK_ESCAPE:
      begin
        doOnKeyDown;
        if EditorMode then
          EditorCancelEditing
        else
          if FDataLink.Active then
            FDataLink.DataSet.Cancel;
        Key:=0;
      end;
      
    VK_INSERT:
      begin
        doOnKeyDown;
        if GridCanModify then
          FDataLink.DataSet.Insert;
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
          if assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X,Y);
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
  aFont: TFont;
begin
  OldOnEvent := OnPrepareCanvas;
  OnPrepareCanvas := nil;
  inherited PrepareCanvas(aCol, aRow, aState);
  // we get the default canvas values
  // now, modify canvas according to column values
  ForTitle:=false;
  TheAlignment := GetColumnAlignment(ACol, ForTitle);
  case TheAlignment of
    taRightJustify: Canvas.TextStyle.Alignment := Classes.taRightJustify;
    taCenter: Canvas.TextStyle.Alignment := Classes.taCenter;
    taLeftJustify: Canvas.TextStyle.Alignment := classes.taLeftJustify;
  end;
  Canvas.TextStyle.Layout := GetColumnLayout(aCol, ForTitle);
  if gdSelected in aState then begin
    // what to do in selected state?
    //Canvas.Brush.Color := GetColumnColor(ACol, false);
  end else begin
    ForTitle := gdFixed in aState;
    Canvas.Brush.Color := GetColumnColor(ACol, ForTitle);
    aFont := GetColumnFont(ACol, ForTitle);
    if aFont<>FLastFont then begin
      Canvas.Font := aFont;
      FLastFont := aFont;
    end;
  end;
  OnPrepareCanvas := OldOnEvent;
  if Assigned(OnPrepareCanvas) then
    OnPrepareCanvas(Self, aCol, aRow, aState);
end;

procedure TCustomDbGrid.SelectEditor;
var
  C: TColumn;
  Ed: TWinControl;
begin
  if not (dgEditing in Options) then
    exit;
    
  Ed := FStringEditor;
  C := ColumnFromGridColumn(Col);
  if C<>nil then begin
    case C.ButtonStyle of
      cbsAuto:
        begin
          if C.PickList.Count>0 then begin
            // Ed := FComboEditor;
          end;
        end;
      cbsEllipsis:
        begin
          Ed := FButtonEditor;
        end;
    end;
  end;
  
  Editor := Ed;
  inherited SelectEditor;
end;

procedure TCustomDbGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  //SelectedField.AsString := AValue; // Delayed to avoid frequent updates
  FTempText := Value;
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

procedure TCustomDbGrid.EditingColumn(aCol: Integer; Ok: Boolean);
begin
  if Ok then
    FEditingColumn := aCol
  else
    FEditingColumn := -1;
end;

procedure TCustomDbGrid.EditorCancelEditing;
begin
  EditingColumn(FEditingColumn, False);
  if EditorMode then begin
    EditorMode := False;
    if dgAlwaysShowEditor in Options then
      EditorMode := True;
  end;
end;

procedure TCustomDbGrid.EditordoGetValue;
begin
  inherited EditordoGetValue;
  UpdateData;
  EditingColumn(FEditingColumn, False);
end;

procedure TCustomDbGrid.CellClick(const aCol, aRow: Integer);
begin
  if Assigned(OnCellClick) then
    OnCellClick(ColumnFromGridColumn(aCol));
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

procedure TCustomDbGrid.DoExit;
begin
  if not EditorShowing then begin
    if FDataLink.Active then begin
      if (FDataLink.DataSet.State=dsInsert) and (dgCancelOnExit in Options)
      then begin
        FDataLink.DataSet.Cancel;
        EditorCancelEditing;
      end;
    end;
  end;
  inherited DoExit;
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
  {$ifdef dbgdbgrid}
    DebugLn(' ReadOnly=', BoolToStr(ReadOnly), ' dgEditing=', BoolToStr(dgEditing in Options));
    DebugLn(' FDatalink.ReadOnly=', BoolToStr(FDatalink.ReadOnly), ' FDatalink.ACtive=', BooltoStr(FDatalink.ACtive));
    DebugLn(' ds.CanModify=',BoolToStr(Fdatalink.Dataset.CanModify));
  {$endif}
  result := not ReadOnly and (dgEditing in Options) and not FDataLink.ReadOnly
    and FDataLink.Active and FDatalink.DataSet.CanModify;
end;

procedure TCustomDbGrid.MoveSelection;
begin
  if FSelectionLock then
    exit;
  inherited MoveSelection;
  if FColEnterPending and Assigned(OnColEnter) then begin
    OnColEnter(Self);
  end;
  FColEnterPending:=False;
  UpdateActive;
end;

procedure TCustomDbGrid.DrawByRows;
var
  CurActiveRecord: Integer;
begin
  //CheckBrowse;
  if FDataLink.Active then begin
  //if FCanBrowse then begin
    CurActiveRecord:=FDataLink.ActiveRecord;
    //PrimerRecord:=FDataLink.FirstRecord;
  end;
  try
    inherited DrawByRows;
  finally
    if FDataLink.Active then
    //if FCanBrowse then
      FDataLink.ActiveRecord:=CurActiveRecord;
  end;
end;

procedure TCustomDbGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect);
begin
  // Draw focused cell if we have the focus
  if {Self.Focused and }(dgAlwaysShowSelection in Options) then
  begin
    CalcFocusRect(aRect);
    DrawRubberRect(Canvas, aRect, FocusColor);
  end;
end;

// 33 31 21 29 80 90 4 3
procedure TCustomDbGrid.DrawRow(ARow: Integer);
begin
  if (ARow>=FixedRows) and FDataLink.Active then begin
    //if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
    FDrawingActiveRecord := ARow = Row;
  end else
    FDrawingActiveRecord := False;
  inherited DrawRow(ARow);
end;

procedure TCustomDbGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  inherited DrawCell(aCol, aRow, aRect, aState);
  if Assigned(OnDrawColumnCell) and not(CsDesigning in ComponentState) then
    OnDrawColumnCell(Self, aRect, aCol, ColumnFromGridColumn(aCol), aState)
  else
    DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TCustomDbGrid.EditButtonClicked(Sender: TObject);
begin
  if Assigned(OnEditButtonClick) then
    OnEditButtonClick(Self);
end;

function TCustomDbGrid.EditorCanAcceptKey(const ch: Char): boolean;
var
  aField: TField;
begin
  result := False;
  if FDataLink.Active then begin
    aField := SelectedField;
    if aField<>nil then begin
      Result := aField.IsValidChar(Ch);
    end;
  end;
end;

function TCustomDbGrid.EditorIsReadOnly: boolean;
begin
  Result := GetColumnReadOnly(Col);
  if not Result then begin
    Result := not FDataLink.Edit;
    EditingColumn(Col, not Result);
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
    {$IfDef dbgdbgrid}
    DebugLn(Name,'.UpdateActive: ActiveRecord=', dbgs(ActiveRecord),
            ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
    {$endif}
    Row:= FixedRows + ActiveRecord;
  end;
  //Invalidate;
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
    ColCount := Result + FCCount;
    if FDataLink.Active then begin
      UpdateBufferCount;
      RecCount := FDataLink.RecordCount + FRCount;
      if RecCount<2 then RecCount:=2;
      RowCount := RecCount;
    end else
      RowCount := 2;
    FixedRows := FRCount;
    FixedCols := FCCount;
    UpdateGridColumnSizes;
    EndVisualChange;
  end;
end;

procedure TCustomDbGrid.UpdateVertScrollbar(const aVisible: boolean;
  const aRange, aPage: Integer);
begin
end;

procedure TCustomDbGrid.VisualChange;
begin
  if FVisualChangeCount=0 then begin
    inherited VisualChange;
  end;
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
  FDataLink.OnEditingChanged:=@OnEditingChanged;
  FDataLink.OnUpdateData:=@OnUpdateData;
  FDataLink.VisualControl:= True;

  FColumns := CreateColumns;

  FOptions := [dgColumnResize, dgTitles, dgIndicator, dgRowLines, dgColLines,
    dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing, dgAlwaysShowSelection];
    
  inherited Options :=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect,
     goSmoothScroll, goColMoving, goTabs, goEditing, goDrawFocusSelected,
     goColSizing ];

  
  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrolLBars:=ssBoth;
  DefaultTextStyle.Wordbreak := false;
  
  FTitleFont := TFont.Create;
  FTitleFont.OnChange := @OnTitleFontChanged;
  
  FButtonEditor := TCellButton.Create(nil);
  FButtonEditor.Visible:=False;
  FButtonEditor.Name:='EditButton';
  FButtonEditor.Caption:='...';
  FButtonEditor.OnClick := @EditButtonClicked;
  
  FStringEditor := TStringCellEditor.Create(nil);
  FStringEditor.Visible:=False;
  FStringEditor.name :='EditString';
  FStringEditor.Text:='';
  FStringEditor.Align:=alNone;


  //ClearGrid;
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
    Inc(Result.Top, 2);
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
      Canvas.TextRect(R,R.Left,R.Top,GetColumnTitle(DataCol));
    end;
  end else begin
    F := GetFieldFromGridColumn(DataCol);
    if F<>nil then begin
      S := F.DisplayText;
    end else
      S := '';
    R := FixRectangle();
    Canvas.TextRect(R,R.Left,R.Top,S);
    //Canvas.TextOut(aRect.Left+2,ARect.Top+2, S);
  end;
end;

destructor TCustomDbGrid.Destroy;
begin
  FStringEditor.Free;
  FButtonEditor.Free;
  FTitleFont.Free;
  FColumns.Free;
  FDataLink.OnDataSetChanged:=nil;
  FDataLink.OnRecordChanged:=nil;
  FDataLink.Free;
  inherited Destroy;
end;
{
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
}
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

procedure TDbGridColumns.TitleFontChanged;
var
  c: TColumn;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    c := Items[i];
    if (c<>nil)and(c.Title.IsDefaultFont) then
      c.Title.FillTitleDefaultFont;
  end;
end;

procedure TDbGridColumns.FontChanged;
var
  c: TColumn;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    c := Items[i];
    if (c<>nil)and(c.IsDefaultFont) then
      c.FillDefaultFont;
  end;
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

procedure TColumn.FontChanged(Sender: TObject);
begin
  FisDefaultFont := False;
  FieldChanged;
end;

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

function TColumn.GetExpanded: Boolean;
begin
  result := True;
end;

function TColumn.GetField: TField;
begin
  if (FFieldName<>'') and (FField<>nil) then
    LinkField;
  result := FField;
end;

function TColumn.GetFont: TFont;
begin
  result := FFont;
end;

function TColumn.GetGrid: TCustomDBGrid;
begin
  if Collection is TDbGridColumns then
    result := (Collection as TDbGridColumns).Grid
  else
    result := nil;
end;

function TColumn.GetLayout: TTextLayout;
begin
  if FLayout=nil then
    result := tlCenter
  else
    result := FLayout^;
end;

function TColumn.GetPickList: TStrings;
begin
  Result := FPickList;
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

function TColumn.IsFontStored: boolean;
begin
  result := not FisDefaultFont;
end;

function TColumn.IsLayoutStored: boolean;
begin
  result := FLayout <> nil;
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

procedure TColumn.SetButtonStyle(const AValue: TColumnButtonStyle);
begin
  if FButtonStyle=AValue then exit;
  FButtonStyle:=AValue;
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

procedure TColumn.SetExpanded(const AValue: Boolean);
begin
  // Todo
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

procedure TColumn.SetFont(const AValue: TFont);
begin
  if AValue.Handle<>FFont.Handle then begin
    FFont.Assign(AValue);
  end;
end;

procedure TColumn.SetLayout(const AValue: TTextLayout);
begin
  if FLayout = nil then
    New(FLayout)
  else if FLayout^ = AValue then
    exit;
  FLayout^ := AValue;
  FieldChanged;
end;

procedure TColumn.SetPickList(const AValue: TStrings);
begin
  if AValue=nil then
    FPickList.Clear
  else
    FPickList.Assign(AValue);
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
  FTitle.Assign(AValue);
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

procedure TColumn.FillDefaultFont;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid := Grid;
  if (theGrid<>nil) then begin
    FFont.Assign(TheGrid.Font);
  end;
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
    Result := inherited GetDisplayName
  else
    Result:=FFieldName;
end;

constructor TColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FTitle := CreateTitle;
  
  FIsDefaultFont := True;
  FFont := TFont.Create;
  FillDefaultFont;
  FFont.OnChange := @FontChanged;
  
  FPickList:= TStringList.Create;
  FButtonStyle := cbsAuto;
  FDropDownRows := 7;
end;

destructor TColumn.Destroy;
begin
  if FAlignment<>nil then Dispose(FAlignment);
  if FColor<>nil then Dispose(FColor);
  if FVisible<>nil then Dispose(FVisible);
  if FReadOnly<>nil then Dispose(FReadOnly);
  if FWidth<>nil then Dispose(FWidth);
  if FLayout<>nil then Dispose(FLayout);
  FFont.Free;
  FTitle.Free;
  inherited Destroy;
end;

function TColumn.IsDefault: boolean;
begin
  result := FTitle.IsDefault and (FAlignment=nil) and (FColor=nil)
    and (FVisible=nil) and (FReadOnly=nil) and (FWidth=nil) and FIsDefaultFont
    and (FLayout=nil);
end;

{ TColumnTitle }

procedure TColumnTitle.FontChanged(Sender: TObject);
begin
  FisDefaultTitleFont := False;
  FColumn.FieldChanged;
end;

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
    result := FCaption;
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

procedure TColumnTitle.FillTitleDefaultFont;
var
  TheGrid: TCustomDbGrid;
begin
  TheGrid :=  FColumn.Grid;
  if TheGrid<>nil then
    FFont.Assign( TheGrid.TitleFont )
  else
    FFont.Assign( FColumn.Font );
end;

function TColumnTitle.GetFont: TFont;
begin
  Result := FFont;
end;

function TColumnTitle.GetLayout: TTextLayout;
begin
  if FLayout = nil then
    result := tlCenter
  else
    result := FLayout^;
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

function TColumnTitle.IsFontStored: boolean;
begin
  result := FFont <> nil;
end;

function TColumnTitle.IsLayoutStored: boolean;
begin
  result := FLayout <> nil;
end;

procedure TColumnTitle.SetAlignment(const AValue: TAlignment);
begin
  if Falignment = nil then
    New(Falignment)
  else if FAlignment^ = AValue then
    exit;
  FAlignment^ := AValue;
  FColumn.FieldChanged;
end;

procedure TColumnTitle.SetCaption(const AValue: string);
begin
  if (FCaption=nil)or(CompareText(AValue, FCaption^)<>0) then begin
    if FCaption<>nil then
      StrDispose(FCaption);
      //DisposeStr(FCaption);
    FCaption := StrNew(PChar(AValue));
    //FCaption := NewStr(AValue);
    FColumn.FieldChanged;
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

procedure TColumnTitle.SetFont(const AValue: TFont);
begin
  if AValue.Handle<>FFont.Handle then begin
    FFont.Assign(AValue);
  end;
end;

procedure TColumnTitle.SetLayout(const AValue: TTextLayout);
begin
  if FLayout = nil then
    New(FLayout)
  else if FLayout^ = AValue then
    exit;
  FLayout^ := AValue;
  FColumn.FieldChanged;
end;

constructor TColumnTitle.Create(TheColumn: TColumn);
begin
  inherited Create;
  FColumn := TheColumn;
  
  FIsDefaultTitleFont := True;
  FFont := TFont.Create;
  FillTitleDefaultFont;
  FFont.OnChange := @FontChanged;
end;

destructor TColumnTitle.Destroy;
begin
  if FFont<>nil then FFont.Free;
  if FAlignment<>nil then Dispose(FAlignment);
  if FColor<>nil then Dispose(FColor);
  if FCaption<>nil then StrDispose(FCaption); //DisposeStr(FCaption);
  if FLayout<>nil then Dispose(FLayout);
  inherited Destroy;
end;

function TColumnTitle.IsDefault: boolean;
begin
  result :=  (FAlignment=nil) and (FColor=nil) and (FCaption=nil) and
    IsDefaultFont and (FLayout=nil);
end;


{ TCellButton }

procedure TCellButton.msg_SetGrid(var Msg: TGridMessage);
begin
  FGrid:=Msg.Grid;
  Msg.Options:=EO_HOOKKEYDOWN or EO_HOOKEXIT or EO_HOOKKEYPRESS or EO_HOOKKEYUP;
end;

procedure TCellButton.msg_SetPos(var Msg: TGridMessage);
begin
  with Msg.CellRect do begin
    if Right-Left>25 then Left:=Right-25;
    SetBounds(Left, Top, Right-Left, Bottom-Top);
  End;
end;

end.

{
  $Log$
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
