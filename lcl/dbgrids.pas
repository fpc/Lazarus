
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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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

TComponentDatalink idea was taken from Joanna Carter's article
"The Ultimate Datalink?" Delphi Magazine Issue #30 February 1998
}
unit DBGrids;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, FileUtil, DB,
  LCLStrConsts, LCLIntf, LCLProc, LCLType, LMessages, LResources,
  Controls, StdCtrls, Graphics, Grids, Dialogs, Themes, Variants;

type
  TCustomDbGrid = class;
  TColumn = class;
  EInvalidGridOperation = class(Exception);


  TDBGridOption = (
    dgEditing,                          // Ya
    dgTitles,                           // Ya
    dgIndicator,                        // Ya
    dgColumnResize,                     // Ya
    dgColumnMove,                       // Ya
    dgColLines,                         // Ya
    dgRowLines,                         // Ya
    dgTabs,                             // Ya
    dgAlwaysShowEditor,                 // Ya
    dgRowSelect,                        // Ya
    dgAlwaysShowSelection,              // Ya
    dgConfirmDelete,
    dgCancelOnExit,                     // Ya
    dgMultiselect,                      // Ya
    dgHeaderHotTracking,
    dgHeaderPushedLook,
    dgPersistentMultiSelect,
    dgAutoSizeColumns,
    dgAnyButtonCanSelect,               // any mouse button can move selection
    dgDisableDelete,                    // disable deleting records with Ctrl+Delete
    dgDisableInsert                     // disable inserting (or append) records
  );
  TDbGridOptions = set of TDbGridOption;

  TDbGridExtraOption = (
    dgeAutoColumns,       // if uncustomized columns, add them anyway?
    dgeCheckboxColumn     // enable the use of checkbox in columns
  );
  TDbGridExtraOptions = set of TDbGridExtraOption;

  TDbGridStatusItem = (gsUpdatingData, gsAddingAutoColumns,
                       gsRemovingAutoColumns, gsAutoSized, gsStartEditing);
  TDbGridStatus = set of TDbGridStatusItem;

  TDataSetScrolledEvent =
    procedure(DataSet: TDataSet; Distance: Integer) of object;

  TDBGridClickEvent =
    procedure(Column: TColumn) of object;

  TMovedEvent =
    procedure(Sender: TObject; FromIndex, ToIndex: Integer) of object;

  TDrawColumnCellEvent =
    procedure(Sender: TObject; const Rect: TRect; DataCol: Integer;
              Column: TColumn; State: TGridDrawState) of object;

  TGetDbEditMaskEvent =
    procedure (Sender: TObject; const Field: TField;
               var Value: string) of object;

  TDbGridSelEditorEvent =
    procedure(Sender: TObject; Column: TColumn;
              var Editor: TWinControl) of object;

  TPrepareDbGridCanvasEvent =
    procedure(sender: TObject; DataCol: Integer;
              Column: TColumn; AState: TGridDrawState) of object;

  TDbGridCheckBoxBitmapEvent =
    procedure(Sender: TObject; const CheckedState: TCheckboxState;
              var ABitmap: TBitmap) of object;

  TDbGridCheckboxStateEvent =
    procedure(Sender: TObject; Column: TColumn;
              var AState: TCheckboxState) of object;
type

  { TBMStringList }

  TBMStringList = class (TStringList)
  protected
    function DoCompareText(const s1,s2 : string) : PtrInt; override;
  end;

  { TBookmarkList }

  TBookmarkList=class
  private
    FList: TBMStringList;
    FGrid: TCustomDbGrid;
    function GetCount: integer;
    function GetCurrentRowSelected: boolean;
    function GetItem(AIndex: Integer): TBookmarkStr;
    procedure SetCurrentRowSelected(const AValue: boolean);
    procedure CheckActive;
  public
    constructor Create(AGrid: TCustomDbGrid);
    destructor Destroy; override;

    procedure Clear;
    procedure Delete;
    function  Find(const Item: TBookmarkStr; var AIndex: Integer): boolean;
    function  IndexOf(const Item: TBookmarkStr): Integer;
    function  Refresh: boolean;

    property Count: integer read GetCount;
    property CurrentRowSelected: boolean
      read GetCurrentRowSelected write SetCurrentRowSelected;
    property Items[AIndex: Integer]: TBookmarkStr read GetItem; default;
  end;

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
    property OnRecordChanged: TFieldNotifyEvent read FOnRecordChanged write FOnRecordChanged;
    property OnDataSetChanged: TDatasetNotifyEvent read FOnDatasetChanged write FOnDataSetChanged;
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
    property Fields[Index: Integer]: TField read GetFields;
    property VisualControl;
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
    FIsAutomaticColumn: boolean;
    FDesignIndex: Integer;
    procedure ApplyDisplayFormat;
    function  GetDataSet: TDataSet;
    function  GetDisplayFormat: string;
    function  GetField: TField;
    function  GetIsDesignColumn: boolean;
    function  IsDisplayFormatStored: boolean;
    procedure SetDisplayFormat(const AValue: string);
    procedure SetField(const AValue: TField);
    procedure SetFieldName(const AValue: String);
  protected
    function  CreateTitle: TGridColumnTitle; override;
    function  GetDefaultAlignment: TAlignment; override;
    function  GetDefaultDisplayFormat: string;
    function  GetDefaultValueChecked: string; override;
    function  GetDefaultValueUnchecked: string; override;
    function  GetDefaultVisible: boolean; override;
    function  GetDisplayName: string; override;
    function  GetDefaultReadOnly: boolean; override;
    function  GetDefaultWidth: Integer; override;
    function  GetPickList: TStrings; override;
    property  IsAutomaticColumn: boolean read FIsAutomaticColumn;
    property  IsDesignColumn: boolean read GetIsDesignColumn;
    procedure LinkField;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function  IsDefault: boolean; override;
    property  DesignIndex: integer read FDesignIndex;
    property  Field: TField read GetField write SetField;
  published
    property  FieldName: String read FFieldName write SetFieldName;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat
      stored IsDisplayFormatStored;
  end;

  TColumnOrder = (coDesignOrder, coFieldIndexOrder);

  { TDBGridColumns }
  TDBGridColumns = class(TGridColumns)
  private
    function GetColumn(Index: Integer): TColumn;
    procedure SetColumn(Index: Integer; Value: TColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
    function ColumnFromField(Field: TField): TColumn;
    function  HasAutomaticColumns: boolean;
    function  HasDesignColumns: boolean;
    procedure RemoveAutoColumns;
  public
    function  Add: TColumn;
    procedure LinkFields;
    procedure ResetColumnsOrder(ColumnOrder: TColumnOrder);
    property Items[Index: Integer]: TColumn read GetColumn write SetColumn; default;
  end;

  { TCustomDBGrid }

  TCustomDBGrid=class(TCustomGrid)
  private
    FDataLink: TComponentDataLink;
    FExtraOptions: TDBGridExtraOptions;
    FOnCellClick: TDBGridClickEvent;
    FOnColEnter,FOnColExit: TNotifyEvent;
    FOnColumnMoved: TMovedEvent;
    FOnColumnSized: TNotifyEvent;
    FOnDrawColumnCell: TDrawColumnCellEvent;
    FOnFieldEditMask: TGetDbEditMaskEvent;
    FOnTitleClick: TDBGridClickEvent;
    FOnSelectEditor: TDbGridSelEditorEvent;
    FOnCheckboxBitmap: TDbGridCheckBoxBitmapEvent;
    FOnCheckboxState: TDbGridCheckboxStateEvent;
    FOptions: TDBGridOptions;
    FReadOnly: Boolean;
    FColEnterPending: Boolean;
    FLayoutChangedCount: integer;
    FTempText : string;
    FDrawingActiveRecord: Boolean;
    FDrawingMultiSelRecord: Boolean;
    FDrawingEmptyDataset: Boolean;
    FEditingColumn: Integer;
    FOldPosition: Integer;
    FDefaultColWidths: boolean;
    FGridStatus: TDBGridStatus;
    FOldControlStyle: TControlStyle;
    FSelectedRows: TBookmarkList;
    FOnPrepareCanvas: TPrepareDbGridCanvasEvent;
    FKeyBookmark: TBookmarkStr;
    FKeySign: Integer;
    procedure EmptyGrid;
    function GetColumns: TDBGridColumns;
    function GetCurrentColumn: TColumn;
    function GetCurrentField: TField;
    function GetDataSource: TDataSource;
    function GetRecordCount: Integer;
    function GetSelectedFieldRect: TRect;
    function GetSelectedIndex: Integer;
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
    procedure SetColumns(const AValue: TDBGridColumns);
    //procedure ReadColumns(Reader: TReader);
    //procedure SetColumns(const AValue: TDBGridColumns);
    procedure SetCurrentField(const AValue: TField);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetExtraOptions(const AValue: TDBGridExtraOptions);
    procedure SetOptions(const AValue: TDBGridOptions);
    procedure SetSelectedIndex(const AValue: Integer);
    procedure SetThumbTracking(const AValue: boolean);
    procedure UpdateBufferCount;

    // Temporal
    function GetColumnCount: Integer;

    function DefaultFieldColWidth(F: TField): Integer;

    procedure UpdateGridColumnSizes;
    procedure UpdateScrollbarRange;
    procedure DoLayoutChanged;
    //procedure WriteColumns(Writer: TWriter);

    procedure RestoreEditor;
    function  ISEOF: boolean;
    function  ValidDataSet: boolean;
    function  InsertCancelable: boolean;
    procedure StartUpdating;
    procedure EndUpdating;
    function  UpdatingData: boolean;
    procedure SwapCheckBox;
    procedure ToggleSelectedRow;
    procedure SelectRecord(AValue: boolean);
    procedure GetScrollbarParams(out aRange, aPage, aPos: Integer);
    procedure CMGetDataLink(var Message: TLMessage); message CM_GETDATALINK;
    procedure ClearSelection(selCurrent:boolean=false);
    function  NeedAutoSizeColumns: boolean;
    procedure RenewColWidths;
  protected
    procedure AddAutomaticColumns;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeforeMoveSelection(const DCol,DRow: Integer); override;
    procedure BeginLayout;
    procedure CellClick(const aCol,aRow: Integer; const Button:TMouseButton); override;
    procedure InvalidateSizes;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    function  ColumnEditorStyle(aCol: Integer; F: TField): TColumnButtonStyle;
    function  CreateColumns: TGridColumns; override;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DefaultDrawCell(aCol,aRow: Integer; aRect: TRect; aState: TGridDrawState);
    function  DefaultEditorStyle(const Style:TColumnButtonStyle; const F:TField): TColumnButtonStyle;
    procedure DoExit; override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoOnChangeBounds; override;
    procedure DoPrepareCanvas(aCol,aRow:Integer; aState: TGridDrawState); override;
    procedure DrawAllRows; override;
    procedure DrawFocusRect(aCol,aRow:Integer; ARect:TRect); override;
    procedure DrawRow(ARow: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DrawCheckboxBitmaps(aCol: Integer; aRect: TRect; F: TField);
    procedure DrawFixedText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
    procedure DrawIndicator(ACanvas: TCanvas; R: TRect; Opt: TDataSetState; MultiSel: boolean); virtual;
    procedure EditingColumn(aCol: Integer; Ok: boolean);
    procedure EditorCancelEditing;
    procedure EditorDoGetValue; override;
    function  EditorCanAcceptKey(const ch: TUTF8Char): boolean; override;
    function  EditorIsReadOnly: boolean; override;
    procedure EditorTextChanged(const aCol,aRow: Integer; const aText:string); override;
    procedure EndLayout;
    function  FieldIndexFromGridColumn(AGridCol: Integer): Integer;
    function  FirstGridColumn: Integer; override;
    function  GetBufferCount: integer;
    function  GetDefaultColumnAlignment(Column: Integer): TAlignment; override;
    function  GetDefaultColumnWidth(Column: Integer): Integer; override;
    function  GetDefaultColumnReadOnly(Column: Integer): boolean; override;
    function  GetDefaultColumnTitle(Column: Integer): string; override;
    function  GetDefaultRowHeight: integer; override;
    function  GetDsFieldFromGridColumn(Column: Integer): TField;
    function  GetEditMask(aCol, aRow: Longint): string; override;
    function  GetEditText(aCol, aRow: Longint): string; override;
    function  GetFieldFromGridColumn(Column: Integer): TField;
    function  GetGridColumnFromField(F: TField): Integer;
    function  GetImageForCheckBox(const aCol,aRow: Integer;
                                  CheckBoxView: TCheckBoxState): TBitmap; override;
    function  GetIsCellSelected(aCol, aRow: Integer): boolean; override;
    function  GetIsCellTitle(aCol,aRow: Integer): boolean; override;
    procedure GetSelectedState(AState: TGridDrawState; out IsSelected:boolean); override;
    function  GridCanModify: boolean;
    procedure GetSBVisibility(out HsbVisible,VsbVisible:boolean);override;
    procedure GetSBRanges(const HsbVisible,VsbVisible: boolean;
                  out HsbRange,VsbRange,HsbPage,VsbPage,HsbPos,VsbPos:Integer); override;
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure HeaderSized(IsColumn: Boolean; Index: Integer); override;
    function  IsValidChar(AField: TField; AChar: TUTF8Char): boolean;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure LinkActive(Value: Boolean); virtual;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure MoveSelection; override;
    function  MouseButtonAllowed(Button: TMouseButton): boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure RemoveAutomaticColumns;
    procedure ResetSizes; override;
    procedure SelectEditor; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure SetFixedCols(const AValue: Integer); override;
    function  SelectCell(aCol, aRow: Integer): boolean; override;
    procedure UpdateActive; virtual;
    procedure UpdateAutoSizeColumns;
    procedure UpdateData; virtual;
    function  UpdateGridCounts: Integer;
    procedure WMVScroll(var Message : TLMVScroll); message LM_VScroll;
    procedure WndProc(var TheMessage : TLMessage); override;

    property Columns: TDBGridColumns read GetColumns write SetColumns;
    property GridStatus: TDBGridStatus read FGridStatus write FGridStatus;
    property Datalink: TComponentDataLink read FDatalink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Options: TDBGridOptions read FOptions write SetOptions default
              [dgColumnResize, dgColumnMove, dgTitles, dgIndicator, dgRowLines,
               dgColLines, dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing,
               dgAlwaysShowSelection];
    property OptionsExtra: TDBGridExtraOptions read FExtraOptions
              write SetExtraOptions default [dgeAutoColumns, dgeCheckboxColumn];
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property SelectedRows: TBookmarkList read FSelectedRows;

    property OnCellClick: TDBGridClickEvent read FOnCellClick write FOnCellClick;
    property OnColEnter: TNotifyEvent read FOnColEnter write FOnColEnter;
    property OnColExit: TNotifyEvent read FOnColExit write FOnColExit;
    property OnColumnMoved: TMovedEvent read FOnColumnMoved write FOnColumnMoved;
    property OnColumnSized: TNotifyEvent read FOnColumnSized write FOnColumnSized;
    property OnDrawColumnCell: TDrawColumnCellEvent read FOnDrawColumnCell write FOnDrawColumnCell;
    property OnFieldEditMask: TGetDbEditMaskEvent read FOnFieldEditMask write FOnFieldEditMask;
    property OnPrepareCanvas: TPrepareDbGridCanvasEvent read FOnPrepareCanvas write FOnPrepareCanvas;
    property OnSelectEditor: TDbGridSelEditorEvent read FOnSelectEditor write FOnSelectEditor;
    property OnTitleClick: TDBGridClickEvent read FOnTitleClick write FOnTitleClick;
    property OnUserCheckboxBitmap: TDbGridCheckboxBitmapEvent read FOnCheckboxBitmap write FOnCheckboxBitmap;
    property OnUserCheckboxState: TDbGridCheckboxStateEvent read FOnCheckboxState write FOnCheckboxState;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AutoSizeColumns;
    procedure InitiateAction; override;
    procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    function  EditorByStyle(Style: TColumnButtonStyle): TWinControl; override;
    procedure ResetColWidths;
    destructor Destroy; override;
    property SelectedField: TField read GetCurrentField write SetCurrentField;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property SelectedColumn: TColumn read GetCurrentColumn;
    property SelectedFieldRect: TRect read GetSelectedFieldRect;
    property ThumbTracking: boolean read GetThumbTracking write SetThumbTracking;
  end;

  TDBGrid=class(TCustomDBGrid)
  public
    property BorderColor;
    property Canvas;
    property DefaultTextStyle;
    property EditorBorderStyle;
    property EditorMode;
    property ExtendedColSizing;
    property FastEditing;
    property FocusColor;
    property FocusRectVisible;
    property GridLineColor;
    property GridLineStyle;
    property SelectedColor;
    property SelectedRows;
  published
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance default aaRightDown;
    property AutoEdit;
    property AutoFillColumns;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Columns; // stored false;
    property Constraints;
    property DataSource;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    //property DragKind;
    property DragMode;
    property Enabled;
    property FixedColor;
    property FixedCols;
    property FixedHotColor;
    property Flat;
    property Font;
    property HeaderHotZones;
    property HeaderPushZones;
    //property ImeMode;
    //property ImeName;
    property Options;
    property OptionsExtra;
    property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    //property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Scrollbars default ssBoth;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property OnCellClick;
    property OnColEnter;
    property OnColExit;
    property OnColumnMoved;
    property OnColumnSized;
    property OnContextPopup;
    property OnDrawColumnCell;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditButtonClick;
    property OnEditingDone;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFieldEditMask;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPrepareCanvas;
    property OnSelectEditor;
    //property OnStartDock;
    property OnStartDrag;
    property OnTitleClick;
    property OnUserCheckboxBitmap;
    property OnUserCheckboxState;
    property OnUTF8KeyPress;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls',[TDBGrid]);
end;

function CalcCanvasCharWidth(Canvas:TCanvas): integer;
begin
  if Canvas.HandleAllocated then
    result := Canvas.TextWidth('MX') div 2
  else
    result := 8;
end;

function CalcColumnFieldWidth(Canvas: TCanvas; hasTitle: boolean;
  aTitle: String; aTitleFont: TFont; Field: TField): Integer;
var
  aCharWidth: Integer;
  aFont: TFont;
  UseTitleFont: boolean;
begin
  if (Field=nil) or (Field.DisplayWidth=0) then
    Result := DEFCOLWIDTH
  else begin

    aCharWidth := CalcCanvasCharWidth(Canvas);
    if Field.DisplayWidth>Length(aTitle) then
      result := aCharWidth * Field.DisplayWidth
    else
      result := aCharWidth * Length(aTitle);

    if HasTitle then begin
      UseTitleFont :=
        (Canvas.Font.Size<>aTitleFont.Size) or
        (Canvas.Font.Style<>aTitleFont.Style) or
        (Canvas.Font.CharSet<>aTitleFont.CharSet) or
        (Canvas.Font.Name<>aTitleFont.Name);
      if UseTitleFont then begin
        aFont := TFont.Create;
        aFont.Assign(Canvas.Font);
        Canvas.Font := aTitleFont;
      end;
      try
        aCharWidth := Canvas.TextWidth(ATitle)+6;
        if aCharWidth>Result then
          Result := aCharWidth;
      finally
        if UseTitleFont then begin
          Canvas.Font := aFont;
          aFont.Free;
        end;
      end;
    end; // if HasTitle ...

  end; // if (Field=nil) or (Field.DisplayWidth=0) ...
end;

var
  LookupTmpSetActive: Boolean;
  LookupBookMark: TBookmark;

procedure LookupGetBookMark(ALookupField: TField);
begin
  LookupTmpSetActive := not ALookupField.LookupDataSet.Active;
  if LookupTmpSetActive then
    ALookupField.LookupDataSet.Active := True
  else
  begin
    LookupBookMark := ALookupField.LookupDataSet.GetBookmark;
    ALookupField.LookupDataSet.DisableControls;
  end;
end;

procedure LookupGotoBookMark(ALookupField: TField);
begin
  if LookupTmpSetActive then
  begin
    ALookupField.LookupDataSet.Active := False;
    LookupTmpSetActive := False;
  end
  else
  try
    ALookupField.LookupDataSet.GotoBookmark(LookupBookMark);
    ALookupField.LookupDataSet.FreeBookmark(LookupBookMark);
  finally
    ALookupField.LookupDataSet.EnableControls;
  end;
end;

{ TCustomDBGrid }

procedure TCustomDBGrid.OnRecordChanged(Field: TField);
var
  c: Integer;
begin
  {$IfDef dbgDBGrid}
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

function TCustomDBGrid.GetDataSource: TDataSource;
begin
  Result:= FDataLink.DataSource;
end;

function TCustomDBGrid.GetRecordCount: Integer;
begin
  result := FDataLink.DataSet.RecordCount;
end;

function TCustomDBGrid.GetSelectedFieldRect: TRect;
begin
  result := CellRect(Col,Row);
end;

function TCustomDBGrid.GetSelectedIndex: Integer;
begin
  if Columns.Enabled then
    Result := ColumnIndexFromGridColumn( Col )
  else
    Result := FieldIndexFromGridColumn( Col );
end;

function TCustomDBGrid.GetThumbTracking: boolean;
begin
  Result :=  goThumbTracking in inherited Options;
end;

procedure TCustomDBGrid.EmptyGrid;
var
  OldFixedCols, OldFixedRows: Integer;
begin
  OldFixedCols := FixedCols;
  OldFixedRows := FixedRows;
  Clear;
  RowCount := OldFixedRows + 1;
  ColCount := OldFixedCols + 1;
  if dgIndicator in Options then
    ColWidths[0]:=12;
end;

function TCustomDBGrid.GetColumns: TDBGridColumns;
begin
  result := TDBGridColumns( inherited Columns );
end;

procedure TCustomDBGrid.InvalidateSizes;
begin
  GridFlags := GridFlags + [gfVisualChange];
end;

function TCustomDBGrid.GetCurrentColumn: TColumn;
begin
  if Columns.Enabled then
    Result := TColumn(Columns[SelectedIndex])
  else
    Result := nil;
end;

function TCustomDBGrid.GetCurrentField: TField;
begin
  result := GetFieldFromGridColumn( Col );
end;

procedure TCustomDBGrid.OnDataSetChanged(aDataSet: TDataSet);
begin
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnDataSetChanged(aDataSet=%s) INIT',[name,dbgsname(ADataset)]);
  {$endif}
  if not (gsStartEditing in FGridStatus) then begin
    if EditorMode then
      EditorMode := False;
    LayoutChanged;
  end;
  UpdateActive;
  if not (gsStartEditing in FGridStatus) then begin
    SelectEditor;
    if (dgAlwaysShowEditor in Options) and not EditorMode then
      EditorMode := true;
  end;
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnDataSetChanged(aDataSet=%s) DONE',[name,dbgsname(ADataset)]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetOpen(aDataSet: TDataSet);
begin
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnDataSetOpen INIT',[name]);
  {$endif}
  RenewColWidths;
  LinkActive(True);
  UpdateActive;
  SelectEditor;
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnDataSetOpen DONE',[name]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetClose(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnDataSetClose');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnEditingChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnEditingChanged');
  if aDataSet<>nil then begin
    DebugLn(['Editing=', dsEdit = aDataSet.State]);
    DebugLn(['Inserting=',dsInsert = aDataSet.State]);
  end else
    DebugLn('Dataset=nil');
  {$endif}
  FDataLink.Modified := False;
  UpdateActive;
end;

procedure TCustomDBGrid.OnInvalidDataSet(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSet');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnInvalidDataSource(aDataSet: TDataset);
begin
  {$ifdef dbgDBGrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnInvalidDataSource');
  {$endif}
  LinkActive(False);
end;

procedure TCustomDBGrid.OnLayoutChanged(aDataSet: TDataSet);
begin
  {$ifdef dbgDBGrid}
  DebugLn('(',name,') ','TCustomDBGrid.OnLayoutChanged');
  {$endif}
  LayoutChanged;
end;

procedure TCustomDBGrid.OnNewDataSet(aDataSet: TDataset);
begin
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnNewDataset INIT',[name]);
  {$endif}
  RenewColWidths;
  LinkActive(True);
  UpdateActive;
  SelectEditor;
  {$Ifdef dbgDBGrid}
  DebugLn('(%s) TCustomDBDrid.OnNewDataset DONE',[name]);
  {$endif}
end;

procedure TCustomDBGrid.OnDataSetScrolled(aDataset: TDataSet; Distance: Integer);
var
  OldEditorMode: boolean;
begin
  {$ifdef dbgDBGrid}
  DebugLn(ClassName, ' (',name,')', '.OnDataSetScrolled(',IntToStr(Distance),')');
  Debugln('Dataset.RecordCount=',IntToStr(aDataSet.RecordCount));
  {$endif}
  UpdateScrollBarRange;
  // todo: Use a fast interface method to scroll a rectangular section of window
  //       if distance=+, Row[Distance] to Row[RowCount-2] UP
  //       if distance=-, Row[FixedRows+1] to Row[RowCount+Distance] DOWN

  OldEditorMode := EditorMode;
  if OldEditorMode then
    EditorMode := False;

  if Distance<>0 then begin
    Row:= FixedRows + FDataLink.ActiveRecord;
    Invalidate;
  end else
    UpdateActive;

  if OldEditorMode and (dgAlwaysShowEditor in Options) then
    EditorMode := True;
end;

procedure TCustomDBGrid.OnUpdateData(aDataSet: TDataSet);
begin
  UpdateData;
end;

procedure TCustomDBGrid.SetColumns(const AValue: TDBGridColumns);
begin
  inherited Columns := TGridColumns(AValue);
end;

{
procedure TCustomDBGrid.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;
procedure TCustomDBGrid.SetColumns(const AValue: TDBGridColumns);
begin
  Columns.Assign(AValue);
end;
}
procedure TCustomDBGrid.SetCurrentField(const AValue: TField);
var
  i: Integer;
begin
  if Avalue<>SelectedField then begin
    i := GetGridColumnFromField( AValue );
    if (i>=FirstGridColumn) and (i>=FixedCols) then
      Col := i;
  end;
end;

procedure TCustomDBGrid.SetDataSource(const AValue: TDataSource);
begin
  if AValue = FDatalink.Datasource then Exit;
  RenewColWidths;
  FDataLink.DataSource := AValue;
  UpdateActive;
end;

procedure TCustomDBGrid.SetExtraOptions(const AValue: TDBGridExtraOptions);
var
  OldOptions: TDBGridExtraOptions;

  function IsOptionChanged(Op: TDBGridExtraOption): boolean;
  begin
    result := ((op in OldOptions) and not (op in AValue)) or
      (not (op in OldOptions) and (op in AValue));
  end;

begin
  if FExtraOptions=AValue then exit;
  OldOptions := FExtraOptions;
  FExtraOptions := AValue;

  if IsOptionChanged(dgeCheckboxColumn) then
    Invalidate;

  if IsOptionChanged(dgeAutoColumns) then begin
    if dgeAutoColumns in aValue then
      AddAutomaticColumns
    else if TDBGridColumns(Columns).HasAutomaticColumns then
      RemoveAutomaticColumns;
    UpdateActive;
  end;

end;

procedure TCustomDBGrid.SetOptions(const AValue: TDBGridOptions);
var
  OldOptions: TGridOptions;
  ChangedOptions: TDbGridOptions;
  MultiSel: boolean;
begin
  if FOptions<>AValue then begin
    MultiSel := dgMultiSelect in FOptions;
    ChangedOptions := (FOptions-AValue) + (AValue-FOptions);
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

    if dgColumnMove in fOptions then
      Include(OldOptions, goColMoving)
    else
      Exclude(OldOptions, goColMoving);

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

    if dgHeaderHotTracking in FOptions then
      Include(OldOptions, goHeaderHotTracking)
    else
      Exclude(OldOptions, goHeaderHotTracking);

    if dgHeaderPushedLook in FOptions then
      Include(OldOptions, goHeaderPushedLook)
    else
      Exclude(OldOptions, goHeaderPushedLook);

    if (dgIndicator in ChangedOptions) then begin
      if (dgIndicator in FOptions) then
        FixedCols := FixedCols + 1
      else
        FixedCols := FixedCols - 1;
    end;

    if (dgTitles in ChangedOptions) then begin
      if dgTitles in FOptions then
        FixedRows := FixedRows + 1
      else
        FixedRows := FixedRows - 1;
    end;

    if (dgAutoSizeColumns in ChangedOptions) then begin
      Exclude(FGridStatus, gsAutoSized);
    end;

    inherited Options := OldOptions;

    if MultiSel and not (dgMultiSelect in FOptions) then begin
      FSelectedRows.Clear;
      FKeyBookmark:='';
    end;

    EndLayout;
  end;
end;

procedure TCustomDBGrid.SetSelectedIndex(const AValue: Integer);
begin
  Col := FirstGridColumn + AValue;
end;

procedure TCustomDBGrid.SetThumbTracking(const AValue: boolean);
begin
  BeginUpdate;
  if Avalue then
    inherited Options := inherited Options + [goThumbTracking]
  else
    inherited Options := inherited Options - [goThumbTracking];
  EndUpdate(false);
end;

procedure TCustomDBGrid.UpdateBufferCount;
var
  BCount: Integer;
begin
  if FDataLink.Active then begin
    BCount := GetBufferCount;
    if BCount<1 then
      BCount := 1;
    FDataLink.BufferCount:= BCount;
    {$ifdef dbgDBGrid}
    DebugLn('%s (%s), FDatalink.BufferCount=%d',[ClassName,Name,FDataLink.BufferCount]);
    {$endif}
  end;
end;

procedure TCustomDBGrid.UpdateData;
var
  selField,edField: TField;
  LookupKeyValues: Variant;
begin
  // get Editor text and update field content
  if not UpdatingData and (FEditingColumn>-1) and FDatalink.Editing then begin
    SelField := SelectedField;
    edField := GetFieldFromGridColumn(FEditingColumn);

    if (edField<>nil) and (edField = SelField) then begin
      {$ifdef dbgDBGrid}
      DebugLn('---> UpdateData: Field[', edField.Fieldname, '(',edField.AsString,')]=', FTempText,' INIT');
      {$endif}

      StartUpdating;
      edField.Text := FTempText;
      if edField.Lookup then
      begin
        LookupKeyValues := Null;
        if edField.LookupCache then
          LookupKeyValues := edField.LookupList.FirstKeyByValue(FTempText)
        else
        begin
          LookupGetBookMark(edField);
          try
            if edField.LookupDataSet.Locate(edField.LookupResultField,
              VarArrayOf([FTempText]), []) then
                LookupKeyValues :=
                  edField.LookupDataSet.FieldValues[edField.LookupKeyFields];
          finally
            LookupGotoBookMark(edField);
          end;
        end;
        edField.DataSet.FieldValues[edField.KeyFields] := LookupKeyValues;
      end;
      EndUpdating;

      EditingColumn(FEditingColumn, False);
      {$ifdef dbgDBGrid}
      DebugLn('<--- UpdateData: Chk: Field:=',edField.ASString,' END');
      {$endif}
    end;

  end;
end;

{$ifdef dbgDBGrid}
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

procedure TCustomDBGrid.WMVScroll(var Message: TLMVScroll);
var
  IsSeq: boolean;
  aPos, aRange, aPage: Integer;
  DeltaRec: integer;

  function MaxPos: Integer;
  begin
    if IsSeq then
      result := GetRecordCount - 1
    else
      result := 4;
  end;

  procedure DsMoveBy(Delta: Integer);
  begin
    FDataLink.MoveBy(Delta);
    GetScrollbarParams(aRange, aPage, aPos);
  end;

  procedure DsGoto(BOF: boolean);
  begin
    if BOF then FDatalink.DataSet.First
    else        FDataLink.DataSet.Last;
    GetScrollbarParams(aRange, aPage, aPos);
  end;

begin
  if not FDatalink.Active then exit;

  {$ifdef dbgDBGrid}
  DebugLn('VSCROLL: Code=',SbCodeToStr(Message.ScrollCode),
          ' Position=', dbgs(Message.Pos),' OldPos=',Dbgs(FOldPosition));
  {$endif}

  IsSeq := FDatalink.DataSet.IsSequenced;
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
        if aPos=FOldPosition then
          exit;
        if aPos>=MaxPos then
          dsGoto(False)
        else if aPos<=0 then
          dsGoto(True)
        else if IsSeq then
          FDatalink.DataSet.RecNo := aPos + 1
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

  if EditorMode then
    RestoreEditor;
  {$ifdef dbgDBGrid}
  DebugLn('---- Diff=',dbgs(DeltaRec), ' FinalPos=',dbgs(aPos));
  {$endif}
end;

procedure TCustomDBGrid.WndProc(var TheMessage: TLMessage);
begin
  if (TheMessage.Msg=LM_SETFOCUS) and (gsUpdatingData in FGridStatus) then begin
    {$ifdef dbgGrid}DebugLn('DBGrid.LM_SETFOCUS while updating');{$endif}
    if EditorMode then begin
      LCLIntf.SetFocus(Editor.Handle);
      EditorSelectAll;
    end;
    exit;
  end;
  inherited WndProc(TheMessage);
end;


function TCustomDBGrid.DefaultFieldColWidth(F: TField): Integer;
begin
  if not HandleAllocated or (F=nil) then
    result:=DefaultColWidth
  else begin
    if F.DisplayWidth = 0 then
      if Canvas.HandleAllocated then
        result := Canvas.TextWidth( F.DisplayName ) + 3
      else
        Result := DefaultColWidth
    else
      result := F.DisplayWidth * CalcCanvasCharWidth(Canvas);
  end;
end;

function TCustomDBGrid.GetColumnCount: Integer;
var
  i: integer;
  F: TField;
begin
  result := 0;
  if Columns.Enabled then
    result := Columns.VisibleCount
  else
    if (dgeAutoColumns in OptionsExtra) and FDataLink.Active then
      for i:=0 to FDataLink.DataSet.FieldCount-1 do begin
        F:= FDataLink.DataSet.Fields[i];
        if (F<>nil) and F.Visible then
          Inc(Result);
      end;
end;

// Get the visible field (from dataset fields) that corresponds to given column
function TCustomDBGrid.GetDsFieldFromGridColumn(Column: Integer): TField;
var
  i: Integer;
begin
  i := FieldIndexFromGridColumn( Column );
  if i>=0 then
    Result := FDataLink.DataSet.Fields[i]
  else
    Result := nil;
end;

function TCustomDBGrid.FirstGridColumn: Integer;
begin
  if (dgIndicator in Options) then
    Result := 1
  else
    Result := 0;
end;

// obtain the field either from a Db column or directly from dataset fields
function TCustomDBGrid.GetFieldFromGridColumn(Column: Integer): TField;
var
  i: integer;
begin
  if Columns.Enabled then begin
    i := ColumnIndexFromGridColumn( Column );
    if i>=0 then
      result := TDBGridColumns(Columns)[i].FField
    else
      result := nil;
  end else
    result := GetDsFieldFromGridColumn(Column);
end;

// obtain the corresponding grid column for the given field
function TCustomDBGrid.GetGridColumnFromField(F: TField): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=FirstGridColumn to ColCount-1 do begin
    if GetFieldFromGridColumn(i) = F then begin
      result := i;
      break;
    end;
  end;
end;

function TCustomDBGrid.GetImageForCheckBox(const aCol, aRow: Integer;
  CheckBoxView: TCheckBoxState): TBitmap;
begin
  Result:=inherited GetImageForCheckBox(aCol, aRow, CheckBoxView);
  if Assigned(OnUserCheckboxBitmap) then
    OnUserCheckboxBitmap(Self, CheckBoxView, Result);
end;

// obtain the visible field index corresponding to the grid column index
function TCustomDBGrid.FieldIndexFromGridColumn(AGridCol: Integer): Integer;
var
  i: Integer;
  Column: TColumn;
begin
  result := -1;
  if not FDatalink.Active then
    exit;

  if Columns.Enabled then begin
    Column := TColumn(ColumnFromGridColumn(AGridCol));
    if (Column<>nil) and (Column.Field<>nil) and Column.Field.Visible then
      Result := FDatalink.Dataset.Fields.IndexOf(Column.Field)
  end else begin
    AGridCol := AGridCol - FirstGridColumn;
    i := 0;
    while (AGridCol>=0) and (i<FDatalink.DataSet.FieldCount) do begin
      if FDatalink.Fields[i].Visible then begin
        Dec(AGridCol);
        if AGridCol<0 then begin
          Result := i;
          break;
        end;
      end;
      inc(i);
    end;
  end;
end;

function TCustomDBGrid.GetBufferCount: integer;
begin
  Result := ClientHeight div DefaultRowHeight;
  if dgTitles in Options then
    Dec(Result, 1);
end;

procedure TCustomDBGrid.UpdateGridColumnSizes;
var
  i: Integer;
begin
  if FDefaultColWidths then begin
    if dgIndicator in Options then
      ColWidths[0]:=12;
    if NeedAutoSizeColumns then
      UpdateAutoSizeColumns
    else
    for i:=FirstGridColumn to ColCount-1 do
      ColWidths[i] := GetColumnWidth(i);
  end;
end;

procedure TCustomDBGrid.UpdateScrollbarRange;
var
  aRange, aPage, aPos: Integer;
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then exit;

  GetScrollBarParams(aRange, aPage, aPos);

  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);

  {TODO: try to move this out}
  {$ifdef WINDOWS}
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.ntrackPos := 0;
  {$else}
  ScrollInfo.fMask := SIF_ALL or SIF_UPDATEPOLICY;
  //ScrollInfo.ntrackPos := SB_POLICY_CONTINUOUS;
  ScrollInfo.ntrackPos := SB_POLICY_DISCONTINUOUS;
  {$endif}
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := aRange;
  ScrollInfo.nPos := Min(aPos,aRange-aPage);
  ScrollInfo.nPage := aPage;
  // the redraw argument of SetScrollInfo means under gtk
  // if the scrollbar is visible or not, in windows it
  // seems to mean if the scrollbar is redrawn or not
  // to reflect the scrollbar changes made
  SetScrollInfo(Handle, SB_VERT, ScrollInfo,
    (ScrollBars in [ssBoth, ssVertical]) or
    ((Scrollbars in [ssAutoVertical, ssAutoBoth]) and (aRange>aPAge))
  );
  FOldPosition := aPos;
  {$ifdef dbgDBGrid}
  DebugLn('UpdateScrollBarRange: Handle=',IntToStr(Handle),
   ' aRange=', IntToStr(aRange),
   ' aPage=', IntToStr(aPage), ' aPos=', IntToStr(aPos));
  {$endif}
end;

procedure TCustomDBGrid.doLayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  {$ifdef dbgDBGrid} DebugLn('doLayoutChanged INIT'); {$endif}
  BeginUpdate;
  if UpdateGridCounts=0 then
    EmptyGrid;
  EndUpdate;
  UpdateScrollbarRange;
  {$ifdef dbgDBGrid} DebugLn('doLayoutChanged FIN'); {$endif}
end;
{
procedure TCustomDBGrid.WriteColumns(Writer: TWriter);
begin
  if Columns.IsDefault then
    Writer.WriteCollection(nil)
  else
    Writer.WriteCollection(Columns);
end;
}
procedure TCustomDBGrid.RestoreEditor;
begin
  if EditorMode then begin
    EditorMode := False;
    EditorMode := True;
  end;
end;

function TCustomDBGrid.IsEOF: boolean;
begin
  with FDatalink do
    result :=
      Active and DataSet.EOF;
end;

function TCustomDBGrid.ValidDataSet: boolean;
begin
  result := FDatalink.Active And (FDatalink.DataSet<>nil)
end;

function TCustomDBGrid.InsertCancelable: boolean;
begin
  with FDatalink.DataSet do
  Result := (State=dsInsert) and not (Modified or FDataLink.FModified);
end;

procedure TCustomDBGrid.StartUpdating;
begin
  if not UpdatingData then begin
    {$ifdef dbgDBGrid} DebugLn('DBGrid.StartUpdating');{$endif}
    Include(FGridStatus, gsUpdatingData);
    FOldControlStyle := ControlStyle;
    ControlStyle := ControlStyle + [csActionClient];
    LockEditor;
  end
  else
    {$ifdef dbgDBGrid} DebugLn('WARNING: multiple call to StartUpdating');{$endif}
end;

procedure TCustomDBGrid.EndUpdating;
begin
  {$ifdef dbgDBGrid} DebugLn('DBGrid.EndUpdating');{$endif}
  Exclude(FGridStatus, gsUpdatingData);
  ControlStyle := FOldControlStyle;
  UnLockEditor;
  if csActionClient in ControlStyle then
    DebugLn('WARNING: still got csActionClient');
end;

function TCustomDBGrid.UpdatingData: boolean;
begin
  result := gsUpdatingData in FGridStatus;
end;

procedure TCustomDBGrid.AddAutomaticColumns;
var
  i: Integer;
  F: TField;
begin
  // add as many columns as there are fields in the dataset
  // do this only at runtime.
  if (csDesigning in ComponentState) or not FDatalink.Active or
    (gsRemovingAutoColumns in FGridStatus) or
    not (dgeAutoColumns in OptionsExtra)
  then
    exit;
  Include(FGridStatus, gsAddingAutoColumns);
  try
    for i:=0 to FDataLink.DataSet.FieldCount-1 do begin

      F:= FDataLink.DataSet.Fields[i];

      if TDBGridColumns(Columns).ColumnFromField(F) <> nil then
        // this field is already in the collection. This could only happen
        // if AddAutomaticColumns was called out of LayoutChanged.
        // to avoid duplicate columns skip this field.
        continue;

      if (F<>nil) then begin
        with TDBGridColumns(Columns).Add do begin
          FIsAutomaticColumn := True;
          Field := F;
          Visible := F.Visible;
        end;
      end;

    end;
    // honor the field.index
    TDBGridColumns(Columns).ResetColumnsOrder(coFieldIndexOrder);
  finally
    Exclude(FGridStatus, gsAddingAutoColumns);
  end;

end;

procedure TCustomDBGrid.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomDbGrid then begin
    // TODO
  end else
    inherited AssignTo(Dest);
end;

procedure TCustomDBGrid.UpdateAutoSizeColumns;
var
  ACol,ARow,w: Integer;
  DatalinkActive: boolean;
  CurActiveRecord: Integer;
  Field: TField;
  ColWidth: Integer;
  tmpCanvas: TCanvas;
  C: TGridColumn;
  s: string;
begin

  if gsAutoSized in GridStatus then
    exit;

  BeginLayout;

  DatalinkActive := FDatalink.Active;
  if DatalinkActive then
    CurActiveRecord := FDatalink.ActiveRecord;

  tmpCanvas := GetWorkingCanvas(Canvas);
  try
    for aCol:=FixedCols to ColCount-1 do begin

      Field := GetFieldFromGridColumn(ACol);
      C := ColumnFromGridColumn(ACol);

      if (C<>nil) and (C.Title<>nil) then begin
        tmpCanvas.Font := C.Title.Font;
        ColWidth := tmpCanvas.TextWidth(trim(C.Title.Caption));
        tmpCanvas.Font := C.Font;
      end else begin
        if (Field<>nil) then begin
          tmpCanvas.Font := TitleFont;
          ColWidth := tmpCanvas.TextWidth(Field.FieldName);
        end
        else
          ColWidth := 0;
        tmpCanvas.Font := Font;
      end;

      if (Field<>nil) and DatalinkActive then
        for ARow := FixedRows to RowCount-1 do begin

          FDatalink.ActiveRecord := ARow - FixedRows;

          if Field.dataType<>ftBlob then
            s := trim(Field.DisplayText)
          else
            s := '(blob)';
          w := tmpCanvas.TextWidth(s);
          if w>ColWidth then
            ColWidth := w;

        end;

      if ColWidth=0 then
        ColWidth := GetColumnWidth(ACol);

      ColWidths[ACol] := ColWidth + 15;
    end;
  finally
    if TmpCanvas<>Canvas then
      FreeWorkingCanvas(tmpCanvas);

    if DatalinkActive then
      FDatalink.ActiveRecord := CurActiveRecord;

    include(FGridStatus, gsAutoSized);

    EndLayout;
  end;

end;

procedure TCustomDBGrid.SwapCheckBox;
var
  SelField: TField;
  TempColumn: TColumn;
begin
  if not GridCanModify then
    exit;

  SelField := SelectedField;
  TempColumn := TColumn(ColumnFromGridColumn(Col));
  if (SelField<>nil) and (TempColumn<>nil) and not TempColumn.ReadOnly and
     FDatalink.Edit then
  begin
    if SelField.DataType=ftBoolean then
    	SelField.AsBoolean := not SelField.AsBoolean
    else
    begin
      if TempColumn.ValueChecked=SelField.AsString then
        SelField.AsString := TempColumn.ValueUnchecked
      else
        SelField.AsString := TempColumn.ValueChecked;
    end;
  end;
end;

procedure TCustomDBGrid.ToggleSelectedRow;
begin
  SelectRecord(not FSelectedRows.CurrentRowSelected);
end;

procedure TCustomDBGrid.LinkActive(Value: Boolean);
begin
  if not Value then
    RemoveAutomaticColumns;
  LayoutChanged;
end;

procedure TCustomDBGrid.LayoutChanged;
begin
  if csDestroying in ComponentState then
    exit;
  if FLayoutChangedCount=0 then begin
    BeginLayout;
    if Columns.Count>0 then
      TDBGridColumns(Columns).LinkFields
    else if not FDataLink.Active then
      FDataLink.BufferCount := 0
    else
      AddAutomaticColumns;
    EndLayout;
  end;
end;

procedure TCustomDBGrid.Loaded;
begin
  LayoutChanged;
  inherited Loaded;
end;

type
  TProtFields=class(TFields)
  end;

procedure TCustomDBGrid.ColRowMoved(IsColumn: Boolean; FromIndex,
  ToIndex: Integer);
var
  F: TField;
begin
  if IsColumn then begin
    if Columns.Enabled then
      inherited ColRowMoved(IsColumn, FromIndex, ToIndex)
    else if FDatalink.Active and (FDataLink.DataSet<>nil) then begin
      F := GetDsFieldFromGridColumn(FromIndex);
      if F<>nil then begin
        TProtFields(FDatalink.DataSet.Fields).SetFieldIndex( F, ToIndex - FirstGridColumn );
      end;
    end;
    if Assigned(OnColumnMoved) then
      OnColumnMoved(Self, FromIndex, ToIndex);
  end;
end;

function TCustomDBGrid.ColumnEditorStyle(aCol: Integer; F: TField): TColumnButtonStyle;
begin
  result := cbsAuto;
  if Columns.Enabled then
    result := ColumnFromGridColumn(aCol).ButtonStyle;

  result := DefaultEditorStyle(result, F);
end;

function TCustomDBGrid.CreateColumns: TGridColumns;
begin
  result := TDBGridColumns.Create(Self, TColumn);
end;

procedure TCustomDBGrid.CreateWnd;
begin
  inherited CreateWnd;
  LayoutChanged;
  if Scrollbars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical] then
    ScrollBarShow(SB_VERT, True);
end;

procedure TCustomDBGrid.DefineProperties(Filer: TFiler);
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

procedure TCustomDBGrid.DefaultDrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

var
  S: string;
  F: TField;
  cbs: TColumnButtonStyle;
begin

  // background
  if (gdFixed in aState) and (TitleStyle=tsNative) then
    DrawThemedCell(aCol, aRow, aRect, aState)
  else
    Canvas.FillRect(aRect);

  if gdFixed in aState then
    DrawFixedText(aCol, aRow, aRect, aState)
  else
  if not FDrawingEmptyDataset then begin

    F := GetFieldFromGridColumn(aCol);
    cbs := ColumnEditorStyle(aCol, F);
    case cbs of
      cbsCheckBoxColumn:
        DrawCheckBoxBitmaps(aCol, aRect, F);
      else
      begin

        if cbs=cbsButtonColumn then
          DrawButtonCell(aCol, aRow, aRect, aState);

        {$ifdef dbggridpaint}
        DbgOut(' Col=%d',[ACol]);
        {$endif}
        if F<>nil then begin
          {$ifdef dbgGridPaint}
          DbgOut(' Field=%s',[F.FieldName]);
          {$endif}
          if F.dataType <> ftBlob then
            S := F.DisplayText
          else
            S := '(blob)';
        end else
          S := '';
        {$ifdef dbggridpaint}
        DbgOut(' Value=%s ',[S]);
        {$endif}
        DrawCellText(aCol,aRow,aRect,aState,S);
      end;
    end;
  end;
end;

function TCustomDBGrid.DefaultEditorStyle(const Style: TColumnButtonStyle;
  const F: TField): TColumnButtonStyle;
begin
  result := Style;
  if (Result=cbsAuto) and (F<>nil) then
    case F.DataType of
      ftBoolean: Result := cbsCheckboxColumn;
    end;
  if (result = cbsCheckBoxColumn) and not (dgeCheckboxColumn in FExtraOptions) then
    Result := cbsAuto;
end;


procedure TCustomDBGrid.DoOnChangeBounds;
begin
  BeginUpdate;
  inherited DoOnChangeBounds;
  if HandleAllocated then
    LayoutChanged;
  EndUpdate;
end;

procedure TCustomDBGrid.DoPrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
var
  DataCol: Integer;
begin
  if Assigned(OnPrepareCanvas) and (ARow>=FixedRows) then begin
    DataCol := ColumnIndexFromGridColumn(aCol);
    if DataCol>=0 then
      OnPrepareCanvas(Self, DataCol, TColumn(Columns[DataCol]), aState);
  end;
end;

procedure TCustomDBGrid.BeforeMoveSelection(const DCol,DRow: Integer);
begin
  {$ifdef dbgDBGrid}DebugLn('DBGrid.BefMovSel INIT');{$endif}
  inherited BeforeMoveSelection(DCol, DRow);
  if DCol<>Col then begin
    if assigned(OnColExit) then
      OnColExit(Self);
    FColEnterPending:=True;
  end;
{$ifdef dbgDBGrid}DebugLn('DBGrid.BefMovSel END');{$endif}
end;

procedure TCustomDBGrid.HeaderClick(IsColumn: Boolean; index: Integer);
var
  Column: TColumn;
begin
  if Assigned(OnTitleClick) and IsColumn then begin
    Column := TColumn(ColumnFromGridColumn(Index));
    if Column<>nil then
      OnTitleClick(Column);
  end;
end;

procedure TCustomDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
type
  TOperation=(opMoveBy,opCancel,opAppend,opInsert,opDelete);
var
  DeltaCol,DeltaRow: Integer;

  procedure DoOnKeyDown;
  begin
    {$ifdef dbgGrid}DebugLn('DoOnKeyDown INIT');{$endif}
    if Assigned(OnKeyDown) then
      OnKeyDown(Self, Key, Shift);
    {$ifdef dbgGrid}DebugLn('DoOnKeyDown FIN');{$endif}
  end;
  {$ifdef dbgGrid}
  function OperToStr(AOper: TOperation): string;
  begin
    case AOper of
      opMoveBy: result := 'opMoveBy';
      opCancel: result := 'opCancel';
      opAppend: result := 'opAppend';
      opInsert: result := 'opInsert';
      opDelete: result := 'opDelete';
    end;
  end;
  {$endif}
  procedure DoOperation(AOper: TOperation; Arg: Integer = 0);
  begin
    {$IfDef dbgGrid}DebugLn('KeyDown.DoOperation(%s,%d) INIT',[OperToStr(AOper),arg]);{$Endif}
    GridFlags := GridFlags + [gfEditingDone];
    case AOper of
      opMoveBy:
        FDatalink.MoveBy(Arg);
      opCancel:
        begin
          if EditorMode then
            EditorCancelEditing;
          FDatalink.Dataset.Cancel;
        end;
      opAppend:
        FDatalink.Dataset.Append;
      opInsert:
        FDatalink.Dataset.Insert;
      opDelete:
        FDatalink.Dataset.Delete;
    end;
    GridFlags := GridFlags - [gfEditingDone];
    {$IfDef dbgGrid}DebugLn('KeyDown.DoOperation(%s,%d) DONE',[OperToStr(AOper),arg]);{$Endif}
  end;
  procedure SelectNext(const AStart,ADown:Boolean);
  var
    N: Integer;
    CurBookmark: TBookmarkStr;
  begin
    if dgPersistentMultiSelect in Options then
      exit;

    if (ssShift in Shift) then begin

      CurBookmark := FDatalink.DataSet.Bookmark;
      if FKeyBookmark='' then
        FKeyBookmark:=CurBookmark;

      if (FKeyBookmark=CurBookmark) then begin
        if AStart then begin
          SelectRecord(true);
          if ADown then
            FKeySign := 1
          else
            FKeySign := -1;
          exit;
        end;
        FKeySign := 0;
      end;

      n := 4*Ord(FKeySign>=0) + 2*Ord(ADown) + 1*Ord(AStart);
      case n of
        0,6,8..11:
          begin
            SelectRecord(True);
          end;
        3,5:
          begin
            SelectRecord(False);
          end;
      end;
    end else
      ClearSelection(true);
  end;
  function doVKDown: boolean;
  begin
    {$ifdef dbgGrid}DebugLn('DoVKDown INIT');{$endif}
    if InsertCancelable then
    begin
      if IsEOF then
        result:=true
      else begin
        doOperation(opCancel);
        result := false;
      end;
    end else begin
      result:=false;
      SelectNext(true,true);
      doOperation(opMoveBy, 1);
      if GridCanModify and FDataLink.EOF then begin
        if not (dgDisableInsert in Options) then
          doOperation(opAppend);
      end else
        SelectNext(false,true);
    end;
    {$ifdef dbgGrid}DebugLn('DoVKDown FIN');{$endif}
  end;
  function DoVKUP: boolean;
  begin
    {$ifdef dbgGrid}DebugLn('DoVKUP INIT');{$endif}
    if InsertCancelable then
      doOperation(opCancel)
    else begin
      SelectNext(true, false);
      doOperation(opMoveBy, -1);
      SelectNext(false, false);
    end;
    result := FDatalink.DataSet.BOF;
    {$ifdef dbgGrid}DebugLn('DoVKUP FIN');{$endif}
  end;
  procedure MoveSel(AReset: boolean);
  begin
    if (DeltaCol<>0) or (DeltaRow<>0) then begin
      if DeltaRow > 0 then begin
        if doVKDown then
          //DeltaCol:=0; // tochk: strict? already in EOF, don't change column
      end else
      if DeltaRow < 0 then begin
        if doVKUP then
          //DeltaCol:=0; // tochk: strict? already in BOF, don't change column
      end;
      GridFlags := GridFlags + [gfEditingDone];
      if (DeltaCol<>0) then
        Col := Col + DeltaCol;
      GridFlags := GridFlags - [gfEditingDone];
    end else
    if AReset then
      ResetEditor;
  end;
begin
  {$IfDef dbgGrid}DebugLn('DBGrid.KeyDown %s INIT Key=%d',[Name,Key]);{$Endif}
  case Key of

    VK_TAB:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if dgTabs in Options then begin

            if ((ssShift in shift) and
               (Col<=GetFirstVisibleColumn) and (Row<=GetFirstVisibleRow)) then begin
              if EditorKey then
                GridFlags := GridFlags + [gfRevEditorTab];
              exit;
            end;

            GetDeltaMoveNext(ssShift in Shift, DeltaCol, DeltaRow);

            if (not (ssShift in Shift)) and (Row>=GetLastVisibleRow) and
               (DeltaRow>0) and (Col=GetLastVisibleColumn) and
               (FDatalink.Editing or not GridCanModify) then begin
              exit;
            end;

            MoveSel(false);
            Key := 0;
          end;
        end;
      end;

    VK_RETURN:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          key:=0;
          if (dgEditing in Options) and not EditorMode then
            EditorMode:=true
          else begin
            GetDeltaMoveNext(ssShift in Shift, DeltaCol, DeltaRow);
            MoveSel(True);
          end;
        end;
      end;

    VK_DELETE:
      begin
        doOnKeyDown;
        if (Key<>0) and (ssCtrl in Shift) and GridCanModify and
           (not (dgDisableDelete in Options)) and
           not FDataLink.DataSet.IsEmpty then begin

          if not (dgConfirmDelete in Options) or
            (MessageDlg(rsDeleteRecord, mtConfirmation, mbOKCancel, 0 )<>mrCancel)
          then begin
            doOperation(opDelete);
            key := 0;
          end;

        end;
      end;

    VK_DOWN:
      if ValidDataSet then begin
        DoOnKeyDown;
        if Key<>0 then begin
          doVKDown;
          Key := 0;
        end;
      end;

    VK_UP:
      if ValidDataSet then begin
        doOnKeyDown;
        if Key<>0 then begin
          doVKUp;
          key := 0;
         end;
      end;

    VK_NEXT:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          doOperation(opMoveBy, VisibleRowCount);
          ClearSelection(true);
          Key := 0;
        end;
      end;

    VK_PRIOR:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          doOperation(opMoveBy, -VisibleRowCount);
          ClearSelection(true);
          key := 0;
        end;
      end;

    VK_ESCAPE:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if EditorMode then begin
            EditorCancelEditing;
            if FDatalink.Active and not FDatalink.Dataset.Modified then
              FDatalink.Modified := False;
            Key := 0;
          end else
            if FDataLink.Active then
              doOperation(opCancel);
        end;
      end;

    VK_INSERT:
      begin
        doOnKeyDown;
        if Key<>0 then
          if not (dgDisableInsert in Options) and GridCanModify then begin
            doOperation(opInsert);
            Key:=0;
          end;
      end;

    VK_HOME:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if FDatalink.Active then begin
            GridFlags := GridFlags + [gfEditingDone];
            if ssCTRL in Shift then
              FDataLink.DataSet.First
            else
              MoveNextSelectable(False, FixedCols, Row);
            GridFlags := GridFlags - [gfEditingDone];
            ClearSelection(true);
            Key:=0;
          end;
        end;
      end;

    VK_END:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if FDatalink.Active then begin
            GridFlags := GridFlags + [gfEditingDone];
            if ssCTRL in shift then
              FDatalink.DataSet.Last
            else
              MoveNextSelectable(False, ColCount-1, Row);
            GridFlags := GridFlags - [gfEditingDone];
            ClearSelection(true);
            Key:=0;
          end;
        end;
      end;

    VK_SPACE:
      begin
        doOnKeyDown;
        if Key<>0 then begin
          if ColumnEditorStyle(Col, SelectedField) = cbsCheckboxColumn then begin
        		SwapCheckBox;
            Key:=0;
          end;
        end;
      end;

    VK_MULTIPLY:
      begin
        if ssCtrl in Shift then
          ToggleSelectedRow;
      end;

    else
      inherited KeyDown(Key, Shift);
  end;
  {$IfDef dbgGrid}DebugLn('DBGrid.KeyDown END Key= %d',[Key]);{$Endif}
end;

procedure TCustomDBGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
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
    {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown MoveBy INIT'); {$Endif}
    FDatalink.MoveBy(P.Y - Row);
    {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown MoveBy END'); {$Endif}
  end;
  procedure doMoveToColumn;
  begin
    {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown MoveToCol INIT Col=', IntToStr(P.X)); {$Endif}
    Col := P.X;
    {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown MoveToCol END'); {$Endif}
  end;
  procedure DoCancel;
  begin
    {$IfDef dbgGrid}DebugLn('DBGrid.MouseDown Dataset.CANCEL INIT');{$Endif}
    if EditorMode then
      EditorCancelEditing;
    FDatalink.Dataset.cancel;
    {$IfDef dbgGrid}DebugLn('DBGrid.MouseDown Dataset.CANCEL FIN');{$Endif}
  end;
  procedure DoAcceptValue;
  begin
    if EditorMode and FDatalink.FModified then
      EditorMode := False;
  end;
begin
  {$ifdef dbgDBGrid}DebugLn('DBGrid.mousedown - INIT');{$endif}
  if (csDesigning in componentState) {or not GCache.ValidGrid }then
    exit;

  if UpdatingData then begin
    {$ifdef dbgDBGrid}DebugLn('DBGrid.MouseDown - UpdatingData');{$endif}
    exit;
  end;

  if not MouseButtonAllowed(Button) then begin
    doInherited;
    exit;
  end;

  {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown INIT'); {$Endif}
  Gz:=MouseToGridZone(X,Y);
  CacheMouseDown(X,Y);
  case Gz of
    gzFixedCells, gzFixedCols:
      doInherited;
    else
      begin

        FKeyBookmark:=''; // force new keyboard selection start
        SetFocus;

        P:=MouseToCell(Point(X,Y));
        if Gz=gzFixedRows then
          P.X := Col;

        if P.Y=Row then begin
          //doAcceptValue;

          if ssCtrl in Shift then begin
            doMouseDown;
            ToggleSelectedRow;
          end
          else begin
            if Button=mbLeft then
              ClearSelection(true);
            if gz=gzFixedRows then
              doMouseDown
            else
              doInherited;
          end;

        end else begin
          doMouseDown;
          if ValidDataSet then begin
            if InsertCancelable and IsEOF then
              doCancel;
            doMoveBy;
          end;
          if ssCtrl in Shift then
            ToggleSelectedRow
          else begin
            if Button=mbLeft then
              ClearSelection(true);
            doMoveToColumn;
          end;
        end;
      end;
  end;
  {$IfDef dbgGrid} DebugLn('DBGrid.MouseDown END'); {$Endif}
end;

procedure TCustomDBGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if fGridState=gsSelecting then
    exit
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TCustomDBGrid.PrepareCanvas(aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);
  if (not FDatalink.Active) and ((gdSelected in aState) or (gdFocused in aState)) then
    Canvas.Brush.Color := Self.Color;
end;

procedure TCustomDBGrid.RemoveAutomaticColumns;
begin
  if not (csDesigning in ComponentState) then
    TDBGridColumns(Columns).RemoveAutoColumns;
end;

procedure TCustomDBGrid.ResetSizes;
begin
  LayoutChanged;
  inherited ResetSizes;
end;

procedure TCustomDBGrid.SelectEditor;
var
  aEditor: TWinControl;
begin
  {$ifdef dbgDBGrid}
  DebugLn('TCustomDBGrid.SelectEditor INIT Editor=',dbgsname(editor));
  {$endif}
  if (FDatalink<>nil) and FDatalink.Active then begin
    inherited SelectEditor;

    if (Editor is TCustomEdit) and (SelectedField is TStringField) then
      TCustomEdit(Editor).MaxLength := SelectedField.Size;

    if Assigned(OnSelectEditor) then begin
      aEditor:=Editor;
      OnSelectEditor(Self, SelectedColumn, aEditor);
      Editor:=aEditor;
    end;
  end else
    Editor := nil;
  {$ifdef dbgDBGrid}
  DebugLn('TCustomDBGrid.SelectEditor DONE Editor=',dbgsname(editor));
  {$endif}
end;

procedure TCustomDBGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  FTempText := Value;
end;

procedure TCustomDBGrid.SetFixedCols(const AValue: Integer);
begin
  if (FixedCols=AValue) or (AValue<FirstGridColumn) then
    exit;
  inherited SetFixedCols(AValue);
end;

function TCustomDBGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:= (ColWidths[aCol] > 0) and (RowHeights[aRow] > 0);
end;

procedure TCustomDBGrid.BeginLayout;
begin
  inc(FLayoutChangedCount);
end;

procedure TCustomDBGrid.EditingColumn(aCol: Integer; Ok: Boolean);
begin
  {$ifdef dbgDBGrid} DebugLn(['DBGrid.EditingColumn INIT aCol=', aCol, ' Ok=', ok]); {$endif}
  if Ok then begin
    FEditingColumn := aCol;
    FDatalink.Modified := True;
  end
  else
    FEditingColumn := -1;
  {$ifdef dbgDBGrid} DebugLn('DBGrid.EditingColumn END'); {$endif}
end;

procedure TCustomDBGrid.EditorCancelEditing;
begin
  EditingColumn(FEditingColumn, False); // prevents updating the value
  if EditorMode then begin
    EditorMode := False;
    if dgAlwaysShowEditor in Options then
      EditorMode := True;
  end;
end;

procedure TCustomDBGrid.EditorDoGetValue;
begin
  {$ifdef dbgDBGrid}DebugLn('DBGrid.EditorDoGetValue INIT');{$endif}
  inherited EditordoGetValue;
  UpdateData;
  {$ifdef dbgDBGrid}DebugLn('DBGrid.EditorDoGetValue FIN');{$endif}
end;

procedure TCustomDBGrid.CellClick(const aCol, aRow: Integer; const Button:TMouseButton);
begin

  if Button<>mbLeft then
    exit;

  if (aCol>=FirstGridColumn)and(aRow>=FixedRows) then
  begin
    if ColumnEditorStyle(ACol, SelectedField) = cbsCheckboxColumn then begin
      // react only if overriden editor is hidden
      if (Editor=nil) or not EditorMode then
  	    SwapCheckBox
    end;
  end;
  if Assigned(OnCellClick) then
    OnCellClick(TColumn(ColumnFromGridColumn(aCol)));
end;

procedure TCustomDBGrid.EndLayout;
begin
  dec(FLayoutChangedCount);
  if FLayoutChangedCount = 0 then
    DoLayoutChanged;
end;

function TCustomDBGrid.GetDefaultColumnAlignment(Column: Integer): TAlignment;
var
  F: TField;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    result := F.Alignment
  else
    result := taLeftJustify;
end;

function TCustomDBGrid.GetDefaultColumnWidth(Column: Integer): Integer;
begin
  Result := DefaultFieldColWidth(GetDsFieldFromGridColumn(Column));
end;

function TCustomDBGrid.GetDefaultColumnReadOnly(Column: Integer): boolean;
var
  F: Tfield;
begin
  result := true;
  if not Self.ReadOnly and (FDataLink.Active and not FDatalink.ReadOnly) then begin
    F := GetDsFieldFromGridColumn(Column);
    result := (F=nil) or F.ReadOnly;
  end;
end;

function TCustomDBGrid.GetDefaultColumnTitle(Column: Integer): string;
var
  F: Tfield;
begin
  F := GetDsFieldFromGridColumn(Column);
  if F<>nil then
    Result := F.DisplayName
  else
    Result := '';
end;

function TCustomDBGrid.GetDefaultRowHeight: integer;
begin
  result := inherited GetDefaultRowHeight;
  Dec(Result, 2); // a litle smaller for dbgrid
end;

procedure TCustomDBGrid.DoExit;
begin
  {$ifdef dbgDBGrid}DebugLn('DBGrid.DoExit INIT');{$Endif}
  if ValidDataSet and (dgCancelOnExit in Options) and
    InsertCancelable then
  begin
    FDataLink.DataSet.Cancel;
    EditingColumn(FEditingColumn, False);
  end;
  inherited DoExit;
  {$ifdef dbgDBGrid}DebugLn('DBGrid.DoExit FIN');{$Endif}
end;

function TCustomDBGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    FDatalink.MoveBy(1);
    Result := True;
  end;
end;

function TCustomDBGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
  if not Result and FDatalink.Active then begin
    FDatalink.MoveBy(-1);
    Result := True;
  end;
end;

function TCustomDBGrid.GetEditMask(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  Result := '';
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if (aField<>nil) then begin
      {$ifdef EnableFieldEditMask}
      // enable following line if TField gets in the future a MaskEdit property
      //Result := aField.EditMask;
      if assigned(OnFieldEditMask) then
        OnFieldEditMask(Self, AField, Result);
      {$endif}
    end;
  end;
end;

function TCustomDBGrid.GetEditText(aCol, aRow: Longint): string;
var
  aField: TField;
begin
  if FDataLink.Active then begin
    aField := GetFieldFromGridColumn(aCol);
    if aField<>nil then begin
      Result := aField.Text;
    end;
  end;
end;

function TCustomDBGrid.GetIsCellSelected(aCol, aRow: Integer): boolean;
begin
  Result:=inherited GetIsCellSelected(aCol, aRow) or
    FDrawingMultiSelRecord;
end;

function TCustomDBGrid.GetIsCellTitle(aCol, aRow: Integer): boolean;
begin
  result := (FixedRows>0) and (aRow=0);
  if result and Columns.Enabled then
    result := (aCol>=FirstGridColumn);
end;

procedure TCustomDBGrid.GetSelectedState(AState: TGridDrawState; out
  IsSelected: boolean);
begin
  inherited GetSelectedState(AState, IsSelected);
  if IsSelected and not Self.Focused and not(dgAlwaysShowSelection in Options) then
    IsSelected := false;
end;

function TCustomDBGrid.GridCanModify: boolean;
begin
  result := not ReadOnly and (dgEditing in Options) and not FDataLink.ReadOnly
    and FDataLink.Active and FDatalink.DataSet.CanModify;
end;

procedure TCustomDBGrid.GetSBVisibility(out HsbVisible, VsbVisible: boolean);
var
  aRange,aPage,aPos: Integer;
begin
  inherited GetSBVisibility(HsbVisible, VsbVisible);
  VSbVisible := (ScrollBars in [ssVertical, ssBoth]);
  if not VSbVisible and ScrollBarAutomatic(ssVertical) then begin
    GetScrollbarParams(aRange,aPage, aPos);
    if ARange>aPage then
      VSbVisible:=True;
  end;
end;

procedure TCustomDBGrid.GetSBRanges(const HsbVisible, VsbVisible: boolean; out
  HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos: Integer);
begin
  inherited GetSBRanges(HsbVisible, VsbVisible, HsbRange, VsbRange, HsbPage, VsbPage, HsbPos, VsbPos);
  if VSbVisible then
    GetScrollbarParams(VsbRange, VsbPage, VsbPos)
  else begin
    VsbRange := 0;
    VsbPage := 0;
    VsbPos := 0;
  end;
end;

procedure TCustomDBGrid.MoveSelection;
begin
  {$ifdef dbgDBGrid}DebugLn('DBGrid.MoveSelection INIT');{$Endif}
  inherited MoveSelection;
  if FColEnterPending and Assigned(OnColEnter) then begin
    OnColEnter(Self);
  end;
  FColEnterPending:=False;
  UpdateActive;
  {$ifdef dbgDBGrid}DebugLn('DBGrid.MoveSelection FIN');{$Endif}
end;

function TCustomDBGrid.MouseButtonAllowed(Button: TMouseButton): boolean;
begin
  Result:= (Button=mbLeft) or (dgAnyButtonCanSelect in Options);
end;

procedure TCustomDBGrid.DrawAllRows;
var
  CurActiveRecord: Integer;
begin
  if FDataLink.Active then begin
    {$ifdef dbgGridPaint}
    DebugLn;
    DebugLn('%s DrawAllRows: Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
    {$endif}
    CurActiveRecord:=FDataLink.ActiveRecord;
    FDrawingEmptyDataset:=FDatalink.DataSet.IsEmpty;
  end else
    FDrawingEmptyDataset:=True;
  try
    inherited DrawAllRows;
  finally
    if FDataLink.Active then begin
      FDataLink.ActiveRecord:=CurActiveRecord;
      {$ifdef dbgGridPaint}
      DebugLn('%s DrawAllRows END Link.ActiveRecord=%d, Row=%d',[Name, FDataLink.ActiveRecord, Row]);
      DebugLn;
      {$endif}
    end;
  end;
end;

procedure TCustomDBGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect);
begin
  // Draw focused cell if we have the focus
  if Self.Focused and (dgAlwaysShowSelection in Options) and
    FDatalink.Active and DefaultDrawing then
  begin
    CalcFocusRect(aRect);
    DrawRubberRect(Canvas, aRect, FocusColor);
  end;
end;

//
procedure TCustomDBGrid.DrawRow(ARow: Integer);
begin
  if (ARow>=FixedRows) and FDataLink.Active then begin
    //if (Arow>=FixedRows) and FCanBrowse then
    FDataLink.ActiveRecord:=ARow-FixedRows;
    FDrawingActiveRecord := ARow = Row;
    FDrawingMultiSelRecord := (dgMultiSelect in Options) and
      SelectedRows.CurrentRowSelected
  end else begin
    FDrawingActiveRecord := False;
    FDrawingMultiSelRecord := False;
  end;
  {$ifdef dbgGridPaint}
  DbgOut('DrawRow Row=', IntToStr(ARow), ' Act=', dbgs(FDrawingActiveRecord));
  {$endif}
  inherited DrawRow(ARow);
  {$ifdef dbgGridPaint}
  DebugLn(' End Row')
  {$endif}
end;

procedure TCustomDBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  DataCol: Integer;
begin
  PrepareCanvas(aCol, aRow, aState);

  {$ifdef dbgGridPaint}
  DbgOut(' ',IntToStr(aCol));
  if gdSelected in aState then DbgOut('S');
  if gdFocused in aState then DbgOut('*');
  if gdFixed in aState then DbgOut('F');
  {$endif dbgGridPaint}

  if DefaultDrawing then
    DefaultDrawCell(aCol, aRow, aRect, aState);

  if (ARow>=FixedRows) and Assigned(OnDrawColumnCell) and
    not (csDesigning in ComponentState) then begin

    DataCol := ColumnIndexFromGridColumn(aCol);
    if DataCol>=0 then
      OnDrawColumnCell(Self, aRect, DataCol, TColumn(Columns[DataCol]), aState);

  end;

  DrawCellGrid(aCol, aRow, aRect, aState);
end;

procedure TCustomDBGrid.DrawCheckboxBitmaps(aCol: Integer; aRect: TRect;
  F: TField);
var
  AState: TCheckboxState;
begin
  if (aCol=Col) and FDrawingActiveRecord then begin
    // show checkbox only if overriden editor is hidden
    if EditorMode then
      exit;
  end;

  // by SSY
  if (F<>nil) then
    if F.DataType=ftBoolean then
      if F.IsNull then
    	  AState := cbGrayed
      else
      if F.AsBoolean then
        AState := cbChecked
      else
        AState := cbUnChecked
    else
      if F.AsString=ColumnFromGridColumn(aCol).ValueChecked then
        AState := cbChecked
      else

      if F.AsString=ColumnFromGridColumn(aCol).ValueUnChecked then
        AState := cbUnChecked
      else
        AState := cbGrayed
  else
    AState := cbGrayed;

  if assigned(OnUserCheckboxState) then
    OnUserCheckboxState(Self, TColumn(ColumnFromGridColumn(aCol)), AState);

  DrawGridCheckboxBitmaps(aCol, Row{dummy}, ARect, AState);
end;

procedure TCustomDBGrid.DrawFixedText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);

  function GetDatasetState: TDataSetState;
  begin
    if FDatalink.Active then
      result := FDataLink.DataSet.State
    else
      result := dsInactive;
  end;

begin
  if (ACol=0) and (dgIndicator in Options) and FDrawingActiveRecord then begin
    DrawIndicator(Canvas, aRect, GetDataSetState, FDrawingMultiSelRecord);
    {$ifdef dbgGridPaint}
    dbgOut('>');
    {$endif}
  end else
  if (ACol=0) and (dgIndicator in Options) and FDrawingMultiSelRecord then
    DrawIndicator(Canvas, aRect, dsCurValue{dummy}, True)
  else
    DrawColumnText(aCol, aRow, aRect, aState);
end;

procedure TCustomDBGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
 aState: TGridDrawState);
var
  F: TField;
begin
  if GetIsCellTitle(aCol, aRow) then
    inherited DrawColumnText(aCol, aRow, aRect, aState)
  else begin
    F := GetFieldFromGridColumn(aCol);
    if F<>nil then
      DrawCellText(aCol, aRow, aRect, aState, F.DisplayText)
  end;
end;

procedure TCustomDBGrid.DrawIndicator(ACanvas: TCanvas; R: TRect;
  Opt: TDataSetState; MultiSel: boolean);
var
  dx,dy, x, y: Integer;
  procedure CenterY;
  begin
    y := R.Top + (R.Bottom-R.Top) div 2;
  end;
  procedure CenterX;
  begin
    X := R.Left + (R.Right-R.Left) div 2;
  end;
  procedure DrawEdit(clr: Tcolor);
  begin
    ACanvas.Pen.Color := clr;
    CenterY;
    CenterX;
    ACanvas.MoveTo(X-2, Y-Dy);
    ACanvas.LineTo(X+3, Y-Dy);
    ACanvas.MoveTo(X, Y-Dy);
    ACanvas.LineTo(X, Y+Dy);
    ACanvas.MoveTo(X-2, Y+Dy);
    ACanvas.LineTo(X+3, Y+Dy);
  end;
begin
  dx := 6;
  dy := 6;
  case Opt of
    dsBrowse:
      begin //
        ACanvas.Brush.Color:=clBlack;
        ACanvas.Pen.Color:=clBlack;
        CenterY;
        x:= R.Left+3;
        if MultiSel then begin
          ACanvas.Polyline([point(x,y-dy),  point(x+dx,y),point(x,y+dy), point(x,y+dy-1)]);
          ACanvas.Polyline([point(x,y-dy+1),point(x+dx-1,y),point(x, y+dy-1), point(x,y+dy-2)]);
          CenterX;
          Dec(X,3);
          ACanvas.Ellipse(Rect(X-2,Y-2,X+2,Y+2));
        end else
          ACanvas.Polygon([point(x,y-dy),point(x+dx,y),point(x, y+dy),point(x,y-dy)]);
       end;
    dsEdit:
      DrawEdit(clBlack);
    dsInsert:
      DrawEdit(clGreen);
    else
    if MultiSel then begin
      ACanvas.Brush.Color:=clBlack;
      ACanvas.Pen.Color:=clBlack;
      CenterX;
      CenterY;
      ACanvas.Ellipse(Rect(X-3,Y-3,X+3,Y+3));
    end;
  end;
end;

function TCustomDBGrid.EditorCanAcceptKey(const ch: TUTF8Char): boolean;
var
  aField: TField;
begin
  result := False;
  if FDataLink.Active then begin
    aField := SelectedField;
    if aField<>nil then begin
      Result := IsValidChar(AField, Ch) and not aField.Calculated and
        (aField.DataType<>ftAutoInc) and not aField.IsBlob;
    end;
  end;
end;

function TCustomDBGrid.EditorIsReadOnly: boolean;
var
  AField : TField;
  FieldList: TList;
  I: Integer;
begin
  Result := inherited EditorIsReadOnly;
  if not Result then begin

    AField := GetFieldFromGridColumn(Col);
    if assigned(AField) then begin

      // if field can't be modified, it's assumed readonly
      result := not AField.CanModify;

      // if field is readonly, check if it's a lookup field
      if result and AField.Lookup then begin
        FieldList := TList.Create;
        try
          AField.DataSet.GetFieldList(FieldList, AField.KeyFields);
          // check if any keyfields are there
          result := (FieldList.Count=0); // if not simply is still readonly
                                         // if yes assumed keyfields are modifiable
          for I := 0 to FieldList.Count-1 do
            if not TField(FieldList[I]).CanModify then begin
              result := true; // at least one keyfield is readonly
              break;
            end;
        finally
          FieldList.Free;
        end;
      end;

      // if it's not readonly and is not already editing, start editing.
      if not result and not FDatalink.Editing then begin
        Include(FGridStatus, gsStartEditing);
        Result := not FDataLink.Edit;
        Exclude(FGridStatus, gsStartEditing);
      end;

    end
    else
      result := true;  // field is nil so it's readonly

    EditingColumn(Col, not Result);
  end;
end;

procedure TCustomDBGrid.EditorTextChanged(const aCol, aRow: Integer;
  const aText: string);
begin
  if not EditorIsReadonly then
    SetEditText(aCol, aRow, aText);
end;

procedure TCustomDBGrid.HeaderSized(IsColumn: Boolean; Index: Integer);
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
    if Assigned(OnColumnSized) then
      OnColumnSized(Self);
  end;
end;

function TCustomDBGrid.IsValidChar(AField: TField; AChar: TUTF8Char): boolean;
begin
  result := False;

  if Length(AChar)>1 then begin
    // problem: AField should validate a unicode char, but AField has no
    //          such facility, ask the user, if user is not interested
    //          do ansi convertion and try with field.

    { TODO: is this really necessary?
    if Assigned(FOnValidateUTF8Char) then begin
      result := true;
      OnValidateUT8Char(Self, AField, AChar, Result)
      exit;
    end else
    }
      AChar := UTF8ToSys(AChar);
  end else
  if Length(AChar)=0 then
    exit;

  Result := AField.IsValidChar(AChar[1])
end;

procedure TCustomDBGrid.UpdateActive;
var
  PrevRow: Integer;
begin
  if (csDestroying in ComponentState) or
    (FDatalink=nil) or (not FDatalink.Active) or
    (FDatalink.ActiveRecord<0) then
    exit;
  {$IfDef dbgDBGrid}
  DebugLn(Name,'.UpdateActive: ActiveRecord=', dbgs(FDataLink.ActiveRecord),
            ' FixedRows=',dbgs(FixedRows), ' Row=', dbgs(Row));
  {$endif}
  PrevRow := Row;
  Row:= FixedRows + FDataLink.ActiveRecord;
  if PrevRow<>Row then
    InvalidateCell(0, PrevRow);//(InvalidateRow(PrevRow);
  InvalidateRow(Row);
end;

function TCustomDBGrid.UpdateGridCounts: Integer;
var
  RecCount: Integer;
  FRCount, FCCount: Integer;
begin
  // find out the column count, if result=0 then
  // there are no visible columns defined or dataset is inactive
  // or there are no visible fields, ie the grid is blank
  {$IfDef dbgDBGrid}DebugLn('TCustomDbgrid.UpdateGridCounts INIT');{$endif}
  BeginUpdate;
  try
    Result := GetColumnCount;
    if Result > 0 then begin

      if dgTitles in Options then FRCount := 1 else FRCount := 0;
      if dgIndicator in Options then FCCount := 1 else FCCount := 0;
      InternalSetColCount(Result + FCCount);

      if FDataLink.Active then begin
        UpdateBufferCount;
        RecCount := FDataLink.RecordCount;
        if RecCount<1 then
          RecCount := 1;
      end else begin
        RecCount := 0;
        if FRCount=0 then
          // need to be large enough to hold indicator
          // if there is one, and if there are no titles
          RecCount := FCCount;
      end;

      Inc(RecCount, FRCount);

      RowCount := RecCount;
      FixedRows := FRCount;

      UpdateGridColumnSizes;

      if FDatalink.Active and (FDatalink.ActiveRecord>=0) then
        AdjustEditorBounds(Col, FixedRows + FDatalink.ActiveRecord);
    end;
  finally
    EndUpdate;
  end;
  {$IfDef dbgDBGrid}DebugLn('TCustomDbgrid.UpdateGridCounts END');{$endif}
end;

constructor TCustomDBGrid.Create(AOwner: TComponent);
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

  FSelectedRows := TBookmarkList.Create(Self);

  RenewColWidths;

  FOptions := [dgColumnResize, dgColumnMove, dgTitles, dgIndicator, dgRowLines,
    dgColLines, dgConfirmDelete, dgCancelOnExit, dgTabs, dgEditing,
    dgAlwaysShowSelection];

  inherited Options :=
    [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
     goSmoothScroll, goColMoving, goTabs, goEditing, goDrawFocusSelected,
     goColSizing ];

  FExtraOptions := [dgeAutoColumns, dgeCheckboxColumn];

  AutoAdvance := aaRightDown;

  // What a dilema!, we need ssAutoHorizontal and ssVertical!!!
  ScrollBars:=ssBoth;
end;

procedure TCustomDBGrid.AutoSizeColumns;
begin
  RenewColWidths;
  LayoutChanged;
end;

procedure TCustomDBGrid.InitiateAction;
begin
  {$ifdef dbgDBGrid}DebugLn('===> DBGrid.InitiateAction INIT');{$endif}
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
  {$ifdef dbgDBGrid}DebugLn('<=== DBGrid.InitiateAction FIN');{$endif}
end;

procedure TCustomDBGrid.DefaultDrawColumnCell(const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
  S: string;
  F: TField;
  DataRow: Integer;
begin

  F := Column.Field;

  DataCol := GridColumnFromColumnIndex(DataCol);
  if FDataLink.Active then
    DataRow := FixedRows + FDataLink.ActiveRecord
  else
    DataRow := 0;

  if DataCol>=FirstGridColumn then
    case ColumnEditorStyle(DataCol, F) of

      cbsCheckBoxColumn:
        DrawCheckBoxBitmaps(DataCol, Rect, F);

      else begin
        if F<>nil then begin
          if F.dataType <> ftBlob then
            S := F.DisplayText
          else
            S := '(blob)';
        end else
          S := '';
        DrawCellText(DataCol, DataRow, Rect, State, S);
      end;

    end;
end;

function TCustomDBGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;
begin
  // we want override the editor style if it is cbsAuto because
  // field.datatype might be ftBoolean or some other cases
  if Style=cbsAuto then
    Style := ColumnEditorStyle(Col, SelectedField);

  Result:=inherited EditorByStyle(Style);
end;

procedure TCustomDBGrid.ResetColWidths;
begin
  if not FDefaultColWidths then begin
    RenewColWidths;
    LayoutChanged;
  end;
end;

procedure TCustomDBGrid.SelectRecord(AValue: boolean);
begin
  if dgMultiSelect in Options then
    FSelectedRows.CurrentRowSelected := AValue;
end;

procedure TCustomDBGrid.GetScrollbarParams(out aRange, aPage, aPos: Integer);
begin
  if (FDatalink<>nil) and FDatalink.Active then begin
    if FDatalink.dataset.IsSequenced then begin
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
end;

procedure TCustomDBGrid.CMGetDataLink(var Message: TLMessage);
begin
  Message.Result := PtrUInt(FDataLink);
end;

procedure TCustomDBGrid.ClearSelection(selCurrent:boolean=false);
begin
  if [dgMultiSelect,dgPersistentMultiSelect]*Options=[dgMultiSelect] then begin
    if SelectedRows.Count>0 then
      SelectedRows.Clear;
    if SelCurrent then
      SelectRecord(true);
  end;
  FKeyBookmark:='';
end;

function TCustomDBGrid.NeedAutoSizeColumns: boolean;
begin
  result := (dgAutoSizeColumns in Options)
            //and (HandleAllocated)
            ;
end;

procedure TCustomDBGrid.RenewColWidths;
begin
  FDefaultColWidths := True;
  exclude(FGridStatus, gsAutoSized);
end;

destructor TCustomDBGrid.Destroy;
begin
  FSelectedRows.Free;
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
  {$ifdef dbgDBGrid}
  DebugLn('TComponentDataLink.RecordChanged');
  {$endif}
  if Assigned(OnRecordChanged) then
    OnRecordChanged(Field);
end;

procedure TComponentDataLink.DataSetChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLn('TComponentDataLink.DataSetChanged, FirstRecord=', dbgs(FirstRecord));
  {$Endif}
  // todo: improve this routine, for example: OnDatasetInserted
  if Assigned(OnDataSetChanged) then
    OnDataSetChanged(DataSet);
end;

procedure TComponentDataLink.ActiveChanged;
begin
  {$ifdef dbgDBGrid}
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
  {$ifdef dbgDBGrid}
  DebugLn('TComponentDataLink.LayoutChanged');
  {$Endif}
  if Assigned(OnLayoutChanged) then
    OnLayoutChanged(DataSet);
end;

procedure TComponentDataLink.DataSetScrolled(Distance: Integer);
begin
  {$ifdef dbgDBGrid}
  DebugLn('TComponentDataLink.DataSetScrolled(',IntToStr(Distance),')');
  {$endif}
  if Assigned(OnDataSetScrolled) then
    OnDataSetScrolled(DataSet, Distance);
end;

procedure TComponentDataLink.FocusControl(Field: TFieldRef);
begin
  {$ifdef dbgDBGrid}
  DebugLn('TComponentDataLink.FocusControl');
  {$endif}
end;

procedure TComponentDataLink.CheckBrowseMode;
begin
  {$ifdef dbgDBGrid}
  DebugLn(ClassName,'.CheckBrowseMode');
  {$endif}
  inherited CheckBrowseMode;
end;

procedure TComponentDataLink.EditingChanged;
begin
  {$ifdef dbgDBGrid}
  DebugLn(ClassName,'.EditingChanged');
  {$endif}
  if Assigned(OnEditingChanged) then
    OnEditingChanged(DataSet);
end;

procedure TComponentDataLink.UpdateData;
begin
  {$ifdef dbgDBGrid}
  DebugLn(ClassName,'.UpdateData');
  {$endif}
  if Assigned(OnUpdatedata) then
    OnUpdateData(DataSet);
end;

function TComponentDataLink.MoveBy(Distance: Integer): Integer;
begin
  (*
  {$ifdef dbgDBGrid}
  DebugLn(ClassName,'.MoveBy  INIT: Distance=',Distance);
  {$endif}
  *)
  Result:=inherited MoveBy(Distance);
  (*
  {$ifdef dbgDBGrid}
  DebugLn(ClassName,'.MoveBy  END: Distance=',Distance);
  {$endif}
  *)
end;

{ TDBGridColumns }

function TDBGridColumns.GetColumn(Index: Integer): TColumn;
begin
  result := TColumn( inherited Items[Index] );
end;

procedure TDBGridColumns.SetColumn(Index: Integer; Value: TColumn);
begin
  Items[Index].Assign( Value );
end;

procedure TDBGridColumns.Update(Item: TCollectionItem);
begin
  if (Grid<>nil) and not (csLoading in Grid.ComponentState) then
    TCustomDBGrid(Grid).LayoutChanged;
end;

function TDBGridColumns.ColumnFromField(Field: TField): TColumn;
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

function TDBGridColumns.HasAutomaticColumns: boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Count-1 do
    if Items[i].IsAutomaticColumn then begin
      Result := true;
      break;
    end;
end;

function TDBGridColumns.HasDesignColumns: boolean;
var
  i: Integer;
begin
  Result := False;
  for i:=0 to Count-1 do
    if Items[i].IsDesignColumn then begin
      Result := true;
      break;
    end;
end;

procedure TDBGridColumns.RemoveAutoColumns;
var
  i: Integer;
  G: TCustomDBGrid;
begin
  if HasAutomaticColumns then begin
    G := TCustomDBGrid(Grid);
    G.GridStatus := G.GridStatus + [gsRemovingAutoColumns];
    BeginUpdate;
    try
      for i:=Count-1 downto 0 do
        if Items[i].IsAutomaticColumn then
          Delete(i);
    finally
      EndUpdate;
      G.GridStatus := G.GridStatus - [gsRemovingAutoColumns];
    end;
  end;
end;

function CompareFieldIndex(P1,P2:Pointer): integer;
begin
  if P1=P2 then
    Result := 0
  else if (P1=nil) or (TColumn(P1).Field=nil) then
    Result := 1
  else if (P2=nil) or (TColumn(P2).Field=nil) then
    Result := -1
  else
    Result := TColumn(P1).Field.Index - TColumn(P2).Field.Index;
end;

function CompareDesignIndex(P1,P2:Pointer): integer;
begin
  result := TColumn(P1).DesignIndex - TColumn(P2).DesignIndex;
end;

procedure TDBGridColumns.ResetColumnsOrder(ColumnOrder: TColumnOrder);
var
  L: TList;
  i: Integer;
begin

  L := TList.Create;
  try

    for i:=0 to Count-1 do
      L.Add(Items[i]);

    case ColumnOrder of
      coDesignOrder:
        begin
          if not HasDesignColumns then
            exit;
          L.Sort(@CompareDesignIndex)
        end;
      coFieldIndexOrder:
        L.Sort(@CompareFieldIndex);
      else
        exit;
    end;

    for i:=0 to L.Count-1 do
      TColumn(L.Items[i]).Index := i;

  finally
    L.Free;
  end;
end;

function TDBGridColumns.Add: TColumn;
var
  G: TCustomDBGrid;
begin
  G := TCustomDBGrid(Grid);
  if G<>nil then begin
    // remove automatic columns before adding user columns
    if not (gsAddingAutoColumns in G.GridStatus) then
      RemoveAutoColumns;
  end;
  result := TColumn( inherited add );
end;

procedure TDBGridColumns.LinkFields;
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

function TColumn.GetIsDesignColumn: boolean;
begin
  result := (DesignIndex>=0) and (DesignIndex<10000);
end;

function TColumn.GetPickList: TStrings;
begin
  Result := inherited GetPickList;
  if (Field<>nil) and FField.Lookup then
  begin
    if FField.LookupCache then
      FField.LookupList.ValuesToStrings(Result)
    else
    begin
      Result.Clear;
      LookupGetBookMark(FField);
      try
      with FField.LookupDataSet do
      begin
        First;
        while not EOF do
        begin
          Result.Add(FieldbyName(FField.LookupResultField).AsString);
          Next;
        end;
      end;
      finally
        LookupGotoBookMark(FField);
      end;
    end;
  end;
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
  AGrid: TCustomDBGrid;
begin
  AGrid := TCustomDBGrid(Grid);
  if (AGrid<>nil) then
    result := AGrid.FDataLink.DataSet
  else
    result :=nil;
end;

procedure TColumn.Assign(Source: TPersistent);
begin
  if Source is TColumn then begin
    //DebugLn('Assigning TColumn[',dbgs(Index),'] a TColumn')
    Collection.BeginUpdate;
    try
      inherited Assign(Source);
      FieldName := TColumn(Source).FieldName;
      DisplayFormat := TColumn(Source).DisplayFormat;
      ValueChecked := TColumn(Source).ValueChecked;
      ValueUnchecked := TColumn(Source).ValueUnchecked;
    finally
      Collection.EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

function TColumn.GetDefaultWidth: Integer;
var
  AGrid: TCustomDBGrid;
  tmpCanvas: TCanvas;
begin
  AGrid := TCustomDBGrid(Grid);
  if AGrid<>nil then begin

    tmpCanvas := GetWorkingCanvas(aGrid.Canvas);
    tmpCanvas.Font := aGrid.Font;

    if FField<>nil then
      result := CalcColumnFieldWidth(
        tmpCanvas,
        dgTitles in aGrid.Options,
        Title.Caption,
        Title.Font,
        FField)
    else
      result := AGrid.DefaultColWidth;

    if tmpCanvas<>AGrid.Canvas then
      FreeWorkingCanvas(tmpCanvas);

  end else
    result := DEFCOLWIDTH;
end;

function TColumn.CreateTitle: TGridColumnTitle;
begin
  Result := TColumnTitle.Create(Self);
end;

constructor TColumn.Create(ACollection: TCollection);
var
  AGrid: TCustomGrid;
begin
  inherited Create(ACollection);
  if ACollection is TDBGridColumns then begin
    AGrid := TDBGridColumns(ACollection).Grid;
    if (AGrid<>nil) and (csLoading in AGrid.ComponentState) then
      FDesignIndex := Index
    else
      FDesignIndex := 10000;
  end;
end;

function TColumn.IsDefault: boolean;
begin
  result := not FDisplayFormatChanged and (inherited IsDefault());
end;

procedure TColumn.LinkField;
var
  AGrid: TCustomDBGrid;
begin
  AGrid:= TCustomDBGrid(Grid);
  if (AGrid<>nil) and AGrid.FDatalink.Active then begin
    Field := AGrid.FDataLink.DataSet.FindField(FFieldName);
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

function TColumn.GetDefaultValueChecked: string;
begin
  if (FField<>nil) and (FField.Datatype=ftBoolean) then
    Result := BoolToStr(True)
  else
    Result := '1';
end;

function TColumn.GetDefaultValueUnchecked: string;
begin
  if (FField<>nil) and (FField.DataType=ftBoolean) then
    Result := BoolToStr(False)
  else
    Result := '0';
end;

function TColumn.GetDefaultReadOnly: boolean;
var
  AGrid: TCustomDBGrid;
begin
  AGrid := TCustomDBGrid(Grid);
  Result := ((AGrid<>nil)and(AGrid.ReadOnly)) or ((FField<>nil)And(FField.ReadOnly))
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
  if FFieldName<>'' then
    Result:=FFieldName
  else
    Result:=inherited GetDisplayName;
end;

function TColumn.GetDefaultAlignment: TAlignment;
var
  Bs: set of TColumnButtonStyle;
begin
  bs := [buttonStyle];
  if Grid<>nil then
    Include(bs, TCustomDbGrid(Grid).DefaultEditorStyle(ButtonStyle, FField));
  if bs*[cbsCheckboxColumn,cbsButtonColumn]<>[] then
    result := taCenter
  else
  if FField<>nil then
    result := FField.Alignment
  else
    Result := taLeftJustify;
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

{ TBookmarkList }

function TBookmarkList.GetCount: integer;
begin
  result := FList.Count;
end;

function TBookmarkList.GetCurrentRowSelected: boolean;
begin
  CheckActive;
  Result := IndexOf(FGrid.Datasource.Dataset.Bookmark)>=0;
end;

function TBookmarkList.GetItem(AIndex: Integer): TBookmarkStr;
begin
  Result := FList[AIndex];
end;

procedure TBookmarkList.SetCurrentRowSelected(const AValue: boolean);
var
  aBookStr: TBookmarkstr;
  aIndex: Integer;
begin
  CheckActive;

  aBookStr := FGrid.Datasource.Dataset.Bookmark;
  if ABookStr='' then
    exit;

  if Find(ABookStr, aIndex) then begin
    if not AValue then begin
      FList.Delete(aIndex);
      FGrid.Invalidate;
    end;
  end else begin
    if AValue then begin
      FList.Add(ABookStr);
      FGrid.Invalidate;
    end;
  end;
end;

procedure TBookmarkList.CheckActive;
begin
  if not Fgrid.FDataLink.Active then
    raise EInvalidGridOperation.Create('Dataset Inactive');
end;

constructor TBookmarkList.Create(AGrid: TCustomDBGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FList := TBMStringList.Create;
  FList.CaseSensitive:=True;
  FList.Sorted:=True;
end;

destructor TBookmarkList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TBookmarkList.Clear;
begin
  FList.Clear;
  FGrid.Invalidate;
end;

procedure TBookmarkList.Delete;
var
  i: Integer;
  ds: TDataSet;
begin
  ds := FGrid.Datasource.Dataset;
  for i:=0 to FList.Count-1 do begin
    ds.Bookmark := Items[i];
    ds.Delete;
    FList.delete(i);
  end;
end;

function TBookmarkList.Find(const Item: TBookmarkStr; var AIndex: Integer): boolean;
var
  Indx: integer;
begin
  Indx := FList.IndexOf(Item);
  if indx<0 then
    Result := False
  else begin
    Result := True;
    AIndex := indx;
  end;
end;

function TBookmarkList.IndexOf(const Item: TBookmarkStr): Integer;
begin
  result := FList.IndexOf(Item)
end;

function TBookmarkList.Refresh: boolean;
var
  ds: TDataset;
  i: LongInt;
begin
  Result := False;
  ds := FGrid.Datasource.Dataset;
  for i:=FList.Count-1 downto 0 do
    if not ds.BookmarkValid(TBookMark(Items[i])) then begin
      Result := True;
      Flist.Delete(i);
    end;
  if Result then
    FGrid.Invalidate;
end;

{ TBMStringList }

function TBMStringList.DoCompareText(const s1, s2: string): PtrInt;
begin
  if Length(s1)<Length(s2) then
    result := -1
  else
  if Length(s1)>Length(s2) then
    result := 1
  else
    result := CompareMemRange(@s1[1],@s2[1], Length(s1));
end;

end.
