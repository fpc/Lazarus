unit stub.bootstraptablewidget;

interface

uses
  Stub.WebWidget, SysUtils, Classes, DB, Stub.JS;

type
  EBootstrapTable = class(EWidgets);

  TCustomDBBootstrapTableWidget = Class;

  TColumnRenderMode = (crmText, crmNumeric, crmDateTime, crmTransformedValue, crmCheckBox, crmButton, crmCustom);
  TColumnButtonType = (cbtInfo, cbtEdit, cbtDelete, cbtCustom);

  TDataTablesFieldMap = Class(TObject)
  Private
  Public
    Function GetValueByName(S: String): JSValue;
  end;

  TOnCustomValueEvent = procedure(Sender: TObject; Fields: TDataTablesFieldMap; out aOutput: String) of object;
  TOnFormatEvent = procedure(Sender: TObject; Data: JSValue; row: TJSObject; RowIndex : Integer; Field : string; out aOutput: JSValue) of object;
  TSortOrderMethod = function(Sender: TObject; Data: JSValue): Integer of object;

  { TStylingClasses }

  TStylingClasses = Class(TPersistent)
  private
    FButtonClass: String;
    FCheckBoxClass: String;
    FDeleteClass: String;
    FEditClass: String;
    FInfoClass: String;
    FReadonlyEditClass: String;
    FWidget : TCustomDBBootstrapTableWidget;
    function GetReadonlyEditClass: String;
    function IsReadonlyStored: Boolean;
  Public
    Constructor Create(aWidget: TCustomDBBootstrapTableWidget); virtual;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property CheckBoxClass : String Read FCheckBoxClass Write FCheckBoxClass;
    Property ButtonClass : String Read FButtonClass Write FButtonClass;
    Property InfoClass : String Read FInfoClass Write FInfoClass;
    Property EditClass : String Read FEditClass Write FEditClass;
    Property ReadonlyEditClass : String Read GetReadonlyEditClass Write FReadonlyEditClass stored IsReadonlyStored;
    Property DeleteClass : String Read FDeleteClass Write FDeleteClass;
  end;

  { TBSTableColumn }

  TBSTableColumn = class(TCollectionItem)
  private
    FFieldName: string;
    FFormatting: string;
    FSelectable: Boolean;
    FTitle: string;
    FRenderMode: TColumnRenderMode;
    FButtonType: TColumnButtonType;
    FOnCustomFormat: TOnFormatEvent;
    FWidth: Integer;
    FCSSClassName: string;
    FCheckBoxClassName: string;
    FVisible: Boolean;
    FSearchable: Boolean;
    FSortable: Boolean;
    FButtonURL: string;
    FButtonURLTarget: string;
    FButtonIconClass: String;
    FOnGetSortValue: TSortOrderMethod;
    FOnGetValue: TOnCustomValueEvent;
    FExtraAttributes: String;
    FWidthUnits: String;
    function GetTitle: string;
  protected
    function GetDisplayName: String; override;
    { private declarations }
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TCollection); override;
  published
    // Fieldname for this column
    property FieldName: string read FFieldName write FFieldName;
    // Title for this column
    property Title: string read GetTitle write FTitle;
    // Render mode: text, numer, checkbox, button custom render
    property RenderMode: TColumnRenderMode read FRenderMode write FRenderMode;
    // When rendermode is rmButton, what button ?
    property ButtonType: TColumnButtonType read FButtonType write FButtonType;
    // When buttontype is btCustom, use the following class (in <i class="">)
    Property ButtonIconClass: String Read FButtonIconClass Write FButtonIconClass;
    // Called when rendermode is rmTransformValue
    Property OnTransformValue: TOnCustomValueEvent Read FOnGetValue Write FOnGetValue;
    // Called when rendermode is rmCustom
    property OnCustomFormat: TOnFormatEvent read FOnCustomFormat write FOnCustomFormat;
    // Called when column is sorted; Return a sortable value for the data
    property OnGetSortValue: TSortOrderMethod read FOnGetSortValue write FOnGetSortValue;
    // Width (in WidthUnits units)
    property Width: Integer Read FWidth Write FWidth;
    // Width (in CSS units)
    property WidthUnits: String Read FWidthUnits Write FWidthUnits;
    // CSS Class name for this column
    property CSSClassName: string read FCSSClassName write FCSSClassName;
    // CSS Class name for this column if there is a check box.
    property CheckBoxClassName: string read FCheckBoxClassName write FCheckBoxClassName;
    // Visible column ?
    property Visible: Boolean Read FVisible Write FVisible;
    // Indication if column is searchable
    property Searchable: Boolean read FSearchable write FSearchable;
    // Indication if column is sortable
    property Sortable: Boolean read FSortable write FSortable;
    // URL to use when the button is clicked
    property ButtonURL: string read FButtonURL write FButtonURL;
    // Target of button URL
    property ButtonURLTarget: string read FButtonURLTarget write FButtonURLTarget;
    // Formatting string:
    // for rmDateTime, specify format as for FormatDateTime
    // for rmNumeric, specify format as for FormatFloat (sysutils)
    property Formatting : string read FFormatting write FFormatting;
    // Add extra attributes to the contents of the column if needed
    property ExtraAttributes: String read FExtraAttributes write FExtraAttributes;
    // Selectable ? This is a native bootstrap-table select column
    Property Selectable : Boolean Read FSelectable Write FSelectable;

  end;

  TBSTableColumns = class(TOwnedCollection)
  private
    function DoGetColumn(const aIndex: Integer): TBSTableColumn;
  public
    // Needed for TMS Web core streaming.
    function Add: TBSTableColumn; reintroduce; overload;
    function Add(const aName: string): TBSTableColumn; overload;
    function IndexOfColumn(const aName: string): Integer;
    function ColumnByName(const aName: string): TBSTableColumn;
    property DatatablesColumn[const aIndex: Integer]: TBSTableColumn read DoGetColumn; default;
    { public declarations }
  end;

//  TOnCreatedRowEvent = reference to function(row: TJSNode; Data: TJSArray; dataIndex: Integer): JSValue;

  { TTableDataLink }

  TTableDataLink = class(TDatalink)
  private
    FTable: TCustomDBBootstrapTableWidget;
  Public
    Constructor Create(aTable : TCustomDBBootstrapTableWidget);
    Procedure ActiveChanged; override;
    Property Table : TCustomDBBootstrapTableWidget Read FTable;
  end;

  TBSTableOption = (btoClickToSelect,btoEscapeHTML,btoSingleSelect, btoMultipleSelectRow,btoRememberOrder,
                    btoResizable,btoDetailViewByClick);
  TBSTableOptions = set of TBSTableOption;

  TBSTableViewOption = (bvoCardview,bvoCheckboxHeader,bvoDetailView,bvoDetailViewIcon,
                        bvoShowButtonIcons,bvoShowButtonText, bvoShowColumns,
                        bvoShowColumnsToggleAll,bvoShowToggle,bvoShowHeader,bvoSmartDisplay,
                        bvoShowRefresh,bvoShowFooter,bvoShowFullscreen,bvoVirtualScroll,
                        bvoShowSearchButton,bvoShowClearButton);
  TBSTableViewOptions = set of TBSTableViewOption;

  TBSTablePaginationOption = (bpoPagination,bpoLoop,bpoUseIntermediate,bpoExtended,bpoShowSwitch);
  TBSTablePaginationOptions = set of TBSTablePaginationOption;

  TBSTableSearchOption = (bsoSearch,bsoSearchOnEnterKey,bsoSearchTimeOut,
                          bsoStrictSearch, bsoTrimOnSearch,bsoVisibleSearch);
  TBSTableSearchOptions = set of TBSTableSearchOption;

  TBSTableSortOption = (booSortStable,booSortable,booSilentSort,booResetSort);
  TBSTableSortOptions = set of TBSTableSortOption;

  { TCustomDBBootstrapTableWidget }

  TCustomDBBootstrapTableWidget = class(TWebWidget)
  private
    FAfterBodyDraw: TNotifyEvent;
    FStylingClasses: TStylingClasses;
    FLink : TTableDataLink;
    FColumns: TBSTableColumns;
    FData: TJSArray;
    FTableOptions: TBSTableOptions;
    FTablePaginationOptions: TBSTablePaginationOptions;
    FTableSearchOptions: TBSTableSearchOptions;
    FTableSortOptions: TBSTableSortOptions;
    FTableViewOptions: TBSTableViewOptions;
    FReadOnly: Boolean;
    function GetData: TJSArray;
    function GetDatasource: TDataSource;
    function IsOptionsStored: Boolean;
    function IsPaginationOptionsStored: Boolean;
    function IsSearchOptionsStored: Boolean;
    function IsSortOPtionsStored: Boolean;
    function IsViewOptionsStored: Boolean;
    procedure SetData(const aData: TJSArray);
    procedure SetDatasource(AValue: TDataSource);
    procedure SetStylingClasses(AValue: TStylingClasses);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTableOptions(AValue: TBSTableOptions);
    procedure SetTablePaginationOptions(AValue: TBSTablePaginationOptions);
    procedure SetTableSearchOptions(AValue: TBSTableSearchOptions);
    procedure SetTableSortOptions(AValue: TBSTableSortOptions);
    procedure SetTableViewOptions(AValue: TBSTableViewOptions);
  Protected
  public

    constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
  protected
    // Classes used in styling
    Property StylingClasses : TStylingClasses Read FStylingClasses Write SetStylingClasses;
    // Columns to create
    property Columns: TBSTableColumns read FColumns write FColumns;
    // Dataset to get fields from.
    property DataSource: TDataSource read GetDatasource write SetDatasource;
    // General behaviour options
    Property Options : TBSTableOptions Read FTableOptions Write SetTableOptions Stored IsOptionsStored;
    // General View options
    Property ViewOptions : TBSTableViewOptions Read FTableViewOptions Write SetTableViewOptions Stored IsViewOptionsStored;
    // Pagination  options
    Property PaginationOptions : TBSTablePaginationOptions Read FTablePaginationOptions Write SetTablePaginationOptions Stored IsPaginationOptionsStored;
    // Search options
    Property SearchOptions : TBSTableSearchOptions Read FTableSearchOptions Write SetTableSearchOptions Stored IsSearchOptionsStored;
    // Sort options
    Property SortOptions : TBSTableSortOptions Read FTableSortOptions Write SetTableSortOptions Stored IsSortOPtionsStored;
    // Read-only table: disable delete, change edit button
    property DisplayReadOnly : Boolean read FReadOnly write SetReadOnly;
      // Called after the table has completed drawing
    Property AfterBodyDraw : TNotifyEvent Read FAfterBodyDraw Write FAfterBodyDraw;
   end;

  TDBBootstrapTableWidget = class(TCustomDBBootstrapTableWidget)
  Published
    Property StylingClasses;
    property Columns;
    property DataSource;
    Property Options;
    Property ViewOptions;
    Property PaginationOptions;
    Property SearchOptions;
    Property SortOptions;
    property DisplayReadOnly;
    Property AfterBodyDraw;
  end;

Const
  DefaultViewOptions = [bvoDetailView,bvoDetailViewIcon,
                        bvoShowButtonIcons,bvoShowColumns,
                        bvoShowColumnsToggleAll,bvoShowToggle,bvoShowHeader,
                        bvoShowRefresh,bvoShowFooter,bvoShowFullscreen,
                        bvoShowSearchButton,bvoShowClearButton];
  DefaultPaginationOptions = [bpoPagination];
  DefaultSortOptions = [booSortable];
  DefaultSearchOptions = [bsoSearch,bsoSearchOnEnterKey,bsoSearchTimeOut];
  DefaultTableOptions = [btoClickToSelect,btoSingleSelect,btoDetailViewByClick];


implementation


{ TTableDataLink }

constructor TTableDataLink.Create(aTable: TCustomDBBootstrapTableWidget);
begin
  FTable:=aTable;
end;

procedure TTableDataLink.ActiveChanged;
begin
  inherited ActiveChanged;

end;

{ TStylingClasses }

function TStylingClasses.GetReadonlyEditClass: String;
begin
  Result:=FReadonlyEditClass;
  if Result='' then
    Result:=InfoClass;
end;

function TStylingClasses.IsReadonlyStored: Boolean;
begin
  Result:=(FReadonlyEditClass<>'');
end;

constructor TStylingClasses.Create(aWidget: TCustomDBBootstrapTableWidget);
begin
  FWidget:=aWidget;
  ButtonClass:='btn btn-secondary btn-sm btn-outline';
  EditClass:='bi bi-pencil';
  DeleteClass:='bi bi-trash';
  InfoClass:='bi bi-info-circle';
  CheckBoxClass:='form-check-input';
end;

procedure TStylingClasses.Assign(Source: TPersistent);

Var
  IC : TStylingClasses absolute Source;

begin
  if Source is TStylingClasses then
    begin
    FCheckBoxClass:=IC.FCheckBoxClass;
    FButtonClass:=IC.FButtonClass;
    FDeleteClass:=IC.FDeleteClass;
    FEditClass:=IC.FEditClass;
    FInfoClass:=IC.FInfoClass;
    FReadonlyEditClass:=IC.FReadonlyEditClass;
    end
  else
    inherited Assign(Source);
end;


  { TBSTableColumn }

procedure TBSTableColumn.Assign(Source: TPersistent);
var
  Src: TBSTableColumn absolute source;
begin
  if Source is TBSTableColumn then
  begin
    FSelectable:=Src.Selectable;
    FieldName:=Src.FieldName;
    Title:=Src.Title;
    RenderMode:=Src.RenderMode;
    ButtonType:=Src.ButtonType;
    Width:=Src.Width;
    CSSClassName:=Src.CSSClassName;
    CheckBoxClassName:=Src.CheckBoxClassName;
    OnCustomFormat:=Src.OnCustomFormat;
    Searchable := Src.Searchable;
    Sortable := Src.Sortable;
    ButtonURL := Src.ButtonURL;
    ButtonURLTarget := Src.ButtonURLTarget;
    ButtonIconClass := Src.ButtonIconClass;
    Formatting := Src.Formatting;
    OnGetSortValue := Src.OnGetSortValue;
    ExtraAttributes := Src.ExtraAttributes;
  end
  else
    inherited;
end;

constructor TBSTableColumn.Create(aOwner: TCollection);
begin
  inherited;
  FRenderMode := crmText;
  FVisible := True;
  FSortable := True;
  FSearchable := True;
end;

function TBSTableColumn.GetDisplayName: String;
begin
  Result:=FieldName;
  if Result='' then
    Result:=Title;
  if Result='' then
    Result:=Inherited GetDisplayName;
end;

function TBSTableColumn.GetTitle: string;
begin
  Result := FTitle;
  if Result='' then
    Result:=FieldName;
end;

{ TBSTableColumns }

function TBSTableColumns.Add(const aName: string): TBSTableColumn;
begin
  if IndexOfColumn(aName) <> -1 then
    raise EBootstrapTable.CreateFmt('Column "%s" already exists!', [aName]);
  Result := Add();
  Result.FieldName := aName;
end;

function TBSTableColumns.Add: TBSTableColumn;
begin
  Result:=TBSTableColumn(inherited Add);
end;

function TBSTableColumns.ColumnByName(const aName: string): TBSTableColumn;
var
  iIdx: Integer;
begin
  iIdx := IndexOfColumn(aName);
  if iIdx > -1 then
    Result := DoGetColumn(iIdx)
  else
    Result := nil;
end;

function TBSTableColumns.DoGetColumn(const aIndex: Integer): TBSTableColumn;
begin
  Result := Items[aIndex] as TBSTableColumn;
end;

function TBSTableColumns.IndexOfColumn(const aName: string): Integer;
begin
  Result := Pred(Count);
  while (Result >= 0) and (not SameText(DoGetColumn(Result).FieldName, aName)) do
    Dec(Result);
end;

{ TCustomDBBootstrapTableWidget }



function TCustomDBBootstrapTableWidget.GetData: TJSArray;
begin
  Result:=Nil;
end;

function TCustomDBBootstrapTableWidget.GetDatasource : TDataSource;
begin
  Result:=FLink.DataSource;
end;

function TCustomDBBootstrapTableWidget.IsOptionsStored: Boolean;
begin
  Result:=Options<>DefaultTableOptions;
end;

function TCustomDBBootstrapTableWidget.IsPaginationOptionsStored: Boolean;
begin
  Result:=PaginationOptions<>DefaultPaginationOptions;
end;

function TCustomDBBootstrapTableWidget.IsSearchOptionsStored: Boolean;
begin
  Result:=SearchOptions<>DefaultSearchOptions;
end;

function TCustomDBBootstrapTableWidget.IsSortOPtionsStored: Boolean;
begin
  Result:=SortOptions<>DefaultSortOptions;
end;

function TCustomDBBootstrapTableWidget.IsViewOptionsStored: Boolean;
begin
  Result:=ViewOptions<>DefaultViewOptions;
end;


procedure TCustomDBBootstrapTableWidget.SetDatasource(AValue: TDataSource);
begin
  FLink.DataSource:=aValue
end;

procedure TCustomDBBootstrapTableWidget.SetStylingClasses(
  AValue: TStylingClasses);
begin
  if FStylingClasses=AValue then Exit;
  FStylingClasses.Assign(AValue);
end;



constructor TCustomDBBootstrapTableWidget.Create(aOwner: TComponent);
begin
  inherited;
  FColumns := TBSTableColumns.Create(Self,TBSTableColumn);
  FStylingClasses:=TStylingClasses.Create(Self);
  FLink:=TTableDataLink.Create(Self);
  FTableViewOptions:=DefaultViewOptions;
  FTablePaginationOptions:=DefaultPaginationOptions;
  FTableSortOptions:=DefaultSortOptions;
  FTableSearchOptions:=DefaultSearchOptions;
  FTableOptions:=DefaultTableOptions;
end;


destructor TCustomDBBootstrapTableWidget.Destroy;
begin
  FreeAndNil(FLink);
  FreeAndNil(FStylingClasses);
  FreeAndNil(FColumns);
  inherited;
end;



procedure TCustomDBBootstrapTableWidget.SetData(const aData: TJSArray);
begin
  if FData=aData then
    exit;
  FData:=aData;
end;


procedure TCustomDBBootstrapTableWidget.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TCustomDBBootstrapTableWidget.SetTableOptions(AValue: TBSTableOptions
  );
begin
  if FTableOptions=AValue then Exit;
  FTableOptions:=AValue;
end;

procedure TCustomDBBootstrapTableWidget.SetTablePaginationOptions(
  AValue: TBSTablePaginationOptions);
begin
  if FTablePaginationOptions=AValue then Exit;
  FTablePaginationOptions:=AValue;
end;

procedure TCustomDBBootstrapTableWidget.SetTableSearchOptions(
  AValue: TBSTableSearchOptions);
begin
  if FTableSearchOptions=AValue then Exit;
  FTableSearchOptions:=AValue;
end;

procedure TCustomDBBootstrapTableWidget.SetTableSortOptions(
  AValue: TBSTableSortOptions);
begin
  if FTableSortOptions=AValue then Exit;
  FTableSortOptions:=AValue;
end;

procedure TCustomDBBootstrapTableWidget.SetTableViewOptions(
  AValue: TBSTableViewOptions);
begin
  if FTableViewOptions=AValue then Exit;
  FTableViewOptions:=AValue;
end;



{ TDataTablesFieldMap }

function TDataTablesFieldMap.GetValueByName(S: String): JSValue;

begin
  if S<>'' then;
  Result:='';
end;

end.
