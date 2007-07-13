{
 Copyright (c) 2007 by Michael Van Canneyt.

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpdatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,inicol, inifiles, contnrs, db;

Type
  // Supported objects in this data dictionary
  TObjectType = (otUnknown,otDictionary,otTables,otTable,otFields,otField,otConnection,otTableData);
  TDDProgressEvent = Procedure(Sender : TObject; Const Msg : String) of Object;

  { TDDFieldDef }

  TDDFieldDef = Class(TIniCollectionItem)
  private
    FAlignMent: TAlignMent;
    FConstraint: string;
    FConstraintErrorMessage: string;
    FCustomConstraint: string;
    FDefault: String;
    FDefaultExpression: string;
    FDisplayLabel: string;
    FDisplayWidth: Longint;
    FFieldName: string;
    FFieldType: TFieldType;
    FHint: String;
    FPrecision: Integer;
    FReadOnly: Boolean;
    FRequired: Boolean;
    FSize: Integer;
    FVisible: Boolean;
    Function IsSizeStored : Boolean;
    Function IsPrecisionStored : Boolean;
  protected
    function GetSectionName: String; override;
    procedure SetSectionName(const Value: String); override;
  Public
    Constructor Create(ACollection : TCollection); override;
    Procedure ImportFromField(F : TField);
    Procedure ApplyToField(F : TField);
    Procedure Assign(Source : TPersistent);  override;
    Procedure SaveToIni(Ini: TCustomInifile; ASection : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; ASection : String); override;
  Published
    property FieldType : TFieldType Read FFieldType Write FFieldType;
    property AlignMent : TAlignMent Read FAlignMent write FAlignment default taLeftJustify;
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ConstraintErrorMessage: string read FConstraintErrorMessage write FConstraintErrorMessage;
    Property DBDefault : String Read FDefault Write FDEfault;
    property DefaultExpression: string read FDefaultExpression write FDefaultExpression;
    property DisplayLabel : string read FDisplayLabel write FDisplayLabel;
    property DisplayWidth: Longint read FDisplayWidth write FDisplayWidth;
    property FieldName: string read FFieldName write FFieldName;
    property Constraint: string read FConstraint write FConstraint;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property Visible: Boolean read FVisible write FVisible default True;
    Property Size : Integer Read FSize Write FSize Stored IsSizeStored;
    Property Precision : Integer Read FPrecision Write FPrecision Stored IsPrecisionStored;
    Property Hint : String Read FHint Write FHint;
  end;
  
  { TDDFieldDefs }

  TDDFieldDefs = Class(TIniCollection)
  private
    FTableName: String;
    function GetField(Index : Integer): TDDFieldDef;
    procedure SetField(Index : Integer; const AValue: TDDFieldDef);
    procedure SetTableName(const AValue: String);
  Public
    Constructor Create(ATableName : String);
    Function AddField(AFieldName: String = '') : TDDFieldDef;
    Function IndexOfField(AFieldName : String) : Integer;
    Function FindField(AFieldName : String) : TDDFieldDef;
    Function FieldByName(AFieldName : String) : TDDFieldDef;
    Property Fields[Index : Integer] : TDDFieldDef Read GetField Write SetField; default;
    Property TableName : String Read FTableName Write SetTableName;
  end;
  
  { TDDIndexDef }
  TDDIndexDef = Class(TIniCollectionItem)
  private
    FCaseinsFields: string;
    FDescFields: string;
    FExpression: string;
    FFields: string;
    FIndexName: String;
    FOptions: TIndexOptions;
    FSource: string;
  protected
    function GetSectionName: String; override;
    procedure SetSectionName(const Value: String); override;
  Published
    Property IndexName : String Read FIndexName Write FIndexName;
    property Expression: string read FExpression write FExpression;
    property Fields: string read FFields write FFields;
    property CaseInsFields: string read FCaseinsFields write FCaseInsFields;
    property DescFields: string read FDescFields write FDescFields;
    property Options: TIndexOptions read FOptions write FOptions;
    property Source: string read FSource write FSource;
  end;
  
  { TDDIndexDefs }

  TDDIndexDefs = Class(TIniCollection)
  private
    FTableName : String;
    function GetIndex(Index : Integer): TDDIndexDef;
    procedure SetIndex(Index : Integer; const AValue: TDDIndexDef);
    procedure SetTableName(const AValue: String);
  Public
    Constructor Create(ATableName : String);
    Property TableName : String Read FTableName Write SetTableName;
    Property Indexes[Index : Integer] : TDDIndexDef Read GetIndex Write SetIndex; default;
  end;

  { TDDTableDef }

  TDDTableDef = Class(TIniCollectionItem)
  private
    FFieldDefs: TDDFieldDefs;
    FIndexDefs: TDDIndexDefs;
    FPrimaryKeyName: String;
    FTableName: String;
    function GetOnProgress: TDDProgressEvent;
    function GetPrimaryKeyName: String;
    procedure SetTableName(const AValue: String);
  protected
    function GetSectionName: String; override;
    procedure SetSectionName(const Value: String); override;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Function ImportFromDataset(Dataset : TDataSet; DoClear : Boolean = False; UpdateExisting : Boolean = True) : Integer;
    Procedure ApplyToDataset(Dataset : TDataset);
    Function AddField(AFieldName : String = '') : TDDFieldDef;
    Procedure SaveToIni(Ini: TCustomInifile; ASection : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; ASection : String); override;
    Property Fields : TDDFieldDefs Read FFieldDefs;
    Property Indexes : TDDIndexDefs Read FIndexDefs;
    Property OnProgress : TDDProgressEvent Read GetOnProgress;
  Published
    Property TableName : String Read FTableName Write SetTableName;
    Property PrimaryKeyConstraintName : String Read GetPrimaryKeyName Write FPrimaryKeyName;
  end;
  
  { TDDTableDefs }

  TDDTableDefs = Class(TIniCollection)
  private
    FOnProgress: TDDProgressEvent;
    function GetTable(Index : Integer): TDDTableDef;
    procedure SetTable(Index : Integer; const AValue: TDDTableDef);
  Public
    Function AddTable(ATableName : String = '') : TDDTableDef;
    Function IndexOfTable(ATableName : String) : Integer;
    Function FindTable(ATableName : String) : TDDTableDef;
    Function TableByName(ATableName : String) : TDDTableDef;
    Property Tables[Index : Integer] : TDDTableDef Read GetTable Write SetTable; default;
    Property OnProgress : TDDProgressEvent Read FOnProgress Write FOnProgress;
  end;


  { TFPDataDictionary }

  TFPDataDictionary = Class(TPersistent)
  private
    FDDName: String;
    FFileName: String;
    FOnProgress: TDDProgressEvent;
    FTables: TDDTableDefs;
    procedure SetOnProgress(const AValue: TDDProgressEvent);
  Public
    Constructor Create;
    Destructor Destroy; override;
    Procedure SaveToFile(AFileName : String; KeepBackup: Boolean = True);
    Procedure SaveToIni(Ini : TCustomIniFile; ASection : String); virtual;
    Procedure LoadFromFile(AFileName : String);
    Procedure LoadFromIni(Ini : TCustomIniFile; ASection : String); virtual;
    Property Tables : TDDTableDefs Read FTables;
    Property FileName : String Read FFileName;
    Property Name : String Read FDDName Write FDDName;
    Property OnProgress : TDDProgressEvent Read FOnProgress Write SetOnProgress;
  Published
    // Using name confuses the object inspector grid.
    Property DataDictionaryName : String Read FDDName Write FDDName;
  end;


  { TFPDDFieldList }

  TFPDDFieldList = Class(TObjectList)
  private
    function GetFieldDef(Index : Integer): TDDFieldDef;
    procedure SetFieldDef(Index : Integer; const AValue: TDDFieldDef);
  Public
    Constructor CreateFromTableDef(TD : TDDTableDef);
    Constructor CreateFromFieldDefs(FD : TDDFieldDefs);
    Property FieldDefs[Index : Integer] : TDDFieldDef Read GetFieldDef Write SetFieldDef; default;
  end;
  
  

  
  { TFPDDSQLEngine }
  TSQLEngineOption = (eoLineFeedAfterField,eoUseOldInWhereParams,eoAndTermsInBrackets,eoQuoteFieldNames,eoLineFeedAfterAndTerm,eoAddTerminator);
  TSQLEngineOptions = Set of TSQLEngineOption;
  

  TFPDDSQLEngine = Class(TPersistent)
  private
    FFieldQuoteChar: Char;
    FIndent: Integer;
    FMaxLineLength: Integer;
    FLastLength: integer;
    FOptions: TSQLEngineOptions;
    FTableDef: TDDTableDef;
    FNoIndent : Boolean;
    FTerminatorChar : Char;
  Protected
    procedure CheckTableDef;
    Procedure NoIndent;
    Procedure ResetLine;
    Procedure AddToStringLN(Var Res : String; S : String);
    Procedure AddToString(Var Res : String; S : String);
    Procedure FixUpStatement(var Res : String);
    Procedure AddWhereClause(Var Res : String; FieldList: TFPDDFieldList; UseOldParam:Boolean);
    Function CreateAndTerm(FD : TDDFieldDef; UseOldParam : Boolean): string;
    // Primitives. Override for engine-specifics
    Procedure AddFieldString(Var Res: String; S : String);
    Function FieldNameString(FD : TDDFieldDef) : string; virtual;
    Function TableNameString(TD : TDDTableDef) : string; virtual;
    Function FieldParamString(FD : TDDFieldDef; UseOldParam : Boolean) : string; virtual;
    Function FieldTypeString(FD : TDDFieldDef) : String; virtual;
    Function FieldDefaultString(FD : TDDFieldDef) : String; virtual;
    Function FieldCheckString(FD : TDDFieldDef) : String; virtual;
    Function FieldDeclarationString(FD : TDDFieldDef) : String; virtual;
    Property FieldQuoteChar : Char Read FFieldQuoteChar Write FFieldQuoteChar;
    Property TerminatorChar : Char Read FTerminatorChar Write FTerminatorChar;
  Public
    Constructor Create; virtual;
    function  CreateWhereSQL(Var Res : String; FieldList: TFPDDFieldList; UseOldParam:Boolean): String;
    Procedure CreateSelectSQLStrings(FieldList,KeyFields : TFPDDFieldList; SQL : TStrings);
    Procedure CreateInsertSQLStrings(FieldList : TFPDDFieldList; SQL : TStrings);
    Procedure CreateUpdateSQLStrings(FieldList,KeyFields : TFPDDFieldList; SQL : TStrings);
    Procedure CreateDeleteSQLStrings(KeyFields : TFPDDFieldList; SQL : TStrings);
    Procedure CreateCreateSQLStrings(Fields,KeyFields : TFPDDFieldList; SQL : TStrings);
    Procedure CreateCreateSQLStrings(KeyFields : TFPDDFieldList; SQL : TStrings);
    Function  CreateSelectSQL(FieldList,KeyFields : TFPDDFieldList) : String; virtual;
    Function  CreateInsertSQL(FieldList : TFPDDFieldList) : String; virtual;
    Function  CreateUpdateSQL(FieldList,KeyFields : TFPDDFieldList) : String; virtual;
    Function  CreateDeleteSQL(KeyFields : TFPDDFieldList) : String; virtual;
    Function  CreateCreateSQL(Fields,KeyFields : TFPDDFieldList) : String; virtual;
    Function  CreateCreateSQL(KeyFields : TFPDDFieldList) : String; virtual;
    Property TableDef : TDDTableDef Read FTableDef Write FTableDef;
  Published
    Property MaxLineLength : Integer Read FMaxLineLength Write FMaxLineLength default 72;
    Property Indent : Integer Read FIndent Write FIndent default 2;
    Property Options : TSQLEngineOptions Read FOptions Write FOptions;
  end;
  
  { TFPDDEngine }
  TFPDDEngineCapability =(ecImport,ecCreateTable,ecViewTable, ectableIndexes, ecRunQuery, ecRowsAffected);
  TFPDDEngineCapabilities = set of TFPDDEngineCapability;
  {
    to avoid dependencies on GUI elements in the data dictionary engines,
    connection string dialogs must be registered separately.

    TGetConnectionEvent is the callback prototype for such a dialog
  }
  TGetConnectionEvent = Procedure(Sender: TObject; Var Connection : String) of object;

  TFPDDEngine = Class(TComponent)
  private
    FOnProgress: TDDProgressEvent;
  Protected
    FConnected: Boolean;
    FConnectString: String;
    Procedure DoProgress(Const Msg : String);
  Public
    Destructor Destroy; override;
    Function GetConnectString : String; virtual;
    Function ImportTables(Tables : TDDTableDefs; List : TStrings; UpdateExisting : Boolean) : Integer;
    // Mandatory for all data dictionary engines.
    Class function Description : string; virtual; abstract;
    Class function DBType : String; virtual; abstract;
    Class function EngineCapabilities : TFPDDEngineCapabilities; virtual;
    Function Connect(const ConnectString : String) : Boolean; virtual; abstract;
    Procedure Disconnect ; virtual; abstract;
    Function GetTableList(List : TStrings) : Integer; virtual; abstract;
    Function ImportFields(Table : TDDTableDef) : Integer; virtual; abstract;
    // Override depending on capabilities
    Procedure CreateTable(Table : TDDTableDef); virtual;
    // Should not open the dataset.
    Function ViewTable(Const TableName: String; DatasetOwner : TComponent) : TDataset; virtual;
    // Run a non-select query. If possible, returns the number of modified records.
    Function RunQuery(SQL : String) : Integer; Virtual;
    // Create a select query TDataset. Do not open the resulting dataset.
    Function CreateQuery(SQL : String; DatasetOwner : TComponent) : TDataset; Virtual;
    // Assign a select query and open the resulting dataset.
    Procedure SetQueryStatement(SQL : String; AQuery : TDataset); Virtual;
    // Override if a better implementation exists.
    Function CreateSQLEngine : TFPDDSQLEngine; virtual;
    Property OnProgress : TDDProgressEvent Read FOnProgress Write FOnProgress;
    Property ConnectString : String Read FConnectString;
    Property Connected : Boolean Read FConnected Write FConnected;
  end;
  TFPDDEngineClass = Class of TFPDDEngine;


  EDataDict = Class(Exception);

Procedure RegisterDictionaryEngine(AEngine :TFPDDEngineClass);
Procedure RegisterConnectionStringCallback(Const AName: String; CallBack : TGetConnectionEvent);
Procedure UnRegisterDictionaryEngine(AEngine :TFPDDEngineClass);
Function  GetDictionaryEngineList(List : TStrings) : Integer;
Function  GetDictionaryEngineInfo(Const AName : String; Var ADescription,ADBType: String; var ACapabilities : TFPDDEngineCapabilities) : boolean;
Function  CreateDictionaryEngine(AName : String; AOWner : TComponent) : TFPDDEngine;

Var
  DefaultDDExt : String = '.fpd';
  
  // Default values for SQL Engine properties.
  
  DefaultSQLEngineOptions : TSQLEngineOptions
                          = [eoLineFeedAfterField,eoUseOldInWhereParams,
                             eoAndTermsInBrackets,eoLineFeedAfterAndTerm];
                             
  DefaultSQLEngineIndent     : Integer = 2;
  DefaultSQLEngineLineLength : Integer = 72;
  DefaultSQLTerminatorChar   : Char = ';';
  DefaultSQLFieldQuoteChar   : Char = '"';
  
implementation

uses typinfo;

{ ---------------------------------------------------------------------
  Constants, not to be localized
  ---------------------------------------------------------------------}

Const
  // Datadict saving
  SDataDict                 = 'FPDataDict';
  KeyDataDictName           = 'DataDictName';

  // Tables Saving
  SDataDictTables           = SDataDict+'_Tables';
  KeyTableName              = 'TableName';
  KeyPrimaryKeyConstraint   = 'PrimaryKeyConstraint';
  
  // Fields Saving
  SFieldSuffix              = '_Fields';
  SIndexSuffix              = '_Indices';
  KeyAlignMent              = 'AlignMent';
  KeyCustomConstraint       = 'CustomConstraint';
  KeyConstraintErrorMessage = 'ConstraintErrorMessage';
  KeyDBDefault              = 'DBDefault';
  KeyDefaultExpression      = 'DefaultExpression';
  KeyDisplayLabel           = 'DisplayLabel';
  KeyDisplayWidth           = 'DisplayWidth';
  KeyFieldName              = 'FieldName';
  KeyConstraint             = 'Constraint';
  KeyReadOnly               = 'ReadOnly';
  KeyRequired               = 'Required';
  KeyVisible                = 'Visible';
  KeySize                   = 'Size';
  KeyFieldType              = 'FieldType';
  KeyHint                   = 'Hint';

  // SQL Keywords
  SSelect      = 'SELECT';
  SFrom        = 'FROM';
  SWhere       = 'WHERE';
  SInsertInto  = 'INSERT INTO';
  SUpdate      = 'UPDATE';
  SSet         = 'SET';
  SDeleteFrom  = 'DELETE FROM';
  SAnd         = 'AND';
  SOLD         = 'OLD';
  SValues      = 'VALUES';
  SCreateTable = 'CREATE TABLE';
  SNotNull     = 'NOT NULL';
  SDefault     = 'DEFAULT';
  SCheck       = 'CHECK';  // Check constraint
  SPrimaryKey  = 'PRIMARY KEY';
  SConstraint  = 'CONSTRAINT';

  SQLFieldTypes : Array[TFieldType] of string = (
    '', 'VARCHAR', 'SMALLINT', 'INT', 'SMALLINT',
    'BOOL', 'FLOAT', 'DECIMAL','DECIMAL','DATE', 'TIME', 'TIMESTAMP',
    '', '', 'INT', 'BLOB', 'BLOB', 'BLOB', 'BLOB',
    '', '', '', '', 'CHAR',
    'CHAR', 'DOUBLE PRECISION', '', '', '',
    '', '', '', '', '',
    '', '', 'TIMESTAMP', 'DECIMAL','CHAR','BLOB');
    
{ ---------------------------------------------------------------------
  Constants which can be localized
  ---------------------------------------------------------------------}

Resourcestring
  SErrFieldNotFound           = '"%s": Field "%s" not found.';
  SErrTableNotFound           = 'Table "%s" not found.';
  SErrDuplicateTableName      = 'Duplicate table name: "%s"';
  SErrDuplicateFieldName      = '"%s": Duplicate field name: "%s"';
  SNewTable                   = 'NewTable';
  SNewField                   = 'NewField';
  SErrNoFileName              = 'No filename given for save';
  SErrNotRegistering          = 'Not registering data dictionary engine "%s": %s';
  SErrNoEngineCapabilities    = 'It reports no capabilities.';
  SErrNoEngineDBType          = 'It reports no database type';
  SErrNoEngineDescription     = 'It reports no description';
  SErrUnknownEngine           = 'Unknown datadictionary: "%s"';
  SErrMissingTableDef         = 'Cannot perform this operation without tabledef.';
  SErrFieldTypeNotSupported   = 'Field type "%s" is not supported in this SQL dialect';
  SErrNoConnectionDialog      = 'No connection dialog registered for data dictionary engine "%s".';
  SDDImportingTable           = 'Importing table definition for table "%s"';
  SErrCreateTableNotSupported = 'Creating tables is not supported by the "%s" engine.';
  SErrViewTableNotSupported   = 'Viewing tables is not supported by the "%s" engine.';
  SErrRunQueryNotSupported    = 'Running queries is not supported by the "%s" engine.';
  SErrOpenQueryNotSupported   = 'Running and opening SELECT queries is not supported by the "%s" engine.';
  SErrSetQueryStatementNotSupported   = 'Setting the SQL statement is not supported by the "%s" engine.';
  SSavingFieldsFrom           = 'Saving fields from %s';
  SLoadingFieldsFrom          = 'Loading fields from %s';
  
{ ---------------------------------------------------------------------
  Dictionary Engine registration
  ---------------------------------------------------------------------}

Var
  DDEngines : TStringList;
  
Type

  { TEngineRegistration }

  TEngineRegistration = Class(TObject)
  Private
    FEngine : TFPDDEngineClass;
    FCallBack : TGetConnectionEvent;
  Public
    Constructor Create(AEngine : TFPDDEngineClass);
  end;

{ TEngineRegistration }

constructor TEngineRegistration.Create(AEngine: TFPDDEngineClass);
begin
  FEngine:=AEngine;
end;

procedure RegisterDictionaryEngine(AEngine: TFPDDEngineClass);
begin
  If (AEngine.EngineCapabilities=[]) then
     Raise EDataDict.CreateFmt(SErrNotRegistering,[AEngine.ClassName,SErrNoEngineCapabilities]);
  If (AEngine.DBType='') then
     Raise EDataDict.CreateFmt(SErrNotRegistering,[AEngine.ClassName,SErrNoEngineDBType]);
  If (AEngine.Description='') then
     Raise EDataDict.CreateFmt(SErrNotRegistering,[AEngine.ClassName,SErrNoEngineDescription]);
  If not assigned(DDEngines) then
    begin
    DDEngines:=TStringList.Create;
    DDEngines.Sorted:=true;
    DDEngines.Duplicates:=dupError;
    end;
  DDEngines.AddObject(Aengine.ClassName,TEngineRegistration.Create(AEngine));
end;


procedure UnRegisterDictionaryEngine(AEngine: TFPDDEngineClass);

Var
  I : Integer;
  
begin
  If Assigned(DDEngines) then
    begin
    I:=DDEngines.IndexOf(Aengine.ClassName);
    If (i<>-1) then
      begin
      DDEngines.Objects[i].Free;
      DDEngines.Delete(i);
      end;
    if (DDEngines.Count=0) then
      FreeAndNil(DDEngines);
    end;
end;

function GetDictionaryEngineList(List: TStrings): Integer;
begin
  If Not Assigned(DDEngines) then
    Result:=0
  else
    begin
    If Assigned(List) then
      List.Text:=DDEngines.Text;
    Result:=DDEngines.Count;
    end;
end;

Function IndexOfDDEngine(Const AName: String) : Integer;

begin
  If Assigned(DDEngines) then
    Result:=DDEngines.IndexOf(AName)
  else
    Result:=-1;
end;

Function FindEngineRegistration(Const AName : String) : TEngineRegistration;

Var
   I : integer;

begin
  I:=IndexOfDDEngine(AName);
  if (I<>-1) then
    Result:=TEngineRegistration(DDEngines.Objects[i])
  else
    Result:=Nil;
end;

Function GetEngineRegistration(Const AName : String) : TEngineRegistration;

begin
  Result:=FindEngineRegistration(AName);
  If (Result=Nil) then
    Raise EDataDict.CreateFmt(SErrUnknownEngine,[AName]);
end;

Function FindDictionaryClass(Const AName : String) : TFPDDEngineClass;

Var
   R : TEngineRegistration;

begin
  R:=FindEngineRegistration(AName);
  If (R=Nil) then
    Result:=Nil
  else
    Result:=R.FEngine;
end;

Function GetDictionaryClass(Const AName : String) : TFPDDEngineClass;

begin
  Result:=GetEngineRegistration(AName).FEngine;
end;

procedure RegisterConnectionStringCallback(Const AName : String;
  CallBack: TGetConnectionEvent);
begin
  GetEngineRegistration(AName).FCallBack:=CallBack;
end;

function GetEngineConnectionStringCallBack(Const AName : String) : TGetConnectionEvent;

begin
  Result:=GetEngineRegistration(AName).FCallBack;
end;

Function  GetDictionaryEngineInfo(Const AName : String; Var ADescription,ADBType: String;var ACapabilities : TFPDDEngineCapabilities) : boolean;

Var
  DDEC : TFPDDEngineClass;
  
begin
  DDEC:=FindDictionaryClass(AName);
  Result:=DDEC<>Nil;
  If Result then
    begin
    ADescription:=DDEC.Description;
    ADBType:=DDEC.DBType;
    ACapabilities:=DDEC.EngineCapabilities;
    end;
end;

function CreateDictionaryEngine(AName: String; AOWner : TComponent): TFPDDEngine;

begin
  Result:=GetDictionaryClass(AName).Create(AOwner);
end;

{ ---------------------------------------------------------------------
  TDDFieldDef
  ---------------------------------------------------------------------}
  
function TDDFieldDef.IsSizeStored: Boolean;
begin
  Result:=FieldType in [ftUnknown, ftString, ftBCD,
    ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftFixedChar,
    ftWideString,ftArray, ftOraBlob, ftOraClob, ftFMTBcd];
end;

function TDDFieldDef.IsPrecisionStored: Boolean;
begin
  Result:=FieldType in [ftFloat,ftBCD,ftFMTBCD];
end;

function TDDFieldDef.GetSectionName: String;
begin
  Result:=FFieldName;
end;

procedure TDDFieldDef.SetSectionName(const Value: String);
begin
  FFieldName:=Value;
end;

constructor TDDFieldDef.Create(ACollection: TCollection);
begin
  Inherited;
  FVisible:=True;
  FAlignMent:=taLeftJustify;
end;

procedure TDDFieldDef.ImportFromField(F: TField);
begin
  FieldName:=F.FieldName;
  FieldType:=F.DataType;
  If IsSizeStored then
    Size:=F.Size;
  If IsPrecisionStored then
    begin
    If F is TBCDFIeld then
      Precision:=TBCDField(F).Precision
    else if F is TFloatField then
      Precision:=TFloatField(F).Precision;
    end;
  AlignMent:=F.AlignMent;
  DisplayWidth:=F.DisplayWidth;
  CustomConstraint:=F.CustomConstraint;
  ConstraintErrorMessage:=F.ConstraintErrorMessage;
  DefaultExpression:=F.DefaultExpression;
  DisplayLabel:=F.DisplayLabel;
  ReadOnly:=F.ReadOnly;
  Required:=F.Required;
  Visible:=F.Visible;
end;

procedure TDDFieldDef.ApplyToField(F: TField);
begin
{ // Normally, these should never be assigned...
  F.FieldName              := FieldName;
  F.DataType               := FieldType;
  If IsSizeStored then
    F.Size:=Size;
}
  F.AlignMent              := AlignMent;
  F.DisplayWidth           := DisplayWidth;
  F.CustomConstraint       := CustomConstraint;
  F.ConstraintErrorMessage := ConstraintErrorMessage;
  F.DefaultExpression      := DefaultExpression;
  F.DisplayLabel           := DisplayLabel;
  F.ReadOnly               := ReadOnly;
  F.Required               := Required;
  F.Visible                := Visible;

end;

procedure TDDFieldDef.Assign(Source: TPersistent);

Var
  DF : TDDFieldDef;
  
begin
  if Source is TField then
    ImportFromField(TField(Source))
  else If Source is TDDFieldDef then
    begin
    DF:=TDDFieldDef(Source);
    FieldType:=DF.FieldType;
    If IsSizeStored then
      Size:=DF.Size;
    AlignMent:=DF.AlignMent;
    DisplayWidth:=DF.DisplayWidth;
    CustomConstraint:=DF.CustomConstraint;
    ConstraintErrorMessage:=DF.ConstraintErrorMessage;
    DefaultExpression:=DF.DefaultExpression;
    DBDefault:=DF.DBDefault;
    DisplayLabel:=DisplayLabel;
    FieldName:=DF.FieldName;
    Constraint:=DF.Constraint;
    Hint:=DF.Hint;
    ReadOnly:=DF.ReadOnly;
    Required:=DF.Required;
    Visible:=DF.Visible;
    end
  else
    Inherited;
end;

procedure TDDFieldDef.SaveToIni(Ini: TCustomInifile; ASection: String);
begin
  With Ini do
    begin
    WriteInteger(ASection,KeyFieldType,Ord(Fieldtype));
    If IsSizeStored then
      WriteInteger(ASection,KeySize,Ord(Size));
    WriteInteger(ASection,KeyAlignMent,Ord(AlignMent));
    WriteInteger(ASection,KeyDisplayWidth,DisplayWidth);
    WriteString(ASection,KeyCustomConstraint,CustomConstraint);
    WriteString(ASection,KeyConstraintErrorMessage,ConstraintErrorMessage);
    WriteString(ASection,KeyDefaultExpression,DefaultExpression);
    WriteString(ASection,KeyDBDefault,DBDefault);
    WriteString(ASection,KeyDisplayLabel,DisplayLabel);
    WriteString(ASection,KeyFieldName,FieldName);
    WriteString(ASection,KeyConstraint,Constraint);
    WriteString(ASection,KeyHint,Hint);
    WriteBool(ASection,KeyReadOnly,ReadOnly);
    WriteBool(ASection,KeyRequired,Required);
    WriteBool(ASection,KeyVisible,Visible);
    end;
end;

procedure TDDFieldDef.LoadFromIni(Ini: TCustomInifile; ASection: String);

begin
  With Ini do
    begin
    FieldType:=TFieldType(ReadInteger(ASection,KeyFieldType,Ord(Fieldtype)));
    If IsSizeStored then
      Size:=ReadInteger(ASection,KeySize,0);
    Alignment:=TAlignment(ReadInteger(ASection,KeyAlignMent,Ord(AlignMent)));
    DisplayWidth:=ReadInteger(ASection,KeyDisplayWidth,DisplayWidth);
    CustomConstraint:=ReadString(ASection,KeyCustomConstraint,CustomConstraint);
    ConstraintErrorMessage:=ReadString(ASection,KeyConstraintErrorMessage,ConstraintErrorMessage);
    DefaultExpression:=ReadString(ASection,KeyDefaultExpression,DefaultExpression);
    DBDefault:=ReadString(ASection,KeyDBDefault,DBDefault);
    DisplayLabel:=ReadString(ASection,KeyDisplayLabel,DisplayLabel);
    FieldName:=ReadString(ASection,KeyFieldName,FieldName);
    Constraint:=ReadString(ASection,KeyConstraint,Constraint);
    Hint:=ReadString(ASection,KeyHint,Hint);
    ReadOnly:=ReadBool(ASection,KeyReadOnly,ReadOnly);
    Required:=ReadBool(ASection,KeyRequired,Required);
    Visible:=ReadBool(ASection,KeyVisible,Visible);
    end;
end;

{ ---------------------------------------------------------------------
  TDDFieldDefs
  ---------------------------------------------------------------------}

procedure TDDFieldDefs.SetTableName(const AValue: String);
begin
  FTableName:=AValue;
  FSectionPrefix:=AValue;
  GlobalSection:=AValue+SFieldSuffix;
end;

function TDDFieldDefs.GetField(Index : Integer): TDDFieldDef;
begin
  Result:=TDDFieldDef(Items[Index]);
end;

procedure TDDFieldDefs.SetField(Index : Integer; const AValue: TDDFieldDef);
begin
  Items[Index]:=AValue;
end;

constructor TDDFieldDefs.Create(ATableName: String);
begin
  Inherited Create(TDDFieldDef);
  FPrefix:='Field';
  TableName:=ATableName;
end;

function TDDFieldDefs.AddField(AFieldName: String): TDDFieldDef;

Var
  I : Integer;

begin
  If (AFieldName<>'') and (IndexOfField(AFieldName)<>-1) then
    Raise EDataDict.CreateFmt(SErrDuplicateFieldName,[TableName,AFieldName]);
  If (AFieldName='') then
    begin
    I:=0;
    Repeat
      Inc(I);
      AFieldName:=SNewField+IntToStr(i);
    Until (IndexOfField(AFieldName)=-1);
    end;
  Result:=Add as TDDFieldDef;
  Result.FieldName:=AFieldName;
end;

function TDDFieldDefs.IndexOfField(AFieldName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetField(Result).FieldName,AFieldName)<>0) do
    Dec(Result)
end;

function TDDFieldDefs.FindField(AFieldName: String): TDDFieldDef;

Var
  I : integer;
  
begin
  I:=IndexOfField(AFieldName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetField(I);
end;

function TDDFieldDefs.FieldByName(AFieldName: String): TDDFieldDef;
begin
  Result:=FindField(AFieldName);
  If Result=Nil then
    Raise EDatadict.CreateFmt(SErrFieldNotFound,[TableName,AFieldName]);
end;

{ ---------------------------------------------------------------------
  TDDTableDef
  ---------------------------------------------------------------------}
  
  
procedure TDDTableDef.SetTableName(const AValue: String);
begin
  FTableName:=AValue;
  FFieldDefs.TableName:=AValue;
end;

function TDDTableDef.GetPrimaryKeyName: String;
begin
  Result:=Tablename+'_PK';
end;

function TDDTableDef.GetOnProgress: TDDProgressEvent;
begin
  Result:=Nil;
  If (Collection Is TDDTableDefs) then
    Result:=(Collection As TDDTableDefs).OnProgress;
end;

function TDDTableDef.GetSectionName: String;
begin
  Result:=FTableName;
end;

procedure TDDTableDef.SetSectionName(const Value: String);
begin
  TableName:=Value;
end;

constructor TDDTableDef.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFieldDefs:=TDDFieldDefs.Create('NewTable');
  FIndexDefs:=TDDIndexDefs.Create('NewTable');
end;

destructor TDDTableDef.Destroy;

begin
  FreeAndNil(FFieldDefs);
  FreeAndNil(FIndexDefs);
  inherited Destroy;
end;

Function TDDTableDef.ImportFromDataset(Dataset: TDataSet; DoClear : Boolean = False; UpdateExisting : Boolean = True) : Integer;

Var
  I  : Integer;
  FD : TDDFieldDef;
  F  : TField;
  
begin
  if DoClear then
    FFieldDefs.Clear;
  For I:=0 to Dataset.Fields.Count-1 do
    begin
    F:=Dataset.Fields[i];
    FD:=FFieldDefs.FindField(F.FieldName);
    If (FD=Nil) then
      FD:=FFieldDefs.AddField(F.FieldName)
    else if not UpdateExisting then
      FD:=Nil;
    if (FD<>Nil) then
      begin
      Inc(Result);
      FD.ImportFromField(F);
      end;
    end;
end;

procedure TDDTableDef.ApplyToDataset(Dataset: TDataset);

var
  I  : integer;
  FD : TDDFieldDef;
  F  : TField;
  
begin
  For I:=0 to Dataset.FieldCount-1 do
    begin
    F:=Dataset.Fields[i];
    FD:=FFieldDefs.FieldByName(F.FieldName);
    If (FD<>Nil) then
      FD.ApplyToField(F);
    end;
end;

function TDDTableDef.AddField(AFieldName: String): TDDFieldDef;
begin
  Result:=Fields.AddField(AFieldName);
end;

procedure TDDTableDef.SaveToIni(Ini: TCustomInifile; ASection: String);
begin
  With Ini do
    begin
    WriteString(ASection,KeyTableName,TableName);
    WriteString(ASection,KeyPrimaryKeyConstraint,FPRimaryKeyName);
    end;
  If Assigned(OnProgress) then
    OnProgress(Self,Format(SSavingFieldsFrom,[TableName]));
  FFieldDefs.SaveToIni(Ini,ASection+SFieldSuffix);
  FIndexDefs.SaveToIni(Ini,ASection+SIndexSuffix);
end;

procedure TDDTableDef.LoadFromIni(Ini: TCustomInifile; ASection: String);
begin
  With Ini do
    begin
    TableName:=ReadString(ASection,KeyTableName,TableName);
    FPrimaryKeyName:=ReadString(ASection,KeyPrimaryKeyConstraint,'');
    end;
  If Assigned(OnProgress) then
    OnProgress(Self,Format(SLoadingFieldsFrom,[TableName]));
  FFieldDefs.LoadFromIni(Ini,ASection+SFieldSuffix);
  FIndexDefs.LoadFromIni(Ini,ASection+SIndexSuffix);
end;

{ ---------------------------------------------------------------------
  TDDTableDefs
  ---------------------------------------------------------------------}

function TDDTableDefs.GetTable(Index : Integer): TDDTableDef;
begin
  Result:=TDDTableDef(Items[Index]);
end;

procedure TDDTableDefs.SetTable(Index : Integer; const AValue: TDDTableDef);
begin
  Items[Index]:=AValue;
end;

function TDDTableDefs.AddTable(ATableName: String): TDDTableDef;

Var
  I : Integer;
  
begin
  If (ATableName<>'') and (IndexOfTable(ATableName)<>-1) then
    Raise EDataDict.CreateFmt(SErrDuplicateTableName,[ATableName]);
  If (ATableName='') then
    begin
    I:=0;
    Repeat
      Inc(I);
      ATAbleName:=SNewTable+IntToStr(i);
    Until (IndexOfTable(ATableName)=-1);
    end;
  Result:=Add as TDDTableDef;
  Result.TableName:=ATableName;
end;

function TDDTableDefs.IndexOfTable(ATableName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetTable(Result).TableName,ATableName)<>0) do
    Dec(Result)
end;

function TDDTableDefs.FindTable(ATableName: String): TDDTableDef;

Var
  I : integer;

begin
  I:=IndexOfTable(ATableName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetTable(I);
end;

function TDDTableDefs.TableByName(ATableName: String): TDDTableDef;
begin
  Result:=FindTable(ATableName);
  If Result=Nil then
    Raise EDatadict.CreateFmt(SErrTableNotFound,[ATableName]);
end;

{ ---------------------------------------------------------------------
  TDatadictionary
  ---------------------------------------------------------------------}

procedure TFPDataDictionary.SetOnProgress(const AValue: TDDProgressEvent);
begin
  FOnProgress:=AValue;
  FTables.OnProgress:=FOnProgress;
end;

constructor TFPDataDictionary.Create;
begin
  FTables:=TDDTableDefs.Create(TDDTableDef);
end;

destructor TFPDataDictionary.Destroy;
begin
  FreeAndNil(FTables);
  inherited Destroy;
end;

procedure TFPDataDictionary.SaveToFile(AFileName: String; KeepBackup: Boolean = True);

Var
  Ini : TMemIniFile;

begin
  If (AFileName='') then
    AFileName:=FFileName;
  if (AFileName='') and (Name<>'') then
    AFileName:=Name+DefaultDDExt;
  if (AFileName='') then
    Raise EDataDict.Create(SErrNoFileName);
  If FileExists(AFileName) then
    If KeepBackup then
      RenameFile(AFileName,AFileName+'.bak')
    else
      DeleteFile(AFileName);
  Ini:=TMemIniFile.Create(AFileName);
  try
    SaveToIni(Ini,SDataDict);
    Ini.UpdateFile;
    FFileName:=AFileName;
  finally
    FreeAndNil(Ini);
  end;
end;

procedure TFPDataDictionary.SaveToIni(Ini: TCustomIniFile; ASection: String);
begin
  Ini.WriteString(ASection,KeyDataDictName,Name);
  FTables.SaveToIni(Ini,SDatadictTables);
end;

procedure TFPDataDictionary.LoadFromFile(AFileName: String);

Var
  Ini : TMemInifile;

begin
  if (AFileName='') then
    Raise EDataDict.Create(SErrNoFileName);
  Ini:=TMemIniFile.Create(AFileName);
  try
    LoadFromIni(Ini,SDataDict);
    FFileName:=AFileName;
    If (Name='') then
      Name:=ChangeFileExt(ExtractFileName(AFileName),'');
  finally
    FreeAndNil(Ini);
  end;

end;

procedure TFPDataDictionary.LoadFromIni(Ini: TCustomIniFile; ASection: String);
begin
  FDDName:=Ini.ReadString(ASection,KeyDataDictName,'');
  FTables.Clear;
  FTables.LoadFromIni(Ini,SDataDictTables);
end;

{ ---------------------------------------------------------------------
  TFPDDEngine
  ---------------------------------------------------------------------}

procedure TFPDDEngine.DoProgress(const Msg: String);
begin
  If Assigned(FOnProgress) then
    FOnProgress(Self,Msg);
end;

destructor TFPDDEngine.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TFPDDEngine.GetConnectString: String;

Var
  CB : TGetConnectionEvent;
  
begin
  CB:=GetEngineConnectionStringCallBack(Self.ClassName);
  if (CB=Nil) then
    Raise EDataDict.CreateFmt(SerrNoConnectionDialog,[Self.ClassName]);
  Result:='';
  CB(Self,Result);
end;

function TFPDDEngine.ImportTables(Tables: TDDTableDefs; List: TStrings; UpdateExisting : Boolean): Integer;

Var
  I,J : Integer;
  TD : TDDTableDef;

begin
  Result:=0;
  For I:=0 to List.Count-1 do
    begin
    TD:=Nil;
    j:=Tables.IndexOfTable(List[i]);
    If (J=-1) then
      TD:=Tables.AddTAble(List[i])
    else if UpdateExisting then
      TD:=Tables[J];
    If (TD<>nil) then
      begin
      DoProgress(Format(SDDImportingTable,[TD.TableName]));
      ImportFields(TD);
      Inc(Result);
      end
    end;
end;

function TFPDDEngine.CreateSQLEngine: TFPDDSQLEngine;
begin
  Result:=TFPDDSQLEngine.Create;
end;

class function TFPDDEngine.EngineCapabilities: TFPDDEngineCapabilities;
begin
  Result:=[];
end;

procedure TFPDDEngine.CreateTable(Table: TDDTableDef);
begin
  Raise EDataDict.CreateFmt(SErrCreateTableNotSupported,[DBType]);
end;

function TFPDDEngine.ViewTable(Const TableName: String; DatasetOwner: TComponent
  ): TDataset;
begin
  Raise EDataDict.CreateFmt(SErrViewTableNotSupported,[DBType]);
end;

function TFPDDEngine.RunQuery(SQL: String): Integer;

begin
  Raise EDataDict.CreateFmt(SErrRunQueryNotSupported,[DBType]);
end;

function TFPDDEngine.CreateQuery(SQL: String; DatasetOwner : TComponent): TDataset;
begin
  Raise EDataDict.CreateFmt(SErrOpenQueryNotSupported,[DBType]);
end;

procedure TFPDDEngine.SetQueryStatement(SQL: String; AQuery: TDataset);
begin
  Raise EDataDict.CreateFmt(SErrSetQueryStatementNotSupported,[DBType]);
end;

{ ---------------------------------------------------------------------
  TFPDDSQLEngine
  ---------------------------------------------------------------------}

{ Utility functions }

constructor TFPDDSQLEngine.Create;
begin
  FTerminatorChar:=DefaultSQLTerminatorChar;
  FFieldQuoteChar:=DefaultSQLFieldQuoteChar;
  FOptions:=DefaultSQLEngineOptions;
  FMaxLineLength:=DefaultSQLEngineLineLength;
  FIndent:=DefaultSQLEngineIndent;
end;

procedure TFPDDSQLEngine.CheckTableDef;
begin
  If (FTableDef=Nil) then
    Raise EDataDict.Create(SErrMissingTableDef);
end;

procedure TFPDDSQLEngine.NoIndent;
begin
  FNoIndent:=True;
end;

procedure TFPDDSQLEngine.ResetLine;
begin
  FLastLength:=0;
  NoIndent;
end;

procedure TFPDDSQLEngine.FixUpStatement(var Res: String);
begin
  Res:=Trim(Res);
  if (eoAddTerminator in Options) then
    Res:=Res+FTerminatorChar;
end;

Procedure TFPDDSQLEngine.AddToStringLN(Var Res : String;S : String);

begin
  AddToString(Res,S);
  Res:=Res+LineEnding;
  FLastLength:=0;
end;

procedure TFPDDSQLEngine.AddToString(Var Res: String; S: String);
begin
  If (FMaxLineLength>0) and (FLastLength+Length(S)+1>FMaxLineLength) then
    begin
    FLastLength:=0;
    Res:=Res+LineEnding;
    end
  else If (FLastLength<>0) and (S<>'') then
    S:=' '+S;
  If (FLastlength=0) then
    begin
    If not FNoIndent then
      begin
      Res:=Res+StringOfChar(' ',Indent);
      FLastlength:=FlastLength+Indent;
      end;
    end;
  FLastLength:=FLastLength+Length(S);
  FNoIndent:=False;
  Res:=Res+S;
end;

procedure TFPDDSQLEngine.AddFieldString(var Res: String; S: String);
begin
  If eoLineFeedAfterField in FOptions then
    AddToStringLn(Res,S)
  else
    AddToString(Res,S)
end;

function TFPDDSQLEngine.CreateAndTerm(FD: TDDFieldDef; UseOldParam: Boolean
  ): string;
begin
  Result:=FieldNameString(FD)+' = '+FieldParamString(FD,UseOldParam);
  if (eoAndTermsInBrackets in FOptions) then
    Result:='('+Result+')';
end;

function TFPDDSQLEngine.CreateWhereSQL(var Res : String;FieldList: TFPDDFieldList; UseOldParam:Boolean): String;

Var
  i : Integer;
  FD : TDDFieldDef;
  S : String;

begin
  Result:='';
  If Assigned(FieldList) and (FieldList.Count>0) then
    begin
    For i:=0 to FieldList.Count-1 do
      begin
      FD:=FieldList[i];
      S:=CreateAndTerm(FD,UseOldParam);
      If (I>0) then
        S:=SAnd+' '+S;
      If eoLineFeedAfterAndTerm in Options then
        AddToStringLN(Res,S)
      else
        AddToString(Res,S);
      end;
    end;
end;

procedure TFPDDSQLEngine.AddWhereClause(var Res: String;
  FieldList: TFPDDFieldList; UseOldParam: Boolean);
begin
  If Assigned(FieldList) and (FieldList.Count>0) then
    begin
    NoIndent;
    AddToStringLn(Res,SWhere);
    CreateWhereSQL(Res,FieldList,UseOldParam);
    end;
end;

{ Functions with engine-specific strings in it. Can be overridden }

function TFPDDSQLEngine.FieldNameString(FD: TDDFieldDef): string;
begin
  Result:=FD.FieldName;
  if (eoQuoteFieldNames in FOptions) then
    Result:=FFieldQuoteChar+Result+FFieldQuoteChar;
end;

function TFPDDSQLEngine.TableNameString(TD: TDDTableDef): string;
begin
  Result:=TD.TableName;
end;

function TFPDDSQLEngine.FieldParamString(FD: TDDFieldDef; UseOldParam: Boolean
  ): string;
begin
  Result:=FD.FieldName;
  If UseOldParam then
    Result:=SOLD+Result;
  Result:=':'+Result;
end;

function TFPDDSQLEngine.FieldTypeString(FD : TDDFieldDef) : String;
{
ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd}
begin
  Result:=SQLFieldTypes[fD.FieldType];
  If (Result='') then
    Raise EDataDict.CreateFmt(SErrFieldTypeNotSupported,[GetEnumName(TypeInfo(TFieldType),Ord(FD.FieldType))]);
  case FD.FieldType of
    ftString,
    ftFixedChar,
    ftWideString :
      Result:=Result+Format('(%d)',[FD.Size]);
    ftBCD,
    ftFMTBCD :
      Result:=Result+Format('(%d,%d)',[FD.Precision,FD.Size]);
  end;
end;

function TFPDDSQLEngine.FieldDefaultString(FD : TDDFieldDef) : String;

begin
  Result:=SDefault+' '+FD.DBDefault;
end;

function TFPDDSQLEngine.FieldCheckString(FD : TDDFieldDef) : String;

begin
  Result:=Trim(FD.Constraint);
  If (Result<>'') then
    begin
    If (Result[1]<>'(') or (Result[Length(Result)]<>')') then
      Result:='('+Result+')';
    Result:=SCheck+' '+Result;
    end;
end;

function TFPDDSQLEngine.FieldDeclarationString(FD : TDDFieldDef) : String;

var
  S : String;

begin
  Result:=FieldNameString(FD)+' '+FieldTypeString(FD);
  If (FD.DBDefault<>'') then
    Result:=Result+' '+FieldDefaultString(FD);
  If FD.Required then
    Result:=Result+' '+SNotNull;
  S:=FieldCheckString(FD);
  If (S<>'') then
    Result:=Result+' '+S;
end;

{ SQL Creation functions. Can be overridden if needed. }
  
function TFPDDSQLEngine.CreateSelectSQL(FieldList, KeyFields: TFPDDFieldList
  ): String;
  
Var
  i : Integer;
  FD : TDDFieldDef;
  S : String;
  
begin
  CheckTableDef;
  Result:='';
  ResetLine;
  AddToStringLn(Result,SSelect);
  For i:=0 to FieldList.Count-1 do
    begin
    FD:=FieldList[i];
    S:=FieldNameString(FD);
    If (I<FieldList.Count-1) then
      S:=S+',';
    AddFieldString(Result,S);
    end;
  If Not (eoLineFeedAfterField in FOptions) then
    AddToStringLn(Result,'');
  NoIndent;
  AddToStringLn(Result,SFrom);
  AddToStringLn(Result,TableNameString(TableDef));
  AddWhereClause(Result,KeyFields,False);
  FixUpStatement(Result);
end;


function TFPDDSQLEngine.CreateInsertSQL(FieldList: TFPDDFieldList): String;

Var
  i : Integer;
  FD : TDDFieldDef;
  S : String;

begin
  CheckTableDef;
  Result:='';
  ResetLine;
  AddToString(Result,SInsertInto);
  AddToStringLn(Result,TableNameString(TableDef));
  For i:=0 to FieldList.Count-1 do
    begin
    FD:=FieldList[i];
    S:=FieldNameString(FD);
    If (I=0) then
      S:='('+S;
    If (I<FieldList.Count-1) then
      S:=S+','
    else
      S:=S+')';
    AddFieldString(Result,S);
    end;
  If Not (eoLineFeedAfterField in FOptions) then
    AddToStringLn(Result,'');
  NoIndent;
  AddToStringLn(Result,SValues);
  For i:=0 to FieldList.Count-1 do
    begin
    FD:=FieldList[i];
    S:=FieldParamString(FD,False);
    If (I=0) then
      S:='('+S;
    If (I<FieldList.Count-1) then
      S:=S+','
    else
      S:=S+')';
    AddFieldString(Result,S);
    end;
  FixUpStatement(Result);
end;

function TFPDDSQLEngine.CreateUpdateSQL(FieldList, KeyFields: TFPDDFieldList
  ): String;

Var
  i : Integer;
  FD : TDDFieldDef;
  S : String;

begin
  CheckTableDef;
  ResetLine;
  Result:='';
  AddToString(Result,SUPDATE);
  AddToStringLN(Result,TableNameString(TableDef));
  NoIndent;
  AddToStringLN(Result,SSET);
  If Assigned(FieldList) and (FieldList.Count>0) then
    begin
    For i:=0 to FieldList.Count-1 do
      begin
      FD:=FieldList[i];
      S:=FieldNameString(FD)+' = '+FieldParamString(FD,False);
      If (I<FieldList.Count-1) then
        S:=S+',';
      AddFieldString(Result,S);
      end;
    end;
  AddWhereClause(Result,KeyFields,eoUseOldInWhereParams in Options);
  FixUpStatement(Result);
end;

function TFPDDSQLEngine.CreateDeleteSQL(KeyFields: TFPDDFieldList): String;
begin
  CheckTableDef;
  ResetLine;
  Result:='';
  AddToStringLN(Result,SDeleteFrom);
  AddToStringLN(Result,TableNameString(TableDef));
  AddWhereClause(Result,KeyFields,eoUseOldInWhereParams in Options);
  FixUpStatement(Result);
end;


function TFPDDSQLEngine.CreateCreateSQL(Fields, KeyFields: TFPDDFieldList
  ): String;
  
Var
  S : String;
  I : integer;
  
begin
  CheckTableDef;
  Result:='';
  ResetLine;
  AddToStringLn(Result,SCreateTable+' '+TableNameString(TableDef)+' (');
  For I:=0 to Fields.Count-1 do
    begin
    S:=FieldDeclarationString(Fields[i]);
    If (I<Fields.Count-1) or (Assigned(KeyFields) and (KeyFields.Count<>0)) then
      S:=S+',';
    AddToStringLn(Result,S);
    end;
  If (Assigned(KeyFields) and (KeyFields.Count<>0)) then
    begin
    S:=SCONSTRAINT+' '+TableDef.PrimaryKeyConstraintName+' '+SPrimaryKey+' (';
    For I:=0 to KeyFields.Count-1 do
      begin
      S:=S+FieldNameString(KeyFields[i]);
      If I<KeyFields.Count-1 then
        S:=S+','
      else
        S:=S+')'
      end;
    AddToStringLn(Result,S);
    end;
  NoIndent;
  AddToStringLn(Result,')');
  FixUpStatement(Result);
end;

function TFPDDSQLEngine.CreateCreateSQL(KeyFields: TFPDDFieldList): String;

Var
  Fl : TFPDDFieldList;

begin
  CheckTableDef;
  FL:=TFPDDfieldList.CreateFromTableDef(TableDef);
  try
    Result:=CreateCreateSQL(FL,KeyFields);
  finally
    FL.Free;
  end;
end;

{ TStrings versions of SQL creation statements. }

procedure TFPDDSQLEngine.CreateSelectSQLStrings(FieldList,KeyFields: TFPDDFieldList; SQL: TStrings);

begin
  SQL.Text:=CreateSelectSQL(FieldList,KeyFields);
end;

procedure TFPDDSQLEngine.CreateInsertSQLStrings(FieldList: TFPDDFieldList; SQL: TStrings);
begin
  SQL.Text:=CreateInsertSQL(FieldList);
end;

procedure TFPDDSQLEngine.CreateUpdateSQLStrings(FieldList, KeyFields: TFPDDFieldList;
  SQL: TStrings);
begin
  SQL.Text:=CreateUpdateSQL(FieldList,KeyFields);
end;

procedure TFPDDSQLEngine.CreateDeleteSQLStrings(KeyFields: TFPDDFieldList;
  SQL: TStrings);
begin
  SQL.Text:=CreateDeleteSQL(KeyFields);
end;

procedure TFPDDSQLEngine.CreateCreateSQLStrings(Fields,
  KeyFields: TFPDDFieldList; SQL: TStrings);
begin
  SQL.Text:=CreateCreateSQL(Fields,KeyFields);
end;

procedure TFPDDSQLEngine.CreateCreateSQLStrings(KeyFields: TFPDDFieldList;
  SQL: TStrings);
begin
  SQL.Text:=CreateCreateSQL(KeyFields);
end;

{ ---------------------------------------------------------------------
  TDDFieldList
  ---------------------------------------------------------------------}

function TFPDDFieldList.GetFieldDef(Index : Integer): TDDFieldDef;
begin
  Result:=TDDFieldDef(Items[Index]);
end;

procedure TFPDDFieldList.SetFieldDef(Index : Integer; const AValue: TDDFieldDef);
begin
  Items[Index]:=AValue;
end;

constructor TFPDDFieldList.CreateFromTableDef(TD: TDDTableDef);

begin
  CreateFromFieldDefs(TD.Fields);
end;

constructor TFPDDFieldList.CreateFromFieldDefs(FD: TDDFieldDefs);

Var
  I : Integer;

begin
  Inherited Create;
  Capacity:=FD.Count;
  For I:=0 to FD.Count-1 do
    Add(FD[i]);
end;

{ TDDIndexDef }

function TDDIndexDef.GetSectionName: String;
begin
  Result:=IndexName;
end;

procedure TDDIndexDef.SetSectionName(const Value: String);
begin
  IndexName:=Value;
end;

{ TDDIndexDefs }

function TDDIndexDefs.GetIndex(Index : Integer): TDDIndexDef;
begin
  Result:=Items[Index] as TDDIndexDef;
end;

procedure TDDIndexDefs.SetIndex(Index : Integer; const AValue: TDDIndexDef);
begin
  Items[Index]:=AValue;
end;

procedure TDDIndexDefs.SetTableName(const AValue: String);
begin
  FTableName:=AValue;
  FSectionPrefix:=AValue;
  GlobalSection:=AValue+SIndexSuffix;
end;

constructor TDDIndexDefs.Create(ATableName: String);
begin
  FPrefix:='Index';
  TableName:=ATableName;
end;

initialization

finalization
  FreeAndNil(DDEngines);
end.

