{
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
unit generatesqldlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DB, SQLDB, ExtCtrls, Buttons, StdCtrls, Spin, ButtonPanel, SynEdit, SynHighlighterSQL,
  sqldbstrconst;

type
  TSQLKeyWord = (skInsert,skInto,skDelete,skFrom,skUpdate,skSelect,skWhere,skAnd,skValues,skSet);

  { TGenerateSQLForm }

  TGenerateSQLForm = class(TForm)
    BGenerate: TButton;
    BPGenSQL: TButtonPanel;
    CBOneFieldPerLine: TCheckBox;
    cbUpperCaseKeywords: TCheckBox;
    CBSystemTables: TCheckBox;
    CBTables: TComboBox;
    CBQuoteFields: TCheckBox;
    edtQuoteChar: TEdit;
    lblQuoteChar: TLabel;
    LBKeyFields: TListBox;
    LCBTables: TLabel;
    Label2: TLabel;
    LLBKeyFields: TLabel;
    LBFields: TListBox;
    LSEIndent: TLabel;
    LSELineLength: TLabel;
    MInsert: TSynEdit;
    MRefresh: TSynEdit;
    MUpdate: TSynEdit;
    MDelete: TSynEdit;
    PKeyFields: TPanel;
    POptions: TPanel;
    PSelectFields: TPanel;
    PCSQL: TPageControl;
    cbFullyQualifiedFields: TCheckBox;
    seIndent: TSpinEdit;
    seLineLength: TSpinEdit;
    MSelect: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    TSRefresh: TTabSheet;
    TSFields: TTabSheet;
    TSSelect: TTabSheet;
    TSInsert: TTabSheet;
    TSUpdate: TTabSheet;
    TSDelete: TTabSheet;
    procedure BGenerateClick(Sender: TObject);
    procedure CBSystemTablesChange(Sender: TObject);
    procedure CBTablesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TSResize(Sender: TObject);
  private
    FConnection : TSQLConnection;
    FDataset: TSQLQuery;
    QuoteChar : Char;
    Function IndentString : string;
    Function SQLKeyWord(aKeyWord : TSQLKeyWord) : String;
    procedure GenDeleteSQL(const TableName : string; KeyFields, SQL: TStrings);
    procedure GenInsertSQL(const TableName : string; UpdateFields, SQL: TStrings);
    procedure GenModifySQL(const TableName : string; KeyFields, UpdateFields, SQL: TStrings);
    procedure GenRefreshSQL(const TableName : string; SelectFields,KeyFields, SQL: TStrings);
    procedure GenWhereClause(const aTableName,ParamPrefix : string; KeyFields, SQL: TStrings);
    function GetAS: Boolean;
    procedure GetDataFieldNames(List: TStrings);
    function GetSQLStatement(Index: integer): TStrings;
    function GetTableName: String;
    function GetQuoted(const aIdentifier : string; Const aTable : String = ''): string;
    procedure SetAS(AValue: Boolean);
    procedure SetConnection(AValue: TSQLConnection);
    procedure SetTableName(const AValue: String);
    Procedure SetFieldLists(aFields : TStrings);
    { private declarations }
  public
    { public declarations }
    Procedure RefreshTableList;
    Procedure GenerateSQL;
    Procedure ClearSQL(clearSelect : Boolean =  False);
    Property Dataset : TSQLQuery Read FDataset Write FDataset;
    Property Connection : TSQLConnection Read FConnection Write SetConnection;
    Property TableName : String Read GetTableName Write SetTableName;
    Property SelectSQL : TStrings Index 0 Read GetSQLStatement;
    Property InsertSQL : TStrings Index 1 Read GetSQLStatement;
    Property UpdateSQL : TStrings Index 2 Read GetSQLStatement;
    Property DeleteSQL : TStrings Index 3 Read GetSQLStatement;
    Property RefreshSQL : TStrings Index 4 Read GetSQLStatement;
    Property AllowSelectTable : Boolean Read GetAS Write SetAS;
  end; 

Function GenerateSQL(Q : TSQLQuery): Boolean;

implementation

Function GenerateSQL(Q : TSQLQuery): Boolean;

begin

  With TGenerateSQLForm.Create(Application) do
    try
      Dataset:=Q;
      Connection:=Q.SQLConnection;
      SelectSQL.Text:=Q.SQL.text;
      UpdateSQL.Text:=Q.UpdateSQL.Text;
      DeleteSQL.Text:=Q.DeleteSQL.Text;
      InsertSQL.Text:=Q.insertSQL.Text;
      RefreshSQL.Text:=Q.RefreshSQL.Text;
      Result:=ShowModal=mrOK;
      if Result then
        begin
        Q.SQL.text        := SelectSQL.Text;
        Q.UpdateSQL.Text  := UpdateSQL.Text;
        Q.DeleteSQL.Text  := DeleteSQL.Text;
        Q.insertSQL.Text  := InsertSQL.Text;
        Q.RefreshSQL.Text := RefreshSQL.Text;
        end;
    finally
      Free;
    end;
end;

{$R *.lfm}

{ TGenerateSQLForm }

procedure TGenerateSQLForm.GenWhereClause(const aTableName, ParamPrefix: string;  KeyFields, SQL: TStrings);
var
  Maxlen, I: Integer;
  isNotLast : Boolean;
  L,FieldName: string;

begin
  L:=IndentString;
  MaxLen:=seLineLength.Value;
  SQL.Add(SQLKeyWord(skWhere));
  for I := 0 to KeyFields.Count-1 do
    begin
    isNotLast:=I<KeyFields.Count-1;
    FieldName:=GetQuoted(KeyFields[I],ATableName);
    L:=L+' '+Format('(%s = :%s%s)', [FieldName,ParamPrefix,KeyFields[I]]);
    if I<KeyFields.Count - 1 then
      L:=L+' '+SQLKeyWord(skAnd);
    if CBOneFieldPerLine.Checked or ((Length(L)>MaxLen) and IsNotLast) then
      begin
      SQL.Add(L);
      L:=IndentString;
      end;
    end;
  SQL.Add(L);
end;

function TGenerateSQLForm.GetQuoted(const aIdentifier: string; const aTable: String =''): string;

begin
  Result:=aIdentifier;
  if CBQuoteFields.Checked then
    Result:=QuoteChar + Result + QuoteChar;
  if (aTable<>'') and CBFullyQualifiedFields.Checked then
    Result:=GetQuoted(aTable)+'.'+Result;
end;

procedure TGenerateSQLForm.GenDeleteSQL(const TableName: string; KeyFields, SQL: TStrings);
begin
  SQL.Clear;
  SQL.Add(Format('%s %s %s', [sqlkeyword(skDelete),sqlkeyword(skFrom),GetQuoted(TableName)])); { Do not localize }
  GenWhereClause(TableName, 'OLD_',KeyFields, SQL);
end;

procedure TGenerateSQLForm.GenInsertSQL(const TableName: string; UpdateFields, SQL: TStrings);

  procedure GenFieldList(isParam : boolean);

  var
    FN,L: string;
    I,MaxLen : integer;
    isNotLast : boolean;

  begin
    L:=IndentString+'(';
    MaxLen:=seLineLength.Value;
    for I := 0 to UpdateFields.Count - 1 do
      begin
      IsNotLast:=(I<UpdateFields.Count-1);
      FN:=UpdateFields[i];
      if not IsParam then
        FN:=GetQuoted(FN,TableName)
      else
        FN:=':'+FN;
      L:=L+FN;
      if IsNotLast then
        L:=L+', ';
      if CBOneFieldPerLine.Checked or ((Length(L)>MaxLen) and IsNotLast) then
        begin
        SQL.Add(L);
        L:=IndentString;
        end;
      end;
    SQL.Add(L+')');
  end;

begin
  SQL.Clear;
  SQL.Add(Format('%s %s %s', [SQLKeyWord(skInsert),SQLKeyWord(skInto), GetQuoted(TableName)]));
  GenFieldList(False);
  SQL.Add(SQLKeyWord(skValues));
  GenFieldList(True);
end;

procedure TGenerateSQLForm.GenModifySQL(const TableName: string; KeyFields, UpdateFields, SQL: TStrings);

var
  MaxLen,I: integer;
  L,FN: string;
  isNotLast : Boolean;

begin
  L:=IndentString;
  MaxLen:=seLineLength.Value;
  SQL.Clear;
  SQL.Add(Format('%s %s', [SQLKeyWord(skUpdate),GetQuoted(TableName)]));  { Do not localize }
  SQL.Add(SQLKeyWord(skSet));                             { Do not localize }
  for I := 0 to UpdateFields.Count-1 do
   begin
   isNotLast:=I<UpdateFields.Count-1;
   FN:=GetQuoted(UpdateFields[i],TableName);
   FN:=FN+' = :'+UpdateFields[i];
   L:=L+FN;
   if IsNotLast then
     L:=L+', ';
   if CBOneFieldPerLine.Checked or ((Length(L)>MaxLen) and IsNotLast) then
     begin
     SQL.Add(L);
     L:=IndentString;
     end;
   end;
  GenWhereClause(TableName, 'OLD_',KeyFields,SQL);
end;

procedure TGenerateSQLForm.GetDataFieldNames(List: TStrings);
var
  I: Integer;
begin
  with Dataset do
  try
    FieldDefs.Update;
    List.BeginUpdate;
    try
      List.Clear;
      for I := 0 to FieldDefs.Count - 1 do
        List.Add(FieldDefs[I].Name);
    finally
      List.EndUpdate;
    end;
  except
      MessageDlg(Format(lrsSQLDataSetOpen, [Dataset.Name]), mtError, [mbOK], 0);
  end;
end;

procedure TGenerateSQLForm.GenRefreshSQL(const TableName: string; SelectFields,KeyFields, SQL: TStrings);

var
  MaxLen,I: integer;
  L,FN: string;
  isNotLast : Boolean;

begin
  MaxLen:=seLineLength.Value;
  SQL.Clear;
  SQL.Add(Format('%s', [SQLKeyWord(skselect)]));  { Do not localize }
  L:=IndentString;
  for I := 0 to SelectFields.Count-1 do
   begin
   isNotLast:=I<SelectFields.Count-1;
   FN:=GetQuoted(SelectFields[i],TableName);
   L:=L+FN;
   if IsNotLast then
     L:=L+', ';
   if CBOneFieldPerLine.Checked or ((Length(L)>MaxLen) and IsNotLast) then
     begin
     SQL.Add(L);
     L:=IndentString;
     end;
   end;
  SQL.Add(Format('%s %s', [SQLKeyWord(skFrom),GetQuoted(TableName)]));  { Do not localize }
  GenWhereClause(TableName,'',KeyFields,SQL);
end;

procedure GetSelectedItems(ListBox: TListBox; List: TStrings);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to ListBox.Items.Count - 1 do
    if ListBox.Selected[I] then
      List.Add(ListBox.Items[I]);
end;

procedure TGenerateSQLForm.GenerateSQL;

  function QuotedTableName(const BaseName: string): string;
  begin
      if CBQuoteFields.Checked then
        Result := Format('"%s"', [BaseName])   {do not localize}
      else
        Result := BaseName;
  end;

var
  KeyFields: TStringList;
  UpdateFields: TStringList;
  DidConnect : Boolean;

begin
  if EdtQuoteChar.text<>'' then
    QuoteChar:=EdtQuoteChar.text[1]
  else
    QuoteChar:='"';
  if (LBKeyFields.SelCount = 0) or (LBFields.SelCount = 0) then
    raise Exception.Create(lrsSQLGenSelect);
  KeyFields := TStringList.Create;
  DidConnect := not DataSet.Database.Connected;
  if DidConnect then
    DataSet.Database.Connected := true;

  try
    GetSelectedItems(LBKeyFields, KeyFields);
    UpdateFields := TStringList.Create;
    try
      GetSelectedItems(LBFields, UpdateFields);
      TableName := CBTables.Text;
      GenDeleteSQL(TableName, KeyFields, MDelete.Lines);
      GenInsertSQL(TableName, UpdateFields, Minsert.Lines);
      GenModifySQL(TableName, KeyFields, UpdateFields, MUpdate.Lines);
      GenRefreshSQL(TableName, UpdateFields, KeyFields, MRefresh.Lines);
    finally
      UpdateFields.Free;
    end;
  finally
    KeyFields.Free;
    if DidConnect then
      DataSet.Database.Connected := false;
  end;
end;

procedure TGenerateSQLForm.TSResize(Sender: TObject);

Var
  W : Integer;
  
begin
  W:=TSFields.CLientWidth div 3;
  POPtions.Width:=W;
  PSelectFIelds.Width:=W;
end;

function TGenerateSQLForm.IndentString: string;
begin
  Result:=StringOfChar(' ',SEIndent.Value);
end;

function TGenerateSQLForm.SQLKeyWord(aKeyWord: TSQLKeyWord): String;

Const
  KeyWords : Array[TSQLKeyWord] of string =
    ('insert','into','delete','from','update','select','where','and','values','set');

begin
  Result:=KeyWords[aKeyWord];
  if CBUppercaseKeyWords.Checked then
    Result:=UpperCase(Result);
end;


function TGenerateSQLForm.GetTableName: String;
begin
  Result:=CBTables.Text;
end;

procedure TGenerateSQLForm.SetAS(AValue: Boolean);
begin
  CBTables.Enabled:=AValue;
end;

procedure TGenerateSQLForm.SetConnection(AValue: TSQLConnection);
begin
  if FConnection=AValue then Exit;
  FConnection:=AValue;
  RefreshTableList;
end;

function TGenerateSQLForm.GetSQLStatement(Index: integer): TStrings;
begin
  Case Index of
    0 : Result:=MSelect.Lines;
    1 : Result:=MInsert.Lines;
    2 : Result:=MUpdate.Lines;
    3 : Result:=MDelete.Lines;
    4 : Result:=MRefresh.Lines;
  end;
end;

function TGenerateSQLForm.GetAS: Boolean;
begin
  Result:=CBTables.Enabled;
end;

procedure TGenerateSQLForm.RefreshTableList;

Var
  TN : String;

begin
  TN:=CBTables.Text;
  With CBTables.Items do
    try
      BeginUpdate;
      Clear;
      if Not Assigned(FConnection) then
        exit;
      FConnection.Connected:=true;
      FConnection.GetTableNames(CBTables.Items,CBSystemTables.Checked);
    finally
      EndUpdate;
    end;
  With CBTables do
    If (TN<>'') then
      ItemIndex:=Items.IndexOf(TN);
end;

procedure TGenerateSQLForm.ClearSQL(clearSelect : Boolean =  False);

begin
  if ClearSelect then
    MSelect.Clear;
  MInsert.Clear;
  MUpdate.Clear;
  MDelete.Clear;
  MRefresh.Clear;
end;

procedure TGenerateSQLForm.SetTableName(const AValue: String);

begin
  With CBTables do
    begin
    ItemIndex:=Items.IndexOf(AValue);
    CBTablesChange(CBTables);
    end;
end;

procedure TGenerateSQLForm.SetFieldLists(aFields: TStrings);

Var
  I,Idx : Integer;

begin
  if aFields=Nil then
    begin
    LBKeyFields.Items.Clear;
    LBFields.Items.Clear;
    end
  else
    begin
    LBKeyFields.Items:=aFields;
    LBFields.Items:=aFields;
    end;
  if not Assigned(Dataset) then exit;
  For I:=0 to FDataset.FieldDefs.Count-1 do
    begin
    Idx:=LBFields.Items.IndexOf(FDataset.FieldDefs[i].Name);
    if Idx>=0 then
      LBFields.Selected[Idx]:=true
    end;
  For I:=0 to FDataset.Fields.Count-1 do
    if ((Dataset.UpdateMode=upWhereKeyOnly) and (pfInKey in FDataset.Fields[i].ProviderFlags)) or
       (Dataset.UpdateMode=upWhereAll) then
    begin
    Idx:=LBKeyFields.Items.IndexOf(FDataset.Fields[i].FieldName);
    if (Idx>=0) then
      LBKeyFields.Selected[Idx]:=true;
    end;
end;

procedure TGenerateSQLForm.CBTablesChange(Sender: TObject);

Var
  l : TStringList;

begin
  With CBTables do
    If (ItemIndex=-1) Then
      SetFieldLists(Nil)
    else
      begin
      L:=TstringList.Create;
      try
        Connection.GetFieldNames(TableName,L);
        SetFieldLists(L)
      finally
        L.Free;
      end;
      end;
  ClearSQL;
end;

procedure TGenerateSQLForm.FormCreate(Sender: TObject);

begin
  Caption:= lrsGeneratesqlstatements;
  EdtQuoteChar.Text:='"';
end;

procedure TGenerateSQLForm.BGenerateClick(Sender: TObject);
begin
  GenerateSQL;
end;

procedure TGenerateSQLForm.CBSystemTablesChange(Sender: TObject);
begin
  if Assigned(Connection) then
    RefreshTableList;
end;


end.

