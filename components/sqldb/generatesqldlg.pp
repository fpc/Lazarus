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
    CBUpperCaseKeywords: TCheckBox;
    CBSystemTables: TCheckBox;
    CBTables: TComboBox;
    CBDelimiters: TCheckBox;
    edtQuoteChar: TEdit;
    edtBrackets: TEdit;
    LStatusMsg: TLabel;
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
    RBBrackets: TRadioButton;
    RBQuoteChar: TRadioButton;
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
    procedure CBDelimitersChange(Sender: TObject);
    procedure CBSystemTablesChange(Sender: TObject);
//    procedure CBTablesChange(Sender: TObject);
    procedure CBTablesSelect(Sender: TObject);
    procedure edtQuoteCharChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TSResize(Sender: TObject);
  private
    { private declarations }
    FConnection : TSQLConnection;
    FDataset: TSQLQuery;
    FActivateCalled: Boolean;
    procedure ApplyTranslation;
    Function IndentString : string;
    Function SQLKeyWord(aKeyWord : TSQLKeyWord) : String;
    procedure GenDeleteSQL(const TableName : string; KeyFields, SQL: TStrings);
    procedure GenInsertSQL(const TableName : string; UpdateFields, SQL: TStrings);
    procedure GenModifySQL(const TableName : string; KeyFields, UpdateFields, SQL: TStrings);
    procedure GenRefreshSQL(const TableName : string; SelectFields,KeyFields, SQL: TStrings);
    procedure GenSelectSQL(const TableName : string; SelectFields, SQL: TStrings);
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
    procedure UpdateStatusMsg(Done: Boolean);
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

Function Max(a, b: Integer): Integer;
begin
  if a > b then Result := a else Result := b;
end;

Function GenerateSQL(Q : TSQLQuery): Boolean;

begin

  With TGenerateSQLForm.Create(Application) do
    try
      Dataset:=Q;
      Connection:=Q.SQLConnection;
      SelectSQL.Text:=Q.SQL.Text;
      UpdateSQL.Text:=Q.UpdateSQL.Text;
      DeleteSQL.Text:=Q.DeleteSQL.Text;
      InsertSQL.Text:=Q.insertSQL.Text;
      RefreshSQL.Text:=Q.RefreshSQL.Text;
      Result:=ShowModal=mrOK;
      if Result then
        begin
        Q.SQL.Text        := SelectSQL.Text;
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
    if CBOneFieldPerLine.Checked or (Length(L)>MaxLen) or (not IsNotLast) then
      begin
      SQL.Add(L);
      L:=IndentString;
      end;
    end;
  SQL.Add(L);
end;

function TGenerateSQLForm.GetQuoted(const aIdentifier: string; const aTable: String =''): string;
var
  ch, ch1: char;
  L: Integer;
begin
  Result:=aIdentifier;
  if CBDelimiters.Checked then
  begin
    if RBQuoteChar.Checked then
    begin
      if edtQuoteChar.text<>'' then
        ch := edtQuoteChar.Text[1]
      else
        ch := '"';
      Result := ch + Result + ch;
    end else
    if RBBrackets.Checked then
    begin
      L := Length(edtBrackets.Text);
      case L of
        0: exit;
        1: begin
             ch := edtBrackets.Text[1];
             case ch of
               '[': ch1 := ']';
               '(': ch1 := ')';
               '{': ch1 := '}';
               else ch1 := ch;
             end;
           end;
        else
          ch := edtBrackets.Text[1];
          ch1 := edtBrackets.Text[2];
      end;
      Result := ch + Result + ch1;
    end;
  end;
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
      if CBOneFieldPerLine.Checked or (Length(L)>MaxLen) or (not IsNotLast) then
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
   if CBOneFieldPerLine.Checked or (Length(L)>MaxLen) or (not IsNotLast) then
     begin
     SQL.Add(L);
     L:=IndentString;
     end;
   end;
  GenWhereClause(TableName, 'OLD_',KeyFields,SQL);
end;

procedure TGenerateSQLForm.GenSelectSQL(const TableName: string; SelectFields, SQL: TStrings);
var
  MaxLen, I: integer;
  L, FN: string;
  isNotLast: Boolean;
begin
  MaxLen:=seLineLength.Value;
  SQL.Clear;
  SQL.Add(Format('%s', [SQLKeyWord(skselect)]));  { Do not localize }
  L := IndentString;
  for I := 0 to SelectFields.Count-1 do
  begin
    isNotLast:=I<SelectFields.Count-1;
    FN:=GetQuoted(SelectFields[i],TableName);
    L:=L+FN;
    if IsNotLast then
      L:=L+', ';
    if CBOneFieldPerLine.Checked or (Length(L)>MaxLen) or (not IsNotLast) then
    begin
      SQL.Add(L);
      L:=IndentString;
    end;
  end;
  SQL.Add(Format('%s %s', [SQLKeyWord(skFrom),GetQuoted(TableName)]));  { Do not localize }
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
   if CBOneFieldPerLine.Checked or (Length(L)>MaxLen) or (not IsNotLast) then
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
      if CBDelimiters.Checked then
        Result := Format('"%s"', [BaseName])   {do not localize}
      else
        Result := BaseName;
  end;

var
  KeyFields: TStringList;
  UpdateFields: TStringList;
  DidConnect : Boolean;

begin
  if TableName = '' then
  begin
    MessageDlg(SSQLNoTable, mtError, [mbOK], 0);
    exit;
  end;
  if (LBKeyFields.SelCount = 0) or (LBFields.SelCount = 0) then
  begin
    MessageDlg(lrsSQLGenSelect, mtError, [mbOK], 0);
    exit;
  end;

  KeyFields := TStringList.Create;
  DidConnect := not DataSet.Database.Connected;
  if DidConnect then
    DataSet.Database.Connected := true;

  try
    GetSelectedItems(LBKeyFields, KeyFields);
    UpdateFields := TStringList.Create;
    try
      GetSelectedItems(LBFields, UpdateFields);
      if TableName <> CBTables.Text then
        TableName := CBTables.Text;
      GenSelectSQL(TableName, UpdateFields, MSelect.Lines);
      GenDeleteSQL(TableName, KeyFields, MDelete.Lines);
      GenInsertSQL(TableName, UpdateFields, Minsert.Lines);
      GenModifySQL(TableName, KeyFields, UpdateFields, MUpdate.Lines);
      GenRefreshSQL(TableName, UpdateFields, KeyFields, MRefresh.Lines);
    finally
      UpdateFields.Free;
    end;
    UpdateStatusMsg(true);
  finally
    KeyFields.Free;
    if DidConnect then
      DataSet.Database.Connected := false;
  end;
end;

procedure TGenerateSQLForm.TSResize(Sender: TObject);

  procedure SetScrollWidth(AListbox: TListbox);
  var
    I, MaxWidth: Integer;
  begin
    MaxWidth := -1;
    AListBox.Canvas.Font := AListBox.Font;
    for I := 0 to AListBox.Items.Count - 1 do
      MaxWidth := Max(MaxWidth, AListBox.Canvas.TextWidth(AListBox.Items[I]));
    if MaxWidth <> -1 then
      AListBox.ScrollWidth := MaxWidth + AListBox.Width - AListBox.ClientWidth;
  end;

Var
  W : Integer;
begin
  W:=TSFields.ClientWidth div 3;
  POptions.Width:=W;
  PSelectFields.Width:= (TSFields.ClientWidth - POptions.Width) div 2;
  SetScrollWidth(LBKeyFields);
  SetScrollWidth(LBFields);
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
  if CBUpperCaseKeywords.Checked then
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
  begin
    If (TN<>'') then
      ItemIndex:=Items.IndexOf(TN);
    If ItemIndex = -1 then
      SetFieldLists(nil);
  end;
  UpdateStatusMsg(false);
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
    CBTablesSelect(CBTables);
    end;
end;

procedure TGenerateSQLForm.SetFieldLists(aFields: TStrings);
Var
  I,Idx : Integer;
  fieldName: String;
  selFields: TStrings;
  selKeyFields: TStrings;
begin
  selFields := TStringList.Create;
  selKeyFields := TStringList.Create;
  try
    if aFields=Nil then
      begin
      LBKeyFields.Items.Clear;
      LBFields.Items.Clear;
      end
    else
    begin
      // Preserve selections from previous runs
      for I := 0 to LBFields.Items.Count-1 do
        if LBFields.Selected[I] then
          selFields.Add(LBFields.Items[I]);
      for I := 0 to LBKeyFields.Items.Count-1 do
        if LBKeyFields.Selected[I] then
          selKeyfields.Add(LBKeyFields.Items[I]);
      LBKeyFields.Items:=aFields;
      LBFields.Items:=aFields;
    end;
    if not Assigned(Dataset) then exit;

    // 1st run: Collect field names to be selected
    if (selFields.Count = 0) and (FDataset.FieldDefs.Count > 0) then
    begin
      for I := 0 to FDataset.FieldDefs.Count-1 do
      begin
        fieldName := FDataset.FieldDefs[I].Name;
        Idx := LBFields.Items.IndexOf(fieldName);
        if Idx>=0 then
          selFields.Add(fieldName);
      end;
    end;
    // Select these fields.
    if selFields.Count > 0 then
      for I := 0 to selFields.Count-1 do
      begin
        idx := LBFields.Items.IndexOf(selFields[I]);
        if idx >= 0 then
          LBFields.Selected[idx] := true;
      end
    else
      LBFields.SelectAll;

    // 1st run: Collect key field names to be selected
    if (selKeyFields.Count = 0) and (FDataset.Fields.Count-1 > 0) then
    begin
      for I := 0 to FDataset.Fields.Count-1 do
      begin
        if ((Dataset.UpdateMode=upWhereKeyOnly) and (pfInKey in FDataset.Fields[i].ProviderFlags)) or
           (Dataset.UpdateMode=upWhereAll) then
        begin
          fieldName := FDataset.Fields[I].FieldName;
          Idx:=LBKeyFields.Items.IndexOf(fieldName);
          if (Idx>=0) then
            selKeyFields.Add(fieldName);
        end;
      end;
    end;
    // Select these fields in the "Keys" listbox
    if selKeyFields.Count > 0 then
      for I := 0 to selKeyFields.Count-1 do
      begin
        idx := LBKeyFields.Items.IndexOf(selKeyFields[I]);
        if idx >= 0 then
          LBKeyFields.Selected[idx] := true;
      end
    else
      if (LBKeyFields.Items.Count > 0) then LBKeyFields.Selected[0] := true;
  finally
    selFields.Free;
    selKeyFields.Free;
  end;
end;

procedure TGenerateSQLForm.CBTablesSelect(Sender: TObject);
Var
  l : TStringList;
begin
  With CBTables do
  begin
    SetFieldLists(nil);
    if (ItemIndex >= 0) then
    begin
      L:=TstringList.Create;
      try
        Connection.GetFieldNames(TableName,L);
        SetFieldLists(L);
        UpdateStatusMsg(false);
      finally
        L.Free;
      end;
    end;
  end;
  ClearSQL;
end;

procedure TGenerateSQLForm.edtQuoteCharChange(Sender: TObject);
begin
  UpdateStatusMsg(false);
end;

procedure TGenerateSQLForm.FormActivate(Sender: TObject);
var
  w: Integer;
begin
  if FActivateCalled then
    exit;
  FActivateCalled := true;

  Constraints.MinHeight := PCSQL.Height - PCSQL.ClientHeight +
    seLineLength.Top + seLineLength.Height +
    bGenerate.Height + bGenerate.BorderSpacing.Top + bGenerate.BorderSpacing.Bottom +
    BPGenSQL.Height + BPGenSQL.BorderSpacing.Around*2;

  w := LCBTables.Left + LCBTables.Width +
    CBSystemTables.BorderSpacing.Left + CBSystemTables.Width;
  w := Max(w, CBDelimiters.Left + CBDelimiters.Width);
  w := Max(w, RBQuoteChar.Left + RBQuoteChar.Width + edtQuoteChar.BorderSpacing.Left + edtQuoteChar.Width);
  w := Max(w, CBUppercaseKeywords.Left + CBUppercaseKeywords.Width);
  w := Max(w, CBFullyQualifiedfields.Left + CBFullyQualifiedFields.Width);
  inc(w, CBTables.BorderSpacing.Right);
  Constraints.MinWidth := 3 * (PCSQL.Width - PCSQL.ClientWidth + w);

  // Enforce constraints
  if Height < Constraints.MinHeight then Height := Constraints.MinHeight;
  if Width < Constraints.MinWidth then Width := Constraints.MinWidth;
end;

procedure TGenerateSQLForm.FormCreate(Sender: TObject);
begin
  ApplyTranslation;
  MSelect.Font.Height := SynDefaultFontHeight;
  MSelect.Font.Name := SynDefaultFontName;
  MSelect.Font.Quality := SynDefaultFontQuality;
  MInsert.Font.Height := SynDefaultFontHeight;
  MInsert.Font.Name := SynDefaultFontName;
  MInsert.Font.Quality := SynDefaultFontQuality;
  MUpdate.Font.Height := SynDefaultFontHeight;
  MUpdate.Font.Name := SynDefaultFontName;
  MUpdate.Font.Quality := SynDefaultFontQuality;
  MDelete.Font.Height := SynDefaultFontHeight;
  MDelete.Font.Name := SynDefaultFontName;
  MDelete.Font.Quality := SynDefaultFontQuality;
  MRefresh.Font.Height := SynDefaultFontHeight;
  MRefresh.Font.Name := SynDefaultFontName;
  MRefresh.Font.Quality := SynDefaultFontQuality;
  PCSQL.TabIndex := 0;
  LStatusMsg.Caption := '';
end;

procedure TGenerateSQLForm.OKButtonClick(Sender: TObject);
var
  hasSQL: Boolean;
  res: TModalResult;
  i: Integer;
begin
  hasSQL := false;
  for i := 0 to 4 do
    if GetSQLStatement(i).Count > 0 then
    begin
      hasSQL := true;
      break;
    end;
  if not hasSQL then
  begin
    res := MessageDlg(SSQLNoSQLGenerated, mtConfirmation, [mbYes, mbNo], 0);
    if res <> mrYes then
      ModalResult := mrNone;
  end;
end;

procedure TGenerateSQLForm.ApplyTranslation;
begin
  Caption:= lrsGeneratesqlstatements;
  TSFields.Caption := SSQLTablesAndFields;
  LCBTables.Caption := SSQLTable;
  CBSystemTables.Caption := SSQLShowSystemTables;
  CBDelimiters.Caption := SSQLDelimitersForFieldTableNames;
  RBQuoteChar.Caption := SSQLQuoteChar;
  RBBrackets.Caption := SSQLBrackets;
  EdtQuoteChar.Text:='"';
  EdtBrackets.Text := '[]';
  CBOneFieldPerLine.Caption := SSQLOneFieldPerLine;
  CBUpperCaseKeywords.Caption := SSQLUppercaseKeywords;
  CBFullyQualifiedFields.Caption := SSQLFullyQualifiedFields;
  LSEIndent.Caption := SSQLIndent;
  LSELineLength.Caption := SSQLLineLength;
  BGenerate.Caption := SSQLGenerateSQL;
  LLBKeyFields.Caption := SSQLKeyFields;
  Label2.Caption := SSQLSelectUpdateInsertFields;
end;

procedure TGenerateSQLForm.BGenerateClick(Sender: TObject);
begin
  GenerateSQL;
end;

procedure TGenerateSQLForm.CBDelimitersChange(Sender: TObject);
begin
  RBQuoteChar.Enabled := CBDelimiters.Checked;
  edtQuoteChar.Enabled := CBDelimiters.Checked and RBQuoteChar.Checked;
  RBBrackets.Enabled := CBDelimiters.Checked;
  edtBrackets.Enabled := CBDelimiters.Checked and RBBrackets.Checked;
  UpdateStatusMsg(false);
end;

procedure TGenerateSQLForm.CBSystemTablesChange(Sender: TObject);
begin
  if Assigned(Connection) then
    RefreshTableList;
end;

procedure TGenerateSQLForm.UpdateStatusMsg(Done: Boolean);
begin
  if Done then
    LStatusMsg.Caption := SSQLGenerated
  else
    LStatusMsg.Caption := '';
end;

end.

