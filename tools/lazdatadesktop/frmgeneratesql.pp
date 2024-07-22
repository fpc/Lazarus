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
unit frmgeneratesql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, ButtonPanel, RTTICtrls, fpdatadict,
  lazdatadeskstr;

type

  { TGenerateSQLForm }

  TGenerateSQLForm = class(TForm)
    BCopyAllToClipboard1: TButton;
    BCopyCreateSQLToClipboard: TButton;
    BCopySelectToClipboard: TButton;
    BCopyInsertSQLToClipboard: TButton;
    BCopyUpdateSQLToClipboard: TButton;
    BCopyDeleteSQLToClipboard1: TButton;
    BGenerate: TButton;
    BCopyAllToClipboard: TButton;
    ButtonPanel: TButtonPanel;
    CBTables: TComboBox;
    CBIgnoreSelection: TCheckBox;
    CBQuoteSQL: TCheckBox;
    CBPreserveLineFeeds: TCheckBox;
    CBAddConst: TCheckBox;
    GBCode: TGroupBox;
    LBKeyFields: TListBox;
    LCBTables: TLabel;
    Label2: TLabel;
    LLBKeyFields: TLabel;
    LBFields: TListBox;
    LSEIndent: TLabel;
    LSELineLength: TLabel;
    MDelete: TMemo;
    MCreate: TMemo;
    MUpdate: TMemo;
    MInsert: TMemo;
    MSelect: TMemo;
    pSQLSelect: TPanel;
    PKeyFields: TPanel;
    POptions: TPanel;
    PSelectFields: TPanel;
    PCSQL: TPageControl;
    pSQLSelect1: TPanel;
    pSQLSelect2: TPanel;
    pSQLSelect3: TPanel;
    pSQLSelect4: TPanel;
    SELineLength: TTISpinEdit;
    SEIndent: TTISpinEdit;
    CLBOptions: TTICheckGroup;
    TSCreate: TTabSheet;
    TSFields: TTabSheet;
    TSSelect: TTabSheet;
    TSInsert: TTabSheet;
    TSUpdate: TTabSheet;
    TSDelete: TTabSheet;
    procedure BCopyCreateSQLToClipboardClick(Sender: TObject);
    procedure BCopyDeleteSQLToClipboard1Click(Sender: TObject);
    procedure BCopyInsertSQLToClipboardClick(Sender: TObject);
    procedure BCopySelectToClipboardClick(Sender: TObject);
    procedure BCopyAllToClipboardClick(Sender: TObject);
    procedure BCopyUpdateSQLToClipboardClick(Sender: TObject);
    procedure BGenerateClick(Sender: TObject);
    procedure CBTablesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TSResize(Sender: TObject);
  private
    FTableDefs : TDDTableDefs;
    FGenerator : TFPDDSQLEngine;
    FSQLGenerated : Boolean;
    procedure CopySQLToClipBoard(aSelectedMemo: TMemo);
    function GetAS: Boolean;
    function GetSQLStatement(Index: integer): TStrings;
    function GetTableDef: TDDTableDef;
    function GetTableName: String;
    procedure PostProcess(SQL: TStrings; aConstName: string; Dest: TStrings);
    procedure SetAS(AValue: Boolean);
    procedure SetTableDefs(const AValue: TDDTableDefs);
    procedure SetTableName(const AValue: String);
    Procedure SetFieldLists(TD : TDDTableDef);
    { private declarations }
  public
    { public declarations }
    Procedure RefreshTableList;
    Procedure GenerateSQL;
    Procedure ClearSQL;
    Class Function GenerateSQLDialog(TDS : TDDTableDefs; TN : String; AllowChangeTable : Boolean = True) : Boolean;
    Property TableDefs : TDDTableDefs Read FTableDefs Write SetTableDefs;
    Property TableName : String Read GetTableName Write SetTableName;
    Property SelectSQL : TStrings Index 0 Read GetSQLStatement;
    Property InsertSQL : TStrings Index 1 Read GetSQLStatement;
    Property UpdateSQL : TStrings Index 2 Read GetSQLStatement;
    Property DeleteSQL : TStrings Index 3 Read GetSQLStatement;
    Property CreateSQL : TStrings Index 4 Read GetSQLStatement;
    Property TableDef  : TDDTableDef Read GetTableDef;
    Property AllowSelectTable : Boolean Read GetAS Write SetAS;
  end; 

var
  GenerateSQLForm: TGenerateSQLForm;


implementation

uses Clipbrd;

Class Function TGenerateSQLFOrm.GenerateSQLDialog(TDS : TDDTableDefs; TN : String; AllowChangeTable : Boolean = True) : Boolean;

begin
  With TGenerateSQLFOrm.Create(Application) do
    try
      TableDefs:=TDS;
      TableName:=TN;
      AllowSelectTable:=AllowChangeTable;
      Result:=ShowModal=mroK;
    Finally
      Free;
    end;
end;

{$R *.lfm}

{ TGenerateSQLForm }
  
procedure TGenerateSQLForm.TSResize(Sender: TObject);

Var
  W : Integer;
  
begin
  W:=TSFields.CLientWidth div 3;
  POPtions.Width:=W;
  PSelectFIelds.Width:=W;
end;

procedure TGenerateSQLForm.SetTableDefs(const AValue: TDDTableDefs);
begin
  if (FTableDefs=AValue) then
    exit;
  FTableDefs:=AValue;
  RefreshTableList;
end;

function TGenerateSQLForm.GetTableName: String;
begin
  Result:=Trim(CBTables.Text);
end;

procedure TGenerateSQLForm.SetAS(AValue: Boolean);
begin
  CBTables.Enabled:=AValue;
end;

function TGenerateSQLForm.GetSQLStatement(Index: integer): TStrings;
begin
  Case Index of
    0 : Result:=MSelect.Lines;
    1 : Result:=MInsert.Lines;
    2 : Result:=MUpdate.Lines;
    3 : Result:=MDelete.Lines;
    4 : Result:=MCreate.Lines;
  end;
end;

function TGenerateSQLForm.GetAS: Boolean;
begin
  Result:=CBTables.Enabled;
end;

function TGenerateSQLForm.GetTableDef: TDDTableDef;
begin
  With CBTables do
    If (ItemIndex=-1) then
      Result:=Nil
    else
      Result:=Items.Objects[ItemIndex] as TDDTableDef;
end;

Procedure TGenerateSQLForm.RefreshTableList;

Var
  TN : String;
  I : Integer;
begin
  TN:=GetTableName;
  With CBTables.Items do
    begin
    Clear;
    If Assigned(FTableDefs) then
      For I:=0 to FTableDefs.Count-1 do
        AddObject(Trim(FTableDefs[i].TableName),FTableDefs[i]);
    end;
  With CBTables do
    If (TN<>'') then
      ItemIndex:=Items.IndexOf(TN);
end;

procedure TGenerateSQLForm.ClearSQL;

begin
  MSelect.Clear;
  MInsert.Clear;
  MUpdate.Clear;
  MDelete.Clear;
  MCreate.Clear;
  FSQLGenerated:=False;
  BCopyAllToClipboard.Enabled:=False;

  ButtonPanel.OKButton.Default:=False;
  BGenerate.Default:=True;
end;

procedure TGenerateSQLForm.PostProcess(SQL: TStrings; aConstName : string; Dest : TStrings);

Var
  lIndent : String;
  i : Integer;
  lLine,lConst : String;
  lAddConst,lPreserveLF : Boolean;

begin
  lIndent:='  ';
  Dest.BeginUpdate;
  try
    Dest.Clear;
    lAddConst:=CBAddConst.Checked;
    lPreserveLF:=CBPreserveLineFeeds.Checked;
    if not CBQuoteSQL.Checked then
      Dest.Assign(SQL)
    else
      begin
      if lAddConst then
        begin
        lConst:='SQL'+aConstName+GetTableName;
        Dest.Add('  '+lConst+' = ');
        lIndent:=lIndent+'  ';
        end;
      For I:=0 to SQL.Count-1 do
        begin
        lLine:=SQL[I];
        lLine:=''''+StringReplace(lLine,'''','''''',[rfReplaceAll]);
        if I=SQL.Count-1 then
          begin
          lLine:=lLine+'''';
          if lAddConst then
            lLine:=lLine+';';
          end
        else
          begin
          if lPreserveLF then
            lLine:=lLine+''' + sLineBreak+'
          else
            lLine:=lLine+' '' + ';
          end;
        Dest.Add(lIndent+lLine);
        end;
      end;
    SQL.Clear;
  finally
    Dest.EndUpdate;
  end;
end;

procedure TGenerateSQLForm.GenerateSQL;

  Function CreateFieldListFromLB(LB : TListBox) : TFPDDFieldList;
  
  Var
    I : integer;
  
  begin
    Result:=TFPDDFieldList.Create(False);
    try
      With LB do
        For I:=0 to Items.Count-1 do
          If Selected[i] then
            Result.Add(Items.Objects[i]);
    except
      Result.Free;
      Raise;
    end;
  end;

Var
  KL,FL : TFPDDFieldList;
  lSQL : TStringList;
  
begin
  ClearSQL;
  If (TableName='') then
    Raise Exception.Create(SErrSelectTable);
  If (LBFields.SelCount=0) then
    Raise Exception.Create(SErrSelectFields);
  lSQL:=Nil;
  FL:=Nil;
  KL:=CreateFieldListFromLB(LBKeyFields);
  try
    lSQL:=TStringList.Create;
    FL:=CreateFieldListFromLB(LBFields);
      With FGenerator do
        begin
        TableDef:=Self.TableDef;
        TableDef.TableName:=Trim(TableDef.TableName);
        CreateSelectSQLStrings(FL,KL,lSQL);
        PostProcess(lSQL,'Select',MSelect.Lines);
        CreateInsertSQLStrings(FL,lSQL);
        PostProcess(lSQL,'Insert',MInsert.Lines);
        CreateUpdateSQLStrings(FL,KL,lSQL);
        PostProcess(lSQL,'Update',MUpdate.Lines);
        CreateDeleteSQLStrings(KL,lSQL);
        PostProcess(lSQL,'Delete',MDelete.Lines);
        If CBIgnoreSelection.Checked  then
          CreateTableSQLStrings(lSQL)
        else
          CreateCreateSQLStrings(FL,KL,lSQL);
        PostProcess(lSQL,'Create',MCreate.Lines);
        end;
      FSQLGenerated:=True;
      BCopyAllToClipboard.Enabled:=True;
      BGenerate.Default:=False;
      ButtonPanel.OKButton.Default:=True;
      PCSQL.ActivePage:=TSSelect;
  finally
    lSQL.Free;
    FL.Free;
    KL.Free;
  end;
end;

procedure TGenerateSQLForm.SetTableName(const AValue: String);

begin
  With CBTables do
    begin
    ItemIndex:=Items.IndexOf(Trim(AValue));
    CBTablesChange(CBTables);
    end;
end;

procedure TGenerateSQLForm.SetFieldLists(TD: TDDTableDef);

  Procedure FillLB(LB : TListBox);

  Var
    I : Integer;
    
  begin
    With LB.Items do
      begin
      Clear;
      If Assigned(TD) then
        For I:=0 to TD.Fields.Count-1 do
          AddObject(TD.FIelds[i].FieldName,TD.FIelds[i]);
      end;
  end;
  
begin
  FillLB(LBKeyFields);
  FillLB(LBFields);
end;

procedure TGenerateSQLForm.CBTablesChange(Sender: TObject);

begin
  With CBTables do
    If (ItemIndex<>-1) Then
      SetFieldLists(Items.Objects[ItemIndex] as TDDTableDef)
    else
      SetFieldLists(Nil);
  ClearSQL;
end;

procedure TGenerateSQLForm.FormCreate(Sender: TObject);
begin
  //
  Caption:= sld_Generatesqlstatements;
  TSFields.Caption:= sld_Tableandfields;
  TSSelect.Caption:= sld_Select;
  TSInsert.Caption:= sld_Insert;
  TSUpdate.Caption:= sld_Update;
  TSDelete.Caption:= sld_Delete;
  TSCreate.Caption:= sld_Createtable;
  LCBTables.Caption:= sld_Table;
  LLBKeyFields.Caption:= sld_Keyfields;
  Label2.Caption:= sld_Selectupdateinsertfields;
  CLBOptions.Caption:= sld_Options;
  LSEIndent.Caption:= sld_Indent;
  LSELineLength.Caption:= sld_Linelength;
  CBIgnoreSelection.Caption:= sld_Createfulltablecreationsql;
  BGenerate.Caption:= sld_Generatesql;
  ButtonPanel.CancelButton.Caption:= sld_Cancel;
  ButtonPanel.OKButton.Caption:= sld_Ok;

  CLBOptions.Link.AliasValues.Values['eoLineFeedAfterField'] := eoLineFeedAfterField;
  CLBOptions.Link.AliasValues.Values['eoUseOldInWhereParams'] := eoUseOldInWhereParams;
  CLBOptions.Link.AliasValues.Values['eoAndTermsInBrackets'] := eoAndTermsInBrackets;
  CLBOptions.Link.AliasValues.Values['eoQuoteFieldNames'] := eoQuoteFieldNames;
  CLBOptions.Link.AliasValues.Values['eoLineFeedAfterAndTerm'] := eoLineFeedAfterAndTerm;
  CLBOptions.Link.AliasValues.Values['eoAddTerminator'] := eoAddTerminator;
  CLBOptions.Link.AliasValues.Values['eoSkipForeignKeys'] := eoSkipForeignKeys;
  //
  FGenerator:=TFPDDSQLEngine.Create;
  CLBOptions.Link.TIObject:=FGenerator;
  SEIndent.Link.TIObject:=FGenerator;
  SELineLength.Link.TIObject:=FGenerator;
end;

procedure TGenerateSQLForm.BGenerateClick(Sender: TObject);
begin
  GenerateSQL;
end;

procedure TGenerateSQLForm.BCopyAllToClipboardClick(Sender: TObject);

begin
  CopySQLToClipBoard(Nil);
end;

procedure TGenerateSQLForm.BCopyUpdateSQLToClipboardClick(Sender: TObject);
begin
  CopySQLToClipBoard(MUpdate);
end;

procedure TGenerateSQLForm.BCopySelectToClipboardClick(Sender: TObject);
begin
  CopySQLToClipBoard(MSelect);
end;

procedure TGenerateSQLForm.BCopyInsertSQLToClipboardClick(Sender: TObject);
begin
  CopySQLToClipBoard(MInsert);
end;

procedure TGenerateSQLForm.BCopyDeleteSQLToClipboard1Click(Sender: TObject);
begin
  CopySQLToClipBoard(MDelete);
end;

procedure TGenerateSQLForm.BCopyCreateSQLToClipboardClick(Sender: TObject);
begin
  CopySQLToClipBoard(MCreate);
end;

Procedure TGenerateSQLForm.CopySQLToClipBoard(aSelectedMemo: TMemo);

Var
  L: TStrings;

  procedure AddLines(M : TMemo; aComment : string);

  begin
    if (aSelectedMemo<>Nil) and (M<>aSelectedMemo) then
      Exit;
    if not CBQuoteSQL.Checked  then
      L.Add('-- '+GetTableName+': '+aComment)
    else if not CBAddConst.Checked then
      L.Add('// '+GetTableName+': '+aComment);
    L.Add('');
    L.AddStrings(M.Lines);
  end;

begin
  L:=TStringList.Create;
  try
    AddLines(MSelect,'Select');
    L.Add('');
    AddLines(MInsert,'Insert');
    L.Add('');
    AddLines(MUpdate,'Update');
    L.Add('');
    AddLines(MDelete,'Delete');
    L.Add('');
    AddLines(MCreate,'Create');
    Clipboard.AsText:=L.Text;
  finally
    L.Free;
  end;
end;


end.

