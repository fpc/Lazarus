{
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
unit SqliteComponentEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, StdCtrls,
  Buttons, customsqliteds, ComponentEditors, LazarusPackageIntf, LazIdeIntf;

type

  {TSqliteEditor}
  
  TSqliteEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

  { TSqliteTableEditorForm }

  TSqliteTableEditorForm = class(TForm)
    butCreate: TButton;
    butClose: TButton;
    butAdd: TButton;
    butDelete: TButton;
    comboFieldType: TComboBox;
    editFieldName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblFilePath: TLabel;
    listFields: TListBox;
    DataSet: TCustomSqliteDataSet;
    procedure LoadCurrentFields;
    procedure FillComboValues;
    procedure SetComboValue(AObject: TObject);
    procedure SqliteTableEditorFormShow(Sender: TObject);
    procedure butAddClick(Sender: TObject);
    procedure butCancelClick(Sender: TObject);
    procedure butDeleteClick(Sender: TObject);
    procedure butOkClick(Sender: TObject);
    procedure comboFieldTypeChange(Sender: TObject);
    procedure editFieldNameEditingDone(Sender: TObject);
    procedure listFieldsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  
implementation

uses
  db;

var
  IsAddingField:Boolean;//hack to avoid LCL bug 1428

function StringListHasDuplicates(const List:TStrings):boolean;
var
  i,j:Integer;
begin
  Result:=False;
  for i := 0 to List.Count - 1 do
    for j:= i+1 to List.Count - 1 do
      if AnsiCompareText(List[i],List[j]) = 0 then
      begin
        Result:=True;
        Exit;
      end;
end;

{TSqliteEditor}

procedure TSqliteEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:Edit;
  end;
end;

function TSqliteEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
    begin
      if not TCustomSqliteDataset(GetComponent).TableExists then
        Result:='Create Table'
      else
        Result:='Edit Table';
    end;
  end;
end;

function TSqliteEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

procedure TSqliteEditor.Edit;
var
  ADataSet:TCustomSqliteDataSet;
  OldDir:String;
begin
  ADataSet:=TCustomSqliteDataSet(GetComponent);
  if ADataSet.Filename = '' then
  begin
    ShowMessage('FileName not set: it''s not possible to create/edit a table');
    exit;
  end;  
  if ADataSet.TableName = '' then
  begin
    ShowMessage('TableName not set: it''s not possible to create/edit a table');
    exit;
  end;
    
  with TSqliteTableEditorForm.Create(Application) do
  begin
    try
      // In case Filename is a relative one, change dir to project dir
      // so the datafile will be created in the right place
      OldDir:=GetCurrentDir;
      if ExtractFilePath (LazarusIDE.ActiveProject.MainFile.FileName) <> '' then
        ChDir(ExtractFilePath (LazarusIDE.ActiveProject.MainFile.FileName));
      Dataset:=ADataset;
      ShowModal;
    finally
      chdir(OldDir);
      Free;
    end;  
  end;    
end;  

{ TSqliteTableEditorForm }

procedure TSqliteTableEditorForm.butAddClick(Sender: TObject);
begin
  //In the case there's no items
  editFieldName.Enabled:=True;
  comboFieldType.Enabled:=True;
  IsAddingField:=True; //to be removed
  listFields.Items.AddObject('AFieldName',TObject(ftString));
  IsAddingField:=False;
  listFields.ItemIndex:=listFields.Items.Count-1;
  editFieldName.Text:='AFieldName';
  editFieldName.SetFocus;
end;

procedure TSqliteTableEditorForm.LoadCurrentFields;
var
  OldSql:String;
  OldActive:Boolean;
  i:Integer;
begin
  with Dataset do
  begin
    OldSql:=Sql;
    OldActive:=Active;
    Sql:='Select * from '+TableName+' where 1 = 0';//dummy sql
    Close;
    Open;
    for i:=0 to FieldDefs.Count - 1 do
      listFields.Items.AddObject(FieldDefs[i].Name,TObject(FieldDefs[i].DataType));
    listFields.ItemIndex:=0;
    Sql:=OldSql;
    Active:=OldActive;
  end;
end;

procedure TSqliteTableEditorForm.FillComboValues;
begin
  with comboFieldType.Items do
  begin
    Clear;
    AddObject('String',TObject(ftString));
    AddObject('Integer',TObject(ftInteger));
    AddObject('LargeInt',TObject(ftLargeInt));
    AddObject('AutoInc',TObject(ftAutoInc));
    AddObject('Word',TObject(ftWord));
    AddObject('Float',TObject(ftFloat));
    AddObject('Currency',TObject(ftCurrency));
    AddObject('Boolean',TObject(ftBoolean));
    AddObject('DateTime',TObject(ftDateTime));
    AddObject('Date',TObject(ftDate));
    AddObject('Time',TObject(ftTime));
    AddObject('Memo',TObject(ftMemo));
  end;
end;

procedure TSqliteTableEditorForm.SetComboValue(AObject: TObject);
var
  AIndex:Integer;
begin
  //warning: using inline in this function causes a crash with fpc 2.0.0
  AIndex:=comboFieldType.Items.IndexOfObject(AObject);

  if AIndex <> -1 then
    comboFieldType.ItemIndex:=AIndex
  else
    raise Exception.Create('TableEditor - FieldType not recognized');
end;

procedure TSqliteTableEditorForm.SqliteTableEditorFormShow(Sender: TObject);
begin
  FillComboValues;
  if Dataset.TableExists then
  begin
    LoadCurrentFields;
  end
  else
  begin
    editFieldName.Enabled:=False;
    comboFieldType.Enabled:=False;
  end;
  lblFilePath.Caption:='File Path: '+ExpandFileName(DataSet.FileName);
  label3.caption:='Table Name: '+ DataSet.TableName;
end;

procedure TSqliteTableEditorForm.butCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSqliteTableEditorForm.butDeleteClick(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex:=listFields.ItemIndex;
  if AIndex <> -1 then
  begin
    listFields.Items.Delete(AIndex);
    if listFields.Items.Count = 0 then
    begin
      editFieldName.Text:='';
      editFieldName.Enabled:=False;
      comboFieldType.ItemIndex:=-1;
      comboFieldType.Enabled:=False;
    end
    else
    begin
      if AIndex <> 0 then
        listFields.ItemIndex:=Pred(AIndex)
      else
        listFields.ItemIndex:=AIndex;
    end;
  end;
end;

procedure TSqliteTableEditorForm.butOkClick(Sender: TObject);
var
  i:Integer;
begin
  if listFields.Items.Count = 0 then
  begin;
    ShowMessage('No fields added - Table will not be created');
    Exit;
  end;
  
  if StringListHasDuplicates(listFields.Items) then
  begin
    ShowMessage('It''s not allowed fields with the same name');
    Exit;
  end;
  
  if Dataset.TableExists then
  begin
    if MessageDlg('A Table named "'+Dataset.TableName+'" already exists. Are you sure you want to replace this table?'#13#10'All data stored will be lost',
       mtWarning,[mbYes,MbNo],0) = mrNo then
      exit
    else
      DataSet.ExecSQL('DROP TABLE '+DataSet.TableName+';');
  end;

  with DataSet.FieldDefs do
  begin
    Clear;
    for i:= 0 to listFields.Items.Count - 1 do
      Add(listFields.Items[i],TFieldType(listFields.Items.Objects[i]));
  end;
  DataSet.CreateTable;

  if Dataset.TableExists then
    ShowMessage('Table created successfully')
  else
    ShowMessage('It was not possible to create the table');
end;

procedure TSqliteTableEditorForm.comboFieldTypeChange(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items.Objects[listFields.ItemIndex]:=TObject(comboFieldType.Items.Objects[comboFieldType.ItemIndex]);
end;

procedure TSqliteTableEditorForm.editFieldNameEditingDone(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items[listFields.ItemIndex]:=editFieldName.Text;
end;

procedure TSqliteTableEditorForm.listFieldsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if (listFields.ItemIndex <> -1) and not IsAddingField then //remove when LCL is fixed
  begin
    editFieldName.Text:=listFields.Items[listFields.ItemIndex];
    SetComboValue(listFields.Items.Objects[listFields.ItemIndex]);
  end;
end;

initialization
  {$i sqlitecomponenteditor.lrs}
  
end.

