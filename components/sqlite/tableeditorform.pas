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
unit tableeditorform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, StdCtrls,
  Buttons, sqliteds, ComponentEditors, LazarusPackageIntf, PropEdits, LazIdeIntf;

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
    DataSet: TSqliteDataSet;
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
  
  procedure Register;
  
var
  SqliteTableEditorForm: TSqliteTableEditorForm;
  
implementation

procedure RegisterUnitSqliteds;
begin
  RegisterComponents('Data Access',[TSqliteDataset]);
end;  

procedure Register;
begin
  RegisterUnit('sqliteds',@RegisterUnitSqliteds);
  RegisterComponentEditor(TSqliteDataset,TSqliteEditor) ;
  RegisterPropertyEditor(TypeInfo(String),TSqliteDataset,'FileName',
                         TFileNamePropertyEditor);
end;  

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
    0:Result:='Create Table';
  end;
end;

function TSqliteEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

procedure TSqliteEditor.Edit;
var
  ADataSet:TSqliteDataSet;
  OldDir:String;
begin
  ADataSet:=TSqliteDataSet(GetComponent);
  if ADataSet.Filename = '' then
  begin
    ShowMessage('FileName not set: it''s not possible to create a table');
    exit;
  end;  
  if ADataSet.TableName = '' then
  begin
    ShowMessage('TableName not set: it''s not possible to create a table');
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
  listFields.Items.AddObject('AFieldName',TObject(0));
  listFields.ItemIndex:=listFields.Items.Count-1;
  comboFieldType.ItemIndex:=0;
  editFieldName.Text:='AFieldName';
  editFieldName.SetFocus;
end;

procedure TSqliteTableEditorForm.SqliteTableEditorFormShow(Sender: TObject);
begin
  lblFilePath.Caption:='File Path: '+ExpandFileName(DataSet.FileName);
  label3.caption:='Table Name: '+ DataSet.TableName;
end;

procedure TSqliteTableEditorForm.butCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TSqliteTableEditorForm.butDeleteClick(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items.Delete(listFields.ItemIndex);
end;

procedure TSqliteTableEditorForm.butOkClick(Sender: TObject);
var
  i:Integer;
  ASql:String;
begin
  if Dataset.TableExists then
  begin
    if MessageDlg('A Table named "'+Dataset.TableName+'"already exists. Are you sure you want to replace this table?'#13#10'All data stored will be lost',
       mtWarning,[mbYes,MbNo],0) = mrNo then
      exit
    else
      DataSet.ExecSQL('DROP TABLE '+DataSet.TableName+';');
  end;

  if listFields.Items.Count = 0 then
  begin;
    ShowMessage('No fields added');
    Exit;
  end;
  
  if StringListHasDuplicates(listFields.Items) then
  begin
    ShowMessage('It''s not allowed fields with the same name');
    Exit;
  end;

  ASql:='CREATE TABLE '+ DataSet.TableName + ' (';
  with listFields do
  for i := 0 to Items.Count - 1 do
  begin
    ASql:=ASql+Items[i]+' ';
    if comboFieldType.Items[longint(Items.Objects[i])] = 'String' then
      ASql:=ASql+'VARCHAR'
    else
      ASql:=ASql+ Upcase(comboFieldType.Items[longint(Items.Objects[i])]);
    if i <>  Items.Count - 1 then
      ASql:=ASql+ ' , ';
  end;
  ASql:=ASql+');';
  DataSet.ExecSQL(ASql);
  if Dataset.TableExists then
    ShowMessage('Table created successfully')
  else
    ShowMessage('It was not possible to create the table');
end;

procedure TSqliteTableEditorForm.comboFieldTypeChange(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items.Objects[listFields.ItemIndex]:=TObject(comboFieldType.ItemIndex);
end;

procedure TSqliteTableEditorForm.editFieldNameEditingDone(Sender: TObject);
begin
  if listFields.ItemIndex <> -1 then
    listFields.Items[listFields.ItemIndex]:=editFieldName.Text;
end;

procedure TSqliteTableEditorForm.listFieldsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if listFields.ItemIndex <> -1 then
  begin
    editFieldName.Text:=listFields.Items[listFields.ItemIndex];
    comboFieldType.ItemIndex:=LongInt(listFields.Items.Objects[listFields.ItemIndex]);
  end;
end;

initialization
  {$I tableeditorform.lrs}
  {$i sqliteds.lrs}

end.

