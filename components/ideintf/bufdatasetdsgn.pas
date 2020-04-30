unit bufdatasetdsgn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, bufdataset, ComponentEditors, FieldsEditor;

Type

  { TBufDatasetDesignEditor }

  TBufDatasetDesignEditor = Class(TFieldsComponentEditor)
    FOffset : Integer;
    FDatasetNames: TStrings;
  private
    procedure GetDatasetNames(const S: string);
  Protected
    function GetProjectDir: String; virtual;
    Procedure CreateDataset(aBufDS : TBufDataset); virtual;
    Procedure LoadDataFromFile(aBufDS : TBufDataset); virtual;
    Procedure SaveDataToFile(aBufDS : TBufDataset); virtual;
    procedure CopyFromDataset(aBufDS: TBufDataset); virtual;
  Public
    constructor Create(AComponent: TComponent;   ADesigner: TComponentEditorDesigner); override;
    Destructor Destroy; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

Uses TypInfo, Forms, Dialogs, LazIDEintf, selectdatasetdlg;

resourcestring
  lrsDatasetActive = 'Dataset is already active, close it first.';
  lrsCreateDataset = 'Create dataset';
  lrsLoadFromFile = 'Load data from file';
  lrsSaveToFile = 'Save data to file';
  lrsSelectDataFileName = 'Select a file with data to load into dataset';
  lrsProvideDataFileName = 'Select a data file to save data to';
  lrsBufDSFilters = 'XML data files|*.xml;Binary data files|*.dat';
  lrsCopyFromDataset = 'Copy data from other dataset';
  lrsNoDatasetsAvailableForCopy = 'No dataset available to copy data from.';

{ TBufDatasetDesignEditor }

procedure TBufDatasetDesignEditor.CreateDataset(aBufDS: TBufDataset);
begin
  if aBufDs.Active then
    ShowMessage(lrsDatasetActive)
  else
    aBufDS.CreateDataset;
end;

procedure TBufDatasetDesignEditor.GetDatasetNames(const S: string);
begin
  FDatasetNames.Add(S);
end;

function TBufDatasetDesignEditor.GetProjectDir: String;

begin
  If Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result:= LazarusIDE.ActiveProject.Directory
  else
    Result:=GetUserDir;
end;

procedure TBufDatasetDesignEditor.LoadDataFromFile(aBufDS: TBufDataset);

Var
  Dlg : TOpenDialog;

begin
  Dlg:=TOpenDialog.Create(Application);
  try
    Dlg.InitialDir:=GetProjectDir;
    Dlg.Title:=lrsSelectDataFileName;
    Dlg.Filter:=lrsBufDSFilters;
    Dlg.Options:=Dlg.Options+[ofFileMustExist];
    if Dlg.Execute then
      aBufDS.LoadFromFile(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TBufDatasetDesignEditor.SaveDataToFile(aBufDS: TBufDataset);
Var
  Dlg : TOpenDialog;

begin
  Dlg:=TSaveDialog.Create(Application);
  try
    Dlg.InitialDir:=GetProjectDir;
    Dlg.Title:=lrsProvideDataFileName;
    Dlg.Filter:=lrsBufDSFilters;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    if Dlg.Execute then
      aBufDS.LoadFromFile(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

procedure TBufDatasetDesignEditor.CopyFromDataset(aBufDS : TBufDataset);

Var
  Idx : Integer;
  aDatasetName : string;
  aSrc : TDataset;

begin
  if not Assigned(Designer.PropertyEditorHook) then
    exit;
  FreeAndNil(FDatasetNames); // Free previous instance if any
  FDatasetNames:=TStringList.Create;
  Designer.PropertyEditorHook.GetComponentNames(GetTypeData(TypeInfo(TDataset)),@GetDatasetNames);
  Idx:=FDatasetNames.IndexOf(aBufDS.Name);
  if Idx<>-1 then
    FDatasetNames.Delete(Idx);
  if FDatasetNames.Count=0 then
    begin
    ShowMessage(lrsNoDatasetsAvailableForCopy);
    exit;
    end;
  aDatasetName:=SelectDataset(FDatasetNames);
  if aDatasetName='' then
    exit;
  aSrc:=TDataset(Designer.PropertyEditorHook.GetComponent(aDatasetName));
  if not assigned(aSrc) then
    exit;
  aSrc.Active:=True;
  aBufDS.CopyFromDataset(aSrc,True);
end;

constructor TBufDatasetDesignEditor.Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FOffset:=Inherited GetVerbCount;
end;

destructor TBufDatasetDesignEditor.Destroy;
begin
  FreeAndNil(FDatasetNames);
  inherited Destroy;
end;

function TBufDatasetDesignEditor.GetVerbCount: Integer;
begin
  Result:=FOffset+4;
end;

function TBufDatasetDesignEditor.GetVerb(Index: Integer): string;
begin
  if Index<FOffset then
    Result:=Inherited GetVerb(Index)
  else
    Case Index-FOffset of
      0 : Result:=lrsCreateDataset;
      1 : Result:=lrsLoadFromFile;
      2 : Result:=lrsSaveToFile;
      3 : Result:=lrsCopyFromDataset;
    end;
end;

procedure TBufDatasetDesignEditor.ExecuteVerb(Index: Integer);

Var
  BufDS : TBufDataset;

begin
  BufDS:=GetComponent as TBufDataset;
  if Index<FOffset then
    Inherited ExecuteVerb(Index)
  else
    Case Index-FOffset of
      0 : CreateDataset(BufDS);
      1 : LoadDataFromFile(BufDS);
      2 : SaveDataToFile(BufDS);
      3 : CopyFromDataset(BufDS);
    end;
end;

initialization
  RegisterComponentEditor(TBufDataset, TBufDatasetDesignEditor);
end.

