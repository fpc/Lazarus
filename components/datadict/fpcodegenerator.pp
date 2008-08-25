unit fpcodegenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, db, fpddCodegen, fpDataDict, controls, dialogs;
  
Type
  { TFPCodeGenerator }

  TFPCodeGenerator = Class(TComponent)
  Private
    FDataset : TDataset;
    FFieldDefs : TDDFieldDefs;
    FFileName: String;
    FGenerator : TDDCustomCodeGenerator;
    FShowResult: Boolean;
    FSQL: TStrings;
    FTableNameHint: String;
    function SelectGenerator: TCodeGeneratorItem;
    procedure SetDataset(const AValue: TDataset);
    procedure SetFieldDefs(const AValue: TDDFieldDefs);
    procedure SetSQL(const AValue: TStrings);
    function SetupGenerator : Boolean;
    procedure ShowCode(L: TStrings);
  public
    Constructor Create(AOWner : TComponent); override;
    Destructor Destroy; override;
    Function Execute : Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Published
    Property Dataset : TDataset Read FDataset Write SetDataset;
    Property DDFieldDefs : TDDFieldDefs Read FFieldDefs Write SetFieldDefs;
    Property SQL : TStrings Read FSQL Write SetSQL;
    Property ShowResult : Boolean Read FShowResult Write FShowResult default true;
    Property FileName : String Read FFileName Write FFileName;
    Property TableNameHint : String Read FTableNameHint Write FTableNameHint;
  end;

implementation

uses typinfo, forms, frmSelectCodeGenerator, frmgeneratedcode, frmBaseConfigCodeGenerator;

{ TFPCodeGenerator }

procedure TFPCodeGenerator.SetDataset(const AValue: TDataset);
begin
  If (AValue<>FDataset) then
    begin
    If Assigned(FDataset) then
      FDataset.RemoveFreeNotification(Self);
    FDataset:=AValue;
    If Assigned(FDataset) then
      FDataset.FreeNotification(Self);
    end;
end;

procedure TFPCodeGenerator.SetFieldDefs(const AValue: TDDFieldDefs);
begin
  FFieldDefs.Assign(AVAlue);
end;

procedure TFPCodeGenerator.SetSQL(const AValue: TStrings);
begin
  if FSQL=AValue then exit;
  FSQL.Assign(AValue);
end;

constructor TFPCodeGenerator.Create(AOWner: TComponent);
begin
  inherited Create(AOWner);
  FShowResult:=True;
  FFieldDefs:=TDDFieldDefs.Create('dummy');
  FSQL:=TStringList.Create;
  If (AOwner is TDataset) then
    Dataset:=AOwner as TDataset;
end;

destructor TFPCodeGenerator.Destroy;
begin
  FreeAndNil(FFieldDefs);
  FreeAndNil(FSQL);
  inherited Destroy;
end;

function TFPCodeGenerator.SelectGenerator : TCodeGeneratorItem;

begin
  Result:=Nil;
  With TSelectCodeGeneratorForm.Create(Application) do
    try
      HaveSQL:=SQL.Count<>0;
      HaveFields:=Self.Dataset<>Nil;
      If (ShowModal=mrOK) then
        Result:=SelectedGenerator;
    finally
      Free;
    end;
end;

Function TFPCodeGenerator.SetupGenerator : boolean;

Var
  FP : TFieldPropDefs;
  F : TBaseConfigGeneratorForm;

begin
  If FGenerator.NeedsFieldDefs then
    begin
    FP:=FGenerator.Fields;
    if Assigned(Dataset) then
      FP.FromDataSet(Dataset)
    else
      FP.FromDDFieldDefs(FFieldDefs);
    end;
  If FGenerator.NeedsSQL then
    FGenerator.SQL:=Self.SQL;
  If (TableNameHint<>'') and IsPublishedProp(FGenerator.CodeOptions,'TableName') then
    SetStrProp(FGenerator.CodeOptions,'TableName',TableNameHint);
  F:=TBaseConfigGeneratorForm.Create(Application);
  try
    F.ShowExtra:=True;
    F.FileName:=Self.FileName;
    F.ShowResult:=Self.ShowResult;
    F.Generator:=Self.FGenerator;
    Result:=(F.ShowModal=mrOK);
    If result then
      begin
      Self.FileName   := F.FileName;
      Self.ShowResult := F.ShowResult;
      end;
  finally
    F.Free
  end;
end;

Procedure TFPCodeGenerator.ShowCode(L : TStrings);

begin
  With TCodeForm.Create(Self) do
    try
      Code:=L;
      ShowModal;
    Finally
      Free;
    end;
end;


function TFPCodeGenerator.Execute: Boolean;

Var
  G : TCodeGeneratorItem;
  L : TStrings;

begin
  G:=SelectGenerator;
  Result:=(G<>Nil);
  If Result then
    begin
    FGenerator:=G.GeneratorClass.Create(Self);
    Try
      if SetupGenerator then
        begin
        L:=TStringList.Create;
        try
          FGenerator.GenerateCode(L);
          If (FFileName<>'') then
            L.SaveToFile(UTF8ToSys(FFileName));
          If ShowResult then
            ShowCode(L);
        finally
          L.Free;
        end;
        end;
    Finally
      FreeAndNil(FGenerator);
    end;
    end;
end;

procedure TFPCodeGenerator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  If (Operation=opRemove) then
    begin
    If (AComponent=FDataset) then
      FDataset:=Nil;
    end;
end;

end.

