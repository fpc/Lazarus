unit reglazdatadict;

{$mode objfpc}{$H+}

interface

uses
  // General units
  Classes, SysUtils, UTF8Process, DB, Typinfo,
  // IDE interface
  MenuIntf, propedits, lresources,
  // Data dict units
  idedatadict,
  fpdatadict,
  frmconfdatadict,
  frmconfprojdatadict,
  // Code generator units
  fpddCodegen,
  fpCodeGenerator,
  LazFileUtils, LazUTF8
{$IFNDEF NOSTDCODEGENS}
  ,fpcgSQLConst
  ,fpcgdbcoll
  ,fpcgCreateDBF
  ,fpcgtiOPF
{$IF FPC_FULLVERSION>=30101}
  ,fpcgfieldmap
  ,fpcgtypesafedataset
{$ENDIF}
{$ENDIF NOSTDCODEGENS}
  ;

Type
  // Object to handle commands and all events.

  { TIDEDataDictCommandHandler }

  TIDEDataDictCommandHandler = Class(TComponent)
  Private
    FVerboseApply : Boolean;
    mnuDDSection,
    mnuCompDDSection : TIDEMenuSection;
    CmdApplyDD,
    //CmdEditFields,
    CmdCreateCode,
    CmdCreateSQL : TIDEMenuCommand;
    procedure ApplyDDToField(Sender: TObject; FD: TDDFieldDef; F: TField; var Allow: Boolean);
    function GetSQLProperty(P: TPersistent; var SQL: String): Boolean;
  Protected
    Procedure DoStartDD(Const AFileName : String); virtual;
    Procedure CheckDataset(Sender : TObject); virtual;
    Procedure CheckSQL(Sender : TObject); virtual;
  Public
    Function GetDataDesktopBinary : String;
    Procedure ShowDDDialog(Sender : TObject);
    Procedure ConfDD(Sender : TObject);
    Procedure OpenDD(Sender : TObject);
    Procedure OpenEmptyDD(Sender : TObject);
    Procedure ApplyDD(Sender : TObject);
    Procedure DesignSQL(Sender : TObject);
    Procedure CreateDDCode(Sender : TObject);
    Property  VerboseApply : Boolean Read FVerboseApply Write FVerboseApply;
  end;

Procedure Register;

Const
{$ifdef windows}
  lazdatadesktop = 'lazdatadesktop.exe';
  ApplicationExt = '.exe';
{$else}
  lazdatadesktop = 'lazdatadesktop';
  ApplicationExt = '';
{$endif}

implementation

{$R reglazdatadict.res}

uses forms, dialogs, controls,
  IDEMsgIntf,
  IDEExternToolIntf,
  ldd_consts;



Function TIDEDataDictCommandHandler.GetDataDesktopBinary : String;

begin
  Result:=DataDesktopBinary;
  If (Result='') or not FileExistsUTF8(Result) then
    begin
    Result:=FileSearchUTF8(LazDataDesktop,GetEnvironmentVariableUTF8('PATH'));
    If (Result='') then
      if MessageDLG(SErrNoDataDesktopDoSelect,mtInformation,[mbYes,mbNo],0)=mrYes then
        begin
        With TOpenDialog.Create(Application) do
          try
            InitialDir:=Application.Location;
            Filter:=SApplicationFilter+ApplicationExt;
            Options:=[ofEnableSizing,ofViewDetail,ofFileMustExist];
            If Execute then
              begin
              Result:=FileName;
              DataDesktopBinary:=Result;
              SaveDDSettings
              end;
          Finally
            Free;
          end;
        end;
    end;
end;


Procedure TIDEDataDictCommandHandler.ShowDDDialog(Sender : TObject);

begin
  InitDDSettings;
  IDEDataDictionary.Update;
  ShowConfigProjectDDDialog;
end;


Procedure TIDEDataDictCommandHandler.DoStartDD(Const AFileName : String);

Var
  FN : String;

begin
  InitDDSettings;
  FN:=GetDataDesktopBinary;
  If (FN='') then
    Exit;
  If (AFileName<>'') then
    If Pos(' ',AFileName)=0 then
      FN:=FN+' '+AFileName
    else
      FN:=FN+' -f "'+AFileName+'"';
  With TProcessUTF8.Create(Nil) do
    try
      CommandLine:=FN;
      Execute;
    Finally
      Free;
    end;
end;

procedure TIDEDataDictCommandHandler.CheckDataset(Sender: TObject);

Var
  ASelection : TPersistentSelectionList;
  OK : Boolean;
  I : Integer;
  
begin
  IDEDataDictionary.Update;
  OK:=IDEDataDictionary.Active;
  If OK then
    begin
    ASelection:=TPersistentSelectionList.Create;
    try
      GlobalDesignHook.GetSelection(ASelection);
      OK:=(ASelection.Count>0);
      If OK then
        begin
        I:=0;
        While OK and (I<ASelection.Count) do
          begin
          OK:=ASelection[i] is TDataset;
          Inc(I);
          end;
        end;
    finally
      ASelection.Free;
    end;
    end;
  cmdApplyDD.Enabled:=OK;
end;

Function TIDEDataDictCommandHandler.GetSQLProperty(P : TPersistent; Var SQL : String) : Boolean;

Const
  StringProps = [tkSString,tkLString,tkAString,tkWString];

Var
  PI : PPropInfo;
  PN : String;
  
begin
  PN:='sql';
  PI:=GetPropInfo(P,PN);
  Result:=(PI<>Nil);
  If Not Result then
    begin
    PN:='commandtext';
    PI:=GetPropInfo(P,PN);
    Result:=(PI<>Nil);
    end;
  If Not Result then
    begin
    PN:='query';
    PI:=GetPropInfo(P,PN);
    Result:=(PI<>Nil);
    end;
  If Result then
    begin
    Result:=(PropType(P,PN) in StringProps);
    If Result then
      SQL:=GetStrProp(P,PN)
    else
      begin
      Result:=(PropIsType(P,PN,tkObject) and
              (GetObjectProp(P,PI,TStrings)<>Nil));
      If Result then
        SQL:=TStrings(GetObjectProp(P,PI,TStrings)).Text;
      end;
    end;
end;

procedure TIDEDataDictCommandHandler.CheckSQL(Sender: TObject);


Var
  ASelection : TPersistentSelectionList;
  OK : Boolean;
  SQL : String;

begin
  IDEDataDictionary.Update;
  OK:=IDEDataDictionary.Active;
  If OK then
    begin
    ASelection:=TPersistentSelectionList.Create;
    try
      GlobalDesignHook.GetSelection(ASelection);
      OK:=(ASelection.Count=1);
      If OK then
        begin
        OK:=GetSQLProperty(ASelection[0],SQL);
        end;
    finally
      ASelection.Free;
    end;
    end;
  cmdCreateSQL.Enabled:=OK;
end;

Procedure TIDEDataDictCommandHandler.ConfDD(Sender : TObject);

begin
  ShowConfigIDEDataDictDialog;
end;

Procedure TIDEDataDictCommandHandler.OpenDD(Sender : TObject);

begin
  DoStartDD(IDEDataDictionary.FileName);
end;

Procedure TIDEDataDictCommandHandler.OpenEmptyDD(Sender : TObject);

begin
  DoStartDD('');
end;

Procedure TIDEDataDictCommandHandler.ApplyDD(Sender : TObject);

Var
  ASelection : TPersistentSelectionList;
  I : Integer;
  DS : TDataset;

begin
  IDEDataDictionary.Update;
  if Not IDEDataDictionary.Active then
    begin
    ShowMessage(SErrNoDatadictActive);
    Exit;
    end;
  ASelection:=TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelection);
    For I:=0 to ASelection.Count-1 do
      if (ASelection[i] is TDataset) then
        begin
        DS:=TDataset(ASelection[i]);
        IDEMessagesWindow.AddCustomMessage(mluImportant,Format(SApplyingDDToDataset,[DS.Name]));
        IDEDataDictionary.Dictionary.ApplyToDataset(DS,@ApplyDDToField);
        end;
  finally
    ASelection.Free;
  end;
end;

Procedure TIDEDataDictCommandHandler.ApplyDDToField(Sender : TObject; FD : TDDFieldDef;F : TField; Var Allow : Boolean);

begin
  If (FD<>Nil) then
    begin
    If VerboseApply then
      IDEMessagesWindow.AddCustomMessage(mluImportant,Format(SApplyingDDToField,[F.FieldName]));
    end
  else
    IDEMessagesWindow.AddCustomMessage(mluImportant,Format(SWarningNoDDForField,[F.FieldName]));
end;

Procedure TIDEDataDictCommandHandler.DesignSQL(Sender : TObject);

begin
  ShowMessage(SNotYetImplemted);
end;

procedure TIDEDataDictCommandHandler.CreateDDCode(Sender: TObject);

Var
  ASelection : TPersistentSelectionList;
  DS : TDataset;
  HaveSQL : Boolean;
  ASQL :String;

begin
  ASelection:=TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelection);
    If (ASelection.Count=1) then
      begin
      if (ASelection[0] is TDataset) then
        begin
        DS:=TDataset(ASelection[0]);
        HaveSQL:=GetSQLproperty(DS,ASQL);
        With TFPCodeGenerator.Create(DS) do
          Try
            If HaveSQL then
              SQL.Text:=ASQL;
            Execute;
          Finally
            Free;
          end;
        end;
      end
    else
      ShowMessage(Format(SWrongSelection, [ASelection.Count]));
  finally
    ASelection.Free;
  end;
end;

Var
  IDEDDC : TIDEDataDictCommandHandler;

Procedure InitDDC;

begin
  If not Assigned(IDEDDC) then
    IDEDDC:=TIDEDataDictCommandHandler.Create(Nil);
end;

Procedure Register;


begin
  RegisterComponents('Data Access',[TFPCodeGenerator]);
  InitDDC;
  // Main menu
  With IDEDDC do
    begin
    mnuDDSection:=RegisterIDESubMenu(mnuProject,'Datadict',SMenuDataDict,Nil,Nil);
    RegisterIDEMenuCommand(mnuDDSection,'set',SMenuConfProjDataDict,@ShowDDDialog,Nil);
    RegisterIDEMenuCommand(mnuDDSection,'open',SMenuOpenDataDict,@OpenDD,Nil);
    RegisterIDEMenuCommand(mnuDDSection,'conf',SMenuConfDataDict,@ConfDD,Nil);
    // Form designer menu
    mnuCompDDSection:=RegisterIDESubMenu(DesignerMenuSectionCustomDynamic,'comdatadict',SMenuDataDict,Nil,Nil);
    mnuCompDDSection.AddHandlerOnShow(@CheckDataset);
    CmdApplyDD:=RegisterIDEMenuCommand(mnuCompDDSection,'ddapply',SMenuDatadictApply,@ApplyDD,Nil,Nil);
//  RegisterIDEMenuCommand(mnuCompDDSection,'ddeditfields',SMenuDatadictApply,@IDEDDC.ApplyDD,Nil,Nil);
    CmdCreateSQL:=RegisterIDEMenuCommand(mnuCompDDSection,'dddesignsql',SMenuDatadictDesignSQL,@DesignSQL,Nil,Nil);
    CmdCreateCode:=RegisterIDEMenuCommand(mnuCompDDSection,'ddcreatecode',SMenuDatadictCreateCode,@CreateDDCode,Nil,Nil);
    RegisterIDEMenuCommand(itmSecondaryTools,'Datadict',SMenuDatadesktop,@OpenEmptyDD,Nil);
    end;
//  RegisterComponentEditor(TDataset, TDataDictComponentEditor);
end;

Initialization

Finalization
  FreeAndNil(IDEDDC)
end.

