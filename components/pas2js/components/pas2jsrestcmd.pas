unit pas2jsrestcmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MenuIntf, propedits, DB, IDEExternToolIntf, stub.restdataset;


Type
  { TIDEPas2JSRestCommandHandler }
  TLogEvent = Procedure(Sender: TObject; Const Msg : String) of object;

  TIDEPas2JSRestCommandHandler = Class(TComponent)
  Private
    FCmdGenHTML: TIDEMenuCommand;
    FCmdGetFieldDefs: TIDEMenuCommand;
    FCmdGetParamDefs: TIDEMenuCommand;
    FCmdShowData: TIDEMenuCommand;
    FLogEnabled: Boolean;
    FmnuCompRestSection: TIDEMenuSection;
    FOnLog: TLogEvent;
  Protected
    // Logging
    Procedure DoLog(Const Msg : String); overload;
    Procedure DoLog(Const Fmt : String; Const Args : Array of const); overload;
    // Get selection if it is a single TSQLDBRestDataset
    function GetDataset: TSQLDBRestDataset;
  Public
    // Register commands in component menu.
    Procedure RegisterCommands;
    // Check whether dataset is TSQLDBRestDataset
    Procedure CheckDataset(Sender : TObject); virtual;
    // Create HTML form
    procedure CreateHTML(Sender: TObject); virtual;
    // Create fielddefs for dataset
    Procedure CreateFieldDefs(Sender : TObject); virtual;
    // Create parameters for dataset
    Procedure CreateParams(Sender : TObject); virtual;
    // Show the data that would be fetched by dataset
    Procedure ShowData(Sender : TObject); virtual;
    // Public access to menu itrms.
    Property CmdShowData : TIDEMenuCommand Read FCmdShowData Write FCmdShowData;
    Property CmdGetFieldDefs : TIDEMenuCommand Read FCmdGetFieldDefs Write FCmdGetFieldDefs;
    Property CmdGetParamDefs : TIDEMenuCommand Read FCmdGetParamDefs Write FCmdGetParamDefs;
    Property CmdGenHTML : TIDEMenuCommand Read FCmdGenHTML Write FCmdGenHTML;
    Property mnuCompRestSection : TIDEMenuSection Read FmnuCompRestSection Write FmnuCompRestSection;
    // Logging
    Property OnLog : TLogEvent read FOnLog write FOnLog;
    Property LogEnabled : Boolean Read FLogEnabled Write FLogEnabled;
  end;

Var
  RestCmdHandler : TIDEPas2JSRestCommandHandler;


implementation

uses clipbrd, controls, datasettoform, frmdatasettoform,  dialogs, bufdataset, strpas2jscomponents, pas2jsrestutils, frmRestData, FormEditingIntf, ComponentEditors;

procedure TIDEPas2JSRestCommandHandler.CreateHTML(Sender: TObject);

var
  DS : TDataset;
  Source : TDatasource;
  Gen : TDatasetHTMLGenerator;
  Frm : TfrmDatasetToHTMLForm;
  HTML : String;

begin
  DS:=GetDataset;
  if DS=Nil then
    exit;
  Frm:=nil;
  Source:=nil;
  Gen:=TDatasetHTMLGenerator.Create(Self);
  try
    Source:=TDatasource.Create(Self);
    Source.Dataset:=DS;
    Gen.Datasource:=Source;
    Gen.PopulateEntries;
    if Gen.FieldEntries.Count=0 then
      begin
      ShowMessage(rsNoFieldsAvailable);
      exit;
      end;
    Frm:=TfrmDatasetToHTMLForm.Create(Self);
    Frm.Generator:=Gen;
    if Frm.ShowModal=mrOK then
      begin
      HTML:=Frm.GetHTML;
      Clipboard.AsText:=HTML;
      ShowMessage(rsHTMLCopiedToClipBoard);
      end;
  finally
    Frm.Free;
    Gen.Free;
    Source.Free;
  end;
end;

procedure TIDEPas2JSRestCommandHandler.DoLog(const Msg: String);
begin
  if FLogEnabled and Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure TIDEPas2JSRestCommandHandler.DoLog(const Fmt: String; const Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

function TIDEPas2JSRestCommandHandler.GetDataset: TSQLDBRestDataset;

Var
  ASelection : TPersistentSelectionList;

begin
  Result:=nil;
  ASelection:=TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelection);
    if (ASelection.Count=1) and (ASelection[0] is TSQLDBRestDataset) then
      Result:=TSQLDBRestDataset(ASelection[0]);
  finally
    ASelection.Free;
  end;
end;

procedure TIDEPas2JSRestCommandHandler.RegisterCommands;
begin
  // Form designer menu
  mnuCompRestSection:=RegisterIDESubMenu(DesignerMenuSectionCustomDynamic,'comPas2JSRest',rsPas2JSRest,Nil,Nil);
//  DesignerMenuSectionCustomDynamic.AddHandlerOnShow(@CheckDataset);
  mnuCompRestSection.AddHandlerOnShow(@CheckDataset);
//  RegisterIDEMenuCommand(mnuCompDDSection,'ddeditfields',SMenuDatadictApply,@IDEDDC.ApplyDD,Nil,Nil);
  CmdShowData:=RegisterIDEMenuCommand(mnuCompRestSection,'showData',rsMenuRestShowData,@ShowData,Nil,Nil);
  CmdGetFieldDefs:=RegisterIDEMenuCommand(mnuCompRestSection,'createFieldDefs',rsMenuRestCreateFieldDefs,@CreateFieldDefs,Nil,Nil);
  CmdGetParamDefs:=RegisterIDEMenuCommand(mnuCompRestSection,'createParamDefs',rsMenuRestCreateParamDefs,@CreateParams,Nil,Nil);
  CmdGenHTML:=RegisterIDEMenuCommand(mnuCompRestSection,'gendatahtml',rsMenuGenHTMLForm,@CreateHTML,Nil,Nil);
end;

procedure TIDEPas2JSRestCommandHandler.CheckDataset(Sender: TObject);

Var
  DS : TSQLDBRestDataset;
  MDOK,OK,HaveFields : Boolean;

begin
  DS:=GetDataset;
  OK:=(DS<>Nil) and (DS.Connection<>Nil) and (DS.ResourceName<>'');
  if OK then
    begin
    MDOK:=OK and (DS.Connection.MetaDataResourceName<>'');
    HaveFields:=((DS.FieldDefs.Count>0) or (DS.Fields.Count>0));
    end
  else
    begin
    MDOK:=False;
    HaveFields:=False;
    end;
  CmdShowData.Enabled:=OK;
  CmdGetParamDefs.Enabled:=OK;
  CmdGenHTML.Enabled:=OK and HaveFields;
  CmdGetFieldDefs.Enabled:=MDOK;
end;

procedure TIDEPas2JSRestCommandHandler.CreateFieldDefs(Sender: TObject);

Var
  DS : TSQLDBRestDataset;
  aCount: Integer;
  CED : TComponentEditorDesigner;

begin
  DS:=GetDataset;
  if (DS=nil) or (DS.Connection=Nil) then exit;
  if DS.Connection.MetaDataResourceName='' then
    begin
    Dolog('%s: %s',[DS.Connection.Name,rsNoMetaDataResource]);
    ShowMessage(rsNoMetaDataResourceCannotCreateFieldDefs);
    exit;
    end;
  if (DS.ResourceName='') then
    begin
    DoLog('%s: %s',[DS.Name,rsNoResource]);
    ShowMessage(rsNoResourceCannotCreateFieldDefs);
    Exit;
    end;
  DS.FieldDefs.Clear;
  aCount:=IDERestUtils.UpdateFieldDefs(DS.Connection,IDERestUtils.GetFullResourceName(DS),DS.FieldDefs);
  if aCount=-1 then
    ShowMessage(rsServerRequestFailedCannotCreateFieldDefs)
  else if aCount=0 then
    ShowMessage(rsCreateFieldDefsNoNew)
  else
    ShowMessage(Format(rsCreateFieldDefsCount,[aCount]));
  if (FormEditingHook.GetCurrentDesigner is TComponentEditorDesigner) then
    begin
    CED:=(FormEditingHook.GetCurrentDesigner as TComponentEditorDesigner);
    CED.Modified;
    if assigned(CED.PropertyEditorHook) then
     CED.PropertyEditorHook.RefreshPropertyValues;
    end;
end;

procedure TIDEPas2JSRestCommandHandler.CreateParams(Sender: TObject);
Var
  DS : TSQLDBRestDataset;
  aCount: Integer;
  CED : TComponentEditorDesigner;

begin
  DS:=GetDataset;
  if (DS=nil) or (DS.Connection=Nil) then exit;
  if DS.Connection.MetaDataResourceName='' then
    begin
    Dolog('%s: %s',[DS.Connection.Name,rsNoMetaDataResource]);
    ShowMessage(rsNoMetaDataResourceCannotCreateParams);
    exit;
    end;
  if (DS.ResourceName='') then
    begin
    DoLog('%s: %s',[DS.Name,rsNoResource]);
    ShowMessage(rsNoResourceCannotCreateParams);
    Exit;
    end;
  DS.Params.Clear;
  aCount:=IDERestUtils.UpdateParams(DS.Connection,IDERestUtils.GetFullResourceName(DS),DS.Params);

  if aCount=-1 then
    ShowMessage(rsServerRequestFailedCannotCreateParams)
  else if aCount=0 then
    ShowMessage(rsCreateParamsNoNew)
  else
    ShowMessage(Format(rsCreateParamsCount,[aCount]));
  if (FormEditingHook.GetCurrentDesigner is TComponentEditorDesigner) then
    begin
    CED:=(FormEditingHook.GetCurrentDesigner as TComponentEditorDesigner);
    CED.Modified;
    if assigned(CED.PropertyEditorHook) then
     CED.PropertyEditorHook.RefreshPropertyValues;
    end;
end;

procedure TIDEPas2JSRestCommandHandler.ShowData(Sender: TObject);

Var
  DS : TSQLDBRestDataset;
  frm  : TRestDataform;
  Buf : TBufDataset;

begin
  DS:=GetDataset;
  frm:=nil;
  Buf:=TLocalBufDataset.Create(nil);
  try
    Buf.Name:=DS.Name;
    IDERestUtils.GetDatasetData(DS,Buf);
    Frm:=TRestDataForm.Create(Nil);
    Frm.SetDatasetAndResource(Buf,DS.ResourceName);
    Frm.ShowModal;
  finally
    Buf.Free;
    frm.Free;
  end;
end;


Procedure InitCmdHandler;

begin
  If not Assigned(RestCmdHandler) then
    RestCmdHandler:=TIDEPas2JSRestCommandHandler.Create(Nil);
end;


initialization
  InitCmdhandler;
finalization
  FreeAndNil(RestCmdHandler);
end.

