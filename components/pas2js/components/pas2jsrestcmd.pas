unit pas2jsrestcmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MenuIntf, propedits, DB, {IDEMsgIntf, }IDEExternToolIntf, stub.restdataset;


Type
  { TIDEPas2JSRestCommandHandler }
  TLogEvent = Procedure(Sender: TObject; Const Msg : String) of object;

  TIDEPas2JSRestCommandHandler = Class(TComponent)
  Private
    FCmdGetFieldDefs: TIDEMenuCommand;
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
    // Create fielddefs for dataset
    Procedure CreateFieldDefs(Sender : TObject); virtual;
    // Show the data that would be fetched by dataset
    Procedure ShowData(Sender : TObject); virtual;
    // Public access to menu itrms.
    Property CmdShowData : TIDEMenuCommand Read FCmdShowData Write FCmdShowData;
    Property CmdGetFieldDefs : TIDEMenuCommand Read FCmdGetFieldDefs Write FCmdGetFieldDefs;
    Property mnuCompRestSection : TIDEMenuSection Read FmnuCompRestSection Write FmnuCompRestSection;
    // Logging
    Property OnLog : TLogEvent read FOnLog write FOnLog;
    Property LogEnabled : Boolean Read FLogEnabled Write FLogEnabled;
  end;

Var
  RestCmdHandler : TIDEPas2JSRestCommandHandler;


implementation

uses dialogs, bufdataset, strpas2jscomponents, pas2jsrestutils, frmRestData;

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
  ASelection:=TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(ASelection);
    if (ASelection.Count=1) and (ASelection[0] is TSQLDBRestDataset) then
      Result:=ASelection[0] as TSQLDBRestDataset;
  finally
    ASelection.Free;
  end;
end;

procedure TIDEPas2JSRestCommandHandler.RegisterCommands;
begin
  // Form designer menu
  mnuCompRestSection:=RegisterIDESubMenu(DesignerMenuSectionCustomDynamic,'comPas2JSRest',rsPas2JSRest,Nil,Nil);
  mnuCompRestSection.AddHandlerOnShow(@CheckDataset);
//  RegisterIDEMenuCommand(mnuCompDDSection,'ddeditfields',SMenuDatadictApply,@IDEDDC.ApplyDD,Nil,Nil);
  CmdShowData:=RegisterIDEMenuCommand(mnuCompRestSection,'showData',rsMenuRestShowData,@ShowData,Nil,Nil);
  CmdGetFieldDefs:=RegisterIDEMenuCommand(mnuCompRestSection,'createFieldDefs',rsMenuRestCreateFieldDefs,@CreateFieldDefs,Nil,Nil);

end;

procedure TIDEPas2JSRestCommandHandler.CheckDataset(Sender: TObject);

Var
  DS : TSQLDBRestDataset;
  OK : Boolean;

begin
  DS:=GetDataset;
  OK:=(DS<>Nil) and (DS.Connection<>Nil) and (DS.ResourceName<>'');
  CmdShowData.Enabled:=OK ;
  CmdGetFieldDefs.Enabled:=OK and (DS.Connection.MetaDataResourceName<>'');
end;

procedure TIDEPas2JSRestCommandHandler.CreateFieldDefs(Sender: TObject);

Var
  DS : TSQLDBRestDataset;
  aCount: Integer;

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
  aCount:=IDERestUtils.UpdateFieldDefs(DS.Connection,IDERestUtils.GetFullResourceName(DS),DS.FieldDefs);
  if aCount=-1 then
    ShowMessage(rsServerRequestFailedCannotCreateFieldDefs)
  else if aCount=0 then
    ShowMessage(rsCreateFieldDefsNoNew)
  else
    ShowMessage(Format(rsCreateFieldDefsCount,[aCount]))

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


begin
  InitCmdhandler;
end.

