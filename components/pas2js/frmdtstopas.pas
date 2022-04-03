unit frmdtstopas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ButtonPanel, ValEdit, ExtCtrls, ComCtrls;

Const
  MinCompleteLength = 2;
  MaxCompletions = 500;

Var
  ModuleList : TStringList;
  OnModuleListChanged : TNotifyEvent;

type

  TConversionOption = (coRaw,coGenericArrays,coUseNativeTypeAliases,coLocalArgumentTypes, coUntypedTuples, coDynamicTuples,
                       coExternalConst,coExpandUnionTypeArgs,coaddOptionsToheader,coInterfaceAsClass,coSkipImportStatements);
  TConversionOptions = Set of TConversionOption;


  TDTSToPascalMode = (dpmLocal,dpmService);

  { TDTSToPascalOptionsForm }

  TDTSToPascalOptionsForm = class(TForm)
    BPOptions: TButtonPanel;
    cbModule: TComboBox;
    CBUseWeb: TCheckBox;
    CGOptions: TCheckGroup;
    edtUnits: TEdit;
    FEDts: TFileNameEdit;
    Label1: TLabel;
    lblDeclarationFile: TLabel;
    lblExtraUnits: TLabel;
    lblModulename: TLabel;
    PCOptions: TPageControl;
    RBLocal: TRadioButton;
    RBService: TRadioButton;
    tmrComplete: TTimer;
    TSInput: TTabSheet;
    TSOptions: TTabSheet;
    VLEAliases: TValueListEditor;
    procedure cbModuleEnter(Sender: TObject);
    procedure cbModuleKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure FEDtsEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrCompleteTimer(Sender: TObject);
  private
    FLastText : String;
    procedure CheckModuleList;
    procedure DoGetList(Sender: TObject);
    function GetAliases: TStrings;
    function GetExtraUnits: String;
    function GetLocalFile: String;
    function GetMode: TDTSToPascalMode;
    function GetModule: String;
    function GetOptions: TConversionOptions;
    function GetUseWeb: Boolean;
    procedure SetAliases(AValue: TStrings);
    procedure SetExtraUnits(AValue: String);
    procedure SetLocalFile(AValue: String);
    procedure SetMode(AValue: TDTSToPascalMode);
    procedure SetModule(AValue: String);
    procedure SetOptions(AValue: TConversionOptions);
    procedure SetUseWeb(AValue: Boolean);
  public
    Property Aliases : TStrings Read GetAliases Write SetAliases;
    Property UseWeb : Boolean Read GetUseWeb Write SetUseWeb;
    Property Mode : TDTSToPascalMode Read GetMode Write SetMode;
    Property Module : String Read GetModule Write SetModule;
    Property LocalFile : String Read GetLocalFile Write SetLocalFile;
    Property ExtraUnits : String Read GetExtraUnits Write SetExtraUnits;
    Property Options : TConversionOptions Read GetOptions Write SetOptions;
  end;

var
  DTSToPascalOptionsForm: TDTSToPascalOptionsForm;

implementation

uses fpJSON, httpprotocol, FPHTTPClient, PJSDsgnOptions, lazloggerbase, IDEExternToolIntf, IDEMsgIntf, strpas2jsdesign;

Const
  SMessageDTS2Pas = 'DTS2PasService';

{$R *.lfm}

Type

  { TGetModulesThread }

  TGetModulesThread = Class(TThread)
  Private
    FURL : String;
    FError : String;
    FResponse : TStrings;
    Procedure DoLogError;
    Procedure DoSetStringList;
  Public
    Destructor Destroy; override;
    Procedure Execute;  override;
  end;

{ TGetModulesThread }

procedure TGetModulesThread.DoLogError;
begin
  IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluError,Format(rsHTTPRequestFailed,[FURL,FError]),'',0,0,SMessageDTS2Pas)
end;

procedure TGetModulesThread.DoSetStringList;

begin
  ModuleList.Clear;
  ModuleList.BeginUpdate;
  try
    ModuleList.Capacity:=FResponse.Count;
    ModuleList.AddStrings(FResponse);
    ModuleList.Sort;
  finally
    ModuleList.EndUpdate;
  end;
  If Assigned(OnModuleListChanged) then
    OnModuleListChanged(ModuleList);
end;

destructor TGetModulesThread.Destroy;
begin
  FreeAndNil(FResponse);
  inherited Destroy;
end;

procedure TGetModulesThread.Execute;

Var
  URL: String;
begin
  FreeOnTerminate:=true;
  try
    URL:=IncludeHTTPPathDelimiter(PJSOptions.DTS2PasServiceURL)+'list?raw=1';
    FResponse:=TStringList.Create;
    FResponse.Capacity:=50*1000;
    TFPHTTPClient.SimpleGet(URL,FResponse);
    Synchronize(@DoSetStringList);
  Except
    On E : Exception do
      begin
      FError:=E.Message;
      Synchronize(@DoLogError);
      end;
  end;
end;

{ TDTSToPascalOptionsForm }

procedure TDTSToPascalOptionsForm.FEDtsEnter(Sender: TObject);
begin
  RBLocal.Checked:=True;
end;

procedure TDTSToPascalOptionsForm.FormCreate(Sender: TObject);
begin
  if (ModuleList=Nil) then
    begin
    if (PJSOptions.DTS2PasServiceURL<>'') then
      begin
      ModuleList:=TStringList.Create;
      OnModuleListChanged:=@DoGetList;
      TGetModulesThread.Create(False);
      end;
    end
  else
    begin
    DoGetList(ModuleList);
    end;
end;

procedure TDTSToPascalOptionsForm.FormDestroy(Sender: TObject);
begin
  OnModuleListChanged:=Nil;
end;

procedure TDTSToPascalOptionsForm.CheckModuleList;

Var
  S : String;
  I,aCount : Integer;
begin
  if (Length(CBModule.Text)<MinCompleteLength) or Not Assigned(ModuleList) then
    Exit;
  S:=UpperCase(CBModule.Text);
  if S=FLastText then
    exit;
  With cbModule.Items do
    try
      BeginUpdate;
      Clear;
      aCount:=0;
      I:=0;
      While (aCount<MaxCompletions) and (I<ModuleList.Count) do
        begin
        if Pos(S,UpperCase(ModuleList[I]))>0 then
          begin
          Add(ModuleList[I]);
          Inc(aCount);
          end;
        Inc(I);
        end;
      if (aCount>=MaxCompletions) and (I<ModuleList.Count-1) then
        Add('...');
    finally
      EndUpdate;
    end;
end;

procedure TDTSToPascalOptionsForm.tmrCompleteTimer(Sender: TObject);
begin
  tmrComplete.Enabled:=False;
  CheckModuleList;
end;

procedure TDTSToPascalOptionsForm.DoGetList(Sender: TObject);
begin
  CheckModuleList;
end;

procedure TDTSToPascalOptionsForm.cbModuleEnter(Sender: TObject);
begin
  RBService.Checked:=True;
end;

procedure TDTSToPascalOptionsForm.cbModuleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  With tmrComplete do
     begin
     Enabled:=False;
     Enabled:=True;
     end;
end;

function TDTSToPascalOptionsForm.GetAliases: TStrings;
begin
  Result:=VLEAliases.Strings;
end;

function TDTSToPascalOptionsForm.GetExtraUnits: String;
begin
  Result:=edtUnits.Text;
end;

function TDTSToPascalOptionsForm.GetLocalFile: String;
begin
  Result:=FEDts.FileName;
end;

function TDTSToPascalOptionsForm.GetMode: TDTSToPascalMode;
begin
  If RBLocal.Checked then
    result:=dpmLocal
  else
    Result:=dpmService;
end;

function TDTSToPascalOptionsForm.GetModule: String;
begin
  Result:=CBModule.Text;
end;

function TDTSToPascalOptionsForm.GetOptions: TConversionOptions;

Var
  CO : TConversionOption;

begin
  Result:=[];
  For CO in TConversionOption do
    if CGOptions.Checked[Ord(CO)] then
       Include(Result,CO);
end;

function TDTSToPascalOptionsForm.GetUseWeb: Boolean;
begin
  Result:=CBUseWeb.Checked;
end;

procedure TDTSToPascalOptionsForm.SetAliases(AValue: TStrings);
begin
  VLEAliases.Strings.Assign(aValue);
end;

procedure TDTSToPascalOptionsForm.SetExtraUnits(AValue: String);
begin
  EDTUnits.Text:=aValue;
end;

procedure TDTSToPascalOptionsForm.SetLocalFile(AValue: String);
begin
  FEDTS.FileName:=aValue;
end;

procedure TDTSToPascalOptionsForm.SetMode(AValue: TDTSToPascalMode);
begin
  if aValue=dpmLocal then
    rbLocal.Checked:=True
  else
    RBService.Checked:=True;
end;

procedure TDTSToPascalOptionsForm.SetModule(AValue: String);
begin
  CBModule.Text:=aValue;
end;

procedure TDTSToPascalOptionsForm.SetOptions(AValue: TConversionOptions);

Var
  CO : TConversionOption;

begin
  For CO in TConversionOption do
    CGOptions.Checked[Ord(CO)]:=CO in aValue;
end;

procedure TDTSToPascalOptionsForm.SetUseWeb(AValue: Boolean);
begin
  CBUseWeb.Checked:=aValue;
end;

Finalization
  OnModuleListChanged:=Nil;
  FreeAndNil(ModuleList);
end.

