unit idedtstopas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEExternToolIntf, frmdtstopas;

Const
  DefaultOptions = [coInterfaceAsClass,coExpandUnionTypeArgs,coaddOptionsToheader,coDynamicTuples,coUseNativeTypeAliases];

Type
  TDTSToPascalMode = frmdtstopas.TDTSToPascalMode;
  TConversionOptions = frmdtstopas.TConversionOptions;

  { TCreateUnitFromDTS }

  TCreateUnitFromDTS = Class(TComponent)
  private
    FAliases: TStrings;
    FConversionOptions: TConversionOptions;
    FExtraUnits: String;
    FLocalFileName: String;
    FMode: TDTSToPascalMode;
    FModuleName: String;
    FSource: TStrings;
    FTargetUnitName: String;
    FUseWeb: Boolean;
    procedure ExecuteLocal;
    procedure ExecuteService;
    procedure SetAliases(AValue: TStrings);
  Public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Function ShowOptionsDialog : Boolean;
    Procedure Execute;
    Property Source : TStrings Read FSource;
    Property TargetUnitName : String Read FTargetUnitName Write FTargetUnitName;
    Property Aliases : TStrings Read FAliases Write SetAliases;
    Property UseWeb : Boolean Read FUseWeb Write FUseWeb;
    Property Mode : TDTSToPascalMode Read FMode Write FMode;
    Property ExtraUnits : String Read FExtraUnits Write FExtraUnits;
    Property ModuleName : String Read FModuleName Write FModuleName;
    Property LocalFileName : String Read FLocalFileName Write FLocalFileName;
    Property ConversionOptions : TConversionOptions Read FConversionOptions Write FConversionOptions;
  end;

implementation

uses Typinfo, PJSDsgnOptions, controls, strpas2jsdesign, idemsgintf, httpprotocol, fphttpclient;

Const
  SMessageDTS2Pas = 'DST2Pas';

{ TCreateUnitFromDTS }

procedure TCreateUnitFromDTS.SetAliases(AValue: TStrings);
begin
  if FAliases=AValue then Exit;
  FAliases.Assign(AValue);
end;

constructor TCreateUnitFromDTS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConversionOptions:=DefaultOptions;
  FUseWeb:=True;
  FAliases:=TStringList.Create;
  FSource:=TStringList.Create;
end;

destructor TCreateUnitFromDTS.Destroy;
begin
  FreeAndNil(FAliases);
  FreeAndNil(FSource);
  inherited Destroy;
end;

function TCreateUnitFromDTS.ShowOptionsDialog: Boolean;

Var
  Frm : TDTSToPascalOptionsForm;

begin
  Frm:=TDTSToPascalOptionsForm.Create(Self);
  try
    Frm.Aliases := Aliases;
    frm.Mode := Mode;
    frm.ExtraUnits := ExtraUnits;
    frm.UseWeb := UseWeb;
    frm.Module := ModuleName;
    Frm.LocalFile := LocalFileName;
    Frm.Options:=ConversionOptions;
    Result:=Frm.ShowModal=mrOK;
    if Result then
      begin
      Aliases:=Frm.Aliases;
      Mode:=frm.Mode;
      ExtraUnits:=frm.ExtraUnits;
      UseWeb:=frm.UseWeb;
      ModuleName:=frm.Module;
      LocalFileName:=Frm.LocalFile;
      ConversionOptions:=Frm.Options;
      end;
  finally
    Frm.Free;
  end;
end;


procedure TCreateUnitFromDTS.ExecuteLocal;

  Function MaybeQuote(s : string) : string;

  begin
    if Pos(' ',S)=0 then
      Result:=S
    else
      Result:=AnsiQuotedStr(S,'"');
  end;


Var
  ToolOpts : TIDEExternalToolOptions;
  AliasFN,OutFN,Opts : string;

begin
  AliasFN:='';
  OutFN:='';
  ToolOpts:=TIDEExternalToolOptions.Create;
  try
    ToolOpts.Parsers.Add(SubToolDefault);
    OutFN:=GetTempFileName;
    ToolOpts.Executable:=PJSOptions.DTS2Pas;
    ToolOpts.ResolveMacros:=True;
    Opts:='-i '+MaybeQuote(LocalFileName)+' -o '+MaybeQuote(outFN)+' -u '+TargetUnitName;
    if UseWeb then
      opts:=Opts+' -w';
    if Aliases.Count>0 then
      begin
      AliasFN:=GetTempFileName;
      Aliases.SaveToFile(AliasFN);
      Opts:=Opts+' -a @'+aliasFN;
      end;
    If ExtraUnits<>'' then
      Opts:=Opts+'-x '+StringReplace(ExtraUnits,' ','',[rfReplaceAll]);
    ToolOpts.CmdLineParams:=Opts;
    if not (RunExternalTool(ToolOpts) and FileExists(OutFN)) then
      IDEMessagesWindow.AddCustomMessage(TMessageLineUrgency.mluError,rsDTSDidNotProduceOutput,'',0,0,SMessageDTS2Pas)
    else
      FSource.LoadFromFile(OutFN)
  finally
    if AliasFN<>'' then
      DeleteFile(aliasFN);
    if OutFN<>'' then
      DeleteFile(OutFN);
    ToolOpts.Free;
  end;
end;

procedure TCreateUnitFromDTS.ExecuteService;

Var
  URL : String;
  CO : TConversionOption;

begin
  URL:=IncludeHTTPPathDelimiter(PJSOptions.DTS2PasServiceURL)+'/convert';
  URL:=URL+'?file='+HTTPEncode(ModuleName);
  URL:=URL+'&unit='+HTTPEncode(TargetUnitName);
  URL:=URL+'&prependLog=1';
  for CO in TConversionOption do
    if CO in ConversionOptions then
      URL:=URL+'&'+GetEnumName(TypeInfo(TConversionOption),Ord(CO))+'=1';
  if aliases.Count>0 then
    URL:=URL+'&Aliases='+HTTPEncode(Aliases.CommaText);
  if Not UseWeb then
    URL:=URL+'&SkipWeb=1';
  If ExtraUnits<>'' then
    URL:=URL+'&ExtraUnits='+HTTPEncode(ExtraUnits);
  TFPHTTPClient.SimpleGet(URL,FSource);
end;

procedure TCreateUnitFromDTS.Execute;
begin
  if TargetUnitName='' then
    TargetUnitName:='libtsmodule';
  If Mode=dpmLocal then
    ExecuteLocal
  else
    ExecuteService;
end;

end.

