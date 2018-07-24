unit CodyOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // CodeTools
  FileProcs,
  // LazUtils
  LazMethodList, LazConfigStorage,
  // IdeIntf
  BaseIDEIntf;

const
  CodyConfigVersion = 1;
var
  CodyMiscOptionID: integer = 1000;
type

  { TCodyMiscOptions }

  TCodyMiscOptions = class(TPersistent)
  private
    FChangeStep: integer;
    FPreferImplementationUsesSection: boolean;
    FUDLoadDelayInS: integer;
    FUDSaveIntervalInS: integer;
    fLastSavedChangeStep: integer;
    fApplyHandlers: TMethodList;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetUDLoadDelayInS(AValue: integer);
    procedure SetUDSaveIntervalInS(AValue: integer);
  public
    // unit / identifier dictionary
    property UDLoadDelayInS: integer read FUDLoadDelayInS write SetUDLoadDelayInS;
    property UDSaveIntervalInS: integer read FUDSaveIntervalInS write SetUDSaveIntervalInS;
    property PreferImplementationUsesSection: boolean
      read FPreferImplementationUsesSection write FPreferImplementationUsesSection;
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
    function Equals(Obj: TObject): boolean; override;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(Filename: string);
    procedure LoadFromFile(Filename: string);
    procedure Clear;
    procedure Apply;
    procedure AddHandlerApply(const OnApplyEvent: TNotifyEvent; AsLast: boolean = false);
    procedure RemoveHandlerApply(const OnApplyEvent: TNotifyEvent);
    property ChangeStep: integer read FChangeStep;
    procedure IncreaseChangeStep;
    property Modified: boolean read GetModified write SetModified;
  end;

var
  CodyOptions: TCodyMiscOptions = nil;

implementation

{ TCodyMiscOptions }

procedure TCodyMiscOptions.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStep
  else
    fLastSavedChangeStep:=FChangeStep;
end;

function TCodyMiscOptions.GetModified: boolean;
begin
  Result:=fLastSavedChangeStep<>FChangeStep;
end;

procedure TCodyMiscOptions.SetUDLoadDelayInS(AValue: integer);
begin
  if FUDLoadDelayInS=AValue then Exit;
  FUDLoadDelayInS:=AValue;
  IncreaseChangeStep;
end;

procedure TCodyMiscOptions.SetUDSaveIntervalInS(AValue: integer);
begin
  if FUDSaveIntervalInS=AValue then Exit;
  FUDSaveIntervalInS:=AValue;
  IncreaseChangeStep;
end;

constructor TCodyMiscOptions.Create;
begin
  inherited Create;
  FChangeStep:=CTInvalidChangeStamp;
  fApplyHandlers:=TMethodList.Create;
end;

destructor TCodyMiscOptions.Destroy;
begin
  FreeAndNil(fApplyHandlers);
  inherited Destroy;
end;

procedure TCodyMiscOptions.Assign(Source: TPersistent);
var
  aSource: TCodyMiscOptions;
begin
  if Source is TCodyMiscOptions then
  begin
    aSource:=TCodyMiscOptions(Source);
    UDSaveIntervalInS:=aSource.UDSaveIntervalInS;
    UDLoadDelayInS:=aSource.UDLoadDelayInS;
  end else
    inherited Assign(Source);
end;

function TCodyMiscOptions.Equals(Obj: TObject): boolean;
var
  Src: TCodyMiscOptions;
begin
  Result:=false;
  if not (Obj is TCodyMiscOptions) then exit;
  Src:=TCodyMiscOptions(Obj);
  if (UDLoadDelayInS<>Src.UDLoadDelayInS)
  or (UDSaveIntervalInS<>Src.UDSaveIntervalInS)
  then exit;
  Result:=true;
end;

procedure TCodyMiscOptions.SaveSafe;
begin
  try
    SaveToFile('codyoptions.xml');
  except
    on E: Exception do
      debugln(['TCodyMiscOptions.SaveSafe ',E.Message]);
  end;
  Modified:=false;
end;

procedure TCodyMiscOptions.LoadSafe;
begin
  try
    LoadFromFile('codyoptions.xml');
  except
    on E: Exception do
      debugln(['TCodyMiscOptions.LoadSafe ',E.Message]);
  end;
  Modified:=false;
end;

procedure TCodyMiscOptions.SaveToFile(Filename: string);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(Filename,false);
  try
    Cfg.SetDeleteValue('UnitDictionary/LoadDelay',UDLoadDelayInS,10);
    Cfg.SetDeleteValue('UnitDictionary/SaveInterval',UDSaveIntervalInS,600);
    Cfg.SetDeleteValue('Uses/PreferImplementationSection',PreferImplementationUsesSection,false);
  finally
    Cfg.Free;
  end;
end;

procedure TCodyMiscOptions.LoadFromFile(Filename: string);
var
  Cfg: TConfigStorage;
begin
  Clear;
  Cfg:=GetIDEConfigStorage(Filename,true);
  try
    UDLoadDelayInS:=Cfg.GetValue('UnitDictionary/LoadDelay',10);
    UDSaveIntervalInS:=Cfg.GetValue('UnitDictionary/SaveInterval',600);
    PreferImplementationUsesSection:=Cfg.GetValue('Uses/PreferImplementationSection',false);
    //debugln(['TCodyMiscOptions.LoadFromFile UDSaveIntervalInS=',UDSaveIntervalInS,' LoadDelay=',UDLoadDelayInS]);
  finally
    Cfg.Free;
  end;
end;

procedure TCodyMiscOptions.Clear;
begin
  UDLoadDelayInS:=10;
  UDSaveIntervalInS:=600;
end;

procedure TCodyMiscOptions.Apply;
begin
  fApplyHandlers.CallNotifyEvents(Self);
end;

procedure TCodyMiscOptions.AddHandlerApply(const OnApplyEvent: TNotifyEvent;
  AsLast: boolean);
begin
  fApplyHandlers.Add(TMethod(OnApplyEvent),AsLast);
end;

procedure TCodyMiscOptions.RemoveHandlerApply(const OnApplyEvent: TNotifyEvent
  );
begin
  fApplyHandlers.Remove(TMethod(OnApplyEvent));
end;

procedure TCodyMiscOptions.IncreaseChangeStep;
begin
  CTIncreaseChangeStamp(FChangeStep);
end;

finalization
  if CodyOptions<>nil then
    CodyOptions.SaveSafe;
  FreeAndNil(CodyOptions);

end.

