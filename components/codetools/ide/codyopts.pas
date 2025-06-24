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

const
  cDefLoadDelayInS    =    10;
  cDefSaveIntervalInS =   600;
  cDefMaxListItems    =    50;
  cMaxListItemsLow    =    10;
  cMaxListItemsHigh   = 10000;

var
  CodyMiscOptionID: integer = 1000;

type
  TCodyMiscOptions = class(TPersistent)
  private
    FChangeStep: integer;
    FPreferImplementationUsesSection: boolean;
    FUDLoadDelayInS: integer;
    FUDSaveIntervalInS: integer;
    FUDMaxListItems: integer;
    fLastSavedChangeStep: integer;
    fApplyHandlers: TMethodList;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetUDLoadDelayInS(AValue: integer);
    procedure SetUDSaveIntervalInS(AValue: integer);
    procedure SetUDMaxListItems(AValue: integer);
  public
    // unit / identifier dictionary
    property UDLoadDelayInS   : integer read FUDLoadDelayInS    write SetUDLoadDelayInS;
    property UDSaveIntervalInS: integer read FUDSaveIntervalInS write SetUDSaveIntervalInS;
    property UDMaxListItems   : integer read FUDMaxListItems    write SetUDMaxListItems;
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

procedure TCodyMiscOptions.SetUDMaxListItems(AValue: integer);
begin
  if FUDMaxListItems=AValue then Exit;
  FUDMaxListItems:=AValue;
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
begin
  if Source is TCodyMiscOptions then
  begin
    UDSaveIntervalInS := TCodyMiscOptions(Source).UDSaveIntervalInS;
    UDLoadDelayInS    := TCodyMiscOptions(Source).UDLoadDelayInS;
    UDMaxListItems    := TCodyMiscOptions(Source).UDMaxListItems;
  end else
    inherited Assign(Source);
end;

function TCodyMiscOptions.Equals(Obj: TObject): boolean;
begin
  Result :=
    (Obj is TCodyMiscOptions) and // "is" also checks for nil
    (UDLoadDelayInS    = TCodyMiscOptions(Obj).UDLoadDelayInS   ) and
    (UDSaveIntervalInS = TCodyMiscOptions(Obj).UDSaveIntervalInS) and
    (UDMaxListItems    = TCodyMiscOptions(Obj).UDMaxListItems);
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
    Cfg.SetDeleteValue('UnitDictionary/LoadDelay',   UDLoadDelayInS,   cDefLoadDelayInS);
    Cfg.SetDeleteValue('UnitDictionary/SaveInterval',UDSaveIntervalInS,cDefSaveIntervalInS);
    Cfg.SetDeleteValue('UnitDictionary/MaxListItems',UDMaxListItems,   cDefMaxListItems);
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
    UDLoadDelayInS   :=Cfg.GetValue('UnitDictionary/LoadDelay',   cDefLoadDelayInS);
    UDSaveIntervalInS:=Cfg.GetValue('UnitDictionary/SaveInterval',cDefSaveIntervalInS);
    UDMaxListItems   :=Cfg.GetValue('UnitDictionary/MaxListItems',cDefMaxListItems);
    PreferImplementationUsesSection:=Cfg.GetValue('Uses/PreferImplementationSection',false);
    //debugln(['TCodyMiscOptions.LoadFromFile UDSaveIntervalInS=',UDSaveIntervalInS,' LoadDelay=',UDLoadDelayInS]);
  finally
    Cfg.Free;
  end;
end;

procedure TCodyMiscOptions.Clear;
begin
  UDLoadDelayInS    := cDefLoadDelayInS;
  UDSaveIntervalInS := cDefSaveIntervalInS;
  UDMaxListItems    := cDefMaxListItems;
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
  if (CodyOptions<>nil) and CodyOptions.Modified then
    CodyOptions.SaveSafe;
  FreeAndNil(CodyOptions);

end.

