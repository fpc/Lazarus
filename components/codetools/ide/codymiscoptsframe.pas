unit CodyMiscOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, LResources, Forms, Controls,
  LazConfigStorage, StdCtrls, ComCtrls, IDEOptionsIntf, LazIDEIntf, BaseIDEIntf,
  FileProcs, CodyIdentifiersDlg;

const
  CodyConfigVersion = 1;
var
  CodyMiscOptionID: integer = 1000;
type

  { TCodyMiscOptions }

  TCodyMiscOptions = class(TPersistent)
  private
    FChangeStep: integer;
    FUDLoadDelayInS: integer;
    FUDSaveIntervalInS: integer;
    fLastSavedChangeStep: integer;
    function GetModified: boolean;
    procedure SetModified(AValue: boolean);
    procedure SetUDLoadDelayInS(AValue: integer);
    procedure SetUDSaveIntervalInS(AValue: integer);
  public
    // unit / identifier dictionary
    property UDLoadDelayInS: integer read FUDLoadDelayInS write SetUDLoadDelayInS;
    property UDSaveIntervalInS: integer read FUDSaveIntervalInS write SetUDSaveIntervalInS;
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    function Equals(Obj: TObject): boolean; override;
    procedure SaveSafe;
    procedure LoadSafe;
    procedure SaveToFile(Filename: string);
    procedure LoadFromFile(Filename: string);
    procedure Clear;
    procedure Apply;
    property ChangeStep: integer read FChangeStep;
    procedure IncreaseChangeStep;
    property Modified: boolean read GetModified write SetModified;
  end;

  { TCodyMiscOptionsFrame }

  TCodyMiscOptionsFrame = class(TAbstractIDEOptionsEditor)
    UDDividerBevel: TDividerBevel;
    UDLoadDelayLabel: TLabel;
    UDLoadDelayTrackBar: TTrackBar;
    UDSaveButton: TButton;
    UDSaveIntervalLabel: TLabel;
    UDSaveIntervalTrackBar: TTrackBar;
    procedure UDLoadDelayTrackBarChange(Sender: TObject);
    procedure UDSaveButtonClick(Sender: TObject);
    procedure UDSaveIntervalTrackBarChange(Sender: TObject);
  private
    FOldOptions: TCodyMiscOptions;
    FLoaded: boolean;
    fSaved: boolean;
    procedure GatherOptions(Options: TCodyMiscOptions);
    function UDLoadDelayToTrackbarPos(Seconds: integer): integer;
    function UDSaveIntervalToTrackbarPos(Seconds: integer): integer;
    function SecondsToStr(Seconds: integer): string;
    procedure UDUpdateLoadDelayInfo;
    procedure UDUpdateSaveIntervalInfo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    property OldOptions: TCodyMiscOptions read FOldOptions;
  end;

var
  CodyOptions: TCodyMiscOptions = nil;

const
  UDLoadDelayTrackbarValues: array[0..9] of integer = (
    // 0,1,2,3,4, 5, 6, 7,  8,  9
       0,1,2,3,5,10,30,60,120,300
  );
  UDSaveIntervalTrackbarValues: array[0..7] of integer = (
    // 0, 1, 2,  3,  4,  5,   6,   7
      10,20,60,120,300,600,1800,3600
  );

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
  CodyUnitDictionary.LoadAfterStartInS:=UDLoadDelayInS;
  CodyUnitDictionary.SaveIntervalInS:=UDSaveIntervalInS;
  //debugln(['TCodyMiscOptions.Apply Save=',CodyUnitDictionary.SaveIntervalInS,' Load=',CodyUnitDictionary.LoadAfterStartInS]);
end;

procedure TCodyMiscOptions.IncreaseChangeStep;
begin
  CTIncreaseChangeStamp(FChangeStep);
end;

{ TCodyMiscOptionsFrame }

procedure TCodyMiscOptionsFrame.UDSaveButtonClick(Sender: TObject);
begin
  CodyUnitDictionary.Save;
end;

procedure TCodyMiscOptionsFrame.UDSaveIntervalTrackBarChange(Sender: TObject);
begin
  if not FLoaded then exit;
  CodyUnitDictionary.SaveIntervalInS:=UDSaveIntervalTrackbarValues[UDSaveIntervalTrackBar.Position];
  UDUpdateSaveIntervalInfo;
end;

procedure TCodyMiscOptionsFrame.UDLoadDelayTrackBarChange(Sender: TObject);
begin
  if not FLoaded then exit;
  CodyUnitDictionary.LoadAfterStartInS:=UDLoadDelayTrackbarValues[UDLoadDelayTrackBar.Position];
  UDUpdateLoadDelayInfo;
end;

function TCodyMiscOptionsFrame.UDLoadDelayToTrackbarPos(Seconds: integer
  ): integer;
begin
  Result:=low(UDLoadDelayTrackbarValues);
  while (Seconds>UDLoadDelayTrackbarValues[Result])
  and (Result<High(UDLoadDelayTrackbarValues)) do
    inc(Result);
end;

procedure TCodyMiscOptionsFrame.GatherOptions(Options: TCodyMiscOptions);
begin
  Options.UDLoadDelayInS:=CodyUnitDictionary.LoadAfterStartInS;
  Options.UDSaveIntervalInS:=CodyUnitDictionary.SaveIntervalInS;
end;

function TCodyMiscOptionsFrame.UDSaveIntervalToTrackbarPos(Seconds: integer
  ): integer;
begin
  Result:=low(UDSaveIntervalTrackbarValues);
  while (Seconds>UDSaveIntervalTrackbarValues[Result])
  and (Result<High(UDSaveIntervalTrackbarValues)) do
    inc(Result);
end;

function TCodyMiscOptionsFrame.SecondsToStr(Seconds: integer): string;
begin
  if Seconds>=60 then
    Result:=IntToStr(Seconds div 60)+' minutes'
  else
    Result:=IntToStr(Seconds)+' seconds';
end;

procedure TCodyMiscOptionsFrame.UDUpdateLoadDelayInfo;
begin
  UDLoadDelayLabel.Caption:='Load dictionary after '+SecondsToStr(CodyUnitDictionary.LoadAfterStartInS);
end;

procedure TCodyMiscOptionsFrame.UDUpdateSaveIntervalInfo;
begin
  UDSaveIntervalLabel.Caption:='Save dictionary every '+SecondsToStr(CodyUnitDictionary.SaveIntervalInS);
end;

constructor TCodyMiscOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  UDDividerBevel.Caption:='Unit / Identifier Dictionary';
  FOldOptions:=TCodyMiscOptions.Create;
end;

destructor TCodyMiscOptionsFrame.Destroy;
begin
  FreeAndNil(FOldOptions);
  inherited Destroy;
end;

function TCodyMiscOptionsFrame.GetTitle: String;
begin
  Result:='Cody';
end;

procedure TCodyMiscOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions
  );
begin
  if not (AOptions is SupportedOptionsClass) then exit;
  if FLoaded then exit;
  FLoaded:=true;
  GatherOptions(OldOptions);
  UDLoadDelayTrackBar.Position:=UDLoadDelayToTrackbarPos(CodyUnitDictionary.LoadAfterStartInS);
  UDUpdateLoadDelayInfo;
  UDSaveIntervalTrackBar.Position:=UDSaveIntervalToTrackbarPos(CodyUnitDictionary.SaveIntervalInS);
  UDUpdateSaveIntervalInfo;
end;

procedure TCodyMiscOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog
  );
begin
  UDLoadDelayTrackBar.ShowHint:=true;
  UDLoadDelayTrackBar.Hint:='The dictionary is loaded on demand or after this time';
  UDLoadDelayTrackBar.Min:=Low(UDLoadDelayTrackbarValues);
  UDLoadDelayTrackBar.Max:=High(UDLoadDelayTrackbarValues);

  UDSaveIntervalLabel.ShowHint:=true;
  UDSaveIntervalLabel.Hint:='The dictionary is saved in intervals';
  UDSaveIntervalTrackBar.Min:=Low(UDSaveIntervalTrackbarValues);
  UDSaveIntervalTrackBar.Max:=High(UDSaveIntervalTrackbarValues);

  UDSaveButton.Caption:='Save dictionary now';
  UDSaveButton.ShowHint:=true;
  UDSaveButton.Hint:='Save to file '+CodyUnitDictionary.GetFilename;
end;

class function TCodyMiscOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEEnvironmentOptions;
end;

procedure TCodyMiscOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions
  );
begin
  if not (AOptions is SupportedOptionsClass) then exit;
  if fSaved then exit;
  fSaved:=true;
  GatherOptions(CodyOptions);
  CodyOptions.SaveSafe;
end;

procedure TCodyMiscOptionsFrame.RestoreSettings(
  AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is SupportedOptionsClass) then exit;
  CodyUnitDictionary.LoadAfterStartInS:=OldOptions.UDLoadDelayInS;
  CodyUnitDictionary.SaveIntervalInS:=OldOptions.UDSaveIntervalInS;
end;

{$R *.lfm}

finalization
  FreeAndNil(CodyOptions);

end.

