unit CodyMiscOptsFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil,
  // LCL
  LResources, Forms, Controls, StdCtrls, ComCtrls,
  // LazControls
  DividerBevel,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, LazIDEIntf,
  // CodeTools
  FileProcs, CodyOpts, CodyIdentifiersDlg, CodyStrConsts;

type
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

{ TCodyMiscOptionsFrame }

procedure TCodyMiscOptionsFrame.UDSaveButtonClick(Sender: TObject);
begin
  // make sure the old db is loaded
  CodyUnitDictionary.Load;
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
    Result:=Format(crsMinutes, [IntToStr(Seconds div 60)])
  else
    Result:=Format(crsSeconds, [IntToStr(Seconds)]);
end;

procedure TCodyMiscOptionsFrame.UDUpdateLoadDelayInfo;
begin
  UDLoadDelayLabel.Caption:=Format(crsLoadDictionaryAfter, [SecondsToStr(
    CodyUnitDictionary.LoadAfterStartInS)]);
end;

procedure TCodyMiscOptionsFrame.UDUpdateSaveIntervalInfo;
begin
  UDSaveIntervalLabel.Caption:=Format(crsSaveDictionaryEvery, [SecondsToStr(
    CodyUnitDictionary.SaveIntervalInS)]);
end;

constructor TCodyMiscOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  UDDividerBevel.Caption:=crsUnitIdentifierDictionary;
  FOldOptions:=TCodyMiscOptions.Create;
end;

destructor TCodyMiscOptionsFrame.Destroy;
begin
  FreeAndNil(FOldOptions);
  inherited Destroy;
end;

function TCodyMiscOptionsFrame.GetTitle: String;
begin
  Result:=crsIDEIntegration;
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
  UDLoadDelayTrackBar.Hint:=crsTheDictionaryIsLoadedOnDemandOrAfterThisTime;
  UDLoadDelayTrackBar.Min:=Low(UDLoadDelayTrackbarValues);
  UDLoadDelayTrackBar.Max:=High(UDLoadDelayTrackbarValues);

  UDSaveIntervalTrackBar.ShowHint:=true;
  UDSaveIntervalTrackBar.Hint:=crsTheDictionaryIsSavedInIntervals;
  UDSaveIntervalTrackBar.Min:=Low(UDSaveIntervalTrackbarValues);
  UDSaveIntervalTrackBar.Max:=High(UDSaveIntervalTrackbarValues);

  UDSaveButton.Caption:=crsSaveDictionaryNow;
  UDSaveButton.ShowHint:=true;
  UDSaveButton.Hint:=Format(crsSaveToFile, [CodyUnitDictionary.GetFilename]);
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

end.

