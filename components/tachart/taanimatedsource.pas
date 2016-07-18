{

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Alexander Klenin

}

unit TAAnimatedSource;

{$H+}

interface

uses
  Classes, CustomTimer,
  TAChartUtils, TACustomSource;

type
  TCustomAnimatedChartSource = class;

  TAnimatedChartSourceItemEvent = procedure (
    ASender: TCustomAnimatedChartSource;
    AIndex: Integer; var AItem: TChartDataItem) of object;
  TAnimatedChartSourceEvent = procedure (
    ASender: TCustomAnimatedChartSource) of object;

  TCustomAnimatedChartSource = class(TCustomChartSource)
  strict private
    FAnimationInterval: Cardinal;
    FAnimationTime: Cardinal;
    FCurrentStep: Cardinal;
    FItem: TChartDataItem;
    FListener: TListener;
    FOnGetItem: TAnimatedChartSourceItemEvent;
    FOnStop: TAnimatedChartSourceEvent;
    FOrigin: TCustomChartSource;
    FProjectedSteps: Cardinal;
    FSkippedFramesCount: Cardinal;
    FStartTime: Cardinal;
    FTimer: TCustomTimer;
    procedure Changed(ASender: TObject);
    procedure OnTimer(ASender: TObject);
    procedure SetOrigin(AValue: TCustomChartSource);
  protected
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
    procedure SetXCount(AValue: Cardinal); override;
    procedure SetYCount(AValue: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Extent: TDoubleRect; override;
    function ExtentCumulative: TDoubleRect; override;
    function ExtentList: TDoubleRect; override;

    function IsAnimating: Boolean; inline;
    function Progress: Double; inline;
    procedure Start;
    procedure Stop(ACallEvent: Boolean = false);

    property CurrentStep: Cardinal read FCurrentStep;
    property ProjectedSteps: Cardinal read FProjectedSteps;
    property SkippedFramesCount: Cardinal read FSkippedFramesCount;
  published
    property AnimationInterval: Cardinal
      read FAnimationInterval write FAnimationInterval default 0;
    property AnimationTime: Cardinal
      read FAnimationTime write FAnimationTime default 0;
    property Origin: TCustomChartSource read FOrigin write SetOrigin;
  published
    property OnGetItem: TAnimatedChartSourceItemEvent
      read FOnGetItem write FOnGetItem;
    property OnStop: TAnimatedChartSourceEvent read FOnStop write FOnStop;
  end;

implementation

uses
  LCLIntf, Math, SysUtils;

{ TCustomAnimatedChartSource }

procedure TCustomAnimatedChartSource.Changed(ASender: TObject);
begin
  Unused(ASender);
  Notify;
end;

constructor TCustomAnimatedChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListener := TListener.Create(@FOrigin, @Changed);
  FTimer := TCustomTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.OnTimer := @OnTimer;
end;

destructor TCustomAnimatedChartSource.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FListener);
  inherited;
end;

function TCustomAnimatedChartSource.Extent: TDoubleRect;
begin
  if Origin = nil then
    Result := EmptyExtent
  else
    Result := Origin.Extent;
end;

function TCustomAnimatedChartSource.ExtentCumulative: TDoubleRect;
begin
  if Origin = nil then
    Result := EmptyExtent
  else
    Result := Origin.ExtentCumulative;
end;

function TCustomAnimatedChartSource.ExtentList: TDoubleRect;
begin
  if Origin = nil then
    Result := EmptyExtent
  else
    Result := Origin.ExtentList;
end;

function TCustomAnimatedChartSource.GetCount: Integer;
begin
  if Origin = nil then
    Result := 0
  else
    Result := Origin.Count;
end;

function TCustomAnimatedChartSource.GetItem(AIndex: Integer): PChartDataItem;
begin
  if Origin = nil then exit(nil);
  if not IsAnimating then exit(Origin.Item[AIndex]);
  FItem := Origin.Item[AIndex]^;
  Result := @FItem;
  if Assigned(OnGetItem) then
    OnGetItem(Self, AIndex, FItem);
end;

function TCustomAnimatedChartSource.IsAnimating: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TCustomAnimatedChartSource.OnTimer(ASender: TObject);
var
  d, s: Cardinal;
begin
  Unused(ASender);
  d := GetTickCount64 - FStartTime;
  if d >= AnimationTime then
    Stop(true);
  s := Round(d * ProjectedSteps / AnimationTime);
  if FCurrentStep + 1 <> s then
    FSkippedFramesCount += 1;
  FCurrentStep := s;
  Notify;
end;

function TCustomAnimatedChartSource.Progress: Double;
begin
  if ProjectedSteps = 0 then
    Result := 0
  else
    Result := (FCurrentStep - 1) / ProjectedSteps;
end;

procedure TCustomAnimatedChartSource.SetOrigin(AValue: TCustomChartSource);
begin
  if AValue = Self then
      AValue := nil;
  if FOrigin = AValue then exit;
  if FOrigin <> nil then
    FOrigin.Broadcaster.Unsubscribe(FListener);
  FOrigin := AValue;
  if FOrigin <> nil then
    FOrigin.Broadcaster.Subscribe(FListener);
end;

procedure TCustomAnimatedChartSource.SetXCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EXCountError.Create('Cannot set XCount');
end;

procedure TCustomAnimatedChartSource.SetYCount(AValue: Cardinal);
begin
  Unused(AValue);
  raise EYCountError.Create('Cannot set YCount');
end;

procedure TCustomAnimatedChartSource.Start;
begin
  Stop;
  FSkippedFramesCount := 0;
  if (AnimationInterval = 0) or (AnimationTime <= AnimationInterval) then exit;
  FProjectedSteps := Round(AnimationTime / AnimationInterval);
  FStartTime := GetTickCount64;
  FTimer.Interval := AnimationInterval;
  FTimer.Enabled := true;
end;

procedure TCustomAnimatedChartSource.Stop(ACallEvent: Boolean);
begin
  FTimer.Enabled := false;
  FCurrentStep := 0;
  if ACallEvent and Assigned(OnStop) then
    OnStop(Self);
end;

initialization
  if ZeroValue = 0 then; // Workaround for an incorrect "unused unit" hint.

end.

