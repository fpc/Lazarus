unit sparta_BasicFakeCustom;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  LCLType, LCLIntf, Controls, Forms,
  sparta_InterfacesMDI, sparta_FormBackgroundForMDI;

type

  { TFormImpl }

  TFormImpl = class(TComponent, IDesignedRealFormHelper, IDesignedForm)
  private
    FDesignedRealForm: IDesignedRealForm;
    FHackLeft: Integer;
    FHackTop: Integer;
    FHackWidth: Integer;
    FHackHeight: Integer;
    FOnChangeHackedBounds: TNotifyEvent;
  protected
    FForm: TCustomForm;
    FUpdate: boolean;
    procedure SetOnChangeHackedBounds(const AValue: TNotifyEvent);
    function GetOnChangeHackedBounds: TNotifyEvent;
    function PositionDelta: TPoint;

    function GetRealBounds(AIndex: Integer): Integer; virtual;
    procedure SetRealBounds(AIndex: Integer; AValue: Integer); virtual;
    function GetPublishedBounds(AIndex: Integer): Integer; virtual;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); virtual;

    procedure SetHorzScrollPosition(AValue: Integer); virtual;
    procedure SetVertScrollPosition(AValue: Integer); virtual;

    // own custom form scrool system
    function GetHorzScrollPosition: Integer; virtual;
    function GetVertScrollPosition: Integer; virtual;

    procedure SetRealBorderStyle(AVal: TFormBorderStyle); virtual;
    procedure SetRealBorderIcons(AVal: TBorderIcons); virtual;
    procedure SetRealFormStyle(AVal: TFormStyle); virtual;
    procedure SetRealPopupMode(AVal: TPopupMode); virtual;
    procedure SetRealPopupParent(AVal: TCustomForm); virtual;

    function GetRealBorderStyle: TFormBorderStyle; virtual;
    function GetRealBorderIcons: TBorderIcons; virtual;
    function GetRealFormStyle: TFormStyle; virtual;
    function GetRealPopupMode: TPopupMode; virtual;
    function GetRealPopupParent: TCustomForm; virtual;

    function GetForm: TCustomForm; virtual;
    function GetUpdate: Boolean; virtual;

    procedure DoChangeHackedBounds; virtual;

    function GetLogicalClientRect(ALogicalClientRect: TRect): TRect; virtual;
  public
    property RealLeft: Integer index 0 read GetRealBounds write SetRealBounds;
    property RealTop: Integer index 1 read GetRealBounds write SetRealBounds;
    property RealWidth: Integer index 2 read GetRealBounds write SetRealBounds;
    property RealHeight: Integer index 3 read GetRealBounds write SetRealBounds;
    property RealBorderStyle: TFormBorderStyle read GetRealBorderStyle write SetRealBorderStyle;
    property RealBorderIcons: TBorderIcons read GetRealBorderIcons write SetRealBorderIcons;
    property RealFormStyle: TFormStyle read GetRealFormStyle write SetRealFormStyle;

    constructor Create(AOwner: TComponent; AForm: TCustomForm); virtual; reintroduce;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate({%H-}AModified: Boolean = False); virtual;

    procedure ShowWindow; virtual;
    procedure HideWindow; virtual;

    property Update: Boolean read GetUpdate;
  public
    property Left: Integer index 0 read GetPublishedBounds write SetPublishedBounds;
    property Top: Integer index 1 read GetPublishedBounds write SetPublishedBounds;
    property Width: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property Height: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
  public
    function QueryInterface(constref IID: TGUID; out Obj): HResult; override;
  end;

  { TFormContainer }

  TFormContainer = class(TCustomForm, IDesignedRealForm, IDesignedForm, IDesignedFormBackground)
  private
    FDesignedForm: TFormImpl;
    function GetDesignedForm: TFormImpl;
  protected
    property DesignedForm: TFormImpl read GetDesignedForm implements IDesignedForm;
    function GetLogicalClientRect: TRect; override;
  protected
    function GetRealBounds(AIndex: Integer): Integer; virtual;
    procedure SetRealBounds(AIndex: Integer; AValue: Integer); virtual;
    function GetPublishedBounds(AIndex: Integer): Integer; virtual;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); virtual;

    procedure SetRealBorderStyle(AVal: TFormBorderStyle); virtual;
    procedure SetRealBorderIcons(AVal: TBorderIcons); virtual;
    procedure SetRealFormStyle(AVal: TFormStyle); virtual;
    procedure SetRealPopupMode(AVal: TPopupMode); virtual;
    procedure SetRealPopupParent(AVal: TCustomForm); virtual;

    function GetRealBorderStyle: TFormBorderStyle; virtual;
    function GetRealBorderIcons: TBorderIcons; virtual;
    function GetRealFormStyle: TFormStyle; virtual;
    function GetRealPopupMode: TPopupMode; virtual;
    function GetRealPopupParent: TCustomForm; virtual;
  protected
    FHandledForm: TCustomForm;
    FBackground: IDesignedFormBackground;

    procedure SetHandledForm(AForm: TCustomForm);
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;

    property HandledForm: TCustomForm read FHandledForm write SetHandledForm;
    property Background: IDesignedFormBackground read FBackground implements IDesignedFormBackground;
  published
    property Left: Integer index 0 read GetPublishedBounds write SetPublishedBounds;
    property Top: Integer index 1 read GetPublishedBounds write SetPublishedBounds;
    property Width: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property Height: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
    property ClientWidth: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property ClientHeight: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
  end;

implementation

type
  TFormAccess = class(TForm);

{ TDesignedFormImpl }

function TFormImpl.GetPublishedBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FForm.Left;
    1: Result := FForm.Top;
    2: Result := FForm.Width;
    3: Result := FForm.Height;
  end;
  //case AIndex of
  //  0: Result := FHackLeft;
  //  1: Result := FHackTop;
  //  2: Result := FHackWidth;
  //  3: Result := FHackHeight;
  //end;
end;

procedure TFormImpl.SetPublishedBounds(AIndex: Integer; AValue: Integer);
const
  cMinWidth = 135;
  cMaxWidth = 5*1024; // huge Mac monitors have 5K pixels width
begin
  if AIndex = 2 then
    if AValue < cMinWidth then
      AValue := cMinWidth;

  if AIndex in [2, 3] then
    if AValue > cMaxWidth then
      AValue := cMaxWidth;

  case AIndex of
    0: FHackLeft := AValue;
    1: FHackTop := AValue;
    2: FHackWidth := AValue;
    3: FHackHeight := AValue;
  end;

  DoChangeHackedBounds;
end;

{-----------------------------------------------
  Real values inherited for design form
{----------------------------------------------}

function TFormImpl.GetRealBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FForm.Left;
    1: Result := FForm.Top;
    2: Result := FForm.Width;
    3: Result := FForm.Height;
  end;

  //FForm.;
  //Result := 0;// FDesignedRealForm.GetRealBounds(AIndex);
end;

procedure TFormImpl.SetRealBounds(AIndex: Integer; AValue: Integer);

  procedure AdjustSize;
  var
    LFormRect: TRect;
    LRealValue, LValue: Integer;
  begin
    LFormRect := Rect(0, 0, 0, 0);;
    LCLIntf.GetClientRect(GetForm.Handle, LFormRect);
    LRealValue := GetRealBounds(AIndex);
    {$IF FPC_FULLVERSION < 30101}
    case AIndex of
      0: LValue := LFormRect.Left;
      1: LValue := LFormRect.Top;
      2: LValue := LFormRect.Right;
      3: LValue := LFormRect.Bottom;
    end;
    {$ELSE}
    LValue := LFormRect.Vector[AIndex];
    {$ENDIF}

    if LValue <> LRealValue then
      FDesignedRealForm.SetRealBounds(AIndex, AValue - (LRealValue - LValue));
  end;

begin
  {FDesignedRealForm.SetRealBounds(AIndex, AValue);

  if AIndex = 2 then
    AdjustSize;}
end;

procedure TFormImpl.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  //FDesignedRealForm.SetRealBorderStyle(AVal);
end;

procedure TFormImpl.SetRealBorderIcons(AVal: TBorderIcons);
begin
  //FDesignedRealForm.SetRealBorderIcons(AVal);
end;

procedure TFormImpl.SetRealFormStyle(AVal: TFormStyle);
begin
  //FDesignedRealForm.SetRealFormStyle(AVal);
end;

procedure TFormImpl.SetRealPopupMode(AVal: TPopupMode);
begin
  //FDesignedRealForm.SetRealPopupMode(AVal);
end;

procedure TFormImpl.SetRealPopupParent(AVal: TCustomForm);
begin
  //FDesignedRealForm.SetRealPopupParent(AVal);
end;

function TFormImpl.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := bsNone;//FDesignedRealForm.GetRealBorderStyle;
end;

function TFormImpl.GetRealBorderIcons: TBorderIcons;
begin
  Result := [];//FDesignedRealForm.GetRealBorderIcons;
end;

function TFormImpl.GetRealFormStyle: TFormStyle;
begin
  Result := fsNormal;//FDesignedRealForm.GetRealFormStyle;
end;

function TFormImpl.GetRealPopupMode: TPopupMode;
begin
  Result := pmNone//FDesignedRealForm.GetRealPopupMode;
end;

function TFormImpl.GetRealPopupParent: TCustomForm;
begin
  Result := nil//FDesignedRealForm.GetRealPopupParent;
end;

//////

function TFormImpl.GetForm: TCustomForm;
begin
  Result := FForm;
end;

function TFormImpl.GetUpdate: Boolean;
begin
  Result := FUpdate;
end;

function TFormImpl.GetOnChangeHackedBounds: TNotifyEvent;
begin
  Result := FOnChangeHackedBounds;
end;

function TFormImpl.PositionDelta: TPoint;

  procedure FormBorderDelta;
  begin
    Result.X := GetSystemMetrics(SM_CXSIZEFRAME);
    Result.Y := GetSystemMetrics(SM_CYSIZEFRAME) + GetSystemMetrics(SM_CYCAPTION);
  end;

begin
  Result := Point(0, 0);
  {$IFDEF WINDOWS}
  FormBorderDelta;
  {$ENDIF}
end;

procedure TFormImpl.SetOnChangeHackedBounds(const AValue: TNotifyEvent);
begin
  FOnChangeHackedBounds := AValue;
end;

/////// positions

procedure TFormImpl.SetHorzScrollPosition(AValue: Integer);
begin
  RealLeft := -PositionDelta.x - AValue;
  // ! must. resize problem for controls with Align = Top, Right etc.
  RealWidth := Width;
  RealHeight := Height;
end;

procedure TFormImpl.SetVertScrollPosition(AValue: Integer);
begin
  RealTop := -PositionDelta.y - AValue;
  // ! must. resize problem for controls with Align = Top, Right etc.
  RealWidth := Width;
  RealHeight := Height;
end;

function TFormImpl.GetHorzScrollPosition: Integer;
begin
  Result := -(RealLeft {+ PositionDelta.x});
end;

function TFormImpl.GetVertScrollPosition: Integer;
begin
  Result := -(RealTop {+ PositionDelta.y});
end;

procedure TFormImpl.BeginUpdate;
begin
  FUpdate := True;
end;

procedure TFormImpl.EndUpdate(AModified: Boolean);
begin
  FUpdate := False;
end;

procedure TFormImpl.ShowWindow;
begin
  if FForm.Parent = nil then
    LCLIntf.ShowWindow(FForm.Handle, SW_SHOW);
end;

procedure TFormImpl.HideWindow;
begin
  if FForm.Parent = nil then
    LCLIntf.ShowWindow(FForm.Handle, SW_HIDE);
end;

function TFormImpl.QueryInterface(constref IID: TGUID; out Obj
  ): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := TFormAccess(FForm).QueryInterface(IID, Obj);
end;

procedure TFormImpl.DoChangeHackedBounds;
begin
  if not FUpdate and Assigned(FOnChangeHackedBounds) then
    FOnChangeHackedBounds(FForm);
end;

function TFormImpl.GetLogicalClientRect(ALogicalClientRect: TRect): TRect;
begin
  Result:=ALogicalClientRect;
end;

constructor TFormImpl.Create(AOwner: TComponent; AForm: TCustomForm);
begin
  inherited Create(AOwner);
  FForm := AForm;
  FDesignedRealForm := Self as IDesignedRealForm;
end;

destructor TFormImpl.Destroy;
begin
  Pointer(FDesignedRealForm) := nil;
  inherited Destroy;
end;

{ TFakeCustomForm }

function TFormContainer.GetDesignedForm: TFormImpl;
begin
  if not Assigned(FDesignedForm) then
    FDesignedForm := TFormImpl.Create(Self, Self);

  Result := FDesignedForm;
end;

function TFormContainer.GetLogicalClientRect: TRect;
begin
  Result := DesignedForm.GetLogicalClientRect(inherited GetLogicalClientRect);
end;

function TFormContainer.GetRealBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := inherited Left;
    1: Result := inherited Top;
    2: Result := inherited Width;
    3: Result := inherited Height;
  end;
end;

procedure TFormContainer.SetRealBounds(AIndex: Integer; AValue: Integer);
begin
  case AIndex of
    0: inherited Left := AValue;
    1: inherited Top := AValue;
    2:
      begin
        inherited Width := AValue;
        if FHandledForm <> nil then
          FHandledForm.Width  := AValue;
      end;
    3:
      begin
        inherited Height := AValue;
        if FHandledForm <> nil then
          FHandledForm.Height  := AValue;
      end;
  end;
end;

function TFormContainer.GetPublishedBounds(AIndex: Integer): Integer;
begin
  Result := DesignedForm.GetPublishedBounds(AIndex);
end;

procedure TFormContainer.SetPublishedBounds(AIndex: Integer; AValue: Integer);
begin
  case AIndex of
    0, 1: DesignedForm.SetPublishedBounds(AIndex, AValue);
    2, 3:
      begin
        DesignedForm.SetPublishedBounds(AIndex, AValue);
        SetRealBounds(AIndex, DesignedForm.GetPublishedBounds(AIndex));
      end;
  end;
end;

constructor TFormContainer.CreateNew(AOwner: TComponent; Num: Integer);
begin
  FBackground := TfrFormBackgroundForMDI.Create(DesignedForm);
  FBackground._AddRef;

  inherited CreateNew(AOwner, Num);

  Left := inherited Left;
  Top := inherited Top;
  Width := inherited Width;
  Height := inherited Height;
end;

destructor TFormContainer.Destroy;
var
  I: IInterfaceComponentReference;
begin
  // we need to call "Screen.RemoveForm" to perform
  // references back to nil by IDesignedForm to FDesignedForm
  inherited Destroy;

  FBackground.QueryInterface(IInterfaceComponentReference, I); // only way to omit SIGSEGV
  I.GetComponent.Free;
  Pointer(I) := nil; // omit _Release (Free is above)
  Pointer(FBackground) := nil; // omit _Release (Free is above)

  if Assigned(FDesignedForm) then
    FreeAndNil(FDesignedForm);
end;

procedure TFormContainer.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  inherited BorderStyle := AVal;
end;

procedure TFormContainer.SetRealBorderIcons(AVal: TBorderIcons);
begin
  inherited BorderIcons := AVal;
end;

procedure TFormContainer.SetRealFormStyle(AVal: TFormStyle);
begin
  inherited FormStyle := AVal;
end;

procedure TFormContainer.SetRealPopupMode(AVal: TPopupMode);
begin
  inherited PopupMode := AVal;
end;

procedure TFormContainer.SetRealPopupParent(AVal: TCustomForm);
begin
  inherited PopupParent := AVal;
end;

function TFormContainer.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := inherited BorderStyle;
end;

function TFormContainer.GetRealBorderIcons: TBorderIcons;
begin
  Result := inherited BorderIcons;
end;

function TFormContainer.GetRealFormStyle: TFormStyle;
begin
  Result := inherited FormStyle;
end;

function TFormContainer.GetRealPopupMode: TPopupMode;
begin
  Result := inherited PopupMode;
end;

function TFormContainer.GetRealPopupParent: TCustomForm;
begin
  Result := inherited PopupParent;
end;

procedure TFormContainer.SetHandledForm(AForm: TCustomForm);
begin
  if FHandledForm = AForm then
    Exit;

  if FHandledForm <> nil then
    FHandledForm.Parent := nil;

  FHandledForm := AForm;

  if FHandledForm <> nil then
    FHandledForm.Parent := Self;
end;


end.

