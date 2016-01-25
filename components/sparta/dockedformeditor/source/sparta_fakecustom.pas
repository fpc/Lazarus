{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_FakeCustom;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, FormEditingIntf, Controls, TypInfo, LCLIntf,
  LCLType, sparta_DesignedForm, Math,
{$IFDEF USE_GENERICS_COLLECTIONS}
  Generics.Defaults,
{$ENDIF}
  SrcEditorIntf;

type
  { TDesignedFormImpl }

  TDesignedFormImpl = class(TComponent, IDesignedRealFormHelper, IDesignedForm)
  private
    FOwner: TForm;
    FDesignedRealForm: IDesignedRealForm;
    FHackLeft: Integer;
    FHackTop: Integer;
    FHackWidth: Integer;
    FHackHeight: Integer;

  private
    FHorzScrollPosition: Integer;
    FVertScrollPosition: Integer;
    FOnChangeHackedBounds: TNotifyEvent;
    FLastActiveSourceWindow: TSourceEditorWindowInterface;

    procedure SetOnChangeHackedBounds(const AValue: TNotifyEvent);
    function GetOnChangeHackedBounds: TNotifyEvent;
    function PositionDelta: TPoint;
  protected
    FUpdate: boolean;
  protected
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

    function GetLastActiveSourceWindow: TSourceEditorWindowInterface; virtual;
    procedure SetLastActiveSourceWindow(AValue: TSourceEditorWindowInterface); virtual;

    function GetForm: TCustomForm; virtual;
    function GetUpdate: Boolean; virtual;
  protected
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

    constructor Create(AOwner: TForm); virtual;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    procedure EndUpdate(AModified: Boolean = False); virtual;

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

  { TFakeCustomForm }

  TFakeCustomForm = class(TForm, IDesignedRealForm, IDesignedForm)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm;
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
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
  published
    property Left: Integer index 0 read GetPublishedBounds write SetPublishedBounds;
    property Top: Integer index 1 read GetPublishedBounds write SetPublishedBounds;
    property Width: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property Height: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
    property ClientWidth: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property ClientHeight: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
  end;

  { TDesignedNonControlFormImpl }

  TDesignedNonControlFormImpl = class(TDesignedFormImpl)
  protected
    function GetPublishedBounds(AIndex: Integer): Integer; override;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); override;
  end;

  { TFakeCustomNonControl }

  TFakeCustomNonControl = class(TNonControlProxyDesignerForm, IDesignedRealForm, IDesignedForm)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm;
    function GetLogicalClientRect: TRect; override;
  protected
    function GetRealBounds(AIndex: Integer): Integer; virtual;
    procedure SetRealBounds(AIndex: Integer; AValue: Integer); virtual;
    function GetPublishedBounds(AIndex: Integer): Integer; override;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); override;

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
    procedure SetLookupRoot(AValue: TComponent); override;
    procedure SetMediator(AValue: TDesignerMediator); override;
  public
    constructor Create(AOwner: TComponent; ANonFormDesigner: INonFormDesigner); override;
    destructor Destroy; override;
    function DockedDesigner: boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  end;


  { TDesignedFrameFormImpl }

  TDesignedFrameFormImpl = class(TDesignedFormImpl)
  protected
    function GetPublishedBounds(AIndex: Integer): Integer; override;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); override;
  end;

  { TFakeCustomFrame }

  TFakeCustomFrame = class(TFrameProxyDesignerForm, IDesignedRealForm, IDesignedForm)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm;
    function GetLogicalClientRect: TRect; override;
  protected
    function GetRealBounds(AIndex: Integer): Integer; virtual;
    procedure SetRealBounds(AIndex: Integer; AValue: Integer); virtual;
    function GetPublishedBounds(AIndex: Integer): Integer; override;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer); override;

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
    procedure SetLookupRoot(AValue: TComponent); override;
  public
    constructor Create(AOwner: TComponent; ANonFormDesigner: INonFormDesigner); override;
    destructor Destroy; override;

    function DockedDesigner: boolean; override;
  end;

implementation

uses
  sparta_MainIDE;

type
  TFormHack = class(TForm);

{ TDesignedNonControlFormImpl }

function TDesignedNonControlFormImpl.GetPublishedBounds(AIndex: Integer
  ): Integer;
var
  LBounds, LClientRect: TRect;
  LMediator: TDesignerMediator;
  LLookupRoot: TComponent;
begin
  LLookupRoot := (FOwner as TNonFormProxyDesignerForm).LookupRoot;
  if LLookupRoot is TDataModule then
    with TDataModule(LLookupRoot) do
    case AIndex of
      0: Result := DesignOffset.x;
      1: Result := DesignOffset.y;
      2: Result := DesignSize.x;
      3: Result := DesignSize.y;
    end
  else
  begin
    LMediator := (FOwner as TNonControlProxyDesignerForm).Mediator;
    if (LLookupRoot <> nil) and (LMediator <> nil) then
    begin
      LMediator.GetFormBounds(LLookupRoot, LBounds, LClientRect);
      //WriteLn(Format('get Bounds >>> %d %d %d %d',[LBounds.Left,LBounds.Top,LBounds.Right,LBounds.Bottom]));
      //WriteLn(Format('get Client Rect >>> %d %d %d %d',[LClientRect.Left,LClientRect.Top,LClientRect.Right,LClientRect.Bottom]));
      case AIndex of
        0: Result := LBounds.Left;
        1: Result := LBounds.Top;
        2: Result := LClientRect.Right;
        3: Result := LClientRect.Bottom;
      end;
    end
    else
      Result := 0; //inherited GetPublishedBounds(AIndex);
  end
end;

procedure TDesignedNonControlFormImpl.SetPublishedBounds(AIndex: Integer;
  AValue: Integer);
var
  LBounds, LClientRect: TRect;
  LMediator: TDesignerMediator;
  LLookupRoot: TComponent;
begin
  LLookupRoot := (FOwner as TNonFormProxyDesignerForm).LookupRoot;
  if LLookupRoot is TDataModule then
    with TDataModule(LLookupRoot) do
    case AIndex of
      0: DesignOffset := Point(AValue, DesignOffset.y);
      1: DesignOffset := Point(DesignOffset.x, AValue);
      2: DesignSize := Point(AValue, DesignSize.y);
      3: DesignSize := Point(DesignSize.x, AValue);
    end
  else
  begin
    LMediator := (FOwner as TNonControlProxyDesignerForm).Mediator;
    if (LLookupRoot <> nil) and (LMediator <> nil) then
    begin
      LMediator.GetFormBounds(LLookupRoot, LBounds, LClientRect);
      //WriteLn(Format('set Bounds >>> %d %d %d %d',[LBounds.Left,LBounds.Top,LBounds.Right,LBounds.Bottom]));
      //WriteLn(Format('set Client Rect >>> %d %d %d %d',[LClientRect.Left,LClientRect.Top,LClientRect.Right,LClientRect.Bottom]));
      case AIndex of
        0: LBounds := Rect(AValue, LBounds.Top, AValue + LClientRect.Right, LBounds.Bottom);
        1: LBounds := Rect(LBounds.Left, AValue, LBounds.Right, AValue + LClientRect.Bottom);
        2: LClientRect := Rect(0, 0, AValue, LClientRect.Bottom);
        3: LClientRect := Rect(0, 0, LClientRect.Right, AValue);
      end;
      if AIndex in [2, 3] then
        LBounds := Rect(LBounds.Left, LBounds.Top, LBounds.Left + LClientRect.Right, LBounds.Top + LClientRect.Bottom);
      LMediator.SetFormBounds(LLookupRoot,LBounds,LClientRect);
    end;
  end;

  // refresh for OI
  inherited SetPublishedBounds(AIndex, AValue);
end;

{ TDesignedFrameFormImpl }

function TDesignedFrameFormImpl.GetPublishedBounds(AIndex: Integer): Integer;
begin
  if (FOwner as TNonFormProxyDesignerForm).LookupRoot <> nil then
    with (TNonFormProxyDesignerForm(FOwner).LookupRoot as TFrame) do
    case AIndex of
      0: Result := Left;
      1: Result := Top;
      2: Result := Width;
      3: Result := Height;
    end
  else
    Result:=inherited GetPublishedBounds(AIndex);
end;

procedure TDesignedFrameFormImpl.SetPublishedBounds(AIndex: Integer;
  AValue: Integer);
begin
  if (FOwner as TNonFormProxyDesignerForm).LookupRoot <> nil then
    with (TNonFormProxyDesignerForm(FOwner).LookupRoot as TControl) do
    case AIndex of
      0: Left := AValue;
      1: Top := AValue;
      2: Width := AValue;
      3: Height := AValue;
    end;

  // refresh for OI
  inherited SetPublishedBounds(AIndex, AValue);
end;

{ TFakeCustomFrame }

function TFakeCustomFrame.GetDesignedForm: TDesignedFormImpl;
begin
  if not Assigned(FDesignedForm) then
    FDesignedForm := TDesignedFrameFormImpl.Create(Self);

  Result := FDesignedForm;
end;

function TFakeCustomFrame.GetLogicalClientRect: TRect;
begin
  Result := DesignedForm.GetLogicalClientRect(inherited GetLogicalClientRect);
end;

function TFakeCustomFrame.GetRealBounds(AIndex: Integer): Integer;
begin
  Result := inherited GetPublishedBounds(AIndex);
end;

procedure TFakeCustomFrame.SetRealBounds(AIndex: Integer; AValue: Integer);
begin
  inherited SetPublishedBounds(AIndex, AValue);
end;

function TFakeCustomFrame.GetPublishedBounds(AIndex: Integer): Integer;
begin
  Result := DesignedForm.GetPublishedBounds(AIndex);
end;

procedure TFakeCustomFrame.SetPublishedBounds(AIndex: Integer; AValue: Integer);
begin
  DesignedForm.SetPublishedBounds(AIndex, AValue);
end;

constructor TFakeCustomFrame.Create(AOwner: TComponent;
  ANonFormDesigner: INonFormDesigner);
begin
  inherited Create(AOwner, ANonFormDesigner);

  //NonFormDesignerOptions := NonFormDesignerOptions - [nfdokSetBounds];

  Left := inherited Left;
  Top := inherited Top;
  Width := inherited Width;
  Height := inherited Height;
end;

destructor TFakeCustomFrame.Destroy;
begin
  // we need to call "Screen.RemoveForm" to perform
  // references back to nil by IDesignedForm to FDesignedForm
  inherited Destroy;
  if Assigned(FDesignedForm) then
    FDesignedForm.Free;
end;

function TFakeCustomFrame.DockedDesigner: boolean;
begin
  Result := True;
end;

procedure TFakeCustomFrame.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  inherited BorderStyle := AVal;
end;

procedure TFakeCustomFrame.SetRealBorderIcons(AVal: TBorderIcons);
begin
  inherited BorderIcons := AVal;
end;

procedure TFakeCustomFrame.SetRealFormStyle(AVal: TFormStyle);
begin
  inherited FormStyle := AVal;
end;

procedure TFakeCustomFrame.SetRealPopupMode(AVal: TPopupMode);
begin
  inherited PopupMode := AVal;
end;

procedure TFakeCustomFrame.SetRealPopupParent(AVal: TCustomForm);
begin
  inherited PopupParent := AVal;
end;

function TFakeCustomFrame.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := inherited BorderStyle;
end;

function TFakeCustomFrame.GetRealBorderIcons: TBorderIcons;
begin
  Result := inherited BorderIcons;
end;

function TFakeCustomFrame.GetRealFormStyle: TFormStyle;
begin
  Result := inherited FormStyle;
end;

function TFakeCustomFrame.GetRealPopupMode: TPopupMode;
begin
  Result := inherited PopupMode;
end;

function TFakeCustomFrame.GetRealPopupParent: TCustomForm;
begin
  Result := inherited PopupParent;
end;

procedure TFakeCustomFrame.SetLookupRoot(AValue: TComponent);
begin
  inherited SetLookupRoot(AValue);

  if AValue <> nil then
  begin
    Left   := (LookupRoot as TFrame).Left;
    Top    := (LookupRoot as TFrame).Top;
    Width  := (LookupRoot as TFrame).Width;
    Height := (LookupRoot as TFrame).Height;

    DesignedForm.RealWidth := Width;
    DesignedForm.RealHeight := Height;
  end
  else
    TSpartaMainIDE.TryFreeFormData(Self);
end;

{ TFakeCustomNonControl }

function TFakeCustomNonControl.GetDesignedForm: TDesignedFormImpl;
begin
  if not Assigned(FDesignedForm) then
    FDesignedForm := TDesignedNonControlFormImpl.Create(Self);

  Result := FDesignedForm;
end;

function TFakeCustomNonControl.GetLogicalClientRect: TRect;
begin
  Result := DesignedForm.GetLogicalClientRect(inherited GetLogicalClientRect);
end;

function TFakeCustomNonControl.GetRealBounds(AIndex: Integer): Integer;
begin
  Result := inherited GetPublishedBounds(AIndex);
end;

procedure TFakeCustomNonControl.SetRealBounds(AIndex: Integer; AValue: Integer);
begin
  inherited SetPublishedBounds(AIndex, AValue);
end;

function TFakeCustomNonControl.GetPublishedBounds(AIndex: Integer): Integer;
begin
  Result := DesignedForm.GetPublishedBounds(AIndex);
end;

procedure TFakeCustomNonControl.SetPublishedBounds(AIndex: Integer; AValue: Integer);
begin
  DesignedForm.SetPublishedBounds(AIndex, AValue);
end;

constructor TFakeCustomNonControl.Create(AOwner: TComponent;
  ANonFormDesigner: INonFormDesigner);
begin
  inherited Create(AOwner, ANonFormDesigner);

  //NonFormDesignerOptions := [];

  Left := inherited Left;
  Top := inherited Top;
  Width := inherited Width;
  Height := inherited Height;
end;

destructor TFakeCustomNonControl.Destroy;
begin
  // we need to call "Screen.RemoveForm" to perform
  // references back to nil by IDesignedForm to FDesignedForm
  inherited Destroy;
  if Assigned(FDesignedForm) then
    FDesignedForm.Free;
end;

function TFakeCustomNonControl.DockedDesigner: boolean;
begin
  Result := True;
end;

procedure TFakeCustomNonControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer
  );
begin
  SetDesignerFormBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TFakeCustomNonControl.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  inherited BorderStyle := AVal;
end;

procedure TFakeCustomNonControl.SetRealBorderIcons(AVal: TBorderIcons);
begin
  inherited BorderIcons := AVal;
end;

procedure TFakeCustomNonControl.SetRealFormStyle(AVal: TFormStyle);
begin
  inherited FormStyle := AVal;
end;

procedure TFakeCustomNonControl.SetRealPopupMode(AVal: TPopupMode);
begin
  inherited PopupMode := AVal;
end;

procedure TFakeCustomNonControl.SetRealPopupParent(AVal: TCustomForm);
begin
  inherited PopupParent := AVal;
end;

function TFakeCustomNonControl.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := inherited BorderStyle;
end;

function TFakeCustomNonControl.GetRealBorderIcons: TBorderIcons;
begin
  Result := inherited BorderIcons;
end;

function TFakeCustomNonControl.GetRealFormStyle: TFormStyle;
begin
  Result := inherited FormStyle;
end;

function TFakeCustomNonControl.GetRealPopupMode: TPopupMode;
begin
  Result := inherited PopupMode;
end;

function TFakeCustomNonControl.GetRealPopupParent: TCustomForm;
begin
  Result := inherited PopupParent;
end;

procedure TFakeCustomNonControl.SetLookupRoot(AValue: TComponent);
var
  LBounds, LClientRect: TRect;
begin
  inherited SetLookupRoot(AValue);

  if AValue <> nil then
  begin
    if LookupRoot is TDataModule then
    begin
      Width  := (LookupRoot as TDataModule).DesignSize.x;
      Height := (LookupRoot as TDataModule).DesignSize.y;
    end
    else if (LookupRoot <> nil) and (Mediator <> nil) then
    begin
      Mediator.GetFormBounds(LookupRoot,LBounds,LClientRect);
      //WriteLn(Format('Bounds >>> %d %d %d %d',[LBounds.Left,LBounds.Top,LBounds.Right,LBounds.Bottom]));
      //WriteLn(Format('Client Rect >>> %d %d %d %d',[LClientRect.Left,LClientRect.Top,LClientRect.Right,LClientRect.Bottom]));
      Width := LClientRect.Right;
      Height := LClientRect.Bottom;
    end else
      Width := 1;

    DesignedForm.RealWidth := Width;
    DesignedForm.RealHeight := Height;
  end
  else
    TSpartaMainIDE.TryFreeFormData(Self);
end;

procedure TFakeCustomNonControl.SetMediator(AValue: TDesignerMediator);
var
  LBounds, LClientRect: TRect;
begin
  inherited SetMediator(AValue);

  if (LookupRoot <> nil) and (Mediator <> nil) then
  begin
    Mediator.GetFormBounds(LookupRoot,LBounds,LClientRect);
    //WriteLn(Format('Bounds >>> %d %d %d %d',[LBounds.Left,LBounds.Top,LBounds.Right,LBounds.Bottom]));
    //WriteLn(Format('Client Rect >>> %d %d %d %d',[LClientRect.Left,LClientRect.Top,LClientRect.Right,LClientRect.Bottom]));
    Width := LClientRect.Right;
    Height := LClientRect.Bottom;
  end else
    ;//WriteLn('o kurwa eh');
end;

{ TFakeCustomForm }

function TFakeCustomForm.GetDesignedForm: TDesignedFormImpl;
begin
  if not Assigned(FDesignedForm) then
    FDesignedForm := TDesignedFormImpl.Create(Self);

  Result := FDesignedForm;
end;

function TFakeCustomForm.GetLogicalClientRect: TRect;
begin
  Result := DesignedForm.GetLogicalClientRect(inherited GetLogicalClientRect);
end;

function TFakeCustomForm.GetRealBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := inherited Left;
    1: Result := inherited Top;
    2: Result := inherited Width;
    3: Result := inherited Height;
  end;
end;

procedure TFakeCustomForm.SetRealBounds(AIndex: Integer; AValue: Integer);
begin
  case AIndex of
    0: inherited Left := AValue;
    1: inherited Top := AValue;
    2: inherited Width := AValue;
    3: inherited Height := AValue;
  end;
end;

function TFakeCustomForm.GetPublishedBounds(AIndex: Integer): Integer;
begin
  Result := DesignedForm.GetPublishedBounds(AIndex);
end;

procedure TFakeCustomForm.SetPublishedBounds(AIndex: Integer; AValue: Integer);
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

constructor TFakeCustomForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);

  Left := inherited Left;
  Top := inherited Top;
  Width := inherited Width;
  Height := inherited Height;
end;

destructor TFakeCustomForm.Destroy;
begin
  // we need to call "Screen.RemoveForm" to perform
  // references back to nil by IDesignedForm to FDesignedForm
  inherited Destroy;
  if Assigned(FDesignedForm) then
    FreeAndNil(FDesignedForm);
end;

procedure TFakeCustomForm.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  inherited BorderStyle := AVal;
end;

procedure TFakeCustomForm.SetRealBorderIcons(AVal: TBorderIcons);
begin
  inherited BorderIcons := AVal;
end;

procedure TFakeCustomForm.SetRealFormStyle(AVal: TFormStyle);
begin
  inherited FormStyle := AVal;
end;

procedure TFakeCustomForm.SetRealPopupMode(AVal: TPopupMode);
begin
  inherited PopupMode := AVal;
end;

procedure TFakeCustomForm.SetRealPopupParent(AVal: TCustomForm);
begin
  inherited PopupParent := AVal;
end;

function TFakeCustomForm.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := inherited BorderStyle;
end;

function TFakeCustomForm.GetRealBorderIcons: TBorderIcons;
begin
  Result := inherited BorderIcons;
end;

function TFakeCustomForm.GetRealFormStyle: TFormStyle;
begin
  Result := inherited FormStyle;
end;

function TFakeCustomForm.GetRealPopupMode: TPopupMode;
begin
  Result := inherited PopupMode;
end;

function TFakeCustomForm.GetRealPopupParent: TCustomForm;
begin
  Result := inherited PopupParent;
end;


{ TDesignedFormImpl }

function TDesignedFormImpl.GetPublishedBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FHackLeft;
    1: Result := FHackTop;
    2: Result := FHackWidth;
    3: Result := FHackHeight;
  end;
end;

procedure TDesignedFormImpl.SetPublishedBounds(AIndex: Integer; AValue: Integer);
begin
  if AIndex = 2 then
    if AValue < 135 then
      AValue := 135;

  if AIndex in [2, 3] then
    if AValue > 4096 then
      AValue := 4096;

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

function TDesignedFormImpl.GetRealBounds(AIndex: Integer): Integer;
begin
  Result := FDesignedRealForm.GetRealBounds(AIndex);
end;

procedure TDesignedFormImpl.SetRealBounds(AIndex: Integer; AValue: Integer);
begin
  FDesignedRealForm.SetRealBounds(AIndex, AValue);
end;

procedure TDesignedFormImpl.SetRealBorderStyle(AVal: TFormBorderStyle);
begin
  FDesignedRealForm.SetRealBorderStyle(AVal);
end;

procedure TDesignedFormImpl.SetRealBorderIcons(AVal: TBorderIcons);
begin
  FDesignedRealForm.SetRealBorderIcons(AVal);
end;

procedure TDesignedFormImpl.SetRealFormStyle(AVal: TFormStyle);
begin
  FDesignedRealForm.SetRealFormStyle(AVal);
end;

procedure TDesignedFormImpl.SetRealPopupMode(AVal: TPopupMode);
begin
  FDesignedRealForm.SetRealPopupMode(AVal);
end;

procedure TDesignedFormImpl.SetRealPopupParent(AVal: TCustomForm);
begin
  FDesignedRealForm.SetRealPopupParent(AVal);
end;

function TDesignedFormImpl.GetRealBorderStyle: TFormBorderStyle;
begin
  Result := FDesignedRealForm.GetRealBorderStyle;
end;

function TDesignedFormImpl.GetRealBorderIcons: TBorderIcons;
begin
  Result := FDesignedRealForm.GetRealBorderIcons;
end;

function TDesignedFormImpl.GetRealFormStyle: TFormStyle;
begin
  Result := FDesignedRealForm.GetRealFormStyle;
end;

function TDesignedFormImpl.GetRealPopupMode: TPopupMode;
begin
  Result := FDesignedRealForm.GetRealPopupMode;
end;

function TDesignedFormImpl.GetRealPopupParent: TCustomForm;
begin
  Result := FDesignedRealForm.GetRealPopupParent;
end;

//////

function TDesignedFormImpl.GetLastActiveSourceWindow: TSourceEditorWindowInterface;
begin
  Result := FLastActiveSourceWindow;
end;

procedure TDesignedFormImpl.SetLastActiveSourceWindow(
  AValue: TSourceEditorWindowInterface);
begin
  FLastActiveSourceWindow := AValue;
end;

function TDesignedFormImpl.GetForm: TCustomForm;
begin
  Result := FOwner;
end;

function TDesignedFormImpl.GetUpdate: Boolean;
begin
  Result := FUpdate;
end;

function TDesignedFormImpl.GetOnChangeHackedBounds: TNotifyEvent;
begin
  Result := FOnChangeHackedBounds;
end;

function TDesignedFormImpl.PositionDelta: TPoint;

  procedure FormBorderDelta;
  var
    LTestCtrl: TWinControl;
    LTestRec, LFormRect: TRect;
    LForm: TCustomForm;
  begin
    LForm := GetForm;
    LTestCtrl := TWinControl.Create(Self);
    try
      LTestCtrl.Parent := LForm;
      LTestCtrl.Left := 0;
      LTestCtrl.Top := 0;

      GetWindowRect(LForm.Handle, LFormRect);
      GetWindowRect(LTestCtrl.Handle, LTestRec);

      Result.x := Result.x + Max(LTestRec.Left - LFormRect.Left, 0);
      Result.y := Result.y + Max(LTestRec.Top  - LFormRect.Top,  0);
    finally
      LTestCtrl.free;
    end;
  end;

begin
  Result := Point(0, 0);
  {$IFDEF WINDOWS}
  FormBorderDelta;
  {$ENDIF}
end;

procedure TDesignedFormImpl.SetOnChangeHackedBounds(const AValue: TNotifyEvent);
begin
  FOnChangeHackedBounds := AValue;
end;

/////// positions

procedure TDesignedFormImpl.SetHorzScrollPosition(AValue: Integer);
begin
  RealLeft := -PositionDelta.x - AValue;
end;

procedure TDesignedFormImpl.SetVertScrollPosition(AValue: Integer);
begin
  RealTop := -PositionDelta.y - AValue;
end;

function TDesignedFormImpl.GetHorzScrollPosition: Integer;
begin
  Result := -(RealLeft + PositionDelta.x);
end;

function TDesignedFormImpl.GetVertScrollPosition: Integer;
begin
  Result := -(RealTop + PositionDelta.y);
end;

procedure TDesignedFormImpl.BeginUpdate;
begin
  TFormHack(FOwner).SetDesigning(False, False);
  FUpdate := True;
end;

procedure TDesignedFormImpl.EndUpdate(AModified: Boolean);
begin
  TFormHack(FOwner).SetDesigning(True, False);
  FUpdate := False;
  if AModified and (FormEditingHook <> nil) then
    if (FormEditingHook.GetCurrentDesigner = FOwner.Designer) and (FormEditingHook.GetCurrentObjectInspector <> nil) then
      FormEditingHook.GetCurrentObjectInspector.RefreshPropertyValues;
end;

procedure TDesignedFormImpl.ShowWindow;
begin
  if FOwner.Parent = nil then
    LCLIntf.ShowWindow(FOwner.Handle, SW_SHOW);
end;

procedure TDesignedFormImpl.HideWindow;
begin
  if FOwner.Parent = nil then
    LCLIntf.ShowWindow(FOwner.Handle, SW_HIDE);
end;

function TDesignedFormImpl.QueryInterface(constref IID: TGUID; out Obj
  ): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  if Result <> S_OK then
    Result := TFormHack(FOwner).QueryInterface(IID, Obj);
end;

procedure TDesignedFormImpl.DoChangeHackedBounds;
begin
  if not FUpdate and Assigned(FOnChangeHackedBounds) then
    FOnChangeHackedBounds(FOwner);
end;

function TDesignedFormImpl.GetLogicalClientRect(ALogicalClientRect: TRect): TRect;
begin
  Result:=ALogicalClientRect;
end;

constructor TDesignedFormImpl.Create(AOwner: TForm);
begin
  FOwner := AOwner;
  FDesignedRealForm := FOwner as IDesignedRealForm;
end;

destructor TDesignedFormImpl.Destroy;
begin
  Pointer(FDesignedRealForm) := nil;
  inherited Destroy;
end;

end.

