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
  Classes, SysUtils,
  // LCL
  Forms, Controls,
  // IdeIntf
  FormEditingIntf, SrcEditorIntf, ObjectInspector,
  // Sparta
  sparta_InterfacesMDI, sparta_DesignedForm, sparta_BasicFakeCustom;

type
  { TDesignedFormImpl }

  TDesignedFormImpl = class(TFormImpl, IDesignedRealForm, IDesignedRealFormHelper, IDesignedForm, IDesignedFormIDE)
  private
    FLastActiveSourceWindow: TSourceEditorWindowInterface;
  protected
    function GetLastActiveSourceWindow: TSourceEditorWindowInterface; virtual;
    procedure SetLastActiveSourceWindow(AValue: TSourceEditorWindowInterface); virtual;
  public
    procedure BeginUpdate; override;
    procedure EndUpdate(AModified: Boolean = False); override;
  end;

  { TFakeCustomForm }

  TFakeCustomForm = class(TForm, IDesignedRealForm, IDesignedForm, IDesignedFormIDE)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm, IDesignedFormIDE;
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

  TFakeCustomNonControl = class(TNonControlProxyDesignerForm, IDesignedRealForm, IDesignedForm, IDesignedFormIDE)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm, IDesignedFormIDE;
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

  TFakeCustomFrame = class(TFrameProxyDesignerForm, IDesignedRealForm, IDesignedForm, IDesignedFormIDE)
  private
    FDesignedForm: TDesignedFormImpl;
    function GetDesignedForm: TDesignedFormImpl;
  protected
    property DesignedForm: TDesignedFormImpl read GetDesignedForm implements IDesignedForm, IDesignedFormIDE;
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
  TFormAccess = class(TForm);

{ TDesignedNonControlFormImpl }

function TDesignedNonControlFormImpl.GetPublishedBounds(AIndex: Integer): Integer;
var
  LBounds, LClientRect: TRect;
  LMediator: TDesignerMediator;
  LLookupRoot: TComponent;
begin
  LLookupRoot := (FForm as TNonFormProxyDesignerForm).LookupRoot;
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
    LMediator := (FForm as TNonControlProxyDesignerForm).Mediator;
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
  LLookupRoot := (FForm as TNonFormProxyDesignerForm).LookupRoot;
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
    LMediator := (FForm as TNonControlProxyDesignerForm).Mediator;
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
  if (FForm as TNonFormProxyDesignerForm).LookupRoot <> nil then
    with (TNonFormProxyDesignerForm(FForm).LookupRoot as TFrame) do
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
  if (FForm as TNonFormProxyDesignerForm).LookupRoot <> nil then
    with (TNonFormProxyDesignerForm(FForm).LookupRoot as TControl) do
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
    FDesignedForm := TDesignedFrameFormImpl.Create(Self,Self);

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
  //DesignedForm.SetPublishedBounds(AIndex, AValue);

  if LookupRoot <> nil then
    with LookupRoot as TControl do
    case AIndex of
      0: Left := AValue;
      1: Top := AValue;
      2: Width := AValue;
      3: Height := AValue;
    end;
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
    FDesignedForm := TDesignedNonControlFormImpl.Create(Self,Self);

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

procedure TFakeCustomNonControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
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
    FDesignedForm := TDesignedFormImpl.Create(Self,Self);

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

function TDesignedFormImpl.GetLastActiveSourceWindow: TSourceEditorWindowInterface;
begin
  Result := FLastActiveSourceWindow;
end;

procedure TDesignedFormImpl.SetLastActiveSourceWindow(
  AValue: TSourceEditorWindowInterface);
begin
  FLastActiveSourceWindow := AValue;
end;

procedure TDesignedFormImpl.BeginUpdate;
begin
  TFormAccess(FForm).SetDesigning(False, False);
  inherited BeginUpdate;
end;

procedure TDesignedFormImpl.EndUpdate(AModified: Boolean);
var
  OI: TObjectInspectorDlg;
begin
  TFormAccess(FForm).SetDesigning(True, False);
  inherited EndUpdate(AModified);
  if AModified and (FormEditingHook <> nil)
  and (FormEditingHook.GetCurrentDesigner = FForm.Designer) then
  begin
    OI := FormEditingHook.GetCurrentObjectInspector;
    if Assigned(OI) then
      OI.RefreshPropertyValues;
  end;
end;

end.

