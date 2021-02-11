{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit DockedFormAccesses;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, LCLIntf, LCLType,
  // IdeIntf
  FormEditingIntf, SrcEditorIntf, ObjectInspector, ComponentEditors,
  // DockedFormEditor
  DockedBasicAnchorDesigner;

type

  { TFormAccess }

  TFormAccess = class
  private
    FForm: TCustomForm;
    FOnChangeHackedBounds: TNotifyEvent;
    FUpdate: Boolean;
  protected
    function GetPublishedBounds(AIndex: Integer): Integer;
    procedure SetPublishedBounds(AIndex: Integer; AValue: Integer);
    procedure DoChangeHackedBounds;
  public
    constructor Create(AForm: TCustomForm); virtual;
    procedure BeginUpdate; virtual;
    function ClientOffset: TPoint;
    procedure EndUpdate({%H-}AModified: Boolean = False); virtual;
    procedure HideWindow;
    procedure ShowWindow;
  public
    property Form: TCustomForm read FForm;
    property Left: Integer index 0 read GetPublishedBounds write SetPublishedBounds;
    property Top: Integer index 1 read GetPublishedBounds write SetPublishedBounds;
    property Width: Integer index 2 read GetPublishedBounds write SetPublishedBounds;
    property Height: Integer index 3 read GetPublishedBounds write SetPublishedBounds;
    property OnChangeHackedBounds: TNotifyEvent read FOnChangeHackedBounds write FOnChangeHackedBounds;
    property Update: Boolean read FUpdate;
  end;

  { TResizeForm }

  TResizeForm = class(TFormAccess)
  protected
    function GetHorzScrollPosition: Integer; virtual;
    function GetVertScrollPosition: Integer; virtual;
    procedure SetHorzScrollPosition({%H-}AValue: Integer); virtual;
    procedure SetVertScrollPosition({%H-}AValue: Integer); virtual;
  public
    property HorzScrollPosition: Integer read GetHorzScrollPosition write SetHorzScrollPosition;
    property VertScrollPosition: Integer read GetVertScrollPosition write SetVertScrollPosition;
  end;

  { TDesignFormIDE }

  TDesignFormIDE = class(TResizeForm)
  private
    FAnchorDesigner: TBasicAnchorDesigner;
    FLastActiveSourceWindow: TSourceEditorWindowInterface;
    FSelectedControl: TControl;
    function GetDesigner: TIDesigner;
    function GetDesignWinControl: TWinControl;
  public
    constructor Create(AForm: TCustomForm); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate(AModified: Boolean = False); override;
    function IsAnchorDesign: Boolean;
  public
    property AnchorDesigner: TBasicAnchorDesigner read FAnchorDesigner write FAnchorDesigner;
    property Designer: TIDesigner read GetDesigner;
    property DesignWinControl: TWinControl read GetDesignWinControl;
    property LastActiveSourceWindow: TSourceEditorWindowInterface read FLastActiveSourceWindow write FLastActiveSourceWindow;
    property SelectedControl: TControl read FSelectedControl write FSelectedControl;
  end;

implementation

type
  THackForm = class(TForm);

{ TResizeForm }

function TResizeForm.GetHorzScrollPosition: Integer;
begin
  Result := -Left;
end;

function TResizeForm.GetVertScrollPosition: Integer;
begin
  Result := -Top;
end;

procedure TResizeForm.SetHorzScrollPosition(AValue: Integer);
begin
end;

procedure TResizeForm.SetVertScrollPosition(AValue: Integer);
begin
end;

{ TDesignFormIDE }

function TFormAccess.GetPublishedBounds(AIndex: Integer): Integer;
begin
  case AIndex of
    0: Result := FForm.Left;
    1: Result := FForm.Top;
    2: Result := FForm.Width;
    3: Result := FForm.Height;
  end;
end;

procedure TFormAccess.SetPublishedBounds(AIndex: Integer; AValue: Integer);
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

  DoChangeHackedBounds;
end;

procedure TFormAccess.DoChangeHackedBounds;
begin
  if not FUpdate and Assigned(FOnChangeHackedBounds) then
    FOnChangeHackedBounds(FForm);
end;

constructor TFormAccess.Create(AForm: TCustomForm);
begin
  FForm := AForm;
  FUpdate := False;
end;

procedure TFormAccess.BeginUpdate;
begin
  FUpdate := True;
end;

function TFormAccess.ClientOffset: TPoint;
begin
  Result := Point(0, 0);
  {$IFDEF WINDOWS}
  Result.X := GetSystemMetrics(SM_CXSIZEFRAME);
  Result.Y := GetSystemMetrics(SM_CYSIZEFRAME) + GetSystemMetrics(SM_CYCAPTION);
  {$ENDIF}
end;

procedure TFormAccess.EndUpdate(AModified: Boolean);
begin
  FUpdate := False;
end;

procedure TFormAccess.HideWindow;
begin
  if FForm.Parent = nil then
    LCLIntf.ShowWindow(FForm.Handle, SW_HIDE);
end;

procedure TFormAccess.ShowWindow;
begin
  if FForm.Parent = nil then
    LCLIntf.ShowWindow(FForm.Handle, SW_SHOW);
end;

{ TDesignFormIDE }

function TDesignFormIDE.GetDesigner: TIDesigner;
begin
  Result := FForm.Designer;
end;

function TDesignFormIDE.GetDesignWinControl: TWinControl;
begin
  Result := Form;
  if Form is TNonFormProxyDesignerForm then
    if TNonFormProxyDesignerForm(Form).LookupRoot is TWinControl then
      Result := TWinControl(TNonFormProxyDesignerForm(Form).LookupRoot)
    else
      Result := nil;
end;

constructor TDesignFormIDE.Create(AForm: TCustomForm);
begin
  inherited Create(AForm);
  FAnchorDesigner := nil;
  FLastActiveSourceWindow := nil;
end;

destructor TDesignFormIDE.Destroy;
begin
  FreeAndNil(FAnchorDesigner);
  inherited Destroy;
end;

procedure TDesignFormIDE.BeginUpdate;
begin
  THackForm(FForm).SetDesigning(False, False);
  if Assigned(FAnchorDesigner) then
    FAnchorDesigner.BeginUpdate;
  inherited BeginUpdate;
end;

procedure TDesignFormIDE.EndUpdate(AModified: Boolean);
var
  OI: TObjectInspectorDlg;
begin
  THackForm(FForm).SetDesigning(True, False);
  if Assigned(FAnchorDesigner) then
    FAnchorDesigner.EndUpdate;
  inherited EndUpdate(AModified);
  if AModified and (FormEditingHook <> nil)
  and (FormEditingHook.GetCurrentDesigner = Designer) then
  begin
    OI := FormEditingHook.GetCurrentObjectInspector;
    if Assigned(OI) then
      OI.RefreshPropertyValues;
  end;
end;

function TDesignFormIDE.IsAnchorDesign: Boolean;
begin
  Result := Assigned(DesignWinControl);
end;

end.

