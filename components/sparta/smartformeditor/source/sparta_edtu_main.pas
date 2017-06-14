{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_EDTU_Main;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, Buttons, ComCtrls,
  StdCtrls, Menus, EditBtn, ComponentReg, FormEditingIntf,
  LazIDEIntf, SpartaAPI, Math, PropEdits, sparta_ComponentPalette, sparta_FakeForm;

type

  { TedtuMain }

  TedtuMain = class(TFrame, ISTADesignTimeUtil, ISTAMainDesignTimeUtil)
    eFilter: TEditButton;
    pInfo: TPanel;
    pComponents: TPanel;
    pSearch: TPanel;
    pEDTU: TPanel;
    sbShowPalette: TSpeedButton;
    procedure eFilterButtonClick(Sender: TObject);
    procedure eFilterChange(Sender: TObject);
    procedure sbShowPaletteClick(Sender: TObject);
  private
    FRoot: TPersistent;
    FEDTU: TList;
    FNonVisualComponentsEDTU: Pointer;
    FComponentsPalette: TComponentsPalette;

    procedure CreateEDTUButtons;
    procedure SetRoot(AValue: TPersistent);
    function GetRoot: TPersistent;

    function GetShowNonVisualComponents: Boolean;

    procedure OnShowEditorClick(Sender: TObject);

    procedure OnDesignRefreshPropertyValues;
    procedure OnPersistentDeleted;
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
  public
    { public declarations }
    pAddons: TWinControl;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Root: TPersistent read GetRoot write SetRoot;
  end;

implementation

{$R *.lfm}

{ TedtuMain }

procedure TedtuMain.eFilterChange(Sender: TObject);
begin
  if Assigned(FComponentsPalette) then
    FComponentsPalette.Filter := eFilter.Text;
end;

procedure TedtuMain.eFilterButtonClick(Sender: TObject);
begin
  eFilter.Text := '';
end;

procedure TedtuMain.sbShowPaletteClick(Sender: TObject);
begin
  if pComponents.Visible = False then
  begin
    Parent.Height:=55;
    Height:=55;
    pComponents.Visible := True;
    eFilter.SetFocus;
    pInfo.Visible := False;
  end
  else
  begin
    Parent.Height:=22;
    Height:=22;
    pComponents.Visible := False;
    pInfo.Visible := True;
  end;

  if not Assigned(FComponentsPalette) then
  begin
    FComponentsPalette := TComponentsPalette.Create(pComponents, pComponents);
    FComponentsPalette.Root := FRoot;
  end;
end;

procedure TedtuMain.CreateEDTUButtons;
var
  i: Integer;
  LLeft: TControl;
  LButton: TSpeedButton;
  LASR: TAnchorSideReference;
begin
  FEDTU := TList.Create;

  LLeft := pEDTU;
  LASR := asrLeft;
  for i := 0 to DTUManager.EDTUCount - 1 do
    if DTUManager.EDTU[i].AvailableForRoot(FRoot) then
    begin
      LButton := TSpeedButton.Create(pEDTU);
      with  LButton do
      begin
        Parent := pEDTU;
        AnchorSideLeft.Control := LLeft;
        AnchorSideLeft.Side := LASR;
        AnchorSideBottom.Control := pEDTU;
        AnchorSideBottom.Side := asrBottom;
        Anchors := [akLeft, akBottom];
        LLeft := LButton;
        Tag := -Succ(i);
        LoadGlyphFromResourceName(HINSTANCE, DTUManager.EDTU[i].GlyphName);
        GroupIndex := 1;
        AllowAllUp := True;
        OnClick:=OnShowEditorClick;
      end;
      LASR := asrRight;
    end;
end;

procedure TedtuMain.SetRoot(AValue: TPersistent);
var
  i: Integer;
begin
  if FRoot = AValue then
    Exit;
  FRoot := AValue;

  // skoro tu jestesmy pierwszy raz to ARoot <> nil (FRoot domyslnie ma nil)
  if FEDTU = nil then
    CreateEDTUButtons
  else
    for i := 0 to FEDTU.Count - 1 do
        ISTAExtendedDesignTimeUtil(FEDTU[i]).Root := FRoot;

  if Assigned(FComponentsPalette) then
    FComponentsPalette.Root := FRoot;

  OnDesignRefreshPropertyValues;
end;

function TedtuMain.GetRoot: TPersistent;
begin
  Result := FRoot;
end;

function TedtuMain.GetShowNonVisualComponents: Boolean;
begin
  if FNonVisualComponentsEDTU <> nil then
    Result := ISTANonVisualComponentsUtil(FNonVisualComponentsEDTU).ShowNonVisualComponents
  else
    Result := True;
end;

procedure TedtuMain.OnShowEditorClick(Sender: TObject);
var
  LCtrl: TControl;
  i: Integer;
  LButton: TSpeedButton absolute Sender;
  LEDTU: ISTAExtendedDesignTimeUtil;
begin
  if FRoot = nil then
    Exit;

  if LButton.Tag < 0 then
  begin
    LEDTU := DTUManager.EDTU[Pred(-LButton.Tag)].CreateEDTUForRoot(pAddons, FRoot);
    LEDTU.Parent := pAddons;
    LEDTU.RefreshValues;
    LButton.Tag := FEDTU.Add(LEDTU);

    if Supports(LEDTU, ISTANonVisualComponentsUtil) then
      FNonVisualComponentsEDTU := LEDTU as ISTANonVisualComponentsUtil;
  end;

  for i := 0 to pEDTU.ControlCount - 1 do
  begin
    LCtrl := pEDTU.Controls[i];
    if LButton = LCtrl then
      Continue;

    if LCtrl.Tag >= 0 then
      ISTAExtendedDesignTimeUtil(FEDTU[LCtrl.Tag]).Visible := False;
  end;

  ISTAExtendedDesignTimeUtil(FEDTU[LButton.Tag]).Visible := LButton.Down;
  pAddons.Width := ifthen(not LButton.Down, 0, 256);
end;

procedure TedtuMain.OnDesignRefreshPropertyValues;
var
  i: Integer;
  f: TFakeForm;
  LCtrlCount: Integer = 0;
  LCompCount: Integer = 0;
  LNonVisualCount: Integer = 0;

  procedure GetCompAndCtrlCount(AComp: TComponent);
  var
    i: Integer;
    LComp: TComponent;
  begin
    Inc(LCompCount, AComp.ComponentCount);
    if AComp is TWinControl then
      Inc(LCtrlCount, TWinControl(AComp).ControlCount);
    for i := 0 to AComp.ComponentCount - 1 do
      GetCompAndCtrlCount(AComp.Components[i]);
  end;

begin
  if FRoot <> GlobalDesignHook.LookupRoot then
    Exit;

  if FRoot is TForm then
  begin
    f := TFakeForm(FRoot);
    //pInfo.Caption := Format('%s (X: %d Y: %d W: %d H: %d) ComponentCount = %d ControlCount = %d NonVisualCount = %d',
    //  [f.Name, f.Left, f.Top, f.Width, f.Height, LCompCount, LCtrlCount, LCompCount - LCtrlCount]);
    pInfo.Caption := Format('%s (X: %d Y: %d W: %d H: %d)',
      [f.Name, f.Left, f.Top, f.Width, f.Height]);
  end;
end;

procedure TedtuMain.OnPersistentDeleted;
begin
  OnDesignRefreshPropertyValues
end;

procedure TedtuMain.OnPersistentAdded(APersistent: TPersistent; Select: boolean
  );
begin
  OnDesignRefreshPropertyValues
end;

constructor TedtuMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  sbShowPalette.LoadGlyphFromResourceName(HINSTANCE, 'SHOW_PALETTE_UP');
  eFilter.Button.LoadGlyphFromResourceName(HINSTANCE, 'MENU_CLOSE');
  GlobalDesignHook.AddHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.AddHandlerPersistentDeleted(OnPersistentDeleted);
  GlobalDesignHook.AddHandlerPersistentAdded(OnPersistentAdded);
end;

destructor TedtuMain.Destroy;
begin
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(OnDesignRefreshPropertyValues);
  GlobalDesignHook.RemoveHandlerPersistentDeleted(OnPersistentDeleted);
  GlobalDesignHook.RemoveHandlerPersistentAdded(OnPersistentAdded);
  FEDTU.Free;
  inherited Destroy;
end;

end.

