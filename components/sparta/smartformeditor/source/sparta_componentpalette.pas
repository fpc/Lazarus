{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_ComponentPalette;

{$mode delphi}{$H+}

interface

uses
  Forms, Classes, SysUtils, Controls, ComCtrls, ComponentReg, ExtCtrls, Buttons,
  Math, LazIDEIntf, PropEdits, LResources, LCLType, Graphics,
{$IF FPC_FULLVERSION>=30200}
  Generics.Collections,
{$ELSE}
  sparta_Generics.Collections,
{$ENDIF}
  FormEditingIntf, IDEImagesIntf;

type

  { TPageData }

  TPageData = record
    FUpDown: TUpDown;
    FComponents: TPanel;
    FSelectionTool: TSpeedButton;

    constructor Create(AUpDown: TUpDown; AComponents: TPanel; ASelectionTool: TSpeedButton);
  end;

  { TComponentsPalette }

  TComponentsPalette = class(TComponent)
  private
    pcComponents: TPageControl;
    FFilter: string;
    FRoot: TPersistent;
    FPages: TDictionary<TTabSheet, TPageData>;
    FLastForm: TCustomForm;
    FIgnoreRoot: Boolean;

    procedure SetRoot(AValue: TPersistent);
    function GetRoot: TPersistent;

    procedure OnComponentClick(Sender: TObject);
    procedure OnComponentDblClick(Sender: TObject);
    procedure ComponentAdded(ALookupRoot, AComponent: TComponent; ARegisteredComponent: TRegisteredComponent);

    procedure AddComponent(AComponent: TRegisteredComponent);

    procedure OnUpdateIDEComponentPalette(Sender: TObject);
    procedure SetFilter(AValue: string);


    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure pcComponentsResize(Sender: TObject);
    procedure pcComponentsChange(Sender: TObject);
    procedure ComponentsPageCtrlChange(Sender: TObject);

    procedure RefreshSearchResult;
    procedure UpdateComponentsList;

    procedure OnDesignSetSelection(const ASelection: TPersistentSelectionList);
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl; AIgnoreRoot: Boolean = False); reintroduce;
    destructor Destroy; override;
    property Root: TPersistent read GetRoot write SetRoot;
    property Filter: string read FFilter write SetFilter;
    function IsEmpty: Boolean;
  end;

implementation

{$R sparta.res}

{ TPageData }

constructor TPageData.Create(AUpDown: TUpDown; AComponents: TPanel;
  ASelectionTool: TSpeedButton);
begin
  FUpDown := AUpDown;
  FComponents := AComponents;
  FSelectionTool := ASelectionTool;
end;

{ TComponentsPalette }

procedure TComponentsPalette.SetRoot(AValue: TPersistent);
begin
  if FRoot = AValue then
    Exit;
  FRoot := AValue;

  UpdateComponentsList;
end;

function TComponentsPalette.GetRoot: TPersistent;
begin
  Result := FRoot;
end;

procedure TComponentsPalette.OnComponentClick(Sender: TObject);
var
  LComponent: TRegisteredComponent;
  LSender: TSpeedButton absolute Sender;
  i: TComponent;
  LButton: TSpeedButton absolute i;
begin
  // ignore click for dblclick event
  if LSender.Owner.Tag = 1 then
  begin
    LSender.Owner.Tag := 0;
    LSender.Down := False;
    Exit;
  end;
  LComponent := TRegisteredComponent(LSender.Tag);
  if Assigned(LComponent) then
  begin
    IDEComponentPalette.SetSelectedComp(LComponent, ssShift in GetKeyShiftState);
    // deactivate "Selection tool button"
    // all buttons with components AllowAllUp := False and for Selection tool AllowAllUp := True
    for i in LSender.Owner do
      if i is TSpeedButton then
        // trick with LSender.Down for DblClick
        if (LButton.Tag <> 0) then
          LButton.AllowAllUp := False
        else // Selection tool
        begin
          LButton.AllowAllUp := True;
          LButton.Down := False;
        end;
  end
  else
  begin
    IDEComponentPalette.SetSelectedComp(nil, false);
    // deactivate all other buttons than "selection tool button"
    for i in LSender.Owner do
      if i is TSpeedButton then
        if LButton.Tag <> 0 then
        begin
          LButton.AllowAllUp := True;
          LButton.Down := False
        end
        else // Selection tool
          LButton.AllowAllUp := False;
  end;
end;

procedure TComponentsPalette.OnComponentDblClick(Sender: TObject);
var
  LSelectionTool: TSpeedButton;
  LButton: TSpeedButton absolute Sender;
begin
  if not Assigned(pcComponents.ActivePage) then
    Exit;

  AddComponent(TRegisteredComponent(LButton.Tag));

  LSelectionTool := FPages[pcComponents.ActivePage].FSelectionTool;
  LSelectionTool.Down := True;
  OnComponentClick(LSelectionTool);
  LButton.Owner.Tag := 1; // ignore click event
end;

procedure TComponentsPalette.ComponentAdded(ALookupRoot,
  AComponent: TComponent; ARegisteredComponent: TRegisteredComponent);
var
  LSelectionTool: TSpeedButton;
begin
  if (ALookupRoot <> FRoot) or IDEComponentPalette.MultiSelect or not Assigned(pcComponents.ActivePage) then
    Exit;

  LSelectionTool := FPages[pcComponents.ActivePage].FSelectionTool;

  LSelectionTool.Down := True;
  OnComponentClick(LSelectionTool);
end;

procedure TComponentsPalette.AddComponent(AComponent: TRegisteredComponent);
var
  LPos: TPoint;
  LClass: TComponentClass;
  LParent: TComponent;
  LNewComponent: TComponent;
begin
  if not Assigned(AComponent) or not Assigned(FormEditingHook) or not Assigned(FRoot) then
    Exit;

  LClass := AComponent.ComponentClass;

  // form for which was clicked component - this form does not necessarily have to be active...
  GlobalDesignHook.LookupRoot := FRoot;

  LParent := FormEditingHook.GetDefaultComponentParent(LClass);

  // default position
  if not Assigned(LParent) or not FormEditingHook.GetDefaultComponentPosition(LClass, LParent, LPos.X, LPos.Y) then
    exit;

  LNewComponent := FormEditingHook.CreateComponent(LParent, LClass, '', LPos.X, LPos.Y, 0, 0, True);

  if not Assigned(LNewComponent) then
    Exit;

  if LNewComponent is TControl then
    TControl(LNewComponent).EnableAutoSizing;
  GlobalDesignHook.PersistentAdded(LNewComponent, true);
end;

procedure TComponentsPalette.RefreshSearchResult;
var
  i, j: Integer;
  LCtrl: TControl;
  LPComponents: TPanel;
  LButtons: TList<TControl>;
  LVisibleButtons: Integer;
  LCompName: string;
  LSearchResult: TTabSheet;

  procedure AddButton(AButton: TSpeedButton);
  begin
    with TSpeedButton.Create(LSearchResult) do
    begin
      Glyph.Assign(AButton.Glyph);
      Hint := AButton.Hint;

      ShowHint := True;
      Flat := True;
      GroupIndex := 1;
      Constraints.MinWidth:=ComponentPaletteBtnWidth;
      Constraints.MinHeight:=ComponentPaletteBtnHeight;
      Constraints.MaxWidth:=ComponentPaletteBtnWidth;
      Constraints.MaxHeight:=ComponentPaletteBtnHeight;

      Parent := LPComponents;
      Tag := AButton.Tag;
      OnClick := OnComponentClick;
      OnDblClick := OnComponentDblClick;
      AllowAllUp := True;
    end;
  end;

begin
  LSearchResult := pcComponents.Pages[0];
  LSearchResult.TabVisible := FFilter <> '';
  if FFilter = '' then
  begin
    // show all
    for i := 1 to pcComponents.PageCount - 1 do
    begin
      pcComponents.Pages[i].TabVisible := True;
      LPComponents := FPages[pcComponents.Pages[i]].FComponents;
      for j := 0 to LPComponents.ControlCount - 1 do
      begin
        LCtrl := LPComponents.Controls[j];
        LCtrl.Visible := True;
      end;
    end;
  end
  // use filter !
  else
  begin
    LButtons := TList<TControl>.Create;
    for i := 1 to pcComponents.PageCount - 1 do
    begin
      LPComponents := FPages[pcComponents.Pages[i]].FComponents;
      LVisibleButtons := LPComponents.ControlCount;
      for j := 0 to LPComponents.ControlCount - 1 do
      begin
        LCtrl := LPComponents.Controls[j];
        LCompName := UpperCase(TRegisteredComponent(LCtrl.Tag).ComponentClass.ClassName);
        if Pos(FFilter, LCompName) > 0 then
        begin
          LButtons.Add(LCtrl);
          LCtrl.Visible := True;
        end
        else
        begin
          Dec(LVisibleButtons);
          LCtrl.Visible := False;
        end;
      end;

      pcComponents.Pages[i].TabVisible := LVisibleButtons > 0;
    end;

    // add all buttons to the new page with results
    LPComponents := FPages[LSearchResult].FComponents;
    for i := LPComponents.ControlCount - 1 downto 0  do
      LPComponents.Controls[i].Free;

    for LCtrl in LButtons do
      AddButton(TSpeedButton(LCtrl));

    LButtons.Free;
    pcComponents.ActivePageIndex:=0;
  end;
  pcComponentsResize(nil);
end;

procedure TComponentsPalette.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
var
  LSender: TUpDown absolute Sender;
begin
  case Direction of
    updUp  : LSender.Tag := ifthen(LSender.Tag > 0, LSender.Tag - 1, 0);
    updDown: LSender.Tag := LSender.Tag + 1;
  end;

  pcComponentsResize(nil);
end;

procedure TComponentsPalette.pcComponentsResize(Sender: TObject);
var
  LPComponents: TPanel;
  LUpDown: TUpDown;
  LLines: Integer;
begin
  if (pcComponents.ActivePage = nil) or (not FPages.ContainsKey(pcComponents.ActivePage)) then
    Exit;

  LPComponents := FPages[pcComponents.ActivePage].FComponents;
  LUpDown := FPages[pcComponents.ActivePage].FUpDown;
  if (LPComponents.ControlCount * ComponentPaletteBtnWidth) < LPComponents.Width then
  begin
    LUpDown.Visible := False;
    LPComponents.ChildSizing.ControlsPerLine := LPComponents.ControlCount;
    LPComponents.Top := 0;
    Exit;
  end;

  LUpDown.Visible := True;
  LPComponents.ChildSizing.ControlsPerLine := LPComponents.ClientWidth div ComponentPaletteBtnWidth;
  if LPComponents.ChildSizing.ControlsPerLine = 0 then
    LPComponents.ChildSizing.ControlsPerLine := 1;
  LLines := LPComponents.ControlCount div LPComponents.ChildSizing.ControlsPerLine;
  Inc(LLines, ifthen(LPComponents.ControlCount mod LPComponents.ChildSizing.ControlsPerLine <> 0, 1, 0));

  if LUpDown.Tag >= LLines then
    LUpDown.Tag := LLines - 1;
  LPComponents.Top := -(LUpDown.Tag * ComponentPaletteBtnHeight);
end;

procedure TComponentsPalette.pcComponentsChange(Sender: TObject);
var
  LComponent: TComponent;
  LButton: TSpeedButton absolute LComponent;
begin
  pcComponentsResize(nil);

  for LComponent in pcComponents.ActivePage do
    if LComponent is TSpeedButton then
      if LButton.Tag = 0 then
      begin
        LButton.Down := True;
        IDEComponentPalette.SetSelectedComp(nil, false);
      end
      else
        LButton.Down := False;
end;

procedure TComponentsPalette.ComponentsPageCtrlChange(Sender: TObject);
var
  i: Integer;
  pc: TPageControl absolute Sender;
  s: string;
begin
  s := pc.Pages[pc.PageIndex].Caption;
  for i := 0 to pcComponents.PageCount do
    if s = pcComponents.Pages[i].Caption then
    begin
      pcComponents.PageIndex := i;
      Exit;
    end;
end;

constructor TComponentsPalette.Create(AOwner: TComponent; AParent: TWinControl;
  AIgnoreRoot: Boolean);
begin
  inherited Create(AOwner);

  FIgnoreRoot := AIgnoreRoot;

  //if AIgnoreRoot then
  //  LazarusIDE.AddHandlerOnUpdateComponentPageControl(ComponentsPageCtrlChange);
  //LazarusIDE.AddHandlerOnUpdateIDEComponentPalette(OnUpdateIDEComponentPalette);

  IDEComponentPalette.AddHandlerComponentAdded(ComponentAdded);
  GlobalDesignHook.AddHandlerSetSelection(OnDesignSetSelection);

  FPages := TDictionary<TTabSheet, TPageData>.Create;

  pcComponents := TPageControl.Create(AOwner);
  pcComponents.Parent := AParent;
  pcComponents.Align:=alClient;
  pcComponents.OnResize:=pcComponentsResize;
  pcComponents.OnChange:=pcComponentsChange;
end;

destructor TComponentsPalette.Destroy;
begin
  if not FIgnoreRoot then
  begin
    //LazarusIDE.RemoveHandlerOnUpdateIDEComponentPalette(OnUpdateIDEComponentPalette);
    IDEComponentPalette.RemoveHandlerComponentAdded(ComponentAdded);
  end;
  FPages.Free;

  inherited Destroy;
end;

function TComponentsPalette.IsEmpty: Boolean;
begin
  Result := pcComponents.PageCount = 0;
end;

procedure TComponentsPalette.OnUpdateIDEComponentPalette(Sender: TObject);
begin
  FLastForm := TCustomForm(Sender);
  if FIgnoreRoot then
    if (Sender <> nil) and (Root = nil) then
      Root := LookupRoot(Sender as TCustomForm);

  if (Sender = nil) or (LookupRoot(Sender as TCustomForm) <> FRoot) then
    Exit;

  if (IDEComponentPalette.Selected = nil) and Assigned(pcComponents.ActivePage) then
    OnComponentClick(FPages[pcComponents.ActivePage].FSelectionTool);
end;

procedure TComponentsPalette.SetFilter(AValue: string);
begin
  if FFilter = AValue then
    Exit;

  FFilter := UpperCase(AValue);
  RefreshSearchResult;
end;

procedure TComponentsPalette.UpdateComponentsList;

  procedure CreatePage(const ACaption: string; APage: TBaseComponentPage);

    function LoadIcon(const AClassName: string): TCustomBitmap;
    var
      LLazResource: TLResource;
    begin
      Result := nil;

      if FindResource(HINSTANCE, PChar(AClassName), PChar(RT_BITMAP)) <> 0 then
      begin
        Result := TBitmap.Create;
        Result.LoadFromResourceName(HINSTANCE, AClassName);
        Result.Transparent := True;
        Exit;
      end
      else
      if FindResource(HINSTANCE, PChar(AClassName), PChar(RT_RCDATA)) <> 0 then
        Result := CreateBitmapFromResourceName(HINSTANCE, AClassName)
      else
      begin
        LLazResource := LazarusResources.Find(AClassName);
        if LLazResource <> nil then
          Exit(CreateBitmapFromLazarusResource(LLazResource));
      end;

      if Result = nil then
        Exit(CreateBitmapFromResourceName(HINSTANCE, 'default'));
    end;

  var
    i: Integer;
    LPage: TTabSheet;
    LUpDown: TUpDown;
    LPSelection: TPanel;
    LPComponents: TPanel;
    LButton: TSpeedButton;
    LComponent: TRegisteredComponent;
    LClass: TComponentClass;
    LIcon: TCustomBitmap;

  begin
    LPage := TTabSheet.Create(pcComponents);
    LPage.Caption := ACaption;
    LPage.PageControl := pcComponents;

    LPSelection := TPanel.Create(LPage);
    LPSelection.Width := ComponentPaletteBtnWidth + ComponentPaletteBtnWidth div 2;
    LPSelection.Align := alLeft;
    LPSelection.ChildSizing.Layout := cclTopToBottomThenLeftToRight;
    LPSelection.ChildSizing.ControlsPerLine := 1;
    LPSelection.Parent := LPage;
    LPSelection.BevelOuter := bvNone;

    LButton := TSpeedButton.Create(LPage);
    with LButton do
    begin
      IDEImages.AssignImage(LButton, 'tmouse');
      Hint := 'Selection tool';

      ShowHint := True;
      Flat := True;
      GroupIndex := 1;
      Constraints.MinWidth:=ComponentPaletteBtnWidth;
      Constraints.MinHeight:=ComponentPaletteBtnHeight;
      Constraints.MaxWidth:=ComponentPaletteBtnWidth;
      Constraints.MaxHeight:=ComponentPaletteBtnHeight;
      Parent := LPSelection;
      AllowAllUp := False;
      Down := True;
      OnClick := OnComponentClick;
    end;

    LUpDown := TUpDown.Create(LPage);
    LUpDown.Parent := LPage;
    LUpDown.Constraints.MaxWidth := 17;
    LUpDown.Anchors := [akRight, akTop, akBottom];
    LUpDown.AnchorSideTop.Control := LPage;
    LUpDown.AnchorSideRight.Control := LPage;
    LUpDown.AnchorSideRight.Side := asrBottom;
    LUpDown.AnchorSideBottom.Control := LPage;
    LUpDown.AnchorSideBottom.Side := asrBottom;
    LUpDown.OnChangingEx := UpDownChangingEx;

    LPComponents := TPanel.Create(LPage);
    LPComponents.Parent := LPage;
    LPComponents.Width := ComponentPaletteBtnWidth + ComponentPaletteBtnWidth div 2;
    LPComponents.Anchors := [akLeft, akTop, akRight, akBottom];

    LPComponents.AnchorSideLeft.Control := LPSelection;
    LPComponents.AnchorSideLeft.Side := asrBottom;
    LPComponents.AnchorSideRight.Control := LUpDown;
    LPComponents.Top := 0;
    LPComponents.AnchorSideBottom.Control := LPage;
    LPComponents.AnchorSideBottom.Side := asrBottom;
    LPComponents.ChildSizing.Layout := cclLeftToRightThenTopToBottom;
    LPComponents.BevelOuter := bvNone;
    LUpDown.Visible := False;
    FPages.Add(LPage, TPageData.Create(LUpDown, LPComponents, LButton));

    // not each page has components - for example: searching result
    if (APage = nil) or (not APage.Visible) then
      Exit;

    for i := 0 to IDEComponentPalette.Comps.Count-1 do
    begin
      LComponent := IDEComponentPalette.Comps[i];

      if LComponent.Visible and (LComponent.RealPage = APage) then
        with TSpeedButton.Create(LPage) do
        begin
          LClass := LComponent.ComponentClass;

          LIcon := LoadIcon(LClass.ClassName);
          Glyph.Assign(LIcon);
          LIcon.Free;

          Hint := Format('%s' + sLineBreak + '(%s)', [LClass.ClassName, LClass.UnitName]);
          ShowHint := True;
          Flat := True;
          GroupIndex := 1;
          Constraints.MinWidth:=ComponentPaletteBtnWidth;
          Constraints.MinHeight:=ComponentPaletteBtnHeight;
          Constraints.MaxWidth:=ComponentPaletteBtnWidth;
          Constraints.MaxHeight:=ComponentPaletteBtnHeight;

          Parent := LPComponents;
          Tag := PtrInt(LComponent);
          OnClick := OnComponentClick;
          OnDblClick := OnComponentDblClick;
          AllowAllUp := True;
        end;
    end;
  end;

var
  LPage: TBaseComponentPage;
  i: Integer;
begin
  if FRoot = nil then
    Exit;
  FPages.Clear;
  if Assigned(IDEComponentPalette) then
  begin
    for i := pcComponents.PageCount - 1 downto 0 do
      pcComponents.Pages[i].Free;
    CreatePage('Search result', nil);
    for i := 0 to IDEComponentPalette.Pages.Count-1 do
    begin
      LPage := IDEComponentPalette.Pages[i];
      if not LPage.Visible then
        Continue;
      CreatePage(LPage.PageName, LPage);
    end;
  end;
  pcComponentsResize(nil);
  RefreshSearchResult;
end;

procedure TComponentsPalette.OnDesignSetSelection(const ASelection: TPersistentSelectionList);
begin
  // to replace original components palette
  if not FIgnoreRoot or (csDestroying in ComponentState) then
    Exit;
  Root := GlobalDesignHook.LookupRoot;
end;

end.

