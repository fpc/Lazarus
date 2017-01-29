{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit window_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  LCLProc, Spin, ObjInspStrConsts, ObjectInspector, IDEOptionsIntf,
  IDEWindowIntf, DividerBevel, EnvironmentOpts, IDEOptionDefs, InterfaceBase,
  LazarusIDEStrConsts;

type
  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    ApplyButton: TButton;
    AutoAdjustIDEHeightFullCompPalCheckBox: TCheckBox;
    lblWindowPosition: TDividerBevel;
    lblShowingWindows: TDividerBevel;
    lblWindowCaption: TDividerBevel;
    NameForDesignedFormList: TCheckBox;
    AutoAdjustIDEHeightCheckBox: TCheckBox;
    TitleIncludesBuildMode: TCheckBox;
    dropSplitterPlacement: TComboBox;
    CustomGeometryRadioButton: TRadioButton;
    FixedDefaultRadioButton: TRadioButton;
    GetWindowPositionButton: TButton;
    HeightEdit: TSpinEdit;
    HeightLabel: TLabel;
    HideIDEOnRunCheckBox: TCheckBox;
    SplitLabel: TLabel;
    LeftEdit: TSpinEdit;
    LeftLabel: TLabel;
    SplitterList: TListBox;
    SplitterPanel: TPanel;
    SingleTaskBarButtonCheckBox: TCheckBox;
    RestoreWindowGeometryRadioButton: TRadioButton;
    SplitEdit: TSpinEdit;
    TitleStartsWithProjectCheckBox: TCheckBox;
    ProjectDirInIdeTitleCheckBox: TCheckBox;
    TopEdit: TSpinEdit;
    TopLabel: TLabel;
    LetWindowManagerDecideRadioButton: TRadioButton;
    WidthEdit: TSpinEdit;
    WidthLabel: TLabel;
    WindowPositionsPanel: TPanel;
    WindowPositionsListBox: TListBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CustomGeometryRadioButtonClick(Sender: TObject);
    procedure GetWindowPositionButtonClick(Sender: TObject);
    procedure WindowGeometryRadioButtonClick(Sender: TObject);
    procedure SplitterListSelectionChange(Sender: TObject; User: boolean);
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TSimpleWindowLayoutList;
    FLayout: TSimpleWindowLayout;
    FDivider: TSimpleWindowLayoutDividerPos;
    FShowSimpleLayout: boolean;
    procedure EnableGeometryEdits(aEnable: Boolean);
    function GetPlacementRadioButtons(APlacement: TIDEWindowPlacement): TRadioButton;
    procedure SetLayout(const AValue: TSimpleWindowLayout);
    procedure SetDivider(const AValue: TSimpleWindowLayoutDividerPos);
    procedure SetWindowPositionsItem(Index: integer);
    procedure SaveCurrentSplitterLayout;
    procedure SaveLayout;
    function GetLayoutCaption(ALayout: TSimpleWindowLayout): String;
    property Layout: TSimpleWindowLayout read FLayout write SetLayout;
    property Divider: TSimpleWindowLayoutDividerPos read FDivider write SetDivider;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TWindowOptionsFrame }

function TWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgWindow;
end;

procedure TWindowOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  // windows
  lblShowingWindows.Caption := dlgShowingWindows;
  SingleTaskBarButtonCheckBox.Caption := dlgSingleTaskBarButton;
  SingleTaskBarButtonCheckBox.Enabled :=
    WidgetSet.GetLCLCapability(lcNeedMininimizeAppWithMainForm) = LCL_CAPABILITY_YES;
  SingleTaskBarButtonCheckBox.Hint:=lisShowOnlyOneButtonInTheTaskbarForTheWholeIDEInstead;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;
  HideIDEOnRunCheckBox.Hint := dlgHideIDEOnRunHint;
  TitleStartsWithProjectCheckBox.Caption:=lisIDETitleStartsWithProjectName;
  TitleStartsWithProjectCheckBox.Hint:=lisTitleInTaskbarShowsForExampleProject1LpiLazarus;
  TitleIncludesBuildMode.Caption:=lisIDETitleShowsBuildMode;
  TitleIncludesBuildMode.Hint:=lisBuildModeInTitleInExample;
  NameForDesignedFormList.Caption:=lisWindowMenuWithNameForDesignedForm;
  NameForDesignedFormList.Hint:=lisWindowMenuWithNameForDesignedFormHint;
  AutoAdjustIDEHeightCheckBox.Caption:=lisAutoAdjustIDEHeight;
  AutoAdjustIDEHeightCheckBox.Hint:=lisAutoAdjustIDEHeightHint;
  AutoAdjustIDEHeightFullCompPalCheckBox.Caption:=lisAutoAdjustIDEHeightFullComponentPalette;
  AutoAdjustIDEHeightFullCompPalCheckBox.Hint:=lisAutoAdjustIDEHeightFullComponentPaletteHint;
  ProjectDirInIdeTitleCheckBox.Caption:=lisIDETitleShowsProjectDir;
  ProjectDirInIdeTitleCheckBox.Hint:=lisProjectDirectoryIsShowedInIdeTitleBar;
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Creator: TIDEWindowCreator;
  i, j: Integer;
begin
  with (AOptions as TEnvironmentOptions).Desktop do
  begin
    // window minimizing and hiding
    SingleTaskBarButtonCheckBox.Checked := SingleTaskBarButton;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    TitleStartsWithProjectCheckBox.Checked:=IDETitleStartsWithProject;
    TitleIncludesBuildMode.Checked:=IDETitleIncludesBuildMode;
    NameForDesignedFormList.Checked:=IDENameForDesignedFormList;
    AutoAdjustIDEHeightCheckBox.Checked:=AutoAdjustIDEHeight;
    AutoAdjustIDEHeightFullCompPalCheckBox.Checked:=AutoAdjustIDEHeightFullCompPal;
    ProjectDirInIdeTitleCheckBox.Checked:=IDEProjectDirectoryInIdeTitle;
  end;

  FLayouts.CopyItemsFrom(IDEWindowCreators.SimpleLayoutStorage);

  if FShowSimpleLayout then begin
    // Window Positions
    lblWindowPosition.Parent:=Self;
    lblWindowPosition.Caption := dlgWinPos;
    WindowPositionsPanel.Parent:=Self;
    WindowPositionsPanel.Caption:='';
    WindowPositionsListBox.Items.BeginUpdate;
    WindowPositionsListBox.Items.Clear;
    // show all registered windows
    // Note: the layouts also contain forms, that were once registered and may be
    // registered in the future again
    for i:=0 to IDEWindowCreators.Count-1 do begin
      Creator:=IDEWindowCreators[i];
      for j:=0 to FLayouts.Count-1 do begin
        if Creator.NameFits(FLayouts[j].FormID) then
          WindowPositionsListBox.Items.AddObject(GetLayoutCaption(FLayouts[j]),FLayouts[j]);
      end;
    end;
    WindowPositionsListBox.Sorted := True;
    WindowPositionsListBox.Items.EndUpdate;

    WindowPositionsListBox.Hint := rsiwpPositionWindowListHint;
    SplitterList.Hint := rsiwpColumnNamesHint;
    dropSplitterPlacement.Hint := rsiwpColumnStrategyHint;
    SplitEdit.Hint := rsiwpColumnWidthHint;

    LeftLabel.Caption := dlgLeftPos;
    TopLabel.Caption := dlgTopPos;
    WidthLabel.Caption := dlgWidthPos;
    HeightLabel.Caption := DlgHeightPos;
    ApplyButton.Caption := lisApply;
    GetWindowPositionButton.Caption := dlgGetPosition;
    SplitLabel.Caption := dlgWidthPos;

    LeftEdit.Hint := rsiwpSplitterCustomPosition;
    TopEdit.Hint := rsiwpSplitterCustomPosition;
    WidthEdit.Hint := rsiwpSplitterCustomPosition;
    HeightEdit.Hint := rsiwpSplitterCustomPosition;

    RestoreWindowGeometryRadioButton.Caption := rsiwpRestoreWindowGeometry;
    LetWindowManagerDecideRadioButton.Caption := rsiwpLetWindowManagerDecide;
    FixedDefaultRadioButton.Caption := rsiwpFixedDefaultGeometry;
    CustomGeometryRadioButton.Caption := rsiwpCustomGeometry;

    RestoreWindowGeometryRadioButton.Hint := rsiwpRestoreWindowGeometryHint;
    LetWindowManagerDecideRadioButton.Hint := rsiwpLetWindowManagerDecideHint;
    FixedDefaultRadioButton.Hint := rsiwpFixedDefaultGeometryHint;
    CustomGeometryRadioButton.Hint := rsiwpCustomGeometryHint;

    dropSplitterPlacement.Clear;
    dropSplitterPlacement.Items.Add(rsiwpSplitterFollowWindow);
    dropSplitterPlacement.Items.Add(rsiwpSplitterRestoreWindowGeometry);
    dropSplitterPlacement.Items.Add(rsiwpSplitterDefault);
    dropSplitterPlacement.Items.Add(rsiwpSplitterCustomPosition);

    SetWindowPositionsItem(0);
  end else begin
    lblWindowPosition.Parent:=nil;
    WindowPositionsPanel.Parent:=nil;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  SaveLayout;
  IDEWindowCreators.SimpleLayoutStorage.CopyItemsFrom(FLayouts);

  with (AOptions as TEnvironmentOptions).Desktop do
  begin
    // window minimizing
    SingleTaskBarButton := SingleTaskBarButtonCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
    IDETitleStartsWithProject:=TitleStartsWithProjectCheckBox.Checked;
    IDETitleIncludesBuildMode := TitleIncludesBuildMode.Checked;
    IDENameForDesignedFormList := NameForDesignedFormList.Checked;
    AutoAdjustIDEHeight := AutoAdjustIDEHeightCheckBox.Checked;
    AutoAdjustIDEHeightFullCompPal := AutoAdjustIDEHeightFullCompPalCheckBox.Checked;
    IDEProjectDirectoryInIdeTitle:=ProjectDirInIdeTitleCheckBox.Checked;
  end;
end;

function TWindowOptionsFrame.GetPlacementRadioButtons(
  APlacement: TIDEWindowPlacement): TRadioButton;
begin
  case APlacement of
    iwpRestoreWindowGeometry:   Result := RestoreWindowGeometryRadioButton;
    iwpDefault:                 Result := FixedDefaultRadioButton;
    iwpCustomPosition:          Result := CustomGeometryRadioButton;
    iwpUseWindowManagerSetting: Result := LetWindowManagerDecideRadioButton;
  else
    Result := nil;
  end;
end;

procedure TWindowOptionsFrame.SetLayout(const AValue: TSimpleWindowLayout);
var
  APlacement: TIDEWindowPlacement;
  RadioButton: TRadioButton;
  p: TPoint;
  i: Integer;
begin
  FLayout := AValue;
  Divider := nil;
  if Layout=nil then begin
    SplitterPanel.Visible := False;
    Exit;
  end;
  //debugln(['TWindowOptionsFrame.SetLayout ',Layout.FormID,' ',IDEWindowPlacementNames[Layout.WindowPlacement]]);

  for APlacement := Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    RadioButton := GetPlacementRadioButtons(APlacement);
    if RadioButton=nil then continue;

    RadioButton.Enabled := True;
    RadioButton.Checked := (APlacement = Layout.WindowPlacement);
  end;
  // custom window position
  if Layout.CustomCoordinatesAreValid then
  begin
    LeftEdit.Value := Layout.Left;
    TopEdit.Value := Layout.Top;
    WidthEdit.Value := Layout.Width;
    HeightEdit.Value := Layout.Height;
  end
  else
  if Layout.Form <> nil then
  begin
    if Layout.Form.Parent<>nil then begin
      p:=Layout.Form.ClientOrigin;
      LeftEdit.Value := p.X;
      TopEdit.Value := p.Y;
    end else begin
      LeftEdit.Value := Layout.Form.Left;
      TopEdit.Value := Layout.Form.Top;
    end;
    WidthEdit.Value := Layout.Form.Width;
    HeightEdit.Value := Layout.Form.Height;
  end
  else
  begin
    LeftEdit.Value := 0;
    TopEdit.Value := 0;
    WidthEdit.Value := 0;
    HeightEdit.Value := 0;
  end;

  GetWindowPositionButton.Enabled := (Layout.Form <> nil);

  SplitterPanel.Visible := Layout.Dividers.NamedCount > 0;
  SplitterList.Clear;
  for i := 0 to Layout.Dividers.NamedCount - 1 do
    SplitterList.AddItem(Layout.Dividers.NamedItems[i].DisplayName, Layout.Dividers.NamedItems[i]);
  if Layout.Dividers.NamedCount > 0 then
    SplitterList.ItemIndex := 0;
end;

procedure TWindowOptionsFrame.SetDivider(const AValue: TSimpleWindowLayoutDividerPos);
begin
  FDivider := AValue;
  if FDivider=nil then exit;

  SplitEdit.Value := FDivider.Size;
  case FDivider.Placement of
    iwpdUseWindowSetting: dropSplitterPlacement.ItemIndex := 0;
    iwpdRestore:          dropSplitterPlacement.ItemIndex := 1;
    iwpdDefault:          dropSplitterPlacement.ItemIndex := 2;
    iwpdCustomSize:       dropSplitterPlacement.ItemIndex := 3;
  end;
end;

procedure TWindowOptionsFrame.WindowPositionsListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TWindowOptionsFrame.ApplyButtonClick(Sender: TObject);
begin
  SaveLayout;
  if (Layout<>nil) and (Layout.Form<>nil) and (Layout.Form.Parent=nil) then begin
    if (Layout.WindowPlacement in [iwpCustomPosition,iwpRestoreWindowGeometry]) then begin
      Layout.ValidateAndSetCoordinates; // Adjust bounds to screen area and apply them.
      Layout.Applied := True;
    end;
    Layout.ApplyDivider(True);
  end;
end;

procedure TWindowOptionsFrame.EnableGeometryEdits(aEnable: Boolean);
begin
  LeftEdit.Enabled := aEnable;
  TopEdit.Enabled := aEnable;
  WidthEdit.Enabled := aEnable;
  HeightEdit.Enabled := aEnable;
end;

procedure TWindowOptionsFrame.WindowGeometryRadioButtonClick(Sender: TObject);
begin
  EnableGeometryEdits(False);
end;

procedure TWindowOptionsFrame.CustomGeometryRadioButtonClick(Sender: TObject);
begin
  EnableGeometryEdits(True);
end;

procedure TWindowOptionsFrame.GetWindowPositionButtonClick(Sender: TObject);
begin
  if (Layout<>nil) and (Layout.Form <> nil) then
  begin
    LeftEdit.Value := Layout.Form.Left;
    TopEdit.Value := Layout.Form.Top;
    WidthEdit.Value := Layout.Form.Width;
    HeightEdit.Value := Layout.Form.Height;
  end;
  Layout.ReadCurrentDividers(True);
  SplitterListSelectionChange(nil, False);
end;

procedure TWindowOptionsFrame.SplitterListSelectionChange(Sender: TObject; User: boolean);
begin
  if User then SaveCurrentSplitterLayout;

  if (SplitterList.Count = 0) or (SplitterList.ItemIndex < 0) then exit;
  SetDivider(TSimpleWindowLayoutDividerPos(SplitterList.Items.Objects[SplitterList.ItemIndex]));
end;

procedure TWindowOptionsFrame.SetWindowPositionsItem(Index: integer);
begin
  SaveLayout;
  WindowPositionsListBox.ItemIndex := Index;

  if Index>=0 then
    Layout:=TSimpleWindowLayout(WindowPositionsListBox.Items.Objects[Index])
  else
    Layout:=nil;

  if Index >= 0 then
    lblWindowCaption.Caption := WindowPositionsListBox.Items[Index];
end;

procedure TWindowOptionsFrame.SaveCurrentSplitterLayout;
begin
  if FDivider = nil then exit;
  case dropSplitterPlacement.ItemIndex of
    0: FDivider.Placement := iwpdUseWindowSetting;
    1: FDivider.Placement := iwpdRestore;
    2: FDivider.Placement := iwpdDefault;
    3: FDivider.Placement := iwpdCustomSize;
  end;
  FDivider.Size := SplitEdit.Value;
end;

procedure TWindowOptionsFrame.SaveLayout;
var
  APlacement: TIDEWindowPlacement;
  ARadioButton: TRadioButton;
begin
  if Layout = nil then
   Exit;
  //debugln(['TWindowOptionsFrame.SaveLayout ',Layout.FormID]);
  for APlacement := Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    ARadioButton := GetPlacementRadioButtons(APlacement);
    if (ARadioButton <> nil) and ARadioButton.Enabled and ARadioButton.Checked then
      Layout.WindowPlacement := APlacement;
    if APlacement = iwpCustomPosition then
    begin
      Layout.Left := LeftEdit.Value;
      Layout.Top := TopEdit.Value;
      Layout.Width := WidthEdit.Value;
      Layout.Height := HeightEdit.Value;
    end;
  end;
  SaveCurrentSplitterLayout;
end;

function TWindowOptionsFrame.GetLayoutCaption(ALayout: TSimpleWindowLayout): String;

  function Fits(FormName, aCaption: string): boolean;
  var
    SubIndex: LongInt;
  begin
    Result:=CompareText(FormName,copy(ALayout.FormID,1,length(FormName)))=0;
    if not Result then exit;
    SubIndex:=StrToIntDef(copy(ALayout.FormID,length(FormName)+1,10),-1);
    if SubIndex<0 then
      GetLayoutCaption:=aCaption   // Set Result of the main function.
    else
      GetLayoutCaption:=aCaption+' '+IntToStr(SubIndex);
  end;

begin
  // use the known resourcestrings
  if Fits('MainIDE',dlgMainMenu) then exit;
  if Fits('SourceNotebook',dlgSrcEdit) then exit;
  if Fits('MessagesView',dlgMsgs) then exit;
  if Fits('ObjectInspectorDlg',oisObjectInspector) then exit;
  if Fits('UnitDependencies',dlgUnitDepCaption) then exit;
  if Fits('CodeExplorerView',lisCodeExplorer) then exit;
  if Fits('FPDocEditor',lisCodeHelpMainFormCaption) then exit;
  if Fits('PkgGraphExplorer',lisMenuPackageGraph) then exit;
  if Fits('ProjectInspector',lisMenuProjectInspector) then exit;
  if Fits('DbgOutput',lisMenuViewDebugOutput) then exit;
  if Fits('DbgEvents',lisMenuViewDebugEvents) then exit;
  if Fits('BreakPoints',lisMenuViewBreakPoints) then exit;
  if Fits('Watches',liswlWatchList) then exit;
  if Fits('Locals',lisLocals) then exit;
  if Fits('CallStack',lisMenuViewCallStack) then exit;
  if Fits('EvaluateModify',lisKMEvaluateModify) then exit;
  if Fits('Registers',lisRegisters) then exit;
  if Fits('Assembler',lisMenuViewAssembler) then exit;
  if Fits('Inspect',lisInspectDialog) then exit;
  if Fits('SearchResults',lisMenuViewSearchResults) then exit;
  if Fits('AnchorEditor',lisMenuViewAnchorEditor) then exit;
  if Fits('TabOrderEditor',lisMenuViewTabOrder) then exit;
  if Fits('CodeBrowser',lisCodeBrowser) then exit;
  if Fits('IssueBrowser',lisMenuViewRestrictionBrowser) then exit;
  if Fits('JumpHistory',lisJHJumpHistory) then exit;
  if Fits('PseudoTerminal', lisMenuViewPseudoTerminal) then exit;
  if Fits('Threads', lisMenuViewThreads) then exit;
  if Fits('DbgHistory', lisMenuViewHistory) then exit;
  if Fits('ComponentList', lisCmpLstComponents) then exit;
  Result:=ALayout.FormCaption;
end;

constructor TWindowOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLayouts:=TSimpleWindowLayoutList.Create(False);
  FShowSimpleLayout:=(IDEDockMaster=nil) or (not IDEDockMaster.HideSimpleLayoutOptions);
end;

destructor TWindowOptionsFrame.Destroy;
begin
  FreeAndNil(FLayouts);
  inherited Destroy;
end;

class function TWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TWindowOptionsFrame, EnvOptionsWindow);
end.

