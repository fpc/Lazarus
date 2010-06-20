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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit window_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  LCLProc, Spin,
  ObjInspStrConsts, ObjectInspector, IDEOptionsIntf, IDEWindowIntf,
  EnvironmentOpts, IDEOptionDefs,
  InterfaceBase, LazarusIDEStrConsts;

type
  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    ApplyButton: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CustomPositionRadioButton: TRadioButton;
    DefaultRadioButton: TRadioButton;
    GetWindowPositionButton: TButton;
    HeightEdit: TSpinEdit;
    HeightLabel: TLabel;
    HideIDEOnRunCheckBox: TCheckBox;
    HideMessagesIconsCheckBox: TCheckBox;
    lblWindowCaption: TLabel;
    LeftEdit: TSpinEdit;
    LeftLabel: TLabel;
    SingleTaskBarButtonCheckBox: TCheckBox;
    RestoreWindowGeometryRadioButton: TRadioButton;
    TitleStartsWithProjectCheckBox: TCheckBox;
    ProjectDirInIdeTitleCheckBox: TCheckBox;
    TopEdit: TSpinEdit;
    TopLabel: TLabel;
    UseWindowManagerSettingRadioButton: TRadioButton;
    WidthEdit: TSpinEdit;
    WidthLabel: TLabel;
    WindowPositionsGroupBox: TGroupBox;
    WindowPositionsListBox: TListBox;
    procedure ApplyButtonClick(Sender: TObject);
    procedure GetWindowPositionButtonClick(Sender: TObject);
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TSimpleWindowLayoutList;
    FLayout: TSimpleWindowLayout;
    function GetPlacementRadioButtons(APlacement: TIDEWindowPlacement): TRadioButton;
    procedure SetLayout(const AValue: TSimpleWindowLayout);
    procedure SetWindowPositionsItem(Index: integer);
    procedure SaveLayout;
    function GetLayoutCaption(ALayout: TSimpleWindowLayout): String;
    property Layout: TSimpleWindowLayout read FLayout write SetLayout;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
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
  SingleTaskBarButtonCheckBox.Caption := dlgSingleTaskBarButton;
  SingleTaskBarButtonCheckBox.Enabled :=
    WidgetSet.GetLCLCapability(lcNeedMininimizeAppWithMainForm) = LCL_CAPABILITY_YES;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;
  HideMessagesIconsCheckBox.Caption := dlgHideMessagesIcons;
  TitleStartsWithProjectCheckBox.Caption:=lisIDETitleStartsWithProjectName;
  TitleStartsWithProjectCheckBox.Hint:=
    lisTitleInTaskbarShowsForExampleProject1LpiLazarus;
  ProjectDirInIdeTitleCheckBox.Caption:=lisIDEProjectDirInIdeTitle;
  ProjectDirInIdeTitleCheckBox.Hint:=
    lisProjectDirectoryIsShowedInIdeTitleBar;
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Creator: TIDEWindowCreator;
  i: Integer;
  j: Integer;
begin
  with AOptions as TEnvironmentOptions do
  begin
    // window minimizing and hiding
    SingleTaskBarButtonCheckBox.Checked := SingleTaskBarButton;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    HideMessagesIconsCheckBox.Checked := HideMessagesIcons;
    TitleStartsWithProjectCheckBox.Checked:=IDETitleStartsWithProject;
    ProjectDirInIdeTitleCheckBox.Checked:=IDEProjectDirectoryInIdeTitle;

  end;

  FLayouts.Assign(IDEWindowCreators.SimpleLayoutStorage);

  if IDEDockMaster=nil then begin
    // Window Positions
    FLayouts.Assign(IDEWindowCreators.SimpleLayoutStorage);
    WindowPositionsGroupBox.Parent:=Self;
    WindowPositionsGroupBox.Caption := dlgWinPos;
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
    WindowPositionsListBox.Items.EndUpdate;

    LeftLabel.Caption := dlgLeftPos;
    TopLabel.Caption := dlgTopPos;
    WidthLabel.Caption := dlgWidthPos;
    HeightLabel.Caption := DlgHeightPos;
    ApplyButton.Caption := dlgButApply;
    GetWindowPositionButton.Caption := dlgGetPosition;

    UseWindowManagerSettingRadioButton.Caption := rsiwpUseWindowManagerSetting;
    DefaultRadioButton.Caption := rsiwpDefault;
    RestoreWindowGeometryRadioButton.Caption := rsiwpRestoreWindowGeometry;
    CustomPositionRadioButton.Caption := rsiwpCustomPosition;

    SetWindowPositionsItem(0);
  end else begin
    WindowPositionsGroupBox.Parent:=nil;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  SaveLayout;
  IDEWindowCreators.SimpleLayoutStorage.Assign(FLayouts);

  with AOptions as TEnvironmentOptions do
  begin
    // window minimizing
    SingleTaskBarButton := SingleTaskBarButtonCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
    HideMessagesIcons:=HideMessagesIconsCheckBox.Checked;
    IDETitleStartsWithProject:=TitleStartsWithProjectCheckBox.Checked;
    IDEProjectDirectoryInIdeTitle:=ProjectDirInIdeTitleCheckBox.Checked;
  end;
end;

function TWindowOptionsFrame.GetPlacementRadioButtons(
  APlacement: TIDEWindowPlacement): TRadioButton;
begin
  case APlacement of
    iwpRestoreWindowGeometry:   Result := RestoreWindowGeometryRadioButton;
    iwpDefault:                 Result := DefaultRadioButton;
    iwpCustomPosition:          Result := CustomPositionRadioButton;
    iwpUseWindowManagerSetting: Result := UseWindowManagerSettingRadioButton;
  else
    Result := nil;
  end;
end;

procedure TWindowOptionsFrame.SetLayout(const AValue: TSimpleWindowLayout);
var
  APlacement: TIDEWindowPlacement;
  RadioButton: TRadioButton;
  p: TPoint;
begin
  FLayout := AValue;
  if Layout=nil then Exit;
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
end;

procedure TWindowOptionsFrame.WindowPositionsListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TWindowOptionsFrame.ApplyButtonClick(Sender: TObject);
var
  NewBounds: TRect;
begin
  SaveLayout;
  if (Layout<>nil) and (Layout.Form<>nil) and (Layout.Form.Parent=nil)
  and (Layout.WindowPlacement in [iwpCustomPosition,iwpRestoreWindowGeometry])
  then begin
    if (Layout.CustomCoordinatesAreValid) then begin
      // explicit position
      NewBounds:=Bounds(Layout.Left,Layout.Top,Layout.Width,Layout.Height);
      // set minimum size
      if NewBounds.Right-NewBounds.Left<20 then
        NewBounds.Right:=NewBounds.Left+20;
      if NewBounds.Bottom-NewBounds.Top<20 then
        NewBounds.Bottom:=NewBounds.Top+20;
      // move to visible area
      if NewBounds.Right<20 then
        OffsetRect(NewBounds,20-NewBounds.Right,0);
      if NewBounds.Bottom<20 then
        OffsetRect(NewBounds,0,20-NewBounds.Bottom);
      if NewBounds.Left>Screen.DesktopWidth-20 then
        OffsetRect(NewBounds,NewBounds.Left-(Screen.DesktopWidth-20),0);
      if NewBounds.Top>Screen.DesktopHeight-20 then
        OffsetRect(NewBounds,NewBounds.Top-(Screen.DesktopHeight-20),0);
      Layout.Form.SetBounds(
        NewBounds.Left,NewBounds.Top,
        NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
    end;
  end;
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
end;

function TWindowOptionsFrame.GetLayoutCaption(ALayout: TSimpleWindowLayout
  ): String;

  function Fits(FormName, aCaption: string): boolean;
  var
    SubIndex: LongInt;
  begin
    Result:=CompareText(FormName,copy(ALayout.FormID,1,length(FormName)))=0;
    if not Result then exit(false);
    SubIndex:=StrToIntDef(copy(ALayout.FormID,length(FormName)+1,10),-1);
    if SubIndex<0 then
      GetLayoutCaption:=aCaption
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
  if Fits('PkgGraphExplorer',dlgPackageGraph) then exit;
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
  if Fits('CodeBrowser',lisCodeBrowser) then exit;
  if Fits('IssueBrowser',lisMenuViewRestrictionBrowser) then exit;
  if Fits('JumpHistory',lisJHJumpHistory) then exit;
  Result:=ALayout.FormCaption;
end;

constructor TWindowOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLayouts:=TSimpleWindowLayoutList.Create;
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

