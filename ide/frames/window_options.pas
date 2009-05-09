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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls,
  EnvironmentOpts, LazarusIDEStrConsts, IDEOptionDefs, ObjectInspector, IDEOptionsIntf;

type
  TOnApplyWindowPos = procedure(Layout: TIDEWindowLayout) of object;

  TIDEWindowSetupLayoutComponent = class(TGroupBox)
    RestoreWindowGeometryRadioButton: TRadioButton;
    DefaultRadioButton: TRadioButton;
    CustomPositionRadioButton: TRadioButton;
    LeftLabel: TLabel;
    LeftEdit: TEdit;
    TopLabel: TLabel;
    TopEdit: TEdit;
    WidthLabel: TLabel;
    WidthEdit: TEdit;
    HeightLabel: TLabel;
    HeightEdit: TEdit;
    UseWindowManagerSettingRadioButton: TRadioButton;
    DockedRadioButton: TRadioButton;
    ApplyButton: TButton;
    GetWindowPositionButton: TButton;
    procedure ApplyButtonClick(Sender: TObject);
    procedure GetWindowPositionButtonClick(Sender: TObject);
  private
    fOnApplyWindowPos: TOnApplyWindowPos;
    fLayout: TIDEWindowLayout;
    fUpdateRadioButtons: boolean;
  protected
    function GetLayout: TIDEWindowLayout;
    procedure SetLayout(const AValue: TIDEWindowLayout);
    function GetPlacementRadioButtons(APlacement: TIDEWindowPlacement): TRadioButton;
    procedure SetPlacementRadioButtons(APlacement: TIDEWindowPlacement;
      const AValue: TRadioButton);
    procedure LoadFrom(ALayout: TIDEWindowLayout);
    procedure BoundsChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Save;
    procedure SaveTo(AnLayout: TIDEWindowLayout);
    property Layout: TIDEWindowLayout read GetLayout write SetLayout;
    property PlacementRadioButtons[APlacement: TIDEWindowPlacement]: TRadioButton
      read GetPlacementRadioButtons write SetPlacementRadioButtons;
    property OnApplyWindowPos: TOnApplyWindowPos
      read fOnApplyWindowPos write fOnApplyWindowPos;
  end;

  { TWindowOptionsFrame }

  TWindowOptionsFrame = class(TAbstractIDEOptionsEditor)
    HideMessagesIconsCheckBox: TCheckBox;
    WindowPositionsGroupBox: TGroupBox;
    HideIDEOnRunCheckBox: TCheckBox;
    MinimizeAllOnMinimizeMainCheckBox: TCheckBox;
    WindowPositionsListBox: TListBox;
    procedure WindowPositionsListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FLayouts: TIDEWindowLayoutList;
    WindowPositionsBox: TIDEWindowSetupLayoutComponent;
    procedure SetWindowPositionsItem(Index: integer);
    function GetCaptionFor(AWindow: TNonModalIDEWindow): String;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TWindowOptionsFrame }

function TWindowOptionsFrame.GetTitle: String;
begin
  Result := dlgWindow;
end;

procedure TWindowOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Window: TNonModalIDEWindow;
begin
  // windows
  MinimizeAllOnMinimizeMainCheckBox.Caption := dlgMinimizeAllOnMinimizeMain;
  HideIDEOnRunCheckBox.Caption := dlgHideIDEOnRun;
  HideMessagesIconsCheckBox.Caption := dlgHideMessagesIcons;

  // Window Positions
  WindowPositionsGroupBox.Caption := dlgWinPos;
  with WindowPositionsListBox.Items do
  begin
    BeginUpdate;
    for Window := Succ(Low(TNonModalIDEWindow)) to High(TNonModalIDEWindow) do
      Add(GetCaptionFor(Window));
    Add(dlgObjInsp);
    EndUpdate;
  end;
  WindowPositionsBox := TIDEWindowSetupLayoutComponent.Create(Self);
  with WindowPositionsBox do
  begin
    Name:='WindowPositionsBox';
    Parent:=WindowPositionsGroupBox;
    BorderSpacing.Around:=6;
    Align:=alBottom;
    AnchorToNeighbour(akTop,6,WindowPositionsListBox);
  end;
end;

procedure TWindowOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    FLayouts := IDEWindowLayoutList;
    SetWindowPositionsItem(0);

    // window minimizing and hiding
    MinimizeAllOnMinimizeMainCheckBox.Checked := MinimizeAllOnMinimizeMain;
    HideIDEOnRunCheckBox.Checked := HideIDEOnRun;
    HideMessagesIconsCheckBox.Checked := HideMessagesIcons;
  end;
end;

procedure TWindowOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    WindowPositionsBox.Save;
    // window minimizing
    MinimizeAllOnMinimizeMain:=MinimizeAllOnMinimizeMainCheckBox.Checked;
    HideIDEOnRun:=HideIDEOnRunCheckBox.Checked;
    HideMessagesIcons:=HideMessagesIconsCheckBox.Checked;
  end;
end;

procedure TWindowOptionsFrame.WindowPositionsListBoxSelectionChange(
  Sender: TObject; User: boolean);
begin
  if User then
    SetWindowPositionsItem(WindowPositionsListBox.ItemIndex);
end;

procedure TWindowOptionsFrame.SetWindowPositionsItem(Index: integer);
begin
  if WindowPositionsBox.Layout <> nil then
    WindowPositionsBox.Save;
  WindowPositionsListBox.ItemIndex := Index;
  if Index < Ord(High(TNonModalIDEWindow)) then
    WindowPositionsBox.Layout := FLayouts.ItemByEnum(TNonModalIDEWindow(Index + 1))
  else
  begin
    case Index - Ord(High(TNonModalIDEWindow)) of
      0: WindowPositionsBox.Layout := FLayouts.ItemByFormID(DefaultObjectInspectorName);
    end;
  end;
  if Index >= 0 then
    WindowPositionsBox.Caption := WindowPositionsListBox.Items[Index];
end;

function TWindowOptionsFrame.GetCaptionFor(AWindow: TNonModalIDEWindow): String;
begin
  case AWindow of
    nmiwMainIDEName: Result := dlgMainMenu;
    nmiwSourceNoteBookName: Result := dlgSrcEdit;
    nmiwMessagesViewName: Result := dlgMsgs;
    nmiwCodeExplorerName: Result := lisCodeExplorer;
    nmiwFPDocEditorName: Result := lisCodeHelpMainFormCaption;
    nmiwPkgGraphExplorer: Result := lisMenuPackageGraph;
    nmiwProjectInspector: Result := lisMenuProjectInspector;
    nmiwUnitDependenciesName: Result := dlgUnitDepCaption;
    nmiwDbgOutput: Result := lisMenuViewDebugOutput;
    nmiwBreakPoints: Result := lisMenuViewBreakPoints;
    nmiwWatches: Result := liswlWatchList;
    nmiwLocals: Result := lisLocals;
    nmiwCallStack: Result := lisMenuViewCallStack;
    nmiwEvaluate: Result := lisKMEvaluateModify;
    nmiwRegisters: Result := lisRegisters;
    nmiwAssembler: Result := lisMenuViewAssembler;
    nmiwSearchResultsViewName: Result := lisMenuViewSearchResults;
    nmiwAnchorEditor: Result := lisMenuViewAnchorEditor;
    nmiwCodeBrowser: Result := lisCodeBrowser;
    nmiwIssueBrowser: Result := lisMenuViewRestrictionBrowser;
    nmiwJumpHistory: Result := lisMenuViewJumpHistory;
  else
    Result := NonModalIDEWindowNames[AWindow];
  end;
end;

class function TWindowOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

{ TIDEWindowSetupLayoutComponent }

procedure TIDEWindowSetupLayoutComponent.BoundsChanged;
begin
  inherited BoundsChanged;
  LoadFrom(fLayout);
end;

procedure TIDEWindowSetupLayoutComponent.LoadFrom(ALayout: TIDEWindowLayout);
var
  APlacement: TIDEWindowPlacement;
  CurY: LongInt;
  Col2X: Integer;
  PreviousButton: TRadioButton;

  procedure SetLabelAndEdit(var ALabel: TLabel;
    var AnEdit: TEdit;  const ACaption: string; x, y: integer);
  begin
    if iwpCustomPosition in ALayout.WindowPlacementsAllowed then begin
      if ALabel=nil then ALabel:=TLabel.Create(Self);
      with ALabel do begin
        Parent:=Self;
        SetBounds(x,y,45,Height);
        Caption:=ACaption;
      end;
      if AnEdit=nil then AnEdit:=TEdit.Create(Self);
      with AnEdit do begin
        Parent:=Self;
        SetBounds(x+ALabel.Width+3,y,40,Height);
        Text:='';
      end;
    end else begin
      FreeAndNil(ALabel);
      FreeAndNil(AnEdit);
    end;
  end;

  function GetRadioBtnCaptions(aPos : TIDEWindowPlacement) : String;
  begin
    Result:='?';
    Case aPos of
      iwpUseWindowManagerSetting : Result:= rsiwpUseWindowManagerSetting;
      iwpDefault                 : Result:= rsiwpDefault;
      iwpRestoreWindowGeometry   : Result:= rsiwpRestoreWindowGeometry;
      iwpDocked                  : Result:= rsiwpDocked;
      iwpCustomPosition          : Result:= rsiwpCustomPosition;
      iwpRestoreWindowSize       : Result:= rsiwpRestoreWindowSize;
    end;
  end;

begin
  if ALayout=nil then exit;
  CurY:=5;
  Col2X:=300;
  PreviousButton:= nil;
  for APlacement:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    if APlacement in ALayout.WindowPlacementsAllowed then
    begin
      if PlacementRadioButtons[APlacement]=nil then
        PlacementRadioButtons[APlacement]:=TRadioButton.Create(Self);
      with PlacementRadioButtons[APlacement] do
      begin
        Parent:=Self;
        Left := 6;
        if PreviousButton=nil then
          Top := 6
        else
          AnchorToNeighbour(akTop,6,PreviousButton);
        Caption:=GetRadioBtnCaptions(APlacement);
        Checked:=(APlacement=ALayout.WindowPlacement);
      end;
      PreviousButton := PlacementRadioButtons[APlacement];

      case APlacement of
      iwpCustomPosition:
        begin
          // custom window position
          SetLabelAndEdit(LeftLabel,LeftEdit,dlgLeftPos,Col2X,CurY);
          SetLabelAndEdit(TopLabel,TopEdit,dlgTopPos,
            LeftEdit.Left+LeftEdit.Width+15,CurY);
          inc(CurY,LeftEdit.Height+6);
          SetLabelAndEdit(WidthLabel,WidthEdit,dlgWidthPos,LeftLabel.Left,CurY);
          SetLabelAndEdit(HeightLabel,HeightEdit,DlgHeightPos,
            WidthEdit.Left+WidthEdit.Width+15,CurY);
          inc(CurY,WidthEdit.Height+6);
          if ALayout.CustomCoordinatesAreValid then begin
            LeftEdit.Text:=IntToStr(ALayout.Left);
            TopEdit.Text:=IntToStr(ALayout.Top);
            WidthEdit.Text:=IntToStr(ALayout.Width);
            HeightEdit.Text:=IntToStr(ALayout.Height);
          end else if ALayout.Form<>nil then begin
            LeftEdit.Text:=IntToStr(ALayout.Form.Left);
            TopEdit.Text:=IntToStr(ALayout.Form.Top);
            WidthEdit.Text:=IntToStr(ALayout.Form.Width);
            HeightEdit.Text:=IntToStr(ALayout.Form.Height);
          end;
        end;
      end;

    end else begin
      // window placement not allowed
      if PlacementRadioButtons[APlacement]<>nil then
      begin
        PlacementRadioButtons[APlacement].Free;
        PlacementRadioButtons[APlacement]:=nil;
      end;
    end;
  end;

  if ApplyButton=nil then
    ApplyButton:=TButton.Create(Self);

  with ApplyButton do
  begin
    Parent:=Self;
    SetBounds(Col2X,CurY,Width,Height);
    OnClick:=@ApplyButtonClick;
    Caption:=dlgButApply;
    AutoSize:=true;
  end;

  if iwpCustomPosition in ALayout.WindowPlacementsAllowed then
  begin
    if GetWindowPositionButton=nil then
      GetWindowPositionButton:=TButton.Create(Self);
    with GetWindowPositionButton do
    begin
      Parent:=Self;
      OnClick:=@GetWindowPositionButtonClick;
      Caption:=dlgGetPosition;
      AutoSize:=true;
      AnchorToNeighbour(akLeft,6,ApplyButton);
      AnchorParallel(akTop,0,ApplyButton);
      Enabled := ALayout.Form <> nil;
    end;
  end;
  //inc(CurY,ApplyButton.Height+7);
end;

procedure TIDEWindowSetupLayoutComponent.SaveTo(AnLayout: TIDEWindowLayout);
var
  APlacement: TIDEWindowPlacement;
  ARadioButton: TRadioButton;
begin
  if AnLayout=nil then exit;
  if LeftEdit<>nil then
    AnLayout.Left:=StrToIntDef(LeftEdit.Text,0);
  if TopEdit<>nil then
    AnLayout.Top:=StrToIntDef(TopEdit.Text,0);
  if WidthEdit<>nil then
    AnLayout.Width:=StrToIntDef(WidthEdit.Text,0);
  if HeightEdit<>nil then
    AnLayout.Height:=StrToIntDef(HeightEdit.Text,0);
  for APlacement:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    ARadioButton:=GetPlacementRadioButtons(APlacement);
    if (ARadioButton<>nil) and ARadioButton.Checked then
      AnLayout.WindowPlacement:=APlacement;
  end;
end;

constructor TIDEWindowSetupLayoutComponent.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fUpdateRadioButtons:=false;
end;

function TIDEWindowSetupLayoutComponent.GetPlacementRadioButtons(
  APlacement: TIDEWindowPlacement): TRadioButton;
begin
  case APlacement of
  iwpRestoreWindowGeometry:   Result:=RestoreWindowGeometryRadioButton;
  iwpDefault:                 Result:=DefaultRadioButton;
  iwpCustomPosition:          Result:=CustomPositionRadioButton;
  iwpUseWindowManagerSetting: Result:=UseWindowManagerSettingRadioButton;
  iwpDocked:                  Result:=DockedRadioButton;
  else
    Result:=nil;
  end;
end;

procedure TIDEWindowSetupLayoutComponent.SetPlacementRadioButtons(
  APlacement: TIDEWindowPlacement; const AValue: TRadioButton);
begin
  case APlacement of
  iwpRestoreWindowGeometry:   RestoreWindowGeometryRadioButton:=AValue;
  iwpDefault:                 DefaultRadioButton:=AValue;
  iwpCustomPosition:          CustomPositionRadioButton:=AValue;
  iwpUseWindowManagerSetting: UseWindowManagerSettingRadioButton:=AValue;
  iwpDocked:                  DockedRadioButton:=AValue;
  end;
end;

procedure TIDEWindowSetupLayoutComponent.ApplyButtonClick(Sender: TObject);
begin
  Save;
  if Assigned(OnApplyWindowPos) then OnApplyWindowPos(Layout);
  Layout.Apply;
end;

procedure TIDEWindowSetupLayoutComponent.GetWindowPositionButtonClick(
  Sender: TObject);
begin
  if Layout.Form<>nil then begin
    if LeftEdit<>nil then
      LeftEdit.Text:=IntToStr(Layout.Form.Left);
    if TopEdit<>nil then
      TopEdit.Text:=IntToStr(Layout.Form.Top);
    if WidthEdit<>nil then
      WidthEdit.Text:=IntToStr(Layout.Form.Width);
    if HeightEdit<>nil then
      HeightEdit.Text:=IntToStr(Layout.Form.Height);
  end;
end;

function TIDEWindowSetupLayoutComponent.GetLayout: TIDEWindowLayout;
begin
  Result:=fLayout;
end;

procedure TIDEWindowSetupLayoutComponent.SetLayout(
  const AValue: TIDEWindowLayout);
begin
  fLayout:=AValue;
  LoadFrom(fLayout);
end;

procedure TIDEWindowSetupLayoutComponent.Save;
begin
  SaveTo(Layout);
end;

initialization
  {$I window_options.lrs}
  RegisterIDEOptionsEditor(GroupEnvironment, TWindowOptionsFrame, EnvOptionsWindow);
end.

