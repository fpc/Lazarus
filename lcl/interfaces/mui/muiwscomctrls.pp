{
 *****************************************************************************
 *                              MuiWSComCtrls.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.LCL, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit MuiWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  muicomctrls, muibaseunit, muistdctrls, tagsparamshelper,
  dos, exec, math,
  // LCL
  Classes,
  ComCtrls, Controls, LCLType, mui,
  // Widgetset
  WSComCtrls, WSLCLClasses;

type


  { TGtk2WSCustomTabControl }

  TMUIWSCustomTabControl = class(TWSCustomTabControl)
  private
  protected
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    //class function GetDefaultClientRect(const AWinControl: TWinControl; const {%H-}aLeft, {%H-}aTop, aWidth, aHeight: integer; var aClientRect: TRect): boolean; override;
    class procedure AddPage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const AIndex: integer); override;

    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;
    class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    class function GetCapabilities: TCTabControlCapabilities; override;
    class procedure GetPreferredSize(const {%H-}AWinControl: TWinControl; var {%H-}PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;

    (*class procedure MovePage(const ATabControl: TCustomTabControl;
      const AChild: TCustomPage; const NewIndex: integer); override;

    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;

    class procedure UpdateProperties(const ATabControl: TCustomTabControl); override;*)
  end;



  { TmuiWSCustomPage }

  TmuiWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TmuiWSCustomNotebook }

  TmuiWSCustomNotebook = class(TWSCustomTabControl)
  private
  protected
  published
    //class function  CreateHandle(const AWinControl: TWinControl;
    //      const AParams: TCreateParams): HWND; override;
    //class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

  { TmuiWSStatusBar }

  TmuiWSStatusBar = class(TWSStatusBar)
  private
  protected
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    class procedure GetPreferredSize(const {%H-}AWinControl: TWinControl;
                        var {%H-}PreferredWidth, PreferredHeight: integer;
                        {%H-}WithThemeSpace: Boolean); override;

    class procedure SetSizeGrip(const AStatusBar: TStatusBar; {%H-}SizeGrip: Boolean); override;
  end;

  { TmuiWSTabSheet }

  TmuiWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TmuiWSPageControl }

  TmuiWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TmuiWSCustomListView }

  TmuiWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;
    //
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
  end;

  { TmuiWSListView }

  TmuiWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TmuiWSProgressBar }

  TmuiWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
//    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TmuiWSCustomUpDown }

  TmuiWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TmuiWSUpDown }

  TmuiWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TmuiWSToolButton }

  TmuiWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TmuiWSToolBar }

  TmuiWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TmuiWSTrackBar }

  TMUIWSTrackBar = class(TWSTrackBar)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer); override;
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const {%H-}AOrientation: TTrackBarOrientation); override;
  end;

  { TmuiWSCustomTreeView }

  TmuiWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TmuiWSTreeView }

  TmuiWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

class function TMUIWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  MUIRegister: TMUIRegister;
  TagList: TATagList;
begin
  //writeln('Create Tabcontrol');
  TagList.Clear;
  MUIRegister := TMUIRegister.Create(MUIC_Group, TagList);
  With MUIRegister do
  begin
    ShowTabs := TCustomTabControl(AWinControl).ShowTabs;
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := PChar(AParams.Caption);
    Color := AWinControl.Color;
  end;
  if AWinControl.Parent <> NIL then
  begin
    MUIRegister.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := HWND(MUIRegister);
end;

class procedure TMUIWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  MUIRegister: TMUIRegister;
  MUIObj: TMUIObject;
begin
  //Writeln('Add now page ', AChild.Caption, ' ', HexStr(ATabControl));
  MUIRegister := TMUIRegister(ATabControl.Handle);
  MUIObj := TMUIObject(AChild.Handle);
  MUIObj.Parent := MUIRegister;
  //AChild.Parent := ATabControl;
end;

class procedure TMUIWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  MUIRegister: TMUIRegister;
begin
  //writeln('Set Pageidx to ', AIndex);
  MUIRegister := TMUIRegister(ATabControl.Handle);
  if Assigned(MUIRegister) then
  begin
    MuiRegister.ActivePage := AIndex;
  end;
end;

class procedure TMUIWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
begin
  //RecreateWnd(ATabControl);
end;

class function TMUIWSCustomTabControl.GetNotebookMinTabHeight(const AWinControl: TWinControl): integer;
begin
  Result := 30;
end;

class function TMUIWSCustomTabControl.GetNotebookMinTabWidth(const AWinControl: TWinControl): integer;
begin
  Result := 50;
end;

class function TMUIWSCustomTabControl.GetCapabilities: TCTabControlCapabilities;
begin
  Result := [];
end;

class procedure TMUIWSCustomTabControl.GetPreferredSize(const {%H-}AWinControl: TWinControl; var {%H-}PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean);
var
  MUIRegister: TMUIRegister;
begin
  MUIRegister := TMUIRegister(AWinControl.Handle);
  if Assigned(MUIRegister) then
  begin
    PreferredHeight := MUIRegister.RegisterHeight;
    PreferredWidth := MUIRegister.NumPages * 50;
  end;
end;

{ TmuiWSStatusBar }

class function  TmuiWSStatusBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  MUIText: TMUIText;
  TagList: TATagList;
begin
  TagList.AddTags([MUIA_Frame, MUIV_Frame_Text]);
  MUIText := TMUIText.Create(TagList);
  with MUIText do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := TStatusBar(AWinControl).SimpleText;
  end;
  if AWinControl.Parent <> NIL then
  begin
    MUIText.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := TLCLIntfHandle(MUIText);
end;

class procedure TmuiWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer);
begin
  //
end;

class procedure TmuiWSStatusBar.SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer);
var
  Area: TMUIText;
begin
  Area := TMUIText(AStatusBar.Handle);
  if Assigned(Area) then
  begin
    Area.Caption := AStatusbar.SimpleText;
  end;
end;

class procedure TmuiWSStatusBar.Update(const AStatusBar: TStatusBar);
begin

end;

class procedure TmuiWSStatusBar.GetPreferredSize(const {%H-}AWinControl: TWinControl;
                    var {%H-}PreferredWidth, PreferredHeight: integer;
                    {%H-}WithThemeSpace: Boolean);
begin
  PreferredHeight := 22;
  PreferredWidth := 100;
end;

class procedure TmuiWSStatusBar.SetSizeGrip(const AStatusBar: TStatusBar; {%H-}SizeGrip: Boolean);
begin

end;


{ TmuiWSCustomListView }

class function TmuiWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
var
  ListView: TMUIArea;
  TagList: TATagList;
begin
  TagList.AddTags([MUIA_Frame, MUIV_Frame_String]);
  ListView := TMUIArea.Create(MUIC_Area, TagList);
  With ListView do
  begin
    Left := AParams.X;
    Top := AParams.Y;
    Width := AParams.Width;
    Height := AParams.Height;
    PasObject := AWinControl;
    Caption := PChar(AParams.Caption);
  end;

  if AWinControl.Parent <> NIL then
  begin
    ListView.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := TLCLIntfHandle(ListView);
end;

class procedure TmuiWSCustomListView.ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String);
begin
  //writeln('SetText(',AIndex,',',ASubIndex,') = ', AText);
end;

{ TmuiWSProgressBar }

class function TmuiWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  TagList: TATagList;
  Gauge: TMUIGauge;
  MUIBusy: TMUIBusy;
  Horiz: Boolean;
  AsBusy: Boolean;
begin
  AsBusy := False;
  if AWinControl is TProgressBar then
  begin
    AsBusy := TProgressBar(AWinControl).Style = pbstMarquee;
    Horiz := (TProgressBar(AWinControl).Orientation = pbHorizontal) or (TProgressBar(AWinControl).Orientation = pbRightToLeft);
    if not AsBusy then
    begin
      TagList.AddTags([MUIA_Gauge_Horiz, IfThen(Horiz, TagTrue, TagFalse)]);
    end else
    begin
      TagList.AddTags([MUIA_Group_Horiz, IfThen(Horiz, TagTrue, TagFalse)]);
    end;
  end;
  if AsBusy then
  begin
    MUIBusy := TMUIBusy.Create('Busy.mcc', TagList);
    //MUIBusy.Horiz := Horiz;
    With MUIBusy do
    begin
      Left := AParams.X;
      Top := AParams.Y;
      Width := AParams.Width;
      Height := AParams.Height;
      PasObject := AWinControl;
    end;
    if AWinControl.Parent <> NIL then
    begin
      MUIBusy.Parent := TMuiObject(AWinControl.Parent.Handle);
    end;
    Result := TLCLIntfHandle(MUIBusy);
  end else
  begin
    TagList.AddTags([
      MUIA_Frame, MUIV_Frame_Gauge,
      MUIA_Font, NativeUInt(MUIV_Font_List),
      MUIA_Gauge_InfoText, NativeUInt(PChar(''))
      ]);
    Gauge := TMUIGauge.Create(MUIC_Gauge, TagList);
    Gauge.Horiz := Horiz;
    With Gauge do
    begin
      Left := AParams.X;
      Top := AParams.Y;
      Width := AParams.Width;
      Height := AParams.Height;
      PasObject := AWinControl;
      Caption := PChar(AParams.Caption);
    end;
    if AWinControl.Parent <> NIL then
    begin
      Gauge.Parent := TMuiObject(AWinControl.Parent.Handle);
    end;
    Result := TLCLIntfHandle(Gauge);
  end;

end;

class procedure TmuiWSProgressBar.DestroyHandle(const AWinControl: TWinControl);
begin
  TMUIGauge(AWinControl.Handle).Free;
  AWinControl.Handle := 0;
end;

class procedure TmuiWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  Gauge: TMUIGauge;
begin
  if TMUIObject(AProgressBar.Handle) is TMUIGauge then
  begin
    if TProgressBar(AProgressBar).Style = pbstMarquee then
    begin
      RecreateWnd(AProgressBar);
      Exit;
    end;
    Gauge := TMUIGauge(AProgressBar.Handle);
    Gauge.MinPos := AProgressBar.Min;
    Gauge.MaxPos := AProgressBar.Max;
    Gauge.ShowText := AProgressBar.BarShowText;
    SetPosition(AProgressBar, AProgressBar.Position);
  end else
  begin
    if TProgressBar(AProgressBar).Style <> pbstMarquee then
    begin
      RecreateWnd(AProgressBar);
      Exit;
    end;
  end;
end;

class procedure TmuiWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if TMUIObject(AProgressBar.Handle) is TMUIGauge then
  begin
    TMUIGauge(AProgressBar.Handle).Position := NewPosition;
  end;
end;

class function TMUIWSTrackBar.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
var
  MUISlider: TMUISlider;
  TagList: TATagList;
  ATrackBar: TCustomTrackBar absolute AWinControl;
  Horiz: Boolean;
begin
  Horiz := ATrackBar.Orientation = trHorizontal;
  TagList.AddTags([MUIA_Slider_Horiz, ifthen(Horiz, TagTrue, TagFalse)]);
  MUISlider := TMUISlider.Create(MUIC_Slider, TagList);
  with MUISlider do
  begin
    Horizontal := Horiz;
    Left := AParams.X;
    Top := AParams.Y;
    if Horiz  then
    begin
      Width := AParams.Width;
      Height := 22;
    end else
    begin
      Width := 22;
      Height := AParams.Height;
    end;
    PasObject := AWinControl;
    MinValue := ATrackBar.Min;
    MaxValue := ATrackBar.Max;
    Value := ATrackBar.Position;
  end;
  if AWinControl.Parent <> NIL then
  begin
    MUISlider.Parent := TMuiObject(AWinControl.Parent.Handle);
  end;
  Result := TLCLIntfHandle(MUISlider);
end;

class procedure TMUIWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  MUISlider: TMUISlider;
begin
  MUISlider := TMUISlider(ATrackBar.Handle);
  if Assigned(MUISlider) then
  begin
    with MUISlider do
    begin
      MinValue := ATrackBar.Min;
      MaxValue := ATrackBar.Max;
      Value := ATrackBar.Position;
    end;
  end;
end;

class function TMUIWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar): integer;
var
  MUISlider: TMUISlider;
begin
  Result := 0;
  MUISlider := TMUISlider(ATrackBar.Handle);
  if Assigned(MUISlider) then
    Result := MUISlider.Value;
end;

class procedure TMUIWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar; const NewPosition: integer);
var
  MUISlider: TMUISlider;
begin
  MUISlider := TMUISlider(ATrackBar.Handle);
  if Assigned(MUISlider) then
    MUISlider.Value := NewPosition;
end;

class procedure TMUIWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar; const {%H-}AOrientation: TTrackBarOrientation);
var
  MUISlider: TMUISlider;
  h: Boolean;
begin
  MUISlider := TMUISlider(ATrackBar.Handle);
  h := AOrientation = trHorizontal;
  if Assigned(MUISlider) then
  begin
    if MUISlider.Horizontal <> h then
      RecreateWnd(ATrackBar);
  end;
end;


end.

