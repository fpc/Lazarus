{  $Id$  }
{
 /***************************************************************************
                          ideoptionsdefs.pp  -  Toolbar
                          -----------------------------


 ***************************************************************************/

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
unit IDEOptionDefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_XMLCfg, Forms, Controls, StdCtrls, Buttons;

const
  // form names for non modal IDE windows:
  DefaultMainIDEName = 'MainIDE';
  DefaultSourceNoteBookName = 'SourceNotebook';
  DefaultMessagesViewName = 'MessagesView';
  DefaultUnitDependenciesName = 'UnitDependencies';

type
  { TIDEWindowLayout stores information about the position, min/maximized state
    and similar things for an IDE window or dialog, like the source editor,
    the object inspector, the main bar or the message view.
  }
  TIDEWindowPlacement = (
    iwpUseWindowManagerSetting, // leave window position, where window manager
                                //   creates the window
    iwpDefault,                 // set window to the default position
    iwpRestoreWindowGeometry,   // save window geometry at end and restore it
                                //   at start
    iwpDocked,                  // dock into other IDE window
    iwpCustomPosition,          // set window to custom position
    iwpRestoreWindowSize        // save window size at end and restore it
                                //   at start
    );
  TIDEWindowPlacements = set of TIDEWindowPlacement;
  TIDEWindowDockMode = (iwdmDefault, iwdmLeft, iwdmRight, iwdmTop, iwdmBottom);
  TIDEWindowDockModes = set of TIDEWindowDockMode;
  TIDEWindowState = (iwsNormal, iwsMaximized, iwsMinimized, iwsHidden);
  TIDEWindowStates = set of TIDEWindowState;
  
  TIDEWindowLayout = class;
  TOnGetDefaultIDEWindowPos = procedure(Sender: TIDEWindowLayout;
                                        var Bounds: TRect) of object;
  TOnApplyIDEWindowLayout = procedure(Layout: TIDEWindowLayout) of object;
                                        
  TIDEWindowLayout = class
  private
    fWindowPlacement: TIDEWindowPlacement;
    fWindowPlacementsAllowed: TIDEWindowPlacements;
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    fWindowState: TIDEWindowState;
    fWindowStatesAllowed: TIDEWindowStates;
    fForm: TForm;
    fDockParent: string;
    fDockChilds: TStringList;
    fDockMode: TIDEWindowDockMode;
    fDockModesAllowed: TIDEWindowDockModes;
    fFormID: string;
    fOnGetDefaultIDEWindowPos: TOnGetDefaultIDEWindowPos;
    fOnApply: TOnApplyIDEWindowLayout;
    fDefaultWindowPlacement: TIDEWindowPlacement;
    function GetFormID: string;
    function GetXMLFormID: string;
    procedure SetFormID(const AValue: string);
    procedure SetOnGetDefaultIDEWindowPos(const AValue: TOnGetDefaultIDEWindowPos);
    procedure SetDockModesAllowed(const AValue: TIDEWindowDockModes);
    procedure SetWindowPlacementsAllowed(const AValue: TIDEWindowPlacements);
    procedure SetWindowStatesAllowed(const AValue: TIDEWindowStates);
    procedure SetDockMode(const AValue: TIDEWindowDockMode);
    procedure SetDockParent(const AValue: string);
    procedure SetForm(const AValue: TForm);
    procedure SetWindowState(const AValue: TIDEWindowState);
    procedure SetLeft(const AValue: integer);
    procedure SetTop(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    procedure SetHeight(const AValue: integer);
    procedure SetWindowPlacement(const AValue: TIDEWindowPlacement);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Apply;
    procedure GetCurrentPosition;
    procedure Assign(Layout: TIDEWindowLayout);
    procedure ReadCurrentCoordinates;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function CustomCoordinatesAreValid: boolean;
    property FormID: string read GetFormID write SetFormID;
    property WindowPlacement: TIDEWindowPlacement
      read fWindowPlacement write SetWindowPlacement;
    property WindowPlacementsAllowed: TIDEWindowPlacements
      read fWindowPlacementsAllowed write SetWindowPlacementsAllowed;
    property DefaultWindowPlacement: TIDEWindowPlacement
      read fDefaultWindowPlacement write fDefaultWindowPlacement;
    property Left: integer read fLeft write SetLeft;
    property Top: integer read fTop write SetTop;
    property Width: integer read fWidth write SetWidth;
    property Height: integer read fHeight write SetHeight;
    property WindowState: TIDEWindowState
      read fWindowState write SetWindowState;
    property WindowStatesAllowed: TIDEWindowStates
      read fWindowStatesAllowed write SetWindowStatesAllowed;
    property Form: TForm read fForm write SetForm;
    property DockParent: string
      read fDockParent write SetDockParent; // for format see GetFormId
    property DockMode: TIDEWindowDockMode read fDockMode write SetDockMode;
    property DockModesAllowed: TIDEWindowDockModes
      read fDockModesAllowed write SetDockModesAllowed;
    property DockChilds: TStringList read fDockChilds; // list of FormIDs
    property OnGetDefaultIDEWindowPos: TOnGetDefaultIDEWindowPos
      read fOnGetDefaultIDEWindowPos write SetOnGetDefaultIDEWindowPos;
    property OnApply: TOnApplyIDEWindowLayout read fOnApply write fOnApply;
  end;
  
  TIDEWindowLayoutList = class(TList)
  private
    function GetItems(Index: Integer): TIDEWindowLayout;
    procedure SetItems(Index: Integer; const AValue: TIDEWindowLayout);
  public
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure ApplyAll;
    procedure Apply(AForm: TForm; const ID: string);
    procedure StoreWindowPositions;
    procedure Assign(SrcList: TIDEWindowLayoutList);
    function IndexOf(const FormID: string): integer;
    function ItemByForm(AForm: TForm): TIDEWindowLayout;
    function ItemByFormID(const FormID: string): TIDEWindowLayout;
    property Items[Index: Integer]: TIDEWindowLayout
      read GetItems write SetItems; default;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  
  // ---------------------------------------------------------------------------
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
    procedure RadioButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure GetWindowPositionButtonClick(Sender: TObject);
  private
    fOnApplyWindowPos: TOnApplyWindowPos;
    fLayout: TIDEWindowLayout;
    fUpdateRadioButtons: boolean;
  protected
    function GetLayout: TIDEWindowLayout;
    procedure SetLayout(const AValue: TIDEWindowLayout);
    function GetPlacementRadioButtons(APlacement: TIDEWindowPlacement
       ): TRadioButton;
    procedure SetPlacementRadioButtons(APlacement: TIDEWindowPlacement;
      const AValue: TRadioButton);
    procedure LoadFrom(AnLayout: TIDEWindowLayout);
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


const
  IDEWindowDockModeNames: array[TIDEWindowDockMode] of string = (
      'Default', 'Left', 'Right', 'Top', 'Bottom'
    );
  IDEWindowPlacementNames: array[TIDEWindowPlacement] of string = (
      'UseWindowManagerSetting',
      'Default',
      'RestoreWindowGeometry',
      'Docked',
      'CustomPosition',
      'RestoreWindowSize'
    );
  IDEWindowStateNames: array[TIDEWindowState] of string = (
      'Normal', 'Maximized', 'Minimized', 'Hidden'
    );

function StrToIDEWindowDockMode(const s: string): TIDEWindowDockMode;
function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
function StrToIDEWindowState(const s: string): TIDEWindowState;

  //----------------------------------------------------------------------------
type
  TIDEDialogLayout = class
  private
    FHeight: integer;
    FName: string;
    FWidth: integer;
    procedure SetHeight(const AValue: integer);
    procedure SetName(const AValue: string);
    procedure SetWidth(const AValue: integer);
  public
    function SizeValid: boolean;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Name: string read FName write SetName;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  
  TIDEDialogLayoutList = class
  private
    FItems: TList;
    function GetItems(Index: integer): TIDEDialogLayout;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyLayout(ADialog: TControl;
      DefaultWidth, DefaultHeight: integer);
    procedure SaveLayout(ADialog: TControl);
    procedure Clear;
    function Count: integer;
    function Find(const DialogName: string;
      CreateIfNotExists: boolean): TIDEDialogLayout;
    function Find(ADialog: TObject;
      CreateIfNotExists: boolean): TIDEDialogLayout;
    function IndexOf(const DialogName: string): integer;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    property Items[Index: integer]: TIDEDialogLayout read GetItems;
  end;

const
  IDEDialogLayoutList: TIDEDialogLayoutList = nil;


implementation


function StrToIDEWindowDockMode(const s: string): TIDEWindowDockMode;
begin
  for Result:=Low(TIDEWindowDockMode) to High(TIDEWindowDockMode) do
    if AnsiCompareText(s,IDEWindowDockModeNames[Result])=0 then exit;
  Result:=iwdmDefault;
end;

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
begin
  for Result:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
    if AnsiCompareText(s,IDEWindowPlacementNames[Result])=0 then exit;
  Result:=iwpDefault;
end;

function StrToIDEWindowState(const s: string): TIDEWindowState;
begin
  for Result:=Low(TIDEWindowState) to High(TIDEWindowState) do
    if AnsiCompareText(s,IDEWindowStateNames[Result])=0 then exit;
  Result:=iwsNormal;
end;

{ TIDEWindowLayout }

constructor TIDEWindowLayout.Create;
begin
  inherited Create;
  fDockChilds:=TStringList.Create;
  fDefaultWindowPlacement:=iwpDefault;
  Clear;
  fWindowPlacementsAllowed:=
    [Low(TIDEWindowPlacement)..High(TIDEWindowPlacement)];
  fWindowStatesAllowed:=[Low(TIDEWindowState)..High(TIDEWindowState)];
  fDockModesAllowed:=[Low(TIDEWindowDockMode)..High(TIDEWindowDockMode)];
end;

procedure TIDEWindowLayout.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  P, DockChild: string;
  DockChildCount, i: integer;
begin
  // set all values to default
  Clear;
  // read settings
  // build path
  P:=GetXMLFormID;
  if P='' then exit;
  P:=Path+P+'/';
  // placement
  fWindowPlacement:=StrToIDEWindowPlacement(XMLConfig.GetValue(
    P+'WindowPlacement/Value',IDEWindowPlacementNames[fWindowPlacement]));
  // custom position
  fLeft:=XMLConfig.GetValue(P+'CustomPosition/Left',fLeft);
  fTop:=XMLConfig.GetValue(P+'CustomPosition/Top',fTop);
  fWidth:=XMLConfig.GetValue(P+'CustomPosition/Width',fWidth);
  fHeight:=XMLConfig.GetValue(P+'CustomPosition/Height',fHeight);
  // state
  fWindowState:=StrToIDEWindowState(XMLConfig.GetValue(
    P+'WindowState/Value',IDEWindowStateNames[fWindowState]));
  // docking
  fDockParent:=XMLConfig.GetValue(P+'Docking/Parent','');
  DockChildCount:=XMLConfig.GetValue(P+'Docking/ChildCount',0);
  for i:=0 to DockChildCount-1 do begin
    DockChild:=XMLConfig.GetValue(P+'Docking/Child'+IntToStr(i),'');
    if DockChild<>'' then begin
      fDockChilds.Add(DockChild);
    end;
  end;
  fDockMode:=StrToIDEWindowDockMode(XMLConfig.GetValue(
    P+'DockMode/Value',IDEWindowDockModeNames[fDockMode]));
end;

procedure TIDEWindowLayout.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  P: string;
  i: integer;
begin
  // build path
  P:=GetXMLFormID;
  if P='' then exit;
  P:=Path+P+'/';
  // placement
  XMLConfig.SetValue(P+'WindowPlacement/Value',
    IDEWindowPlacementNames[fWindowPlacement]);
  // custom position
  XMLConfig.SetValue(P+'CustomPosition/Left',fLeft);
  XMLConfig.SetValue(P+'CustomPosition/Top',fTop);
  XMLConfig.SetValue(P+'CustomPosition/Width',fWidth);
  XMLConfig.SetValue(P+'CustomPosition/Height',fHeight);
  // state
  XMLConfig.SetValue(P+'WindowState/Value',IDEWindowStateNames[fWindowState]);
  // docking
  XMLConfig.SetValue(P+'Docking/Parent',fDockParent);
  XMLConfig.SetValue(P+'Docking/ChildCount',fDockChilds.Count);
  for i:=0 to fDockChilds.Count-1 do begin
    XMLConfig.SetValue(P+'Docking/Child'+IntToStr(i),fDockChilds[i]);
  end;
  XMLConfig.SetValue(P+'DockMode/Value',IDEWindowDockModeNames[fDockMode]);
end;

procedure TIDEWindowLayout.SetWindowPlacement(
  const AValue: TIDEWindowPlacement);
begin
  fWindowPlacement:=AValue;
end;

procedure TIDEWindowLayout.SetHeight(const AValue: integer);
begin
  fHeight:=AValue;
end;

procedure TIDEWindowLayout.SetLeft(const AValue: integer);
begin
  fLeft:=AValue;
end;

procedure TIDEWindowLayout.SetTop(const AValue: integer);
begin
  fTop:=AValue;
end;

procedure TIDEWindowLayout.SetWidth(const AValue: integer);
begin
  fWidth:=AValue;
end;

procedure TIDEWindowLayout.SetWindowState(const AValue: TIDEWindowState);
begin
  fWindowState:=AValue;
end;

function TIDEWindowLayout.CustomCoordinatesAreValid: boolean;
begin
  Result:=(Width>0) and (Height>0) and (Left>10-Width) and (Top>10-Height);
end;

procedure TIDEWindowLayout.SetForm(const AValue: TForm);
begin
  if fForm=AValue then exit;
  fForm:=AValue;
end;

function TIDEWindowLayout.GetFormID: string;
begin
  if FForm=nil then
    Result:=fFormID
  else
    Result:=FForm.Name;
end;

function TIDEWindowLayout.GetXMLFormID: string;
var
  i: integer;
begin
  Result:=GetFormID;
  for i:=1 to length(Result) do
    if not (Result[i] in ['A'..'Z','a'..'z','_']) then
      Result[i]:='_';
end;

procedure TIDEWindowLayout.SetDockParent(const AValue: string);
begin
  fDockParent:=AValue;
end;

destructor TIDEWindowLayout.Destroy;
begin
  fDockChilds.Free;
  inherited Destroy;
end;

procedure TIDEWindowLayout.SetDockMode(const AValue: TIDEWindowDockMode);
begin
  fDockMode:=AValue;
end;

procedure TIDEWindowLayout.SetWindowStatesAllowed(
  const AValue: TIDEWindowStates);
begin
  fWindowStatesAllowed:=AValue;
end;

procedure TIDEWindowLayout.SetWindowPlacementsAllowed(
  const AValue: TIDEWindowPlacements);
begin
  fWindowPlacementsAllowed:=AValue;
end;

procedure TIDEWindowLayout.SetDockModesAllowed(
  const AValue: TIDEWindowDockModes);
begin
  fDockModesAllowed:=AValue;
end;

procedure TIDEWindowLayout.SetOnGetDefaultIDEWindowPos(
  const AValue: TOnGetDefaultIDEWindowPos);
begin
  fOnGetDefaultIDEWindowPos:=AValue;
end;

procedure TIDEWindowLayout.Clear;
begin
  fWindowPlacement:=fDefaultWindowPlacement;
  fLeft:=0;
  fTop:=0;
  fWidth:=0;
  fHeight:=0;
  fWindowState:=iwsNormal;
  fDockParent:='';
  fDockChilds.Clear;
  fDockMode:=iwdmDefault;
end;

procedure TIDEWindowLayout.SetFormID(const AValue: string);
begin
  if Form=nil then
    fFormID:=AValue;
end;

procedure TIDEWindowLayout.Apply;
begin
  if Assigned(OnApply) then OnApply(Self);
end;

procedure TIDEWindowLayout.ReadCurrentCoordinates;
begin
  if Form<>nil then begin
    Left:=Form.Left;
    Top:=Form.Top;
    Width:=Form.Width;
    Height:=Form.Height;
  end else begin
    Left:=0;
    Top:=0;
    Width:=0;
    Height:=0;
  end;
end;

procedure TIDEWindowLayout.Assign(Layout: TIDEWindowLayout);
begin
  Clear;
  fWindowPlacement:=Layout.fWindowPlacement;
  fWindowPlacementsAllowed:=Layout.fWindowPlacementsAllowed;
  fLeft:=Layout.fLeft;
  fTop:=Layout.fTop;
  fWidth:=Layout.fWidth;
  fHeight:=Layout.fHeight;
  fWindowState:=Layout.fWindowState;
  fWindowStatesAllowed:=Layout.fWindowStatesAllowed;
  fForm:=Layout.fForm;
  fDockParent:=Layout.fDockParent;
  fDockChilds.Assign(Layout.fDockChilds);
  fDockMode:=Layout.fDockMode;
  fDockModesAllowed:=Layout.fDockModesAllowed;
  fFormID:=Layout.fFormID;
  fOnGetDefaultIDEWindowPos:=Layout.fOnGetDefaultIDEWindowPos;
  fOnApply:=Layout.fOnApply;
  fDefaultWindowPlacement:=Layout.fDefaultWindowPlacement;
end;

procedure TIDEWindowLayout.GetCurrentPosition;
begin
  case WindowPlacement of
  iwpRestoreWindowGeometry:
    ReadCurrentCoordinates;
    
  iwpRestoreWindowSize:
    ReadCurrentCoordinates;
    
  end;
end;

{ TIDEWindowLayoutList }

procedure TIDEWindowLayoutList.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Clear;
end;

procedure TIDEWindowLayoutList.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

function TIDEWindowLayoutList.GetItems(Index: Integer): TIDEWindowLayout;
begin
  Result:=TIDEWindowLayout(inherited Items[Index]);
end;

procedure TIDEWindowLayoutList.SetItems(Index: Integer;
  const AValue: TIDEWindowLayout);
begin
  Items[Index]:=AValue;
end;

function TIDEWindowLayoutList.IndexOf(const FormID: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FormID<>Items[Result].GetFormID) do dec(Result);
end;

procedure TIDEWindowLayoutList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].LoadFromXMLConfig(XMLConfig,Path);
end;

procedure TIDEWindowLayoutList.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].SaveToXMLConfig(XMLConfig,Path);
end;

function TIDEWindowLayoutList.ItemByForm(AForm: TForm): TIDEWindowLayout;
var i: integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    Result:=Items[i];
    if Result.Form=AForm then exit;
    dec(i);
  end;
  Result:=nil;
end;

function TIDEWindowLayoutList.ItemByFormID(const FormID: string
  ): TIDEWindowLayout;
var i: integer;
begin
  i:=IndexOf(FormID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TIDEWindowLayoutList.ApplyAll;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].Apply;
end;

procedure TIDEWindowLayoutList.Apply(AForm: TForm; const ID: string);
var ALayout: TIDEWindowLayout;
begin
  ALayout:=ItemByFormID(ID);
  ALayout.Form:=AForm;
  ALayout.Apply;
end;

procedure TIDEWindowLayoutList.StoreWindowPositions;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].GetCurrentPosition;
end;

procedure TIDEWindowLayoutList.Assign(SrcList: TIDEWindowLayoutList);
var i: integer;
  NewLayout: TIDEWindowLayout;
begin
  Clear;
  if SrcList=nil then exit;
  for i:=0 to SrcList.Count-1 do begin
    NewLayout:=TIDEWindowLayout.Create;
    NewLayout.Assign(SrcList[i]);
    Add(NewLayout);
  end;
end;

{ TIDEWindowSetupLayoutComponent }


procedure TIDEWindowSetupLayoutComponent.BoundsChanged;
begin
  inherited BoundsChanged;
  LoadFrom(fLayout);
end;

procedure TIDEWindowSetupLayoutComponent.LoadFrom(AnLayout: TIDEWindowLayout);
var
  CurY: integer;
  APlacement: TIDEWindowPlacement;
  
  procedure SetLabelAndEdit(var ALabel: TLabel;
    var AnEdit: TEdit;  const ACaption: string; x, y: integer);
  begin
    if iwpCustomPosition in AnLayout.WindowPlacementsAllowed then begin
      if ALabel=nil then ALabel:=TLabel.Create(Self);
      with ALabel do begin
        Parent:=Self;
        SetBounds(x,y,45,Height);
        Caption:=ACaption;
        Visible:=true;
      end;
      if AnEdit=nil then AnEdit:=TEdit.Create(Self);
      with AnEdit do begin
        Parent:=Self;
        SetBounds(x+ALabel.Width+3,y,40,Height);
        Text:='';
        Visible:=true;
      end;
    end else begin
      FreeAndNil(ALabel);
      FreeAndNil(AnEdit);
    end;
  end;

const
  RadioBtnCaptions: array[TIDEWindowPlacement] of string = (
      'Use windowmanager setting',
      'Default',
      'Restore window geometry',
      'Docked',
      'Custom position',
      'Restore window size'
    );
begin
  if AnLayout=nil then exit;
  CurY:=5;
  for APlacement:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do begin
    if APlacement in AnLayout.WindowPlacementsAllowed then begin
      if PlacementRadioButtons[APlacement]=nil then
        PlacementRadioButtons[APlacement]:=TRadioButton.Create(Self);
      with PlacementRadioButtons[APlacement] do begin
        Parent:=Self;
        SetBounds(5,CurY,Self.ClientWidth-Left,Height);
        inc(CurY,Height+2);
        OnClick:=@RadioButtonClick;
        Caption:=RadioBtnCaptions[APlacement];
        Checked:=(APlacement=AnLayout.WindowPlacement);
        Visible:=true;
      end;
      case APlacement of
      iwpCustomPosition:
        begin
          // custom window position
          SetLabelAndEdit(LeftLabel,LeftEdit,'Left:',15,CurY);
          SetLabelAndEdit(TopLabel,TopEdit,'Top:',
            LeftEdit.Left+LeftEdit.Width+15,CurY);
          inc(CurY,LeftEdit.Height+3);
          SetLabelAndEdit(WidthLabel,WidthEdit,'Width:',15,CurY);
          SetLabelAndEdit(HeightLabel,HeightEdit,'Height:',
            WidthEdit.Left+WidthEdit.Width+15,CurY);
          inc(CurY,WidthEdit.Height+3);
          if AnLayout.CustomCoordinatesAreValid then begin
            LeftEdit.Text:=IntToStr(AnLayout.Left);
            TopEdit.Text:=IntToStr(AnLayout.Top);
            WidthEdit.Text:=IntToStr(AnLayout.Width);
            HeightEdit.Text:=IntToStr(AnLayout.Height);
          end else if AnLayout.Form<>nil then begin
            LeftEdit.Text:=IntToStr(AnLayout.Form.Left);
            TopEdit.Text:=IntToStr(AnLayout.Form.Top);
            WidthEdit.Text:=IntToStr(AnLayout.Form.Width);
            HeightEdit.Text:=IntToStr(AnLayout.Form.Height);
          end;
        end;
      end;
    end else begin
      if PlacementRadioButtons[APlacement]<>nil then begin
        PlacementRadioButtons[APlacement].Free;
        PlacementRadioButtons[APlacement]:=nil;
      end;
    end;
  end;
  inc(CurY,2);
  if ApplyButton=nil then ApplyButton:=TButton.Create(Self);
  with ApplyButton do begin
    Parent:=Self;
    SetBounds(5,CurY,70,Height);
    OnClick:=@ApplyButtonClick;
    Caption:='Apply';
    Visible:=true;
  end;
  if iwpCustomPosition in AnLayout.WindowPlacementsAllowed then begin
    if GetWindowPositionButton=nil then
      GetWindowPositionButton:=TButton.Create(Self);
    with GetWindowPositionButton do begin
      Parent:=Self;
      SetBounds(85,CurY,110,Height);
      OnClick:=@GetWindowPositionButtonClick;
      Caption:='Get position';
      Visible:=true;
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

procedure TIDEWindowSetupLayoutComponent.RadioButtonClick(Sender: TObject);
var APlacement: TIDEWindowPlacement;
  ARadioButton: TRadioButton;
begin
  if fUpdateRadioButtons then exit;
  fUpdateRadioButtons:=true;
  for APlacement:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
  begin
    ARadioButton:=GetPlacementRadioButtons(APlacement);
    if ARadioButton<>nil then
      ARadioButton.Checked:=(ARadioButton=Sender);
  end;
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
  if LeftEdit<>nil then
    LeftEdit.Text:=IntToStr(Layout.Form.Left);
  if TopEdit<>nil then
    TopEdit.Text:=IntToStr(Layout.Form.Top);
  if WidthEdit<>nil then
    WidthEdit.Text:=IntToStr(Layout.Form.Width);
  if HeightEdit<>nil then
    HeightEdit.Text:=IntToStr(Layout.Form.Height);
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

{ TIDEDialogLayout }

procedure TIDEDialogLayout.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
end;

procedure TIDEDialogLayout.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TIDEDialogLayout.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

function TIDEDialogLayout.SizeValid: boolean;
begin
  Result:=(Width>0) and (Height>0);
end;

procedure TIDEDialogLayout.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  FName:=XMLConfig.GetValue(Path+'/Name/Value','');
  FWidth:=XMLConfig.GetValue(Path+'/Size/Width',0);
  FHeight:=XMLConfig.GetValue(Path+'/Size/Height',0);
end;

procedure TIDEDialogLayout.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetValue(Path+'/Name/Value',Name);
  XMLConfig.SetValue(Path+'/Size/Width',Width);
  XMLConfig.SetValue(Path+'/Size/Height',Height);
end;

{ TIDEDialogLayoutList }

function TIDEDialogLayoutList.GetItems(Index: integer): TIDEDialogLayout;
begin
  Result:=TIDEDialogLayout(FItems[Index]);
end;

constructor TIDEDialogLayoutList.Create;
begin
  inherited Create;
  FItems:=TList.Create;
end;

destructor TIDEDialogLayoutList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIDEDialogLayoutList.ApplyLayout(ADialog: TControl;
  DefaultWidth, DefaultHeight: integer);
var
  ALayout: TIDEDialogLayout;
  NewWidth, NewHeight: integer;
begin
  if ADialog=nil then exit;
  ALayout:=Find(ADialog,true);
  if ALayout.SizeValid then begin
    NewWidth:=ALayout.Width;
    NewHeight:=ALayout.Height;
  end else begin
    NewWidth:=DefaultWidth;
    NewHeight:=DefaultHeight;
  end;
  ADialog.SetBounds(ADialog.Left,ADialog.Top,NewWidth,NewHeight);
end;

procedure TIDEDialogLayoutList.SaveLayout(ADialog: TControl);
var
  ALayout: TIDEDialogLayout;
begin
  if ADialog=nil then exit;
  ALayout:=Find(ADialog,true);
  ALayout.Width:=ADialog.Width;
  ALayout.Height:=ADialog.Height;
end;

procedure TIDEDialogLayoutList.Clear;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

function TIDEDialogLayoutList.Count: integer;
begin
  Result:=FItems.Count;
end;

function TIDEDialogLayoutList.Find(const DialogName: string;
  CreateIfNotExists: boolean): TIDEDialogLayout;
var i: integer;
begin
  i:=IndexOf(DialogName);
  if (i<0) then begin
    if CreateIfNotExists then begin
      Result:=TIDEDialogLayout.Create;
      FItems.Add(Result);
      Result.Name:=DialogName;
    end else begin
      Result:=nil;
    end;
  end else begin
    Result:=Items[i];
  end;
end;

function TIDEDialogLayoutList.Find(ADialog: TObject; CreateIfNotExists: boolean
  ): TIDEDialogLayout;
begin
  if ADialog<>nil then begin
    Result:=Find(ADialog.ClassName,CreateIfNotExists);
  end else begin
    Result:=nil;
  end;
end;

function TIDEDialogLayoutList.IndexOf(const DialogName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(DialogName,Items[Result].Name)<>0) do
    dec(Result);
end;

procedure TIDEDialogLayoutList.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount, i: integer;
  NewDialogLayout: TIDEDialogLayout;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'/Count',0);
  for i:=0 to NewCount-1 do begin
    NewDialogLayout:=TIDEDialogLayout.Create;
    FItems.Add(NewDialogLayout);
    NewDialogLayout.LoadFromXMLConfig(XMLConfig,Path+'/Dialog'+IntToStr(i+1));
  end;
end;

procedure TIDEDialogLayoutList.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var i: integer;
begin
  XMLConfig.SetValue(Path+'/Count',Count);
  for i:=0 to Count-1 do
    Items[i].SaveToXMLConfig(XMLConfig,Path+'/Dialog'+IntToStr(i+1));
end;

end.

