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
  Classes, SysUtils, Laz_XMLCfg, LCLProc, FileUtil,
  Forms, Controls, StdCtrls, Buttons, BaseIDEIntf, LazConfigStorage,
  LazConf, LazarusIDEStrConsts;

type
  { TXMLOptionsStorage }

  TXMLOptionsStorage = class(TConfigStorage)
  private
    FFreeXMLConfig: boolean;
    FXMLConfig: TXMLConfig;
  protected
    function  GetFullPathValue(const APath, ADefault: String): String; override;
    function  GetFullPathValue(const APath: String; ADefault: Integer): Integer; override;
    function  GetFullPathValue(const APath: String; ADefault: Boolean): Boolean; override;
    procedure SetFullPathValue(const APath, AValue: String); override;
    procedure SetDeleteFullPathValue(const APath, AValue, DefValue: String); override;
    procedure SetFullPathValue(const APath: String; AValue: Integer); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Integer); override;
    procedure SetFullPathValue(const APath: String; AValue: Boolean); override;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Boolean); override;
    procedure DeleteFullPath(const APath: string); override;
    procedure DeleteFullPathValue(const APath: string); override;
  public
    constructor Create(const Filename: string; LoadFromDisk: Boolean); override;
    constructor Create(TheXMLConfig: TXMLConfig);
    constructor Create(TheXMLConfig: TXMLConfig; const StartPath: string);
    destructor Destroy; override;
    property XMLConfig: TXMLConfig read FXMLConfig;
    property FreeXMLConfig: boolean read FFreeXMLConfig write FFreeXMLConfig;
    procedure WriteToDisk; override;
    function GetFilename: string; override;
  end;


  { non modal IDE windows }
type
  TNonModalIDEWindow = (
    nmiwNone, // empty/none/undefined
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    nmiwMessagesViewName,
    nmiwUnitDependenciesName,
    nmiwCodeExplorerName,
    nmiwFPDocEditorName,
    nmiwClipbrdHistoryName,
    nmiwPkgGraphExplorer,
    nmiwProjectInspector,
    // debugger
    nmiwDbgOutput,
    nmiwDbgEvents,
    nmiwBreakPoints,
    nmiwWatches,
    nmiwLocals,
    nmiwCallStack,
    nmiwEvaluate,
    nmiwRegisters,
    nmiwAssembler,
    nmiwInspect,
    // extra
    nmiwSearchResultsViewName,
    nmiwAnchorEditor,
    nmiwCodeBrowser,
    nmiwIssueBrowser,
    nmiwJumpHistory
    );

const
  // This is the list of IDE windows, that will not be automatically reopened
  // on startup.
  NonModalIDEWindowManualOpen = [
    nmiwNone,
    nmiwMainIDEName,
    nmiwSourceNoteBookName,
    nmiwDbgOutput,
    nmiwDbgEvents,
    nmiwSearchResultsViewName,
    nmiwAnchorEditor
    ];

  // form names for non modal IDE windows:
  NonModalIDEWindowNames: array[TNonModalIDEWindow] of string = (
    '?',
    'MainIDE',
    'SourceNotebook',
    'MessagesView',
    'UnitDependencies',
    'CodeExplorerView',
    'FPDocEditor',
    'ClipBrdHistory',
    'PkgGraphExplorer',
    'ProjectInspector',
    // debugger
    'DbgOutput',
    'DbgEvents',
    'BreakPoints',
    'Watches',
    'Locals',
    'CallStack',
    'EvaluateModify',
    'Registers',
    'Assembler',
    'Inspect',
    // extra
    'SearchResults',
    'AnchorEditor',
    'CodeBrowser',
    'IssueBrowser',
    'JumpHistory'
   );
   
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
                                        
  { TIDEWindowLayout }

  TIDEWindowLayout = class
  private
    FFormCaption: string;
    FVisible: boolean;
    fWindowPlacement: TIDEWindowPlacement;
    fWindowPlacementsAllowed: TIDEWindowPlacements;
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    fWindowState: TIDEWindowState;
    fWindowStatesAllowed: TIDEWindowStates;
    fForm: TCustomForm;
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
    procedure SetVisible(const AValue: boolean);
    procedure SetWindowPlacementsAllowed(const AValue: TIDEWindowPlacements);
    procedure SetWindowStatesAllowed(const AValue: TIDEWindowStates);
    procedure SetDockMode(const AValue: TIDEWindowDockMode);
    procedure SetDockParent(const AValue: string);
    procedure SetForm(const AValue: TCustomForm);
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
    procedure ReadCurrentState;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function CustomCoordinatesAreValid: boolean;
    procedure CloseForm;
  public
    property FormID: string read GetFormID write SetFormID;
    function FormBaseID(out SubIndex: Integer): String;
    property FormCaption: string read FFormCaption;
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
    property Form: TCustomForm read fForm write SetForm;
    property DockParent: string
      read fDockParent write SetDockParent; // for format see GetFormId
    property DockMode: TIDEWindowDockMode read fDockMode write SetDockMode;
    property DockModesAllowed: TIDEWindowDockModes
      read fDockModesAllowed write SetDockModesAllowed;
    property DockChilds: TStringList read fDockChilds; // list of FormIDs
    property Visible: boolean read FVisible write SetVisible;
    property OnGetDefaultIDEWindowPos: TOnGetDefaultIDEWindowPos
      read fOnGetDefaultIDEWindowPos write SetOnGetDefaultIDEWindowPos;
    property OnApply: TOnApplyIDEWindowLayout read fOnApply write fOnApply;
  end;

  { TIDEWindowLayoutList }

  TIDEWindowLayoutList = class(TList)
  private
    function GetItems(Index: Integer): TIDEWindowLayout;
    procedure SetItems(Index: Integer; const AValue: TIDEWindowLayout);
  public
    procedure Clear; override;
    procedure Delete(Index: Integer);
    procedure ApplyAll;
    procedure Apply(AForm: TCustomForm; const ID: string);
    procedure StoreWindowPositions;
    procedure Assign(SrcList: TIDEWindowLayoutList);
    function IndexOf(const FormID: string): integer;
    function ItemByForm(AForm: TCustomForm): TIDEWindowLayout;
    function ItemByFormID(const FormID: string): TIDEWindowLayout;
    function ItemByFormCaption(const aFormCaption: string): TIDEWindowLayout;
    function ItemByEnum(ID: TNonModalIDEWindow): TIDEWindowLayout;
    procedure CloseForm(AForm: TCustomForm);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    property Items[Index: Integer]: TIDEWindowLayout
      read GetItems write SetItems; default;
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

function CreateNiceWindowPosition(Width, Height: integer): TRect;
function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
                                ): TConfigStorage; // load errors: raises exceptions

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

function CreateNiceWindowPosition(Width, Height: integer): TRect;

  function FindFormAt(x,y: integer): TCustomForm;
  var
    i: Integer;
  begin
    for i := 0 to Screen.CustomFormCount - 1 do
    begin
      Result := Screen.CustomForms[i];
      if Result.HandleAllocated and Result.Visible
      and (Result.Left >= x - 5) and (Result.Left <= x + 5)
      and (Result.Top >= y - 5) and (Result.Top <= y + 5)
      then
        exit;
    end;
    Result := nil;
  end;

var
  MinX: Integer;
  MinY: Integer;
  MaxX: Integer;
  MaxY: Integer;
  x: Integer;
  y: Integer;
  MidX: Integer;
  MidY: Integer;
  Step: Integer;
  ABounds: TRect;
begin
  if Screen.ActiveCustomForm <> nil then
    ABounds := Screen.ActiveCustomForm.Monitor.BoundsRect
  else
  if Application.MainForm <> nil then
    ABounds := Application.MainForm.Monitor.BoundsRect
  else
    ABounds := Screen.PrimaryMonitor.BoundsRect;

  MinX := ABounds.Left;
  MinY := ABounds.Top;
  MaxX := ABounds.Right - Width - 10;
  if MaxX < MinX + 10 then MaxX := MinX + 10;
  MaxY := ABounds.Bottom - Height - 100; // why -100?
  if MaxY < MinY + 10 then MaxY := MinY + 10;
  MidX := (MaxX + MinX) div 2;
  MidY := (MaxY + MinY) div 2;
  Step := 0;
  repeat
    x := MidX - Step * 20;
    y := MidY - Step * 20;
    if (x < MinX) or (x > MaxX) or (y < MinY) or (y > MaxY) then break;
    if (FindFormAt(x, y)=nil) or (Step > 1000) then break;
    inc(Step);
  until False;
  Result.Left := x;
  Result.Top := y;
  Result.Right := x + Width;
  Result.Bottom := y + Height;
end;

function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;
begin
  for Result:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if NonModalIDEWindowNames[Result]=FormID then
      exit;
  Result:=nmiwNone;
end;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
  ): TConfigStorage;
var
  ConfigFilename: String;
begin
  if LoadFromDisk then begin
    // copy template config file to users config directory
    CopySecondaryConfigFile(Filename);
  end;
  // create storage
  ConfigFilename:=AppendPathDelim(GetPrimaryConfigPath)+Filename;
  Result:=TXMLOptionsStorage.Create(ConfigFilename,LoadFromDisk);
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
  FFormCaption := XMLConfig.GetValue(P+'Caption/Value', fFormID);
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
  FVisible:=XMLConfig.GetValue(P+'Visible/Value',false);
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
  XMLConfig.SetDeleteValue(P+'Caption/Value',FFormCaption,'');
  // placement
  XMLConfig.SetDeleteValue(P+'WindowPlacement/Value',
    IDEWindowPlacementNames[fWindowPlacement],
    IDEWindowPlacementNames[iwpRestoreWindowSize]);
  // custom position
  XMLConfig.SetDeleteValue(P+'CustomPosition/Left',fLeft,0);
  XMLConfig.SetDeleteValue(P+'CustomPosition/Top',fTop,0);
  XMLConfig.SetDeleteValue(P+'CustomPosition/Width',fWidth,0);
  XMLConfig.SetDeleteValue(P+'CustomPosition/Height',fHeight,0);
  // state
  XMLConfig.SetValue(P+'WindowState/Value',IDEWindowStateNames[fWindowState]);
  // docking
  XMLConfig.SetDeleteValue(P+'Docking/Parent',fDockParent,'');
  XMLConfig.SetDeleteValue(P+'Docking/ChildCount',fDockChilds.Count,0);
  for i:=0 to fDockChilds.Count-1 do begin
    XMLConfig.SetDeleteValue(P+'Docking/Child'+IntToStr(i),fDockChilds[i],'');
  end;
  XMLConfig.SetValue(P+'DockMode/Value',IDEWindowDockModeNames[fDockMode]);
  XMLConfig.SetDeleteValue(P+'Visible/Value',FVisible,false);
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

procedure TIDEWindowLayout.CloseForm;
begin
  GetCurrentPosition;
  Form:=nil;
end;

function TIDEWindowLayout.FormBaseID(out SubIndex: Integer): String;
var
  i: Integer;
begin
  Result := FormID;
  SubIndex := -1;
  i := length(Result);
  while (i > 0) and (Result[i] in ['0'..'9']) do
    dec(i);
  if (i < 1) or (i = length(Result)) then
    exit;
  SubIndex := StrToInt(copy(Result, i+1, length(Result)));
  Result := copy(Result, 1, i);
end;

procedure TIDEWindowLayout.SetForm(const AValue: TCustomForm);
begin
  if fForm=AValue then exit;
  fForm:=AValue;
  if (Form<>nil) then begin
    fFormID := FForm.Name;
    FFormCaption := fForm.Caption;
  end;
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
    if not ( (Result[i] in ['A'..'Z','a'..'z','_'])
            or ( (i > 1) and (Result[i] in ['0'..'9'])) )
    then
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

procedure TIDEWindowLayout.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
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
    Left:=Form.RestoredLeft;
    Top:=Form.RestoredTop;
    Width:=Form.RestoredWidth;
    Height:=Form.RestoredHeight;
  end;
end;

procedure TIDEWindowLayout.ReadCurrentState;
begin
  Visible:=(Form<>nil) and Form.Visible;
  if Form<>nil then begin
    case Form.WindowState of
    wsMinimized: fWindowState:=iwsMinimized;
    wsMaximized: fWindowState:=iwsMaximized;
    else
      fWindowState:=iwsNormal;
    end;
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
  //debugln('TIDEWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' ',IDEWindowPlacementNames[WindowPlacement]);
  case WindowPlacement of
  iwpRestoreWindowGeometry, iwpRestoreWindowSize:
    ReadCurrentCoordinates;

  end;
  ReadCurrentState;
  //debugln('TIDEWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' Width=',dbgs(Width));
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
  XMLConfig.SetDeleteValue(Path+'FormIdCount',Count,0);
  for i:=0 to Count-1 do Begin
    XMLConfig.SetDeleteValue(Path+'FormIdList/a'+IntToStr(i), Items[i].FormID, '');
    Items[i].SaveToXMLConfig(XMLConfig,Path);
  end
end;

function TIDEWindowLayoutList.ItemByForm(AForm: TCustomForm): TIDEWindowLayout;
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

function TIDEWindowLayoutList.ItemByFormCaption(const aFormCaption: string
  ): TIDEWindowLayout;
var i: integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    Result := Items[i];
    if Result.FormCaption = aFormCaption then
      exit;
    dec(i);
  end;
  Result:=nil;
end;

function TIDEWindowLayoutList.ItemByEnum(ID: TNonModalIDEWindow
  ): TIDEWindowLayout;
begin
  Result:=ItemByFormID(NonModalIDEWindowNames[ID]);
end;

procedure TIDEWindowLayoutList.CloseForm(AForm: TCustomForm);
var
  ALayout: TIDEWindowLayout;
begin
  ALayout:=ItemByForm(AForm);
  if ALayout<>nil then
    ALayout.CloseForm;
end;

procedure TIDEWindowLayoutList.ApplyAll;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].Apply;
end;

procedure TIDEWindowLayoutList.Apply(AForm: TCustomForm; const ID: string);
var ALayout: TIDEWindowLayout;
begin
  ALayout:=ItemByFormID(ID);
  if ALayout=nil then
    RaiseGDBException(ID);
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

{ TXMLOptionsStorage }

function TXMLOptionsStorage.GetFullPathValue(const APath, ADefault: String): String;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Integer): Integer;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

function TXMLOptionsStorage.GetFullPathValue(const APath: String;
  ADefault: Boolean): Boolean;
begin
  Result:=XMLConfig.GetValue(APath, ADefault);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath, AValue: String);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath, AValue,
  DefValue: String);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Integer);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Integer);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.SetFullPathValue(const APath: String;
  AValue: Boolean);
begin
  XMLConfig.SetValue(APath, AValue);
end;

procedure TXMLOptionsStorage.SetDeleteFullPathValue(const APath: String;
  AValue, DefValue: Boolean);
begin
  XMLConfig.SetDeleteValue(APath, AValue, DefValue);
end;

procedure TXMLOptionsStorage.DeleteFullPath(const APath: string);
begin
  XMLConfig.DeletePath(APath);
end;

procedure TXMLOptionsStorage.DeleteFullPathValue(const APath: string);
begin
  XMLConfig.DeleteValue(APath);
end;

constructor TXMLOptionsStorage.Create(const Filename: string;
  LoadFromDisk: Boolean);
begin
  if LoadFromDisk then
    FXMLConfig:=TXMLConfig.Create(Filename)
  else
    FXMLConfig:=TXMLConfig.CreateClean(Filename);
  FFreeXMLConfig:=true;
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig);
begin
  FXMLConfig:=TheXMLConfig;
  if FXMLConfig=nil then
    raise Exception.Create('');
end;

constructor TXMLOptionsStorage.Create(TheXMLConfig: TXMLConfig;
  const StartPath: string);
begin
  Create(TheXMLConfig);
  AppendBasePath(StartPath);
end;

destructor TXMLOptionsStorage.Destroy;
begin
  if FreeXMLConfig then FreeAndNil(FXMLConfig);
  inherited Destroy;
end;

procedure TXMLOptionsStorage.WriteToDisk;
begin
  FXMLConfig.Flush;
end;

function TXMLOptionsStorage.GetFilename: string;
begin
  Result:=FXMLConfig.Filename;
end;

initialization
  DefaultConfigClass:=TXMLOptionsStorage;
  GetIDEConfigStorage:=@GetLazIDEConfigStorage;

end.

