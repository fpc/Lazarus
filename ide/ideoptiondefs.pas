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
  Math, Classes, SysUtils, types, Laz_XMLCfg, LCLProc, FileUtil,
  Forms, Controls, StdCtrls, Buttons, BaseIDEIntf, LazConfigStorage,
  IDEWindowIntf, LazConf, LazarusIDEStrConsts;

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
    procedure Clear; override;
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
  // on startup. These windows are opened automatically when needed.
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
    iwpCustomPosition,          // set window to custom position
    iwpRestoreWindowSize        // save window size at end and restore it
                                //   at start
    );
  TIDEWindowPlacements = set of TIDEWindowPlacement;
  TIDEWindowState = (iwsNormal, iwsMaximized, iwsMinimized, iwsHidden);
  TIDEWindowStates = set of TIDEWindowState;
  
  TSimpleWindowLayout = class;
  TOnGetDefaultIDEWindowPos = procedure(Sender: TSimpleWindowLayout;
                                        var Bounds: TRect) of object;
  TOnApplySimpleWindowLayout = procedure(Layout: TSimpleWindowLayout) of object;
                                        
  { TSimpleWindowLayout }

  TSimpleWindowLayout = class(TComponent)
  private
    FApplied: boolean;
    FFormCaption: string;
    FVisible: boolean;
    fWindowPlacement: TIDEWindowPlacement;
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    fWindowState: TIDEWindowState;
    fForm: TCustomForm;
    fFormID: string;
    fDefaultWindowPlacement: TIDEWindowPlacement;
    function GetFormCaption: string;
    function GetFormID: string;
    procedure SetForm(const AValue: TCustomForm);
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AFormID: string); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure GetCurrentPosition;
    procedure Assign(Layout: TSimpleWindowLayout); reintroduce;
    procedure ReadCurrentCoordinates;
    procedure ReadCurrentState;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    function CustomCoordinatesAreValid: boolean;
    procedure CloseForm;
  public
    property FormID: string read GetFormID write FFormID;
    function FormBaseID(out SubIndex: Integer): String; // split FormID into name+number
    property FormCaption: string read GetFormCaption;
    property WindowPlacement: TIDEWindowPlacement read fWindowPlacement write FWindowPlacement;
    property DefaultWindowPlacement: TIDEWindowPlacement
      read fDefaultWindowPlacement write fDefaultWindowPlacement;
    property Left: integer read fLeft write FLeft;
    property Top: integer read fTop write FTop;
    property Width: integer read fWidth write FWidth;
    property Height: integer read fHeight write FHeight;
    property WindowState: TIDEWindowState read fWindowState write FWindowState;
    property Form: TCustomForm read fForm write SetForm;
    property Visible: boolean read FVisible write FVisible;
    property Applied: boolean read FApplied write FApplied;
  end;

  { TSimpleWindowLayoutList }

  TSimpleWindowLayoutList = class
  private
    fItems: TFPList;
    function GetItems(Index: Integer): TSimpleWindowLayout;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure ApplyAndShow(Sender: TObject; AForm: TCustomForm;
                           BringToFront: boolean);
    procedure StoreWindowPositions;
    procedure Assign(SrcList: TSimpleWindowLayoutList);
    function Add(ALayout: TSimpleWindowLayout): integer;
    function IndexOf(const FormID: string): integer;
    function ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
    function ItemByFormID(const FormID: string): TSimpleWindowLayout;
    function ItemByFormCaption(const aFormCaption: string): TSimpleWindowLayout;
    procedure CloseForm(AForm: TCustomForm);
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
  public
    function Count: integer;
    property Items[Index: Integer]: TSimpleWindowLayout read GetItems; default;
  end;
  
const
  IDEWindowPlacementNames: array[TIDEWindowPlacement] of string = (
      'UseWindowManagerSetting',
      'Default',
      'RestoreWindowGeometry',
      'CustomPosition',
      'RestoreWindowSize'
    );
  IDEWindowStateNames: array[TIDEWindowState] of string = (
      'Normal', 'Maximized', 'Minimized', 'Hidden'
    );

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
function StrToIDEWindowState(const s: string): TIDEWindowState;

function CreateNiceWindowPosition(Width, Height: integer): TRect;
function NonModalIDEFormIDToEnum(const FormID: string): TNonModalIDEWindow;

function GetLazIDEConfigStorage(const Filename: string; LoadFromDisk: Boolean
                                ): TConfigStorage; // load errors: raises exceptions

implementation


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

{ TSimpleWindowLayout }

constructor TSimpleWindowLayout.Create(AFormID: string);
begin
  inherited Create(nil);
  FormID:=AFormID;
  fDefaultWindowPlacement:=iwpRestoreWindowGeometry;
  Clear;
end;

destructor TSimpleWindowLayout.Destroy;
begin
  Form:=nil;
  inherited Destroy;
end;

procedure TSimpleWindowLayout.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
var
  P: string;
begin
  // set all values to default
  Clear;
  // read settings
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  FFormCaption := Config.GetValue(P+'Caption/Value', fFormID);
  // placement
  fWindowPlacement:=StrToIDEWindowPlacement(Config.GetValue(
    P+'WindowPlacement/Value',IDEWindowPlacementNames[fWindowPlacement]));
  // custom position
  fLeft:=Config.GetValue(P+'CustomPosition/Left',fLeft);
  fTop:=Config.GetValue(P+'CustomPosition/Top',fTop);
  fWidth:=Config.GetValue(P+'CustomPosition/Width',fWidth);
  fHeight:=Config.GetValue(P+'CustomPosition/Height',fHeight);
  // state
  fWindowState:=StrToIDEWindowState(Config.GetValue(
    P+'WindowState/Value',IDEWindowStateNames[fWindowState]));
  FVisible:=Config.GetValue(P+'Visible/Value',false);
end;

procedure TSimpleWindowLayout.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var
  P: string;
begin
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  Config.SetDeleteValue(P+'Caption/Value',FFormCaption,'');
  // placement
  Config.SetDeleteValue(P+'WindowPlacement/Value',
    IDEWindowPlacementNames[fWindowPlacement],
    IDEWindowPlacementNames[iwpRestoreWindowSize]);
  // custom position
  Config.SetDeleteValue(P+'CustomPosition/Left',fLeft,0);
  Config.SetDeleteValue(P+'CustomPosition/Top',fTop,0);
  Config.SetDeleteValue(P+'CustomPosition/Width',fWidth,0);
  Config.SetDeleteValue(P+'CustomPosition/Height',fHeight,0);
  // state
  Config.SetValue(P+'WindowState/Value',IDEWindowStateNames[fWindowState]);
  Config.SetDeleteValue(P+'Visible/Value',FVisible,false);
end;

procedure TSimpleWindowLayout.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GetCurrentPosition;
end;

procedure TSimpleWindowLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if fForm=AComponent then begin
      fForm:=nil;
      Applied:=false;
    end;
  end;
end;

function TSimpleWindowLayout.CustomCoordinatesAreValid: boolean;
begin
  Result:=(Width>0) and (Height>0) and (Left>10-Width) and (Top>10-Height);
end;

procedure TSimpleWindowLayout.CloseForm;
begin
  GetCurrentPosition;
  Form:=nil;
end;

function TSimpleWindowLayout.FormBaseID(out SubIndex: Integer): String;
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

procedure TSimpleWindowLayout.SetForm(const AValue: TCustomForm);
begin
  if fForm=AValue then exit;
  if (Form<>nil) then begin
    RemoveFreeNotification(Form);
    Form.RemoveHandlerClose(@OnFormClose);
  end;
  fForm:=AValue;
  if (Form<>nil) then begin
    fFormID := Form.Name;
    FFormCaption := Form.Caption;
    FreeNotification(Form);
    Applied:=false;
  end;
end;

function TSimpleWindowLayout.GetFormID: string;
begin
  if Form=nil then
    Result:=fFormID
  else
    Result:=Form.Name;
end;

function TSimpleWindowLayout.GetFormCaption: string;
begin
  if Form<>nil then
    FFormCaption:=Form.Caption
  else if FFormCaption='' then
    FFormCaption:=FormID;
  Result:=FFormCaption;
end;

procedure TSimpleWindowLayout.Clear;
begin
  fWindowPlacement:=fDefaultWindowPlacement;
  fLeft:=0;
  fTop:=0;
  fWidth:=0;
  fHeight:=0;
  fWindowState:=iwsNormal;
end;

procedure TSimpleWindowLayout.ReadCurrentCoordinates;
var
  p: TPoint;
begin
  if (Form<>nil) and (Form.WindowState=wsNormal) then begin
    if Form.Parent<>nil then
      p:=Form.ClientOrigin
    else
      p:=Point(0,0);
    Left:=Form.Left+p.X;
    Top:=Form.Top+p.Y;
    Width:=Form.Width;
    Height:=Form.Height;
  end;
end;

procedure TSimpleWindowLayout.ReadCurrentState;
begin
  Visible:=(Form<>nil) and Form.IsVisible;
  if Form<>nil then begin
    case Form.WindowState of
    wsMinimized: fWindowState:=iwsMinimized;
    wsMaximized: fWindowState:=iwsMaximized;
    else
      fWindowState:=iwsNormal;
    end;
  end;
end;

procedure TSimpleWindowLayout.Assign(Layout: TSimpleWindowLayout);
begin
  Clear;
  FApplied:=Layout.Applied;
  Form:=Layout.Form;
  fWindowPlacement:=Layout.fWindowPlacement;
  fLeft:=Layout.fLeft;
  fTop:=Layout.fTop;
  fWidth:=Layout.fWidth;
  fHeight:=Layout.fHeight;
  fWindowState:=Layout.fWindowState;
  fFormID:=Layout.fFormID;
  fDefaultWindowPlacement:=Layout.fDefaultWindowPlacement;
end;

procedure TSimpleWindowLayout.GetCurrentPosition;
begin
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' ',IDEWindowPlacementNames[WindowPlacement]);
  case WindowPlacement of
  iwpRestoreWindowGeometry, iwpRestoreWindowSize:
    ReadCurrentCoordinates;
  end;
  ReadCurrentState;
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' Width=',dbgs(Width));
end;

{ TSimpleWindowLayoutList }

procedure TSimpleWindowLayoutList.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  fItems.Clear;
end;

procedure TSimpleWindowLayoutList.Delete(Index: Integer);
begin
  Items[Index].Free;
  fItems.Delete(Index);
end;

function TSimpleWindowLayoutList.GetItems(Index: Integer): TSimpleWindowLayout;
begin
  Result:=TSimpleWindowLayout(fItems[Index]);
end;

constructor TSimpleWindowLayoutList.Create;
begin
  fItems:=TFPList.Create;
end;

destructor TSimpleWindowLayoutList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TSimpleWindowLayoutList.IndexOf(const FormID: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FormID<>Items[Result].GetFormID) do dec(Result);
end;

procedure TSimpleWindowLayoutList.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].LoadFromConfig(Config,Path);
end;

procedure TSimpleWindowLayoutList.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var i: integer;
begin
  Config.SetDeleteValue(Path+'FormIdCount',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToConfig(Config,Path);
end;

function TSimpleWindowLayoutList.Count: integer;
begin
  Result:=fItems.Count;
end;

function TSimpleWindowLayoutList.ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
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

function TSimpleWindowLayoutList.ItemByFormID(const FormID: string
  ): TSimpleWindowLayout;
var i: integer;
begin
  i:=IndexOf(FormID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TSimpleWindowLayoutList.ItemByFormCaption(const aFormCaption: string
  ): TSimpleWindowLayout;
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

procedure TSimpleWindowLayoutList.CloseForm(AForm: TCustomForm);
var
  ALayout: TSimpleWindowLayout;
begin
  ALayout:=ItemByForm(AForm);
  if ALayout<>nil then
    ALayout.CloseForm;
end;

procedure TSimpleWindowLayoutList.ApplyAndShow(Sender: TObject;
  AForm: TCustomForm; BringToFront: boolean);
var
  ALayout: TSimpleWindowLayout;
  NewBounds: TRect;
  Creator: TIDEWindowCreator;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  DockSiblingBounds: TRect;
  Offset: TPoint;
begin
  {$IFDEF VerboseIDEDocking}
  debugln(['TSimpleWindowLayoutList.ApplyAndShow Form=',DbgSName(AForm),' ',BringToFront]);
  {$ENDIF}
  try
    ALayout:=ItemByFormID(AForm.Name);
    if ALayout<>nil then
    begin
      ALayout.Form:=AForm;
      if ALayout.Applied then exit;
      ALayout.Applied:=true;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow restore ',ALayout.FormID]);
      {$ENDIF}

      case ALayout.WindowPlacement of
      iwpCustomPosition,iwpRestoreWindowGeometry:
        begin
          //DebugLn(['TMainIDE.OnApplyWindowLayout ',IDEWindowStateNames[ALayout.WindowState]]);
          case ALayout.WindowState of
          iwsMinimized: AForm.WindowState:=wsMinimized;
          iwsMaximized: AForm.WindowState:=wsMaximized;
          end;

          if (ALayout.CustomCoordinatesAreValid) then begin
            // explicit position
            NewBounds:=Bounds(ALayout.Left,ALayout.Top,ALayout.Width,ALayout.Height);
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
            // set bounds (do not use SetRestoredBounds - that flickers with the current LCL implementation)
            AForm.SetBounds(
              NewBounds.Left,NewBounds.Top,
              NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
            exit;
          end;

          if ALayout.WindowState in [iwsMinimized, iwsMaximized] then
            exit;
        end;

      iwpUseWindowManagerSetting:
        begin
          exit;
        end;
      end;
    end;

    // no layout found => use default
    Creator:=IDEWindowCreators.FindWithName(AForm.Name);
    if Creator<>nil then
    begin
      debugln(['TSimpleWindowLayoutList.ApplyAndShow creator found for ',DbgSName(AForm),': Left=',Creator.Left,' Top=',Creator.Top,' Width=',Creator.Width,' Height=',Creator.Height,' DockSibling=',Creator.DockSibling,' DockAlign=',dbgs(Creator.DockAlign)]);
      if Creator.OnGetLayout<>nil then
        Creator.OnGetLayout(Self,AForm.Name,NewBounds,DockSiblingName,DockAlign)
      else begin
        Creator.GetDefaultBounds(AForm,NewBounds);
        DockSiblingName:=Creator.DockSibling;
        DockAlign:=Creator.DockAlign;
      end;
      if DockSiblingName<>'' then
      begin
        DockSibling:=Screen.FindForm(DockSiblingName);
        if DockSibling<>nil then
        begin
          DockSiblingBounds:=DockSibling.BoundsRect;
          if DockSibling.Parent<>nil then
          begin
            Offset:=DockSibling.ClientToScreen(Point(0,0));
            OffsetRect(DockSiblingBounds,Offset.X,Offset.Y);
          end;
          case DockAlign of
          alLeft:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Left-6-NewBounds.Right,0);
            end;
          alRight:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Right+6-NewBounds.Left,0);
            end;
          alTop:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Top-25-NewBounds.Bottom);
            end;
          alBottom:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Bottom+25-NewBounds.Top);
            end;
          alClient:
            NewBounds:=DockSibling.BoundsRect;
          end;
        end;
      end;
      debugln(['TSimpleWindowLayoutList.ApplyAndShow ',DbgSName(AForm),' NewBounds=',dbgs(NewBounds)]);
      NewBounds.Left:=Min(10000,Max(-10000,NewBounds.Left));
      NewBounds.Top:=Min(10000,Max(-10000,NewBounds.Top));
      NewBounds.Right:=Max(NewBounds.Left+100,NewBounds.Right);
      NewBounds.Bottom:=Max(NewBounds.Top+100,NewBounds.Bottom);
      AForm.BoundsRect:=NewBounds;
    end;
  finally
    if (AForm.WindowState in [wsNormal,wsMaximized]) and BringToFront then
      AForm.ShowOnTop
    else
      AForm.Visible:=true;
  end;
end;

procedure TSimpleWindowLayoutList.StoreWindowPositions;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].GetCurrentPosition;
end;

procedure TSimpleWindowLayoutList.Assign(SrcList: TSimpleWindowLayoutList);
var i: integer;
  NewLayout: TSimpleWindowLayout;
begin
  Clear;
  if SrcList=nil then exit;
  for i:=0 to SrcList.Count-1 do begin
    NewLayout:=TSimpleWindowLayout.Create(SrcList[i].FormID);
    NewLayout.Assign(SrcList[i]);
    Add(NewLayout);
  end;
end;

function TSimpleWindowLayoutList.Add(ALayout: TSimpleWindowLayout): integer;
begin
  Result:=fItems.Add(ALayout);
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

procedure TXMLOptionsStorage.Clear;
begin
  FXMLConfig.Clear;
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

