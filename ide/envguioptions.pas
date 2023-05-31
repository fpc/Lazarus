unit EnvGuiOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Math,
  // LCL
  Graphics, Forms, Controls,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, LazUTF8, Laz2_XMLCfg, Laz2_DOM,
  // BuildIntf
  ProjectIntf, IDEOptionsIntf, IDEExternToolIntf,
  // IDEIntf
  ObjectInspector, IDEWindowIntf, IDEOptEditorIntf, ComponentReg, SrcEditorIntf,
  // IdeConfig
  IDEOptionDefs, EnvironmentOpts, ModeMatrixOpts, CoolBarOptions, EditorToolBarOptions;

const
  DefaultRubberbandSelectsGrandChilds = false;
  DefaultBorderSpacingColor = clRed;
  DefaultGridColor = clBlack;
  DefaultGridSize = 8;
  DefaultGuideLineColorLeftTop = clBlue;
  DefaultGuideLineColorRightBottom = clGreen;

type
  { Messages window }
  TMsgWndFileNameStyle = (
    mwfsShort,    // = ExtractFilename
    mwfsRelative, // = CreateRelativePath
    mwfsFull
    );
  TMsgWndFileNameStyles = set of TMsgWndFileNameStyle;
const
  MsgWndFileNameStyleNames: array[TMsgWndFileNameStyle] of string = (
    'Short',    // mwfsShort
    'Relative', // mwfsRelative
    'Full'      // mwfsFull
    );
type
  TMsgWndColor = (
    mwBackground,
    mwRunning,
    mwSuccess,
    mwFailed,
    mwAutoHeader,
    mwTextColor
    );
const
  MsgWndDefBackgroundColor = clWindow;
  MsgWndDefHeaderBackgroundRunning = clYellow;
  MsgWndDefHeaderBackgroundSuccess = TColor($60FF60); // light green
  MsgWndDefHeaderBackgroundFailed = TColor($6060FF); // light red
  MsgWndDefAutoHeaderBackground = clSkyBlue;
  MsgWndDefTextColor = clDefault;

  MsgWndDefaultColors: array[TMsgWndColor] of TColor = (
    MsgWndDefBackgroundColor,         // mwBackground
    MsgWndDefHeaderBackgroundRunning, // mwRunning
    MsgWndDefHeaderBackgroundSuccess, // mwSuccess
    MsgWndDefHeaderBackgroundFailed,  // mwFailed
    MsgWndDefAutoHeaderBackground,    // mwAutoHeader
    MsgWndDefTextColor
    );
  MsgWndColorNames: array[TMsgWndColor] of string = (
    'Background',
    'Running',
    'Success',
    'Failed',
    'AutoHeader',
    'TextColor'
    );

type
  { TCustomDesktopOpt }

  TCustomDesktopOpt = class
  protected
    FName:string;
    FAssociatedDebugDesktopName: String;
    FConfigStore: TXMLOptionsStorage;
    FIsDocked: Boolean;
    FXMLCfg: TRttiXMLConfig;
    function GetCompatible: Boolean; virtual;
  public
    constructor Create(const aName: String); virtual; overload;
    constructor Create(const aName: String; const aIsDocked: Boolean); virtual; overload;
    destructor Destroy;  override;
    procedure SetConfig(aXMLCfg: TRttiXMLConfig; aConfigStore: TXMLOptionsStorage);
    procedure Load(Path: String); virtual;
    procedure Save(Path: String); virtual; abstract;
    property Name: String read FName write FName;
    property AssociatedDebugDesktopName: String read FAssociatedDebugDesktopName write FAssociatedDebugDesktopName;
    property IsDocked: Boolean read FIsDocked;
    property Compatible: Boolean read GetCompatible;
  end;
  TDesktopOptClass = class of TCustomDesktopOpt;

  TEnvGuiOptions = class;

  { TDesktopOIOptions }

  TDesktopOIOptions = class(TPersistent)
  private
    FComponentTreeHeight: integer;
    FInfoBoxHeight: integer;
    FShowComponentTree: Boolean;
    FShowInfoBox: boolean;
    FSplitterX: array[TObjectInspectorPage] of Integer;

    function GetSplitterX(const APage: TObjectInspectorPage): Integer;
    procedure SetSplitterX(const APage: TObjectInspectorPage; const ASplitterX: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    procedure ImportSettingsFromIDE(const AOptions: TEnvGuiOptions);
    procedure ExportSettingsToIDE(const AOptions: TEnvGuiOptions);
    procedure Load(XMLConfig: TXMLConfig; Path: String);
    procedure Save(XMLConfig: TXMLConfig; Path: String);

    property ShowComponentTree: Boolean read FShowComponentTree write FShowComponentTree;
    property ComponentTreeHeight: integer read FComponentTreeHeight write FComponentTreeHeight;
    property SplitterX[const APage: TObjectInspectorPage]: Integer read GetSplitterX write SetSplitterX;
    property ShowInfoBox: boolean read FShowInfoBox write FShowInfoBox;
    property InfoBoxHeight: integer read FInfoBoxHeight write FInfoBoxHeight;
  end;

  { TDesktopOpt }

  TDesktopOpt = class(TCustomDesktopOpt)
  private
    // window layout
    FIDEWindowCreatorsLayoutList: TSimpleWindowLayoutList;
    FIDEDialogLayoutList: TIDEDialogLayoutList;
    FObjectInspectorOptions: TDesktopOIOptions;
    FSingleTaskBarButton: boolean;
    FHideIDEOnRun: boolean;
    FAutoAdjustIDEHeight: boolean;
    FAutoAdjustIDEHeightFullCompPal: boolean;
    FProjectInspectorShowProps: boolean;
    // window menu
    FIDENameForDesignedFormList: boolean;
    // CompletionWindow
    FCompletionWindowWidth: Integer;
    FCompletionWindowHeight: Integer;
    // title
    FIDETitleStartsWithProject: boolean;
    FIDETitleIncludesBuildMode: boolean;
    FIDEProjectDirectoryInIdeTitle: boolean;
    // IDE Coolbar
    FIDECoolBarOptions: TIDECoolBarOptions;
    // Editor Toolbar
    FEditorToolBarOptions: TEditorToolBarOptions;
    // component palette
    FComponentPaletteOptions: TCompPaletteOptions;
    // Docking options
    FDockedOpt: TAbstractDesktopDockingOpt;

    procedure InitLayoutList;
  protected
    function GetCompatible: Boolean; override;
  public
    constructor Create(const aName: String; const aIsDocked: Boolean); override; overload;
    destructor Destroy; override;
    procedure Assign(Source: TDesktopOpt; const AssignName: Boolean = False;
      const IsCompatible: Boolean = True);
  public
    procedure Load(Path: String); override;
    procedure Save(Path: String); override;
    procedure ImportSettingsFromIDE(const AOptions: TEnvGuiOptions);
    procedure ExportSettingsToIDE(const AOptions: TEnvGuiOptions);
    procedure RestoreDesktop;

    property IDEWindowCreatorsLayoutList: TSimpleWindowLayoutList read FIDEWindowCreatorsLayoutList write FIDEWindowCreatorsLayoutList;
    property IDEDialogLayoutList: TIDEDialogLayoutList read FIDEDialogLayoutList;
    property SingleTaskBarButton: boolean read FSingleTaskBarButton write FSingleTaskBarButton;
    property HideIDEOnRun: boolean read FHideIDEOnRun write FHideIDEOnRun;
    property AutoAdjustIDEHeight: Boolean read FAutoAdjustIDEHeight write FAutoAdjustIDEHeight;
    property AutoAdjustIDEHeightFullCompPal: Boolean read FAutoAdjustIDEHeightFullCompPal
                                                     write FAutoAdjustIDEHeightFullCompPal;
    property IDENameForDesignedFormList: boolean read FIDENameForDesignedFormList
                                               write FIDENameForDesignedFormList;
    property CompletionWindowWidth: Integer read FCompletionWindowWidth write FCompletionWindowWidth;
    property CompletionWindowHeight: Integer read FCompletionWindowHeight write FCompletionWindowHeight;
    property IDETitleStartsWithProject: boolean read FIDETitleStartsWithProject
                                               write FIDETitleStartsWithProject;
    property IDETitleIncludesBuildMode: boolean read FIDETitleIncludesBuildMode
                                               write FIDETitleIncludesBuildMode;
    property IDEProjectDirectoryInIdeTitle: boolean read FIDEProjectDirectoryInIdeTitle
                                                    write FIDEProjectDirectoryInIdeTitle;
    property IDECoolBarOptions: TIDECoolBarOptions read FIDECoolBarOptions;
    property EditorToolBarOptions: TEditorToolBarOptions read FEditorToolBarOptions;
    property ComponentPaletteOptions: TCompPaletteOptions read FComponentPaletteOptions;
    property ObjectInspectorOptions: TDesktopOIOptions read FObjectInspectorOptions;
    property ProjectInspectorShowProps: boolean read FProjectInspectorShowProps
                                               write FProjectInspectorShowProps;
  end;

  { TUnsupportedDesktopOpt }

  TUnsupportedDesktopOpt = Class(TCustomDesktopOpt)
  private
    FRetainXMLData:TDOMDocument;
  public
    destructor Destroy; override;
    procedure Load(Path: String); override;
    procedure Save(Path: String); override;
  end;

  { TDesktopOptList }

  TDesktopOptList = class(TObjectList)
  private
    FXMLCfg: TRttiXMLConfig;
    FConfigStore: TXMLOptionsStorage;
    function GetItem(Index: Integer): TCustomDesktopOpt;
    procedure SetConfig(aXMLCfg: TRttiXMLConfig; aConfigStore: TXMLOptionsStorage);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddFromCfg(Path: String);
    function IndexOf(aName: string): integer;
    function Find(aName: string): TCustomDesktopOpt;
    property Items[Index: Integer]: TCustomDesktopOpt read GetItem; default;
  end;

  { TEnvGuiOptions }

  TEnvGuiOptions = class(TIDESubOptions)
  private
    FConfigStorage: TXMLOptionsStorage;
    // hints
    FShowHintsForComponentPalette: boolean;
    FShowHintsForMainSpeedButtons: boolean;
    // glyphs
    FShowButtonGlyphs: TApplicationShowGlyphs;
    FShowMenuGlyphs: TApplicationShowGlyphs;
    // designer
    FCreateComponentFocusNameProperty: boolean;
    FSwitchToFavoritesOITab: boolean;
    FShowBorderSpacing: boolean;
    FBorderSpacingColor: TColor;
    FShowGrid: boolean;
    FSnapToGrid: boolean;
    FGridSizeX: integer;
    FGridSizeY: integer;
    FGridColor: TColor;
    FShowGuideLines: boolean;
    FSnapToGuideLines: boolean;
    FGuideLineColorLeftTop: TColor;
    FGuideLineColorRightBottom: TColor;
    FShowComponentCaptions: boolean;
    FShowEditorHints: boolean;
    FAutoCreateFormsOnOpen: boolean;
    FRightClickSelects: boolean;
    FGrabberColor: TColor;
    FMarkerColor: TColor;
    FNonFormBackgroundColor: TColor;
    FRubberbandSelectionColor: TColor;
    FRubberbandCreationColor: TColor;
    FRubberbandSelectsGrandChilds: boolean;
    FCheckPackagesOnFormCreate: boolean;
    FFormTitleBarChangesObjectInspector: boolean;
    FForceDPIScalingInDesignTime: boolean;
    // component list
    FComponentListKeepOpen: Boolean;
    FComponentListPageIndex: Integer;
    // object inspector
    FObjectInspectorOptions: TOIOptions;
    // messages
    FMsgViewDblClickJumps: boolean;
    FMsgViewFocus: boolean;
    FShowMessagesIcons: boolean;
    FMsgViewStayOnTop: boolean;
    FMsgViewShowTranslations: boolean;
    FMsgViewAlwaysDrawFocused: boolean;
    FMsgViewFilenameStyle: TMsgWndFileNameStyle;
    FMsgViewColors: array[TMsgWndColor] of TColor;
    FMsgColors: array[TMessageLineUrgency] of TColor;
    FMsgViewFilters: TLMsgViewFilters;
    FMsgViewShowFPCMsgLinesCompiled: Boolean;
    // desktops
    FDesktops: TDesktopOptList;
    FDesktop: TDesktopOpt;
    FLastDesktopBeforeDebug: TDesktopOpt;
    FActiveDesktopName: string;
    FAutoSaveActiveDesktop: Boolean;
    FDebugDesktopName: string;
    function GetMsgColors(u: TMessageLineUrgency): TColor;
    function GetMsgViewColors(c: TMsgWndColor): TColor;
    procedure SetMsgColors(u: TMessageLineUrgency; AValue: TColor);
    procedure SetMsgViewColors(c: TMsgWndColor; AValue: TColor);

    function GetActiveDesktop: TDesktopOpt;
    function GetDebugDesktop: TDesktopOpt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromXml(OnlyDesktop: boolean); override;
    procedure WriteToXml(OnlyDesktop: boolean); override;
    procedure InitConfig; override;

    procedure UseDesktop(ADesktop: TDesktopOpt);
    procedure EnableDebugDesktop;
    procedure DisableDebugDesktop;
    class function DesktopCanBeLoaded(const aDockMaster: string): Boolean;

    // hints
    property ShowHintsForComponentPalette: boolean read FShowHintsForComponentPalette
                                                  write FShowHintsForComponentPalette;
    property ShowHintsForMainSpeedButtons: boolean read FShowHintsForMainSpeedButtons
                                                  write FShowHintsForMainSpeedButtons;
    // glyphs
    property ShowButtonGlyphs: TApplicationShowGlyphs read FShowButtonGlyphs write FShowButtonGlyphs;
    property ShowMenuGlyphs: TApplicationShowGlyphs read FShowMenuGlyphs write FShowMenuGlyphs;
    // form designer
    property ShowBorderSpacing: boolean read FShowBorderSpacing write FShowBorderSpacing;
    property BorderSpacingColor: TColor read FBorderSpacingColor write FBorderSpacingColor;
    property ShowGrid: boolean read FShowGrid write FShowGrid;
    property SnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property GridColor: TColor read FGridColor write FGridColor;
    property GridSizeX: integer read FGridSizeX write FGridSizeX;
    property GridSizeY: integer read FGridSizeY write FGridSizeY;
    property ShowGuideLines: boolean read FShowGuideLines write FShowGuideLines;
    property SnapToGuideLines: boolean read FSnapToGuideLines write FSnapToGuideLines;
    property GuideLineColorLeftTop: TColor read FGuideLineColorLeftTop
                                           write FGuideLineColorLeftTop;
    property GuideLineColorRightBottom: TColor read FGuideLineColorRightBottom
                                               write FGuideLineColorRightBottom;
    property ShowComponentCaptions: boolean  read FShowComponentCaptions
                                            write FShowComponentCaptions;
    property ShowEditorHints: boolean read FShowEditorHints
                                      write FShowEditorHints;
    property AutoCreateFormsOnOpen: boolean read FAutoCreateFormsOnOpen
                                            write FAutoCreateFormsOnOpen;
    property CheckPackagesOnFormCreate: boolean read FCheckPackagesOnFormCreate
                                                write FCheckPackagesOnFormCreate;
    property RightClickSelects: boolean read FRightClickSelects
                                        write FRightClickSelects;
    property GrabberColor: TColor read FGrabberColor write FGrabberColor;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property NonFormBackgroundColor: TColor read FNonFormBackgroundColor
                                            write FNonFormBackgroundColor;
    property RubberbandSelectionColor: TColor read FRubberbandSelectionColor
                                              write FRubberbandSelectionColor;
    property RubberbandCreationColor: TColor read FRubberbandCreationColor
                                             write FRubberbandCreationColor;
    property RubberbandSelectsGrandChilds: boolean read FRubberbandSelectsGrandChilds
                                                  write FRubberbandSelectsGrandChilds;
    property CreateComponentFocusNameProperty: boolean read FCreateComponentFocusNameProperty
                                                      write FCreateComponentFocusNameProperty;
    property SwitchToFavoritesOITab: boolean read FSwitchToFavoritesOITab
                                             write FSwitchToFavoritesOITab;
    property FormTitleBarChangesObjectInspector: boolean read FFormTitleBarChangesObjectInspector
                                                        write FFormTitleBarChangesObjectInspector;
    property ForceDPIScalingInDesignTime: boolean read FForceDPIScalingInDesignTime
                                                        write FForceDPIScalingInDesignTime;
    // component list
    property ComponentListKeepOpen: Boolean read FComponentListKeepOpen write FComponentListKeepOpen;
    property ComponentListPageIndex: Integer read FComponentListPageIndex write FComponentListPageIndex;
    // object inspector
    property ObjectInspectorOptions: TOIOptions read FObjectInspectorOptions;
    // messages view
    property MsgViewDblClickJumps: boolean read FMsgViewDblClickJumps
      write FMsgViewDblClickJumps; // true=dbl click jump to error, false=single click jumps
    property MsgViewFocus: boolean read FMsgViewFocus
      write FMsgViewFocus; // when showing the message window, focus it
    property ShowMessagesIcons: boolean read FShowMessagesIcons write FShowMessagesIcons;
    property MsgViewStayOnTop: boolean read FMsgViewStayOnTop write FMsgViewStayOnTop;
    property MsgViewShowTranslations: boolean read FMsgViewShowTranslations
             write FMsgViewShowTranslations;
    property MsgViewAlwaysDrawFocused: boolean read FMsgViewAlwaysDrawFocused
             write FMsgViewAlwaysDrawFocused;
    property MsgViewFilenameStyle: TMsgWndFileNameStyle read FMsgViewFilenameStyle
                                                       write FMsgViewFilenameStyle;
    property MsgViewColors[c: TMsgWndColor]: TColor read GetMsgViewColors write SetMsgViewColors;
    property MsgViewFilters: TLMsgViewFilters read FMsgViewFilters;
    property MsgColors[u: TMessageLineUrgency]: TColor read GetMsgColors write SetMsgColors;
    property MsgViewShowFPCMsgLinesCompiled: Boolean read FMsgViewShowFPCMsgLinesCompiled write FMsgViewShowFPCMsgLinesCompiled;
    // desktops
    property Desktops: TDesktopOptList read FDesktops;
    property Desktop: TDesktopOpt read FDesktop;               // the working desktop, standalone
    property DebugDesktopName: string read FDebugDesktopName write FDebugDesktopName;
    property DebugDesktop: TDesktopOpt read GetDebugDesktop;   // debug desktop from Desktops list
    property LastDesktopBeforeDebug: TDesktopOpt read FLastDesktopBeforeDebug write FLastDesktopBeforeDebug;
    property ActiveDesktopName: string read FActiveDesktopName write FActiveDesktopName;
    property ActiveDesktop: TDesktopOpt read GetActiveDesktop; // active desktop from Desktops list
    property AutoSaveActiveDesktop: Boolean read FAutoSaveActiveDesktop write FAutoSaveActiveDesktop;
  end;

  function StrToMsgWndFilenameStyle(const s: string): TMsgWndFileNameStyle;

var
  EnvironmentGuiOpts: TEnvGuiOptions;


implementation

function StrToMsgWndFilenameStyle(const s: string): TMsgWndFileNameStyle;
begin
  for Result in TMsgWndFileNameStyle do
    if CompareText(s,MsgWndFileNameStyleNames[Result])=0 then exit;
  Result:=mwfsShort;
end;

{ TCustomDesktopOpt }

function TCustomDesktopOpt.GetCompatible: Boolean;
begin
  Result := false;
end;

procedure TCustomDesktopOpt.Load(Path: String);
begin
  FAssociatedDebugDesktopName:=FXMLCfg.GetValue(Path+'AssociatedDebugDesktopName/Value', '');
end;

constructor TCustomDesktopOpt.Create(const aName: String);
begin
  Create(aName, Assigned(IDEDockMaster));
end;

constructor TCustomDesktopOpt.Create(const aName: String; const aIsDocked: Boolean);
begin
  inherited Create;
  FName:=aName;
  FIsDocked := aIsDocked;
end;

destructor TCustomDesktopOpt.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomDesktopOpt.SetConfig(aXMLCfg: TRttiXMLConfig;
  aConfigStore: TXMLOptionsStorage);
begin
  FXMLCfg := aXMLCfg;
  FConfigStore := aConfigStore;
end;

{ TDesktopOIOptions }

constructor TDesktopOIOptions.Create;
var
  I: TObjectInspectorPage;
begin
  FComponentTreeHeight := -1;
  FInfoBoxHeight := -1;
  FShowInfoBox := True;
  for I in TObjectInspectorPage do
    SplitterX[I] := -1;
end;

procedure TDesktopOIOptions.AssignTo(Dest: TPersistent);
var
  DDest: TDesktopOIOptions;
  I: TObjectInspectorPage;
begin
  if Dest is TDesktopOIOptions then
  begin
    DDest := TDesktopOIOptions(Dest);

    for I in TObjectInspectorPage do
      DDest.SplitterX[I] := SplitterX[I];
    DDest.ShowInfoBox := ShowInfoBox;
    DDest.ComponentTreeHeight := ComponentTreeHeight;
    DDest.ShowComponentTree := ShowComponentTree;
    DDest.InfoBoxHeight := InfoBoxHeight;
  end else
    inherited AssignTo(Dest);
end;

function TDesktopOIOptions.GetSplitterX(const APage: TObjectInspectorPage): Integer;
begin
  Result := FSplitterX[APage];
end;

procedure TDesktopOIOptions.ImportSettingsFromIDE(const AOptions: TEnvGuiOptions);
var
  I: TObjectInspectorPage;
  o: TOIOptions;
begin
  o := AOptions.ObjectInspectorOptions;
  for I in TObjectInspectorPage do
    FSplitterX[I] := o.GridSplitterX[I];

  ShowInfoBox := o.ShowInfoBox;
  ComponentTreeHeight := o.ComponentTreeHeight;
  ShowComponentTree := o.ShowComponentTree;
  InfoBoxHeight := o.InfoBoxHeight;
end;

procedure TDesktopOIOptions.ExportSettingsToIDE(const AOptions: TEnvGuiOptions);
var
  I: TObjectInspectorPage;
  o: TOIOptions;
begin
  o := AOptions.ObjectInspectorOptions;
  for I in TObjectInspectorPage do
    if FSplitterX[I]>=0 then
      o.GridSplitterX[I] := Max(10, FSplitterX[I]);

  o.ShowInfoBox := ShowInfoBox;
  o.ShowComponentTree := ShowComponentTree;
  if ComponentTreeHeight>=0 then
    o.ComponentTreeHeight := Max(10, ComponentTreeHeight);
  if InfoBoxHeight>=0 then
    o.InfoBoxHeight := Max(10, InfoBoxHeight);
end;

procedure TDesktopOIOptions.Load(XMLConfig: TXMLConfig; Path: String);
var
  I: TObjectInspectorPage;
begin
  Path := Path + 'ObjectInspectorOptions/';
  for I in TObjectInspectorPage do
    FSplitterX[I] := XMLConfig.GetValue(Path+'SplitterX/'+DefaultOIPageNames[I]+'/Value',-1);
  ShowComponentTree := XMLConfig.GetValue(Path+'ComponentTree/Show/Value',True);
  ComponentTreeHeight := XMLConfig.GetValue(Path+'ComponentTree/Height/Value',-1);
  ShowInfoBox := XMLConfig.GetValue(Path+'InfoBox/Show/Value',True);
  InfoBoxHeight := XMLConfig.GetValue(Path+'InfoBox/Height/Value',-1);
end;

procedure TDesktopOIOptions.Save(XMLConfig: TXMLConfig; Path: String);
var
  I: TObjectInspectorPage;
begin
  Path := Path + 'ObjectInspectorOptions/';
  for I in TObjectInspectorPage do
    XMLConfig.SetDeleteValue(Path+'SplitterX/'+DefaultOIPageNames[I]+'/Value',FSplitterX[I],-1);
  XMLConfig.SetDeleteValue(Path+'ComponentTree/Show/Value',ShowComponentTree,True);
  XMLConfig.SetDeleteValue(Path+'ComponentTree/Height/Value',ComponentTreeHeight,-1);
  XMLConfig.SetDeleteValue(Path+'InfoBox/Show/Value',ShowInfoBox,True);
  XMLConfig.SetDeleteValue(Path+'InfoBox/Height/Value',InfoBoxHeight,-1);
end;

procedure TDesktopOIOptions.SetSplitterX(const APage: TObjectInspectorPage;
  const ASplitterX: Integer);
begin
  FSplitterX[APage] := ASplitterX;
end;

{ TDesktopOpt }

constructor TDesktopOpt.Create(const aName: String; const aIsDocked: Boolean);
begin
  if aIsDocked and not Assigned(IDEDockMaster) then
    raise Exception.Create('Internal error: TEnvironmentOptions.CreateDesktop cannot create docked desktop in undocked environment.');

  inherited;

  if aIsDocked then
    FDockedOpt := IDEDockMaster.DockedDesktopOptClass.Create;

  FSingleTaskBarButton:=false;
  FHideIDEOnRun:=false;
  FAutoAdjustIDEHeight:=true;
  FAutoAdjustIDEHeightFullCompPal := true;
  // window menu
  FIDENameForDesignedFormList:=false;
  // CompletionWindow
  FCompletionWindowWidth := 320 * Screen.PixelsPerInch div 96;
  FCompletionWindowHeight := 6;
  // title
  FIDETitleStartsWithProject:=false;
  FIDETitleIncludesBuildMode:=false;
  FIDEProjectDirectoryInIdeTitle:=false;
  // IDE Coolbar
  FIDECoolBarOptions:=TIDECoolBarOptions.Create;
  // Editor Toolbar
  FEditorToolBarOptions:=TEditorToolBarOptions.Create;
  // component palette
  FComponentPaletteOptions:=TCompPaletteOptions.Create;
  // object inspector
  FObjectInspectorOptions:=TDesktopOIOptions.Create;
  // project inspector
  FProjectInspectorShowProps := true;
  // Windows layout
  InitLayoutList;

  FIDEDialogLayoutList:=TIDEDialogLayoutList.Create;
  FIDEWindowCreatorsLayoutList:=TSimpleWindowLayoutList.Create(False);
  FIDEDialogLayoutList.Assign(IDEWindowIntf.IDEDialogLayoutList);
  FIDEWindowCreatorsLayoutList.CopyItemsFrom(IDEWindowIntf.IDEWindowCreators.SimpleLayoutStorage);
end;

destructor TDesktopOpt.Destroy;
begin
  FreeAndNil(FComponentPaletteOptions);
  FreeAndNil(FEditorToolBarOptions);
  FreeAndNil(FIDECoolBarOptions);
  FreeAndNil(FDockedOpt);
  FreeAndNil(FObjectInspectorOptions);

  FreeAndNil(FIDEDialogLayoutList);
  FreeAndNil(FIDEWindowCreatorsLayoutList);

  inherited Destroy;
end;

function TDesktopOpt.GetCompatible: Boolean;
begin
  Result := (IsDocked = Assigned(IDEDockMaster));
end;

procedure TDesktopOpt.Assign(Source: TDesktopOpt; const AssignName: Boolean;
  const IsCompatible: Boolean);
begin
  if AssignName then
    FName := Source.FName;

  if IsCompatible and (Assigned(FDockedOpt) <> Assigned(Source.FDockedOpt)) then
    raise Exception.Create('Internal error: TDesktopOpt.Assign mixed docked/undocked desktops.');

  // window layout
  if IsCompatible then
  begin
    FIDEWindowCreatorsLayoutList.CopyItemsFrom(Source.FIDEWindowCreatorsLayoutList);
    FIDEDialogLayoutList.Assign(Source.FIDEDialogLayoutList);
    FAssociatedDebugDesktopName := Source.FAssociatedDebugDesktopName;
  end;
  FSingleTaskBarButton := Source.FSingleTaskBarButton;
  FHideIDEOnRun := Source.FHideIDEOnRun;
  FAutoAdjustIDEHeight := Source.FAutoAdjustIDEHeight;
  FAutoAdjustIDEHeightFullCompPal := Source.FAutoAdjustIDEHeightFullCompPal;
  // window menu
  FIDENameForDesignedFormList := Source.FIDENameForDesignedFormList;
  // CompletionWindow
  FCompletionWindowWidth := Source.FCompletionWindowWidth;
  FCompletionWindowHeight := Source.FCompletionWindowHeight;
  // title
  FIDETitleStartsWithProject := Source.FIDETitleStartsWithProject;
  FIDETitleIncludesBuildMode := Source.FIDETitleIncludesBuildMode;
  FIDEProjectDirectoryInIdeTitle := Source.FIDEProjectDirectoryInIdeTitle;
  // IDE Coolbar
  FIDECoolBarOptions.Assign(Source.FIDECoolBarOptions);
  // Editor Toolbar
  FEditorToolBarOptions.Assign(Source.FEditorToolBarOptions);
  // component palette
  FComponentPaletteOptions.Assign(Source.FComponentPaletteOptions);
  // object inspector
  FObjectInspectorOptions.Assign(Source.FObjectInspectorOptions);
  // project inspector
  FProjectInspectorShowProps := Source.FProjectInspectorShowProps;

  if IsCompatible and Assigned(FDockedOpt) then
    FDockedOpt.Assign(Source.FDockedOpt);
end;

procedure TDesktopOpt.Load(Path: String);
begin
  inherited;

  // Windows layout
  FIDEWindowCreatorsLayoutList.LoadFromConfig(FConfigStore, Path);
  FIDEDialogLayoutList.LoadFromConfig(FConfigStore, Path+'Dialogs/');

  FSingleTaskBarButton:=FXMLCfg.GetValue(Path+'SingleTaskBarButton/Value', False);
  FHideIDEOnRun:=FXMLCfg.GetValue(Path+'HideIDEOnRun/Value',false);
  FAutoAdjustIDEHeight:=FXMLCfg.GetValue(Path+'AutoAdjustIDEHeight/Value',true);
  FAutoAdjustIDEHeightFullCompPal:=FXMLCfg.GetValue(Path+'AutoAdjustIDEHeightFullComponentPalette/Value',true);
  // Window menu
  FIDENameForDesignedFormList:=FXMLCfg.GetValue(Path+'IDENameForDesignedFormList/Value',false);
  // title
  FIDETitleStartsWithProject:=FXMLCfg.GetValue(Path+'IDETitleStartsWithProject/Value',false);
  FIDETitleIncludesBuildMode:=FXMLCfg.GetValue(Path+'IDETitleIncludesBuildMode/Value',false);
  FIDEProjectDirectoryInIdeTitle:=FXMLCfg.GetValue(Path+'IDEProjectDirectoryInIdeTitle/Value',false);
  // CompletionWindow
  FCompletionWindowWidth:=FXMLCfg.GetValue(Path+'CompletionWindowOptions/Width/Value', FCompletionWindowWidth);
  FCompletionWindowHeight:=FXMLCfg.GetValue(Path+'CompletionWindowOptions/Height/Value', 6);
  // Project Inspector
  FProjectInspectorShowProps := FXMLCfg.GetValue(Path+'ProjectInspectorShowProps/Value', true);

  if not FXMLCfg.HasPath(Path+'IDECoolBarOptions/', True) then
    Path := '';             // Toolbars and palette were at the top level in XML.
  // IDE Coolbar
  FIDECoolBarOptions.Load(FXMLCfg, Path);
  // Editor Toolbar
  FEditorToolBarOptions.Load(FXMLCfg, Path);
  // component palette
  FComponentPaletteOptions.Load(FXMLCfg, Path);
  // Object Inspector
  FObjectInspectorOptions.Load(FXMLCfg, Path);

  if Assigned(FDockedOpt) then
    FDockedOpt.Load(Path, FXMLCfg);
end;

procedure TDesktopOpt.RestoreDesktop;
begin
  IDEWindowCreators.RestoreSimpleLayout;
  if Assigned(FDockedOpt) then
    FDockedOpt.RestoreDesktop;
end;

procedure TDesktopOpt.Save(Path: String);
begin
  // windows
  FXMLCfg.SetDeleteValue(Path+'Name', FName, '');
  if Assigned(FDockedOpt) then
    FXMLCfg.SetDeleteValue(Path+'DockMaster', IDEDockMaster.ClassName, '')
  else
    FXMLCfg.DeleteValue(Path+'DockMaster');

  FIDEWindowCreatorsLayoutList.SaveToConfig(FConfigStore, Path);
  FIDEDialogLayoutList.SaveToConfig(FConfigStore,Path+'Dialogs/');

  FXMLCfg.SetDeleteValue(Path+'AssociatedDebugDesktopName/Value', FAssociatedDebugDesktopName, '');
  FXMLCfg.SetDeleteValue(Path+'SingleTaskBarButton/Value',FSingleTaskBarButton, False);
  FXMLCfg.SetDeleteValue(Path+'HideIDEOnRun/Value',FHideIDEOnRun,false);
  FXMLCfg.SetDeleteValue(Path+'AutoAdjustIDEHeight/Value',FAutoAdjustIDEHeight,true);
  FXMLCfg.SetDeleteValue(Path+'AutoAdjustIDEHeightFullComponentPalette/Value',
                           FAutoAdjustIDEHeightFullCompPal,true);
  // Window menu
  FXMLCfg.SetDeleteValue(Path+'IDENameForDesignedFormList/Value',FIDENameForDesignedFormList,false);
  // title
  FXMLCfg.SetDeleteValue(Path+'IDETitleStartsWithProject/Value',FIDETitleStartsWithProject,false);
  FXMLCfg.SetDeleteValue(Path+'IDETitleIncludesBuildMode/Value',FIDETitleIncludesBuildMode,false);
  FXMLCfg.SetDeleteValue(Path+'IDEProjectDirectoryInIdeTitle/Value',FIDEProjectDirectoryInIdeTitle,false);
  // CompletionWindow
  FXMLCfg.SetValue(Path+'CompletionWindowOptions/Width/Value',FCompletionWindowWidth);
  FXMLCfg.SetDeleteValue(Path+'CompletionWindowOptions/Height/Value',FCompletionWindowHeight, 6);
  // Project Inspector
  FXMLCfg.SetDeleteValue(Path+'ProjectInspectorShowProps/Value', FProjectInspectorShowProps, true);
  // IDE Coolbar
  FIDECoolBarOptions.Save(FXMLCfg, Path);
  // Editor Toolbar
  FEditorToolBarOptions.Save(FXMLCfg, Path);
  // component palette
  FComponentPaletteOptions.Save(FXMLCfg, Path);
  // Object Inspector
  FObjectInspectorOptions.Save(FXMLCfg, Path);

  if Assigned(FDockedOpt) then
    FDockedOpt.Save(Path, FXMLCfg);
end;

procedure TDesktopOpt.ImportSettingsFromIDE(const AOptions: TEnvGuiOptions);
begin
  IDEWindowIntf.IDEWindowCreators.SimpleLayoutStorage.StoreWindowPositions;
  FIDEDialogLayoutList.Assign(IDEWindowIntf.IDEDialogLayoutList);
  FIDEWindowCreatorsLayoutList.CopyItemsFrom(IDEWindowIntf.IDEWindowCreators.SimpleLayoutStorage);
  FObjectInspectorOptions.ImportSettingsFromIDE(AOptions);

  if Assigned(FDockedOpt) then
    FDockedOpt.ImportSettingsFromIDE;
end;

procedure TDesktopOpt.ExportSettingsToIDE(const AOptions: TEnvGuiOptions);
var
  ComplForm: TCustomForm;
begin
  if Assigned(FDockedOpt) then
    FDockedOpt.ExportSettingsToIDE;

  IDEWindowIntf.IDEDialogLayoutList.Assign(FIDEDialogLayoutList);
  IDEWindowIntf.IDEWindowCreators.SimpleLayoutStorage.CopyItemsFrom(FIDEWindowCreatorsLayoutList);
  FObjectInspectorOptions.ExportSettingsToIDE(AOptions);
  if Assigned(SourceEditorManagerIntf) then
  begin
    ComplForm := SourceEditorManagerIntf.DefaultSynCompletionForm;
    if Assigned(ComplForm) then
      ComplForm.Width := Max(50, CompletionWindowWidth);
    SourceEditorManagerIntf.SynCompletionLinesInWindow := Max(3, CompletionWindowHeight);
  end;
end;

procedure InitLayoutHelper(const FormID: string);
begin
  with IDEWindowCreators.SimpleLayoutStorage do
    if not Assigned(ItemByFormID(FormID)) then
      CreateWindowLayout(FormID);
end;

procedure TDesktopOpt.InitLayoutList;
var
  l: TNonModalIDEWindow;
begin
  for l:=Low(TNonModalIDEWindow) to High(TNonModalIDEWindow) do
    if l<>nmiwNone then
      InitLayoutHelper(NonModalIDEWindowNames[l]);
  InitLayoutHelper(DefaultObjectInspectorName);
end;

{ TUnsupportedDesktopOpt }

destructor TUnsupportedDesktopOpt.Destroy;
begin
  freeandnil(FRetainXMLData);
  inherited Destroy;
end;

procedure TUnsupportedDesktopOpt.Load(Path: string);
var
  lPnode, lChldNode: TDOMNode;
begin
  inherited;

  FreeAndNil(FRetainXMLData);
  FRetainXMLData := TDOMDocument.Create;
  lPnode := FXMLCfg.FindNode(Path, False);
  lChldNode := lPnode.CloneNode(True, FRetainXMLData);
  FRetainXMLData.AppendChild(lChldNode);
end;

procedure TUnsupportedDesktopOpt.Save(Path: string);
var
  lChldNode, lChCh: TDOMNode;
  lsNodeName: DOMString;
  lParentNode: TDOMNode;
  PathM1, PreD, LastD: String;
begin
  if Assigned(FRetainXMLData)  then
  begin
    PathM1 := copy(Path, 1, length(Path) - 1);
    PreD := ExtractFilePath(PathM1);
    LastD := ExtractFileNameOnly(PathM1);
    lParentNode:= FXMLCfg.FindNode(path, False);
    lChldNode := FRetainXMLData.FirstChild.CloneNode(True, FXMLCfg.Document);
    lsNodeName := lChldNode.NodeName;
    if LastD = lsNodeName then
      FXMLCfg.FindNode(PreD,False).ReplaceChild(lChldNode,FXMLCfg.FindNode(Path,False))
    else
    begin
      try
        if not assigned(lParentNode) then
        begin
          lParentNode:=FXMLCfg.Document.CreateElement(LastD);
          FXMLCfg.FindNode(PreD, False).AppendChild(lParentNode);
        end;
        while lChldNode.HasChildNodes do
        begin
          lChCh := lChldNode.FirstChild;
          lChldNode.DetachChild(lChCh);
          lParentNode.AppendChild(lChCh);
        end;
      finally
        FreeAndNil(lChldNode);
      end;
    end;
  end;
end;

{ TDesktopOptList }

constructor TDesktopOptList.Create;
begin
  inherited Create;
end;

destructor TDesktopOptList.Destroy;
begin
  inherited Destroy;
end;

procedure TDesktopOptList.SetConfig(aXMLCfg: TRttiXMLConfig; aConfigStore: TXMLOptionsStorage);
begin
  FXMLCfg := aXMLCfg;
  FConfigStore := aConfigStore;
end;

procedure TDesktopOptList.AddFromCfg(Path: String);
var
  dsk: TCustomDesktopOpt;
  dskClass: TDesktopOptClass;
  dskName, dskDockMaster: String;
begin
  dskName := FXMLCfg.GetValue(Path+'Name', 'default');
  dskDockMaster := FXMLCfg.GetValue(Path+'DockMaster', '');

  if IndexOf(dskname) >=0 then
    exit;

  if TEnvGuiOptions.DesktopCanBeLoaded(dskDockMaster) then
     dskClass := TDesktopOpt
   else
     dskClass := TUnsupportedDesktopOpt;

  dsk := dskClass.Create(dskName, dskDockMaster<>'');
  dsk.SetConfig(FXMLCfg, FConfigStore);
  dsk.Load(Path);
  Add(dsk);
end;

function TDesktopOptList.IndexOf(aName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0)
  and (CompareText(aName, Items[Result].Name)<>0) do
    dec(Result);
end;

function TDesktopOptList.Find(aName: string): TCustomDesktopOpt;
var
  i: LongInt;
begin
  i:=IndexOf(aName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TDesktopOptList.GetItem(Index: Integer): TCustomDesktopOpt;
begin
  Result := TCustomDesktopOpt(inherited Items[Index]);
end;

{ TEnvGuiOptions }

constructor TEnvGuiOptions.Create;
var
  c: TMsgWndColor;
  u: TMessageLineUrgency;
begin
  FShowHintsForComponentPalette:=true;
  FShowHintsForMainSpeedButtons:=true;
  // glyphs
  FShowButtonGlyphs := sbgSystem;
  FShowMenuGlyphs := sbgSystem;
  // designer
  FShowGrid:=true;
  FShowBorderSpacing:=false;
  FBorderSpacingColor:=DefaultBorderSpacingColor;
  FGridColor:=DefaultGridColor;
  FSnapToGrid:=true;
  FGridSizeX:=DefaultGridSize;
  FGridSizeY:=DefaultGridSize;
  FShowGuideLines:=true;
  FSnapToGuideLines:=true;
  FGuideLineColorLeftTop:=DefaultGuideLineColorLeftTop;
  FGuideLineColorRightBottom:=DefaultGuideLineColorRightBottom;
  FShowComponentCaptions:=false;
  FShowEditorHints:=true;
  FAutoCreateFormsOnOpen:=true;
  FCheckPackagesOnFormCreate:=true;
  FRightClickSelects:=true;
  FGrabberColor:=clBlack;
  FMarkerColor:=clDkGray;
  FNonFormBackgroundColor:=clWindow;
  FRubberbandSelectionColor:=clNavy;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectsGrandChilds:=DefaultRubberbandSelectsGrandChilds;
  FCreateComponentFocusNameProperty:=false;
  FSwitchToFavoritesOITab:=false;
  FFormTitleBarChangesObjectInspector:=false;
  FForceDPIScalingInDesignTime:=true;
  // object inspector
  FObjectInspectorOptions:=TOIOptions.Create;
  // messages view
  fMsgViewDblClickJumps:=true;
  fMsgViewFocus:=DefaultMsgViewFocus;
  FShowMessagesIcons:=true;
  FMsgViewStayOnTop:=false;
  FMsgViewShowTranslations:=false;
  FMsgViewAlwaysDrawFocused:=false;
  FMsgViewFilenameStyle:=mwfsShort;
  for c:=low(TMsgWndColor) to high(TMsgWndColor) do
    fMsgViewColors[c]:=MsgWndDefaultColors[c];
  for u:=low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    fMsgColors[u] := clDefault;
  FMsgViewFilters:=TLMsgViewFilters.Create(nil);
  FMsgViewShowFPCMsgLinesCompiled:=false;
  // Desktop collection
  FDesktops := TDesktopOptList.Create;
  // FDesktop points to the IDE properties
  FDesktop := TDesktopOpt.Create('');
  FAutoSaveActiveDesktop := True;
end;

destructor TEnvGuiOptions.Destroy;
begin
  FreeAndNil(FDesktop);
  FreeAndNil(FDesktops);
  FreeAndNil(FMsgViewFilters);
  FreeAndNil(FObjectInspectorOptions);
  FreeAndNil(FConfigStorage);
  inherited Destroy;
end;

procedure TEnvGuiOptions.ReadFromXml(OnlyDesktop: boolean);
var
  mwc: TMsgWndColor;
  Rec: PIDEOptionsGroupRec;
  Path, CurPath, NodeName: String;
  u: TMessageLineUrgency;
  i, j: Integer;
begin
  Path:='EnvironmentOptions/';
  // hints
  FShowHintsForComponentPalette:=XMLCfg.GetValue(Path+'ShowHintsForComponentPalette/Value',true);
  FShowHintsForMainSpeedButtons:=XMLCfg.GetValue(Path+'ShowHintsForMainSpeedButtons/Value',true);
  // glyphs
  FShowButtonGlyphs := TApplicationShowGlyphs(XMLCfg.GetValue(Path+'ShowButtonGlyphs/Value',
    Ord(sbgSystem)));
  FShowMenuGlyphs := TApplicationShowGlyphs(XMLCfg.GetValue(Path+'ShowMenuGlyphs/Value',
    Ord(sbgSystem)));
  // form editor
  FShowGrid:=XMLCfg.GetValue(Path+'FormEditor/ShowGrid',true);
  FShowBorderSpacing:=XMLCfg.GetValue(Path+'FormEditor/ShowBorderSpacing',false);
  FBorderSpacingColor:=XMLCfg.GetValue(Path+'FormEditor/BorderSpacingColor',DefaultBorderSpacingColor);
  FGridColor:=XMLCfg.GetValue(Path+'FormEditor/GridColor',DefaultGridColor);
  FSnapToGrid:=XMLCfg.GetValue(Path+'FormEditor/SnapToGrid',true);
  FGridSizeX:=XMLCfg.GetValue(Path+'FormEditor/GridSizeX',DefaultGridSize);
  FGridSizeY:=XMLCfg.GetValue(Path+'FormEditor/GridSizeY',DefaultGridSize);
  FShowGuideLines:=XMLCfg.GetValue(Path+'FormEditor/ShowGuideLines',true);
  FSnapToGuideLines:=XMLCfg.GetValue(Path+'FormEditor/SnapToGuideLines',true);
  FGuideLineColorLeftTop:=XMLCfg.GetValue(Path+'FormEditor/GuideLineColorLeftTop',
     DefaultGuideLineColorLeftTop);
  FGuideLineColorRightBottom:=XMLCfg.GetValue(Path+'FormEditor/GuideLineColorRightBottom',
     DefaultGuideLineColorRightBottom);
  FShowComponentCaptions:=XMLCfg.GetValue(Path+'FormEditor/ShowComponentCaptions',true);
  FShowEditorHints:=XMLCfg.GetValue(Path+'FormEditor/ShowEditorHints',true);
  FAutoCreateFormsOnOpen:=XMLCfg.GetValue(Path+'FormEditor/AutoCreateFormsOnOpen',true);
  FCheckPackagesOnFormCreate:=XMLCfg.GetValue(Path+'FormEditor/CheckPackagesOnFormCreate',true);
  FRightClickSelects:=XMLCfg.GetValue(Path+'FormEditor/RightClickSelects',true);
  FGrabberColor:=XMLCfg.GetValue(Path+'FormEditor/GrabberColor/Value',FGrabberColor);
  FMarkerColor:=XMLCfg.GetValue(Path+'FormEditor/MarkerColor/Value',FMarkerColor);
  FNonFormBackgroundColor:=XMLCfg.GetValue(Path+'FormEditor/NonFormBackgroundColor/Value',FNonFormBackgroundColor);
  FRubberbandSelectionColor:=XMLCfg.GetValue(Path+'FormEditor/Rubberband/SelectionColor/Value',
     FRubberbandSelectionColor);
  FRubberbandCreationColor:=XMLCfg.GetValue(Path+'FormEditor/Rubberband/CreationColor/Value',
     FRubberbandCreationColor);
  FRubberbandSelectsGrandChilds:=XMLCfg.GetValue(Path+'FormEditor/Rubberband/SelectsGrandChilds/Value',DefaultRubberbandSelectsGrandChilds);
  FCreateComponentFocusNameProperty:=XMLCfg.GetValue(
     Path+'FormEditor/CreateComponentFocusNameProperty/Value',false);
  FSwitchToFavoritesOITab:=XMLCfg.GetValue(Path+'FormEditor/SwitchToFavoritesOITab/Value',false);
  FFormTitleBarChangesObjectInspector:=XMLCfg.GetValue(Path+'FormEditor/FormTitleBarChangesObjectInspector/Value',false);
  FForceDPIScalingInDesignTime:=XMLCfg.GetValue(Path+'FormEditor/ForceDPIScalingInDesignTime/Value',true);
  // component list
  FComponentListKeepOpen:=XMLCfg.GetValue(Path+'ComponentList/KeepOpen',false);
  FComponentListPageIndex:=XMLCfg.GetValue(Path+'ComponentList/PageIndex',0);
  // object inspector
  FObjectInspectorOptions.Load;
  FObjectInspectorOptions.SaveBounds:=false;
  // messages view
  fMsgViewDblClickJumps:=XMLCfg.GetValue(Path+'MsgViewDblClickJumps/Value',false);
  fMsgViewFocus:=XMLCfg.GetValue(Path+'MsgViewFocus/Value',DefaultMsgViewFocus);
  FShowMessagesIcons:=XMLCfg.GetValue(Path+'MsgView/ShowMessagesIcons/Value',true);
  FMsgViewStayOnTop:=XMLCfg.GetValue(Path+'MsgView/StayOnTop/Value',false);
  FMsgViewShowTranslations:=XMLCfg.GetValue(Path+'MsgView/ShowTranslations/Value',false);
  FMsgViewAlwaysDrawFocused:=XMLCfg.GetValue(Path+'MsgView/AlwaysDrawFocused/Value',false);
  FMsgViewFilenameStyle:=StrToMsgWndFilenameStyle(XMLCfg.GetValue(
    Path+'MsgView/Filename/Style',MsgWndFileNameStyleNames[mwfsShort]));
  for mwc:=low(TMsgWndColor) to high(TMsgWndColor) do
    fMsgViewColors[mwc]:=XMLCfg.GetValue(
      Path+'MsgView/Colors/'+MsgWndColorNames[mwc],MsgWndDefaultColors[mwc]);
  for u:=low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    fMsgColors[u] := XMLCfg.GetValue(
      Path+'MsgView/MsgColors/'+dbgs(u),clDefault);
  FMsgViewFilters.LoadFromXMLConfig(XMLCfg,'MsgView/Filters/');
  FMsgViewShowFPCMsgLinesCompiled:=XMLCfg.GetValue(Path+'MsgView/FPCMsg/ShowLinesCompiled',false);
  // IDEEditorGroups
  for i := 0 to IDEEditorGroups.Count-1 do
  begin
    Rec := IDEEditorGroups[i];
    NodeName := Rec^.GroupClass.ClassName;
    Rec^.Collapsed := XMLCfg.GetValue(Path+'OptionDialog/Tree/' + NodeName + '/Value',
                                         Rec^.DefaultCollapsed);
    if Rec^.Items <> nil then begin
      for j := 0 to Rec^.Items.Count-1 do begin
        Rec^.Items[j]^.Collapsed := XMLCfg.GetValue(Path+'OptionDialog/Tree/' + NodeName
              + '/' + Rec^.Items[j]^.EditorClass.ClassName + '/Value',
              Rec^.Items[j]^.DefaultCollapsed);
      end;
    end;
  end;

  // The user can define many desktops. They are saved under path Desktops/.
  FDesktops.Clear;
  FDesktops.SetConfig(XMLCfg, FConfigStorage);
  FActiveDesktopName := '';
  if Version<109 then begin
    //load old default desktop - backwards compatibility - or create a new default desktop
    CurPath := 'Desktop/';               // New place: Desktop/
    if not XMLCfg.HasPath(CurPath, True) then
      CurPath := Path+'Desktop/';        // Old place: EnvironmentOptions/Desktop/
    if XMLCfg.HasPath(CurPath, True) or//default desktop exists in the settings
       ((ActiveDesktop.IDECoolBarOptions.ToolBars.Count = 0) and
        (ActiveDesktop.FIDEDialogLayoutList.Count = 0))//desktop is empty, load it to recreate!
    then
    begin
      ActiveDesktop.SetConfig(XMLCfg, FConfigStorage);
      ActiveDesktop.Load(CurPath);
    end;
  end else begin
    CurPath := 'Desktops/';
    FDebugDesktopName := XMLCfg.GetValue(CurPath+'DebugDesktop', '');
    FActiveDesktopName := XMLCfg.GetValue(CurPath+'ActiveDesktop', '');
    j := XMLCfg.GetValue(CurPath+'Count', 1);
    for i := 1 to j do
      FDesktops.AddFromCfg(CurPath+'Desktop'+IntToStr(i)+'/');
  end;
  if Version<=109 then begin
    XMLCfg.DeletePath('Desktop');
    XMLCfg.DeletePath(CurPath+'Desktop');
  end;
  FDesktop.Assign(ActiveDesktop, False);
  FDesktop.ExportSettingsToIDE(Self);
end;

procedure TEnvGuiOptions.WriteToXml(OnlyDesktop: boolean);
var
  mwc: TMsgWndColor;
  Rec: PIDEOptionsGroupRec;
  Path, CurPath, NodeName: String;
  u: TMessageLineUrgency;
  i, j: Integer;
  xSaveDesktop: TCustomDesktopOpt;
  xActiveDesktopName: string;
begin
  Path:='EnvironmentOptions/';
  // hints
  XMLCfg.SetDeleteValue(Path+'ShowHintsForComponentPalette/Value',FShowHintsForComponentPalette,true);
  XMLCfg.SetDeleteValue(Path+'ShowHintsForMainSpeedButtons/Value',FShowHintsForMainSpeedButtons,true);
  // glyphs
  XMLCfg.SetDeleteValue(Path+'ShowButtonGlyphs/Value',Ord(FShowButtonGlyphs), Ord(sbgSystem));
  XMLCfg.SetDeleteValue(Path+'ShowMenuGlyphs/Value',Ord(FShowMenuGlyphs), Ord(sbgSystem));
  // form editor
  XMLCfg.SetDeleteValue(Path+'FormEditor/ShowBorderSpacing',FShowBorderSpacing,false);
  XMLCfg.SetDeleteValue(Path+'FormEditor/BorderSpacingColor',FBorderSpacingColor,DefaultBorderSpacingColor);
  XMLCfg.SetDeleteValue(Path+'FormEditor/ShowGrid',FShowGrid,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GridColor',FGridColor,DefaultGridColor);
  XMLCfg.SetDeleteValue(Path+'FormEditor/SnapToGrid',FSnapToGrid,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GridSizeX',FGridSizeX,DefaultGridSize);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GridSizeY',FGridSizeY,DefaultGridSize);
  XMLCfg.SetDeleteValue(Path+'FormEditor/ShowGuideLines',FShowGuideLines,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/SnapToGuideLines',FSnapToGuideLines,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GuideLineColorLeftTop',FGuideLineColorLeftTop,DefaultGuideLineColorLeftTop);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GuideLineColorRightBottom',FGuideLineColorRightBottom,DefaultGuideLineColorRightBottom);
  XMLCfg.SetDeleteValue(Path+'FormEditor/ShowComponentCaptions',FShowComponentCaptions,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/ShowEditorHints',FShowEditorHints,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/AutoCreateFormsOnOpen',FAutoCreateFormsOnOpen,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/CheckPackagesOnFormCreate',FCheckPackagesOnFormCreate,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/RightClickSelects',FRightClickSelects,true);
  XMLCfg.SetDeleteValue(Path+'FormEditor/GrabberColor/Value',FGrabberColor,clBlack);
  XMLCfg.SetDeleteValue(Path+'FormEditor/MarkerColor/Value',FMarkerColor,clDkGray);
  XMLCfg.SetDeleteValue(Path+'FormEditor/NonFormBackgroundColor/Value',FNonFormBackgroundColor,clWindow);
  XMLCfg.SetDeleteValue(Path+'FormEditor/Rubberband/SelectionColor/Value',
     FRubberbandSelectionColor,clBlack);
  XMLCfg.SetDeleteValue(Path+'FormEditor/Rubberband/CreationColor/Value',
     FRubberbandCreationColor,clRed);
  XMLCfg.SetDeleteValue(Path+'FormEditor/Rubberband/SelectsGrandChilds/Value',
     FRubberbandSelectsGrandChilds,DefaultRubberbandSelectsGrandChilds);
  XMLCfg.SetDeleteValue(Path+'FormEditor/CreateComponentFocusNameProperty/Value',
     FCreateComponentFocusNameProperty,false);
  XMLCfg.SetDeleteValue(Path+'FormEditor/SwitchToFavoritesOITab/Value',FSwitchToFavoritesOITab,false);
  XMLCfg.SetDeleteValue(Path+'FormEditor/FormTitleBarChangesObjectInspector/Value',FFormTitleBarChangesObjectInspector,false);
  XMLCfg.SetDeleteValue(Path+'FormEditor/ForceDPIScalingInDesignTime/Value',FForceDPIScalingInDesignTime,true);
  // component list
  XMLCfg.SetDeleteValue(Path+'ComponentList/KeepOpen',FComponentListKeepOpen,false);
  XMLCfg.SetDeleteValue(Path+'ComponentList/PageIndex',FComponentListPageIndex,0);
  // object inspector
  FObjectInspectorOptions.SaveBounds:=false;
  FObjectInspectorOptions.Save;
  // messages view
  XMLCfg.SetDeleteValue(Path+'MsgViewDblClickJumps/Value',fMsgViewDblClickJumps,false);
  XMLCfg.SetDeleteValue(Path+'MsgViewFocus/Value',fMsgViewFocus,DefaultMsgViewFocus);
  XMLCfg.SetDeleteValue(Path+'MsgView/ShowMessagesIcons/Value',FShowMessagesIcons,true);
  XMLCfg.SetDeleteValue(Path+'MsgView/StayOnTop/Value',FMsgViewStayOnTop,false);
  XMLCfg.SetDeleteValue(Path+'MsgView/ShowTranslations/Value',FMsgViewShowTranslations,false);
  XMLCfg.SetDeleteValue(Path+'MsgView/AlwaysDrawFocused/Value',FMsgViewAlwaysDrawFocused,false);
  XMLCfg.SetDeleteValue(Path+'MsgView/Filename/Style',
    MsgWndFileNameStyleNames[FMsgViewFilenameStyle],
    MsgWndFileNameStyleNames[mwfsShort]);
  for mwc:=low(TMsgWndColor) to high(TMsgWndColor) do
    XMLCfg.SetDeleteValue(Path+'MsgView/Colors/'+MsgWndColorNames[mwc],
    fMsgViewColors[mwc],MsgWndDefaultColors[mwc]);
  for u:=low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    XMLCfg.SetDeleteValue(Path+'MsgView/MsgColors/'+dbgs(u),
    fMsgColors[u],clDefault);
  MsgViewFilters.SaveToXMLConfig(XMLCfg,'MsgView/Filters/');
  XMLCfg.SetDeleteValue(Path+'MsgView/FPCMsg/ShowLinesCompiled',FMsgViewShowFPCMsgLinesCompiled,false);
  // IDEEditorGroups
  for i := 0 to IDEEditorGroups.Count-1 do
  begin
    Rec := IDEEditorGroups[i];
    NodeName := Rec^.GroupClass.ClassName;
    XMLCfg.SetDeleteValue(Path+'OptionDialog/Tree/' + NodeName + '/Value',
                             Rec^.Collapsed,
                             Rec^.DefaultCollapsed);
    if Rec^.Items <> nil then begin
      for j := 0 to Rec^.Items.Count-1 do begin
        XMLCfg.SetDeleteValue(Path+'OptionDialog/Tree/' + NodeName
                                 + '/' + Rec^.Items[j]^.EditorClass.ClassName + '/Value',
                                 Rec^.Items[j]^.Collapsed,
                                 Rec^.Items[j]^.DefaultCollapsed);
      end;
    end;
  end;

  //automatically save active desktops
  if AutoSaveActiveDesktop
  and (Application.MainForm<>nil) and Application.MainForm.Visible then
  begin
    //save active desktop
    FDesktop.ImportSettingsFromIDE(Self);
    ActiveDesktop.Assign(FDesktop);
    if Assigned(FLastDesktopBeforeDebug) then//are we in debug session?
    begin
      //save last desktop before the debug desktop
      xSaveDesktop := FDesktops.Find(FLastDesktopBeforeDebug.Name);
      if Assigned(xSaveDesktop) and xSaveDesktop.InheritsFrom(TDesktopOpt) then
        TDesktopOpt(xSaveDesktop).Assign(FLastDesktopBeforeDebug, False);
    end;
  end;
  if Assigned(FLastDesktopBeforeDebug) then
    xActiveDesktopName := FLastDesktopBeforeDebug.Name
  else
    xActiveDesktopName := FActiveDesktopName;
  // The user can define many desktops. They are saved under path Desktops/.
  XMLCfg.DeletePath('Desktops/');
  CurPath:='Desktops/';
  XMLCfg.SetDeleteValue(CurPath+'Count', FDesktops.Count, 0);
  XMLCfg.SetDeleteValue(CurPath+'DebugDesktop', FDebugDesktopName, '');
  XMLCfg.SetDeleteValue(CurPath+'ActiveDesktop', xActiveDesktopName, '');
  for i := 0 to FDesktops.Count-1 do
  begin
    FDesktops[i].SetConfig(XMLCfg, FConfigStorage);
    FDesktops[i].Save(CurPath+'Desktop'+IntToStr(i+1)+'/');
  end;
end;

procedure TEnvGuiOptions.InitConfig;
begin
  FreeAndNil(FConfigStorage);
  FConfigStorage:=TXMLOptionsStorage.Create(XMLCfg);
  ObjectInspectorOptions.ConfigStore:=FConfigStorage;
end;

function _ContainsControl(const _Parent, _Control: TWinControl): Boolean;
var
  I: Integer;
begin
  for I := 0 to _Parent.ControlCount-1 do
    if _Parent.Controls[I] is TWinControl then
      if (_Parent.Controls[I] = _Control)
      or _ContainsControl(TWinControl(_Parent.Controls[I]), _Control)
      then
        Exit(True);
  Result := False;
end;

procedure TEnvGuiOptions.UseDesktop(ADesktop: TDesktopOpt);
var
  xLastFocusControl: TWinControl;
  xLastFocusForm: TCustomForm;
  s: String;
begin
  xLastFocusControl := Screen.ActiveControl;
  xLastFocusForm := Screen.ActiveCustomForm;
  // needed to get EditorToolBar refreshed! - needed only here in UseDesktop()
  EnvironmentOptions.DoBeforeWrite(False);

  Desktop.Assign(ADesktop);
  ActiveDesktopName := ADesktop.Name;
  s := ADesktop.AssociatedDebugDesktopName;
  if (s<>'') and Assigned(Desktops.Find(s)) then
    DebugDesktopName := s;
  Desktop.ExportSettingsToIDE(Self);

  EnvironmentOptions.DoAfterWrite(False); // needed to get EditorToolBar refreshed!
  Desktop.RestoreDesktop;

  //set focus back to the previously focused control
  if Screen.CustomFormIndex(xLastFocusForm) >= 0 then
  begin                          //check if form or control hasn't been destroyed
    if ((xLastFocusForm = xLastFocusControl) or _ContainsControl(xLastFocusForm, xLastFocusControl))
    and xLastFocusForm.CanFocus
    and xLastFocusControl.CanFocus
    then
      xLastFocusControl.SetFocus;
  end;
end;

procedure TEnvGuiOptions.EnableDebugDesktop;
begin
  if not Assigned(LastDesktopBeforeDebug)
  and Assigned(DebugDesktop)
  and (DebugDesktop <> ActiveDesktop) then
  begin
    LastDesktopBeforeDebug := TDesktopOpt.Create(ActiveDesktopName);
    if AutoSaveActiveDesktop then
      Desktop.ImportSettingsFromIDE(Self);
    LastDesktopBeforeDebug.Assign(Desktop, False);
    UseDesktop(DebugDesktop);
  end;
end;

procedure TEnvGuiOptions.DisableDebugDesktop;
begin
  if (LastDesktopBeforeDebug=nil)
  or (Desktop=nil) then
    Exit;
  try
    if AutoSaveActiveDesktop
    and Assigned(DebugDesktop) then
    begin
      Desktop.ImportSettingsFromIDE(EnvironmentGuiOpts);
      DebugDesktop.Assign(Desktop);
    end;
    UseDesktop(LastDesktopBeforeDebug);
  finally
    LastDesktopBeforeDebug.Free;
    LastDesktopBeforeDebug:=Nil
  end;
end;

class function TEnvGuiOptions.DesktopCanBeLoaded(const aDockMaster: string): Boolean;
begin
  Result := (aDockMaster = '') or (
    Assigned(IDEDockMaster) and (IDEDockMaster.ClassName = aDockMaster));
end;

function TEnvGuiOptions.GetMsgColors(u: TMessageLineUrgency): TColor;
begin
  Result:=fMsgColors[u];
end;

function TEnvGuiOptions.GetMsgViewColors(c: TMsgWndColor): TColor;
begin
  Result:=fMsgViewColors[c];
end;

procedure TEnvGuiOptions.SetMsgColors(u: TMessageLineUrgency; AValue: TColor);
begin
  fMsgColors[u] := AValue;
end;

procedure TEnvGuiOptions.SetMsgViewColors(c: TMsgWndColor; AValue: TColor);
begin
  fMsgViewColors[c]:=AValue;
end;

function TEnvGuiOptions.GetActiveDesktop: TDesktopOpt;

  procedure ChooseDefault;
  begin
    //use default desktop name
    if Assigned(IDEDockMaster) then
      FActiveDesktopName := 'default docked'//name for desktop with AnchorDocking
    else
      FActiveDesktopName := 'default';
  end;

var
  OldActiveDesktopName: string;
  OldActiveDesktop, lDskTpOpt: TCustomDesktopOpt;
begin
  Result := nil;
  if FActiveDesktopName <> '' then
  begin
    lDskTpOpt := FDesktops.Find(FActiveDesktopName);
    if Assigned(lDskTpOpt) and lDskTpOpt.InheritsFrom(TDesktopOpt) and lDskTpOpt.Compatible then
      Exit(TDesktopOpt(lDskTpOpt));
  end;

  //the selected desktop is unsupported (docked/undocked)
  // -> use default
  OldActiveDesktopName := FActiveDesktopName;
  ChooseDefault;
  lDskTpOpt := FDesktops.Find(FActiveDesktopName);
  if Assigned(lDskTpOpt) and lDskTpOpt.InheritsFrom(TDesktopOpt) then
    if lDskTpOpt.Compatible then
      Exit(TDesktopOpt(lDskTpOpt))
    else
      Result := TDesktopOpt(lDskTpOpt);

  //recreate desktop with ActiveDesktopName
  if Assigned(Result) then
    FDesktops.Remove(Result);

  Result := TDesktopOpt.Create(FActiveDesktopName);
  FDesktops.Add(Result);
  Result.Assign(FDesktop);
  if Assigned(IDEDockMaster) then
    Result.FDockedOpt.LoadDefaults;
  OldActiveDesktop := FDesktops.Find(OldActiveDesktopName);
  if not (OldActiveDesktop is TDesktopOpt) then
  begin
    lDskTpOpt := FDesktops.Find('default');
    if Assigned(lDskTpOpt) and lDskTpOpt.InheritsFrom(TDesktopOpt) and lDskTpOpt.Compatible then
      OldActiveDesktop := TDesktopOpt(lDskTpOpt)
    else
      OldActiveDesktop := nil;
  end;
  if Assigned(OldActiveDesktop) then
    Result.Assign(TDesktopOpt(OldActiveDesktop), False, False);
end;

function TEnvGuiOptions.GetDebugDesktop: TDesktopOpt;
var
  lDskTpOpt: TCustomDesktopOpt;
begin
  Result := nil;
  if FDebugDesktopName <> '' then
  begin
    lDskTpOpt := FDesktops.Find(FDebugDesktopName);
    if Assigned(lDskTpOpt)                 //do not mix docked/undocked desktops
    and lDskTpOpt.InheritsFrom(TDesktopOpt) and lDskTpOpt.Compatible then
      Result := TDesktopOpt(lDskTpOpt);
  end;
end;

end.

