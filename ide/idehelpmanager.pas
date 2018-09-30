{
 /***************************************************************************
                            helpmanager.pas
                            ---------------


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit IDEHelpManager;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LCLIntf, LCLType, FileProcs, Forms, Controls, ComCtrls, StdCtrls,
  Dialogs, Graphics, Buttons, ButtonPanel,
  // LazUtils
  LConvEncoding, LazUTF8Classes, LazFileUtils, HTML2TextRender,
  // CodeTools
  BasicCodeTools, CodeToolManager, CodeCache, CustomCodeTool, CodeTree,
  PascalParserTool, FindDeclarationTool,
  // IDEIntf
  PropEdits, ObjectInspector, TextTools,
  IDEDialogs, LazHelpIntf, LazHelpHTML, HelpFPDoc, MacroIntf, IDEWindowIntf,
  IDEMsgIntf, PackageIntf, LazIDEIntf, HelpIntfs, IDEHelpIntf,
  IDEExternToolIntf, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, DialogProcs, ObjInspExt, EnvironmentOpts, AboutFrm,
  Project, MainBar, IDEFPDocFileSearch, PackageDefs, PackageSystem, HelpOptions,
  MainIntf, LazConf, HelpFPCMessages, CodeHelp, IDEWindowHelp, CodeBrowser;

type

  { TSimpleFPCKeywordHelpDatabase }

  TSimpleFPCKeywordHelpDatabase = class(THTMLHelpDatabase)
  private
    FKeywordPrefixNode: THelpNode;
  public
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var {%H-}ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; {%H-}BaseNode, {%H-}NewNode: THelpNode;
                      {%H-}QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;

  TLIHProviders = class;

  { TLazIDEHTMLProvider }

  TLazIDEHTMLProvider = class(TAbstractIDEHTMLProvider)
  private
    fWaitingForAsync: boolean;
    FProviders: TLIHProviders;
    procedure SetProviders(const AValue: TLIHProviders);
    procedure OpenNextURL({%H-}Data: PtrInt); // called via Application.QueueAsyncCall
    procedure OpenFPDoc(Path: string);
  public
    NextURL: string;
    destructor Destroy; override;
    function URLHasStream(const URL: string): boolean; override;
    procedure OpenURLAsync(const URL: string); override;
    function GetStream(const URL: string; Shared: Boolean): TStream; override;
    procedure ReleaseStream(const URL: string); override;
    property Providers: TLIHProviders read FProviders write SetProviders;
  end;

  { TLIHProviderStream }

  TLIHProviderStream = class
  private
    FRefCount: integer;
  public
    Stream: TStream;
    URL: string;
    destructor Destroy; override;
    procedure IncreaseRefCount;
    procedure DecreaseRefCount;
    property RefCount: integer read FRefCount;
  end;

  { TLIHProviders
    manages all TLazIDEHTMLProvider }

  TLIHProviders = class
  private
    FStreams: TAVLTree;// tree of TLIHProviderStream sorted for URL
  public
    constructor Create;
    destructor Destroy; override;
    function FindStream(const URL: string; CreateIfNotExists: Boolean): TLIHProviderStream;
    function GetStream(const URL: string; Shared: boolean): TStream;
    procedure ReleaseStream(const URL: string);
  end;

  { TSimpleHTMLControl }

  TSimpleHTMLControl = class(TLabel,TIDEHTMLControlIntf)
  private
    FMaxLineCount: integer;
    FProvider: TAbstractIDEHTMLProvider;
    FURL: string;
    procedure SetProvider(const AValue: TAbstractIDEHTMLProvider);
  public
    constructor Create(AOwner: TComponent); override;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property Provider: TAbstractIDEHTMLProvider read FProvider write SetProvider;
    procedure SetHTMLContent(Stream: TStream; const NewURL: string);
    procedure GetPreferredControlSize(out AWidth, AHeight: integer);
    property MaxLineCount: integer read FMaxLineCount write FMaxLineCount;
  end;

  { TScrollableHTMLControl }

  TScrollableHTMLControl = class(TMemo,TIDEHTMLControlIntf)
  private
    FProvider: TAbstractIDEHTMLProvider;
    FURL: string;
    procedure SetProvider(const AValue: TAbstractIDEHTMLProvider);
  public
    constructor Create(AOwner: TComponent); override;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property Provider: TAbstractIDEHTMLProvider read FProvider write SetProvider;
    procedure SetHTMLContent(Stream: TStream; const NewURL: string);
    procedure GetPreferredControlSize(out AWidth, AHeight: integer);
  end;

  { TIDEHelpDatabases }

  TIDEHelpDatabases = class(THelpDatabases)
  public
    function ShowHelpSelector({%H-}Query: THelpQuery; Nodes: THelpNodeQueryList;
                              var {%H-}ErrMsg: string;
                              var Selection: THelpNodeQuery
                              ): TShowHelpResult; override;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; override;
    function ShowHelpForSourcePosition(Query: THelpQuerySourcePosition;
                                       var ErrMsg: string): TShowHelpResult; override;
    function SubstituteMacros(var s: string): boolean; override;
  end;
  
  
  { TIDEHelpManager }

  TIDEHelpManager = class(TBaseHelpManager)
    // help menu of the IDE menu bar
    procedure mnuHelpAboutLazarusClicked(Sender: TObject);
    procedure mnuHelpOnlineHelpClicked(Sender: TObject);
    procedure mnuHelpReportBugClicked(Sender: TObject);
    // fpdoc
    procedure mnuSearchInFPDocFilesClick(Sender: TObject);
    // messages
    procedure mnuEditMessageHelpClick(Sender: TObject);
  private
    FFCLHelpDB: THelpDatabase;
    FFCLHelpDBPath: THelpBaseURLObject;
    FHTMLProviders: TLIHProviders;
    FLCLHelpDB: THelpDatabase;
    FLCLHelpDBPath: THelpBaseURLObject;
    FMainHelpDB: THelpDatabase;
    FMainHelpDBPath: THelpBasePathObject;
    FRTLHelpDB: THelpDatabase;
    FRTLHelpDBPath: THelpBaseURLObject;
    FLazUtilsHelpDB: THelpDatabase;
    FLazUtilsHelpDBPath: THelpBaseURLObject;

    procedure RegisterIDEHelpDatabases;
    procedure RegisterDefaultIDEHelpViewers;
    procedure FindDefaultBrowser(var DefaultBrowser, Params: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure LoadHelpOptions; override;
    procedure SaveHelpOptions; override;

    procedure ShowLazarusHelpStartPage;
    procedure ShowIDEHelpForContext(HelpContext: THelpContext);
    procedure ShowIDEHelpForKeyword(const Keyword: string); // an arbitrary keyword, not an FPC keyword

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; override;
    procedure ShowHelpForMessage; override;
    procedure ShowHelpForObjectInspector(Sender: TObject); override;
    procedure ShowHelpForIDEControl(Sender: TControl); override;
    function GetHintForSourcePosition(const ExpandedFilename: string;
      const CodePos: TPoint; out BaseURL, HTMLHint: string;
      Flags: TIDEHelpManagerCreateHintFlags = []): TShowHelpResult; override;
    function ConvertSourcePosToPascalHelpContext(const CaretPos: TPoint;
               const Filename: string): TPascalHelpContextList; override;
    function ConvertCodePosToPascalHelpContext(
               ACodePos: PCodeXYPosition): TPascalHelpContextList;
    function GetFPDocFilenameForSource(SrcFilename: string;
      ResolveIncludeFiles: Boolean; out AnOwner: TObject): string; override;
  public
    property FCLHelpDB: THelpDatabase read FFCLHelpDB;
    property FCLHelpDBPath: THelpBaseURLObject read FFCLHelpDBPath;
    property MainHelpDB: THelpDatabase read FMainHelpDB;
    property MainHelpDBPath: THelpBasePathObject read FMainHelpDBPath;
    property LCLHelpDB: THelpDatabase read FLCLHelpDB;
    property LCLHelpDBPath: THelpBaseURLObject read FLCLHelpDBPath;
    property RTLHelpDB: THelpDatabase read FRTLHelpDB;
    property RTLHelpDBPath: THelpBaseURLObject read FRTLHelpDBPath;
    property LazUtilsHelpDB: THelpDatabase read FLazUtilsHelpDB;
    property LazUtilsHelpDBPath: THelpBaseURLObject read FLazUtilsHelpDBPath;
  end;

  { TIDEHintWindowManager }

  TIDEHintWindowManager = class(THintWindowManager)
  public
    function HintIsComplex: boolean;
    function SenderIsHintControl(Sender: TObject): Boolean;
    function PtIsOnHint(Pt: TPoint): boolean;
  end;

  { THelpSelectorDialog }
  
  THelpSelectorDialog = class(TForm)
    BtnPanel: TButtonPanel;
    NodesGroupBox: TGroupBox;
    NodesTreeView: TTreeView;
    procedure HelpSelectorDialogClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure NodesTreeViewDblClick(Sender: TObject);
    procedure NodesTreeViewSelectionChanged(Sender: TObject);
  private
    FNodes: THelpNodeQueryList;
    FImgIndexDB, FImgIndexNode: Integer;
    procedure SetNodes(const AValue: THelpNodeQueryList);
    procedure FillNodesTV;
    procedure UpdateButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    property Nodes: THelpNodeQueryList read FNodes write SetNodes;
    function GetSelectedNodeQuery: THelpNodeQuery;
  end;

  { Help Contexts for IDE help }
const
  lihcStartPage = 'StartPage';
  lihcRTLUnits = 'RTLUnits';
  lihcFCLUnits = 'FCLUnits';
  lihcLCLUnits = 'LCLUnits';
  lihcLazUtilsUnits = 'LazUtilsUnits';

  
  lihBaseUrl = 'http://lazarus-ccr.sourceforge.net/docs/';

  lihRTLURL = lihBaseUrl+'rtl/';
  lihFCLURL = lihBaseUrl+'fcl/';
  lihLCLURL = lihBaseUrl+'lcl/';

  lihLazUtilsURL = 'lazutils.chm://';
  // not important see: ../components/chmhelp/packages/idehelp/lazchmhelp.pas

var
  HelpBoss: TBaseHelpManager = nil;
  
implementation

{$R *.lfm}

// Default help control generator if no other is registered.
function LazCreateIDEHTMLControl(Owner: TComponent;
  var Provider: TAbstractIDEHTMLProvider;
  Flags: TIDEHTMLControlFlags): TControl;
begin
  if ihcScrollable in Flags then
    Result:=TScrollableHTMLControl.Create(Owner)
  else
    Result:=TSimpleHTMLControl.Create(Owner);
  if Provider=nil then
    Provider:=CreateIDEHTMLProvider(Result);
  if ihcScrollable in Flags then
  begin
    Provider.ControlIntf:=TScrollableHTMLControl(Result);
    TScrollableHTMLControl(Result).Provider:=Provider;
  end
  else
  begin
    Provider.ControlIntf:=TSimpleHTMLControl(Result);
    TSimpleHTMLControl(Result).Provider:=Provider;
  end;
end;

// Default provider generator if no other is registered.
function LazCreateIDEHTMLProvider(Owner: TComponent): TAbstractIDEHTMLProvider;
begin
  Result:=TLazIDEHTMLProvider.Create(Owner);
  TLazIDEHTMLProvider(Result).Providers:=TIDEHelpManager(HelpBoss).FHTMLProviders;
end;

function CompareLIHProviderStream(Data1, Data2: Pointer): integer;
begin
  Result:=CompareStr(TLIHProviderStream(Data1).URL,TLIHProviderStream(Data2).URL);
end;

function CompareURLWithLIHProviderStream(URL, Stream: Pointer): integer;
begin
  Result:=CompareStr(AnsiString(URL),TLIHProviderStream(Stream).URL);
end;

{ TSimpleFPCKeywordHelpDatabase }

function TSimpleFPCKeywordHelpDatabase.GetNodesForKeyword(
  const HelpKeyword: string; var ListOfNodes: THelpNodeQueryList;
  var ErrMsg: string): TShowHelpResult;
var
  KeyWord: String;
begin
  Result:=shrHelpNotFound;
  if (csDesigning in ComponentState) then exit;
  if (FPCKeyWordHelpPrefix<>'')
  and (LeftStr(HelpKeyword,length(FPCKeyWordHelpPrefix))=FPCKeyWordHelpPrefix) then begin
    // HelpKeyword starts with KeywordPrefix
    KeyWord:=copy(HelpKeyword,length(FPCKeyWordHelpPrefix)+1,length(HelpKeyword));
    // test: testfcpkeyword
    if KeyWord='testfcpkeyword' then begin
      // this help database knows this keyword
      // => add a node, so that if there are several possibilities the IDE can
      //    show the user a dialog to choose
      if FKeywordPrefixNode=nil then
        FKeywordPrefixNode:=THelpNode.CreateURL(Self,'','');
      FKeywordPrefixNode.Title:='Pascal keyword '+KeyWord;
      CreateNodeQueryListAndAdd(FKeywordPrefixNode,nil,ListOfNodes,true);
      Result:=shrSuccess;
    end;
  end;
end;

function TSimpleFPCKeywordHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  KeywordQuery: THelpQueryKeyword;
  KeyWord: String;
begin
  Result:=shrHelpNotFound;
  if not (Query is THelpQueryKeyword) then exit;
  KeywordQuery:=THelpQueryKeyword(Query);
  KeyWord:=copy(KeywordQuery.Keyword,length(FPCKeyWordHelpPrefix)+1,length(KeywordQuery.Keyword));
  debugln(['TSimpleFPCKeywordHelpDatabase.ShowHelp Keyword=',Keyword]);
  // ToDo: implement me
  ErrMsg:='';
end;

{ TSimpleHTMLControl }

procedure TSimpleHTMLControl.SetProvider(const AValue: TAbstractIDEHTMLProvider);
begin
  if FProvider=AValue then exit;
  FProvider:=AValue;
end;

constructor TSimpleHTMLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxLineCount:=30;
  WordWrap := True;
  Layout := tlCenter;
  Alignment := taLeftJustify;
  Font.Color := clInfoText;
  BorderSpacing.Around := 4;
  ShowAccelChar := False;  //don't underline after &
end;

function TSimpleHTMLControl.GetURL: string;
begin
  Result:=FURL;
end;

procedure TSimpleHTMLControl.SetURL(const AValue: string);
var
  Stream: TStream;
  Renderer: THTML2TextRenderer;
  NewURL: String;
begin
  if Provider=nil then raise Exception.Create('TSimpleHTMLControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.MakeURLAbsolute(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL,true);
    Renderer:=THTML2TextRenderer.Create(Stream);
    try
      Caption:=Renderer.Render(MaxLineCount);
    finally
      Renderer.Free;
      Provider.ReleaseStream(FURL);
    end;
  except
    on E: Exception do begin
      Caption:=E.Message;
    end;
  end;
end;

procedure TSimpleHTMLControl.SetHTMLContent(Stream: TStream; const NewURL: string);
var
  Renderer: THTML2TextRenderer;
begin
  FURL:=NewURL;
  Renderer:=THTML2TextRenderer.Create(Stream);
  try
    Caption:=Renderer.Render(MaxLineCount);
  finally
    Renderer.Free;
  end;
  //debugln(['TSimpleHTMLControl.SetHTMLContent: ',Caption]);
end;

procedure TSimpleHTMLControl.GetPreferredControlSize(out AWidth, AHeight: integer);
var
  DC: HDC;
  R: TRect;
  OldFont: HGDIOBJ;
  Flags: Cardinal;
  LabelText: String;
begin
  AWidth:=0;
  AHeight:=0;
  DC := GetDC(Parent.Handle);
  try
    R := Rect(0, 0, 600, 200);
    OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
    Flags := DT_CALCRECT or DT_EXPANDTABS;
    inc(Flags, DT_WordBreak);
    LabelText := GetLabelText;
    DrawText(DC, PChar(LabelText), Length(LabelText), R, Flags);
    SelectObject(DC, OldFont);
    AWidth := R.Right - R.Left + 8; // border
    AHeight := R.Bottom - R.Top + 8; // border
  finally
    ReleaseDC(Parent.Handle, DC);
  end;
  //DebugLn(['TSimpleHTMLControl.GetPreferredControlSize Caption="',Caption,'" ',AWidth,'x',AHeight]);
end;

{ TScrollableHTMLControl }

procedure TScrollableHTMLControl.SetProvider(const AValue: TAbstractIDEHTMLProvider);
begin
  if FProvider=AValue then exit;
  FProvider:=AValue;
end;

constructor TScrollableHTMLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderSpacing.Around := 4;
  BorderStyle := bsNone;
  ReadOnly := True;
  ScrollBars := ssAutoVertical;
end;

function TScrollableHTMLControl.GetURL: string;
begin
  Result:=FURL;
end;

procedure TScrollableHTMLControl.SetURL(const AValue: string);
var
  Stream: TStream;
  Renderer: THTML2TextRenderer;
  NewURL: String;
begin
  if Provider=nil then raise Exception.Create('TScrollableHTMLControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.MakeURLAbsolute(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL,true);
    Renderer:=THTML2TextRenderer.Create(Stream);
    try
      Caption:=Renderer.Render;
    finally
      Renderer.Free;
      Provider.ReleaseStream(FURL);
    end;
  except
    on E: Exception do begin
      Caption:=E.Message;
    end;
  end;
end;

procedure TScrollableHTMLControl.SetHTMLContent(Stream: TStream; const NewURL: string);
var
  Renderer: THTML2TextRenderer;
begin
  FURL:=NewURL;
  Renderer:=THTML2TextRenderer.Create(Stream);
  try
    Caption:=Renderer.Render;
  finally
    Renderer.Free;
  end;
  //debugln(['TScrollableHTMLControl.SetHTMLContent: ',Caption]);
end;

procedure TScrollableHTMLControl.GetPreferredControlSize(out AWidth, AHeight: integer);
begin
  AWidth:=0;
  AHeight:=0;
  GetPreferredSize(AWidth, AHeight);
end;

{ TLazIDEHTMLProvider }

procedure TLazIDEHTMLProvider.SetProviders(const AValue: TLIHProviders);
begin
  if FProviders=AValue then exit;
  FProviders:=AValue;
end;

procedure TLazIDEHTMLProvider.OpenNextURL(Data: PtrInt);
var
  URLScheme: string;
  URLPath: string;
  URLParams: string;
  AFilename: String;
  p: TPoint;
begin
  fWaitingForAsync:=false;
  SplitURL(NextURL,URLScheme,URLPath,URLParams);
  debugln(['TLazIDEHTMLProvider.OpenNextURL "',URLScheme,'" :// "',URLPath,'" & "',URLParams,'"']);
  if URLScheme='source' then begin
    p:=Point(1,1);
    if REMatches(URLPath,'(.*)\((.*),(.*)\)') then begin
      AFilename:=REVar(1);
      p.Y:=StrToIntDef(REVar(2),p.x);
      p.X:=StrToIntDef(REVar(3),p.y);
    end else begin
      AFilename:=URLPath;
    end;
    AFilename:=GetForcedPathDelims(AFilename);
    LazarusIDE.DoOpenFileAndJumpToPos(AFilename,p,-1,-1,-1,[]);
  end else if (URLScheme='openpackage') and IsValidIdent(URLPath) then begin
    PackageEditingInterface.DoOpenPackageWithName(URLPath,[],false);
  end else if (URLScheme='fpdoc') and (URLParams<>'') then begin
    OpenFPDoc(URLParams);
  end;
end;

procedure TLazIDEHTMLProvider.OpenFPDoc(Path: string);
var
  RestPath: string;

  function ExtractSubPath: string;
  var
    p: SizeInt;
  begin
    p:=System.Pos('.',RestPath);
    if p<1 then p:=length(RestPath)+1;
    Result:=copy(RestPath,1,p-1);
    RestPath:=copy(RestPath,p+1,length(RestPath));
  end;

  procedure InvalidPathError(Msg: string);
  begin
    debugln(['InvalidPathError Path="',Path,'" Msg="',Msg,'"']);
    IDEMessageDialog('Unable to open fpdoc help',
      'The fpdoc path "'+Path+'" is invalid.'+LineEnding+Msg,
      mtError,[mbCancel]);
  end;

var
  PkgName: String;
  Pkg: TLazPackage;
  AnUnitName: String;
  PkgFile: TPkgFile;
  ContextList: TPascalHelpContextList;
  ElementName: String;
  Filename: String;
  ErrMsg: string;
  PascalHelpContextLists: TList;
  i: Integer;
  PkgList: TFPList;
  SubPkg: TLazPackage;
begin
  RestPath:=Path;
  PkgName:=ExtractSubPath;
  if (PkgName='') or (PkgName[1]<>'#') then begin
    InvalidPathError('It does not start with a package name, for example #rtl.');
    exit;
  end;
  PkgName:=copy(PkgName,2,length(PkgName));
  if not IsValidIdent(PkgName) then begin
    InvalidPathError('It does not start with a package name, for example #rtl.');
    exit;
  end;
  if SysUtils.CompareText(PkgName,'rtl')=0 then PkgName:='fcl';
  Pkg:=TLazPackage(PackageEditingInterface.FindPackageWithName(PkgName));
  if Pkg=nil then begin
    InvalidPathError('Package "'+PkgName+'" not found.');
    exit;
  end;
  if Pkg.IsVirtual then begin
    InvalidPathError('Package "'+PkgName+'" has no help.');
    exit;
  end;

  AnUnitName:=ExtractSubPath;
  if not IsValidIdent(AnUnitName) then begin
    InvalidPathError('Unit name "'+AnUnitName+'" is invalid.');
    exit;
  end;

  Filename:='';
  PkgFile:=Pkg.FindUnit(AnUnitName);
  if PkgFile=nil then begin
    // search in all sub packages
    PkgList:=nil;
    try
      PackageGraph.GetAllRequiredPackages(nil,Pkg.FirstRequiredDependency,
        PkgList);
      if PkgList<>nil then begin
        for i:=0 to PkgList.Count-1 do begin
          SubPkg:=TLazPackage(PkgList[i]);
          PkgFile:=SubPkg.FindUnit(AnUnitName);
          if PkgFile<>nil then begin
            Pkg:=SubPkg;
            break;
          end;
        end;
      end;
    finally
      PkgList.Free;
    end;
  end;
  if (PkgFile<>nil) and (PkgFile.FileType in PkgFileRealUnitTypes) then begin
    // normal unit in lpk
    Filename:=PkgFile.GetFullFilename;
  end else if SysUtils.CompareText(PkgName,'fcl')=0 then begin
    // search in FPC sources
    Filename:=CodeToolBoss.DirectoryCachePool.FindUnitInUnitSet('',AnUnitName);
  end;
  if Filename='' then begin
    InvalidPathError('Unit "'+AnUnitName+'" was not found in package '+Pkg.Name+'.');
    exit;
  end;

  PascalHelpContextLists:=TList.Create;
  try
    // create a context list (and add it as sole element to the PascalHelpContextLists)
    ContextList:=TPascalHelpContextList.Create;
    PascalHelpContextLists.Add(ContextList);
    ContextList.Add(pihcFilename,Filename);
    ContextList.Add(pihcSourceName,AnUnitName);
    repeat
      ElementName:=ExtractSubPath;
      if ElementName='' then break;
      ContextList.Add(pihcType,ElementName);
    until false;
    ErrMsg:='TLazIDEHTMLProvider.OpenFPDoc ShowHelpForPascalContexts';
    ShowHelpForPascalContexts(Filename,Point(1,1),PascalHelpContextLists,ErrMsg);
  finally
    if PascalHelpContextLists<>nil then begin
      for i:=0 to PascalHelpContextLists.Count-1 do
        TObject(PascalHelpContextLists[i]).Free;
      PascalHelpContextLists.Free;
    end;
  end;
end;

destructor TLazIDEHTMLProvider.Destroy;
begin
  if (Application<>nil) and fWaitingForAsync then
    Application.RemoveAsyncCalls(Self);
  inherited Destroy;
end;

function TLazIDEHTMLProvider.URLHasStream(const URL: string): boolean;
var
  URLScheme: string;
  URLPath: string;
  URLParams: string;
begin
  Result:=false;
  SplitURL(URL,URLScheme,URLPath,URLParams);
  if (URLScheme='file') or (URLScheme='lazdoc') or (URLScheme='fpdoc') then
    Result:=true;
end;

procedure TLazIDEHTMLProvider.OpenURLAsync(const URL: string);
begin
  NextURL:=URL;
  //debugln(['TLazIDEHTMLProvider.OpenURLAsync URL=',URL]);
  if not fWaitingForAsync then begin
    Application.QueueAsyncCall(@OpenNextURL,0);
    fWaitingForAsync:=true;
  end;
end;

function TLazIDEHTMLProvider.GetStream(const URL: string; Shared: Boolean): TStream;
begin
  Result:=FProviders.GetStream(URL,Shared);
end;

procedure TLazIDEHTMLProvider.ReleaseStream(const URL: string);
begin
  FProviders.ReleaseStream(URL);
end;

{ TLIHProviders }

constructor TLIHProviders.Create;
begin
  FStreams:=TAVLTree.Create(@CompareLIHProviderStream);
end;

destructor TLIHProviders.Destroy;
begin
  FStreams.FreeAndClear;
  FreeAndNil(FStreams);
  inherited Destroy;
end;

function TLIHProviders.FindStream(const URL: string; CreateIfNotExists: Boolean
  ): TLIHProviderStream;
var
  Node: TAVLTreeNode;
begin
  if URL='' then
    exit(nil);
  Node:=FStreams.FindKey(Pointer(URL),@CompareURLWithLIHProviderStream);
  if Node<>nil then begin
    Result:=TLIHProviderStream(Node.Data);
  end else if CreateIfNotExists then begin
    Result:=TLIHProviderStream.Create;
    Result.URL:=URL;
    FStreams.Add(Result);
  end else
    Result:=nil;
end;

function TLIHProviders.GetStream(const URL: string; Shared: boolean): TStream;

  procedure OpenFile(out Stream: TStream; const Filename: string;
    UseCTCache: boolean);
  var
    fs: TFileStreamUTF8;
    ok: Boolean;
    Buf: TCodeBuffer;
    ms: TMemoryStream;
  begin
    if UseCTCache then begin
      Buf:=CodeToolBoss.LoadFile(Filename,true,false);
      if Buf=nil then
        raise Exception.Create('TLIHProviders.GetStream: unable to open file '+Filename);
      ms:=TMemoryStream.Create;
      Buf.SaveToStream(ms);
      ms.Position:=0;
      Result:=ms;
    end else begin
      fs:=nil;
      ok:=false;
      try
        DebugLn(['TLIHProviders.GetStream.OpenFile ',Filename]);
        fs:=TFileStreamUTF8.Create(Filename,fmOpenRead);
        Stream:=fs;
        ok:=true;
      finally
        if not ok then
          fs.Free;
      end;
    end;
  end;


{const
  HTML =
     '<HTML>'+#10
    +'<BODY>'+#10
    +'Test'+#10
    +'</BODY>'+#10
    +'</HTML>';}
var
  Stream: TLIHProviderStream;
  URLType: string;
  URLPath: string;
  URLParams: string;
begin
  if URL='' then raise Exception.Create('TLIHProviders.GetStream no URL');
  if Shared then begin
    Stream:=FindStream(URL,true);
    Stream.IncreaseRefCount;
    Result:=Stream.Stream;
  end else begin
    Stream:=nil;
    Result:=nil;
  end;
  try
    if Result=nil then begin
      SplitURL(URL,URLType,URLPath,URLParams);
      {$ifdef VerboseLazDoc}
      DebugLn(['TLIHProviders.GetStream URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams]);
      {$endif}
      if URLType='lazdoc' then begin
        if copy(URLPath,1,8)='lazarus/' then begin
          URLPath:=copy(URLPath,9,length(URLPath));
          if (URLPath='index.html')
          or (URLPath='images/laztitle.jpg')
          or (URLPath='images/cheetah1.png')
          or (URLPath='lazdoc.css')
          then begin
            OpenFile(Result,
              EnvironmentOptions.GetParsedLazarusDirectory
                +GetForcedPathDelims('/docs/'+URLPath),
              true);
          end;
        end;
      end else if URLType='file' then begin
        OpenFile(Result,GetForcedPathDelims(URLPath),true);
      end;
      {Result:=TMemoryStream.Create;
      Stream.Stream:=Result;
      Result.Write(HTML[1],length(HTML));
      Result.Position:=0;}
      if Result=nil then
        raise Exception.Create('TLIHProviders.GetStream: URL not found "'+dbgstr(URL)+'"');
      if Stream<>nil then
        Stream.Stream:=Result;
    end;
  finally
    if (Result=nil) and (Stream<>nil) then
      ReleaseStream(URL);
  end;
end;

procedure TLIHProviders.ReleaseStream(const URL: string);
var
  Stream: TLIHProviderStream;
begin
  Stream:=FindStream(URL,false);
  if Stream=nil then
    raise Exception.Create('TLIHProviders.ReleaseStream "'+URL+'"');
  Stream.DecreaseRefCount;
  if Stream.RefCount=0 then begin
    FStreams.Remove(Stream);
    Stream.Free;
  end;
end;

{ TLIHProviderStream }

destructor TLIHProviderStream.Destroy;
begin
  FreeAndNil(Stream);
  inherited Destroy;
end;

procedure TLIHProviderStream.IncreaseRefCount;
begin
  inc(FRefCount);
end;

procedure TLIHProviderStream.DecreaseRefCount;
begin
  if FRefCount<=0 then
    raise Exception.Create('TLIHProviderStream.DecreaseRefCount');
  dec(FRefCount);
end;

{ THelpSelectorDialog }

procedure THelpSelectorDialog.HelpSelectorDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure THelpSelectorDialog.NodesTreeViewDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure THelpSelectorDialog.NodesTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure THelpSelectorDialog.SetNodes(const AValue: THelpNodeQueryList);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
  FillNodesTV;
end;

procedure THelpSelectorDialog.FillNodesTV;
var
  i: Integer;
  NodeQuery: THelpNodeQuery;
  Node: THelpNode;
  DB: THelpDatabase;
  DBTVNode, TVNode: TTreeNode;
begin
  NodesTreeView.BeginUpdate;
  try
    TVNode:=nil;
    NodesTreeView.Items.Clear;
    if (Nodes<>nil) then begin
      for i:=0 to Nodes.Count-1 do begin
        NodeQuery:=Nodes[i];
        Node:=NodeQuery.Node;
        DB:=Node.Owner;

        DBTVNode:=NodesTreeView.Items.FindTopLvlNode(DB.ID);
        if DBTVNode=nil then
        begin
          DBTVNode:=NodesTreeView.Items.AddChild(nil,DB.ID);
          DBTVNode.ImageIndex:=FImgIndexDB;
          DBTVNode.SelectedIndex:=FImgIndexDB;
        end;

        TVNode:=NodesTreeView.Items.AddChild(DBTVNode,NodeQuery.AsString);
        TVNode.ImageIndex:=FImgIndexNode;
        TVNode.SelectedIndex:=FImgIndexNode;
        TVNode.Data:=NodeQuery;

        DBTVNode.Expand(true);
      end;
    end;
    NodesTreeView.Selected:=TVNode;
  finally
    NodesTreeView.EndUpdate;
  end;
end;

procedure THelpSelectorDialog.UpdateButtons;
begin
  BtnPanel.OKButton.Enabled:=GetSelectedNodeQuery<>nil;
end;

constructor THelpSelectorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,500,300);

  Caption := lisHelpSelectorDialog;
  NodesGroupBox.Caption:=lisSelectAHelpItem;
  BtnPanel.OKButton.Caption:=lisMenuOk;

  NodesTreeView.Images:=IDEImages.Images_16;
  FImgIndexDB:=IDEImages.LoadImage('item_package');
  FImgIndexNode:=IDEImages.LoadImage('btn_help');
end;

function THelpSelectorDialog.GetSelectedNodeQuery: THelpNodeQuery;
var
  TVNode: TTreeNode;
begin
  Result:=nil;
  TVNode:=NodesTreeView.Selected;
  if (TVNode=nil) or (TVNode.Data=nil) then exit;
  Result:=TObject(TVNode.Data) as THelpNodeQuery;
end;

{ TIDEHelpDatabases }

function TIDEHelpDatabases.ShowHelpSelector(Query: THelpQuery;
  Nodes: THelpNodeQueryList;
  var ErrMsg: string;
  var Selection: THelpNodeQuery
  ): TShowHelpResult;
var
  Dialog: THelpSelectorDialog;
begin
  Selection:=nil;
  Result:=shrNone;
  Dialog:=THelpSelectorDialog.Create(nil);
  try
    Dialog.Nodes:=Nodes;
    if Dialog.ShowModal=mrOk then begin
      Selection:=Dialog.GetSelectedNodeQuery;
      if Selection<>nil then
        Result:=shrSuccess;
    end else begin
      Result:=shrCancel;
    end;
  finally
    Dialog.Free;
  end;
end;

function TIDEHelpDatabases.GetBaseDirectoryForBasePathObject(
  BasePathObject: TObject): string;
var
  s: String;
begin
  Result:='';
  DebugLn('TIDEHelpDatabases.GetBaseDirectoryForBasePathObject BasePathObject=',dbgsName(BasePathObject));
  if (BasePathObject is THelpBasePathObject) then
    Result:=THelpBasePathObject(BasePathObject).BasePath
  else if (BasePathObject=HelpBoss) or (BasePathObject=MainIDEInterface) then
    Result:=EnvironmentOptions.GetParsedLazarusDirectory
  else if BasePathObject is TProject then
    Result:=TProject(BasePathObject).Directory
  else if BasePathObject is TLazPackage then
    Result:=TLazPackage(BasePathObject).Directory;
  if Result<>'' then begin
    s:=Result;
    if not IDEMacros.SubstituteMacros(Result) then
      debugln(['TIDEHelpDatabases.GetBaseDirectoryForBasePathObject macros failed "',s,'"']);
  end;
  Result:=AppendPathDelim(Result);
end;

function TIDEHelpDatabases.ShowHelpForSourcePosition(
  Query: THelpQuerySourcePosition; var ErrMsg: string): TShowHelpResult;
begin
  Result:=HelpBoss.ShowHelpForSourcePosition(Query.Filename,
                                             Query.SourcePosition,ErrMsg);
end;

function TIDEHelpDatabases.SubstituteMacros(var s: string): boolean;
begin
  Result:=IDEMacros.SubstituteMacros(s);
end;

{ TIDEHelpManager }

procedure TIDEHelpManager.mnuSearchInFPDocFilesClick(Sender: TObject);
begin
  ShowFPDocFileSearch;
end;

procedure TIDEHelpManager.mnuEditMessageHelpClick(Sender: TObject);
begin

end;

procedure TIDEHelpManager.mnuHelpAboutLazarusClicked(Sender: TObject);
begin
  ShowAboutForm;
end;

procedure TIDEHelpManager.mnuHelpOnlineHelpClicked(Sender: TObject);
begin
  ShowLazarusHelpStartPage;
end;

procedure TIDEHelpManager.mnuHelpReportBugClicked(Sender: TObject);
begin
  OpenURL(lisReportingBugURL);
end;

procedure TIDEHelpManager.RegisterIDEHelpDatabases;

  procedure CreateMainIDEHelpDB;
  var
    StartNode: THelpNode;
    HTMLHelp: THTMLHelpDatabase;
  begin
    FMainHelpDB:=HelpDatabases.CreateHelpDatabase(lihcStartPage,
                                                  THTMLHelpDatabase,true);
    HTMLHelp:=FMainHelpDB as THTMLHelpDatabase;
    FMainHelpDBPath:=THelpBasePathObject.Create('$(LazarusDir)/docs');
    HTMLHelp.BasePathObject:=FMainHelpDBPath;

    // HTML nodes for the IDE
    StartNode:=THelpNode.CreateURLID(HTMLHelp,'Lazarus',
                                     'file://index.html',lihcStartPage);
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,StartNode);// once as TOC
    HTMLHelp.RegisterItemWithNode(StartNode);// and once as normal page
  end;
  
  procedure CreateRTLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FRTLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcRTLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FRTLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihRTLURL;
    FRTLHelpDBPath:=THelpBaseURLObject.Create;
    HTMLHelp.BasePathObject:=FRTLHelpDBPath;

    // FPDoc nodes for units in the RTL
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'RTL - Free Pascal Run Time Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);// once as TOC
    DirItem:=THelpDBISourceDirectories.Create(FPDocNode,'$(FPCSrcDir)',
          'rtl;packages/rtl-console/src;packages/rtl-extra/src;packages/rtl-objpas/src;packages/rtl-unicode/src',
          '*.pp;*.pas',true);// and once as normal page
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateFCLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FFCLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcFCLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FFCLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihFCLURL;
    FFCLHelpDBPath:=THelpBaseURLObject.Create;
    HTMLHelp.BasePathObject:=FFCLHelpDBPath;

    // FPDoc nodes for units in the FCL
    // create TOC
    HTMLHelp.TOCNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
                   
    // FPC 2.0.x FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units (2.0.x)',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,
                                     '$(FPCSrcDir)/fcl/inc','*.pp;*.pas',false);
    HTMLHelp.RegisterItem(DirItem);
    
    // FPC 2.2.x FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,
                   '$(FPCSrcDir)/packages/fcl-base/src','*.pp;*.pas',true);
    HTMLHelp.RegisterItem(DirItem);

    // FPC 2.4.4+ FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectories.Create(FPDocNode,'$(FPCSrcDir)/packages',
      'fcl-base/src;fcl-db/src;fcl-extra/src;fcl-process/src;fcl-web/src;paszlib/src',
      '*.pp;*.pas',true);
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateLCLHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FLCLHelpDB:=HelpDatabases.CreateHelpDatabase(lihcLCLUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FLCLHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihLCLURL;
    FLCLHelpDBPath:=THelpBaseURLObject.Create;
    HTMLHelp.BasePathObject:=FLCLHelpDBPath;

    // FPDoc nodes for units in the LCL
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'LCL - Lazarus Component Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);// once as TOC
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,'$(LazarusDir)/lcl',
                                   '*.pp;*.pas',true);// and once as normal page
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateLazUtilsHelpDB;
  var
    HTMLHelp: TFPDocHTMLHelpDatabase;
    FPDocNode: THelpNode;
    DirItem: THelpDBISourceDirectory;
  begin
    FLazUtilsHelpDB:=HelpDatabases.CreateHelpDatabase(lihcLazUtilsUnits,
                                                 TFPDocHTMLHelpDatabase,true);
    HTMLHelp:=FLazUtilsHelpDB as TFPDocHTMLHelpDatabase;
    HTMLHelp.DefaultBaseURL:=lihLazUtilsURL;
    FLazUtilsHelpDBPath:=THelpBaseURLObject.Create;
    HTMLHelp.BasePathObject:=FLazUtilsHelpDBPath;

    // FPDoc nodes for units in the LazUtils
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'LazUtils - Lazarus Utilities Library Units',
                   'file://index.html');
    HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,FPDocNode);// once as TOC
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,
                    '$(LazarusDir)/components/lazutils',
                    '*.pp;*.pas',true);// and once as normal page
    HTMLHelp.RegisterItem(DirItem);
  end;

  procedure CreateFPCKeywordsHelpDB;
  begin
    {$IFDEF EnableSimpleFPCKeyWordHelpDB}
    HelpDatabases.CreateHelpDatabase('SimpleDemoForFPCKeyWordHelpDB',
                                            TSimpleFPCKeywordHelpDatabase,true);
    {$ENDIF}
  end;

begin
  CreateMainIDEHelpDB;
  CreateRTLHelpDB;
  CreateFCLHelpDB;
  CreateLCLHelpDB;
  CreateFPCMessagesHelpDB;
  CreateFPCKeywordsHelpDB;
  CreateLazUtilsHelpDB;
end;

procedure TIDEHelpManager.RegisterDefaultIDEHelpViewers;
var
  HelpViewer: THTMLBrowserHelpViewer;
begin
  HelpViewer:= THTMLBrowserHelpViewer.Create(nil);
  HelpViewer.OnFindDefaultBrowser := @FindDefaultBrowser;
  HelpViewers.RegisterViewer(HelpViewer);
end;

procedure TIDEHelpManager.FindDefaultBrowser(var DefaultBrowser, Params: string);
begin
  GetDefaultBrowser(DefaultBrowser, Params);
end;

constructor TIDEHelpManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  HelpBoss:=Self;
  LazarusHelp:=Self;
  HelpOpts:=THelpOptions.Create;
  HelpOpts.SetDefaultFilename;
  HelpDatabases:=TIDEHelpDatabases.Create;
  HelpIntfs.HelpManager:=HelpDatabases;
  HelpViewers:=THelpViewers.Create;
  RegisterIDEHelpDatabases;
  RegisterDefaultIDEHelpViewers;
  CombineSameIdentifiersInUnit:=true;
  ShowCodeBrowserOnUnknownIdentifier:=true;
  
  CodeHelpBoss:=TCodeHelpManager.Create(Self);

  // register property editors for URL handling
  RegisterPropertyEditor(TypeInfo(AnsiString),
                       THTMLHelpDatabase,'BaseURL',TURLDirectoryPropertyEditor);

  FHTMLProviders:=TLIHProviders.Create;

  if CreateIDEHTMLControl=nil then
    CreateIDEHTMLControl:=@LazCreateIDEHTMLControl;
  if CreateIDEHTMLProvider=nil then
    CreateIDEHTMLProvider:=@LazCreateIDEHTMLProvider;
end;

destructor TIDEHelpManager.Destroy;
begin
  FreeThenNil(FHTMLProviders);
  FreeThenNil(CodeHelpBoss);
  FPCMessagesHelpDB:=nil;
  FreeLCLHelpSystem;
  FreeThenNil(HelpOpts);
  FreeThenNil(FMainHelpDBPath);
  FreeThenNil(FRTLHelpDBPath);
  FreeThenNil(FFCLHelpDBPath);
  FreeThenNil(FLCLHelpDBPath);
  FreeThenNil(FLazUtilsHelpDBPath);
  HelpBoss:=nil;
  LazarusHelp:=nil;
  inherited Destroy;
end;

procedure TIDEHelpManager.ConnectMainBarEvents;
begin
  {$IFDEF Darwin}
  // ToDo: Place the "About Lazarus" under MacOS Application menu. See issue #12294.
  MainIDEBar.itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
  {$ELSE}
  MainIDEBar.itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
  {$ENDIF}
  MainIDEBar.itmHelpOnlineHelp.OnClick := @mnuHelpOnlineHelpClicked;
  MainIDEBar.itmHelpReportingBug.OnClick := @mnuHelpReportBugClicked;

  {$IFDEF EnableFPDocSearch}
  MainIDEBar.itmSearchInFPDocFiles.OnClick:=@mnuSearchInFPDocFilesClick;
  {$ENDIF}
end;

procedure TIDEHelpManager.LoadHelpOptions;
begin
  HelpOpts.Load;
end;

procedure TIDEHelpManager.SaveHelpOptions;
begin
  HelpOpts.Save;
end;

procedure TIDEHelpManager.ShowLazarusHelpStartPage;
begin
  ShowIDEHelpForKeyword(lihcStartPage);
end;

procedure TIDEHelpManager.ShowIDEHelpForContext(HelpContext: THelpContext);
begin
  ShowHelpOrErrorForContext(MainHelpDB.ID,HelpContext);
end;

procedure TIDEHelpManager.ShowIDEHelpForKeyword(const Keyword: string);
begin
  ShowHelpOrErrorForKeyword(MainHelpDB.ID,Keyword);
end;

function TIDEHelpManager.ShowHelpForSourcePosition(const Filename: string;
  const CodePos: TPoint; var ErrMsg: string): TShowHelpResult;
  
  function CollectKeyWords(CodeBuffer: TCodeBuffer; out Identifier: string): TShowHelpResult;
  // Collect keywords and show help if possible
  var
    p: Integer;
    IdentStart, IdentEnd: integer;
    KeyWord: String;
    ErrorMsg: String;
  begin
    Result:=shrHelpNotFound;
    Identifier:='';
    p:=0;
    CodeBuffer.LineColToPosition(CodePos.Y,CodePos.X,p);
    if p<1 then exit;
    GetIdentStartEndAtPosition(CodeBuffer.Source,p,IdentStart,IdentEnd);
    if IdentEnd<=IdentStart then exit;
    Identifier:=copy(CodeBuffer.Source,IdentStart,IdentEnd-IdentStart);
    if (IdentStart > 1) and (CodeBuffer.Source[IdentStart - 1] in ['$','%']) then
      Dec(IdentStart);
    KeyWord:=copy(CodeBuffer.Source,IdentStart,IdentEnd-IdentStart);
    ErrorMsg:='';
    if KeyWord[1] = '$' then
      Result:=ShowHelpForDirective('',FPCDirectiveHelpPrefix+Keyword,ErrorMsg)
    else if KeyWord[1] = '%' then
      Result:=ShowHelpForDirective('',IDEDirectiveHelpPrefix+Keyword,ErrorMsg)
    else
      Result:=ShowHelpForKeyword('',FPCKeyWordHelpPrefix+Keyword,ErrorMsg);
    if Result=shrSuccess then
      exit;
    if Result in [shrNone,shrDatabaseNotFound,shrContextNotFound,shrHelpNotFound] then
      exit(shrHelpNotFound); // not an FPC keyword
    // viewer error
    HelpManager.ShowError(Result,ErrorMsg);
    Result:=shrCancel;
  end;

  function CollectDeclarations(CodeBuffer: TCodeBuffer;
    out Complete: boolean): TShowHelpResult;
  // Collect declarations and show help if possible
  var
    NewList: TPascalHelpContextList;
    PascalHelpContextLists: TList;
    ListOfPCodeXYPosition: TFPList;
    CurCodePos: PCodeXYPosition;
    i: Integer;
    Flags: TFindDeclarationListFlags;
  begin
    Complete:=false;
    Result:=shrHelpNotFound;
    ListOfPCodeXYPosition:=nil;
    PascalHelpContextLists:=nil;
    try
      // get all possible declarations of this identifier
      debugln(['CollectDeclarations ',CodeBuffer.Filename,' line=',CodePos.Y,' col=',CodePos.X]);
      Flags:=[fdlfWithoutEmptyProperties,fdlfWithoutForwards];
      if CombineSameIdentifiersInUnit then
        Include(Flags,fdlfOneOverloadPerUnit);
      if CodeToolBoss.FindDeclarationAndOverload(CodeBuffer,CodePos.X,CodePos.Y,
        ListOfPCodeXYPosition,Flags)
      then begin
        if ListOfPCodeXYPosition=nil then exit;
        debugln('TIDEHelpManager.ShowHelpForSourcePosition Success, number of declarations: ',dbgs(ListOfPCodeXYPosition.Count));
        // convert the source positions in Pascal help context list
        for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
          CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
          debugln('TIDEHelpManager.ShowHelpForSourcePosition Declaration at ',dbgs(CurCodePos));
          NewList:=ConvertCodePosToPascalHelpContext(CurCodePos);
          if NewList<>nil then begin
            if PascalHelpContextLists=nil then
              PascalHelpContextLists:=TList.Create;
            PascalHelpContextLists.Add(NewList);
          end;
        end;
        if PascalHelpContextLists=nil then exit;

        // invoke help system
        Complete:=true;
        debugln(['TIDEHelpManager.ShowHelpForSourcePosition PascalHelpContextLists.Count=',PascalHelpContextLists.Count,' calling ShowHelpForPascalContexts...']);
        Result:=ShowHelpForPascalContexts(Filename,CodePos,PascalHelpContextLists,ErrMsg);
      end else if CodeToolBoss.ErrorCode<>nil then begin
        MainIDEInterface.DoJumpToCodeToolBossError;
        Complete:=True;
      end;
    finally
      FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      if PascalHelpContextLists<>nil then begin
        for i:=0 to PascalHelpContextLists.Count-1 do
          TObject(PascalHelpContextLists[i]).Free;
        PascalHelpContextLists.Free;
      end;
    end;
  end;

var
  CodeBuffer: TCodeBuffer;
  Complete: boolean;
  Identifier: string;
begin
  debugln('TIDEHelpManager.ShowHelpForSourcePosition A Filename=',Filename,' ',dbgs(CodePos));
  Result:=shrHelpNotFound;
  ErrMsg:='No help found for "'+Filename+'"'
         +' at ('+IntToStr(CodePos.Y)+','+IntToStr(CodePos.X)+')';
  // commit editor changes
  if not CodeToolBoss.GatherExternalChanges then exit;
  // get code buffer for Filename
  if mrOk<>LoadCodeBuffer(CodeBuffer,FileName,[lbfCheckIfText],false) then
    exit;

  Result:=CollectDeclarations(CodeBuffer,Complete);
  if Complete then exit;
  debugln(['TIDEHelpManager.ShowHelpForSourcePosition no declaration found, trying keywords...']);
  Result:=CollectKeyWords(CodeBuffer,Identifier);
  if Result in [shrCancel,shrSuccess] then exit;
  if IsValidIdent(Identifier) and ShowCodeBrowserOnUnknownIdentifier then
  begin
    debugln(['TIDEHelpManager.ShowHelpForSourcePosition "',Identifier,'" is not an FPC keyword, search via code browser...']);
    ShowCodeBrowser(Identifier);
    exit(shrSuccess);
  end;
  debugln(['TIDEHelpManager.ShowHelpForSourcePosition "',Identifier,'" is not an FPC keyword']);
end;

function TIDEHelpManager.ConvertCodePosToPascalHelpContext(
  ACodePos: PCodeXYPosition): TPascalHelpContextList;

  procedure AddContext(Descriptor: TPascalHelpContextType;
    const Context: string);
  begin
    Result.Add(Descriptor,Context);
    //debugln('  AddContext Descriptor=',dbgs(ord(Descriptor)),' Context="',Context,'"');
  end;

  procedure AddContextsBackwards(Tool: TCodeTool;
    Node: TCodeTreeNode);
  begin
    if Node=nil then exit;
    AddContextsBackwards(Tool,Node.Parent);
    case Node.Desc of
    ctnUnit, ctnPackage, ctnProgram, ctnLibrary:
      AddContext(pihcSourceName,Tool.GetSourceName);
    ctnVarDefinition:
      AddContext(pihcVariable,Tool.ExtractDefinitionName(Node));
    ctnTypeDefinition:
      AddContext(pihcType,Tool.ExtractDefinitionName(Node));
    ctnConstDefinition:
      AddContext(pihcConst,Tool.ExtractDefinitionName(Node));
    ctnProperty:
      AddContext(pihcProperty,Tool.ExtractPropName(Node,false));
    ctnProcedure:
      AddContext(pihcProcedure,Tool.ExtractProcName(Node,
                                                    [phpWithoutClassName]));
    ctnProcedureHead:
      AddContext(pihcParameterList,Tool.ExtractProcHead(Node,
                [phpWithoutClassKeyword,phpWithoutClassName,phpWithoutName,
                 phpWithoutSemicolon]));
    end;
  end;

var
  MainCodeBuffer: TCodeBuffer;
  Tool: TCustomCodeTool;
  CleanPos: integer;
  i: Integer;
  Node: TCodeTreeNode;
  IncludeChain: TFPList;
  ConversionResult: LongInt;
begin
  Result:=nil;
  // find code buffer
  if ACodePos^.Code=nil then begin
    debugln('WARNING: ConvertCodePosToPascalHelpContext ACodePos.Code=nil');
    exit;
  end;
  Result:=TPascalHelpContextList.Create;
  // add filename and all filenames of the include chain
  IncludeChain:=nil;
  try
    CodeToolBoss.GetIncludeCodeChain(ACodePos^.Code,true,IncludeChain);
    if IncludeChain=nil then begin
      debugln('WARNING: ConvertCodePosToPascalHelpContext IncludeChain=nil');
      exit;
    end;
    for i:=0 to IncludeChain.Count-1 do
      AddContext(pihcFilename,TCodeBuffer(IncludeChain[i]).Filename);
    MainCodeBuffer:=TCodeBuffer(IncludeChain[0]);
  finally
    IncludeChain.Free;
  end;
  // find code tool
  Tool:=CodeToolBoss.FindCodeToolForSource(MainCodeBuffer);
  if not (Tool is TCodeTool) then begin
    debugln('WARNING: ConvertCodePosToPascalHelpContext not (Tool is TCodeTool) MainCodeBuffer=',MainCodeBuffer.Filename);
    exit;
  end;
  // convert cursor position to clean position
  ConversionResult:=Tool.CaretToCleanPos(ACodePos^,CleanPos);
  if ConversionResult<>0 then begin
    // position not in clean code, maybe a comment, maybe behind last line
    // => ignore
    exit;
  end;
  // find node
  Node:=Tool.FindDeepestNodeAtPos(CleanPos,false);
  if Node=nil then begin
    // position not in a scanned pascal node, maybe in between
    // => ignore
    exit;
  end;
  AddContextsBackwards(TCodeTool(Tool),Node);
end;

function TIDEHelpManager.GetFPDocFilenameForSource(SrcFilename: string;
  ResolveIncludeFiles: Boolean; out AnOwner: TObject): string;
var
  CacheWasUsed: boolean;
begin
  Result:=CodeHelpBoss.GetFPDocFilenameForSource(SrcFilename,ResolveIncludeFiles,
    CacheWasUsed,AnOwner);
end;

procedure TIDEHelpManager.ShowHelpForMessage;
var
  Line: TMessageLine;
  Parts: TStringList;
begin
  if IDEMessagesWindow=nil then exit;
  Line:=IDEMessagesWindow.GetSelectedLine;
  if Line=nil then exit;
  Parts:=TStringList.Create;
  Line.GetAttributes(Parts);
  ShowHelpOrErrorForMessageLine(Line.Msg,Parts);
end;

procedure TIDEHelpManager.ShowHelpForObjectInspector(Sender: TObject);
var
  AnInspector: TObjectInspectorDlg;
  Code: TCodeBuffer;
  Caret: TPoint;
  ErrMsg: string;
  NewTopLine: integer;
begin
  //DebugLn('TIDEHelpManager.ShowHelpForObjectInspector ',dbgsName(Sender));
  if Sender=nil then Sender:=ObjectInspector1;
  if Sender is TObjectInspectorDlg then begin
    AnInspector:=TObjectInspectorDlg(Sender);
    if AnInspector.GetActivePropertyRow<>nil then begin
      if FindDeclarationOfOIProperty(AnInspector,nil,Code,Caret,NewTopLine) then
      begin
        if NewTopLine=0 then ;
        ErrMsg:='TIDEHelpManager.ShowHelpForObjectInspector ShowHelpForSourcePosition';
        ShowHelpForSourcePosition(Code.Filename,Caret,ErrMsg);
      end;
    end else begin
      DebugLn('TIDEHelpManager.ShowHelpForObjectInspector show default help for OI');
      ShowHelpForIDEControl(AnInspector);
    end;
  end;
end;

procedure TIDEHelpManager.ShowHelpForIDEControl(Sender: TControl);
begin
  LoadIDEWindowHelp;
  IDEWindowHelpNodes.InvokeHelp(Sender);
end;

function TIDEHelpManager.GetHintForSourcePosition(const ExpandedFilename: string;
  const CodePos: TPoint; out BaseURL, HTMLHint: string;
  Flags: TIDEHelpManagerCreateHintFlags): TShowHelpResult;
var
  Code: TCodeBuffer;
  CacheWasUsed: boolean;
  HintFlags: TCodeHelpHintOptions;
  PropDetails: string;
begin
  BaseURL:='';
  HTMLHint:='';
  Code:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if (Code=nil) or Code.LineColIsSpace(CodePos.Y,CodePos.X) then
    exit(shrHelpNotFound);
  HintFlags:=[chhoDeclarationHeader,chhoComments];
  if ihmchAddFocusHint in Flags then
    Include(HintFlags,chhoShowFocusHint);
  if CodeHelpBoss.GetHTMLHint(Code,CodePos.X,CodePos.Y,
    HintFlags,BaseURL,HTMLHint,PropDetails,CacheWasUsed)=chprSuccess
  then
    exit(shrSuccess);
  Result:=shrHelpNotFound;
end;

function TIDEHelpManager.ConvertSourcePosToPascalHelpContext(
  const CaretPos: TPoint; const Filename: string): TPascalHelpContextList;
var
  CodePos: TCodeXYPosition;
  Code: TCodeBuffer;
  ACodeTool: TCodeTool;
begin
  Result:=nil;
  Code:=CodeToolBoss.FindFile(Filename);
  if Code=nil then exit;
  CodePos.Code:=Code;
  CodePos.X:=CaretPos.X;
  CodePos.Y:=CaretPos.Y;
  if not CodeToolBoss.Explore(Code,ACodeTool,false) then exit;
  if ACodeTool=nil then ;
  Result:=ConvertCodePosToPascalHelpContext(@CodePos);
end;

{ TIDEHintWindowManager }

function TIDEHintWindowManager.HintIsComplex: boolean;
begin
  Result := HintIsVisible and (CurHintWindow.ControlCount > 0)
  and not (CurHintWindow.Controls[0] is TSimpleHTMLControl);
end;

function TIDEHintWindowManager.PtIsOnHint(Pt: TPoint): boolean;
begin
  Result := PtInRect(CurHintWindow.BoundsRect, Pt);
end;

function TIDEHintWindowManager.SenderIsHintControl(Sender: TObject): Boolean;
// ToDo: simplify. FHintWindow only has one child control.

  function IsHintControl(Control: TWinControl): Boolean;
  var
    I: Integer;
  begin
    if not Control.Visible then
      Exit(False);
    Result := Control = Sender;
    if Result then
      Exit;
    for I := 0 to Control.ControlCount - 1 do
    begin
      Result := Control.Controls[I] = Sender;
      if Result then
        Exit;
      if (Control.Controls[I] is TWinControl) then
      begin
        Result := IsHintControl(TWinControl(Control.Controls[I]));
        if Result then
          Exit;
      end;
    end;
  end;

begin
  if Assigned(CurHintWindow) then
    Assert(CurHintWindow.ControlCount < 2,
      'SenderIsHintControl: ControlCount = ' + IntToStr(CurHintWindow.ControlCount));
  Result := Assigned(Sender) and Assigned(CurHintWindow) and IsHintControl(CurHintWindow);
end;


end.

