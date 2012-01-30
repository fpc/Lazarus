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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit IDEHelpManager;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, AVL_Tree, LCLProc, LCLIntf, LCLType, Forms, Controls, Buttons,
  StdCtrls, Dialogs, ExtCtrls, FileProcs, Graphics, ButtonPanel, LConvEncoding,
  // CodeTools
  BasicCodeTools, CodeToolManager, CodeAtom, CodeCache, CustomCodeTool, CodeTree,
  PascalParserTool, FindDeclarationTool,
  // IDEIntf
  PropEdits, ObjectInspector, FormEditingIntf, ProjectIntf, TextTools,
  IDEDialogs, LazHelpIntf, LazHelpHTML, HelpFPDoc, MacroIntf, IDEWindowIntf,
  IDEMsgIntf, PackageIntf, LazIDEIntf, HelpIntfs, IDEHelpIntf,
  // IDE
  LazarusIDEStrConsts, TransferMacros, DialogProcs, IDEOptionDefs,
  ObjInspExt, EnvironmentOpts, AboutFrm, MsgView, Project, MainBar, OutputFilter,
  IDEFPDocFileSearch, PackageDefs, PackageSystem,
  HelpOptions, MainIntf, LazConf, HelpFPCMessages, CodeHelp,
  IDEContextHelpEdit, IDEWindowHelp;

type

  { TSimpleFPCKeywordHelpDatabase }

  TSimpleFPCKeywordHelpDatabase = class(THTMLHelpDatabase)
  private
    FKeywordPrefixNode: THelpNode;
  public
    function GetNodesForKeyword(const HelpKeyword: string;
                        var ListOfNodes: THelpNodeQueryList; var ErrMsg: string
                        ): TShowHelpResult; override;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;

  TLIHProviders = class;

  { TLazIDEHTMLProvider }

  TLazIDEHTMLProvider = class(TAbstractIDEHTMLProvider)
  private
    fWaitingForAsync: boolean;
    FProviders: TLIHProviders;
    procedure SetProviders(const AValue: TLIHProviders);
    procedure OpenNextURL(Data: PtrInt); // called via Application.QueueAsyncCall
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

  { TSimpleHTMLControl
    At the moment it is a TLabel that simply strips all tags }

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

  { TScrollableHTMLControl
    At the moment it is a TMemo that simply strips all tags }

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
    function ShowHelpSelector(Query: THelpQuery; Nodes: THelpNodeQueryList;
                              var ErrMsg: string;
                              var Selection: THelpNodeQuery
                              ): TShowHelpResult; override;
    function GetBaseDirectoryForBasePathObject(BasePathObject: TObject): string; override;
    function ShowHelpForSourcePosition(Query: THelpQuerySourcePosition;
                                       var ErrMsg: string): TShowHelpResult; override;
    function SubstituteMacros(var s: string): boolean; override;
  end;
  
  
  { TIDEHelpManager }

  TIDEHelpManager = class(TBaseHelpManager)
    procedure mnuSearchInFPDocFilesClick(Sender: TObject);
    // help menu of the IDE menu bar
    procedure mnuHelpAboutLazarusClicked(Sender: TObject);
    procedure mnuHelpOnlineHelpClicked(Sender: TObject);
    procedure mnuHelpReportBugClicked(Sender: TObject);
  private
    FFCLHelpDBPath: THelpBaseURLObject;
    FMainHelpDB: THelpDatabase;
    FMainHelpDBPath: THelpBasePathObject;
    FRTLHelpDB: THelpDatabase;
    FFCLHelpDB: THelpDatabase;
    FRTLHelpDBPath: THelpBaseURLObject;
    FHTMLProviders: TLIHProviders;
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
    procedure ShowIDEHelpForKeyword(const Keyword: string); // an arbitrary keyword, not a fpc keyword

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; override;
    procedure ShowHelpForMessage(Line: integer); override;
    procedure ShowHelpForObjectInspector(Sender: TObject); override;
    procedure ShowHelpForIDEControl(Sender: TControl); override;

    function CreateHint(aHintWindow: THintWindow; ScreenPos: TPoint;
                    const BaseURL: string; var TheHint: string;
                    out HintWinRect: TRect): boolean; override;
    function GetHintForSourcePosition(const ExpandedFilename: string;
                  const CodePos: TPoint;
                  out BaseURL, HTMLHint: string;
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
    property RTLHelpDB: THelpDatabase read FRTLHelpDB;
    property RTLHelpDBPath: THelpBaseURLObject read FRTLHelpDBPath;
  end;

  { THelpSelectorDialog }
  
  THelpSelectorDialog = class(TForm)
    BtnPanel: TButtonPanel;
    NodesGroupBox: TGroupBox;
    NodesListBox: TListBox;
    procedure HelpSelectorDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure NodesListBoxDblClick(Sender: TObject);
  private
    FNodes: THelpNodeQueryList;
    procedure SetNodes(const AValue: THelpNodeQueryList);
    procedure FillNodesListBox;
  public
    constructor Create(TheOwner: TComponent); override;
    property Nodes: THelpNodeQueryList read FNodes write SetNodes;
  end;

  { Help Contexts for IDE help }
const
  lihcStartPage = 'StartPage';
  lihcRTLUnits = 'RTLUnits';
  lihcFCLUnits = 'FCLUnits';
  lihcLCLUnits = 'LCLUnits';
  
  lihBaseUrl = 'http://lazarus-ccr.sourceforge.net/docs/';

  lihRTLURL = lihBaseUrl+'rtl/';
  lihFCLURL = lihBaseUrl+'fcl/';
  lihLCLURL = lihBaseUrl+'lcl/';

var
  HelpBoss: TBaseHelpManager = nil;
  
implementation

{$R *.lfm}

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
end;

function HTMLToCaption(const s: string; MaxLines: integer): string;
var
  p: Integer;
  EndPos: Integer;
  NewTag: String;
  Line: Integer;
  sp: LongInt;
  InHeader: Boolean;
  CurTagName: String;
begin
  Result:=s;
  //debugln(['HTMLToCaption HTML="',Result,'"']);
  Line:=1;
  p:=1;
  // remove UTF8 BOM
  if copy(Result,1,3)=UTF8BOM then
    Result:=copy(s,4,length(Result));
  InHeader:=false; // it could be a snippet
  while p<=length(Result) do begin
    if Result[p]='<' then begin
      // removes html tags
      EndPos:=p+1;
      if (EndPos<=length(Result)) and (Result[EndPos]='/') then inc(EndPos);
      while (EndPos<=length(Result))
      and (not (Result[EndPos] in [' ','>','"','/',#9,#10,#13])) do
        inc(EndPos);
      CurTagName:=UpperCase(copy(Result,p+1,EndPos-p-1));
      while (EndPos<=length(Result)) do begin
        if Result[EndPos]='"' then begin
          // skip " tag
          inc(EndPos);
          while (EndPos<=length(Result)) and (Result[EndPos]<>'"') do
            inc(EndPos);
          if EndPos>length(Result) then break;
        end;
        if (Result[EndPos]='>') then begin
          inc(EndPos);
          break;
        end;
        inc(EndPos);
      end;
      //debugln(['HTMLToCaption CurTagName=',CurTagName,' Tag="',copy(Result,p,EndPos-p),'"']);

      if CurTagName='HTML' then
      begin
        // it's a whole page
        InHeader:=true;
      end;
      if CurTagName='BODY' then
      begin
        // start of body => ignore header
        InHeader:=false;
        Result:=copy(Result,EndPos,length(Result));
        p:=1;
        EndPos:=1;
        Line:=1;
      end;
      if CurTagName='/BODY' then
      begin
        // end of body
        Result:=copy(Result,1,p-1);
        break;
      end;

      if (CurTagName='P') or (CurTagName='/P') then begin
        // add a line break if there is not already one
        sp:=p;
        while (sp>1) and (Result[sp-1] in [' ',#9]) do dec(sp);
        if (sp>1) and (not (Result[sp-1] in [#10,#13])) then
          CurTagName:='BR';
      end;
      if (CurTagName='DIV') or (CurTagName='/DIV')
      then begin
        // add a line break if not in first line
        if Line>1 then
          CurTagName:='BR';
      end;

      if CurTagName='BR' then
      begin
        NewTag:=LineEnding;
        if not InHeader then
          inc(Line);
        if Line>MaxLines then begin
          Result:=copy(Result,1,p)+LineEnding+'...';
          break;
        end;
      end
      else
        NewTag:='';
      if NewTag='' then begin
        //debugln(['HTMLToCaption deleting tag ',copy(Result,p,EndPos-p)]);
        System.Delete(Result,p,EndPos-p);
      end
      else begin
        Result:=copy(Result,1,p-1)+NewTag+copy(Result,EndPos,length(Result));
        inc(p,length(NewTag));
      end;
    end else if Result[p] in [' ',#9,#10,#13] then begin
      // replace spaces and newline characters with a single space
      EndPos:=p+1;
      while (EndPos<=length(Result)) and (Result[EndPos] in [' ',#9,#10,#13]) do
        inc(EndPos);
      if (p > 1) and not (Result[p-1] in [' ',#9,#10,#13]) then
      begin
        Result:=copy(Result,1,p-1)+' '+copy(Result,EndPos,length(Result));
        inc(p);
      end
      else
        Result:=copy(Result,1,p-1)+copy(Result,EndPos,length(Result));
    end else if Result[p]='&' then begin
      // special chars: &lt; &gt; &amp;
      if (Result[p+1]='l') and (Result[p+2]='t') then begin
        EndPos:=p+3;
        if (EndPos<length(Result)) and (Result[EndPos]=';') then
          inc(EndPos);
        Result:=copy(Result,1,p-1)+'<'+copy(Result,EndPos,length(Result));
      end else
      if (Result[p+1]='g') and (Result[p+2]='t') then begin
        EndPos:=p+3;
        if (EndPos<length(Result)) and (Result[EndPos]=';') then
          inc(EndPos);
        Result:=copy(Result,1,p-1)+'>'+copy(Result,EndPos,length(Result));
      end else
      if (Result[p+1]='a') and (Result[p+2]='m') and (Result[p+3]='p') then begin
        EndPos:=p+4;
        if (EndPos<length(Result)) and (Result[EndPos]=';') then
          inc(EndPos);
        // double '&' to prevent underlining
        Result:=copy(Result,1,p-1)+'&&'+copy(Result,EndPos,length(Result));
      end;
      inc(p);
    end else
      inc(p);
  end;
  // trim space at end
  p:=length(Result);
  while (p>0) and (Result[p] in [' ',#9,#10,#13]) do dec(p);
  SetLength(Result,p);

  //DebugLn(['HTMLToCaption Caption="',dbgstr(Result),'"']);
end;

function HTMLToCaption(Stream: TStream; MaxLines: integer): string;
var
  s: string;
begin
  SetLength(s,Stream.Size);
  if s<>'' then
    Stream.Read(s[1],length(s));
  Result:=HTMLToCaption(s,MaxLines);
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
end;

function TSimpleHTMLControl.GetURL: string;
begin
  Result:=FURL;
end;

procedure TSimpleHTMLControl.SetURL(const AValue: string);
var
  Stream: TStream;
  NewURL: String;
begin
  if Provider=nil then raise Exception.Create('TSimpleHTMLControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.MakeURLAbsolute(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL,true);
    try
      Caption:=HTMLToCaption(Stream, MaxLineCount);
    finally
      Provider.ReleaseStream(FURL);
    end;
  except
    on E: Exception do begin
      Caption:=E.Message;
    end;
  end;
end;

procedure TSimpleHTMLControl.SetHTMLContent(Stream: TStream;
  const NewURL: string);
begin
  FURL:=NewURL;
  Caption:=HTMLToCaption(Stream,MaxLineCount);
  //debugln(['TSimpleHTMLControl.SetHTMLContent ',Caption]);
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
  NewURL: String;
begin
  if Provider=nil then raise Exception.Create('TScrollableHTMLControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.MakeURLAbsolute(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL,true);
    try
      Caption:=HTMLToCaption(Stream, MaxInt);
    finally
      Provider.ReleaseStream(FURL);
    end;
  except
    on E: Exception do begin
      Caption:=E.Message;
    end;
  end;
end;

procedure TScrollableHTMLControl.SetHTMLContent(Stream: TStream;
  const NewURL: string);
begin
  FURL:=NewURL;
  Caption:=HTMLToCaption(Stream,MaxInt);
  //debugln(['TScrollableHTMLControl.SetHTMLContent ',Caption]);
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
    AFilename:=SetDirSeparators(AFilename);
    LazarusIDE.DoOpenFileAndJumpToPos(AFilename,p,-1,-1,-1,[]);
  end else if (URLScheme='openpackage') and (URLPath<>'')
  and IsValidIdent(URLPath) then begin
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
      'The fpdoc path "'+Path+'" is invalid.'#13+Msg,mtError,[mbCancel]);
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
  if (PkgName='') or not IsValidIdent(PkgName) then begin
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
  if (AnUnitName='') or (not IsValidIdent(AnUnitName)) then begin
    InvalidPathError('Unit name "'+AnUnitName+'" is invalid.');
    exit;
  end;

  Filename:='';
  PkgFile:=Pkg.FindUnit(AnUnitName);
  if PkgFile=nil then begin
    // search in all sub packages
    PkgList:=nil;
    try
      PackageGraph.GetAllRequiredPackages(Pkg.FirstRequiredDependency,PkgList);
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
    if PkgFile.IsVirtual then begin
      InvalidPathError('Unit "'+PkgFile.Filename+'" has no help.');
      exit;
    end;
    Filename:=PkgFile.Filename;
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
  SplitURL(NextURL,URLScheme,URLPath,URLParams);
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

function TLazIDEHTMLProvider.GetStream(const URL: string; Shared: Boolean
  ): TStream;
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
    fs: TFileStream;
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
        fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
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
              EnvironmentOptions.LazarusDirectory+SetDirSeparators('/docs/'+URLPath),
              true);
          end;
        end;
      end else if URLType='file' then begin
        OpenFile(Result,SetDirSeparators(URLPath),true);
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

procedure THelpSelectorDialog.NodesListBoxDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure THelpSelectorDialog.SetNodes(const AValue: THelpNodeQueryList);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
  FillNodesListBox;
end;

procedure THelpSelectorDialog.FillNodesListBox;
var
  List: TStringList;
  i: Integer;
  NodeQuery: THelpNodeQuery;
begin
  List:=TStringList.Create;
  if (Nodes<>nil) then begin
    for i:=0 to Nodes.Count-1 do begin
      NodeQuery:=Nodes[i];
      List.Add(NodeQuery.AsString);
    end;
  end;
  NodesListBox.Items.Assign(List);
  List.Free;
  if NodesListBox.Count > 0 then NodesListBox.ItemIndex := 0;
end;

constructor THelpSelectorDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,500,300);

  Caption := lisHelpSelectorDialog;
  NodesGroupBox.Caption:=lisSelectAHelpItem;
  BtnPanel.OKButton.Caption:=lisLazBuildOk;
end;

{ TIDEHelpDatabases }

function TIDEHelpDatabases.ShowHelpSelector(Query: THelpQuery;
  Nodes: THelpNodeQueryList;
  var ErrMsg: string;
  var Selection: THelpNodeQuery
  ): TShowHelpResult;
var
  Dialog: THelpSelectorDialog;
  i: LongInt;
begin
  Selection:=nil;
  Result:=shrNone;
  Dialog:=THelpSelectorDialog.Create(nil);
  try
    Dialog.Nodes:=Nodes;
    if Dialog.ShowModal=mrOk then begin
      i:=Dialog.NodesListBox.ItemIndex;
      if i>=0 then begin
        Selection:=Nodes[i];
        Result:=shrSuccess;
      end;
    end else begin
      Result:=shrCancel;
    end;
  finally
    Dialog.Free;
  end;
end;

function TIDEHelpDatabases.GetBaseDirectoryForBasePathObject(
  BasePathObject: TObject): string;
begin
  Result:='';
  DebugLn('TIDEHelpDatabases.GetBaseDirectoryForBasePathObject BasePathObject=',dbgsName(BasePathObject));
  if (BasePathObject is THelpBasePathObject) then
    Result:=THelpBasePathObject(BasePathObject).BasePath
  else if (BasePathObject=HelpBoss) or (BasePathObject=MainIDEInterface) then
    Result:=EnvironmentOptions.LazarusDirectory
  else if BasePathObject is TProject then
    Result:=TProject(BasePathObject).ProjectDirectory
  else if BasePathObject is TLazPackage then
    Result:=TLazPackage(BasePathObject).Directory;
  if Result<>'' then
    IDEMacros.SubstituteMacros(Result);
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
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,'$(FPCSrcDir)/rtl',
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
                   
    // fpc 2.0.x FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units (2.0.x)',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,
                                     '$(FPCSrcDir)/fcl/inc','*.pp;*.pas',false);
    HTMLHelp.RegisterItem(DirItem);
    
    // fpc 2.2.x FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectory.Create(FPDocNode,
                   '$(FPCSrcDir)/packages/fcl-base/src','*.pp;*.pas',true);
    HTMLHelp.RegisterItem(DirItem);

    // fpc 2.4.4+ FCL source directory
    FPDocNode:=THelpNode.CreateURL(HTMLHelp,
                   'FCL - Free Pascal Component Library Units',
                   'file://index.html');
    DirItem:=THelpDBISourceDirectories.Create(FPDocNode,'$(FPCSrcDir)/packages',
      'fcl-base/src;fcl-db/src;fcl-extra/src;fcl-process/src;fcl-web/src;paszlib/src',
      '*.pp;*.pas',true);
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
  CreateFPCMessagesHelpDB;
  CreateFPCKeywordsHelpDB;
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
  HelpBoss:=nil;
  LazarusHelp:=nil;
  inherited Destroy;
end;

procedure TIDEHelpManager.ConnectMainBarEvents;
begin
  with MainIDEBar do
  begin
    itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
    itmHelpOnlineHelp.OnClick := @mnuHelpOnlineHelpClicked;
    itmHelpReportingBug.OnClick := @mnuHelpReportBugClicked;

    {$IFDEF EnableFPDocSearch}
    itmSearchInFPDocFiles.OnClick:=@mnuSearchInFPDocFilesClick;
    {$ENDIF}
  end;
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
  
  function CollectKeyWords(CodeBuffer: TCodeBuffer): TShowHelpResult;
  var
    p: Integer;
    IdentStart, IdentEnd: integer;
    KeyWord: String;
    ErrorMsg: String;
  begin
    Result:=shrHelpNotFound;
    p:=0;
    CodeBuffer.LineColToPosition(CodePos.Y,CodePos.X,p);
    if p<1 then exit;
    GetIdentStartEndAtPosition(CodeBuffer.Source,p,IdentStart,IdentEnd);
    if IdentEnd<=IdentStart then exit;
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
    if Result=shrHelpNotFound then exit;
    HelpManager.ShowError(Result,ErrorMsg);
  end;

  function CollectDeclarations(CodeBuffer: TCodeBuffer;
    out Complete: boolean): TShowHelpResult;
  var
    NewList: TPascalHelpContextList;
    PascalHelpContextLists: TList;
    ListOfPCodeXYPosition: TFPList;
    CurCodePos: PCodeXYPosition;
    i: Integer;
  begin
    Complete:=false;
    Result:=shrHelpNotFound;
    ListOfPCodeXYPosition:=nil;
    PascalHelpContextLists:=nil;
    try
      // get all possible declarations of this identifier
      if CodeToolBoss.FindDeclarationAndOverload(CodeBuffer,CodePos.X,CodePos.Y,
        ListOfPCodeXYPosition,[fdlfWithoutEmptyProperties,fdlfWithoutForwards])
      then begin
        if ListOfPCodeXYPosition=nil then exit;
        debugln('TIDEHelpManager.ShowHelpForSourcePosition B Success ',dbgs(ListOfPCodeXYPosition.Count));
        // convert the source positions in pascal help context list
        for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
          CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
          debugln('TIDEHelpManager.ShowHelpForSourcePosition C ',CurCodePos^.Code.Filename,' X=',dbgs(CurCodePos^.X),' Y=',dbgs(CurCodePos^.Y));
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
        debugln('TIDEHelpManager.ShowHelpForSourcePosition D PascalHelpContextLists.Count=',dbgs(PascalHelpContextLists.Count));
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
  Result:=CollectKeyWords(CodeBuffer);
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

procedure TIDEHelpManager.ShowHelpForMessage(Line: integer);

  function ParseMessage(MsgItem: TIDEMessageLine): TStringList;
  begin
    Result:=TStringList.Create;
    Result.Values['Message']:=MsgItem.Msg;
    if MsgItem.Parts<>nil then
      Result.Assign(MsgItem.Parts);
  end;

var
  MsgItem: TIDEMessageLine;
  MessageParts: TStringList;
begin
  //debugln('TIDEHelpManager.ShowHelpForMessage A Line=',dbgs(Line));
  if MessagesView=nil then exit;
  if Line<0 then
    Line:=MessagesView.SelectedMessageIndex;
  //DebugLn('TIDEHelpManager.ShowHelpForMessage B Line=',dbgs(Line),' ',dbgs(MessagesView.VisibleItemCount));
  if (Line<0) or (Line>=MessagesView.VisibleItemCount) then exit;
  MsgItem:=MessagesView.VisibleItems[Line];
  if MsgItem=nil then exit;
  if MsgItem.Msg<>'' then begin
    MessageParts:=ParseMessage(MsgItem);
    ShowHelpOrErrorForMessageLine(MsgItem.Msg,MessageParts);
  end;
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

function TIDEHelpManager.CreateHint(aHintWindow: THintWindow; ScreenPos: TPoint;
  const BaseURL: string; var TheHint: string; out HintWinRect: TRect): boolean;
var
  IsHTML: Boolean;
  Provider: TAbstractIDEHTMLProvider;
  HTMLControl: TControl;
  ms: TMemoryStream;
  NewWidth, NewHeight: integer;
begin
  IsHTML:=SysUtils.CompareText(copy(TheHint,1,6),'<HTML>')=0;

  if aHintWindow.ControlCount>0 then begin
    aHintWindow.Controls[0].Free;
  end;
  if IsHTML then begin
    Provider:=nil;
    HTMLControl:=CreateIDEHTMLControl(aHintWindow,Provider);
    Provider.BaseURL:=BaseURL;
    HTMLControl.Parent:=aHintWindow;
    HTMLControl.Align:=alClient;
    ms:=TMemoryStream.Create;
    try
      if TheHint<>'' then
        ms.Write(TheHint[1],length(TheHint));
      ms.Position:=0;
      Provider.ControlIntf.SetHTMLContent(ms,'');
    finally
      ms.Free;
    end;
    Provider.ControlIntf.GetPreferredControlSize(NewWidth,NewHeight);

    if NewWidth <= 0 then
      NewWidth := 500
    else
      inc(NewWidth, 8); // border

    if NewHeight <= 0 then
      NewHeight := 200
    else
      inc(NewHeight, 8); // border

    HintWinRect := Rect(0, 0, NewWidth, NewHeight);
    TheHint:='';
  end else begin
    HintWinRect := aHintWindow.CalcHintRect(Screen.Width, TheHint, nil);
  end;
  OffsetRect(HintWinRect, ScreenPos.X, ScreenPos.Y+30);

  Result:=true;
end;

function TIDEHelpManager.GetHintForSourcePosition(
  const ExpandedFilename: string; const CodePos: TPoint; out BaseURL,
  HTMLHint: string; Flags: TIDEHelpManagerCreateHintFlags): TShowHelpResult;
var
  Code: TCodeBuffer;
  CacheWasUsed: boolean;
  HintFlags: TCodeHelpHintOptions;
begin
  BaseURL:='';
  HTMLHint:='';
  Code:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if (Code=nil) or Code.LineColIsSpace(CodePos.Y,CodePos.X) then
    exit(shrHelpNotFound);
  HintFlags:=[chhoDeclarationHeader];
  if ihmchAddFocusHint in Flags then
    Include(HintFlags,chhoShowFocusHint);
  if CodeHelpBoss.GetHTMLHint(Code,CodePos.X,CodePos.Y,
    HintFlags,BaseURL,HTMLHint,CacheWasUsed)=chprSuccess
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

end.

