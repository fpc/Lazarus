{  $Id$  }
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
unit HelpManager;

{$mode objfpc}{$H+}

interface

uses
  // FCL+LCL
  Classes, SysUtils, AVL_Tree, LCLProc, LCLIntf, LCLType, Forms, Controls, Buttons,
  StdCtrls, Dialogs, ExtCtrls, FileUtil, Graphics,
  // CodeTools
  BasicCodeTools, CodeToolManager, CodeAtom, CodeCache, CustomCodeTool, CodeTree,
  PascalParserTool, FindDeclarationTool,
  // IDEIntf
  PropEdits, ObjectInspector, FormEditingIntf, ProjectIntf,
  LazHelpIntf, LazHelpHTML, HelpFPDoc, MacroIntf, IDEWindowIntf, IDEMsgIntf,
  LazIDEIntf, HelpIntfs, IDEHelpIntf,
  // IDE
  LazarusIDEStrConsts, TransferMacros, DialogProcs, IDEOptionDefs,
  ObjInspExt, EnvironmentOpts, AboutFrm, MsgView, Project, PackageDefs, MainBar,
  OutputFilter, HelpOptions, MainIntf, LazConf, HelpFPCMessages, CodeHelp,
  IDEContextHelpEdit, ButtonPanel;

type

  TLIHProviders = class;

  { TLazIDEHTMLProvider }

  TLazIDEHTMLProvider = class(TAbstractIDEHTMLProvider)
  private
    FProviders: TLIHProviders;
    procedure SetProviders(const AValue: TLIHProviders);
  public
    function GetStream(const URL: string): TStream; override;
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
    function GetStream(const URL: string): TStream;
    procedure ReleaseStream(const URL: string);
  end;

  { TSimpleHTMLControl }

  TSimpleHTMLControl = class(TLabel,TIDEHTMLControlIntf)
  private
    FProvider: TAbstractIDEHTMLProvider;
    FURL: string;
    procedure SetProvider(const AValue: TAbstractIDEHTMLProvider);
    function HTMLToCaption(const s: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property Provider: TAbstractIDEHTMLProvider read FProvider write SetProvider;
    procedure SetHTMLContent(Stream: TStream);
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
    // help menu of the IDE menu bar
    procedure mnuHelpAboutLazarusClicked(Sender: TObject);
    procedure mnuHelpOnlineHelpClicked(Sender: TObject);
    procedure mnuHelpReportBugClicked(Sender: TObject);
  private
    FFCLHelpDBPath: THelpBaseURLObject;
    FLCLHelpDBPath: THelpBaseURLObject;
    FMainHelpDB: THelpDatabase;
    FMainHelpDBPath: THelpBasePathObject;
    FRTLHelpDB: THelpDatabase;
    FFCLHelpDB: THelpDatabase;
    FLCLHelpDB: THelpDatabase;
    FRTLHelpDBPath: THelpBaseURLObject;
    FHTMLProviders: TLIHProviders;
    procedure RegisterIDEHelpDatabases;
    procedure RegisterDefaultIDEHelpViewers;
    procedure FindDefaultBrowser(var DefaultBrowser, Params: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFPCDocsHTMLDirectory;

    procedure ConnectMainBarEvents; override;
    procedure LoadHelpOptions; override;
    procedure SaveHelpOptions; override;

    procedure ShowLazarusHelpStartPage;
    procedure ShowIDEHelpForContext(HelpContext: THelpContext);
    procedure ShowIDEHelpForKeyword(const Keyword: string);

    function ShowHelpForSourcePosition(const Filename: string;
                                       const CodePos: TPoint;
                                       var ErrMsg: string): TShowHelpResult; override;
    procedure ShowHelpForMessage(Line: integer); override;
    procedure ShowHelpForObjectInspector(Sender: TObject); override;
    function CreateHint(aHintWindow: THintWindow; ScreenPos: TPoint;
                    const BaseURL: string; var TheHint: string;
                    out HintWinRect: TRect): boolean; override;
    function GetHintForSourcePosition(const ExpandedFilename: string;
                                      const CodePos: TPoint;
                                      out BaseURL, HTMLHint: string): TShowHelpResult; override;

    function ConvertSourcePosToPascalHelpContext(const CaretPos: TPoint;
               const Filename: string): TPascalHelpContextList; override;
    function ConvertCodePosToPascalHelpContext(
               ACodePos: PCodeXYPosition): TPascalHelpContextList;
  public
    property FCLHelpDB: THelpDatabase read FFCLHelpDB;
    property FCLHelpDBPath: THelpBaseURLObject read FFCLHelpDBPath;
    property LCLHelpDB: THelpDatabase read FLCLHelpDB;
    property LCLHelpDBPath: THelpBaseURLObject read FLCLHelpDBPath;
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
  var Provider: TAbstractIDEHTMLProvider): TControl;
var
  HTMLControl: TSimpleHTMLControl;
begin
  HTMLControl:=TSimpleHTMLControl.Create(Owner);
  Result:=HTMLControl;
  if Provider=nil then
    Provider:=CreateIDEHTMLProvider(HTMLControl);
  Provider.ControlIntf:=HTMLControl;
  HTMLControl.Provider:=Provider;
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

{ TSimpleHTMLControl }

procedure TSimpleHTMLControl.SetProvider(const AValue: TAbstractIDEHTMLProvider
  );
begin
  if FProvider=AValue then exit;
  FProvider:=AValue;
end;

function TSimpleHTMLControl.HTMLToCaption(const s: string): string;
var
  p: Integer;
  EndPos: Integer;
  CurTag: String;
  NewTag: String;
begin
  Result:=s;
  p:=1;
  while p<=length(Result) do begin
    if Result[p]='<' then begin
      // skip html tag
      EndPos:=p+1;
      while (EndPos<=length(Result)) do begin
        if Result[EndPos]='"' then begin
          // skip " tag
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
      CurTag:=copy(Result,p,EndPos-p);
      if SysUtils.CompareText(CurTag,'<BR>')=0 then
        NewTag:=LineEnding
      else
        NewTag:='';
      if NewTag='' then
        System.Delete(Result,p,EndPos-p)
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
    end else
      inc(p);
  end;
  //DebugLn(['TSimpleHTMLControl.HTMLToCaption "',dbgstr(Result),'"']);
end;

constructor TSimpleHTMLControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
  s: string;
  NewURL: String;
begin
  if Provider=nil then raise Exception.Create('TSimpleHTMLControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.BuildURL(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL);
    SetLength(s,Stream.Size);
    if s<>'' then
      Stream.Read(s[1],length(s));
    Caption:=HTMLToCaption(s);
    Provider.ReleaseStream(FURL);
  except
    on E: Exception do begin
      Caption:=E.Message;
    end;
  end;
end;

procedure TSimpleHTMLControl.SetHTMLContent(Stream: TStream);
var
  s: string;
begin
  SetLength(s,Stream.Size);
  if s<>'' then
    Stream.Read(s[1],length(s));
  Caption:=HTMLToCaption(s);
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

{ TLazIDEHTMLProvider }

procedure TLazIDEHTMLProvider.SetProviders(const AValue: TLIHProviders);
begin
  if FProviders=AValue then exit;
  FProviders:=AValue;
end;

function TLazIDEHTMLProvider.GetStream(const URL: string): TStream;
begin
  Result:=FProviders.GetStream(URL);
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

function TLIHProviders.GetStream(const URL: string): TStream;

  procedure OpenFile(out Stream: TStream; const Filename: string);
  var
    fs: TFileStream;
    ok: Boolean;
  begin
    fs:=nil;
    ok:=false;
    try
      fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
      //DebugLn(['OpenFile ',Filename,' ',fs.Size,' ',fs.Position]);
      Stream:=fs;
      ok:=true;
    finally
      if not ok then
        fs.Free;
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
  Stream:=FindStream(URL,true);
  Stream.IncreaseRefCount;
  Result:=Stream.Stream;
  try
    if Result=nil then begin
      SplitURL(URL,URLType,URLPath,URLParams);
      DebugLn(['TLIHProviders.GetStream URLType=',URLType,' URLPath=',URLPath,' URLParams=',URLParams]);
      if URLType='lazdoc' then begin
        if copy(URLPath,1,8)='lazarus/' then begin
          URLPath:=copy(URLPath,9,length(URLPath));
          if (URLPath='index.html')
          or (URLPath='images/laztitle.jpg')
          or (URLPath='images/cheetah1.png') then begin
            OpenFile(Result,EnvironmentOptions.LazarusDirectory+PathDelim+'docs'+PathDelim+URLPath);
          end;
        end;
      end else begin

      end;
      {Result:=TMemoryStream.Create;
      Stream.Stream:=Result;
      Result.Write(HTML[1],length(HTML));
      Result.Position:=0;}
      if Result=nil then
        raise Exception.Create('TLIHProviders.GetStream: URL not found "'+dbgstr(URL)+'"');
      Stream.Stream:=Result;
    end;
  finally
    if Result=nil then
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
                                  '*.pp;*.pas',false);// and once as normal page
    HTMLHelp.RegisterItem(DirItem);
  end;

begin
  CreateMainIDEHelpDB;
  CreateRTLHelpDB;
  CreateFCLHelpDB;
  CreateLCLHelpDB;
  CreateFPCMessagesHelpDB;
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
  
  CodeHelpBoss:=TCodeHelpManager.Create;

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
  HelpBoss:=nil;
  LazarusHelp:=nil;
  inherited Destroy;
end;

procedure TIDEHelpManager.UpdateFPCDocsHTMLDirectory;

  function IsFPCDocsHTMDirectory(Directory: string): boolean;
  var
    RefFilename: String;
  begin
    Result:=false;
    if Directory='' then exit;
    RefFilename:=AppendPathDelim(TrimFilename(Directory))
                                 +'ref'+PathDelim+'ref.kwd';
    Result:=FileExistsUTF8(RefFilename);
    //DebugLn(['IsFPCDocsHTMDirectory RefFilename="',RefFilename,'" Result=',Result]);
  end;
  
  function TryDirectory(const Directory: string): boolean;
  var
    NewDir: String;
  begin
    NewDir:=CleanAndExpandDirectory(Directory);
    if not IsFPCDocsHTMDirectory(NewDir) then exit(false);
    HelpOpts.FPCDocsHTMLDirectory:=NewDir;
    DebugLn(['TryDirectory Changing FPCDocsHTMLDirectory to "',HelpOpts.FPCDocsHTMLDirectory,'"']);
    SaveHelpOptions;
    Result:=true;
  end;
  
  function TryDirectoryMask(const Directory: string): boolean;
  var
    DirMask: String;
    CurDir: String;
    FileInfo: TSearchRec;
    NewDir: String;
  begin
    Result:=false;
    DirMask:=TrimFilename(Directory);
    CurDir:=ExtractFilePath(DirMask);
    if FindFirstUTF8(DirMask,faDirectory,FileInfo)=0 then begin
      repeat
        // skip special files
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        if ((FileInfo.Attr and faDirectory)>0) then begin
          NewDir:=CurDir+FileInfo.Name;
          if TryDirectory(NewDir) then
            exit(true);
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
  
  function SearchInCommonInstallDir: boolean;
  var
    SystemPPU: String;
    p: LongInt;
    FPCInstallDir: String;
    FPCVersion: String;
    CurUnitName: String;
  begin
    Result:=false;
    { Linux:
        normally fpc ppu are installed in
          /somewhere/lib/fpc/$fpcversion/units/$fpctarget/
        and the docs are installed in
          /somewhere/share/doc/fpcdocs-$fpcversion/
      }
    CurUnitName:='system.ppu';
    SystemPPU:=CodeToolBoss.DirectoryCachePool.FindCompiledUnitInCompletePath(
                                                    '',CurUnitName);
    DebugLn(['SearchInCommonInstallDir SystemPPU=',SystemPPU]);
    // SystemPPU is now e.g. /usr/lib/fpc/2.0.4/units/i386-linux/rtl/system.ppu
    if SystemPPU='' then exit;
    p:=System.Pos(PathDelim+'fpc'+PathDelim,SystemPPU);
    if p<1 then exit;
    FPCInstallDir:=copy(SystemPPU,1,p);// FPCInstallDir is now e.g. /usr/lib/
    FPCVersion:=copy(SystemPPU,p+5,length(SystemPPU));
    p:=System.Pos(PathDelim,FPCVersion);
    FPCVersion:=copy(FPCVersion,1,p-1);// FPCVersion is now e.g. 2.0.4
    DebugLn(['SearchInCommonInstallDir FPCInstallDir="',FPCInstallDir,'" FPCVersion="',FPCVersion,'"']);
    // try first with the current fpc version
    if (FPCVersion<>'') then begin
      if TryDirectory(FPCInstallDir
                      +SetDirSeparators('../share/doc/fpdocs-'+FPCVersion))
      then exit;
      if TryDirectory(FPCInstallDir+SetDirSeparators('doc/fpdocs-'+FPCVersion))
      then exit;
    end;
    // try any fpc version
    if TryDirectoryMask(FPCInstallDir
                        +SetDirSeparators('../share/doc/fpdocs-*'))
    then exit;
    if TryDirectoryMask(FPCInstallDir+SetDirSeparators('doc/fpdocs-*')) then
      exit;
  end;
  
begin
  if IsFPCDocsHTMDirectory(HelpOpts.GetEffectiveFPCDocsHTMLDirectory) then exit;
  // search the docs at common places
  if SearchInCommonInstallDir then exit;
  if TryDirectoryMask('/usr/share/doc/fpdocs-*') then exit;
  if TryDirectoryMask('/usr/local/share/doc/fpdocs-*') then exit;
end;

procedure TIDEHelpManager.ConnectMainBarEvents;
begin
  with MainIDEBar do
  begin
    itmHelpAboutLazarus.OnClick := @mnuHelpAboutLazarusClicked;
    itmHelpOnlineHelp.OnClick := @mnuHelpOnlineHelpClicked;
    itmHelpReportingBug.OnClick := @mnuHelpReportBugClicked;
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
  
  function ShowHelpForFPCKeyWord(const KeyWord: string): TShowHelpResult;
  var
    RefFilename: String;
    i: Integer;
    List: TStrings;
    Line: string;
    FileStartPos: Integer;
    FileEndPos: LongInt;
    HTMLFilename: String;
  begin
    Result:=shrHelpNotFound;
    if Keyword='' then exit;
    UpdateFPCDocsHTMLDirectory;
    RefFilename:=HelpOpts.GetEffectiveFPCDocsHTMLDirectory;
    if (RefFilename='') then exit;
    RefFilename:=AppendPathDelim(RefFilename)+'ref'+PathDelim+'ref.kwd';
    if not FileExistsUTF8(RefFilename) then begin
      DebugLn(['ShowHelpForFPCKeyWord file not found RefFilename="',RefFilename,'"']);
      exit;
    end;
    List:=nil;
    try
      if LoadStringListFromFile(RefFilename,'FPC keyword list',List)<>mrOk then
        exit;
      for i:=0 to List.Count-1 do begin
        // example: integer=refsu5.html#keyword:integer
        Line:=List[i];
        if (length(Line)>length(KeyWord))
        and (Line[length(KeyWord)+1]='=')
        and (SysUtils.CompareText(KeyWord,copy(Line,1,length(KeyWord)))=0) then
        begin
          FileStartPos:=length(KeyWord)+2;
          FileEndPos:=FileStartPos;
          while (FileEndPos<=length(Line)) and (Line[FileEndPos]<>'#') do
            inc(FileEndPos);
          HTMLFilename:=copy(Line,FileStartPos,FileEndPos-FileStartPos);
          HTMLFilename:=HelpOpts.GetEffectiveFPCDocsHTMLDirectory+'ref'
                        +PathDelim+HTMLFilename;
          Result:=ShowHelpFileOrError(HTMLFilename,
                              'FPC help for keyword "'+KeyWord+'"',
                              'text/html');
          break;
        end;
      end;
    finally
      List.Free;
    end;
  end;
  
  function CollectKeyWords(CodeBuffer: TCodeBuffer): TShowHelpResult;
  // true: help found
  var
    p: Integer;
    IdentStart, IdentEnd: integer;
    KeyWord: String;
  begin
    Result:=shrHelpNotFound;
    p:=0;
    CodeBuffer.LineColToPosition(CodePos.Y,CodePos.X,p);
    if p<1 then exit;
    GetIdentStartEndAtPosition(CodeBuffer.Source,p,IdentStart,IdentEnd);
    if IdentEnd<=IdentStart then exit;
    KeyWord:=copy(CodeBuffer.Source,IdentStart,IdentEnd-IdentStart);
    Result:=ShowHelpForFPCKeyWord(KeyWord);
  end;

  procedure CollectDeclarations(CodeBuffer: TCodeBuffer);
  var
    NewList: TPascalHelpContextList;
    PascalHelpContextLists: TList;
    ListOfPCodeXYPosition: TFPList;
    CurCodePos: PCodeXYPosition;
    i: Integer;
  begin
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
        debugln('TIDEHelpManager.ShowHelpForSourcePosition D PascalHelpContextLists.Count=',dbgs(PascalHelpContextLists.Count));
        Result:=ShowHelpForPascalContexts(Filename,CodePos,PascalHelpContextLists,ErrMsg);
      end else begin
        MainIDEInterface.DoJumpToCodeToolBossError;
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
    
  Result:=CollectKeyWords(CodeBuffer);
  if Result=shrSuccess then exit;
  CollectDeclarations(CodeBuffer);
end;

function TIDEHelpManager.ConvertCodePosToPascalHelpContext(
  ACodePos: PCodeXYPosition): TPascalHelpContextList;

  procedure AddContext(Descriptor: TPascalHelpContextType;
    const Context: string);
  var
    CurContext: TPascalHelpContext;
  begin
    CurContext.Descriptor:=Descriptor;
    CurContext.Context:=Context;
    Result.Add(CurContext);
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
      ShowContextHelpForIDE(AnInspector);
    end;
  end;
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
      Provider.ControlIntf.SetHTMLContent(ms);
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
    TheHint := '';
  end else begin
    HintWinRect := aHintWindow.CalcHintRect(Screen.Width, TheHint, nil);
  end;
  OffsetRect(HintWinRect, ScreenPos.X, ScreenPos.Y+30);

  Result:=true;
end;

function TIDEHelpManager.GetHintForSourcePosition(const ExpandedFilename: string;
  const CodePos: TPoint; out BaseURL, HTMLHint: string): TShowHelpResult;
var
  Code: TCodeBuffer;
  CacheWasUsed: boolean;
begin
  BaseURL:='';
  HTMLHint:='';
  Code:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
  if (Code=nil) or Code.LineColIsSpace(CodePos.Y,CodePos.X) then
    exit(shrHelpNotFound);
  if CodeHelpBoss.GetHTMLHint(Code,CodePos.X,CodePos.Y,
    [chhoSmartHint, chhoComplete, chhoComments],
    BaseURL,HTMLHint,CacheWasUsed)=chprSuccess
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

