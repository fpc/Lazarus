{
 /***************************************************************************
                            lpkcache.pas
                            ------------


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

  Author: Mattias Gaertner

  Abstract:
    Multithreaded scanner for lpk files to gather information about all
    available lpk files.

  Why this unit is needed:
    The *loaded* packages are handled by the PackageGraph (unit packagesystem).
    The IDE remembers all lpk files (file name and version) of the users
    disk in $(PrimaryConfigPath)/packagefiles.xml.
    The lpk files are often scattered on the disk, might be outdated,
    broken, wrong version or on slow network shares, so scanning them is
    expensive. That's why this is done in another thread.

  Usage:
    LPKInfoCache.StartLPKReaderWithAllAvailable;
    or LPKInfoCache.StartLPKReader(ListOfLPKFiles)

}
unit LPKCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg, LazLoggerBase, LazTracer, LazMethodList,
  // IdeIntf
  PackageDependencyIntf, PackageIntf, PackageLinkIntf,
  // IDE
  EnvironmentOpts, PackageLinks, PackageDefs, PackageSystem;

type
  TLPKInfoState = (
    lpkiNotParsed,
    lpkiParsing,
    lpkiParsedError,
    lpkiParsed
    );

  { TLPKInfo }

  TLPKInfo = class
  public
    ID: TLazPackageID; // name and version
    LPKFilename: string;
    InLazSrc: boolean; // lpk is in lazarus source directory
    Installed: TPackageInstallType;
    Base: boolean; // is base package, can not be uninstalled

    LPKParsed: TLPKInfoState;
    LPKError: string;

    // the below is only valid if TLPKInfoState=lpkiParsed
    Author: string;
    Description: string;
    License: string;
    PkgType: TLazPackageType; // design, runtime

    procedure Assign(Source: TObject);
    constructor Create(TheID: TLazPackageID);
    destructor Destroy; override;
  end;

  TLPKInfoCache = class;

  { TIPSLPKReader }

  TIPSLPKReader = class(TThread)
  protected
    procedure SynChangePkgVersion;
    procedure SynQueueEmpty;
    procedure Log(Msg: string);
    procedure Execute; override;
  public
    Cache: TLPKInfoCache;
    NewVersion: TPkgVersion;
    Info: TLPKInfo; // currently processed info
    Abort: boolean;
    FilenameQueue: TStrings; // list of file names to parse by the lpkreader thread
    destructor Destroy; override;
  end;

  TLPKInfoCacheEvent = (
    liceOnBeforeVersionChange,
    liceOnAfterVersionChange,
    liceOnQueueEmpty
    );
  TOnLPKInfoBeforeVersionChange = procedure(PkgInfo: TLPKInfo; NewID: TPkgVersion) of object;
  TOnLPKInfoAfterVersionChange = procedure(PkgInfo: TLPKInfo; OldID: string) of object;

  { TLPKInfoCache }

  TLPKInfoCache = class
  private
    FCritSec: TRTLCriticalSection;
    FLPKReader: TIPSLPKReader;
    fLPKByFilename: TAvlTree; // tree of TLPKInfo sorted for LPKFilename
    fLPKByID: TAvlTree; // tree of TLPKInfo sorted for ID
    fEvents: array[TLPKInfoCacheEvent] of TMethodList;
    fAvailableFiles: TStrings; // used by OnIterateAvailablePackages
    procedure QueueEmpty;
    procedure OnIterateAvailablePackages(APackage: TLazPackageID);
  public
    constructor Create;
    destructor Destroy; override;

    // call by main thread only
    procedure StartLPKReaderWithAllAvailable;
    procedure StartLPKReader(Filenames: TStrings);
    procedure EndLPKReader;
    procedure ParseLPKInfoInMainThread(Info: TLPKInfo);
    procedure AddOnBeforeVersionChange(const OnBefore: TOnLPKInfoBeforeVersionChange;
      AsLast: boolean = true);
    procedure RemoveOnBeforeVersionChange(const OnBefore: TOnLPKInfoBeforeVersionChange);
    procedure AddOnAfterVersionChange(const OnAfter: TOnLPKInfoAfterVersionChange;
      AsLast: boolean = true);
    procedure RemoveOnAfterVersionChange(const OnAfter: TOnLPKInfoAfterVersionChange);
    procedure AddOnQueueEmpty(const OnEmpty: TNotifyEvent; AsLast: boolean = true);
    procedure RemoveOnQueueEmpty(const OnEmpty: TNotifyEvent);
    procedure ChangePkgVersion(PkgInfo: TLPKInfo; NewVersion: TPkgVersion);

    // requires critical section
    procedure EnterCritSection;
    procedure LeaveCritSection;
    function FindPkgInfoWithFilename(aFilename: string): TLPKInfo; // requires crit sec
    function FindPkgInfoWithID(PkgID: TLazPackageID): TLPKInfo; // requires crit sec
    function FindPkgInfoWithIDAsString(PkgID: string): TLPKInfo; // requires crit sec
    property LPKByFilename: TAvlTree read fLPKByFilename; // tree of TLPKInfo sorted for LPKFilename
    property LPKByID: TAvlTree read fLPKByID; // tree of TLPKInfo sorted for ID

    // thread safe
    function IsValidLPKFilename(LPKFilename: string): boolean;
    procedure ParseLPK(LPKFilename: string;
      out ErrorMsg, Author, License, Description: string;
      out PkgType: TLazPackageType;
      var Version: TPkgVersion); // called by main and helper thread
    procedure ParseLPKInfo(Info: TLPKInfo; var NewVersion: TPkgVersion);
  end;

var
  LPKInfoCache: TLPKInfoCache = nil; // set by main.pp

function CompareIPSPkgInfos(PkgInfo1, PkgInfo2: Pointer): integer;
function ComparePkgIDWithIPSPkgInfo(PkgID, PkgInfo: Pointer): integer;
function CompareIPSPkgInfosWithFilename(PkgInfo1, PkgInfo2: Pointer): integer;
function CompareFilenameWithIPSPkgInfo(Filename, PkgInfo: Pointer): integer;

implementation

function CompareIPSPkgInfos(PkgInfo1, PkgInfo2: Pointer): integer;
var
  Info1: TLPKInfo absolute PkgInfo1;
  Info2: TLPKInfo absolute PkgInfo2;
begin
  Result:=CompareLazPackageIDNames(Info1.ID,Info2.ID);
end;

function ComparePkgIDWithIPSPkgInfo(PkgID, PkgInfo: Pointer): integer;
var
  ID: TLazPackageID absolute PkgID;
  Info: TLPKInfo absolute PkgInfo;
begin
  Result:=CompareLazPackageIDNames(ID,Info.ID);
end;

function CompareIPSPkgInfosWithFilename(PkgInfo1, PkgInfo2: Pointer): integer;
var
  Info1: TLPKInfo absolute PkgInfo1;
  Info2: TLPKInfo absolute PkgInfo2;
begin
  Result:=CompareFilenames(Info1.LPKFilename,Info2.LPKFilename);
end;

function CompareFilenameWithIPSPkgInfo(Filename, PkgInfo: Pointer): integer;
var
  Info: TLPKInfo absolute PkgInfo;
begin
  Result:=CompareFilenames(AnsiString(Filename),Info.LPKFilename);
end;

{ TLPKInfoCache }

procedure TLPKInfoCache.StartLPKReader(Filenames: TStrings);
var
  i: Integer;
  CurFilename: String;
  Info: TLPKInfo;
  ID: TLazPackageID;
  NeedsStart: Boolean;
  Pkg: TLazPackage;
begin
  if (Filenames=nil) or (Filenames.Count=0) then begin
    QueueEmpty;
    exit;
  end;
  NeedsStart:=false;
  EnterCritSection;
  try
    for i:=Filenames.Count-1 downto 0 do
    begin
      CurFilename:=Filenames[i];
      if not IsValidLPKFilename(CurFilename) then continue;
      Info:=FindPkgInfoWithFilename(CurFilename);
      if Info<>nil then begin
        // info is known
        if Info.LPKParsed<>lpkiNotParsed then continue;
      end else begin
        // new info
        ID:=TLazPackageID.Create;
        ID.Name:=ExtractFileNameOnly(CurFilename);
        Info:=TLPKInfo.Create(ID);
        Info.LPKFilename:=CurFilename;
        Info.InLazSrc:=FileIsInPath(Info.LPKFilename,
                                  EnvironmentOptions.GetParsedLazarusDirectory);
        Info.Base:=Info.InLazSrc and PackageGraph.IsStaticBasePackage(Info.ID.Name);
        Pkg:=PackageGraph.FindPackageWithFilename(Info.LPKFilename);
        if Pkg<>nil then
          Info.Installed:=Pkg.Installed;
        fLPKByFilename.Add(Info);
        fLPKByID.Add(Info);
      end;
      if FLPKReader=nil then begin
        // create thread
        FLPKReader:=TIPSLPKReader.Create(true);
        FLPKReader.Cache:=Self;
        FLPKReader.FreeOnTerminate:=true;
        FLPKReader.FilenameQueue:=TStringList.Create;
      end;
      FLPKReader.FilenameQueue.Add(Info.LPKFilename);
      NeedsStart:=true;
    end;
  finally
    LeaveCritSection;
  end;

  if NeedsStart then
    FLPKReader.Start
  else
    QueueEmpty;
end;

procedure TLPKInfoCache.EndLPKReader;
var
  i: Integer;
begin
  EnterCritSection;
  try
    if FLPKReader=nil then exit;
    FLPKReader.Abort:=true;
  finally
    LeaveCritSection;
  end;
  i:=0;
  while FLPKReader<>nil do begin
    Sleep(10);
    inc(i,10);
    if i>=1000 then begin
      debugln(['TLPKInfoCache.EndLPKReader still waiting for lpk reader to end ...']);
      i:=0;
    end;
  end;
end;

procedure TLPKInfoCache.ParseLPKInfoInMainThread(Info: TLPKInfo);
var
  NewVersion: TPkgVersion;
begin
  NewVersion:=nil;
  try
    ParseLPKInfo(Info,NewVersion);
    ChangePkgVersion(Info,NewVersion);
  finally
    NewVersion.Free;
  end;
end;

function TLPKInfoCache.IsValidLPKFilename(LPKFilename: string): boolean;
var
  PkgName: String;
begin
  Result:=false;
  if not FilenameIsAbsolute(LPKFilename) then exit;
  if CompareFilenames(ExtractFileExt(LPKFilename),'.lpk')<>0 then exit;
  PkgName:=ExtractFileNameOnly(LPKFilename);
  if not IsValidIdent(PkgName) then exit;
  Result:=true;
end;

procedure TLPKInfoCache.AddOnBeforeVersionChange(
  const OnBefore: TOnLPKInfoBeforeVersionChange; AsLast: boolean);
begin
  fEvents[liceOnBeforeVersionChange].Add(TMethod(OnBefore),AsLast);
end;

procedure TLPKInfoCache.RemoveOnBeforeVersionChange(
  const OnBefore: TOnLPKInfoBeforeVersionChange);
begin
  fEvents[liceOnBeforeVersionChange].Remove(TMethod(OnBefore));
end;

procedure TLPKInfoCache.AddOnAfterVersionChange(
  const OnAfter: TOnLPKInfoAfterVersionChange; AsLast: boolean);
begin
  fEvents[liceOnAfterVersionChange].Add(TMethod(OnAfter),AsLast);
end;

procedure TLPKInfoCache.RemoveOnAfterVersionChange(
  const OnAfter: TOnLPKInfoAfterVersionChange);
begin
  fEvents[liceOnAfterVersionChange].Remove(TMethod(OnAfter));
end;

procedure TLPKInfoCache.AddOnQueueEmpty(const OnEmpty: TNotifyEvent;
  AsLast: boolean);
begin
  fEvents[liceOnQueueEmpty].Add(TMethod(OnEmpty),AsLast);
end;

procedure TLPKInfoCache.RemoveOnQueueEmpty(const OnEmpty: TNotifyEvent);
begin
  fEvents[liceOnQueueEmpty].Remove(TMethod(OnEmpty));
end;

procedure TLPKInfoCache.OnIterateAvailablePackages(APackage: TLazPackageID);
begin
  if APackage is TLazPackage then
    fAvailableFiles.Add(TLazPackage(APackage).Filename)
  else if APackage is TLazPackageLink then begin
    {if (OPMInterface<>nil) and (TLazPackageLink(APackage).Origin=ploOnline) and
        (not OPMInterface.IsPackageAvailable(TLazPackageLink(APackage), 2)) then
      fAvailableFiles.Add(TLazPackageLink(APackage).OPMFileName)
    else}
      fAvailableFiles.Add(TLazPackageLink(APackage).LPKFilename);
  end;
end;

procedure TLPKInfoCache.QueueEmpty;
begin
  fEvents[liceOnQueueEmpty].CallNotifyEvents(Self);
end;

procedure TLPKInfoCache.ChangePkgVersion(PkgInfo: TLPKInfo;
  NewVersion: TPkgVersion);
var
  OldID: String;
  i: Integer;
begin
  if PkgInfo.ID.Version.Compare(NewVersion)=0 then exit;
  // notify before
  i:=fEvents[liceOnBeforeVersionChange].Count;
  while fEvents[liceOnBeforeVersionChange].NextDownIndex(i) do
    TOnLPKInfoBeforeVersionChange(fEvents[liceOnBeforeVersionChange].Items[i])(PkgInfo,NewVersion);
  // change
  fLPKByID.Remove(PkgInfo);
  OldID:=PkgInfo.ID.IDAsString;
  PkgInfo.ID.Version.Assign(NewVersion);
  fLPKByID.Add(PkgInfo);
  // notify after
  i:=fEvents[liceOnAfterVersionChange].Count;
  while fEvents[liceOnAfterVersionChange].NextDownIndex(i) do
    TOnLPKInfoAfterVersionChange(fEvents[liceOnAfterVersionChange].Items[i])(PkgInfo,OldID);
end;

function TLPKInfoCache.FindPkgInfoWithFilename(aFilename: string): TLPKInfo;
var
  Node: TAvlTreeNode;
begin
  Node:=fLPKByFilename.FindKey(Pointer(aFilename),@CompareFilenameWithIPSPkgInfo);
  if Node<>nil then
    Result:=TLPKInfo(Node.Data)
  else
    Result:=nil;
end;

function TLPKInfoCache.FindPkgInfoWithID(PkgID: TLazPackageID): TLPKInfo;
var
  Node: TAvlTreeNode;
begin
  Node:=fLPKByID.FindKey(Pointer(PkgID),@ComparePkgIDWithIPSPkgInfo);
  if Node<>nil then
    Result:=TLPKInfo(Node.Data)
  else
    Result:=nil;
end;

function TLPKInfoCache.FindPkgInfoWithIDAsString(PkgID: string): TLPKInfo;
var
  ID: TLazPackageID;
begin
  Result:=nil;
  ID:=TLazPackageID.Create;
  try
    if not ID.StringToID(PkgID) then exit;
    Result:=FindPkgInfoWithID(ID);
  finally
    ID.Free;
  end;
end;

procedure TLPKInfoCache.ParseLPK(LPKFilename: string; out ErrorMsg, Author,
  License, Description: string; out PkgType: TLazPackageType;
  var Version: TPkgVersion);
var
  Path: String;
  XMLConfig: TXMLConfig;
  FileVersion: Integer;
begin
  ErrorMsg:='';
  Author:='';
  License:='';
  Description:='';
  PkgType:=lptRunAndDesignTime;
  Version.Clear;
  if FilenameIsAbsolute(LPKFilename) and FileExistsUTF8(LPKFilename) then begin
    // load the package file
    try
      XMLConfig:=TXMLConfig.Create(LPKFilename);
      try
        Path:='Package/';
        FileVersion:=XMLConfig.GetValue(Path+'Version',0);
        Author:=XMLConfig.GetValue(Path+'Author/Value','');
        Description:=XMLConfig.GetValue(Path+'Description/Value','');
        License:=XMLConfig.GetValue(Path+'License/Value','');
        PkgType:=LazPackageTypeIdentToType(XMLConfig.GetValue(Path+'Type/Value',
                                                LazPackageTypeIdents[lptRunTime]));
        PkgVersionLoadFromXMLConfig(Version,XMLConfig,Path+'Version/',FileVersion);
      finally
        XMLConfig.Free;
      end;
    except
      on E: Exception do begin
        ErrorMsg:='file="'+LPKFilename+'": '+E.Message;
        DebugLn('TLPKInfoCache.ParseLPK ERROR: '+ErrorMsg);
      end;
    end;
  end else begin
    ErrorMsg:='file not found "'+LPKFilename+'"';
  end;
end;

procedure TLPKInfoCache.ParseLPKInfo(Info: TLPKInfo;
  var NewVersion: TPkgVersion);
// if not done, parse the lpk and update LPKError, LPKParsed,
// Author, Description, License, PkgType
// Version is not changed, but returned in NewVersion
var
  ErrorMsg: string;
  Author: string;
  License: string;
  Description: string;
  PkgType: TLazPackageType;
begin
  // check if alread parsed
  EnterCritSection;
  try
    if NewVersion=nil then begin
      NewVersion:=TPkgVersion.Create;
      NewVersion.Assign(Info.ID.Version);
    end;
    if Info.LPKParsed<>lpkiNotParsed then exit;
    Info.LPKParsed:=lpkiParsing;
  finally
    LeaveCritSection;
  end;

  // parse
  ParseLPK(Info.LPKFilename,ErrorMsg,Author,License,Description,PkgType,NewVersion);

  // change info
  // Note: the version is not changed
  EnterCritSection;
  try
    if Info.LPKParsed<>lpkiParsing then exit;
    if ErrorMsg<>'' then begin
      Info.LPKError:=ErrorMsg;
      Info.LPKParsed:=lpkiParsedError;
    end else begin
      Info.LPKError:='';
      Info.LPKParsed:=lpkiParsed;
      Info.Author:=Author;
      Info.Description:=Description;
      Info.License:=License;
      Info.PkgType:=PkgType;
    end;
  finally
    LeaveCritSection;
  end;
end;

constructor TLPKInfoCache.Create;
var
  e: TLPKInfoCacheEvent;
begin
  InitCriticalSection(FCritSec);
  fLPKByFilename:=TAvlTree.Create(@CompareIPSPkgInfosWithFilename);
  fLPKByID:=TAvlTree.Create(@CompareIPSPkgInfos);
  for e:=Low(TLPKInfoCacheEvent) to high(TLPKInfoCacheEvent) do
    fEvents[e]:=TMethodList.Create;
end;

destructor TLPKInfoCache.Destroy;
var
  e: TLPKInfoCacheEvent;
begin
  EndLPKReader;
  FreeAndNil(fLPKByID);
  fLPKByFilename.FreeAndClear;
  FreeAndNil(fLPKByFilename);
  for e:=Low(TLPKInfoCacheEvent) to high(TLPKInfoCacheEvent) do
    FreeAndNil(fEvents[e]);
  inherited Destroy;
  DoneCriticalsection(FCritSec);
end;

procedure TLPKInfoCache.StartLPKReaderWithAllAvailable;
begin
  fAvailableFiles:=TStringList.Create;
  try
    PackageGraph.IteratePackages(fpfSearchAllExisting,@OnIterateAvailablePackages);
    StartLPKReader(fAvailableFiles);
  finally
    FreeAndNil(fAvailableFiles);
  end;
end;

procedure TLPKInfoCache.EnterCritSection;
begin
  EnterCriticalsection(FCritSec);
end;

procedure TLPKInfoCache.LeaveCritSection;
begin
  LeaveCriticalsection(FCritSec);
end;

{ TIPSLPKReader }

procedure TIPSLPKReader.Execute;
begin
  try
    while not Abort do begin
      // get next lpk to parse
      Cache.EnterCritSection;
      try
        Info:=nil;
        while FilenameQueue.Count>0 do begin
          Info:=Cache.FindPkgInfoWithFilename(FilenameQueue[FilenameQueue.Count-1]);
          FilenameQueue.Delete(FilenameQueue.Count-1);
          if Info=nil then continue;
          if Info.LPKParsed=lpkiNotParsed then
            break
          else
            Info:=nil;
        end;
        if Info=nil then break;
      finally
        Cache.LeaveCritSection;
      end;
      Cache.ParseLPKInfo(Info,NewVersion);
      if NewVersion.Compare(Info.ID.Version)<>0 then begin
        Synchronize(@SynChangePkgVersion);
      end;
      Info:=nil;
    end;
  except
    on E: Exception do begin
      Log('ERROR: TIPSLPKReader.Execute: '+E.Message);
    end;
  end;

  Synchronize(@SynQueueEmpty);

  Cache.EnterCritSection;
  try
    Cache.FLPKReader:=nil;
  finally
    Cache.LeaveCritSection;
  end;
end;

procedure TIPSLPKReader.SynChangePkgVersion;
begin
  Cache.ChangePkgVersion(Info,NewVersion);
end;

procedure TIPSLPKReader.SynQueueEmpty;
begin
  Cache.QueueEmpty;
end;

procedure TIPSLPKReader.Log(Msg: string);
begin
  debugln(['TIPSLPKReader.Log: ',Msg]);
end;

destructor TIPSLPKReader.Destroy;
begin
  FreeAndNil(FilenameQueue);
  FreeAndNil(NewVersion);
  inherited Destroy;
end;

{ TLPKInfo }

constructor TLPKInfo.Create(TheID: TLazPackageID);
begin
  ID:=TheID;
end;

procedure TLPKInfo.Assign(Source: TObject);
var
  SrcInfo: TLPKInfo;
  SrcID: TLazPackageID;
begin
  if Source is TLPKInfo then
  begin
    SrcInfo:=TLPKInfo(Source);
    PkgType:=SrcInfo.PkgType;
    LPKParsed:=SrcInfo.LPKParsed;
    LPKFilename:=SrcInfo.LPKFilename;
    LPKError:=SrcInfo.LPKError;
    License:=SrcInfo.License;
    Installed:=SrcInfo.Installed;
    InLazSrc:=SrcInfo.InLazSrc;
    ID.AssignID(SrcInfo.ID);
    Description:=SrcInfo.Description;
    Base:=SrcInfo.Base;
    Author:=SrcInfo.Author;
  end else if Source is TLazPackageID then begin
    SrcID:=TLazPackageID(Source);
    ID.AssignID(SrcID);
  end else
    RaiseGDBException('');
end;

destructor TLPKInfo.Destroy;
begin
  FreeAndNil(ID);
  inherited Destroy;
end;

end.

