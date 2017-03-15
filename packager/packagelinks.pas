{
 /***************************************************************************
                            packagelinks.pas
                            ----------------


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
    Package links helps the IDE to find package filenames by name.
    If you are searching for the dialog to see the package links: pkglinksdlg.pas

}
unit PackageLinks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, LCLProc,
  // LazUtils
  Laz2_XMLCfg, LazFileCache, AvgLvlTree, LazUTF8Classes, LazFileUtils, FileUtil,
  // Codetools
  FileProcs, CodeToolManager, CodeToolsStructs,
  // IdeIntf
  MacroIntf, PackageDependencyIntf, PackageLinkIntf, PackageIntf,
  // IDE
  IDEProcs, EnvironmentOpts, LazConf, IDECmdLine, PackageDefs;
  
const
  PkgLinksFileVersion = 3;
  { 3: changed "LastUsed" from day to seconds, so that last used lpk is loaded
       after IDE restart }

type
  TLazPackageLink = class(TPackageLink)
  private
    FAutoCheckExists: boolean;
    fReferenceCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEffectiveFilename: string; override;
    procedure Reference; override;
    procedure Release; override;
  public
    property AutoCheckExists: boolean read FAutoCheckExists write FAutoCheckExists;
  end;

  { TLazPackageLinks }
  
  TLazPackageLinks = class;

  TPkgLinksState = (
    plsUserLinksNeedUpdate,
    plsGlobalLinksNeedUpdate
    );
  TPkgLinksStates = set of TPkgLinksState;
  
  TLazPackageLinks = class(TPackageLinks)
  private
    FGlobalLinks: TAvgLvlTree; // tree of global TLazPackageLink sorted for ID
    FChangeStamp: integer;
    FQueueSaveUserLinks: boolean;
    FSavedChangeStamp: integer;
    FUserLinksSortID: TAvgLvlTree; // tree of user TLazPackageLink sorted for ID
    FUserLinksSortFile: TAvgLvlTree; // tree of user TLazPackageLink sorted for
                                     // Filename and FileDate
    fUpdateLock: integer;
    FStates: TPkgLinksStates;
    function FindLeftMostNode(LinkTree: TAvgLvlTree;
      const PkgName: string): TAvgLvlTreeNode;
    function FindLinkWithPkgNameInTree(LinkTree: TAvgLvlTree;
      const PkgName: string; IgnoreFiles: TFilenameToStringTree): TLazPackageLink;
    function FindLinkWithDependencyInTree(LinkTree: TAvgLvlTree;
      Dependency: TPkgDependencyID; IgnoreFiles: TFilenameToStringTree): TLazPackageLink;
    function FindLinkWithPackageIDInTree(LinkTree: TAvgLvlTree;
      APackageID: TLazPackageID): TLazPackageLink;
    function FindLinkWithLPKFilenameInTree(LinkTree: TAvgLvlTree;
      const PkgName, LPKFilename: string): TLazPackageLink;
    function GetModified: boolean;
    procedure IteratePackagesInTree(MustExist: boolean; LinkTree: TAvgLvlTree;
      Event: TIteratePackagesEvent);
    procedure SetModified(const AValue: boolean);
    procedure SetQueueSaveUserLinks(AValue: boolean);
    procedure OnAsyncSaveUserLinks({%H-}Data: PtrInt);
    function GetNewerLink(Link1, Link2: TLazPackageLink): TLazPackageLink;
  public
    UserLinkLoadTime: longint;
    UserLinkLoadTimeValid: boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function GetUserLinkFile(WithPath: boolean = true): string;
    function GetGlobalLinkDirectory: string;
    procedure UpdateGlobalLinks; // reloads the lpl files, keeping LastUsed dates
    procedure UpdateUserLinks; // reloads user links and global LastUsed dates
    procedure UpdateAll;
    procedure RemoveOldUserLinks;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    procedure SaveUserLinks(Immediately: boolean = false);
    function NeedSaveUserLinks(const ConfigFilename: string): boolean;
    procedure WriteLinkTree(LinkTree: TAvgLvlTree);
    procedure IncreaseChangeStamp;
    // Methods defined as interface in TLazPackageLinks.
    function FindLinkWithPkgName(const PkgName: string): TPackageLink; override;
    function FindLinkWithPkgNameWithIgnore(const PkgName: string;
      IgnoreFiles: TFilenameToStringTree): TPackageLink;
    function FindLinkWithDependency(Dependency: TPkgDependencyID): TPackageLink; override;
    function FindLinkWithDependencyWithIgnore(Dependency: TPkgDependencyID;
      IgnoreFiles: TFilenameToStringTree): TPackageLink;
    function FindLinkWithPackageID(APackageID: TLazPackageID): TPackageLink; override;
    function FindLinkWithFilename(const PkgName, LPKFilename: string): TPackageLink; override;
    procedure IteratePackages(MustExist: boolean; Event: TIteratePackagesEvent;
      Origins: TPkgLinkOrigins = AllPkgLinkOrigins); override;
    function AddUserLink(APackage: TIDEPackage): TPackageLink; override;
    function AddUserLink(const PkgFilename, PkgName: string): TPackageLink; override;
    procedure RemoveUserLink(Link: TPackageLink); override;
    procedure RemoveUserLinks(APackageID: TLazPackageID); override;
  public
    property Modified: boolean read GetModified write SetModified;
    property ChangeStamp: integer read FChangeStamp;
    property QueueSaveUserLinks: boolean read FQueueSaveUserLinks write SetQueueSaveUserLinks;
  end;
  
var
  LazPackageLinks: TLazPackageLinks = nil; // set by the PkgBoss

function ComparePackageLinks(Data1, Data2: Pointer): integer;
function dbgs(Origin: TPkgLinkOrigin): string; overload;


implementation


function ComparePackageLinks(Data1, Data2: Pointer): integer;
var
  Link1: TLazPackageLink;
  Link2: TLazPackageLink;
begin
  Link1:=TLazPackageLink(Data1);
  Link2:=TLazPackageLink(Data2);
  Result:=Link1.Compare(Link2);
end;

function dbgs(Origin: TPkgLinkOrigin): string;
begin
  case Origin of
  ploGlobal: Result:='Global';
  ploUser: Result:='User';
  else Result:='?';
  end;
end;

function ComparePackageIDAndLink(Key, Data: Pointer): integer;
var
  Link: TLazPackageLink;
  PkgID: TLazPackageID;
begin
  if Key=nil then
    Result:=-1
  else begin
    PkgID:=TLazPackageID(Key);
    Link:=TLazPackageLink(Data);
    Result:=PkgID.Compare(Link);
  end;
end;

function ComparePkgNameAndLink(Key, Data: Pointer): integer;
var
  PkgName: String;
  Link: TLazPackageLink;
begin
  if Key=nil then
    Result:=-1
  else begin
    PkgName:=AnsiString(Key);
    Link:=TLazPackageLink(Data);
    Result:=CompareText(PkgName,Link.Name);
  end;
end;

function CompareLinksForFilename(Data1, Data2: Pointer): integer;
var
  Link1: TLazPackageLink absolute Data1;
  Link2: TLazPackageLink absolute Data2;
begin
  Result:=CompareFilenames(Link1.LPKFilename,Link2.LPKFilename);
end;

function CompareLinksForFilenameAndFileAge(Data1, Data2: Pointer): integer;
var
  Link1: TLazPackageLink absolute Data1;
  Link2: TLazPackageLink absolute Data2;
begin
  // first compare filenames
  Result:=CompareFilenames(Link1.LPKFilename,Link2.LPKFilename);
  if Result<>0 then exit;
  // then compare file date
  if Link1.LPKFileDateValid then begin
    if Link2.LPKFileDateValid then begin
      if Link1.LPKFileDate>Link2.LPKFileDate then
        Result:=1
      else if Link1.LPKFileDate<Link2.LPKFileDate then
        Result:=-1;
    end else begin
      Result:=1;
    end;
  end else begin
    if Link2.LPKFileDateValid then begin
      Result:=-1;
    end;
  end;
  if Result<>0 then exit;
  // finally compare version and name
  Result:=Link1.Compare(Link2);
end;

{ TLazPackageLink }

constructor TLazPackageLink.Create;
begin
  inherited Create;
  FAutoCheckExists:=true;
end;

destructor TLazPackageLink.Destroy;
begin
  //debugln('TPackageLink.Destroy ',IDAsString,' ',dbgs(Pointer(Self)));
  //if Origin=ploGlobal then RaiseException('');
  inherited Destroy;
end;

function TLazPackageLink.GetEffectiveFilename: string;
begin
  Result:=LPKFilename;
  if (not FilenameIsAbsolute(Result)) then
    Result:=TrimFilename(EnvironmentOptions.GetParsedLazarusDirectory+PathDelim+Result);
end;

procedure TLazPackageLink.Reference;
begin
  inc(fReferenceCount);
end;

procedure TLazPackageLink.Release;
begin
  if fReferenceCount<=0 then RaiseGDBException('');
  dec(fReferenceCount);
  if fReferenceCount=0 then Free;
end;

{ TLazPackageLinks }

procedure TLazPackageLinks.OnAsyncSaveUserLinks(Data: PtrInt);
begin
  SaveUserLinks(true);
end;

function TLazPackageLinks.GetNewerLink(Link1, Link2: TLazPackageLink): TLazPackageLink;
begin
  if Link1=nil then
    Result:=Link2
  else if Link2=nil then
    Result:=Link1
  else if Link1.LastUsed>Link2.LastUsed then
    Result:=Link1
  else
    Result:=Link2;

  {DbgOut('TPackageLinks.GetNewerLink ');
  if Link1<>nil then
    DbgOut(' Link1=',Link1.IDAsString,'=',DateToCfgStr(Link1.LastUsed,DateTimeAsCfgStrFormat))
  else
    DbgOut(' Link1=nil');
  if Link2<>nil then
    DbgOut(' Link2=',Link2.IDAsString,'=',DateToCfgStr(Link2.LastUsed,DateTimeAsCfgStrFormat))
  else
    DbgOut(' Link2=nil');
  if Result<>nil then
    DbgOut(' Result=',Result.IDAsString,'=',DateToCfgStr(Result.LastUsed,DateTimeAsCfgStrFormat))
  else
    DbgOut(' Result=nil');
  debugln;}
end;

function TLazPackageLinks.FindLeftMostNode(LinkTree: TAvgLvlTree;
  const PkgName: string): TAvgLvlTreeNode;
// find left most link with PkgName
begin
  Result:=nil;
  if PkgName='' then exit;
  Result:=LinkTree.FindLeftMostKey(PChar(PkgName),@ComparePkgNameAndLink);
end;

constructor TLazPackageLinks.Create;
begin
  PkgLinks:=Self;
  UserLinkLoadTimeValid:=false;
  FGlobalLinks:=TAvgLvlTree.Create(@ComparePackageLinks);
  FUserLinksSortID:=TAvgLvlTree.Create(@ComparePackageLinks);
  FUserLinksSortFile:=TAvgLvlTree.Create(@CompareLinksForFilenameAndFileAge);
  FSavedChangeStamp:=CTInvalidChangeStamp;
  FChangeStamp:=CTInvalidChangeStamp;
end;

destructor TLazPackageLinks.Destroy;
begin
  Clear;
  FreeAndNil(FGlobalLinks);
  FreeAndNil(FUserLinksSortID);
  FreeAndNil(FUserLinksSortFile);
  inherited Destroy;
end;

procedure TLazPackageLinks.Clear;
begin
  QueueSaveUserLinks:=false;
  FGlobalLinks.FreeAndClear;
  FUserLinksSortID.FreeAndClear;
  FUserLinksSortFile.Clear;
  FStates:=[plsUserLinksNeedUpdate,plsGlobalLinksNeedUpdate];
end;

function TLazPackageLinks.GetUserLinkFile(WithPath: boolean): string;
begin
  Result:='packagefiles.xml';
  if WithPath then
    Result:=AppendPathDelim(GetPrimaryConfigPath)+Result;
end;

function TLazPackageLinks.GetGlobalLinkDirectory: string;
begin
  Result:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory)
                                  +'packager'+PathDelim+'globallinks'+PathDelim;
end;

procedure TLazPackageLinks.UpdateGlobalLinks;

  function ParseFilename(const Filename: string;
    out PkgName: string; PkgVersion: TPkgVersion): boolean;
  // checks if filename has the form
  // <identifier>-<version>.lpl
  var
    StartPos: Integer;
    i: Integer;
    EndPos: Integer;
    ints: array[1..4] of integer;
  begin
    Result:=false;
    PkgName:='';
    if CompareFileExt(Filename,'.lpl',false)<>0 then exit;
    StartPos:=1;
    // parse identifier
    if (StartPos>length(Filename))
    or (not (Filename[StartPos] in ['a'..'z','A'..'Z'])) then exit;
    inc(StartPos);
    while (StartPos<=length(Filename))
    and (Filename[StartPos] in ['a'..'z','A'..'Z','_','0'..'9']) do
      inc(StartPos);
    PkgName:=lowercase(copy(Filename,1,StartPos-1));
    // parse -
    if (StartPos>length(Filename)) or (Filename[StartPos]<>'-') then exit;
    inc(StartPos);
    // parse version (1-4 times 'int.')
    for i:=Low(ints) to High(ints) do ints[i]:=0;
    i:=Low(ints);
    while i<=High(ints) do begin
      // parse int
      EndPos:=StartPos;
      while (EndPos<=length(Filename))
      and (Filename[EndPos] in ['0'..'9']) do inc(EndPos);
      ints[i]:=StrToIntDef(copy(Filename,StartPos,EndPos-StartPos),-1);
      if (ints[i]<0) or (ints[i]>99999) then exit;
      StartPos:=EndPos;
      // parse .
      if (StartPos>length(Filename)) or (Filename[StartPos]<>'.') then exit;
      if StartPos=length(Filename)-length('lpl') then break;
      inc(StartPos);
      inc(i);
    end;
    PkgVersion.Major:=ints[1];
    PkgVersion.Minor:=ints[2];
    PkgVersion.Release:=ints[3];
    PkgVersion.Build:=ints[4];
    Result:=true;
  end;

var
  GlobalLinksDir: String;
  NewPkgName: string;
  PkgVersion: TPkgVersion;
  CurPkgLink, OldPkgLink, OtherPkgLink: TLazPackageLink;
  sl: TStringListUTF8;
  LPLFilename: String;
  LPKFilename, LazDir: string;
  Files: TStrings;
  i: Integer;
  OldNode, OtherNode: TAvgLvlTreeNode;
  UnmappedGlobalLinks, MappedGlobalLinks: TAvgLvlTree;
begin
  if fUpdateLock>0 then begin
    Include(FStates,plsGlobalLinksNeedUpdate);
    exit;
  end;
  Exclude(FStates,plsGlobalLinksNeedUpdate);

  {$IFDEF VerboseGlobalPkgLinks}
  debugln(['TPackageLinks.UpdateGlobalLinks START']);
  {$ENDIF}
  UnmappedGlobalLinks:=FGlobalLinks;
  FGlobalLinks:=TAvgLvlTree.Create(@ComparePackageLinks);
  MappedGlobalLinks:=TAvgLvlTree.Create(@ComparePackageLinks);
  Files:=TStringListUTF8.Create;
  PkgVersion:=TPkgVersion.Create;
  try
    GlobalLinksDir:=GetGlobalLinkDirectory;

    CodeToolBoss.DirectoryCachePool.GetListing(GlobalLinksDir,Files,false);
    LazDir:=EnvironmentOptions.GetParsedLazarusDirectory;
    for i:=0 to Files.Count-1 do begin
      LPLFilename:=GlobalLinksDir+Files[i];
      if CompareFileExt(LPLFilename,'lpl')<>0 then continue;
      if (not ParseFilename(Files[i],NewPkgName,PkgVersion))
      then begin
        DebugLn('Warning: (lazarus) suspicious pkg link file found (name): ',LPLFilename);
        continue;
      end;
      LPKFilename:='';
      sl:=TStringListUTF8.Create;
      try
        sl.LoadFromFile(LPLFilename);
        if sl.Count<=0 then begin
          DebugLn('Warning: (lazarus) pkg link file is empty: ',LPLFilename);
          continue;
        end;
        LPKFilename:=GetForcedPathDelims(sl[0]);
      except
        on E: Exception do begin
          DebugLn('Warning: (lazarus) unable to read pkg link file: ',LPLFilename,' : ',E.Message);
        end;
      end;
      sl.Free;
      if LPKFilename='' then begin
        debugln(['Warning: (lazarus) TPackageLinks.UpdateGlobalLinks lpl file has empty first line: ',LPLFilename]);
        continue;
      end;
      //debugln(['TPackageLinks.UpdateGlobalLinks NewFilename="',LPKFilename,'"']);

      CurPkgLink:=TLazPackageLink.Create;
      CurPkgLink.Reference;
      CurPkgLink.Origin:=ploGlobal;
      CurPkgLink.LPLFilename:=LPLFilename;
      CurPkgLink.LPLFileDate:=FileAgeCached(LPLFilename);
      CurPkgLink.Name:=NewPkgName;
      CurPkgLink.Version.Assign(PkgVersion);
      IDEMacros.SubstituteMacros(LPKFilename);
      //debugln(['TPackageLinks.UpdateGlobalLinks EnvironmentOptions.LazarusDirectory=',LazDir]);
      LPKFilename:=TrimFilename(LPKFilename);
      if (FileIsInDirectory(LPKFilename,LazDir)) then
        LPKFilename:=CreateRelativePath(LPKFilename,LazDir);
      CurPkgLink.LPKFilename:=LPKFilename;
      //debugln('TPackageLinks.UpdateGlobalLinks PkgName="',CurPkgLink.Name,'" ',
      //  ' PkgVersion=',CurPkgLink.Version.AsString,
      //  ' Filename="',CurPkgLink.LPKFilename,'"',
      //  ' MakeSense=',dbgs(CurPkgLink.IsMakingSense));
      if CurPkgLink.IsMakingSense then begin
        OldNode:=UnmappedGlobalLinks.Find(CurPkgLink);
        if OldNode<>nil then begin
          // keep LastUsed date for global link
          OldPkgLink:=TLazPackageLink(OldNode.Data);
          CurPkgLink.LastUsed:=OldPkgLink.LastUsed;
          UnmappedGlobalLinks.Delete(OldNode);
          MappedGlobalLinks.Add(OldPkgLink);
          //if CompareText(OldPkgLink.Name,'lclbase')=0 then
          //  debugln(['TPackageLinks.UpdateGlobalLinks keeping LastUsed of '+OldPkgLink.Name,' ',DateToCfgStr(OldPkgLink.LastUsed,DateTimeAsCfgStrFormat)]);
        end;
        FGlobalLinks.Add(CurPkgLink);
      end else begin
        debugln('Warning: (lazarus) TPackageLinks.UpdateGlobalLinks Invalid lpl "',LPLFilename,'"'
          ,' PkgName="',CurPkgLink.Name,'" '
          ,' PkgVersion=',CurPkgLink.Version.AsString
          ,' Filename="',CurPkgLink.LPKFilename,'"');
        CurPkgLink.Release;
      end;
    end;

    // map unmapped global links (e.g. a package version changed)
    // Note: When the IDE knows several versions of a lpk it loads the last one
    //       used (i.e. highest LastUsed date). When the version of the lpk
    //       increased on disk (e.g. svn update or user installed a new Lazarus
    //       version) the LastUsed date must be moved to the new lpk.
    OldNode:=UnmappedGlobalLinks.FindLowest;
    while OldNode<>nil do begin
      OldPkgLink:=TLazPackageLink(OldNode.Data);
      // this old lpl was not found in the new lpl files
      //debugln(['TPackageLinks.UpdateGlobalLinks formerly used lpl '+OldPkgLink.IDAsString+' not found in new lpl directory -> searching new lpl ...']);
      OldNode:=UnmappedGlobalLinks.FindSuccessor(OldNode);
      OtherNode:=FindLeftMostNode(FGlobalLinks,OldPkgLink.Name);
      while OtherNode<>nil do begin
        OtherPkgLink:=TLazPackageLink(OtherNode.Data);
        if CompareText(OtherPkgLink.Name,OldPkgLink.Name)<>0 then break;
        OtherNode:=FGlobalLinks.FindSuccessor(OtherNode);
        if MappedGlobalLinks.Find(OtherPkgLink)<>nil then continue;
        // found a new lpl for the old lpl
        if not UnmappedGlobalLinks.Remove(OldPkgLink) then
          debugln(['TPackageLinks.UpdateGlobalLinks inconsistency UnmappedGlobalLinks.Remove']);
        MappedGlobalLinks.Add(OldPkgLink);
        if OtherPkgLink.LastUsed<OldPkgLink.LastUsed then begin
          debugln(['Hint: (lazarus) [TPackageLinks.UpdateGlobalLinks] using LastUsed date of '+OldPkgLink.IDAsString+' for new '+OtherPkgLink.IDAsString+' in '+OtherPkgLink.LPKFilename]);
          OtherPkgLink.LastUsed:=OldPkgLink.LastUsed;
        end;
        break;
      end;
    end;

    //WriteLinkTree(FGlobalLinks);
  finally
    Files.Free;
    PkgVersion.Free;
    UnmappedGlobalLinks.FreeAndClear;
    UnmappedGlobalLinks.Free;
    MappedGlobalLinks.FreeAndClear;
    MappedGlobalLinks.Free;
  end;
end;

procedure TLazPackageLinks.UpdateUserLinks;
var
  ConfigFilename: String;
  Path: String;
  XMLConfig: TXMLConfig;
  LinkCount: Integer;
  i: Integer;
  NewPkgLink: TLazPackageLink;
  ItemPath: String;
  FileVersion: LongInt;
  LastUsedFormat: String;
  OtherNode, ANode: TAvgLvlTreeNode;
  OtherLink: TLazPackageLink;
  UnmappedGlobalLinks, MappedGlobalLinks: TAvgLvlTree;
begin
  if fUpdateLock>0 then begin
    Include(FStates,plsUserLinksNeedUpdate);
    exit;
  end;
  Exclude(FStates,plsUserLinksNeedUpdate);

  // check if file has changed
  ConfigFilename:=GetUserLinkFile;
  if UserLinkLoadTimeValid and FileExistsCached(ConfigFilename)
  and (FileAgeCached(ConfigFilename)=UserLinkLoadTime) then
    exit;
  
  // copy system default if needed
  CopySecondaryConfigFile(GetUserLinkFile(false));
  
  FUserLinksSortID.FreeAndClear;
  FUserLinksSortFile.Clear;
  IncreaseChangeStamp;
  FileVersion:=PkgLinksFileVersion;
  XMLConfig:=nil;
  try
    XMLConfig:=TXMLConfig.Create(ConfigFilename);

    // load user links
    Path:='UserPkgLinks/';
    FileVersion:=XMLConfig.GetValue(Path+'Version',0);
    LinkCount:=XMLConfig.GetValue(Path+'Count',0);
    if FileVersion<3 then
      LastUsedFormat:=DateAsCfgStrFormat
    else
      LastUsedFormat:=DateTimeAsCfgStrFormat;
    for i:=1 to LinkCount do begin
      ItemPath:=Path+'Item'+IntToStr(i)+'/';
      NewPkgLink:=TLazPackageLink.Create;
      NewPkgLink.Reference;
      NewPkgLink.Origin:=ploUser;
      NewPkgLink.Name:=XMLConfig.GetValue(ItemPath+'Name/Value','');
      PkgVersionLoadFromXMLConfig(NewPkgLink.Version,XMLConfig,ItemPath+'Version/',
                                                          FileVersion);
      NewPkgLink.LPKFilename:=XMLConfig.GetValue(ItemPath+'Filename/Value','');
      NewPkgLink.AutoCheckExists:=
                      XMLConfig.GetValue(ItemPath+'AutoCheckExists/Value',true);
                      
      NewPkgLink.LPKFileDateValid:=
                       XMLConfig.GetValue(ItemPath+'FileDateValid/Value',false);
      if NewPkgLink.LPKFileDateValid then begin
        NewPkgLink.LPKFileDateValid:=
                  CfgStrToDate(XMLConfig.GetValue(ItemPath+'FileDate/Value',''),
                               NewPkgLink.FFileDate);
      end;
      
      if not CfgStrToDate(XMLConfig.GetValue(ItemPath+'LastUsed/Value',''),
                            NewPkgLink.FLastUsed,LastUsedFormat)
      then
        NewPkgLink.FLastUsed := 0;
      //if CompareText(NewPkgLink.Name,'lclbase')=0 then
      //  debugln(['TPackageLinks.UpdateUserLinks ',NewPkgLink.IDAsString,' ',DateToCfgStr(NewPkgLink.LastUsed,DateTimeAsCfgStrFormat)]);

      if not NewPkgLink.IsMakingSense then begin
        debugln(['Warning: (lazarus) TPackageLinks.UpdateUserLinks invalid link: ',NewPkgLink.IDAsString]);
        NewPkgLink.Release;
        continue;
      end;
      OtherNode:=FUserLinksSortFile.FindKey(NewPkgLink,@CompareLinksForFilename);
      if OtherNode<>nil then begin
        // a link to the same file
        OtherLink:=TLazPackageLink(OtherNode.Data);
        if ConsoleVerbosity>0 then
          debugln(['Warning: (lazarus) TPackageLinks.UpdateUserLinks two links for file: ',NewPkgLink.LPKFilename,' A=',OtherLink.IDAsString,' B=',NewPkgLink.IDAsString]);
        if OtherLink.LastUsed<NewPkgLink.LastUsed then begin
          if ConsoleVerbosity>0 then
            debugln(['Warning: (lazarus) TPackageLinks.UpdateUserLinks ignoring older link ',OtherLink.IDAsString]);
          FUserLinksSortID.RemovePointer(OtherLink);
          FUserLinksSortFile.Delete(OtherNode);
          OtherLink.Release;
        end else begin
          if ConsoleVerbosity>0 then
            debugln(['Warning: (lazarus) TPackageLinks.UpdateUserLinks ignoring older link ',NewPkgLink.IDAsString]);
          NewPkgLink.Release;
          continue;
        end;
      end;

      FUserLinksSortID.Add(NewPkgLink);
      FUserLinksSortFile.Add(NewPkgLink);
    end;

    // load LastUsed dates of global links
    Path:='GlobalPkgLinks/';
    LinkCount:=XMLConfig.GetValue(Path+'Count',0);
    UnmappedGlobalLinks:=TAvgLvlTree.Create(@ComparePackageLinks);
    MappedGlobalLinks:=TAvgLvlTree.Create(@ComparePackageLinks);
    try
      for i:=1 to LinkCount do begin
        ItemPath:=Path+'Item'+IntToStr(i)+'/';
        NewPkgLink:=TLazPackageLink.Create; // create temporary TLazPackageLink

        if not CfgStrToDate(XMLConfig.GetValue(ItemPath+'LastUsed/Value',''),
                            NewPkgLink.FLastUsed,LastUsedFormat)
        then begin
          debugln(['Hint: (lazarus) [TPackageLinks.UpdateUserLinks] ignoring invalid entry '+ItemPath]);
          NewPkgLink.Free;
          continue;
        end;

        NewPkgLink.Name:=XMLConfig.GetValue(ItemPath+'Name/Value','');
        PkgVersionLoadFromXMLConfig(NewPkgLink.Version,XMLConfig,ItemPath+'Version/',
                                                          FileVersion);
        if not IsValidPkgName(NewPkgLink.Name) then begin
          debugln(['Hint: (lazarus) [TPackageLinks.UpdateUserLinks] ignoring invalid global link LastUsed of '+NewPkgLink.IDAsString]);
          NewPkgLink.Free;
          continue;
        end;
        //if CompareText(NewPkgLink.Name,'lclbase')=0 then
        //  debugln(['TPackageLinks.UpdateUserLinks ',NewPkgLink.IDAsString,' LastUsed=',DateToCfgStr(NewPkgLink.LastUsed,DateTimeAsCfgStrFormat)]);

        OtherNode:=FGlobalLinks.Find(NewPkgLink);
        if OtherNode<>nil then begin
          // global link (.lpl) still exists -> load LastUsed date
          OtherLink:=TLazPackageLink(OtherNode.Data);
          MappedGlobalLinks.Add(NewPkgLink);
          if OtherLink.LastUsed<NewPkgLink.LastUsed then
            OtherLink.LastUsed:=NewPkgLink.LastUsed;
          //if CompareText(OtherLink.Name,'lclbase')=0 then
          //  debugln(['TPackageLinks.UpdateUserLinks updating LastUsed of '+OtherLink.Name,' ',DateToCfgStr(OtherLink.LastUsed,DateTimeAsCfgStrFormat)]);
          continue;
        end;

        // this global link does not exist (e.g. the version has changed)
        // => check after all data was loaded
        if UnmappedGlobalLinks.Find(NewPkgLink)<>nil then
          NewPkgLink.Free
        else
          UnmappedGlobalLinks.Add(NewPkgLink);
      end;

      // map unmapped global links to new global links
      // Note: When the IDE knows several versions of a lpk it loads the last one
      //       used (i.e. highest LastUsed date). When the version of the lpk
      //       increased on disk (e.g. svn update or user installed a new Lazarus
      //       version) the LastUsed date must be moved to the new lpk.
      ANode:=UnmappedGlobalLinks.FindLowest;
      while ANode<>nil do begin
        NewPkgLink:=TLazPackageLink(ANode.Data);
        //debugln(['TPackageLinks.UpdateUserLinks LastUsed date of '+NewPkgLink.IDAsString+' has no lpl file -> searching a new lpl file ...']);
        ANode:=UnmappedGlobalLinks.FindSuccessor(ANode);
        // check all global links with same pkg name
        OtherNode:=FindLeftMostNode(FGlobalLinks,NewPkgLink.Name);
        while (OtherNode<>nil) do begin
          OtherLink:=TLazPackageLink(OtherNode.Data);
          if CompareText(OtherLink.Name,NewPkgLink.Name)<>0 then break;
          OtherNode:=FGlobalLinks.FindSuccessor(OtherNode);
          if MappedGlobalLinks.Find(OtherLink)<>nil then
            continue;// this lpl LastUsed date was already set
          // this lpl LastUsed date was not yet set => set it
          UnmappedGlobalLinks.Remove(NewPkgLink);
          MappedGlobalLinks.Add(NewPkgLink);
          if OtherLink.LastUsed<NewPkgLink.LastUsed then begin
            debugln(['Hint: (lazarus) [TPackageLinks.UpdateUserLinks] using LastUsed date of old '+NewPkgLink.IDAsString+' for '+OtherLink.IDAsString+' in '+OtherLink.LPKFilename]);
            OtherLink.LastUsed:=NewPkgLink.LastUsed;
          end;
          break;
        end;
      end;

    finally
      MappedGlobalLinks.FreeAndClear;
      MappedGlobalLinks.Free;
      UnmappedGlobalLinks.FreeAndClear;
      UnmappedGlobalLinks.Free;
    end;

    XMLConfig.Modified:=false;
    XMLConfig.Free;
    
    UserLinkLoadTime:=FileAgeCached(ConfigFilename);
    UserLinkLoadTimeValid:=true;
  except
    on E: Exception do begin
      DebugLn('Note: (lazarus) unable to read ',ConfigFilename,' ',E.Message);
      exit;
    end;
  end;
  RemoveOldUserLinks;
  Modified:=FileVersion<>PkgLinksFileVersion;
end;

procedure TLazPackageLinks.UpdateAll;
begin
  UpdateGlobalLinks;
  UpdateUserLinks;
end;

procedure TLazPackageLinks.RemoveOldUserLinks;
// search for links pointing to the same file but older version
var
  ANode: TAvgLvlTreeNode;
  NextNode: TAvgLvlTreeNode;
  OldPkgLink: TLazPackageLink;
  NewPkgLink: TLazPackageLink;
begin
  // sort UserLinks for filename
  ANode:=FUserLinksSortFile.FindLowest;
  while ANode<>nil do begin
    NextNode:=FUserLinksSortFile.FindSuccessor(ANode);
    if NextNode=nil then break;
    OldPkgLink:=TLazPackageLink(ANode.Data);
    NewPkgLink:=TLazPackageLink(NextNode.Data);
    if CompareFilenames(OldPkgLink.GetEffectiveFilename,
      NewPkgLink.GetEffectiveFilename)=0
    then begin
      // two links to the same file -> delete the older
      //debugln('TPackageLinks.RemoveOldUserLinks',
      // ' Newer=',NewPkgLink.IDAsString,'=',dbgs(Pointer(NewPkgLink)),
      // ' Older=',OldPkgLink.IDAsString,'=',dbgs(Pointer(OldPkgLink)));
      FUserLinksSortID.RemovePointer(OldPkgLink);
      FUserLinksSortFile.RemovePointer(OldPkgLink);
      OldPkgLink.Release;
    end;
    ANode:=NextNode;
  end;
end;

procedure TLazPackageLinks.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TLazPackageLinks.EndUpdate;
begin
  if fUpdateLock<=0 then RaiseException('TPackageLinks.EndUpdate');
  dec(fUpdateLock);
  if (plsGlobalLinksNeedUpdate in FStates) then UpdateGlobalLinks;
  if (plsUserLinksNeedUpdate in FStates) then UpdateUserLinks;
end;

function TLazPackageLinks.IsUpdating: boolean;
begin
  Result:=fUpdateLock>0;
end;

procedure TLazPackageLinks.SaveUserLinks(Immediately: boolean);
var
  ConfigFilename: String;
  Path: String;
  CurPkgLink: TLazPackageLink;
  XMLConfig: TXMLConfig;
  ANode: TAvgLvlTreeNode;
  ItemPath: String;
  i: Integer;
  LazSrcDir: String;
  AFilename: String;
begin
  //debugln(['TPackageLinks.SaveUserLinks ']);
  if (FUserLinksSortFile=nil) or (FUserLinksSortFile.Count=0) then exit;
  ConfigFilename:=GetUserLinkFile;
  
  // check if file needs saving
  if not NeedSaveUserLinks(ConfigFilename) then exit;
  if ConsoleVerbosity>1 then
    DebugLn(['Hint: (lazarus) TPackageLinks.SaveUserLinks saving ... ',ConfigFilename,' Modified=',Modified,' UserLinkLoadTimeValid=',UserLinkLoadTimeValid,' ',FileAgeUTF8(ConfigFilename)=UserLinkLoadTime,' Immediately=',Immediately]);

  if Immediately then begin
    QueueSaveUserLinks:=false;
  end else begin
    QueueSaveUserLinks:=true;
    exit;
  end;

  LazSrcDir:=EnvironmentOptions.GetParsedLazarusDirectory;

  XMLConfig:=nil;
  try
    XMLConfig:=TXMLConfig.CreateClean(ConfigFilename);

    // store user links
    Path:='UserPkgLinks/';
    XMLConfig.SetValue(Path+'Version',PkgLinksFileVersion);
    ANode:=FUserLinksSortID.FindLowest;
    i:=0;
    while ANode<>nil do begin
      CurPkgLink:=TLazPackageLink(ANode.Data);
      ANode:=FUserLinksSortID.FindSuccessor(ANode);

      inc(i);
      ItemPath:=Path+'Item'+IntToStr(i)+'/';
      XMLConfig.SetDeleteValue(ItemPath+'Name/Value',CurPkgLink.Name,'');
      //debugln(['TPackageLinks.SaveUserLinks ',CurPkgLink.Name,' ',dbgs(Pointer(CurPkgLink))]);
      PkgVersionSaveToXMLConfig(CurPkgLink.Version,XMLConfig,ItemPath+'Version/');

      // save package files in lazarus directory relative
      AFilename:=CurPkgLink.LPKFilename;
      if (LazSrcDir<>'') and FileIsInPath(AFilename,LazSrcDir) then begin
        AFilename:=CreateRelativePath(AFilename,LazSrcDir);
        //DebugLn(['TPackageLinks.SaveUserLinks ',AFilename]);
      end;
      XMLConfig.SetDeleteValue(ItemPath+'Filename/Value',AFilename,'');

      XMLConfig.SetDeleteValue(ItemPath+'LastUsed/Value',
                   DateToCfgStr(CurPkgLink.LastUsed,DateTimeAsCfgStrFormat),'');
    end;
    XMLConfig.SetDeleteValue(Path+'Count',i,0);

    // store LastUsed dates of global links
    Path:='GlobalPkgLinks/';
    XMLConfig.SetValue(Path+'Version',PkgLinksFileVersion);
    i:=0;
    ANode:=FGlobalLinks.FindLowest;
    while ANode<>nil do begin
      CurPkgLink:=TLazPackageLink(ANode.Data);
      ANode:=FGlobalLinks.FindSuccessor(ANode);
      if CurPkgLink.LastUsed<=0 then continue;

      inc(i);
      ItemPath:=Path+'Item'+IntToStr(i)+'/';
      XMLConfig.SetDeleteValue(ItemPath+'Name/Value',CurPkgLink.Name,'');
      PkgVersionSaveToXMLConfig(CurPkgLink.Version,XMLConfig,ItemPath+'Version/');
      XMLConfig.SetDeleteValue(ItemPath+'LastUsed/Value',
                   DateToCfgStr(CurPkgLink.LastUsed,DateTimeAsCfgStrFormat),'');
    end;
    XMLConfig.SetDeleteValue(Path+'Count',i,0);

    InvalidateFileStateCache(ConfigFilename);
    XMLConfig.Flush;
    XMLConfig.Free;

    UserLinkLoadTime:=FileAgeCached(ConfigFilename);
    UserLinkLoadTimeValid:=true;
  except
    on E: Exception do begin
      DebugLn('Note: (lazarus) unable to read ',ConfigFilename,' ',E.Message);
      exit;
    end;
  end;
  Modified:=false;
end;

function TLazPackageLinks.NeedSaveUserLinks(const ConfigFilename: string): boolean;
begin
  Result:=Modified
          or (not UserLinkLoadTimeValid)
          or (not FileExistsCached(ConfigFilename))
          or (FileAgeCached(ConfigFilename)<>UserLinkLoadTime);
end;

procedure TLazPackageLinks.WriteLinkTree(LinkTree: TAvgLvlTree);
var
  ANode: TAvgLvlTreeNode;
  Link: TLazPackageLink;
begin
  if LinkTree=nil then exit;
  ANode:=LinkTree.FindLowest;
  while ANode<>nil do begin
    Link:=TLazPackageLink(ANode.Data);
    debugln('  ',Link.IDAsString);
    ANode:=LinkTree.FindSuccessor(ANode);
  end;
end;

function TLazPackageLinks.FindLinkWithPkgNameInTree(LinkTree: TAvgLvlTree;
  const PkgName: string; IgnoreFiles: TFilenameToStringTree): TLazPackageLink;
// find left most link with PkgName
var
  CurNode: TAvgLvlTreeNode;
  Link: TLazPackageLink;
begin
  Result:=nil;
  if PkgName='' then exit;
  CurNode:=FindLeftMostNode(LinkTree,PkgName);
  while CurNode<>nil do begin
    Link:=TLazPackageLink(CurNode.Data);
    if (CompareText(PkgName,Link.Name)=0)
    and ((IgnoreFiles=nil) or (not IgnoreFiles.Contains(Link.GetEffectiveFilename)))
    then begin
      if Result=nil then
        Result:=Link
      else begin
        // there are two packages fitting
        if ((Link.LastUsed>Result.LastUsed)
            or ((Abs(Link.LastUsed-Result.LastUsed)<1/86400)
                and (Link.Version.Compare(Result.Version)>0)))
        and FileExistsCached(Link.GetEffectiveFilename) then
          Result:=Link; // this one is better
      end;
    end;
    CurNode:=LinkTree.FindSuccessor(CurNode);
    if CurNode=nil then break;
    if CompareText(TLazPackageLink(CurNode.Data).Name,PkgName)<>0
    then
      break;
  end;
end;

function TLazPackageLinks.FindLinkWithDependencyInTree(LinkTree: TAvgLvlTree;
  Dependency: TPkgDependencyID; IgnoreFiles: TFilenameToStringTree): TLazPackageLink;
var
  Link: TLazPackageLink;
  CurNode: TAvgLvlTreeNode;
  {$IFDEF VerbosePkgLinkSameName}
  Node1: TAvgLvlTreeNode;
  {$ENDIF}
begin
  Result:=nil;
  if (Dependency=nil) or (not Dependency.IsMakingSense) then begin
    DebugLn(['Warning: (lazarus) TPackageLinks.FindLinkWithDependencyInTree Dependency makes no sense']);
    exit;
  end;
  {$IFDEF VerbosePkgLinkSameName}
  if CompareText(Dependency.PackageName,'tstver')=0 then
    debugln(['TPackageLinks.FindLinkWithDependencyInTree START ',Dependency.AsString(true)]);
  {$ENDIF}
  // if there are several fitting the description, use the last used
  // and highest version
  CurNode:=FindLeftMostNode(LinkTree,Dependency.PackageName);
  {$IFDEF VerbosePkgLinkSameName}
  if CompareText(Dependency.PackageName,'tstver')=0 then begin
    Node1:=CurNode.Precessor;
    if Node1<>nil then
      debugln(['TPackageLinks.FindLinkWithDependencyInTree Precessor=',TPackageLink(Node1.Data).IDAsString]);
    Node1:=CurNode.Successor;
    if Node1<>nil then
      debugln(['TPackageLinks.FindLinkWithDependencyInTree Successor=',TPackageLink(Node1.Data).IDAsString]);
  end;
  {$ENDIF}

  while CurNode<>nil do begin
    Link:=TLazPackageLink(CurNode.Data);
    {$IFDEF VerbosePkgLinkSameName}
    if CompareText(Dependency.PackageName,'tstver')=0 then
      debugln(['TPackageLinks.FindLinkWithDependencyInTree Link=',Link.IDAsString]);
    {$ENDIF}
    if Dependency.IsCompatible(Link.Version)
    and ((IgnoreFiles=nil) or (not IgnoreFiles.Contains(Link.GetEffectiveFilename)))
    then begin
      if Result=nil then
        Result:=Link
      else begin
        {$IFDEF VerbosePkgLinkSameName}
        if CompareText(Dependency.PackageName,'tstver')=0 then
          debugln(['TPackageLinks.FindLinkWithDependencyInTree Link=',Link.IDAsString,' LastUsed=',DateTimeToStr(Link.LastUsed),' Result=',Result.IDAsString,' LastUsed=',DateTimeToStr(Result.LastUsed)]);
        {$ENDIF}
        // there are two packages fitting
        if ((Link.LastUsed>Result.LastUsed)
            or ((Abs(Link.LastUsed-Result.LastUsed)<1/86400)
                and (Link.Version.Compare(Result.Version)>0)))
        and FileExistsCached(Link.GetEffectiveFilename) then
          Result:=Link; // this one is better
      end;
    end;
    CurNode:=LinkTree.FindSuccessor(CurNode);
    if CurNode=nil then break;
    if CompareText(TLazPackageLink(CurNode.Data).Name,Dependency.PackageName)<>0
    then
      break;
  end;
end;

function TLazPackageLinks.FindLinkWithPackageIDInTree(LinkTree: TAvgLvlTree;
  APackageID: TLazPackageID): TLazPackageLink;
var
  ANode: TAvgLvlTreeNode;
begin
  ANode:=LinkTree.FindKey(APackageID,@ComparePackageIDAndLink);
  if ANode<>nil then
    Result:=TLazPackageLink(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageLinks.FindLinkWithLPKFilenameInTree(LinkTree: TAvgLvlTree;
  const PkgName, LPKFilename: string): TLazPackageLink;
var
  CurNode: TAvgLvlTreeNode;
begin
  CurNode:=FindLeftMostNode(LinkTree,PkgName);
  while CurNode<>nil do begin
    Result:=TLazPackageLink(CurNode.Data);
    if CompareText(PkgName,Result.Name)<>0 then break;
    if CompareFilenames(Result.GetEffectiveFilename,LPKFilename)=0 then exit;
    CurNode:=LinkTree.FindSuccessor(CurNode);
  end;
  Result:=nil;
end;

function TLazPackageLinks.GetModified: boolean;
begin
  Result:=FSavedChangeStamp<>FChangeStamp;
end;

procedure TLazPackageLinks.IteratePackagesInTree(MustExist: boolean;
  LinkTree: TAvgLvlTree; Event: TIteratePackagesEvent);
var
  ANode: TAvgLvlTreeNode;
  PkgLink: TLazPackageLink;
  AFilename: String;
begin
  ANode:=LinkTree.FindLowest;
  while ANode<>nil do begin
    PkgLink:=TLazPackageLink(ANode.Data);
    //debugln('TPackageLinks.IteratePackagesInTree PkgLink.Filename=',PkgLink.LPKFilename);
    AFilename:=PkgLink.GetEffectiveFilename;
    if (not MustExist) or FileExistsUTF8(AFilename) then
      Event(PkgLink);
    ANode:=LinkTree.FindSuccessor(ANode);
  end;
end;

procedure TLazPackageLinks.SetModified(const AValue: boolean);
begin
  if Modified=AValue then exit;
  if not AValue then
    FSavedChangeStamp:=FChangeStamp
  else
    IncreaseChangeStamp;
end;

procedure TLazPackageLinks.SetQueueSaveUserLinks(AValue: boolean);
begin
  if FQueueSaveUserLinks=AValue then Exit;
  FQueueSaveUserLinks:=AValue;
  if Application=nil then exit;
  if FQueueSaveUserLinks then
    Application.QueueAsyncCall(@OnAsyncSaveUserLinks,0)
  else
    Application.RemoveAsyncCalls(Self);
end;

function TLazPackageLinks.FindLinkWithPkgName(const PkgName: string): TPackageLink;
begin
  Result := FindLinkWithPkgNameWithIgnore(PkgName, Nil);
end;

function TLazPackageLinks.FindLinkWithPkgNameWithIgnore(const PkgName: string;
  IgnoreFiles: TFilenameToStringTree): TPackageLink;
var
  UserLink, GlobalLink: TLazPackageLink;
begin
  UserLink:=FindLinkWithPkgNameInTree(FUserLinksSortID,PkgName,IgnoreFiles);
  GlobalLink:=FindLinkWithPkgNameInTree(FGlobalLinks,PkgName,IgnoreFiles);
  Result:=GetNewerLink(UserLink,GlobalLink);
end;

function TLazPackageLinks.FindLinkWithDependency(Dependency: TPkgDependencyID): TPackageLink;
begin
  Result := FindLinkWithDependencyWithIgnore(Dependency, Nil);
end;

function TLazPackageLinks.FindLinkWithDependencyWithIgnore(Dependency: TPkgDependencyID;
  IgnoreFiles: TFilenameToStringTree): TPackageLink;
var
  UserLink, GlobalLink: TLazPackageLink;
begin
  UserLink:=FindLinkWithDependencyInTree(FUserLinksSortID,Dependency,IgnoreFiles);
  GlobalLink:=FindLinkWithDependencyInTree(FGlobalLinks,Dependency,IgnoreFiles);
  Result:=GetNewerLink(UserLink,GlobalLink);
end;

function TLazPackageLinks.FindLinkWithPackageID(APackageID: TLazPackageID): TPackageLink;
var
  UserLink, GlobalLink: TLazPackageLink;
begin
  UserLink:=FindLinkWithPackageIDInTree(FUserLinksSortID,APackageID);
  GlobalLink:=FindLinkWithPackageIDInTree(FGlobalLinks,APackageID);
  Result:=GetNewerLink(UserLink,GlobalLink);
end;

function TLazPackageLinks.FindLinkWithFilename(const PkgName, LPKFilename: string): TPackageLink;
var
  UserLink, GlobalLink: TLazPackageLink;
begin
  UserLink:=FindLinkWithLPKFilenameInTree(FUserLinksSortID,PkgName,LPKFilename);
  GlobalLink:=FindLinkWithLPKFilenameInTree(FGlobalLinks,PkgName,LPKFilename);
  Result:=GetNewerLink(UserLink,GlobalLink);
end;

procedure TLazPackageLinks.IteratePackages(MustExist: boolean;
  Event: TIteratePackagesEvent; Origins: TPkgLinkOrigins);
begin
  if ploUser in Origins then
    IteratePackagesInTree(MustExist,FUserLinksSortID,Event);
  if ploGlobal in Origins then
    IteratePackagesInTree(MustExist,FGlobalLinks,Event);
end;

function TLazPackageLinks.AddUserLink(APackage: TIDEPackage): TPackageLink;
var
  OldLink: TPackageLink;
  NewLink: TPackageLink;
begin
  BeginUpdate;
  try
    // check if link already exists
    OldLink:=FindLinkWithPackageID(APackage);
    if (OldLink<>nil) then begin
      // link exists -> check if it is already the right value
      if (OldLink.Compare(APackage)=0)
      and (OldLink.GetEffectiveFilename=APackage.Filename) then begin
        Result:=OldLink;
        Result.LastUsed:=Now;
        IncreaseChangeStamp;
        exit;
      end;
      RemoveUserLinks(APackage);
    end;
    // add user link
    NewLink:=TLazPackageLink.Create;
    NewLink.Reference;
    NewLink.AssignID(APackage);
    NewLink.LPKFilename:=APackage.Filename;
    if NewLink.IsMakingSense then begin
      FUserLinksSortID.Add(NewLink);
      FUserLinksSortFile.Add(NewLink);
      IncreaseChangeStamp;
    end else begin
      NewLink.Release;
      NewLink:=nil;
    end;
    Result:=NewLink;
    Result.LastUsed:=Now;
  finally
    EndUpdate;
  end;
end;

function TLazPackageLinks.AddUserLink(const PkgFilename, PkgName: string): TPackageLink;
var
  OldLink: TPackageLink;
  NewLink: TPackageLink;
  LPK: TXMLConfig;
  PkgVersion: TPkgVersion;
begin
  PkgVersion:=TPkgVersion.Create;
  LPK:=nil;
  BeginUpdate;
  try
    // load version
    LPK:=LoadXMLConfigViaCodeBuffer(PkgFilename);
    if LPK<>nil then
      PkgVersionLoadFromXMLConfig(PkgVersion,LPK);

    // check if link already exists
    OldLink:=FindLinkWithFilename(PkgName,PkgFilename);
    if (OldLink<>nil) then begin
      // link exists
      Result:=OldLink;
      Result.LastUsed:=Now;
      if LPK<>nil then
        Result.Version.Assign(PkgVersion);
      exit;
    end;

    // add user link
    NewLink:=TLazPackageLink.Create;
    NewLink.Reference;
    NewLink.Name:=PkgName;
    NewLink.LPKFilename:=PkgFilename;
    if LPK<>nil then
      NewLink.Version.Assign(PkgVersion);
    if NewLink.IsMakingSense then begin
      FUserLinksSortID.Add(NewLink);
      FUserLinksSortFile.Add(NewLink);
      IncreaseChangeStamp;
    end else begin
      NewLink.Release;
      NewLink:=nil;
    end;
    Result:=NewLink;
    if Result<>nil then
      Result.LastUsed:=Now;
  finally
    EndUpdate;
    PkgVersion.Free;
    LPK.Free;
  end;
end;

procedure TLazPackageLinks.RemoveUserLink(Link: TPackageLink);
var
  ANode: TAvgLvlTreeNode;
begin
  BeginUpdate;
  try
    // remove from user links
    ANode:=FUserLinksSortFile.FindPointer(Link);
    if ANode<>nil then begin
      FUserLinksSortID.RemovePointer(Link);
      FUserLinksSortFile.RemovePointer(Link);
      Link.Release;
      IncreaseChangeStamp;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLazPackageLinks.RemoveUserLinks(APackageID: TLazPackageID);
var
  ANode: TAvgLvlTreeNode;
  OldLink: TLazPackageLink;
begin
  BeginUpdate;
  try
    // remove from user links
    repeat
      ANode:=FUserLinksSortID.FindKey(APackageID,@ComparePackageIDAndLink);
      if ANode=nil then exit;
      OldLink:=TLazPackageLink(ANode.Data);
      FUserLinksSortID.Delete(ANode);
      FUserLinksSortFile.RemovePointer(OldLink);
      OldLink.Release;
      IncreaseChangeStamp;
    until false;
  finally
    EndUpdate;
  end;
end;

procedure TLazPackageLinks.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

initialization
  LazPackageLinks:=nil;

end.

