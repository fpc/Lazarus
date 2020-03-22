unit PackageLinkIntf;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  // LazUtils
  LazFileUtils, UITypes,
  // BuildIntf
  PackageDependencyIntf, PackageIntf;

type

  { TPackageLink
    There are several types of package links.

    Global: These are collected from the lazarus source directory.
            EnvironmentOptions.LazarusDirectory+'packager/globallinks/*.lpl'
            This way packages can install/uninstall themselves to one lazarus
            source directory, and this lazarus directory can then be shared
            by several users/configs.

    Online: Got through Online Package Manager

    User:   These are collected from the user config directory, from the file
            packagelinks.xml.
            These links are maintained by the IDE. Everytime the user opens a
            package a user link is created, so that the next time the package
            can be automatically opened. The list is checked by the IDE from
            time to time and missing packages are first marked and after several
            months deleted from the list.
            Relative files are expanded with the Lazarus directory.
  }

  TPkgLinkOrigin = (
    ploGlobal,
    ploOnline,
    ploUser
    );
  TPkgLinkOrigins = set of TPkgLinkOrigin;

const
  AllPkgLinkOrigins = [low(TPkgLinkOrigin)..high(TPkgLinkOrigin)];

type

  { TPackageLink }

  TPackageLink = class(TLazPackageID)
  private
    procedure SetFilename(const AValue: string);
  protected
    FFileDate: TDateTime;
    FFileDateValid: boolean;
    FFilename: string;
    FURL: String;
    FLPLFileDate: TDateTime;
    FLPLFilename: string;
    FPackageType: TLazPackageType;
    FOrigin: TPkgLinkOrigin;
    FLastUsed: TDateTime;
    FOPMFileName: string;
    FOPMFileDate: TDateTime;
    FAuthor: string;
    FDescription: string;
    FLicense: string;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsMakingSense: boolean;
    function GetEffectiveFilename: string; virtual; abstract;
    procedure Reference; virtual; abstract;
    procedure Release; virtual; abstract;
  public
    property LPKFileDateValid: boolean read FFileDateValid write FFileDateValid;
    property LPKFileDate: TDateTime read FFileDate write FFileDate;
    // if relative it is relative to the LazarusDir
    property LPKFilename: string read FFilename write SetFilename;
    property LPKUrl: string read FURL write FURL;
    property LPLFilename: string read FLPLFilename write FLPLFilename;
    property LPLFileDate: TDateTime read FLPLFileDate write FLPLFileDate;
    property PackageType: TLazPackageType read FPackageType write FPackageType;
    property Origin: TPkgLinkOrigin read FOrigin write FOrigin;
    property LastUsed: TDateTime read FLastUsed write FLastUsed;
    property OPMFileName: string read FOPMFileName write FOPMFileName;
    property OPMFileDate: TDateTime read FOPMFileDate write FOPMFileDate;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
    property License: string read FLicense write FLicense;
  end;

  { TPackageLinks }

  TPackageLinks = class
  private
  public
    function FindLinkWithPkgName(const PkgName: string): TPackageLink; virtual; abstract;
    function FindLinkWithDependency(Dependency: TPkgDependencyID): TPackageLink; virtual; abstract;
    function FindLinkWithPackageID(APackageID: TLazPackageID): TPackageLink; virtual; abstract;
    function FindLinkWithFilename(const PkgName, LPKFilename: string): TPackageLink; virtual; abstract;
    procedure IteratePackages(MustExist: boolean; Event: TIteratePackagesEvent;
      Origins: TPkgLinkOrigins = AllPkgLinkOrigins); virtual; abstract;
    function AddOnlineLink(const PkgFilename, PkgName, PkgURL: string): TPackageLink; virtual; abstract;
    function AddUserLink(APackage: TIDEPackage): TPackageLink; virtual; abstract;
    // do not use this if package is open in IDE
    function AddUserLink(const PkgFilename, PkgName: string): TPackageLink; virtual; abstract;
    procedure RemoveUserLink(Link: TPackageLink); virtual; abstract;
    procedure RemoveUserLinks(APackageID: TLazPackageID); virtual; abstract;
    procedure ClearOnlineLinks; virtual; abstract;
    procedure SaveUserLinks(Immediately: boolean = false); virtual; abstract;
  end;

  { TOPMInterface }

  TOPMInterface = class
  private
    FPackageListAvailable: TNotifyEvent;
  public
    {confirmation/install/extract/download dialogs will be displayed in the center of WorkArea}
    function DownloadPackages(APkgLinks: TList): TModalResult; virtual; abstract;
    function InstallPackages(APkgLinks: TList; var ANeedToRebuild: Boolean): TModalResult; virtual; abstract;
    function IsPackageAvailable(APkgLink: TPackageLink; AType: Integer): Boolean; virtual; abstract;
    function FindOnlineLink(const AName: String): TPackageLink; virtual; abstract;
    property OnPackageListAvailable: TNotifyEvent read FPackageListAvailable write FPackageListAvailable;
  end;

var
  PkgLinks: TPackageLinks;
  OPMInterface: TOPMInterface;

implementation

{ TPackageLink }

constructor TPackageLink.Create;
begin
  inherited Create;
end;

destructor TPackageLink.Destroy;
begin
  inherited Destroy;
end;

procedure TPackageLink.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=TrimFilename(AValue);
end;

function TPackageLink.IsMakingSense: boolean;
begin
  Result:=IsValidPkgName(Name)
           and PackageFileNameIsValid(LPKFilename)
           and (CompareText(Name,ExtractFileNameOnly(LPKFilename))=0);
end;

end.

