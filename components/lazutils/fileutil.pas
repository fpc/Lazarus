{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ ****************************************************************************
BB: 2013-05-19

Note to developers:

This unit should contain functions and procedures to
maintain compatibility with Delphi's FileUtil unit.

File routines that specifically deal with UTF8 filenames should go into
the LazFileUtils unit.

***************************************************************************** }
unit FileUtil;

{$mode objfpc}{$H+}
{$i lazutils_defines.inc}

interface

uses
  Classes, SysUtils, StrUtils,
  // LazUtils
  Masks, LazUTF8, LazFileUtils;

{$IF defined(Windows) or defined(darwin) or defined(HASAMIGA)}
{$define CaseInsensitiveFilenames}
{$ENDIF}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$define NotLiteralFilenames}
{$ENDIF}

const
  UTF8FileHeader = #$ef#$bb#$bf;
  FilenamesCaseSensitive = {$IFDEF CaseInsensitiveFilenames}false{$ELSE}true{$ENDIF};// lower and upper letters are treated the same
  FilenamesLiteral = {$IFDEF NotLiteralFilenames}false{$ELSE}true{$ENDIF};// file names can be compared using = string operator

// basic functions similar to the RTL but working with UTF-8 instead of the
// system encoding

// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
// but normally these OS use UTF-8 as system encoding so the widestringmanager
// is not needed.

// file and directory operations
function ComparePhysicalFilenames(const Filename1, Filename2: string): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer; overload;
function ExtractShortPathNameUTF8(Const FileName : String) : String;
function DeleteDirectory(const DirectoryName: string; OnlyChildren: boolean): boolean;
function ProgramDirectory: string;
function ProgramDirectoryWithBundle: string;

function ExpandUNCFileNameUTF8(const FileName: string): string;
function FileSize(const Filename: string): int64; overload; inline;
function FilenameIsPascalUnit(const Filename: string): boolean;
function FileIsInPath(const Filename, Path: string): boolean;
function FileIsInDirectory(const Filename, Directory: string): boolean;

function ExtractFileNameWithoutExt(const AFilename: string): string; deprecated 'Use the function from unit LazFileUtils';
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string; deprecated 'Use the function from unit LazFileUtils';
function CreateAbsolutePath(const Filename, BaseDirectory: string): string; deprecated 'Use the function from unit LazFileUtils';

function GetAllFilesMask: string; inline;
function GetExeExt: string; inline;
function ReadFileToString(const Filename: string): string;

// file search
type
  TSearchFileInPathFlag = (
    sffDontSearchInBasePath, // do not search in BasePath, search only in SearchPath.
    sffSearchLoUpCase,
    sffFile, // must be file, not directory
    sffExecutable // file must be executable
    );
  TSearchFileInPathFlags = set of TSearchFileInPathFlag;
const
  sffFindProgramInPath = [{$IFDEF Unix}sffDontSearchInBasePath,{$ENDIF}sffFile,sffExecutable];

function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string; overload;
function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
function FindDiskFilename(const Filename: string): string;
function FindDiskFileCaseInsensitive(const Filename: string): string;
function FindDefaultExecutablePath(const Executable: string; const BaseDir: string = ''): string;

type

  { TFileIterator }

  TFileIterator = class
  private
    FPath: String;
    FLevel: Integer;
    FFileInfo: TSearchRec;
    FSearching: Boolean;
    function GetFileName: String;
  public
    procedure Stop;
    function IsDirectory: Boolean;
  public
    property FileName: String read GetFileName;
    property FileInfo: TSearchRec read FFileInfo;
    property Level: Integer read FLevel;
    property Path: String read FPath;
    property Searching: Boolean read FSearching;
  end;

  TFileFoundEvent = procedure (FileIterator: TFileIterator) of object;
  TDirectoryFoundEvent = procedure (FileIterator: TFileIterator) of object;
  TDirectoryEnterEvent = procedure (FileIterator: TFileIterator) of object;

  { TFileSearcher }

  TFileSearcher = class(TFileIterator)
  private
    FMaskSeparator: char;
    FPathSeparator: char;
    FFollowSymLink: Boolean;
    FOnFileFound: TFileFoundEvent;
    FOnDirectoryFound: TDirectoryFoundEvent;
    FOnDirectoryEnter: TDirectoryEnterEvent;
    FFileAttribute: Word;
    FDirectoryAttribute: Word;
    procedure RaiseSearchingError;
  protected
    procedure DoDirectoryEnter; virtual;
    procedure DoDirectoryFound; virtual;
    procedure DoFileFound; virtual;
  public
    constructor Create;
    procedure Search(ASearchPath: String; ASearchMask: String = '';
      ASearchSubDirs: Boolean = True; CaseSensitive: Boolean = False);
  public
    property MaskSeparator: char read FMaskSeparator write FMaskSeparator;
    property PathSeparator: char read FPathSeparator write FPathSeparator;
    property FollowSymLink: Boolean read FFollowSymLink write FFollowSymLink;
    property FileAttribute: Word read FFileAttribute write FFileAttribute default faAnyfile;
    property DirectoryAttribute: Word read FDirectoryAttribute write FDirectoryAttribute default faDirectory;
    property OnDirectoryFound: TDirectoryFoundEvent read FOnDirectoryFound write FOnDirectoryFound;
    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
    property OnDirectoryEnter: TDirectoryEnterEvent read FOnDirectoryEnter write FOnDirectoryEnter;
  end;

  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  private
    FList: TStrings;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(AList: TStrings);
  end;

  { TListDirectoriesSearcher }

  TListDirectoriesSearcher = class(TFileSearcher)
  private
    FDirectoriesList :TStrings;
  protected
    procedure DoDirectoryFound; override;
  public
    constructor Create(AList: TStrings);
  end;

function FindAllFiles(const SearchPath: String; SearchMask: String = '';
  SearchSubDirs: Boolean = True; DirAttr: Word = faDirectory;
  MaskSeparator: char = ';'; PathSeparator: char = ';'): TStringList; overload;
procedure FindAllFiles(AList: TStrings; const SearchPath: String;
  SearchMask: String = ''; SearchSubDirs: Boolean = True; DirAttr: Word = faDirectory;
  MaskSeparator: char = ';'; PathSeparator: char = ';'); overload;

function FindAllDirectories(const SearchPath: string;
  SearchSubDirs: Boolean = True; PathSeparator: char = ';'): TStringList; overload;
procedure FindAllDirectories(AList: TStrings; const SearchPath: String;
  SearchSubDirs: Boolean = true; PathSeparator: char = ';'); overload;

// flags for copy
type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;

// Copy a file and a whole directory tree
function CopyFile(const SrcFilename, DestFilename: string;
                  Flags: TCopyFileFlags=[cffOverwriteFile]; ExceptionOnError: Boolean=False): boolean;
function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean; ExceptionOnError: Boolean=False): boolean;
function CopyDirTree(const SourceDir, TargetDir: string; Flags: TCopyFileFlags=[]): Boolean;

// filename parts
const
  PascalFileExt: array[1..3] of string = ('.pas','.pp','.p');
  PascalSourceExt: array[1..6] of string = ('.pas','.pp','.p','.lpr','.dpr','.dpk');

  AllDirectoryEntriesMask = '*';

implementation

uses
{$IFDEF windows}
  Windows;
{$ELSE}
  {$IFDEF HASAMIGA}
  AmigaDOS;
  {$ELSE}
  Unix;
  {$ENDIF}
{$ENDIF}

{$I fileutil.inc}
{$IFDEF windows}
  {$i winfileutil.inc}
{$ELSE}
  {$IFDEF HASAMIGA}
  {$i unixfileutil.inc}   // Reuse UNIX code for Amiga
  {$ELSE}
  {$i unixfileutil.inc}
  {$ENDIF}
{$ENDIF}

end.

