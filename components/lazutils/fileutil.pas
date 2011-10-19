{  $Id: fileutil.pas 32656 2011-10-03 15:09:04Z sekelsenmat $  }
{
 /***************************************************************************
                               fileutil.pas
                               -----------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit FileUtil;

{$mode objfpc}{$H+}

interface

uses
  // For Smart Linking: Do not use the LCL!
  SysConst, Classes, SysUtils, Masks, LazUtilsStrConsts;
  
{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

const
  UTF8FileHeader = #$ef#$bb#$bf;
  FilenamesCaseSensitive = {$IFDEF CaseInsensitiveFilenames}false{$ELSE}true{$ENDIF};// lower and upper letters are treated the same
  FilenamesLiteral = {$IFDEF NotLiteralFilenames}false{$ELSE}true{$ENDIF};// file names can be compared using = string operator

// file attributes and states
function CompareFilenames(const Filename1, Filename2: string): integer;
function CompareFilenamesIgnoreCase(const Filename1, Filename2: string): integer;
function CompareFilenames(const Filename1, Filename2: string;
                          ResolveLinks: boolean): integer;
function CompareFilenames(Filename1: PChar; Len1: integer;
  Filename2: PChar; Len2: integer; ResolveLinks: boolean): integer;
function FilenameIsAbsolute(const TheFilename: string):boolean;
function FilenameIsWinAbsolute(const TheFilename: string):boolean;
function FilenameIsUnixAbsolute(const TheFilename: string):boolean;
procedure CheckIfFileIsExecutable(const AFilename: string);
procedure CheckIfFileIsSymlink(const AFilename: string);
function FileIsReadable(const AFilename: string): boolean;
function FileIsWritable(const AFilename: string): boolean;
function FileIsText(const AFilename: string): boolean;
function FileIsText(const AFilename: string; out FileReadable: boolean): boolean;
function FileIsExecutable(const AFilename: string): boolean;
function FileIsSymlink(const AFilename: string): boolean;
function FileSize(const Filename: string): int64;
function GetFileDescription(const AFilename: string): string;
function ReadAllLinks(const Filename: string;
                      ExceptionOnError: boolean): string; // if a link is broken returns ''
function TryReadAllLinks(const Filename: string): string; // if a link is broken returns Filename

// directories
function DirPathExists(const FileName: String): Boolean;
function ForceDirectory(DirectoryName: string): boolean;
function DeleteDirectory(const DirectoryName: string;
  OnlyChilds: boolean): boolean;
function ProgramDirectory: string;
function DirectoryIsWritable(const DirectoryName: string): boolean;

// filename parts
const
  PascalFileExt: array[1..3] of string = ('.pas','.pp','.p');

function ExtractFileNameOnly(const AFilename: string): string;
function ExtractFileNameWithoutExt(const AFilename: string): string;
function CompareFileExt(const Filename, Ext: string;
                        CaseSensitive: boolean): integer;
function CompareFileExt(const Filename, Ext: string): integer;
function FilenameIsPascalUnit(const Filename: string): boolean;
function AppendPathDelim(const Path: string): string;
function ChompPathDelim(const Path: string): string;
function TrimFilename(const AFilename: string): string;
function CleanAndExpandFilename(const Filename: string): string;
function CleanAndExpandDirectory(const Filename: string): string;
function CreateAbsoluteSearchPath(const SearchPath, BaseDirectory: string): string;
function CreateRelativePath(const Filename, BaseDirectory: string;
                            UsePointDirectory: boolean = false): string;
function CreateAbsolutePath(const Filename, BaseDirectory: string): string;
function FileIsInPath(const Filename, Path: string): boolean;
function FileIsInDirectory(const Filename, Directory: string): boolean;

// file search
type
  TSearchFileInPathFlag = (
    sffDontSearchInBasePath,
    sffSearchLoUpCase
    );
  TSearchFileInPathFlags = set of TSearchFileInPathFlag;

const
  AllDirectoryEntriesMask = '*';
  
function GetAllFilesMask: string;
function GetExeExt: string;
function SearchFileInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): string;
function SearchAllFilesInPath(const Filename, BasePath, SearchPath,
  Delimiter: string; Flags: TSearchFileInPathFlags): TStrings;
function FindDiskFilename(const Filename: string): string;
function FindDiskFileCaseInsensitive(const Filename: string): string;
function FindDefaultExecutablePath(const Executable: string): string;
{$IFDEF darwin}
function GetDarwinSystemFilename(Filename: string): string;
{$ENDIF}

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

  { TFileSearcher }

  TFileSearcher = class(TFileIterator)
  private
    FOnFileFound: TFileFoundEvent;
    FOnDirectoryFound: TDirectoryFoundEvent;

    procedure RaiseSearchingError;
  protected
    procedure DoDirectoryEnter; virtual;
    procedure DoDirectoryFound; virtual;
    procedure DoFileFound; virtual;
  public
    constructor Create;

    procedure Search(const ASearchPath: String; ASearchMask: String = '';
      ASearchSubDirs: Boolean = True; AMaskSeparator: char = ';');
  public
    property OnDirectoryFound: TDirectoryFoundEvent read FOnDirectoryFound write FOnDirectoryFound;
    property OnFileFound: TFileFoundEvent read FOnFileFound write FOnFileFound;
  end;

function FindAllFiles(const SearchPath: String; SearchMask: String = '';
  SearchSubDirs: Boolean = True): TStringList;
function FindAllDirectories(const SearchPath: string;
  SearchSubDirs: Boolean = True): TStringList;

// file actions
function ReadFileToString(const Filename: string): string;
function CopyFile(const SrcFilename, DestFilename: string): boolean;
function CopyFile(const SrcFilename, DestFilename: string; PreserveTime: boolean): boolean;
function GetTempFilename(const Directory, Prefix: string): string;

// basic functions similar to the RTL but working with UTF-8 instead of the
// system encoding

// AnsiToUTF8 and UTF8ToAnsi need a widestring manager under Linux, BSD, MacOSX
// but normally these OS use UTF-8 as system encoding so the widestringmanager
// is not needed.
function NeedRTLAnsi: boolean;// true if system encoding is not UTF-8
procedure SetNeedRTLAnsi(NewValue: boolean);
function UTF8ToSys(const s: string): string;// as UTF8ToAnsi but more independent of widestringmanager
function SysToUTF8(const s: string): string;// as AnsiToUTF8 but more independent of widestringmanager
function ConsoleToUTF8(const s: string): string;// converts OEM encoded string to UTF8 (used with some Windows specific functions)
function UTF8ToConsole(const s: string): string;// converts UTF8 string to console encoding (used by Write, WriteLn)

// file operations
function FileExistsUTF8(const Filename: string): boolean;
function FileAgeUTF8(const FileName: string): Longint;
function DirectoryExistsUTF8(const Directory: string): Boolean;
function ExpandFileNameUTF8(const FileName: string): string;
function ExpandUNCFileNameUTF8(const FileName: string): string;
function ExtractShortPathNameUTF8(Const FileName : String) : String;
function FindFirstUTF8(const Path: string; Attr: Longint; out Rslt: TSearchRec): Longint;
function FindNextUTF8(var Rslt: TSearchRec): Longint;
procedure FindCloseUTF8(var F: TSearchrec);
function FileSetDateUTF8(const FileName: String; Age: Longint): Longint;
function FileGetAttrUTF8(const FileName: String): Longint;
function FileSetAttrUTF8(const Filename: String; Attr: longint): Longint;
function DeleteFileUTF8(const FileName: String): Boolean;
function RenameFileUTF8(const OldName, NewName: String): Boolean;
function FileSearchUTF8(const Name, DirList : String; ImplicitCurrentDir : Boolean = True): String;
function FileIsReadOnlyUTF8(const FileName: String): Boolean;
function GetCurrentDirUTF8: String;
function SetCurrentDirUTF8(const NewDir: String): Boolean;
function CreateDirUTF8(const NewDir: String): Boolean;
function RemoveDirUTF8(const Dir: String): Boolean;
function ForceDirectoriesUTF8(const Dir: string): Boolean;
function FileOpenUTF8(Const FileName : utf8string; Mode : Integer) : THandle;
function FileCreateUTF8(Const FileName : utf8String) : THandle; overload;
function FileCreateUTF8(Const FileName : utf8String; Rights: Cardinal) : THandle; overload;

// environment
function ParamStrUTF8(Param: Integer): string;
function GetEnvironmentStringUTF8(Index: Integer): string;
function GetEnvironmentVariableUTF8(const EnvVar: string): String;
function GetAppConfigDirUTF8(Global: Boolean): string;
function GetAppConfigFileUTF8(Global: Boolean; SubDir: boolean = false): string;

// other
function SysErrorMessageUTF8(ErrorCode: Integer): String;

implementation

uses
{$IFDEF windows}
  Windows;
{$ELSE}
  {$IFDEF darwin}
  MacOSAll,
  {$ENDIF}
  Unix, BaseUnix;
{$ENDIF}

{$I fileutil.inc}
{$IFDEF windows}
  {$i winfileutil.inc}
{$ELSE}
  {$i unixfileutil.inc}
{$ENDIF}

initialization
  InitFileUtils;
end.

