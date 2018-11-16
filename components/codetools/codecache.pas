{
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
    TCodeCache is an AVL Tree of TCodeBuffer. It can load and save files.
    
    TCodeBuffer is a descendent of TSourceLog and manages a single file.
}
unit CodeCache;

{$ifdef fpc}{$mode objfpc}{$endif}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Laz_Avl_Tree,
  // Codetools
  SourceLog, LinkScanner, FileProcs, DirectoryCacher,
  // LazUtils
  LazFileUtils, LazFileCache, Laz2_XMLCfg, LazDbgLog;

const
  IncludeLinksFileVersion = 2;
type
  TCodeCache = class;
  
  { TCodeBuffer }

  TCodeBuffer = class(TSourceLog)
  private
    FFilename: string;
    FReferenceCount: integer;
    FScanner: TLinkScanner;
    FOnSetScanner: TNotifyEvent;
    FOnSetFilename: TNotifyEvent;
    FFileChangeStep: integer;
    FLoadDateValid: boolean;
    FLoadDate: longint;
    FLastIncludedByFile: string;
    FCodeCache: TCodeCache;
    FIsVirtual: boolean;
    FIsDeleted: boolean;
    FAutoDiskRevertLock: integer;
    FGlobalWriteLockStepOnLastLoad: integer;
    function GetLastIncludedByFile: string;
    procedure SetFilename(Value: string);
    procedure SetScanner(const Value: TLinkScanner);
    procedure SetIsDeleted(const NewValue: boolean);
  protected
    procedure DoSourceChanged; override;
    procedure DecodeLoaded(const AFilename: string;
                    var ASource, ADiskEncoding, AMemEncoding: string); override;
    procedure EncodeSaving(const AFilename: string; var ASource: string); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ConsistencyCheck;
    procedure WriteDebugReport;
    function CalcMemSize: PtrUInt; override;
    function LoadFromFile(const AFilename: string): boolean; override;
    function Reload: boolean; // = LoadFromFile(Filename)
    function Revert: boolean; // ignore changes and reload source
    function SaveToFile(const AFilename: string): boolean; override;
    function Save: boolean;
    function FileDateOnDisk: longint;
    function FileNeedsUpdate(IgnoreModifiedFlag: Boolean = False): boolean; // needs loading
    function FileOnDiskNeedsUpdate: boolean;
    function FileOnDiskHasChanged(IgnoreModifiedFlag: Boolean = False): boolean;
    function FileOnDiskIsEqual: boolean;
    function AutoRevertFromDisk: boolean;
    procedure LockAutoDiskRevert;
    procedure UnlockAutoDiskRevert;
    procedure IncrementRefCount;
    procedure ReleaseRefCount;
    procedure MakeFileDateValid;
    procedure InvalidateLoadDate;
    function SourceIsText: boolean;
  public
    property CodeCache: TCodeCache read FCodeCache write FCodeCache;
    property Filename: string read FFilename write SetFilename;
    property GlobalWriteLockStepOnLastLoad: integer
       read FGlobalWriteLockStepOnLastLoad write FGlobalWriteLockStepOnLastLoad;
    property IsDeleted: boolean read FIsDeleted write SetIsDeleted;
    property IsVirtual: boolean read FIsVirtual;
    property LastIncludedByFile: string read GetLastIncludedByFile
                                        write FLastIncludedByFile;
    property LoadDate: longint read FLoadDate;
    property LoadDateValid: boolean read FLoadDateValid;
    property FileChangeStep: integer read FFileChangeStep; // last loaded/saved changestep, only valid if LoadDateValid=true
    property OnSetFilename: TNotifyEvent read FOnSetFilename write FOnSetFilename;
    property OnSetScanner: TNotifyEvent read FOnSetScanner write FOnSetScanner;
    property Scanner: TLinkScanner read FScanner write SetScanner;
    property ReferenceCount: integer read FReferenceCount;
  end;
  
  { TIncludedByLink }

  TIncludedByLink = class
  public
    IncludeFilename: string;
    IncludedByFile: string;
    LastTimeUsed: TDateTime;
    constructor Create(const AnIncludeFilename,AnIncludedByFile: string;
       ALastTimeUsed: TDateTime);
    function CalcMemSize: PtrUInt;
  end;

  TOnCodeCacheDecodeLoaded = procedure(Code: TCodeBuffer; const Filename: string;
                        var Source, DiskEncoding, MemEncoding: string) of object;
  TOnCodeCacheEncodeSaving = procedure(Code: TCodeBuffer;
                          const Filename: string; var Source: string) of object;

  { TCodeCache }

  TCodeCache = class(TObject)
  private
    FChangeStamp: int64;
    FDefaultEncoding: string;
    FDirectoryCachePool: TCTDirectoryCachePool;
    FItems: TAVLTree;  // tree of TCodeBuffer
    FIncludeLinks: TAVLTree; // tree of TIncludedByLink
    FDestroying: boolean;
    FExpirationTimeInDays: integer;
    FGlobalWriteLockIsSet: boolean;
    FGlobalWriteLockStep: integer;
    fLastIncludeLinkFile: string;
    fLastIncludeLinkFileAge: integer;
    fLastIncludeLinkFileValid: boolean;
    fLastIncludeLinkFileChangeStep: integer;
    fChangeStep: integer;
    FOnDecodeLoaded: TOnCodeCacheDecodeLoaded;
    FOnEncodeSaving: TOnCodeCacheEncodeSaving;
    function FindIncludeLink(const IncludeFilename: string): string;
    function FindIncludeLinkNode(const IncludeFilename: string): TIncludedByLink;
    function FindIncludeLinkAVLNode(const IncludeFilename: string): TAVLTreeNode;
    function OnScannerCheckFileOnDisk(Code: pointer): boolean; // true if code changed
    function OnScannerGetFileName(Sender: TObject; Code: pointer): string;
    function OnScannerGetSource(Sender: TObject; Code: pointer): TSourceLog;
    function OnScannerLoadSource(Sender: TObject; const AFilename: string;
                                 OnlyIfExists: boolean): pointer;
    procedure OnScannerDeleteSource(Sender: TObject; Code: Pointer;
                 Pos, Len: integer);
    procedure OnScannerGetSourceStatus(Sender: TObject; Code:Pointer;
                 var ReadOnly: boolean);
    procedure OnScannerIncludeCode(ParentCode, IncludeCode: pointer);
    procedure UpdateIncludeLinks;
    procedure IncreaseChangeStep;
    procedure DecodeLoaded(Code: TCodeBuffer; const AFilename: string;
                           var ASource, ADiskEncoding, AMemEncoding: string);
    procedure EncodeSaving(Code: TCodeBuffer;
                           const AFilename: string; var ASource: string);
  public
    constructor Create;
    destructor Destroy;  override;
    procedure ConsistencyCheck;
    function Count: integer;
    function CreateFile(const AFilename: string): TCodeBuffer;
    function FindFile(AFilename: string): TCodeBuffer;
    function LastIncludedByFile(const IncludeFilename: string): string;
    function LoadFile(AFilename: string): TCodeBuffer;
    procedure RemoveCodeBuffer(Buffer: TCodeBuffer);
    procedure LoadIncludeLinksDataFromList(List: TStrings);
    function LoadIncludeLinksFromFile(const AFilename: string): boolean;
    function LoadIncludeLinksFromXML(XMLConfig: TXMLConfig;
                                     const XMLPath: string): boolean;
    function SaveBufferAs(OldBuffer: TCodeBuffer; const AFilename: string;
                          out NewBuffer: TCodeBuffer): boolean;
    procedure SaveIncludeLinksDataToList(List: TStrings);
    function SaveIncludeLinksToFile(const AFilename: string;
                                    OnlyIfChanged: boolean): boolean;
    function SaveIncludeLinksToXML(XMLConfig: TXMLConfig;
                                   const XMLPath: string): boolean;
    procedure Clear;
    procedure ClearAllSourceLogEntries;
    procedure ClearIncludedByEntry(const IncludeFilename: string);
    procedure ClearAllModified;
    procedure OnBufferSetFileName(Sender: TCodeBuffer;
          const OldFilename: string);
    procedure OnBufferSetScanner(Sender: TCodeBuffer);
    procedure WriteAllFileNames;
    procedure WriteDebugReport;
    function CalcMemSize(Stats: TCTMemStats): PtrUInt;
    procedure IncreaseChangeStamp; inline;
  public
    property ExpirationTimeInDays: integer
          read FExpirationTimeInDays write FExpirationTimeInDays;
    property GlobalWriteLockIsSet: boolean
          read FGlobalWriteLockIsSet write FGlobalWriteLockIsSet;
    property GlobalWriteLockStep: integer
          read FGlobalWriteLockStep write FGlobalWriteLockStep;
    property OnDecodeLoaded: TOnCodeCacheDecodeLoaded read FOnDecodeLoaded
                                                      write FOnDecodeLoaded;
    property OnEncodeSaving: TOnCodeCacheEncodeSaving read FOnEncodeSaving
                                                      write FOnEncodeSaving;
    property DefaultEncoding: string read FDefaultEncoding write FDefaultEncoding;
    property ChangeStamp: int64 read FChangeStamp;
    property DirectoryCachePool: TCTDirectoryCachePool read FDirectoryCachePool
                                                      write FDirectoryCachePool;
  end;

type
  TCodePosition = packed record
    Code: TCodeBuffer;
    P: integer;
  end;
  PCodePosition = ^TCodePosition;

  TCodeXYPosition = packed record
    Code: TCodeBuffer;
    X, Y: integer;
  end;
  PCodeXYPosition = ^TCodeXYPosition;
const
  CleanCodeXYPosition: TCodeXYPosition = (Code:nil; X:0; Y:0);

type
  { TCodeXYPositions - a list of PCodeXYPosition }

  TCodeXYPositions = class
  private
    FItems: TFPList; // list of PCodeXYPosition, can be nil
    function GetCaretsXY(Index: integer): TPoint;
    function GetCodes(Index: integer): TCodeBuffer;
    function GetItems(Index: integer): PCodeXYPosition;
    procedure SetCaretsXY(Index: integer; const AValue: TPoint);
    procedure SetCodes(Index: integer; const AValue: TCodeBuffer);
    procedure SetItems(Index: integer; const AValue: PCodeXYPosition);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Position: TCodeXYPosition): integer;
    function Add(X,Y: integer; Code: TCodeBuffer): integer;
    procedure Assign(Source: TCodeXYPositions);
    function IsEqual(Source: TCodeXYPositions): boolean;
    function Count: integer;
    procedure Delete(Index: integer);
    function CreateCopy: TCodeXYPositions;
    function CalcMemSize: PtrUint;
  public
    property Items[Index: integer]: PCodeXYPosition
                                          read GetItems write SetItems; default;
    property CaretsXY[Index: integer]: TPoint read GetCaretsXY write SetCaretsXY;
    property Codes[Index: integer]: TCodeBuffer read GetCodes write SetCodes;
  end;


function CompareCodeBuffers(NodeData1, NodeData2: pointer): integer;
function CompareAnsistringWithCodeBuffer(AString, ABuffer: pointer): integer;
function CompareIncludedByLink(NodeData1, NodeData2: pointer): integer;
function CompareAnsiStringWithIncludedByLink(Key, Data: pointer): integer;

function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;
function CodeXYPosition(X, Y: integer; Code: TCodeBuffer): TCodeXYPosition;
function CompareCodeXYPositions(Pos1, Pos2: PCodeXYPosition): integer;

function CompareCodePositions(Pos1, Pos2: PCodePosition): integer;

procedure AddCodePosition(var ListOfPCodeXYPosition: TFPList;
                          const NewCodePos: TCodeXYPosition);
function IndexOfCodePosition(var ListOfPCodeXYPosition: TFPList;
                             const APosition: PCodeXYPosition): integer;
procedure FreeListOfPCodeXYPosition(ListOfPCodeXYPosition: TFPList);

function CreateTreeOfPCodeXYPosition: TAVLTree;
procedure AddCodePosition(var TreeOfPCodeXYPosition: TAVLTree;
                          const NewCodePos: TCodeXYPosition);
procedure FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition: TAVLTree);
procedure AddListToTreeOfPCodeXYPosition(SrcList: TFPList;
                          DestTree: TAVLTree; ClearList, CreateCopies: boolean);
function ListOfPCodeXYPositionToStr(const ListOfPCodeXYPosition: TFPList): string;

function Dbgs(const p: TCodeXYPosition): string; overload;
function Dbgs(const p: TCodePosition): string; overload;

implementation


function CompareCodeBuffers(NodeData1, NodeData2: pointer): integer;
var
  CodeBuf1: TCodeBuffer absolute NodeData1;
  CodeBuf2: TCodeBuffer absolute NodeData2;
begin
  Result:=CompareFilenames(CodeBuf1.Filename,CodeBuf2.Filename);
end;

function CompareAnsistringWithCodeBuffer(AString, ABuffer: pointer): integer;
var
  Code: TCodeBuffer absolute ABuffer;
  Filename: String;
begin
  Filename:=AnsiString(AString);
  Result:=CompareFilenames(Filename,Code.Filename);
end;

function CompareIncludedByLink(NodeData1, NodeData2: pointer): integer;
var
  Link1: TIncludedByLink absolute NodeData1;
  Link2: TIncludedByLink absolute NodeData2;
begin
  Result:=CompareFilenames(Link1.IncludeFilename,Link2.IncludeFilename);
end;

function CompareAnsiStringWithIncludedByLink(Key, Data: pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Key),
                           TIncludedByLink(Data).IncludeFilename);
end;

function CodePosition(P: integer; Code: TCodeBuffer): TCodePosition;
begin
  Result.P:=P;
  Result.Code:=Code;
end;

function CodeXYPosition(X, Y: integer; Code: TCodeBuffer): TCodeXYPosition;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Code:=Code;
end;

function CompareCodeXYPositions(Pos1, Pos2: PCodeXYPosition): integer;
begin
  if Pointer(Pos1^.Code)>Pointer(Pos2^.Code) then Result:=1
  else if Pointer(Pos1^.Code)<Pointer(Pos2^.Code) then Result:=-1
  else if Pos1^.Y<Pos2^.Y then Result:=1
  else if Pos1^.Y>Pos2^.Y then Result:=-1
  else if Pos1^.X<Pos2^.X then Result:=1
  else if Pos1^.Y<Pos2^.Y then Result:=-1
  else Result:=0;
end;

function CompareCodePositions(Pos1, Pos2: PCodePosition): integer;
begin
  if Pointer(Pos1^.Code)>Pointer(Pos2^.Code) then Result:=1
  else if Pointer(Pos1^.Code)<Pointer(Pos2^.Code) then Result:=-1
  else if Pos1^.P<Pos2^.P then Result:=1
  else if Pos1^.P>Pos2^.P then Result:=-1
  else Result:=0;
end;

procedure AddCodePosition(var ListOfPCodeXYPosition: TFPList;
  const NewCodePos: TCodeXYPosition);
var
  AddCodePos: PCodeXYPosition;
begin
  if ListOfPCodeXYPosition=nil then ListOfPCodeXYPosition:=TFPList.Create;
  New(AddCodePos);
  AddCodePos^:=NewCodePos;
  ListOfPCodeXYPosition.Add(AddCodePos);
end;

function IndexOfCodePosition(var ListOfPCodeXYPosition: TFPList;
  const APosition: PCodeXYPosition): integer;
begin
  if ListOfPCodeXYPosition=nil then
    Result:=-1
  else begin
    Result:=ListOfPCodeXYPosition.Count-1;
    while (Result>=0)
    and (CompareCodeXYPositions(APosition,
                             PCodeXYPosition(ListOfPCodeXYPosition[Result]))<>0)
    do
      dec(Result);
  end;
end;

procedure FreeListOfPCodeXYPosition(ListOfPCodeXYPosition: TFPList);
var
  CurCodePos: PCodeXYPosition;
  i: Integer;
begin
  if ListOfPCodeXYPosition=nil then exit;
  for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
    CurCodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
    Dispose(CurCodePos);
  end;
  ListOfPCodeXYPosition.Free;
end;

function CreateTreeOfPCodeXYPosition: TAVLTree;
begin
  Result:=TAVLTree.Create(TListSortCompare(@CompareCodeXYPositions));
end;

procedure AddCodePosition(var TreeOfPCodeXYPosition: TAVLTree;
  const NewCodePos: TCodeXYPosition);
var
  AddCodePos: PCodeXYPosition;
begin
  if TreeOfPCodeXYPosition=nil then
    TreeOfPCodeXYPosition:=TAVLTree.Create(TListSortCompare(@CompareCodeXYPositions));
  New(AddCodePos);
  AddCodePos^:=NewCodePos;
  TreeOfPCodeXYPosition.Add(AddCodePos);
end;

procedure FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition: TAVLTree);
var
  ANode: TAVLTreeNode;
  CursorPos: PCodeXYPosition;
begin
  if TreeOfPCodeXYPosition=nil then exit;
  ANode:=TreeOfPCodeXYPosition.FindLowest;
  while ANode<>nil do begin
    CursorPos:=PCodeXYPosition(ANode.Data);
    if CursorPos<>nil then
      Dispose(CursorPos);
    ANode:=TreeOfPCodeXYPosition.FindSuccessor(ANode);
  end;
  TreeOfPCodeXYPosition.Free;
end;

procedure AddListToTreeOfPCodeXYPosition(SrcList: TFPList; DestTree: TAVLTree;
  ClearList, CreateCopies: boolean);
var
  i: Integer;
  CodePos: PCodeXYPosition;
  NewCodePos: PCodeXYPosition;
begin
  if SrcList=nil then exit;
  for i:=SrcList.Count-1 downto 0 do begin
    CodePos:=PCodeXYPosition(SrcList[i]);
    if DestTree.Find(CodePos)=nil then begin
      // new position -> add
      if CreateCopies and (not ClearList) then begin
        // list items should be kept and copies should be added to the tree
        New(NewCodePos);
        NewCodePos^:=CodePos^;
      end else
        NewCodePos:=CodePos;
      DestTree.Add(NewCodePos);
    end else if ClearList then begin
      // position already exists and items should be deleted
      Dispose(CodePos);
    end;
  end;
  if ClearList then
    SrcList.Clear;
end;

function ListOfPCodeXYPositionToStr(const ListOfPCodeXYPosition: TFPList
  ): string;
var
  p: TCodeXYPosition;
  i: Integer;
begin
  if ListOfPCodeXYPosition=nil then
    Result:='nil'
  else begin
    Result:='';
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      p:=PCodeXYPosition(ListOfPCodeXYPosition[i])^;
      Result:=Result+'  '+Dbgs(p)+LineEnding;
    end;
  end;
end;

function Dbgs(const p: TCodeXYPosition): string;
begin
  if p.Code=nil then
    Result:='(none)'
  else
    Result:=p.Code.Filename+'(y='+dbgs(p.y)+',x='+dbgs(p.x)+')';
end;

function Dbgs(const p: TCodePosition): string;
var
  CodeXYPosition: TCodeXYPosition;
begin
  FillChar(CodeXYPosition{%H-},SizeOf(TCodeXYPosition),0);
  CodeXYPosition.Code:=p.Code;
  if CodeXYPosition.Code<>nil then begin
    CodeXYPosition.Code.AbsoluteToLineCol(p.P,CodeXYPosition.Y,CodeXYPosition.X);
  end;
  Result:=Dbgs(CodeXYPosition);
end;

{ TCodeCache }

procedure TCodeCache.Clear;
begin
  FItems.FreeAndClear;
end;

procedure TCodeCache.ClearAllSourceLogEntries;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FItems.FindLowest;
  while ANode<>nil do begin
    TCodeBuffer(ANode.Data).ClearEntries;
    ANode:=FItems.FindSuccessor(ANode);
  end;
end;

procedure TCodeCache.ClearIncludedByEntry(const IncludeFilename: string);
var Code: TCodeBuffer;
  Node: TAVLTreeNode;
begin
  Code:=FindFile(IncludeFilename);
  if Code<>nil then
    Code.LastIncludedByFile:=''
  else begin
    Node:=FindIncludeLinkAVLNode(IncludeFilename);
    if Node<>nil then
      FIncludeLinks.FreeAndDelete(Node);
  end;
end;

procedure TCodeCache.ClearAllModified;
var
  Code: TCodeBuffer;
  ANode: TAVLTreeNode;
begin
  ANode:=FItems.FindLowest;
  while ANode<>nil do begin
    Code:=TCodeBuffer(ANode.Data);
    if Code.Modified then
      Code.Clear;
    ANode:=FItems.FindSuccessor(ANode);
  end;
end;

function TCodeCache.Count: integer;
begin
  Result:=FItems.Count;
end;

constructor TCodeCache.Create;
begin
  inherited Create;
  FItems:=TAVLTree.Create(@CompareCodeBuffers);
  FIncludeLinks:=TAVLTree.Create(@CompareIncludedByLink);
end;

destructor TCodeCache.Destroy;
begin
  FDestroying:=true;
  Clear;
  FIncludeLinks.FreeAndClear;
  FIncludeLinks.Free;
  FItems.Free;
  inherited Destroy;
end;

function TCodeCache.FindFile(AFilename: string): TCodeBuffer;
var c: integer;
  ANode: TAVLTreeNode;
begin
  AFilename:=TrimFilename(AFilename);
  ANode:=FItems.Root;
  while ANode<>nil do begin
    Result:=TCodeBuffer(ANode.Data);
    c:=CompareFilenames(AFilename,Result.Filename);
    {$IFDEF CTDEBUG}
    if c=0 then DebugLn(' File found !!! ',Result.Filename);
    {$ENDIF}
    if c<0 then ANode:=ANode.Left
    else if c>0 then ANode:=ANode.Right
    else exit;
  end;
  Result:=nil;
end;

function TCodeCache.LoadFile(AFilename: string): TCodeBuffer;
var
  DiskFilename: String;

  procedure FindDiskFilenameInconsistent;
  var
    s: String;
  begin
    s:='[TCodeCache.LoadFile] Inconsistency found: AFilename="'+AFilename+'" FindDiskFilename="'+DiskFilename+'"';
    s:=s+' CompareFilenames='+dbgs(CompareFilenames(AFilename,DiskFilename));
    raise Exception.Create(s);
  end;

begin
  AFilename:=TrimFilename(AFilename);
  Result:=FindFile(AFilename);
  if FilenameIsAbsolute(AFilename) then begin
    if Result=nil then begin
      // load new buffer
      if (not FileExistsCached(AFilename)) then exit;
      if DirectoryCachePool<>nil then
        DiskFilename:=DirectoryCachePool.FindDiskFilename(AFilename)
      else
        DiskFilename:=FindDiskFilename(AFilename);
      if FindFile(DiskFilename)<>nil then
        FindDiskFilenameInconsistent;
      Result:=TCodeBuffer.Create;
      Result.Filename:=DiskFilename;
      Result.FCodeCache:=Self;
      if (not Result.LoadFromFile(Result.Filename)) then begin
        Result.FCodeCache:=nil;
        Result.Free;
        Result:=nil;
        exit;
      end;
      FItems.Add(Result);
      with Result do begin
        LastIncludedByFile:=FindIncludeLink(Result.Filename);
        ReadOnly:=not FileIsWritable(Result.Filename);
      end;
    end else if Result.IsDeleted then begin
      // file in cache, but marked as deleted -> load from disk
      if (not FileExistsCached(AFilename))
      or (not Result.LoadFromFile(AFilename)) then
      begin
        Result:=nil;
      end;
    end;
  end else begin
    // virtual file
    if (Result <> nil) and Result.IsDeleted then begin
      // file in cache, but marked as deleted -> no virtual file
      Result:=nil;
    end;
  end;
end;

procedure TCodeCache.RemoveCodeBuffer(Buffer: TCodeBuffer);
begin
  if not FDestroying then
    FItems.Remove(Buffer);
end;

procedure TCodeCache.LoadIncludeLinksDataFromList(List: TStrings);
{ First line is the base date as DateToCfgStr

  The following lines are compressed. Each line starting with a number of
  characters to use from the previous line. Then a colon and the rest of the
  line.
  Each include link has two lines, the first is the IncludeFilename, the
  second the the IncludedByFile plus semicolon and the age in days.
}
var
  BaseDate: TDateTime;
  LastLine: string;
  Index: integer;

  function NextLine: string;
  begin
    // skip empty lines
    repeat
      if Index>=List.Count then begin
        Result:='';
        exit;
      end;
      Result:=List[Index];
      inc(Index);
    until Result<>'';
  end;

  function NextUncompressedLine: string;
  var
    p: Integer;
    Same: Integer;
  begin
    Result:=NextLine;
    p:=1;
    Same:=0;
    while (p<=length(Result)) and (Result[p] in ['0'..'9']) do begin
      Same:=Same*10+ord(Result[p])-ord('0');
      inc(p);
    end;
    while (p<=length(Result)) and (Result[p]<>':') do inc(p);
    Result:=copy(LastLine,1,Same)+copy(Result,p+1,length(Result));
    LastLine:=Result;
    //debugln(['NextUncompressedLine "',Result,'"']);
  end;

var
  IncludeFilename: String;
  IncludedByFile: String;
  p: Longint;
  Days: LongInt;
  Link: TIncludedByLink;
  LastTimeUsed: TDateTime;
  CurrDate: TDateTime;
begin
  FIncludeLinks.FreeAndClear;
  Index:=0;
  CurrDate:=Date;
  LastLine:='';
  if not CfgStrToDate(NextLine,BaseDate) then BaseDate:=Date;
  repeat
    IncludeFilename:=TrimFilename(NextUncompressedLine);
    if IncludeFilename='' then exit;
    IncludedByFile:=TrimFilename(NextUncompressedLine);
    if IncludedByFile='' then begin
      debugln(['TCodeCache.LoadIncludeLinksDataFromList missing IncludedByFile: IncludeFilename=',IncludeFilename,' line=',Index]);
      exit;
    end;
    if not FilenameIsAbsolute(IncludedByFile) then begin
      debugln(['TCodeCache.LoadIncludeLinksDataFromList ignoring relative IncludedByFile: IncludeFilename=',IncludeFilename,' line=',Index]);
      exit;
    end;
    p:=System.Pos(';',IncludedByFile);
    if p<1 then begin
      debugln(['TCodeCache.LoadIncludeLinksDataFromList missing age in IncludedByFile line: ',IncludedByFile,' line=',Index]);
      exit;
    end;
    Days:=StrToIntDef(copy(IncludedByFile,p+1,length(IncludedByFile)),0);
    IncludedByFile:=copy(IncludedByFile,1,p-1);
    LastTimeUsed:=BaseDate-Days;
    //debugln(['TCodeCache.LoadIncludeLinksDataFromList ',IncludeFilename,' ',IncludedByFile,' ',LastTimeUsed]);
    if (FExpirationTimeInDays<=0)
    or (CurrDate-LastTimeUsed<=FExpirationTimeInDays) then begin
      Link:=FindIncludeLinkNode(IncludeFilename);
      if Link=nil then begin
        Link:=TIncludedByLink.Create(IncludeFilename,IncludedByFile,
                                     BaseDate-Days);
        FIncludeLinks.Add(Link);
      end else if Link.LastTimeUsed<=LastTimeUsed then begin
        Link.IncludedByFile:=IncludedByFile;
        Link.LastTimeUsed:=LastTimeUsed;
      end;
    end;
  until false;
end;

function TCodeCache.CreateFile(const AFilename: string): TCodeBuffer;
begin
  Result:=FindFile(AFileName);
  if Result<>nil then begin
    Result.Clear;
  end else begin
    Result:=TCodeBuffer.Create;
    Result.FileName:=AFileName;
    FItems.Add(Result);
    Result.FCodeCache:=Self;// must be called after FileName:=
    Result.LastIncludedByFile:=FindIncludeLink(Result.Filename);
  end;
  Result.DiskEncoding:=DefaultEncoding;
  Result.MemEncoding:=Result.DiskEncoding;
end;

function TCodeCache.SaveBufferAs(OldBuffer: TCodeBuffer; 
  const AFilename: string; out NewBuffer: TCodeBuffer): boolean;
begin
  //DebugLn('[TCodeCache.SaveBufferAs] ',OldBuffer.Filename,' ',AFilename);
  if (OldBuffer=nil) then begin
    NewBuffer:=nil;
    Result:=false;
    exit;
  end;
  if OldBuffer.Filename=AFilename then begin // do not use CompareFilenames() !
    NewBuffer:=OldBuffer;
    Result:=OldBuffer.Save;
    exit;
  end;
  NewBuffer:=FindFile(AFilename);
  //DebugLn('[TCodeCache.SaveBufferAs] B ',NewBuffer=nil);
  //WriteAllFileNames;
  if NewBuffer=nil then begin
    NewBuffer:=TCodeBuffer.Create;
    NewBuffer.FileName:=AFilename;
    NewBuffer.Source:=OldBuffer.Source;
    NewBuffer.DiskEncoding:=NewBuffer.DiskEncoding;
    NewBuffer.MemEncoding:=NewBuffer.MemEncoding;
    NewBuffer.FCodeCache:=Self;
    Result:=NewBuffer.IsVirtual or NewBuffer.Save;
    //DebugLn('[TCodeCache.SaveBufferAs] C ',Result,' ',NewBuffer.IsVirtual);
    if not Result then begin
      NewBuffer.FCodeCache:=nil;
      NewBuffer.Free;
      NewBuffer:=nil;
      exit;
    end;
    FItems.Add(NewBuffer);
    NewBuffer.LastIncludedByFile:=FindIncludeLink(AFilename);
  end else begin
    NewBuffer.Source:=OldBuffer.Source;
    NewBuffer.IsDeleted:=false;
    Result:=NewBuffer.Save;
  end;
  if not Result then exit;
  if (OldBuffer<>NewBuffer) then begin
    OldBuffer.IsDeleted:=true;
    OldBuffer.Source:='';
  end;
end;

procedure TCodeCache.SaveIncludeLinksDataToList(List: TStrings);
{ First line is the base date as DateToCfgStr

  The following lines are compressed. Each line starting with a number of
  characters to use from the previous line. Then a colon and the rest of the
  line.
  Each include link has two lines, the first is the IncludeFilename, the
  second the the IncludedByFile plus semicolon and the age in days.
}
var
  LastLine: String;
  CurrDate: TDateTime;
  ExpirationTime: TDateTime;
  Node: TAVLTreeNode;

  procedure AddLine(Line: string);
  var
    p1: PChar;
    p2: PChar;
    p: PtrUint;
  begin
    p1:=PChar(Line);
    p2:=PChar(LastLine);
    while (p1^=p2^) and (p1^<>#0) do begin
      inc(p1);
      inc(p2);
    end;
    p:=p1-PChar(Line);
    List.Add(IntToStr(p)+':'+copy(Line,p+1,length(Line)));
    LastLine:=Line;
  end;

var
  ALink: TIncludedByLink;
  DiffTime: TDateTime;
begin
  UpdateIncludeLinks;
  if FIncludeLinks.Count=0 then exit;
  ExpirationTime:=TDateTime(FExpirationTimeInDays);
  LastLine:='';
  CurrDate:=Date;
  List.Add(DateToCfgStr(CurrDate));
  Node:=FIncludeLinks.FindLowest;
  while Node<>nil do begin
    ALink:=TIncludedByLink(Node.Data);
    DiffTime:=CurrDate-ALink.LastTimeUsed;
    if (FExpirationTimeInDays<=0) or (DiffTime<ExpirationTime) then begin
      AddLine(ALink.IncludeFilename);
      AddLine(ALink.IncludedByFile+';'+IntToStr(round(CurrDate-ALink.LastTimeUsed)));
    end;
    Node:=FIncludeLinks.FindSuccessor(Node);
  end;
end;

function TCodeCache.LastIncludedByFile(const IncludeFilename: string): string;
var Code: TCodeBuffer;
begin
  Code:=FindFile(IncludeFilename);
  if Code<>nil then
    Result:=Code.LastIncludedByFile
  else begin
    Result:=FindIncludeLink(IncludeFilename);
  end;
end;

procedure TCodeCache.OnBufferSetScanner(Sender: TCodeBuffer);
var
  s: TLinkScanner;
begin
  s:=Sender.Scanner;
  if s=nil then exit;
  s.OnGetSource:=@Self.OnScannerGetSource;
  s.OnGetFileName:=@Self.OnScannerGetFileName;
  s.OnLoadSource:=@Self.OnScannerLoadSource;
  s.OnCheckFileOnDisk:=@Self.OnScannerCheckFileOnDisk;
  s.OnIncludeCode:=@Self.OnScannerIncludeCode;
  s.OnGetSourceStatus:=@Self.OnScannerGetSourceStatus;
  s.OnDeleteSource:=@Self.OnScannerDeleteSource;
end;

procedure TCodeCache.OnBufferSetFileName(Sender: TCodeBuffer;
  const OldFilename: string);
begin
  FItems.Delete(FItems.Find(FindFile(OldFilename)));
  if FindFile(Sender.Filename)=nil then
    FItems.Add(Sender);
end;

function TCodeCache.OnScannerGetFileName(Sender: TObject;
  Code: pointer): string;
begin
  if (Code<>nil) then
    Result:=TCodeBuffer(Code).Filename
  else
    raise Exception.Create('[TCodeCache.OnScannerGetFilename] Code=nil');
end;

function TCodeCache.OnScannerGetSource(Sender: TObject;
  Code: pointer): TSourceLog;
begin
//DebugLn('[TCodeCache.OnScannerGetSource] A ',DbgS(Code),'/',Count);
  if (Code<>nil) then
    Result:=TSourceLog(Code)
  else
    raise Exception.Create('[TCodeCache.OnScannerGetFilename] Code=nil');
end;

function TCodeCache.OnScannerLoadSource(Sender: TObject;
  const AFilename: string; OnlyIfExists: boolean): pointer;
begin
  if OnlyIfExists then begin
    Result:=FindFile(AFilename);
    if (Result=nil)
    and (FilenameIsAbsolute(AFilename) and FileExistsCached(AFilename)) then
      Result:=LoadFile(AFilename);
  end else
    Result:=LoadFile(AFilename);
  //debugln(['TCodeCache.OnScannerLoadSource ']);
  if Result<>nil then
    OnScannerCheckFileOnDisk(Result);
end;

function TCodeCache.OnScannerCheckFileOnDisk(Code: pointer): boolean;
var Buf: TCodeBuffer;
begin
  Result:=false;
  Buf:=TCodeBuffer(Code);
  //DebugLn(['OnScannerCheckFileOnDisk A ',Buf.Filename,' AutoRev=',Buf.AutoRevertFromDisk,' WriteLock=',GlobalWriteLockIsSet,' DiskChg=',Buf.FileOnDiskHasChanged,' IsDeleted=',Buf.IsDeleted]);
  if Buf.AutoRevertFromDisk or Buf.IsDeleted then begin
    if GlobalWriteLockIsSet then begin
      if GlobalWriteLockStep<>Buf.GlobalWriteLockStepOnLastLoad then begin
        Buf.GlobalWriteLockStepOnLastLoad:=GlobalWriteLockStep;
        if Buf.FileNeedsUpdate then
          Result:=true;
      end;
    end else begin
      if Buf.FileNeedsUpdate then
        Result:=true;
    end;
  end else begin
    //DebugLn(['TCodeCache.OnScannerCheckFileOnDisk AutoRevertFromDisk=',Buf.AutoRevertFromDisk,' ',Buf.Filename]);
  end;
  if Result then
    Buf.Revert;
  //if buf.IsDeleted then debugln(['TCodeCache.OnScannerCheckFileOnDisk ',Buf.Filename,' still deleted']);
end;

procedure TCodeCache.OnScannerIncludeCode(ParentCode, IncludeCode: pointer);
var
  CodeBuffer: TCodeBuffer;
begin
  if (ParentCode<>nil) and (IncludeCode<>nil) and (ParentCode<>IncludeCode) then
  begin
    CodeBuffer:=TCodeBuffer(IncludeCode);
    if CodeBuffer.LastIncludedByFile=TCodeBuffer(ParentCode).Filename then exit;
    CodeBuffer.LastIncludedByFile:=TCodeBuffer(ParentCode).Filename;
    IncreaseChangeStep;
  end;
end;

procedure TCodeCache.OnScannerGetSourceStatus(Sender: TObject; Code:Pointer;
  var ReadOnly: boolean);
begin
  ReadOnly:=TCodeBuffer(Code).ReadOnly;
end;

procedure TCodeCache.OnScannerDeleteSource(Sender: TObject; Code: Pointer;
  Pos, Len: integer);
begin
  TCodeBuffer(Code).Delete(Pos,Len);
end;

function TCodeCache.FindIncludeLinkNode(const IncludeFilename: string
  ): TIncludedByLink;
var
  ANode: TAVLTreeNode;
  cmp: integer;
begin
  ANode:=FIncludeLinks.Root;
  while ANode<>nil do begin
    Result:=TIncludedByLink(ANode.Data);
    cmp:=CompareFilenames(IncludeFilename,Result.IncludeFilename);
    if cmp<0 then ANode:=ANode.Left
    else if cmp>0 then ANode:=ANode.Right
    else begin
      exit;
    end;
  end;
  Result:=nil;
end;

function TCodeCache.FindIncludeLinkAVLNode(const IncludeFilename: string
  ): TAVLTreeNode;
begin
  Result:=FIncludeLinks.FindKey(Pointer(IncludeFilename),
                                @CompareAnsiStringWithIncludedByLink);
end;

function TCodeCache.FindIncludeLink(const IncludeFilename: string): string;
var Link: TIncludedByLink;
begin
  Link:=FindIncludeLinkNode(IncludeFilename);
  if Link<>nil then begin
    Result:=Link.IncludedByFile;
    if CompareFilenames(Result,IncludeFilename)=0 then Result:='';
  end else
    Result:='';
end;

procedure TCodeCache.UpdateIncludeLinks;
var CodeNode: TAVLTreeNode;
  IncludeNode: TIncludedByLink;
  Code: TCodeBuffer;
  CurrDate: TDateTime;
begin
  CodeNode:=FItems.FindLowest;
  CurrDate:=Date;
  while CodeNode<>nil do begin
    Code:=TCodeBuffer(CodeNode.Data);
    IncludeNode:=FindIncludeLinkNode(Code.Filename);
    if IncludeNode<>nil then begin
      // there is already an entry for this file -> update it
      IncludeNode.IncludedByFile:=Code.LastIncludedByFile;
      IncludeNode.LastTimeUsed:=CurrDate;
    end else if Code.LastIncludedByFile<>'' then begin
      // there is no entry for this include file -> add one
      FIncludeLinks.Add(TIncludedByLink.Create(Code.Filename,
                        Code.LastIncludedByFile,CurrDate));
    end;
    CodeNode:=FItems.FindSuccessor(CodeNode);
  end;
end;

procedure TCodeCache.IncreaseChangeStep;
begin
  inc(fChangeStep);
  if fChangeStep=$7fffffff then fChangeStep:=-$7fffffff;
end;

procedure TCodeCache.DecodeLoaded(Code: TCodeBuffer; const AFilename: string;
  var ASource, ADiskEncoding, AMemEncoding: string);
begin
  if Assigned(OnDecodeLoaded) then
    OnDecodeLoaded(Code,AFilename,ASource,ADiskEncoding,AMemEncoding);
end;

procedure TCodeCache.EncodeSaving(Code: TCodeBuffer; const AFilename: string;
  var ASource: string);
begin
  if Assigned(OnEncodeSaving) then
    OnEncodeSaving(Code,AFilename,ASource);
end;

function TCodeCache.SaveIncludeLinksToFile(const AFilename: string;
  OnlyIfChanged: boolean): boolean;
var XMLConfig: TXMLConfig;
begin
  try
    if OnlyIfChanged and fLastIncludeLinkFileValid
    and (fLastIncludeLinkFileChangeStep=fChangeStep)
    and (fLastIncludeLinkFile=AFilename)
    and FileExistsCached(AFilename)
    and (FileAgeCached(AFilename)=fLastIncludeLinkFileAge)
    then begin
      //debugln(['TCodeCache.SaveIncludeLinksToFile file valid']);
      exit;
    end;
    XMLConfig:=TXMLConfig.CreateClean(AFilename);
    try
      Result:=SaveIncludeLinksToXML(XMLConfig,'');
      fLastIncludeLinkFile:=AFilename;
      fLastIncludeLinkFileAge:=FileAgeCached(AFilename);
      fLastIncludeLinkFileChangeStep:=fChangeStep;
      fLastIncludeLinkFileValid:=true;
    finally
      XMLConfig.Free;
    end;
  except
    fLastIncludeLinkFileValid:=false;
    Result:=false;
  end;
end;

function TCodeCache.LoadIncludeLinksFromFile(const AFilename: string): boolean;
var XMLConfig: TXMLConfig;
begin
  try
    XMLConfig:=TXMLConfig.Create(AFilename);
    try
      Result:=LoadIncludeLinksFromXML(XMLConfig,'');
      fLastIncludeLinkFile:=AFilename;
      fLastIncludeLinkFileAge:=FileAgeCached(AFilename);
      fLastIncludeLinkFileChangeStep:=fChangeStep;
      fLastIncludeLinkFileValid:=true;
    finally
      XMLConfig.Free;
    end;
  except
    fLastIncludeLinkFileValid:=false;
    Result:=false;
  end;
end;

function TCodeCache.SaveIncludeLinksToXML(XMLConfig: TXMLConfig;
  const XMLPath: string): boolean;
var
  List: TStringList;
begin
  UpdateIncludeLinks;
  XMLConfig.SetValue(XMLPath+'IncludeLinks/Version',IncludeLinksFileVersion);
  XMLConfig.SetDeleteValue(XMLPath+'IncludeLinks/ExpirationTimeInDays',
      FExpirationTimeInDays,0);
  List:=TStringList.Create;
  try
    SaveIncludeLinksDataToList(List);
    XMLConfig.SetDeleteValue(XMLPath+'IncludeLinks/Data',List.Text,'');
  finally
    List.Free;
  end;
  Result:=true;
end;

function TCodeCache.LoadIncludeLinksFromXML(XMLConfig: TXMLConfig;
  const XMLPath: string): boolean;
var LinkCnt, i: integer;
  LastTimeUsed, CurrDate: TDateTime;
  IncludeFilename, IncludedByFile, APath: string;
  NewLink: TIncludedByLink;
  CurrDateStr: String;
  FileVersion: longint;
  List: TStringList;
begin
  FIncludeLinks.FreeAndClear;

  FileVersion:=XMLConfig.GetValue(XMLPath+'IncludeLinks/Version',IncludeLinksFileVersion);
  FExpirationTimeInDays:=XMLConfig.GetValue(
      XMLPath+'IncludeLinks/ExpirationTimeInDays',
      FExpirationTimeInDays);
  if FileVersion>=2 then begin
    List:=TStringList.Create;
    try
      List.Text:=XMLConfig.GetValue(XMLPath+'IncludeLinks/Data','');
      LoadIncludeLinksDataFromList(List);
    finally
      List.Free;
    end;
  end else if FileVersion<=1 then begin
    CurrDate:=Date;
    CurrDateStr:=DateToCfgStr(CurrDate);
    LinkCnt:=XMLConfig.GetValue(XMLPath+'IncludeLinks/Count',0);
    for i:=0 to LinkCnt-1 do begin
      APath:=XMLPath+'IncludeLinks/Link'+IntToStr(i)+'/';
      if not CfgStrToDate(XMLConfig.GetValue(APath+'LastTimeUsed/Value',
           CurrDateStr),LastTimeUsed)
      then begin
        debugln(['TCodeCache.LoadIncludeLinksFromXML invalid date: ',XMLConfig.GetValue(APath+'LastTimeUsed/Value','')]);
        LastTimeUsed:=CurrDate;
      end;
      // ToDo: check if link has expired

      IncludeFilename:=XMLConfig.GetValue(APath+'IncludeFilename/Value','');
      //debugln(['TCodeCache.LoadIncludeLinksFromXML CurrDate=',DateToStr(CurrDate),' xml=',XMLConfig.GetValue(APath+'LastTimeUsed/Value',''),' Days=',CurrDate-LastTimeUsed,' ',IncludeFilename]);
      if IncludeFilename='' then continue;
      IncludedByFile:=XMLConfig.GetValue(APath+'IncludedByFilename/Value','');
      if (FExpirationTimeInDays<=0)
      or (CurrDate-LastTimeUsed<=FExpirationTimeInDays) then begin
        NewLink:=TIncludedByLink.Create(IncludeFilename,IncludedByFile,
                                        LastTimeUsed);
        FIncludeLinks.Add(NewLink);
      end;
    end;
  end;
  Result:=true;
end;

procedure TCodeCache.ConsistencyCheck;
// 0 = ok
var ANode: TAVLTreeNode;
begin
  FItems.ConsistencyCheck;
  FIncludeLinks.ConsistencyCheck;
  ANode:=FItems.FindLowest;
  while ANode<>nil do begin
    if ANode.Data=nil then
      RaiseCatchableException('');
    TCodeBuffer(ANode.Data).ConsistencyCheck;
    ANode:=FItems.FindSuccessor(ANode);
  end;
  ANode:=FIncludeLinks.FindLowest;
  while ANode<>nil do begin
    if ANode.Data=nil then
      RaiseCatchableException('');
    ANode:=FIncludeLinks.FindSuccessor(ANode);
  end;
end;

procedure TCodeCache.WriteDebugReport;
begin
  DebugLn('[TCodeCache.WriteDebugReport]');
  DebugLn(FItems.ReportAsString);
  DebugLn(FIncludeLinks.ReportAsString);
  ConsistencyCheck;
end;

function TCodeCache.CalcMemSize(Stats: TCTMemStats): PtrUInt;
var
  m: PtrUInt;
  Node: TAVLTreeNode;
  IncLink: TIncludedByLink;
  Buf: TCodeBuffer;
begin
  Result:=PtrUInt(InstanceSize)
     +MemSizeString(FDefaultEncoding)
     +MemSizeString(fLastIncludeLinkFile);
  Stats.Add('TCodeCache',Result);
  if FItems<>nil then begin
    m:=FItems.Count*SizeOf(Node);
    Node:=FItems.FindLowest;
    while Node<>nil do begin
      Buf:=TCodeBuffer(Node.Data);
      inc(m,Buf.CalcMemSize);
      Node:=FItems.FindSuccessor(Node);
    end;
    Stats.Add('TCodeCache.Items.Count',FItems.Count);
    Stats.Add('TCodeCache.Items',m);
    inc(Result,m);
  end;
  if FIncludeLinks<>nil then begin
    m:=FIncludeLinks.Count*SizeOf(Node);
    Node:=FIncludeLinks.FindLowest;
    while Node<>nil do begin
      IncLink:=TIncludedByLink(Node.Data);
      inc(m,IncLink.CalcMemSize);
      Node:=FIncludeLinks.FindSuccessor(Node);
    end;
    Stats.Add('TCodeCache.FIncludeLinks.Count',FIncludeLinks.Count);
    Stats.Add('TCodeCache.FIncludeLinks',m);
    inc(Result,m);
  end;
end;

procedure TCodeCache.IncreaseChangeStamp;
begin
  //debugln(['TCodeCache.IncreaseChangeStamp ']);
  CTIncreaseChangeStamp64(FChangeStamp);
end;

procedure TCodeCache.WriteAllFileNames;
  procedure WriteNode(ANode: TAVLTreeNode);
  begin
    if ANode=nil then exit;
    WriteNode(ANode.Left);
    DebugLn('  ',TCodeBuffer(ANode.Data).Filename);
    WriteNode(ANode.Right);
  end;

begin
  DebugLn('TCodeCache.WriteAllFileNames: ',dbgs(FItems.Count));
  WriteNode(FItems.Root);
end;

{ TCodeBuffer }

constructor TCodeBuffer.Create;
begin
  inherited Create('');
  FFilename:='';
  FLastIncludedByFile:='';
  FLoadDateValid:=false;
  FIsVirtual:=true;
  FIsDeleted:=false;
end;

destructor TCodeBuffer.Destroy;
begin
  if Scanner<>nil then Scanner.Free;
  if FCodeCache<>nil then FCodeCache.RemoveCodeBuffer(Self);
  inherited Destroy;
end;

procedure TCodeBuffer.Clear;
begin
  FIsDeleted:=false;
  FLoadDateValid:=false;
  inherited Clear;
end;

function TCodeBuffer.LoadFromFile(const AFilename: string): boolean;
begin
  //DebugLn('[TCodeBuffer.LoadFromFile] WriteLock=',WriteLock,' ReadOnly=',ReadOnly,
  //' IsVirtual=',IsVirtual,' Old="',Filename,'" ',CompareFilenames(AFilename,Filename));
  if (WriteLock>0) or ReadOnly then begin
    Result:=false;
    exit;
  end;
  if (not IsVirtual) or (Filename='') then begin
    if CompareFilenames(AFilename,Filename)=0 then begin
      //DebugLn('[TCodeBuffer.LoadFromFile] ',Filename,' FileDateValid=',FileDateValid,' ',FFileDate,',',FileAgeUTF8(Filename),',',FFileChangeStep,',',ChangeStep,', NeedsUpdate=',FileNeedsUpdate);
      if FileNeedsUpdate then begin
        Result:=inherited LoadFromFile(AFilename);
        if Result then MakeFileDateValid;
      end else
        Result:=true;
    end else begin
      Result:=inherited LoadFromFile(AFilename);
      if Result then MakeFileDateValid;
    end;
    if Result then IsDeleted:=false;
  end else
    Result:=false;
end;

function TCodeBuffer.SaveToFile(const AFilename: string): boolean;
begin
  Result:=inherited SaveToFile(AFilename);
  //DebugLn(['TCodeBuffer.SaveToFile ',Filename,' -> ',AFilename,' ',Result]);
  if CompareFilenames(AFilename,Filename)=0 then begin
    if Result then begin
      IsDeleted:=false;
      MakeFileDateValid;
      Modified:=false;
    end;
  end;
  //debugln(['TCodeBuffer.SaveToFile FileOnDiskHasChanged=',FileOnDiskHasChanged,' LoadDate=',LoadDate,' FileAgeCached=',FileAgeCached(Filename)]);
end;

function TCodeBuffer.Reload: boolean;
begin
  Result:=LoadFromFile(Filename);
end;

function TCodeBuffer.Revert: boolean;
// ignore changes and reload source
begin
  if not IsVirtual then begin
    Result:=inherited LoadFromFile(Filename);
    if Result then MakeFileDateValid;
  end else
    Result:=false;
end;

function TCodeBuffer.Save: boolean;
begin
  if not IsVirtual then
    Result:=SaveToFile(Filename)
  else
    Result:=false;
end;

function TCodeBuffer.GetLastIncludedByFile: string;
begin
  Result:=FLastIncludedByFile;
  if Result=Filename then Result:='';
end;

procedure TCodeBuffer.SetFilename(Value: string);
var OldFilename: string;
begin
  Value:=TrimFilename(Value);
  if FFilename=Value then exit;
  OldFilename:=FFilename;
  FFilename := Value;
  FIsVirtual:=not FilenameIsAbsolute(Filename);
  if CompareFilenames(OldFileName,Value)<>0 then begin
    FLoadDateValid:=false;
  end;
  FLastIncludedByFile:='';
  if FCodeCache<>nil then FCodeCache.OnBufferSetFilename(Self,OldFilename);
  if Assigned(FOnSetFilename) then FOnSetFilename(Self);
end;

procedure TCodeBuffer.SetScanner(const Value: TLinkScanner);
begin
  if FScanner=Value then exit;
  FScanner := Value;
  if Assigned(FOnSetScanner) then FOnSetScanner(Self);
  if FCodeCache<>nil then FCodeCache.OnBufferSetScanner(Self);
  if FScanner<>nil then
    FScanner.MainCode:=Self;
end;

procedure TCodeBuffer.SetIsDeleted(const NewValue: boolean);
begin
  if FIsDeleted=NewValue then exit;
  //debugln(['TCodeBuffer.SetIsDeleted ',Filename,' ',NewValue]);
  IncreaseChangeStep;
  FIsDeleted:=NewValue;
  if FIsDeleted then begin
    Clear;
    FIsDeleted:=true;
    //DebugLn(['TCodeBuffer.SetIsDeleted ',Filename,' ',FileNeedsUpdate]);
  end;
end;

procedure TCodeBuffer.DoSourceChanged;
begin
  //debugln(['TCodeBuffer.DoSourceChanged ',Filename]);
  inherited DoSourceChanged;
  if FCodeCache<>nil then
    FCodeCache.IncreaseChangeStamp;
end;

procedure TCodeBuffer.DecodeLoaded(const AFilename: string; var ASource,
  ADiskEncoding, AMemEncoding: string);
begin
  inherited DecodeLoaded(AFilename,ASource,ADiskEncoding,AMemEncoding);
  if CodeCache<>nil then
    CodeCache.DecodeLoaded(Self,AFilename,ASource,ADiskEncoding,AMemEncoding);
end;

procedure TCodeBuffer.EncodeSaving(const AFilename: string; var ASource: string);
begin
  inherited EncodeSaving(AFilename,ASource);
  if CodeCache<>nil then
    CodeCache.EncodeSaving(Self,AFilename,ASource);
end;

procedure TCodeBuffer.MakeFileDateValid;
begin
  FFileChangeStep:=ChangeStep;
  FLoadDateValid:=true;
  FLoadDate:=FileAgeCached(Filename);
end;

procedure TCodeBuffer.InvalidateLoadDate;
begin
  FLoadDateValid:=false;
end;

function TCodeBuffer.SourceIsText: boolean;
var
  l: LongInt;
  i: Integer;
  s: String;
begin
  l:=SourceLength;
  if l>1024 then l:=1024;
  s:=Source;
  for i:=1 to l do
    if s[i] in [#0..#8,#11..#12,#14..#31] then exit(false);
  Result:=true;
end;

function TCodeBuffer.FileDateOnDisk: longint;
begin
  Result:=FileAgeCached(Filename);
end;

function TCodeBuffer.FileNeedsUpdate(IgnoreModifiedFlag: Boolean): boolean;
// file needs update (to be loaded), if file is not modified and file on disk has changed
begin
  if IgnoreModifiedFlag then
  begin
    if IsVirtual then exit(false);
    Result:=FileDateOnDisk<>LoadDate; // ignore LoadDateValid because it is set to false after edit
  end else
  begin
    if Modified or IsVirtual then exit(false);
    if LoadDateValid then
      Result:=(FFileChangeStep=ChangeStep) and (FileDateOnDisk<>LoadDate)
    else
      Result:=true;
  end;
end;

function TCodeBuffer.FileOnDiskNeedsUpdate: boolean;
// file on disk needs update (= file needs to be saved), if memory is modified or file does not exist
begin
  if IsVirtual or IsDeleted then exit(false);
  Result:=Modified
          or (not LoadDateValid) // file was created in memory, but not yet saved to disk
          or (FFileChangeStep<>ChangeStep) // file was modified since last load/save
          or (not FileExistsCached(Filename));
end;

function TCodeBuffer.FileOnDiskHasChanged(IgnoreModifiedFlag: Boolean): boolean;
// file on disk has changed since last load/save
begin
  if IsVirtual then exit(false);
  if IgnoreModifiedFlag then
  begin
    if FileExistsCached(Filename) then
      Result:=(FileDateOnDisk<>LoadDate) // ignore LoadDateValid because it is set to false after edit
    else
      Result:=false;
  end else
  begin
    if LoadDateValid and FileExistsCached(Filename) then
      Result:=(FileDateOnDisk<>LoadDate)
    else
      Result:=false;
  end;
end;

function TCodeBuffer.FileOnDiskIsEqual: boolean;
begin
  if IsVirtual then
    exit(true);
  if IsDeleted then
    exit(not FileExistsCached(Filename));
  if (not LoadDateValid)
  or Modified or (FFileChangeStep<>ChangeStep)
  or (not FileExistsCached(Filename))
  or (FileDateOnDisk<>LoadDate)
  then
    exit(false);
  Result:=true;
end;

function TCodeBuffer.AutoRevertFromDisk: boolean;
begin
  Result:=FAutoDiskRevertLock=0;
end;

procedure TCodeBuffer.LockAutoDiskRevert;
begin
  inc(FAutoDiskRevertLock);
end;

procedure TCodeBuffer.UnlockAutoDiskRevert;
begin
  if FAutoDiskRevertLock>0 then dec(FAutoDiskRevertLock);
end;

procedure TCodeBuffer.IncrementRefCount;
begin
  inc(FReferenceCount);
end;

procedure TCodeBuffer.ReleaseRefCount;
begin
  if FReferenceCount=0 then
    raise Exception.Create('TCodeBuffer.ReleaseRefCount');
  dec(FReferenceCount);
end;

procedure TCodeBuffer.ConsistencyCheck;
begin
  if FScanner<>nil then
    FScanner.ConsistencyCheck;
end;

procedure TCodeBuffer.WriteDebugReport;
begin
  DebugLn('[TCodeBuffer.WriteDebugReport] ');
  ConsistencyCheck;
end;

function TCodeBuffer.CalcMemSize: PtrUInt;
begin
  Result:=(inherited CalcMemSize)
    +MemSizeString(FFilename)
    +MemSizeString(FLastIncludedByFile);
end;

{ TIncludedByLink }

constructor TIncludedByLink.Create(const AnIncludeFilename,
  AnIncludedByFile: string; ALastTimeUsed: TDateTime);
begin
  inherited Create;
  IncludeFilename:=AnIncludeFilename;
  IncludedByFile:=AnIncludedByFile;
  LastTimeUsed:=ALastTimeUsed;
end;

function TIncludedByLink.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize)
    +MemSizeString(IncludedByFile)
    +MemSizeString(IncludeFilename);
end;

{ TCodeXYPositions }

function TCodeXYPositions.GetItems(Index: integer): PCodeXYPosition;
begin
  Result:=PCodeXYPosition(FItems[Index]);
end;

function TCodeXYPositions.GetCaretsXY(Index: integer): TPoint;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Point(Item^.X,Item^.Y);
end;

function TCodeXYPositions.GetCodes(Index: integer): TCodeBuffer;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Item^.Code;
end;

procedure TCodeXYPositions.SetCaretsXY(Index: integer; const AValue: TPoint);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.X:=AValue.X;
  Item^.Y:=AValue.Y;
end;

procedure TCodeXYPositions.SetCodes(Index: integer; const AValue: TCodeBuffer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.Code:=AValue;
end;

procedure TCodeXYPositions.SetItems(Index: integer;
  const AValue: PCodeXYPosition);
begin
  FItems[Index]:=AValue;
end;

constructor TCodeXYPositions.Create;
begin

end;

destructor TCodeXYPositions.Destroy;
begin
  Clear;
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

procedure TCodeXYPositions.Clear;
var
  i: Integer;
  Item: PCodeXYPosition;
begin
  if FItems<>nil then begin
    for i:=0 to FItems.Count-1 do begin
      Item:=Items[i];
      Dispose(Item);
    end;
    FItems.Clear;
  end;
end;

function TCodeXYPositions.Add(const Position: TCodeXYPosition): integer;
var
  NewItem: PCodeXYPosition;
begin
  New(NewItem);
  NewItem^:=Position;
  if FItems=nil then FItems:=TFPList.Create;
  Result:=FItems.Add(NewItem);
end;

function TCodeXYPositions.Add(X, Y: integer; Code: TCodeBuffer): integer;
var
  NewItem: TCodeXYPosition;
begin
  NewItem.X:=X;
  NewItem.Y:=Y;
  NewItem.Code:=Code;
  Result:=Add(NewItem);
end;

procedure TCodeXYPositions.Assign(Source: TCodeXYPositions);
var
  i: Integer;
begin
  if IsEqual(Source) then exit;
  Clear;
  for i:=0 to Source.Count-1 do
    Add(Source[i]^);
end;

function TCodeXYPositions.IsEqual(Source: TCodeXYPositions): boolean;
var
  SrcItem: TCodeXYPosition;
  CurItem: TCodeXYPosition;
  i: Integer;
begin
  if Source=Self then
    Result:=true
  else if (Source=nil) or (Source.Count<>Count) then
    Result:=false
  else begin
    for i:=0 to Count-1 do begin
      SrcItem:=Source[i]^;
      CurItem:=Items[i]^;
      if (SrcItem.X<>CurItem.X)
      or (SrcItem.Y<>CurItem.Y)
      or (SrcItem.Code<>CurItem.Code)
      then begin
        Result:=false;
        exit;
      end;
    end;
    Result:=true;
  end;
end;

function TCodeXYPositions.Count: integer;
begin
  if FItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

procedure TCodeXYPositions.Delete(Index: integer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Dispose(Item);
  FItems.Delete(Index);
end;

function TCodeXYPositions.CreateCopy: TCodeXYPositions;
begin
  Result:=TCodeXYPositions.Create;
  Result.Assign(Self);
end;

function TCodeXYPositions.CalcMemSize: PtrUint;
begin
  Result:=PtrUInt(InstanceSize);
  if FItems<>nil then
    inc(Result,PtrUInt(FItems.InstanceSize)
      +PtrUInt(FItems.Capacity)*SizeOf(TCodeXYPosition));
end;

end.
 
