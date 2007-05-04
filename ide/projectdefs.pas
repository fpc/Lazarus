{
 /***************************************************************************
                projectdefs.pas  -  project definitions file
                --------------------------------------------


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
unit ProjectDefs;

{$mode objfpc}{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  Classes, SysUtils, Laz_XMLCfg,
  Forms, SynRegExpr, FileUtil, LCLProc,
  NewItemIntf, ProjectIntf,
  LazarusIDEStrConsts, PublishModule;

type
  TOnLoadSaveFilename = procedure(var Filename:string; Load:boolean) of object;

  TProjectWriteFlag = (
    pwfDontSaveClosedUnits,
    pwfSaveOnlyProjectUnits,
    pwfSkipDebuggerSettings,
    pwfSkipJumpPoints,
    pwfDoNotSaveProjectInfo,
    pwfDoNotSaveSessionInfo
    );
  TProjectWriteFlags = set of TProjectWriteFlag;

const
  pwfSkipSessionInfo = [pwfSaveOnlyProjectUnits,pwfSkipDebuggerSettings,
                        pwfSkipJumpPoints];
  
type
  TNewUnitType = (
    nuEmpty,      // no code
    nuUnit,       // unit
    nuForm,       // unit with form
    nuDataModule, // unit with data module
    nuCGIDataModule, // unit with cgi data module
    nuText,
    nuCustomProgram  // program
   );

  TUnitUsage = (uuIsPartOfProject, uuIsLoaded, uuIsModified, uuNotUsed);
  
  
  { TLazProjectFileDescriptors }
  
  TLazProjectFileDescriptors = class(TProjectFileDescriptors)
  private
    FDefaultPascalFileExt: string;
    fDestroying: boolean;
    fItems: TList; // list of TProjectFileDescriptor
    procedure SetDefaultPascalFileExt(const AValue: string);
  protected
    function GetItems(Index: integer): TProjectFileDescriptor; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; override;
    function GetUniqueName(const Name: string): string; override;
    function IndexOf(const Name: string): integer; override;
    function IndexOf(FileDescriptor: TProjectFileDescriptor): integer; override;
    function FindByName(const Name: string): TProjectFileDescriptor; override;
    procedure RegisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); override;
    procedure UnregisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); override;
    procedure UpdateDefaultPascalFileExtensions;
  public
    property DefaultPascalFileExt: string read FDefaultPascalFileExt write SetDefaultPascalFileExt;
  end;
  
  
  { TLazProjectDescriptors }

  TLazProjectDescriptors = class(TProjectDescriptors)
  private
    fDestroying: boolean;
    fItems: TList; // list of TProjectDescriptor
  protected
    function GetItems(Index: integer): TProjectDescriptor; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer; override;
    function GetUniqueName(const Name: string): string; override;
    function IndexOf(const Name: string): integer; override;
    function IndexOf(Descriptor: TProjectDescriptor): integer; override;
    function FindByName(const Name: string): TProjectDescriptor; override;
    procedure RegisterDescriptor(Descriptor: TProjectDescriptor); override;
    procedure UnregisterDescriptor(Descriptor: TProjectDescriptor); override;
  end;

var
  LazProjectFileDescriptors: TLazProjectFileDescriptors;
  LazProjectDescriptors: TLazProjectDescriptors;

type
  //---------------------------------------------------------------------------
  // bookmarks of a single file
  TFileBookmark = class
  private
    fCursorPos: TPoint;
    fID: integer;
  public
    constructor Create;
    constructor Create(NewX,NewY,AnID: integer);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    function X: integer;
    function Y: integer;
  public
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property ID: integer read fID write fID;
  end;
  
  TFileBookmarks = class
  private
    FBookmarks:TList;  // list of TFileBookmark
    function GetBookmarks(Index:integer):TFileBookmark;
    procedure SetBookmarks(Index:integer; ABookmark: TFileBookmark);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TFileBookmark
       read GetBookmarks write SetBookmarks; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABookmark: TFileBookmark):integer;
    function Add(X,Y,ID: integer):integer;
    function IndexOfID(ID:integer):integer;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  
  //---------------------------------------------------------------------------
  // The currently visible bookmarks of the project
  TProjectBookmark = class
  private
    fCursorPos: TPoint;
    fEditorIndex: integer;
    fID: integer;
  public
    constructor Create;
    constructor Create(X,Y, AnEditorIndex, AnID: integer);
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property EditorIndex: integer read fEditorIndex write fEditorIndex;
    property ID:integer read fID write fID;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  TProjectBookmarkList = class
  private
    FBookmarks:TList;  // list of TProjectBookmark
    function GetBookmarks(Index:integer):TProjectBookmark;
    procedure SetBookmarks(Index:integer;  ABookmark: TProjectBookmark);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TProjectBookmark
       read GetBookmarks write SetBookmarks; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABookmark: TProjectBookmark):integer;
    function Add(X,Y,EditorID,ID: integer):integer;
    procedure DeleteAllWithEditorIndex(EditorIndex:integer);
    function IndexOfID(ID:integer):integer;
    function BookmarkWithIndex(ID: integer): TProjectBookmark;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

type
//---------------------------------------------------------------------------
  TProjectJumpHistoryPosition = class
  private
    FCaretXY: TPoint;
    FFilename: string;
    FTopLine: integer;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
  public
    procedure Assign(APosition: TProjectJumpHistoryPosition);
    constructor Create(const AFilename: string; ACaretXY: TPoint; 
      ATopLine: integer);
    constructor Create(APosition: TProjectJumpHistoryPosition);
    function IsEqual(APosition: TProjectJumpHistoryPosition): boolean;
    function IsSimilar(APosition: TProjectJumpHistoryPosition): boolean;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    property CaretXY: TPoint read FCaretXY write FCaretXY; // logical (byte) position
    property Filename: string read FFilename write FFilename;
    property TopLine: integer read FTopLine write FTopLine;
    property OnLoadSaveFilename: TOnLoadSaveFilename
        read fOnLoadSaveFilename write fOnLoadSaveFilename;
  end;

  TCheckPositionEvent = 
    function(APosition:TProjectJumpHistoryPosition): boolean of object;

  TProjectJumpHistory = class
  private
    FHistoryIndex: integer;
    FOnCheckPosition: TCheckPositionEvent;
    FPositions:TList;  // list of TProjectJumpHistoryPosition
    FMaxCount: integer;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
    function GetPositions(Index:integer):TProjectJumpHistoryPosition;
    procedure SetPositions(Index:integer; APosition: TProjectJumpHistoryPosition);
  public
    function Add(APosition: TProjectJumpHistoryPosition):integer;
    function AddSmart(APosition: TProjectJumpHistoryPosition):integer;
    constructor Create;
    procedure Clear;
    procedure DeleteInvalidPositions;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure DeleteFirst;
    procedure DeleteForwardHistory;
    procedure DeleteLast;
    destructor Destroy; override;
    function IndexOf(APosition: TProjectJumpHistoryPosition): integer;
    function FindIndexOfFilename(const Filename: string; 
      StartIndex: integer): integer;
    procedure Insert(Index: integer; APosition: TProjectJumpHistoryPosition);
    procedure InsertSmart(Index: integer; APosition: TProjectJumpHistoryPosition);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure WriteDebugReport;
    property HistoryIndex: integer read FHistoryIndex write FHistoryIndex;
    property Items[Index:integer]:TProjectJumpHistoryPosition 
       read GetPositions write SetPositions; default;
    property MaxCount: integer read FMaxCount write FMaxCount;
    property OnCheckPosition: TCheckPositionEvent
       read FOnCheckPosition write FOnCheckPosition;
    property OnLoadSaveFilename: TOnLoadSaveFilename
        read fOnLoadSaveFilename write fOnLoadSaveFilename;
  end;

  //---------------------------------------------------------------------------

  { TPublishProjectOptions }
  
  TPublishProjectOptions = class(TPublishModuleOptions)
  private
    FSaveClosedEditorFilesInfo: boolean;
    FSaveEditorInfoOfNonProjectFiles: boolean;
    procedure SetSaveClosedEditorFilesInfo(const AValue: boolean);
    procedure SetSaveEditorInfoOfNonProjectFiles(const AValue: boolean);
  public
    procedure LoadDefaults; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const APath: string;
                                AdjustPathDelims: boolean); override;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const APath: string); override;
    function WriteFlags: TProjectWriteFlags;
  public
    // project info
    property SaveEditorInfoOfNonProjectFiles: boolean
                read FSaveEditorInfoOfNonProjectFiles
                write SetSaveEditorInfoOfNonProjectFiles;
    property SaveClosedEditorFilesInfo: boolean
                read FSaveClosedEditorFilesInfo
                write SetSaveClosedEditorFilesInfo;
  end;


  //---------------------------------------------------------------------------

const
  DefPublProjIncFilter = '*.(pas|pp|inc|lfm|lpr|lrs|lpi|lpk|sh|xml)';
  DefPublProjExcFilter = '*.(bak|ppu|ppw|o|so);*~;backup';


implementation


{ TProjectBookmark }

constructor TProjectBookmark.Create;
begin
  inherited Create;
end;

constructor TProjectBookmark.Create(X,Y, AnEditorIndex, AnID: integer);
begin
  inherited Create;
  fCursorPos.X:=X;
  fCursorPos.Y:=Y;
  fEditorIndex:=AnEditorIndex;
  fID:=AnID;
end;

procedure TProjectBookmark.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  XMLConfig.SetValue(Path+'ID',ID);
  XMLConfig.SetValue(Path+'CursorPosX',CursorPos.X);
  XMLConfig.SetValue(Path+'CursorPosY',CursorPos.Y);
  XMLConfig.SetValue(Path+'EditorIndex',EditorIndex);
end;

procedure TProjectBookmark.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  ID:=XMLConfig.GetValue(Path+'ID',-1);
  CursorPos:=Point(XMLConfig.GetValue(Path+'CursorPosX',0),
                  XMLConfig.GetValue(Path+'CursorPosY',0));
  EditorIndex:=XMLConfig.GetValue(Path+'EditorIndex',-1);
end;


{ TProjectBookmarkList }

constructor TProjectBookmarkList.Create;
begin
  inherited Create;
  fBookmarks:=TList.Create;
end;

destructor TProjectBookmarkList.Destroy;
begin
  Clear;
  fBookmarks.Free;
  inherited Destroy;
end;

procedure TProjectBookmarkList.Clear;
var a:integer;
begin
  for a:=0 to fBookmarks.Count-1 do Items[a].Free;
  fBookmarks.Clear;
end;

function TProjectBookmarkList.Count:integer;
begin
  Result:=fBookmarks.Count;
end;

function TProjectBookmarkList.GetBookmarks(Index:integer):TProjectBookmark;
begin
  Result:=TProjectBookmark(fBookmarks[Index]);
end;

procedure TProjectBookmarkList.SetBookmarks(Index:integer;  
  ABookmark: TProjectBookmark);
begin
  fBookmarks[Index]:=ABookmark;
end;

function TProjectBookmarkList.IndexOfID(ID:integer):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].ID<>ID) do dec(Result);
end;

function TProjectBookmarkList.BookmarkWithIndex(ID: integer
  ): TProjectBookmark;
var
  i: Integer;
begin
  i:=IndexOfID(ID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TProjectBookmarkList.Delete(Index:integer);
begin
  Items[Index].Free;
  fBookmarks.Delete(Index);
end;

procedure TProjectBookmarkList.DeleteAllWithEditorIndex(
  EditorIndex:integer);
var i:integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    if Items[i].EditorIndex=EditorIndex then Delete(i);
    dec(i);
  end;
end;

function TProjectBookmarkList.Add(ABookmark: TProjectBookmark):integer;
var
  i: Integer;
begin
  i:=IndexOfID(ABookmark.ID);
  if i>=0 then Delete(i);
  Result:=fBookmarks.Add(ABookmark);
end;

function TProjectBookmarkList.Add(X, Y, EditorID, ID: integer): integer;
begin
  Result:=Add(TProjectBookmark.Create(X,Y,EditorID,ID));
end;

procedure TProjectBookmarkList.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a:integer;
begin
  XMLConfig.SetDeleteValue(Path+'Bookmarks/Count',Count,0);
  for a:=0 to Count-1 do
    Items[a].SaveToXMLConfig(XMLConfig,Path+'Bookmarks/Mark'+IntToStr(a)+'/');
end;

procedure TProjectBookmarkList.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a,NewCount:integer;
  NewBookmark:TProjectBookmark;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Bookmarks/Count',0);
  for a:=0 to NewCount-1 do begin
    NewBookmark:=TProjectBookmark.Create;
    Add(NewBookmark);
    NewBookmark.LoadFromXMLConfig(XMLConfig,Path+'Bookmarks/Mark'+IntToStr(a)+'/');
  end;
end;

{ TProjectJumpHistoryPosition }

constructor TProjectJumpHistoryPosition.Create(const AFilename: string;
  ACaretXY: TPoint; ATopLine: integer);
begin
  inherited Create;
  FCaretXY:=ACaretXY;
  FFilename:=AFilename;
  FTopLine:=ATopLine;
end;

constructor TProjectJumpHistoryPosition.Create(
  APosition: TProjectJumpHistoryPosition);
begin
  inherited Create;
  Assign(APosition);
end;

procedure TProjectJumpHistoryPosition.Assign(
  APosition: TProjectJumpHistoryPosition);
begin
  FCaretXY:=APosition.CaretXY;
  FFilename:=APosition.Filename;
  FTopLine:=APosition.TopLine;
end;

function TProjectJumpHistoryPosition.IsEqual(
  APosition: TProjectJumpHistoryPosition): boolean;
begin
  Result:=(Filename=APosition.Filename)
      and (CaretXY.X=APosition.CaretXY.X) and (CaretXY.Y=APosition.CaretXY.Y)
      and (TopLine=APosition.TopLine);
end;

function TProjectJumpHistoryPosition.IsSimilar(
  APosition: TProjectJumpHistoryPosition): boolean;
begin
  Result:=(Filename=APosition.Filename)
      and (CaretXY.Y=APosition.CaretXY.Y);
end;

procedure TProjectJumpHistoryPosition.LoadFromXMLConfig(
  XMLConfig: TXMLConfig; const Path: string);
var AFilename: string;
begin
  FCaretXY.Y:=XMLConfig.GetValue(Path+'Caret/Line',0);
  FCaretXY.X:=XMLConfig.GetValue(Path+'Caret/Column',0);
  FTopLine:=XMLConfig.GetValue(Path+'Caret/TopLine',0);
  AFilename:=XMLConfig.GetValue(Path+'Filename/Value','');
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,true);
  fFilename:=AFilename;
end;

procedure TProjectJumpHistoryPosition.SaveToXMLConfig(
  XMLConfig: TXMLConfig; const Path: string);
var AFilename: string;
begin
  AFilename:=Filename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,false);
  XMLConfig.SetValue(Path+'Filename/Value',AFilename);
  XMLConfig.SetValue(Path+'Caret/Line',FCaretXY.Y);
  XMLConfig.SetValue(Path+'Caret/Column',FCaretXY.X);
  XMLConfig.SetValue(Path+'Caret/TopLine',FTopLine);
end;

{ TProjectJumpHistory }

function TProjectJumpHistory.GetPositions(
  Index:integer):TProjectJumpHistoryPosition;
begin
  if (Index<0) or (Index>=Count) then
    raise Exception.Create('TProjectJumpHistory.GetPositions: Index '
      +IntToStr(Index)+' out of bounds. Count='+IntToStr(Count));
  Result:=TProjectJumpHistoryPosition(FPositions[Index]);
end;

procedure TProjectJumpHistory.SetPositions(Index:integer;
  APosition: TProjectJumpHistoryPosition);
begin
  if (Index<0) or (Index>=Count) then
    raise Exception.Create('TProjectJumpHistory.SetPositions: Index '
      +IntToStr(Index)+' out of bounds. Count='+IntToStr(Count));
  Items[Index].Assign(APosition);
end;

function TProjectJumpHistory.Add(
  APosition: TProjectJumpHistoryPosition):integer;
begin
  Result:=FPositions.Add(APosition);
  APosition.OnLoadSaveFilename:=OnLoadSaveFilename;
  FHistoryIndex:=Count-1;
  if Count>MaxCount then DeleteFirst;
end;

function TProjectJumpHistory.AddSmart(
  APosition: TProjectJumpHistoryPosition):integer;
// add, if last Item is not equal to APosition
begin
  if (Count=0) or (not Items[Count-1].IsEqual(APosition)) then
    Result:=Add(APosition)
  else begin
    APosition.Free;
    Result:=-1;
  end;
end;

constructor TProjectJumpHistory.Create;
begin
  inherited Create;
  FPositions:=TList.Create;
  FHistoryIndex:=-1;
  FMaxCount:=30;
end;

procedure TProjectJumpHistory.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].Free;
  FPositions.Clear;
  FHistoryIndex:=-1;
end;

function TProjectJumpHistory.Count:integer;
begin
  Result:=FPositions.Count;
end;

procedure TProjectJumpHistory.Delete(Index:integer);
begin
  Items[Index].Free;
  FPositions.Delete(Index);
  if FHistoryIndex>=Index then dec(FHistoryIndex);
end;

destructor TProjectJumpHistory.Destroy;
begin
  Clear;
  FPositions.Free;
  inherited Destroy;
end;

function TProjectJumpHistory.IndexOf(APosition: TProjectJumpHistoryPosition
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (not APosition.IsEqual(Items[Result])) do
    dec(Result);
end;

procedure TProjectJumpHistory.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var i, NewCount, NewHistoryIndex: integer;
  NewPosition: TProjectJumpHistoryPosition;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'JumpHistory/Count',0);
  NewHistoryIndex:=XMLConfig.GetValue(Path+'JumpHistory/HistoryIndex',-1);
  NewPosition:=nil;
  for i:=0 to NewCount-1 do begin
    if NewPosition=nil then
      NewPosition:=TProjectJumpHistoryPosition.Create('',Point(0,0),0);
    NewPosition.LoadFromXMLConfig(XMLConfig,
                                 Path+'JumpHistory/Position'+IntToStr(i+1)+'/');
    if (NewPosition.Filename<>'') and (NewPosition.CaretXY.Y>0)
    and (NewPosition.CaretXY.X>0) and (NewPosition.TopLine>0)
    and (NewPosition.TopLine<=NewPosition.CaretXY.Y) then begin
      Add(NewPosition);
      NewPosition:=nil;
    end else if NewHistoryIndex>=i then
      dec(NewHistoryIndex);
  end;
  if NewPosition<>nil then NewPosition.Free;
  if (NewHistoryIndex<0) or (NewHistoryIndex>=Count) then 
    NewHistoryIndex:=Count-1;
  FHistoryIndex:=NewHistoryIndex;
end;

procedure TProjectJumpHistory.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var i: integer;
begin
  XMLConfig.SetValue(Path+'JumpHistory/Count',Count);
  XMLConfig.SetValue(Path+'JumpHistory/HistoryIndex',HistoryIndex);
  for i:=0 to Count-1 do begin
    Items[i].SaveToXMLConfig(XMLConfig,
                             Path+'JumpHistory/Position'+IntToStr(i+1)+'/');
  end;
end;

function TProjectJumpHistory.FindIndexOfFilename(const Filename: string; 
  StartIndex: integer): integer;
begin
  Result:=StartIndex;
  while (Result<Count) do begin
    if (CompareFilenames(Filename,Items[Result].Filename)=0) then exit;
    inc(Result);
  end;
  Result:=-1;
end;

procedure TProjectJumpHistory.DeleteInvalidPositions;
var i: integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    if (Items[i].Filename='') or (Items[i].CaretXY.Y<1)
    or (Items[i].CaretXY.X<1)
    or (Assigned(FOnCheckPosition) and (not FOnCheckPosition(Items[i]))) then
    begin
      Delete(i);
    end;
    dec(i);
  end;
end;

procedure TProjectJumpHistory.DeleteLast;
begin
  if Count=0 then exit;
  Delete(Count-1);
end;

procedure TProjectJumpHistory.DeleteFirst;
begin
  if Count=0 then exit;
  Delete(0);
end;

procedure TProjectJumpHistory.Insert(Index: integer;
  APosition: TProjectJumpHistoryPosition);
begin
  APosition.OnLoadSaveFilename:=OnLoadSaveFilename;
  if Count=MaxCount then begin
    if Index>0 then begin
      DeleteFirst;
      dec(Index);
    end else
      DeleteLast;
  end;
  if Index<0 then Index:=0;
  if Index>Count then Index:=Count;
  FPositions.Insert(Index,APosition);
  if (FHistoryIndex<0) and (Count=1) then
    FHistoryIndex:=0
  else if FHistoryIndex>=Index then
    inc(FHistoryIndex);
end;

procedure TProjectJumpHistory.InsertSmart(Index: integer;
  APosition: TProjectJumpHistoryPosition);
// insert if item after or in front of Index is not similar to APosition
// else replace the similar with the new updated version
var
  NewIndex: integer;
begin
  if Index<0 then Index:=Count;
  if (Index<=Count) then begin
    if (Index>0) and Items[Index-1].IsSimilar(APosition) then begin
      //writeln('TProjectJumpHistory.InsertSmart Replacing prev: Index=',Index,
      //  ' Old=',Items[Index-1].CaretXY.X,',',Items[Index-1].CaretXY.Y,' ',Items[Index-1].Filename,
      //  ' New=',APosition.CaretXY.X,',',APosition.CaretXY.Y,' ',APosition.Filename,
      //  ' ');
      Items[Index-1]:=APosition;
      NewIndex:=Index-1;
      APosition.Free;
    end else if (Index<Count) and Items[Index].IsSimilar(APosition) then begin
      //writeln('TProjectJumpHistory.InsertSmart Replacing next: Index=',Index,
      //  ' Old=',Items[Index].CaretXY.X,',',Items[Index].CaretXY.Y,' ',Items[Index].Filename,
      //  ' New=',APosition.CaretXY.X,',',APosition.CaretXY.Y,' ',APosition.Filename,
      //  ' ');
      Items[Index]:=APosition;
      NewIndex:=Index;
      APosition.Free;
    end else begin
      //writeln('TProjectJumpHistory.InsertSmart Adding: Index=',Index,
      //  ' New=',APosition.CaretXY.X,',',APosition.CaretXY.Y,' ',APosition.Filename,
      //  ' ');
      Insert(Index,APosition);
      NewIndex:=IndexOf(APosition);
    end;
    if (HistoryIndex<0) or (HistoryIndex=NewIndex-1) then
      HistoryIndex:=NewIndex;
    //writeln('  HistoryIndex=',HistoryIndex);
  end else begin
    APosition.Free;
  end;
end;

procedure TProjectJumpHistory.DeleteForwardHistory;
var i, d: integer;
begin
  d:=FHistoryIndex+1;
  if d<0 then d:=0;
  for i:=Count-1 downto d do Delete(i);
end;

procedure TProjectJumpHistory.WriteDebugReport;
var i: integer;
begin
  DebugLn('[TProjectJumpHistory.WriteDebugReport] Count=',IntToStr(Count),
    ' MaxCount=',IntToStr(MaxCount),' HistoryIndex=',IntToStr(HistoryIndex));
  for i:=0 to Count-1 do begin
    DebugLn('  ',IntToStr(i),': Line=',IntToStr(Items[i].CaretXY.Y),
      ' Col=',IntToStr(Items[i].CaretXY.X), ' "',Items[i].Filename,'"');
  end;
end;

{ TPublishProjectOptions }

procedure TPublishProjectOptions.SetSaveClosedEditorFilesInfo(
  const AValue: boolean);
begin
  if FSaveClosedEditorFilesInfo=AValue then exit;
  FSaveClosedEditorFilesInfo:=AValue;
end;

procedure TPublishProjectOptions.SetSaveEditorInfoOfNonProjectFiles(
  const AValue: boolean);
begin
  if FSaveEditorInfoOfNonProjectFiles=AValue then exit;
  FSaveEditorInfoOfNonProjectFiles:=AValue;
end;

procedure TPublishProjectOptions.LoadDefaults;
begin
  inherited LoadDefaults;
  DestinationDirectory:='$(TestDir)/publishedproject/';
  CommandAfter:='';
  UseIncludeFileFilter:=true;
  IncludeFilterSimpleSyntax:=true;
  IncludeFileFilter:=DefPublProjIncFilter;
  UseExcludeFileFilter:=false;
  ExcludeFilterSimpleSyntax:=true;
  ExcludeFileFilter:=DefPublProjExcFilter;
  SaveClosedEditorFilesInfo:=false;
  SaveEditorInfoOfNonProjectFiles:=false;
end;

procedure TPublishProjectOptions.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const APath: string; AdjustPathDelims: boolean);
//var
//  XMLVersion: integer;
begin
  inherited LoadFromXMLConfig(XMLConfig,APath,AdjustPathDelims);
  //XMLVersion:=XMLConfig.GetValue(APath+'Version/Value',0);
  FSaveClosedEditorFilesInfo:=XMLConfig.GetValue(
             APath+'SaveClosedEditorFilesInfo/Value',SaveClosedEditorFilesInfo);
  FSaveEditorInfoOfNonProjectFiles:=XMLConfig.GetValue(
             APath+'SaveEditorInfoOfNonProjectFiles/Value',
             SaveEditorInfoOfNonProjectFiles);
end;

procedure TPublishProjectOptions.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const APath: string);
begin
  inherited SaveToXMLConfig(XMLConfig,APath);
  XMLConfig.SetDeleteValue(APath+'SaveClosedEditorFilesInfo/Value',
    SaveClosedEditorFilesInfo,false);
  XMLConfig.SetDeleteValue(APath+'SaveEditorInfoOfNonProjectFiles/Value',
    SaveEditorInfoOfNonProjectFiles,false);
end;

function TPublishProjectOptions.WriteFlags: TProjectWriteFlags;
begin
  Result:=[];
  if not SaveEditorInfoOfNonProjectFiles then
    Include(Result,pwfSaveOnlyProjectUnits);
  if not SaveClosedEditorFilesInfo then
    Include(Result,pwfDontSaveClosedUnits);
end;


{ TFileBookmark }

constructor TFileBookmark.Create;
begin

end;

constructor TFileBookmark.Create(NewX, NewY, AnID: integer);
begin
  fCursorPos.X:=NewX;
  fCursorPos.Y:=NewY;
  fID:=AnID;
end;

procedure TFileBookmark.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetValue(Path+'X',fCursorPos.X);
  XMLConfig.SetValue(Path+'Y',fCursorPos.Y);
  XMLConfig.SetValue(Path+'ID',fID);
end;

procedure TFileBookmark.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  fCursorPos.X:=XMLConfig.GetValue(Path+'X',1);
  fCursorPos.Y:=XMLConfig.GetValue(Path+'Y',1);
  fID:=XMLConfig.GetValue(Path+'ID',0);
end;

function TFileBookmark.X: integer;
begin
  Result:=fCursorPos.X;
end;

function TFileBookmark.Y: integer;
begin
  Result:=fCursorPos.Y;
end;

{ TFileBookmarks }

function TFileBookmarks.GetBookmarks(Index: integer): TFileBookmark;
begin
  Result:=TFileBookmark(FBookmarks[Index]);
end;

procedure TFileBookmarks.SetBookmarks(Index: integer; ABookmark: TFileBookmark
  );
begin
  FBookmarks[Index]:=ABookmark;
end;

constructor TFileBookmarks.Create;
begin
  FBookmarks:=TList.Create;
  Clear;
end;

destructor TFileBookmarks.Destroy;
begin
  Clear;
  FBookmarks.Free;
  inherited Destroy;
end;

function TFileBookmarks.Count: integer;
begin
  Result:=FBookmarks.Count;
end;

procedure TFileBookmarks.Delete(Index: integer);
begin
  Items[Index].Free;
  FBookmarks.Delete(Index);
end;

procedure TFileBookmarks.Clear;
var
  i: Integer;
begin
  for i:=0 to FBookmarks.Count-1 do Items[i].Free;
  FBookmarks.Clear;
end;

function TFileBookmarks.Add(ABookmark: TFileBookmark): integer;
var
  i: Integer;
begin
  i:=IndexOfID(ABookmark.ID);
  if i>=0 then Delete(i);
  Result:=FBookmarks.Add(ABookmark);
end;

function TFileBookmarks.Add(X, Y, ID: integer): integer;
begin
  Result:=Add(TFileBookmark.Create(X,Y,ID));
end;

function TFileBookmarks.IndexOfID(ID: integer): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].ID<>ID) do dec(Result);
end;

procedure TFileBookmarks.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  i: Integer;
begin
  XMLConfig.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
end;

procedure TFileBookmarks.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
var
  NewCount: Integer;
  NewBookmark: TFileBookmark;
  i: Integer;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewBookmark:=TFileBookmark.Create;
    NewBookmark.LoadFromXMLConfig(XMLConfig,Path+'Item'+IntToStr(i)+'/');
    Add(NewBookmark);
  end;
end;

{ TLazProjectFileDescriptors }

procedure TLazProjectFileDescriptors.SetDefaultPascalFileExt(
  const AValue: string);
begin
  if FDefaultPascalFileExt=AValue then exit;
  FDefaultPascalFileExt:=AValue;
  UpdateDefaultPascalFileExtensions;
end;

function TLazProjectFileDescriptors.GetItems(Index: integer): TProjectFileDescriptor;
begin
  Result:=TProjectFileDescriptor(FItems[Index]);
end;

constructor TLazProjectFileDescriptors.Create;
begin
  ProjectFileDescriptors:=Self;
  FItems:=TList.Create;
end;

destructor TLazProjectFileDescriptors.Destroy;
var
  i: Integer;
begin
  fDestroying:=true;
  for i:=Count-1 downto 0 do Items[i].Release;
  FItems.Free;
  FItems:=nil;
  ProjectFileDescriptors:=nil;
  inherited Destroy;
end;

function TLazProjectFileDescriptors.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazProjectFileDescriptors.GetUniqueName(const Name: string): string;
var
  i: Integer;
begin
  Result:=Name;
  if IndexOf(Result)<0 then exit;
  i:=0;
  repeat
    inc(i);
    Result:=Name+IntToStr(i);
  until IndexOf(Result)<0;
end;

function TLazProjectFileDescriptors.IndexOf(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Name,Items[Result].Name)<>0) do
    dec(Result);
end;

function TLazProjectFileDescriptors.IndexOf(
  FileDescriptor: TProjectFileDescriptor): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>FileDescriptor) do
    dec(Result);
end;

function TLazProjectFileDescriptors.FindByName(const Name: string
  ): TProjectFileDescriptor;
var
  i: LongInt;
begin
  i:=IndexOf(Name);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TLazProjectFileDescriptors.RegisterFileDescriptor(
  FileDescriptor: TProjectFileDescriptor);
var
  DefPasExt: String;
begin
  if FileDescriptor.Name='' then
    raise Exception.Create('TLazProjectFileDescriptors.RegisterFileDescriptor FileDescriptor.Name empty');
  if FileDescriptor.DefaultFilename='' then
    raise Exception.Create('TLazProjectFileDescriptors.RegisterFileDescriptor FileDescriptor.DefaultFilename empty');
  if IndexOf(FileDescriptor)>=0 then
    raise Exception.Create('TLazProjectFileDescriptors.RegisterFileDescriptor FileDescriptor already registered');
  // make name unique
  FileDescriptor.Name:=GetUniqueName(FileDescriptor.Name);
  // override pascal extension with users choice
  DefPasExt:=DefaultPascalFileExt;
  if DefPasExt<>'' then
    FileDescriptor.UpdateDefaultPascalFileExtension(DefPasExt);
  FItems.Add(FileDescriptor);
  
  // register ResourceClass, so that the IDE knows, what means
  // '= class(<ResourceClass.ClassName>)'
  if FileDescriptor.ResourceClass<>nil then
    RegisterClass(FileDescriptor.ResourceClass);
end;

procedure TLazProjectFileDescriptors.UnregisterFileDescriptor(
  FileDescriptor: TProjectFileDescriptor);
var
  i: LongInt;
begin
  if fDestroying then exit;
  i:=FItems.IndexOf(FileDescriptor);
  if i<0 then
    raise Exception.Create('TLazProjectFileDescriptors.UnregisterFileDescriptor');
  FItems.Delete(i);
  FileDescriptor.Release;
end;

procedure TLazProjectFileDescriptors.UpdateDefaultPascalFileExtensions;
var
  i: Integer;
  DefPasExt: String;
begin
  DefPasExt:=DefaultPascalFileExt;
  if DefPasExt='' then exit;
  for i:=0 to Count-1 do Items[i].UpdateDefaultPascalFileExtension(DefPasExt);
end;

{ TLazProjectDescriptors }

function TLazProjectDescriptors.GetItems(Index: integer): TProjectDescriptor;
begin
  Result:=TProjectDescriptor(FItems[Index]);
end;

constructor TLazProjectDescriptors.Create;
var
  EmptyProjectDesc: TProjectDescriptor;
begin
  ProjectDescriptors:=Self;
  FItems:=TList.Create;
  EmptyProjectDesc:=TProjectDescriptor.Create;
  EmptyProjectDesc.Name:='Empty';
  EmptyProjectDesc.VisibleInNewDialog:=false;
  RegisterDescriptor(EmptyProjectDesc);
  //DebugLn('TLazProjectDescriptors.Create ',dbgs(EmptyProjectDesc.VisibleInNewDialog));
end;

destructor TLazProjectDescriptors.Destroy;
var
  i: Integer;
begin
  fDestroying:=true;
  for i:=Count-1 downto 0 do Items[i].Release;
  FItems.Free;
  FItems:=nil;
  ProjectDescriptors:=nil;
  inherited Destroy;
end;

function TLazProjectDescriptors.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazProjectDescriptors.GetUniqueName(const Name: string): string;
var
  i: Integer;
begin
  Result:=Name;
  if IndexOf(Result)<0 then exit;
  i:=0;
  repeat
    inc(i);
    Result:=Name+IntToStr(i);
  until IndexOf(Result)<0;
end;

function TLazProjectDescriptors.IndexOf(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (AnsiCompareText(Name,Items[Result].Name)<>0) do
    dec(Result);
end;

function TLazProjectDescriptors.IndexOf(Descriptor: TProjectDescriptor
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result]<>Descriptor) do
    dec(Result);
end;

function TLazProjectDescriptors.FindByName(const Name: string
  ): TProjectDescriptor;
var
  i: LongInt;
begin
  i:=IndexOf(Name);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TLazProjectDescriptors.RegisterDescriptor(
  Descriptor: TProjectDescriptor);
begin
  if Descriptor.Name='' then
    raise Exception.Create('TLazProjectDescriptors.RegisterDescriptor Descriptor.Name empty');
  if IndexOf(Descriptor)>=0 then
    raise Exception.Create('TLazProjectDescriptors.RegisterDescriptor Descriptor already registered');
  Descriptor.Name:=GetUniqueName(Descriptor.Name);
  FItems.Add(Descriptor);
  if Descriptor.VisibleInNewDialog then
    ;
end;

procedure TLazProjectDescriptors.UnregisterDescriptor(
  Descriptor: TProjectDescriptor);
var
  i: LongInt;
begin
  if fDestroying then exit;
  i:=FItems.IndexOf(Descriptor);
  if i<0 then
    raise Exception.Create('TLazProjectDescriptors.UnregisterDescriptor');
  FItems.Delete(i);
  Descriptor.Release;
end;

initialization
  LazProjectFileDescriptors:=nil;
  
end.

