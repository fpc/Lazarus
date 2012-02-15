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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Scanning FPC sources in background.

}
unit FPCSrcScan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, DefineTemplates, CodeToolManager,
  LazarusIDEStrConsts, ProgressWnd, BaseBuildManager;

type
  TFPCSrcScans = class;

  { TFPCSrcScan }

  TFPCSrcScan = class(TThread)
  protected
    fLogMsg: string;
    Files: TStringList;
    procedure Execute; override;
    procedure OnFilesGathered; // main thread, called after thread has collected Files
    procedure MainThreadLog;
    procedure Log(Msg: string);
  public
    Directory: string;
    Scans: TFPCSrcScans;
    ProgressItem: TIDEProgressItem;
  end;

  { TFPCSrcScans }

  TFPCSrcScans = class(TComponent)
  private
    fItems: TFPList;
    FCritSec: TRTLCriticalSection;
    function GetItems(Index: integer): TFPCSrcScan;
    procedure Remove(Item: TFPCSrcScan);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Count: integer; // requires Enter/Leave
    property Items[Index: integer]: TFPCSrcScan read GetItems; default; // requires Enter/Leave
    procedure Clear; // waits until all
    procedure EnterCriticalsection;
    procedure LeaveCriticalsection;
    procedure Scan(Directory: string);
  end;

implementation

procedure ApplyFPCSrcFiles(FPCSrcDir: string; var Files: TStringList);
var
  SrcCache: TFPCSourceCache;
begin
  debugln(['ApplyFPCSrcFiles ',FPCSrcDir,' FileCount=',Files.Count]);
  // copy Files to codetools cache
  if CodeToolBoss<>nil then
  begin
    SrcCache:=CodeToolBoss.FPCDefinesCache.SourceCaches.Find(FPCSrcDir,true);
    debugln(['ApplyFPCSrcFiles SrcCache.Update ...']);
    SrcCache.Update(Files);

    debugln(['ApplyFPCSrcFiles BuildBoss.RescanCompilerDefines ...']);
    if BuildBoss<>nil then
      BuildBoss.RescanCompilerDefines(false,false,false,true);
  end;
  FreeAndNil(Files);
end;

{ TFPCSrcScan }

procedure TFPCSrcScan.Execute;
begin
  try
    Log('TFPCSrcScan.Execute START '+Directory);
    // scan fpc source directory, check for terminated
    Files:=GatherFilesInFPCSources(Directory,nil);
    Log('TFPCSrcScan.Execute found some files: '+dbgs((Files<>nil) and (Files.Count>0)));
  except
    on E: Exception do begin
      Log('TFPCSrcScan.Execute error: '+E.Message);
    end;
  end;
  if Files=nil then
    Files:=TStringList.Create;
  // let main thread update the codetools fpc source cache
  Synchronize(@OnFilesGathered);
end;

procedure TFPCSrcScan.OnFilesGathered;
begin
  try
    ApplyFPCSrcFiles(Directory,Files);
    // delete item in progress window
    debugln(['TFPCSrcScan.OnFilesGathered closing progress item ...']);
    FreeAndNil(ProgressItem);
    Scans.Remove(Self);
    debugln(['TFPCSrcScan.OnFilesGathered END']);
  except
    on E: Exception do
      debugln(['TFPCSrcScan.OnFilesGathered ERROR: ',E.Message]);
  end;
end;

procedure TFPCSrcScan.MainThreadLog;
begin
  debugln(fLogMsg);
end;

procedure TFPCSrcScan.Log(Msg: string);
begin
  fLogMsg:=Msg;
  Synchronize(@MainThreadLog);
end;

{ TFPCSrcScans }

function TFPCSrcScans.GetItems(Index: integer): TFPCSrcScan;
begin
  Result:=TFPCSrcScan(fItems[Index]);
end;

procedure TFPCSrcScans.Remove(Item: TFPCSrcScan);
begin
  EnterCriticalsection;
  try
    fItems.Remove(Item);
  finally
    LeaveCriticalsection;
  end;
end;

constructor TFPCSrcScans.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fItems:=TFPList.Create;
  InitCriticalSection(FCritSec);
end;

destructor TFPCSrcScans.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  DoneCriticalsection(FCritSec);
  inherited Destroy;
end;

function TFPCSrcScans.Count: integer;
begin
  Result:=fItems.Count;
end;

procedure TFPCSrcScans.Clear;
var
  i: Integer;
begin
  // terminate all threads
  EnterCriticalsection;
  try
    for i:=0 to Count-1 do
      Items[i].Terminate;
  finally
    LeaveCriticalsection;
  end;
  repeat
    EnterCriticalsection;
    try
      if Count=0 then break;
    finally
      LeaveCriticalsection;
    end;
    Sleep(10);
  until false;
end;

procedure TFPCSrcScans.EnterCriticalsection;
begin
  System.EnterCriticalsection(FCritSec);
end;

procedure TFPCSrcScans.LeaveCriticalsection;
begin
  System.LeaveCriticalsection(FCritSec);
end;

procedure TFPCSrcScans.Scan(Directory: string);
var
{$IFDEF DisableMultiThreading}
  Files: TStringList;
{$ELSE}
  i: Integer;
  Item: TFPCSrcScan;
{$ENDIF}
begin
  {$IFDEF DisableMultiThreading}
  // scan fpc source directory, check for terminated
  Files:=GatherFilesInFPCSources(Directory,nil);
  if Files=nil then
    Files:=TStringList.Create;
  ApplyFPCSrcFiles(Directory,Files);
  {$ELSE}
    EnterCriticalsection;
    try
      // check if already scanning that directory
      for i:=0 to Count-1 do
        if CompareFilenames(Directory,Items[i].Directory)=0 then exit;
      // create thread and create progress window
      Item:=TFPCSrcScan.Create(true);
      Item.FreeOnTerminate:=true;
      Item.Scans:=Self;
      Item.Directory:=Directory;
      fItems.Add(Item);
    finally
      LeaveCriticalsection;
    end;
    Item.ProgressItem:=CreateProgressItem('FPCSrcScan',
      Format(lisCreatingFileIndexOfFPCSources, [Directory]),
      lisTheFileIndexIsNeededForFunctionsLikeFindDeclaratio);
    {$IF defined(VER2_4_2) or defined(VER2_4_3)}
    Item.Resume;
    {$ELSE}
    Item.Start;
    {$ENDIF}
  {$ENDIF}
end;

end.

