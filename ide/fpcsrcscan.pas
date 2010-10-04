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
    Files: TStringList;
    procedure Execute; override;
    procedure OnFilesGathered; // main thread, called after thread has collected Files
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

{ TFPCSrcScan }

procedure TFPCSrcScan.Execute;
begin
  try
    // scan fpc source directory, check for terminated
    Files:=GatherFilesInFPCSources(Directory,nil);
    //debugln(['TFPCSrcScan.Execute ',Files<>nil]);
    // let main thread update the codetools fpc source cache
    Synchronize(@OnFilesGathered);
  except
    on E: Exception do begin
      debugln(['TFPCSrcScan.Execute error: ',E.Message]);
    end;
  end;
end;

procedure TFPCSrcScan.OnFilesGathered;
var
  SrcCache: TFPCSourceCache;
begin
  //debugln(['TFPCSrcScan.OnFilesGathered ',Directory,' FileCount=',Files.Count]);
  // copy Files to codetools cache
  if CodeToolBoss<>nil then
  begin
    SrcCache:=CodeToolBoss.FPCDefinesCache.SourceCaches.Find(Directory,true);
    SrcCache.Update(Files);

    //debugln(['TFPCSrcScan.OnFilesGathered BuildBoss.RescanCompilerDefines ...']);
    if BuildBoss<>nil then
      BuildBoss.RescanCompilerDefines(false,false,false);
  end;
  FreeAndNil(Files);
  // delete item in progress window
  //debugln(['TFPCSrcScan.OnFilesGathered closing progress item ...']);
  FreeAndNil(ProgressItem);
  Scans.Remove(Self);
  //debugln(['TFPCSrcScan.OnFilesGathered END']);
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
  i: Integer;
  Item: TFPCSrcScan;
begin
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
  Item.Resume;
end;

end.

