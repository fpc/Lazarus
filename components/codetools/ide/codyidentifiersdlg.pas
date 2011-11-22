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
    Dictionary of identifiers.
    Dialog to view and search the whole list.

  ToDo:
    -quickfix for identifier not found
      -show dialog
      -list with units and packages
      -show hint about full unit file name and package file name
      -check if unit still exists
      -check if identifier still exists
      -check if package still exist
      -check if unit conflicts with another unit in path
      -buttons: add unit to interface, add unit to implementation
      -add unit to uses
      -add dependency to owner
    -clean up old entries
      -When, How?
    -gzip? lot of cpu, may be faster on first load
}
unit CodyIdentifiersDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LResources, LCLProc, avl_tree, Forms, Controls,
  Graphics, Dialogs, ButtonPanel, StdCtrls, ExtCtrls,
  PackageIntf, LazIDEIntf,
  BasicCodeTools, CustomCodeTool, CodeToolManager, UnitDictionary,
  CodyStrConsts;

type
  TCodyUnitDictionary = class;

  { TCodyUDLoadSaveThread }

  TCodyUDLoadSaveThread = class(TThread)
  public
    Load: boolean;
    Dictionary: TCodyUnitDictionary;
    Filename: string;
    Done: boolean;
    procedure Execute; override;
  end;

  { TCodyUnitDictionary }

  TCodyUnitDictionary = class(TUnitDictionary)
  private
    FLoadAfterStartInS: integer;
    FLoadSaveError: string;
    FSaveIntervalInS: integer;
    fTimer: TTimer;
    FIdleConnected: boolean;
    fQueuedTools: TAVLTree; // tree of TCustomCodeTool
    fParsingTool: TCustomCodeTool;
    fLoadSaveThread: TCodyUDLoadSaveThread;
    fCritSec: TRTLCriticalSection;
    fLoaded: boolean; // has loaded the file
    fStartTime: TDateTime;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetLoadAfterStartInS(AValue: integer);
    procedure SetLoadSaveError(AValue: string);
    procedure SetSaveIntervalInS(AValue: integer);
    procedure ToolTreeChanged(Tool: TCustomCodeTool; {%H-}NodesDeleting: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WaitForThread;
    procedure OnTimer(Sender: TObject);
    function StartLoadSaveThread: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Loaded: boolean read fLoaded;
    function GetFilename: string;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property SaveIntervalInS: integer read FSaveIntervalInS write SetSaveIntervalInS;
    property LoadAfterStartInS: integer read FLoadAfterStartInS write SetLoadAfterStartInS;
    procedure BeginCritSec;
    procedure EndCritSec;
    property LoadSaveError: string read FLoadSaveError write SetLoadSaveError;
  end;

  { TCodyIdentifiersDlg }

  TCodyIdentifiersDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    FilterEdit: TEdit;
    InfoLabel: TLabel;
    ItemsListBox: TListBox;
    procedure FileLabelClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
  private
    FLastFilter: string;
    FIdleConnected: boolean;
    FMaxItems: integer;
    FNoFilterText: string;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetMaxItems(AValue: integer);
    procedure UpdateItemsList;
    procedure UpdateInfo;
    function GetFilterEditText: string;
  public
    procedure Init;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property MaxItems: integer read FMaxItems write SetMaxItems;
  end;

var
  CodyUnitDictionary: TCodyUnitDictionary = nil;

procedure ShowUnitDictionaryDialog(Sender: TObject);
procedure InitUnitDictionary;

implementation

{$R *.lfm}

procedure ShowUnitDictionaryDialog(Sender: TObject);
var
  CodyIdentifiersDlg: TCodyIdentifiersDlg;
begin
  CodyIdentifiersDlg:=TCodyIdentifiersDlg.Create(nil);
  try
    CodyIdentifiersDlg.Init;
    CodyIdentifiersDlg.ShowModal;
  finally
    CodyIdentifiersDlg.Free;
  end;
end;

procedure InitUnitDictionary;
begin
  CodyUnitDictionary:=TCodyUnitDictionary.Create;
end;

{ TCodyUDLoadSaveThread }

procedure TCodyUDLoadSaveThread.Execute;
var
  UncompressedMS: TMemoryStream;
  TempFilename: String;
begin
  Dictionary.LoadSaveError:='';
  FreeOnTerminate:=true;
  try
    if Load then begin
      // load
      //debugln('TCodyUDLoadSaveThread.Execute loading '+Filename+' exists='+dbgs(FileExistsUTF8(Filename)));
      if FileExistsUTF8(Filename) then begin
        UncompressedMS:=TMemoryStream.Create;
        try
          UncompressedMS.LoadFromFile(Filename);
          UncompressedMS.Position:=0;
          Dictionary.BeginCritSec;
          try
            // Note: if loading fails, then the format or read permissions are wrong
            // mark as loaded, so that the next save will create a valid one
            Dictionary.fLoaded:=true;
            Dictionary.LoadFromStream(UncompressedMS,true);
          finally
            Dictionary.EndCritSec;
          end;
        finally
          UncompressedMS.Free;
        end;
      end;
    end else begin
      // save
      //debugln('TCodyUDLoadSaveThread.Execute saving '+Filename);
      UncompressedMS:=TMemoryStream.Create;
      try
        Dictionary.BeginCritSec;
        try
          Dictionary.SaveToStream(UncompressedMS);
        finally
          Dictionary.EndCritSec;
        end;
        UncompressedMS.Position:=0;
        // reduce the risk of file corruption due to crashes while saving:
        // save to a temporary file and then rename
        TempFilename:=FileProcs.GetTempFilename(Filename,'unitdictionary');
        UncompressedMS.SaveToFile(TempFilename);
        if not RenameFileUTF8(TempFilename,Filename) then
          raise Exception.Create('unable to rename "'+TempFilename+'" to "'+Filename+'"');
      finally
        UncompressedMS.Free;
      end;
    end;
  except
    on E: Exception do begin
      debugln('TCodyUDLoadSaveThread.Execute '+E.Message);
      Dictionary.LoadSaveError:=E.Message;
    end;
  end;
  Done:=true;
  Dictionary.BeginCritSec;
  try
    Dictionary.fLoadSaveThread:=nil;
  finally
    Dictionary.EndCritSec;
  end;
  WakeMainThread(nil);
  //debugln('TCodyUDLoadSaveThread.Execute END');
end;

{ TCodyUnitDictionary }

procedure TCodyUnitDictionary.ToolTreeChanged(Tool: TCustomCodeTool;
  NodesDeleting: boolean);
begin
  if fParsingTool=Tool then exit;
  //debugln(['TCodyUnitDictionary.ToolTreeChanged ',Tool.MainFilename]);
  if fQueuedTools.Find(Tool)<>nil then exit;
  fQueuedTools.Add(Tool);
  IdleConnected:=true;
end;

procedure TCodyUnitDictionary.OnIdle(Sender: TObject; var Done: Boolean);
var
  OwnerList: TFPList;
  i: Integer;
  Pkg: TIDEPackage;
  UDUnit: TUDUnit;
  UDGroup: TUDUnitGroup;
  ok: Boolean;
  OldChangeStamp: Int64;
begin
  // check without critical section if currently loading/saving
  if fLoadSaveThread<>nil then
    exit;

  if fQueuedTools.Root<>nil then begin
    fParsingTool:=TCustomCodeTool(fQueuedTools.Root.Data);
    fQueuedTools.Delete(fQueuedTools.Root);
    //debugln(['TCodyUnitDictionary.OnIdle parsing ',fParsingTool.MainFilename]);
    OwnerList:=nil;
    try
      ok:=false;
      OldChangeStamp:=ChangeStamp;
      try
        BeginCritSec;
        try
          UDUnit:=ParseUnit(fParsingTool.MainFilename);
        finally
          EndCritSec;
        end;
        ok:=true;
      except
        // parse error
      end;
      if Ok then begin
        OwnerList:=PackageEditingInterface.GetPossibleOwnersOfUnit(
          fParsingTool.MainFilename,[piosfIncludeSourceDirectories]);
        if (OwnerList<>nil) then begin
          BeginCritSec;
          try
            for i:=0 to OwnerList.Count-1 do begin
              if TObject(OwnerList[i]) is TIDEPackage then begin
                Pkg:=TIDEPackage(OwnerList[i]);
                if Pkg.IsVirtual then continue;
                UDGroup:=AddUnitGroup(Pkg.Filename,Pkg.Name);
                //debugln(['TCodyUnitDictionary.OnIdle Pkg=',Pkg.Filename]);
                UDGroup.AddUnit(UDUnit);
              end;
            end;
          finally
            EndCritSec;
          end;
        end;
        if ChangeStamp<>OldChangeStamp then begin
          if fTimer=nil then begin
            fTimer:=TTimer.Create(nil);
            fTimer.Interval:=SaveIntervalInS*1000;
            fTimer.OnTimer:=@OnTimer;
          end;
          //debugln(['TCodyUnitDictionary.OnIdle starting timer: ms=',fTimer.Interval]);
          fTimer.Enabled:=true;
        end;
      end;
    finally
      fParsingTool:=nil;
      OwnerList.Free;
    end;
  end else begin
    // nothing to do, maybe it's time to load the database
    if fStartTime=0 then
      fStartTime:=Now
    else if (fLoadSaveThread=nil) and (not fLoaded)
    and (Abs(Now-fStartTime)*86400>=LoadAfterStartInS) then
      StartLoadSaveThread;
  end;
  Done:=fQueuedTools.Count=0;
  if Done then
    IdleConnected:=false;
end;

procedure TCodyUnitDictionary.WaitForThread;
begin
  repeat
    BeginCritSec;
    try
      if fLoadSaveThread=nil then exit;
    finally
      EndCritSec;
    end;
    Sleep(10);
  until false;
end;

procedure TCodyUnitDictionary.OnTimer(Sender: TObject);
begin
  if StartLoadSaveThread then
    fTimer.Enabled:=false;
end;

function TCodyUnitDictionary.GetFilename: string;
begin
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'codyunitdictionary.txt';
end;

function TCodyUnitDictionary.StartLoadSaveThread: boolean;
begin
  Result:=false;
  if (Application=nil) or (CodyUnitDictionary=nil) then exit;
  //debugln(['TCodyUnitDictionary.StartLoadSaveThread ',fLoadSaveThread<>nil]);
  BeginCritSec;
  try
    if fLoadSaveThread<>nil then exit;
  finally
    EndCritSec;
  end;
  Result:=true;
  fLoadSaveThread:=TCodyUDLoadSaveThread.Create(true);
  fLoadSaveThread.Load:=not fLoaded;
  fLoadSaveThread.Dictionary:=Self;
  fLoadSaveThread.Filename:=GetFilename;
  fLoadSaveThread.Start;
end;

procedure TCodyUnitDictionary.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if Application=nil then exit;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCodyUnitDictionary.SetLoadAfterStartInS(AValue: integer);
begin
  if FLoadAfterStartInS=AValue then Exit;
  FLoadAfterStartInS:=AValue;
end;

procedure TCodyUnitDictionary.SetLoadSaveError(AValue: string);
begin
  BeginCritSec;
  try
    FLoadSaveError:=AValue;
  finally
    EndCritSec;
  end;
end;

procedure TCodyUnitDictionary.SetSaveIntervalInS(AValue: integer);
begin
  if FSaveIntervalInS=AValue then Exit;
  FSaveIntervalInS:=AValue;
  if fTimer<>nil then
    fTimer.Interval:=SaveIntervalInS;
end;

constructor TCodyUnitDictionary.Create;
begin
  inherited Create;
  FSaveIntervalInS:=60*3; // every 3 minutes
  FLoadAfterStartInS:=3;
  InitCriticalSection(fCritSec);
  fQueuedTools:=TAVLTree.Create;
  CodeToolBoss.AddHandlerToolTreeChanging(@ToolTreeChanged);
end;

destructor TCodyUnitDictionary.Destroy;
begin
  CodeToolBoss.RemoveHandlerToolTreeChanging(@ToolTreeChanged);
  FreeAndNil(fTimer);
  WaitForThread;
  IdleConnected:=false;
  FreeAndNil(fQueuedTools);
  DoneCriticalsection(fCritSec);
  inherited Destroy;
end;

procedure TCodyUnitDictionary.Load;
begin
  if fLoaded then exit;
  WaitForThread;
  if fLoaded then exit;
  StartLoadSaveThread;
  WaitForThread;
end;

procedure TCodyUnitDictionary.Save;
begin
  WaitForThread;
  fLoaded:=true;
  StartLoadSaveThread;
  WaitForThread;
end;

procedure TCodyUnitDictionary.BeginCritSec;
begin
  EnterCriticalsection(fCritSec);
end;

procedure TCodyUnitDictionary.EndCritSec;
begin
  LeaveCriticalsection(fCritSec);
end;

{ TCodyIdentifiersDlg }

procedure TCodyIdentifiersDlg.FilterEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TCodyIdentifiersDlg.FileLabelClick(Sender: TObject);
begin

end;

procedure TCodyIdentifiersDlg.FilterEditExit(Sender: TObject);
begin
  if GetFilterEditText='' then
    FilterEdit.Text:=FNoFilterText;
end;

procedure TCodyIdentifiersDlg.FormCreate(Sender: TObject);
begin
  Caption:=crsCodyIdentifierDictionary;
  FMaxItems:=20;
  FNoFilterText:=crsFilter;
end;

procedure TCodyIdentifiersDlg.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if not CodyUnitDictionary.Loaded then begin
    CodyUnitDictionary.Load;
    UpdateInfo;
  end;
  if FLastFilter<>GetFilterEditText then
    UpdateItemsList;
  IdleConnected:=false;
end;

procedure TCodyIdentifiersDlg.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if Application=nil then exit;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCodyIdentifiersDlg.SetMaxItems(AValue: integer);
begin
  if FMaxItems=AValue then Exit;
  FMaxItems:=AValue;
  UpdateItemsList;
end;

procedure TCodyIdentifiersDlg.UpdateItemsList;
var
  Filter: String;
  Node: TAVLTreeNode;
  FilterP: PChar;
  sl: TStringList;
  Item: TUDIdentifier;
  s: String;
  Found: Integer;
  GroupNode: TAVLTreeNode;
  Group: TUDUnitGroup;
begin
  Filter:=GetFilterEditText;
  FilterP:=PChar(Filter);
  sl:=TStringList.Create;
  try
    Found:=0;
    Node:=CodyUnitDictionary.Identifiers.FindLowest;
    //debugln(['TCodyIdentifiersDlg.UpdateItemsList Filter="',Filter,'"']);
    while Node<>nil do begin
      if ComparePrefixIdent(FilterP,PChar(Pointer(TUDIdentifier(Node.Data).Name)))
      then begin
        inc(Found);
        if Found<MaxItems then begin
          Item:=TUDIdentifier(Node.Data);
          GroupNode:=Item.DUnit.UnitGroups.FindLowest;
          while GroupNode<>nil do begin
            Group:=TUDUnitGroup(GroupNode.Data);
            s:=Item.Name+' in '+Item.DUnit.Name;
            if Group.Name<>'' then
              s:=s+' of '+Group.Name;
            GroupNode:=Item.DUnit.UnitGroups.FindSuccessor(GroupNode);
          end;
          sl.Add(s);
        end;
      end;
      Node:=CodyUnitDictionary.Identifiers.FindSuccessor(Node);
    end;

    if Found>sl.Count then
      sl.Add(Format(crsAndMoreIdentifiers, [IntToStr(Found-sl.Count)]));

    ItemsListBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TCodyIdentifiersDlg.UpdateInfo;
var
  s: String;
begin
  s:=Format(crsPackagesUnitsIdentifiersFile,
    [IntToStr(CodyUnitDictionary.UnitGroupsByFilename.Count),
     IntToStr(CodyUnitDictionary.UnitsByFilename.Count),
     IntToStr(CodyUnitDictionary.Identifiers.Count),
     #13#10,
     CodyUnitDictionary.GetFilename]);
  if CodyUnitDictionary.LoadSaveError<>'' then
    s:=s+#13#10+Format(crsError, [CodyUnitDictionary.LoadSaveError]);
  InfoLabel.Caption:=s;
end;

function TCodyIdentifiersDlg.GetFilterEditText: string;
begin
  Result:=FilterEdit.Text;
  if Result=FNoFilterText then
    Result:='';
end;

procedure TCodyIdentifiersDlg.Init;
begin
  UpdateInfo;
  FilterEdit.Text:=FNoFilterText;
  FLastFilter:='...'; // force one update
  IdleConnected:=true;
end;

finalization
  FreeAndNil(CodyUnitDictionary);

end.

