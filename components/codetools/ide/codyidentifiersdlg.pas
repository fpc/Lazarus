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
    -use identifier: check package version
    -check for conflict: other unit with same name already in search path
    -check for conflict: other identifier in scope, use unitname.identifier
    -gzip? lot of cpu, may be faster on first load
}
unit CodyIdentifiersDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileProcs, LResources, LCLProc, avl_tree, contnrs, Forms,
  Controls, Graphics, Dialogs, ButtonPanel, StdCtrls, ExtCtrls, LCLType,
  Buttons, PackageIntf, LazIDEIntf, SrcEditorIntf, ProjectIntf, CompOptsIntf,
  IDEDialogs, CodeCache, BasicCodeTools, CustomCodeTool, CodeToolManager,
  UnitDictionary, CodeTree, LinkScanner, DefineTemplates, CodeToolsStructs,
  CodyStrConsts, CodyUtils, CodyOpts;

const
  PackageNameFPCSrcDir = 'FPCSrcDir';
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
    fClosing: boolean;
    fCheckFiles: TStringToStringTree;
    procedure CheckFiles;
    procedure SetIdleConnected(AValue: boolean);
    procedure SetLoadAfterStartInS(AValue: integer);
    procedure SetLoadSaveError(AValue: string);
    procedure SetSaveIntervalInS(AValue: integer);
    procedure ToolTreeChanged(Tool: TCustomCodeTool; {%H-}NodesDeleting: boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure WaitForThread;
    procedure OnTimer(Sender: TObject);
    function StartLoadSaveThread: boolean;
    procedure OnIDEClose(Sender: TObject);
    procedure OnApplyOptions(Sender: TObject);
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
    procedure CheckFileAsync(aFilename: string); // check eventually if file exists and delete unit/group
    property LoadSaveError: string read FLoadSaveError write SetLoadSaveError;
  end;

  TCodyIdentifierDlgAction = (
    cidaUseIdentifier,
    cidaJumpToIdentifier
    );

  TCodyIdentifierFilter = (
    cifStartsWith,
    cifContains
    );

  { TCodyIdentifier }

  TCodyIdentifier = class
  public
    Identifier: string;
    Unit_Name: string;
    UnitFile: string;
    GroupName: string;
    GroupFile: string;
    MatchExactly: boolean;
    DirectUnit: boolean; // belongs to same owner
    InUsedPackage: boolean;
    PathDistance: integer; // how far is UnitFile from the current unit
    constructor Create(const TheIdentifier, TheUnitName, TheUnitFile,
      ThePackageName, ThePackageFile: string; TheMatchExactly: boolean);
  end;

  { TCodyIdentifiersDlg }

  TCodyIdentifiersDlg = class(TForm)
    AddToImplementationUsesCheckBox: TCheckBox;
    ButtonPanel1: TButtonPanel;
    ContainsSpeedButton: TSpeedButton;
    FilterEdit: TEdit;
    HideOtherProjectsCheckBox: TCheckBox;
    InfoLabel: TLabel;
    ItemsListBox: TListBox;
    PackageLabel: TLabel;
    StartsSpeedButton: TSpeedButton;
    UnitLabel: TLabel;
    procedure ButtonPanel1HelpButtonClick(Sender: TObject);
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
    procedure ContainsSpeedButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditExit(Sender: TObject);
    procedure FilterEditKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure JumpButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HideOtherProjectsCheckBoxChange(Sender: TObject);
    procedure ItemsListBoxClick(Sender: TObject);
    procedure ItemsListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure StartsSpeedButtonClick(Sender: TObject);
  private
    FDlgAction: TCodyIdentifierDlgAction;
    FJumpButton: TBitBtn;
    FLastFilter: string;
    FLastHideOtherProjects: boolean;
    FIdleConnected: boolean;
    FMaxItems: integer;
    FNoFilterText: string;
    FItems: TObjectList; // list of TCodyIdentifier
    FLastFilterType: TCodyIdentifierFilter;
    procedure SetDlgAction(NewAction: TCodyIdentifierDlgAction);
    procedure SetIdleConnected(AValue: boolean);
    procedure SetMaxItems(AValue: integer);
    procedure UpdateGeneralInfo;
    procedure UpdateItemsList;
    procedure SortItems;
    procedure UpdateIdentifierInfo;
    function GetFilterEditText: string;
    function FindSelectedItem(out Identifier, UnitFilename,
      GroupName, GroupFilename: string): boolean;
    procedure UpdateCurOwnerOfUnit;
    procedure AddToUsesSection;
    procedure UpdateTool;
    function AddButton: TBitBtn;
    function GetCurOwnerCompilerOptions: TLazCompilerOptions;
  public
    CurIdentifier: string;
    CurIdentStart: integer; // column
    CurIdentEnd: integer; // column
    CurInitError: TCUParseError;
    CurTool: TCodeTool;
    CurCleanPos: integer;
    CurNode: TCodeTreeNode;
    CurCodePos: TCodeXYPosition;
    CurSrcEdit: TSourceEditorInterface;
    CurMainFilename: string; // if CurSrcEdit is an include file, then CurMainFilename<>CurSrcEdit.Filename
    CurMainCode: TCodeBuffer;
    CurInImplementation: Boolean;

    CurOwner: TObject; // only valid after UpdateCurOwnerOfUnit and till next event
    CurUnitPath: String; // depends on CurOwner

    NewIdentifier: string;
    NewUnitFilename: string;
    NewGroupName: string;
    NewGroupFilename: string;

    function Init: boolean;
    procedure UseIdentifier;
    procedure JumpToIdentifier;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property MaxItems: integer read FMaxItems write SetMaxItems;
    function OwnerToString(AnOwner: TObject): string;
    property DlgAction: TCodyIdentifierDlgAction read FDlgAction;
    function GetFilterType: TCodyIdentifierFilter;
  end;

var
  CodyUnitDictionary: TCodyUnitDictionary = nil;

procedure ShowUnitDictionaryDialog(Sender: TObject);
procedure InitUnitDictionary;

function CompareCodyIdentifiersAlphaScope(Item1, Item2: Pointer): integer;
function CompareCodyIdentifiersScopeAlpha(Item1, Item2: Pointer): integer;
function CompareCodyIdentifiersAlpha(Item1, Item2: Pointer): integer;
function CompareCodyIdentifiersScope(Item1, Item2: Pointer): integer;

implementation

{$R *.lfm}

procedure ShowUnitDictionaryDialog(Sender: TObject);
var
  CodyIdentifiersDlg: TCodyIdentifiersDlg;
begin
  CodyIdentifiersDlg:=TCodyIdentifiersDlg.Create(nil);
  try
    if not CodyIdentifiersDlg.Init then exit;
    if CodyIdentifiersDlg.ShowModal=mrOk then begin
      case CodyIdentifiersDlg.DlgAction of
      cidaUseIdentifier: CodyIdentifiersDlg.UseIdentifier;
      cidaJumpToIdentifier: CodyIdentifiersDlg.JumpToIdentifier;
      end;
    end;
  finally
    CodyIdentifiersDlg.Free;
  end;
end;

procedure InitUnitDictionary;
begin
  CodyUnitDictionary:=TCodyUnitDictionary.Create;
end;

function CompareCodyIdentifiersAlphaScope(Item1, Item2: Pointer): integer;
begin
  Result:=CompareCodyIdentifiersAlpha(Item1,Item2);
  if Result<>0 then exit;
  Result:=CompareCodyIdentifiersScope(Item1,Item2);
end;

function CompareCodyIdentifiersScopeAlpha(Item1, Item2: Pointer): integer;
begin
  Result:=CompareCodyIdentifiersScope(Item1,Item2);
  if Result<>0 then exit;
  Result:=CompareCodyIdentifiersAlpha(Item1,Item2);
end;

function CheckFlag(Flag1, Flag2: boolean; var r: integer): boolean;
begin
  if Flag1=Flag2 then exit(false);
  Result:=true;
  if Flag1 then r:=-1 else r:=1;
end;

function CompareCodyIdentifiersAlpha(Item1, Item2: Pointer): integer;
// positive is sorted on top
var
  i1: TCodyIdentifier absolute Item1;
  i2: TCodyIdentifier absolute Item2;
begin
  Result:=0;
  // an exact match is better
  if CheckFlag(i1.MatchExactly,i2.MatchExactly,Result) then exit;
  // otherwise alphabetically
  Result:=-CompareIdentifiers(PChar(i1.Identifier),PChar(i2.Identifier));
end;

function CompareCodyIdentifiersScope(Item1, Item2: Pointer): integer;
// positive is sorted on top
var
  i1: TCodyIdentifier absolute Item1;
  i2: TCodyIdentifier absolute Item2;
begin
  Result:=0;
  // an exact match is better
  if CheckFlag(i1.MatchExactly,i2.MatchExactly,Result) then exit;
  // an unit of the owner is better
  if CheckFlag(i1.DirectUnit,i2.DirectUnit,Result) then exit;
  // an unit in a used package is better
  if CheckFlag(i1.InUsedPackage,i2.InUsedPackage,Result) then exit;
  // a fpc unit is better
  if CheckFlag(i1.GroupName=PackageNameFPCSrcDir,i2.GroupName=PackageNameFPCSrcDir,Result) then exit;
  // a near directory is better
  Result:=i2.PathDistance-i1.PathDistance;
end;

{ TCodyIdentifier }

constructor TCodyIdentifier.Create(const TheIdentifier, TheUnitName,
  TheUnitFile, ThePackageName, ThePackageFile: string; TheMatchExactly: boolean
  );
begin
  Identifier:=TheIdentifier;
  Unit_Name:=TheUnitName;
  UnitFile:=TheUnitFile;
  GroupName:=ThePackageName;
  GroupFile:=ThePackageFile;
  MatchExactly:=TheMatchExactly;
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
      // Note: if loading fails, then the format or read permissions are wrong
      // mark as loaded, so that the next save will create a valid one
      Dictionary.fLoaded:=true;
      if FileExistsUTF8(Filename) then begin
        UncompressedMS:=TMemoryStream.Create;
        try
          UncompressedMS.LoadFromFile(Filename);
          UncompressedMS.Position:=0;
          Dictionary.BeginCritSec;
          try
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
          raise Exception.Create(Format(crsUnableToRenameTo, [TempFilename,
            Filename]));
      finally
        UncompressedMS.Free;
      end;
    end;
  except
    on E: Exception do begin
      debugln(['WARNING: TCodyUDLoadSaveThread.Execute Load=',Load,' ',E.Message]);
      Dictionary.LoadSaveError:=E.Message;
      DumpExceptionBackTrace;
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
  UnitSet: TFPCUnitSetCache;
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
      //ConsistencyCheck;
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
                //debugln(['TCodyUnitDictionary.OnIdle Pkg=',Pkg.Filename,' Name=',Pkg.Name]);
                if UDGroup=nil then begin
                  debugln(['ERROR: TCodyUnitDictionary.OnIdle unable to AddUnitGroup: File=',Pkg.Filename,' Name=',Pkg.Name]);
                  exit;
                end;
                UDGroup.AddUnit(UDUnit);
                //ConsistencyCheck;
              end;
            end;
          finally
            EndCritSec;
          end;
        end;

        // check if in FPC source directory
        UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
        if (UnitSet<>nil) and (UnitSet.FPCSourceDirectory<>'')
        and FileIsInPath(fParsingTool.MainFilename,UnitSet.FPCSourceDirectory)
        then begin
          BeginCritSec;
          try
            UDGroup:=AddUnitGroup(
              AppendPathDelim(UnitSet.FPCSourceDirectory)+PackageNameFPCSrcDir+'.lpk',
              PackageNameFPCSrcDir);
            UDGroup.AddUnit(UDUnit);
          finally
            EndCritSec;
          end;
        end;

        if ChangeStamp<>OldChangeStamp then begin
          if (fTimer=nil) and (not fClosing) then begin
            fTimer:=TTimer.Create(nil);
            fTimer.Interval:=SaveIntervalInS*1000;
            fTimer.OnTimer:=@OnTimer;
          end;
          if fTimer<>nil then
            fTimer.Enabled:=true;
        end;
      end;
    finally
      fParsingTool:=nil;
      OwnerList.Free;
    end;
  end else if fCheckFiles<>nil then begin
    CheckFiles;
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
    if fTimer<>nil then
      fTimer.Enabled:=false;
end;

function TCodyUnitDictionary.GetFilename: string;
begin
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+'codyunitdictionary.txt';
end;

function TCodyUnitDictionary.StartLoadSaveThread: boolean;
begin
  Result:=false;
  if (Self=nil) or fClosing then exit;
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

procedure TCodyUnitDictionary.OnIDEClose(Sender: TObject);
begin
  fClosing:=true;
  FreeAndNil(fTimer);
end;

procedure TCodyUnitDictionary.OnApplyOptions(Sender: TObject);
begin
  LoadAfterStartInS:=CodyOptions.UDLoadDelayInS;
  SaveIntervalInS:=CodyOptions.UDSaveIntervalInS;
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

procedure TCodyUnitDictionary.CheckFiles;
var
  aFilename: String;
  StrItem: PStringToStringTreeItem;
  List: TStringList;
  UDGroup: TUDUnitGroup;
  CurUnit: TUDUnit;
begin
  List:=TStringList.Create;
  try
    for StrItem in fCheckFiles do
      List.Add(StrItem^.Name);
    FreeAndNil(fCheckFiles);
    for aFilename in List do begin
      if FileExistsCached(aFilename) then continue;
      BeginCritSec;
      try
        UDGroup:=FindGroupWithFilename(aFilename);
        if UDGroup<>nil then
          DeleteGroup(UDGroup,true);
        CurUnit:=FindUnitWithFilename(aFilename);
        if CurUnit<>nil then
          DeleteUnit(CurUnit,true);
      finally
        EndCritSec;
      end;
    end;
  finally
    List.Free;
  end;
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
  LazarusIDE.AddHandlerOnIDEClose(@OnIDEClose);
  CodyOptions.AddHandlerApply(@OnApplyOptions);
end;

destructor TCodyUnitDictionary.Destroy;
begin
  fClosing:=true;
  CodyOptions.RemoveHandlerApply(@OnApplyOptions);
  FreeAndNil(fCheckFiles);
  CodeToolBoss.RemoveHandlerToolTreeChanging(@ToolTreeChanged);
  FreeAndNil(fTimer);
  WaitForThread;
  IdleConnected:=false;
  FreeAndNil(fQueuedTools);
  inherited Destroy;
  DoneCriticalsection(fCritSec);
end;

procedure TCodyUnitDictionary.Load;
begin
  if fLoaded then exit;
  WaitForThread;
  if fLoaded then exit;
  StartLoadSaveThread;
  WaitForThread;
  //debugln(['TCodyUnitDictionary.Load ']);
  //ConsistencyCheck;
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

procedure TCodyUnitDictionary.CheckFileAsync(aFilename: string);
begin
  if fClosing then exit;
  if (aFilename='') or (not FilenameIsAbsolute(aFilename)) then exit;
  if fCheckFiles=nil then
    fCheckFiles:=TStringToStringTree.Create(false);
  fCheckFiles[aFilename]:='1';
  IdleConnected:=true;
end;

{ TCodyIdentifiersDlg }

procedure TCodyIdentifiersDlg.FilterEditChange(Sender: TObject);
begin
  if FItems=nil then exit;
  IdleConnected:=true;
end;

procedure TCodyIdentifiersDlg.ButtonPanel1OKButtonClick(Sender: TObject);
begin
  SetDlgAction(cidaUseIdentifier);
end;

procedure TCodyIdentifiersDlg.ButtonPanel1HelpButtonClick(Sender: TObject);
begin
  OpenCodyHelp('#Identifier_Dictionary');
end;

procedure TCodyIdentifiersDlg.ContainsSpeedButtonClick(Sender: TObject);
begin
  UpdateItemsList;
end;

procedure TCodyIdentifiersDlg.FilterEditExit(Sender: TObject);
begin
  if FItems=nil then exit;
  if GetFilterEditText='' then
    FilterEdit.Text:=FNoFilterText;
end;

procedure TCodyIdentifiersDlg.FilterEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  i:=ItemsListBox.ItemIndex;
  case Key of
  VK_DOWN:
    if i<0 then
      ItemsListBox.ItemIndex:=0
    else if i<ItemsListBox.Count-1 then
      ItemsListBox.ItemIndex:=i+1;
  VK_UP:
    if i<0 then
      ItemsListBox.ItemIndex:=ItemsListBox.Count-1
    else if i>0 then
      ItemsListBox.ItemIndex:=i-1;
  end;
end;

procedure TCodyIdentifiersDlg.JumpButtonClick(Sender: TObject);
begin
  SetDlgAction(cidaJumpToIdentifier);
end;

procedure TCodyIdentifiersDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CodyOptions.PreferImplementationUsesSection:=
                                        AddToImplementationUsesCheckBox.Checked;
  FreeAndNil(FItems);
end;

procedure TCodyIdentifiersDlg.FormCreate(Sender: TObject);
begin
  Caption:=crsCodyIdentifierDictionary;
  ButtonPanel1.HelpButton.OnClick:=@ButtonPanel1HelpButtonClick;
  ButtonPanel1.OKButton.Caption:=crsUseIdentifier;
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OKButtonClick;
  FMaxItems:=40;
  FNoFilterText:=crsFilter;
  FItems:=TObjectList.Create;
  HideOtherProjectsCheckBox.Checked:=true;
  HideOtherProjectsCheckBox.Caption:=crsHideUnitsOfOtherProjects;
  AddToImplementationUsesCheckBox.Caption:=
    lisAddUnitToImplementationUsesSection;
  AddToImplementationUsesCheckBox.Hint:=
    lisIfIdentifierIsAddedToTheImplementationSectionAndNe;

  FJumpButton:=AddButton;
  FJumpButton.Name:='JumpButton';
  FJumpButton.OnClick:=@JumpButtonClick;
  FJumpButton.Caption:= crsJumpTo;

  StartsSpeedButton.Down:=true;
  StartsSpeedButton.Caption:='Starts';
  StartsSpeedButton.Hint:='Show only identifiers starting with filter text';
  ContainsSpeedButton.Down:=false;
  ContainsSpeedButton.Caption:='Contains';
  ContainsSpeedButton.Hint:='Show only identifiers containing filter text';
end;

procedure TCodyIdentifiersDlg.HideOtherProjectsCheckBoxChange(Sender: TObject);
begin
  if FItems=nil then exit;
  IdleConnected:=true;
end;

procedure TCodyIdentifiersDlg.ItemsListBoxClick(Sender: TObject);
begin
  if FItems=nil then exit;

end;

procedure TCodyIdentifiersDlg.ItemsListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  if FItems=nil then exit;
  UpdateIdentifierInfo;
end;

procedure TCodyIdentifiersDlg.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if not CodyUnitDictionary.Loaded then begin
    CodyUnitDictionary.Load;
    UpdateGeneralInfo;
    UpdateItemsList;
  end;
  if (FLastFilter<>GetFilterEditText)
  or (FLastHideOtherProjects<>HideOtherProjectsCheckBox.Checked)
  or (FLastFilterType<>GetFilterType) then
    UpdateItemsList;
  IdleConnected:=false;
end;

procedure TCodyIdentifiersDlg.StartsSpeedButtonClick(Sender: TObject);
begin
  UpdateItemsList;
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

procedure TCodyIdentifiersDlg.SetDlgAction(NewAction: TCodyIdentifierDlgAction);
begin
  FDlgAction:=NewAction;
  if FindSelectedItem(NewIdentifier, NewUnitFilename, NewGroupName,
    NewGroupFilename)
  then
    ModalResult:=mrOk
  else
    ModalResult:=mrNone;
end;

procedure TCodyIdentifiersDlg.SetMaxItems(AValue: integer);
begin
  if FMaxItems=AValue then Exit;
  FMaxItems:=AValue;
  UpdateItemsList;
end;

procedure TCodyIdentifiersDlg.UpdateItemsList;
var
  FilterP: PChar;
  Found: Integer;
  UnitSet: TFPCUnitSetCache;
  FPCSrcDir: String;
  CfgCache: TFPCTargetConfigCache;

  procedure AddItems(AddExactMatches: boolean);
  var
    FPCSrcFilename: String;
    Dir: String;
    Group: TUDUnitGroup;
    GroupNode: TAVLTreeNode;
    Item: TUDIdentifier;
    Node: TAVLTreeNode;
  begin
    Node:=CodyUnitDictionary.Identifiers.FindLowest;
    //debugln(['TCodyIdentifiersDlg.UpdateItemsList Filter="',FLastFilter,'" Count=',CodyUnitDictionary.Identifiers.Count]);
    while Node<>nil do begin
      Item:=TUDIdentifier(Node.Data);
      Node:=CodyUnitDictionary.Identifiers.FindSuccessor(Node);
      if CompareIdentifiers(FilterP,PChar(Pointer(Item.Name)))=0 then begin
        // exact match
        if not AddExactMatches then continue;
      end else begin
        // not exact
        if AddExactMatches then continue;
        case FLastFilterType of
        cifStartsWith:
          if not ComparePrefixIdent(FilterP,PChar(Pointer(Item.Name))) then continue;
        cifContains:
          if IdentifierPos(FilterP,PChar(Pointer(Item.Name)))<0 then continue;
        end;
      end;
      if Found>MaxItems then begin
        inc(Found); // only count, do not check
        continue;
      end;
      GroupNode:=Item.DUnit.Groups.FindLowest;
      while GroupNode<>nil do begin
        Group:=TUDUnitGroup(GroupNode.Data);
        GroupNode:=Item.DUnit.Groups.FindSuccessor(GroupNode);
        if not FilenameIsAbsolute(Item.DUnit.Filename) then continue;
        if Group.Name='' then begin
          // it's a unit without package
          if FLastHideOtherProjects then begin
            // check if unit is in unit path of current owner
            if CurUnitPath='' then continue;
            Dir:=ExtractFilePath(Item.DUnit.Filename);
            if (Dir<>'')
            and (FindPathInSearchPath(PChar(Dir),length(Dir),
                PChar(CurUnitPath),length(CurUnitPath))=nil)
            then continue;
          end;
        end else if Group.Name=PackageNameFPCSrcDir then begin
          // it's a FPC source directory
          // => check if it is the current one
          Dir:=ChompPathDelim(ExtractFilePath(Group.Filename));
          if CompareFilenames(Dir,FPCSrcDir)<>0 then continue;
          // some units have multiple sources in FPC => check target platform
          if UnitSet<>nil then begin
            FPCSrcFilename:=UnitSet.GetUnitSrcFile(Item.DUnit.Name);
            if (FPCSrcFilename<>'')
            and (CompareFilenames(FPCSrcFilename,Item.DUnit.Filename)<>0)
            then continue; // this is not the source for this target platform
            if FLastHideOtherProjects then begin
              // Note: some units do no exists on all targets (e.g. windows.pp)
              if CfgCache.Units[Item.DUnit.Name]='' then
                continue; // the unit has no ppu file
            end;
          end;
        end else if FileExistsCached(Group.Filename) then begin
          // lpk exists
        end else begin
          // lpk does not exist any more
          CodyUnitDictionary.CheckFileAsync(Group.Filename);
        end;
        if FileExistsCached(Item.DUnit.Filename) then begin
          inc(Found);
          if Found<MaxItems then begin
            FItems.Add(TCodyIdentifier.Create(Item.Name,
              Item.DUnit.Name,Item.DUnit.Filename,
              Group.Name,Group.Filename,AddExactMatches));
          end;
        end else begin
          // unit does not exist any more
          CodyUnitDictionary.CheckFileAsync(Item.DUnit.Filename);
        end;
      end;
    end;
  end;

var
  sl: TStringList;
  i: Integer;
  Item: TCodyIdentifier;
  s: String;
begin
  if not CodyUnitDictionary.Loaded then exit;
  FLastFilter:=GetFilterEditText;
  FilterP:=PChar(FLastFilter);
  FLastHideOtherProjects:=HideOtherProjectsCheckBox.Checked;
  FLastFilterType:=GetFilterType;
  UpdateCurOwnerOfUnit;

  FItems.Clear;
  sl:=TStringList.Create;
  try
    Found:=0;
    UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
    FPCSrcDir:='';
    if (UnitSet<>nil) then begin
      FPCSrcDir:=ChompPathDelim(UnitSet.FPCSourceDirectory);
      CfgCache:=UnitSet.GetConfigCache(false);
    end;
    AddItems(true);
    AddItems(false);

    SortItems;

    for i:=0 to FItems.Count-1 do begin
      Item:=TCodyIdentifier(FItems[i]);
      s:=Item.Identifier+' in '+Item.Unit_Name;
      if Item.GroupName<>'' then
        s:=s+' of '+Item.GroupName;
      sl.Add(s);
    end;
    if Found>sl.Count then
      sl.Add(Format(crsAndMoreIdentifiers, [IntToStr(Found-sl.Count)]));

    ItemsListBox.Items.Assign(sl);
    if Found>0 then
      ItemsListBox.ItemIndex:=0;
    UpdateIdentifierInfo;
  finally
    sl.Free;
  end;
end;

procedure TCodyIdentifiersDlg.SortItems;
var
  i: Integer;
  Item: TCodyIdentifier;
  DepOwner: TObject;
  BaseDir: String;
  Dir: String;
begin
  BaseDir:=ExtractFilePath(CurMainFilename);
  for i:=0 to FItems.Count-1 do begin
    Item:=TCodyIdentifier(FItems[i]);
    Item.PathDistance:=length(CreateRelativePath(ExtractFilePath(Item.UnitFile),BaseDir));
    Dir:=ChompPathDelim(ExtractFilePath(Item.UnitFile));
    if (not FilenameIsAbsolute(Item.UnitFile)) or (Dir='') then begin
      // new unit is always very near
      Item.DirectUnit:=true;
      continue;
    end;
    if (CurUnitPath<>'')
    and (FindPathInSearchPath(PChar(Dir),length(Dir),
      PChar(CurUnitPath),length(CurUnitPath))<>nil)
    then begin
      // unit is in search path of current unit
      Item.DirectUnit:=true;
      continue;
    end;
    if Item.GroupName='' then
      continue; // other project is always far away
    if Item.GroupName=PackageNameFPCSrcDir then
      continue; // FPC unit
    if CurOwner=nil then continue;
    // package unit
    Item.InUsedPackage:=PackageEditingInterface.IsOwnerDependingOnPkg(CurOwner,
                                                       Item.GroupName,DepOwner);
  end;
  FItems.Sort(@CompareCodyIdentifiersAlphaScope);
end;

procedure TCodyIdentifiersDlg.UpdateIdentifierInfo;
var
  Identifier: string;
  UnitFilename: string;
  GroupName, GroupFilename: string;
begin
  if FindSelectedItem(Identifier, UnitFilename, GroupName, GroupFilename) then begin
    if GroupFilename<>'' then
      UnitFilename:=CreateRelativePath(UnitFilename,ExtractFilePath(GroupFilename));
    UnitLabel.Caption:=Format(crsUnit2, [UnitFilename]);
    PackageLabel.Caption:=Format(crsPackage2, [GroupFilename]);
    ButtonPanel1.OKButton.Enabled:=true;
  end else begin
    UnitLabel.Caption:= Format(crsUnit2, [crsNoneSelected]);
    PackageLabel.Caption:= Format(crsPackage2, [crsNoneSelected]);
    ButtonPanel1.OKButton.Enabled:=false;
  end;
end;

procedure TCodyIdentifiersDlg.UpdateGeneralInfo;
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

function TCodyIdentifiersDlg.FindSelectedItem(out Identifier, UnitFilename,
  GroupName, GroupFilename: string): boolean;
var
  i: Integer;
  Item: TCodyIdentifier;
begin
  Result:=false;
  Identifier:='';
  UnitFilename:='';
  GroupName:='';
  GroupFilename:='';
  if FItems=nil then exit;
  i:=ItemsListBox.ItemIndex;
  if (i<0) or (i>=FItems.Count) then exit;
  Item:=TCodyIdentifier(FItems[i]);
  Identifier:=Item.Identifier;
  UnitFilename:=Item.UnitFile;
  GroupName:=Item.GroupName;
  GroupFilename:=Item.GroupFile;
  //debugln(['TCodyIdentifiersDlg.FindSelectedItem ',Identifier,' Unit=',UnitFilename,' Pkg=',GroupFilename]);
  Result:=true;
end;

function TCodyIdentifiersDlg.Init: boolean;
var
  ErrorHandled: boolean;
  Line: String;
begin
  Result:=true;
  CurInitError:=ParseTilCursor(CurTool, CurCleanPos, CurNode, ErrorHandled, false, @CurCodePos);

  CurIdentifier:='';
  CurIdentStart:=0;
  CurIdentEnd:=0;
  if (CurCodePos.Code<>nil) then begin
    Line:=CurCodePos.Code.GetLine(CurCodePos.Y-1);
    GetIdentStartEndAtPosition(Line,CurCodePos.X,CurIdentStart,CurIdentEnd);
    if CurIdentStart<CurIdentEnd then
      CurIdentifier:=copy(Line,CurIdentStart,CurIdentEnd-CurIdentStart);
  end;
  CurInImplementation:=false;
  if (CurNode<>nil) and (CurTool.FindImplementationNode.StartPos<=CurNode.StartPos)
  then
    CurInImplementation:=true;
  AddToImplementationUsesCheckBox.Enabled:=CurInImplementation;
  AddToImplementationUsesCheckBox.Checked:=
                                    CodyOptions.PreferImplementationUsesSection;

  CurSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if CurTool<>nil then begin
    CurMainFilename:=CurTool.MainFilename;
    CurMainCode:=TCodeBuffer(CurTool.Scanner.MainCode);
  end else if CurSrcEdit<>nil then begin
    CurMainFilename:=CurSrcEdit.FileName;
    CurMainCode:=TCodeBuffer(CurSrcEdit.CodeToolsBuffer);
  end else begin
    CurMainFilename:='';
    CurMainCode:=nil;
  end;

  UpdateCurOwnerOfUnit;
  UpdateGeneralInfo;
  FLastFilter:='...'; // force one update
  if CurIdentifier='' then
    FilterEdit.Text:=FNoFilterText
  else
    FilterEdit.Text:=CurIdentifier;
  IdleConnected:=true;
end;

procedure TCodyIdentifiersDlg.UseIdentifier;
var
  UnitSet: TFPCUnitSetCache;
  NewUnitInPath: Boolean;
  FPCSrcFilename: String;
  CompOpts: TLazCompilerOptions;
  UnitPathAdd: String;
  Pkg: TIDEPackage;
  CurUnitName: String;
  NewUnitName: String;
  SameUnitName: boolean;
  PkgDependencyAdded: boolean;
  NewUnitCode: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopLine: integer;

  function OpenDependency: boolean;
  // returns false to abort
  var
    DepOwner: TObject;
  begin
    debugln(['TCodyIdentifiersDlg.UseIdentifier not in unit path, loading package "'+NewGroupName+'", "'+NewGroupFilename+'" ...']);
    Result:=true;
    Pkg:=PackageEditingInterface.FindPackageWithName(NewGroupName);
    if (Pkg=nil) or (CompareFilenames(Pkg.Filename,NewGroupFilename)<>0) then
    begin
      if PackageEditingInterface.DoOpenPackageFile(NewGroupFilename,
        [pofDoNotOpenEditor],false)<>mrOK
      then begin
        debugln(['TCodyIdentifiersDlg.UseIdentifier: DoOpenPackageFile failed']);
        exit(false);
      end;
      Pkg:=PackageEditingInterface.FindPackageWithName(NewGroupName);
      if Pkg=nil then begin
        IDEMessageDialog(crsPackageNotFound,
          Format(crsPackageNotFoundItShouldBeIn, [NewGroupName, NewGroupFilename
            ]),
          mtError,[mbCancel]);
        exit(false);
      end;
    end;
    if PackageEditingInterface.IsOwnerDependingOnPkg(CurOwner,NewGroupName,DepOwner)
    then begin
      // already depending on package name
      PkgDependencyAdded:=true;
      debugln(['TCodyIdentifiersDlg.UseIdentifier owner is already using "'+NewGroupName+'"']);
      // ToDo: check version
    end;
  end;

  function AddDependency: boolean;
  // returns false to abort
  var
    OwnerList: TFPList;
  begin
    if PkgDependencyAdded then exit;
    PkgDependencyAdded:=true;
    // add dependency
    OwnerList:=TFPList.Create;
    try
      OwnerList.Add(CurOwner);
      if PackageEditingInterface.AddDependencyToOwners(OwnerList,Pkg,true)<>mrOK
      then begin
        debugln(['TCodyIdentifiersDlg.UseIdentifier checking via AddDependencyToOwners failed for new package "'+NewGroupName+'"']);
        exit(false);
      end;
      if PackageEditingInterface.AddDependencyToOwners(OwnerList,Pkg,false)<>mrOK
      then begin
        debugln(['TCodyIdentifiersDlg.UseIdentifier AddDependencyToOwners failed for new package "'+NewGroupName+'"']);
        exit(false);
      end;
      debugln(['TCodyIdentifiersDlg.UseIdentifier added dependency "'+NewGroupName+'"']);
    finally
      OwnerList.Free;
    end;
    Result:=true;
  end;

begin
  if CurSrcEdit=nil then exit;

  UpdateCurOwnerOfUnit;

  // do some sanity checks
  NewUnitInPath:=false;
  UnitPathAdd:=ChompPathDelim(
    CreateRelativePath(ExtractFilePath(CurMainFilename),
                       ExtractFilePath(NewUnitFilename)));
  CurUnitName:=ExtractFileNameOnly(CurMainFilename);
  NewUnitName:=ExtractFileNameOnly(NewUnitFilename);
  FPCSrcFilename:='';
  Pkg:=nil;
  PkgDependencyAdded:=false;

  SameUnitName:=CompareDottedIdentifiers(PChar(CurUnitName),PChar(NewUnitName))=0;
  if SameUnitName and (CompareFilenames(CurMainFilename,NewUnitFilename)<>0)
  then begin
    // another unit with same name
    IDEMessageDialog(crsUnitNameClash,
      Format(crsTheTargetUnitHasTheSameNameAsTheCurrentUnitFreePas, [#13]),
      mtError,[mbCancel]);
    exit;
  end;

  if CompareFilenames(CurMainFilename,NewUnitFilename)=0 then begin
    // same file
    NewUnitInPath:=true;
    debugln(['TCodyIdentifiersDlg.UseIdentifier same unit']);
  end
  else if (CompareFilenames(ExtractFilePath(CurMainFilename),
                        ExtractFilePath(NewUnitFilename))=0)
  then begin
    // same directory
    debugln(['TCodyIdentifiersDlg.UseIdentifier same directory']);
    NewUnitInPath:=true;
  end
  else if (CurUnitPath<>'')
  and FilenameIsAbsolute(CurMainFilename)
  and (FindPathInSearchPath(PChar(CurUnitPath),length(CurUnitPath),
                            PChar(CurMainFilename),length(CurMainFilename))<>nil)
  then begin
    // in unit search path
    debugln(['TCodyIdentifiersDlg.UseIdentifier in unit search path of owner']);
    NewUnitInPath:=true;
  end;

  UnitSet:=CodeToolBoss.GetUnitSetForDirectory('');
  if not NewUnitInPath then begin
    // new unit is not in the projects/package unit path
    if NewGroupName=PackageNameFPCSrcDir then begin
      // new unit is a FPC unit
      debugln(['TCodyIdentifiersDlg.UseIdentifier in FPCSrcDir']);
      if UnitSet<>nil then
        FPCSrcFilename:=UnitSet.GetUnitSrcFile(ExtractFileNameOnly(NewUnitFilename));
      if FPCSrcFilename='' then begin
        // a FPC unit without a ppu file
        // => ask for confirmation
        if IDEQuestionDialog(crsFPCUnitWithoutPpu,
          crsThisUnitIsLocatedInTheFreePascalSourcesButNoPpuFil,
          mtConfirmation, [mrOk, crsExtendUnitPath, mrCancel])<> mrOk then exit;
      end else
        NewUnitInPath:=true;
    end else if NewGroupName<>'' then begin
      // new unit is part of a package
      debugln(['TCodyIdentifiersDlg.UseIdentifier unit is part of a package in "'+NewGroupFilename+'"']);
      Pkg:=PackageEditingInterface.FindPackageWithName(NewGroupName);
      if (Pkg<>nil) and (CompareFilenames(Pkg.Filename,NewGroupFilename)<>0) then
      begin
        if Pkg=CurOwner then begin
          IDEMessageDialog(crsImpossibleDependency,
            Format(crsTheUnitIsPartOfItCanNotUseAnotherPackageWithTheSam, [
              CurMainFilename, #13, Pkg.Filename, #13, #13, NewGroupFilename]),
              mtError, [mbCancel]);
          exit;
        end;
        if IDEQuestionDialog(crsPackageWithSameName,
          Format(crsThereIsAlreadyAnotherPackageLoadedWithTheSameNameO, [#13,
            Pkg.Filename, #13, NewGroupFilename, #13]),
          mtConfirmation, [mrCancel, crsBTNCancel, mrOk,
            crsCloseOtherPackageAndOpenNew])<> mrOk
        then exit;
      end;
    end else begin
      // new unit is a rogue unit (no package)
      debugln(['TCodyIdentifiersDlg.UseIdentifier unit is not in a package']);
    end;
  end;

  // open package to get the compiler settings to parse the unit
  if (CurOwner<>nil) and (not NewUnitInPath)
  and (NewGroupName<>'') and (NewGroupName<>PackageNameFPCSrcDir) then begin
    if not OpenDependency then exit;
  end;

  // check if target unit is readable
  NewUnitCode:=CodeToolBoss.LoadFile(NewUnitFilename,true,false);
  if NewUnitCode=nil then begin
    IDEMessageDialog(crsFileReadError,
      Format(crsUnableToReadFile, [NewUnitFilename]),
      mtError,[mbCancel]);
    exit;
  end;

  // check if identifier still exist
  if not CodeToolBoss.FindDeclarationInInterface(NewUnitCode,NewIdentifier,
    NewCode, NewX, NewY, NewTopLine)
  then begin
    IDEMessageDialog(crsIdentifierNotFound,
      Format(crsIdentifierNotFoundInUnit, [NewIdentifier, NewUnitFilename]),
      mtError,[mbCancel]);
    exit;
  end;

  CurSrcEdit.BeginUndoBlock;
  try
    // insert or replace identifier
    if (not CurSrcEdit.SelectionAvailable)
    and (CurIdentStart<CurIdentEnd) then
      CurSrcEdit.SelectText(CurCodePos.Y,CurIdentStart,CurCodePos.Y,CurIdentEnd);
    CurSrcEdit.Selection:=NewIdentifier;

    debugln(['TCodyIdentifiersDlg.UseIdentifier CurOwner=',DbgSName(CurOwner),' ',NewUnitInPath]);
    if (CurOwner<>nil) and (not NewUnitInPath) then begin
      debugln(['TCodyIdentifiersDlg.UseIdentifier not in unit path, connecting ...']);
      if (NewGroupName<>'') and (NewGroupName<>PackageNameFPCSrcDir) then begin
        // add dependency
        if not AddDependency then exit;
      end else if FilenameIsAbsolute(NewUnitFilename)
      and FilenameIsAbsolute(CurMainFilename) then begin
        // extend unit path
        CompOpts:=GetCurOwnerCompilerOptions;
        if CompOpts<>nil then begin
          CompOpts.OtherUnitFiles:=CompOpts.OtherUnitFiles+';'+UnitPathAdd;
        end;
      end;
    end;

    if not SameUnitName then
      AddToUsesSection;
  finally
    CurSrcEdit.EndUndoBlock;
  end;
end;

procedure TCodyIdentifiersDlg.JumpToIdentifier;
var
  NewUnitCode: TCodeBuffer;
  NewCode: TCodeBuffer;
  NewX: integer;
  NewY: integer;
  NewTopLine: integer;
  Pkg: TIDEPackage;
begin
  if not FileExistsUTF8(NewUnitFilename) then begin
    IDEMessageDialog(crsFileNotFound,
      Format(crsFileDoesNotExistAnymore, [NewUnitFilename]),
      mtError,[mbCancel]);
    exit;
  end;

  // open package to get proper settings
  if (NewGroupName<>'') and (NewGroupName<>PackageNameFPCSrcDir) then begin
    Pkg:=PackageEditingInterface.FindPackageWithName(NewGroupName);
    if (Pkg=nil) or (CompareFilenames(Pkg.Filename,NewGroupFilename)<>0) then
    begin
      if PackageEditingInterface.DoOpenPackageFile(NewGroupFilename,
        [pofAddToRecent],true)=mrAbort
      then
        exit;
    end;
  end;

  // load file
  NewUnitCode:=CodeToolBoss.LoadFile(NewUnitFilename,true,false);
  if NewUnitCode=nil then begin
    IDEMessageDialog(crsFileReadError,
      Format(crsUnableToReadFile, [NewUnitFilename]),
      mtError,[mbCancel]);
    exit;
  end;

  if not CodeToolBoss.FindDeclarationInInterface(NewUnitCode,NewIdentifier,
    NewCode, NewX, NewY, NewTopLine)
  then begin
    IDEMessageDialog(crsIdentifierNotFound,
      Format(crsIdentifierNotFoundInUnit, [NewIdentifier, NewUnitFilename]),
      mtError,[mbCancel]);
    exit;
  end;

  LazarusIDE.DoOpenFileAndJumpToPos(NewCode.Filename,Point(NewX,NewY),NewTopLine,
    -1,-1,[ofDoNotLoadResource]);
end;

function TCodyIdentifiersDlg.OwnerToString(AnOwner: TObject): string;
begin
  Result:='nil';
  if AnOwner is TLazProject then
    Result:='project'
  else if AnOwner is TIDEPackage then
    Result:=TIDEPackage(AnOwner).Name;
end;

function TCodyIdentifiersDlg.GetFilterType: TCodyIdentifierFilter;
begin
  if ContainsSpeedButton.Down then
    exit(cifContains)
  else
    exit(cifStartsWith);
end;

procedure TCodyIdentifiersDlg.UpdateCurOwnerOfUnit;

  procedure GetBest(OwnerList: TFPList);
  var
    i: Integer;
  begin
    if OwnerList=nil then exit;
    for i:=0 to OwnerList.Count-1 do begin
      if (TObject(OwnerList[i]) is TLazProject)
      or ((TObject(OwnerList[i]) is TIDEPackage) and (CurOwner=nil)) then
        CurOwner:=TObject(OwnerList[i]);
    end;
    OwnerList.Free;
  end;

var
  CompOpts: TLazCompilerOptions;
begin
  CurOwner:=nil;
  if CurMainFilename='' then exit;
  //debugln(['TCodyIdentifiersDlg.UpdateCurOwnerOfUnit CurMainFilename=',CurMainFilename]);
  GetBest(PackageEditingInterface.GetOwnersOfUnit(CurMainFilename));
  if CurOwner=nil then
    GetBest(PackageEditingInterface.GetPossibleOwnersOfUnit(CurMainFilename,
             [piosfIncludeSourceDirectories]));
  if CurOwner<>nil then begin
    CompOpts:=GetCurOwnerCompilerOptions;
    if CompOpts<>nil then
      CurUnitPath:=CompOpts.GetUnitPath(false);
  end;
end;

procedure TCodyIdentifiersDlg.AddToUsesSection;
var
  NewUnitCode: TCodeBuffer;
  NewUnitName: String;
  CurUnitName: String;
  UsesNode: TCodeTreeNode;
begin
  if (CurTool=nil) or (NewUnitFilename='') then exit;
  UpdateTool;
  if (CurNode=nil) then exit;

  // check if already in uses section
  NewUnitName:=ExtractFileNameOnly(NewUnitFilename);
  if CurTool.IsHiddenUsedUnit(PChar(NewUnitName)) then exit;
  UsesNode:=CurTool.FindMainUsesSection;
  if (UsesNode<>nil) and (CurTool.FindNameInUsesSection(UsesNode,NewUnitName)<>nil)
  then exit;
  if CurInImplementation then begin
    UsesNode:=CurTool.FindImplementationUsesSection;
    if (UsesNode<>nil) and (CurTool.FindNameInUsesSection(UsesNode,NewUnitName)<>nil)
    then exit;
  end;

  // get unit name
  NewUnitCode:=CodeToolBoss.LoadFile(NewUnitFilename,true,false);
  if NewUnitCode=nil then exit;
  NewUnitName:=CodeToolBoss.GetSourceName(NewUnitCode,false);
  if NewUnitName='' then
    NewUnitName:=ExtractFileNameOnly(NewUnitFilename);
  CurUnitName:=ExtractFileNameOnly(CurMainFilename);
  if CompareDottedIdentifiers(PChar(CurUnitName),PChar(NewUnitName))=0 then
    exit; // is the same unit

  if (CurNode.Desc in [ctnUnit,ctnUsesSection]) then begin
    debugln(['TCodyIdentifiersDlg.AddToUsesSection identifier in uses section, not adding unit to uses section']);
    exit;
  end;

  // add to uses section
  debugln(['TCodyIdentifiersDlg.AddToUsesSection adding to uses section']);
  if CurInImplementation and AddToImplementationUsesCheckBox.Checked then
    CodeToolBoss.AddUnitToImplementationUsesSection(CurMainCode,NewUnitName,'')
  else
    CodeToolBoss.AddUnitToMainUsesSection(CurMainCode,NewUnitName,'');
end;

procedure TCodyIdentifiersDlg.UpdateTool;
begin
  if (CurTool=nil) or (NewUnitFilename='') then exit;
  if not LazarusIDE.BeginCodeTools then exit;
  try
    CurTool.BuildTree(lsrEnd);
  except
  end;
  CurNode:=CurTool.FindDeepestNodeAtPos(CurCleanPos,false);
end;

function TCodyIdentifiersDlg.AddButton: TBitBtn;
begin
  Result := TBitBtn.Create(Self);
  Result.Align := alCustom;
  Result.Default := false;
  Result.Constraints.MinWidth:=25;
  Result.AutoSize := true;
  Result.Parent := ButtonPanel1;
end;

function TCodyIdentifiersDlg.GetCurOwnerCompilerOptions: TLazCompilerOptions;
begin
  if CurOwner is TLazProject then
    Result:=TLazProject(CurOwner).LazCompilerOptions
  else if CurOwner is TIDEPackage then
    Result:=TIDEPackage(CurOwner).LazCompilerOptions
  else
    Result:=nil;
end;

finalization
  FreeAndNil(CodyUnitDictionary);

end.

