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
    Defines the TExternalToolList which stores the settings of all external
    tools. (= Programfilename and parameters)
    And provides TExternalToolDlg which is a dialog for editing a
    TExternalToolList;
}
unit ExtToolDialog;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Process, contnrs, LCLType, LCLProc, Controls, Forms,
  Buttons, StdCtrls, ComCtrls, Dialogs, ExtCtrls, ButtonPanel, Menus,
  LazConfigStorage, FileProcs, UTF8Process,
  IDEExternToolIntf, IDEImagesIntf, IDEDialogs, IDEHelpIntf, IDECommands,
  CompOptsIntf, ProjectIntf,
  EnvironmentOpts,
  ExtToolEditDlg, KeyMapping, TransferMacros, IDEProcs, LazFileUtils,
  InfoBuild, CompilerOptions, OutputFilter, LazarusIDEStrConsts, IDEOptionDefs;

const
  MaxExtTools = ecExtToolLast-ecExtToolFirst+1;

{$IFDEF EnableNewExtTools}
const
  ExternalToolOptionsVersion = 3;
  // 3: changed ScanOutputForFPCMessages to scanner SubToolFPC
  //    changed ScanOutputForMakeMessages to scanner SubToolMake
type

  { TExternalToolMenuItem - the options of an external tool in the IDE menu Tools }

  TExternalToolMenuItem = class(TComponent)
  private
    FChangeStamp: integer;
    fCmdLineParams: string;
    FEnvironmentOverrides: TStringList;
    fFilename: string;
    FHideMainForm: boolean;
    FKey: word;
    FScanners: TStrings;
    FShift: TShiftState;
    fTitle: string;
    fWorkingDirectory: string;
    fSavedChangeStamp: integer;
    function GetModified: boolean;
    procedure SetChangeStamp(AValue: integer);
    procedure SetCmdLineParams(AValue: string);
    procedure SetEnvironmentOverrides(AValue: TStringList);
    procedure SetFilename(AValue: string);
    procedure SetHideMainForm(AValue: boolean);
    procedure SetModified(AValue: boolean);
    procedure SetScanners(AValue: TStrings);
    procedure SetTitle(AValue: string);
    procedure SetWorkingDirectory(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Obj: TObject): boolean; override;
    function Load(Config: TConfigStorage; CfgVersion: integer): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    procedure AddScanner(const Scanner: string);
    property CmdLineParams: string read fCmdLineParams write SetCmdLineParams;
    property Filename: string read fFilename write SetFilename;
    property Title: string read fTitle write SetTitle;
    property WorkingDirectory: string read fWorkingDirectory write SetWorkingDirectory;
    property EnvironmentOverrides: TStringList read FEnvironmentOverrides write SetEnvironmentOverrides;
    property HideMainForm: boolean read FHideMainForm write SetHideMainForm default true;
    property Scanners: TStrings read FScanners write SetScanners;
    property Modified: boolean read GetModified write SetModified;
    property ChangeStamp: integer read FChangeStamp write SetChangeStamp;
    procedure IncreaseChangeStamp; inline;
  public
    // these properties are saved in the keymappings, not in the config
    property Key: word read FKey write FKey;
    property Shift: TShiftState read FShift write FShift;
  end;

  { TExternalToolMenuItems }

  TExternalToolMenuItems = class(TBaseExternalToolMenuItems)
  private
    fItems: TObjectList; // list of TExternalToolMenuItem
    function GetItems(Index: integer): TExternalToolMenuItem; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; inline;
    function Equals(Obj: TObject): boolean; override;
    procedure Assign(Src: TExternalToolMenuItems);
    procedure Add(Item: TExternalToolMenuItem);
    procedure Insert(Index: integer; Item: TExternalToolMenuItem);
    procedure Move(CurIndex, NewIndex: integer);
    function Load(Config: TConfigStorage): TModalResult;
    function Load(Config: TConfigStorage; const Path: string): TModalResult;
      override;
    function Save(Config: TConfigStorage): TModalResult;
    function Save(Config: TConfigStorage; const Path: string): TModalResult;
      override;
    procedure LoadShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    procedure SaveShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    function Count: integer; inline;
    property Items[Index: integer]: TExternalToolMenuItem read GetItems; default;
  end;

{$ELSE EnableNewExtTools}

type
  TOnNeedsOutputFilter = procedure(var OutputFilter: TOutputFilter;
                           var Abort: boolean) of object;
  TOnFreeOutputFilter = procedure(OutputFilter: TOutputFilter;
                           ErrorOccurred: boolean) of object;

  { TExternalToolList -
    the storage object for all external tools }

  TExternalToolList = class(TBaseExternalToolList)
  private
    fOnFreeOutputFilter: TOnFreeOutputFilter;
    fOnNeedsOutputFilter: TOnNeedsOutputFilter;
    fRunningTools: TList; // list of TProcess
    function GetToolOpts(Index: integer): TExternalToolOptions;
    procedure SetToolOpts(Index: integer; NewTool: TExternalToolOptions);
    procedure AddRunningTool(TheProcess: TProcess; ExecuteProcess: boolean);
  public
    procedure Add(NewTool: TExternalToolOptions);
    procedure Assign(Source: TExternalToolList);
    procedure Clear; override;
    constructor Create;
    procedure Delete(Index: integer); 
    destructor Destroy; override;
    procedure FreeStoppedProcesses;
    procedure Insert(Index: integer; NewTool: TExternalToolOptions);
    function Load(Config: TConfigStorage): TModalResult;
    function Load(Config: TConfigStorage; const Path: string): TModalResult; override;
    procedure LoadShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    function Run(ExtTool: TIDEExternalToolOptions;
                 Macros: TTransferMacroList;
                 ShowAbort: boolean;
                 CompilerOptions: TLazCompilerOptions = nil): TModalResult; override;
    function Run(Index: integer; Macros: TTransferMacroList;
                 ShowAbort: boolean): TModalResult;
    function Save(Config: TConfigStorage): TModalResult;
    function Save(Config: TConfigStorage; const Path: string): TModalResult; override;
    procedure SaveShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    
    property Items[Index: integer]: TExternalToolOptions
      read GetToolOpts write SetToolOpts; default;
    property OnFreeOutputFilter: TOnFreeOutputFilter
      read fOnFreeOutputFilter write fOnFreeOutputFilter;
    property OnNeedsOutputFilter: TOnNeedsOutputFilter
      read fOnNeedsOutputFilter write fOnNeedsOutputFilter;
  end;
{$ENDIF EnableNewExtTools}

  { TExternalToolDialog -
    the dialog to edit all external tools }

  TExternalToolDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    ListBox: TListBox;
    MenuItemImport: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemSeparator: TMenuItem;
    MenuItemClone: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupDropdownMenu: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ToolBar: TToolBar;
    AddButton: TToolButton;
    RemoveButton: TToolButton;
    EditButton: TToolButton;
    tbSeparator: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    tbSeparator2: TToolButton;
    ExtraButton: TToolButton;
    procedure AddButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure MenuItemCloneClick(Sender: TObject);
    procedure MenuItemExportClick(Sender: TObject);
    procedure MenuItemImportClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure ListboxClick(Sender: TObject);
  private
    {$IFDEF EnableNewExtTools}
    fExtToolList: TExternalToolMenuItems;
    {$ELSE}
    fExtToolList: TExternalToolList;
    {$ENDIF}
    fTransferMacros: TTransferMacroList;
    procedure Load;
    procedure SetExtToolList(NewExtToolList: {$IFDEF EnableNewExtTools}TExternalToolMenuItems{$ELSE}TExternalToolList{$ENDIF});
    procedure SetTransferMacros(NewMacros: TTransferMacroList);
    function ToolDescription(Index: integer): string;
    procedure EnableButtons;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property ExtToolList: {$IFDEF EnableNewExtTools}TExternalToolMenuItems{$ELSE}TExternalToolList{$ENDIF}
      read fExtToolList write SetExtToolList;
    property TransferMacros: TTransferMacroList
                                   read fTransferMacros write SetTransferMacros;
  end;
  
function ShowExtToolDialog(ExtToolList: {$IFDEF EnableNewExtTools}TExternalToolMenuItems{$ELSE}TExternalToolList{$ENDIF};
  TransferMacros: TTransferMacroList):TModalResult;

implementation

{$R *.lfm}

function ShowExtToolDialog(ExtToolList: {$IFDEF EnableNewExtTools}TExternalToolMenuItems{$ELSE}TExternalToolList{$ENDIF};
  TransferMacros: TTransferMacroList):TModalResult;
var
  ExternalToolDialog: TExternalToolDialog;
begin
  Result:=mrCancel;
  ExternalToolDialog:=TExternalToolDialog.Create(nil);
  try
    ExternalToolDialog.TransferMacros:=TransferMacros;
    ExternalToolDialog.ExtToolList:=ExtToolList;
    Result:=ExternalToolDialog.ShowModal;
    if Result=mrOk then
      ExtToolList.Assign(ExternalToolDialog.ExtToolList);
  finally
    ExternalToolDialog.Free;
  end;
end;

{$IFDEF EnableNewExtTools}
{ TExternalToolMenuItems }

// inline
function TExternalToolMenuItems.Count: integer;
begin
  Result:=fItems.Count;
end;

// inline
function TExternalToolMenuItems.GetItems(Index: integer): TExternalToolMenuItem;
begin
  Result:=TExternalToolMenuItem(fItems[Index]);
end;

// inline
procedure TExternalToolMenuItems.Clear;
begin
  fItems.Clear;
end;

constructor TExternalToolMenuItems.Create;
begin
  fItems:=TObjectList.Create(true);
end;

destructor TExternalToolMenuItems.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TExternalToolMenuItems.Equals(Obj: TObject): boolean;
var
  Src: TExternalToolMenuItems;
  i: Integer;
begin
  if Obj=Self then exit;
  if Obj is TExternalToolMenuItems then begin
    Src:=TExternalToolMenuItems(Obj);
    Result:=false;
    if Count<>Src.Count then exit;
    for i:=0 to Count-1 do
      if not Items[i].Equals(Src[i]) then exit;
    Result:=true;
  end else
    Result:=inherited Equals(Obj);
end;

procedure TExternalToolMenuItems.Assign(Src: TExternalToolMenuItems);
var
  Item: TExternalToolMenuItem;
begin
  if Equals(Src) then exit;
  Clear;
  for i:=0 to Src.Count-1 do begin
    Item:=TExternalToolMenuItem.Create(nil);
    Item.Assign(Src[i]);
    Add(Item);
  end;
end;

procedure TExternalToolMenuItems.Add(Item: TExternalToolMenuItem);
begin
  fItems.Add(Item);
end;

procedure TExternalToolMenuItems.Insert(Index: integer;
  Item: TExternalToolMenuItem);
begin
  fItems.Insert(Index,Item);
end;

procedure TExternalToolMenuItems.Move(CurIndex, NewIndex: integer);
begin
  fItems.Move(CurIndex,NewIndex);
end;

function TExternalToolMenuItems.Load(Config: TConfigStorage): TModalResult;
var
  i: integer;
  NewTool: TExternalToolOptions;
begin
  Clear;
  Count:=Config.GetValue('Count',0);
  for i:=0 to Count-1 do begin
    NewTool:=TExternalToolOptions.Create;
    Items[i]:=NewTool;
    Config.AppendBasePath('Tool'+IntToStr(i+1)+'/');
    try
      if NewTool.Load(Config)<>mrOk then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

function TExternalToolMenuItems.Load(Config: TConfigStorage; const Path: string
  ): TModalResult;
begin
  if Path<>'' then
    Config.AppendBasePath(Path);
  try
    Result:=Load(Config);
  finally
    if Path<>'' then
      Config.UndoAppendBasePath;
  end;
end;

function TExternalToolMenuItems.Save(Config: TConfigStorage): TModalResult;
var
  i: integer;
begin
  Config.SetValue('Count',Count);
  for i:=0 to Count-1 do begin
    Config.AppendBasePath('Tool'+IntToStr(i+1)+'/');
    try
      if Items[i].Save(Config)<>mrOk then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

function TExternalToolMenuItems.Save(Config: TConfigStorage; const Path: string
  ): TModalResult;
begin
  if Path<>'' then
    Config.AppendBasePath(Path);
  try
    Result:=Save(Config);
  finally
    if Path<>'' then
      Config.UndoAppendBasePath;
  end;
end;

procedure TExternalToolMenuItems.LoadShortCuts(
  KeyCommandRelationList: TKeyCommandRelationList);
var
  i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      Items[i].Key:=KeyCommandRelation.ShortcutA.Key1;
      Items[i].Shift:=KeyCommandRelation.ShortcutA.Shift1;
    end else begin
      Items[i].Key:=VK_UNKNOWN;
      Items[i].Shift:=[];
    end;
  end;
end;

procedure TExternalToolMenuItems.SaveShortCuts(
  KeyCommandRelationList: TKeyCommandRelationList);
var
  i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  KeyCommandRelationList.ExtToolCount:=Count;
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      KeyCommandRelation.ShortcutA:=IDEShortCut(Items[i].Key,Items[i].Shift,
                                           VK_UNKNOWN,[]);
    end else begin
      DebugLn('[TExternalToolMenuItems.SaveShortCuts] Error: '
        +'unable to save shortcut for external tool "',Items[i].Title,'"');
    end;
  end;
end;

{$ELSE EnableNewExtTools}

{ TExternalToolList }

function TExternalToolList.GetToolOpts(Index: integer): TExternalToolOptions;
begin
  Result:=TExternalToolOptions(inherited Items[Index]);
end;

procedure TExternalToolList.SetToolOpts(Index: integer; 
  NewTool: TExternalToolOptions);
begin
  inherited Items[Index]:=NewTool;
end;

procedure TExternalToolList.Add(NewTool: TExternalToolOptions);
begin
  inherited Add(NewTool);
end;

procedure TExternalToolList.Assign(Source: TExternalToolList);
var
  i: integer;
begin
  if Source=Self then exit;
  Clear;
  if Source=nil then exit;
  Count:=Source.Count;
  for i:=0 to Count-1 do begin
    Items[i]:=TExternalToolOptions.Create;
    Items[i].Assign(Source[i]);
  end;
end;

constructor TExternalToolList.Create;
begin
  inherited Create;
  Clear;
end;

procedure TExternalToolList.Delete(Index: integer); 
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

destructor TExternalToolList.Destroy;
var
  i: Integer;
begin
  FreeStoppedProcesses;
  if fRunningTools<>nil then begin
    for i:=0 to fRunningTools.Count-1 do
      TProcess(fRunningTools[i]).Free;
    fRunningTools.Free;
  end;
  inherited Destroy;
end;

procedure TExternalToolList.Clear; 
var
  i: integer;
begin
  for i:=0 to Count-1 do
    TExternalToolOptions(Items[i]).Free;
  inherited Clear;
end;

procedure TExternalToolList.Insert(Index: integer; NewTool: TExternalToolOptions);
begin
  inherited Insert(Index,NewTool);
end;

function TExternalToolList.Load(Config: TConfigStorage): TModalResult;
var
  i: integer;
  NewTool: TExternalToolOptions;
begin
  Clear;
  Count:=Config.GetValue('Count',0);
  for i:=0 to Count-1 do begin
    NewTool:=TExternalToolOptions.Create;
    Items[i]:=NewTool;
    Config.AppendBasePath('Tool'+IntToStr(i+1)+'/');
    try
      if NewTool.Load(Config)<>mrOk then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

function TExternalToolList.Load(Config: TConfigStorage; const Path: string
  ): TModalResult;
begin
  if Path<>'' then
    Config.AppendBasePath(Path);
  try
    Result:=Load(Config);
  finally
    if Path<>'' then
      Config.UndoAppendBasePath;
  end;
end;

procedure TExternalToolList.LoadShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
var
  i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      Items[i].Key:=KeyCommandRelation.ShortcutA.Key1;
      Items[i].Shift:=KeyCommandRelation.ShortcutA.Shift1;
    end else begin
      Items[i].Key:=VK_UNKNOWN;
      Items[i].Shift:=[];
    end;
  end;
end;

function TExternalToolList.Run(Index: integer;
  Macros: TTransferMacroList; ShowAbort: boolean): TModalResult;
begin
  Result:=mrCancel;
  if (Index<0) or (Index>=Count) then exit;
  Result:=Run(Items[Index],Macros,ShowAbort);
end;

function TExternalToolList.Run(ExtTool: TIDEExternalToolOptions;
  Macros: TTransferMacroList; ShowAbort: boolean;
  CompilerOptions: TLazCompilerOptions): TModalResult;
var
  WorkingDir, Filename, Params, CmdLine, Title: string;
  TheProcess: TProcessUTF8;
  Abort, ErrorOccurred: boolean;
  NewFilename: String;
  TheOutputFilter: TOutputFilter;
begin
  Result:=mrCancel;
  if ExtTool=nil then exit;
  TheOutputFilter:=nil;
  Filename:=ExtTool.Filename;
  WorkingDir:=ExtTool.WorkingDirectory;
  Params:=ExtTool.CmdLineParams;
  Title:=ExtTool.Title;
  if Title='' then Title:=Filename;
  if (not Macros.SubstituteStr(Filename)) then exit;
  if (not Macros.SubstituteStr(WorkingDir)) then exit;
  if (not Macros.SubstituteStr(Params)) then exit;

  // expand working directory
  WorkingDir:=TrimAndExpandDirectory(WorkingDir);
  if (WorkingDir<>'')
  and (not DirPathExists(WorkingDir)) then begin
    Result:=IDEMessageDialogAb(lisExtToolFailedToRunTool,
      Format(lisExtToolUnableToRunTheTool, ['"', Title, '"', LineEnding,
        Format(lisWorkingDirectoryNotFound, [WorkingDir])]),
      mtError,[mbCancel],ShowAbort);
    CompileProgress.Ready(lisExtToolUnableToRunTheTool, ['"', Title, '"', LineEnding,
        Format(lisWorkingDirectoryNotFound, [WorkingDir])]);
    exit;
  end;

  // expand file name
  if not FilenameIsAbsolute(Filename) then begin
    NewFilename:=FindProgram(Filename,GetCurrentDirUTF8,false);
    if NewFilename='' then begin
      Result:=IDEMessageDialogAb(lisExtToolFailedToRunTool,
        Format(lisExtToolUnableToRunTheTool, ['"', Title, '"', LineEnding,
          Format(lisProgramNotFound, [Filename])]),
        mtError,[mbCancel],ShowAbort);
      CompileProgress.Ready(lisExtToolUnableToRunTheTool, ['"', Title, '"', LineEnding,
          Format(lisProgramNotFound, [Filename])]);
      exit;
    end;
    Filename:=NewFilename;
  end;
  WorkingDir:=TrimFilename(WorkingDir);
  Filename:=TrimFilename(Filename);
  CmdLine:=Filename;
  if Params<>'' then
    CmdLine:=CmdLine+' '+Params;
  DebugLn('[TExternalToolList.Run] CmdLine="',CmdLine,'" WorkDir="',WorkingDir,'"');
  TheProcess:=nil;
  try
    try
      CheckIfFileIsExecutable(Filename);
      TheProcess := TOutputFilterProcess.Create(nil);
      TheProcess.Executable := FileName;
      SplitCmdLineParams(Params,TheProcess.Parameters);
      TheProcess.Options:= [poUsePipes,poStdErrToOutPut];
      if ExtTool.HideMainForm then
        TheProcess.ShowWindow := swoHide
      else
        TheProcess.ShowWindow := swoShowNormal;
      TheProcess.CurrentDirectory := WorkingDir;
      if ExtTool.EnvironmentOverrides.Count>0 then
        ExtTool.AssignEnvironmentTo(TheProcess.Environment);
      if (ExtTool.NeedsOutputFilter) and (TheOutputFilter=nil)
      and Assigned(OnNeedsOutputFilter) then begin
        Abort:=false;
        OnNeedsOutputFilter(TheOutputFilter,Abort);
        if Abort then begin
          CompileProgress.Ready(lisInfoBuildAbort);
          Result:=mrAbort;
          exit;
        end;
      end;
      if TheOutputFilter<>nil then begin
        ErrorOccurred:=false;
        try
          TheOutputFilter.CompilerOptions:=CompilerOptions as TBaseCompilerOptions;
          TheOutputFilter.Options:=[ofoExceptionOnError,ofoMakeFilenamesAbsolute];
          if ExtTool.ScanOutputForFPCMessages then
            TheOutputFilter.Options:=TheOutputFilter.Options+[ofoSearchForFPCMessages];
          if ExtTool.ScanOutputForMakeMessages then
            TheOutputFilter.Options:=TheOutputFilter.Options+[ofoSearchForMakeMessages];
          if ExtTool.ShowAllOutput then
            TheOutputFilter.Options:=TheOutputFilter.Options+[ofoShowAll];
          try
            Result:=mrCancel;
            try
              if TheOutputFilter.Execute(TheProcess,Self,ExtTool) then begin
                TheOutputFilter.ReadConstLine(Format(lisExtToolTitleCompleted,[Title]),true);
              end;
              if TheOutputFilter.ErrorExists then begin
                ErrorOccurred:=true;
              end;
            finally
              TheProcess.WaitOnExit;
              FreeAndNil(TheProcess);
            end;
            if ErrorOccurred then
              Result:=mrCancel
            else if TheOutputFilter.Aborted then
              Result:=mrAbort
            else
              Result:=mrOk;
          except
            on e: EOutputFilterError do begin
              DebugLn('TExternalToolList.Run Exception: ',E.Message);
              ErrorOccurred:=true;
            end
            else
              raise
          end;
        finally
          if Assigned(OnFreeOutputFilter) then
            OnFreeOutputFilter(TheOutputFilter,ErrorOccurred);
        end;
      end else begin
        AddRunningTool(TheProcess,true);
        TheProcess:=nil;
        Result:=mrOk;
      end;
    finally
      FreeAndNil(TheProcess);
    end;
  except
    on e: Exception do begin
      DebugLn('TExternalToolList.Run ',lisExtToolFailedToRunTool, ' ', E.Message);
      DumpExceptionBackTrace;
      Result:=IDEMessageDialogAb(lisExtToolFailedToRunTool,
        Format(lisExtToolUnableToRunTheTool, ['"', Title, '"', LineEnding, e.Message]),
        mtError,[mbCancel],ShowAbort);
      CompileProgress.Ready(lisExtToolUnableToRunTheTool,
                            ['"', Title, '"', LineEnding, e.Message]);
    end;
  end;
end;

function TExternalToolList.Save(Config: TConfigStorage): TModalResult;
var
  i: integer;
begin
  Config.SetValue('Count',Count);
  for i:=0 to Count-1 do begin
    Config.AppendBasePath('Tool'+IntToStr(i+1)+'/');
    try
      if Items[i].Save(Config)<>mrOk then exit;
    finally
      Config.UndoAppendBasePath;
    end;
  end;
  Result:=mrOk;
end;

function TExternalToolList.Save(Config: TConfigStorage; const Path: string): TModalResult;
begin
  if Path<>'' then
    Config.AppendBasePath(Path);
  try
    Result:=Save(Config);
  finally
    if Path<>'' then
      Config.UndoAppendBasePath;
  end;
end;

procedure TExternalToolList.SaveShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
var
  i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  KeyCommandRelationList.ExtToolCount:=Count;
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      KeyCommandRelation.ShortcutA:=IDEShortCut(Items[i].Key,Items[i].Shift,
                                           VK_UNKNOWN,[]);
    end else begin
      DebugLn('[TExternalToolList.SaveShortCuts] Error: '
        +'unable to save shortcut for external tool "',Items[i].Title,'"');
    end;
  end;
end;

procedure TExternalToolList.AddRunningTool(TheProcess: TProcess;
  ExecuteProcess: boolean);
begin
  if fRunningTools=nil then fRunningTools:=TList.Create;
  fRunningTools.Add(TheProcess);
  if ExecuteProcess then
    TheProcess.Execute;
end;

procedure TExternalToolList.FreeStoppedProcesses;
var
  i: integer;
  TheProcess: TProcess;
begin
  if fRunningTools=nil then exit;
  i:=fRunningTools.Count-1;
  while i>=0 do begin
    try
      TheProcess:=TProcess(fRunningTools[i]);
      if not TheProcess.Running then begin
        try
          TheProcess.WaitOnExit;
          TheProcess.Free;
        finally
          fRunningTools.Delete(i);
        end;
      end;
    except
      on E: Exception do begin
        DebugLn('Error freeing stopped process: ',E.Message);
      end;
    end;
    dec(i);
  end;
end;
{$ENDIF}

{ TExternalToolDialog }

constructor TExternalToolDialog.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  Name:='ExternalToolDialog';

  Caption:=lisExtToolExternalTools;
  
  ToolBar.Images := IDEImages.Images_16;

  AddButton.Caption:=lisAdd;
  RemoveButton.Caption:=lisRemove;
  EditButton.Caption:=lisEdit;
  MoveUpButton.Caption:=lisUp;
  MoveDownButton.Caption:=lisDown;

  ExtraButton.Caption:=lisMoreSub;
  MenuItemClone.Caption:=lisClone;
  MenuItemExport.Caption:=lisDlgExport;
  MenuItemImport.Caption:=lisDlgImport;

  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  AddButton.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  RemoveButton.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  EditButton.ImageIndex := IDEImages.LoadImage(16, 'laz_edit');
  MoveUpButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  MoveDownButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');

  {$IFDEF EnableNewExtTools}
  fExtToolList:=TExternalToolMenuItems.Create;
  {$ELSE}
  fExtToolList:=TExternalToolList.Create;
  {$ENDIF}
end;

destructor TExternalToolDialog.Destroy;
begin
  FreeAndNil(fExtToolList);
  inherited Destroy;
end;

procedure TExternalToolDialog.SetExtToolList(NewExtToolList: {$IFDEF EnableNewExtTools}TExternalToolMenuItems{$ELSE}TExternalToolList{$ENDIF});
begin
  if fExtToolList=NewExtToolList then exit;
  fExtToolList.Assign(NewExtToolList);
  Load;
end;

procedure TExternalToolDialog.SetTransferMacros(NewMacros: TTransferMacroList);
begin
  if fTransferMacros=NewMacros then exit;
  fTransferMacros:=NewMacros;
end;

function TExternalToolDialog.ToolDescription(Index: integer): string;
begin
  Result:=fExtToolList[Index].ShortDescription;
  if Result='' then
    Result:=fExtToolList[Index].Title;
  if Result='' then
    Result:=ExtractFilename(fExtToolList[Index].Filename);
  //DebugLn(['TExternalToolDialog.ToolDescription Index=',Index,' Result=',Result,' Cmd="',fExtToolList[Index].Filename,' ',fExtToolList[Index].CmdLineParams,'"']);
end;

procedure TExternalToolDialog.Load;
var
  i: integer;
begin
  Listbox.Items.BeginUpdate;
  Listbox.Items.Clear;
  for i:=0 to fExtToolList.Count-1 do 
    Listbox.Items.Add(ToolDescription(i));
  Listbox.Items.EndUpdate;
  EnableButtons;
end;

procedure TExternalToolDialog.AddButtonClick(Sender: TObject);
var
  {$IFDEF EnableNewExtTools}
  NewTool: TExternalToolMenuItem;
  {$ELSE}
  NewTool: TExternalToolOptions;
  {$ENDIF}
begin
  if fExtToolList.Count>=MaxExtTools then begin
    IDEMessageDialog(lisExtToolMaximumToolsReached,
                  Format(lisExtToolThereIsAMaximumOfTools, [IntToStr(MaxExtTools)]),
                  mtInformation,[mbCancel]);
    exit;
  end;
  {$IFDEF EnableNewExtTools}
  NewTool:=TExternalToolMenuItem.Create;
  {$ELSE}
  NewTool:=TExternalToolOptions.Create;
  {$ENDIF}
  if ShowExtToolOptionDlg(fTransferMacros,NewTool)=mrOk then begin
    fExtToolList.Add(NewTool);
    Listbox.Items.Add(ToolDescription(fExtToolList.Count-1));
  end else begin
    NewTool.Free;
  end;
  EnableButtons;
end;

procedure TExternalToolDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TExternalToolDialog.MenuItemCloneClick(Sender: TObject);
var
  {$IFDEF EnableNewExtTools}
  NewTool, OldTool: TExternalToolMenuItem;
  {$ELSE}
  NewTool, OldTool: TExternalToolOptions;
  {$ENDIF}
begin
  If Listbox.ItemIndex <> -1 Then Begin
    OldTool := fExtToolList.Items[Listbox.ItemIndex];
    If Assigned(OldTool) Then Begin
      {$IFDEF EnableNewExtTools}
      NewTool:=TExternalToolMenuItem.Create;
      {$ELSE}
      NewTool:=TExternalToolOptions.Create;
      {$ENDIF}
      NewTool.Assign(OldTool);
      fExtToolList.Add(NewTool);
      Listbox.Items.Add(ToolDescription(fExtToolList.Count-1));
    end;
  end;
end;

procedure TExternalToolDialog.MenuItemExportClick(Sender: TObject);
Var
  FileConfig : TXMLOptionsStorage;
  AFileName : String;
begin
  If SaveDialog1.Execute Then Begin
    AFileName := SaveDialog1.FileName;
    Case SaveDialog1.FilterIndex Of
      1 : Begin
            AFileName := ChangeFileExt(AFileName, '.xml');
          end;
    end;
    FileConfig := TXMLOptionsStorage.Create(AFileName, False);
    fExtToolList.Save(FileConfig);
    FileConfig.WriteToDisk;
    FreeAndNil(FileConfig);
  end;
end;

procedure TExternalToolDialog.MenuItemImportClick(Sender: TObject);
Var
  FileConfig: TXMLOptionsStorage;
  {$IFDEF EnableNewExtTools}
  NewToolList: TExternalToolMenuItems;
  {$ELSE}
  NewToolList : TExternalToolList;
  {$ENDIF}
begin
  If OpenDialog1.Execute Then Begin
    {$IFDEF EnableNewExtTools}
    NewToolList := TExternalToolMenuItems.Create;
    {$ELSE}
    NewToolList := TExternalToolList.Create;
    {$ENDIF}
    FileConfig := TXMLOptionsStorage.Create(OpenDialog1.FileName, True);
    NewToolList.Load(FileConfig);
    SetExtToolList(NewToolList);
    FreeAndNil(FileConfig);
    FreeAndNil(NewToolList);
  end;
end;

procedure TExternalToolDialog.RemoveButtonClick(Sender: TObject);
begin
  if Listbox.ItemIndex<0 then exit;
  fExtToolList.Delete(Listbox.ItemIndex);
  ListBox.Items.Delete(Listbox.ItemIndex);
  EnableButtons;
end;

procedure TExternalToolDialog.EditButtonClick(Sender: TObject);
var
  i: LongInt;
begin
  i:=Listbox.ItemIndex;
  if i<0 then exit;
  if ShowExtToolOptionDlg(fTransferMacros,fExtToolList[i])=mrOk
  then begin
    Listbox.Items[i]:=ToolDescription(i);
    EnableButtons;
  end;
end;

procedure TExternalToolDialog.MoveUpButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  if i<1 then exit;
  fExtToolList.Move(i,i-1);
  Listbox.Items.Move(i,i-1);
  Listbox.ItemIndex:=i-1;
  EnableButtons;
end;

procedure TExternalToolDialog.MoveDownButtonClick(Sender: TObject);
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  if (i<0) or (i>=Listbox.Items.Count-1) then exit;
  fExtToolList.Move(i,i+1);
  Listbox.Items.Move(i,i+1);
  Listbox.ItemIndex:=i+1;
  EnableButtons;
end;

procedure TExternalToolDialog.EnableButtons;
var
  i: integer;
begin
  i:=Listbox.ItemIndex;
  AddButton.Enabled:=fExtToolList.Count<MaxExtTools;
  RemoveButton.Enabled:=(i>=0);
  EditButton.Enabled:=(i>=0);
  MoveUpButton.Enabled:=(i>0);
  MoveDownButton.Enabled:=(i>=0) and (i<fExtToolList.Count-1);
end;

procedure TExternalToolDialog.ListboxClick(Sender: TObject);
begin
  EnableButtons;
end;

{$IFDEF EnableNewExtTools}
{ TExternalToolMenuItem }

// inline
procedure TExternalToolMenuItem.IncreaseChangeStamp;
begin
  CTIncreaseChangeStamp(FChangeStamp);
end;

function TExternalToolMenuItem.GetModified: boolean;
begin
  Result:=FChangeStamp=fSavedChangeStamp;
end;

procedure TExternalToolMenuItem.SetChangeStamp(AValue: integer);
begin
  if FChangeStamp=AValue then Exit;
  FChangeStamp:=AValue;
end;

procedure TExternalToolMenuItem.SetCmdLineParams(AValue: string);
begin
  AValue:=UTF8Trim(AValue,[]);
  if fCmdLineParams=AValue then Exit;
  fCmdLineParams:=AValue;
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetEnvironmentOverrides(AValue: TStringList);
begin
  if (FEnvironmentOverrides=AValue) or FEnvironmentOverrides.Equals(AValue) then Exit;
  FEnvironmentOverrides.Assign(AValue);
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetFilename(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  if fFilename=AValue then Exit;
  fFilename:=AValue;
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetHideMainForm(AValue: boolean);
begin
  if FHideMainForm=AValue then Exit;
  FHideMainForm:=AValue;
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetModified(AValue: boolean);
begin
  if AValue then
    IncreaseChangeStamp
  else
    fSavedChangeStamp:=FChangeStamp;
end;

procedure TExternalToolMenuItem.SetScanners(AValue: TStrings);
begin
  if (FScanners=AValue) or FScanners.Equals(AValue) then Exit;
  FScanners.Assign(AValue);
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetTitle(AValue: string);
begin
  AValue:=UTF8Trim(AValue,[]);
  if fTitle=AValue then Exit;
  fTitle:=AValue;
  IncreaseChangeStamp;
end;

procedure TExternalToolMenuItem.SetWorkingDirectory(AValue: string);
begin
  AValue:=TrimFilename(AValue);
  if fWorkingDirectory=AValue then Exit;
  fWorkingDirectory:=AValue;
  IncreaseChangeStamp;
end;

constructor TExternalToolMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnvironmentOverrides:=TStringList.Create;
  FScanners:=TStringList.Create;
  fSavedChangeStamp:=CTInvalidChangeStamp;
  Clear;
end;

destructor TExternalToolMenuItem.Destroy;
begin
  FreeAndNil(FEnvironmentOverrides);
  FreeAndNil(FScanners);
  inherited Destroy;
end;

procedure TExternalToolMenuItem.Clear;
begin
  CmdLineParams:='';
  if FEnvironmentOverrides.Count>0 then
  begin
    IncreaseChangeStamp;
    FEnvironmentOverrides.Clear;
  end;
  Filename:='';
  HideMainForm:=true;
  if FScanners.Count>0 then
  begin
    FScanners.Clear;
    IncreaseChangeStamp;
  end;
  Title:='';
  WorkingDirectory:='';
end;

function TExternalToolMenuItem.Equals(Obj: TObject): boolean;
var
  Src: TExternalToolMenuItem;
begin
  if Obj is TExternalToolMenuItem then begin
    Src:=TExternalToolMenuItem(Obj);
    Result:=(CmdLineParams=Src.CmdLineParams)
      and EnvironmentOverrides.Equals(Src.EnvironmentOverrides)
      and (Filename=Src.Filename)
      and (HideMainForm=Src.HideMainForm)
      and Scanners.Equals(Src.Scanners)
      and (Title=Src.Title)
      and (WorkingDirectory=Src.WorkingDirectory);
  end else
    Result:=inherited Equals(Obj);
end;

function TExternalToolMenuItem.Load(Config: TConfigStorage; CfgVersion: integer
  ): TModalResult;
begin
  Clear;
  fTitle:=Config.GetValue('Title/Value','');
  fFilename:=Config.GetValue('Filename/Value','');
  fCmdLineParams:=Config.GetValue('CmdLineParams/Value','');
  fWorkingDirectory:=Config.GetValue('WorkingDirectory/Value','');
  Config.GetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  Config.GetValue('Scanners/',FScanners);
  HideMainForm:=Config.GetValue('HideMainForm/Value',true);

  if CfgVersion<3 then
  begin
    if Config.GetValue('ScanOutputForFPCMessages/Value',false) then
      AddScanner(SubToolFPC);
    if Config.GetValue('ScanOutputForMakeMessages/Value',false) then
      AddScanner(SubToolMake);
    if Config.GetValue('ShowAllOutput/Value',false) then
      AddScanner(SubToolDefault);
  end;

  Modified:=false;
  Result:=mrOk;
end;

function TExternalToolMenuItem.Save(Config: TConfigStorage): TModalResult;
begin
  Config.SetDeleteValue('Title/Value',Title,'');
  Config.SetDeleteValue('Filename/Value',Filename,'');
  Config.SetDeleteValue('CmdLineParams/Value',CmdLineParams,'');
  Config.SetDeleteValue('WorkingDirectory/Value',WorkingDirectory,'');
  Config.SetValue('EnvironmentOverrides/',FEnvironmentOverrides);
  Config.SetValue('Scanners/',FScanners);
  Config.SetDeleteValue('HideMainForm/Value',HideMainForm,true);
  Modified:=false;
end;

procedure TExternalToolMenuItem.AddScanner(const Scanner: string);
begin
  if IndexInStringList(FScanners,cstCaseInsensitive,Scanner)>=0 then exit;
  FScanners.Add(Scanner);
  IncreaseChangeStamp;
end;
{$ENDIF}

initialization
  ExternalToolListClass:=TExternalToolList;

end.
