{ Directory cleaning component configuration window

  Copyright (C) 2007 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}


unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ComCtrls, ExtDlgs, ExtCtrls, RTTICtrls, StdCtrls, Buttons,
  dircleaner, FileUtil, RTTIGrids;

type

  { TMainForm }

  TMainForm = class(TForm)
    AExecute: TAction;
    ATest: TAction;
    ADeleteItem: TAction;
    ANewItem: TAction;
    AQuit: TAction;
    ASaveAs: TAction;
    ASave: TAction;
    AOpen: TAction;
    ANew: TAction;
    ALMain: TActionList;
    ILMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LEDailyAt: TLabel;
    LSEHourlyAt: TLabel;
    LSEFileActionMinCompressSize: TLabel;
    LELocationName: TLabel;
    LVFileActions: TListView;
    LVLocations: TListView;
    LVDirectories: TListView;
    MIRun: TMenuItem;
    MITest: TMenuItem;
    MRun: TMenuItem;
    MIDeleteItem: TMenuItem;
    MINewItem: TMenuItem;
    MItems: TMenuItem;
    MIQuit: TMenuItem;
    MISep1: TMenuItem;
    MISaveAs: TMenuItem;
    MISave: TMenuItem;
    MIOpen: TMenuItem;
    MNew: TMenuItem;
    MFile: TMenuItem;
    MMMain: TMainMenu;
    ODConfig: TOpenDialog;
    PFileAction: TPanel;
    PLocation: TPanel;
    PCConfig: TPageControl;
    POptions: TPanel;
    PDirectories: TPanel;
    SBLocationPath: TSpeedButton;
    SDConfig: TSaveDialog;
    SDDir: TSelectDirectoryDialog;
    SBDirPath: TSpeedButton;
    CBDirEnabled: TTICheckBox;
    CBDirRecurse: TTICheckBox;
    CBStopOnError: TTICheckBox;
    CBAllFileActions: TTICheckBox;
    EDirName: TTIEdit;
    EDirPath: TTIEdit;
    ELocationName: TTIEdit;
    ELocationBaseDir: TTIEdit;
    CGLocationSubdirs: TTICheckGroup;
    CBFileActionDelete: TTICheckBox;
    CBFileActionCompress: TTICheckBox;
    CBFileActionLocationName: TTIComboBox;
    EFileActionName: TTIEdit;
    EFileActionExtensions: TTIEdit;
    SEFileActionMinCompressSize: TTISpinEdit;
    CBLogAllFiles: TTICheckBox;
    CGScheduleDays: TTICheckGroup;
    EDailyAt: TTIEdit;
    RGScheduleMode: TTIRadioGroup;
    SEHourlyAt: TTISpinEdit;
    TSScheduling: TTabSheet;
    ToolButton1: TToolButton;
    TBNewItem: TToolButton;
    TBDeleteItem: TToolButton;
    TSLocations: TTabSheet;
    TSFileActions: TTabSheet;
    TSOptions: TTabSheet;
    TBMain: TToolBar;
    TBQuit: TToolButton;
    ToolButton2: TToolButton;
    TBNew: TToolButton;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    procedure ADeleteItemExecute(Sender: TObject);
    procedure ADeleteItemUpdate(Sender: TObject);
    procedure AExecuteUpdate(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ANewItemExecute(Sender: TObject);
    procedure AOpenExecute(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure ASaveAsExecute(Sender: TObject);
    procedure ASaveExecute(Sender: TObject);
    procedure ASaveUpdate(Sender: TObject);
    procedure ATestExecute(Sender: TObject);
    procedure ActionChanged(Sender: TObject);
    procedure CBFileActionCompressChange(Sender: TObject);
    procedure DirectoryChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure LocationChanged(Sender: TObject);
    procedure LVDirectoriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure LVFileActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure LVLocationsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure OptionsChanged(Sender: TObject);
    procedure PCConfigChange(Sender: TObject);
    procedure RGScheduleModeClick(Sender: TObject);
    procedure SBDirPathClick(Sender: TObject);
    procedure SBLocationPathClick(Sender: TObject);
    procedure ScheduleModeChanged(Sender: TObject);
  private
    { private declarations }
    FCleaner : TCleanDirs;
    // For easy access
    FLocations : TLocations;
    FDirectories : TDirectories;
    FFileActions : TFileActions;
    // Currently shown
    FLocation : TLocation;
    FDirectory : TDirectory;
    FFileAction : TFileAction;
    // Tracking changes
    FLocationChanged,
    FDirectoryChanged,
    FFileActionChanged,
    FOptionChanged : Boolean;
    procedure CheckScheduleMode;
    procedure RunConfig(ACleaner: TCleanDirs; TestOnly: Boolean);
    Procedure SaveCurrentRTTI;
    procedure CheckInit;
    procedure ClearChanged;
    Function DataModified : Boolean;
    procedure CheckCompressMinSize;
    function CheckSave: Boolean;
    procedure DeleteDirectory;
    procedure DeleteFileAction;
    procedure DeleteItem;
    function DeleteListItem(LV: TListView): TListItem;
    procedure DeleteLocation;
    function HaveDirectory: Boolean;
    function HaveFileAction: Boolean;
    function HaveItem: Boolean;
    function HaveLocation: Boolean;
    procedure LoadFile(AFileName: String);
    procedure NewConfig;
    procedure NewItem;
    procedure NewDirectory;
    procedure NewFileAction;
    procedure NewLocation;
    function OpenFile: Boolean;
    procedure RefreshDirectoryItem(LI: TListItem);
    procedure RefreshFileActionItem(LI: TListItem);
    procedure RefreshLocationItem(LI: TListItem);
    function SaveData: Boolean;
    function SaveDataAs: Boolean;
    Function Selectdir(Current : String) : String;
    procedure ShowDirectoryItem(LI: TListItem);
    procedure ShowFileActionItem(LI: TListItem);
    procedure ShowLocationItem(LI: TListItem);
    procedure UpdateActionLocations(Loc: TLocations);
  public
    { public declarations }
    Procedure ShowDiskCleaner(ACleaner : TCleanDirs);
    Procedure ShowLocations(ALocations : TLocations);
    Procedure ShowDirectories(ADirectories : TDirectories);
    Procedure ShowFileActions(AFileActions : TFileActions);
  end;

var
  MainForm: TMainForm;

implementation

uses
   frmLog,
   typinfo;

ResourceString
  SYes             = 'Yes';
  SNo              = 'No';
  SSubdirExtension = 'Extension';
  SSubdirYear      = 'Year';
  SSubdirMonth     = 'Month';
  SSubdirDay       = 'Day';
  SSubdirDate      = 'Date';
  SSubdirHour      = 'Hour';
  SSubdirMin       = 'Min';
  SSubdirTime      = 'Time';
  SDataChanged     = 'The configuration has changed. Save changes ?';
  

Function BoolStr(B : Boolean) : String;

begin
  if B then
    Result:=SYes
  else
    Result:=SNo;
end;

Function SubdirsToStr(S : TSubDirs) : String;

  Procedure AddToResult(Const ToAdd: String);
  
  begin
    If (Result<>'') then
      Result:=Result+',';
    Result:=Result+ToAdd
      
  end;

Var
  D : TSubDir;
  A : String;

begin
  Result:='';
  For D:=Low(TSubDir) to High(TSubDir) do
    If D in S then
      begin
      case D of
        sdExtension : A:=SSubDirExtension;
        sdYear      : A:=SSubDirYear;
        sdMonth     : A:=SSubDirMonth;
        sdDay       : A:=SSubDirDay;
        sdDate      : A:=SSubDirDate;
        sdHour      : A:=SSubDirHour;
        sdMin       : A:=SSubDirMin;
        sdTime      : A:=SSubDirTime;
      else
        A:='?';
      end;
      AddToResult(A);
      end;
end;

{ TMainForm }

procedure TMainForm.ANewExecute(Sender: TObject);
begin
  NewConfig;
end;

procedure TMainForm.ADeleteItemExecute(Sender: TObject);
begin
  DeleteItem;
end;

procedure TMainForm.ADeleteItemUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=HaveItem;
end;

procedure TMainForm.AExecuteUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(FCleaner<>Nil) and (FFileActions.Count>0);
end;

procedure TMainForm.ANewItemExecute(Sender: TObject);
begin
  NewItem;
end;

procedure TMainForm.AOpenExecute(Sender: TObject);
begin
  OpenFile;
end;

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ASaveAsExecute(Sender: TObject);
begin
  SaveDataAs;
end;

procedure TMainForm.ASaveExecute(Sender: TObject);
begin
  SaveData;
end;

procedure TMainForm.ASaveUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=DataModified;
end;

procedure TMainForm.ATestExecute(Sender: TObject);
begin
  RunConfig(FCleaner,(Sender=ATest));
end;

procedure TMainForm.ActionChanged(Sender: TObject);
begin
  FFileActionChanged:=True;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=CheckSave;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  CheckInit;
end;

Function TMainForm.Selectdir(Current : String) : String;

begin
  With SDDir do
    begin
    If (Current<>'') then
      InitialDir:=Current
    else
      InitialDir:=UserDir;
    If Execute then
      Result:=FileName
    else
      Result:=Current;
    end;
end;

{ ---------------------------------------------------------------------
  Config file management;
  ---------------------------------------------------------------------}

procedure TMainForm.NewConfig;

begin
  If Not CheckSave Then
    Exit;
  FreeAndNil(FCleaner);
  ShowDiskCleaner(TCleanDirs.Create(Self));
end;

procedure TMainForm.ShowDiskCleaner(ACleaner: TCleanDirs);
begin
  FCleaner:=ACleaner;
  CBStopOnError.Link.TIObject:=FCleaner;
  CBAllFileActions.Link.TIObject:=FCleaner;
  CBLogAllFiles.Link.TIObject:=FCleaner;
  RGScheduleMode.link.TIObject:=FCLeaner;
  CGScheduleDays.link.TIObject:=FCLeaner;
  EDailyAt.Link.TIObject:=FCleaner;
  SEHourlyAt.Link.TIObject:=FCleaner;
  ShowDirectories(FCleaner.Directories);
  ShowLocations(FCleaner.Locations);
  ShowFileActions(FCleaner.FileActions);
  ClearChanged;
end;

Procedure TMainForm.ClearChanged;

begin
  FLocationChanged:=False;
  FDirectoryChanged:=False;
  FFileActionChanged:=False;
  FOptionChanged:=False;
end;

procedure TMainForm.SaveCurrentRTTI;

Var
  C : TWinControl;
  L : TPropertyLink;

begin
  C:=Self.ActiveControl;
  If Assigned(C) then
    begin
    If IsPublishedProp(C,'Link') then
      begin
      L:=GetObjectProp(C,'Link',TPropertyLink) as TPropertyLink;
      L.SaveToProperty;
      end;
    end;
end;

function TMainForm.DataModified: Boolean;
begin
  Result:=FOptionChanged
          or FDirectoryChanged
          or FFileActionChanged
          or FLocationChanged;
end;


Function TMainForm.CheckSave : Boolean; // Return true if all OK, false if cancelled

begin
  SaveCurrentRTTI;
  Result:=Not DataModified;
  If Not Result then
    begin
    Result:=True;
    case MessageDlg(SDataChanged,mtWarning,[mbYes,mbNo,mbCancel],0) of
      mrYes    : Result:=SaveData;
      mrCancel : Result:=False;
    end;
    end;
end;

Function TMainForm.SaveData : Boolean;

begin
  If (FCleaner.ConfigFile<>'') then
    begin
    FCleaner.SaveToFile(FCleaner.ConfigFile);
    ClearChanged;
    end
  else
    Result:=SaveDataAs;
end;

Function TMainForm.SaveDataAs : Boolean;

begin
  With SDConfig do
    begin
    FileName:=FCleaner.ConfigFile;
    Result:=Execute;
    If Result then
      begin
      FCleaner.SaveToFile(FileName);
      ClearChanged;
      end;
    end;
end;

Function TMainForm.OpenFile : Boolean;

begin
  Result:=CheckSave;
  If Result then
    With ODConfig do
      begin
      Result:=Execute;
      If Result then
        LoadFile(FileName);
      end;
end;

procedure TMainForm.LoadFile(AFileName : String);

Var
  D : TCleanDirs;
  
begin
  D:=TCleanDirs.Create(Self);
  Try
    D.LoadFromFile(UTF8ToSys(AFileName));
  except
    D.Free;
    Raise;
  end;
  FreeAndNil(FCleaner);
  ShowDiskCleaner(D);
end;

procedure TMainForm.CheckInit;

begin
  If (Application.ParamCount>0) and FileExistsUTF8(Application.Params[1]) then
    LoadFile(Application.Params[1])
  else
    NewConfig;
end;

procedure TMainForm.NewItem;

Var
  TS : TTabSheet;

begin
  SaveCurrentRTTI;
  TS:=PCConfig.ActivePage;
  If (TS=TSOptions) then
    NewDirectory
  else If (TS=TSLocations) then
    NewLocation
  else if (TS=TSFileActions) then
    NewFileAction;
end;

procedure TMainForm.DeleteItem;

Var
  TS : TTabSheet;

begin
  TS:=PCConfig.ActivePage;
  If (TS=TSOptions) then
    DeleteDirectory
  else If (TS=TSLocations) then
    DeleteLocation
  else if (TS=TSFileActions) then
    DeleteFileAction;
end;

Function TMainForm.HaveItem : Boolean;

Var
  TS : TTabSheet;

begin
  Result:=False;
  TS:=PCConfig.ActivePage;
  If (TS=TSOptions) then
    Result:=HaveDirectory
  else If (TS=TSLocations) then
    Result:=HaveLocation
  else if (TS=TSFileActions) then
    Result:=HaveFileAction
end;

procedure TMainForm.OptionsChanged(Sender: TObject);
begin
  FOptionChanged:=True;
end;

procedure TMainForm.PCConfigChange(Sender: TObject);
begin
  If (PCConfig.ActivePage=TSFileActions) and Assigned(FLocations) then
    UpdateActionLocations(FLocations);
end;

procedure TMainForm.RGScheduleModeClick(Sender: TObject);
begin

end;

procedure TMainForm.RunConfig(ACleaner : TCleanDirs; TestOnly :  Boolean);

Var
  LO : Boolean;
  F : TLogForm;

begin
  LO:=ACleaner.LogOnly;
  try
    ACleaner.LogOnly:=TestOnly;
    F:=TLogForm.Create(Self);
    try
      F.Cleaner:=ACleaner;
      F.ShowModal;
    finally
      F.Free;
    end;
  finally
    ACleaner.LogOnly:=LO;
  end;
end;

procedure TMainForm.ScheduleModeChanged(Sender: TObject);
begin
  CheckScheduleMode;
  OptionsChanged(Sender);
end;

procedure TMainForm.CheckScheduleMode;

Var
  D : Boolean;


begin
  D:=(RGScheduleMode.ItemIndex=0);
  EDailyAt.Enabled:=D;
  SEHourlyAt.Enabled:=Not D;
end;


{ ---------------------------------------------------------------------
  Directories
  ---------------------------------------------------------------------}


procedure TMainForm.ShowDirectories(ADirectories: TDirectories);

Var
  LI : TListItem;
  D : TDirectory;
  I : Integer;
  
begin
  FDirectories:=ADirectories;
  With LVDirectories do
    begin
    BeginUpdate;
    Try
      Items.Clear;
      For I:=0 to FDirectories.Count-1 do
        begin
        D:=FDirectories[i];
        LI:=LVDirectories.Items.Add;
        LI.Data:=D;
        RefreshDirectoryItem(LI);
        end;
      If (LVDirectories.Items.Count>0) then
        ShowDirectoryItem(LVDirectories.Items[i])
      else
        ShowDirectoryItem(Nil)
    Finally
      EndUpdate;
    end;
    end;
end;


procedure TMainForm.LVDirectoriesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  If Selected then
    ShowDirectoryItem(Item)
  else
    ShowDirectoryItem(Nil);
end;



procedure TMainForm.RefreshDirectoryItem(LI: TListItem);

Var
  D : TDirectory;

begin
  If Not Assigned(Li) then
    Exit;
  D:=TDirectory(LI.Data);
  LI.Caption:=BoolStr(D.Enabled);
  LI.SubItems.Clear;
  LI.SubItems.Add(BoolStr(D.Recurse));
  LI.SubItems.Add(D.Name);
  LI.SubItems.Add(D.Path);
end;


procedure TMainForm.ShowDirectoryItem(LI: TListItem);

begin
  If (LI=Nil) then
    FDirectory:=Nil
  else
    FDirectory:=TDirectory(LI.Data);
  EDirName.Link.TIObject:=FDirectory;
  EDirPath.Link.TIObject:=FDirectory;
  CBDirEnabled.Link.TIObject:=FDirectory;
  CBDirRecurse.link.TIOBject:=FDirectory;
end;


procedure TMainForm.NewDirectory;

Var
  D : TDirectory;
  LI : TListItem;

begin
  RefreshDirectoryItem(LVDirectories.Selected);
  D:=FDirectories.AddDirectory;
  LI:=LVDirectories.Items.Add;
  LI.Data:=D;
  LVDirectories.Selected:=LI;
  RefreshDirectoryItem(LI);
  ShowDirectoryItem(LI);
  FDirectoryChanged:=True;
end;


Function TMainForm.DeleteListItem(LV : TListView) : TListItem;

Var
  LI : TListItem;
  I : Integer;

begin
  Result:=Nil;
  LI:=LV.Selected;
  if (Li<>Nil) then
    begin
    I:=LI.Index;
    LI.Free;
    If (I>=LV.Items.Count) then
      Dec(I);
    If (I<LV.Items.Count) then
      Result:=LV.Items[I];
    end;
end;

procedure TMainForm.DeleteDirectory;

begin
  FreeAndNil(FDirectory);
  ShowDirectoryItem(DeleteListItem(LVDirectories));
  FDirectoryChanged:=True;
end;

Function TMainForm.HaveDirectory : Boolean;

begin
  Result:=(FDirectory<>Nil);
end;

procedure TMainForm.DirectoryChanged(Sender: TObject);
begin
  RefreshDirectoryItem(LVDirectories.Selected);
  FDirectoryChanged:=True;
end;


procedure TMainForm.SBDirPathClick(Sender: TObject);

begin
  If Assigned(FDirectory) then
    begin
    FDirectory.Path:=SelectDir(FDirectory.Path);
    EDirPath.Link.LoadFromProperty;
    end;
end;

{ ---------------------------------------------------------------------
  Locations
  ---------------------------------------------------------------------}
  
procedure TMainForm.LVLocationsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  If Selected then
    ShowLocationItem(Item)
  else
    ShowLocationItem(Nil)
end;

procedure TMainForm.LocationChanged(Sender: TObject);
begin
  RefreshLocationItem(LVLocations.Selected);
  FLocationChanged:=True;
end;


procedure TMainForm.SBLocationPathClick(Sender: TObject);
begin
  If Assigned(FLocation) then
    begin
    FLocation.BasePath:=SelectDir(FLocation.BasePath);
    ELocationBaseDir.Link.LoadFromProperty;
    end;
end;



procedure TMainForm.ShowLocations(ALocations: TLocations);

Var
  LI : TListItem;
  L : TLocation;
  I : Integer;

begin
  FLocations:=ALocations;
  With LVLocations do
    begin
    BeginUpdate;
    Try
      Items.Clear;
      For I:=0 to FLocations.Count-1 do
        begin
        L:=FLocations[i];
        LI:=LVLocations.Items.Add;
        LI.Data:=L;
        RefreshLocationItem(LI);
        end;
      If (LVLocations.Items.Count>0) then
        ShowLocationItem(LVLocations.Items[i])
      else
        ShowLocationItem(Nil)
    Finally
      EndUpdate;
    end;
    end;
  UpdateActionLocations(ALocations);
end;

procedure TMainForm.RefreshLocationItem(LI: TListItem);

Var
  L : TLocation;

begin
  If Not Assigned(Li) then
    Exit;
  L:=TLocation(LI.Data);
  LI.Caption:=L.Name;
  LI.SubItems.Clear;
  LI.SubItems.Add(L.BasePath);
  LI.SubItems.Add(SubdirsToStr(L.SubDirs));
end;


procedure TMainForm.ShowLocationItem(LI: TListItem);

begin
  If (LI=Nil) then
    FLocation:=Nil
  else
    FLocation:=TLocation(LI.Data);
  ELocationName.Link.TIObject:=FLocation;
  ELocationBaseDir.Link.TIObject:=FLocation;
  CGLocationSubdirs.Link.TIObject:=FLocation;
end;

procedure TMainForm.UpdateActionLocations(Loc : TLocations);

Var
  I,J : Integer;

begin
  With CBFileActionLocationName.Items do
    begin
    BeginUpdate;
    Try
      Clear;
      For I:=0 to Loc.Count-1 do
        begin
        J:=Add(Loc[i].Name);
        Objects[J]:=Loc[i];
        end;
    Finally
      EndUpdate;
    end;
    end;
end;

procedure TMainForm.NewLocation;

Var
  D : TLocation;
  LI : TListItem;

begin
  RefreshLocationItem(LVLocations.Selected);
  D:=FLocations.AddLocation;
  LI:=LVLocations.Items.Add;
  LI.Data:=D;
  LVLocations.Selected:=LI;
  ShowLocationItem(LI);
end;

procedure TMainForm.DeleteLocation;

begin
  FreeAndNil(FLocation);
  ShowDirectoryItem(DeleteListItem(LVLocations));
end;

Function TMainForm.HaveLocation : Boolean;

begin
  Result:=(FLocation<>Nil)
end;

{ ---------------------------------------------------------------------
  FileActions
  ---------------------------------------------------------------------}
  
procedure TMainForm.LVFileActionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  If Selected then
    ShowFileActionItem(Item)
  else
    ShowFileActionItem(Nil)
end;

procedure TMainForm.CBFileActionCompressChange(Sender: TObject);
begin
  ActionChanged(Sender);
  CheckCompressMinSize;
end;


procedure TMainForm.ShowFileActions(AFileActions: TFileActions);

Var
  LI : TListItem;
  A : TFileAction;
  I : Integer;

begin
  FFileActions:=AFileActions;
  With LVFileActions do
    begin
    BeginUpdate;
    Try
      Items.Clear;
      For I:=0 to FFileActions.Count-1 do
        begin
        A:=FFileActions[i];
        LI:=LVFileActions.Items.Add;
        LI.Data:=A;
        RefreshFileActionItem(LI);
        end;
      If (LVFileActions.Items.Count>0) then
        ShowFileActionItem(LVFileActions.Items[i])
      else
        ShowFileActionItem(Nil)
    Finally
      EndUpdate;
    end;
    end;
end;

procedure TMainForm.RefreshFileActionItem(LI: TListItem);

Var
  A : TFileAction;

begin
  If Not Assigned(Li) then
    Exit;
  A:=TFileAction(LI.Data);
  LI.Caption:=A.Name;
  LI.SubItems.Clear;
  LI.SubItems.Add(A.Extensions);
  LI.SubItems.Add(A.LocationName);
  LI.SubItems.Add(BoolStr(A.Delete));
  LI.SubItems.Add(BoolStr(A.Compress));
  LI.SubItems.Add(IntToStr(A.MinCompressSize));
  LI.SubItems.Add(BoolStr(A.CaseSensitive));
end;


procedure TMainForm.ShowFileActionItem(LI: TListItem);

begin
  If (LI=Nil) then
    FFileAction:=Nil
  else
    FFileAction:=TFileAction(LI.Data);
  EFileActionName.Link.TIObject:=FFileAction;
  EFileActionExtensions.Link.TIObject:=FFileAction;
  CBFileActionLocationName.Link.TIObject:=FFileAction;
  // Bug in control: the items list is cleared !!
//  UpdateActionLocations(FLocations);
  CBFileActionDelete.Link.TIObject:=FFileAction;
  CBFileActionCompress.Link.TIObject:=FFileAction;
  SEFileActionMinCompressSize.Link.TIObject:=FFileAction;
  CheckCompressMinSize;
end;

procedure TMainForm.CheckCompressMinSize;

begin
  With SEFileActionMinCompressSize do
    begin
    Enabled:=CBFileActionCompress.State=cbChecked;
    If Not enabled then
      SEFileActionMinCompressSize.Value:=0;
    LSEFileActionMinCompressSize.Enabled:=Enabled;
    end;
end;

procedure TMainForm.NewFileAction;

Var
  D : TFileAction;
  LI : TListItem;

begin
  RefreshFileActionItem(LVFileActions.Selected);
  D:=FFileActions.AddFileAction;
  LI:=LVFileActions.Items.Add;
  LI.Data:=D;
  LVFileActions.Selected:=LI;
  ShowFileActionItem(LI);
end;

procedure TMainForm.DeleteFileAction;

begin
  FreeAndNil(FFileAction);
  ShowDirectoryItem(DeleteListItem(LVFileActions));
end;

Function TMainForm.HaveFileAction : Boolean;

begin
  Result:=(FFileAction<>Nil)
end;

initialization
  {$I frmmain.lrs}

end.

