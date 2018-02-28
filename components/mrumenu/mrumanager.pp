{ MRU (Most Recent Used) menu item manager

  Copyright (C) 2011 Michael Van Canneyt (michael@freepascal.org)
                     Modifications by Werner Pamler

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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit mrumanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles, menus;

Type
  { TRecentMenuItem }

  TRecentMenuItem = Class(TMenuItem)
  Private
    FFileName : string;
  Public
    Property FileName : String Read FFileName;
  end;
  TRecentMenuItemClass = Class of TRecentMenuItem;

  { TMRUMenuManager }

  TOnRecentFileEvent = Procedure(Sender : TObject; Const AFileName : String) of object;

  TMRUMenuManager = Class(TComponent)
  Private
    FIniFileName: String;
    FIniSection: String;
    FOnRecent: TOnRecentFileEvent;
    FRecent : TStrings;
    FMaxRecent : Integer;
    FMenuCaptionMask : string;
    FMIRecent : TMenuItem;
    FPMRecent : TPopupMenu;
    FMaxItemLength : integer;
    procedure SetIniFileName(const AValue:string);
    procedure SetIniSection(const AValue:string);
    procedure SetMaxItemLength(const AValue:integer);
    procedure SetMenuCaptionMask(const AValue:string);
    procedure SetMIRecent(const AValue: TMenuItem);
    procedure SetPMRecent(const AValue: TPopupMenu);
    procedure SetRecent(const AValue: TStrings);
  protected
    // Overrides.
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Return default name and section if either is empty.
    procedure GetFileNameAndSection(Var AFileName,ASection : String); virtual;
    // Override if you want to load additional values.
    procedure LoadFromIni(Ini: TCustomIniFile; ASection: String); virtual;
    // Override if you want to write additional values.
    procedure SaveToIni(Ini: TCustomIniFile; ASection: String); virtual;
    // Called when menu item is clicked.
    procedure DoOnRecentClick(Sender: TObject); virtual;
    // Override this if you want to create a custom class of menu itel.
    function CreateMenuItem(AOwner: TComponent): TRecentMenuItem; virtual;
    // Create a menu caption. Default is index followed by filename.
    // Override if you want to customize.
    Function CreateMenuCaption(AIndex : Integer; Const AFileName : String) : String; virtual;
  Public
    Constructor Create(AOwner : TComponent);override;
    Destructor Destroy; override;
    // Load files from ini file AFileName in section ASection. Calls ShowRecentFiles
    // Need for explicit call only when IniFileName='' and IniSection='' and class created at run-time
    procedure LoadRecentFilesFromIni(const AFileName: string=''; const ASection: String='');
    // Saves files to ini file AFileName in section ASection.
    procedure SaveRecentFilesToIni(const AFileName: string=''; const ASection: String='');
    // Add a filename to the list of files.
    // If an existing file is added, it is moved first in the list.
    // If MaxRecent is attained, the last one is removed.
    // Calls ShowRecentFiles.
    procedure AddToRecent(AFileName: String);
    // Re-populate the menu.
    procedure ShowRecentFiles;
  Published
    // Max. items to be kept in the list.
    Property MaxRecent : Integer Read FMaxRecent write FMaxRecent default 10;
    // Menu item to create a submenu under. Existing items will be removed.
    Property MenuItem : TMenuItem Read FMIRecent Write SetMIRecent;
    // Popupmenu attached to a toolbar button. Existing items will be removed.
    Property PopupMenu : TPopupMenu Read FPMRecent Write SetPMRecent;
    // Default ini filename.
    Property IniFileName : String Read FIniFileName Write SetIniFileName;
    // Default ini section.
    Property IniSection : String Read FIniSection Write SetIniSection;
    // Maximum length of recent menu item
    Property MaxItemLength : integer Read FMaxItemLength Write SetMaxItemLength default 80;
    // Format mask for MenuCaption: first placeholder must be %d, second %s, e.g. '%d - %s'
    Property MenuCaptionMask : string read FMenuCaptionMask Write SetMenuCaptionMask;
    // Recent items. If adding manually to the list, ShowRecentFiles must be called manually.
    Property Recent : TStrings Read FRecent Write SetRecent;
    // Called when the user clicks an recent meu item.
    Property OnRecentFile : TOnRecentFileEvent Read FOnRecent Write FOnRecent;
  end;
  EMRUManager = Class(Exception);

Const
  DefaultIniFile  = 'recent.ini';
  DefaultSection  = 'Global';
  KeyMaxRecent    = 'MaxRecent';
  KeyCount        = 'Count';
  KeyFile         = 'File%d';

implementation

Resourcestring
  SErrFailedToCreateDir = 'Failed to create directory "%s"';

const
  DEFAULT_MASK = '%0:d.  %1:s';

function MinimizeFileName(const AFileName:string; AMaxLen:integer) : string;

  procedure SplitPath(const APath:String; Parts: TStrings);
  { Splits the provided path into constituent folder names }
  var
    i, j : Integer;
  begin
    if APath = '' then exit;
    if not Assigned(Parts) then exit;

    i := Length(APath);
    j := i;
    while (i >= 1) do begin
      if APath[i] = DirectorySeparator then begin
        Parts.Insert(0, copy(APath, i+1, j-i));
        j := i;
      end;
      dec(i);
    end;
    Parts.Insert(0, copy(APath, 1, j));
  end;

  function AddStringsFromTo(AList:TStrings; FromIndex,ToIndex:integer) : string;
  var
    i : integer;
  begin
    result := '';
    for i:=FromIndex to ToIndex do
      result := result + AList[i];
  end;

var
  Parts : TStringList;
  i : integer;
  tmp : string;
begin
  result := AFileName;
  if Length(AFileName) > AMaxLen then begin
    Parts := TStringList.Create;
    try
      SplitPath(AFileName, Parts);
      i := Parts.Count div 2;
      while (i < Parts.Count) do begin
        tmp := Format('%s...%s%s', [
          AddStringsFromTo(Parts, 0, i-1),
          DirectorySeparator,
          AddStringsFromTo(Parts, i+1, Parts.Count-1)
        ]);
        if Length(tmp) < AMaxLen then begin
          result := tmp;
          exit;
        end else
          Parts.Delete(i);
        i := Parts.Count div 2;
      end;
      result := ExtractFileName(AFileName);
    finally
      Parts.Free;
    end;
  end;
end;

procedure TMRUMenuManager.AddToRecent(AFileName : String);

Var
  I,J : Integer;
  B : Boolean;

begin
  AFileName:=ExpandFileName(AFileName);
  With FRecent do
    begin
    J:=IndexOf(AFileName);
    If (J<>-1) then
      begin
      if (J>0) then
        Exchange(0,J)
      end
    else
      begin
      While (Count>=FMaxRecent) do
        Delete(Count-1);
      Insert(0,AFileName)
      end;
    end;
  ShowRecentFiles;
end;

function TMRUMenuManager.CreateMenuItem(AOwner :TComponent) : TRecentMenuItem;

begin
  Result:=TRecentMenuItem.Create(AOwner);
end;

function TMRUMenuManager.CreateMenuCaption(AIndex: Integer;
  const AFileName: String): String;
var
  fn : string;
  mask : string;
begin
  if FMaxItemLength > 0 then
    fn := MinimizeFileName(AFileName, FMaxItemLength)
  else
    fn := AFileName;
  if FMenuCaptionMask = '' then
    mask := DEFAULT_MASK
  else
    mask := FMenuCaptionMask;
  Result:=Format(mask, [AIndex+1,fn]);
end;

procedure TMRUMenuManager.ShowRecentFiles;

Var
  I : Integer;
  M : TRecentMenuItem;

begin
  if Assigned(FMIRecent) then begin
    FMIRecent.clear;
    For I:=0 to FRecent.Count-1 do
    begin
      M:=CreateMenuItem(Self.Owner);
      M.Caption:=CreateMenuCaption(I,FRecent[i]);
      M.FFileName:=FRecent[i];
      M.OnClick:=@DoOnRecentClick;
      FMIRecent.Add(M);
    end;
  end;
  if Assigned(FPMRecent) then begin
    FPMRecent.Items.Clear;
    for i:=0 to FRecent.Count-1 do
    begin
      M := CreateMenuItem(Self.Owner);
      M.Caption := CreateMenuCaption(I, Recent[i]);
      M.FFileName := FRecent[i];
      M.OnClick := @DoOnRecentClick;
      FPMRecent.Items.Add(M);
    end;
  end;
end;

procedure TMRUMenuManager.LoadFromIni(Ini : TCustomIniFile; ASection : String);

Var
  I,Count : Integer;
  FN : String;

begin
  FRecent.Clear;
  FMaxRecent:=Ini.ReadInteger(ASection,KeyMaxRecent,10);
  Count:=Ini.ReadInteger(ASection,KeyCount,0);
  For I:=1 to Count do
    begin
    FN:=Ini.ReadString(ASection,Format(KeyFile,[i]),'');
    If (FN<>'') then
      FRecent.Add(FN);
    end;
end;

procedure TMRUMenuManager.GetFileNameAndSection(var AFileName, ASection: String);

begin
  if (AFileName='') then
    begin
    AFileName:=GetAppConfigDir(False);
    AFileName:=IncludeTrailingPathDelimiter(AFileName)+DefaultIniFile;
    end;
  if (ASection='') then
    ASection:=DefaultSection;
end;

procedure TMRUMenuManager.LoadRecentFilesFromIni(Const AFileName : string = ''; Const ASection : String = '');

Var
  DN,FN,Sec : String;
  Ini : TIniFile;

begin
  FN:=AFileName;
  Sec:=ASection;
  GetFileNameAndSection(FN,Sec);
  DN:=ExtractFilePath(FN);
  If ForceDirectories(DN) then
    begin
    If FileExists(FN) then
      begin
      Ini:=TIniFile.Create(FN);
      try
        LoadFromIni(Ini,Sec);
      finally
        Ini.Free;
      end;
      end;
    end;
  ShowRecentFiles;
end;

procedure TMRUMenuManager.SaveToIni(Ini : TCustomIniFile; ASection : String);

Var
  I : Integer;
begin
  Ini.EraseSection(ASection);
  Ini.WriteInteger(ASection,KeyMaxRecent,FMaxRecent);
  Ini.WriteInteger(ASection,KeyCount,FRecent.Count);
  For I:=0 to FRecent.Count-1 do
    Ini.WriteString(ASection,Format(KeyFile,[i+1]),FRecent[i]);
  Ini.UpdateFile;
end;

procedure TMRUMenuManager.SaveRecentFilesToIni(Const AFileName : string = ''; Const ASection : String = '');

Var
  DN,FN,Sec : String;
  Ini : TMemIniFile;

begin
  FN:=AFileName;
  Sec:=ASection;
  GetFileNameAndSection(FN,Sec);
  DN:=ExtractFilePath(FN);
  If not ForceDirectories(DN) then
    Raise EMRUManager.CreateFmt(SErrFailedToCreateDir,[DN]);
  Ini:=TMemIniFile.Create(FN);
  try
    SaveToIni(Ini,Sec);
  finally
    Ini.Free;
  end;
end;

procedure TMRUMenuManager.SetIniFileName(const AValue:string);
begin
  if AValue <> FIniFileName then begin
    FIniFileName := AValue;
    LoadRecentFilesFromIni(FIniFileName, FIniSection);
  end;
end;

procedure TMRUMenuManager.SetIniSection(const AValue:string);
begin
  if AValue <> FIniSection then begin
    FIniSection := AValue;
    LoadRecentFilesFromini(FIniFileName, FIniSection);
  end;
end;

procedure TMRUMenuManager.SetMaxItemLength(const AValue:integer);
begin
  if FMaxItemLength <> AValue then begin
    FMaxItemLength := AValue;
    ShowRecentFiles;
  end;
end;

procedure TMRUMenuManager.SetMenuCaptionMask(const AValue:string);
begin
  if FMenuCaptionMask <> AValue then begin
    FMenuCaptionMask := AValue;
    ShowRecentFiles;
  end;
end;

procedure TMRUMenuManager.SetMIRecent(const AValue: TMenuItem);
begin
  if FMIRecent=AValue then exit;
  If Assigned(FMIRecent) then
    FMIRecent.RemoveFreeNotification(Self);
  FMIRecent:=AValue;
  If Assigned(FMIRecent) then
    FMIRecent.FreeNotification(Self);
  ShowRecentFiles;
end;

procedure TMRUMenuManager.SetPMRecent(const AValue: TPopupMenu);
begin
  if FPMRecent=AValue then exit;
  if Assigned(FPMRecent) then
    FPMRecent.RemoveFreeNotification(self);
  FPMRecent := AValue;
  if Assigned(FPMRecent) then
    FPMRecent.FreeNotification(self);
  ShowRecentFiles;
end;

procedure TMRUMenuManager.SetRecent(const AValue: TStrings);
begin
  if FRecent=AValue then exit;
  FRecent.Assign(AValue);
  ShowRecentFiles;
end;

procedure TMRUMenuManager.loaded;
begin
  inherited loaded;
  if (FRecent.Count>0) and (assigned(FMIRecent) or assigned(FPMRecent))then
    LoadRecentFilesFromIni(FIniFileName, FIniSection);
end;

constructor TMRUMenuManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecent:=TStringList.Create;
  FMaxRecent := 10;
  FMaxItemLength := 80;
  FMenuCaptionMask := DEFAULT_MASK;
end;

destructor TMRUMenuManager.Destroy;
begin
  SaveRecentFilesToIni(FIniFileName, FIniSection);
  FreeAndNil(FRecent);
  inherited Destroy;
end;

procedure TMRUMenuManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then begin
    if AComponent = FMIRecent then FMIRecent := nil;
    if AComponent = FPMRecent then FPMRecent := nil;
  end;
  { original code - I think this is not correct:
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and ((AComponent=FMIRecent) or (AComponent=FPMRecent)) then
     exit;
  }
end;

procedure TMRUMenuManager.DoOnRecentClick(Sender: TObject);
Var
  FN : String;
begin
  With (Sender as TRecentMenuItem) do
    FN:=FileName;
  if (FN<>'') and (OnRecentFile<>Nil) then
    begin
    OnRecentFile(Self,FN);
    AddToRecent(FN); // Gets moved to the top
    end;
end;

end.

