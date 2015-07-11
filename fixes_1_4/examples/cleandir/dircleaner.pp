{ Directory cleaning component

  Copyright (C) 2007 Michael Van Canneyt

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
unit dircleaner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inicol, IniFiles, FileUtil;
  
Type
  TSubDir = (sdExtension,sdYear,sdMonth,sdDay,sdDate,sdHour,sdMin,sdTime);
  TSubDirs = Set of TSubDir;
  
  { TLocation }

  TLocation = Class(TNamedIniCollectionItem)
  private
    FBasePath: String;
    FSubDirs: TSubDirs;
    procedure SetSubDirs(const AValue: TSubDirs);
  Public
    Function GetLocation(AExtension : String;ATime : TDateTime) : String;
    Procedure Assign(Source : TPersistent); override;
    Procedure SaveToIni(Ini: TCustomInifile; Section : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; Section : String); override;
  Published
    Property BasePath : String Read FBasePath Write FBasePath;
    Property SubDirs : TSubDirs Read FSubDirs Write SetSubDirs;
  end;

  { TLocations }

  TLocations = Class(TNamedIniCollection)
  private
    function GetLocation(Index : Integer): TLocation;
    procedure SetLocation(Index : Integer; const AValue: TLocation);
  Public
    Constructor Create;
    Function AddLocation : TLocation;
    Function LocationByName(AName : String) : TLocation;
    Property Locations [Index : Integer] : TLocation Read GetLocation Write SetLocation; Default;
  end;
  
  { TFileAction }

  TFileAction = Class(TNamedIniCollectionItem)
  private
    FCaseSensitive: Boolean;
    FCompress: Boolean;
    FDelete: Boolean;
    FExtensions: String;
    FLocation: TLocation;
    FLocationName: String;
    FMinCompressSize: Integer;
    FWorkExt : String;
  Public
    Procedure Prepare;
    Function FileNameFits(AFileName : String) : Boolean;
    Property Location : TLocation Read FLocation Write FLocation;
    Procedure SaveToIni(Ini: TCustomInifile; Section : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; Section : String); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property LocationName : String Read FLocationName Write FLocationName;
    Property Extensions : String Read FExtensions Write FExtensions;
    Property Delete : Boolean Read FDelete Write FDelete;
    Property Compress : Boolean Read FCompress write FCompress;
    Property CaseSensitive : Boolean Read FCaseSensitive Write FCaseSensitive;
    Property MinCompressSize : Integer Read FMinCompressSize Write FMinCompressSize; // In Kb.
  end;
  
  { TFileActions }

  TFileActions = Class(TNamedIniCollection)
  private
    function GetAction(Index : Integer): TFileAction;
    procedure SetAction(Index : Integer; const AValue: TFileAction);
  Public
    Constructor Create;
    Procedure Prepare;
    Function AddFileAction : TFileAction;
    Procedure BindLocations(Locations : TLocations);
    Property FileActions [Index : Integer] : TFileAction Read GetAction Write SetAction; Default;
  end;

  { TDirectory }

  TDirectory = Class(TNamedIniCollectionItem)
  private
    FEnabled: Boolean;
    FPath: String;
    FRecurse: Boolean;
  Public
    Procedure Assign(Source : TPersistent); override;
    Procedure SaveToIni(Ini: TCustomInifile; Section : String); override;
    Procedure LoadFromIni(Ini: TCustomInifile; Section : String); override;
  Published
    Property Path : String Read FPath Write FPath;
    Property Enabled : Boolean Read FEnabled Write FEnabled;
    Property Recurse : Boolean Read FRecurse Write FRecurse;
  end;


  { TDirectories }

  TDirectories = Class(TNamedIniCollection)
  private
    function GetDir(Index : Integer): TDirectory;
    procedure SetDir(Index : Integer; const AValue: TDirectory);
  Public
    Constructor Create;
    Function AddDirectory : TDirectory;
    Property Directories [Index : Integer] : TDirectory Read GetDir Write SetDir; Default;
  end;
  
  { TCleanDirs }
  TLogEvent = Procedure(Msg : String) of Object;
  
  TScheduleMode = (smDaily,smHourly);
  
  TScheduleDay = (sdMonday,sdTuesDay,sdWednesday,sdThursDay,sdFriday,sdSaturday,sdSunday);
  TScheduleDays = set of TScheduleDay;

  TCleanDirs = Class(TComponent)
  private
    FAllFileActions: Boolean;
    FCancelled: Boolean;
    FConfigFile: String;
    FDailyAt: TDateTime;
    FDirectories: TDirectories;
    FEndTime: TDateTime;
    FFileActions: TFileActions;
    FHourlyAt: Integer;
    FLocations: TLocations;
    FLogAllFiles: Boolean;
    FlogEvent: TLogEvent;
    FLogOnly: Boolean;
    FScheduleDays: TScheduleDays;
    FScheduleMode: TScheduleMode;
    FStopOnError: Boolean;
    FCreatedDirs : TStrings;
    FStartTime : TDateTime;
    function NeedDirectory(D: String): Boolean;
    procedure SetDirectories(const AValue: TDirectories);
    procedure SetFileActions(const AValue: TFileActions);
    procedure SetLocations(const AValue: TLocations);
  Protected
    Procedure LogAction(Const Msg : String);
    Procedure LogAction(Const Fmt: String; Args : Array of const);
    Function CreateLocation(L : TLocation; AExtension : String) : boolean; virtual;
    Procedure DoCleanDir(Dir : String; Recurse : Boolean); virtual;
    function DoCompressFile(const AFileName, ADestDir: String): Boolean;
    function DoCopyFile(const AFileName, ADestDir: String): Boolean;
    function DoDeleteFile(const AFileName: String): Boolean; virtual; // Returns true
    function DoFileAction(F: TFileAction; const AFileName: String): Boolean; virtual;
    Property CreatedDirs : TStrings Read FCreatedDirs;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function CleanFile(const AFileName: String): Boolean;
    procedure CleanDirectory(D: TDirectory);
    Procedure Execute;
    Procedure Cancel;
    Procedure LoadFromIni(Ini : TCustomInifile); virtual;
    Procedure SaveToIni(Ini : TCustomInifile); virtual;
    Procedure LoadFromFile(AFileName : String);
    Procedure SaveToFile(AFileName : String);
    Property StartTime : TDateTime Read FStartTime;
    Property EndTime : TDateTime Read FEndTime;
    Property ConfigFile : String Read FConfigFile;
    Property Cancelled : Boolean Read FCancelled;
  Published
    Property Directories : TDirectories Read FDirectories Write SetDirectories;
    Property FileActions : TFileActions Read FFileActions Write SetFileActions;
    Property Locations : TLocations Read FLocations Write SetLocations;
    Property LogOnly : Boolean Read FLogOnly Write FLogOnly;
    Property OnLog : TLogEvent Read FlogEvent Write FLogEvent;
    Property StopOnError : Boolean Read FStopOnError Write FStopOnError;
    Property AllFileActions : Boolean Read FAllFileActions Write FAllFileActions;
    Property LogAllFiles : Boolean Read FLogAllFiles Write FLogAllFiles;
    // Scheduling
    Property ScheduleMode : TScheduleMode Read FScheduleMode Write FScheduleMode;
    Property ScheduleDays : TScheduleDays Read FScheduleDays Write FScheduleDays;
    Property DailyAt : TDateTime Read FDailyAt Write FDailyAt;
    Property HourlyAt : Integer Read FHourlyAt Write FHourlyAt;
  end;
  
  EDiskClean = Class(Exception);

// Auxiliary functions
Function UserDir : String;
Function CleanDirApp : String; // Can be used to force single app name

Var
  LogOnly : Boolean;
  LogEvent : TLogEvent;

implementation

uses
{$ifdef mswindows}
   shfolder,
{$endif}
   zstream,
   typinfo;

ResourceString
  SCleanDirApp = 'CleanDirs';
  SNewDirectory = 'New directory';
  SNewLocation = 'New Location';
  SNewFileAction = 'New Action';
  SCreatingDir = 'Creating directory "%s".';
  SErrCreateDir = 'Could not create directory "%s".';
  SErrUnknownLocation = 'Unknown location : "%s"';
  SSkippingDir = 'Skipping disabled directory "%s": %s';
  SNoSuchDir = 'Directory does not exist: %s';
  SCleaningDir = 'Cleaning directory: %s';
  SDeleteFailed = 'Failed to delete file: %s';
  SDeletingFile = 'Deleting file: %s';
  SCopyFailed = 'Failed to copy file "%s" to "%s": %s';
  SCopyingFile = 'Copying file "%s" to directory: %s';
  SCompressingFile = 'Compressing file %s to directory: %s';
  SCompressFailed = 'Failed to compress file "%s" to "%s": %s';
  SUsercancelled = 'User cancelled';
  SConsideringFile = 'Considering file: %s';
  SFileFitsAction = 'File %s fits action %s';
  
{ ---------------------------------------------------------------------
  Auxiliary functions
  ---------------------------------------------------------------------}
  
Function UserDir : String;

{$ifdef mswindows}
Var
  PATH : Array[0..MAX_PATH] of char;
{$endif}

begin
{$ifdef mswindows}
   Path[0]:=#0;
   SHGetFolderPath(0,CSIDL_PERSONAL,0,0,@PATH);
   Result:=StrPas(Path);
{$else}
   Result:=GetEnvironmentVariableUTF8('HOME');
{$endif}
end;

{ TLocation }


procedure TLocation.SetSubDirs(const AValue: TSubDirs);

Var
  V : TSubDirs;

begin
  V:=AValue;
  If (sdDate in V) then
    V:=V-[sdYear,sdMonth,sdDay];
  If (sdTime in AValue) then
    V:=V-[sdHour,sdMin];
  FSubDirs:=V;
end;


function TLocation.GetLocation(AExtension: String;ATime : TDateTime): String;

begin
  Result:=IncludeTrailingPathDelimiter(BasePath);
  If (sdExtension in SubDirs) then
    begin
    If (Length(AExtension)>0) then
      If AExtension[1]='.' then
        Delete(AExtension,1,1);
    Result:=Result+AExtension+PathDelim;
    end;
  if (sdDate in SubDirs) then
    Result:=Result+FormatDateTime('yyyy-mm-dd',ATime)+PathDelim
  else
    begin
    if (sdYear in SubDirs) then
      Result:=Result+FormatDateTime('yyyy',ATime)+PathDelim;
    if (sdMonth in SubDirs) then
      Result:=Result+FormatDateTime('mm',ATime)+PathDelim;
    if (sdDay in SubDirs) then
        Result:=Result+FormatDateTime('dd',ATime)+PathDelim;
    end;
  if (sdTime in SubDirs) then
    Result:=Result+FormatDateTime('hh-nn',ATime)+PathDelim
  else
    begin
    if (sdHour in SubDirs) then
      Result:=Result+FormatDateTime('hh',ATime)+PathDelim;
    if (sdMin in SubDirs) then
      Result:=Result+FormatDateTime('nn',ATime)+PathDelim;
    end;
end;

procedure TLocation.Assign(Source: TPersistent);

Var
  L : TLocation;

begin
  if (Source is TLocation) then
    begin
    L:=TLocation(Source);
    Name:=L.Name;
    BasePath:=L.BasePath;
    SubDirs:=L.SubDirs;
    end
  else
    inherited Assign(Source);
end;

procedure TLocation.SaveToIni(Ini: TCustomInifile; Section: String);

Var
  S : String;

begin
  With Ini do
    begin
    WriteString(Section,'BasePath',BasePath);
    S:=SetToString(FindPropInfo(Self,'Subdirs'),Integer(SubDirs));
    WriteString(Section,'Subdirs',S);
    end;
end;

procedure TLocation.LoadFromIni(Ini: TCustomInifile; Section: String);

Var
  S : String;
  P : PPropInfo;
  
begin
  With Ini do
    begin
    BasePath:=ReadString(Section,'BasePath',BasePath);
    P:=FindPropInfo(Self,'Subdirs');
    S:=SetToString(P,Integer(SubDirs));
    S:=ReadString(Section,'Subdirs',S);
    SubDirs:=TSubDirs(StringToSet(P,S));
    end;
end;

{ TLocations }

function TLocations.GetLocation(Index : Integer): TLocation;
begin
  Result:=TLocation(Items[Index]);
end;

procedure TLocations.SetLocation(Index : Integer; const AValue: TLocation);
begin
  Items[Index]:=AValue;
end;

constructor TLocations.Create;
begin
  Inherited Create(TLocation);
  FPrefix:='Location';
  FSectionPrefix:='Location';
end;

function TLocations.AddLocation: TLocation;

Var
  I : Integer;
  N : String;
  
begin
  Result:=Add as TLocation;
  I:=1;
  N:=SNewLocation;
  While Self.IndexOfName(N)<>-1 do
    begin
    Inc(I);
    N:=SNewLocation+IntToStr(I);
    end;
  Result.Name:=N;
end;

function TLocations.LocationByName(AName: String): TLocation;
begin
  Result:=TLocation(FindByName(AName));
  If (Result=Nil) then
    Raise EDiskClean.CreateFmt(SErrUnknownLocation,[AName]);
end;

{ TFileAction }


procedure TFileAction.Prepare;
begin
  FWorkExt:=' '+FExtensions+' ';
  If Not CaseSensitive then
    FWorkExt:=UpperCase(FWorkExt);
end;

function TFileAction.FileNameFits(AFileName: String): Boolean;

Var
  Ext : String;

begin
  Ext:=ExtractFileExt(AFileName);
  system.Delete(Ext,1,1);
  if Not CaseSensitive then
    Ext:=UpperCase(Ext);
  Ext:=' '+Ext+' ';
  FileNameFits:=Pos(Ext,FWorkExt)<>0;
end;

procedure TFileAction.SaveToIni(Ini: TCustomInifile; Section: String);
begin
  With Ini do
    begin
    WriteString(Section,'Location',LocationName);
    WriteString(Section,'Extensions',Extensions);
    WriteBool(Section,'Delete',Delete);
    WriteBool(Section,'Compress',Compress);
    WriteBool(Section,'CaseSensitive',CaseSensitive);
    WriteInteger(Section,'MinCompressSize',MinCompressSize);
    end;
end;

procedure TFileAction.LoadFromIni(Ini: TCustomInifile; Section: String);
begin
  With Ini do
    begin
    LocationName:=ReadString(Section,'Location',LocationName);
    Extensions:=ReadString(Section,'Extensions',Extensions);
    Delete:=ReadBool(Section,'Delete',Delete);
    Compress:=ReadBool(Section,'Compress',Compress);
    CaseSensitive:=ReadBool(Section,'CaseSensitive',CaseSensitive);
    MinCompressSize:=ReadInteger(Section,'MinCompressSize',MinCompressSize);
    end;
end;

procedure TFileAction.Assign(Source: TPersistent);

Var
  A : TFileAction;

begin
  if (Source is TFileAction) then
    begin
    A:=TFileAction(Source);
    LocationName:=A.LocationName;
    Location:=A.Location;
    Extensions:=A.Extensions;
    Delete:=A.Delete;
    Compress:=A.Compress;
    CaseSensitive:=A.CaseSensitive;
    MinCompressSize:=A.MinCompressSize;
    end
  else
    inherited Assign(Source);
end;

{ TFileActions }

function TFileActions.GetAction(Index : Integer): TFileAction;
begin
  Result:=TFileAction(Items[Index]);
end;

procedure TFileActions.SetAction(Index : Integer; const AValue: TFileAction);
begin
  Items[Index]:=AValue;
end;

constructor TFileActions.Create;
begin
  Inherited Create(TFileAction);
  FPrefix:='Action';
  FSectionPrefix:='Action';
end;

procedure TFileActions.Prepare;

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    GetAction(I).Prepare;
end;

function TFileActions.AddFileAction: TFileAction;

Var
  I : Integer;
  N : String;

begin
  Result:=Add as TFileAction;
  I:=1;
  N:=SNewFileAction;
  While Self.IndexOfName(N)<>-1 do
    begin
    Inc(I);
    N:=SNewFileAction+IntToStr(I);
    end;
  Result.Name:=N;
end;

procedure TFileActions.BindLocations(Locations: TLocations);

Var
  I : Integer;
  A : TFileAction;

begin
  For I:=0 to Count-1 do
    begin
    A:=GetAction(I);
    If (A.LocationName<>'') then
      A.Location:=Locations.LocationByName(A.LocationName);
    end;
end;

{ TDirectory }

procedure TDirectory.Assign(Source: TPersistent);

Var
  D : TDirectory;

begin
  If (Source is TDirectory) then
    begin
    D:=TDirectory(Source);
    Path:=D.Path;
    Enabled:=D.Enabled;
    Recurse:=D.Recurse;
    end
  else
    inherited Assign(Source);
end;

procedure TDirectory.SaveToIni(Ini: TCustomInifile; Section: String);
begin
  With Ini do
    begin
    WriteString(Section,'Path',Path);
    WriteBool(Section,'Enabled',Enabled);
    WriteBool(Section,'Recurse',Recurse);
    end;
end;

procedure TDirectory.LoadFromIni(Ini: TCustomInifile; Section: String);
begin
  With Ini do
    begin
    Path:=ReadString(Section,'Path',Path);
    Enabled:=ReadBool(Section,'Enabled',Enabled);
    recurse:=ReadBool(Section,'Recurse',Recurse);
    end;
end;

{ TDirectories }

function TDirectories.GetDir(Index : Integer): TDirectory;
begin
  Result:=TDirectory(Items[Index])
end;

procedure TDirectories.SetDir(Index : Integer; const AValue: TDirectory);
begin
  Items[Index]:=AVAlue;
end;

constructor TDirectories.Create;
begin
  FPrefix:='Directory';
  FSectionPrefix:='Directory';
  Inherited Create(TDirectory);
end;

function TDirectories.AddDirectory: TDirectory;

Var
  I : Integer;
  N : String;

begin
  Result:=Add as TDirectory;
  I:=1;
  N:=SNewDirectory;
  While Self.IndexOfName(N)<>-1 do
    begin
    Inc(I);
    N:=SNewDirectory+IntToStr(I);
    end;
  Result.Name:=N;
end;

{ TCleanDirs }

procedure TCleanDirs.SetDirectories(const AValue: TDirectories);
begin
  if FDirectories=AValue then
    exit;
  If AValue=Nil then
    FDirectories.Clear
  else
    FDirectories.Assign(AValue);
end;

procedure TCleanDirs.SetFileActions(const AValue: TFileActions);
begin
  if FFileActions=AValue then
    exit;
  if (AValue=Nil) then
    FFileActions.Clear
  else
    FFileActions.Assign(AValue);
end;

procedure TCleanDirs.SetLocations(const AValue: TLocations);
begin
  if FLocations=AValue then exit;
  If (AValue=Nil) then
    FLocations.Clear
  else
    FLocations.Assign(AValue);
end;

Function TCleanDirs.NeedDirectory(D : String) : Boolean;

Var
  L : TStringList;

begin
  Result:=Not DirectoryExistsUTF8(D);
  If Result and LogOnly then
    begin
    If (FCreatedDirs=Nil) then
      begin
      L:=TStringList.Create;
      L.Sorted:=True;
      FCreatedDirs:=L;
      end;
    Result:=FCreatedDirs.IndexOf(D)=-1;
    end;
end;

Function TCleanDirs.CreateLocation(L: TLocation; AExtension: String) : Boolean;

Var
  D,Msg : String;

begin
  if Cancelled then
    Exit;
  Result:=True;
  D:=L.GetLocation(AExtension,FStartTime);
  If NeedDirectory(D) then
    begin
    LogAction(SCreatingDir,[D]);
    If LogOnly then
      FCreatedDirs.Add(D)
    else
      begin
      Result:=ForceDirectoriesUTF8(D);
      If Not Result then
        begin
        Msg:=Format(SErrCreateDir,[D]);
        LogAction(Msg);
        if StopOnError then
          Raise EDiskClean.Create(Msg)
        end;
       end;
    end;
end;

Function TCleanDirs.DoDeleteFile(Const AFileName : String) : Boolean;

Var
  Msg : String;

begin
  LogAction(SDeletingFile,[AFileName]);
  if Cancelled then
    Exit;
  Result:=LogOnly;
  If Result then
    Exit;
  Result:=DeleteFileUTF8(AFileName);
  If Not Result then
    begin
    Msg:=Format(SDeleteFailed,[AFileName]);
    LogAction(Msg);
    If StopOnError then
      Raise EDiskClean.Create(Msg);
    end;
end;

Function TCleanDirs.DoCompressFile(Const AFileName,ADestDir : String) : Boolean;

Var
  DestFile : String;
  F1 : TFileStream;
  F2 : TGZFileStream;
  
begin
  LogAction(SCompressingFile,[AFileName,ADestDir]);
  if Cancelled then
    Exit;
  Result:=LogOnly;
  If Result then
    Exit;
  DestFile:=IncludeTrailingPathDelimiter(ADestDir)+ExtractFileName(AFileName)+'.gz';
  Try
    F1:=TFileStream.Create(UTF8ToSys(AFileName),fmOpenRead);
    Try
      F2:=TGZFileStream.Create(DestFile,gzOpenWrite);
      Try
        F2.CopyFrom(F1,0);
      Finally
        F2.Free;
      end;
    finally
      F1.Free;
    end;
    Result:=True;
  except
    On E : Exception do
      begin
      If StopOnError then
        Raise
      Else
        LogAction(SCompressFailed,[AFileName,ADestDir,E.Message]);
      end;
  end;
end;

Function TCleanDirs.DoCopyFile(Const AFileName,ADestDir : String) : Boolean;

Var
  DestFile : String;
  F1,F2 : TFileStream;
  
begin
  LogAction(SCopyingFile,[AFileName,ADestDir]);
  if Cancelled then
    Exit;
  Result:=LogOnly;
  If Result then
    Exit;
  DestFile:=IncludeTrailingPathDelimiter(ADestDir)+ExtractFileName(AFileName);
  Try
    F1:=TFileStream.Create(UTF8ToSys(AFileName),fmOpenRead);
    Try
      F2:=TFileStream.Create(UTF8ToSys(DestFile),fmCreate);
      Try
        F2.CopyFrom(F1,0);
      Finally
        F2.Free;
      end;
    finally
      F1.Free;
    end;
    Result:=True;
  except
    On E : Exception do
      begin
      If StopOnError then
        Raise
      Else
        LogAction(SCopyFailed,[AFileName,ADestDir,E.Message]);
      end;
  end;
end;


Function TCleanDirs.DoFileAction(F: TFileAction; Const AFileName : String) : Boolean;

  Function NeedCompress : boolean;

  Var
    Info : TSearchRec;

  begin
    Result:=F.Compress;
    If Result and (F.MinCompressSize>0) then
      begin
      Result:=FindFirstUTF8(AFileName,faAnyFile,Info)=0;
      If Result then
        Result:=Info.Size>(F.MinCompressSize*1024);
      end;
  end;

Var
  Ext : String;
  Dir : String;

begin
  if Cancelled then
    Exit;
  Ext:=ExtractFileExt(AFileName);
  If (F.Location<>Nil) and CreateLocation(F.Location,Ext) then
    begin
    Dir:=F.Location.GetLocation(Ext,FStartTime);
    If NeedCompress then
      Result:=DoCompressFile(AFileName,Dir)
    else
      Result:=DoCopyFile(AfileName,Dir);
    end;
end;

Function TCleanDirs.CleanFile(Const AFileName : String) : Boolean;

Var
  I : Integer;
  F : TFileAction;
  D : Boolean;
  
begin
  if Cancelled then
    Exit;
  if LogAllFiles then
    LogAction(SConsideringFile,[AFileName]);
  I:=0;
  D:=False;
  While (I<FileActions.Count) do
    begin
    F:=FileActions[i];
    if F.FileNameFits(AFileName) then
      begin
      if LogAllFiles then
        LogAction(SFileFitsAction,[AFileName,F.Name]);
      Result:=True;
      if (F.Location=Nil) or DoFileAction(F,AFileName) then
        D:=D or F.Delete; // Do not delete yet. Allow all actions to be done.
      If Not FAllFileActions then
        I:=FFileActions.Count;
      end;
    Inc(I);
    end;
  // After all actions are done, delete file if this was specified.
  If D then
    DoDeleteFile(AFileName);
end;

procedure TCleanDirs.DoCleanDir(Dir: String; Recurse: Boolean);

Var
  Info : TSearchRec;

begin
  if Cancelled then
    Exit;
  LogAction(SCleaningDir,[Dir]);
  Dir:=IncludeTrailingPathDelimiter(Dir);
  If FindFirstUTF8(Dir+'*',faAnyFile and not faDirectory,Info)=0 then
    try
      repeat
        CleanFile(Dir+Info.Name);
      until (FindNextUTF8(Info)<>0);
    finally
      FindCloseUTF8(Info);
    end;
  If Recurse then
    If FindFirstUTF8(Dir+'*',faDirectory,Info)=0 then
      try
        repeat
        If (Info.Attr=faDirectory) and
           Not ((info.Name='.') or (Info.Name='..')) then
          DoCleanDir(Dir+Info.Name,Recurse);
        until (FindNextUTF8(Info)<>0);
      finally
        FindCloseUTF8(Info);
      end;
end;

constructor TCleanDirs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocations:=TLocations.Create;
  FDirectories:=TDirectories.Create;
  FFileActions:=TFileActions.Create;
end;

destructor TCleanDirs.Destroy;
begin
  FreeAndNil(FLocations);
  FreeAndNil(FDirectories);
  FreeAndNIl(FFileActions);
  inherited Destroy;
end;

procedure TCleanDirs.CleanDirectory(D : TDirectory);

Var
  Dir : String;
  Msg : String;

begin
  Dir:=D.Path;
  If Not DirectoryExistsUTF8(Dir) then
    begin
    Msg:=format(SNoSuchDir,[Dir]);
    LogAction(Msg);
    If StopOnError then
      Raise EDiskClean.Create(Msg);
    end
  else
    DoCleanDir(Dir,D.Recurse);
end;

procedure TCleanDirs.Execute;

Var
  I : Integer;
  D : TDirectory;

begin
  FStartTime:=Now;
  Try
    FileActions.Prepare;
    FileActions.BindLocations(FLocations);
    I:=0;
    While (I<Directories.Count) and Not Cancelled do
      begin
      D:=Directories[i];
      If D.Enabled then
        CleanDirectory(D)
      else
        LogAction(SSkippingDir,[D.Name,D.Path]);
      Inc(I);
      end;
    If Cancelled then
      LogAction(SUsercancelled);
  Finally
    FEndTime:=Now;
  end;
end;

procedure TCleanDirs.Cancel;
begin
  FCancelled:=True;
end;

procedure TCleanDirs.LoadFromIni(Ini: TCustomInifile);
Var
  SD : TScheduleDay;
  O : Integer;
begin
  FLocations.LoadFromIni(Ini,'Locations');
  FDirectories.LoadFromIni(Ini,'Directories');
  FFileActions.LoadFromIni(Ini,'FileActions');
  With Ini do
    begin
    LogAllFiles:=ReadBool('Global','LogAllFiles',LogAllFiles);
    StopOnError:=ReadBool('Global','StopOnError',StopOnError);
    LogOnly:=ReadBool('Global','LogOnly',LogOnly);
    ScheduleMode:=TScheduleMode(ReadInteger('Schedule','Mode',Ord(ScheduleMode)));
    O:=ReadInteger('Schedule','Days',0);
    For sd:=Low(TScheduleDay) to High(TScheduleDay) do
      if ((O and (1 shl Ord(sd)))<>0) then
        ScheduleDays:=ScheduleDays + [Sd];
    DailyAt:=ReadTime('Schedule','DailyAt',DailyAt);
    HourlyAt:=ReadInteger('Schedule','HourlyAt',HourlyAt);
    end;
end;

procedure TCleanDirs.SaveToIni(Ini: TCustomInifile);

Var
  SD : TScheduleDay;
  O : Integer;

begin
  FLocations.SaveToIni(Ini,'Locations');
  FDirectories.SaveToIni(Ini,'Directories');
  FFileActions.SaveToIni(Ini,'FileActions');
  With Ini do
    begin
    WriteBool('Global','LogAllFiles',LogAllFiles);
    WriteBool('Global','StopOnError',StopOnError);
    WriteBool('Global','LogOnly',LogOnly);
    WriteInteger('Schedule','Mode',Ord(ScheduleMode));
    O:=0;
    For sd:=Low(TScheduleDay) to High(TScheduleDay) do
      if sd in ScheduleDays then
        O:=O or (1 shl ord(sd));
    WriteInteger('Schedule','Days',O);
    WriteTime('Schedule','DailyAt',DailyAt);
    WriteInteger('Schedule','HourlyAt',HourlyAt);
    end;
end;


procedure TCleanDirs.LoadFromFile(AFileName: String);

Var
  Ini : TMemIniFile;

begin
  Ini:=TMemInifile.Create(AFileName);
  try
    LoadFromIni(Ini);
    FConfigFile:=AFileName;
  finally
    Ini.Free;
  end;
end;

procedure TCleanDirs.SaveToFile(AFileName: String);
Var
  Ini : TMemIniFile;

begin
  Ini:=TMemInifile.Create(AFileName);
  try
    Ini.CacheUpdates:=True;
    SaveToIni(Ini);
    Ini.UpdateFile;
    FConfigFile:=AFileName;
  finally
    Ini.Free;
  end;
end;

Procedure TCleanDirs.LogAction(Const Msg: String);
begin
  If Assigned(FLogEvent) then
    FLogEvent(Msg);
end;


Procedure TCleanDirs.LogAction(Const Fmt: String; Args : Array of const);

begin
  LogAction(Format(Fmt,Args));
end;

Function CleanDirApp : String;

begin
  Result:=SCleanDirApp;
end;

end.

