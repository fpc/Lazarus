{ Copyright (C) 2006 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}
unit H2PasConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LazConfigStorage, XMLPropStorage, FileUtil;
  
type
  TH2PasProject = class;

  { TH2PasFile }

  TH2PasFile = class(TPersistent)
  private
    FEnabled: boolean;
    FFilename: string;
    FModified: boolean;
    FProject: TH2PasProject;
    procedure SetEnabled(const AValue: boolean);
    procedure SetFilename(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure SetProject(const AValue: TH2PasProject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AFile: TH2PasFile): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
  public
    property Project: TH2PasProject read FProject write SetProject;
    property Filename: string read FFilename write SetFilename;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Modified: boolean read FModified write SetModified;
  end;

  { TH2PasProject }

  TH2PasProject = class(TPersistent)
  private
    FBaseDir: string;
    FCHeaderFiles: TFPList;// list of TH2PasFile
    FConstantsInsteadOfEnums: boolean;
    FCreateIncludeFile: boolean;
    FFilename: string;
    FIsVirtual: boolean;
    FLibname: string;
    FModified: boolean;
    FOutputDirectory: string;
    FOutputExt: string;
    FPalmOSSYSTrap: boolean;
    FPforPointers: boolean;
    FStripComments: boolean;
    FTforTypedefs: boolean;
    FUseExternal: boolean;
    FUseExternalLibname: boolean;
    FVarParams: boolean;
    FWin32Header: boolean;
    function GetCHeaderFileCount: integer;
    function GetCHeaderFiles(Index: integer): TH2PasFile;
    procedure InternalAddCHeaderFile(AFile: TH2PasFile);
    procedure InternalRemoveCHeaderFile(AFile: TH2PasFile);
    procedure SetConstantsInsteadOfEnums(const AValue: boolean);
    procedure SetCreateIncludeFile(const AValue: boolean);
    procedure SetFilename(const AValue: string);
    procedure SetLibname(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure FilenameChanged;
    procedure SetOutputDirectory(const AValue: string);
    procedure SetOutputExt(const AValue: string);
    procedure SetPalmOSSYSTrap(const AValue: boolean);
    procedure SetPforPointers(const AValue: boolean);
    procedure SetStripComments(const AValue: boolean);
    procedure SetTforTypedefs(const AValue: boolean);
    procedure SetUseExternal(const AValue: boolean);
    procedure SetUseExternalLibname(const AValue: boolean);
    procedure SetVarParams(const AValue: boolean);
    procedure SetWin32Header(const AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AProject: TH2PasProject): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    procedure AddFiles(List: TStrings);
    procedure DeleteFiles(List: TStrings);
    function CHeaderFileWithFilename(const AFilename: string): TH2PasFile;
    function CHeaderFileIndexWithFilename(const AFilename: string): integer;
    function ShortenFilename(const AFilename: string): string;
    function LongenFilename(const AFilename: string): string;
    function NormalizeFilename(const AFilename: string): string;
  public
    property CHeaderFileCount: integer read GetCHeaderFileCount;
    property CHeaderFiles[Index: integer]: TH2PasFile read GetCHeaderFiles;
    property Modified: boolean read FModified write SetModified;
    property Filename: string read FFilename write SetFilename;
    property BaseDir: string read FBaseDir;
    property IsVirtual: boolean read FIsVirtual;

    // h2pas options
    property ConstantsInsteadOfEnums: boolean read FConstantsInsteadOfEnums write SetConstantsInsteadOfEnums;
    property CreateIncludeFile: boolean read FCreateIncludeFile write SetCreateIncludeFile;
    property Libname: string read FLibname write SetLibname;
    property OutputExt: string read FOutputExt write SetOutputExt;
    property PalmOSSYSTrap: boolean read FPalmOSSYSTrap write SetPalmOSSYSTrap;
    property PforPointers: boolean read FPforPointers write SetPforPointers;
    property StripComments: boolean read FStripComments write SetStripComments;
    property TforTypedefs: boolean read FTforTypedefs write SetTforTypedefs;
    property UseExternal: boolean read FUseExternal write SetUseExternal;
    property UseExternalLibname: boolean read FUseExternalLibname write SetUseExternalLibname;
    property VarParams: boolean read FVarParams write SetVarParams;
    property Win32Header: boolean read FWin32Header write SetWin32Header;
    property OutputDirectory: string read FOutputDirectory write SetOutputDirectory;
  end;
  
  { TH2PasConverter }

  TH2PasConverter = class(TPersistent)
  private
    FAutoOpenLastProject: boolean;
    Fh2pasFilename: string;
    FModified: boolean;
    FProject: TH2PasProject;
    FProjectHistory: TStrings;
    FWindowSize: TPoint;
    function GetCurrentProjectFilename: string;
    procedure SetAutoOpenLastProject(const AValue: boolean);
    procedure SetCurrentProjectFilename(const AValue: string);
    procedure SetProject(const AValue: TH2PasProject);
    procedure SetProjectHistory(const AValue: TStrings);
    procedure SetWindowSize(const AValue: TPoint);
    procedure Seth2pasFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    function IsEqual(AConverter: TH2PasConverter): boolean;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    procedure LoadProject(const Filename: string);
    procedure SaveProject(const Filename: string);
  public
    property Project: TH2PasProject read FProject write SetProject;
    property ProjectHistory: TStrings read FProjectHistory write SetProjectHistory;
    property CurrentProjectFilename: string read GetCurrentProjectFilename
                                            write SetCurrentProjectFilename;
    property WindowSize: TPoint read FWindowSize write SetWindowSize;
    property AutoOpenLastProject: boolean read FAutoOpenLastProject
                                          write SetAutoOpenLastProject;
    property h2pasFilename: string read Fh2pasFilename write Seth2pasFilename;
    property Modified: boolean read FModified write FModified;
  end;

implementation

{ TH2PasFile }

procedure TH2PasFile.SetFilename(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FFilename=NewValue then exit;
  FFilename:=NewValue;
  Modified:=true;
end;

procedure TH2PasFile.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
  Modified:=true;
end;

procedure TH2PasFile.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if FModified and (Project<>nil) then
    Project.Modified:=true;
end;

procedure TH2PasFile.SetProject(const AValue: TH2PasProject);
begin
  if FProject=AValue then exit;
  if FProject<>nil then begin
    FProject.InternalRemoveCHeaderFile(Self);
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    FProject.InternalAddCHeaderFile(Self);
  end;
  Modified:=true;
end;

constructor TH2PasFile.Create;
begin
  Clear;
end;

destructor TH2PasFile.Destroy;
begin
  if FProject<>nil then begin
    Project:=nil;
  end;
  Clear;
  inherited Destroy;
end;

procedure TH2PasFile.Clear;
begin
  FEnabled:=true;
  FFilename:='';
  FModified:=false;
end;

procedure TH2PasFile.Assign(Source: TPersistent);
var
  Src: TH2PasFile;
begin
  if Source is TH2PasFile then begin
    Src:=TH2PasFile(Source);
    if not IsEqual(Src) then begin
      FEnabled:=Src.FEnabled;
      FFilename:=Src.FFilename;
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasFile.IsEqual(AFile: TH2PasFile): boolean;
begin
  Result:=(CompareFilenames(Filename,AFile.Filename)=0)
          and (Enabled=AFile.Enabled);
end;

procedure TH2PasFile.Load(Config: TConfigStorage);
begin
  FEnabled:=Config.GetValue('Enabled/Value',true);
  FFilename:=Config.GetValue('Filename/Value','');
  if Project<>nil then
    FFilename:=Project.NormalizeFilename(FFilename);
  FModified:=false;
end;

procedure TH2PasFile.Save(Config: TConfigStorage);
var
  AFilename: String;
begin
  Config.SetDeleteValue('Enabled/Value',Enabled,true);
  AFilename:=FFilename;
  if Project<>nil then
    AFilename:=Project.ShortenFilename(AFilename);
  Config.SetDeleteValue('Filename/Value',AFilename,'');
  FModified:=false;
end;

{ TH2PasProject }

function TH2PasProject.GetCHeaderFileCount: integer;
begin
  Result:=FCHeaderFiles.Count;
end;

function TH2PasProject.GetCHeaderFiles(Index: integer): TH2PasFile;
begin
  Result:=TH2PasFile(FCHeaderFiles[Index]);
end;

procedure TH2PasProject.InternalAddCHeaderFile(AFile: TH2PasFile);
begin
  FCHeaderFiles.Add(AFile);
end;

procedure TH2PasProject.InternalRemoveCHeaderFile(AFile: TH2PasFile);
begin
  FCHeaderFiles.Remove(AFile);
end;

procedure TH2PasProject.SetConstantsInsteadOfEnums(const AValue: boolean);
begin
  if FConstantsInsteadOfEnums=AValue then exit;
  FConstantsInsteadOfEnums:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetCreateIncludeFile(const AValue: boolean);
begin
  if FCreateIncludeFile=AValue then exit;
  FCreateIncludeFile:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetFilename(const AValue: string);
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if FFilename=NewValue then exit;
  FFilename:=NewValue;
  FilenameChanged;
  Modified:=true;
end;

procedure TH2PasProject.SetLibname(const AValue: string);
begin
  if FLibname=AValue then exit;
  FLibname:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

procedure TH2PasProject.FilenameChanged;
begin
  FIsVirtual:=(FFilename='') or (not FilenameIsAbsolute(FFilename));
  FBaseDir:=ExtractFilePath(FFilename);
end;

procedure TH2PasProject.SetOutputDirectory(const AValue: string);
begin
  if FOutputDirectory=AValue then exit;
  FOutputDirectory:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetOutputExt(const AValue: string);
begin
  if FOutputExt=AValue then exit;
  FOutputExt:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetPalmOSSYSTrap(const AValue: boolean);
begin
  if FPalmOSSYSTrap=AValue then exit;
  FPalmOSSYSTrap:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetPforPointers(const AValue: boolean);
begin
  if FPforPointers=AValue then exit;
  FPforPointers:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetStripComments(const AValue: boolean);
begin
  if FStripComments=AValue then exit;
  FStripComments:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetTforTypedefs(const AValue: boolean);
begin
  if FTforTypedefs=AValue then exit;
  FTforTypedefs:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseExternal(const AValue: boolean);
begin
  if FUseExternal=AValue then exit;
  FUseExternal:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetUseExternalLibname(const AValue: boolean);
begin
  if FUseExternalLibname=AValue then exit;
  FUseExternalLibname:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetVarParams(const AValue: boolean);
begin
  if FVarParams=AValue then exit;
  FVarParams:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetWin32Header(const AValue: boolean);
begin
  if FWin32Header=AValue then exit;
  FWin32Header:=AValue;
  Modified:=true;
end;

constructor TH2PasProject.Create;
begin
  FCHeaderFiles:=TFPList.Create;
  Clear;
end;

destructor TH2PasProject.Destroy;
begin
  Clear;
  FreeAndNil(FCHeaderFiles);
  inherited Destroy;
end;

procedure TH2PasProject.Clear;
begin
  // FFilename is kept
  FConstantsInsteadOfEnums:=true;
  FCreateIncludeFile:=true;
  FLibname:='';
  FOutputExt:='.inc';
  FPalmOSSYSTrap:=false;
  FPforPointers:=true;
  FStripComments:=false;
  FTforTypedefs:=false;
  FUseExternal:=false;
  FUseExternalLibname:=true;
  FVarParams:=false;
  FWin32Header:=false;
  FOutputDirectory:='';
  while CHeaderFileCount>0 do
    CHeaderFiles[CHeaderFileCount-1].Free;
  FModified:=false;
end;

procedure TH2PasProject.Assign(Source: TPersistent);
var
  Src: TH2PasProject;
  i: Integer;
  NewCHeaderFile: TH2PasFile;
begin
  if Source is TH2PasProject then begin
    Src:=TH2PasProject(Source);
    if not IsEqual(Src) then begin
      // FFilename is kept
      FConstantsInsteadOfEnums:=Src.FConstantsInsteadOfEnums;
      FCreateIncludeFile:=Src.FCreateIncludeFile;
      FLibname:=Src.FLibname;
      FOutputExt:=Src.FOutputExt;
      FPalmOSSYSTrap:=Src.FPalmOSSYSTrap;
      FPforPointers:=Src.FPforPointers;
      FStripComments:=Src.FStripComments;
      FTforTypedefs:=Src.FTforTypedefs;
      FUseExternal:=Src.FUseExternal;
      FUseExternalLibname:=Src.FUseExternalLibname;
      FVarParams:=Src.FVarParams;
      FWin32Header:=Src.FWin32Header;
      FOutputDirectory:=Src.FOutputDirectory;
      Clear;
      for i:=0 to Src.CHeaderFileCount-1 do begin
        NewCHeaderFile:=TH2PasFile.Create;
        NewCHeaderFile.Project:=Self;
        NewCHeaderFile.Assign(Src.CHeaderFiles[i]);
      end;
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasProject.IsEqual(AProject: TH2PasProject): boolean;
var
  i: Integer;
begin
  Result:=(AProject.CHeaderFileCount=CHeaderFileCount)
      and (FConstantsInsteadOfEnums=AProject.FConstantsInsteadOfEnums)
      and (FCreateIncludeFile=AProject.FCreateIncludeFile)
      and (FLibname=AProject.FLibname)
      and (FOutputExt=AProject.FOutputExt)
      and (FPalmOSSYSTrap=AProject.FPalmOSSYSTrap)
      and (FPforPointers=AProject.FPforPointers)
      and (FStripComments=AProject.FStripComments)
      and (FTforTypedefs=AProject.FTforTypedefs)
      and (FUseExternal=AProject.FUseExternal)
      and (FUseExternalLibname=AProject.FUseExternalLibname)
      and (FVarParams=AProject.FVarParams)
      and (FWin32Header=AProject.FWin32Header)
      and (FOutputDirectory=AProject.FOutputDirectory);
  if not Result then exit;
  for i:=0 to CHeaderFileCount-1 do
    if not CHeaderFiles[i].IsEqual(AProject.CHeaderFiles[i]) then exit(false);
end;

procedure TH2PasProject.Load(Config: TConfigStorage);
var
  NewCHeaderFileCount: LongInt;
  NewCHeaderFile: TH2PasFile;
  i: Integer;
begin
  // FFilename is not saved
  FConstantsInsteadOfEnums:=Config.GetValue('ConstantsInsteadOfEnums/Value',true);
  FCreateIncludeFile:=Config.GetValue('CreateIncludeFile/Value',true);
  FLibname:=Config.GetValue('Libname/Value','');
  FOutputExt:=Config.GetValue('OutputExt/Value','.inc');
  FPalmOSSYSTrap:=Config.GetValue('PalmOSSYSTrap/Value',false);
  FPforPointers:=Config.GetValue('PforPointers/Value',true);
  FStripComments:=Config.GetValue('StripComments/Value',false);
  FTforTypedefs:=Config.GetValue('TforTypedefs/Value',false);
  FUseExternal:=Config.GetValue('UseExternal/Value',false);
  FUseExternalLibname:=Config.GetValue('UseExternalLibname/Value',true);
  FVarParams:=Config.GetValue('VarParams/Value',false);
  FWin32Header:=Config.GetValue('Win32Header/Value',false);
  FOutputDirectory:=NormalizeFilename(Config.GetValue('OutputDirectory/Value',''));

  // load CHeaderFiles
  Config.AppendBasePath('CHeaderFiles');
  try
    NewCHeaderFileCount:=Config.GetValue('Count',0);
    for i:=0 to NewCHeaderFileCount-1 do begin
      Config.AppendBasePath('File'+IntToStr(i+1));
      try
        NewCHeaderFile:=TH2PasFile.Create;
        NewCHeaderFile.Project:=Self;
        NewCHeaderFile.Load(Config);
      finally
        Config.UndoAppendBasePath;
      end;
    end;
  finally
    Config.UndoAppendBasePath;
  end;

  FModified:=false;
end;

procedure TH2PasProject.Save(Config: TConfigStorage);
var
  i: Integer;
begin
  // FFilename is kept
  Config.SetDeleteValue('ConstantsInsteadOfEnums/Value',FConstantsInsteadOfEnums,true);
  Config.SetDeleteValue('CreateIncludeFile/Value',FCreateIncludeFile,true);
  Config.SetDeleteValue('Libname/Value',FLibname,'');
  Config.SetDeleteValue('OutputExt/Value',FOutputExt,'.inc');
  Config.SetDeleteValue('PalmOSSYSTrap/Value',FPalmOSSYSTrap,false);
  Config.SetDeleteValue('PforPointers/Value',FPforPointers,true);
  Config.SetDeleteValue('StripComments/Value',FStripComments,false);
  Config.SetDeleteValue('TforTypedefs/Value',FTforTypedefs,false);
  Config.SetDeleteValue('UseExternal/Value',FUseExternal,false);
  Config.SetDeleteValue('UseExternalLibname/Value',FUseExternalLibname,true);
  Config.SetDeleteValue('VarParams/Value',FVarParams,false);
  Config.SetDeleteValue('Win32Header/Value',FWin32Header,false);
  Config.SetDeleteValue('OutputDirectory/Value',ShortenFilename(FOutputDirectory),'');

  // save CHeaderFiles
  Config.AppendBasePath('CHeaderFiles');
  try
    Config.SetDeleteValue('Count',CHeaderFileCount,0);
    for i:=0 to CHeaderFileCount-1 do begin
      Config.AppendBasePath('File'+IntToStr(i+1));
      try
        CHeaderFiles[i].Save(Config);
      finally
        Config.UndoAppendBasePath;
      end;
    end;
  finally
    Config.UndoAppendBasePath;
  end;

  FModified:=false;
end;

procedure TH2PasProject.LoadFromFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,true);
  try
    Load(Config);
  finally
    Config.Free;
  end;
end;

procedure TH2PasProject.SaveToFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,false);
  try
    Save(Config);
    DebugLn(['TH2PasProject.SaveToFile ',AFilename]);
    Config.WriteToDisk;
  finally
    Config.Free;
  end;
end;

procedure TH2PasProject.AddFiles(List: TStrings);
var
  i: Integer;
  NewFilename: string;
  NewFile: TH2PasFile;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do begin
    NewFilename:=CleanAndExpandFilename(List[i]);
    if (NewFilename='') or (not FileExists(NewFilename)) then exit;
    if CHeaderFileWithFilename(NewFilename)<>nil then exit;
    NewFile:=TH2PasFile.Create;
    NewFile.Project:=Self;
    NewFile.Filename:=NewFilename;
  end;
end;

procedure TH2PasProject.DeleteFiles(List: TStrings);
var
  i: Integer;
  NewFilename: String;
  CurFile: TH2PasFile;
begin
  if List=nil then exit;
  for i:=0 to List.Count-1 do begin
    NewFilename:=CleanAndExpandFilename(List[i]);
    if (NewFilename='') then exit;
    CurFile:=CHeaderFileWithFilename(NewFilename);
    if CurFile<>nil then begin
      CurFile.Free;
    end;
  end;
end;

function TH2PasProject.CHeaderFileWithFilename(const AFilename: string
  ): TH2PasFile;
var
  i: LongInt;
begin
  i:=CHeaderFileIndexWithFilename(AFilename);
  if i>=0 then
    Result:=CHeaderFiles[i]
  else
    Result:=nil;
end;

function TH2PasProject.CHeaderFileIndexWithFilename(const AFilename: string
  ): integer;
begin
  Result:=CHeaderFileCount-1;
  while (Result>=0)
  and (CompareFilenames(AFilename,CHeaderFiles[Result].Filename)<>0) do
    dec(Result);
end;

function TH2PasProject.ShortenFilename(const AFilename: string): string;
begin
  if IsVirtual then
    Result:=AFilename
  else
    Result:=CreateRelativePath(AFilename,fBaseDir);
end;

function TH2PasProject.LongenFilename(const AFilename: string): string;
begin
  if IsVirtual then
    Result:=AFilename
  else if not FilenameIsAbsolute(AFilename) then
    Result:=TrimFilename(BaseDir+AFilename);
end;

function TH2PasProject.NormalizeFilename(const AFilename: string): string;
begin
  Result:=LongenFilename(SetDirSeparators(AFilename));
end;

{ TH2PasConverter }

function TH2PasConverter.GetCurrentProjectFilename: string;
begin
  if FProjectHistory.Count>0 then
    Result:=FProjectHistory[FProjectHistory.Count-1]
  else
    Result:='';
end;

procedure TH2PasConverter.SetAutoOpenLastProject(const AValue: boolean);
begin
  if FAutoOpenLastProject=AValue then exit;
  FAutoOpenLastProject:=AValue;
  Modified:=true;
end;

procedure TH2PasConverter.SetCurrentProjectFilename(const AValue: string);
const
  ProjectHistoryMax=30;
var
  NewValue: String;
begin
  NewValue:=TrimFilename(AValue);
  if NewValue='' then exit;
  if CompareFilenames(GetCurrentProjectFilename,NewValue)=0 then exit;
  FProjectHistory.Add(NewValue);
  while FProjectHistory.Count>ProjectHistoryMax do
    FProjectHistory.Delete(0);
  Modified:=true;
end;

procedure TH2PasConverter.SetProject(const AValue: TH2PasProject);
begin
  if FProject=AValue then exit;
  FProject:=AValue;
  if FProject<>nil then begin
    if FProject.Filename<>'' then
      CurrentProjectFilename:=FProject.Filename;
  end;
end;

procedure TH2PasConverter.SetProjectHistory(const AValue: TStrings);
begin
  if FProjectHistory=AValue then exit;
  FProjectHistory.Assign(AValue);
end;

procedure TH2PasConverter.SetWindowSize(const AValue: TPoint);
begin
  if ComparePoints(FWindowSize,AValue)=0 then exit;
  FWindowSize:=AValue;
  Modified:=true;
end;

procedure TH2PasConverter.Seth2pasFilename(const AValue: string);
begin
  if Fh2pasFilename=AValue then exit;
  Fh2pasFilename:=AValue;
  Modified:=true;
end;

constructor TH2PasConverter.Create;
begin
  FProjectHistory:=TStringList.Create;
  Clear;
end;

destructor TH2PasConverter.Destroy;
begin
  FreeAndNil(FProject);
  Clear;
  inherited Destroy;
end;

procedure TH2PasConverter.Clear;
begin
  FAutoOpenLastProject:=true;
  if FProject<>nil then FreeAndNil(FProject);
  FProjectHistory.Clear;
  FWindowSize:=Point(0,0);
  Fh2pasFilename:='h2pas';
  FModified:=false;
end;

procedure TH2PasConverter.Assign(Source: TPersistent);
var
  Src: TH2PasConverter;
begin
  if Source is TH2PasConverter then begin
    Src:=TH2PasConverter(Source);
    if not IsEqual(Src) then begin
      Clear;
      // Note: project is kept unchanged
      FProjectHistory.Assign(Src.FProjectHistory);
      FWindowSize:=Src.FWindowSize;
      Fh2pasFilename:=Src.Fh2pasFilename;
      Modified:=true;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

function TH2PasConverter.IsEqual(AConverter: TH2PasConverter): boolean;
begin
  if (FAutoOpenLastProject<>AConverter.FAutoOpenLastProject)
  or (ComparePoints(FWindowSize,AConverter.FWindowSize)<>0)
  or (Fh2pasFilename<>AConverter.h2pasFilename)
  or (not FProjectHistory.Equals(AConverter.FProjectHistory))
  then
    exit(false);
  Result:=true;
end;

procedure TH2PasConverter.Load(Config: TConfigStorage);
begin
  FAutoOpenLastProject:=Config.GetValue('AutoOpenLastProject/Value',true);
  Fh2pasFilename:=Config.GetValue('h2pas/Filename','h2pas');
  Config.GetValue('WindowSize/',FWindowSize,Point(0,0));
  Config.GetValue('ProjectHistory/',FProjectHistory);
  // Note: project is saved in its own file
end;

procedure TH2PasConverter.Save(Config: TConfigStorage);
begin
  Config.SetDeleteValue('AutoOpenLastProject/Value',FAutoOpenLastProject,true);
  Config.SetDeleteValue('h2pas/Filename',Fh2pasFilename,'h2pas');
  Config.SetDeleteValue('WindowSize/',FWindowSize,Point(0,0));
  Config.SetValue('ProjectHistory/',FProjectHistory);
end;

procedure TH2PasConverter.LoadFromFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,true);
  try
    Load(Config);
  finally
    Config.Free;
  end;
end;

procedure TH2PasConverter.SaveToFile(const AFilename: string);
var
  Config: TXMLConfigStorage;
begin
  Config:=TXMLConfigStorage.Create(AFilename,false);
  try
    Save(Config);
    Config.WriteToDisk;
  finally
    Config.Free;
  end;
end;

procedure TH2PasConverter.LoadProject(const Filename: string);
begin
  DebugLn(['TH2PasConverter.LoadProject ',Filename]);
  if FProject=nil then
    FProject:=TH2PasProject.Create;
  FProject.Filename:=Filename;
  FProject.LoadFromFile(Filename);
  CurrentProjectFilename:=Filename;
end;

procedure TH2PasConverter.SaveProject(const Filename: string);
begin
  DebugLn(['TH2PasConverter.SaveProject ',Filename]);
  FProject.Filename:=Filename;
  FProject.SaveToFile(Filename);
  CurrentProjectFilename:=Filename;
end;

end.


