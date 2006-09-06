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
  Classes, SysUtils, LCLProc, LResources, LazConfigStorage, XMLPropStorage,
  Forms, Controls, Dialogs, FileUtil, FileProcs,
  TextTools, IDEExternToolIntf, IDEDialogs, LazIDEIntf, IDEMsgIntf,
  IDETextConverter;
  
type
  TH2PasProject = class;
  TH2PasConverter = class;

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
    function GetOutputFilename: string;
    function GetOutputDirectory: string;
    function GetOutputExtension: string;
    function GetH2PasParameters(const InputFilename: string = ''): string;
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
    FCompactOutputmode: boolean;
    FConstantsInsteadOfEnums: boolean;
    FConverter: TH2PasConverter;
    FCreateIncludeFile: boolean;
    FFilename: string;
    FIsVirtual: boolean;
    FLibname: string;
    FModified: boolean;
    FOutputDirectory: string;
    FOutputExt: string;
    FPackAllRecords: boolean;
    FPalmOSSYSTrap: boolean;
    FPforPointers: boolean;
    FPreH2PasTools: TComponent;
    FStripComments: boolean;
    FStripCommentsAndInfo: boolean;
    FTforTypedefs: boolean;
    FTforTypedefsRemoveUnderscore: boolean;
    FUseExternal: boolean;
    FUseExternalLibname: boolean;
    FUseProcVarsForImport: boolean;
    FVarParams: boolean;
    FWin32Header: boolean;
    function GetCHeaderFileCount: integer;
    function GetCHeaderFiles(Index: integer): TH2PasFile;
    procedure InternalAddCHeaderFile(AFile: TH2PasFile);
    procedure InternalRemoveCHeaderFile(AFile: TH2PasFile);
    procedure SetCompactOutputmode(const AValue: boolean);
    procedure SetConstantsInsteadOfEnums(const AValue: boolean);
    procedure SetCreateIncludeFile(const AValue: boolean);
    procedure SetFilename(const AValue: string);
    procedure SetLibname(const AValue: string);
    procedure SetModified(const AValue: boolean);
    procedure FilenameChanged;
    procedure SetOutputDirectory(const AValue: string);
    procedure SetOutputExt(const AValue: string);
    procedure SetPackAllRecords(const AValue: boolean);
    procedure SetPalmOSSYSTrap(const AValue: boolean);
    procedure SetPforPointers(const AValue: boolean);
    procedure SetStripComments(const AValue: boolean);
    procedure SetStripCommentsAndInfo(const AValue: boolean);
    procedure SetTforTypedefs(const AValue: boolean);
    procedure SetTforTypedefsRemoveUnderscore(const AValue: boolean);
    procedure SetUseExternal(const AValue: boolean);
    procedure SetUseExternalLibname(const AValue: boolean);
    procedure SetUseProcVarsForImport(const AValue: boolean);
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
    function HasEnabledFiles: boolean;
  public
    property CHeaderFileCount: integer read GetCHeaderFileCount;
    property CHeaderFiles[Index: integer]: TH2PasFile read GetCHeaderFiles;
    property Modified: boolean read FModified write SetModified;
    property Filename: string read FFilename write SetFilename;
    property BaseDir: string read FBaseDir;
    property IsVirtual: boolean read FIsVirtual;
    property Converter: TH2PasConverter read FConverter;
    property PreH2PasTools: TComponent read FPreH2PasTools;

    // h2pas options
    property ConstantsInsteadOfEnums: boolean read FConstantsInsteadOfEnums write SetConstantsInsteadOfEnums;
    property CompactOutputmode: boolean read FCompactOutputmode write SetCompactOutputmode;
    property CreateIncludeFile: boolean read FCreateIncludeFile write SetCreateIncludeFile;
    property Libname: string read FLibname write SetLibname;
    property OutputExt: string read FOutputExt write SetOutputExt;
    property PalmOSSYSTrap: boolean read FPalmOSSYSTrap write SetPalmOSSYSTrap;
    property PforPointers: boolean read FPforPointers write SetPforPointers;
    property PackAllRecords: boolean read FPackAllRecords write SetPackAllRecords;
    property StripComments: boolean read FStripComments write SetStripComments;
    property StripCommentsAndInfo: boolean read FStripCommentsAndInfo write SetStripCommentsAndInfo;
    property TforTypedefs: boolean read FTforTypedefs write SetTforTypedefs;
    property TforTypedefsRemoveUnderscore: boolean read FTforTypedefsRemoveUnderscore write SetTforTypedefsRemoveUnderscore;
    property UseExternal: boolean read FUseExternal write SetUseExternal;
    property UseExternalLibname: boolean read FUseExternalLibname write SetUseExternalLibname;
    property UseProcVarsForImport: boolean read FUseProcVarsForImport write SetUseProcVarsForImport;
    property VarParams: boolean read FVarParams write SetVarParams;
    property Win32Header: boolean read FWin32Header write SetWin32Header;
    property OutputDirectory: string read FOutputDirectory write SetOutputDirectory;
  end;
  
  { TH2PasTool }

  TH2PasTool = class(TIDEExternalToolOptions)
  private
    FH2PasFile: TH2PasFile;
    FTargetFilename: string;
  public
    property H2PasFile: TH2PasFile read FH2PasFile write FH2PasFile;
    property TargetFilename: string read FTargetFilename write FTargetFilename;
  end;
  
  { TH2PasConverter }

  TH2PasConverter = class(TPersistent)
  private
    FAutoOpenLastProject: boolean;
    FExecuting: boolean;
    Fh2pasFilename: string;
    FLastUsedFilename: string;
    FModified: boolean;
    FProject: TH2PasProject;
    FProjectHistory: TStrings;
    FWindowBounds: TRect;
    function GetCurrentProjectFilename: string;
    procedure SetAutoOpenLastProject(const AValue: boolean);
    procedure SetCurrentProjectFilename(const AValue: string);
    procedure SetProject(const AValue: TH2PasProject);
    procedure SetProjectHistory(const AValue: TStrings);
    procedure SetWindowBounds(const AValue: TRect);
    procedure Seth2pasFilename(const AValue: string);
    procedure OnParseH2PasLine(Sender: TObject; Line: TIDEScanMessageLine);
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
    function Execute: TModalResult;
    function ConvertFile(AFile: TH2PasFile): TModalResult;
    function GetH2PasFilename: string;
    function FindH2PasErrorMessage: integer;
    function GetH2PasErrorPostion(const Line: string;
                                  out aFilename: string;
                                  out LineNumber, Column: integer): boolean;
    function FileIsRelated(const aFilename: string): Boolean;
  public
    property Project: TH2PasProject read FProject write SetProject;
    property ProjectHistory: TStrings read FProjectHistory write SetProjectHistory;
    property CurrentProjectFilename: string read GetCurrentProjectFilename
                                            write SetCurrentProjectFilename;
    property WindowBounds: TRect read FWindowBounds write SetWindowBounds;
    property AutoOpenLastProject: boolean read FAutoOpenLastProject
                                          write SetAutoOpenLastProject;
    property h2pasFilename: string read Fh2pasFilename write Seth2pasFilename;
    property Modified: boolean read FModified write FModified;
    property Executing: boolean read FExecuting;
    property LastUsedFilename: string read FLastUsedFilename;
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

function TH2PasFile.GetOutputFilename: string;
begin
  Result:=GetOutputDirectory+ExtractFileNameOnly(Filename)+GetOutputExtension;
end;

function TH2PasFile.GetOutputDirectory: string;
begin
  Result:=Project.OutputDirectory;
  if Result='' then
    Result:=Project.BaseDir;
end;

function TH2PasFile.GetOutputExtension: string;
begin
  Result:=Project.OutputExt;
end;

function TH2PasFile.GetH2PasParameters(const InputFilename: string): string;

  procedure Add(const AnOption: string);
  begin
    if Result<>'' then
      Result:=Result+' ';
    Result:=Result+AnOption;
  end;

begin
  Result:='';
  if Project.ConstantsInsteadOfEnums then Add('-e');
  if Project.CompactOutputmode then Add('-c');
  if Project.CreateIncludeFile then Add('-i');
  if Project.PalmOSSYSTrap then Add('-x');
  if Project.PforPointers then Add('-p');
  if Project.PackAllRecords then Add('-pr');
  if Project.StripComments then Add('-s');
  if Project.StripCommentsAndInfo then Add('-S');
  if Project.TforTypedefs then Add('-t');
  if Project.TforTypedefsRemoveUnderscore then Add('-T');
  if Project.UseExternal then Add('-d');
  if Project.UseExternalLibname then Add('-D');
  if Project.UseProcVarsForImport then Add('-P');
  if Project.VarParams then Add('-v');
  if Project.Win32Header then Add('-w');
  if Project.Libname<>'' then Add('-l '+Project.Libname);
  Add('-o '+GetOutputFilename);
  if InputFilename<>'' then
    Add(InputFilename)
  else
    Add(Filename);
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

procedure TH2PasProject.SetCompactOutputmode(const AValue: boolean);
begin
  if FCompactOutputmode=AValue then exit;
  FCompactOutputmode:=AValue;
  Modified:=true;
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

procedure TH2PasProject.SetPackAllRecords(const AValue: boolean);
begin
  if FPackAllRecords=AValue then exit;
  FPackAllRecords:=AValue;
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

procedure TH2PasProject.SetStripCommentsAndInfo(const AValue: boolean);
begin
  if FStripCommentsAndInfo=AValue then exit;
  FStripCommentsAndInfo:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetTforTypedefs(const AValue: boolean);
begin
  if FTforTypedefs=AValue then exit;
  FTforTypedefs:=AValue;
  Modified:=true;
end;

procedure TH2PasProject.SetTforTypedefsRemoveUnderscore(const AValue: boolean);
begin
  if FTforTypedefsRemoveUnderscore=AValue then exit;
  FTforTypedefsRemoveUnderscore:=AValue;
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

procedure TH2PasProject.SetUseProcVarsForImport(const AValue: boolean);
begin
  if FUseProcVarsForImport=AValue then exit;
  FUseProcVarsForImport:=AValue;
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
  FPreH2PasTools:=TComponent.Create(nil);
  Clear;
end;

destructor TH2PasProject.Destroy;
begin
  Clear;
  if (Converter<>nil) and (Converter.Project=Self) then
    Converter.Project:=nil;
  FreeAndNil(FCHeaderFiles);
  inherited Destroy;
end;

procedure TH2PasProject.Clear;
begin
  // FFilename is kept
  FConstantsInsteadOfEnums:=true;
  FCompactOutputmode:=false;
  FCreateIncludeFile:=true;
  FLibname:='';
  FOutputExt:='.inc';
  FPackAllRecords:=false;
  FPalmOSSYSTrap:=false;
  FPforPointers:=true;
  FStripComments:=false;
  FStripCommentsAndInfo:=false;
  FTforTypedefs:=false;
  FTforTypedefsRemoveUnderscore:=false;
  FUseExternal:=false;
  FUseExternalLibname:=true;
  FUseProcVarsForImport:=false;
  FVarParams:=false;
  FWin32Header:=true;
  FOutputDirectory:='';
  while CHeaderFileCount>0 do
    CHeaderFiles[CHeaderFileCount-1].Free;
  FPreH2PasTools.Free;
  FPreH2PasTools:=TComponent.Create(nil);
  FModified:=false;
end;

procedure TH2PasProject.Assign(Source: TPersistent);
var
  Src: TH2PasProject;
  i: Integer;
  NewCHeaderFile: TH2PasFile;
  SrcComponent: TComponent;
  NewComponent: TObject;
begin
  if Source is TH2PasProject then begin
    Src:=TH2PasProject(Source);
    if not IsEqual(Src) then begin
      // FFilename is kept
      FConstantsInsteadOfEnums:=Src.FConstantsInsteadOfEnums;
      FCompactOutputmode:=Src.FCompactOutputmode;
      FCreateIncludeFile:=Src.FCreateIncludeFile;
      FLibname:=Src.FLibname;
      FOutputExt:=Src.FOutputExt;
      FPackAllRecords:=Src.FPackAllRecords;
      FPalmOSSYSTrap:=Src.FPalmOSSYSTrap;
      FPforPointers:=Src.FPforPointers;
      FStripComments:=Src.FStripComments;
      FStripCommentsAndInfo:=Src.FStripCommentsAndInfo;
      FTforTypedefs:=Src.FTforTypedefs;
      FTforTypedefsRemoveUnderscore:=Src.FTforTypedefsRemoveUnderscore;
      FUseExternal:=Src.FUseExternal;
      FUseExternalLibname:=Src.FUseExternalLibname;
      FUseProcVarsForImport:=Src.FUseProcVarsForImport;
      FVarParams:=Src.FVarParams;
      FWin32Header:=Src.FWin32Header;
      FOutputDirectory:=Src.FOutputDirectory;
      Clear;
      for i:=0 to Src.CHeaderFileCount-1 do begin
        NewCHeaderFile:=TH2PasFile.Create;
        NewCHeaderFile.Project:=Self;
        NewCHeaderFile.Assign(Src.CHeaderFiles[i]);
      end;
      FPreH2PasTools.Free;
      FPreH2PasTools:=TComponent.Create(nil);
      for i:=0 to Src.FPreH2PasTools.ComponentCount-1 do begin
        SrcComponent:=Src.FPreH2PasTools.Components[i];
        if SrcComponent is TCustomTextConverterTool then begin
          NewComponent:=
                 TComponentClass(SrcComponent.ClassType).Create(FPreH2PasTools);
          TCustomTextConverterTool(NewComponent).Assign(SrcComponent);
        end;
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
      and (FCompactOutputmode=AProject.FCompactOutputmode)
      and (FCreateIncludeFile=AProject.FCreateIncludeFile)
      and (FLibname=AProject.FLibname)
      and (FOutputExt=AProject.FOutputExt)
      and (FPackAllRecords=AProject.FPackAllRecords)
      and (FPalmOSSYSTrap=AProject.FPalmOSSYSTrap)
      and (FPforPointers=AProject.FPforPointers)
      and (FStripComments=AProject.FStripComments)
      and (FStripCommentsAndInfo=AProject.FStripCommentsAndInfo)
      and (FTforTypedefs=AProject.FTforTypedefs)
      and (FTforTypedefsRemoveUnderscore=AProject.FTforTypedefsRemoveUnderscore)
      and (FUseExternal=AProject.FUseExternal)
      and (FUseExternalLibname=AProject.FUseExternalLibname)
      and (FUseProcVarsForImport=AProject.FUseProcVarsForImport)
      and (FVarParams=AProject.FVarParams)
      and (FWin32Header=AProject.FWin32Header)
      and (FOutputDirectory=AProject.FOutputDirectory);
  if not Result then exit;
  for i:=0 to CHeaderFileCount-1 do
    if not CHeaderFiles[i].IsEqual(AProject.CHeaderFiles[i]) then
      exit(false);
  if not CompareComponents(FPreH2PasTools,AProject.FPreH2PasTools) then
    exit(false);
end;

procedure TH2PasProject.Load(Config: TConfigStorage);
var
  NewCount: LongInt;
  i: Integer;
  NewCHeaderFile: TH2PasFile;
  NewComponent: TComponent;
begin
  Clear;
  
  // FFilename is not saved
  FConstantsInsteadOfEnums:=Config.GetValue('ConstantsInsteadOfEnums/Value',true);
  FCompactOutputmode:=Config.GetValue('CompactOutputmode/Value',false);
  FCreateIncludeFile:=Config.GetValue('CreateIncludeFile/Value',true);
  FLibname:=Config.GetValue('Libname/Value','');
  FOutputExt:=Config.GetValue('OutputExt/Value','.inc');
  FPackAllRecords:=Config.GetValue('PackAllRecords/Value',false);
  FPalmOSSYSTrap:=Config.GetValue('PalmOSSYSTrap/Value',false);
  FPforPointers:=Config.GetValue('PforPointers/Value',true);
  FStripComments:=Config.GetValue('StripComments/Value',false);
  FStripCommentsAndInfo:=Config.GetValue('StripCommentsAndInfo/Value',false);
  FTforTypedefs:=Config.GetValue('TforTypedefs/Value',false);
  FTforTypedefsRemoveUnderscore:=Config.GetValue('TforTypedefsRemoveUnderscore/Value',false);
  FUseExternal:=Config.GetValue('UseExternal/Value',false);
  FUseExternalLibname:=Config.GetValue('UseExternalLibname/Value',true);
  FUseProcVarsForImport:=Config.GetValue('UseProcVarsForImport/Value',false);
  FVarParams:=Config.GetValue('VarParams/Value',false);
  FWin32Header:=Config.GetValue('Win32Header/Value',true);
  FOutputDirectory:=NormalizeFilename(Config.GetValue('OutputDirectory/Value',''));

  // load CHeaderFiles
  Config.AppendBasePath('CHeaderFiles');
  try
    NewCount:=Config.GetValue('Count',0);
    for i:=0 to NewCount-1 do begin
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

  // load PreH2PasTools
  Config.AppendBasePath('PreH2PasTools');
  try
    NewCount:=Config.GetValue('Count',0);
    for i:=0 to NewCount-1 do begin
      Config.AppendBasePath('Tool'+IntToStr(i+1));
      try
        NewComponent:=nil;
        LoadComponentFromConfig(Config,'Value',NewComponent,
                            @TextConverterToolClasses.FindClass,FPreH2PasTools);
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
  Config.SetDeleteValue('CompactOutputmode/Value',FCompactOutputmode,false);
  Config.SetDeleteValue('CreateIncludeFile/Value',FCreateIncludeFile,true);
  Config.SetDeleteValue('Libname/Value',FLibname,'');
  Config.SetDeleteValue('OutputExt/Value',FOutputExt,'.inc');
  Config.SetDeleteValue('PackAllRecords/Value',FPackAllRecords,false);
  Config.SetDeleteValue('PalmOSSYSTrap/Value',FPalmOSSYSTrap,false);
  Config.SetDeleteValue('PforPointers/Value',FPforPointers,true);
  Config.SetDeleteValue('StripComments/Value',FStripComments,false);
  Config.SetDeleteValue('StripCommentsAndInfo/Value',FStripCommentsAndInfo,false);
  Config.SetDeleteValue('TforTypedefs/Value',FTforTypedefs,false);
  Config.SetDeleteValue('TforTypedefsRemoveUnderscore/Value',FTforTypedefsRemoveUnderscore,false);
  Config.SetDeleteValue('UseExternal/Value',FUseExternal,false);
  Config.SetDeleteValue('UseExternalLibname/Value',FUseExternalLibname,true);
  Config.SetDeleteValue('UseProcVarsForImport/Value',FUseProcVarsForImport,false);
  Config.SetDeleteValue('VarParams/Value',FVarParams,false);
  Config.SetDeleteValue('Win32Header/Value',FWin32Header,true);
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
  
  Config.AppendBasePath('PreH2PasTools');
  try
    Config.SetDeleteValue('Count',FPreH2PasTools.ComponentCount,0);
    for i:=0 to FPreH2PasTools.ComponentCount-1 do begin
      Config.AppendBasePath('Tool'+IntToStr(i+1));
      try
        SaveComponentToConfig(Config,'Value',FPreH2PasTools.Components[i]);
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

function TH2PasProject.HasEnabledFiles: boolean;
var
  i: Integer;
begin
  for i:=0 to CHeaderFileCount-1 do
    if CHeaderFiles[i].Enabled then exit(true);
  Result:=false;
end;

{ TH2PasConverter }

procedure TH2PasConverter.OnParseH2PasLine(Sender: TObject;
  Line: TIDEScanMessageLine);
var
  Tool: TH2PasTool;
  LineNumber: String;
  MsgType: String;
  Msg: String;
begin
  if Line.Tool is TH2PasTool then begin
    Tool:=TH2PasTool(Line.Tool);
    if REMatches(Line.Line,'^at line ([0-9]+) (error) : (.*)$') then begin
      LineNumber:=REVar(1);
      MsgType:=REVar(2);
      Msg:=REVar(3);
      Line.Line:=Tool.TargetFilename+'('+LineNumber+') '+MsgType+': '+Msg;
    end;
    //DebugLn(['TH2PasConverter.OnParseH2PasLine ',Line.Line]);
  end;
end;

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
  if FProject<>nil then begin
    FProject.fConverter:=nil;
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    FProject.fConverter:=Self;
    if FProject.Filename<>'' then
      CurrentProjectFilename:=FProject.Filename;
  end;
end;

procedure TH2PasConverter.SetProjectHistory(const AValue: TStrings);
begin
  if FProjectHistory=AValue then exit;
  FProjectHistory.Assign(AValue);
end;

procedure TH2PasConverter.SetWindowBounds(const AValue: TRect);
begin
  if CompareRect(@FWindowBounds,@AValue) then exit;
  FWindowBounds:=AValue;
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
  FWindowBounds:=Rect(0,0,0,0);
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
      FWindowBounds:=Src.FWindowBounds;
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
  or (not CompareRect(@FWindowBounds,@AConverter.FWindowBounds))
  or (Fh2pasFilename<>AConverter.h2pasFilename)
  or (not FProjectHistory.Equals(AConverter.FProjectHistory))
  then
    exit(false);
  Result:=true;
end;

procedure TH2PasConverter.Load(Config: TConfigStorage);
var
  i: Integer;
begin
  FAutoOpenLastProject:=Config.GetValue('AutoOpenLastProject/Value',true);
  Fh2pasFilename:=Config.GetValue('h2pas/Filename','h2pas');
  Config.GetValue('WindowBounds/',FWindowBounds,Rect(0,0,0,0));
  Config.GetValue('ProjectHistory/',FProjectHistory);
  for i:=FProjectHistory.Count-1 downto 0 do
    if FProjectHistory[i]='' then FProjectHistory.Delete(i);
  
  // Note: project is saved in its own file
end;

procedure TH2PasConverter.Save(Config: TConfigStorage);
begin
  Config.SetDeleteValue('AutoOpenLastProject/Value',FAutoOpenLastProject,true);
  Config.SetDeleteValue('h2pas/Filename',Fh2pasFilename,'h2pas');
  Config.SetDeleteValue('WindowBounds/',FWindowBounds,Rect(0,0,0,0));
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

function TH2PasConverter.Execute: TModalResult;
var
  i: Integer;
  AFile: TH2PasFile;
  CurResult: TModalResult;
begin
  if FExecuting then begin
    DebugLn(['TH2PasConverter.Execute FAILED: Already executing']);
    exit(mrCancel);
  end;

  Result:=mrOK;
  FExecuting:=true;
  try
    FLastUsedFilename:='';
    // convert every c header file
    for i:=0 to Project.CHeaderFileCount-1 do begin
      AFile:=Project.CHeaderFiles[i];
      if not AFile.Enabled then continue;
      CurResult:=ConvertFile(AFile);
      if CurResult=mrAbort then begin
        DebugLn(['TH2PasConverter.Execute aborted on file ',AFile.Filename]);
        exit(mrAbort);
      end;
      if CurResult<>mrOK then Result:=mrCancel;
    end;
  finally
    FExecuting:=false;
  end;
end;

function TH2PasConverter.ConvertFile(AFile: TH2PasFile): TModalResult;
var
  OutputFilename: String;
  TempCHeaderFilename: String;
  InputFilename: String;
  Tool: TH2PasTool;
  TextConverter: TIDETextConverter;
begin
  Result:=mrCancel;
  FLastUsedFilename:='';

  // check if file exists
  InputFilename:=AFile.Filename;
  if not FileExistsCached(InputFilename) then begin
    Result:=IDEMessageDialog('File not found',
      'C header file "'+InputFilename+'" not found',
      mtError,[mbCancel,mbAbort],'');
    exit;
  end;

  OutputFilename:=AFile.GetOutputFilename;
  TempCHeaderFilename:=ChangeFileExt(OutputFilename,'.tmp.h');
  if not CopyFile(InputFilename,TempCHeaderFilename) then begin
    Result:=IDEMessageDialog('Copying file failed',
      'Unable to copy file "'+InputFilename+'"'#13
      +'to "'+TempCHeaderFilename+'"',
      mtError,[mbCancel,mbAbort],'');
    exit;
  end;

  TextConverter:=TIDETextConverter.Create(nil);
  try
    TextConverter.Filename:=TempCHeaderFilename;
    FLastUsedFilename:=TextConverter.Filename;
    TextConverter.LoadFromFile(InputFilename);
    DebugLn(['TH2PasConverter.ConvertFile TempCHeaderFilename="',TempCHeaderFilename,'" CurrentType=',ord(TextConverter.CurrentType),' FileSize=',FileSize(TempCHeaderFilename)]);

    // run converters for .h file to make it compatible for h2pas
    Result:=TextConverter.Execute(Project.PreH2PasTools);
    if Result<>mrOk then begin
      DebugLn(['TH2PasConverter.ConvertFile Failed running Project.PreH2PasTools on ',TempCHeaderFilename]);
      exit;
    end;

    // run h2pas
    Tool:=TH2PasTool.Create;
    try
      Tool.Title:='h2pas';
      Tool.H2PasFile:=AFile;
    DebugLn(['TH2PasConverter.ConvertFile AAA TempCHeaderFilename="',TempCHeaderFilename,'" CurrentType=',ord(TextConverter.CurrentType),' FileSize=',FileSize(TempCHeaderFilename)]);
      Tool.TargetFilename:=TextConverter.Filename;
    DebugLn(['TH2PasConverter.ConvertFile BBB TempCHeaderFilename="',TempCHeaderFilename,'" CurrentType=',ord(TextConverter.CurrentType),' FileSize=',FileSize(TempCHeaderFilename)]);
      Tool.Filename:=GetH2PasFilename;
      Tool.CmdLineParams:=AFile.GetH2PasParameters(Tool.TargetFilename);
      Tool.ScanOutput:=true;
      Tool.ShowAllOutput:=true;
      Tool.WorkingDirectory:=Project.BaseDir;
      Tool.OnParseLine:=@OnParseH2PasLine;
      DebugLn(['TH2PasConverter.ConvertFile Tool.Filename="',Tool.Filename,'" Tool.CmdLineParams="',Tool.CmdLineParams,'"']);
      Result:=RunExternalTool(Tool);
      if Result<>mrOk then exit(mrAbort);
      if FindH2PasErrorMessage>=0 then exit(mrAbort);
    finally
      Tool.Free;
    end;

    // TODO: beautify unit

  finally
    TextConverter.Free;
  end;

  Result:=mrOk;
end;

function TH2PasConverter.GetH2PasFilename: string;
begin
  Result:=FindDefaultExecutablePath(h2pasFilename);
end;

function TH2PasConverter.FindH2PasErrorMessage: integer;
var
  i: Integer;
  Line: TIDEMessageLine;
begin
  for i:=0 to IDEMessagesWindow.LinesCount-1 do begin
    Line:=IDEMessagesWindow.Lines[i];
    if REMatches(Line.Msg,'^(.*)\([0-9]+\)') then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

function TH2PasConverter.GetH2PasErrorPostion(const Line: string;
  out aFilename: string; out LineNumber, Column: integer): boolean;
begin
  Result:=REMatches(Line,'^(.*)\(([0-9]+)\)');
  if Result then begin
    aFilename:=REVar(1);
    LineNumber:=StrToIntDef(REVar(2),-1);
    Column:=1;
  end else begin
    aFilename:='';
    LineNumber:=-1;
    Column:=-1;
  end;
end;

function TH2PasConverter.FileIsRelated(const aFilename: string): Boolean;
begin
  Result:=(CompareFilenames(AFilename,LastUsedFilename)=0)
      or ((Project<>nil) and (Project.CHeaderFileWithFilename(aFilename)<>nil));
end;

end.



