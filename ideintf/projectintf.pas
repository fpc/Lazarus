{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    IDE interface to the IDE projects.
}
unit ProjectIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileCtrl;
  
const
  FileDescNamePascalUnit = 'unit';
  FileDescNameLCLForm = 'form';
  FileDescNameDatamodule = 'datamodule';
  FileDescNameText = 'text';

type
  { TProjectFileDescriptor }

  TProjectFileDescriptor = class(TPersistent)
  private
    FAddToProject: boolean;
    FDefaultFileExt: string;
    FDefaultFilename: string;
    FDefaultResFileExt: string;
    FDefaultResourceName: string;
    FDefaultSourceName: string;
    FIsComponent: boolean;
    FIsPascalUnit: boolean;
    FName: string;
    FReferenceCount: integer;
    FResourceClass: TPersistentClass;
    FRequiredPackages: string;
    FUseCreateFormStatements: boolean;
    FVisibleInNewDialog: boolean;
  protected
    procedure SetDefaultFilename(const AValue: string); virtual;
    procedure SetDefaultFileExt(const AValue: string); virtual;
    procedure SetDefaultSourceName(const AValue: string); virtual;
    procedure SetDefaultResFileExt(const AValue: string); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetRequiredPackages(const AValue: string); virtual;
    procedure SetResourceClass(const AValue: TPersistentClass); virtual;
  public
    constructor Create; virtual;
    function GetLocalizedName: string; virtual;
    function GetLocalizedDescription: string; virtual;
    procedure Release;
    procedure Reference;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; virtual;
    procedure UpdateDefaultPascalFileExtension(const DefPasExt: string); virtual;
  public
    property Name: string read FName write SetName;
    property DefaultFilename: string read FDefaultFilename write SetDefaultFilename;
    property DefaultFileExt: string read FDefaultFileExt write SetDefaultFileExt;
    property DefaultSourceName: string read FDefaultSourceName write SetDefaultSourceName;
    property DefaultResFileExt: string read FDefaultResFileExt write SetDefaultResFileExt;
    property DefaultResourceName: string read FDefaultResourceName write FDefaultResourceName;
    property RequiredPackages: string read FRequiredPackages write SetRequiredPackages; // package names separated by semicolon
    property ResourceClass: TPersistentClass read FResourceClass write SetResourceClass;
    property IsComponent: boolean read FIsComponent;
    property UseCreateFormStatements: boolean read FUseCreateFormStatements write FUseCreateFormStatements;
    property VisibleInNewDialog: boolean read FVisibleInNewDialog write FVisibleInNewDialog;
    property IsPascalUnit: boolean read FIsPascalUnit write FIsPascalUnit;
    property AddToProject: boolean read FAddToProject write FAddToProject;
  end;
  
  
  { TFileDescPascalUnit }

  TFileDescPascalUnit = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function CreateSource(const Filename, SourceName,
                          ResourceName: string): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetInterfaceUsesSection: string; virtual;
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string; virtual;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; virtual;
  end;


  { TFileDescPascalUnitWithResource }

  TFileDescPascalUnitWithResource = class(TFileDescPascalUnit)
  public
    function GetInterfaceSource(const Filename, SourceName,
                                ResourceName: string): string; override;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; override;
  end;


  { TProjectFileDescriptors }
  
  TProjectFileDescriptors = class(TPersistent)
  protected
    function GetItems(Index: integer): TProjectFileDescriptor; virtual; abstract;
  public
    function Count: integer; virtual; abstract;
    function GetUniqueName(const Name: string): string; virtual; abstract;
    function IndexOf(const Name: string): integer; virtual; abstract;
    function FindByName(const Name: string): TProjectFileDescriptor; virtual; abstract;
    procedure RegisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); virtual; abstract;
    procedure UnregisterFileDescriptor(FileDescriptor: TProjectFileDescriptor); virtual; abstract;
  public
    property Items[Index: integer]: TProjectFileDescriptor read GetItems; default;
  end;
  
var
  ProjectFileDescriptors: TProjectFileDescriptors; // will be set by the IDE

function FileDescriptorUnit: TProjectFileDescriptor;
function FileDescriptorForm: TProjectFileDescriptor;
function FileDescriptorDatamodule: TProjectFileDescriptor;
function FileDescriptorText: TProjectFileDescriptor;

implementation

function FileDescriptorUnit: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNamePascalUnit);
end;

function FileDescriptorForm: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameLCLForm);
end;

function FileDescriptorDatamodule: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameDatamodule);
end;

function FileDescriptorText: TProjectFileDescriptor;
begin
  Result:=ProjectFileDescriptors.FindByName(FileDescNameText);
end;

{ TProjectFileDescriptor }

procedure TProjectFileDescriptor.SetResourceClass(
  const AValue: TPersistentClass);
begin
  if FResourceClass=AValue then exit;
  FResourceClass:=AValue;
  FIsComponent:=(FResourceClass<>nil)
                and (FResourceClass.InheritsFrom(TComponent));
  if FResourceClass=nil then
    FDefaultResourceName:=''
  else begin
    FDefaultResourceName:=
      copy(FResourceClass.ClassName,2,length(FResourceClass.ClassName)-1);
  end;
end;

procedure TProjectFileDescriptor.SetDefaultFileExt(const AValue: string);
begin
  if FDefaultFileExt=AValue then exit;
  FDefaultFileExt:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultResFileExt(const AValue: string);
begin
  if FDefaultResFileExt=AValue then exit;
  FDefaultResFileExt:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultSourceName(const AValue: string);
begin
  if FDefaultSourceName=AValue then exit;
  FDefaultSourceName:=AValue;
end;

procedure TProjectFileDescriptor.SetDefaultFilename(const AValue: string);
begin
  if FDefaultFilename=AValue then exit;
  FDefaultFilename:=AValue;
  DefaultFileExt:=ExtractFileExt(FDefaultFilename);
  FIsPascalUnit:=(CompareFileExt(DefaultFileExt,'.pp',false)=0)
              or (CompareFileExt(DefaultFileExt,'.pas',false)=0);
end;

procedure TProjectFileDescriptor.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TProjectFileDescriptor.SetRequiredPackages(const AValue: string);
begin
  if FRequiredPackages=AValue then exit;
  FRequiredPackages:=AValue;
end;

constructor TProjectFileDescriptor.Create;
begin
  FReferenceCount:=1;
  DefaultResFileExt:='.lrs';
  AddToProject:=true;
end;

function TProjectFileDescriptor.GetLocalizedName: string;
begin
  Result:=Name;
end;

function TProjectFileDescriptor.GetLocalizedDescription: string;
begin
  Result:=GetLocalizedName;
end;

procedure TProjectFileDescriptor.Release;
begin
  //debugln('TProjectFileDescriptor.Release A ',Name,' ',dbgs(FReferenceCount));
  if FReferenceCount=0 then
    raise Exception.Create('');
  dec(FReferenceCount);
  if FReferenceCount=0 then Free;
end;

procedure TProjectFileDescriptor.Reference;
begin
  inc(FReferenceCount);
end;

function TProjectFileDescriptor.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='';
end;

procedure TProjectFileDescriptor.UpdateDefaultPascalFileExtension(
  const DefPasExt: string);
begin
  if DefPasExt='' then exit;
  if FilenameIsPascalUnit(DefaultFileExt) then
    DefaultFileExt:=DefPasExt;
  if FilenameIsPascalUnit(DefaultFilename) then
    DefaultFilename:=ChangeFileExt(DefaultFilename,DefPasExt);
end;

{ TFileDescPascalUnit }

constructor TFileDescPascalUnit.Create;
begin
  inherited Create;
  Name:=FileDescNamePascalUnit;
  DefaultFilename:='unit.pas';
  DefaultSourceName:='Unit1';
end;

function TFileDescPascalUnit.CreateSource(const Filename, SourceName,
  ResourceName: string): string;
var
  LE: string;
begin
  LE:=LineEnding;
  Result:=
     'unit '+SourceName+';'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+GetInterfaceUsesSection+';'+LE
    +LE
    +GetInterfaceSource(Filename,SourceName,ResourceName)
    +'implementation'+LE
    +LE
    +GetImplementationSource(Filename,SourceName,ResourceName)
    +'end.'+LE
    +LE;
end;

function TFileDescPascalUnit.GetLocalizedName: string;
begin
  Result:='Unit';
end;

function TFileDescPascalUnit.GetLocalizedDescription: string;
begin
  Result:='Create a new pascal unit.';
end;

function TFileDescPascalUnit.GetInterfaceUsesSection: string;
begin
  Result:='Classes, SysUtils';
end;

function TFileDescPascalUnit.GetInterfaceSource(const Filename, SourceName,
  ResourceName: string): string;
begin
  Result:='';
end;

function TFileDescPascalUnit.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  Result:='';
end;

{ TFileDescPascalUnitWithResource }

function TFileDescPascalUnitWithResource.GetInterfaceSource(const Filename,
  SourceName, ResourceName: string): string;
var
  LE: string;
begin
  LE:=LineEnding;
  Result:=
     'type'+LE
    +'  T'+ResourceName+' = class('+ResourceClass.ClassName+')'+LE
    +'  private'+LE
    +'    { private declarations }'+LE
    +'  public'+LE
    +'    { public declarations }'+LE
    +'  end;'+LE
    +LE
    +'var'+LE
    +'  '+ResourceName+': T'+ResourceName+';'+LE;
end;

function TFileDescPascalUnitWithResource.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
var
  ResourceFilename: String;
  LE: String;
begin
  ResourceFilename:=TrimFilename(ExtractFilenameOnly(Filename)+DefaultResFileExt);
  LE:=LineEnding;
  Result:='initialization'+LE
    +'  {$I '+ResourceFilename+'}'+LE
    +LE
end;

initialization
  ProjectFileDescriptors:=nil;

end.

