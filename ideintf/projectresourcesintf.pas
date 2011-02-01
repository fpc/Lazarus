{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ProjectResourcesIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ProjectIntf, resource;

type
  TAbstractProjectResources = class;

  { TAbstractProjectResource }
  TAbstractProjectResource = class
  protected
    FModified: boolean;
    FOnModified: TNotifyEvent;
    procedure SetModified(const AValue: boolean);
  public
    constructor Create; virtual;

    procedure DoAfterBuild(AResources: TAbstractProjectResources; AReason: TCompileReason; SaveToTestDir: boolean); virtual;
    procedure DoBeforeBuild(AResources: TAbstractProjectResources; AReason: TCompileReason; SaveToTestDir: boolean); virtual;
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean; virtual; abstract;
    procedure WriteToProjectFile(AConfig: {TXMLConfig}TObject; Path: String); virtual; abstract;
    procedure ReadFromProjectFile(AConfig: {TXMLConfig}TObject; Path: String); virtual; abstract;

    property Modified: boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

  TAbstractProjectResourceClass = class of TAbstractProjectResource;

  { TAbstractProjectResources }

  TAbstractProjectResources = class
  private
    FProject: TLazProject;
    FResourceType: TResourceType;
  protected
    FMessages: TStringList;
    procedure SetResourceType(const AValue: TResourceType); virtual;
    function GetProjectResource(AIndex: TAbstractProjectResourceClass): TAbstractProjectResource; virtual; abstract;
    class function GetRegisteredResources: TList;
  public
    constructor Create(AProject: TLazProject); virtual;
    destructor Destroy; override;

    procedure AddSystemResource(AResource: TAbstractResource); virtual; abstract;
    procedure AddLazarusResource(AResource: TStream;
                   const AResourceName, AResourceType: String); virtual; abstract;

    property Messages: TStringList read FMessages;
    property Project: TLazProject read FProject;
    property ResourceType: TResourceType read FResourceType write SetResourceType;
    property Resource[AIndex: TAbstractProjectResourceClass]: TAbstractProjectResource read GetProjectResource; default;
  end;

  procedure RegisterProjectResource(AResource: TAbstractProjectResourceClass);

implementation

var
  FRegisteredProjectResources: TList = nil;

procedure RegisterProjectResource(AResource: TAbstractProjectResourceClass);
begin
  if FRegisteredProjectResources = nil then
    FRegisteredProjectResources := TList.Create;
  FRegisteredProjectResources.Add(AResource);
end;

{ TAbstractProjectResource }

procedure TAbstractProjectResource.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if Assigned(OnModified) then OnModified(Self);
end;

constructor TAbstractProjectResource.Create;
begin
  FModified := False;
end;

procedure TAbstractProjectResource.DoAfterBuild(AResources: TAbstractProjectResources; 
  AReason: TCompileReason; SaveToTestDir: boolean);
begin
  // nothing
end;

procedure TAbstractProjectResource.DoBeforeBuild(AResources: TAbstractProjectResources; 
  AReason: TCompileReason; SaveToTestDir: boolean);
begin
  // nothing
end;

{ TAbstractProjectResources }

procedure TAbstractProjectResources.SetResourceType(const AValue: TResourceType);
begin
  FResourceType := AValue;
end;

class function TAbstractProjectResources.GetRegisteredResources: TList;
begin
  Result := FRegisteredProjectResources;
end;

constructor TAbstractProjectResources.Create(AProject: TLazProject);
begin
  inherited Create;
  FProject := AProject;
  FMessages := TStringList.Create;
end;

destructor TAbstractProjectResources.Destroy;
begin
  FreeAndNil(FMessages);
  inherited Destroy;
end;

finalization
  FRegisteredProjectResources.Free;
end.
