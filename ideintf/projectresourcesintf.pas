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
  Classes, SysUtils;

type
  TAbstractProjectResources = class;

  { TAbstractProjectResource }
  TAbstractProjectResource = class
  protected
    FModified: boolean;
    FOnModified: TNotifyEvent;
    procedure SetModified(const AValue: boolean);
  public
    function UpdateResources(AResources: TAbstractProjectResources; const MainFilename: string): Boolean; virtual; abstract;

    constructor Create; virtual;
    property Modified: boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

  { TAbstractProjectResources }

  TAbstractProjectResources = class
  protected
    FMessages: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AddSystemResource(const AResource: String); virtual; abstract;
    procedure AddLazarusResource(AResource: TStream; const ResourceName, ResourceType: String); virtual; abstract;

    property Messages: TStringList read FMessages;
  end;

implementation

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

{ TAbstractProjectResources }

constructor TAbstractProjectResources.Create;
begin
  FMessages := TStringList.Create;
end;

destructor TAbstractProjectResources.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

end.
