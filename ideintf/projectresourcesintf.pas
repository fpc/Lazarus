unit projectresourcesintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAbstractProjectResources }

  TAbstractProjectResources = class
  public
    procedure AddSystemResource(const AResource: String); virtual; abstract;
    procedure AddLazarusResource(AResource: TStream; const ResourceName, ResourceType: String); virtual; abstract;
  end;

implementation

end.
