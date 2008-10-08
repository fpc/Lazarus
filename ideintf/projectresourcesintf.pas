{ Copyright (C) 2004

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

  { TAbstractProjectResources }

  TAbstractProjectResources = class
  public
    procedure AddSystemResource(const AResource: String); virtual; abstract;
    procedure AddLazarusResource(AResource: TStream; const ResourceName, ResourceType: String); virtual; abstract;
  end;

implementation

end.
