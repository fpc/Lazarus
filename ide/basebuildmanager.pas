{  $Id: helpmanager.pas 9796 2006-09-02 21:10:32Z mattias $  }
{
 /***************************************************************************
                            buildmanager.pas
                            ----------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit BaseBuildManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Project;
  
type

  { TBaseBuildManager }

  TBaseBuildManager = class
  public
    constructor Create;
    destructor Destroy; override;

    function GetTargetOS(UseCache: boolean): string; virtual; abstract;
    function GetTargetCPU(UseCache: boolean): string; virtual; abstract;
    function GetLCLWidgetType(UseCache: boolean): string; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;

    function GetProjectPublishDir: string; virtual; abstract;
    function GetProjectTargetFilename: string; virtual; abstract;
    function GetTestProjectFilename: string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function GetTestBuildDirectory: string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
  end;

var
  BuildBoss: TBaseBuildManager = nil;

implementation

{ TBaseBuildManager }

constructor TBaseBuildManager.Create;
begin
  BuildBoss:=Self;
  inherited Create;
end;

destructor TBaseBuildManager.Destroy;
begin
  inherited Destroy;
  BuildBoss:=nil;
end;

end.

