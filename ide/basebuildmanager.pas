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
  Classes, SysUtils, Forms,
  Project;
  
type

  { TBaseBuildManager }

  TBaseBuildManager = class(TComponent)
  private
    FHasGUI: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HasGUI: boolean read FHasGUI write FHasGUI;

    function GetBuildMacroOverride(const MacroName: string): string; virtual; abstract;
    function GetBuildMacroOverrides: TStrings; virtual; abstract;
    function GetTargetOS(UseCache: boolean): string; virtual; abstract;
    function GetTargetCPU(UseCache: boolean): string; virtual; abstract;
    function GetLCLWidgetType(UseCache: boolean): string; virtual; abstract;
    function GetRunCommandLine: string; virtual; abstract;

    function GetProjectPublishDir: string; virtual; abstract;
    function GetProjectTargetFilename(aProject: TProject): string; virtual; abstract;
    function GetProjectUsesAppBundle: Boolean; virtual; abstract;
    function GetTestProjectFilename(aProject: TProject): string; virtual; abstract;
    function GetTestUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;
    function GetTestBuildDirectory: string; virtual; abstract;
    function IsTestUnitFilename(const AFilename: string): boolean; virtual; abstract;
    function GetTargetUnitFilename(AnUnitInfo: TUnitInfo): string; virtual; abstract;

    procedure RescanCompilerDefines(ResetBuildTarget, ClearCaches,
                                    WaitTillDone: boolean); virtual; abstract;

    function CheckAmbiguousSources(const AFilename: string;
                                   Compiling: boolean): TModalResult; virtual; abstract;
    function DeleteAmbiguousFiles(const Filename:string
                                  ): TModalResult; virtual; abstract;
    function CheckUnitPathForAmbiguousPascalFiles(const BaseDir, TheUnitPath,
                                    CompiledExt, ContextDescription: string
                                    ): TModalResult; virtual; abstract;
    function CreateProjectApplicationBundle: Boolean; virtual; abstract;

    function BackupFile(const Filename: string): TModalResult; virtual; abstract;

    function UpdateProjectAutomaticFiles(TestDir: string): TModalResult; virtual; abstract;
  end;

var
  BuildBoss: TBaseBuildManager = nil;

implementation

{ TBaseBuildManager }

constructor TBaseBuildManager.Create(AOwner: TComponent);
begin
  BuildBoss:=Self;
  inherited Create(AOwner);
end;

destructor TBaseBuildManager.Destroy;
begin
  inherited Destroy;
  BuildBoss:=nil;
end;

end.

