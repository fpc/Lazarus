{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit ProjPackIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // BuildIntf
  IDEOptionsIntf, CompOptsIntf;

type

  {$M+}
  TIDEOwnedFile = class
  protected
    FUnitName: string;
    function GetFilename: string; virtual; abstract;
    procedure SetFilename(const AValue: string); virtual; abstract;
    procedure SetUnitName(const AValue: string); virtual; abstract;
  public
    function GetFullFilename: string; virtual; abstract; // if no path, the file was not saved yet
    function GetShortFilename(UseUp: boolean): string; virtual; abstract;
    function GetFileOwner: TObject; virtual; abstract;
    function GetFileOwnerName: string; virtual; abstract;
    property Filename: string read GetFilename write SetFilename;
    property Unit_Name: string read FUnitName write SetUnitName;
  end;
  {$M-}

  { TIDEProjPackBase }

  TIDEProjPackBase = class(TComponent)
  private
  protected
    FIDEOptions: TAbstractIDEOptions; //actually TProjectIDEOptions or TPackageIDEOptions;
    FLazCompilerOptions: TLazCompilerOptions;
    function GetDirectory: string; virtual; abstract;
    //procedure SetDirectory(AValue: string); virtual; abstract;
    function HasDirectory: boolean; virtual;
    function GetLazCompilerOptions: TLazCompilerOptions;
  public
    property Directory: string read GetDirectory;// write SetDirectory; // directory of .lpi or .lpk file
    property LazCompilerOptions: TLazCompilerOptions read GetLazCompilerOptions;
  end;


implementation

{ TIDEProjPackBase }

function TIDEProjPackBase.HasDirectory: boolean;
begin
  Result := True;
end;

function TIDEProjPackBase.GetLazCompilerOptions: TLazCompilerOptions;
begin
  Result := FLazCompilerOptions;
end;

end.

