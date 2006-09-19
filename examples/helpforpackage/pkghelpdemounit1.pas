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
    Example unit demonstrating how to register IDE online help for the sources
    of a package.
}
unit PkgHelpDemoUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HelpFPDoc;
  
procedure Register;

implementation

procedure Register;
begin
  RegisterFPDocHTMLHelpForPackage('Demo Help DB','Demo Help Database',
              'file://$PkgDir(DemoPackageWithHelp)/html','DemoPackageWithHelp');
end;

end.

