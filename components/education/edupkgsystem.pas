{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Options for Lazarus package system.
}
unit EduPkgSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, StdCtrls;

type

  { TEduPkgSystemFrame }

  TEduPkgSystemFrame = class(TFrame)
    HideCreatePackageCheckBox: TCheckBox;
    HideOpenPackageCheckBox: TCheckBox;
    HidePackageGraphCheckBox: TCheckBox;
    HideConfigureInstalledPkgsCheckBox: TCheckBox;
    procedure FrameClick(Sender: TObject);
  private
  public
  end;

implementation

{ TEduPkgSystemFrame }

procedure TEduPkgSystemFrame.FrameClick(Sender: TObject);
begin
  HideCreatePackageCheckBox.Caption:='Hide items to create new packages';
end;

initialization
  {$I edupkgsystem.lrs}

end.

