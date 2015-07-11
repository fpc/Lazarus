{ Package detail form for the lazarus package manager

  Copyright (C) 2011 Darius Blaszyk

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit fppkg_details;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  StdCtrls, ButtonPanel, laz_pkgrepos;

type

  { TPkgDetailsForm }

  TPkgDetailsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DescriptionMemo: TMemo;
    AuthorLabel: TLabel;
    LicenseLabel: TLabel;
    HomepageURLLabel: TLabel;
    DownloadURLLabel: TLabel;
    FileNameLabel: TLabel;
    EmailLabel: TLabel;
    OSLabel: TLabel;
    CPULabel: TLabel;
    VersionLabel: TLabel;
    NameLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPackageName: string;
    { private declarations }
  public
    { public declarations }
    property PackageName: string read FPackageName write FPackageName;
  end; 

var
  PkgDetailsForm: TPkgDetailsForm;

implementation

{$R *.lfm}

{ TPkgDetailsForm }

procedure TPkgDetailsForm.FormCreate(Sender: TObject);
begin
  Caption := 'Package details';
end;

procedure TPkgDetailsForm.FormShow(Sender: TObject);
var
  pkg: TLazPackageData;
begin
  if PackageName = '' then
    exit;

  pkg := Laz_Packages.FindPackage(PackageName);

  NameLabel.Caption := PackageName;
  DescriptionMemo.Text:= pkg.Description;

  AuthorLabel.Caption:= 'Author: ' + pkg.Author;
  VersionLabel.Caption:= 'Version: ' + pkg.AvialableVersion;
  LicenseLabel.Caption:= 'License: ' + pkg.License;
  HomePageURLLabel.Caption:= 'Homepage: ' + pkg.HomepageURL;
  DownloadURLLabel.Caption:= 'Download: ' + pkg.DownloadURL;
  FileNameLabel.Caption:= 'Filename: ' + pkg.FileName;
  EmailLabel.Caption:= 'Email: ' + pkg.Email;
  OSLabel.Caption:= 'OS: ' + pkg.OS;
  CPULabel.Caption:= 'CPU: ' + pkg.CPU;
end;

end.

