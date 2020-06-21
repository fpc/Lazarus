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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit fppkg_details;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  StdCtrls, ButtonPanel, ComCtrls,
  fpmkunit,
  fprepos,
  pkgoptions,
  laz_pkgrepos,
  fppkg_const;

type

  { TPkgDetailsForm }

  TPkgDetailsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    SupportLabel: TLabel;
    KeywordsLabel: TLabel;
    CategoryLabel: TLabel;
    DescriptionMemo: TMemo;
    AuthorLabel: TLabel;
    RepoLabel: TLabel;
    LicenseLabel: TLabel;
    HomepageURLLabel: TLabel;
    DownloadURLLabel: TLabel;
    FileNameLabel: TLabel;
    EmailLabel: TLabel;
    PackageListView: TListView;
    OSLabel: TLabel;
    CPULabel: TLabel;
    VersionLabel: TLabel;
    NameLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PackageListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    FPackageName: string;
    FLazPackages: TLazPackages;
    FAvailableFPPackage: TFPPackage;
    procedure SetupColumns;
  public
    property PackageName: string read FPackageName write FPackageName;
    property LazPackages: TLazPackages read FLazPackages write FLazPackages;
  end;

var
  PkgDetailsForm: TPkgDetailsForm;

implementation

{$R *.lfm}

{ TPkgDetailsForm }

procedure TPkgDetailsForm.FormCreate(Sender: TObject);
begin
  Caption := 'Package details';
  SetupColumns;
end;

procedure TPkgDetailsForm.FormShow(Sender: TObject);
var
  fppkg: TFPPackage;
  pkg: TLazPackage;
  i: Integer;
  li: TListItem;
begin
  if (PackageName = '') or not Assigned(FLazPackages) then
    exit;

  pkg := FLazPackages.FindPackage(PackageName);

  NameLabel.Caption := PackageName;
  DescriptionMemo.Text:= pkg.Description;

  for i := 0 to pkg.FPPackageCount -1 do
    begin
    fppkg := pkg.FPPackage[i];
    li := PackageListView.Items.Add;
    li.Caption := fppkg.Repository.RepositoryName;
    li.Data := fppkg;
    li.SubItems.Add(fppkg.Repository.Description);
    li.SubItems.Add(fppkg.Version.AsString);
    case fppkg.Repository.RepositoryType of
      fprtInstalled:
        li.SubItems.Add(SInstalled);
      fprtAvailable:
        begin
        FAvailableFPPackage := fppkg;
        li.SubItems.Add(SAvailable);
        end;
    end;
    end;

  PackageListView.ItemIndex := 0;
end;

procedure TPkgDetailsForm.PackageListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);

  procedure SetLabel(ALabelText, ADefaultCaption, ACaption : string; ALabel: TLabel);
  var
    s: String;
  begin
    s := ACaption;
    if s='' then
      s := ADefaultCaption;
    if s='' then
      s := SUnknown;
    ALabel.Caption:= ALabelText + ': ' + s;
  end;

var
  Fppkg: TFPPackage;
begin
  Fppkg := TFPPackage(PackageListView.Selected.Data);

  if Assigned(FAvailableFPPackage) then
    begin
    SetLabel(SAuthor, FAvailableFPPackage.Author, Fppkg.Author, AuthorLabel);
    SetLabel(SLicense, FAvailableFPPackage.License, Fppkg.License, LicenseLabel);
    SetLabel(SHomepage, FAvailableFPPackage.HomepageURL, Fppkg.HomepageURL, HomepageURLLabel);
    SetLabel(SDownload, FAvailableFPPackage.DownloadURL, Fppkg.DownloadURL, DownloadURLLabel);
    SetLabel(SFilename, FAvailableFPPackage.FileName, Fppkg.FileName, FileNameLabel);
    SetLabel(SEmail, FAvailableFPPackage.Email, Fppkg.Email, EmailLabel);
    SetLabel(SOS, OSesToString(FAvailableFPPackage.OSes), OSesToString(Fppkg.OSes), OSLabel);
    SetLabel(SCPU, CPUSToString(FAvailableFPPackage.CPUs), CPUSToString(Fppkg.CPUs), CPULabel);
    SetLabel(SSupport, FAvailableFPPackage.Support, Fppkg.Support, SupportLabel);
    SetLabel(SKeywords, FAvailableFPPackage.Keywords, Fppkg.Keywords, KeywordsLabel);
    SetLabel(SCategory, FAvailableFPPackage.Category, Fppkg.Category, CategoryLabel);
    end
  else
    begin
    SetLabel(SAuthor, '', Fppkg.Author, AuthorLabel);
    SetLabel(SLicense, '', Fppkg.License, LicenseLabel);
    SetLabel(SHomepage, '', Fppkg.HomepageURL, HomepageURLLabel);
    SetLabel(SDownload, '', Fppkg.DownloadURL, DownloadURLLabel);
    SetLabel(SFilename, '', Fppkg.FileName, FileNameLabel);
    SetLabel(SEmail, '', Fppkg.Email, EmailLabel);
    SetLabel(SOS, '', OSesToString(Fppkg.OSes), OSLabel);
    SetLabel(SCPU, '', CPUSToString(Fppkg.CPUs), CPULabel);
    SetLabel(SSupport, '', Fppkg.Support, SupportLabel);
    SetLabel(SKeywords, '', Fppkg.Keywords, KeywordsLabel);
    SetLabel(SCategory, '', Fppkg.Category, CategoryLabel);
    end;
  VersionLabel.Caption:= 'Version: ' + fppkg.Version.AsString;
end;

procedure TPkgDetailsForm.SetupColumns;
var
  col: TListColumn;
begin
  PackageListView.BeginUpdate;
  //setup columns
  PackageListView.Columns.Clear;

  col := PackageListView.Columns.Add;
  col.Caption := 'Repository';
  col.AutoSize := True;

  col := PackageListView.Columns.Add;
  col.Caption := 'Repository description';
  col.AutoSize := True;

  col := PackageListView.Columns.Add;
  col.Caption := 'Version';
  col.AutoSize := True;

  col := PackageListView.Columns.Add;
  col.Caption := 'Type';
  col.AutoSize := True;

  PackageListView.EndUpdate;
end;

end.

