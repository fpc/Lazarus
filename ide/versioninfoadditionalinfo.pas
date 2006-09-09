{
 /***************************************************************************
                   VersionInfoAdditionalInfo.pas  -  Lazarus IDE unit
                   --------------------------------------------------
                   TVersionInfo is responsible for the inclusion of the
                   version information in windows executables.


                   Initial Revision  : Sun Feb 20 12:00:00 CST 2006


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
unit VersionInfoAdditionalInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, W32VersionInfo;

type

  { TVersionInfoAdditinalInfoForm }

  TVersionInfoAdditinalInfoForm = class(TForm)
    OKButton: TButton;
    AdditionalInfoGroupBox: TGroupBox;
    CancelButton: TButton;
    CommentsEdit: TEdit;
    CompanyEdit: TEdit;
    InternalNameEdit: TEdit;
    LegalTrademarksEdit: TEdit;
    OriginalFileNameEdit: TEdit;
    ProductNameEdit: TEdit;
    ProductVersionEdit: TEdit;
    CommentsLabel: TLabel;
    CompanyLabel: TLabel;
    InternalNameLabel: TLabel;
    LegalTrademarksLabel: TLabel;
    OriginalFileNameLabel: TLabel;
    ProductNameLabel: TLabel;
    ProductVersionLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end; 


function ShowVersionInfoAdditionailInfoForm(LocalVersionInfo: TProjectVersionInfo;
                                            var ProjectModified: boolean) : TModalResult;

implementation


function ShowVersionInfoAdditionailInfoForm(LocalVersionInfo: TProjectVersionInfo;
                                            var ProjectModified: boolean) : TModalResult;

var VersionInfoAdditionalInfoForm: TVersionInfoAdditinalInfoForm;
begin
   VersionInfoAdditionalInfoForm := TVersionInfoAdditinalInfoForm.Create(nil);

   VersionInfoAdditionalInfoForm.CommentsEdit.Text := LocalVersionInfo.CommentsString;
   VersionInfoAdditionalInfoForm.CommentsEdit.Text := LocalVersionInfo.CommentsString;
   VersionInfoAdditionalInfoForm.CompanyEdit.Text := LocalVersionInfo.CompanyString;
   VersionInfoAdditionalInfoForm.InternalNameEdit.Text := LocalVersionInfo.InternalNameString;
   VersionInfoAdditionalInfoForm.LegalTrademarksEdit.Text := LocalVersionInfo.TrademarksString;
   VersionInfoAdditionalInfoForm.OriginalFilenameEdit.Text := LocalVersionInfo.OriginalFilenameString;
   VersionInfoAdditionalInfoForm.ProductNameEdit.Text := LocalVersionInfo.ProdNameString;
   VersionInfoAdditionalInfoForm.ProductVersionEdit.Text := LocalVersionInfo.ProductVersionString;

   Result := VersionInfoAdditionalInfoForm.ShowModal;
   if Result = mrOk then
      begin
         LocalVersionInfo.CommentsString:=VersionInfoAdditionalInfoForm.CommentsEdit.Text;
         LocalVersionInfo.CompanyString:=VersionInfoAdditionalInfoForm.CompanyEdit.Text;
         LocalVersionInfo.InternalNameString:=VersionInfoAdditionalInfoForm.InternalNameEdit.Text;
         LocalVersionInfo.TrademarksString:=VersionInfoAdditionalInfoForm.LegalTrademarksEdit.Text;
         LocalVersionInfo.OriginalFilenameString:=VersionInfoAdditionalInfoForm.OriginalFilenameEdit.Text;
         LocalVersionInfo.ProdNameString:=VersionInfoAdditionalInfoForm.ProductNameEdit.Text;
         LocalVersionInfo.ProductVersionString:=VersionInfoAdditionalInfoForm.ProductVersionEdit.Text;
      end;
   VersionInfoAdditionalInfoForm.Free;
end;

{ TVersionInfoAdditinalInfoForm }

procedure TVersionInfoAdditinalInfoForm.Button1Click(Sender: TObject);
begin
   ModalResult := MrOk;
end;

procedure TVersionInfoAdditinalInfoForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := MrCancel;
end;

constructor TVersionInfoAdditinalInfoForm.Create(TheOwner: TComponent);
begin
   inherited Create(TheOwner);
end;

initialization
  {$I versioninfoadditionalinfo.lrs}

end.

