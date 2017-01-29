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
unit AskCompNameDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ButtonPanel, ExtCtrls, PropEdits, LazarusIDEStrConsts;

type

  { TAskCompNameDialog }

  TAskCompNameDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    InfoPanel: TPanel;
    NameEdit: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
  private
    FLookupRoot: TComponent;
    FNewComponent: TComponent;
    function GetNewName: TComponentName;
    procedure SetNewName(const AValue: TComponentName);
  public
    function IsValidName(AName: TComponentName; out ErrorMsg: string): boolean;
    property LookupRoot: TComponent read FLookupRoot write FLookupRoot;
    property NewName: TComponentName read GetNewName write SetNewName;
    property NewComponent: TComponent read FNewComponent write FNewComponent;
  end; 

function ShowComponentNameDialog(ALookupRoot: TComponent; ANewComponent: TComponent): string;

implementation

{$R *.lfm}

function ShowComponentNameDialog(ALookupRoot: TComponent; ANewComponent: TComponent): string;
begin
  with TAskCompNameDialog.Create(nil) do
  try
    LookupRoot:=ALookupRoot;
    NewComponent:=ANewComponent;
    NewName:=NewComponent.Name;
    Result:=NewComponent.Name; // Default name is the component's current name.
    if ShowModal=mrOk then
      Result:=NewName;
  finally
    Free;
  end;
end;

{ TAskCompNameDialog }

procedure TAskCompNameDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisChooseName;
  Label1.Caption:=lisChooseANameForTheComponent;
  NameEdit.Hint:=lisTheComponentNameMustBeUniqueInAllComponentsOnTheFo;
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  ButtonPanel1.OKButton.Enabled:=false;
end;

procedure TAskCompNameDialog.NameEditChange(Sender: TObject);
var
  Ok: boolean;
  ErrorMsg: string;
begin
  Ok:=IsValidName(NameEdit.Text, ErrorMsg);
  ButtonPanel1.OKButton.Enabled:=Ok;
  InfoPanel.Caption:=ErrorMsg;
  InfoPanel.Visible:=not Ok;
end;

function TAskCompNameDialog.GetNewName: TComponentName;
begin
  Result:=NameEdit.Text;
end;

procedure TAskCompNameDialog.SetNewName(const AValue: TComponentName);
begin
  NameEdit.Text:=AValue;
  NameEditChange(nil);
end;

function TAskCompNameDialog.IsValidName(AName: TComponentName; out
  ErrorMsg: string): boolean;
var
  ConflictComponent: TComponent;
begin
  Result:=false;
  if (AName='') then begin
    ErrorMsg:=lisEmpty;
    exit;
  end;
  if not IsValidIdent(AName) then begin
    ErrorMsg:=lisNotAValidPascalIdentifier;
    exit;
  end;
  if (FLookupRoot<>nil) then begin
    ConflictComponent:=FLookupRoot.FindComponent(AName);
    if (ConflictComponent<>nil)
    and (ConflictComponent<>NewComponent) then begin
      ErrorMsg:=lisThereIsAlreadyAComponentWithThisName;
      exit;
    end;
    if SysUtils.CompareText(AName,FLookupRoot.Name)=0 then begin
      ErrorMsg:=lisTheOwnerHasThisName;
      exit;
    end;
    if SysUtils.CompareText(AName,FLookupRoot.ClassName)=0 then begin
      ErrorMsg:=lisTheOwnerClassHasThisName;
      exit;
    end;
    if SysUtils.CompareText(AName,GetClassUnitName(FLookupRoot.ClassType))=0 then begin
      ErrorMsg:=lisTheUnitHasThisName;
      exit;
    end;
  end;
  ErrorMsg:='';
  Result:=true;
end;

end.

