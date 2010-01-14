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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit AskCompNameDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, PropEdits, LazarusIDEStrConsts;

type

  { TAskCompNameDialog }

  TAskCompNameDialog = class(TForm)
    OkButton: TButton;
    NameEdit: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FLookupRoot: TComponent;
    FNewComponent: TComponent;
    function GetNewName: TComponentName;
    procedure SetLookupRoot(const AValue: TComponent);
    procedure SetNewComponent(const AValue: TComponent);
    procedure SetNewName(const AValue: TComponentName);
  public
    function IsValidName(AName: TComponentName; out ErrorMsg: string): boolean;
    property LookupRoot: TComponent read FLookupRoot write SetLookupRoot;
    property NewName: TComponentName read GetNewName write SetNewName;
    property NewComponent: TComponent read FNewComponent write SetNewComponent;
  end; 

function ShowComponentNameDialog(LookupRoot: TComponent; NewComponent: TComponent;
  var NewName: string): TModalResult;

implementation

{$R *.lfm}

function ShowComponentNameDialog(LookupRoot: TComponent; NewComponent: TComponent;
  var NewName: string): TModalResult;
var
  AskCompNameDialog: TAskCompNameDialog;
begin
  AskCompNameDialog:=TAskCompNameDialog.Create(nil);
  try
    AskCompNameDialog.LookupRoot:=LookupRoot;
    AskCompNameDialog.NewComponent:=NewComponent;
    AskCompNameDialog.NewName:=NewName;
    Result:=AskCompNameDialog.ShowModal;
    if Result=mrOk then
      NewName:=AskCompNameDialog.NewName;
  finally
    AskCompNameDialog.Free;
  end;
end;

{ TAskCompNameDialog }

procedure TAskCompNameDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisChooseName;
  Label1.Caption:=lisChooseANameForTheNewComponent;
  NameEdit.Hint:=lisTheComponentNameMustBeUniqueInAllComponentsOnTheFo;
  OkButton.Caption:=dlgMouseOptBtnOk;
  OkButton.Enabled:=false;
end;

procedure TAskCompNameDialog.NameEditChange(Sender: TObject);
var
  ErrorMsg: string;
begin
  OkButton.Enabled:=IsValidName(NameEdit.Text,ErrorMsg);
  OkButton.ShowHint:=ErrorMsg<>'';
  OkButton.Hint:=ErrorMsg;
end;

procedure TAskCompNameDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TAskCompNameDialog.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot=AValue then exit;
  FLookupRoot:=AValue;
end;

procedure TAskCompNameDialog.SetNewComponent(const AValue: TComponent);
begin
  if FNewComponent=AValue then exit;
  FNewComponent:=AValue;
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
  if (not IsValidIdent(AName)) then begin
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

