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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ButtonPanel, ExtCtrls, PropEdits, LazarusIDEStrConsts,
  TypInfo, LCLType;

type

  { TAskCompNameDialog }

  TAskCompNameDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    TextMemo: TMemo;
    InfoPanel: TPanel;
    TextLabel: TLabel;
    NameEdit: TEdit;
    NameLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NameEditChange(Sender: TObject);
    procedure TextMemoEnter(Sender: TObject);
  private
    FLookupRoot: TComponent;
    FNewComponent: TComponent;
    FNewTextPropertyName: string;
    function GetNewText: string;
    function GetNewTextEnabled: Boolean;
    function GetNewName: TComponentName;
    procedure SetNewComponent(const ANewComponent: TComponent);
  public
    function IsValidName(AName: TComponentName; out ErrorMsg: string): boolean;
    property LookupRoot: TComponent read FLookupRoot write FLookupRoot;
    property NewName: TComponentName read GetNewName;
    property NewText: string read GetNewText;
    property NewTextPropertyName: string read FNewTextPropertyName;
    property NewTextEnabled: Boolean read GetNewTextEnabled;
    property NewComponent: TComponent read FNewComponent write SetNewComponent;
  end;

  TAskCompNameDialogResult = record
    NameChanged: Boolean;
    TextChanged: Boolean;
    TextPropertyName: string;

    function Changed: Boolean;
  end;

function ShowComponentNameDialog(ALookupRoot: TComponent; ANewComponent: TComponent): TAskCompNameDialogResult;

implementation

{$R *.lfm}

function TryGetComponentText(AComponent: TComponent; out AText, ATextPropertyName: string): Boolean;
var
  PInfo: PPropInfo;
  Lines: TObject;
const
  StringProperties: array[0..0] of string = ('Caption');
  TStringsProperties: array[0..2] of string = ('Lines', 'Items', 'SQL');
begin
  // first check simple string properties
  for ATextPropertyName in StringProperties do
  begin
    PInfo := GetPropInfo(AComponent, ATextPropertyName);
    Result := (PInfo<>nil) and (PInfo^.SetProc<>nil) and (PInfo^.PropType^.Kind in [tkSString,tkLString,tkAString,tkWString]);
    if Result then
    begin
      AText := GetPropValue(AComponent, PInfo, True);
      Exit;
    end;
  end;

  // then check TStrings properties
  for ATextPropertyName in TStringsProperties do
  begin
    PInfo := GetPropInfo(AComponent, ATextPropertyName);
    Result := (PInfo<>nil) and (PInfo^.SetProc<>nil) and (PInfo^.PropType^.Kind in [tkClass]);
    if Result then
    begin
      Lines := GetObjectProp(AComponent, PInfo);
      Result := Lines is TStrings;
      if Result then
      begin
        AText := TStrings(Lines).Text;
        Exit;
      end;
    end;
  end;

  // add more properties here

  ATextPropertyName := '';
  AText := '';
end;

procedure SetComponentText(AComponent: TComponent; AText, ATextPropertyName: string);
var
  PInfo: PPropInfo;
  Obj: TObject;
  StrL: TStringList;
begin
  PInfo := GetPropInfo(AComponent, ATextPropertyName);
  case PInfo^.PropType^.Kind of
    tkSString,tkLString,tkAString,tkWString: SetPropValue(AComponent, PInfo, AText);
    tkClass:
    begin
      Obj := GetObjectProp(AComponent, PInfo);
      if Obj is TStrings then
      begin
        StrL := TStringList.Create;
        try
          StrL.Text := AText;
          SetObjectProp(AComponent, PInfo, StrL);
        finally
          StrL.Free;
        end;
      end else
        raise Exception.CreateFmt('Unhandled object %s', [Obj.ClassName]);
    end;
  else
    raise Exception.CreateFmt('Unhandled property type %d', [PInfo^.PropType^.Kind]);
  end;
end;

function ShowComponentNameDialog(ALookupRoot: TComponent; ANewComponent: TComponent): TAskCompNameDialogResult;
var
  OldName: TComponentName;
  OldText: string;
begin
  Result := Default(TAskCompNameDialogResult);
  with TAskCompNameDialog.Create(nil) do
  try
    LookupRoot:=ALookupRoot;
    NewComponent:=ANewComponent;
    OldName := NewName;
    OldText := NewText;
    if ShowModal=mrOk then
    begin
      if OldName<>NewName then
      begin
        Result.NameChanged := True;
        ANewComponent.Name := NewName;
      end;
      if NewTextEnabled and (OldText<>NewText) then
      begin
        Result.TextChanged := True;
        Result.TextPropertyName := NewTextPropertyName;
        SetComponentText(ANewComponent, NewText, NewTextPropertyName);
      end;
    end;
  finally
    Free;
  end;
end;

{ TAskCompNameDialogResult }

function TAskCompNameDialogResult.Changed: Boolean;
begin
  Result := NameChanged or TextChanged;
end;

{ TAskCompNameDialog }

procedure TAskCompNameDialog.FormCreate(Sender: TObject);
begin
  Caption:=lisChooseNameAndText;
  NameLabel.Caption:=lisChooseANameForTheComponent;
  NameEdit.Hint:=lisTheComponentNameMustBeUniqueInAllComponentsOnTheFo;
  TextLabel.Caption:=lisMenuEditorCaption;
  ButtonPanel1.OKButton.Caption:=lisOk;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  ButtonPanel1.OKButton.Enabled:=false;
end;

procedure TAskCompNameDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (ssCtrl in Shift) then
  begin
    Key := 0;
    ButtonPanel1.OKButton.Click;
  end;
end;

function TAskCompNameDialog.GetNewText: string;
begin
  Result := TextMemo.Text;
end;

function TAskCompNameDialog.GetNewTextEnabled: Boolean;
begin
  Result := TextMemo.Enabled;
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

procedure TAskCompNameDialog.SetNewComponent(const ANewComponent: TComponent);
var
  ReadText: string;
begin
  if FNewComponent = ANewComponent then Exit;
  FNewComponent := ANewComponent;
  NameEdit.Text := FNewComponent.Name;
  NameEditChange(nil);
  TextMemo.Enabled := TryGetComponentText(FNewComponent, ReadText, FNewTextPropertyName);
  TextLabel.Enabled := TextMemo.Enabled;
  if TextMemo.Enabled then
  begin
    TextMemo.Text := ReadText;
    TextLabel.Caption:=FNewTextPropertyName;
  end else
    TextMemo.Text := '';
end;

procedure TAskCompNameDialog.TextMemoEnter(Sender: TObject);
begin
  TextMemo.SelectAll;
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

