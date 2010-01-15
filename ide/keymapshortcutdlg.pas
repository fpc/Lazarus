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

  Author: Mattias Gaertner

  Abstract:
    TShortCutGrabBox - a control to edit a shortcut
    TShortCutDialog - a dialog to edit ide shortcuts
}
unit KeyMapShortCutDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLType,
  PropEdits, IDECommands, IDEWindowIntf,
  KeyMapping, LazarusIDEStrConsts, Buttons, ButtonPanel;

type

  { TShortCutDialog }

  TShortCutDialog = class(TForm)
    BtnPanel: TButtonPanel;
    PrimaryGroupBox: TGroupBox;
    SecondaryGroupBox: TGroupBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FKeyCommandRelationList: TKeyCommandRelationList;
    FPrimaryKey1Box: TShortCutGrabBox;
    FPrimaryKey2Box: TShortCutGrabBox;
    FRelationIndex: integer;
    FSecondaryKey1Box: TShortCutGrabBox;
    FSecondaryKey2Box: TShortCutGrabBox;
    FShowSecondary: boolean;
    FShowSequence: boolean;
    function GetPrimaryShortCut: TIDEShortCut;
    function GetSecondaryShortCut: TIDEShortCut;
    procedure SetPrimaryShortCut(const AValue: TIDEShortCut);
    procedure SetSecondaryShortCut(const AValue: TIDEShortCut);
    procedure SetShowSecondary(const AValue: boolean);
    procedure SetShowSequence(const AValue: boolean);
    function ResolveConflicts(Key: TIDEShortCut; Scope: TIDECommandScope): boolean;
    procedure UpdateCaptions;
  public
    procedure ClearKeys;
    procedure SetRelation(AKeyCommandRelationList: TKeyCommandRelationList;
                          Index: integer);
    property KeyCommandRelationList: TKeyCommandRelationList
                     read FKeyCommandRelationList write FKeyCommandRelationList;
    property RelationIndex: integer read FRelationIndex write FRelationIndex;
    property ShowSecondary: boolean read FShowSecondary write SetShowSecondary;
    property ShowSequence: boolean read FShowSequence write SetShowSequence;
    property PrimaryKey1Box: TShortCutGrabBox read FPrimaryKey1Box;
    property PrimaryKey2Box: TShortCutGrabBox read FPrimaryKey2Box;
    property SecondaryKey1Box: TShortCutGrabBox read FSecondaryKey1Box;
    property SecondaryKey2Box: TShortCutGrabBox read FSecondaryKey2Box;
    property PrimaryShortCut: TIDEShortCut read GetPrimaryShortCut write SetPrimaryShortCut;
    property SecondaryShortCut: TIDEShortCut read GetSecondaryShortCut write SetSecondaryShortCut;
  end;

function ShowKeyMappingEditForm(Index: integer;
                AKeyCommandRelationList: TKeyCommandRelationList): TModalResult;
function ShowKeyMappingGrabForm(out Key: TIDEShortCut;
  AllowSequence: boolean = false): TModalResult;

implementation

{$R *.lfm}

function ShowKeyMappingEditForm(Index: integer;
  AKeyCommandRelationList: TKeyCommandRelationList): TModalResult;
var
  ShortCutDialog: TShortCutDialog;
begin
  ShortCutDialog:=TShortCutDialog.Create(nil);
  try
    ShortCutDialog.ShowSecondary:=true;
    ShortCutDialog.ShowSequence:=true;
    ShortCutDialog.SetRelation(AKeyCommandRelationList,Index);
    Result:=ShortCutDialog.ShowModal;
  finally
    ShortCutDialog.Free;
  end;
end;

function ShowKeyMappingGrabForm(out Key: TIDEShortCut;
  AllowSequence: boolean): TModalResult;
var
  ShortCutDialog: TShortCutDialog;
begin
  ShortCutDialog:=TShortCutDialog.Create(nil);
  try
    ShortCutDialog.ShowSecondary:=false;
    ShortCutDialog.ShowSequence:=AllowSequence;
    ShortCutDialog.Caption:=lisChooseAKey;
    Result:=ShortCutDialog.ShowModal;
    Key:=ShortCutDialog.PrimaryShortCut;
  finally
    ShortCutDialog.Free;
  end;
end;


{ TShortCutDialog }

procedure TShortCutDialog.FormCreate(Sender: TObject);
begin
  Caption := srkmEditForCmd;
  BtnPanel.OKButton.OnClick := @OkButtonClick;
  BtnPanel.CancelButton.OnClick := @CancelButtonClick;

  IDEDialogLayoutList.ApplyLayout(Self, 480, 480);

  FShowSecondary:=true;
  FShowSequence:=true;

  FPrimaryKey1Box:=TShortCutGrabBox.Create(Self);
  with FPrimaryKey1Box do begin
    Name:='FPrimaryKey1Box';
    Align:=alClient;
    AutoSize:=true;
    BorderSpacing.Around:=6;
    Parent:=PrimaryGroupBox;
  end;
  FPrimaryKey2Box:=TShortCutGrabBox.Create(Self);
  with FPrimaryKey2Box do begin
    Name:='FPrimaryKey2Box';
    Align:=alBottom;
    AutoSize:=true;
    BorderSpacing.Around:=6;
    Parent:=PrimaryGroupBox;
  end;
  PrimaryGroupBox.AutoSize:=true;

  FSecondaryKey1Box:=TShortCutGrabBox.Create(Self);
  with FSecondaryKey1Box do begin
    Name:='FSecondaryKey1Box';
    Align:=alClient;
    AutoSize:=true;
    BorderSpacing.Around:=6;
    Parent:=SecondaryGroupBox;
  end;
  FSecondaryKey2Box:=TShortCutGrabBox.Create(Self);
  with FSecondaryKey2Box do begin
    Name:='FSecondaryKey2Box';
    Align:=alBottom;
    AutoSize:=true;
    BorderSpacing.Around:=6;
    Parent:=SecondaryGroupBox;
  end;
  SecondaryGroupBox.AutoSize:=true;

  UpdateCaptions;
  ClearKeys;
end;

procedure TShortCutDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TShortCutDialog.OkButtonClick(Sender: TObject);
var
  NewKeyA: TIDEShortCut;
  NewKeyB: TIDEShortCut;
  CurRelation: TKeyCommandRelation;
begin
  IDEDialogLayoutList.SaveLayout(Self);

  if KeyCommandRelationList=nil then begin
    ModalResult:=mrOk;
    exit;
  end;

  // set defaults
  NewKeyA:=PrimaryShortCut;
  NewKeyB:=SecondaryShortCut;

  //debugln('TShortCutDialog.OkButtonClick A ShortcutA=',KeyAndShiftStateToEditorKeyString(NewKeyA),' ShortcutB=',KeyAndShiftStateToEditorKeyString(NewKeyB));

  // get old relation
  CurRelation:=KeyCommandRelationList.Relations[RelationIndex];

  if not ResolveConflicts(NewKeyA,CurRelation.Category.Scope) then
  begin
    debugln('TShortCutDialog.OkButtonClick ResolveConflicts failed for key1');
    exit;
  end;

  //debugln('TShortCutDialog.OkButtonClick B ShortcutA=',KeyAndShiftStateToEditorKeyString(NewKeyA),' ShortcutB=',KeyAndShiftStateToEditorKeyString(NewKeyB));

  if (NewKeyA.Key1=NewKeyB.Key1) and (NewKeyA.Shift1=NewKeyB.Shift1) and
     (NewKeyA.Key2=NewKeyB.Key2) and (NewKeyA.Shift2=NewKeyB.Shift2) then
  begin
    NewKeyB.Key1:=VK_UNKNOWN;
    NewKeyB.Shift1:=[];
    NewKeyB.Key2:=VK_UNKNOWN;
    NewKeyB.Shift2:=[];
  end
  else if not ResolveConflicts(NewKeyB,CurRelation.Category.Scope)
  then begin
    debugln('TShortCutDialog.OkButtonClick ResolveConflicts failed for key1');
    exit;
  end;

  //debugln('TShortCutDialog.OkButtonClick C ShortcutA=',KeyAndShiftStateToEditorKeyString(NewKeyA),' ShortcutB=',KeyAndShiftStateToEditorKeyString(NewKeyB));

  if NewKeyA.Key1=VK_UNKNOWN then
  begin
    NewKeyA:=NewKeyB;
    NewKeyB.Key1:=VK_UNKNOWN;
    NewKeyB.Shift1:=[];
    NewKeyB.Key2:=VK_UNKNOWN;
    NewKeyB.Shift2:=[];
  end;

  //debugln('TShortCutDialog.OkButtonClick D ShortcutA=',KeyAndShiftStateToEditorKeyString(NewKeyA),' ShortcutB=',KeyAndShiftStateToEditorKeyString(NewKeyB));

  CurRelation.ShortcutA:=NewKeyA;
  CurRelation.ShortcutB:=NewKeyB;

  //debugln('TShortCutDialog.OkButtonClick B ShortcutA=',KeyAndShiftStateToEditorKeyString(NewKeyA),' ShortcutB=',KeyAndShiftStateToEditorKeyString(NewKeyB));
  ModalResult:=mrOk;
end;

procedure TShortCutDialog.SetShowSecondary(const AValue: boolean);
begin
  if FShowSecondary=AValue then exit;
  FShowSecondary:=AValue;
  SecondaryGroupBox.Visible:=FShowSecondary;
end;

procedure TShortCutDialog.SetPrimaryShortCut(const AValue: TIDEShortCut);
var
  APrimaryShortCut: TIDEShortCut;
begin
  APrimaryShortCut:=GetPrimaryShortCut;
  if CompareIDEShortCuts(@APrimaryShortCut,@AValue)=0 then exit;
  PrimaryKey1Box.Key:=AValue.Key1;
  PrimaryKey1Box.ShiftState:=AValue.Shift1;
  PrimaryKey2Box.Key:=AValue.Key2;
  PrimaryKey2Box.ShiftState:=AValue.Shift2;
end;

function TShortCutDialog.GetPrimaryShortCut: TIDEShortCut;
begin
  Result.Key1:=PrimaryKey1Box.Key;
  Result.Shift1:=PrimaryKey1Box.ShiftState;
  Result.Key2:=PrimaryKey2Box.Key;
  Result.Shift2:=PrimaryKey2Box.ShiftState;
end;

function TShortCutDialog.GetSecondaryShortCut: TIDEShortCut;
begin
  Result.Key1:=SecondaryKey1Box.Key;
  Result.Shift1:=SecondaryKey1Box.ShiftState;
  Result.Key2:=SecondaryKey2Box.Key;
  Result.Shift2:=SecondaryKey2Box.ShiftState;
end;

procedure TShortCutDialog.SetSecondaryShortCut(const AValue: TIDEShortCut);
var
  ASecondaryShortCut: TIDEShortCut;
begin
  ASecondaryShortCut:=SecondaryShortCut;
  if CompareIDEShortCuts(@ASecondaryShortCut,@AValue)=0 then exit;
  SecondaryKey1Box.Key:=AValue.Key1;
  SecondaryKey1Box.ShiftState:=AValue.Shift1;
  SecondaryKey2Box.Key:=AValue.Key2;
  SecondaryKey2Box.ShiftState:=AValue.Shift2;
end;

procedure TShortCutDialog.SetShowSequence(const AValue: boolean);
begin
  if FShowSequence=AValue then exit;
  FShowSequence:=AValue;
  FPrimaryKey2Box.Visible:=FShowSequence;
  FSecondaryKey2Box.Visible:=FShowSequence;
  UpdateCaptions;
end;

function TShortCutDialog.ResolveConflicts(Key: TIDEShortCut;
  Scope: TIDECommandScope): boolean;
type
  TConflictType = (ctNone,ctConflictKeyA,ctConflictKeyB);
var
  ConflictRelation: TKeyCommandRelation;
  ConflictName: String;
  CurRelation: TKeyCommandRelation;
  CurName: String;
  j: integer;
  conflictType: TConflictType;
begin
  // search for conflict
  CurRelation:=KeyCommandRelationList.Relations[RelationIndex];
  if Key.Key1=VK_UNKNOWN then
  begin
    Result:=true;
    exit;
  end;
  //Try to find an IDE command that conflicts
  for j:=0 to KeyCommandRelationList.RelationCount-1 do begin
    conflictType:=ctNone;
    ConflictRelation:=KeyCommandRelationList.Relations[j];
    with ConflictRelation do
    begin
      if (j=RelationIndex) then continue;

      if not Category.ScopeIntersects(Scope) then continue;

      if ((Key.Key1=ShortcutA.Key1) and (Key.Shift1=ShortcutA.Shift1))
      and (((Key.Key2=ShortcutA.Key2) and (Key.Shift2=ShortcutA.Shift2))
            or (Key.Key2=VK_UNKNOWN) or (ShortcutA.Key2=VK_UNKNOWN))
      then begin
        conflictType:=ctConflictKeyA; // ShortcutA bites
      end
      else if ((Key.Key1=ShortcutB.Key1) and (Key.Shift1=ShortcutB.Shift1))
      and (((Key.Key2=ShortcutB.Key2) and (Key.Shift2=ShortcutB.Shift2))
           or (Key.Key2=VK_UNKNOWN) or (ShortcutB.Key2=VK_UNKNOWN))
      then begin
        conflictType:=ctConflictKeyB; // ShortcutB bites
      end;
    end;
    if (conflictType<>ctNone) then begin
      CurName:=CurRelation.GetCategoryAndName;
      ConflictName:=ConflictRelation.GetCategoryAndName;
      if conflictType=ctConflictKeyA then
        ConflictName:=ConflictName
                  +' ('+KeyAndShiftStateToEditorKeyString(ConflictRelation.ShortcutA)
      else
        ConflictName:=ConflictName
                 +' ('+KeyAndShiftStateToEditorKeyString(ConflictRelation.ShortcutB);
      if MessageDlg(lisPEConflictFound,
         Format(lisTheKeyIsAlreadyAssignedToRemoveTheOldAssignmentAnd, [
           KeyAndShiftStateToEditorKeyString(Key), #13, ConflictName, #13, #13,
           #13, CurName]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      begin
        Result:=false;
        exit;
      end;
      if (conflictType=ctConflictKeyA) then
        ConflictRelation.ShortcutA:=ConflictRelation.ShortcutB;
      ConflictRelation.ClearShortcutB;
    end;
  end;

  Result:=true;
end;

procedure TShortCutDialog.UpdateCaptions;
begin
  if ShowSequence then begin
    PrimaryGroupBox.Caption:=lisKeyOr2KeySequence;
    SecondaryGroupBox.Caption:=lisAlternativeKeyOr2KeySequence;
  end else begin
    PrimaryGroupBox.Caption:=lisEdtExtToolKey;
    SecondaryGroupBox.Caption:=lisAlternativeKey;
  end;
end;

procedure TShortCutDialog.ClearKeys;
begin
  PrimaryShortCut:=CleanIDEShortCut;
  SecondaryShortCut:=CleanIDEShortCut;
end;

procedure TShortCutDialog.SetRelation(
  AKeyCommandRelationList: TKeyCommandRelationList; Index: integer);
var
  CurRelation: TKeyCommandRelation;
begin
  KeyCommandRelationList:=AKeyCommandRelationList;
  RelationIndex:=Index;
  CurRelation:=AKeyCommandRelationList.Relations[RelationIndex];
  PrimaryShortCut:=CurRelation.ShortcutA;
  SecondaryShortCut:=CurRelation.ShortcutB;
  Caption:=srkmCommand+' "'+CurRelation.LocalizedName+'"';
end;

end.

