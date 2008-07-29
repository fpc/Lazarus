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
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLType,
  IDECommands, IDEWindowIntf,
  KeyMapping, LazarusIDEStrConsts;

type
  { TCustomShortCutGrabBox }

  TCustomShortCutGrabBox = class(TCustomPanel)
  private
    FAllowedShifts: TShiftState;
    FGrabButton: TButton;
    FKey: Word;
    FKeyComboBox: TComboBox;
    FShiftButtons: TShiftState;
    FShiftState: TShiftState;
    FCheckBoxes: array[TShiftStateEnum] of TCheckBox;
    FGrabForm: TForm;
    function GetShiftCheckBox(Shift: TShiftStateEnum): TCheckBox;
    procedure SetAllowedShifts(const AValue: TShiftState);
    procedure SetKey(const AValue: Word);
    procedure SetShiftButtons(const AValue: TShiftState);
    procedure SetShiftState(const AValue: TShiftState);
    procedure OnGrabButtonClick(Sender: TObject);
    procedure OnShitCheckBoxClick(Sender: TObject);
    procedure OnGrabFormKeyDown(Sender: TObject; var AKey: Word;
      AShift: TShiftState);
    procedure OnKeyComboboxEditingDone(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure UpdateShiftButons;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
           override;
    function ShiftToStr(s: TShiftStateEnum): string;
  public
    constructor Create(TheOwner: TComponent); override;
    function GetDefaultShiftButtons: TShiftState;
    property ShiftState: TShiftState read FShiftState write SetShiftState;
    property Key: Word read FKey write SetKey;
    property ShiftButtons: TShiftState read FShiftButtons write SetShiftButtons;
    property AllowedShifts: TShiftState read FAllowedShifts write SetAllowedShifts;
    property KeyComboBox: TComboBox read FKeyComboBox;
    property GrabButton: TButton read FGrabButton;
    property ShiftCheckBox[Shift: TShiftStateEnum]: TCheckBox read GetShiftCheckBox;
  end;

  { TShortCutGrabBox }

  TShortCutGrabBox = class(TCustomShortCutGrabBox)
  published
    property Align;
    property Alignment;
    property AllowedShifts;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property Key;
    property OnClick;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetDockCaption;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShiftButtons;
    property ShiftState;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
  end;

  { TShortCutDialog }

  TShortCutDialog = class(TForm)
    BtnPanel: TPanel;
    PrimaryGroupBox: TGroupBox;
    SecondaryGroupBox: TGroupBox;
    OkButton: TButton;
    CancelButton: TButton;
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

{ TCustomShortCutGrabBox }

procedure TCustomShortCutGrabBox.SetKey(const AValue: Word);
var
  s: String;
  i: LongInt;
begin
  if FKey=AValue then exit;
  FKey:=AValue;
  s:=KeyAndShiftStateToEditorKeyString(FKey,[]);
  i:=KeyComboBox.Items.IndexOf(s);
  if i>=0 then
    KeyComboBox.ItemIndex:=i
  else if EditorKeyStringIsIrregular(s) then begin
    KeyComboBox.Items.Add(s);
    KeyComboBox.ItemIndex:=KeyComboBox.Items.IndexOf(s);
  end else
    KeyComboBox.ItemIndex:=0;
end;

procedure TCustomShortCutGrabBox.OnGrabButtonClick(Sender: TObject);
begin
  FGrabForm:=TForm.Create(Self);
  FGrabForm.KeyPreview:=true;
  FGrabForm.Position:=poDesktopCenter;
  FGrabForm.OnKeyDown:=@OnGrabFormKeyDown;
  FGrabForm.Caption:='Press a key ...';
  with TLabel.Create(Self) do begin
    Caption:='Press a key ...';
    BorderSpacing.Around:=25;
    Parent:=FGrabForm;
  end;
  FGrabForm.AutoSize:=true;
  FGrabForm.ShowModal;
  FreeAndNil(FGrabForm);
end;

procedure TCustomShortCutGrabBox.OnShitCheckBoxClick(Sender: TObject);
var
  s: TShiftStateEnum;
begin
  for s:=Low(TShiftStateEnum) to High(TShiftStateEnum) do
    if FCheckBoxes[s]=Sender then
      if FCheckBoxes[s].Checked then
        Include(FShiftState,s)
      else
        Exclude(FShiftState,s);
end;

procedure TCustomShortCutGrabBox.OnGrabFormKeyDown(Sender: TObject;
  var AKey: Word; AShift: TShiftState);
begin
  //DebugLn(['TCustomShortCutGrabBox.OnGrabFormKeyDown ',AKey,' ',dbgs(AShift)]);
  if not (AKey in [VK_CONTROL, VK_LCONTROL, VK_RCONTROL,
             VK_SHIFT, VK_LSHIFT, VK_RSHIFT,
             VK_MENU, VK_LMENU, VK_RMENU,
             VK_UNKNOWN, VK_UNDEFINED])
  then begin
    Key:=AKey;
    ShiftState:=AShift;
    FGrabForm.ModalResult:=mrOk;
  end;
end;

procedure TCustomShortCutGrabBox.OnKeyComboboxEditingDone(Sender: TObject);
begin
  Key:=EditorKeyStringToVKCode(KeyComboBox.Text);
end;

function TCustomShortCutGrabBox.GetShiftCheckBox(Shift: TShiftStateEnum
  ): TCheckBox;
begin
  Result:=FCheckBoxes[Shift];
end;

procedure TCustomShortCutGrabBox.SetAllowedShifts(const AValue: TShiftState);
begin
  if FAllowedShifts=AValue then exit;
  FAllowedShifts:=AValue;
  ShiftState:=ShiftState*FAllowedShifts;
end;

procedure TCustomShortCutGrabBox.SetShiftButtons(const AValue: TShiftState);
begin
  if FShiftButtons=AValue then exit;
  FShiftButtons:=AValue;
  UpdateShiftButons;
end;

procedure TCustomShortCutGrabBox.SetShiftState(const AValue: TShiftState);
var
  s: TShiftStateEnum;
begin
  if FShiftState=AValue then exit;
  FShiftState:=AValue;
  for s:=low(TShiftStateEnum) to High(TShiftStateEnum) do
    if FCheckBoxes[s]<>nil then
      FCheckBoxes[s].Checked:=s in FShiftState;
end;

procedure TCustomShortCutGrabBox.Loaded;
begin
  inherited Loaded;
  UpdateShiftButons;
end;

procedure TCustomShortCutGrabBox.UpdateShiftButons;
var
  s: TShiftStateEnum;
  LastCheckBox: TCheckBox;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  LastCheckBox:=nil;
  DisableAlign;
  try
    for s:=low(TShiftStateEnum) to High(TShiftStateEnum) do begin
      if s in FShiftButtons then begin
        if FCheckBoxes[s]=nil then begin
          FCheckBoxes[s]:=TCheckBox.Create(Self);
          with FCheckBoxes[s] do begin
            Name:='CheckBox'+ShiftToStr(s);
            Caption:=ShiftToStr(s);
            AutoSize:=true;
            Checked:=s in FShiftState;
            if LastCheckBox<>nil then
              AnchorToNeighbour(akLeft,6,LastCheckBox)
            else
              AnchorParallel(akLeft,0,Self);
            AnchorParallel(akTop,0,Self);
            AnchorParallel(akBottom,0,Self);
            Parent:=Self;
            OnClick:=@OnShitCheckBoxClick;
          end;
        end;
        LastCheckBox:=FCheckBoxes[s];
      end else begin
        FreeAndNil(FCheckBoxes[s]);
      end;
    end;
    if LastCheckBox<>nil then
      FKeyComboBox.AnchorToNeighbour(akLeft,6,LastCheckBox)
    else
      FKeyComboBox.AnchorParallel(akLeft,0,Self);
  finally
    EnableAlign;
  end;
end;

procedure TCustomShortCutGrabBox.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  s: TShiftStateEnum;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=FGrabButton then
      FGrabButton:=nil;
    if AComponent=FKeyComboBox then
      FKeyComboBox:=nil;
    if AComponent=FGrabForm then
      FGrabForm:=nil;
    for s:=Low(TShiftStateEnum) to High(TShiftStateEnum) do
      if FCheckBoxes[s]=AComponent then begin
        FCheckBoxes[s]:=nil;
        Exclude(FShiftButtons,s);
      end;
  end;
end;

function TCustomShortCutGrabBox.ShiftToStr(s: TShiftStateEnum): string;
begin
  case s of
  ssShift: Result:='Shift';
  ssAlt: Result:='Alt';
  ssCtrl: Result:='Ctrl';
  ssMeta: Result:='Meta';
  ssSuper: Result:='Super';
  ssHyper: {$IFDEF Darwin}
           Result:='Cmd';
           {$ELSE}
           Result:='Hyper';
           {$ENDIF}
  ssAltGr: Result:='AltGr';
  ssCaps: Result:='Caps';
  ssNum: Result:='Numlock';
  ssScroll: Result:='Scroll';
  else Result:='Modifier'+IntToStr(ord(s));
  end;
end;

constructor TCustomShortCutGrabBox.Create(TheOwner: TComponent);
var
  i: Integer;
  s: String;
begin
  inherited Create(TheOwner);

  FAllowedShifts:=[ssShift, ssAlt, ssCtrl,
    ssMeta, ssSuper, ssHyper, ssAltGr,
    ssCaps, ssNum, ssScroll];

  FGrabButton:=TButton.Create(Self);
  with FGrabButton do begin
    Name:='GrabButton';
    Caption:='Grab key';
    Align:=alRight;
    AutoSize:=true;
    Parent:=Self;
    OnClick:=@OnGrabButtonClick;
  end;

  FKeyComboBox:=TComboBox.Create(Self);
  with FKeyComboBox do begin
    Name:='FKeyComboBox';
    AutoSize:=true;
    Items.BeginUpdate;
    for i:=1 to 145 do begin
      s := KeyAndShiftStateToEditorKeyString(i, []);
      if not EditorKeyStringIsIrregular(s) then
        Items.Add(s);
    end;
    Items.EndUpdate;
    OnEditingDone:=@OnKeyComboboxEditingDone;
    Parent:=Self;
    AnchorToNeighbour(akRight,6,FGrabButton);
    AnchorVerticalCenterTo(FGrabButton);
  end;

  BevelOuter:=bvNone;
  ShiftButtons:=GetDefaultShiftButtons;
  ShiftState:=[];
  Key:=VK_UNKNOWN;
  KeyComboBox.Text:=KeyAndShiftStateToEditorKeyString(Key,[]);
end;

function TCustomShortCutGrabBox.GetDefaultShiftButtons: TShiftState;
begin
  {$IFDEF Darwin}
  Result:=[ssCtrl,ssShift,ssAlt,ssMeta];
  {$ELSE}
  Result:=[ssCtrl,ssShift,ssAlt];
  {$ENDIF}
end;

{ TShortCutDialog }

procedure TShortCutDialog.FormCreate(Sender: TObject);
begin
  Caption := srkmEditForCmd;
  OkButton.Caption:=lisOkBtn;
  CancelButton.Caption:=dlgCancel;

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
           or (Key.Key2<>VK_UNKNOWN) or (ShortcutB.Key2=VK_UNKNOWN))
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

initialization
  {$I keymapshortcutdlg.lrs}

end.

