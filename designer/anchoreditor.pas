{ /***************************************************************************
                 AnchorEditor.pas - Lazarus IDE unit
                 -----------------------------------

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Editor for editing Anchors, AnchorSide properties.
}
unit AnchorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, StdCtrls, Buttons, Spin,
  ExtCtrls, Graphics, IDECommands, PropEdits, IDEDialogs, LazarusIDEStrConsts,
  IDEOptionDefs, IDEImagesIntf, EnvironmentOpts;

type

  TAnchorSideRefSet = set of TAnchorSideReference;

  { TAnchorDesignerSideValues }

  TAnchorDesignerSideValues = class
  strict private
    FBorderSpace_IsAmbiguous: boolean;
    FEnabled_IsAmbiguous: boolean;
    FSibling_IsAmbiguous: boolean;
    FAnchorKind: TAnchorKind;
    FBorderSpace: Integer;
    FCount: Integer;
    FEnabled: Boolean;
    FSibling: string;
    FSideRefs: TAnchorSideRefSet;
    function GetSideRef_IsAmbiguous: Boolean;
  public
    constructor Create(TheKind: TAnchorKind);
    procedure SetValues(AControl: TControl);
    procedure MergeValues(AControl: TControl);
  public
    property AnchorKind: TAnchorKind read FAnchorKind;
    property Enabled: boolean read FEnabled write FEnabled;
    property Enabled_IsAmbiguous: boolean read FEnabled_IsAmbiguous write FEnabled_IsAmbiguous;
    property Sibling: string read FSibling write FSibling;
    property Sibling_IsAmbiguous: boolean read FSibling_IsAmbiguous write FSibling_IsAmbiguous;
    property SideRef_IsAmbiguous: Boolean read GetSideRef_IsAmbiguous;
    property BorderSpace: integer read FBorderSpace write FBorderSpace;
    property BorderSpace_IsAmbiguous: boolean read FBorderSpace_IsAmbiguous write FBorderSpace_IsAmbiguous;
    property SideRefs: TAnchorSideRefSet read FSideRefs;
    property Count: Integer read FCount;
  end;

  { TAnchorDesignerValues }

  TAnchorDesignerValues = class
  strict private
    FAmbiguousBorderspaceAround: boolean;
    FBorderspaceAround: integer;
    FSides: array[TAnchorKind] of TAnchorDesignerSideValues;
    function GetSides(Kind: TAnchorKind): TAnchorDesignerSideValues;
    procedure SetAmbiguousBorderspaceAround(const AValue: boolean);
    procedure SetBorderspaceAround(const AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetValues(AControl: TControl);
    procedure MergeValues(AControl: TControl);
  public
    property Sides[Kind: TAnchorKind]: TAnchorDesignerSideValues read GetSides;
    property BorderspaceAround: integer read FBorderspaceAround write SetBorderspaceAround;
    property AmbiguousBorderspaceAround: boolean read FAmbiguousBorderspaceAround write SetAmbiguousBorderspaceAround;
  end;

  { TAnchorDesignerSideControls }

  TAnchorDesigner = class;

  TAnchorDesignerSideControls = class
  strict private
    FForm: TAnchorDesigner;
    {Controls}
    FEnabled_CheckBox: TCheckBox;
    FSibling_ComboBox: TComboBox;
    FSideRef_SButtons: array [TAnchorSideReference] of TSpeedButton;
    FBorderSpace_SpinEdit: TSpinEdit;
    {Frames}
    FEnabled_Frame: TShape;
    FSibling_Frame: TShape;
    FSideRef_Frames: array [TAnchorSideReference] of TShape;
    FBorderSpace_Frame: TShape;
  public
    constructor Create(
        AEnabled: TCheckBox;
        ASibling: TComboBox;
        ABorderSpace_SpinEdit: TSpinEdit;
        ASideRefTop{Left}, ASideRefCenter, ASideRefBottom{Right}: TSpeedButton;
        AForm: TAnchorDesigner);
    procedure Refresh(ASideValues: TAnchorDesignerSideValues);
  end;

  { TAnchorDesigner }

  TAnchorDesigner = class(TForm)
    AroundBorderSpaceSpinEdit: TSpinEdit;
    BorderSpaceGroupBox: TGroupBox;
    BottomAnchoredCheckBox: TCheckBox;
    BottomBorderSpaceSpinEdit: TSpinEdit;
    BottomGroupBox: TGroupBox;
    BottomRefBottomSpeedButton: TSpeedButton;
    BottomRefCenterSpeedButton: TSpeedButton;
    BottomRefTopSpeedButton: TSpeedButton;
    BottomSiblingComboBox: TComboBox;
    BottomSiblingLabel: TLabel;
    LeftAnchoredCheckBox: TCheckBox;
    LeftBorderSpaceSpinEdit: TSpinEdit;
    LeftGroupBox: TGroupBox;
    LeftRefCenterSpeedButton: TSpeedButton;
    LeftRefLeftSpeedButton: TSpeedButton;
    LeftRefRightSpeedButton: TSpeedButton;
    LeftSiblingComboBox: TComboBox;
    LeftSiblingLabel: TLabel;
    RightAnchoredCheckBox: TCheckBox;
    RightBorderSpaceSpinEdit: TSpinEdit;
    RightGroupBox: TGroupBox;
    RightRefCenterSpeedButton: TSpeedButton;
    RightRefLeftSpeedButton: TSpeedButton;
    RightRefRightSpeedButton: TSpeedButton;
    RightSiblingComboBox: TComboBox;
    RightSiblingLabel: TLabel;
    TopAnchoredCheckBox: TCheckBox;
    TopBorderSpaceSpinEdit: TSpinEdit;
    TopGroupBox: TGroupBox;
    TopRefBottomSpeedButton: TSpeedButton;
    TopRefCenterSpeedButton: TSpeedButton;
    TopRefTopSpeedButton: TSpeedButton;
    TopSiblingComboBox: TComboBox;
    TopSiblingLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AnchorEnabledCheckBoxChange(Sender: TObject);
    procedure BorderSpaceSpinEditChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure SiblingComboBoxChange(Sender: TObject);
    procedure ReferenceSideButtonClicked(Sender: TObject);
  private
    Values: TAnchorDesignerValues;
    FSelection: TPersistentSelectionList;
    FSelectedControlsList: TList;
    FUpdating: Boolean;
    FNeedUpdate: boolean;
    FSideControls: array[TAnchorKind] of TAnchorDesignerSideControls;
    procedure Refresh;
    procedure OnRefreshPropertyValues;
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    function GetSelectedControls: TList;
    function FindSibling(const Sibling: string): TControl;
    procedure FillComboBoxWithSiblings(AComboBox: TComboBox);
    function AnchorDesignerNoSiblingText: string;
    function AnchorDesignerNeighbourText(direction: TAnchorKind): string;
    procedure CollectValues(const ASelection: TList;
                            out TheValues: TAnchorDesignerValues;
                            out SelectedControlCount: integer);
    procedure SetCaptionsAndHints;
    procedure LoadGlyphs;
    procedure CreateSideControls;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure UpdateShowing; override;
  public
    destructor Destroy; override;
    class function ControlToStr(AControl: TControl): string;
    property Selection: TPersistentSelectionList read FSelection;
  end;
  
  { TAnchorPropertyEditor }

  TAnchorPropertyEditor = class(TSetPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  
var
  AnchorDesigner: TAnchorDesigner = nil;
  ShowAnchorDesigner: TNotifyEvent = nil;

implementation

{$R *.lfm}

type
  TFrameStyle=(fsNormal, fsAmbigous, fsIgnored, fsNone);

const
  FrameMargin = 1;

  clNormal=clRed;             // color for fsNormal
  clAmbigous=clRed;           // color for fsAmbigous
  clIgnored=clBtnShadow;      // color for fsIgnored

  psNormal=psSolid;           // pen style for fsNormal
  psAmbigous=psDot;           // pen style for fsAmbigous
  psIgnored=psSolid;          // pen style for fsIgnored


function CreateFrameForControl(AControl: TControl; AFrameMargin: Integer=FrameMargin): TShape;
begin
  Result:=TShape.Create(AControl.Owner);
  with Result do
  begin
    Brush.Style:=bsClear;
    Brush.Color:=clNone;
    Shape:=stRectangle;
    Pen.Cosmetic:=True;
    Height:=AControl.Height+AFrameMargin*2;
    Width:=AControl.Width+AFrameMargin*2;
    Parent:=AControl.Parent;
    AnchorVerticalCenterTo(AControl);
    AnchorHorizontalCenterTo(AControl);
    SendToBack;
    Visible:=False;
  end;
end;

procedure SetEnabledControls(AEnabled: Boolean; const AControls: array of TControl);
var
  i: Integer;
begin
  for i:=Low(AControls) to High(AControls) do
    AControls[i].Enabled:=AEnabled;
end;

procedure SetFrameStyle(AFrame: TShape; AStyle: TFrameStyle);
begin
  case AStyle of
    fsNormal:
    begin
      AFrame.Pen.Color:=clNormal;
      AFrame.Pen.Style:=psNormal;
      AFrame.Visible:=True;
    end;

    fsAmbigous:
    begin
      AFrame.Pen.Color:=clAmbigous;
      AFrame.Pen.Style:=psAmbigous;
      AFrame.Visible:=True;
    end;

    fsIgnored:
    begin
      AFrame.Pen.Color:=clIgnored;
      AFrame.Pen.Style:=psIgnored;
      AFrame.Visible:=True;
    end;

    fsNone:
    begin
      AFrame.Visible:=False;
    end;
  end;
end;

{ TAnchorDesignerSideControls }

constructor TAnchorDesignerSideControls.Create(
    AEnabled: TCheckBox;
    ASibling: TComboBox;
    ABorderSpace_SpinEdit: TSpinEdit;
    ASideRefTop{Left}, ASideRefCenter, ASideRefBottom{Right}: TSpeedButton;
    AForm: TAnchorDesigner);
var
  SideRefIndex: TAnchorSideReference;
begin
  inherited Create;
  FForm:=AForm;
  // Assign controls
  FEnabled_CheckBox:=AEnabled;
  FSibling_ComboBox:=ASibling;
  FBorderSpace_SpinEdit:=ABorderSpace_SpinEdit;
  FSideRef_SButtons[asrTop]:=ASideRefTop;
  FSideRef_SButtons[asrCenter]:=ASideRefCenter;
  FSideRef_SButtons[asrBottom]:=ASideRefBottom;
  // Create and assign frames
  FEnabled_Frame:=CreateFrameForControl(FEnabled_CheckBox);
  FSibling_Frame:=CreateFrameForControl(FSibling_ComboBox);
  FBorderSpace_Frame:=CreateFrameForControl(FBorderSpace_SpinEdit);
  for SideRefIndex:=Low(SideRefIndex) to High(SideRefIndex) do
    FSideRef_Frames[SideRefIndex]:=CreateFrameForControl(FSideRef_SButtons[SideRefIndex]);
end;

procedure TAnchorDesignerSideControls.Refresh(ASideValues: TAnchorDesignerSideValues);
var
  SiblingText: String;
  SideRefIndex: TAnchorSideReference;
begin
  // Enabled
  FEnabled_CheckBox.AllowGrayed:=ASideValues.Enabled_IsAmbiguous;
  if ASideValues.Enabled_IsAmbiguous then
    FEnabled_CheckBox.State:=cbGrayed
  else
    FEnabled_CheckBox.Checked:=ASideValues.Enabled;

  // BorderSpace
  if ASideValues.BorderSpace_IsAmbiguous then
    FBorderSpace_SpinEdit.Value:=-1
  else
    FBorderSpace_SpinEdit.Value:=ASideValues.BorderSpace;
  FBorderSpace_SpinEdit.ValueEmpty:=ASideValues.BorderSpace_IsAmbiguous;

  // Sibling
  SiblingText:=ASideValues.Sibling;
  FSibling_ComboBox.Text:=SiblingText;
  FForm.FillComboBoxWithSiblings(FSibling_ComboBox);

  // SideRefs                (after Enabled & Sibling)
  for SideRefIndex:=Low(SideRefIndex) to High(SideRefIndex) do
  begin
    // Set Down
    FSideRef_SButtons[SideRefIndex].Down:=(SideRefIndex in ASideValues.SideRefs);

    // Not Down => fsNone
    if not FSideRef_SButtons[SideRefIndex].Down then
    begin
      SetFrameStyle(FSideRef_Frames[SideRefIndex], fsNone);
      Continue;
    end;

    // Single select
    if ASideValues.Count=1 then
      if FEnabled_CheckBox.Checked and (FSibling_ComboBox.Text<>'') then
      begin
        SetFrameStyle(FSideRef_Frames[SideRefIndex], fsNormal);
        Continue;
      end
      else
      begin
        SetFrameStyle(FSideRef_Frames[SideRefIndex], fsIgnored);
        Continue;
      end;

    // Multiselect
    if ASideValues.SideRef_IsAmbiguous then
    begin
      SetFrameStyle(FSideRef_Frames[SideRefIndex], fsAmbigous);
      Continue;
    end
    else
      if (FEnabled_CheckBox.State<>cbUnchecked) then
      begin
        SetFrameStyle(FSideRef_Frames[SideRefIndex], fsNormal);
        Continue;
      end
      else
      begin
        SetFrameStyle(FSideRef_Frames[SideRefIndex], fsIgnored);
        Continue;
      end;
  end;
end;

{ TAnchorDesigner }

procedure TAnchorDesigner.FormCreate(Sender: TObject);
begin
  Name:=NonModalIDEWindowNames[nmiwAnchorEditor];
  KeyPreview:=true;
  FSelection:=TPersistentSelectionList.Create;
  FSelectedControlsList := TList.Create;

  SetCaptionsAndHints;
  LoadGlyphs;
  CreateSideControls;

  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
end;

procedure TAnchorDesigner.FormDestroy(Sender: TObject);
  var
  i: TAnchorKind;
begin
  FreeAndNil(Values);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
  FreeAndNil(FSelection);
  FreeAndNil(FSelectedControlsList);

  for i:=Low(i) to High(i) do
    FSideControls[i].Free;
end;

procedure TAnchorDesigner.SetCaptionsAndHints;
var
  AnchorEnabledHint: String;
begin
  AnchorEnabledHint:=lisAnchorEnabledHint;

  AroundBorderSpaceSpinEdit.Hint:=lisAroundBorderSpaceHint;
  BorderSpaceGroupBox.Caption:=lisBorderSpace;

  BottomAnchoredCheckBox.Caption:=lisEnabled;
  BottomAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akBottom']);
  BottomBorderSpaceSpinEdit.Hint:=lisBottomBorderSpaceSpinEditHint;
  BottomGroupBox.Caption:=lisBottomGroupBoxCaption;
  BottomRefBottomSpeedButton.Hint:=lisAnchorBottomToBottomSide;
  BottomRefCenterSpeedButton.Hint:=lisCenterControlVerticallyRelativeToSibling;
  BottomRefTopSpeedButton.Hint:=lisAnchorBottomToTopSide;
  BottomSiblingComboBox.Hint:=lisBottomSiblingComboBoxHint;
  BottomSiblingLabel.Caption:=lisSibling;

  LeftAnchoredCheckBox.Caption:=lisEnabled;
  LeftAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akLeft']);
  LeftBorderSpaceSpinEdit.Hint:=lisLeftBorderSpaceSpinEditHint;
  LeftGroupBox.Caption:=lisLeftGroupBoxCaption;
  LeftRefCenterSpeedButton.Hint:=lisCenterControlHorizontallyRelativeToSibling;
  LeftRefLeftSpeedButton.Hint:=lisAnchorLeftToLeftSide;
  LeftRefRightSpeedButton.Hint:=lisAnchorLeftToRightSide;
  LeftSiblingComboBox.Hint:=lisLeftSiblingComboBoxHint;
  LeftSiblingLabel.Caption:=lisSibling;

  RightAnchoredCheckBox.Caption:=lisEnabled;
  RightAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akRight']);
  RightBorderSpaceSpinEdit.Hint:=lisRightBorderSpaceSpinEditHint;
  RightGroupBox.Caption:=lisRightAnchoring;
  RightRefCenterSpeedButton.Hint:=lisCenterControlHorizontallyRelativeToSibling;
  RightRefLeftSpeedButton.Hint:=lisAnchorRightToLeftSide;
  RightRefRightSpeedButton.Hint:=lisAnchorRightToRightSide;
  RightSiblingComboBox.Hint:=lisRightSiblingComboBoxHint;
  RightSiblingLabel.Caption:=lisSibling;

  TopAnchoredCheckBox.Caption:=lisEnabled;
  TopAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akTop']);
  TopBorderSpaceSpinEdit.Hint:=lisTopBorderSpaceSpinEditHint;
  TopGroupBox.Caption:=lisTopAnchoring;
  TopRefBottomSpeedButton.Hint:=lisAnchorTopToBottomSide;
  TopRefCenterSpeedButton.Hint:=lisCenterControlVerticallyRelativeToSibling;
  TopRefTopSpeedButton.Hint:= lisAnchorTopToTopSide;
  TopSiblingComboBox.Hint:=lisTopSiblingComboBoxHint;
  TopSiblingLabel.Caption:=lisSibling;
end;

procedure TAnchorDesigner.LoadGlyphs;
begin
  IDEImages.AssignImage(LeftRefLeftSpeedButton, 'anchor_left_left');
  IDEImages.AssignImage(LeftRefCenterSpeedButton, 'anchor_left_center');
  IDEImages.AssignImage(LeftRefRightSpeedButton, 'anchor_left_right');
  IDEImages.AssignImage(RightRefLeftSpeedButton, 'anchor_right_left');
  IDEImages.AssignImage(RightRefCenterSpeedButton, 'anchor_right_center');
  IDEImages.AssignImage(RightRefRightSpeedButton, 'anchor_right_right');
  IDEImages.AssignImage(TopRefTopSpeedButton, 'anchor_top_top');
  IDEImages.AssignImage(TopRefCenterSpeedButton, 'anchor_top_center');
  IDEImages.AssignImage(TopRefBottomSpeedButton, 'anchor_top_bottom');
  IDEImages.AssignImage(BottomRefTopSpeedButton, 'anchor_bottom_top');
  IDEImages.AssignImage(BottomRefCenterSpeedButton, 'anchor_bottom_center');
  IDEImages.AssignImage(BottomRefBottomSpeedButton, 'anchor_bottom_bottom');
end;

procedure TAnchorDesigner.CreateSideControls;
begin
  FSideControls[akTop]:=TAnchorDesignerSideControls.Create(
      TopAnchoredCheckBox,
      TopSiblingComboBox,
      TopBorderSpaceSpinEdit,
      TopRefTopSpeedButton,
      TopRefCenterSpeedButton,
      TopRefBottomSpeedButton,
      Self);

  FSideControls[akBottom]:=TAnchorDesignerSideControls.Create(
      BottomAnchoredCheckBox,
      BottomSiblingComboBox,
      BottomBorderSpaceSpinEdit,
      BottomRefTopSpeedButton,
      BottomRefCenterSpeedButton,
      BottomRefBottomSpeedButton,
      Self);

  FSideControls[akLeft]:=TAnchorDesignerSideControls.Create(
      LeftAnchoredCheckBox,
      LeftSiblingComboBox,
      LeftBorderSpaceSpinEdit,
      LeftRefLeftSpeedButton,     // Left <-> Top
      LeftRefCenterSpeedButton,
      LeftRefRightSpeedButton,    // Right <-> Bottom
      Self);

  FSideControls[akRight]:=TAnchorDesignerSideControls.Create(
      RightAnchoredCheckBox,
      RightSiblingComboBox,
      RightBorderSpaceSpinEdit,
      RightRefLeftSpeedButton,    // Left <-> Top
      RightRefCenterSpeedButton,
      RightRefRightSpeedButton,   // Right <-> Bottom
      Self);
end;

procedure TAnchorDesigner.FormShow(Sender: TObject);
begin
  Refresh;
end;

procedure TAnchorDesigner.AnchorEnabledCheckBoxChange(Sender: TObject);
var
  Kind: TAnchorKind;
  CurSide: TAnchorDesignerSideValues;
  NewValue: Boolean;
  SelectedControls: TList;
  i: Integer;
  CurControl: TControl;
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  CheckPosition: Integer;
begin
  if FUpdating or (Values=nil) then exit;
  if Sender=LeftAnchoredCheckBox then
    Kind:=akLeft
  else if Sender=RightAnchoredCheckBox then
    Kind:=akRight
  else if Sender=TopAnchoredCheckBox then
    Kind:=akTop
  else if Sender=BottomAnchoredCheckBox then
    Kind:=akBottom
  else
    exit;
  NewValue:=TCheckBox(Sender).Checked;
  CurSide:=Values.Sides[Kind];
  if CurSide.Enabled_IsAmbiguous or (CurSide.Enabled<>NewValue) then begin
    // user changed an anchor
    SelectedControls:=GetSelectedControls;
    if SelectedControls=nil then exit;

    // check
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if NewValue and (not CurControl.AnchorSide[Kind].CheckSidePosition(
        CurControl.AnchorSide[Kind].Control,
        CurControl.AnchorSide[Kind].Side,
        ReferenceControl,ReferenceSide,CheckPosition))
      then begin
        if IDEMessageDialog(lisCCOWarningCaption, lisThisWillCreateACircularDependency,
                            mtWarning, [mbIgnore, mbCancel]) <> mrIgnore
        then begin
          Refresh;
          exit;
        end;
        break;
      end;
    end;

    // commit
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if NewValue then
        CurControl.Anchors:=CurControl.Anchors+[Kind]
      else
        CurControl.Anchors:=CurControl.Anchors-[Kind];
    end;
    GlobalDesignHook.Modified(Self, 'Anchors');
    GlobalDesignHook.RefreshPropertyValues;
  end;
end;

procedure TAnchorDesigner.BorderSpaceSpinEditChange(Sender: TObject);
var
  Around: Boolean;
  NewValue: LongInt;
  CurSide: TAnchorDesignerSideValues;
  SelectedControls: TList;
  i: Integer;
  CurControl: TControl;
  Kind: TAnchorKind;
begin
  if FUpdating or (Values=nil) then exit;
  Around:=false;
  if Sender=LeftBorderSpaceSpinEdit then
    Kind:=akLeft
  else if Sender=RightBorderSpaceSpinEdit then
    Kind:=akRight
  else if Sender=TopBorderSpaceSpinEdit then
    Kind:=akTop
  else if Sender=BottomBorderSpaceSpinEdit then
    Kind:=akBottom
  else if Sender=AroundBorderSpaceSpinEdit then begin
    Kind:=akLeft;
    Around:=true;
  end else
    exit;
  NewValue:=RoundToInt(TSpinEdit(Sender).Value);
  CurSide:=Values.Sides[Kind];
  if (Around and (Values.AmbiguousBorderspaceAround
                  or (Values.BorderspaceAround<>NewValue)))
  or ((not Around) and (CurSide.BorderSpace_IsAmbiguous
                        or (CurSide.BorderSpace<>NewValue)))
  then begin
    // user changed a BorderSpace
    SelectedControls:=GetSelectedControls;
    if SelectedControls=nil then exit;
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if Around then
        CurControl.BorderSpacing.Around:=NewValue
      else
        CurControl.BorderSpacing.Space[Kind]:=NewValue;
    end;
    GlobalDesignHook.Modified(Self, 'Anchors');
    GlobalDesignHook.RefreshPropertyValues;
  end;
end;

procedure TAnchorDesigner.FormDeactivate(Sender: TObject);
begin
  // commit changes made by keyboard
  if (ActiveControl is TComboBox) then
    SiblingComboBoxChange(ActiveControl);
end;

function compareControlTop(Item1, Item2: pointer): Integer;
begin
  if TControl(Item1).Top<TControl(Item2).Top then Result:=-1
  else if TControl(Item1).Top>TControl(Item2).Top then Result:=1
  else Result:=0;
end;

function compareControlLeft(Item1, Item2: pointer): Integer;
begin
  if TControl(Item1).Left<TControl(Item2).Left then Result:=-1
  else if TControl(Item1).Left>TControl(Item2).Left then Result:=1
  else result:=0;
end;

function compareControlRight(Item1, Item2: pointer): Integer;
begin
  if TControl(item1).left+TControl(item1).width>TControl(item2).left+TControl(item2).width then result:=-1
  else if TControl(item1).left+TControl(item1).width<TControl(item2).left+TControl(item2).width then result:=1
  else result:=0;
end;

function compareControlBottom(Item1, Item2: pointer): Integer;
begin
  if TControl(item1).top+TControl(item1).Height>TControl(item2).top+TControl(item2).Height then result:=-1
  else if TControl(item1).top+TControl(item1).Height<TControl(item2).top+TControl(item2).Height then result:=1
  else result:=0;
end;


procedure TAnchorDesigner.SiblingComboBoxChange(Sender: TObject);
var
  Kind,CurNeighbour: TAnchorKind;
  NewSibling: TControl;
  CurSide: TAnchorDesignerSideValues;
  SelectedControls: TList;
  i: Integer;
  CurControl: TControl;
  NewValue: String;
  UseNeighbours: boolean;
  OldPositions,OldPositions2: array of Integer;

  function NeighbourPosition(c: TControl): Integer;
  begin
    result:=0;
    case CurNeighbour of
      akTop: result:=c.top;
      akLeft: result:=c.Left;
      akRight: result:=c.left+c.Width;
      akBottom: result:=c.Top+c.Height;
    end;
  end;

  function FindNeighbour(i:longint):TControl;
  var firstNeighbour,lastNeighbour,cur,resultId: Integer;
  begin
    if i=0 then exit(nil);
    firstNeighbour:=i-1;
    while (firstNeighbour>=0) and (OldPositions[firstNeighbour] = OldPositions[i]) do
      dec(firstNeighbour);
    if firstNeighbour=-1 then exit(nil); //there is no real neighbour at this side
    lastNeighbour:=firstNeighbour;
    while (lastNeighbour>=0) and (OldPositions[lastNeighbour] = OldPositions[firstNeighbour]) do
      dec(lastNeighbour);
    inc(lastNeighbour);
    //take nearest
    resultId:=lastNeighbour;
    for cur:=lastNeighbour+1 to firstNeighbour do
      if abs(OldPositions2[cur]-OldPositions2[i]) < abs(OldPositions2[resultId]-OldPositions2[i])  then
         resultid:=cur;
    result:=TControl(SelectedControls[resultId]);
 end;

var
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  CheckPosition: Integer;
begin
  if FUpdating or (Values=nil) then exit;
  if Sender=LeftSiblingComboBox then
    Kind:=akLeft
  else if Sender=RightSiblingComboBox then
    Kind:=akRight
  else if Sender=TopSiblingComboBox then
    Kind:=akTop
  else if Sender=BottomSiblingComboBox then
    Kind:=akBottom
  else
    exit;
  NewValue:=TComboBox(Sender).Caption;
  CurSide:=Values.Sides[Kind];
  if CurSide.Sibling_IsAmbiguous or (CompareText(CurSide.Sibling,NewValue)<>0) then
  begin
    // user changed a sibling
    SelectedControls:=GetSelectedControls;
    if SelectedControls=nil then exit;
    UseNeighbours:=false;
    NewSibling:=nil;
    if (NewValue<>AnchorDesignerNoSiblingText) then
    begin
      CurNeighbour:=low(TAnchorKind);
      // Check if relative Anchor to be used, such as '(selected left neighbour)'
      while true do begin
        if NewValue=AnchorDesignerNeighbourText(CurNeighbour) then begin
          UseNeighbours:=true;
          break;
        end;
        if (CurNeighbour = high(TAnchorKind)) then
          break;
        inc(CurNeighbour);
      end;
      if UseNeighbours then
      begin
        case CurNeighbour of //todo: use just one sorting function
          akTop: SelectedControls.Sort(@compareControlTop);
          akLeft: SelectedControls.Sort(@compareControlLeft);
          akRight: SelectedControls.Sort(@compareControlRight);
          akBottom: SelectedControls.Sort(@compareControlBottom);
        end;
        setlength(OldPositions,SelectedControls.Count);
        setlength(OldPositions2,SelectedControls.Count);
        for i:=0 to SelectedControls.Count-1 do begin
          OldPositions[i]:=NeighbourPosition(TControl(SelectedControls[i]));
          case CurNeighbour of
            akLeft,akRight: OldPositions2[i]:=TControl(SelectedControls[i]).top;
            akTop,akBottom: OldPositions2[i]:=TControl(SelectedControls[i]).Left;
          end;
        end;
     end
      else begin
        NewSibling:=FindSibling(NewValue);
        if NewSibling=nil then exit;
      end;
    end;
    // check
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if UseNeighbours then begin
        NewSibling:=findNeighbour(i);
        if (NewSibling=nil) and (i<>0) then continue;
      end;
      if (Kind in CurControl.Anchors)
      and (not CurControl.AnchorSide[Kind].CheckSidePosition(NewSibling,
        CurControl.AnchorSide[Kind].Side,
        ReferenceControl,ReferenceSide,CheckPosition))
      then begin
        if IDEMessageDialog(lisCCOWarningCaption,
          lisThisWillCreateACircularDependency, mtWarning, [mbIgnore, mbCancel])<>
            mrIgnore
        then begin
          Refresh;
          exit;
        end;
        break;
      end;
    end;
    // commit
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if UseNeighbours then begin
        NewSibling:=findNeighbour(i);
        if (NewSibling=nil) and (i<>0) then continue;
      end;
      try
        CurControl.AnchorSide[Kind].Control:=NewSibling;
      except
        on E: Exception do begin
          IDEMessageDialog(lisCCOErrorCaption, lisUnableToSetAnchorSideControl+
            LineEnding+E.Message,
            mtError,[mbCancel]);
        end;
      end;
    end;

    GlobalDesignHook.Modified(Self, 'Anchors');
    GlobalDesignHook.RefreshPropertyValues;
    if UseNeighbours then
      TComboBox(Sender).Caption:=NewValue;
  end;
end;

procedure TAnchorDesigner.ReferenceSideButtonClicked(Sender: TObject);
var
  Kind: TAnchorKind;
  SideRef: TAnchorSideReference;
  SelectedControls: TList;
  i: Integer;
  CurControl: TControl;
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  CheckPosition: Integer;
begin
  if FUpdating or (Values=nil) then Exit;

  // Get Kind and SideRef
  if Sender=LeftRefCenterSpeedButton then begin
    Kind:=akLeft;
    SideRef:=asrCenter;
  end
  else if Sender=LeftRefLeftSpeedButton then begin
    Kind:=akLeft;
    SideRef:=asrLeft;
  end
  else if Sender=LeftRefRightSpeedButton then begin
    Kind:=akLeft;
    SideRef:=asrRight;
  end
  else if Sender=RightRefCenterSpeedButton then begin
    Kind:=akRight;
    SideRef:=asrCenter;
  end
  else if Sender=RightRefLeftSpeedButton then begin
    Kind:=akRight;
    SideRef:=asrLeft;
  end
  else if Sender=RightRefRightSpeedButton then begin
    Kind:=akRight;
    SideRef:=asrRight;
  end
  else if Sender=TopRefCenterSpeedButton then begin
    Kind:=akTop;
    SideRef:=asrCenter;
  end
  else if Sender=TopRefTopSpeedButton then begin
    Kind:=akTop;
    SideRef:=asrTop;
  end
  else if Sender=TopRefBottomSpeedButton then begin
    Kind:=akTop;
    SideRef:=asrBottom;
  end
  else if Sender=BottomRefCenterSpeedButton then begin
    Kind:=akBottom;
    SideRef:=asrCenter;
  end
  else if Sender=BottomRefTopSpeedButton then begin
    Kind:=akBottom;
    SideRef:=asrTop;
  end
  else if Sender=BottomRefBottomSpeedButton then begin
    Kind:=akBottom;
    SideRef:=asrBottom;
  end else
    Exit;

    // User changed a sibling
    SelectedControls:=GetSelectedControls;
    if SelectedControls=nil then Exit;
    // Check
    for i:=0 to SelectedControls.Count-1 do
    begin
      CurControl:=TControl(SelectedControls[i]);
      if (Kind in CurControl.Anchors)
          and (not CurControl.AnchorSide[Kind].CheckSidePosition(
          CurControl.AnchorSide[Kind].Control, SideRef,
          ReferenceControl, ReferenceSide, CheckPosition)) then
      begin
        if IDEMessageDialog(lisCCOWarningCaption,
            lisThisWillCreateACircularDependency,
            mtWarning, [mbIgnore, mbCancel]) <> mrIgnore then
        begin
          Refresh;
          Exit;
        end;
        Break;
      end;
    end;
    // Commit
    for i:=0 to SelectedControls.Count-1 do
    begin
      CurControl:=TControl(SelectedControls[i]);
      CurControl.AnchorSide[Kind].Side:=SideRef;
    end;
    GlobalDesignHook.Modified(Self, 'Anchors');
    GlobalDesignHook.RefreshPropertyValues;
end;

procedure TAnchorDesigner.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
end;

procedure TAnchorDesigner.UpdateShowing;
begin
  inherited UpdateShowing;
  if IsVisible and FNeedUpdate then
    Refresh;
end;

procedure TAnchorDesigner.FillComboBoxWithSiblings(AComboBox: TComboBox);
var
  sl: TStringList;
  i: Integer;
  CurControl: TControl;
  j: Integer;
  Sibling: TControl;
  SelectedControls: TList;
  OldText: String;
  Kind: TAnchorKind;
  HasSelectedSiblings: Boolean;

  function AddSibling(AControl: TControl): boolean;
  var
    NewControlStr: String;
  begin
    Result:=false;
    if AControl.Name='' then exit;
    if SelectedControls.IndexOf(AControl)>=0 then exit;
    NewControlStr:=ControlToStr(AControl);
    if sl.IndexOf(NewControlStr)>=0 then exit;
    sl.Add(NewControlStr);
    Result:=true;
  end;
  
begin
  sl:=TStringList.Create;
  sl.Add(AnchorDesignerNoSiblingText);
  HasSelectedSiblings:=false;
  SelectedControls:=GetSelectedControls;
  if SelectedControls<>nil then begin
    for i:=0 to SelectedControls.Count-1 do begin
      if TObject(SelectedControls[i]) is TControl then begin
        CurControl:=TControl(SelectedControls[i]);
        if (CurControl.Parent<>nil) 
          and not (csDesignInstance in CurControl.ComponentState)
        then begin
          AddSibling(CurControl.Parent);
          for j:=0 to CurControl.Parent.ControlCount-1 do begin
            Sibling:=CurControl.Parent.Controls[j];
            if (Sibling<>CurControl) then begin
              AddSibling(Sibling);
              if SelectedControls.IndexOf(Sibling)>=0 then
                HasSelectedSiblings:=true;
            end;
          end;
        end;
        break;
      end;
    end;
  end;
  if HasSelectedSiblings then
    for Kind:=akTop to akBottom do
      sl.add(AnchorDesignerNeighbourText(Kind));
  OldText:=AComboBox.Text;
  AComboBox.Items.Assign(sl);
  AComboBox.Text:=OldText;
  sl.Free;
end;

function TAnchorDesigner.AnchorDesignerNoSiblingText: string;
begin
  Result:='(nil)';
end;

function TAnchorDesigner.AnchorDesignerNeighbourText(direction: TAnchorKind): string;
begin
  result:='';
  case direction of
    akLeft: result:=lisSelectedLeftNeighbour;
    akRight: result:=lisSelectedRightNeighbour;
    akTop: result:=lisSelectedTopNeighbour;
    akBottom: result:=lisSelectedBottomNeighbour;
  end;
end;

destructor TAnchorDesigner.Destroy;
begin
  inherited Destroy;
  if AnchorDesigner=Self then AnchorDesigner:=nil;
end;

procedure TAnchorDesigner.Refresh;
var
  SelectedControlCount: Integer;
  CurSelection: TList;
  AnchorKindIndex: TAnchorKind;
begin
  // Check if update is needed
  if not IsVisible then
  begin
    FNeedUpdate:=True;
    Exit;
  end;
  if FUpdating then Exit;
  FUpdating:=True;
  FNeedUpdate:=False;
  try
    FreeAndNil(Values);
    CurSelection:=GetSelectedControls;
    CollectValues(CurSelection,Values,SelectedControlCount);  // out TheValues

    SetEnabledControls(Assigned(Values), [BorderSpaceGroupBox, TopGroupBox,
      LeftGroupBox, RightGroupBox, BottomGroupBox]);

    if (Values=nil) then
    begin
      Caption:=lisAnchorEditorNoControlSelected;
    end
    else
    begin
      if CurSelection.Count=1 then
        Caption:=Format(lisAnchorsOf,[TControl(CurSelection[0]).Name])
      else
        Caption:=lisAnchorsOfSelectedControls;

      // All
      BorderSpaceGroupBox.Enabled:=true;
      if Values.AmbiguousBorderspaceAround then
        AroundBorderSpaceSpinEdit.Value:=-1
      else
        AroundBorderSpaceSpinEdit.Value:=Values.BorderspaceAround;

      // Sides
      for AnchorKindIndex:=Low(AnchorKindIndex) to High(AnchorKindIndex) do
        FSideControls[AnchorKindIndex].Refresh(Values.Sides[AnchorKindIndex]);
    end;
  finally
    FUpdating:=False;
  end;
end;

procedure TAnchorDesigner.OnRefreshPropertyValues;
begin
  Refresh;
end;

function TAnchorDesigner.GetSelectedControls: TList;
var
  CurPersistent: TPersistent;
  AControl: TControl;
  i: Integer;
begin
  Result:=nil;
  GlobalDesignHook.GetSelection(FSelection);
  if FSelection=nil then exit;
  // Collect values of selected controls
  for i:=0 to FSelection.Count-1 do begin
    CurPersistent:=FSelection[i];
    if CurPersistent is TControl then begin
      AControl:=TControl(CurPersistent);
      if Result = nil then
      begin
        Result := FSelectedControlsList;
        Result.Clear;
      end;
      Result.Add(AControl);
    end;
  end;
end;

function TAnchorDesigner.FindSibling(const Sibling: string): TControl;
var
  Root: TPersistent;
  RootComponent: TComponent;
  i: Integer;
  CurComponent: TComponent;
  CurControl: TControl;
begin
  Result:=nil;
  Root:=GlobalDesignHook.LookupRoot;
  if not (Root is TComponent) then exit;
  RootComponent:=TComponent(Root);
  if (RootComponent is TControl)
  and (CompareText(Sibling,ControlToStr(TControl(RootComponent)))=0) then begin
    Result:=TControl(RootComponent);
    exit;
  end;
  for i:=0 to RootComponent.ComponentCount-1 do begin
    CurComponent:=TComponent(RootComponent.Components[i]);
    if CurComponent is TControl then begin
      CurControl:=TControl(CurComponent);
      if CompareText(Sibling,ControlToStr(CurControl))=0 then begin
        Result:=CurControl;
        exit;
      end;
    end;
  end;
end;

procedure TAnchorDesigner.FormActivate(Sender: TObject);
begin
  LeftSiblingComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  RightSiblingComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  TopSiblingComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  BottomSiblingComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

class function TAnchorDesigner.ControlToStr(AControl: TControl): string;
begin
  if AControl=nil then
    Result:=''
  else
    Result:=AControl.Name+':'+AControl.ClassName;
end;

procedure TAnchorDesigner.CollectValues(const ASelection: TList; out
  TheValues: TAnchorDesignerValues; out SelectedControlCount: integer);
var
  i: Integer;
  AControl: TControl;
  CurObject: TObject;
begin
  TheValues:=nil;
  SelectedControlCount:=0;
  if ASelection=nil then exit;
  // Collect values of selected controls
  for i:=0 to ASelection.Count-1 do begin
    CurObject:=TObject(ASelection[i]);
    if CurObject is TControl then begin
      AControl:=TControl(CurObject);
      if SelectedControlCount=0 then begin
        TheValues:=TAnchorDesignerValues.Create;
        TheValues.SetValues(AControl);
      end else begin
        TheValues.MergeValues(AControl);
      end;
      inc(SelectedControlCount);
    end;
  end;
end;

procedure TAnchorDesigner.OnSetSelection(
  const ASelection: TPersistentSelectionList);
begin
  if FSelection.IsEqual(ASelection) then exit;
  Refresh;
end;

{ TAnchorDesignerValues }

function TAnchorDesignerValues.GetSides(Kind: TAnchorKind
  ): TAnchorDesignerSideValues;
begin
  Result:=FSides[Kind];
end;

procedure TAnchorDesignerValues.SetAmbiguousBorderspaceAround(
  const AValue: boolean);
begin
  if FAmbiguousBorderspaceAround=AValue then exit;
  FAmbiguousBorderspaceAround:=AValue;
end;

procedure TAnchorDesignerValues.SetBorderspaceAround(const AValue: integer);
begin
  if FBorderspaceAround=AValue then exit;
  FBorderspaceAround:=AValue;
end;

constructor TAnchorDesignerValues.Create;
var
  a: TAnchorKind;
begin
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    FSides[a]:=TAnchorDesignerSideValues.Create(a);
end;

destructor TAnchorDesignerValues.Destroy;
var
  a: TAnchorKind;
begin
  for a:=Low(TAnchorKind) to High(TAnchorKind) do FSides[a].Free;
  inherited Destroy;
end;

procedure TAnchorDesignerValues.SetValues(AControl: TControl);
var
  a: TAnchorKind;
begin
  BorderspaceAround:=AControl.BorderSpacing.Around;
  AmbiguousBorderspaceAround:=false;
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    FSides[a].SetValues(AControl);
end;

procedure TAnchorDesignerValues.MergeValues(AControl: TControl);
var
  a: TAnchorKind;
begin
  FAmbiguousBorderspaceAround:=FAmbiguousBorderspaceAround
                         or (FBorderspaceAround<>AControl.BorderSpacing.Around);
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    FSides[a].MergeValues(AControl);
end;

{ TAnchorDesignerSideValues }

function TAnchorDesignerSideValues.GetSideRef_IsAmbiguous: Boolean;
var
  SideRefIndex: TAnchorSideReference;
  n: Integer;
begin
  n:=0;
  for SideRefIndex:=Low(SideRefIndex) to High(SideRefIndex) do
    if (SideRefIndex in FSideRefs)
      then Inc(n);
  Result:=(n>1);
end;

constructor TAnchorDesignerSideValues.Create(TheKind: TAnchorKind);
begin
  FAnchorKind:=TheKind;
end;

procedure TAnchorDesignerSideValues.SetValues(AControl: TControl);
var
  CurSide: TAnchorSide;
begin
  FCount:=1;
  FBorderSpace_IsAmbiguous:=false;
  FBorderSpace:=AControl.BorderSpacing.GetSpace(FAnchorKind);
  FEnabled_IsAmbiguous:=false;
  FEnabled:=(FAnchorKind in AControl.Anchors);
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FSideRefs:=[CurSide.Side];
  FSibling_IsAmbiguous:=false;
  FSibling:=TAnchorDesigner.ControlToStr(CurSide.Control);
end;

procedure TAnchorDesignerSideValues.MergeValues(AControl: TControl);
var
  CurSide: TAnchorSide;
begin
  Inc(FCount);
  FBorderSpace_IsAmbiguous:=FBorderSpace_IsAmbiguous
      or (FBorderSpace<>AControl.BorderSpacing.GetSpace(FAnchorKind));
  FEnabled_IsAmbiguous:=FEnabled_IsAmbiguous
      or (FEnabled<>(FAnchorKind in AControl.Anchors));
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FSibling_IsAmbiguous:=FSibling_IsAmbiguous
      or (TAnchorDesigner.ControlToStr(CurSide.Control)<>FSibling);
  FSideRefs:=FSideRefs+[CurSide.Side];
end;

{ TAnchorPropertyEditor }

function TAnchorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)+[paDialog];
end;

procedure TAnchorPropertyEditor.Edit;
begin
  ShowAnchorDesigner(Self);
end;

initialization
  RegisterPropertyEditor(TypeInfo(TAnchors), TControl, 'Anchors',
      TAnchorPropertyEditor);

end.

