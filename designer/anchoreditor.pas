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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, Spin,
  IDECommands, PropEdits, IDEWindowIntf,
  LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs;

type

  { TAnchorDesignerSideValues }

  TAnchorDesignerSideValues = class
  private
    FAmbiguousBorderSpace: boolean;
    FAmbiguousEnabled: boolean;
    FAmbiguousSide: boolean;
    FAmbiguousSibling: boolean;
    FAnchorKind: TAnchorKind;
    FBorderSpace: integer;
    FEnabled: boolean;
    FSibling: string;
    FSide: TAnchorSideReference;
  public
    constructor Create(TheKind: TAnchorKind);
    procedure SetValues(AControl: TControl);
    procedure MergeValues(AControl: TControl);
  public
    property AnchorKind: TAnchorKind read FAnchorKind;
    property Enabled: boolean read FEnabled write FEnabled;
    property AmbiguousEnabled: boolean read FAmbiguousEnabled write FAmbiguousEnabled;
    property Sibling: string read FSibling write FSibling;
    property AmbiguousSibling: boolean read FAmbiguousSibling write FAmbiguousSibling;
    property Side: TAnchorSideReference read FSide write FSide;
    property AmbiguousSide: boolean read FAmbiguousSide write FAmbiguousSide;
    property BorderSpace: integer read FBorderSpace write FBorderSpace;
    property AmbiguousBorderSpace: boolean read FAmbiguousBorderSpace write FAmbiguousBorderSpace;
  end;
  

  { TAnchorDesignerValues }

  TAnchorDesignerValues = class
  private
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
    procedure AnchorDesignerCreate(Sender: TObject);
    procedure AnchorDesignerDestroy(Sender: TObject);
    procedure AnchorDesignerShow(Sender: TObject);
    procedure AnchorEnabledCheckBoxChange(Sender: TObject);
    procedure BorderSpaceSpinEditChange(Sender: TObject);
    procedure SiblingComboBoxChange(Sender: TObject);
    procedure ReferenceSideButtonClicked(Sender: TObject);
  private
    Values: TAnchorDesignerValues;
    FSelection: TPersistentSelectionList;
    FSelectedControlsList: TList;
    FUpdating: Boolean;
    procedure Refresh(Force: boolean);
    procedure OnRefreshPropertyValues;
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    function GetSelectedControls: TList;
    function FindSibling(const Sibling: string): TControl;
    procedure FillComboBoxWithSiblings(AComboBox: TComboBox);
    function AnchorDesignerNoSiblingText: string;
    function AnchorDesignerNeighbourText(direction: TAnchorKind): string;
    procedure CollectValues(const ASelection: TList;
                            var TheValues: TAnchorDesignerValues;
                            var SelectedControlCount: integer);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
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

uses 
  math;
{ TAnchorDesigner }

procedure TAnchorDesigner.AnchorDesignerCreate(Sender: TObject);
var
  AnchorEnabledHint: String;
begin
  Name:=NonModalIDEWindowNames[nmiwAnchorEditor];
  KeyPreview:=true;
  FSelection:=TPersistentSelectionList.Create;
  FSelectedControlsList := TList.Create;

  AnchorEnabledHint:=lisAnchorEnabledHint;

  AroundBorderSpaceSpinEdit.Hint:=lisAroundBorderSpaceHint;
  BorderSpaceGroupBox.Caption:=lisBorderSpace;

  BottomAnchoredCheckBox.Caption:=lisEnabled;
  BottomAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akBottom']);
  BottomBorderSpaceSpinEdit.Hint:=lisBottomBorderSpaceSpinEditHint;
  BottomGroupBox.Caption:=lisBottomGroupBoxCaption;
  BottomRefBottomSpeedButton.Hint:=lisAnchorToBottomSideKeepBorderSpace;
  BottomRefCenterSpeedButton.Hint:=lisCenterControlVerticallyRelativeToSibling;
  BottomRefTopSpeedButton.Hint:=lisAnchorToTopSideKeepBorderSpace;
  BottomSiblingComboBox.Hint:=lisBottomSiblingComboBoxHint;
  BottomSiblingLabel.Caption:=lisSibling;

  LeftAnchoredCheckBox.Caption:=lisEnabled;
  LeftAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akLeft']);
  LeftBorderSpaceSpinEdit.Hint:=lisLeftBorderSpaceSpinEditHint;
  LeftGroupBox.Caption:=lisLeftGroupBoxCaption;
  LeftRefCenterSpeedButton.Hint:=lisCenterControlHorizontallyRelativeToSibling;
  LeftRefLeftSpeedButton.Hint:=lisAnchorToLeftSideKeepBorderSpace;
  LeftRefRightSpeedButton.Hint:=lisAnchorToRightSideKeepBorderSpace;
  LeftSiblingComboBox.Hint:=lisLeftSiblingComboBoxHint;
  LeftSiblingLabel.Caption:=lisSibling;

  RightAnchoredCheckBox.Caption:=lisEnabled;
  RightAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akRight']);
  RightBorderSpaceSpinEdit.Hint:=lisRightBorderSpaceSpinEditHint;
  RightGroupBox.Caption:=lisRightAnchoring;
  RightRefCenterSpeedButton.Hint:=lisCenterControlHorizontallyRelativeToSibling;
  RightRefLeftSpeedButton.Hint:=lisAnchorToLeftSideKeepBorderSpace;
  RightRefRightSpeedButton.Hint:=lisAnchorToRightSideKeepBorderSpace;
  RightSiblingComboBox.Hint:=lisRightSiblingComboBoxHint;
  RightSiblingLabel.Caption:=lisSibling;

  TopAnchoredCheckBox.Caption:=lisEnabled;
  TopAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akTop']);
  TopBorderSpaceSpinEdit.Hint:=lisTopBorderSpaceSpinEditHint;
  TopGroupBox.Caption:=lisTopAnchoring;
  TopRefBottomSpeedButton.Hint:=lisAnchorToBottomSideKeepBorderSpace;
  TopRefCenterSpeedButton.Hint:=lisCenterControlVerticallyRelativeToSibling;
  TopRefTopSpeedButton.Hint:= lisAnchorToTopSideKeepBorderSpace;
  TopSiblingComboBox.Hint:=lisTopSiblingComboBoxHint;
  TopSiblingLabel.Caption:=lisSibling;

  LeftRefLeftSpeedButton.LoadGlyphFromLazarusResource('anchor_left');
  LeftRefCenterSpeedButton.LoadGlyphFromLazarusResource('anchor_center_horizontal');
  LeftRefRightSpeedButton.LoadGlyphFromLazarusResource('anchor_left_right');
  RightRefLeftSpeedButton.LoadGlyphFromLazarusResource('anchor_left_right');
  RightRefCenterSpeedButton.LoadGlyphFromLazarusResource('anchor_center_horizontal');
  RightRefRightSpeedButton.LoadGlyphFromLazarusResource('anchor_right');
  TopRefTopSpeedButton.LoadGlyphFromLazarusResource('anchor_top');
  TopRefCenterSpeedButton.LoadGlyphFromLazarusResource('anchor_center_vertical');
  TopRefBottomSpeedButton.LoadGlyphFromLazarusResource('anchor_top_bottom');
  BottomRefTopSpeedButton.LoadGlyphFromLazarusResource('anchor_top_bottom');
  BottomRefCenterSpeedButton.LoadGlyphFromLazarusResource('anchor_center_vertical');
  BottomRefBottomSpeedButton.LoadGlyphFromLazarusResource('anchor_bottom');

  // autosizing
  BottomSiblingLabel.AnchorToNeighbour(akLeft,10,BottomAnchoredCheckBox);
  BottomSiblingComboBox.AnchorToNeighbour(akLeft,5,BottomSiblingLabel);
  BottomSiblingLabel.AnchorVerticalCenterTo(BottomSiblingComboBox);
  BottomAnchoredCheckBox.AnchorVerticalCenterTo(BottomSiblingComboBox);
  TopSiblingLabel.AnchorToNeighbour(akLeft,10,TopAnchoredCheckBox);
  TopSiblingComboBox.AnchorToNeighbour(akLeft,5,TopSiblingLabel);
  TopSiblingLabel.AnchorVerticalCenterTo(TopSiblingComboBox);
  TopAnchoredCheckBox.AnchorVerticalCenterTo(TopSiblingComboBox);

  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
end;

procedure TAnchorDesigner.AnchorDesignerDestroy(Sender: TObject);
begin
  FreeAndNil(Values);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
  FreeAndNil(FSelection);
  FreeAndNil(FSelectedControlsList);
end;

procedure TAnchorDesigner.AnchorDesignerShow(Sender: TObject);
begin
  Refresh(true);
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
  //debugln('TAnchorDesigner.AnchorEnabledCheckBoxChange ',DbgSName(Sender),' ',dbgs(TCheckBox(Sender).Checked));
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
  //debugln('TAnchorDesigner.AnchorEnabledCheckBoxChange CurSide.AmbiguousEnabled=',dbgs(CurSide.AmbiguousEnabled),' CurSide.Enabled=',dbgs(CurSide.Enabled),' NewValue=',dbgs(NewValue));
  if CurSide.AmbiguousEnabled or (CurSide.Enabled<>NewValue) then begin
    //debugln('TAnchorDesigner.AnchorEnabledCheckBoxChange ',DbgSName(Sender),' NewValue=',dbgs(NewValue));
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
        if MessageDlg(lisCCOWarningCaption,
          lisThisWillCreateACircularDependency, mtWarning, [mbIgnore, mbCancel], 0)<>
            mrIgnore
        then begin
          Refresh(false);
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
    GlobalDesignHook.Modified(Self);
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
  //debugln('TAnchorDesigner.BorderSpaceSpinEditChange ',DbgSName(Sender),' ',dbgs(TSpinEdit(Sender).Value));
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
  or ((not Around) and (CurSide.AmbiguousBorderSpace
                        or (CurSide.BorderSpace<>NewValue)))
  then begin
    //debugln('TAnchorDesigner.BorderSpaceSpinEditChange ',DbgSName(Sender),' NewValue=',dbgs(NewValue));
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
    GlobalDesignHook.Modified(Self);
    GlobalDesignHook.RefreshPropertyValues;
  end;
end;

function compareControlTop(Item1, Item2: pointer): Integer;
begin
  if tcontrol(item1).top<tcontrol(item2).top then result:=-1
  else if tcontrol(item1).top>tcontrol(item2).top then result:=1
  else result:=0;
end;

function compareControlLeft(Item1, Item2: pointer): Integer;
begin
  if tcontrol(item1).left<tcontrol(item2).left then result:=-1
  else if tcontrol(item1).left>tcontrol(item2).left then result:=1
  else result:=0;
end;

function compareControlRight(Item1, Item2: pointer): Integer;
begin
  if tcontrol(item1).left+tcontrol(item1).width>tcontrol(item2).left+tcontrol(item2).width then result:=-1
  else if tcontrol(item1).left+tcontrol(item1).width<tcontrol(item2).left+tcontrol(item2).width then result:=1
  else result:=0;
end;

function compareControlBottom(Item1, Item2: pointer): Integer;
begin
  if tcontrol(item1).top+tcontrol(item1).Height>tcontrol(item2).top+tcontrol(item2).Height then result:=-1
  else if tcontrol(item1).top+tcontrol(item1).Height<tcontrol(item2).top+tcontrol(item2).Height then result:=1
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

  function NeighbourPosition(c: tcontrol):Integer;
  begin
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
    result:=tcontrol(SelectedControls[resultId]);
 end;

var
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  CheckPosition: Integer;
begin
  //debugln('TAnchorDesigner.SiblingComboBoxChange ',DbgSName(Sender),' ',TComboBox(Sender).Text);
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
  if CurSide.AmbiguousSibling or (CompareText(CurSide.Sibling,NewValue)<>0) then
  begin
    //debugln('TAnchorDesigner.SiblingComboBoxChange ',DbgSName(Sender),' NewSibling=',DbgSName(NewSibling));
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
        //todo: copy the list if it is needed unsorted somewhere else
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
        if MessageDlg(lisCCOWarningCaption,
          lisThisWillCreateACircularDependency, mtWarning, [mbIgnore, mbCancel], 0)<>
            mrIgnore
        then begin
          Refresh(false);
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
          MessageDlg('Error', lisUnableToSetAnchorSideControl+#13 +E.Message,
            mtError,[mbCancel],0);
        end;
      end;
    end;

    GlobalDesignHook.Modified(Self);
    GlobalDesignHook.RefreshPropertyValues;
    if UseNeighbours then TComboBox(Sender).Caption:=NewValue;
  end;
end;

procedure TAnchorDesigner.ReferenceSideButtonClicked(Sender: TObject);
var
  CurSide: TAnchorDesignerSideValues;
  Kind: TAnchorKind;
  Side: TAnchorSideReference;
  SelectedControls: TList;
  i: Integer;
  CurControl: TControl;
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  CheckPosition: Integer;
begin
  //debugln('TAnchorDesigner.ReferenceSideButtonClicked ',DbgSName(Sender),' ',dbgs(TSpeedButton(Sender).Down));
  if FUpdating or (Values=nil) then exit;
  if Sender=LeftRefCenterSpeedButton then begin
    Kind:=akLeft;
    Side:=asrCenter;
  end
  else if Sender=LeftRefLeftSpeedButton then begin
    Kind:=akLeft;
    Side:=asrLeft;
  end
  else if Sender=LeftRefRightSpeedButton then begin
    Kind:=akLeft;
    Side:=asrRight;
  end
  else if Sender=RightRefCenterSpeedButton then begin
    Kind:=akRight;
    Side:=asrCenter;
  end
  else if Sender=RightRefLeftSpeedButton then begin
    Kind:=akRight;
    Side:=asrLeft;
  end
  else if Sender=RightRefRightSpeedButton then begin
    Kind:=akRight;
    Side:=asrRight;
  end
  else if Sender=TopRefCenterSpeedButton then begin
    Kind:=akTop;
    Side:=asrCenter;
  end
  else if Sender=TopRefTopSpeedButton then begin
    Kind:=akTop;
    Side:=asrTop;
  end
  else if Sender=TopRefBottomSpeedButton then begin
    Kind:=akTop;
    Side:=asrBottom;
  end
  else if Sender=BottomRefCenterSpeedButton then begin
    Kind:=akBottom;
    Side:=asrCenter;
  end
  else if Sender=BottomRefTopSpeedButton then begin
    Kind:=akBottom;
    Side:=asrTop;
  end
  else if Sender=BottomRefBottomSpeedButton then begin
    Kind:=akBottom;
    Side:=asrBottom;
  end else
    exit;
  CurSide:=Values.Sides[Kind];
  if CurSide.AmbiguousSide or (CurSide.Side<>Side) then
  begin
    //debugln('TAnchorDesigner.ReferenceSideButtonClicked ',DbgSName(Sender));
    // user changed a sibling
    SelectedControls:=GetSelectedControls;
    if SelectedControls=nil then exit;
    // check
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      if (Kind in CurControl.Anchors)
      and (not CurControl.AnchorSide[Kind].CheckSidePosition(
        CurControl.AnchorSide[Kind].Control,Side,
        ReferenceControl,ReferenceSide,CheckPosition))
      then begin
        if MessageDlg(lisCCOWarningCaption,
          lisThisWillCreateACircularDependency, mtWarning, [mbIgnore, mbCancel], 0)<>
            mrIgnore
        then begin
          Refresh(false);
          exit;
        end;
        break;
      end;
    end;
    // commit
    for i:=0 to SelectedControls.Count-1 do begin
      CurControl:=TControl(SelectedControls[i]);
      CurControl.AnchorSide[Kind].Side:=Side;
    end;
    GlobalDesignHook.Modified(Self);
    GlobalDesignHook.RefreshPropertyValues;
  end;
end;

procedure TAnchorDesigner.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDEShortCut(Self,Key,Shift,nil);
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
    //debugln('TAnchorDesigner.FillComboBoxWithSiblings.AddSibling ',NewControlStr);
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
        if (CurControl.Parent<>nil) then begin
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
  //debugln('TAnchorDesigner.FillComboBoxWithSiblings ',sl.Text);
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
  //todo: add translations
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

procedure TAnchorDesigner.Refresh(Force: boolean);
var
  SelectedControlCount: Integer;
  CurSide: TAnchorDesignerSideValues;
  Sibling: String;
  CurSelection: TList;
begin
  //debugln('TAnchorDesigner.Refresh A ');
  if not Force then begin
    // check if update is needed
    if not IsVisible then exit;
  end;
  if FUpdating then exit;
  FUpdating:=true;
  try
    FreeAndNil(Values);
    CurSelection:=GetSelectedControls;
    CollectValues(CurSelection,Values,SelectedControlCount);
    //debugln('TAnchorDesigner.Refresh B ',dbgs(SelectedControlCount));

    if (Values=nil) then begin
      Caption:=lisAnchorEditorNoControlSelected;
      BorderSpaceGroupBox.Enabled:=false;
      TopGroupBox.Enabled:=false;
      LeftGroupBox.Enabled:=false;
      RightGroupBox.Enabled:=false;
      BottomGroupBox.Enabled:=false;
    end else begin
      Caption:=lisAnchorsOfSelectedControls;

      // all
      BorderSpaceGroupBox.Enabled:=true;
      if Values.AmbiguousBorderspaceAround then
        AroundBorderSpaceSpinEdit.Value:=-1
      else
        AroundBorderSpaceSpinEdit.Value:=Values.BorderspaceAround;

      // Top
      TopGroupBox.Enabled:=true;
      CurSide:=Values.Sides[akTop];
      TopAnchoredCheckBox.AllowGrayed:=CurSide.AmbiguousEnabled;
      if CurSide.AmbiguousEnabled then
        TopAnchoredCheckBox.State:=cbGrayed
      else
        TopAnchoredCheckBox.Checked:=CurSide.Enabled;
      if CurSide.AmbiguousBorderSpace then
        TopBorderSpaceSpinEdit.Value:=-1
      else
        TopBorderSpaceSpinEdit.Value:=CurSide.BorderSpace;
      TopBorderSpaceSpinEdit.ValueEmpty:=CurSide.AmbiguousBorderSpace;
      Sibling:=CurSide.Sibling;
      TopSiblingComboBox.Text:=Sibling;
      //debugln('TAnchorDesigner.Refresh A TopSiblingComboBox.Text=',TopSiblingComboBox.Text,' Sibling=',Sibling);
      FillComboBoxWithSiblings(TopSiblingComboBox);
      //debugln('TAnchorDesigner.Refresh B TopSiblingComboBox.Text=',TopSiblingComboBox.Text,' Sibling=',Sibling);
      TopRefBottomSpeedButton.Down:=(CurSide.Side=asrBottom);
      TopRefCenterSpeedButton.Down:=(CurSide.Side=asrCenter);
      TopRefTopSpeedButton.Down:=(CurSide.Side=asrTop);

      // Bottom
      BottomGroupBox.Enabled:=true;
      CurSide:=Values.Sides[akBottom];
      BottomAnchoredCheckBox.AllowGrayed:=CurSide.AmbiguousEnabled;
      if CurSide.AmbiguousEnabled then
        BottomAnchoredCheckBox.State:=cbGrayed
      else
        BottomAnchoredCheckBox.Checked:=CurSide.Enabled;
      if CurSide.AmbiguousBorderSpace then
        BottomBorderSpaceSpinEdit.Value:=-1
      else
        BottomBorderSpaceSpinEdit.Value:=CurSide.BorderSpace;
      BottomBorderSpaceSpinEdit.ValueEmpty:=CurSide.AmbiguousBorderSpace;
      Sibling:=CurSide.Sibling;
      BottomSiblingComboBox.Text:=Sibling;
      FillComboBoxWithSiblings(BottomSiblingComboBox);
      BottomRefBottomSpeedButton.Down:=(CurSide.Side=asrBottom);
      BottomRefCenterSpeedButton.Down:=(CurSide.Side=asrCenter);
      BottomRefTopSpeedButton.Down:=(CurSide.Side=asrTop);

      // Left
      LeftGroupBox.Enabled:=true;
      CurSide:=Values.Sides[akLeft];
      LeftAnchoredCheckBox.AllowGrayed:=CurSide.AmbiguousEnabled;
      if CurSide.AmbiguousEnabled then
        LeftAnchoredCheckBox.State:=cbGrayed
      else
        LeftAnchoredCheckBox.Checked:=CurSide.Enabled;
      if CurSide.AmbiguousBorderSpace then
        LeftBorderSpaceSpinEdit.Value:=-1
      else
        LeftBorderSpaceSpinEdit.Value:=CurSide.BorderSpace;
      LeftBorderSpaceSpinEdit.ValueEmpty:=CurSide.AmbiguousBorderSpace;
      Sibling:=CurSide.Sibling;
      LeftSiblingComboBox.Text:=Sibling;
      FillComboBoxWithSiblings(LeftSiblingComboBox);
      LeftRefRightSpeedButton.Down:=(CurSide.Side=asrBottom);
      LeftRefCenterSpeedButton.Down:=(CurSide.Side=asrCenter);
      LeftRefLeftSpeedButton.Down:=(CurSide.Side=asrTop);

      // Right
      RightGroupBox.Enabled:=true;
      CurSide:=Values.Sides[akRight];
      RightAnchoredCheckBox.AllowGrayed:=CurSide.AmbiguousEnabled;
      if CurSide.AmbiguousEnabled then
        RightAnchoredCheckBox.State:=cbGrayed
      else
        RightAnchoredCheckBox.Checked:=CurSide.Enabled;
      if CurSide.AmbiguousBorderSpace then
        RightBorderSpaceSpinEdit.Value:=-1
      else
        RightBorderSpaceSpinEdit.Value:=CurSide.BorderSpace;
      RightBorderSpaceSpinEdit.ValueEmpty:=CurSide.AmbiguousBorderSpace;
      Sibling:=CurSide.Sibling;
      RightSiblingComboBox.Text:=Sibling;
      FillComboBoxWithSiblings(RightSiblingComboBox);
      RightRefRightSpeedButton.Down:=(CurSide.Side=asrBottom);
      RightRefCenterSpeedButton.Down:=(CurSide.Side=asrCenter);
      RightRefLeftSpeedButton.Down:=(CurSide.Side=asrTop);
    end;
  finally
    FUpdating:=false;
  end;
end;

procedure TAnchorDesigner.OnRefreshPropertyValues;
begin
  Refresh(false);
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
  // collect values of selected controls
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

class function TAnchorDesigner.ControlToStr(AControl: TControl): string;
begin
  if AControl=nil then
    Result:=''
  else
    Result:=AControl.Name+':'+AControl.ClassName;
end;

procedure TAnchorDesigner.CollectValues(
  const ASelection: TList; var TheValues: TAnchorDesignerValues;
  var SelectedControlCount: integer);
var
  i: Integer;
  AControl: TControl;
  CurObject: TObject;
begin
  TheValues:=nil;
  SelectedControlCount:=0;
  //debugln('TAnchorDesigner.CollectValues A ');
  if ASelection=nil then exit;
  // collect values of selected controls
  for i:=0 to ASelection.Count-1 do begin
    CurObject:=TObject(ASelection[i]);
    //debugln('TAnchorDesigner.CollectValues B ',dbgs(i),' ',DbgSName(CurObject));
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
  Refresh(false);
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

constructor TAnchorDesignerSideValues.Create(TheKind: TAnchorKind);
begin
  FAnchorKind:=TheKind;
end;

procedure TAnchorDesignerSideValues.SetValues(AControl: TControl);
var
  CurSide: TAnchorSide;
begin
  FAmbiguousBorderSpace:=false;
  FBorderSpace:=AControl.BorderSpacing.GetSpace(FAnchorKind);
  FAmbiguousEnabled:=false;
  FEnabled:=(FAnchorKind in AControl.Anchors);
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FAmbiguousSide:=false;
  FSide:=CurSide.Side;
  FAmbiguousSibling:=false;
  FSibling:=TAnchorDesigner.ControlToStr(CurSide.Control);
end;

procedure TAnchorDesignerSideValues.MergeValues(AControl: TControl);
var
  CurSide: TAnchorSide;
begin
  FAmbiguousBorderSpace:=FAmbiguousBorderSpace
            or (FBorderSpace<>AControl.BorderSpacing.GetSpace(FAnchorKind));
  FAmbiguousEnabled:=FAmbiguousEnabled
                     or (FEnabled<>(FAnchorKind in AControl.Anchors));
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FAmbiguousSide:=FAmbiguousSide or (CurSide.Side<>FSide);
  FAmbiguousSibling:=FAmbiguousSibling
                   or (TAnchorDesigner.ControlToStr(CurSide.Control)<>FSibling);
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

