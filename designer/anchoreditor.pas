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
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, Spin,
  IDECommands, PropEdits,
  LazarusIDEStrConsts, EnvironmentOpts, IDEOptionDefs;

type

  { TAnchorDesignerSideValues }

  TAnchorDesignerSideValues = class
  private
    FAmbigiousBorderSpace: boolean;
    FAmbigiousEnabled: boolean;
    FAmbigiousSide: boolean;
    FAmbigiousSibling: boolean;
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
    property AmbigiousEnabled: boolean read FAmbigiousEnabled write FAmbigiousEnabled;
    property Sibling: string read FSibling write FSibling;
    property AmbigiousSibling: boolean read FAmbigiousSibling write FAmbigiousSibling;
    property Side: TAnchorSideReference read FSide write FSide;
    property AmbigiousSide: boolean read FAmbigiousSide write FAmbigiousSide;
    property BorderSpace: integer read FBorderSpace write FBorderSpace;
    property AmbigiousBorderSpace: boolean read FAmbigiousBorderSpace write FAmbigiousBorderSpace;
  end;
  

  { TAnchorDesignerValues }

  TAnchorDesignerValues = class
  private
    FAmbigiousBorderspaceAround: boolean;
    FBorderspaceAround: integer;
    FSides: array[TAnchorKind] of TAnchorDesignerSideValues;
    function GetSides(Kind: TAnchorKind): TAnchorDesignerSideValues;
    procedure SetAmbigiousBorderspaceAround(const AValue: boolean);
    procedure SetBorderspaceAround(const AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetValues(AControl: TControl);
    procedure MergeValues(AControl: TControl);
  public
    property Sides[Kind: TAnchorKind]: TAnchorDesignerSideValues read GetSides;
    property BorderspaceAround: integer read FBorderspaceAround write SetBorderspaceAround;
    property AmbigiousBorderspaceAround: boolean read FAmbigiousBorderspaceAround write SetAmbigiousBorderspaceAround;
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
    procedure AnchorDesignerClose(Sender: TObject; var CloseAction: TCloseAction
      );
    procedure AnchorDesignerCreate(Sender: TObject);
    procedure AnchorDesignerDestroy(Sender: TObject);
    procedure AnchorDesignerShow(Sender: TObject);
  private
    FSelection: TPersistentSelectionList;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    SrcTypeImageList: TImageList;
    procedure Refresh(Force: boolean);
    procedure OnRefreshPropertyValues;
    class function ControlToStr(AControl: TControl): string;
    procedure CollectValues(const ASelection: TPersistentSelectionList;
                            var Values: TAnchorDesignerValues;
                            var SelectedControlCount: integer);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    property Selection: TPersistentSelectionList read FSelection;
  end;

var
  AnchorDesigner: TAnchorDesigner;

implementation

{ TAnchorDesigner }

procedure TAnchorDesigner.AnchorDesignerCreate(Sender: TObject);

  function AddResImg(ImgList: TImageList; const ResName: string): integer;
  var Bitmap: TBitmap;
  begin
    Bitmap:=TBitmap.Create;
    if LazarusResources.Find(ResName)=nil then begin
      DebugLn('TAnchorDesigner.AnchorDesignerCreate: ',
        ' WARNING: icon not found: "',ResName,'"');
      Result:=-1;
      exit;
    end;
    Bitmap.LoadFromLazarusResource(ResName);
    Result:=ImgList.Add(Bitmap,nil);
  end;

var
  AnchorEnabledHint: String;
begin
  Name:=NonModalIDEWindowNames[nmiwAnchorEditor];
  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  KeyPreview:=true;
  FSelection:=TPersistentSelectionList.Create;

  AnchorEnabledHint:='Enabled = Include %s in Anchors';

  AroundBorderSpaceSpinEdit.Hint:='Borderspace around the control. The other four borderspaces are added to this value.';
  BorderSpaceGroupBox.Caption:='BorderSpace';
  BottomAnchoredCheckBox.Caption:='Enabled';
  BottomAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akBottom']);
  BottomBorderSpaceSpinEdit.Hint:='Bottom borderspace. This value is added to base borderspace and used for the space below the control.';
  BottomGroupBox.Caption:='Bottom anchoring';
  BottomRefBottomSpeedButton.Hint:='Anchor to bottom side of sibling, keep border space';
  BottomRefCenterSpeedButton.Hint:='Center control vertically relative to the given sibling';
  BottomRefTopSpeedButton.Hint:='Anchor to top side of sibling, keep border space';
  BottomSiblingComboBox.Hint:='This is the sibling control to which the bottom side is anchored. Leave empty for parent.';
  BottomSiblingLabel.Caption:='Sibling';
  LeftAnchoredCheckBox.Caption:='Enabled';
  LeftAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akLeft']);
  LeftBorderSpaceSpinEdit.Hint:='Left borderspace. This value is added to base borderspace and used for the space left to the control.';
  LeftGroupBox.Caption:='Left anchoring';
  LeftRefCenterSpeedButton.Hint:='Center control horizontally relative to the given sibling';
  LeftRefLeftSpeedButton.Hint:='Anchor to left side of sibling, keep border space';
  LeftRefRightSpeedButton.Hint:='Anchor to right side of sibling, keep border space';
  LeftSiblingComboBox.Hint:='This is the sibling control to which the left side is anchored. Leave empty for parent.';
  LeftSiblingLabel.Caption:='Sibling';
  RightAnchoredCheckBox.Caption:='Enabled';
  RightAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akRight']);
  RightBorderSpaceSpinEdit.Hint:='Right borderspace. This value is added to base borderspace and used for the space right to the control.';
  RightGroupBox.Caption:='Right anchoring';
  RightRefCenterSpeedButton.Hint:='Center control horizontally relative to the given sibling';
  RightRefLeftSpeedButton.Hint:='Anchor to left side of sibling, keep border space';
  RightRefRightSpeedButton.Hint:='Anchor to right side of sibling, keep border space';
  RightSiblingComboBox.Hint:='This is the sibling control to which the right side is anchored. Leave empty for parent.';
  RightSiblingLabel.Caption:='Sibling';
  TopAnchoredCheckBox.Caption:='Enabled';
  TopAnchoredCheckBox.Hint:=Format(AnchorEnabledHint,['akTop']);
  TopBorderSpaceSpinEdit.Hint:='Top borderspace. This value is added to base borderspace and used for the space above the control.';
  TopGroupBox.Caption:='Top anchoring';
  TopRefBottomSpeedButton.Hint:='Anchor to bottom side of sibling, keep border space';
  TopRefCenterSpeedButton.Hint:='Center control vertically relative to the given sibling';
  TopRefTopSpeedButton.Hint:='Anchor to top side of sibling, keep border space';
  TopSiblingComboBox.Hint:='This is the sibling control to which the top side is anchored. Leave empty for parent.';
  TopSiblingLabel.Caption:='Sibling';

  SrcTypeImageList:=TImageList.Create(Self);
  with SrcTypeImageList do
  begin
    Name:='SrcTypeImageList';
    Width:=25;
    Height:=25;
    //AddResImg(SrcTypeImageList,'anchorside_bottomtop');
    //AddResImg(SrcTypeImageList,'anchorside_bottombottom');
    //AddResImg(SrcTypeImageList,'anchorside_centervert');
    //AddResImg(SrcTypeImageList,'anchorside_toptop');
    //AddResImg(SrcTypeImageList,'anchorside_topbottom');
    //AddResImg(SrcTypeImageList,'anchorside_leftleft');
    //AddResImg(SrcTypeImageList,'anchorside_leftright');
    //AddResImg(SrcTypeImageList,'anchorside_centerhorz');
    //AddResImg(SrcTypeImageList,'anchorside_rightleft');
    //AddResImg(SrcTypeImageList,'anchorside_rightright');
  end;
  
  // autosizing
  BottomSiblingLabel.AnchorSide[akLeft].Side:=asrRight;
  BottomSiblingLabel.BorderSpacing.Left:=10;
  BottomSiblingLabel.AnchorSide[akLeft].Control:=BottomAnchoredCheckBox;
  BottomSiblingComboBox.AnchorSide[akLeft].Side:=asrRight;
  BottomSiblingComboBox.AnchorSide[akLeft].Control:=BottomSiblingLabel;
  TopSiblingLabel.AnchorSide[akLeft].Side:=asrRight;
  TopSiblingLabel.BorderSpacing.Left:=BottomSiblingLabel.BorderSpacing.Left;
  TopSiblingLabel.AnchorSide[akLeft].Control:=TopAnchoredCheckBox;
  TopSiblingComboBox.AnchorSide[akLeft].Side:=asrRight;
  TopSiblingComboBox.AnchorSide[akLeft].Control:=TopSiblingLabel;
  
  
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
end;

procedure TAnchorDesigner.AnchorDesignerDestroy(Sender: TObject);
begin
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
  FreeThenNil(FSelection);
end;

procedure TAnchorDesigner.AnchorDesignerShow(Sender: TObject);
begin
  Refresh(true);
end;

procedure TAnchorDesigner.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  ExecuteIDECommand(Self,Key,Shift,caMenuOnly);
end;

procedure TAnchorDesigner.Refresh(Force: boolean);
var
  SelectedControlCount: Integer;
  Values: TAnchorDesignerValues;
  CurSide: TAnchorDesignerSideValues;
  Sibling: String;
begin
  debugln('TAnchorDesigner.Refresh A ');
  if not Force then begin
    // check if uddate is needed
    if not Visible then exit;
  end;
  Values:=nil;
  try
    GlobalDesignHook.GetSelection(FSelection);
    CollectValues(FSelection,Values,SelectedControlCount);
    debugln('TAnchorDesigner.Refresh B ',dbgs(SelectedControlCount));

    LeftGroupBox.Enabled:=false;
    RightGroupBox.Enabled:=false;
    BottomGroupBox.Enabled:=false;

    if (Values=nil) then begin
      Caption:='Anchor Editor - no control selected';
      TopGroupBox.Enabled:=false;
    end else begin
      Caption:='Anchors of selected controls';
      TopGroupBox.Enabled:=true;
      CurSide:=Values.Sides[akTop];
      if CurSide.AmbigiousEnabled then
        TopAnchoredCheckBox.State:=cbGrayed
      else
        TopAnchoredCheckBox.Checked:=CurSide.Enabled;
      if CurSide.AmbigiousBorderSpace then
        TopBorderSpaceSpinEdit.Value:=-1
      else
        TopBorderSpaceSpinEdit.Value:=CurSide.BorderSpace;
      Sibling:=CurSide.Sibling;
      TopSiblingComboBox.Text:=Sibling;
      TopRefBottomSpeedButton.Enabled:=Sibling<>'';
      TopRefBottomSpeedButton.Down:=(CurSide.Side=asrBottom);
      TopRefCenterSpeedButton.Enabled:=Sibling<>'';
      TopRefCenterSpeedButton.Down:=(CurSide.Side=asrCenter);
      TopRefTopSpeedButton.Enabled:=Sibling<>'';
      TopRefTopSpeedButton.Down:=(CurSide.Side=asrTop);
    end;
  finally
    Values.Free;
  end;
end;

procedure TAnchorDesigner.OnRefreshPropertyValues;
begin
  Refresh(false);
end;

function TAnchorDesigner.ControlToStr(AControl: TControl): string;
begin
  if AControl=nil then
    Result:=''
  else
    Result:=AControl.Name+':'+AControl.ClassName;
end;

procedure TAnchorDesigner.CollectValues(
  const ASelection: TPersistentSelectionList; var Values: TAnchorDesignerValues;
  var SelectedControlCount: integer);
var
  CurPersistent: TPersistent;
  i: Integer;
  AControl: TControl;
begin
  Values:=nil;
  SelectedControlCount:=0;
  debugln('TAnchorDesigner.CollectValues A ');
  if ASelection=nil then exit;
  // collect values of selected controls
  for i:=0 to ASelection.Count-1 do begin
    CurPersistent:=ASelection[i];
    debugln('TAnchorDesigner.CollectValues B ',dbgs(i),' ',DbgSName(CurPersistent));
    if CurPersistent is TControl then begin
      AControl:=TControl(CurPersistent);
      if SelectedControlCount=0 then begin
        Values:=TAnchorDesignerValues.Create;
        Values.SetValues(AControl);
      end else begin
        Values.MergeValues(AControl);
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

procedure TAnchorDesigner.AnchorDesignerClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  EnvironmentOptions.IDEWindowLayoutList.ItemByForm(Self).GetCurrentPosition;
end;

{ TAnchorDesignerValues }

function TAnchorDesignerValues.GetSides(Kind: TAnchorKind
  ): TAnchorDesignerSideValues;
begin
  Result:=FSides[Kind];
end;

procedure TAnchorDesignerValues.SetAmbigiousBorderspaceAround(
  const AValue: boolean);
begin
  if FAmbigiousBorderspaceAround=AValue then exit;
  FAmbigiousBorderspaceAround:=AValue;
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
  AmbigiousBorderspaceAround:=false;
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    FSides[a].SetValues(AControl);
end;

procedure TAnchorDesignerValues.MergeValues(AControl: TControl);
var
  a: TAnchorKind;
begin
  FAmbigiousBorderspaceAround:=FAmbigiousBorderspaceAround
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
  FAmbigiousBorderSpace:=false;
  FBorderSpace:=AControl.BorderSpacing.GetSpace(FAnchorKind);
  FAmbigiousEnabled:=false;
  FEnabled:=(FAnchorKind in AControl.Anchors);
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FAmbigiousSide:=false;
  FSide:=CurSide.Side;
  FAmbigiousSibling:=false;
  FSibling:=TAnchorDesigner.ControlToStr(CurSide.Control);
end;

procedure TAnchorDesignerSideValues.MergeValues(AControl: TControl);
var
  CurSide: TAnchorSide;
begin
  FAmbigiousBorderSpace:=FAmbigiousBorderSpace
                or (FBorderSpace<>AControl.BorderSpacing.GetSpace(FAnchorKind));
  FAmbigiousEnabled:=FAmbigiousEnabled
                     or (FEnabled<>(FAnchorKind in AControl.Anchors));
  CurSide:=AControl.AnchorSide[FAnchorKind];
  FAmbigiousSide:=FAmbigiousSide or (CurSide.Side<>FSide);
  FAmbigiousSibling:=FAmbigiousSibling
                   or (TAnchorDesigner.ControlToStr(CurSide.Control)<>FSibling);
end;

initialization
  {$I anchoreditor.lrs}

end.

