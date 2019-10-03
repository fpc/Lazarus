{ $Id$ }
{               ----------------------------------------------
                 breakpointsdlg.pp  -  Overview of breakpoints
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the Breakpoint dialog.


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

unit BreakPointsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Dialogs,
  IDEWindowIntf, DebuggerStrConst, Menus, ComCtrls, Debugger, DebuggerDlg, ActnList, MainBase,
  IDEImagesIntf, DbgIntfDebuggerBase, DbgIntfMiscClasses, SourceEditor, MainIntf;

type
  TBreakPointsDlgState = (
    bpdsItemsNeedUpdate
    );
  TBreakPointsDlgStates = set of TBreakPointsDlgState;

  { TBreakPointsDlg }

  TBreakPointsDlg = class(TDebuggerDlg)
    actAddSourceBP: TAction;
    actAddAddressBP: TAction;
    actAddWatchPoint: TAction;
    actGroupSetNone: TAction;
    actGroupSetNew: TAction;
    actShow: TAction;
    actProperties: TAction;
    actToggleCurrentEnable: TAction;
    actDeleteAllInSrc: TAction;
    actEnableSelected: TAction;
    actDisableSelected: TAction;
    actDeleteSelected: TAction;
    actEnableAll: TAction;
    actDisableAll: TAction;
    actDeleteAll: TAction;
    actEnableAllInSrc: TAction;
    actDisableAllInSrc: TAction;
    ActionList1: TActionList;
    lvBreakPoints: TListView;
    popGroupSep: TMenuItem;
    popGroupSetNew: TMenuItem;
    popGroupSetNone: TMenuItem;
    popGroup: TMenuItem;
    popAddWatchPoint: TMenuItem;
    popAddAddressBP: TMenuItem;
    N0: TMenuItem;
    popShow: TMenuItem;
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    popAddSourceBP: TMenuItem;
    N1: TMenuItem; //--------------
    popProperties: TMenuItem;
    popEnabled: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem; //--------------
    popDisableAll: TMenuItem;
    popEnableAll: TMenuItem;
    popDeleteAll: TMenuItem;
    N3: TMenuItem; //--------------
    popDisableAllSameSource: TMenuItem;
    popEnableAllSameSource: TMenuItem;
    popDeleteAllSameSource: TMenuItem;
    ToolBar1: TToolBar;
    ToolButtonAdd: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolSep2: TToolButton;
    ToolButtonEnable: TToolButton;
    ToolButtonDisable: TToolButton;
    ToolButtonTrash: TToolButton;
    ToolSep1: TToolButton;
    ToolButtonEnableAll: TToolButton;
    ToolButtonDisableAll: TToolButton;
    ToolButtonTrashAll: TToolButton;
    procedure actAddAddressBPExecute(Sender: TObject);
    procedure actAddSourceBPExecute(Sender: TObject);
    procedure actAddWatchPointExecute(Sender: TObject);
    procedure actDisableSelectedExecute(Sender: TObject);
    procedure actEnableSelectedExecute(Sender: TObject);
    procedure actGroupSetNoneExecute(Sender: TObject);
    procedure actGroupSetNewExecute(Sender: TObject);
    procedure actShowExecute(Sender: TObject);
    procedure BreakpointsDlgCREATE(Sender: TObject);
    procedure lvBreakPointsClick(Sender: TObject);
    procedure lvBreakPointsDBLCLICK(Sender: TObject);
    procedure lvBreakPointsSelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: Boolean);
    procedure mnuPopupPopup(Sender: TObject);
    procedure popDeleteAllSameSourceCLICK(Sender: TObject);
    procedure popDisableAllSameSourceCLICK(Sender: TObject);
    procedure popEnableAllSameSourceCLICK(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
  private
    FBaseDirectory: string;
    FStates: TBreakPointsDlgStates;
    FLockActionUpdate: Integer;
    procedure BreakPointAdd(const {%H-}ASender: TIDEBreakPoints;
                            const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointUpdate(const ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure BreakPointRemove(const {%H-}ASender: TIDEBreakPoints;
                               const ABreakpoint: TIDEBreakPoint);
    procedure SetBaseDirectory(const AValue: string);
    procedure popSetGroupItemClick(Sender: TObject);
    procedure SetGroup(const NewGroup: TIDEBreakPointGroup);

    procedure UpdateItem(const AnItem: TListItem;
                         const ABreakpoint: TIDEBreakPoint);
    procedure UpdateAll;
    
    procedure DeleteSelectedBreakpoints;
    procedure JumpToCurrentBreakPoint;
    procedure ShowProperties;
  protected
    procedure DoBreakPointsChanged; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DisableAllActions;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory;
    property BreakPoints;
  end;
  
function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;


implementation

{$R *.lfm}
uses
  LazarusIDEStrConsts, BaseDebugManager;

var
  BreakPointDlgWindowCreator: TIDEWindowCreator;

const
  COL_BREAK_STATE     = 1;
  COL_BREAK_FILE      = 2;
  COL_BREAK_LINE      = 3;
  COL_BREAK_CONDITION = 4;
  COL_BREAK_ACTION    = 5;
  COL_BREAK_PASS      = 6;
  COL_BREAK_GROUP     = 7;
  COL_WIDTHS: Array[0..6] of integer = ( 50, 150, 100,  75, 150, 100, 80);

function BreakPointDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TBreakPointsDlg;
  if Result then
    Result := TBreakPointsDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure BreakPointDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TBreakPointsDlg then
    TBreakPointsDlg(AForm).ColSizeSetter(AColId, ASize);
end;

function GetBreakPointStateDescription(ABreakpoint: TBaseBreakpoint): string;
var
  DEBUG_STATE: array[Boolean, TValidState] of ShortString;
begin
  DEBUG_STATE[false, vsUnknown]:=lisOff;
  DEBUG_STATE[false, vsValid]:=lisBPSDisabled;
  DEBUG_STATE[false, vsInvalid]:=lisInvalidOff;
  DEBUG_STATE[false, vsPending]:=lisInvalidOff;
  DEBUG_STATE[true, vsUnknown]:=lisOn;
  DEBUG_STATE[true, vsValid]:=lisBPSEnabled;
  DEBUG_STATE[true, vsInvalid]:=lisInvalidOn;
  DEBUG_STATE[true, vsPending]:=lisPendingOn;
  Result:=DEBUG_STATE[ABreakpoint.Enabled,ABreakpoint.Valid];
end;

function GetBreakPointActionsDescription(ABreakpoint: TBaseBreakpoint): string;
var
  DEBUG_ACTION: array[TIDEBreakPointAction] of ShortString;
  CurBreakPoint: TIDEBreakPoint;
  Action: TIDEBreakPointAction;
begin
  Result := '';

  DEBUG_ACTION[bpaStop]:=lisBreak;
  DEBUG_ACTION[bpaEnableGroup]:=lisEnableGroups;
  DEBUG_ACTION[bpaDisableGroup]:=lisDisableGroups;
  DEBUG_ACTION[bpaLogMessage]:=lisLogMessage;
  DEBUG_ACTION[bpaEValExpression]:=lisLogEvalExpression;
  DEBUG_ACTION[bpaLogCallStack]:=lisLogCallStack;
  DEBUG_ACTION[bpaTakeSnapshot]:=lisTakeSnapshot;

  if ABreakpoint is TIDEBreakPoint then begin
    CurBreakPoint:=TIDEBreakPoint(ABreakpoint);
    for Action := Low(TIDEBreakPointAction) to High(TIDEBreakPointAction) do
      if Action in CurBreakpoint.Actions
      then begin
        if Result <> '' then Result := Result + ', ';
        Result := Result + DEBUG_ACTION[Action]
      end;
  end;
end;

procedure TBreakPointsDlg.BreakPointAdd(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  Item: TListItem;
  n: Integer;
begin
  Item := lvBreakPoints.Items.FindData(ABreakpoint);
  if Item = nil
  then begin
    Item := lvBreakPoints.Items.Add;
    Item.Data := ABreakPoint;
    for n := 0 to 5 do
      Item.SubItems.Add('');
  end;

  UpdateItem(Item, ABreakPoint);
end;

procedure TBreakPointsDlg.BreakPointUpdate(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  Item: TListItem;
begin
  if ABreakpoint = nil then Exit;

  Item := lvBreakPoints.Items.FindData(ABreakpoint);
  if Item = nil
  then BreakPointAdd(ASender, ABreakPoint)
  else begin
    if UpdateCount>0 then begin
      Include(FStates,bpdsItemsNeedUpdate);
      exit;
    end;
    UpdateItem(Item, ABreakPoint);
  end;
end;

procedure TBreakPointsDlg.BreakPointRemove(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
begin
  lvBreakPoints.Items.FindData(ABreakpoint).Free;
end;

procedure TBreakPointsDlg.SetBaseDirectory(const AValue: string);
begin
  if FBaseDirectory=AValue then exit;
  FBaseDirectory:=AValue;
  UpdateAll;
end;

procedure TBreakPointsDlg.SetGroup(const NewGroup: TIDEBreakPointGroup);
var
  OldGroup: TIDEBreakPointGroup;
  OldGroups: TList;
  i: Integer;
  PrevChoice: TModalResult;
begin
  PrevChoice := mrNone;
  OldGroups := TList.Create;
  try
    for i := 0 to lvBreakPoints.Items.Count - 1 do
      if lvBreakPoints.Items[i].Selected then
      begin
        OldGroup := TIDEBreakPoint(lvBreakPoints.Items[i].Data).Group;
        TIDEBreakPoint(lvBreakPoints.Items[i].Data).Group := NewGroup;
        if (OldGroup <> nil) and (OldGroup.Count = 0) and (OldGroups.IndexOf(OldGroup) < 0) then
          OldGroups.Add(OldGroup);
      end;
  finally
    while OldGroups.Count > 0 do begin
      OldGroup := TIDEBreakPointGroup(OldGroups[0]);
      OldGroups.Delete(0);
      if not (PrevChoice in [mrYesToAll, mrNoToAll]) then
      begin
        if OldGroups.Count > 0 then
          PrevChoice := MessageDlg(Caption, Format(lisGroupEmptyDelete + lisGroupEmptyDeleteMore,
            [OldGroup.Name, LineEnding, OldGroups.Count]),
            mtConfirmation, mbYesNo + [mbYesToAll, mbNoToAll], 0)
        else
          PrevChoice := MessageDlg(Caption, Format(lisGroupEmptyDelete,
            [OldGroup.Name]), mtConfirmation, mbYesNo, 0);
      end;
      if PrevChoice in [mrYes, mrYesToAll] then
        OldGroup.Free;
    end;
    OldGroups.Free;
  end;
end;

constructor TBreakPointsDlg.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  Name:='BreakPointsDlg';
  BreakpointsNotification.OnAdd    := @BreakPointAdd;
  BreakpointsNotification.OnUpdate := @BreakPointUpdate;
  BreakpointsNotification.OnRemove := @BreakPointRemove;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;
  mnuPopup.Images := IDEImages.Images_16;
  lvBreakPoints.SmallImages := IDEImages.Images_16;

  ToolButtonAdd.ImageIndex := IDEImages.LoadImage('laz_add');

  actEnableSelected.Caption := lisDbgItemEnable;
  actEnableSelected.Hint    := lisDbgItemEnableHint;
  actEnableSelected.ImageIndex := IDEImages.LoadImage('debugger_enable');

  actDisableSelected.Caption := lisDbgItemDisable;
  actDisableSelected.Hint    := lisDbgItemDisableHint;
  actDisableSelected.ImageIndex := IDEImages.LoadImage('debugger_disable');

  actDeleteSelected.Caption := lisBtnDelete;
  actDeleteSelected.Hint    := lisDbgItemDeleteHint;
  actDeleteSelected.ImageIndex := IDEImages.LoadImage('laz_delete');

  actEnableAll.Caption := lisEnableAll;
  actEnableAll.Hint    := lisDbgAllItemEnableHint;
  actEnableAll.ImageIndex := IDEImages.LoadImage('debugger_enable_all');

  actDisableAll.Caption := liswlDIsableAll;
  actDisableAll.Hint    := lisDbgAllItemDisableHint;
  actDisableAll.ImageIndex := IDEImages.LoadImage('debugger_disable_all');

  actDeleteAll.Caption := lisDeleteAll;
  actDeleteAll.Hint    := lisDbgAllItemDeleteHint;
  actDeleteAll.ImageIndex := IDEImages.LoadImage('menu_clean');

  actProperties.Caption:= liswlProperties;
  actProperties.Hint := lisDbgBreakpointPropertiesHint;
  actProperties.ImageIndex := IDEImages.LoadImage('menu_environment_options');

  actToggleCurrentEnable.Caption:= lisBtnEnabled;

  actEnableAllInSrc.Caption:= lisEnableAllInSameSource;
  actDisableAllInSrc.Caption:= lisDisableAllInSameSource;
  actDeleteAllInSrc.Caption:= lisDeleteAllInSameSource;
  for i := low(COL_WIDTHS) to high(COL_WIDTHS) do
    lvBreakPoints.Column[i].Width := COL_WIDTHS[i];

  FLockActionUpdate := 0;
end;

procedure TBreakPointsDlg.BreakpointsDlgCREATE(Sender: TObject);
begin
  Caption:= lisMenuViewBreakPoints;
  lvBreakPoints.Align:=alClient;
  lvBreakPoints.Columns[0].Caption:= lisOIPState;
  lvBreakPoints.Columns[1].Caption:= lisFilenameAddress;
  lvBreakPoints.Columns[2].Caption:= lisLineLength;
  lvBreakPoints.Columns[3].Caption:= lisCondition;
  lvBreakPoints.Columns[4].Caption:= lisLazBuildABOAction;
  lvBreakPoints.Columns[5].Caption:= lisPassCount;
  lvBreakPoints.Columns[6].Caption:= lisGroup;
  actShow.Caption := lisViewSource;
  popAdd.Caption:= lisAdd;
  actAddSourceBP.Caption := lisSourceBreakpoint;
  actAddAddressBP.Caption := lisAddressBreakpoint;
  actAddWatchPoint.Caption := lisWatchPoint;
  popGroup.Caption := lisGroup;
  actGroupSetNew.Caption := lisGroupSetNew;
  actGroupSetNone.Caption := lisGroupSetNone;
end;

procedure TBreakPointsDlg.actEnableSelectedExecute(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvBreakPoints.Items.Count -1 do
    begin
      Item := lvBreakPoints.Items[n];
      if Item.Selected then
        TIDEBreakPoint(Item.Data).Enabled := True;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.actGroupSetNewExecute(Sender: TObject);
var
  GroupName: String;
  NewGroup: TIDEBreakPointGroup;
begin
  GroupName := '';
  if not InputQuery(Caption, lisGroupNameInput, GroupName) then Exit;
  if GroupName = '' then
  begin
    if MessageDlg(Caption, lisGroupNameEmptyClearInstead,
      mtConfirmation, mbYesNo, 0) = mrYes then Exit;
    NewGroup := nil;
  end
  else begin
    NewGroup := DebugBoss.BreakPointGroups.GetGroupByName(GroupName);
    if NewGroup = nil then
    begin
      if not TIDEBreakPointGroup.CheckName(GroupName) then
      begin
        MessageDlg(Caption, lisGroupNameInvalid, mtError, [mbOk], 0);
        Exit;
      end;
      NewGroup := TIDEBreakPointGroup(DebugBoss.BreakPointGroups.Add);
      try
        NewGroup.Name := GroupName;
      except
        NewGroup.Free;
        raise;
      end;
    end
    else if MessageDlg(Caption, Format(lisGroupAssignExisting,
        [GroupName]), mtConfirmation, mbYesNo, 0) <> mrYes
      then
        Exit;
  end;

  SetGroup(NewGroup);
end;

procedure TBreakPointsDlg.actGroupSetNoneExecute(Sender: TObject);
begin
  SetGroup(nil);
end;

procedure TBreakPointsDlg.popSetGroupItemClick(Sender: TObject);
var
  Group: TIDEBreakPointGroup;
begin
  Group := DebugBoss.BreakPointGroups.GetGroupByName((Sender as TMenuItem).Caption);
  if Group = nil then
    raise Exception.CreateFmt('Group %s not found', [(Sender as TMenuItem).Caption]);
  SetGroup(Group);
end;

procedure TBreakPointsDlg.actShowExecute(Sender: TObject);
begin
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.actDisableSelectedExecute(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvBreakPoints.Items.Count -1 do
    begin
      Item := lvBreakPoints.Items[n];
      if Item.Selected then
        TIDEBreakPoint(Item.Data).Enabled := False;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.actAddSourceBPExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
  SrcEdit: TSourceEditor;
begin
  SrcEdit := SourceEditorManager.GetActiveSE;
  if SrcEdit <> nil then
    NewBreakpoint := BreakPoints.Add(SrcEdit.FileName, SrcEdit.CurrentCursorYLine, True)
  else
    NewBreakpoint := BreakPoints.Add('', 0, True);
  if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
    NewBreakpoint.EndUpdate;
    UpdateAll;
  end
  else
    ReleaseRefAndNil(NewBreakpoint);
end;

procedure TBreakPointsDlg.actAddWatchPointExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
begin
  NewBreakpoint := BreakPoints.Add('', wpsGlobal, wpkWrite, True);
  if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
    NewBreakpoint.EndUpdate;
    UpdateAll;
  end
  else
    ReleaseRefAndNil(NewBreakpoint);
end;

procedure TBreakPointsDlg.actAddAddressBPExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
begin
  NewBreakpoint := BreakPoints.Add(0, True);
  if DebugBoss.ShowBreakPointProperties(NewBreakpoint) = mrOk then begin
    NewBreakpoint.EndUpdate;
    UpdateAll;
  end
  else
    ReleaseRefAndNil(NewBreakpoint);
end;

procedure TBreakPointsDlg.lvBreakPointsClick(Sender: TObject);
begin
  lvBreakPointsSelectItem(nil, nil, False);
end;

procedure TBreakPointsDlg.lvBreakPointsDBLCLICK(Sender: TObject);
begin
  lvBreakPointsSelectItem(nil, nil, False);
  JumpToCurrentBreakPoint;
end;

procedure TBreakPointsDlg.lvBreakPointsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  ItemSelected: Boolean;
  SelCanEnable, SelCanDisable: Boolean;
  AllCanEnable, AllCanDisable: Boolean;
  CurBreakPoint: TIDEBreakPoint;
  i: Integer;
begin
  if FLockActionUpdate > 0 then exit;

  ItemSelected := lvBreakPoints.Selected <> nil;
  if ItemSelected then
    CurBreakPoint := TIDEBreakPoint(lvBreakPoints.Selected.Data)
  else
    CurBreakPoint := nil;
  SelCanEnable := False;
  SelCanDisable := False;
  AllCanEnable := False;
  allCanDisable := False;
  for i := 0 to lvBreakPoints.Items.Count - 1 do begin
    if lvBreakPoints.Items[i].Data = nil then
      continue;
    if lvBreakPoints.Items[i].Selected then begin
      SelCanEnable := SelCanEnable or not TIDEBreakPoint(lvBreakPoints.Items[i].Data).Enabled;
      SelCanDisable := SelCanDisable or TIDEBreakPoint(lvBreakPoints.Items[i].Data).Enabled;
    end;
    AllCanEnable := AllCanEnable or not TIDEBreakPoint(lvBreakPoints.Items[i].Data).Enabled;
    AllCanDisable := AllCanDisable or TIDEBreakPoint(lvBreakPoints.Items[i].Data).Enabled;
  end;

  actToggleCurrentEnable.Enabled := ItemSelected;
  actToggleCurrentEnable.Checked := (CurBreakPoint <> nil) and CurBreakPoint.Enabled;

  actEnableSelected.Enabled := SelCanEnable;
  actDisableSelected.Enabled := SelCanDisable;
  actDeleteSelected.Enabled := ItemSelected;

  actEnableAll.Enabled := AllCanEnable;
  actDisableAll.Enabled := AllCanDisable;
  actDeleteAll.Enabled := lvBreakPoints.Items.Count > 0;

  actEnableAllInSrc.Enabled := ItemSelected;
  actDisableAllInSrc.Enabled := ItemSelected;
  actDeleteAllInSrc.Enabled := ItemSelected;

  actProperties.Enabled := ItemSelected;
  actShow.Enabled := ItemSelected;

  popGroup.Enabled := ItemSelected;
  actGroupSetNew.Enabled := ItemSelected;
  actGroupSetNone.Enabled := ItemSelected;
end;

procedure TBreakPointsDlg.mnuPopupPopup(Sender: TObject);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  for i := popGroup.Count - 1 downto popGroup.IndexOf(popGroupSep) +1 do
    popGroup.Items[i].Free;
  for i := 0 to DebugBoss.BreakPointGroups.Count - 1 do
  begin
    MenuItem := TMenuItem.Create(popGroup);
    MenuItem.Caption := DebugBoss.BreakPointGroups[i].Name;
    MenuItem.OnClick := @popSetGroupItemClick;
    popGroup.Add(MenuItem);
  end;
  popGroupSep.Visible := DebugBoss.BreakPointGroups.Count <> 0;
end;

procedure TBreakPointsDlg.popDeleteAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  try
    DisableAllActions;
    CurItem:=lvBreakPoints.Selected;
    if (CurItem=nil) then exit;
    Filename:=TIDEBreakpoint(CurItem.Data).Source;
    if MessageDlg(lisDeleteAllBreakpoints,
      Format(lisDeleteAllBreakpoints2, [Filename]),
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
    then exit;
    for n := lvBreakPoints.Items.Count - 1 downto 0 do
    begin
      Item := lvBreakPoints.Items[n];
      CurBreakPoint:=TIDEBreakPoint(Item.Data);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then ReleaseRefAndNil(CurBreakPoint);
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popDisableAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  try
    DisableAllActions;
    CurItem:=lvBreakPoints.Selected;
    if (CurItem=nil) then exit;
    Filename:=TIDEBreakpoint(CurItem.Data).Source;
    for n := 0 to lvBreakPoints.Items.Count - 1 do
    begin
      Item := lvBreakPoints.Items[n];
      CurBreakPoint:=TIDEBreakPoint(Item.Data);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then CurBreakPoint.Enabled := False;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popEnableAllSameSourceCLICK(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Filename: String;
begin
  try
    DisableAllActions;
    CurItem:=lvBreakPoints.Selected;
    if (CurItem=nil) then exit;
    Filename:=TIDEBreakpoint(CurItem.Data).Source;
    for n := 0 to lvBreakPoints.Items.Count - 1 do
    begin
      Item := lvBreakPoints.Items[n];
      CurBreakPoint:=TIDEBreakPoint(Item.Data);
      if CompareFilenames(CurBreakPoint.Source,Filename)=0
      then CurBreakPoint.Enabled := True;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin                                    
  try
    DisableAllActions;
    if MessageDlg(lisDeleteAllBreakpoints,
      lisDeleteAllBreakpoints,
      mtConfirmation,[mbYes,mbCancel],0)<>mrYes
    then exit;
    lvBreakPoints.BeginUpdate;
    try
      for n := lvBreakPoints.Items.Count - 1 downto 0 do
        TIDEBreakPoint(lvBreakPoints.Items[n].Data).ReleaseReference;
    finally
      lvBreakPoints.EndUpdate;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popDeleteClick(Sender: TObject);
begin
  try
    DisableAllActions;
    DeleteSelectedBreakpoints;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popDisableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvBreakPoints.Items.Count - 1 do
    begin
      Item := lvBreakPoints.Items[n];
      if Item.Data <> nil
      then TIDEBreakPoint(Item.Data).Enabled := False;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popEnableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvBreakPoints.Items.Count - 1 do
    begin
      Item := lvBreakPoints.Items[n];
      if Item.Data <> nil
      then TIDEBreakPoint(Item.Data).Enabled := True;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popEnabledClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
  Enable: Boolean;
begin
  try
    DisableAllActions;
    Item:=lvBreakPoints.Selected;
    if (Item=nil) then exit;

    Enable := not TIDEBreakPoint(Item.Data).Enabled;

    if lvBreakPoints.SelCount > 1
    then begin
      for n := 0 to lvBreakPoints.Items.Count -1 do
      begin
        Item := lvBreakPoints.Items[n];
        if Item.Selected then
          TIDEBreakPoint(Item.Data).Enabled := Enable;
      end;
    end
    else begin
      TIDEBreakPoint(Item.Data).Enabled:= Enable;
    end;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.popPropertiesClick(Sender: TObject);
begin
  try
    DisableAllActions;
    ShowProperties;
  finally
    lvBreakPointsSelectItem(nil, nil, False);
  end;
end;

procedure TBreakPointsDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if bpdsItemsNeedUpdate in FStates then UpdateAll;
  lvBreakPointsSelectItem(nil, nil, False);
end;

procedure TBreakPointsDlg.DisableAllActions;
var
  i: Integer;
begin
  for i := 0 to ActionList1.ActionCount - 1 do
    (ActionList1.Actions[i] as TAction).Enabled := False;
  actAddSourceBP.Enabled := True;
  actAddAddressBP.Enabled := True;
  actAddWatchPoint.Enabled := True;
end;

function TBreakPointsDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if (AColId - 1 >= 0) and (AColId - 1 < lvBreakPoints.ColumnCount) then begin
    ASize := lvBreakPoints.Column[AColId - 1].Width;
    Result := ASize <> COL_WIDTHS[AColId - 1];
  end
  else
    Result := False;
end;

procedure TBreakPointsDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_BREAK_STATE:     lvBreakPoints.Column[0].Width := ASize;
    COL_BREAK_FILE:      lvBreakPoints.Column[1].Width := ASize;
    COL_BREAK_LINE:      lvBreakPoints.Column[2].Width := ASize;
    COL_BREAK_CONDITION: lvBreakPoints.Column[3].Width := ASize;
    COL_BREAK_ACTION:    lvBreakPoints.Column[4].Width := ASize;
    COL_BREAK_PASS:      lvBreakPoints.Column[5].Width := ASize;
    COL_BREAK_GROUP:     lvBreakPoints.Column[6].Width := ASize;
  end;
end;

procedure TBreakPointsDlg.UpdateItem(const AnItem: TListItem;
  const ABreakpoint: TIDEBreakPoint);
var
  s, Filename: String;
begin
  // Filename/Address
  // Line/Length
  // Condition
  // Action
  // Pass Count
  // Group

  // state
  AnItem.Caption := GetBreakPointStateDescription(ABreakpoint);
  AnItem.ImageIndex := GetBreakPointImageIndex(ABreakpoint);
  
  // filename/address
  case ABreakpoint.Kind of
    bpkSource:
      begin
        Filename:=ABreakpoint.Source;
        if BaseDirectory<>'' then
          Filename:=CreateRelativePath(Filename,BaseDirectory);
        AnItem.SubItems[0] := Filename;
        // line
        if ABreakpoint.Line > 0
        then AnItem.SubItems[1] := IntToStr(ABreakpoint.Line)
        else AnItem.SubItems[1] := '';
      end;
    bpkAddress:
      begin
        // todo: how to define digits count? 8 or 16 depends on gdb pointer size for platform
        AnItem.SubItems[0] := '$' + IntToHex(ABreakpoint.Address, 8);
      end;
    bpkData:
      begin
        AnItem.SubItems[0] := ABreakpoint.WatchData;
        case ABreakpoint.WatchScope of
          wpsGlobal: s:= lisWatchScopeGlobal;
          wpsLocal:  s:= lisWatchScopeLocal;
          else s := '';
        end;
        s := s +' / ';
        case ABreakpoint.WatchKind of
          wpkRead:      s := s + lisWatchKindRead;
          wpkReadWrite: s := s + lisWatchKindReadWrite;
          wpkWrite:     s := s + lisWatchKindWrite;
        end;
        AnItem.SubItems[1] := s;
      end;
  end;

  // expression
  AnItem.SubItems[2] := ABreakpoint.Expression;
  
  // actions
  AnItem.SubItems[3]  := GetBreakPointActionsDescription(ABreakpoint);
  
  // hitcount
  AnItem.SubItems[4] := IntToStr(ABreakpoint.HitCount);
  
  // group
  if ABreakpoint.Group = nil
  then AnItem.SubItems[5] := ''
  else AnItem.SubItems[5] := ABreakpoint.Group.Name;

  lvBreakPointsSelectItem(nil, nil, False);
end;

procedure TBreakPointsDlg.UpdateAll;
var
  i: Integer;
  CurItem: TListItem;
begin
  if UpdateCount>0 then begin
    Include(FStates,bpdsItemsNeedUpdate);
    exit;
  end;
  Exclude(FStates,bpdsItemsNeedUpdate);
  inc(FLockActionUpdate);
  for i:=0 to lvBreakPoints.Items.Count-1 do begin
    CurItem:=lvBreakPoints.Items[i];
    UpdateItem(CurItem,TIDEBreakPoint(CurItem.Data));
  end;
  dec(FLockActionUpdate);
  lvBreakPointsSelectItem(nil, nil, False);
end;

procedure TBreakPointsDlg.DeleteSelectedBreakpoints;
var
  Item: TListItem;
  CurBreakPoint: TIDEBreakPoint;
  Msg: String;
  List: TList;
  n, Idx: Integer;
begin
  Idx := lvBreakPoints.ItemIndex;
  Item:=lvBreakPoints.Selected;
  if Item = nil then exit;

  if lvBreakPoints.SelCount = 1 then
  begin
    CurBreakPoint:=TIDEBreakPoint(Item.Data);
    case CurBreakPoint.Kind of
      bpkSource: Msg := Format(lisDeleteBreakpointAtLine,
                           [LineEnding, CurBreakPoint.Source, CurBreakPoint.Line]);
      bpkAddress: Msg := Format(lisDeleteBreakpointForAddress, ['$' + IntToHex(CurBreakPoint.Address, 8)]);
      bpkData: Msg := Format(lisDeleteBreakpointForWatch, [CurBreakPoint.WatchData]);
    end;
  end
  else
    Msg := lisDeleteAllSelectedBreakpoints;
  if MessageDlg(Msg, mtConfirmation, [mbYes,mbCancel],0) <> mrYes then exit;

  if lvBreakPoints.SelCount = 1
  then begin
    TBaseBreakPoint(Item.Data).ReleaseReference;
  end
  else begin
    List := TList.Create;
    for n := 0 to lvBreakPoints.Items.Count - 1 do
    begin
      Item := lvBreakPoints.Items[n];
      if Item.Selected
      then List.Add(Item.Data);
    end;

    lvBreakPoints.BeginUpdate;
    try
      for n := 0 to List.Count - 1 do
        TBaseBreakPoint(List[n]).ReleaseReference;
    finally
      lvBreakPoints.EndUpdate;
    end;
    List.Free;
  end;
  if Idx > lvBreakPoints.Items.Count - 1 then
    Idx := lvBreakPoints.Items.Count - 1;
  if Idx >= 0 then
    lvBreakPoints.ItemIndex := Idx;
end;

procedure TBreakPointsDlg.JumpToCurrentBreakPoint;
var
  CurItem: TListItem;
  CurBreakPoint: TIDEBreakPoint;
begin
  CurItem:=lvBreakPoints.Selected;
  if CurItem=nil then exit;
  CurBreakPoint:=TIDEBreakPoint(CurItem.Data);
  if CurBreakPoint.Kind = bpkSource then
    MainIDE.DoJumpToSourcePosition(CurBreakPoint.Source, 0, CurBreakPoint.Line, 0,
      [jfAddJumpPoint, jfFocusEditor, jfMarkLine, jfSearchVirtualFullPath]);
end;

procedure TBreakPointsDlg.ShowProperties;
var
  Item: TListItem;
  CurBreakPoint: TIDEBreakPoint;
begin
  Item:=lvBreakPoints.Selected;
  if Item = nil then exit;

  CurBreakPoint:=TIDEBreakPoint(Item.Data);

  DebugBoss.ShowBreakPointProperties(CurBreakPoint);
end;

procedure TBreakPointsDlg.DoBreakPointsChanged;
var
  i: Integer;
begin
  lvBreakPoints.Items.Clear;
  if BreakPoints <> nil
  then begin
    for i:=0 to BreakPoints.Count-1 do
      BreakPointUpdate(BreakPoints, BreakPoints.Items[i]);
  end;
end;

procedure TBreakPointsDlg.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
  DisableAllActions;
end;

initialization

  BreakPointDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtBreakpoints]);
  BreakPointDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  BreakPointDlgWindowCreator.OnSetDividerSize := @BreakPointDlgColSizeSetter;
  BreakPointDlgWindowCreator.OnGetDividerSize := @BreakPointDlgColSizeGetter;
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakState',     COL_BREAK_STATE,     @drsColWidthState);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakFile',      COL_BREAK_FILE,      @drsBreakPointColWidthFile);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakLine',      COL_BREAK_LINE,      @drsBreakPointColWidthLine);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakCondition', COL_BREAK_CONDITION, @drsBreakPointColWidthCondition);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakAction',    COL_BREAK_ACTION,    @drsBreakPointColWidthAction);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakPassCnt',   COL_BREAK_PASS,      @drsBreakPointColWidthPassCount);
  BreakPointDlgWindowCreator.DividerTemplate.Add('ColumnBreakGroup',     COL_BREAK_GROUP,     @drsBreakPointColWidthGroup);
  BreakPointDlgWindowCreator.CreateSimpleLayout;

end.

