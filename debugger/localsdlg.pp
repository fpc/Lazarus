{ $Id$ }
{               ----------------------------------------------  
                 localsdlg.pp  -  Overview of local variables 
                ---------------------------------------------- 
 
 @created(Thu Mar 14st WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Locals debugger dialog.
 
 
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
unit LocalsDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ClipBrd, LCLProc,
  IDEWindowIntf, IDEOptionDefs, DebuggerStrConst,
  ComCtrls, ActnList, Menus, BaseDebugManager, Debugger, DebuggerDlg;

type

  { TLocalsDlg }

  TLocalsDlg = class(TDebuggerDlg)
    actInspect: TAction;
    actEvaluate: TAction;
    actCopyName: TAction;
    actCopyValue: TAction;
    actWath: TAction;
    ActionList1: TActionList;
    lvLocals: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actEvaluateExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actInspectUpdate(Sender: TObject);
    procedure actWathExecute(Sender: TObject);
  private
    FUpdateFlags: set of (ufNeedUpdating);
    procedure LocalsChanged(Sender: TObject);
    function  GetThreadId: Integer;
    function  GetSelectedThreads(Snap: TSnapshot): TThreads;
    function GetStackframe: Integer;
    function  GetSelectedSnapshot: TSnapshot;
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property LocalsMonitor;
    property ThreadsMonitor;
    property CallStackMonitor;
    property SnapshotManager;
  end;


implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;

var
  LocalsDlgWindowCreator: TIDEWindowCreator;

const
  COL_LOCALS_NAME   = 1;
  COL_LOCALS_VALUE  = 2;

function LocalsDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TLocalsDlg;
  if Result then
    Result := TLocalsDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure LocalsDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TLocalsDlg then
    TLocalsDlg(AForm).ColSizeSetter(AColId, ASize);
end;

{ TLocalsDlg }

constructor TLocalsDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LocalsNotification.OnChange     := @LocalsChanged;
  ThreadsNotification.OnCurrent   := @LocalsChanged;
  CallstackNotification.OnCurrent := @LocalsChanged;
  SnapshotNotification.OnCurrent  := @LocalsChanged;

  Caption:= lisLocals;
  lvLocals.Columns[0].Caption:= lisLocalsDlgName;
  lvLocals.Columns[1].Caption:= lisLocalsDlgValue;
  actInspect.Caption := lisInspect;
  actWath.Caption := lisWatch;
  actEvaluate.Caption := lisEvaluateModify;
  actCopyName.Caption := lisLocalsDlgCopyName;
  actCopyValue.Caption := lisLocalsDlgCopyValue;
end;

procedure TLocalsDlg.actInspectUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(lvLocals.Selected);
end;

procedure TLocalsDlg.actWathExecute(Sender: TObject);
var
  S: String;
  Watch: TCurrentWatch;
begin
  S := lvLocals.Selected.Caption;
  if DebugBoss.Watches.CurrentWatches.Find(S) = nil then
  begin
    Watch := DebugBoss.Watches.CurrentWatches.Add(S);
    Watch.Enabled := True;
  end;
  DebugBoss.ViewDebugDialog(ddtWatches);
end;

procedure TLocalsDlg.actInspectExecute(Sender: TObject);
begin
  DebugBoss.Inspect(lvLocals.Selected.Caption);
end;

procedure TLocalsDlg.actEvaluateExecute(Sender: TObject);
begin
  DebugBoss.EvaluateModify(lvLocals.Selected.Caption);
end;

procedure TLocalsDlg.actCopyNameExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := lvLocals.Selected.Caption;
  Clipboard.Close;
end;

procedure TLocalsDlg.actCopyValueExecute(Sender: TObject);
begin
  Clipboard.Open;
  Clipboard.AsText := lvLocals.Selected.SubItems[0];
  Clipboard.Close;
end;

procedure TLocalsDlg.LocalsChanged(Sender: TObject);
var
  n, idx: Integer;                               
  List: TStringList;
  Item: TListItem;
  S: String;
  Locals: TLocals;
  Snap: TSnapshot;
begin
  if (ThreadsMonitor = nil) or (CallStackMonitor = nil) or (LocalsMonitor=nil) then begin
    lvLocals.Items.Clear;
    exit;
  end;

  if IsUpdating then begin
    {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TLocalsDlg.LocalsChanged  in IsUpdating']); {$ENDIF}
    Include(FUpdateFlags, ufNeedUpdating);
    exit;
  end;
  Exclude(FUpdateFlags, ufNeedUpdating);
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataMonitor: TLocalsDlg.LocalsChanged']); {$ENDIF}

  if GetStackframe < 0 then begin // TODO need dedicated validity property
    lvLocals.Items.Clear;
    exit;
  end;

  Snap := GetSelectedSnapshot;
  if (Snap <> nil)
  then begin
    Locals := LocalsMonitor.Snapshots[Snap][GetThreadId, GetStackframe];
    Caption:= lisLocals + ' ('+ Snap.LocationAsText +')';
  end
  else begin
    Locals := LocalsMonitor.CurrentLocalsList[GetThreadId, GetStackframe];
    Caption:= lisLocals;
  end;

  List := TStringList.Create;
  try
    BeginUpdate;
    try
      if Locals = nil
      then begin
        lvLocals.Items.Clear;
        Item := lvLocals.Items.Add;
        Item.Caption := '';
        Item.SubItems.add(lisLocalsNotEvaluated);
        Exit;
      end;

      //Get existing items
      for n := 0 to lvLocals.Items.Count - 1 do
      begin
        Item := lvLocals.Items[n];
        S := Item.Caption;
        S := UpperCase(S);
        List.AddObject(S, Item);
      end;

      // add/update entries
      for n := 0 to Locals.Count - 1 do
      begin
        idx := List.IndexOf(Uppercase(Locals.Names[n]));
        if idx = -1
        then begin
          // New entry
          Item := lvLocals.Items.Add;
          Item.Caption := Locals.Names[n];
          Item.SubItems.Add(Locals.Values[n]);
        end
        else begin
          // Existing entry
          Item := TListItem(List.Objects[idx]);
          Item.SubItems[0] := Locals.Values[n];
          List.Delete(idx);
        end;
      end;

      // remove obsolete entries
      for n := 0 to List.Count - 1 do
        lvLocals.Items.Delete(TListItem(List.Objects[n]).Index);

    finally
      EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

function TLocalsDlg.GetThreadId: Integer;
var
  Threads: TThreads;
begin
  Result := -1;
  if (ThreadsMonitor = nil) then exit;
  Threads := GetSelectedThreads(GetSelectedSnapshot);
  if Threads <> nil
  then Result := Threads.CurrentThreadId
  else Result := 1;
end;

function TLocalsDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if ThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

function TLocalsDlg.GetStackframe: Integer;
var
  Snap: TSnapshot;
  Threads: TThreads;
  tid: LongInt;
  Stack: TCallStack;
begin
  if (CallStackMonitor = nil) or (ThreadsMonitor = nil)
  then begin
    Result := 0;
    exit;
  end;

  Snap := GetSelectedSnapshot;
  Threads := GetSelectedThreads(Snap);
  if Threads <> nil
  then tid := Threads.CurrentThreadId
  else tid := 1;

  if (Snap <> nil)
  then Stack := CallStackMonitor.Snapshots[Snap].EntriesForThreads[tid]
  else Stack := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];

  if Stack <> nil
  then Result := Stack.CurrentIndex
  else Result := 0;
end;

function TLocalsDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.SelectedEntry <> nil)
  then Result := SnapshotManager.SelectedEntry;
end;

procedure TLocalsDlg.DoBeginUpdate;
begin
  lvLocals.BeginUpdate;
end;

procedure TLocalsDlg.DoEndUpdate;
begin
  if ufNeedUpdating in FUpdateFlags then LocalsChanged(nil);
  lvLocals.EndUpdate;
end;

function TLocalsDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := True;
  case AColId of
    COL_LOCALS_NAME:   ASize := lvLocals.Column[0].Width;
    COL_LOCALS_VALUE:  ASize := lvLocals.Column[1].Width;
    else
      Result := False;
  end;
end;

procedure TLocalsDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_LOCALS_NAME:   lvLocals.Column[0].Width := ASize;
    COL_LOCALS_VALUE:  lvLocals.Column[1].Width := ASize;
  end;
end;

initialization

  LocalsDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtLocals]);
  LocalsDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  LocalsDlgWindowCreator.OnSetDividerSize := @LocalsDlgColSizeSetter;
  LocalsDlgWindowCreator.OnGetDividerSize := @LocalsDlgColSizeGetter;
  LocalsDlgWindowCreator.DividerTemplate.Add('LocalsName',  COL_LOCALS_NAME,  drsColWidthName);
  LocalsDlgWindowCreator.DividerTemplate.Add('LocalsValue', COL_LOCALS_VALUE, drsColWidthValue);
  LocalsDlgWindowCreator.CreateSimpleLayout;

end.

