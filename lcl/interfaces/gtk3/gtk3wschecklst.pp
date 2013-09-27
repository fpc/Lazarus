{
 *****************************************************************************
 *                             Gtk3WSCheckLst.pp                             *
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Gtk3WSCheckLst;

{$i gtk3defines.inc}
{$mode objfpc}{$H+}

interface

uses
  LazGObject2, LazGtk3, LazGLib2,
  gtk3widgets,
  ////////////////////////////////////////////////////
  // I M P O R T A N T
  ////////////////////////////////////////////////////
  // To get as little as posible circles,
  // uncomment only when needed for registration
  ////////////////////////////////////////////////////
  CheckLst, StdCtrls, Controls, LCLType, SysUtils, Classes, LMessages, LCLProc,
  ////////////////////////////////////////////////////
   WSCheckLst, WSLCLClasses, WSProc;

type

  { TGtk3WSCheckListBox }

  { TGtk3WSCustomCheckListBox }

  TGtk3WSCustomCheckListBox = class(TWSCustomCheckListBox)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle; override;
    class function GetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): Boolean; override;
    class function GetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer): TCheckBoxState; override;
    class procedure SetItemEnabled(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AEnabled: Boolean); override;
    class procedure SetState(const ACheckListBox: TCustomCheckListBox;
      const AIndex: integer; const AState: TCheckBoxState); override;
  end;


implementation

uses
  Gtk3WSControls, gtk3procs;

{ TGtk3WSCheckListBox }

class function TGtk3WSCustomCheckListBox.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  // DebugLn('>TGtk3WSCustomCheckListBox.CreateHandle');
  Result := TLCLIntfHandle(TGtk3CheckListBox.Create(AWinControl, AParams));
  // DebugLn('<TGtk3WSCustomCheckListBox.CreateHandle: Result=',dbgHex(Result));
end;

class function TGtk3WSCustomCheckListBox.GetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer): Boolean;
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
  Disabled: gboolean;
begin
  Result := False;
  if not WSCheckHandleAllocated(ACheckListBox, 'GetItemEnabled') then
    Exit;
  // DebugLn('TGtk3WSCustomCheckListBox.GetItemEnabled AIndex=',dbgs(AIndex));
  TreeView := PGtkTreeView(TGtk3CheckListBox(ACheckListBox.Handle).GetContainerWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    gtk_tree_model_get(ListStore, @Iter, [gtk3CLBDisabled, @Disabled, -1]);
    Result := not Disabled;
  end;
end;

class function TGtk3WSCustomCheckListBox.GetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer
  ): TCheckBoxState;
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
  b: byte;
begin
  Result := cbUnchecked;
  if not WSCheckHandleAllocated(ACheckListBox, 'GetState') then
    Exit;

  // DebugLn('TGtk3WSCustomCheckListBox.GetState AIndex=',dbgs(AIndex));
  TreeView := PGtkTreeView(TGtk3CheckListBox(ACheckListBox.Handle).GetContainerWidget);

  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    gtk_tree_model_get(ListStore, @Iter, [gtk3CLBState, @b, -1]);
    Result := TCheckBoxState(b);
  end;
end;

class procedure TGtk3WSCustomCheckListBox.SetItemEnabled(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AEnabled: Boolean);
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
  Disabled: gboolean;
begin
  if not WSCheckHandleAllocated(ACheckListBox, 'SetItemEnabled') then
    Exit;
  // DebugLn('TGtk3WSCustomCheckListBox.SetItemEnabled AIndex=',dbgs(AIndex),' AEnabled=',dbgs(AEnabled));
  TreeView := PGtkTreeView(TGtk3CheckListBox(ACheckListBox.Handle).GetContainerWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
  begin
    Disabled := not AEnabled;
    gtk_list_store_set(PGtkListStore(ListStore), @Iter, [gtk3CLBDisabled, Disabled, -1]);
  end;
end;

class procedure TGtk3WSCustomCheckListBox.SetState(
  const ACheckListBox: TCustomCheckListBox; const AIndex: integer;
  const AState: TCheckBoxState);
var
  Iter : TGtkTreeIter;
  TreeView: PGtkTreeView;
  ListStore: PGtkTreeModel;
begin
  if not WSCheckHandleAllocated(ACheckListBox, 'SetState') then
    Exit;
  // DebugLn('TGtk3WSCustomCheckListBox.SetState AIndex=',dbgs(AIndex),' AEnabled=',dbgs(Ord(AState)));

  TreeView := PGtkTreeView(TGtk3CheckListBox(ACheckListBox.Handle).GetContainerWidget);
  ListStore := gtk_tree_view_get_model(TreeView);
  if gtk_tree_model_iter_nth_child(ListStore, @Iter, nil, AIndex) then
    gtk_list_store_set(PGtkListStore(ListStore), @Iter, [gtk3CLBState, Byte(AState), -1]);

end;

end.
