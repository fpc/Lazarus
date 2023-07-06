unit SynPopupMenu;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, SynEdit, SynEditStrConst;

type
  TSynDefaultPopupMenu = (dpmDisabled, dpmBefore, dpmAfter);

  TSynPopupMenu = class(TPopupMenu)
  private
    FDefaultPopupMenu: TSynDefaultPopupMenu;
    procedure FillDefaultMenu;
    procedure ClearDefaultMenu;
  protected
    procedure ItemOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure PopUp(X, Y: Integer); override;
  published
    property DefaultPopupMenu: TSynDefaultPopupMenu read
      FDefaultPopupMenu write FDefaultPopupMenu default dpmBefore;
  end;

implementation

type
  TMenuEntry = (meNone, meUndo, meRedo, meCut, meCopy, mePaste,
                meDelete, meSelectAll);

{ TSynPopupMenu }

constructor TSynPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultPopupMenu := dpmBefore;
end;

procedure TSynPopupMenu.FillDefaultMenu;
var
  i: Integer;

  procedure AddMenuItem(const ACaption: string; const ATag: TMenuEntry);
  var
    FItem: TMenuItem;
  begin
    FItem := TMenuItem.Create(Self);
    FItem.Caption := ACaption;
    FItem.OnClick := @ItemOnClick;
    FItem.Tag := Integer(ATag);
    if FDefaultPopupMenu = dpmAfter then
      Items.Add(FItem)
    else
      Items.Insert(i, FItem);
    Inc(i);
  end;

var
  FEmpty: Boolean;
begin
  if FDefaultPopupMenu = dpmDisabled then
    Exit;
  i := 0;
  FEmpty := Items.Count = 0;
  if not FEmpty and (FDefaultPopupMenu = dpmAfter) then     // separator
    AddMenuItem('-', meNone);
  AddMenuItem(SYNS_Undo, meUndo);
  AddMenuItem(SYNS_Redo, meRedo);
  AddMenuItem('-', meNone);
  AddMenuItem(SYNS_Cut, meCut);
  AddMenuItem(SYNS_Copy, meCopy);
  AddMenuItem(SYNS_Paste, mePaste);
  AddMenuItem('-', meNone);
  AddMenuItem(SYNS_Delete, meDelete);
  AddMenuItem(SYNS_SelectAll, meSelectAll);
  if not FEmpty and (FDefaultPopupMenu = dpmBefore) then    // separator
    AddMenuItem('-', meNone);
end;

procedure TSynPopupMenu.ClearDefaultMenu;
var
  i: Integer;
begin
  for i := Items.Count - 1 downto 0 do
    if Items[i].OnClick = @ItemOnClick then
      Items.Delete(i);
end;

procedure TSynPopupMenu.ItemOnClick(Sender: TObject);
begin
  with TCustomSynEdit(PopupComponent) do
    case TMenuEntry(TMenuItem(Sender).Tag) of
      meUndo:      Undo;
      meRedo:      Redo;
      meCut:       CutToClipboard;
      meCopy:      CopyToClipboard;
      mePaste:     PasteFromClipboard;
      meDelete:    SelText := '';
      meSelectAll: SelectAll;
    end;
end;

procedure TSynPopupMenu.PopUp(X, Y: Integer);
var
  i: Integer;
begin
  ClearDefaultMenu;
  if PopupComponent is TCustomSynEdit then
  begin
    FillDefaultMenu;
    for i := 0 to Items.Count - 1 do
      with TCustomSynEdit(PopupComponent) do
        if Items[i].OnClick = @ItemOnClick then   // make sure it's ours
          case TMenuEntry(Items[i].Tag) of
            meUndo:      Items[i].Enabled := CanUndo;
            meRedo:      Items[i].Enabled := CanRedo;
            meCut:       Items[i].Enabled := SelAvail and not ReadOnly;
            meCopy:      Items[i].Enabled := SelAvail;
            mePaste:     Items[i].Enabled := CanPaste;
            meDelete:    Items[i].Enabled := SelAvail and not ReadOnly;
            meSelectAll: Items[i].Enabled := (Lines.Count > 1) or (Lines.Text <> '');
          end;
  end;
  inherited;
end;

end.

