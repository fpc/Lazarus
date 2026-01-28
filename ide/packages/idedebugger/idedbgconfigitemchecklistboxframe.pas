unit IdeDbgConfigItemCheckListBoxFrame;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils, Forms, Controls, CheckLst, ExtCtrls, StdCtrls, IdeDebuggerStringConstants;

type

  TIdeDbgConfItemSelection = procedure(AnIndex, AnOldIndex: integer; AnObject, AnOldObject: TObject) of object;
  TIdeDbgConfItemChecked = procedure(AnIndex: integer; AnObject: TObject; ANewChecked: boolean) of object;
  TIdeDbgConfItemMove = procedure(AnIndex: integer; AnObject: TObject; ANewIndex: integer) of object;
  TIdeDbgConfItemAdd = procedure() of object;
  TIdeDbgConfItemRemove = procedure(AnIndex: integer; AnObject: TObject) of object;

  IIdeDbgConfigItemCheckListIntf = interface ['{26621F4A-ABA8-47C7-83B5-D98C8DAB0F01}']
    function Count: integer;
    function DcclItem(AnIndex: integer): TObject;
    function DcclItemName(AnIndex: integer): String;
    function DcclItemEnabled(AnIndex: integer): Boolean;
    procedure DcclSetItemEnabled(AnIndex: integer; AValue: Boolean);
    procedure DcclMoveItem(AnIndex, ANewIndex: integer);
  end;

  { TIdeDbgConfigItemCheckListFrame }

  TIdeDbgConfigItemCheckListFrame = class(TFrame)
    btnAdd: TButton;
    btnDown: TButton;
    btnRemove: TButton;
    btnUp: TButton;
    lstItems: TCheckListBox;
    Panel2: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lstItemsClick(Sender: TObject);
    procedure lstItemsItemClick(Sender: TObject; Index: integer);
  private
    FItems: IIdeDbgConfigItemCheckListIntf;
    FOnAdd: TIdeDbgConfItemAdd;
    FOnCheckedChange: TIdeDbgConfItemChecked;
    FOnDelete: TIdeDbgConfItemRemove;
    FOnMoveDown: TIdeDbgConfItemMove;
    FOnMoveUp: TIdeDbgConfItemMove;
    FOnSelectionChange: TIdeDbgConfItemSelection;
    FCurIdx: integer;
    FCurObj: TObject;
    FUpdateLock: Integer;
    FChanging: boolean;
    function GetChecked(AnIndex: integer): boolean;
    function GetCurrentChecked: boolean;
    function GetCurrentIndex: integer;
    function GetCurrentName: String;
    function GetCurrentObject: TObject;
    function GetNames(AnIndex: integer): String;
    function GetObjects(AnIndex: integer): TObject;
    procedure SetCurrentChecked(AValue: boolean);
    procedure SetCurrentIndex(AValue: integer);
    procedure SetCurrentName(AValue: String);
    procedure SetItems(AValue: IIdeDbgConfigItemCheckListIntf);
  protected
    procedure UpdateButtons;

  public
    procedure Setup;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Refresh;
    function Count: integer;
    property Objects[AnIndex: integer]: TObject read GetObjects;
    property Names[AnIndex: integer]: String read GetNames;
    property Checked[AnIndex: integer]: boolean read GetChecked;
    // Current... change *after* the OnSelectionChange event
    property CurrentIndex: integer read GetCurrentIndex write SetCurrentIndex;
    property CurrentObject: TObject read GetCurrentObject;
    property CurrentName: String read GetCurrentName write SetCurrentName;
    property CurrentChecked: boolean read GetCurrentChecked write SetCurrentChecked;

    property Items: IIdeDbgConfigItemCheckListIntf read FItems write SetItems;

    property OnSelectionChange: TIdeDbgConfItemSelection read FOnSelectionChange write FOnSelectionChange;
    property OnCheckedChange: TIdeDbgConfItemChecked read FOnCheckedChange write FOnCheckedChange;
    property OnAdd: TIdeDbgConfItemAdd read FOnAdd write FOnAdd;
    property OnDelete: TIdeDbgConfItemRemove read FOnDelete write FOnDelete;
    property OnMoveUp: TIdeDbgConfItemMove read FOnMoveUp write FOnMoveUp;
    property OnMoveDown: TIdeDbgConfItemMove read FOnMoveDown write FOnMoveDown;
  end;

implementation

{$R *.lfm}

{ TIdeDbgConfigItemCheckListFrame }

procedure TIdeDbgConfigItemCheckListFrame.lstItemsClick(Sender: TObject);
  function GetObj(i: integer): TObject;
  begin
    if i >= 0 then
      Result := lstItems.Items.Objects[i]
    else
      Result := nil;
  end;
begin
  if FChanging or
     ( (FCurIdx = lstItems.ItemIndex) and (FCurObj = GetObj(FCurIdx)) )
  then
    exit;
  if FUpdateLock > 0 then begin
    FCurIdx := -2;
    exit;
  end;

  BeginUpdate;
  if FOnSelectionChange <> nil then
    FOnSelectionChange(lstItems.ItemIndex, FCurIdx, GetObj(lstItems.ItemIndex), CurrentObject);
  FCurIdx := lstItems.ItemIndex;
  FCurObj := GetObj(FCurIdx);

  EndUpdate;
end;

procedure TIdeDbgConfigItemCheckListFrame.btnAddClick(Sender: TObject);
begin
  if FChanging then
    exit;
  BeginUpdate;
  if FOnAdd <> nil then
    FOnAdd();
  EndUpdate;
end;

procedure TIdeDbgConfigItemCheckListFrame.btnDownClick(Sender: TObject);
var
  i: Integer;
  o: TObject;
begin
  BeginUpdate;
  i := CurrentIndex;
  if (FOnMoveDown <> nil) and (i >= 0) and (i < Count - 1)
  then begin
    o := CurrentObject;
    FOnMoveDown(i, CurrentObject, i + 1);
    if o = CurrentObject then // not moved by event
      lstItems.Items.Move(i, i+1);
  end;
  EndUpdate;
end;

procedure TIdeDbgConfigItemCheckListFrame.btnUpClick(Sender: TObject);
var
  i: Integer;
  o: TObject;
begin
  BeginUpdate;
  i := CurrentIndex;
  if (FOnMoveUp <> nil) and (i > 0)
  then begin
    o := CurrentObject;
    FOnMoveUp(i, CurrentObject, i - 1);
    if o = CurrentObject then // not moved by event
      lstItems.Items.Move(i, i-1);
  end;
  EndUpdate;
end;

procedure TIdeDbgConfigItemCheckListFrame.btnRemoveClick(Sender: TObject);
begin
  BeginUpdate;
  if FOnDelete <> nil then
    FOnDelete(CurrentIndex, CurrentObject);
  EndUpdate;
end;

procedure TIdeDbgConfigItemCheckListFrame.lstItemsItemClick(Sender: TObject; Index: integer);
begin
  if FChanging then exit;
  BeginUpdate;
  if FOnCheckedChange <> nil then
    FOnCheckedChange(Index, lstItems.Items.Objects[Index], lstItems.Checked[Index]);
  EndUpdate;
end;

function TIdeDbgConfigItemCheckListFrame.GetChecked(AnIndex: integer): boolean;
begin
  Result := lstItems.Checked[AnIndex];
end;

function TIdeDbgConfigItemCheckListFrame.GetCurrentChecked: boolean;
begin
  if FCurIdx < 0 then exit(False);
  Result := Checked[FCurIdx];
end;

function TIdeDbgConfigItemCheckListFrame.GetCurrentIndex: integer;
begin
  Result := FCurIdx;
end;

function TIdeDbgConfigItemCheckListFrame.GetCurrentName: String;
begin
  if FCurIdx < 0 then exit('');
  Result := Names[FCurIdx];
end;

function TIdeDbgConfigItemCheckListFrame.GetCurrentObject: TObject;
begin
  if FCurIdx < 0 then exit(nil);
  Result := Objects[FCurIdx];
end;

function TIdeDbgConfigItemCheckListFrame.GetNames(AnIndex: integer): String;
begin
  Result := lstItems.Items[AnIndex];
end;

function TIdeDbgConfigItemCheckListFrame.GetObjects(AnIndex: integer): TObject;
begin
  if (AnIndex < 0) or (AnIndex >= Count) then
    exit(nil);
  Result := lstItems.Items.Objects[AnIndex];
end;

procedure TIdeDbgConfigItemCheckListFrame.SetCurrentChecked(AValue: boolean);
begin
  if FCurIdx < 0 then exit;
  lstItems.Checked[FCurIdx] := AValue;
end;

procedure TIdeDbgConfigItemCheckListFrame.SetCurrentIndex(AValue: integer);
begin
  FChanging := True;
  lstItems.ItemIndex := AValue;
  FCurIdx := AValue;
  FChanging := False;
  UpdateButtons;
end;

procedure TIdeDbgConfigItemCheckListFrame.SetCurrentName(AValue: String);
begin
  if FCurIdx < 0 then exit;
  lstItems.Items[FCurIdx] := AValue;
end;

procedure TIdeDbgConfigItemCheckListFrame.SetItems(AValue: IIdeDbgConfigItemCheckListIntf);
begin
  if FItems = AValue then Exit;
  FItems := AValue;
end;

procedure TIdeDbgConfigItemCheckListFrame.UpdateButtons;
begin
  btnRemove.Enabled := (Count > 0) and (CurrentIndex >= 0);
  btnUp.Enabled := (CurrentIndex > 0);
  btnDown.Enabled := (CurrentIndex >= 0) and (CurrentIndex < Count - 1);
end;

procedure TIdeDbgConfigItemCheckListFrame.Setup;
begin
  btnAdd.Caption       := dlgBackConvOptAddNew;
  btnRemove.Caption    := dlgBackConvOptRemove;
  btnUp.Caption        := dlgSortUp;
  btnDown.Caption      := dlgSortDown;
end;

procedure TIdeDbgConfigItemCheckListFrame.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TIdeDbgConfigItemCheckListFrame.EndUpdate;
begin
  if FUpdateLock > 0 then begin
    if FUpdateLock = 1 then
      UpdateButtons;
    dec(FUpdateLock);
  end;
  if FUpdateLock = 0 then begin
    lstItemsClick(nil);
  end;
end;

procedure TIdeDbgConfigItemCheckListFrame.Refresh;
var
  i, n, c: Integer;
begin
  BeginUpdate;
  c := lstItems.ItemIndex;
  lstItems.Clear;
  for i := 0 to FItems.Count - 1 do begin
    n := lstItems.Items.AddObject(FItems.DcclItemName(i), FItems.DcclItem((i)));
    lstItems.Checked[n] := FItems.DcclItemEnabled(i);
  end;
  if c < lstItems.Count then
    lstItems.ItemIndex := c;
  EndUpdate;
end;

function TIdeDbgConfigItemCheckListFrame.Count: integer;
begin
  Result := lstItems.Items.Count;
end;

end.

