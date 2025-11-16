unit ToolBarTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  customdrawncontrols, customdrawndrawers, ComCtrls, StdCtrls;

type

  { TFormToolBar }

  TFormToolBar = class(TForm)
    cbFlat: TCheckBox;
    cbShowCaptions: TCheckBox;
    cbEnabled: TCheckBox;
    cbActivateOnClick: TCheckBox;
    ImageList1: TImageList;
    Label1: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure cbActivateOnClickChange(Sender: TObject);
    procedure cbFlatChange(Sender: TObject);
    procedure cbShowCaptionsChange(Sender: TObject);
    procedure cbEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure ToolBarItemClick(Sender: TObject; ArrowClicked: Boolean);
  public
    { public declarations }
    lToolBar: TCDToolBar;
  end;

var
  FormToolBar: TFormToolBar;

implementation

uses
  MainForm;

{$R *.lfm}

{ TFormToolBar }

procedure TFormToolBar.FormCreate(Sender: TObject);
const
  W_BTN = 56;
var
  lBmp: TCustomBitmap;
  lItem: TCDToolBarItem;
begin
  lToolBar := TCDToolBar.Create(Self);
  lToolBar.Parent := Self;
  lToolbar.Height := 44;

  lBmp := TBitmap.Create;
  lBmp.LoadFromFile('icons/lupa.bmp');
  lBmp.Transparent := true;
  lItem := lToolBar.AddItem(tikButton);
  lItem.Image := lBmp;
  lItem.Caption := 'Btn 1';
  lItem.Width := W_BTN;
  lItem.OnClick := @ToolBarItemClick;
  lBmp.Free;

  lItem := lToolBar.AddItem(tikSeparator);

  lBmp := TBitmap.Create;
  lBmp.LoadFromFile('icons/usplegal.bmp');
  lBmp.Transparent := true;
  lItem := lToolBar.AddItem(tikCheckButton);
  lItem.Image := lBmp;
  lItem.Caption := 'Btn 2';
  lItem.Width := W_BTN;
  lItem.OnClick := @ToolBarItemClick;
  lBmp.Free;

  lItem := lToolBar.AddItem(tikDivider);

  lBmp := TPortableNetworkGraphic.Create;
  lBmp.LoadFromFile('icons/Folder_08_24.png');
  lItem := lToolBar.AddItem(tikDropDownButton);
  lItem.Image := lBmp;
  lItem.Caption := 'Btn 3';
  lItem.Width := W_BTN;
  lItem.OnClick := @ToolBarItemClick;
  lBmp.Free;

  lBmp := TPortableNetworkGraphic.Create;
  lBmp.LoadFromFile('icons/Save_01_24.png');
  lItem := lToolBar.AddItem(tikButton);
  lItem.Image := lBmp;
  lItem.Caption := 'Btn 4';
  lItem.Width := W_BTN;
  lItem.OnClick := @ToolBarItemClick;
  lBmp.Free;

  lBmp := TPortableNetworkGraphic.Create;
  lBmp.LoadFromFile('icons/Lock_01_32.png');   // This is a larger icon. Testing scale-down...
  lItem := lToolBar.AddItem(tikCheckButton);
  lItem.Image := lBmp;
  lItem.Caption := 'Btn 5';
  lItem.Width := W_BTN;
  lItem.OnClick := @ToolBarItemClick;
  lBmp.Free;
end;

procedure TFormToolBar.FormShow(Sender: TObject);
begin
  lToolBar.DrawStyle := TCDDrawStyle(FormCDControlsTest.comboDrawer.ItemIndex);
end;

procedure TFormToolBar.cbActivateOnClickChange(Sender: TObject);
var
  i: Integer;
  item: TCDToolBarItem;
begin
  for i := 0 to lToolbar.GetItemCount-1 do
  begin
    item := lToolBar.GetItem(i);
    if cbActivateOnClick.Checked then
      item.OnClick := @ToolBarItemClick
    else
      item.OnClick := nil;
  end;
end;

procedure TFormToolBar.cbFlatChange(Sender: TObject);
begin
  lToolbar.Flat := cbFlat.Checked;
  Toolbar1.Flat := cbFlat.Checked;
end;

procedure TFormToolBar.cbShowCaptionsChange(Sender: TObject);
begin
  lToolbar.ShowCaptions := cbShowCaptions.Checked;
  Toolbar1.ShowCaptions := cbShowCaptions.Checked;
  if cbShowCaptions.Checked then
  begin
    lToolbar.Height := 56;
    Toolbar1.ButtonHeight := 38;
  end else
  begin
    lToolbar.Height := 44;
    Toolbar1.ButtonHeight := 22;
  end;
  Toolbar1.AutoSize := true;
end;

procedure TFormToolBar.cbEnabledChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lToolBar.GetItemCount-1 do
    lToolbar.GetItem(i).Enabled := cbEnabled.Checked;
  lToolbar.Invalidate;
end;

procedure TFormToolBar.ToolbarItemClick(Sender: TObject; ArrowClicked: Boolean);
var
  msg: String;
  item: TCDToolBarItem;
begin
  if (Sender is TCDToolBarItem) then
  begin
    item := TCDToolBarItem(Sender);
    msg := item.Caption + ' clicked';
    if ArrowClicked then
      msg := msg + ' on arrow';
    ShowMessage(msg);
  end;
end;

end.

