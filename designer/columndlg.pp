unit ColumnDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, ComCtrls,
  StdCtrls, Buttons, ExtCtrls;

type
  TColumnDlg = class(TForm)
    Listbox1: TLISTBOX;
    Label1: TLABEL;
    Edit1: TEDIT;
    Label2: TLABEL;
    Edit2: TEDIT;
    Button1: TBUTTON;
    Button2: TBUTTON;
    Radiogroup1: TRADIOGROUP;
    Button3: TBUTTON;
    Button4: TBUTTON;
    btnOK : TBitBtn;
    btnCancel : TBitBtn;
  private
    { private declarations }
    FItems : TList;
    FSelectedIndex : Integer;
    function GetCount: Integer;
    function GetItem(Index : Integer): TViewColumn;
    Procedure DisplayColumn(Value : Integer);
  protected
    procedure Button1OnClick(sender : TObject);
    procedure Button2OnClick(sender : TObject);
    procedure Button3OnClick(sender : TObject);
    procedure Button4OnClick(sender : TObject);
    procedure RadioGroup1OnClick(sender : TObject);
    procedure Listbox1OnClick(sender : TObject);
    Procedure Edit1OnChange(Sender : TObject);
    Procedure Edit2OnChange(Sender : TObject);
    Procedure FormOnShow(Sender : TObject);
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    procedure Clear;
    Function  Add(S : String) : Integer;
    property Count : Integer read GetCount;
    property Item[Index : Integer]: TViewColumn read GetItem; default;
//    property Items : TList read FItems write FItems;
  end;


implementation

{ TColumnDlg }

constructor TColumnDlg.Create(AOwner : TComponent);
Begin
  inherited;
//  if LazarusResources.Find(Classname)=nil then
  begin
     Caption := 'Column Editor';
     Width := 400;
     Height := 300;
     OnShow := @FormOnShow;
     Position := poScreenCenter;
     Listbox1 := TListBox.Create(self);
     with Listbox1 do
       Begin
         Parent := Self;
         left := 1;
         Width := 170;
         Top := 1;
         Height := 210;
         Visible := True;
         OnClick := @Listbox1OnClick;
       end;
     Label1 := TLabel.Create(self);
     with Label1 do
       Begin
          Parent := self;
          Caption := 'Caption';
          Left := self.width div 2;
          Top := 15;
          Visible := True;
       end;

     Edit1 := TEdit.Create(self);
     with Edit1 do
       Begin
         Parent := Self;
         Text := '';
         Left := self.Width div 2;
         Height := 25;
         Top := Label1.Top+Label1.Height+5;
          Visible := True;
          OnChange := @Edit1OnChange;
       end;
         
     Label2 := TLabel.Create(self);
     with Label2 do
       Begin
          Parent := self;
          Caption := 'Width';
          Left := self.width div 2;
          Top := Edit1.Top+Edit1.Height+5;
          Visible := True;
       end;

     Edit2 := TEdit.Create(self);
     with Edit2 do
       Begin
         Parent := Self;
         Text := '';
         Left := self.Width div 2;
         Height := 25;
         Top := Label2.Top+Label2.Height+5;
         Visible := True;
         OnChange := @Edit2OnChange;
       end;
       
     RadioGroup1 := TRadioGroup.Create(self);
     with RadioGroup1 do
       Begin
         Parent := Self;
         Caption := 'Alignment';
         Left := self.Width div 2;
         Top := Edit2.Top+Edit2.Height+5;
         Visible := True;
         Columns := 3;
         Height := 50;
         Width := 200;
         Items.Add('Left');
         Items.Add('Center');
         Items.Add('Right');
         ItemIndex := 0;
         OnClick := @RadioGroup1OnClick;
       end;
       
     Button1 := TButton.Create(self);
     with Button1 do
       Begin
          Parent := self;
          Caption := 'Add';
          Left := self.width div 2;
          Top := RadioGroup1.Top+RadioGroup1.Height+5;
          Visible := True;
          OnClick := @Button1OnClick;
       end;

     Button2 := TButton.Create(self);
     with Button2 do
       Begin
          Parent := self;
          Caption := 'Delete';
          Left := Button1.Left+Button1.Width+5;
          Top := Button1.Top;
          Visible := True;
          OnClick := @Button2OnClick;
       end;
       
     Button3 := TButton.Create(self);
     with Button3 do
       Begin
          Parent := self;
          Caption := 'Move up';
          Left := 5;
          Top := ListBox1.Top+Listbox1.Height+5;
          Visible := True;
          OnClick := @Button3OnClick;
       end;

     Button4 := TButton.Create(self);
     with Button4 do
       Begin
          Parent := self;
          Caption := 'Move down';
          Left := Button3.Left+Button3.Width+5;
          Top := Button3.Top;
          Visible := True;
          OnClick := @Button4OnClick;
       end;
       
     btnOK := TBitbtn.Create(self);
     with btnOK do
       Begin
          Parent := self;
          Caption := 'OK';
          Left := self.Width div 2+5;
          Top := Button3.Top;
          Visible := True;
          kind := bkOK;
       end;

     btnCancel := TBitbtn.Create(self);
     with btnCancel do
       Begin
          Parent := self;
          Caption := 'Cancel';
          Left := btnOK.left + btnOK.Width + 5;
          Top :=btnOK.top;
          Visible := True;
          Kind := bkCancel;
       end;

  end;
  FItems := TList.Create;
  FSelectedIndex:= -1;

end;


procedure TColumnDlg.Button1OnClick(sender : TObject);
var
  ViewColumn : TViewColumn;
Begin
  //add
  ViewColumn := TViewColumn.Create;
  ViewColumn.Caption := 'Caption';
  FSelectedIndex := FItems.Add(ViewColumn);
  Listbox1.Items.Add(ViewColumn.Caption);
  Listbox1.Selected[FSelectedIndex] := True;
  DisplayColumn(FSelectedIndex);
end;

procedure TColumnDlg.Listbox1OnClick(sender : TObject);
var
  I : Integer;
begin
  Edit1.ReadOnly := True;
  FSelectedIndex := -1;
  if Listbox1.SelCount = 0 then Exit;
  Edit1.ReadOnly := False;
  I := 0;
  While not Listbox1.Selected[i] do
    inc(i);
  DisplayColumn(I);
  
end;

Procedure TColumnDlg.Edit1OnChange(Sender : TObject);
Var
  ViewColumn : TViewColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ViewColumn := TViewColumn(FItems.Items[FSelectedIndex]);
  ViewColumn.Caption := Edit1.Caption;
  Listbox1.Items[FSelectedIndex] := Edit1.Caption;
  Listbox1.Selected[FSelectedIndex] := True;
end;

Procedure TColumnDlg.Edit2OnChange(Sender : TObject);
Var
  ViewColumn : TViewColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ViewColumn := TViewColumn(FItems.Items[FSelectedIndex]);
  if Edit2.Caption = '' then
    ViewColumn.Width := 0
    else
    try
      ViewColumn.Width := StrtoInt(Edit2.Caption);
    except
        raise Exception.Create('Invalid numeric Value');
        Edit2.Caption := '0';
    end;
end;

procedure TColumnDlg.Button2OnClick(sender : TObject);
var
  Index : Integer;
begin
  //delete
  if FSelectedIndex = -1 then Exit;
  Index := FSelectedIndex;
  FSelectedIndex := -1;
  FItems.Delete(Index);
  Listbox1.Items.Delete(Index);
  if Index > 0 then
  Listbox1.Selected[Index-1] := True;
  DisplayColumn(Index-1);

end;

procedure TColumnDlg.Button3OnClick(sender : TObject);
Var
  ViewColumn : TViewColumn;
  Index : Integer;
begin
  //move up
  if FSelectedIndex <= 0 then Exit;
  Index := FSelectedIndex;
  FSelectedIndex := -1;
  ViewColumn := TViewColumn(FItems.Items[Index]);
  
  FItems.Insert(Index-1,ViewColumn);
  FItems.Delete(Index+1);
  Listbox1.Items.Insert(Index-1,ViewColumn.Caption);
  Listbox1.Items.Delete(Index+1);
  Listbox1.Selected[Index-1] := True;
  DisplayColumn(Index-1);

end;

procedure TColumnDlg.Button4OnClick(sender : TObject);
Var
  ViewColumn : TViewColumn;
  Index : Integer;
begin
  //move down
  if FSelectedIndex = -1 then Exit;
  if (FSelectedIndex >= Listbox1.Items.Count-1) then Exit;

  Index := FSelectedIndex;
  FSelectedIndex := -1;

  ViewColumn := TViewColumn(FItems.Items[Index]);

  FItems.Insert(Index+2,ViewColumn);
  FItems.Delete(Index);
  Listbox1.Items.Insert(Index+2,ViewColumn.Caption);
  Listbox1.Items.Delete(Index);
  Listbox1.Selected[Index+1] := True;
  DisplayColumn(Index+1);
end;

Procedure TColumnDlg.DisplayColumn(Value : Integer);
Var
  ViewColumn : TViewColumn;
begin
  FSelectedIndex := -1;
  if Value = -1 then exit;

  ViewColumn := TViewColumn(FItems.Items[Value]);
  Edit1.Caption := ViewColumn.Caption;
  Edit2.Caption := Inttostr(ViewColumn.Width);

  case ViewColumn.Alignment of
    caLEft :  RadioGroup1.ItemIndex := 0;
    caCenter:RadioGroup1.ItemIndex := 1;
    caRight : RadioGroup1.ItemIndex := 2;
  end;  //case

  FSelectedIndex := Value;
  
end;

procedure TColumnDlg.RadioGroup1OnClick(sender : TObject);
Var
  ViewColumn : TViewColumn;
begin
  if FSelectedIndex = -1 then Exit;
  ViewColumn := TViewColumn(FItems.Items[FSelectedIndex]);
  case  RadioGroup1.ItemIndex of
     0 : ViewColumn.Alignment := caLeft;
     1 : ViewColumn.Alignment := caCenter;
     2 : ViewColumn.Alignment := caRight;
  end;
end;

Procedure TColumnDlg.FormOnShow(Sender : TObject);
var
  I : Integer;
  ViewColumn : TViewColumn;
begin
  //clear the listbox and display the items if any...
  Listbox1.Items.Clear;
  for I := 0 to FItems.Count-1 do
    Begin
     ViewColumn := TViewColumn(FItems.Items[I]);
     Listbox1.Items.Add(ViewColumn.Caption);
    end;
  if Listbox1.Items.Count > 0 then
  begin
    Listbox1.Selected[0] := True;
    DisplayColumn(0);
  end;
  
end;

procedure TColumnDlg.Clear;
begin
  FItems.Clear;
end;

function TColumnDlg.GetItem(Index : Integer): TViewColumn;
begin
    Result := nil;
    if Index > FItems.Count-1 then exit;
    Result := TViewColumn(FItems.Items[Index]);
end;

function TColumnDlg.GetCount: Integer;
begin
  Result := FItems.Count;
end;

Function TColumnDlg.Add(S : String) : Integer;
var
  ViewColumn : TViewColumn;
begin
    ViewColumn := TViewColumn.Create;
    ViewColumn.Caption := S;
    Result := FItems.Add(ViewColumn);
end;

initialization
  { $I columndlg.lrs}

end.

