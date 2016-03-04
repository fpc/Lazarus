{%mainunit testallform.pp}
{
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

//******************************************************************************
//                              TestTools.inc
//******************************************************************************

type

  { TArrDummy }

  TArrDummy = class
    arr: TArrow;
    procedure OnRgTypeClick(Sender: TObject);
    procedure OnRgShadowClick(Sender: TObject);
  end;

procedure TArrDummy.OnRgTypeClick(Sender: TObject);
begin
  if not Assigned(arr) then Exit;
  arr.ArrowType := TArrowType(TRadioGroup(Sender).ItemIndex);
end;

procedure TArrDummy.OnRgShadowClick(Sender: TObject);
begin
  if not Assigned(arr) then Exit;
  arr.ShadowType := TShadowType(TRadioGroup(Sender).ItemIndex);
end;

procedure TForm1.TestArrow;
var
  arr: TArrow;
  dummy: TArrDummy;
  rgtype, rgshadow: TRadioGroup;
begin
  debugln('TForm1.TestArrow');
  Clear;
  arr := TArrow.Create(nil);
  dummy := TArrDummy.Create;
  dummy.arr := arr;
  FObjList.Add(arr);
  FObjList.Add(dummy);
  ConnectStandardEvents(arr);
  arr.Top := 5;
  arr.Left := 5;
  arr.Width := 30;
  arr.Height := 30;
  arr.Color := clRed;
  arr.Parent := TestPnl;
  rgtype := TRadioGroup.Create(nil);
  FObjList.Add(rgtype);
  rgtype.Items.AddStrings(['atUp', 'atDown', 'atLeft', 'atRight']);
  rgtype.ItemIndex := Ord(arr.ArrowType);
  rgtype.OnClick := @dummy.OnRgTypeClick;
  rgtype.Left := arr.Left;
  rgtype.Top := arr.Top + arr.Height + 20;
  rgtype.Caption := 'ArrowType';
  rgtype.Parent := TestPnl;
  rgtype.AutoSize := True;
  rgshadow := TRadioGroup.Create(nil);
  FobjList.Add(rgshadow);
  rgshadow.Items.AddStrings(['stNone', 'stIn', 'stOut', 'stEtchedIn', 'stEtchedOut', 'stFilled']);
  rgshadow.ItemIndex := Ord(arr.ShadowType);
  rgshadow.OnClick := @dummy.OnRgShadowClick;
  rgshadow.Left := rgtype.Left + rgtype.Width + 20;
  rgshadow.Top := rgtype.Top;
  rgshadow.Caption := 'ShadowType';
  rgshadow.Parent := TestPnl;
  rgshadow.AutoSize := True;

end;

type
  { TBBDummy }
  TBBDummy = class
    B: TBitBtn;
    procedure OnRgKindClick(Sender: TObject);
  end;
{ TBBDummy }
procedure TBBDummy.OnRgKindClick(Sender: TObject);
begin
  if not assigned(B) then Exit;
  B.Kind := TBitBtnKind(TRadioGroup(Sender).ItemIndex);
  B.ModalResult := mrNone;
  if (B.Kind = bkCustom) then
  begin
    B.LoadGlyphFromResourceName(hInstance,'default');
    B.Caption := 'Custom';
  end;
end;

procedure TForm1.TestBitBtn;
var
  Btn: TBitBtn;
  dummy: TBBDummy;
  rg: TRadioGroup;
begin
  Clear;
  Btn := TBitBtn.Create(nil);
  dummy := TBBDummy.Create;
  rg := TRadioGroup.Create(nil);
  FObjList.Add(Btn);
  FObjList.Add(dummy);
  FObjList.Add(rg);
  ConnectStandardEvents(Btn);
  dummy.B := Btn;
  Btn.Kind := bkOk;
  Btn.ModalResult := mrNone;
  Btn.AutoSize := True;
  rg.Items.AddStrings(['bkCustom', 'bkOK', 'bkCancel', 'bkHelp', 'bkYes', 'bkNo',
                 'bkClose', 'bkAbort', 'bkRetry', 'bkIgnore', 'bkAll',
                 'bkNoToAll', 'bkYesToAll']);
  rg.AutoSize := True;
  rg.ItemIndex := Ord(Btn.Kind);
  rg.OnClick := @dummy.OnRgKindClick;
  Btn.Top := 10;
  Btn.Left := 5;
  rg.Top := Btn.Top + Btn.Height + 20;
  rg.Left := Btn.Left;
  Btn.Parent := TestPnl;
  rg.Parent := TestPnl;
end;


type
  { TMsgDlgDummmy }
  TMsgDlgDummmy = class
    ed: TEdit;
    cg: TCheckGroup;
    rg: TRadioGroup;
    btn: TButton;
    procedure OnBtnClick(Sender: TObject);
  end;
{ TMsgDlgDummmy }
procedure TMsgDlgDummmy.OnBtnClick(Sender: TObject);
var
  DT: TMsgDlgType;
  Btns: TMsgDlgButtons;
  i: Integer;
  aMsg: TCaption;
begin
  DT := TMsgDlgType(rg.ItemIndex);
  aMsg := ed.Text;
  if (aMsg = '')
    then aMsg := 'You did not enter a message';
  Btns := [];
  for i := 0 to cg.Items.Count - 1 do
  begin
    if cg.Checked[i] then
      Btns := Btns + [TMsgDlgBtn(i)];
  end;
  if (Btns = []) then
    Btns := [mbOk];
  MessageDlg('MessageDlg test', aMsg, DT, Btns, 0);
end;

procedure TForm1.TestMsgDlg;
var
  ed: TEdit;
  cg: TCheckGroup;
  rg: TRadioGroup;
  dummy: TMsgDlgDummmy;
  btn: TButton;
begin
  Clear;
  ed := TEdit.Create(nil);
  cg := TCheckGroup.Create(nil);
  rg := TRadioGroup.Create(nil);
  btn := TButton.Create(nil);
  dummy := TMsgDlgDummmy.Create;
  FObjList.Add(ed);
  FObjList.Add(cg);
  FObjList.Add(rg);
  FObjList.Add(btn);
  FObjList.Add(dummy);
  dummy.ed := ed;
  dummy.cg := cg;
  dummy.rg := rg;
  btn.OnClick := @dummy.OnBtnClick;
  ed.TextHint := 'Type your message here';
  ed.Parent := TestPnl;
  cg.Items.AddStrings(['mbYes', 'mbNo', 'mbOK', 'mbCancel', 'mbAbort', 'mbRetry', 'mbIgnore',
                       'mbAll', 'mbNoToAll', 'mbYesToAll', 'mbHelp', 'mbClose']);
  cg.Caption := 'Buttons';
  cg.AutoSize := True;
  rg.Items.AddStrings(['mtWarning', 'mtError', 'mtInformation', 'mtConfirmation',
                       'mtCustom']);
  cg.Parent := TestPnl;
  rg.Caption := 'DialogType';
  rg.AutoSize := True;
  rg.ItemIndex := Ord(mtInformation);
  ed.Left := 5;
  ed.Top := 10;
  ed.Width := TestPnl.Width - 2*5;
  cg.Left := 5;
  cg.Top := ed.Top + ed.Height + 10;
  rg.Left := cg.Left + cg.Width + 20;
  rg.Top := cg.Top;
  rg.Parent := TestPnl;
  btn.Caption := 'Show dialog';
  btn.AutoSize := True;
  btn.Top := rg.Top + rg.Height + 20;
  btn.Left := rg.Left;
  btn.Parent := TestPnl;
end;


