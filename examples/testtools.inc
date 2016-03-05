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

{---------------- TButton; ----------------}
type
  { TButDummy }
  TButDummy = class
    Btn: TButton;
    Tm: TTimer;
    OldClick: TNotifyEvent;
    procedure OnTimer(Sender: TObject);
    procedure OnBtnClick(Sender: TObject);
  end;
{ TButDummy }
procedure TButDummy.OnTimer(Sender: TObject);
var
  PW, PH, BW, BH: Integer;
begin
  if not Assigned(Btn) then Exit;
  if not Assigned(Btn.Parent) then Exit;
  PW := Btn.Parent.Width;
  PH := Btn.Parent.Height;
  BW := Btn.Width;
  BH := Btn.Height;
  Btn.Left := Random(PW-BW);
  Btn.Top := Random(PH-BH);
end;

procedure TButDummy.OnBtnClick(Sender: TObject);
begin
  OldClick(Sender);
  if Tm.Enabled then
  begin
    OnTimer(Sender); //move the button first
    Btn.Caption := Btn.Caption + '...';
    Tm.Interval := Tm.Interval - 100;
    if Tm.Interval < 500 then
    begin
      Tm.Enabled := False;
      Btn.Caption := 'Stop hitting me!';
    end;
  end
  else
  begin
    ShowMessage('I told you NOT to hit me!'^m^m'Game''s over!');
    Btn.Enabled := False;
  end;
end;

procedure TForm1.TestButton;
var
  Btn: TButton;
  Tm: TTimer;
  dummy: TButDummy;
begin
  Clear;
  Btn := TButton.Create(nil);
  Tm := TTimer.Create(nil);
  dummy := TButDummy.Create;
  FObjList.Add(Btn);
  FObjList.Add(Tm);  //Add it after Btn, so Tm will be destroyed before Btn!
  FObjList.Add(dummy);
  dummy.Btn := Btn;
  dummy.Tm := Tm;
  Btn.Caption := 'Click me ...';
  Btn.AutoSize := True;
  Btn.Top := 10;
  Btn.Left := 5;
  Btn.Cursor := crCross;
  ConnectStandardEvents(Btn);
  dummy.OldClick := Btn.OnClick;
  Btn.OnClick := @dummy.OnBtnClick;

  Btn.Parent := TestPnl;
  Tm.Interval := 1000;
  Tm.OnTimer := @dummy.OnTimer;
  Tm.Enabled := True;
end;
{---------------- End of TButton; ---------}


{---------------- TButtonpanel; ----------------}
type
  { TBPDummy }
  TBPDummy = class
    bp: TButtonPanel;
    cg: TCheckGroup;
    rg: TRadioGroup;
    procedure OnCgItemClick(Sender: TObject; Index: integer);
    procedure OnRgClick(Sender: TObject);
  end;

procedure TBPDummy.OnCgItemClick(Sender: TObject; Index: integer);
var
  b: TPanelButton;
begin
  if not assigned(bp) then Exit;
  b := TPanelButton(Index);
  if cg.Checked[Index] then
  begin
    bp.ShowButtons := bp.ShowButtons + [b];
  end
  else
  begin
    bp.ShowButtons := bp.ShowButtons - [b];
  end;
  if Assigned(bp.CloseButton) then bp.CloseButton.ModalResult := mrNo;
end;

procedure TBPDummy.OnRgClick(Sender: TObject);
begin
  if not assigned(bp) then Exit;
  bp.ButtonOrder := TButtonOrder(rg.ItemIndex);
end;

procedure TForm1.TestButtonpanel;
var
  bp: TButtonPanel;
  cg: TCheckGroup;
  b: TPanelButton;
  dummy: TBPDummy;
  rg: TRadioGroup;
begin
  Clear;
  bp := TButtonPanel.Create(nil);
  cg := TCheckGroup.Create(nil);
  rg := TRadioGroup.Create(nil);
  dummy := TBPDummy.Create;
  FObjList.Add(bp);
  FObjList.Add(cg);
  FObjList.Add(rg);
  FObjList.Add(dummy);
  dummy.bp := bp;
  dummy.cg := cg;
  dummy.rg := rg;
  if assigned(bp.CloseButton) then bp.CloseButton.ModalResult := mrNO; //otherwise it will close the application
  ConnectStandardEvents(bp);
  ConnectStandardEvents(bp.OKButton);
  ConnectStandardEvents(bp.CancelButton);
  ConnectStandardEvents(bp.CloseButton);
  ConnectStandardEvents(bp.HelpButton);
  cg.Items.AddStrings(['pbOK','pbCancel','pbClose','pbHelp']);
  for b := Low(TPanelButton) to High(TPanelButton) do
    if (b in bp.ShowButtons) then cg.Checked[Ord(b)] := True;
  cg.OnItemClick := @dummy.OnCgItemClick;
  cg.AutoSize := True;
  cg.Caption := 'ShowButtons';
  bp.Parent := TestPnl;
  cg.Left := 5;
  cg.Top := 20;
  cg.Parent := TestPnl;
  rg.Items.AddStrings(['boDefault', 'boCloseCancelOK', 'boCloseOKCancel']);
  rg.ItemIndex := Ord(bp.ButtonOrder);
  rg.Caption := 'ButtonOrder';
  rg.Autosize := True;
  rg.OnClick := @dummy.OnRgClick;
  rg.Left := cg.Left + cg.Width + 20;
  rg.Top := cg.Top;
  rg.Parent := TestPnl;

end;
{---------------- End of TButtonpanel; ---------}


{---------------- TCalcedit; ----------------}

procedure TForm1.TestCalcedit;
begin
  Clear;
end;
{---------------- End of TCalcedit; ---------}


{---------------- TCalendar; ----------------}

procedure TForm1.TestCalendar;
begin
  Clear;
end;
{---------------- End of TCalendar; ---------}


{---------------- TCheckbox; ----------------}

procedure TForm1.TestCheckbox;
begin
  Clear;
end;
{---------------- End of TCheckbox; ---------}


{---------------- TCheckcombobox; ----------------}

procedure TForm1.TestCheckcombobox;
begin
  Clear;
end;
{---------------- End of TCheckcombobox; ---------}


{---------------- TCheckgroup; ----------------}

procedure TForm1.TestCheckgroup;
begin
  Clear;
end;
{---------------- End of TCheckgroup; ---------}


{---------------- TChecklistbox; ----------------}

procedure TForm1.TestChecklistbox;
begin
  Clear;
end;
{---------------- End of TChecklistbox; ---------}


{---------------- TColorbox; ----------------}

procedure TForm1.TestColorbox;
begin
  Clear;
end;
{---------------- End of TColorbox; ---------}


{---------------- TColorbutton; ----------------}

procedure TForm1.TestColorbutton;
begin
  Clear;
end;
{---------------- End of TColorbutton; ---------}


{---------------- TColorlistbox; ----------------}

procedure TForm1.TestColorlistbox;
begin
  Clear;
end;
{---------------- End of TColorlistbox; ---------}


{---------------- TCombobox; ----------------}

procedure TForm1.TestCombobox;
begin
  Clear;
end;
{---------------- End of TCombobox; ---------}


{---------------- TComboboxex; ----------------}

procedure TForm1.TestComboboxex;
begin
  Clear;
end;
{---------------- End of TComboboxex; ---------}


{---------------- TControlbar; ----------------}

procedure TForm1.TestControlbar;
begin
  Clear;
end;
{---------------- End of TControlbar; ---------}


{---------------- TCoolbar; ----------------}

procedure TForm1.TestCoolbar;
begin
  Clear;
end;
{---------------- End of TCoolbar; ---------}


{---------------- TDateedit; ----------------}

procedure TForm1.TestDateedit;
begin
  Clear;
end;
{---------------- End of TDateedit; ---------}


{---------------- TDirectoryedit; ----------------}

procedure TForm1.TestDirectoryedit;
begin
  Clear;
end;
{---------------- End of TDirectoryedit; ---------}


{---------------- TEdit; ----------------}

procedure TForm1.TestEdit;
begin
  Clear;
end;
{---------------- End of TEdit; ---------}


{---------------- TEditbutton; ----------------}

procedure TForm1.TestEditbutton;
begin
  Clear;
end;
{---------------- End of TEditbutton; ---------}


{---------------- TFilelistbox; ----------------}

procedure TForm1.TestFilelistbox;
begin
  Clear;
end;
{---------------- End of TFilelistbox; ---------}


{---------------- TFilenameedit; ----------------}

procedure TForm1.TestFilenameedit;
begin
  Clear;
end;
{---------------- End of TFilenameedit; ---------}


{---------------- TFiltercombobox; ----------------}

procedure TForm1.TestFiltercombobox;
begin
  Clear;
end;
{---------------- End of TFiltercombobox; ---------}


{---------------- TFloatspinedit; ----------------}

procedure TForm1.TestFloatspinedit;
begin
  Clear;
end;
{---------------- End of TFloatspinedit; ---------}


{---------------- TGroupbox; ----------------}

procedure TForm1.TestGroupbox;
begin
  Clear;
end;
{---------------- End of TGroupbox; ---------}


{---------------- THeadercontrol; ----------------}

procedure TForm1.TestHeadercontrol;
begin
  Clear;
end;
{---------------- End of THeadercontrol; ---------}


{---------------- TImage; ----------------}

procedure TForm1.TestImage;
begin
  Clear;
end;
{---------------- End of TImage; ---------}


{---------------- TLabel; ----------------}

procedure TForm1.TestLabel;
begin
  Clear;
end;
{---------------- End of TLabel; ---------}


{---------------- TLabelededit; ----------------}

procedure TForm1.TestLabelededit;
begin
  Clear;
end;
{---------------- End of TLabelededit; ---------}


{---------------- TListbox; ----------------}

procedure TForm1.TestListbox;
begin
  Clear;
end;
{---------------- End of TListbox; ---------}


{---------------- TListview; ----------------}

procedure TForm1.TestListview;
begin
  Clear;
end;
{---------------- End of TListview; ---------}


{---------------- TMaskedit; ----------------}

procedure TForm1.TestMaskedit;
begin
  Clear;
end;
{---------------- End of TMaskedit; ---------}


{---------------- TMemo; ----------------}

procedure TForm1.TestMemo;
begin
  Clear;
end;
{---------------- End of TMemo; ---------}


{---------------- TNotebook; ----------------}

procedure TForm1.TestNotebook;
begin
  Clear;
end;
{---------------- End of TNotebook; ---------}


{---------------- TPagecontrol; ----------------}

procedure TForm1.TestPagecontrol;
begin
  Clear;
end;
{---------------- End of TPagecontrol; ---------}


{---------------- TPaintbox; ----------------}

procedure TForm1.TestPaintbox;
begin
  Clear;
end;
{---------------- End of TPaintbox; ---------}


{---------------- TPanel; ----------------}

procedure TForm1.TestPanel;
begin
  Clear;
end;
{---------------- End of TPanel; ---------}


{---------------- TProgressbar; ----------------}

procedure TForm1.TestProgressbar;
begin
  Clear;
end;
{---------------- End of TProgressbar; ---------}


{---------------- TRadiobutton; ----------------}

procedure TForm1.TestRadiobutton;
begin
  Clear;
end;
{---------------- End of TRadiobutton; ---------}


{---------------- TRadiogroup; ----------------}

procedure TForm1.TestRadiogroup;
begin
  Clear;
end;
{---------------- End of TRadiogroup; ---------}


{---------------- TShape; ----------------}

procedure TForm1.TestShape;
begin
  Clear;
end;
{---------------- End of TShape; ---------}


{---------------- TShelllistview; ----------------}

procedure TForm1.TestShelllistview;
begin
  Clear;
end;
{---------------- End of TShelllistview; ---------}


{---------------- TShelltreeview; ----------------}

procedure TForm1.TestShelltreeview;
begin
  Clear;
end;
{---------------- End of TShelltreeview; ---------}


{---------------- TSpeedbutton; ----------------}

procedure TForm1.TestSpeedbutton;
begin
  Clear;
end;
{---------------- End of TSpeedbutton; ---------}


{---------------- TSpinedit; ----------------}

procedure TForm1.TestSpinedit;
begin
  Clear;
end;
{---------------- End of TSpinedit; ---------}


{---------------- TSplitter; ----------------}

procedure TForm1.TestSplitter;
begin
  Clear;
end;
{---------------- End of TSplitter; ---------}


{---------------- TStacTictext; ----------------}

procedure TForm1.TestStacTictext;
begin
  Clear;
end;
{---------------- End of TStacTictext; ---------}


{---------------- TStatusbar; ----------------}

procedure TForm1.TestStatusbar;
begin
  Clear;
end;
{---------------- End of TStatusbar; ---------}


{---------------- TStringgrid; ----------------}

procedure TForm1.TestStringgrid;
begin
  Clear;
end;
{---------------- End of TStringgrid; ---------}


{---------------- TTabcontrol; ----------------}

procedure TForm1.TestTabcontrol;
begin
  Clear;
end;
{---------------- End of TTabcontrol; ---------}


{---------------- TTimeedit; ----------------}

procedure TForm1.TestTimeedit;
begin
  Clear;
end;
{---------------- End of TTimeedit; ---------}


{---------------- TTimer; ----------------}

procedure TForm1.TestTimer;
begin
  Clear;
end;
{---------------- End of TTimer; ---------}


{---------------- TTogglebox; ----------------}

procedure TForm1.TestTogglebox;
begin
  Clear;
end;
{---------------- End of TTogglebox; ---------}


{---------------- TToolbar; ----------------}

procedure TForm1.TestToolbar;
begin
  Clear;
end;
{---------------- End of TToolbar; ---------}


{---------------- TTrackbar; ----------------}

procedure TForm1.TestTrackbar;
begin
  Clear;
end;
{---------------- End of TTrackbar; ---------}


{---------------- TTreeview; ----------------}

procedure TForm1.TestTreeview;
begin
  Clear;
end;
{---------------- End of TTreeview; ---------}


{---------------- TUpdown; ----------------}

procedure TForm1.TestUpdown;
begin
  Clear;
end;
{---------------- End of TUpdown; ---------}


{---------------- TValuelisteditor; ----------------}

procedure TForm1.TestValuelisteditor;
begin
  Clear;
end;
{---------------- End of TValuelisteditor; ---------}


{---------------- MessageDialog; ----------------}

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

procedure TForm1.TestMessageDialog;
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
{---------------- End of MessageDialog; ---------}


{---------------- CalculatorDialog; ----------------}

procedure TForm1.TestCalculatorDialog;
begin
  Clear;
end;
{---------------- End of CalculatorDialog; ---------}


{---------------- CalendarDialog; ----------------}

procedure TForm1.TestCalendarDialog;
begin
  Clear;
end;
{---------------- End of CalendarDialog; ---------}


{---------------- ColorDialog; ----------------}

procedure TForm1.TestColorDialog;
begin
  Clear;
end;
{---------------- End of ColorDialog; ---------}


{---------------- FindDialog; ----------------}

procedure TForm1.TestFindDialog;
begin
  Clear;
end;
{---------------- End of FindDialog; ---------}


{---------------- FontDialog; ----------------}

procedure TForm1.TestFontDialog;
begin
  Clear;
end;
{---------------- End of FontDialog; ---------}


{---------------- OpenDialog; ----------------}

procedure TForm1.TestOpenDialog;
begin
  Clear;
end;
{---------------- End of OpenDialog; ---------}


{---------------- OpenpictureDialog; ----------------}

procedure TForm1.TestOpenpictureDialog;
begin
  Clear;
end;
{---------------- End of OpenpictureDialog; ---------}


{---------------- ReplaceDialog; ----------------}

procedure TForm1.TestReplaceDialog;
begin
  Clear;
end;
{---------------- End of ReplaceDialog; ---------}


{---------------- SaveDialog; ----------------}

procedure TForm1.TestSaveDialog;
begin
  Clear;
end;
{---------------- End of SaveDialog; ---------}


{---------------- SavepictureDialog; ----------------}

procedure TForm1.TestSavepictureDialog;
begin
  Clear;
end;
{---------------- End of SavepictureDialog; ---------}


{---------------- SelectdirectoryDialog; ----------------}

procedure TForm1.TestSelectdirectoryDialog;
begin
  Clear;
end;
{---------------- End of SelectdirectoryDialog; ---------}


{---------------- QuesionDialog; ----------------}

procedure TForm1.TestQuestionDialog;
begin
  Clear;
end;
{---------------- End of QuesionDialog; ---------}

