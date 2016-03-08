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

{---------------------------  End of Components -------------------------}


{--------------------------- Start of Dialogs ---------------------------}

const
  MsgDlgBtnStr: array[TMsgDlgBtn] of String = (
    'mbYes', 'mbNo', 'mbOK', 'mbCancel', 'mbAbort', 'mbRetry', 'mbIgnore',
    'mbAll', 'mbNoToAll', 'mbYesToAll', 'mbHelp', 'mbClose'
    );
  MsgDlgTypeStr: array[TMsgDlgType] of String = (
    'mtWarning', 'mtError', 'mtInformation',
    'mtConfirmation', 'mtCustom'
    );

function ModalResultToStr(Mr: TModalResult): String;
begin
  case Mr of
    mrNone: Result :=  'mrNone';
    mrOK: Result :=  'mrOK';
    mrCancel: Result :=  'mrCancel';
    mrAbort: Result :=  'mrAbort';
    mrRetry: Result :=  'mrRetry';
    mrIgnore: Result :=  'mrInore';
    mrYes: Result :=  'mrYes';
    mrNo: Result :=  'mrNo';
    mrAll: Result :=  'mrAll';
    mrNoToAll: Result :=  'mrNoToAll';
    mrYesToAll: Result :=  'mrYesToAll';
    mrClose: Result :=  'mrClose';
    else Result := IntToStr(Mr);
  end;
end;

{---------------- MessageDialog; ----------------}

type
  { TMsgDlgDummmy }
  TMsgDlgDummmy = class
    ed: TEdit;
    cg: TCheckGroup;
    rg: TRadioGroup;
    btn: TButton;
    lbl: TLabel;
    procedure OnBtnClick(Sender: TObject);
  end;
{ TMsgDlgDummmy }
procedure TMsgDlgDummmy.OnBtnClick(Sender: TObject);
var
  DT: TMsgDlgType;
  Btns: TMsgDlgButtons;
  i: Integer;
  aMsg: TCaption;
  Res: TModalResult;
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
  Res := MessageDlg('MessageDlg test', aMsg, DT, Btns, 0);
  lbl.Caption := 'Dialog result = ' + ModalResultToStr(Res);
end;

procedure TForm1.TestMessageDialog;
var
  ed: TEdit;
  cg: TCheckGroup;
  rg: TRadioGroup;
  dummy: TMsgDlgDummmy;
  lbl: TLabel;
  btn: TButton;
  mdbtn: TMsgDlgBtn;
  mdt: TMsgDlgType;
begin
  Clear;
  ed := TEdit.Create(nil);
  cg := TCheckGroup.Create(nil);
  rg := TRadioGroup.Create(nil);
  btn := TButton.Create(nil);
  lbl := TLabel.Create(nil);
  dummy := TMsgDlgDummmy.Create;
  FObjList.Add(ed);
  FObjList.Add(cg);
  FObjList.Add(rg);
  FObjList.Add(btn);
  FObjList.Add(lbl);
  FObjList.Add(dummy);
  dummy.ed := ed;
  dummy.cg := cg;
  dummy.rg := rg;
  dummy.lbl := lbl;
  btn.OnClick := @dummy.OnBtnClick;

  ed.TextHint := 'Type your message here';
  ed.Parent := TestPnl;

  cg.Caption := 'Buttons';
  for mdbtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    cg.Items.Add(MsgDlgBtnStr[mdbtn]);
  cg.AutoSize := True;
  cg.Parent := TestPnl;

  rg.Caption := 'DialogType';
  for mdt := Low(TMsgDlgType) to high(TMsgDlgType) do
    rg.items.Add(MsgDlgTypeStr[mdt]);
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

  lbl.Caption := '';
  lbl.Top := btn.Top + btn.Height + 20;
  lbl.Left := btn.Left;
  lbl.Parent := TestPnl;
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
type
{ TSelDirDlgDummmy }
 TSelDirDlgDummy = class
  cg : TCheckGroup;
  btn: TButton;
  ed: TEdit;
  lbl: TLabel;
  Sdd: TSelectDirectoryDialog;
  Procedure OnBtnClick(sender:TObject);
end;

procedure TSelDirDlgDummy.OnBtnClick(sender: TObject);
var
  Opts:TOpenOptions;
  i: Integer;
begin
  Opts:= [];
  for i := 0 to cg.Items.Count - 1 do
    if cg.Checked[i] then
       Opts := Opts + [TOpenOption(i)];
  if Opts = [] then
    Opts := sdd.Options;
  sdd.Options:=Opts;
  if sdd.Execute then
    lbl.Caption := 'Selected: ' + {ExtractFileDir}(sdd.FileName)
  else
    lbl.Caption := 'Dialog was canceled';
end;


procedure TForm1.TestSelectdirectoryDialog;
var
  dummy: TSelDirDlgDummy;
  cg : TCheckGroup;
  btn: TButton;
  ed: TEdit;
  lbl:TLabel;
  Sdd: TSelectDirectoryDialog;
  i: Integer;
  opt: TOpenOption;
begin
  Clear;
  ed := TEdit.Create(nil);
  sdd := TSelectDirectoryDialog.Create(nil);
  btn := TButton.Create(nil);
  cg := TCheckGroup.Create(nil);
  lbl := TLabel.Create(nil);
  FObjList.Add(ed);
  FObjList.Add(sdd);
  FObjList.Add(btn);
  FObjList.Add(cg);
  FObjList.Add(lbl);
  dummy := TSelDirDlgDummy.Create;
  FObjList.Add(dummy);
  dummy.ed := ed;
  dummy.Sdd := Sdd;
  dummy.btn := btn;
  dummy.cg :=cg;
  dummy.lbl := lbl;

  ed.TextHint := 'Enter a Start-Directory here';
  ed.Parent := TestPnl;
  ed.Top:=7;
  ed.left:=5;
  ed.width := TestPnl.Width - 120;
  ed.Anchors:=ed.Anchors + [akRight];

  btn.Caption:= 'Select Directory';
  btn.AutoSize:=true;
  btn.Top:=ed.top -2;
  btn.left:=ed.Left + ed.Width + 4;
  btn.parent := TestPnl;
  btn.onclick := @dummy.OnBtnClick;
  btn.Anchors:=ed.Anchors + [akRight]-[akLeft];

  for opt := Low(TOpenOption) to High(TOpenOption) do
    cg.Items.Add(DbgS(opt));
  cg.Caption := 'OpenOptions';
  cg.ColumnLayout:=clVerticalThenHorizontal;
  cg.Columns:=2;
  cg.Parent := TestPnl;
  cg.Left := 5;
  cg.AutoSize := True;
  cg.Top := btn.Top + btn.Height + 10;
  cg.Anchors:=cg.Anchors + [akRight,akBottom];
  for i := 0 to cg.Items.Count - 1 do
    cg.Checked[i]:= TOpenOption(i) in sdd.Options;

  lbl.Top := cg.Top + cg.Height + 10;
  lbl.Left := cg.Left;
  lbl.Caption := '';
  lbl.Parent := TestPnl;
end;
{---------------- End of SelectdirectoryDialog; ---------}


{---------------- QuesionDialog; ----------------}
type
  TQRange = 1..3;
const
  QAnsStrings: array[TQRange] of String = (
    'Appels', 'Pears', 'Programmers');
  QAnsResults: array[TQRange] of Integer = (
    -1, -2, -3);
type
  { TQuesionDlgDummmy }
  TQuesionDlgDummmy = class
    qed, a1ed, a2ed, a3ed: TEdit;
    rg: TRadioGroup;
    btn: TButton;
    lbl: TLabel;
    procedure OnBtnClick(Sender: TObject);
  end;

procedure TQuesionDlgDummmy.OnBtnClick(Sender: TObject);
var
  DT: TMsgDlgType;
  aMsg: TCaption;
  Res: TModalResult;
  Ans: Array[TQRange] of String;
  i: TQRange;
begin
  DT := TMsgDlgType(rg.ItemIndex);
  aMsg := qed.Text;
  Ans[1] := a1ed.Text;
  Ans[2] := a2ed.Text;
  Ans[3] := a3ed.Text;
  if (aMsg = '') then
  begin
    if (Ans[1]='') and (Ans[2]='') and (Ans[3]='') then
    begin
      aMsg := 'Which fruit do you prefer?';
      for i := Low(TQRange) to High(TQRange) do Ans[i] := QAnsStrings[i];
    end
    else
      aMsg := 'The question is undefined: answer as best as you can anyway ...';
  end;
  for i := Low(TQRange) to High(TQRange) do
    if (Ans[i] = '') then Ans[i] := Format('Undefined answer [%d]',[i]);
  //Res := QuestionDlg('QuestionDlg test', aMsg, DT, [mrYes,'&Yes','isDefault',mrNo, '&No'], 0);
  Res := QuestionDlg('QuestionDlg test', aMsg, DT,
                     [QAnsResults[1], Ans[1],
                      QAnsResults[2], Ans[2],
                      QAnsResults[3], Ans[3] ], 0);
  if (-Res in [-QAnsResults[Low(QAnsResults)]..-QAnsResults[High(QAnsResults)]]) then
    lbl.Caption := Format('You chose: "%s"',[Ans[-Res]])
  else
    lbl.Caption := 'You chose nothing';
end;


procedure TForm1.TestQuestionDialog;
var
  dummy: TQuesionDlgDummmy;
  qed, a1ed, a2ed, a3ed: TEdit;
  btn: TButton;
  rg: TRadioGroup;
  mdt: TMsgDlgType;
  lbl: TLabel;
begin
  Clear;
  qed := TEdit.Create(nil);
  a1ed := TEdit.Create(nil);
  a2ed := TEdit.Create(nil);
  a3ed := TEdit.Create(nil);
  rg := TRadioGroup.Create(nil);
  btn := TButton.Create(nil);
  lbl := TLabel.Create(nil);
  FObjList.Add(btn);
  FObjList.Add(qed);
  FObjList.Add(a1ed);
  FObjList.Add(a2ed);
  FObjList.Add(a3ed);
  FObjList.Add(rg);
  FObjList.Add(lbl);
  dummy := TQuesionDlgDummmy.Create;
  FObjList.Add(dummy);
  dummy.qed := qed;
  dummy.a1ed := a1ed;
  dummy.a2ed := a2ed;
  dummy.a3ed := a3ed;
  dummy.btn := btn;
  dummy.rg := rg;
  dummy.lbl := lbl;
  qed.TextHint := 'Type your question here';
  qed.Left := 5;
  qed.Top := 10;
  qed.Width := TestPnl.Width - 2*5;
  qed.Parent := TestPnl;

  a1ed.Left := qed.Left;
  a1ed.Width := qed.Width;
  a1ed.Top := qed.Top + qed.Height + 10;
  a1ed.MaxLength := 40;
  //a1ed.Text := QAnsStrings[1];
  a1ed.TextHint := 'Type the first answer here';
  a1ed.Parent := TestPnl;

  a2ed.Left := a1ed.Left;
  a2ed.Width := qed.Width;
  a2ed.Top := a1ed.Top + a1ed.Height + 10;
  a2ed.MaxLength := 40;
  //a2ed.Text := QAnsStrings[2];
  a2ed.TextHint := 'Type the second answer here';
  a2ed.Parent := TestPnl;

  a3ed.Left := a2ed.Left;
  a3ed.Width := qed.Width;
  a3ed.Top := a2ed.Top + a2ed.Height + 10;
  a3ed.MaxLength := 40;
  //a3ed.Text := QAnsStrings[3];
  a3ed.TextHint := 'Type the third answer here';
  a3ed.Parent := TestPnl;

  rg.Caption := 'DialogType';
  for mdt := Low(TMsgDlgType) to high(TMsgDlgType) do
    rg.items.Add(MsgDlgTypeStr[mdt]);
  rg.AutoSize := True;
  rg.ItemIndex := Ord(mtConfirmation);
  rg.Left := qed.Left;
  rg.Top := a3ed.Top + a3ed.Height + 10;
  rg.Parent := TestPnl;

  btn.OnClick := @dummy.OnBtnClick;
  btn.Caption := 'Show dialog';
  btn.AutoSize := True;
  btn.Top := rg.Top + rg.Height + 20;
  btn.Left := rg.Left;
  btn.Parent := TestPnl;

  lbl.Top := btn.Top;
  lbl.Left := btn.Left + btn.Width + 10;;
  lbl.Caption := '';
  lbl.Parent := TestPnl;
end;
{---------------- End of QuesionDialog; ---------}



