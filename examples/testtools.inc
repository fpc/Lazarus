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
Const

Info : Array[0..5] of PChar = ('        Do not exist yet...       ',
                               ' This component is completed !  '+#13#10+' Minor errors could be at presence, '+#13#10+'report them to the Lazarus team.',
                               ' Almost completed, some procedures '+#13#10+' are missing in consideration to '+#13#10+' Delphi5 compatibility and some minor '+#13#10+' errors are at presence',
                               ' Basic code completed, finalization '+#13#10+' and cleanup is necessary. '+#13#10+' Errors are at presence.  ',
                               ' A few procedures are completed, many errors are at presence. ',
                               ' Could crash/freeze your system! It will be created when you push this button. Try it on your own risk!');
FileN : Array[0..8] of String =('tbitbtn','tlistbox','btn_newform','btn_newunit',
                                'tnotebook','tmemo','btn_mouse','tcheckbox','btn_viewunits');
//-----------------------------------------------------------------------------------
//                              CreateTestTools is a procedure
//                              that loads all test components
//                              and procedures
//-----------------------------------------------------------------------------------
procedure TForm1.CreateTestTools;
var
i : Integer;
begin
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TApplication Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rdbOk               := TRadioButton.Create(Self); //RadioButton OK
   FBoxStyle        := 0;
   With rdbOk do
   begin
     Checked        := True;
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 10;
     Left           := 130;
     Height         := 20;
     Width          := 130;
     Tag            := 0;
     Show;
     Caption        := 'OK';
   end;
rdbOkCancel         := TRadioButton.Create(Self); //RadioButton OkCancel
   With rdbOkCancel do
   begin
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 30;
     Left           := 130;
     Checked        := False;
     Height         := 20;
     Width          := 130;
     Tag            := 1;
     Show;
     Caption        := 'OK,Cancel';
   end;
rdbAbortRetryIgnore := TRadioButton.Create(Self); //RadioButton AbortRetryIgnore
   With rdbAbortRetryIgnore do
   begin
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 50;
     Left           := 130;
     Checked        := False;
     Height         := 20;
     Width          := 130;
     Tag            := 2;
     Show;
     Caption        := 'Abort,Retry,Ignore';
   end;
rdbYesNoCancel      := TRadioButton.Create(Self); //RadioButton YesNoCancel
   With rdbYesNoCancel do
   begin
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 70;
     Left           := 130;
     Checked        := False;
     Height         := 20;
     Width          := 130;
     Tag := 2;
     Show;
     Caption        := 'Yes,No,Cancel';
   end;
rdbYesNo            := TRadioButton.Create(Self); //RadioButton YesNo
   With rdbYesNo do
   begin
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 90;
     Left           := 130;
     Checked        := False;
     Height         := 20;
     Width          := 130;
     Tag            := 2;
     Show;
     Caption        := 'Yes,No';
   end;
rdbRetryCancel      := TRadioButton.Create(Self); //RadioButton RetryCancel
   With rdbRetryCancel do
   begin
     Parent         := BenchForm[1];
     OnClick        := @BoxStyle;
     Top            := 110;
     Left           := 130;
     Checked        := False;
     Height         := 20;
     Width          := 130;
     Tag            := 2;
     Show;
     Caption        := 'Retry,Cancel';
   end;
btnShowBox          := TBitBtn.Create(Self); //MessageBox show button
   with btnShowBox do
   begin
     OnClick        := @ShowBox;
     Parent         := BenchForm[1];
     Top            := 10;
     Left           := 10;
     Width          := 85;
     Height         := 21;
     Kind           := bkCustom;
     Caption        := 'MessageBox';
     Hint           := 'Show MessageBox style';
     ShowHint       := True;
     Visible        := True;
   end;
lblExeName          := TLabel.Create(Self); //Label showing Application.ExeName
   With lblExeName do
   begin
     Parent         := BenchForm[1];
     Top            := 130;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'ExeName: '+Application.ExeName;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TBevel Tools +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  chbBevelStyle := TRadioGroup.Create (self);
  with chbBevelStyle do
  begin
     Parent := BenchForm[2];
     OnClick := @BevelSettings;
     Top    := 10;
     Left   := 160;
     Height := 60;
     Width  := 180;
     Items.Add ('lowered');
     Items.Add ('raised');
     Hint := 'Change style (lowered/raised)';
     ShowHint := True;
     ItemIndex := 0;
     Show;
     Caption := 'Bevel Styles';
  End;
  chbBevelShape := TRadioGroup.Create (self);
  with chbBevelShape do
  begin
     Parent := BenchForm[2];
     top    := 80;
     left   := 160;
     Height := 200;
     Width  := 180;
     OnClick := @BevelSettings;
     Items.Add ('bsBox');
     Items.Add ('bsFrame');
     Items.Add ('bsTopLine');
     Items.Add ('bsBottomLine');
     Items.Add ('bsLeftLine');
     Items.Add ('bsRightLine');
     ItemIndex := 0;
     Show;
     Caption := 'Bevel Shapes';
  end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TBitBtn Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cbbKind       := TComboBox.Create(Self);
   with cbbKind do
   begin
     OnChange := @KindComboChange;
     Parent   := BenchForm[3];
     Top      := 10;
     Left     := 110;
     Width    := 105;
     Height   := 28;
     Items.Add('bkOK');
     Items.Add('bkCancel');
     Items.Add('bkHelp');
     Items.Add('bkClose');
     Items.Add('bkAll');
     Items.Add('bkCustom');
     Show;
   end;
cbbAlign      := TComboBox.Create(Self);
   with cbbAlign do
   begin
     OnChange := @AlignComboChange;
     Parent   := BenchForm[3];
     Top      := 38;
     Left     := 110;
     Width    := 105;
     Height   := 28;
     Items.Add('blGlyphTop');
     Items.Add('blGlyphBottom');
     Items.Add('blGlyphRight');
     Items.Add('blGlyphLeft');
     Show;
   end;
btnGlyph      := TBitBtn.Create(Self);
   with btnGlyph do
   begin
     OnClick  := @LoadGlyph;
     Parent   := BenchForm[3];
     Top      := 70;
     Left     := 110;
     Width    := 45;
     Height   := 25;
     Kind     := bkCustom;
     Caption  := 'Glyph';
     Hint     := 'Pick a Glyph (ONLY .xpm FILES)';
     ShowHint := True;
     Visible  := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TButton Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnClrLotto   := TBitBtn.Create(Self); //Reset Lotto
   with btnClrLotto do
   begin
     OnClick  := @LottoClear;
     Parent   := BenchForm[4];
     Top      := 140;  
     Left     := 30;
     Width    := 45;
     Height   := 21;
     Kind     := bkCustom;
     Caption  := 'Again';
     Hint     := 'Clear the Buttons';
     ShowHint := True;
     Visible  := True;
   end;
lblLResult    := TLabel.Create(Self); //Label Result
   With lblLResult do
   begin
     Parent   := BenchForm[4];
     Top      := 3;
     Left     := 30;
     Width    := 400;
     Height   := 23;
     Show;
     Caption  := 'Result: ';
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TCanvas Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cbbPaintType  := TComboBox.Create(Self); //What to paint
   with cbbPaintType do
   begin
     Parent   := BenchForm[5];
     Top      := 260;
     Left     := 10;
     Width    := 90;
     Height   := 28;
     Items.Add('Rectangle');
     Items.Add('LineTo');
     Show;
   end;
lblPaintWhat  := TLabel.Create(Self); //Label Paint what
   With lblPaintWhat do
   begin
     Parent   := BenchForm[5];
     Top      := 240;
     Left     := 10;
     Width    := 200;
     Height   := 23;
     Show;
     Caption  := 'Paint what';
   end;
cbbBrushStyle := TComboBox.Create(Self); //Canvas.BrushStyle
   with cbbBrushStyle do
   begin
     OnChange := @StyleSelect;
     Parent   := BenchForm[5];
     Top      := 260;
     Left     := 105;
     Width    := 100;
     Height   := 28;
     Items.Add('bsBDiagonal');
     Items.Add('bsClear');
     Items.Add('bsCross');
     Items.Add('bsDiagCross');
     Items.Add('bsFDiagonal');
     Items.Add('bsHorizontal');
     Items.Add('bsVertical');
     Items.Add('bsSolid');
     Show;
   end;

lblBrushStyle := TLabel.Create(Self); //Label Brush style
   With lblBrushStyle do
   begin
     Parent   := BenchForm[5];
     Top      := 240;
     Left     := 105;
     Width    := 200;
     Height   := 23;
     Show;
     Caption  := 'Brush style';
   end;

cbbPenStyle := TComboBox.Create(Self); //Canvas.PenStyle
   with cbbPenStyle do
   begin
     OnChange := @PenStyleSelect;
     Parent   := BenchForm[5];
     Top      := 220;
     Left     := 10;
     Width    := 110;
     Height   := 28;
     Items.Add('psClear');
     Items.Add('psDash');
     Items.Add('psDashDot');
     Items.Add('psDashDotDot');
     Items.Add('psDot');
     Items.Add('psSolid');
     Show;
   end;

lblPenStyle := TLabel.Create(Self); //Label Pen style
   With lblPenStyle do
   begin
     Parent   := BenchForm[5];
     Top      := 200;
     Left     := 10;
     Width    := 200;
     Height   := 23;
     Show;
     Caption  := 'Pen style';
   end;

btnPenColor   := TBitBtn.Create(Self); //PenColor select ColorDialog
   with btnPenColor do
   begin
     OnClick  := @ColorSelect;
     Parent   := BenchForm[5];
     Top      := 263;
     Left     := 210;
     Width    := 45;
     Height   := 21;
     Kind     := bkCustom;
     Caption  := 'Pen';
     Hint     := 'Pen Color';
     ShowHint := True;
     Visible  := True;
   end;

btnBrushColor := TBitBtn.Create(Self); //BrushColor select ColorDialog
   with btnBrushColor do
   begin
     OnClick  := @ColorSelect;
     Parent   := BenchForm[5];
     Top      := 263;
     Left     := 260;
     Width    := 45;
     Height   := 21;
     Kind     := bkCustom;
     Caption  := 'Brush';
     Hint     := 'Brush Color';
     ShowHint := True;
     Visible  := True;
   end;

btnGrad       := TBitBtn.Create(Self); //Gradient Canvas
   with btnGrad do
   begin
     OnClick  := @GradCanvas;
     Parent   := BenchForm[5];
     Top      := 263;
     Left     := 310;
     Width    := 45;
     Height   := 21;
     Kind     := bkCustom;
     Caption  := 'Grad';
     Hint     := 'Gradient Canvas';
     ShowHint := True;
     Visible  := True;
   end;

btnCanvasClear := TBitBtn.Create(Self); //Clear Canvas
   with btnCanvasClear do
   begin
     OnClick   := @ClearCanvas;
     Parent    := BenchForm[5];
     Top       := 5;  
     Left      := 345;
     Width     := 45;
     Height    := 21;
     Kind      := bkCustom;
     Caption   := 'Clear';
     Hint      := 'Clear the Canvas';
     ShowHint  := True;
     Visible   := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TCheckBox Tools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblPick := Tlabel.Create(Self); //Label Pick a number
   With lblPick do
   begin
     Parent := BenchForm[6];
     Top := 3;
     Left := 25;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'Pick a number !';
   end;
lblCount := Tlabel.Create(Self); //Label Count picks
   With lblCount do
   begin
     Parent := BenchForm[6];
     Top := 165;
     Left := 25;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'Counter: '+IntToStr(CHCount);
   end;
btnAgain := TBitBtn.Create(Self);
   with btnAgain do
   begin
     OnClick := @AgainClick;
     Parent := BenchForm[6];
     Top := 3;
     Left:= 150;
     Width := 45;
     Height := 20;
     Kind := bkCustom;
     Caption := 'Again';
     Hint := 'Play one more time !';
     ShowHint := True;
     Visible := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TClipboard Tools +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD Clipboard CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TCListBox Tools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD CListBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TColorDialog Tool ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnColorSelect := TBitBtn.Create(Self);
   with btnColorSelect do
   begin
     OnClick := @SelectedColor;
     Parent := BenchForm[9];
     Top := 10;
     Left:= 10;
     Width := 45;
     Height := 20;
     Kind := bkCustom;
     Caption := 'Color';
     Hint := 'Color Dialog (Some repaint problem, move the form to se the effect !!)';
     ShowHint := True;
     Visible := True;
   end;
lblColorInt := Tlabel.Create(Self); //Label The Color in Integer;
   With lblColorInt do
   begin
     Parent := BenchForm[9];
     Top := 8;
     Left := 65;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'Integer Color: ';
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TComboBox Tools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnCbbAdd := TButton.Create(Self);
   with btnCbbAdd do
   begin
     OnClick := @AddToCbb;
     Height := 20;
     Width := 55;
     Parent := BenchForm[10];
     Top := 16;
     Left:= 178;
     Cursor := crDefault;
     Enabled := True;
     Hint := 'Add this to ComboBox1';
     ShowHint := True;
     Caption := 'Add >';
     Name := 'btnCbbAdd';
     PopupMenu := PopupMenu1;
     Visible := True;
   end;
edtCbbAdd := TEdit.Create(Self);
   with edtCbbAdd do
   begin
     Parent := BenchForm[10];
     Caption := '';
     Width := 145;
     Height := 20;
     Left := 240;
     Top := 16;
     Show;
   end;
btnCbbRemove := TBitBtn.Create(Self);
   with btnCbbRemove do
   begin
     OnClick := @RemoveCbb;
     Parent := BenchForm[10];
     Top := 45;
     Left:= 178;
     Width := 55;
     Height := 20;
     Caption := 'Remove';
     Visible := True;
   end;
lblRemoved := Tlabel.Create(Self); //Label ComboBox Removed
   With lblRemoved do
   begin
     Parent := BenchForm[10];
     Top := 275;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := ' ';
   end;
lblIndex := Tlabel.Create(Self); //Label ItemIndex
   With lblIndex do
   begin
     Parent := BenchForm[10];
     Top := 255;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'ItemIndex: ';
   end;
lblSelLength := Tlabel.Create(Self); //Label ItemIndex
   With lblSelLength do
   begin
     Parent := BenchForm[10];
     Top := 235;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'SelLength: ';
   end;
lblSelStart := Tlabel.Create(Self); //Label ItemIndex
   With lblSelStart do
   begin
     Parent := BenchForm[10];
     Top := 215;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'SelStart: ';
   end;
lblSelText := Tlabel.Create(Self); //Label ItemIndex
   With lblSelText do
   begin
     Parent := BenchForm[10];
     Top := 195;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'SelText: ';
   end;
cbbMoveTo := TComboBox.Create(Self);
   with cbbMoveTo do
   begin
     OnChange := @EventOnChange;
     Parent := BenchForm[10];
     Top := 70;
     Left := 10;
     Width := 160;
     Height := 28;
     Show;
   end;
btnMoveTo := TBitBtn.Create(Self);
   with btnMoveTo do
   begin
     OnClick := @MoveToCbb;
     Parent := BenchForm[10];
     Top := 45;
     Left:= 10;
     Width := 130;
     Height := 20;
     Caption := 'Copy Sel to Combo2';
     Visible := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TDirectoryListBox Tools ++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD DirectoryListBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TDrawGrid Tools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD DrawGrid CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TDriveComboBox Tools +++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD DriveComboBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TEdit Tools ++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TFileListBox Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD FileListBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TFilterComboBox Tools ++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD FilterComboBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TFindDialog Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD FindDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TFontDialog Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnShowFont          := TBitBtn.Create(Self); //FontDialog show button
   with btnShowFont do
   begin
     OnClick        := @SelectFont;
     Parent         := BenchForm[18];
     Top            := 10;
     Left           := 10;
     Width          := 50;
     Height         := 21;
     Kind           := bkCustom;
     Caption        := 'Select';
     Hint           := 'Select Font';
     ShowHint       := True;
     Visible        := True;
   end;
lblWhatFont          := TLabel.Create(Self); //Label showing Application.ExeName
   With lblWhatFont do
   begin
     Parent         := BenchForm[18];
     Top            := 265;
     Left           := 10;
     Width          := 350;
     Height         := 23;
     Show;
     Caption        := 'Font.Name:';
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TForm Tools ++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblWidth := Tlabel.Create(Self); //Label Form.Width
   With lblWidth do
   begin
     Parent := BenchForm[19];
     Top := 10;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'Width: '+IntToStr(BenchForm[19].Width);
   end; 
lblHeight := Tlabel.Create(Self); //Label Form.Heigt
   With lblHeight do
   begin
     Parent := BenchForm[19];
     Top := 30;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'Height: '+IntToStr(BenchForm[19].Height);
   end; 
lblCWidth := Tlabel.Create(Self); //Label Form.Width
   With lblCWidth do
   begin
     Parent := BenchForm[19];
     Top := 50;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'ClientWidth: '+IntToStr(BenchForm[19].ClientWidth);
   end; 
lblCHeight := Tlabel.Create(Self); //Label Form.Heigt
   With lblCHeight do
   begin
     Parent := BenchForm[19];
     Top := 70;
     Left := 10;
     Width := 200;
     Height := 20;
     Show;
     Caption := 'ClientHeight: '+IntToStr(BenchForm[19].ClientHeight);
   end;
cbbBorder := TComboBox.Create(Self);
   with cbbBorder do
   begin
     OnChange := @FormBorder;
     Parent := BenchForm[19];
     Top := 260;
     Left := 10;
     Width := 106;
     Height := 28;
     Items.Add('bsNone');
     Items.Add('bsSingle');
     Items.Add('bsDialog');
     Items.Add('bsToolWindow');
     Items.Add('bsSizeable');
     Show;
   end;
cbbPosition := TComboBox.Create(Self);
   with cbbPosition do
   begin
     OnChange := @FormPosition;
     Parent := BenchForm[19];
     Top := 260;
     Left := 126;
     Width := 140;
     Height := 28;
     Items.Add('poOwnerFormCenter');
     Items.Add('poMainFormCenter');
     Items.Add('poDesktopCenter');
     Items.Add('poDefaultSizeOnly');
     Items.Add('poDefaultPosOnly');
     Items.Add('poDefault');
     Items.Add('poDesigned');
     Items.Add('poScreenCenter');
     Show;
   end;
btnKeyDown := TBitBtn.Create(Self); //"keyDown" button
   with btnKeyDown do
   begin
     Parent := BenchForm[19];
     Top := 10;
     Left:= 325;
     Width := 65;
     Height := 21;
     Kind := bkCustom;
     Caption := 'KeyDown';
     Hint := 'For KeyDown to work';
     ShowHint := True;
     Visible := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TGroupBox Tools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TImage Tools +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD Image CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++++ TIniFile Tools +++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
If IniFile1.ReadString('INFORMATION','Project','NODATA')='NODATA' then
begin
With IniFile1 do
  begin
    WriteString('INFORMATION','Project','Lazarus');
    WriteFloat('INFORMATION','Version',0.5);
    WriteInteger('INFORMATION','Integer',1234567);
    WriteDate('INFORMATION','Today',Now);
    WriteTime('INFORMATION','AppStart',Now);
  end;
end;

edtString := TEdit.Create(Self);
   with edtString do
   begin
     Parent := BenchForm[22];
     Caption := Inifile1.ReadString('INFORMATION','Project','NODATA');
     Width := 150;
     Height := 20;
     Left := 50;
     Top := 10;
     Show;
   end;

lblString           := TLabel.Create(Self);
   With lblString do
   begin
     Parent         := BenchForm[22];
     Top            := 10;
     Left           := 5;
     Width          := 200;
     Height         := 17;
     Show;
     Caption        := 'String';
   end;

edtFloat := TEdit.Create(Self);
   with edtFloat do
   begin
     Parent := BenchForm[22];
     Caption := Inifile1.ReadString('INFORMATION','Version','NODATA');
     Width := 150;
     Height := 20;
     Left := 50;
     Top := 50;
     Show;
   end;

lblFloat           := TLabel.Create(Self);
   With lblFloat do
   begin
     Parent         := BenchForm[22];
     Top            := 50;
     Left           := 5;
     Width          := 200;
     Height         := 17;
     Show;
     Caption        := 'Float';
   end;

edtInteger := TEdit.Create(Self);
   with edtInteger do
   begin
     Parent := BenchForm[22];
     Caption := Inifile1.ReadString('INFORMATION','Integer','NODATA');
     Width := 150;
     Height := 20;
     Left := 50;
     Top := 90;
     Show;
   end;

lblInteger          := TLabel.Create(Self);
   With lblInteger do
   begin
     Parent         := BenchForm[22];
     Top            := 90;
     Left           := 5;
     Width          := 200;
     Height         := 17;
     Show;
     Caption        := 'Integer';
   end;

edtDate := TEdit.Create(Self);
   with edtDate do
   begin
     Parent := BenchForm[22];
     Caption := Inifile1.ReadString('INFORMATION','Today','NODATA');
     Width := 150;
     Height := 20;
     Left := 50;
     Top := 130;
     Show;
   end;

lblDate            := TLabel.Create(Self);
   With lblDate do
   begin
     Parent         := BenchForm[22];
     Top            := 130;
     Left           := 5;
     Width          := 200;
     Height         := 17;
     Show;
     Caption        := 'Date';
   end;

edtTime := TEdit.Create(Self);
   with edtTime do
   begin
     Parent := BenchForm[22];
     Caption := Inifile1.ReadString('INFORMATION','Appstart','NODATA');
     Width := 150;
     Height := 20;
     Left := 50;
     Top := 170;
     Show;
   end;

lblTime            := TLabel.Create(Self);
   With lblTime do
   begin
     Parent         := BenchForm[22];
     Top            := 170;
     Left           := 5;
     Width          := 200;
     Height         := 17;
     Show;
     Caption        := 'Time';
   end;

btnUpdate := TBitBtn.Create(Self); //"Update" button
   with btnUpdate do
   begin
     OnClick := @IniUpdate;
     Parent := BenchForm[22];
     Top := 200;
     Left:= 50;
     Width := 55;
     Height := 21;
     Kind := bkCustom;
     Caption := 'Update';
     Hint := 'Adding your data';
     ShowHint := True;
     Visible := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TLabel ++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cbbAlignment  := TComboBox.Create(Self);
   with cbbAlignment do
   begin
     OnChange := @SelectAlignment;
     Parent   := BenchForm[23];
     Top      := 10;
     Left     := 170;
     Width    := 105;
     Height   := 28;
     Items.Add('taRightJustify');
     Items.Add('taCenter');
     Items.Add('taLeftJustify');
     Show;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TListBox ++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnRight := TBitBtn.Create(Self); //Button >>>>>
   with btnRight do
   begin
     OnClick := @ListBoxMove;
     Parent := BenchForm[24];
     Top := 60;
     Left:= 173;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := '   >> ';
     Hint := 'Move from ListBox1 to ListBox2';
     ShowHint := True;
     Visible := True;
   end;
btnLeft := TBitBtn.Create(Self); //Button <<<<<
   with btnLeft do
   begin
     OnClick := @ListBoxMove;
     Parent := BenchForm[24];
     Top := 100;
     Left:= 173;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := '   << ';
     Hint := 'Move from ListBox2 to ListBox1';
     ShowHint := True;
     Visible := True;
   end;
btnAdd := TBitBtn.Create(Self); //Button Add
   with btnAdd do
   begin
     OnClick := @ListBoxMove;
     Parent := BenchForm[24];
     Top := 10;
     Left:= 173;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := '  Add ';
     Hint := '^   Add to the ListBox   ^';
     ShowHint := True;
     Visible := True;
   end;
btnCopyToMem := TBitBtn.Create(Self); //Button Add
   with btnCopyToMem do
   begin
     OnClick := @CopyToMem;
     Parent := BenchForm[24];
     Top := 200;
     Left:= 173;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := ' Copy ';
     Hint := 'Copy ListBox1 to Memo';
     ShowHint := True;
     Visible := True;
   end;
btnSaveFile := TBitBtn.Create(Self); //Button Add
   with btnSaveFile do
   begin
     OnClick := @SaveFile;
     Parent := BenchForm[24];
     Top := 162;
     Left:= 173;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := ' Save ';
     Hint := 'Save ListBox1 to file ./List.txt';
     ShowHint := True;
     Visible := True;
   end;
memAddTo := TMemo.Create(Self);
   with memAddTo do
   begin
     Parent := BenchForm[24];
     Top := 200;
     Left := 10;
     Width := 150;
     Height := 85;
     ScrollBars	   := ssBoth;
     Lines.Text := ' ';
     Show;
   end;
edtAddLis1 := TEdit.Create(Self);
   with edtAddLis1 do
   begin
     Parent := BenchForm[24];
     Caption := '';
     Width := 150;
     Height := 20;
     Left := 10;
     Top := 10;
     Show;
   end;
edtAddLis2 := TEdit.Create(Self);
   with edtAddLis2 do
   begin
     Parent := BenchForm[24];
     Caption := '';
     Width := 150;
     Height := 20;
     Left := 235;
     Top := 10;
     Show;
   end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TMaskEdit +++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD MaskEdit CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TMediaPlayer ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD MediaPlayer CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TMemo Tools +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
btnNew := TBitBtn.Create(Self); //Button New
   with btnNew do
   begin
     OnClick := @NewMemo;
     Parent := BenchForm[27];
     Top := 260;
     Left:= 19;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := '  New ';
     Hint := 'New Memo document';
     ShowHint := True;
     Visible := True;
   end;
btnSave := TBitBtn.Create(Self); //Button Save
   with btnSave do
   begin
     OnClick := @SaveMemoAs;
     Parent := BenchForm[27];
     Top := 260;
     Left:= 70;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := ' Save ';
     Hint := 'Save Memo as...';
     ShowHint := True;
     Visible := True;
   end;
btnOpen := TBitBtn.Create(Self); //Button Open
   with btnOpen do
   begin
     OnClick := @OpenMemo;
     Parent := BenchForm[27];
     Top := 260;
     Left:= 121;
     Width := 50;
     Height := 21;
     Kind := bkCustom;
     Caption := ' Open ';
     Hint := 'Open File...';
     ShowHint := True;
     Visible := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TMessageDialog Tools ++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD MessageDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TNoteBook Tools +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD NoteBook CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TOpenDialog Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPageControl Tools ++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD PageControl CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPaintBox Tools +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   pntCount := 0;
//             ADD PaintBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPanel Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  rgPanelOutBevel := TRadioGroup.Create (self);
  with rgPanelOutBevel do
  begin
     Parent := Panel2;
     OnClick := @PanelOutBevelClick;
     Top    := 10;
     Left   := 250;
     Height := 100;
     Width  := 100;
     Items.Add ('lowered');
     Items.Add ('raised');
     Items.Add ('none');
     Items.Add ('(!)space');
     Hint := 'Change style (lowered/raised..)';
     ShowHint := True;
     ItemIndex := 1;
     Show;
     Caption := 'Outer-Bevel';
  End;
  rgPanelInBevel := TRadioGroup.Create (self);
  with rgPanelInBevel do
  begin
     Parent := Panel2;
     OnClick := @PanelInBevelClick;
     Top    := 10;
     Left   := 140;
     Height := 100;
     Width  := 100;
     Items.Add ('lowered');
     Items.Add ('raised');
     Items.Add ('none');
     Items.Add ('(!)space');
     Hint := 'Change style (lowered/raised..)';
     ShowHint := True;
     ItemIndex := 2;
     Show;
     Caption := 'Inner-Bevel';
  End;
  rgPanelAlign := TRadioGroup.Create (self);
  with rgPanelAlign do
  begin
     Parent := Panel2;
     OnClick := @PanelAlignClick;
     top    := 10;
     left   := 10;
     Height := 130;
     Width  := 120;
     Items.Add ('alNone');
     Items.Add ('alBottom');
     Items.Add ('alClient');
     Items.Add ('alLeft');
     Items.Add ('alRight');
     Items.Add ('alTop');
     ItemIndex := 0;
     Show;
     Caption := 'Alignment';
  end;
  btPanelNorm := TButton.Create(self);
  with btPanelNorm do
  begin
     Parent := Panel2;
     OnClick:= @PanelNormClick; 
     top    := 10;
     left   := 360;
     Height := 30;
     Width  := 50;
     Show;
     Caption := 'Norm';
  end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPopupMenu Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD PopupMenu CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPrintDialog Tools ++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD PrinDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TPrinterSetupDialog Tools +++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD PrinterSetupDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TProgressBar Tools ++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ProgTime := TTimer.Create(Self);
  with ProgTime do
  begin
    OnTimer := @UpdateProgressBar;
    Enabled := False;
    Interval := 50;
    Name := 'ProgTime';
  end;
chbProgEnable := TCheckBox.Create(Self);
  with chbProgEnable do
  begin
     OnClick := @ProgSettings;
     Parent := BenchForm[37];
     Top := 28;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Run';
     Cursor := crHandPoint;
     Hint := 'Run the ProgressBar';
     ShowHint := True;
     Visible := True;
  End;
chbProgOrient := TCheckBox.Create(Self);
  with chbProgOrient do
  begin
     OnClick := @ProgSettings;
     Parent := BenchForm[37];
     Top := 48;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Vertical';
     Cursor := crHandPoint;
     Hint := 'Change orientation';
     ShowHint := True;
     Visible := True;
  End;
chbProgDirect := TCheckBox.Create(Self);
  with chbProgDirect do
  begin
     OnClick := @ProgSettings;
     Parent := BenchForm[37];
     Top := 68;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Direction';
     Cursor := crHandPoint;
     Hint := 'Change direction';
     ShowHint := True;
     Visible := True;
  End;
chbProgText := TCheckBox.Create(Self);
  with chbProgText do
  begin
     OnClick := @ProgSettings;
     Parent := BenchForm[37];
     Top := 88;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Text';
     Cursor := crHandPoint;
     Hint := 'Show progress text';
     ShowHint := True;
     Visible := True;
  End;
chbSmooth := TCheckBox.Create(Self);
  with chbSmooth do
  begin
     OnClick := @ProgSettings;
     Parent := BenchForm[37];
     Top := 108;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Smooth';
     Cursor := crHandPoint;
     Hint := 'Show progress NOT smooth';
     ShowHint := True;
     Visible := True;
  End;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TRadioButton Tools ++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD RadioButton CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TRadioGroup Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD RadioGroup CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TReplaceDialog Tools ++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD ReplaceDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TRichEdit Tools +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD RichEdit CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TSaveDialog Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD SaveDialog CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TScreen Tools +++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblScreenWidth      := TLabel.Create(Self); //Label showing Screen.Width
   With lblScreenWidth do
   begin
     Parent         := BenchForm[43];
     Top            := 10;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'ScreenWidth: '+IntToStr(Screen.Width);
   end;
lblScreenHeight     := TLabel.Create(Self); //Label showing Screen.Height
   With lblScreenHeight do
   begin
     Parent         := BenchForm[43];
     Top            := 35;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'ScreenHeight: '+IntToStr(Screen.Height);
   end;
lblPixels           := TLabel.Create(Self); //Label showing Screen.PixelsPerInch
   With lblPixels do
   begin
     Parent         := BenchForm[43];
     Top            := 60;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'PixelsPerInch: '+IntToStr(Screen.PixelsPerInch);
   end;
lblFormCount        := TLabel.Create(Self); //Label showing Screen.PixelsPerInch
   With lblFormCount do
   begin
     Parent         := BenchForm[43];
     Top            := 85;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'FormCount: '+IntToStr(Screen.FormCount);
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TScrollBar Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD ScrollBar CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TScrollBox Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD ScrollBox CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TShape Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD Shape CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TSpeedButton Tools ++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FLeft := 10;
For i:= 0 to 8 do
begin
  
  Pixmap1:=TPixMap.Create;
  Pixmap1.TransparentColor:=clBtnFace;
  if not LoadResource(FileN[i],Pixmap1) then
  begin
    LoadResource('default',Pixmap1);
  end;

  SpeedB[i]     := TSpeedButton.Create(Self);
  with SpeedB[i] do
  begin
    Parent      := BenchForm[47];
    OnClick     := @EventOnClick;
    OnMouseDown := @EventOnMouseDown;
    OnMouseUp   := @EventOnMouseUp;
    Left        := SpeedButton1.Left + (i * 23);
    Top         := 20;
    if (i > 4) then
    begin
       Flat := true;
       Hint := FileN [i] + '(flat button)';
    end
    else begin
       Flat := false;
       Hint := FileN [i] + '(normal buton)';
    end; 
    Color       := clBtnFace;
    Glyph.Assign(Pixmap1);
    Enabled     := True;
    Visible     := True;
    ShowHint    := true;
  end;
  FreeAndNil(Pixmap1);
end;
    
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TSpinButton Tools +++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD SpinButton CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TSpinEdit Tools +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblOnChange         := TLabel.Create(Self); //Label showing Screen.Width
   With lblOnChange do
   begin
     Parent         := BenchForm[49];
     Top            := 50;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'Value: ';//+IntToStr(SpinEdit1.Value);
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TStatusBar Tools ++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD StatusBar CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TStringGrid +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD StringGrid CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TTabbedNoteBook +++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD TabbedNoteBook CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TTabControl +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD TabControl CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++ TThread +++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD Thread CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ TTimer Tools +++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblTimeNow     := TLabel.Create(Self); //Label showing TimeNow
   With lblTimeNow do
   begin
     Parent         := BenchForm[55];
     Top            := 10;
     Left           := 10;
     Width          := 200;
     Height         := 23;
     Show;
     Caption        := 'TimeNow: '+TimeToStr(Now);
   end;
lblHourMinSec       := TLabel.Create(Self); //Label showing Hour Min Sec
   With lblHourMinSec  do
   begin
     Parent         := BenchForm[55];
     Top            := 35;
     Left           := 10;
     Width          := 85;
     Height         := 23;
     Show;
     Caption        := 'Track: 00:00:00';
   end;
lblMSec             := TLabel.Create(Self); //Label showing Milli Seconds
   With lblMSec  do
   begin
     Parent         := BenchForm[55];
     Top            := 35;
     Left           := 96;
     Width          := 20;
     Height         := 23;
     Show;
     Caption        := ':00';
   end;
btnStart            := TBitBtn.Create(Self); //Start Timetrack button
   with btnStart do
   begin
     OnClick        := @TimeTrack;
     Parent         := BenchForm[55];
     Top            := 65;
     Left           := 10;
     Width          := 40;
     Height         := 21;
     Kind           := bkCustom;
     Caption        := 'Start';
     Hint           := 'Start/Stop timetrack';
     ShowHint       := True;
     Visible        := True;
   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ TToggleBox Tools +++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
lblChecked := TLabel.Create(Self);
  with lblChecked do
  begin
    Parent := BenchForm[56];
    Left := 10;
    Top := 275;
    Width := 100;
    Height := 20;
    Caption := 'Checked= ';
    Show;
  end;
lblState := TLabel.Create(Self);
  with lblState do
  begin
    Parent := BenchForm[56];
    Left := 10;
    Top := 255;
    Width := 120;
    Height := 20;
    Caption := 'State= ';
    Show;
  end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ TToolBar Tools +++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD ToolBar CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ TTrackBar Tools ++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD TrackBar CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ TUpDown Tools ++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//             ADD UpDown CompTools HERE !!!!!!!!!
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++ Universal TestTools ++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
AboutForm := TForm.Create(Self);
  With AboutForm do
  begin
    OnClose := @TestFormClose;//+++++ ABOUT +++++++++++++
    Height := 300;            //+++++++++++++++++++++++++
    Width := 420;
    Caption := 'About';
    BorderStyle := bsDialog;
    Left := Screen.Width div 2 - (AboutForm.Width div 2);
    Top := 10;
  end;
//++++++++++++++++++++++++++++++ EVENT SNOOPER +++++++++
EventForm := TForm.Create(Self);
    With EventForm do
    Begin
      Height := 294;
      Width := 255;
      Caption := 'Events';
      BorderStyle := bsDialog;
      Left := 45;
      Top := 100;
      Show;
    end;
lbEvents := TListBox.Create(Self);  // The Event ListBox
  with lbEvents do
  begin
    Parent := EventForm;
    Align := alClient;
    Show;
  end;
//++++++++++++++++++++++++++++++++ About Comps ++++++++++
gbAbout := TGroupBox.Create(Self);
  with gbAbout do
  begin
    Parent := AboutForm;
    Left := 5;
    Top := 5;
    Width := 407;
    Height := 160;
  //Ctl3D := False;
    Caption := ' About this application ';
    Visible := True;
  end;
btnBigOk := TBitBtn.Create(Self); //Button Ok
   with btnBigOk do
   begin
     OnClick := @CloseAbout;
     Parent := AboutForm;
     Top := 172;
     Left:= 5;
     Width :=406;
     Height := 117;
     Kind := bkCustom;
     Caption := '                                             OK';
     Hint := 'Well....ehem... OK !';
     ShowHint := True;
     Visible := True;
   end;
lblAbout := TMemo.Create(Self);
  with lblAbout do
  begin
    Parent := AboutForm;
    Width := 386;
    Height := 80;
    Left := 15;
    Top := 30;
    Caption := 'This program is free software; you can redistribute it and/or modify it'+
               ' under the terms of the GNU General Public License as published '+
               'by the Free Software Foundation; either version 2 of the License, or'+#13#10+
               '(at your option) any later version.';
    ReadOnly := True;
    Visible := True;
    WordWrap := True;
   end;
lblAuthor := TMemo.Create(Self);
  with lblAuthor do
  begin
    Parent := AboutForm;
    Width := 386;
    Height := 20;
    Left := 15;
    Top := 130;
    Caption := 'Key-Pusher: christer.t.johansson@se.abb.com  "Chris"';
    ReadOnly := True;
    Visible := True;
    WordWrap := True;
   end;
END;
//++++++++++++++++++++++++++++++ Create TestTools END +++++++++++++++++++++++++++++++
//***********************************************************************************
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//                              TApplication Procedures
//...................................................................................
procedure TForm1.BoxStyle(Sender : TObject);
Begin
  If Sender=rdbOk then
  begin
    FBoxStyle := 0;
    lbEvents.Items.Add('MessageBox.[OK]');
    Assert(False, 'Trace: MessageBox.[OK]');
  End Else if Sender = rdbOkCancel then
  begin
    FBoxStyle := 1;
    lbEvents.Items.Add('MessageBox.[OK,Cancel]');
    Assert(False, 'Trace: MessageBox.[OK,Cancel]');
  End Else if Sender = rdbAbortRetryIgnore then
  begin
    FBoxStyle := 2;
    lbEvents.Items.Add('MessageBox.[Abort,Retry,Ignore]');
    Assert(False, 'Trace: MessageBox.[Abort,Retry,Ignore]');
  End Else if Sender = rdbYesNoCancel then
  begin
    FBoxStyle := 3;
    lbEvents.Items.Add('MessageBox.[Yes,No,Cancel]');
    Assert(False, 'Trace: MessageBox.[Yes,No,Cancel]');
  End Else if Sender = rdbYesNo then
  begin
    FBoxStyle := 4;
    lbEvents.Items.Add('MessageBox.[Yes,No]');
    Assert(False, 'Trace: MessageBox.[Yes,No]');
  End Else If Sender = rdbRetryCancel then
  begin
    FBoxStyle := 5;
    lbEvents.Items.Add('MessageBox.[Retry,Cancel]');
    Assert(False, 'Trace: MessageBox.[Retry,Cancel]');
  End; 
  EventWatch;
end;
procedure TForm1.ShowBox(Sender : TObject);
begin
  Assert(False, 'Trace: >>>>>> MessageBoxShow');
  Assert(False, 'Trace: ^^^^^^^^^^^^^^^^^^^^^');
  Case Application.MessageBox('     User information.............     ','MessageBox',FBoxStyle) of
    1: lbEvents.Items.Add('MessageBoxClick.<OK>');
    2: lbEvents.Items.Add('MessageBoxClick.<Cancel>');
    3: lbEvents.Items.Add('MessageBoxClick.<Abort>');
    4: lbEvents.Items.Add('MessageBoxClick.<Retry>');
    5: lbEvents.Items.Add('MessageBoxClick.<Ignore>');
    6: lbEvents.Items.Add('MessageBoxClick.<Yes>');
    7: lbEvents.Items.Add('MessageBoxClick.<No>');
  End;
  FFocusControl:=nil;
  EventWatch;
end;
//++++++++++++++++++++++++++++++++++ TBitBtn ++++++++++++++++++++++++++++++++++++++++
procedure TForm1.KindComboChange(Sender : TObject);
begin
  If cbbKind.Text='bkOK' then
  begin
    BitBtn1.Kind := bkOK;
    lbEvents.Items.Add('BitBtn1.Kind:=bkOk');
  end else if cbbKind.Text='bkCancel' then
  begin
    BitBtn1.Kind := bkCancel;
    lbEvents.Items.Add('BitBtn1.Kind:=bkCancel');
  end else if cbbKind.Text='bkHelp' then
  begin
    BitBtn1.Kind := bkHelp;
    lbEvents.Items.Add('BitBtn1.Kind:=bkHelp');
  end else if cbbKind.Text='bkClose' then
  begin
    BitBtn1.Kind := bkClose;
    lbEvents.Items.Add('BitBtn1.Kind:=bkClose');
  end else if cbbKind.Text='bkAll' then
  begin
    BitBtn1.Kind := bkAll;
    lbEvents.Items.Add('BitBtn1.Kind:=bkAll');
  end else if cbbKind.Text='bkCustom' then
  begin
    BitBtn1.Kind := bkCustom;
    lbEvents.Items.Add('BitBtn1.Kind:=bkCustom');
  end;
  EventWatch;
end;
procedure TForm1.AlignComboChange(Sender : TObject);
begin
  If cbbAlign.Text='blGlyphLeft' then
  begin
    BitBtn1.Layout := blGlyphLeft;
    lbEvents.Items.Add('BitBtn1.Layout:=blGlyphLeft');
  end else If cbbAlign.Text='blGlyphTop' then
  begin
    BitBtn1.Layout := blGlyphTop;
    lbEvents.Items.Add('BitBtn1.Layout:=blGlyphTop');
  end else If cbbAlign.Text='blGlyphBottom' then
  begin
    BitBtn1.Layout := blGlyphBottom;
    lbEvents.Items.Add('BitBtn1.Layout:=blGlyphBottom');
  end else If cbbAlign.Text='blGlyphRight' then
  begin
    BitBtn1.Layout := blGlyphRight;
    lbEvents.Items.Add('BitBtn1.Layout:=blGlyphRight');
  end;
  EventWatch;
end;
procedure TForm1.LoadGlyph(Sender : TObject);// This thing don't work
var
FileName : String;
begin
  lbEvents.Items.Add('TBITBTN.OpenDialog');
  EventWatch;
  lbEvents.Items.Add('Select a 16X16 .xpm file');
  EventWatch;

  //OpenDialog1 := TOpenDialog.Create(Self);
  If OpenDialog1.Execute then
    FileName := OpenDialog1.FileName;

  if FileName <> '' then
  begin
    lbEvents.Items.Add(FileName+' selected');
    EventWatch;

    S := TFileStream.Create(UTF8ToSys(FileName), fmOpenRead);
    try
      PixMap1 := TPixMap.Create;
      Pixmap1.TransparentColor := clBtnFace;
      PixMap1.LoadFromStream(S);
    finally
      S.Free;
    end;
    BitBtn1.Glyph.Assign(PixMap1);
    FreeAndNil(PixMap1);
  end;

  lbEvents.Items.Add('<<< WORKS WITH STREAM.PIXMAPS >>>');
  EventWatch;
end;
//++++++++++++++++++++++++++++++++ TButton ++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.Button1Click(Sender : TObject);
var
i : Integer;
begin
  for i := 0 to 15 do
    If Sender=Button1[i] then
    begin
      lbEvents.Items.Add('Button1['+IntToStr(i)+'].Click');
      ButtonTag(i);
    end;

  EventWatch;
end;

procedure TForm1.ButtonTag(Sender : Integer);
begin
  If Button1[Sender].Caption = ' ' then
  begin
    Button1[Sender].Caption := IntToStr(Random(100));
    Button1[Sender].Hint := 'Lotto# '+Button1[Sender].Caption;
    lbEvents.Items.Add('Button1['+IntToStr(Sender)+'].Random '+Button1[Sender].Caption);
    EventWatch;
    lblLResult.Caption := lblLResult.Caption+Button1[Sender].Caption;
    Inc(LCount);
    If LCount<16 then
      lblLResult.Caption := lblLResult.Caption+','
    Else If LCount=16 then
      lblLResult.Caption := lblLResult.Caption+'.'
  End;
end;
procedure TForm1.LottoClear(Sender : TObject);
var
i : Integer;
begin
   For i := 0 to 15 do
   Begin
     Button1[i].Caption := ' ';
     lblLResult.Caption := 'Result: ';
     LCount := 0;
   End;
end;
//++++++++++++++++++++++++++++++++ TCanvas ++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.DrawRect(X1,Y1,X2,Y2 : Integer);
begin
    BenchForm[5].Canvas.Rectangle(X1,Y1,X2,Y2);
end;

procedure TForm1.StyleSelect(Sender : TObject);
begin
  If cbbBrushStyle.Text = 'bsBDiagonal' then
    BenchForm[5].Canvas.Brush.Style := bsBDiagonal
  Else If cbbBrushStyle.Text = 'bsClear' then
    BenchForm[5].Canvas.Brush.Style := bsClear
  Else If cbbBrushStyle.Text = 'bsCross' then
    BenchForm[5].Canvas.Brush.Style := bsCross
  Else If cbbBrushStyle.Text = 'bsDiagCross' then
    BenchForm[5].Canvas.Brush.Style := bsDiagCross
  Else If cbbBrushStyle.Text = 'bsFDiagonal' then
    BenchForm[5].Canvas.Brush.Style := bsFDiagonal
  Else If cbbBrushStyle.Text = 'bsHorizontal' then
    BenchForm[5].Canvas.Brush.Style := bsHorizontal
  Else If cbbBrushStyle.Text = 'bsVertical' then
    BenchForm[5].Canvas.Brush.Style := bsVertical
  Else If cbbBrushStyle.Text = 'bsSolid' then
    BenchForm[5].Canvas.Brush.Style := bsSolid;

    Assert(False, 'Trace:CANVAS.BRUSH.STYLE='+cbbBrushStyle.Text);
end;
procedure TForm1.ClearCanvas(Sender : TObject);
begin
  BenchForm[5].Close;
  BenchForm[5].Show;
end;

procedure TForm1.GradCanvas(Sender : TObject); //Gradient Canvas
var
FColor, i : Integer;
begin
  FColor := clBlack;
  For i := 0 to 300 do
  Begin
    BenchForm[5].Canvas.Pen.Color := FColor;
    BenchForm[5].Canvas.LineTo(400,i);
    BenchForm[5].Canvas.MoveTo(0,i);
    If i > 40 then
      FColor := FColor + 1;
  End;
end;
procedure TForm1.PenStyleSelect(Sender : TObject);
begin
  If cbbPenStyle.Text = 'psClear' then
    BenchForm[5].Canvas.Pen.Style := psClear
  Else If cbbPenStyle.Text = 'psDash' then
    BenchForm[5].Canvas.Pen.Style := psDash
  Else If cbbPenStyle.Text = 'psDashDot' then
    BenchForm[5].Canvas.Pen.Style := psDashDot
  Else If cbbPenStyle.Text = 'psDashDotDot' then
    BenchForm[5].Canvas.Pen.Style := psDashDotDot
  Else If cbbPenStyle.Text = 'psDot' then
    BenchForm[5].Canvas.Pen.Style := psDot
  Else If cbbPenStyle.Text = 'psSolid' then
    BenchForm[5].Canvas.Pen.Style := psSolid;

    Assert(False, 'Trace:CANVAS.PEN.STYLE='+cbbPenStyle.Text);
end;
//++++++++++++++++++++++++++++++++ TCheckBox ++++++++++++++++++++++++++++++++++++++++
procedure TForm1.CheckClick(Sender : TObject);
var
i : Integer;
begin
  For i:= 1 to 35 do
  begin
    If Sender=CheckBox1[i] then
    begin
      lbEvents.Items.Add('CheckBox1['+IntToStr(i)+'].Click');
      EventWatch;
      Assert(False, 'Trace: CheckBox1['+IntToStr(i)+']Clicked');
      If i = RandCh then
      begin
        lblPick.Caption := 'That´s the one !!';
        CheckBox1[i].Hint := 'That´s the one !!';
        lbEvents.Items.Add('VICTORY !!');
        EventWatch;
        Assert(False, 'Trace: VICTORY !!');
      End Else
      begin
        lblPick.Caption := 'Nop, try again !!';
        CheckBox1[i].Hint := 'Already picked';
        lbEvents.Items.Add('NOP !!');
        EventWatch;
        Assert(False, 'Trace: NOP !!');
      end;
      CHCount := CHCount + 1;
      lblCount.Caption := 'Counter: '+IntToStr(CHCount);
    end;
  end;
end;
procedure TForm1.AgainClick(Sender : Tobject);
var
i : Integer;
begin
  RandCH := 0;
  While RandCH < 1 do
    RandCH := Random(35);
  For i := 1 to 35 do
    If CheckBox1[i].Checked = True then
    begin
      CheckBox1[i].Checked:= not CheckBox1[i].Checked;
      CheckBox1[i].Hint := 'Maybe this one';
    end;

  CHCount := 0;
  lblCount.Caption := 'Counter: '+IntToStr(CHCount);
  lblPick.Caption := 'Better luck this time !!';
  lbEvents.Items.Add('Don´t look, but it´s #'+IntToStr(RandCH));
  EventWatch;
end;
//++++++++++++++++++++++++++++++++ TColorDialog +++++++++++++++++++++++++++++++++++++
procedure TForm1.ColorSelect(Sender : TObject);
begin
  If ColorDialog1.Execute then
    If Sender=btnPenColor then
    Begin
      BenchForm[5].Canvas.Pen.Color := ColorDialog1.Color;
      Assert(False, 'Trace: CANVAS.PEN.COLOR='+IntToStr(ColorDialog1.Color));
    End Else If Sender=btnBrushColor then
    Begin
      BenchForm[5].Canvas.Brush.Color := ColorDialog1.Color;
    End Else If Sender=btnColorSelect then
    Begin
      BenchForm[9].Canvas.Brush.Color := ColorDialog1.Color;
      BenchForm[9].Canvas.Pen.Color := ColorDialog1.Color;
      lblColorInt.Caption := 'Integer Color: '+IntToStr(ColorDialog1.Color);
    End; 
  lbEvents.Items.Add('TCOLOR.Selected='+IntToStr(ColorDialog1.Color));
  EventWatch;
End;

procedure TForm1.SelectedColor(Sender : TObject);
begin
  ColorSelect(Sender);
  BenchForm[9].Canvas.Rectangle(10,35,53,70);
End;
//++++++++++++++++++++++++++++++++ TCombpoBox +++++++++++++++++++++++++++++++++++++++
procedure TForm1.AddToCbb(Sender : TObject);
begin
  If edtCbbAdd.Text <> '' then
  begin
    ComboBox1.Items.Add(edtCbbAdd.Text);
    lbEvents.Items.Add('TCOMBOBOX.Items.Add('+edtCbbAdd.Text+')');
    EventWatch;
    Assert(False, 'Trace: TCOMBOBOX.Item.Add('+edtCbbAdd.Text+')');
    lblRemoved.Caption := edtCbbAdd.Text+' Added';
    edtCbbAdd.Text := '';
  end;
end;

procedure TForm1.RemoveCbb(Sender : TObject);
begin
  If ComboBox1.Items.Count>0 then
  Begin
    lblRemoved.Caption := ComboBox1.Items.Strings[ComboBox1.Items.Count-1]+' Removed';
    ComboBox1.Items.Delete(ComboBox1.Items.Count - 1);
  end else
    lblRemoved.Caption := ' ';
end;

procedure TForm1.MoveToCbb(Sender : TObject);
begin
  cbbMoveTo.Items.Add(ComboBox1.SelText);
end;
//++++++++++++++++++++++++++++++++ TFontDialog ++++++++++++++++++++++++++++++++++++++
procedure TForm1.SelectFont(Sender : TObject);
begin
  If FontDialog1.Execute then
  begin
    lblWhatFont.Caption := 'Font.Name: '+FontDialog1.Font.Name;
  end;
end;
//++++++++++++++++++++++++++++++++ TForm ++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.FormBorder(Sender : TObject);
begin
  If cbbBorder.Text='bsNone' then
    BenchForm[19].BorderStyle := bsNone
  else if cbbBorder.Text='bsSingle' then
    BenchForm[19].BorderStyle := bsSingle
  else if cbbBorder.Text='bsDialog' then
    BenchForm[19].BorderStyle := bsDialog
  else if cbbBorder.Text='bsToolWindow' then
    BenchForm[19].BorderStyle := bsToolWindow
  else if cbbBorder.Text='bsSizeable' then
    BenchForm[19].BorderStyle := bsSizeable;

end;
procedure TForm1.FormPosition(Sender : TObject);
begin
  If cbbPosition.Text='poDesigned' then
    BenchForm[19].Position := poDesigned
  else if cbbPosition.Text='poDefault' then
    BenchForm[19].Position := poDesigned
  else if cbbPosition.Text='poDefaultPosOnly' then
    BenchForm[19].Position := poDefaultPosOnly
  else if cbbPosition.Text='poDefaultSizeOnly' then
    BenchForm[19].Position := poDefaultSizeOnly
  else if cbbPosition.Text='poScreenCenter' then
    BenchForm[19].Position := poScreenCenter
  else if cbbPosition.Text='poDesktopCenter' then
    BenchForm[19].Position := poDesktopCenter
  else if cbbPosition.Text='poMainFormCenter' then
    BenchForm[19].Position := poMainFormCenter
  else if cbbPosition.Text='poOwnerFormCenter' then
    BenchForm[19].Position := poOwnerFormCenter;

end;
//++++++++++++++++++++++++++++++++ TIniFile +++++++++++++++++++++++++++++++++++++++++
procedure TForm1.IniUpdate(Sender : TObject);
begin
  with IniFile1 do
  begin
    WriteString('INFORMATION','Project',edtString.Text);
    WriteFloat('INFORMATION','Version',StrToFloat(edtFloat.Text));
    WriteInteger('INFORMATION','Integer',StrToInt(edtInteger.Text));
    WriteDate('INFORMATION','Today',StrToDate(edtDate.Text));
    WriteTime('INFORMATION','Appstart',StrToTime(edtTime.Text));
  end;
end;
//++++++++++++++++++++++++++++++++ TLalel +++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.SelectAlignment(Sender : TObject);
begin
  If cbbAlignment.Text='taLeftJustify' then
    Label1.Alignment := taLeftJustify
  else if cbbAlignment.Text='taCenter' then
    Label1.Alignment := taCenter
  else if cbbAlignment.Text='taRightJustify' then
    Label1.Alignment := taRightJustify;
end;
//++++++++++++++++++++++++++++++++ TListBox +++++++++++++++++++++++++++++++++++++++++
Function TForm1.AddNotDub(AList: TCustomListBox; nText: String):Boolean;
begin
  Result := AList.Items.IndexOf(nText)<0;
  If Result then
    AList.Items.Add(nText);
end;
procedure TForm1.ListBoxMove(Sender : TObject);
var
nItem : Integer;
begin
  If Sender=btnRight then    //Move from ListBox1 to ListBox2
    For nItem:=ListBox1.Items.Count-1 downto 0 do
      If ListBox1.Selected[nItem] then
        If AddNotDub(ListBox2,ListBox1.Items[nItem]) then  //Check if exists
          ListBox1.Items.Delete(nItem)
        else
          Application.MessageBox('    Do allready exist    ','No',0);
  If Sender=btnLEft then    //Move from ListBox2 to ListBox1
    For nItem:=ListBox2.Items.Count-1 downto 0 do
      If ListBox2.Selected[nItem] then
        If AddNotDub(ListBox1,ListBox2.Items[nItem]) then  //Check if exists
          ListBox2.Items.Delete(nItem)
        else
          Application.MessageBox('    Do allready exist    ','No',0);
  If Sender=btnAdd then     //Add from the Edits
    begin
      If edtAddLis1.Text<>'' then
        If AddNotDub(ListBox1,edtAddLis1.Text) then        //Check if exists
          edtAddLis1.text := ''
        else
          Application.MessageBox('    Do allready exist    ','No',0);
      If edtAddLis2.Text<>'' then
        If AddNotDub(ListBox2,edtAddLis2.Text) then        //Check if exists
          edtAddLis2.text := ''
        else
          Application.MessageBox('    Do allready exist    ','No',0);
    end;
end;
procedure TForm1.CopyToMem(Sender : TObject);
begin
  memAddTo.Lines := ListBox1.Items;
end;

procedure TForm1.SaveFile(Sender : TObject);
begin
  ListBox1.Items.SaveToFile('./List.txt');
end;
//++++++++++++++++++++++++++++++++ TMemo ++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.NewMemo(Sender : TObject);
begin
  Memo1.Lines.Clear;
end;
procedure TForm1.SaveMemoAs(Sender : TObject);
begin
  SaveDialog1.FileName := 'Untitled.txt';
  SaveDialog1.Filter := 'TextFiles|*.txt';
  If SaveDialog1.Execute then
    Memo1.Lines.SaveToFile(UTF8ToSys(SaveDialog1.FileName));

end;
procedure TForm1.OpenMemo(Sender : TObject);
begin
  OpenDialog1.Filter := 'TextFile|*.txt';
  If OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(UTF8ToSys(OpenDialog1.FileName));

end;
//++++++++++++++++++++++++++++++++ TProgressBar +++++++++++++++++++++++++++++++++++++
procedure TForm1.UpdateProgressBar(Sender : TObject);
begin
  If ProgressBar1.Position > 99 then 
     ProgressBar1.Position := 0;
  ProgressBar1.Position := ProgressBar1.Position + 1;

end;
procedure TForm1.ProgSettings(Sender : TObject);
begin
  If chbSmooth.State = cbChecked then
     ProgressBar1.Smooth := False
  Else ProgressBar1.Smooth := True;

  If chbProgEnable.State = cbChecked then
     ProgTime.Enabled := True
  Else ProgTime.Enabled := False;

  If chbProgOrient.State = cbChecked then
     ProgressBar1.Orientation := pbVertical
  Else ProgressBar1.Orientation := pbHorizontal;

  If (chbProgOrient.State = cbChecked) and (chbProgDirect.State = cbChecked) then
     ProgressBar1.Orientation := pbTopDown
  Else If (chbProgOrient.State = cbUnchecked) and (chbProgDirect.State = cbChecked) then
     ProgressBar1.Orientation := pbRightToLeft;

  If chbProgText.State = cbChecked then
     ProgressBar1.BarShowText := True
  Else ProgressBar1.BarShowText := False;

end;


//++++++++++++++++++++++++++++++++ TPanel +++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.PanelInBevelClick(Sender: TObject);
begin
  case rgPanelInBevel.ItemIndex of
     0 : Panel1.BevelInner := bvLowered;
     1 : Panel1.BevelInner := bvRaised;
     2 : Panel1.BevelInner := bvNone;
     3 : Panel1.BevelInner := bvNone;
  else
     Panel1.BevelInner := bvNone;
  end;
  Panel1.Show;
end;

procedure TForm1.PanelOutBevelClick(Sender: TObject);
begin
  case rgPanelInBevel.ItemIndex of
     0 : Panel1.BevelOuter := bvLowered;
     1 : Panel1.BevelOuter := bvRaised;
     2 : Panel1.BevelOuter := bvNone;
     3 : Panel1.BevelOuter := bvNone;
  else
     Panel1.BevelOuter := bvNone;
  end;
  Panel1.Show;
end;

procedure TForm1.PanelAlignClick(Sender: TObject);
begin
  case rgPanelAlign.ItemIndex of
     0 : Panel1.Align := alNone;
     1 : Panel1.Align := alBottom;
     2 : Panel1.Align := alClient;
     3 : Panel1.Align := alLeft;
     4 : Panel1.Align := alRight;
     5 : Panel1.Align := alTop;
  else
     Panel1.Align := alNone;
  end;
  Panel1.Show;
end;

procedure TForm1.PanelNormClick(Sender:TObject);
begin
   with Panel1 do
   begin
      Left   := 10;
      Top    := 10;
      Width  := 300;
      Height := 100;
    //Ctl3D  := False;
      Caption := 'Panel1';
      Visible := True;
      Align   := alNone;
      BevelOuter := bvRaised;
      BevelInner := bvNone;
   end;
end;
//++++++++++++++++++++++++++++++++ TBevel +++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.BevelSettings(Sender : TObject);
begin
   case chbBevelShape.ItemIndex of
      0 : Bevel1.Shape := bsBox;
      1 : Bevel1.Shape := bsFrame;
      2 : Bevel1.Shape := bsTopLine;
      3 : Bevel1.Shape := bsBottomLine;
      4 : Bevel1.Shape := bsLeftLine;
      5 : Bevel1.Shape := bsRightLine;
   end;
   if chbBevelStyle.ItemIndex = 0
     then Bevel1.Style := bsLowered
     else Bevel1.Style := bsRaised
end;
//+++++++++++++++++++++++++++++++++ TRadioGroup +++++++++++++++++++++++++++++++++++++
procedure TForm1.RadioGroupClick(Sender : TObject);
begin
  RadioGroup1.Caption := ' Radio '+IntToStr(RadioGroup1.ItemIndex)+' Checked';
end;
//+++++++++++++++++++++++++++++++++ TTimer ++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.Timer1OnTimer(Sender : TObject);
begin
lblTimeNow.Caption := 'TimeNow: '+TimeToStr(Now);
  If FTrack = True then
  begin
    MSec:=MSec+1;
    If MSec>10 then
    begin
       Sec := Sec+1;
       If Sec>59 then
       begin
         Min := Min+1;
         lblHourMinSec.Caption:='Track: 00:0'+IntToStr(Min)+':00';
         Sec := 0;
       end;
       If Sec<10 then
         lblHourMinSec.Caption:='Track: 00:0'+IntToStr(Min)+':0'+IntToStr(Sec)
       else lblHourMinSec.Caption:='Track: 00:0'+IntToStr(Min)+':'+IntToStr(Sec);      
       MSec := 0;
    end;
    If MSec<10 then
      lblMSec.Caption := ':0'+IntToStr(MSec)
    Else lblMSec.Caption := ':'+IntToStr(MSec);
  end;
end;
procedure TForm1.TimeTrack(Sender : TObject);
begin
Hour := 0;
Min  := 0;
Sec  := 0;
MSec := 0;
  If FTrack = false then
  begin
    FTrack := True;
    btnStart.Caption := 'Stop';
  end else
  begin
    FTrack := False;
    btnStart.Caption := 'Start';
    lblHourMinSec.Caption := 'Track: 00:00:00';
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//++++++++++++++++++++++++++++++++ Universal Tools ++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.ShowTestForm(Sender : TObject);
begin
  AboutForm.Show;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++ EVENTS ++++++++++++++++++
procedure TForm1.EventFormShow(Sender : TObject);
begin
  EventForm.Show;
  Assert(False, 'Trace: EventFormShow');
end;

procedure TForm1.EventOnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
begin
  lbEvents.Items.Add(Sender.ClassName+'.MouseDown '+IntToStr(X)+','+IntToStr(Y));
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventOnMouseDown');
  If Sender = BenchForm[5] then
  Begin
    Assert(False, 'Trace: CANVAS.PaintWhat='+cbbPaintType.Text);
    If cbbPaintType.Text = 'LineTo' then
    Begin
      BenchForm[5].Canvas.MoveTo(X,Y);
      lbEvents.Items.Add('TFORM.CANVAS.MoveTo: '+IntToStr(X)+','+IntToStr(Y));
      EventWatch;
    End Else
    Begin
      Start1 := X;
      Start2 := Y;
      lbEvents.Items.Add('TFORM.CANVAS.RectStartPos: '+IntToStr(X)+','+IntToStr(Y));
      EventWatch;
    end;
    FPaint := True;
  End;
end;

procedure TForm1.EventOnMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
begin
  lbEvents.Items.Add(Sender.ClassName+'.MouseUp '+IntToStr(X)+','+IntToStr(Y));
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventOnMouseUp');
//+++++++ TCanvas ++++++++++++++++++++++++
  If Sender = BenchForm[5] then
    FPaint := False;
  If Sender = ComboBox1 then
  Begin
    lblSelLength.Caption := 'SelLength: '+IntToStr(ComboBox1.SelLength);
    lblSelStart.Caption := 'SelStart: '+IntToStr(ComboBox1.SelStart);
    lblSelText.Caption := 'SelText: '+ComboBox1.SelText;
  end;
  If Sender=SpinEdit1 then
    lblOnChange.Caption := 'Value: '+FloatToStr(SpinEdit1.Value);
end;

procedure TForm1.EventOnMouseMove(Sender : TObject; Shift : TShiftState; X,Y : Integer);
begin
  If Sender = BenchForm[5] then
  Begin
     If FPaint = True then
     begin
       If cbbPaintType.Text='LineTo' then
       begin
         BenchForm[5].Canvas.LineTo(X,Y);
         lbEvents.Items.Add('TFORM.CANVAS.LineTo: '+IntToStr(X)+','+IntToStr(Y));
         EventWatch;
       End else
       begin
         DrawRect(Start1,Start2,X,Y);
         lbEvents.Items.Add('TFORM.CANVAS.Rectangle: '+IntToStr(X)+','+IntToStr(Y));
         EventWatch;
       end;
     end;
  End Else
  begin
    lbEvents.Items.Add(Sender.ClassName+'.MouseMove '+IntToStr(X)+','+IntToStr(Y));
    EventWatch;
  end;
end;

procedure TForm1.EventOnMouseEnter(Sender : TObject);
begin
 
end;

procedure TForm1.EventOnMouseLeave(Sender : TObject);
begin

end;

procedure TForm1.EventOnClick(Sender : TObject);
begin
  lbEvents.Items.Add(Sender.ClassName+'.Click');
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventOnClick');
  If Sender.ClassName='TBUTTON' then
    Button1Click(Sender);
  If Sender.ClassName='TRADIOGROUP' then
    RadioGroupClick(Sender);
  If Sender.ClassName='TCHECKBOX' then
    CheckClick(Sender);
  If Sender=ToggleBox1 then
  Begin
    If ToggleBox1.Checked = true then
      lblChecked.Caption := 'Checked= True'
    Else lblChecked.Caption := 'Checked= False';
    If ToggleBox1.State= cbChecked then
      lblState.Caption := 'State= cbChecked'
    Else lblState.Caption := 'State= cbUnchecked';
  end;
  If Sender.ClassName='TPAINTBOX' then
  begin
     with TPaintBox(Sender).canvas do
     begin
        moveto (0,0);
        inc (pntCount);
        lineto (100, pntCount * 10);
     end;
  end;
end;

procedure TForm1.EventOnDblClick(Sender : TObject);
begin
  lbEvents.Items.Add(Sender.ClassName+'.DoubleClick');
  EventWatch;
  Assert(False, 'Trace: EventOnDblClick');
end;

procedure TForm1.EventOnResize(Sender : TObject);
begin
  lbEvents.Items.Add('TFORM.Width: '+IntToStr(BenchForm[19].Width));
  lbEvents.Items.Add('TFORM.Height: '+IntToStr(BenchForm[19].Height));
  EventWatch;
  Assert(False, 'Trace: EventOnReSize');
  lblWidth.Caption := 'Width: '+IntToStr(BenchForm[19].Width);
  lblWidth.Caption := 'Height: '+IntToStr(BenchForm[19].Height);
end;

procedure TForm1.EventOnShow(Sender : TObject);
var
i : Integer;
begin
  For i:=1 to 59 do
  begin
    If Sender=BenchForm[i] then
    begin
      lbEvents.Items.Add('TESTFORM['+IntToStr(i)+'].EventOnShow '+TimeToStr(Now));
      EventWatch;
      Assert(False, 'Trace:TESTFORM['+IntToStr(i)+'].EventOnShow');
    end;
  end;
end;

procedure TForm1.EventOnChange(Sender : TObject);
begin
  if not (assigned (lbEvents)) then exit;

  lbEvents.Items.Add(Sender.ClassName+'.Change');
  EventWatch;
  Assert(False, 'Trace: EventOnChange');
  If Sender=ComboBox1 then
  begin
    lblIndex.Caption := 'ItemIndex: '+IntToStr(ComboBox1.ItemIndex);
    lbEvents.Items.Add('ComboBox1.ItemIndex='+IntToStr(ComboBox1.ItemIndex));
    EventWatch;
    Assert(False, 'Trace: ComboBox1.ItemIndex='+IntToStr(ComboBox1.ItemIndex));
  end;
  If Sender=TrackBar2 then
  begin
    TrackBar1.PageSize := TrackBar1.PageSize + 1;
  end;
end;

procedure TForm1.EventOnKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
begin
  If Sender=Edit1 then
  begin
    lbEvents.Items.Add(Sender.ClassName+'.Text='+Edit1.Text);
    EventWatch;
    //If Edit1.Modified = True then  //To get Edit1.OnChange event
      //EventOnChange(Sender);
  end;

  lbEvents.Items.Add(Sender.ClassName+'.EventKeyDown');
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventKeyDown '+IntToStr(Key));
end;

procedure TForm1.EventOnKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
begin

end;
//+++++++++++++++++++++ Virtual MouseEnter/Leave ++++++++++++++++++++++++++++++++++++
procedure TForm1.ApplicationIdle(Sender: TObject; var Done: Boolean); 
var 
  CurControl: TControl;
  P: TPoint;
begin 
  GetCursorPos(P);  
  CurControl := FindControlAtPosition(P, True);
  if FFocusControl <> CurControl then
    begin  
    if FFocusControl <> nil then  
      VirOnLeave(FFocusControl);  
    FFocusControl := CurControl;  
    if FFocusControl <> nil then  
      VirOnEnter(FFocusControl);  
  end; 
end; 
procedure TForm1.VirOnEnter(Sender: TObject); 
begin 
  lbEvents.Items.Add(Sender.ClassName+'.MouseEnter');
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventOnEnter');
end; 
procedure TForm1.VirOnLeave(Sender: TObject); 
begin 
  lbEvents.Items.Add(Sender.ClassName+'.MouseLeave');
  EventWatch;
  Assert(False, 'Trace:'+Sender.ClassName+'.EventOnLeave');
end;
//++++++++++++++++++++++ Event Watch ++++++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.EventWatch;
begin
  If lbEvents.Items.Count>18 then
    lbEvents.Items.Delete(0);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TForm1.TestFormClose(Sender : TObject; var CloseAction : TCloseAction);
var
i : Integer;
begin
  For i:=1 to 59 do
  begin
    If Sender=BenchForm[i] then
    begin
      lbEvents.Items.Add('TESTFORM['+IntToStr(i)+'].EventOnClose '+TimeToStr(Now));
      EventWatch;
      Assert(False, 'Trace:TESTFORM['+IntToStr(i)+'].EventOnClose');
    End;
  End;

  Form1.Visible := True;
   //Form1.WindowState := wsNormal;
   //Memo1.Parent:=Self;
   Comps1.Enabled := True;
   EventForm.Caption := 'Events';
   //Timer1.Enabled := False;
end;

procedure TForm1.ShowMessage0(Sender : TObject);
begin
  AppliCation.MessageBox(Info[0],'Status Information',0); //For future components
end;

procedure TForm1.ShowMessage1(Sender : TObject);
begin
  If Sender=TAppl then
  begin
    BenchForm[1].Show;
    EventForm.Caption := EventForm.Caption+' TApplication';
    Assert(False, 'Trace: >>> TApplication TEST');
  end;
  If Sender=TBev then
  begin
    BenchForm[2].Show;
    EventForm.Caption := EventForm.Caption+' TBevel';
    Assert(False, 'Trace: >>> TBevel TEST');
  end;
  If Sender=TBit then
  Begin
    BenchForm[3].Show;
    EventForm.Caption := EventForm.Caption+' BitBtn1';
    Assert(False, 'Trace: >>> TBitBtn TEST');
  end;
  If Sender=TBut then
  begin
    BenchForm[4].Show;
    EventForm.Caption := EventForm.Caption+' Button1[0..15]';
    Assert(False, 'Trace: >>> TButton TEST');
  end;
  If Sender=TCan then
  begin
    BenchForm[5].Show;
    EventForm.Caption := EventForm.Caption+' TCanvas';
    Assert(False, 'Trace: >>> TCanvas TEST');
  end;
  If Sender=TChe then
  begin
    BenchForm[6].Show;
    EventForm.Caption := EventForm.Caption+' CheckBox1[1..35]';
    Assert(False, 'Trace: >>> TCheckBox TEST');
  end;
  If Sender=TClip then
  begin
    BenchForm[7].Show;
    EventForm.Caption := EventForm.Caption+' ClipBoard1';
    Assert(False, 'Trace: >>> TClipBoard TEST');
  end;
  If Sender=TCLis then
  begin
    BenchForm[8].Show;
    EventForm.Caption := EventForm.Caption+' CListBox1';
    Assert(False, 'Trace: >>> TCListBox TEST');
  end;
  If Sender=TCol then
  begin
    BenchForm[9].Caption := BenchForm[9].Caption+' ++Move the form to se the Color-Selected <^>';
    BenchForm[9].Show;
    EventForm.Caption := EventForm.Caption+' TColorDialog';
    Assert(False, 'Trace: >>> TColorDialog TEST');
    Assert(False, 'Trace: **********************************');
    Assert(False, 'Trace: ******** MOVE THE FORM TO ********');
    Assert(False, 'Trace: ******** TO SE THE COLOR  ********');
    Assert(False, 'Trace: ******** SELECTED !!!     ********');
    Assert(False, 'Trace: **********************************');
  end;
  If Sender=TCom then
  begin
    BenchForm[10].Show;
    EventForm.Caption := EventForm.Caption+' TComboBox';
    Assert(False, 'Trace: >>> TComboBox TEST');
  end;
  If Sender=TDir then
  begin
    BenchForm[11].Show;
    EventForm.Caption := EventForm.Caption+' TDirectoryListBox';
    Assert(False, 'Trace: >>> TDirectoryListBox TEST');
  end;
  If Sender=TDra then
  begin
    BenchForm[12].Show;
    EventForm.Caption := EventForm.Caption+' TDrawGrid';
    Assert(False, 'Trace: >>> TDrawGrid TEST');
  end;
  If Sender=TDri then
  begin
    BenchForm[13].Show;
    EventForm.Caption := EventForm.Caption+' TDriveComboBox';
    Assert(False, 'Trace: >>> TDriveComboBox TEST');
  end;
  If Sender=TEdi then
  begin
    BenchForm[14].Show;
    EventForm.Caption := EventForm.Caption+' TEdit';
    Assert(False, 'Trace: >>> TEdit TEST');
  end;
  If Sender=TFile then
  begin
    BenchForm[15].Show;
    EventForm.Caption := EventForm.Caption+' TFileListBox';
    Assert(False, 'Trace: >>> TFileListBox TEST');
  end;
  If Sender=TFilt then
  begin
    BenchForm[16].Show;
    EventForm.Caption := EventForm.Caption+' TFilterComboBox';
    Assert(False, 'Trace: >>> TFilterComboBox TEST');
  end;
  If Sender=TFin then
  begin
    BenchForm[17].Show;
    EventForm.Caption := EventForm.Caption+' TFindDialog';
    Assert(False, 'Trace: >>> TFindDialog TEST');
  end;
  If Sender=TFon then
  begin
    BenchForm[18].Show;
    EventForm.Caption := EventForm.Caption+' TFontDialog';
    Assert(False, 'Trace: >>> TFontDialog TEST');
  end;
  If Sender=TFor then
  begin
    BenchForm[19].Show;
    EventForm.Caption := EventForm.Caption+' TForm';
    Assert(False, 'Trace: >>> TForm TEST');
  end;
  If Sender=TGro then
  begin
    BenchForm[20].Show;
    EventForm.Caption := EventForm.Caption+' TGroupBox';
    Assert(False, 'Trace: >>> TGoup TEST');
  end;
  If Sender=TIma then
  begin
    BenchForm[21].Show;
    EventForm.Caption := EventForm.Caption+' TImage';
    Assert(False, 'Trace: >>> TImage TEST');
  end;
  If Sender=TIni then
  begin
    BenchForm[22].Show;
    EventForm.Caption := EventForm.Caption+' TIniFile';
    Assert(False, 'Trace: >>> TIniFile TEST');
  end;
  If Sender=TLab then
  begin
    BenchForm[23].Show;
    EventForm.Caption := EventForm.Caption+' TLabel';
    Assert(False, 'Trace: >>> TLabel TEST');
  end;
  If Sender=TLis then
  begin
    BenchForm[24].Show;
    EventForm.Caption := EventForm.Caption+' TListBox';
    Assert(False, 'Trace: >>> TListBox TEST');
  end;
  If Sender=TMas then
  begin
    BenchForm[25].Show;
    EventForm.Caption := EventForm.Caption+' TMaskEdit';
    Assert(False, 'Trace: >>> TMaskEdit TEST');
  end;
  If Sender=TMed then
  begin
    BenchForm[26].Show;
    EventForm.Caption := EventForm.Caption+' TMediaPlayer';
    Assert(False, 'Trace: >>> TMediaPlayer TEST');
  end;
  If Sender=TMem then
  begin
    Memo1.Parent := BenchForm[27];
    Memo1.Lines.Clear;
    BenchForm[27].Show;
    EventForm.Caption := EventForm.Caption+' TMemo';
    Assert(False, 'Trace: >>> TMemo TEST');
  end;
  If Sender=TMes then
  begin
    BenchForm[28].Show;
    EventForm.Caption := EventForm.Caption+' TMessageDialog';
    Assert(False, 'Trace: >>> TMessageDialog TEST');
  end;
  If Sender=TNot then
  begin
    BenchForm[29].Show;
    EventForm.Caption := EventForm.Caption+' TNoteBook';
    Assert(False, 'Trace: >>> TNoteBook TEST');
  end;
  If Sender=TOpe then
  begin
    BenchForm[30].Show;
    EventForm.Caption := EventForm.Caption+' TOpenDialog';
    Assert(False, 'Trace: >>> TOpenDialog TEST');
  end;
  If Sender=TPag then
  begin
    BenchForm[31].Show;
    EventForm.Caption := EventForm.Caption+' TPageControl';
    Assert(False, 'Trace: >>> TPageControl TEST');
  end;
  If Sender=TPai then
  begin
    BenchForm[32].Show;
    EventForm.Caption := EventForm.Caption+' TPaintBox';
    Assert(False, 'Trace: >>> TPaintBox TEST');
  end;
  if Sender=TPan then
  begin
    BenchForm[33].Show;
    EventForm.Caption := EventForm.Caption+' TPanel';
    Assert(False, 'Trace: >>> TPanel TEST');
  end;
  If Sender=TPop then
  begin
    BenchForm[34].Show;
    EventForm.Caption := EventForm.Caption+' TPopupMenu';
    Assert(False, 'Trace: >>> TPopupMenu TEST');
  end;
  If Sender=TPro then
  begin
    BenchForm[37].Show;
    EventForm.Caption := EventForm.Caption+' TProgressBar';
    Assert(False, 'Trace: >>> TProgressBar TEST');
  end;
  If Sender=TRadB then
  begin
    BenchForm[38].Show;
    EventForm.Caption := EventForm.Caption+' TRadioButton';
    Assert(False, 'Trace: >>> TRadioButton TEST');
  end;
  If Sender=TRadG then
  begin
    BenchForm[39].Show;
    EventForm.Caption := EventForm.Caption+' TRadioGroup';
    Assert(False, 'Trace: >>> TRadioGroup TEST');
  end;
  If Sender=TSav then
  begin
    BenchForm[42].Show;
    EventForm.Caption := EventForm.Caption+' TSaveDialog';
    Assert(False, 'Trace: >>> TSaveDialog TEST');
  end;
  If Sender=TScre then
  begin
    BenchForm[43].Show;
    EventForm.Caption := EventForm.Caption+' TScreen';
    Assert(False, 'Trace: >>> TScreen TEST');
  end;
  If Sender=TScroBa then
  begin
    BenchForm[44].Show;
    EventForm.Caption := EventForm.Caption+' TScrollBar';
    Assert(False, 'Trace: >>> TScrollBar TEST');
  end;
  If Sender=TSha then
  begin
    BenchForm[46].Show;
    //AppliCation.MessageBox(Info[3],'Status Information',0);
    EventForm.Caption := EventForm.Caption+' TShape';
    Assert(False, 'Trace: >>> TShape TEST');
  end;
  If Sender=TSpee then
  begin
    BenchForm[47].Show;
    //AppliCation.MessageBox(Info[3],'Status Information',0);
    EventForm.Caption := EventForm.Caption+' TSpeedButton';
    Assert(False, 'Trace: >>> TSpeedButton TEST');
  end;
  If Sender=TSPinE then
  begin
    BenchForm[49].Show;
    //AppliCation.MessageBox(Info[3],'Status Information',0);
    EventForm.Caption := EventForm.Caption+' TSpinEdit';
    Assert(False, 'Trace: >>> TSpinEdit TEST');
  end;
  If Sender=TStat then
  begin
    BenchForm[50].Show;
    //AppliCation.MessageBox(Info[3],'Status Information',0);
    EventForm.Caption := EventForm.Caption+' TStatusBar';
    Assert(False, 'Trace: >>> TStatusBar TEST');
  end;
  If Sender=TThre then
  begin
    BenchForm[54].Show;
    Timer1.Enabled := True;
    EventForm.Caption := EventForm.Caption+' TThread';
    Assert(False, 'Trace: >>> TThread TEST');
  end;
  If Sender=TTim then
  begin
    BenchForm[55].Show;
    Timer1.Enabled := True;
    EventForm.Caption := EventForm.Caption+' TTimer';
    Assert(False, 'Trace: >>> TTimer TEST');
  end;
  If Sender=TTog then
  begin
    BenchForm[56].Show;
    //AppliCation.MessageBox(Info[2],'Status Information',0);
  end;
  If Sender=TToo then
  begin
    BenchForm[57].Show;
    EventForm.Caption := EventForm.Caption+' TToolBar';
    Assert(False, 'Trace: >>> TToolBar TEST');
  end;
  If Sender=TTrac then
  begin
    BenchForm[58].Show;
    EventForm.Caption := EventForm.Caption+' TTrackBar';
    Assert(False, 'Trace: >>> TTrackBar TEST');
  end;
  //Comps1.Enabled := False;
  Form1.Visible := False;
end;

procedure TForm1.CloseAbout(Sender : TObject);
begin
  AboutForm.Close;
end;

procedure TForm1.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent=FFocusControl) and (Operation=opRemove) then
    FFocusControl:=nil;
end;

procedure TForm1.QuitClick(Sender : TObject);
begin
  Close;
end;

Destructor TForm1.Destroy;
begin
  IniFile1.Free;
  inherited Destroy;
end;

