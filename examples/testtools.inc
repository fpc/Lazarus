//******************************************************************************
//				TestTools.inc
//******************************************************************************

//------------------------------------------------------------------------------
//				LoadTestTools is a procedure
//				that loads all test components
//------------------------------------------------------------------------------
procedure TForm1.LoadTestTools;
begin
AWidth := TEdit.Create(Self);		//Sets the width to Button2 when OK is pressed
   with AWidth do
   begin
     OnChange := @ButtonSizeChange;
     Parent := NoteBook1.Page[1];
     Top := 10;
     Left := 90;
     width := 40;
     Text := IntToStr(Button2.Width);
     Visible := true;
   end;
AHeight := TEdit.Create(Self);		//Sets the height to Button2 when OK is pressed
   with AHeight do
   begin
     OnChange := @ButtonSizeChange;
     Parent := NoteBook1.Page[1];
     Top := 10;
     Left := 135;
     width := 40;
     Text := IntToStr(Button2.Height);
     Visible := true;
   end;
btnOK := TBitBtn.Create(Self);		//The "ChangeSize Button2" OK button
   with btnOK do
   begin
     OnClick := @ButtonSizeChange;
     Parent := NoteBook1.Page[1];
     Top := 10;
     Left:= 185;
     Width := 29;
     Kind := bkCustom;
     Caption := 'OK';
     Hint := 'Change Size on Button2';
     ShowHint := True;
     Visible := True;
   end;
ChangeFont := TBitBtn.Create(Self);	//The BitBtn change font button
   with ChangeFont do
   begin
     OnClick := @ChangeFontClick;
     Parent := NoteBook1.Page[0];
     Top := 43;
     Left := 100;
     Width := 90;
     Height := 28;
     Kind := bkCustom;
     Caption := 'Change Font';
     Hint := 'Execute FontDialog1...'+#10#13+'(Don´t work)';
     ShowHint := True;
     Show;
   end;
LoadGlyph := TBitBtn.Create(Self);	//The BitBtn Load Glyph button
   with LoadGlyph do
   begin
     OnClick := @LoadGlyphClick;
     Parent := NoteBook1.Page[0];
     Top := 10;
     Left := 100;
     Width := 90;
     Height := 28;
     Kind := bkCustom;
     Caption := 'Load Glyph';
     Hint := 'Execute OpenDialog1...'+#10#13+'(Don´t work)';
     ShowHint := True;
     Show;
   end;
ChangeStyleCombo := TComboBox.Create(Self);//The change style combobox
   with ChangeStyleCombo do
   begin
     OnChange := @StyleComboChange;
     Parent := NoteBook1.Page[0];
     Top := 172;
     Left := 100;
     Width := 110;
     Height := 28;
     Items.Add('blGlyphLeft');
     Items.Add('blGlyphTop');
     Items.Add('blGlyphBottom');
     Items.Add('blGlyphRight');
     Show;
   end;
lblProp := TLabel.Create(Self);		//The ListBox "Properties" Label
   with lblProp do
   begin
     Parent := NoteBook1.Page[SaveIni.ReadInteger('TestAllSettings','Pageshow',0)];
     Top := 8;
     Left := 295;
     Width := 232;
     Caption := 'Component Properties';
     Show;
   end;
CoProp := TListBox.Create(Self);	//Component Properties ListBox
   with CoProp do
   begin
     Parent := NoteBook1.Page[SaveIni.ReadInteger('TestAllSettings','Pageshow',0)];
     Top := 28;
     Left := 295;
     Width := 232;
     Height := 100;
     Visible := True;   
   end;
   CoProperties(BitBtn1);		//Load the CoProp at startup with BitBtn1
lblEvent := TLabel.Create(Self);	//The Event Log Label
   with lblEvent do
   begin
     Parent := Notebook1.Page[SaveIni.ReadInteger('TestAllSettings','Pageshow',0)];
     top := 135;
     Left := 295;
     Width := 150;
     Caption := 'Event Log';
     Show;
   end;
ClearEvents := TBitBtn.Create(Self);	//The Clear button wich clears the Event Log
   with ClearEvents do
   begin
     OnClick := @ClearEventLog;
     Parent := NoteBook1.Page[SaveIni.ReadInteger('TestAllSettings','Pageshow',0)];
     Top := 131;
     Left := 485;
     Width := 42;
     Height := 22;
     Kind := bkCustom;
     Caption := 'Clear';
     Hint := 'Clear Event Log';
     ShowHint := True;
     Show;
   end;
CoEvent := TListBox.Create(Self);	//Component Event Log ListBox
   with CoEvent do
   begin
     Parent := NoteBook1.Page[SaveIni.ReadInteger('TestAllSettings','Pageshow',0)];
     Top := 155;
     Left := 295;
     Height := 100;
     Width := 232;
     Show;
   end;
lblPages := TLabel.Create(Self);	//Pages count
   with lblPages do
   begin
     Parent := Self;
     Caption := IntToStr(NoteBook1.Pages.Count)+' Pages';
     Top := 2;
     Left := 480;
     Width := 100;
     Show;
   end;
btnPrev := TBitBtn.Create(Self);	//Slides the NoteBook to the right
   with btnPrev do
   begin
     OnClick := @PrevClick;
     Parent := Self;
     Width := 25;
     Caption := '<';
     Name := 'btnPrev';
     Top := 52;
     Left := 488;
     Kind := bkCustom;
     layout := blGlyphLeft;
     Height := 25;
     Hint := 'Previous Page';
     ShowHint := True;
     Show;
   end;
btnNext := TBitBtn.Create(Self);	//Slides the NoteBook to the left
   with btnNext do
   begin
     OnClick := @NextClick;
     Parent := Self;
     Width := 25;
     Caption := '>';
     Name := 'btnNext';
     Top := 52;
     Left := 513;
     Kind := bkCustom;
     layout := blGlyphRight;
     Height := 25;
     Hint := 'Next Page';
     ShowHint := True;
     Show;
   end;
SaveSettings := TCheckBox.Create(Self);		//If checked the lastpage setting is saved
   with SaveSettings do
   begin
     OnClick := @SaveSettingsClick;
     Checked := SaveIni.ReadBool('TestAllSettings','SaveSettings',False);
     Parent := Self;
     Width := 82;
     Caption := 'Save View';
     Left := 455;
     Top := 20;
     Enabled := True;
     Hint := 'Save this view';
     ShowHint := True;
     Show;
   end;
btnColor := TBitBtn.Create(Self);	//ColorDialog button
   with btnColor do
   begin
     OnClick := @Speed4Click;
     Parent := NoteBook1.Page[3];
     Width := 80;
     Caption := 'ColorDialog';
     Name := 'btnColor';
     Top := 10;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Runs the ColorDialog';
     ShowHint := True;
     Show;
   end;
btnFont := TBitBtn.Create(Self);	//FontDialog button
   with btnFont do
   begin
     OnClick := @Speed3Click;
     Parent := NoteBook1.Page[7];
     Width := 80;
     Caption := 'FontDialog';
     Name := 'btnFont';
     Top := 10;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Runs the FontDialog';
     ShowHint := True;
     Show;
   end;
btnOpen := TBitBtn.Create(Self);	//OpenDialog button
   with btnOpen do
   begin
     OnClick := @Speed1Click;
     Parent := NoteBook1.Page[13];
     Width := 80;
     Caption := 'OpenDialog';
     Name := 'btnOpen';
     Top := 10;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Runs the OpenDialog';
     ShowHint := True;
     Show;
   end;
btnSave := TBitBtn.Create(Self);	//SaveDialog button
   with btnOpen do
   begin
     OnClick := @Speed2Click;
     Parent := NoteBook1.Page[17];
     Width := 80;
     Caption := 'SaveDialog';
     Name := 'btnSave';
     Top := 10;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Runs the SaveDialog';
     ShowHint := True;
     Show;
   end;
//**	**	**	**	**	**	**	**	**	**	
//*********************************** Ini test *********************************
TestIni := TInifile.Create('TestIni.ini');
TestIni.WriteTime('TestAll Application','Application StartTime',Now);
TestIni.WriteDate('TestAll Application','Today',Now);
lblSection := TLabel.Create(Self);
   with lblSection do
   begin
     Parent := NoteBook1.Page[8];
     Width := 50;
     Caption := 'Section';
     Name := 'lblSection';
     Top := 10;
     Left := 10;
     Show;
   end;
edtSection := TEdit.Create(Self);
   with edtSection do
   begin
     Parent := NoteBook1.Page[8];
     Top := 28;
     Left := 10;
     Width := 200;
     Height := 21;
     Text := 'TestAll Application';
     Name := 'edtSection';
     Visible := True;
   end;
lblIdent := TLabel.Create(Self);
   with lblIdent do
   begin
     Parent := NoteBook1.Page[8];
     Width := 50;
     Caption := 'Ident';
     Name := 'lblIdent';
     Top := 53;
     Left := 10;
     Show;
   end;
edtIdent := TEdit.Create(Self);
   with edtIdent do
   begin
     Parent := NoteBook1.Page[8];
     Top := 71;
     Left := 10;
     Width := 200;
     Height := 21;
     Text := 'Author';
     Name := 'edtIdent';
     Visible := True;
   end;
lblValue := TLabel.Create(Self);
   with lblValue do
   begin
     Parent := NoteBook1.Page[8];
     Width := 50;
     Caption := 'Value';
     Name := 'lblIValue';
     Top := 96;
     Left := 10;
     Show;
   end;
edtValue := TEdit.Create(Self);
   with edtValue do
   begin
     Parent := NoteBook1.Page[8];
     Top := 114;
     Left := 10;
     Width := 200;
     Height := 21;
     Text := TestIni.ReadString('TestAll Application','Author','ERROR');
     Name := 'edtValue';
     Visible := True;
   end;
rbString := TRadioButton.Create(Self);
   with rbString do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 28;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'String';
     Checked := True;
     Name := 'rbString';
     Visible := true;
   end;
rbFloat := TRadioButton.Create(Self);
   with rbFloat do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 46;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'Float';
     Checked := False;
     Name := 'rbFloat';
     Visible := true;
   end;
rbInteger := TRadioButton.Create(Self);
   with rbInteger do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 64;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'Integer';
     Checked := False;
     Name := 'rbInteger';
     Visible := true;
   end;
rbBool := TRadioButton.Create(Self);
   with rbBool do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 82;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'Boolean';
     Checked := False;
     Name := 'rbBool';
     Visible := true;
   end;
rbTime := TRadioButton.Create(Self);
   with rbTime do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 100;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'Time';
     Checked := False;
     Name := 'rbTime';
     Visible := true;
   end;
rbDate := TRadioButton.Create(Self);
   with rbDate do
   begin
     OnClick := @IniType;
     Parent := NoteBook1.Page[8];
     Top := 118;
     Left := 220;
     width := 70;
     Height := 18;
     Caption := 'Date';
     Checked := False;
     Name := 'rbDate';
     Visible := true;
   end;
btnReadIni := TBitBtn.Create(Self);	//Read from IniFile
   with btnReadIni do
   begin
     OnClick := @ReadInifile;
     Parent := NoteBook1.Page[8];
     Width := 43;
     Caption := 'Read';
     Name := 'btnReadIni';
     Top := 150;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Reads from TestIni.ini';
     ShowHint := True;
     Show;
   end;
btnWriteIni := TBitBtn.Create(Self);	//Write to IniFile
   with btnWriteIni do
   begin
     OnClick := @WriteIniFile;
     OnMouseEnter := @GivesAHint;
     OnMouseLeave := @MLeave;
     Parent := NoteBook1.Page[8];
     Width := 43;
     Caption := 'Write';
     Name := 'btnWriteIni';
     Top := 150;
     Left := 167;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Write to TestIni.ini';
     ShowHint := True;
     Show;
   end;
//******************************************************************************
//********************************* ProgressBar ********************************
ProgTime := TTimer.Create(Self);
  with ProgTime do
  begin
    OnTimer := @UpdateProgressBar;
    Enabled := False;
    Interval := 50;
    Name := 'ProgTime';
  end;
ProgEnable := TCheckBox.Create(Self);
  with ProgEnable do
  begin
     OnClick := @ProgSettings;
     Parent := NoteBook1.Page[14];
     Top := 28;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Run';
     Checked := False;
     Enabled := True;
     Hint := 'Run the ProgressBar';
     ShowHint := True;
     Visible := True;
  End;
ProgOrient := TCheckBox.Create(Self);
  with ProgOrient do
  begin
     OnClick := @ProgSettings;
     Parent := NoteBook1.Page[14];
     Top := 48;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Vertical';
     Checked := False;
     Enabled := True;
     Hint := 'Change orientation';
     ShowHint := True;
     Visible := True;
  End;
ProgDirect := TCheckBox.Create(Self);
  with ProgDirect do
  begin
     OnClick := @ProgSettings;
     Parent := NoteBook1.Page[14];
     Top := 68;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Direction';
     Checked := False;
     Enabled := True;
     Hint := 'Change direction';
     ShowHint := True;
     Visible := True;
  End;
ProgText := TCheckBox.Create(Self);
  with ProgText do
  begin
     OnClick := @ProgSettings;
     Parent := NoteBook1.Page[14];
     Top := 88;
     Left := 220;
     Height := 20;
     Width := 70;
     Caption := 'Text';
     Checked := False;
     Enabled := True;
     Hint := 'Show progress text';
     ShowHint := True;
     Visible := True;
  End;

TrackMemo := TMemo.Create(Self);
   with TrackMemo do
   begin
     Parent := NoteBook1.Page[23];
     Top := 25;
     Left := 50;
     Height := 1;
     Width := 1;
     Name := 'TrackMemo';
     Text := 'Well, it´s something to test the OnChange event anyway...';
     Show;
   end;
lblTimer := TLabel.Create(Self);
   with lblTimer do
   begin
     Parent:=NoteBook1.Page[21];
     Top:=10;
     Left:=10;
     width:=150;
     Caption := 'Time: 00:00:00';
     Name := 'lblTimer';
     Show;
   end;
btnForm2Show := TBitBtn.Create(self);
   with btnForm2Show do
   begin
     OnClick := @Form2Show;
     Parent := NoteBook1.Page[6];
     Width := 45;
     Caption := 'Show';
     Name := 'btnForm2Show';
     Top := 10;
     Left := 10;
     Kind := bkCustom;
     Height := 25;
     Hint := 'Show Form2 and gives it your settings';
     ShowHint := True;
     Show;
   end;
lblForm2 := TLabel.Create(Self);
   with lblForm2 do
   begin
     Parent:= Form2;
     Top:=10;
     Left:=10;
     width:=150;
     Caption := 'That´s it !!!';
     Name := 'lblForm2';
     Show;
   end;
end;
//------------------------------------------------------------------------------
//				NoteBook Change Page: the procedure
//				that contol what will be shown on
//				the NoteBook all pages
//------------------------------------------------------------------------------
procedure TForm1.NoteBookChangePage(Sender : TObject);
begin
  CoProp.Clear;
    CoProp.Parent := NoteBook1.Page[NoteBook1.PageIndex];
    CoEvent.Parent := NoteBook1.Page[NoteBook1.PageIndex]; 
    lblEvent.Parent := NoteBook1.Page[NoteBook1.PageIndex];
    lblProp.Parent := NoteBook1.Page[NoteBook1.PageIndex];
    ClearEvents.Parent := NoteBook1.Page[NoteBook1.PageIndex];
end;
//------------------------------------------------------------------------------
//				Component Properties
//				The common properties from TControl
//------------------------------------------------------------------------------
procedure TForm1.CoProperties(Sender : TControl);
begin
with CoProp.Items do
   begin
     If Sender.Anchors=Sender.Anchors + [akTop] then
       Add(Sender.Name+'.Anchors:=akTop');		// Anchors
       Add(Sender.Name+'.Caption:='+Sender.Caption);	// Caption
     If Sender.Cursor=crDefault then
       Add(Sender.Name+'.Cursor:=crDefault')		//All, insert more cursors
     else If Sender.Cursor=crNone then
       Add(Sender.Name+'.Cursor:=crNone')
     else If Sender.Cursor=crArrow then
       Add(Sender.Name+'.Cursor:=crArrow')
     else If Sender.Cursor=crHandPoint then
       Add(Sender.Name+'.Cursor:=crHandPoint')
     else If Sender.Cursor=crCross then
       Add(Sender.Name+'.Cursor:=crCross')
     else
       Add(Sender.Name+'.Cursor:=crDefault');

     If Sender.DragCursor=crDrag then
       Add(Sender.Name+'.DragCursor:=crDrag'); // All, insert more cursors
   end;
   If Sender.DragKind=dkDock then
      CoProp.Items.Add(Sender.Name+'.DragKind:=dkDock')
   else CoProp.Items.Add(Sender.Name+'.DragKind:=dkDrag'); // All...

   If Sender.DragMode=dmManual then
     CoProp.Items.Add(Sender.Name+'.DragMode:=dmManual'); // All, insert more dmXXXXX 

   If Sender.Enabled=True then
     CoProp.Items.Add(Sender.Name+'.Enabled:=True')
   else CoProp.Items.Add(Sender.Name+'.Enabled:=False');

   //If Sender.Font.Charset=DEFAULT_CHARSET then
   //   CoProp.Items.Add(Sender.Name+'.Font.Charset:=DEFAULT_CHARSET');
   CoProp.Items.Add(Sender.Name+'.Font.Color:='+IntToStr(Sender.Font.Color));// All in Int..
   CoProp.Items.Add(Sender.Name+'.Font.Height:='+IntToStr(Sender.Font.Height)); // All...
   CoProp.Items.Add(Sender.Name+'.Font.Name:='+Sender.Font.Name);// All...
   If Sender.Font.Pitch=fpDefault then
     CoProp.Items.Add(Sender.Name+'.Font.Pitch:=fpDefault'); //All...

   CoProp.Items.Add(Sender.Name+'.Font.Size:='+IntToStr(Sender.Font.Size));

   If Sender.Font.Style=Sender.Font.Style+[fsBold] then
     CoProp.Items.Add(Sender.Name+'.Font.Style:=fsBold');// All   

   CoProp.Items.Add(Sender.Name+'.Height:='+IntToStr(Sender.Height)); // All..
   CoProp.Items.Add(Sender.Name+'.Hint:='+Sender.Hint); // All..

  If Sender=BitBtn1 then begin
     BitBtnProp(BitBtn1);
     ButtonProp(BitBtn1);
  end else if Sender=BitBtn2 then begin
     BitBtnProp(BitBtn2);
     ButtonProp(BitBtn2);
  end else if Sender=BitBtn4 then begin
     BitBtnProp(BitBtn4);
     ButtonProp(BitBtn4);
  end else if Sender=BitBtn6 then begin
     BitBtnProp(BitBtn6);
     ButtonProp(BitBtn6);
  end else if Sender=Button1 then
     ButtonProp(Button1)
  else if Sender=CheckBox1 then
     CheckProp(CheckBox1)
  else if Sender=CheckBox2 then
     CheckProp(CheckBox2);

   CoProp.Items.Add(Sender.Name+'.Left:='+IntToStr(Sender.Left));// All...

   CoProp.Items.Add(Sender.Name+'.Name:='+Sender.Name);// All... 

   If Sender.ParentFont=True then
     CoProp.Items.Add(Sender.Name+'.ParentFont:=True')
   else 
     CoProp.Items.Add(Sender.Name+'.ParentFont:=False');// All...
   If Sender.ParentShowHint=True then
     CoProp.Items.Add(Sender.Name+'.ParentShowHint:=True')
   else 
     CoProp.Items.Add(Sender.Name+'.ParentShowHint:=False');// All..
   CoProp.Items.Add(Sender.Name+'.PopupMenu:='+Sender.PopupMenu.Name); // PopupMenu
   If Sender.ShowHint=True then
     CoProp.Items.Add(Sender.Name+'.ShowHint:=True')
   else 
     CoProp.Items.Add(Sender.Name+'.ShowHint:=False'); // All
   CoProp.Items.Add(Sender.Name+'.Spacing:='+IntToStr(BitBtn1.Spacing)); // BitBtn
   CoProp.Items.Add(Sender.Name+'.TabOrder:='+IntToStr(BitBtn1.TabOrder));// Buttons OK
   If BitBtn1.TabStop=True then
     CoProp.Items.Add(Sender.Name+'.TabStop:=True')
   else 
     CoProp.Items.Add(Sender.Name+'.TabStop:=False');
   CoProp.Items.Add(Sender.Name+'.Tag:='+IntToStr(Sender.Tag));
   CoProp.Items.Add(Sender.Name+'.Top:='+IntToStr(Sender.Top));
   If Sender.Visible=True then
     CoProp.Items.Add(Sender.Name+'.Visible:=True')
   else 
     CoProp.Items.Add(Sender.Name+'.Visible:=False');
   CoProp.Items.Add(Sender.Name+'.Width:='+IntToStr(Sender.Width));
   lblProp.Caption := 'Component Properetties' + '  '+IntToStr(CoProp.Items.Count);
end;
//------------------------------------------------------------------------------
//				BitBtn Properties
//------------------------------------------------------------------------------
procedure TForm1.BitBtnProp(Sender : TBitBtn);
begin
   CoProp.Items.Add(Sender.Name+'.Glyph:=BitMap1');

   If Sender.Kind=bkCustom then
     CoProp.Items.Add(Sender.Name+'.Kind:=bkCustom')
   Else If Sender.Kind=bkOK then
     CoProp.Items.Add(Sender.Name+'.Kind:=bkOK')
   Else If Sender.Kind=bkHelp then
     CoProp.Items.Add(Sender.Name+'.Kind:=bkHelp')
   Else If Sender.Kind=bkAll then
     CoProp.Items.Add(Sender.Name+'.Kind:=bkAll');

   If Sender.Layout=blGlyphLeft then
     CoProp.Items.Add(Sender.Name+'.Layout:=blGlyphLeft')
   else if Sender.Layout=blGlyphTop then
     CoProp.Items.Add(Sender.Name+'.Layout:=blGlyphTop')    
   else if Sender.Layout=blGlyphRight then
     CoProp.Items.Add(Sender.Name+'.Layout:=blGlyphRight')
   else if Sender.Layout=blGlyphBottom then
     CoProp.Items.Add(Sender.Name+'.Layout:=blGlyphBottom');
end;
//------------------------------------------------------------------------------
//				Button Properties
//------------------------------------------------------------------------------
procedure TForm1.ButtonProp(Sender : TButton);
begin

   If Sender.Default=True then
     CoProp.Items.Add(Sender.Name+'.Default:=True')
   else CoProp.Items.Add(Sender.Name+'.Default:=False');

   If Sender.ModalResult=mrNone then
     CoProp.Items.Add(Sender.Name+'.ModalResult:=mrNone')
   Else If Sender.ModalResult=mrOK then
     CoProp.Items.Add(Sender.Name+'.ModalResult:=mrOK')
   Else If Sender.ModalResult=mrAll then
     CoProp.Items.Add(Sender.Name+'.ModalResult:=mrAll')
   Else CoProp.Items.Add(Sender.Name+'.ModalResult:=0');
end;
//------------------------------------------------------------------------------
//				CheckBox Properties
//------------------------------------------------------------------------------
procedure TForm1.CheckProp(Sender : TCheckBox);
begin
   If Sender.AllowGrayed=False then
     CoProp.Items.Add(Sender.Name+'.AllowGrayed:=False')
   else CoProp.Items.Add(Sender.Name+'.AllowGrayed:=True');

   If Sender.Checked=False then
     CoProp.Items.Add(Sender.Name+'.Checked:=False')
   else CoProp.Items.Add(Sender.Name+'.Checked:=True');

   If Sender.State=cbChecked then
     CoProp.Items.Add(Sender.Name+'.State:=cbChecked')
   else If Sender.State=cbUnchecked then
     CoProp.Items.Add(Sender.Name+'.State:=cbUnchecked')
   else If Sender.State=cbGrayed then
     CoProp.Items.Add(Sender.Name+'.State:=cbGrayed');
end;
//------------------------------------------------------------------------------
//				ComboBox that set the Layout
//				of BitBtn All.
//------------------------------------------------------------------------------
procedure TForm1.StyleComboChange(Sender : TObject);
begin
  if ChangeStyleCombo.Text='blGlyphTop' then
    BitBtn6.Layout := blGlyphTop
  else if ChangeStyleCombo.Text='blGlyphLeft' then
    BitBtn6.Layout := blGlyphLeft
  else if ChangeStyleCombo.Text='blGlyphBottom' then
    BitBtn6.Layout := blGlyphBottom
  else if ChangeStyleCombo.Text='blGlyphRight' then
    BitBtn6.Layout := blGlyphRight;
end;
//------------------------------------------------------------------------------
//				EventWatch is a procedure
//				that delete all "old" events
//				and keep the CoEvent "short".
//------------------------------------------------------------------------------
procedure TForm1.EventWatch;
begin
  If CoEvent.Items.Count>6 then
    CoEvent.Items.Delete(0);
end;
//------------------------------------------------------------------------------
//				The OK button on TButton page
//				that accept the resize and .......
//------------------------------------------------------------------------------
procedure TForm1.ButtonSizeChange(Sender : TObject);
begin
  If StrToInt(AHeight.Text) > 190 then
    AHeight.Text := '190';
  If StrToInt(AWidth.Text) > 208 then
    AWidth.Text := '208';
  Button2.Height := StrToInt(AHeight.Text);
  Button2.Width := StrToInt(AWidth.Text);
end;
//------------------------------------------------------------------------------
//				The Clear button on TBitBtn page
//				It will clear the Event Log...
//------------------------------------------------------------------------------
procedure TForm1.ClearEventLog(Sender : TObject);
begin
  CoEvent.Clear;
end;
//------------------------------------------------------------------------------
//            ***************   THIS IS THE EVENTS   ***************
//				Mouse Enter
//------------------------------------------------------------------------------
procedure TForm1.MEnter(Sender : TObject);
begin

{  If Sender = BitBtn1 then
    CoEvent.Items.Add(F+'BitBtn1.OnMouseEnter')
  Else If Sender = BitBtn2 then
    CoEvent.Items.Add(F+'BitBtn2.OnMouseEnter')
  Else If Sender = BitBtn3 then
    CoEvent.Items.Add(F+'BitBtn3.OnMouseEnter')
  Else If Sender = BitBtn4 then
    CoEvent.Items.Add(F+'BitBtn4.OnMouseEnter')
  Else If Sender = BitBtn5 then
    CoEvent.Items.Add(F+'BitBtn5.OnMouseEnter')
  Else If Sender = BitBtn6 then
    CoEvent.Items.Add(F+'BitBtn6.OnMouseEnter')
  Else If Sender = Button1 then
    CoEvent.Items.Add(F+'Button1.OnMouseEnter')
  Else If Sender = btnWriteIni then
    CoEvent.Items.Add(F+'btnWriteIni.OnMouseEnter');
}
  CoEvent.Items.Add(F+TControl(Sender).name+'.OnMouseEnter');

  EventWatch;
end;
//------------------------------------------------------------------------------
//				Mouse Leave
//------------------------------------------------------------------------------
procedure TForm1.MLeave(Sender : TObject);
begin
{  If Sender = BitBtn1 then
    CoEvent.Items.Add(F+'BitBtn1.OnMouseLeave')
  Else If Sender = BitBtn2 then
    CoEvent.Items.Add(F+'BitBtn2.OnMouseLeave')
  Else If Sender = BitBtn3 then
    CoEvent.Items.Add(F+'BitBtn3.OnMouseLeave')
  Else If Sender = BitBtn4 then
    CoEvent.Items.Add(F+'BitBtn4.OnMouseLeave')
  Else If Sender = BitBtn5 then
    CoEvent.Items.Add(F+'BitBtn5.OnMouseLeave')
  Else If Sender = BitBtn6 then
    CoEvent.Items.Add(F+'BitBtn6.OnMouseLeave')
  Else If Sender = Button1 then
    CoEvent.Items.Add(F+'Button1.OnMouseLeave')
  Else If Sender = btnWriteIni then
    CoEvent.Items.Add(F+'btnWriteIni.OnMouseLeave');}

  CoEvent.Items.Add(F+TControl(Sender).name+'.OnMouseLeave');

  EventWatch;
end;
//------------------------------------------------------------------------------
//				Mouse Down
//------------------------------------------------------------------------------
procedure TForm1.MDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
begin
{  If Sender = BitBtn1 then
    CoEvent.Items.Add(F+'BitBtn1.OnMouseDown')
  else if Sender = BitBtn2 then
    CoEvent.Items.Add(F+'BitBtn2.OnMouseDown')
  else if Sender = BitBtn4 then
    CoEvent.Items.Add(F+'BitBtn4.OnMouseDown')
  else if Sender = BitBtn6 then
    CoEvent.Items.Add(F+'BitBtn6.OnMouseDown')
  else if Sender = Button1 then
    CoEvent.Items.Add(F+'Button1.OnMouseDown');
}
  CoEvent.Items.Add(F+TControl(Sender).name+'.OnMouseDown');

  EventWatch;
end;
//------------------------------------------------------------------------------
//				Mouse Move
//------------------------------------------------------------------------------
procedure TForm1.MMove(Sender : TObject; Shift : TShiftState; X,Y : Integer);
begin
//  lblCords.Caption := IntToStr(X)+','+IntToStr(Y);
end;
//------------------------------------------------------------------------------
//				Mouse Up
//------------------------------------------------------------------------------
procedure TForm1.MUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
begin
{  If Sender = BitBtn1 then
    CoEvent.Items.Add(F+'BitBtn1.OnMouseUp')
  else if Sender = BitBtn2 then
    CoEvent.Items.Add(F+'BitBtn2.OnMouseUp')
  else if Sender = BitBtn4 then
    CoEvent.Items.Add(F+'BitBtn4.OnMouseUp')
  else if Sender = BitBtn6 then
    CoEvent.Items.Add(F+'BitBtn6.OnMouseUp')
  else if Sender = Button1 then
   CoEvent.Items.Add(F+'Button1.OnMouseUp');
}
  CoEvent.Items.Add(F+TControl(Sender).name+'.OnMouseUp');

  EventWatch;
end;

procedure TForm1.ChangeFontClick(Sender : TObject);//Change fontdialog create
begin
   FontDialog1 := TFontDialog.Create(Self);
  If FontDialog1.Execute then
  begin
    BitBtn2.Font.Name := FontDialog1.FontName;
    //BitBtn2.Font.Size := FontDialog1.Font.Size;
    //BitBtn2.Font.Color := FontDialog1.Font.Color;
    //BitBtn2.Font.Style := FontDialog1.Font.Style;
  end;
  FontDialog1.Free;
end;

procedure TForm1.LoadGlyphClick(Sender : TObject);//Change GlyphDialog Create
begin
   OpenDialog1 := TOpenDialog.Create(Self);
   OpenDialog1.Filter := '*.xpm';
   If OpenDialog1.Execute then
   begin
     BitMap1.LoadFromXPMFile('OpenDialog1.FileName');
   end;
   OpenDialog1.Free; 
end;

procedure TForm1.WhenClick(Sender : TObject);
begin
  CoProp.Clear;
  If (Sender is TBitBtn) or (Sender is TButton) or (Sender is TCheckBox) then
     CoProperties(TControl(Sender));

end;

procedure TForm1.NextClick(Sender : TObject);
begin
  Inc(FTabLeft);
  If FTabLeft > 17 then
    FTabLeft := 17;
  SlidePage(FTabLeft);
end;
procedure TForm1.PrevClick(Sender : TObject);
begin
  Dec(FTabLeft);
  If FTabLeft < 0 then
    FTabLeft := 0;
  SlidePage(FTabLeft);
end;
procedure TForm1.SlidePage(i : Integer);
begin
  NoteBook1.Left := 0 - TabLeft[i];
  BitBtn1.Left := 10 + TabLeft[i];
  BitBtn2.Left := 10 + TabLeft[i];
  BitBtn3.Left := 10 + TabLeft[i];
  BitBtn4.Left := 10 + TabLeft[i];
  BitBtn5.Left := 10 + TabLeft[i];
  BitBtn6.Left := 10 + TabLeft[i];
  ProgressBar1.Left := 10 + TabLeft[i];
  LoadGlyph.Left := 100 + TabLeft[i];
  ChangeFont.Left := 100 + TabLeft[i];
  ChangeStyleCombo.Left := 100 + TabLeft[i];
  lblProp.Left := 295 + TabLeft[i];
  CoProp.Left := 295 + TabLeft[i];
  lblEvent.Left := 295 + TabLeft[i];
  CoEvent.Left := 295 + TabLeft[i];
  ClearEvents.Left := 485 + TabLeft[i];
  Button1.Left := 10 + TabLeft[i];
  Button2.Left := 10 + TabLeft[i];
  btnOk.Left := 185 + TabLeft[i];
  AWidth.Left := 90 + TabLeft[i];
  AHeight.Left := 135 + TabLeft[i];
  Memo1.Left := 10 + TabLeft[i];
  ListBox1.Left := 10 + TabLeft[i];
  CheckBox1.Left := 10 + TabLeft[i];
  CheckBox2.Left := 10 + TabLeft[i];
  ComboBox1.Left := 10 + TabLeft[i];
  NoteBook2.Left := 10 + TabLeft[i];
  btnColor.Left := 10 + TabLeft[i];
  btnFont.Left := 10 + TabLeft[i];
  btnOpen.Left := 10 + TabLeft[i];
  btnSave.Left := 10 + TabLeft[i];
  lblSection.Left := 10 + TabLeft[i];
  edtSection.Left := 10 + TabLeft[i];
  lblIdent.Left := 10 + TabLeft[i];
  edtIdent.Left := 10 + TabLeft[i];
  lblValue.Left := 10 + TabLeft[i];
  edtValue.Left := 10 + TabLeft[i];
  rbString.Left := 220 + TabLeft[i];
  rbFloat.Left := 220 + TabLeft[i];
  rbInteger.Left := 220 + TabLeft[i];
  rbTime.Left := 220 + TabLeft[i];
  rbDate.Left := 220 + TabLeft[i];
  rbBool.Left := 220 + TabLeft[i];
  btnReadIni.Left := 10 + TabLeft[i];
  btnwriteIni.Left := 167 + TabLeft[i];
  ProgEnable.Left := 220 + TabLeft[i];
  ProgOrient.Left := 220 + TabLeft[i];
  ProgDirect.Left := 220 + TabLeft[i];
  ProgText.Left := 220 + TabLeft[i];
  TrackBar1.Left := 20 + TabLeft[i];
  TrackBar2.Left := 50 + TabLeft[i];
  TrackMemo.Left := 50 + TabLeft[i];
  lblTimer.Left := 10 + TabLeft[i];
  btnForm2Show.Left := 10 + TabLeft[i];
end;

procedure TForm1.SaveSettingsClick(Sender : TObject);
begin
  If SaveSettings.State=cbChecked then
  Begin
    SaveIni.WriteBool('TestAllSettings','SaveSettings',True);
    SaveIni.WriteInteger('TestAllSettings','Pageshow',NoteBook1.PageIndex);
    SaveIni.WriteInteger('TestAllSettings','TabIndex',FTabLeft);
  End Else If SaveSettings.State=cbUnchecked then
  Begin
    SaveIni.WriteBool('TestAllSettings','SaveSettings',False);
    SaveIni.WriteInteger('TestAllSettings','Pageshow',0);
    SaveIni.WriteInteger('TestAllSettings','TabIndex',0);
  End;
end;

procedure TForm1.ReadInifile(Sender : TObject);
begin
  If rbString.Checked=True then
    edtValue.Text := TestIni.ReadString(edtSection.Text,edtIdent.Text,'ERROR');
  If rbFloat.Checked=True then
    edtValue.Text := FloatToStr(TestIni.ReadFloat(edtSection.Text,edtIdent.Text,0));
  If rbInteger.Checked=True then
    edtValue.Text := IntToStr(TestIni.ReadInteger(edtSection.Text,edtIdent.Text,0));
end;

procedure TForm1.WriteIniFile(Sender : TObject);
begin
   TestIni.WriteString(edtSection.Text,edtIdent.Text,edtValue.Text);
end;

procedure TForm1.IniType(Sender : TObject);
begin
  If Sender=rbString then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Author';
    edtValue.Text := TestIni.ReadString('TestAll Application','Author','ERROR');
  End Else If Sender=rbFloat then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Pi';
    edtValue.Text := FloatToStr(TestIni.ReadFloat('TestAll Application','Pi',0));
  End Else If Sender=rbInteger then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Number';
    edtValue.Text := IntToStr(TestIni.ReadInteger('TestAll Application','Number',0));
  End Else If Sender=rbBool then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Life is good';
    If TestIni.ReadBool('TestAll Application','Life is good',False)=False then
      edtValue.Text:='False'
    else edtValue.Text:='True';
  End Else If Sender=rbTime then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Application StartTime';
    edtValue.Text := TimeToStr(TestIni.ReadTime('TestAll Application','Application StartTime',0));
  End Else If Sender=rbDate then begin
    edtSection.Text := 'TestAll Application';
    edtIdent.Text := 'Today';
    edtValue.Text := DateToStr(TestIni.ReadDate('TestAll Application','Today',0));
  End;
end;

procedure TForm1.UpdateProgressBar(Sender : TObject);
begin
  If ProgressBar1.Position > 99 then 
     ProgressBar1.Position := 0;
  ProgressBar1.Position := ProgressBar1.Position + 1;
end;

procedure TForm1.ProgSettings(Sender : TObject);
begin
  If ProgEnable.State = cbChecked then
     ProgTime.Enabled := True
  Else If ProgEnable.State = cbUnchecked then
     ProgTime.Enabled := False;

  If ProgOrient.State = cbChecked then
     ProgressBar1.Orientation := pbVertical
  Else If ProgOrient.State = cbUnchecked then
     ProgressBar1.Orientation := pbHorizontal;

  If (ProgOrient.State = cbChecked) and (ProgDirect.State = cbChecked) then
     ProgressBar1.Orientation := pbTopDown
  Else If (ProgOrient.State = cbUnchecked) and (ProgDirect.State = cbChecked) then
     ProgressBar1.Orientation := pbRightToLeft;

  If ProgText.State = cbChecked then
     ProgressBar1.BarShowText := True
  Else If ProgText.State = cbUnchecked then
     ProgressBar1.BarShowText := False;
end;

procedure TForm1.GivesAHint(Sender : TObject);
begin
  btnWriteIni.Hint := 'Write '+edtValue.Text+' to TestIni.ini';
  MEnter(Sender);
end;

procedure TForm1.TrackBarChange(Sender : TObject);
begin
  If Sender=TrackBar1 then
     TrackMemo.Height := TrackBar1.Position;
  If Sender=TrackBar2 then
     TrackMemo.Width := TrackBar2.Position;
end;

procedure TForm1.Form2Show(Sender : TObject);
begin
  Form2.Show;
end;

Procedure TForm1.DestroyForm(Sender : TObject);
begin
  SaveIni.Free;
  TestIni.Free;
end;
{
  $Log$
  Revision 1.2  2000/08/07 13:17:35  lazarus
  Cleaned up the Testtools.inc file a bit.  Used Sender.name instead of checking each one in the Mouseup event, Mouse down, leave and enter.
  Also changed the WhenClick procedure.

  Shane

  Revision 1.1  2000/07/31 20:33:33  lazarus
  + added "testall" demo provided by <christer.t.johansson@se.abb.com>
  stoppok

}
