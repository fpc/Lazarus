{/***************************************************************************
                               testallform.pp
                             -------------------
                             Example application TestAll
                   Initial Revision  : Fri Jul 14 20:00:00 PDT 2000
                   Author : christer.t.johansson@se.abb.com


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

unit testallform;

{$mode objfpc}
{$H+}


interface

uses classes, forms, buttons, StdCtrls, controls, menus, ExtCtrls,
     ComCtrls, SysUtils, Graphics, Dialogs, Inifiles;

Const
    F : String[7] = ('TForm1.');
    TabLeft : Array[0..17] of Integer = (0,52,104,178,262,338,376,421,499,550,599,655,705,778,861,946,1030,1112);
    // Some poor coding to get around missing component function...
type
   TForm1 = class(TForm)
   private
     { Private Declarations }
     FTabLeft : Integer;
   protected
     { Protected Declarations }
   public
      NoteBook1 : TNoteBook;
      NoteBook2 : TNoteBook;
      BitBtn1 : TBitBtn;
      BitBtn2 : TBitBtn;
      BitBtn3 : TBitBtn;
      BitBtn4 : TBitBtn;
      BitBtn5 : TBitBtn;
      BitBtn6 : TBitBtn;
      Button1 : TButton;
      Button2 : TButton;
      CheckBox1 : TCheckBox;
      CheckBox2 : TCheckBox;
      CheckBox3 : TCheckBox;
      ComboBox1 : TComboBox;
      Form2 : TForm;
      BitMap1 : TBitmap;
      ProgressBar1 : TProgressBar;
      Timer1 : TTimer;
      TrackBar1 : TTrackBar;
      TrackBar2 : TTrackBar;
      //Page1 : TPageControl;
//------- TestTools ------------------------------------
      ChangeFont : TBitBtn;
      LoadGlyph : TBitBtn;
      ClearEvents : TBitBtn;
      CoProp : TListBox;
      lblProp : TLabel;
      CoEvent : TListBox;
      lblEvent : TLabel;
      AWidth : TEdit;
      AHeight : TEdit;
      ACaption : TEdit;
      edtSection : TEdit;
      edtIdent : TEdit;
      edtValue : TEdit;
      btnOK : TBitBtn;
      btnPrev : TBitBtn;
      btnNext : TBitBtn;
      btnColor : TBitBtn;
      btnFont : TBitBtn;
      btnOpen : TBitBtn;
      btnSave : TBitBtn;
      btnReadIni : TBitBtn;
      btnWriteIni : TBitBtn;
      btnForm2Show : TBitBtn;
      lblPages : TLabel;
      lblSection : TLabel;
      lblIdent : TLabel;
      lblValue : TLabel;
      lblTimer : TLabel;
      lblForm2 : TLabel;
      rbString : TRadioButton;
      rbFloat : TRadioButton;
      rbInteger : TRadioButton;
      rbBool : TRadioButton;
      rbTime : TRadioButton;
      rbDate : TRadioButton;
      SaveSettings : TCheckBox;
      ProgEnable : TCheckBox;
      ProgOrient : TCheckBox;
      ProgDirect : TCheckBox;
      ProgText : TCheckBox;
      ChangeStyleCombo : TComboBox;
      SaveIni : TInifile;
      TestIni : TInifile;
      ProgTime : TTimer;
      TrackMemo : TMemo;
      procedure Form2Show(Sender : TObject);
      procedure TrackBarChange(Sender : TObject);
      procedure UpdateProgressBar(Sender : TObject);
      procedure ProgSettings(Sender : TObject);
      procedure IniType(Sender : TObject);
      procedure GivesAHint(Sender : TObject);
      procedure ReadInifile(Sender : TObject);
      procedure WriteIniFile(Sender : TObject);
      procedure SaveSettingsClick(Sender : TObject);
      procedure ChangeFontClick(Sender : TObject);
      procedure LoadGlyphClick(Sender : TObject);
      procedure StyleComboChange(Sender : TObject);
      procedure CoProperties(Sender : TControl);
      procedure NoteBookChangePage(Sender : TObject);
      procedure ClearEventLog(Sender : TObject);
      procedure MEnter(Sender : TObject);
      procedure MLeave(Sender : TObject);
      procedure MDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
      procedure MMove(Sender : TObject; Shift : TShiftState; X,Y : Integer);
      procedure MUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
      procedure EventWatch;
      procedure WhenClick(Sender : TObject);
      procedure BitBtnProp(Sender : TBitBtn);
      procedure ButtonProp(Sender : TButton);
      procedure CheckProp(Sender : TCheckBox);      
      procedure ButtonSizeChange(Sender : TObject);
      procedure LoadTestTools;
      procedure PrevClick(Sender : TObject);
      procedure NextClick(Sender : TObject);
      procedure SlidePage(i : Integer);
     // procedure ChangeKindChange(Sender : TObject);
//-------------------------------------------------------
      SpeedButton1 : TSpeedButton;
      SpeedButton2 : TSpeedButton;
      SpeedButton3 : TSpeedButton;
      SpeedButton4 : TSpeedButton;
      OpenDialog1 : TOpenDialog;
      SaveDialog1 : TSaveDialog;
      FontDialog1 : TFontDialog;
      //FindDialog1 : TFindDialog;
      //ReplaceDialog1 : TReplaceDialog;
      ColorDialog1 : TColorDialog;
      PixMap1 : TPixMap;
      Memo1 : TMemo;
      Time : Integer;
      PopupMenu1 : TPopupMenu;
      Font1 : TMenuItem;
      Color1 : TMenuItem;
      MainMenu1 : TMainMenu;
      File1 : TMenuItem;
      New1 : TMenuItem;
      Open1 : TMenuItem;
      Sep1 : TMenuItem;
      Exit1 : TMenuItem;
      TEdit1 : TMenuItem;
      View1 : TMenuItem;
      Help1 : TMenuItem;
      About1 : TMenuItem;
      ListBox1 : TListBox;
      constructor Create(AOwner: TComponent); override;
      Procedure DestroyForm(Sender : TObject);
      procedure LoadMainMenu;
      procedure LoadComponents;
      procedure ExitClick(Sender : TObject);
      //procedure button1Click(Sender : TObject);
      procedure Speed1Click(Sender : TObject);
      procedure Speed2Click(Sender : TObject);
      procedure Speed3Click(Sender : TObject);
      procedure Speed4Click(Sender : TObject);
      procedure Timer1Timer(Sender : TObject);
      //procedure CheckBox1Click(Sender : TObject);
   end;



var
   Form1 : TForm1;
   S : TFileStream;


implementation

{$I testtools.inc}


//******** Create Form1.TForm1 ********************************************
constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   OnClick := @WhenClick;
   OnDestroy := @DestroyForm;

   //Action
   ActiveControl := Button1;
   Align := alNone;
   //Anchors := Form1.Anchors+[akLeft,akTop];		//Unhandled exeption
   //AutoScroll := True;
   AutoSize := False;
   //BiDiMode := Form1.BiDiMode+[bdLefToRight];
   //BorderIcons := Form1.BorderIcons+[biSystemMenu];
   BorderStyle := bsDialog;
   BorderWidth := 0;
   Caption := 'Test All LCL Components V 0.1';
   ClientHeight := 333;
   ClientWidth := 534;
   Color := clBtnFace;
   //Constraints
   Cursor := crDefault;					//Only crDefault
   //DefaultMonitor := dmActiveForm;
   //DockSite := False;
   DragKind := dkDrag;
   DragMode := dmManual;
   //Enabled := True;		//If set before MainMenu it disappers, before Height SpeedButton disappers, after Hint and it will not show
   //Font.Charset := DEFAULT_CHARSET;
   Font.Color := clBlack;
   Font.Height := -11;
   Font.Name := 'avantgarde';		
   Font.Pitch := fpDefault;
   Font.Size := 10;
   //Font.Style := Form1.Font.Style+[fsBold];		//Access violation
   FormStyle := fsStayOnTop;
   Height := 360;
   //HelpContext := 0;
   //HelpFile
   Hint := 'The TEST Station';
   //HorzScrollBars			//Not been tested yet
   //Icon				//Not been tested yet
   KeyPreview := False;
   Left := 200;
   Menu := MainMenu1;
   Name := 'Form1';
   //ObjectMenuItem := File1;
   //ParentBiDiMode := False;
   ParentFont := True;
   //PixelsPerInch := 96;
   PopupMenu := PopupMenu1;
   Position := poScreenCenter;
   //PrintScale := poProportional;
   //Scaled := True;
   //ShowHint := True;			//Has to be set after Enabled
   Tag := 9;
   Top := 200;
   //UseDockManager := False;
   //VertScrollBar			//Not been tested yet
   //Visible := True;			//2 X Access violation
   Width := 542;
   //WindowMenu := File1;
   WindowState := wsNormal;

   SaveIni := TInifile.Create('Settings.ini');// Save Settings...

   LoadMainMenu;			//Has to load before SpeedButtons
   Enabled := True;			//Has to be set after SpeedButtons
   ShowHint := True;			//Has to be set after Enabled

   //LoadNoteBook;			//ToDo create this

   FTabLeft := SaveIni.ReadInteger('TestAllSettings','TabIndex',0);
   

PopupMenu1 := TPopupMenu.Create(Self);  //Don't work yet, No errors, No show
//PopUpMenu1.TrackButton := tbLeftButton;
PopupMenu1.Name := 'PopupMenu1';
PopupMenu1.AutoPopup := True;
Font1 := TMenuItem.Create(Self);
Font1.Caption := '&Font';
Font1.Enabled := True;
Font1.Visible := True;
PopupMenu1.Items.add(Font1);

//------- NoteBook -----------------
NoteBook1 := TNoteBook.Create(Self);
  With NoteBook1 do
  begin
    OnClick:= @NoteBookChangePage;
    OnMouseMove := @MMove;
    Parent := Self;
    Height := 334;
    Left := 0;
    Top := 50;
    Width := 2000;
    Align := alNone;
    Pages.Strings[0] := 'TBitBtn';	//Pege 0
    Pages.Add('TButton');
    Pages.Add('TCheckBox');
    Pages.Add('TColorDialog');
    Pages.Add('TComboBox');
    Pages.Add('TEdit');			//Page 5
    Pages.Add('TForm');	
    Pages.Add('TFontDialog');			
    Pages.Add('TIniFile');
    Pages.Add('TLabel');
    Pages.Add('TListBox');		//Page 10
    Pages.Add('TMemo');			
    Pages.Add('TNoteBook');	
    Pages.Add('TOpenDialog');	
    Pages.Add('TProgressBar');		
    Pages.Add('TRadioButton');		//Page 15
    Pages.Add('TRadioCroup');		
    Pages.Add('TSaveDialog');
    Pages.Add('TScrollBar');
    Pages.Add('TSpeedButton');
    Pages.Add('TStatusBar');		//Page 20
    Pages.Add('TTimer');
    Pages.Add('TToolBar');
    Pages.Add('TTrackBar');
    PageIndex := SaveIni.ReadInteger('TestAllSettings','Pageshow',0);
    Hint := 'NoteBook';
    ShowHint := True;
    Show;
  end;

NoteBook2 := TNoteBook.Create(Self); // TODO : Add all properties
  With NoteBook2 do
  begin
    //OnClick:= @NoteBookChangePage;
    Parent := NoteBook1.Page[12];
    Height := 146;
    Left := 10;
    Top := 10;
    Width := 270;
    //Align := alNone;
    Pages.Strings[0] := 'Page1';	//Pege 0
    Pages.Add('Page2');
    Pages.Add('Page3');
    PageIndex := 0;
    Hint := 'NoteBook';
    ShowHint := True;
    Show;
  end;


//------- SpeedButton ------------
S := TFileStream.Create('../images/openfile.xpm', fmOpenRead);
  try
    Pixmap1 := TPixmap.Create;
    Pixmap1.TransparentColor := clBtnFace;
    Pixmap1.LoadFromStream(S);
  finally
    S.Free;
  end;

SpeedButton1 := TSpeedButton.Create(Self);
   With SpeedButton1 do
   begin
     OnClick := @Speed1Click;
     Parent := Self;
     Left := 0;
     Top := 27;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     Glyph := Pixmap1;
     Enabled := True;
     Visible := True;
  end;

S := TFileStream.Create('../images/save.xpm', fmOpenRead);
  try
    Pixmap1 := TPixmap.Create;
    Pixmap1.TransparentColor := clBtnFace;
    Pixmap1.LoadFromStream(S);
  finally
    S.Free;
  end;

SpeedButton2 := TSpeedButton.Create(Self);
   With SpeedButton2 do
   begin
     OnClick := @Speed2Click;
     Parent := Self;
     Left := 25;
     Top := 27;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     Glyph := Pixmap1;
     Enabled := True;
     Visible := True;
  end;

S := TFileStream.Create('../images/fonts.xpm', fmOpenRead);
  try
    Pixmap1 := TPixmap.Create;
    Pixmap1.TransparentColor := clBtnFace;
    Pixmap1.LoadFromStream(S);
  finally
    S.Free;
  end;

SpeedButton3 := TSpeedButton.Create(Self);
   With SpeedButton3 do
   begin
     OnClick := @Speed3Click;
     Parent := Self;
     Left := 50;
     Top := 27;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     Glyph := Pixmap1;
     Enabled := True;
     Visible := True;
  end;

S := TFileStream.Create('../images/color.xpm', fmOpenRead);
  try
    Pixmap1 := TPixmap.Create;
    Pixmap1.TransparentColor := clBtnFace;
    Pixmap1.LoadFromStream(S);
  finally
    S.Free;
  end;

SpeedButton4 := TSpeedButton.Create(Self);
   With SpeedButton4 do
   begin
     OnClick := @Speed4Click;
     Parent := Self;
     Left := 75;
     Top := 27;
     Flat := False;
     Hint := 'SpeedButton4';
     Color := clBtnFace;
     ShowHint := True;
     Glyph := Pixmap1;
     Enabled := True;
     Visible := True;
  end;         

   LoadComponents;
   LoadTestTools;
   SaveSettings.Checked := SaveIni.ReadBool('TestAllSettings','SaveSettings',False);
   SlidePage(FTabLeft);
end;

// *************** Loading The MainMenu *****************************
procedure TForm1.LoadMainMenu;
begin
//------- Main Menu ------------------
  MainMenu1 := TMainMenu.Create(Self);
  Menu := MainMenu1;

  File1 := TMenuItem.Create(Self);
  File1.Caption := '&File';
  MainMenu1.Items.Add(File1);

  New1 := TMenuItem.Create(Self);
  New1.Caption := '&New';
  File1.Add(New1);

  Open1 := TMenuItem.Create(Self);
  Open1.Caption := '&Open';
  File1.Add(Open1);

  Sep1 := TMenuItem.Create(Self);
  Sep1.Caption := '-';
  File1.Add(Sep1);

  Exit1 := TMenuItem.Create(Self);
  Exit1.Caption := '&Exit';
  Exit1.OnClick := @ExitClick;
  File1.Add(Exit1);

  TEdit1 := TMenuItem.Create(Self);
  TEdit1.Caption := '&Edit';
  MainMenu1.Items.Add(TEdit1);

  View1 := TMenuItem.Create(Self);
  View1.Caption := '&View';
  MainMenu1.Items.Add(View1);

  Help1 := TMenuItem.Create(Self);
  Help1.Caption := '&Help';
  MainMenu1.Items.Add(Help1);

  About1 := TMenuItem.Create(Self);
  About1.Caption := '&About...';
  //About1.OnClick := AboutClick;
  Help1.Add(About1);

end;


procedure TForm1.LoadComponents;
begin
//------- Memo Component -----------
Memo1 := TMemo.Create(Self);
  With Memo1 do
  begin
    Parent := NoteBook1.Page[11];
    Top := 10;
    Left := 10;
    Height := 132;
    Width := 198;
    Hint := 'Memo Component';
    ShowHint := True;
    WordWrap := True;
    Lines.Text := 'This is a "Test All Component" example, written by Chris... christer.t.johansson@se.abb.com yada yada yada yada...';
    Show;
  end;
//-------- ListBox -----------------
ListBox1 := TListBox.Create(Self);
  With ListBox1 do
  begin
    Parent := NoteBook1.Page[10];
    Top := 10;
    Left := 10;
    Width := 200;
    Height := 132;
    Items.Add('ListBox -> TListBox | TCustomListBox | TWinControl | TControl | TComponent');
    Items.Add('ComboBox');
    Items.Add('BitBtn');
    Items.Add('Timer');
    Items.Add('ProgressBar');
    Items.Add('Button');
    Items.Add('CheckBox');
    Items.Add('MainMenu');
    Items.Add('Memo');
    Items.Add('NoteBook');
    Items.Add('SpeedButton');
    Hint := 'The Components in use...';//Don't work...
    ShowHint := True;                  //Don't work...
    Show;
  end;

//------- BitBtn bkCustom -----------------

//S := TFileStream.Create('./images/custom.bmp', fmOpenRead);
  //try
    BitMap1 := TBitmap.Create;
    //Pixmap1.TransparentColor := clBtnFace;
    //BitMap1.LoadFromXPMFile('/opt/proj/testall/images/custom.xpm');
  //finally
    //S.Free;
  //end;

BitBtn1 := TBitBtn.Create(Self);
  With BitBtn1 do
  begin
    OnClick := @WhenClick;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    //OnMouseMove  := @MMove;
    Parent := NoteBook1.Page[0];
    width := 80;
    left := 10;
    height := 28;


    //Action
    Anchors := BitBtn1.Anchors + [akTop];
    //BiDiMode := bdLeftToRight;	//Identifier not found
    //Cancel := False;			//Identifier not found
    Caption := 'Custom';
    //Constraints := BitBtn1.Constraints//Identifier not found
    Cursor := crHandPoint;		//No function
    Default := False;			//Startup error
    DragCursor := crDrag;
    DragKind := dkDrag;
    DragMode := dmManual;
    Enabled := True;			//No function
    //Font.Charset := DEFAULT_CHARSET;	//Identifier not found
    Font.Color := clBlue;
    Font.Height := -11;
    Font.Name := 'avantgarde';		//No function
    Font.Pitch := fpDefault;
    Font.Size := 10;
    Font.Style := BitBtn1.Font.Style+[fsBold];
    Glyph := BitMap1;
    //height := 28;			Has to be set before Glyph
    //HelpContext := 0;			Identifier not found
    Hint := 'The HINT';
    kind := bkCustom;
    layout := blGlyphLeft;
    //left := 10;			Has to be set before Kind
    //Margin := -1;			Identifier not found
    ModalResult := mrNone;
    Name := 'BitBtn1';
    //NumGlyphs := 1;			Identifier not found
    //ParentBiDiMode := False;		Identifier not found
    ParentFont := True;
    ParentShowHint := False;
    PopupMenu := PopupMenu1;
    ShowHint := True;
    Spacing := 4;
    //Style := bsAutoDetect;		Identifier not found
    TabOrder := 0;
    TabStop := True;
    Tag := 0;
    top := 10;
    Visible := True;
    //width := 80;			Has to be set before Height
  end;
//------- BitBtn bkOk -----------------
BitBtn2 := TBitBtn.Create(Self);
  With BitBtn2 do
  begin
    OnClick := @WhenClick;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    Parent := NoteBook1.Page[0];
    width := 80;
    height := 28;
    left := 10;
    Name := 'BitBtn2';
    top := 43;
    layout := blGlyphLeft;
    kind := bkOk;
    PopupMenu := PopupMenu1;
    Hint := 'BitBtn2 Hint TEST !!!';
    ShowHint := True;
    Show;
  end;
//------- BitBtn bkCancel -----------------
BitBtn3 := TBitBtn.Create(Self);
  With BitBtn3 do
  begin
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    Parent := NoteBook1.Page[0];
    width := 80;
    height := 28;
    left := 10;
    Name := 'BitBtn3';
    top := 76;
    layout := blGlyphLeft;
    kind := bkCancel;
    PopupMenu := PopupMenu1;
    Hint := 'BitBtn3 Hint                             TEST !!!';
    ShowHint := True;
    Show;
  end;
//------- BitBtn bkHelp -----------------
BitBtn4 := TBitBtn.Create(Self);
  With BitBtn4 do
  begin
    OnClick := @WhenClick;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    Parent := NoteBook1.Page[0];
    width := 80;
    height := 28;
    left := 10;
    Name := 'BitBtn4';
    top := 109;
    layout := blGlyphLeft;
    kind := bkHelp;
    PopupMenu := PopupMenu1;
    Hint := 'BitBtn4 Hint'+#10#13+'TEST !!!'+#10#13+'TEST !!!!!';
    ShowHint := True;
    Show;
  end;
//------- BitBtn bkClose -----------------
BitBtn5 := TBitBtn.Create(Self);
  With BitBtn5 do
  begin
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    Parent := NoteBook1.Page[0];
    width := 80;
    height := 28;
    left := 10;
    Name := 'BitBtn5';
    top := 142;
    layout := blGlyphLeft;
    kind := bkClose;
    PopupMenu := PopupMenu1;
    Hint := 'BitBtn5 Hint'+#10#13+'This is a line feed TEST of HINT !!!';
    ShowHint := True;
    Show;
  end;
//------- BitBtn bkAll -----------------
BitBtn6 := TBitBtn.Create(Self);
  With BitBtn6 do
  begin
    OnClick := @WhenClick;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    Parent := NoteBook1.Page[0];
    width := 80;
    height := 50;
    left := 10;
    Name := 'BitBtn6';
    top := 175;
    Tag := 1;
    kind := bkAll;
    PopupMenu := PopupMenu1;
    Hint := 'BitBtn6 Hint';
    ShowHint := True;
    Show;
  end;

//------- Standard Button -------
button1 := TButton.Create(Self);
  With button1 do
  begin
    OnClick := @WhenClick;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    Parent := NoteBook1.Page[1];
    Top := 10;
    Left := 10;
    Height := 26;

    //Action:=nil;
    Anchors:= button1.Anchors+[akLeft];
    //BiDiMode := bdLeftToRight;	//Identifier not found
    //Cancel := False;			//Identifier not found
    Caption := 'Button1';
    //Constraints :=
    Cursor := crDefault;			
    Default := True;
    DragCursor := crDrag;
    DragKind := dkDrag;
    DragMode := dmManual;
    Enabled := True;
    //Font.Charset := DEFAULT_CHARSET	//Identifier not found
    Font.Color := clRed;
    Font.Height := -11;
    Font.Name:= 'adventure';
    Font.Pitch := fpDefault;
    Font.Size := 6;
    Font.Style := Button1.Font.Style+[fsUnderline];
    //Height := 26;			//Has to be set before "Enabled"
    //HelpContext := 0;
    Hint := 'Button Hint';
    //Left := 10;			//Has to be set before "Default"
    ModalResult := mrNone;
    Name := 'Button1';
    //ParentBiDiMode := False;		//Identifier not found
    ParentFont := False;
    ParentShowHint := False;
    PopupMenu := PopupMenu1;
    ShowHint := True;
    TabOrder := 5;
    TabStop := False;
    Tag := 7;
    //Top := 10;			//Has to be set before "Default"
    Visible := True;
    Width := 75;
  end;

button2 := TButton.Create(Self);
  With button2 do
  begin
    OnClick := @WhenClick;
    OnMouseDown  := @MDown;
    OnMouseUp    := @MUp;
    OnMouseEnter := @MEnter;
    OnMouseLeave := @MLeave;
    Parent := NoteBook1.Page[1];
    Top := 66;
    Left := 10;
    Height := 26;
    Width := 208;
    Enabled := True;
    Hint := 'Button2 Hint';
    ShowHint := True;
    Caption := 'Button2';
    Name := 'Button2';
    PopupMenu := PopupMenu1;
    Visible := True;
  end;

//------- CheckBox ---------------
CheckBox1 := TCheckBox.Create(Self);
  With CheckBox1 do
  begin
    OnClick := @WhenClick;
    Parent := NoteBook1.Page[2];
    Left := 10;
    Top := 10;

    //Alignment := taRightJustify;	//Identifier not found
    AllowGrayed := True;
    Anchors := CheckBox1.Anchors+[akLeft];
    //BiDiMode := bdLeftToRight;
    Caption := 'CheckBox1';
    Checked := False;
    Color := clBlue;
    //Constraits
    Cursor := crHandPoint;
    DragCursor := crDrag;
    DragKind := dkDrag;
    DragMode := dmManual;
    Enabled := True;			//Has to be set for Hint to show
    //Font.Charset := DEFAULT_CHARSET;
    Font.Color := clYellow;
    Font.Height := -11;
    Font.Name := 'beast wars';
    Font.Pitch := fpDefault;
    Font.Size := 10;
    Font.Style := CheckBox1.Font.Style+[fsUnderline];
    Height := 24;
    //HelpContext := 0;
    Hint := 'CheckBox1 Hint';
    //Left := 10;			//Has to be set before Cursor
    Name := 'CheckBox1';
    //ParentBiDiMode := False;
    ParentColor := False;
    ParentFont := False;
    ParentShowHint := True;
    PopupMenu := PopupMenu1;
    ShowHint := True;
    State := cbGrayed;
    TabOrder := 2;
    TabStop := False;
    Tag := 7;
    //Top := 10;			//Has to be set before Cursor
    Visible := True;
    Width := 85;
  end;
CheckBox2 := TCheckBox.Create(Self);
  with CheckBox2 do
  begin
    OnClick := @WhenClick;
    OnMouseMove := @MMove;
    Parent := NoteBook1.Page[2];
    Left := 10;
    Top := 35;
    Caption := 'CheckBox2';
//    Checked := True;
//    Color := clBlue;
    Cursor := crHandPoint;		//Has to be set for Hint Show
    DragCursor := crDrag;
//    DragKind := dkDrag;
//    DragMode := dmManual;
    Enabled := False;			//Doesn't work...
    Hint := 'CheckBox2 Hint';
    Name := 'CheckBox2';
//    ParentShowHint := True;
    PopupMenu := PopupMenu1;
    ShowHint := True;
    State := cbChecked;
//    TabOrder := 2;
//    TabStop := False;
    Visible := True;
    Width := 85;
  end;

ComboBox1 := TComboBox.Create(Self);
  With ComboBox1 do
  begin
    //OnChange := @ChangeKindChange;
    Parent := NoteBook1.Page[4];
    Left := 10;
    Top := 28;
    Width := 200;
    Items.Add('TListBox -> stdctrls.pp');
    Items.Add('TComboBox -> stdctrls.pp');
    Items.Add('TBitBtn -> buttons.pp');
    Items.Add('TTimer -> extctrls.pp');
    Items.Add('TProgressBar -> comctrls.pp');
    Items.Add('TButton -> buttons.pp');
    Items.Add('TCheckBox -> stdctrls.pp');
    Items.Add('TMainMenu -> menus.pp');
    Items.Add('TMemo -> stdctrls.pp');
    Items.Add('TNoteBook -> extctrls.pp');
    Hint := 'ComboBox Hint';
    ShowHint := True;
    Show;
  end;

ProgressBar1 := TProgressBar.Create(Self);
   with ProgressBar1 do
   begin
     Parent := NoteBook1.Page[14];
     BarShowText := False;
     Top := 28;
     Left := 10;
     Height := 100;
     Width := 200;
     Min := 0;
     Max := 100;
     Smooth := True;
     Orientation := pbHorizontal;
     Position := 0;
     Enabled := True;
     Hint := 'ProgressBar1';
     ShowHint := True;
     Visible := True;
   end;
TrackBar1 := TTrackBar.Create(Self);
   with TrackBar1 do
   begin
     OnChange := @TrackBarChange;
     Parent := NoteBook1.Page[23];
     Top := 10;
     Left := 20;
     Height := 220;
     Orientation := trVertical;
     Position := 1;
     Max := 204;
     Min := 1;
     Width := 15;
     Name := 'TrackBar1';
     ShowScale := True;
     ScalePos := trTop;
     TickMarks := tmBoth;
     TickStyle := tsAuto;
     Visible := True;
  end;
TrackBar2 := TTrackBar.Create(Self);
   with TrackBar2 do
   begin
     OnChange := @TrackBarChange;
     Parent := NoteBook1.Page[23];
     Top := 239;
     Left := 50;
     Height := 15;
     Orientation := trHorizontal;
     Position := 1;
     Max := 194;
     Min := 1;
     Width := 220;
     Name := 'TrackBar2';
     ShowScale := True;
     ScalePos := trRight;
     TickMarks := tmBoth;
     TickStyle := tsAuto;
     Visible := True;
  end;
Timer1 := TTimer.Create(Self);
   with Timer1 do
   begin
     OnTimer := @Timer1Timer;
     Interval := 1000;
     Enabled := True;
     Name := 'Timer1';
  end;

Form2 := TForm.Create(Self);
  With Form2 do
  begin
    Parent := Self;
    Height := 200;
    Width := 200;
    Caption := 'Form2';
    Position := poScreenCenter;
  end;
end;

//******************** MainMenu Exit ******************************
procedure TForm1.ExitClick(Sender : TObject);
begin
  Close;
end;

procedure TForm1.Speed1Click(Sender : TObject);
begin
  OpenDialog1 := TOpenDialog.Create(Self);
  with OpenDialog1 do
  begin
    Filter := '*.pp';
    Execute;
    Free;
  end;
end;

procedure TForm1.Speed2Click(Sender : TObject);
begin
  SaveDialog1 := TSaveDialog.Create(Self);
  with SaveDialog1 do
  begin
    Filename := 'untitled.txt';
    Execute;
    Free;
  end;
end;

procedure TForm1.Speed3Click(Sender : TObject);
begin
  FontDialog1 := TFontDialog.Create(Self);
  with FontDialog1 do
  begin
    Execute;
    Free;
  end;
end;

procedure TForm1.Speed4Click(Sender : TObject);
begin
  ColorDialog1 := TColorDialog.Create(Self);
  with ColorDialog1 do
  begin
    if Execute then Self.Color := Color;
    Free;
  end;
end;

procedure TForm1.Timer1Timer(Sender : TObject);
begin
  lblTimer.Caption := 'Time: '+Copy(TimeToStr(now),1,8);
end;

end.
{
  $Log$
  Revision 1.1  2000/07/31 20:33:33  lazarus
  + added "testall" demo provided by <christer.t.johansson@se.abb.com>
  stoppok

}
