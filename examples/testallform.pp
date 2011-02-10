{/***************************************************************************
                               testallform.pp
                             -------------------
                             Example application TestAll
                   Initial Revision  : Fri Jul 14 20:00:00 PDT 2000
                   Author : christer.t.johansson@se.abb.com


 ***************************************************************************/

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

unit TestAllForm;

{$mode objfpc}
{$H+}


interface


{$ASSERTIONS ON}


uses Classes, SysUtils, FileUtil, Forms, Buttons, StdCtrls, Controls, Menus,
     ExtCtrls, ComCtrls, GraphType, Graphics, Dialogs, Inifiles, Spin, ClipBrd,
     LCLIntf, LResources;

type
   TForm1 = class(TForm)
   private
      Bevel1            : TBevel;
      BitBtn1           : TBitBtn;
    //Bitmap1           : TBitmap;
      Button1           : Array[0..15] of TButton;
      CheckBox1         : Array[1..35] of TCheckBox;
    //ClipBoard1        : TClipBoard;
      ColorDialog1      : TColorDialog;
      ComboBox1         : TComboBox;
    //DirectoryListBox1 : TDirectoryListBox;
    //DrawGrid1         : TDrawGrid;
    //DriveComboBox1    : TDriveComboBox;
      Edit1             : TEdit;
    //FileListBox1      : TFileListBox;
    //FilterComboBox1   : TFilterComboBox;
    //FindDialog1       : TFindDialog;
      FontDialog1       : TFontDialog;
      GroupBox1         : TGroupBox;
    //Image1            : TImage;
      IniFile1          : TIniFile;
      Label1            : TLabel;
      ListBox1, ListBox2 : TListBox;
      MainMenu1         : TMainMenu;
//+++++++++++++++++++++++++++++ MenuItems +++++++++++++++++++++++++++++++++++++++++++
      File1,  New1,   Open1, Save1,  Sep1,    Quit1,    Settings1, Comps1,  Help1, About1 : TMenuItem;
      EditM,  Event2, Prop2, Sep3  : TMenuItem;
      View1,  Prop1,  Event1       : TMenuItem;
      Color1, Find1,  Font1, Print1, PrintS1, Replace1, Copy1,     Paste1,  Cut1,  Sep2   : TMenuItem;
      AC1,    DF1,    GM1,   NP1,    QS1,     TZ1     : TMenuItem;
      TAppl,  TBev,   TBit,  TBut,   TCan,    TChe,     TClip,     TClis,   TCol,  TCom   : TMenuItem;
      TDir,   TDra,   TDri,  TEdi,   TFile,   TFilt,    TFin,      TFon,    TFor : TMenuItem;
      TGro,   TIma,   TIni,  TLab,   TLis,    TMas,     TMed,      TMem,    TMes : TMenuItem;
      TNot,   TOpe,   TPag,  TPai,   TPan,    TPop,     TPriD,     TPriS,   TPro : TMenuItem;
      TRadB,  TRadG,  TRep,  TRic,   TSav,    TSCre,    TScroBa,   TScroBo, TSha,  TSpee,   TSpinB, TSpinE, TStat, TStri : TMenuItem;
      TTabN,  TTabC,  TThre, TTim,   TTog,    TToo,     TTrac,     TUpD   : TMenuItem;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //MaskEdit1         : TMaskEdit;
    //MediaPlayer1      : TMediaPlayer;
      Memo1             : TMemo;
    //MessageDialog1    : TMessageDialog;
      NoteBook1         : TNoteBook;
      OpenDialog1       : TOpenDialog;
    //PageControl1      : TPageControl;
    //TabSheet1, TabSheet2, TabSheet3 : TTabSheet;
      PaintBox1         : TPaintBox;
      Panel1            : TPanel;
      PixMap1           : TPixMap;
      PopupMenu1        : TPopupMenu;
      Hello             : TMenuItem;
      Doctor            : TMenuItem;
    //FName: TMenuItem;
    //Yesterday: TMenuItem;
    //Tomorrow : TMenuItem;
    //PrintDialog1      : TPrintDialog;
    //PrinterSetupDialog1 : TPrinterSetupDialog;
      ProgressBar1      : TProgressBar;
      RadioButton1      : TRadioButton;
      RadioGroup1       : TRadioGroup;
    //ReplaceDialog1    : TReplaceDialog;
    //RichEdit1         : TRichEdit;
      SaveDialog1       : TSaveDialog;
      ScrollBar1        : TScrollBar;
    //ScrollBox1        : TScrollBox;
    //Shape1            : TShape;
      SpeedButton1,       SpeedButton2, SpeedButton3, SpeedButton4 : TSpeedButton;
    //SpinButton1       : TSpinButton;
      SpinEdit1         : TSpinEdit;
      StatusBar1        : TStatusBar;
    //StringGrid1       : TStringGrid;
    //TabbedNoteBook    : TTabbedNoteBook;
    //TabControl        : TTabControl;
    //Thread1           : TThread;
      Timer1            : TTimer;
      ToggleBox1        : TToggleBox;
      ToolBar1          : TToolBar;
    //ToolButton1       : TToolButton;
      TrackBar1, TrackBar2 : TTrackBar;
    //UpDown1           : TUpDown;
//+++++++++++++ TestTools +++++++++++++++++++++++++++++
      AboutForm          : TForm;
      BenchForm         : Array[1..59] of TForm;
      EventForm         : TForm;
      
      procedure EventFormShow(Sender : TObject);

      procedure EventOnMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
      procedure EventOnMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X,Y : Integer);
      procedure EventOnMouseMove(Sender : TObject; Shift : TShiftState; X,Y : Integer);
      procedure EventOnMouseEnter(Sender : TObject);
      procedure EventOnMouseLeave(Sender : TObject);
      procedure EventOnClick(Sender : TObject);
      procedure EventOnResize(Sender : TObject);
      procedure EventOnShow(Sender : TObject);
      procedure EventOnChange(Sender : TObject);
      procedure EventOnDblClick(Sender : TObject);
      procedure EventOnKeyDown(Sender : TObject; var Key : Word; Shift : TShiftState);
      procedure EventOnKeyUp(Sender : TObject; var Key : Word; Shift : TShiftState);
    private
//+++++++++++++ TApplication ++++++++++++++++++++++++++
      lblExeName : TLabel;
      rdbOk,rdbOkCancel,rdbAbortRetryIgnore,rdbYesNoCancel,rdbYesNo,rdbRetryCancel : TRadioButton;
      btnShowBox : TBitBtn;
      FBoxStyle  : Integer;
      procedure BoxStyle(Sender : TObject);
      procedure ShowBox(Sender : TObject);
    private
//+++++++++++++ TBevel ++++++++++++++++++++++++++++++++
      chbBevelStyle,
      chbBevelShape : TRadiogroup;
      procedure BevelSettings(Sender : TObject);
    private
//+++++++++++++ TButton +++++++++++++++++++++++++++++++
      LCount      : Integer;
      btnCLRLotto : TBitBtn;
      lblLResult  : TLabel;
      procedure LottoClear(Sender : TObject);
      procedure Button1Click(Sender : TObject);
      procedure ButtonTag(Sender : Integer);
    private
//+++++++++++++ TBitBtn +++++++++++++++++++++++++++++++
      cbbKind, cbbAlign : TComboBox;
      btnGlyph : TBitBtn; 
      procedure KindComboChange(Sender : TObject); 
      procedure AlignComboChange(Sender : TObject);
      procedure LoadGlyph(Sender : TObject);
    private
//+++++++++++++ TCanvas +++++++++++++++++++++++++++++++
      FPaint : Boolean;
      Start1, Start2 : Integer;
      cbbPaintType, cbbBrushStyle, cbbPenStyle : TComboBox;
      btnCanvasClear, btnPenColor, btnBrushColor, btnGrad : TBitBtn;
      lblPaintWhat, lblBrushStyle, lblPenStyle : TLabel;
      procedure StyleSelect(Sender : TObject);
      procedure PenStyleSelect(Sender : TObject);
      procedure DrawRect(X1,Y1,X2,Y2 : Integer);
      procedure ClearCanvas(Sender : TObject);
      procedure GradCanvas(Sender : TObject);
    private
//+++++++++++++ TCheckBox +++++++++++++++++++++++++++++
      lblPick, lblCount : TLabel;
      RandCH, CHCount : Integer;
      btnAgain : TBitBtn;
      procedure CheckClick(Sender : TObject);
      procedure AgainClick(Sender : Tobject);
    private
//+++++++++++++ TColorDialog ++++++++++++++++++++++++++
      btnColorSelect : TBitBtn;
      lblColorInt : TLabel;
      procedure ColorSelect(Sender : TObject);
      procedure SelectedColor(Sender : TObject);
    private
//+++++++++++++ TComboBox +++++++++++++++++++++++++++++
      btnCbbAdd : TButton; 
      btnCbbRemove, btnMoveTo : TBitBtn;
      edtCbbAdd : TEdit;
      cbbMoveTo : TComboBox;
      lblRemoved, lblIndex, lblSelLength, lblSelStart, lblSelText : TLabel;
      procedure AddToCbb(Sender : TObject);
      procedure RemoveCbb(Sender : TObject);
      procedure MoveToCbb(Sender : TObject);
    private
//+++++++++++++ TForm +++++++++++++++++++++++++++++++++
      lblWidth, lblHeight, lblCWidth, lblCHeight : TLabel;
      cbbBorder, cbbPosition : TComboBox;
      btnKeyDown : TBitBtn;
      procedure FormBorder(Sender : TObject);
      procedure FormPosition(Sender : TObject);
    private
//+++++++++++++ TFontDialog +++++++++++++++++++++++++++
      lblWhatFont: TLabel;
    //blFontSize: TLabel;
    //lblFontStyle : TLabel;
      btnShowFont : TBitBtn;
      procedure SelectFont(Sender : TObject);
    private
//+++++++++++++ TIniFile ++++++++++++++++++++++++++++++
      edtString, edtFloat, edtInteger, edtDate, edtTime : TEdit;
      lblString, lblFloat, lblInteger, lblDate, lblTime : TLabel;
      btnUpdate : TBitBtn;
      procedure IniUpdate(Sender : TObject);
    private
//+++++++++++++ TLabel ++++++++++++++++++++++++++++++++
      cbbAlignment : TComboBox;
      procedure SelectAlignment(Sender : TObject);
    private
//+++++++++++++ TListBox ++++++++++++++++++++++++++++++
      btnRight, btnLeft, btnAdd, btnCopyToMem, btnSaveFile : TBitBtn;
      memAddTo : TMemo;
      edtAddLis1, edtAddLis2 : TEdit;
      Function AddNotDub(AList: TCustomListBox; nText: String):Boolean;
      procedure ListBoxMove(Sender : TObject);
      procedure CopyToMem(Sender : TObject);
      procedure SaveFile(Sender : TObject);
    private
//+++++++++++++ TMemo +++++++++++++++++++++++++++++++++
      btnNew, btnSave, btnOpen : TBitBtn;
      procedure NewMemo(Sender : TObject);
      procedure SaveMemoAs(Sender : TObject);
      procedure OpenMemo(Sender : TObject);
//+++++++++++++ TOpenDialog +++++++++++++++++++++++++++

    private
//+++++++++++++ TPaintBox +++++++++++++++++++++++++++++
      pntCount : integer;
//+++++++++++++ TPanel ++++++++++++++++++++++++++++++++ 
      Panel2         : TPanel;
      btPanelNorm    : TButton;
      rgPanelInBevel,
      rgPanelOutBevel,
      rgPanelAlign   : TRadioGroup;
      procedure PanelInBevelClick(Sender: TObject);
      procedure PanelOutBevelClick(Sender: TObject);
      procedure PanelAlignClick(Sender: TObject);
      procedure PanelNormClick(Sender:TObject);
    private
//+++++++++++++ TProgressBar ++++++++++++++++++++++++++
      chbProgText, chbProgDirect, chbProgOrient, chbProgEnable, chbSmooth : TCheckBox;
      ProgTime : TTimer;
      procedure ProgSettings(Sender : TObject);
      procedure UpdateProgressBar(Sender : TObject);
//+++++++++++++ TRadioGroup +++++++++++++++++++++++++++
      procedure RadioGroupClick(Sender : TObject);
    private
//+++++++++++++ TScreen +++++++++++++++++++++++++++++++
      lblScreenWidth, lblScreenHeight, lblPixels, lblFormCount : TLabel;
//+++++++++++++ TSpeedButton ++++++++++++++++++++++++++
      SpeedB : Array[0..8] of TSpeedButton;
//+++++++++++++ TSpinEdit +++++++++++++++++++++++++++++
      lblOnChange : TLabel;
//+++++++++++++ TTimer ++++++++++++++++++++++++++++++++
      lblTimeNow, lblHourMinSec, lblMSec : TLabel;
      btnStart : TBitBtn;
      MSec, Hour, Min, Sec : Integer;
      FTrack : Boolean;
      procedure Timer1OnTimer(Sender : TObject);
      procedure TimeTrack(Sender : TObject);
    private
//+++++++++++++ TToggleBox ++++++++++++++++++++++++++++
      lblChecked, lblState : TLabel;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
      procedure CreateMainMenu;
      procedure CreateTestTools;
      procedure TestFormClose(Sender : TObject; var CloseAction : TCloseAction);
      procedure CreateComponents;
      procedure QuitClick(Sender : TObject);
      procedure ShowTestForm(Sender : TObject);
      procedure ShowMessage0(Sender : TObject);
      procedure ShowMessage1(Sender : TObject);
   protected
//+++++++++++++ Virtual MouseEnter/Leave ++++++++++++++
      FFocusControl : TControl;
      procedure ApplicationIdle(Sender : TObject; var Done : Boolean);
      procedure VirOnEnter(Sender: TObject); 
      procedure VirOnLeave(Sender: TObject); 
   protected
//+++++++++++++ About +++++++++++++++++++++++++++++++++
      lblAbout, lblAuthor : TMemo;
      gbAbout : TGroupBox;
      btnBigOk : TBitBtn;
      procedure CloseAbout(Sender : TObject);
      procedure Notification(AComponent: TComponent; Operation : TOperation);override;
   public
      lbEvents: TListBox;
      FLeft : Integer;
      procedure EventWatch;
      Constructor Create(AOwner: TComponent); override;
      Destructor Destroy; override;
   end;

var
   Form1 : TForm1;
   S : TFileStream;

implementation

  function LoadResource(ResourceName:string; PixMap:TPixMap):boolean;
  var 
    ms:TMemoryStream;
    res:TLResource;
  begin
    Result:=false;
    res:=LazarusResources.Find(ResourceName);
    if (res <> nil) then
    begin
      if res.ValueType='XPM' then begin
        ms:=TMemoryStream.Create;
        try
          ms.Write(res.Value[1],length(res.Value));
          ms.Position:=0;
          PixMap.LoadFromStream(ms);
          Result:=true;
        finally
          ms.Free;
        end;
      end;
    end
    else
       //debugln ('TestAll Warning: resource "', ResourceName,'" not found!');
  end;

{$I testtools.inc}

//******** Create Form1.TForm1 ******************************************************
constructor TForm1.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   OnMouseDown    := @EventOnMouseDown;
   OnMouseUp      := @EventOnMouseUp;
 //OnMouseMove    := @EventOnMouseMove;
   OnClick        := @EventOnClick;
 //OnDestroy      := @Destroy;
   OnKeyDown      := @EventOnKeyDown;
   Height         := 300;

 //Action
 //ActiveControl  := btnKeyDown;
   Align          := alNone;
 //Anchors        := Form1.Anchors+[akLeft,akTop];       //Unhandled exeption
 //AutoScroll     := True;
   AutoSize       := False;
 //BiDiMode       := Form1.BiDiMode+[bdLefToRight];
 //BorderIcons    := Form1.BorderIcons+[biSystemMenu];
   BorderStyle    := bsSizeable;
   BorderWidth    := 0;
   Caption        := 'Test All Components V 0.2';
 //ClientHeight   := 333;
 //ClientWidth    := 534;
   Color          := clBtnFace;
 //Constraints
 //Cursor         := crHandPoint;                           //Only crDefault
 //DefaultMonitor := dmActiveForm;
 //DockSite       := False;
   DragKind       := dkDrag;
   DragMode       := dmManual;
 //Enabled        := True;            //If set before MainMenu it disappers, before Height SpeedButton disappers, after Hint and it will not show
 //Font.Charset   := DEFAULT_CHARSET;
   Font.Color     := clBlack;
   Font.Height    := -11;
   Font.Name      := 'avantgarde';		
   Font.Pitch     := fpDefault;
   Font.Size      := 10;
 //Font.Style     := Form1.Font.Style+[fsBold];           //Access violation
   FormStyle      := fsNormal;
 //Height         := 300;
 //HelpContext    := 0;
 //HelpFile
   Hint           := 'The TEST Station';
 //HorzScrollBars                                         //Not been tested yet
 //Icon	                                                  //Not been tested yet
   KeyPreview     := True;
   Left           := 200;
 //Menu           := MainMenu1;
   Name           := 'Form1';
 //ObjectMenuItem := File1;
 //ParentBiDiMode := False;
   ParentFont     := True;
 //PixelsPerInch  := 96;
   PopupMenu      := PopupMenu1;
   Position       := poScreenCenter;
 //PrintScale     := poProportional;
 //Scaled         := True;
 //ShowHint       := True;                   //Has to be set after Enabled
   Tag            := 9;
   Top            := 200;
 //UseDockManager := False;
 //VertScrollBar                            //Not been tested yet
 //Visible        := True;                  //2 X Access violation
   Width          := 403;
 //WindowMenu     := File1;
   WindowState    := wsNormal;

   Enabled := True;			//Has to be set after SpeedButtons
   ShowHint := True;			//Has to be set after Enabled

   LCount := 0;
   FPaint := False;
   Randomize;
   CreateMainMenu;
   CreateComponents;
   CreateTestTools;

   FFocusControl := nil; 
   Application.OnIdle := @ApplicationIdle;
   
End;

procedure TForm1.CreateMainMenu;
begin
  MainMenu1 := TMainMenu.Create(Self);
  MainMenu1.Name := 'MainMenu1';

  Menu := MainMenu1;

  File1 := TMenuItem.Create(Self);
  File1.Caption := '&File';
  MainMenu1.Items.Add(File1);

  New1 := TMenuItem.Create(Self);
  New1.Caption := '&New';
  New1.OnClick := @NewMemo;
  File1.Add(New1);

  Open1 := TMenuItem.Create(Self);
  Open1.Caption := '&Open';
  Open1.OnClick := @OpenMemo;
  File1.Add(Open1);

  Save1 := TMenuItem.Create(Self);
  Save1.Caption := '&Save as...';
  Save1.OnClick := @SaveMemoAs;
  File1.Add(Save1);

  Sep1 := TMenuItem.Create(Self);
  Sep1.Caption := '-';
  File1.Add(Sep1);

  Quit1 := TMenuItem.Create(Self);
  Quit1.Caption := '&Quit';
  Quit1.OnClick := @QuitClick;
  File1.Add(Quit1);

  EditM := TMenuItem.Create(Self);
  EditM.Caption := '&Edit';
  MainMenu1.Items.Add(EditM);

  Copy1 := TMenuItem.Create(Self);
  Copy1.Caption  := 'C&opy    Ctrl+C';
  Paste1 := TMenuItem.Create(Self);
  Paste1.Caption := '&Paste    Ctrl+V';
  Cut1 := TMenuItem.Create(Self);
  Cut1.Caption   := 'C&ut       Ctrl+X';
  Sep2 := TMenuItem.Create(Self);
  Sep2.Caption := '-';
  Find1 := TMenuItem.Create(Self);
  Find1.Caption := '&Find';
  Replace1 := TMenuItem.Create(Self);
  Replace1.Caption := '&Replace';
  EditM.Add(Copy1);
  EditM.Add(Paste1);
  EditM.Add(Cut1);
  EditM.Add(Sep2);
  EditM.Add(Find1);
  EditM.Add(Replace1);

  View1 := TMenuItem.Create(Self);
  View1.Caption := '&View';
  MainMenu1.Items.Add(View1);

  Prop1 := TMenuItem.Create(Self);
  Prop1.Caption := '&Properties';

  Event1 := TMenuItem.Create(Self);
  Event1.Caption := '&EventSnoop';
  Event1.OnClick := @EventFormShow;

  View1.Add(Prop1);
  View1.Add(Event1);

  Settings1 := TMenuItem.Create(Self);
  Settings1.Caption := '&Settings';
  MainMenu1.Items.Add(Settings1);

  Event2 := TMenuItem.Create(Self);
  Event2.Caption := 'E&ventSnoop';
  Prop2 := TMenuItem.Create(Self);
  Prop2.Caption := 'P&roperties';
  Sep3 := TMenuItem.Create(Self);
  Sep3.Caption := '-';
  Color1 := TMenuItem.Create(Self);
  Color1.Caption := '&Color';
  Font1 := TMenuItem.Create(Self);
  Font1.Caption := 'F&ont';
  Print1 := TMenuItem.Create(Self);
  Print1.Caption := '&Print';
  PrintS1 := TMenuItem.Create(Self);
  PrintS1.Caption := 'Printer &Setup';
  Settings1.Add(Event2);
  Settings1.Add(Prop2);
  Settings1.Add(Sep3);
  Settings1.Add(Color1);
  Settings1.Add(Font1);
  Settings1.Add(Print1);
  Settings1.Add(PrintS1);
//++++++++++++++++++++++++++++++++++++ Components +++++++++++++++++++++++++++++++++++
  Comps1 := TMenuItem.Create(Self);
  Comps1.Caption := '&Components';
  MainMenu1.Items.Add(Comps1);

  AC1 := TMenuItem.Create(Self);//++++ A - C
  AC1.Caption := 'A - C';
  Comps1.Add(AC1);
  
  TAppl := TMenuItem.Create(Self);
  TAppl.Caption := 'TApplication';
  TAppl.OnClick := @ShowMessage1;

  TBev := TMenuItem.Create(Self);
  TBev.Caption := 'TBevel';
  TBev.OnClick := @ShowMessage1;

  TBit := TMenuItem.Create(Self);
  TBit.Caption := 'TBitBtn';
  TBit.OnClick := @ShowMessage1;

  TBut := TMenuItem.Create(Self);
  TBut.Caption := 'TButton';
  TBut.OnClick := @ShowMessage1;

  TCan := TMenuItem.Create(Self);
  TCan.Caption := 'TCanvas';
  TCan.OnClick := @ShowMessage1;

  TChe := TMenuItem.Create(Self);
  TChe.Caption := 'TCheckBox';
  TChe.OnClick := @ShowMessage1;

  TClip := TMenuItem.Create(Self);
  TClip.Caption := 'TClipBoard';
  TClip.OnClick := @ShowMessage1;

  TCol := TMenuItem.Create(Self);
  TCol.Caption := 'TColorDialog';
  TCol.OnClick := @ShowMessage1;

  TCom := TMenuItem.Create(Self);
  TCom.Caption := 'TComboBox';
  TCom.OnClick := @ShowMessage1;

  AC1.Add(TAppl);
  AC1.Add(TBev);
  AC1.Add(TBit);
  AC1.Add(TBut);
  AC1.Add(TCan);
  AC1.Add(TChe);
  AC1.Add(TClip);
  AC1.Add(TCol);
  AC1.Add(TCom);

  DF1 := TMenuItem.Create(Self);//++++ D - F
  DF1.Caption := 'D - F';
  Comps1.Add(DF1);

  TDir := TMenuItem.Create(Self);
  TDir.Caption := 'TDirectoryListBox';
  TDir.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TDra := TMenuItem.Create(Self);
  TDra.Caption := 'TDrawGrid';
  TDra.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TDri := TMenuItem.Create(Self);
  TDri.Caption := 'TDriveComboBox';
  TDri.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TEdi := TMenuItem.Create(Self);
  TEdi.Caption := 'TEdit';
  TEdi.OnClick := @ShowMessage1;

  TFile := TMenuItem.Create(Self);
  TFile.Caption := 'TFileListBox';
  TFile.OnClick := @ShowMessage0;         //++ Do not exist yet ++

  TFilt := TMenuItem.Create(Self);
  TFilt.Caption := 'TFilterComboBox';
  TFilt.OnClick := @ShowMessage0;         //++ Do not exist yet ++

  TFin := TMenuItem.Create(Self);
  TFin.Caption := 'TFindDialog';
  TFin.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TFon := TMenuItem.Create(Self);
  TFon.Caption := 'TFontDialog';
  TFon.OnClick := @ShowMessage1;

  TFor := TMenuItem.Create(Self);
  TFor.Caption := 'TForm';
  TFor.OnClick := @ShowMessage1;

  DF1.Add(TDir);
  DF1.Add(TDra);
  DF1.Add(TDri);
  DF1.Add(TEdi);
  DF1.Add(TFile);
  DF1.Add(TFilt);
  DF1.Add(TFin);
  DF1.Add(TFon);
  DF1.Add(TFor);

  GM1 := TMenuItem.Create(Self);//++++ G - M
  GM1.Caption := 'G - M';
  Comps1.Add(GM1);

  TGro := TMenuItem.Create(Self);
  TGro.Caption := 'TGroupBox';
  TGro.OnClick := @ShowMessage1;

  TIma := TMenuItem.Create(Self);
  TIma.Caption := 'TImage';
  TIma.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TIni := TMenuItem.Create(Self);
  TIni.Caption := 'TIniFile';
  TIni.OnClick := @ShowMessage1;

  TLab := TMenuItem.Create(Self);
  TLab.Caption := 'TLabel';
  TLab.OnClick := @ShowMessage1;

  TLis := TMenuItem.Create(Self);
  TLis.Caption := 'TListBox';
  TLis.OnClick := @ShowMessage1;

  TMas := TMenuItem.Create(Self);
  TMas.Caption := 'TMaskEdit';
  TMas.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TMed := TMenuItem.Create(Self);
  TMed.Caption := 'TMediaPlayer';
  TMed.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TMem := TMenuItem.Create(Self);
  TMem.Caption := 'TMemo';
  TMem.OnClick := @ShowMessage1;

  TMes := TMenuItem.Create(Self);
  TMes.Caption := 'TMessageDialog';
  TMes.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  GM1.Add(TGro);
  GM1.Add(TIma);
  GM1.Add(TIni);
  GM1.Add(TLab);
  GM1.Add(TLis);
  GM1.Add(TMas);
  GM1.Add(TMed);
  GM1.Add(TMem);
  GM1.Add(TMes);

  NP1 := TMenuItem.Create(Self);//++++ N - P
  NP1.Caption := 'N - P';
  Comps1.Add(NP1);

  TNot := TMenuItem.Create(Self);
  TNot.Caption := 'TNoteBook';
  TNot.OnClick := @ShowMessage1;

  TOpe := TMenuItem.Create(Self);
  TOpe.Caption := 'TOpenDialog';
  TOpe.OnClick := @ShowMessage1;

  TPag := TMenuItem.Create(Self);
  TPag.Caption := 'TPageControl';
  TPag.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TPai := TMenuItem.Create(Self);
  TPai.Caption := 'TPaintBox';
  TPai.OnClick := @ShowMessage1;

  TPan := TMenuItem.Create(Self);
  TPan.Caption := 'TPanel';
  TPan.OnClick := @ShowMessage1;

  TPop := TMenuItem.Create(Self);
  TPop.Caption := 'TPopupMenu';
  TPop.OnClick := @ShowMessage1;

  TPriD := TMenuItem.Create(Self);
  TPriD.Caption := 'TPrinterDialog';
  TPriD.OnClick := @ShowMessage0;         //++ Do not exist yet ++

  TPriS := TMenuItem.Create(Self);
  TPriS.Caption := 'TPrinterSetupDialog';
  TPriS.OnClick := @ShowMessage0;         //++ Do not exist yet ++

  TPro := TMenuItem.Create(Self);
  TPro.Caption := 'TProgressBar';
  TPro.OnClick := @ShowMessage1;

  NP1.Add(TNot);
  NP1.Add(TOpe);
  NP1.Add(TPag);
  NP1.Add(TPai);
  NP1.Add(TPan);
  NP1.Add(TPop);
  NP1.Add(TPriD);
  NP1.Add(TPriS);
  NP1.Add(TPro);

  QS1 := TMenuItem.Create(Self);//++++ Q - S
  QS1.Caption := 'Q - S';
  Comps1.Add(QS1);

  TRadB := TMenuItem.Create(Self);
  TRadB.Caption := 'TRadioButton';
  TRadB.OnClick := @ShowMessage1;

  TRadG := TMenuItem.Create(Self);
  TRadG.Caption := 'TRadioGroup';
  TRadG.OnClick := @ShowMessage1;

  TRep := TMenuItem.Create(Self);
  TRep.Caption := 'TReplaceDialog';
  TRep.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TRic := TMenuItem.Create(Self);
  TRic.Caption := 'TRichEdit';
  TRic.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TSav := TMenuItem.Create(Self);
  TSav.Caption := 'TSaveDialog';
  TSav.OnClick := @ShowMessage1;

  TScre := TMenuItem.Create(Self);
  TScre.Caption := 'TScreen';
  TScre.OnClick := @ShowMessage1;

  TScroBa := TMenuItem.Create(Self);
  TScroBa.Caption := 'TScrollBar';
  TScroBa.OnClick := @ShowMessage1;

  TScroBo := TMenuItem.Create(Self);
  TScroBo.Caption := 'TScrollBox';
  TScroBo.OnClick := @ShowMessage0;       //++ Do not exist yet ++

  TSha := TMenuItem.Create(Self);
  TSha.Caption := 'TShape';
  TSha.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TSpee := TMenuItem.Create(Self);
  TSpee.Caption := 'TSpeedButton';
  TSpee.OnClick := @ShowMessage1;

  TSpinB := TMenuItem.Create(Self);
  TSpinB.Caption := 'TSpinButton';
  TSpinB.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TSpinE := TMenuItem.Create(Self);
  TSpinE.Caption := 'TSpinEdit';
  TSpinE.OnClick := @ShowMessage1;

  TStat := TMenuItem.Create(Self);
  TStat.Caption := 'TStatusBar';
  TStat.OnClick := @ShowMessage1;

  TStri := TMenuItem.Create(Self);
  TStri.Caption := 'TStringGrid';
  TStri.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  QS1.Add(TRadB);
  QS1.Add(TRadG);
  QS1.Add(TRep);
  QS1.Add(TRic); 
  QS1.Add(TSav);
  QS1.Add(TScre);
  QS1.Add(TScroBa);
  QS1.Add(TScroBo);
  QS1.Add(TSha);
  QS1.Add(TSpee);
  QS1.Add(TSpinB);
  QS1.Add(TSpinE);
  QS1.Add(TStat);
  QS1.Add(TStri);

  TZ1 := TMenuItem.Create(Self);//++++ T - Z
  TZ1.Caption := 'T - Z';
  Comps1.Add(TZ1);

  TTabN := TMenuItem.Create(Self);
  TTabN.Caption := 'TTabbedNoteBook';
  TTabN.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TTabC := TMenuItem.Create(Self);
  TTabC.Caption := 'TTabControl';
  TTabC.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TThre := TMenuItem.Create(Self);
  TThre.Caption := 'TThread';
  TThre.OnClick := @ShowMessage1;

  TTim := TMenuItem.Create(Self);
  TTim.Caption := 'TTimer';
  TTim.OnClick := @ShowMessage1;

  TTog := TMenuItem.Create(Self);
  TTog.Caption := 'TToggleBox';
  TTog.OnClick := @ShowMessage1;

  TToo := TMenuItem.Create(Self);
  TToo.Caption := 'TToolBar';
  TToo.OnClick := @ShowMessage1;

  TTrac := TMenuItem.Create(Self);
  TTrac.Caption := 'TTrackBar';
  TTrac.OnClick := @ShowMessage1;

  TUpD := TMenuItem.Create(Self);
  TUpD.Caption := 'TUpDown';
  TUpD.OnClick := @ShowMessage0;          //++ Do not exist yet ++

  TZ1.Add(TTabN);
  TZ1.Add(TTabC);
  TZ1.Add(TThre);
  TZ1.Add(TTim);
  TZ1.Add(TTog);
  TZ1.Add(TToo);
  TZ1.Add(TTrac);
  TZ1.Add(TUpD);
//++++++++++++++++++++++++++++++++++++ Help +++++++++++++++++++++++++++++++++++++++++
  Help1 := TMenuItem.Create(Self);
  Help1.Caption := '&Help';
  MainMenu1.Items.Add(Help1);

  About1 := TMenuItem.Create(Self);
  About1.Caption := '&About...';
  About1.OnClick := @ShowTestForm;
  Help1.Add(About1);
end;

procedure TForm1.CreateComponents;//++ CREATE COMPONENTS ++++++++++++++++++++++++++++
var
i, CHLeft, CHTop : Integer;
begin
CHLeft:= 25;
CHTop := 25;
CHCount := 0;
For i:=1 to 59 do
begin
  BenchForm[i] := TForm.Create(Self);
    With BenchForm[i] do
    Begin
      OnMouseDown    := @EventOnMouseDown;
      OnMouseUp      := @EventOnMouseUp;
      OnClick        := @EventOnClick;
      OnDblClick     := @EventOnDblClick;
      OnShow         := @EventOnShow;
      OnClose        := @TestFormClose;
      OnKeyDown      := @EventOnKeyDown;
      OnKeyUp        := @EventOnKeyUp;
      ClientHeight   := 254;
      ClientWidth    := 392;
      KeyPreview     := True;
      Height         := 300;
      Width          := 400;
      Caption        := 'TestForm '+IntToStr(i);
      Color          := clBtnFace;
      BorderStyle    := bsDialog;
      Position       := poScreenCenter;
      Left           := 100;
      Top            := 100;
    end;
end;
BenchForm[54].KeyPreview  := True;
BenchForm[19].BorderStyle := bsSizeable;
BenchForm[19].OnMouseMove := @EventOnMouseMove;
BenchForm[5].OnMouseMove := @EventOnMouseMove;
BenchForm[19].OnResize    := @EventOnResize;
BenchForm[19].ActiveControl  := btnKeyDown;
//BenchForm[5].Canvas.Pen.Style := psDash;
//BenchForm[5].Canvas.Brush.Style := bsCross;
//++++++++++++++++++++++++++++++++++ TBevel +++++++++++++++++++++++++++++++++++++++++
Bevel1 := TBevel.Create(Self);
  with Bevel1 do
  begin
    Parent := BenchForm[2];
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;

    Align := alNone;
    Cursor := crArrow;
    Height := 100;
    Hint := 'Bevel1';
    Left := 10;
  //Name := 'Hello';
    ParentShowHint := True;
    Shape := bsBox;
    ShowHint := True;
    Style := bsLowered;
    Tag := 0;
    Top := 10;
    Visible := True;
    Width := 100;
  end;
//++++++++++++++++++++++++++++++++++ TBitBtn1 +++++++++++++++++++++++++++++++++++++++

BitBtn1 := TBitBtn.Create(Self);
  With BitBtn1 do
  begin
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseEnter   := @EventOnMouseEnter;
    OnMouseLeave   := @EventOnMouseLeave;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
  //OnMouseMove    := @EventOnMouseMove;
    Parent	   := BenchForm[3];
    width 	   := 80;
    left 	   := 10;
    height 	   := 50;
    top 	   := 13;
//			******** Delphi 5 Properties *********
  //Action
    Anchors 	   := BitBtn1.Anchors + [akTop]-[akLeft];
  //BiDiMode 	   := bdLeftToRight;	//Identifier not found
  //Cancel 	   := False;		//Identifier not found
    Caption 	   := 'Glyph';
  //Constraints    := BitBtn1.Constraints//Identifier not found
    Cursor 	   := crHandPoint;	//No function
    Default 	   := False;		//Startup error
    DragCursor 	   := crDrag;
    DragKind 	   := dkDrag;
    DragMode 	   := dmManual;
    Enabled 	   := True;		//No function
  //Font.Charset   := DEFAULT_CHARSET;	//Identifier not found
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';	//No function
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := BitBtn1.Font.Style+[fsBold];
    LoadGlyphFromLazarusResource('topendialog');
  //height	   := 28;		Has to be set before Glyph
  //HelpContext    := 0;		Identifier not found
    Hint 	   := 'Settings for BitBtn1 >>';
    kind 	   := bkCustom;
    layout 	   := blGlyphLeft;
  //left	   := 10;		Has to be set before Kind
  //Margin  	   := -1;		Identifier not found
    ModalResult	   := mrNone;
    Name 	   := 'BitBtn1';
  //NumGlyphs 	   := 1;		Identifier not found
  //ParentBiDiMode := False;		Identifier not found
    ParentFont 	   := True;
    ParentShowHint := False;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    Spacing 	   := 2;
  //Style 	   := bsAutoDetect;	Identifier not found
    TabOrder 	   := 0;
    TabStop 	   := True;
    Tag 	   := 0;
  //top 	   := 10;		//Could not be here ??
    Visible 	   := True;
  //width 	   := 80;		Has to be set before Height
  end;
//+++++++++++++++++++++++++++++++++++++ Button ++++++++++++++++++++++++++++++++++++++
Button1[0] := TButton.Create(Self);
  With Button1[0] do
  begin
    Parent         := BenchForm[4];
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
  //OnMouseMove    := @EventOnMouseMove;
    OnMouseEnter   := @EventOnMouseEnter;
    OnMouseLeave   := @EventOnMouseLeave;
    Top            := 30;
    Left           := 30;
    Height         := 25;
    Width          := 25;


  //Action:=nil
  //  Anchors 	   := button1.Anchors+[akLeft];
  //BiDiMode 	   := bdLeftToRight;	//Identifier not found
  //Cancel 	   := False;		//Identifier not found
    Caption 	   := ' ';
  //Constraints    :=
    Cursor 	   := crHandPoint;			
    Default 	   := True;
    DragCursor 	   := crDrag;
    DragKind	   := dkDrag;
    DragMode	   := dmManual;
    Enabled 	   := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
   // Font.Style 	   := Button1.Font.Style+[fsBold];
  //Height 	   := 26;		//Has to be set before "Enabled"
  //HelpContext    := 0;		//Identifier not found
    Hint 	   := 'Lotto';
  //Left 	   := 10;		//Has to be set before "Default"
    ModalResult    := mrNone;
    Name 	   := 'Button1';
  //ParentBiDiMode := False;		//Identifier not found
    ParentFont 	   := False;
    ParentShowHint := False;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    TabOrder	   := 5;
    TabStop 	   := False;
    Tag 	   := 1;
  //Top 	   := 10;		//Has to be set before "Default"
    Visible 	   := True;
  //Width 	   := 75;		//Found out why it has to be set earlier
  end;
For i:=1 to 15 do   // the array of buttons
begin
Button1[i] := TButton.Create(Self);
  With Button1[i] do
  begin
    Parent 	   := BenchForm[4];
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
  //OnMouseMove    := @EventOnMouseMove;
    OnMouseEnter   := @EventOnMouseEnter;
    OnMouseLeave   := @EventOnMouseLeave;
    If (i>3) and (i<8) then
    begin
      Top := Button1[0].Top+25;
      Left := Button1[i-4].Left;
    End else
    begin
      Top := 30;  
      Left 	   := Button1[i-1].Left+25;
    end;
    If (i>7) and (i<12) then
    begin
      Top := Button1[0].Top+50;
      Left := Button1[i-4].Left;
    End;
    If i>11 then
    begin
      Top := Button1[0].Top+75;
      Left := Button1[i-4].Left;
    End;

    Height 	   := 25;
    Width 	   := 25;


  //Action:=nil
  //  Anchors 	   := button1.Anchors+[akLeft];
  //BiDiMode 	   := bdLeftToRight;	//Identifier not found
  //Cancel 	   := False;		//Identifier not found
    Caption 	   := ' ';
  //Constraints    :=
    Cursor 	   := crHandPoint;			
    Default 	   := True;
    DragCursor 	   := crDrag;
    DragKind	   := dkDrag;
    DragMode	   := dmManual;
    Enabled 	   := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
   // Font.Style 	   := Button1.Font.Style+[fsBold];
  //Height 	   := 26;		//Has to be set before "Enabled"
  //HelpContext    := 0;		//Identifier not found
    Hint 	   := 'Lotto';
  //Left 	   := 10;		//Has to be set before "Default"
    ModalResult    := mrNone;
    //Name 	   := 'Lotto';
  //ParentBiDiMode := False;		//Identifier not found
    ParentFont 	   := False;
    ParentShowHint := False;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    TabOrder	   := 5;
    TabStop 	   := False;
    Tag 	   := i+1;
  //Top 	   := 10;		//Has to be set before "Default"
    Visible 	   := True;
  //Width 	   := 75;		//Found out why it has to be set earlier
end;
end;
//+++++++++++++++++++++++++++++++++++++ TCheckBox +++++++++++++++++++++++++++++++++++
For i:=1 to 35 do
Begin
CheckBox1[i] := TCheckBox.Create(Self);
  with CheckBox1[i] do
  begin
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
  //OnMouseMove    := @EventOnMouseMove;
    Parent         := BenchForm[6];

  //Action
  //Alignment      := taRigthJustify;
    AllowGrayed    := True;
    Anchors        := CheckBox1[i].Anchors + [akTop, akLeft];
  //BiDiMode       := bdLeftToRight;
    Caption        := IntToStr(i);
    Checked        := False;
    Color          := clBlue;
  //Constraints
    Cursor         := crDefault;  //Error if not crDefault
    DragCursor     := crDrag;
    DragKind       := dkDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := CheckBox1[i].Font.Style+[fsBold];
    Height         := 20;
  //HelpContext    := 0;
    Hint           := 'Maybe this one';
    Left           := CHLeft;
    Name           := 'CheckBox1'+IntToStr(i);
  //ParentBiDiMode := False;
    ParentColor    := True;
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowHint       := True;
    State          := cbUnchecked;
    TabOrder       := 10;
    Tag            := 0;   
    Top            := CHTop;
    Visible        := True;    
    Width          := 35;
  end;
  CHLeft:=CHLeft+35;
  If (i = 5) or (i = 10) or (i = 15) or (i = 20) or (i = 25) or (i = 30) then
  begin
    CHLeft:= 25;
    CHTop:= CHTop + 20;
  end;
RandCH := 0;
While RandCH < 1 do
  RandCH := Random(35);  
end;
//+++++++++++++++++++++++++++++++++++++ Clipboard1 ++++++++++++++++++++++++++++++++++
//ClipBoard1 := TClipBoard.Create; //BenchForm[7]  Don´t know how to use !?!
//+++++++++++++++++++++++++++++++++++++ CListBox1 +++++++++++++++++++++++++++++++++++

//+++++++++++++++++++++++++++++++++++++ ColorDialog1 ++++++++++++++++++++++++++++++++
ColorDialog1 := TColorDialog.Create(Self); //BenchForm[9]
//+++++++++++++++++++++++++++++++++++++ ComboBox1 +++++++++++++++++++++++++++++++++++
ComboBox1 := TComboBox.Create(Self);
  with ComboBox1 do
  begin
    Parent         := BenchForm[10];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    OnChange       := @EventOnChange;

    Anchors 	   := ComboBox1.Anchors + [akTop,akLeft];
  //BiDiMode 	   := bdLeftToRight;	//Identifier no found
    Color 	   := clRed;
  //Constraints    :=			//Identifier no found
    Cursor 	   := crDefault;	//Only crDefault
    DragCursor 	   := crDrag;
    DragKind	   := dkDrag;
    DragMode	   := dmManual;
  //DropDownCount  := 8;
    Enabled	   := True;
  //Font.Charset   := DEFAULT_CHARSET;	//Identifier not found
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';	//No function
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := ComboBox1.Font.Style+[fsItalic];
    Height 	   := 32;
  //HelpContext	   := 0;		//Identifier not found
    Hint 	   := 'Components';	//Don't work...
  //ImeMode	   := imDontCare;
  //ImeName
  //IntegralHeight := True;
  //ItemHeight	   := 13;
    Items.Add('HELLO');
    Items.Add('DOCTOR');
    Items.Add('YESTERDAY');
    Items.Add('TOMORROW');
    Items.Add('MY GOD!');
    Items.Add('IT´S FULL OF STARS !!');
    Items.Add('HAL9000 : ERROR');
    Left 	   := 10;
    Name 	   := 'ComboBox1';
  //ParentBiDiMode := True;		//Identifier not found
    ParentColor	   := False;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;		//Don't work...
    Sorted	   := False;		//If true Access violation
    Style	   := csDropDown;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Text           := 'Hello there !!';
    Top 	   := 10;
    Visible	   := True;
    Width 	   := 160;
  end;
//+++++++++++++++++++++++++++++++++++++ DirectoryListBox1 +++++++++++++++++++++++++++
{DirectoryListBox1 := TDirectoryListBox.Create(Self);
  With DirectoryListBox1 do
  begin
    Parent         := BenchForm[11];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;

    Align          := alNone;
  //BiDiMode 	   := bdLeftToRight;
    Color 	   := clRed;
  //Columns 	   := 0;
  //Constraints    :=
  //Ctl3D          := True;
    Cursor 	   := crDefault;
  //DirLabel       := 
    DragCursor 	   := crDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
  //FileList       :=
  //Font.Charset   := DEFAULT_CHARSET;
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := ListBox1.Font.Style+[fsItalic];
    Height 	   := 196;
  //HelpContext	   := 0;
    Hint 	   := 'DirectoryListBox1';
  //ImeMode	   := imDontCare;
  //ImeName
  //IntegralHeight := True;
  //ItemHeight	   := 13;
    Left 	   := 10;
    MultiSelect	   := True;
    Name 	   := 'DirectoryListBox1';
  //ParentBiDiMode := True;
    ParentColor	   := False;
  //ParnetCtl3D    := True;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Top 	   := 35;
    Visible	   := True;
    Width 	   := 150;
  end;}
//+++++++++++++++++++++++++++++++++++++ DrawGrid1 +++++++++++++++++++++++++++++++++++
{DrawGrid1 := TDrawGrid.Create(Self);
  With DrawGrid1 do
  begin
    Parent           := BenchForm[12];
    OnClick          := @EventOnClick;
    OnMouseDown      := @EventOnMouseDown;
    OnMouseUp        := @EventOnMouseUp;
    OnMouseMove      := @EventOnMouseMove;

    Align            := alNone;
  //BiDiMode         := bdLeftToRight;
    BorderStyle      := bsSingle;
    ColCount         := 5;
    Color 	     := clWindow;
  //Ctl3D            := True;
    Cursor           := crDefault;
    DefaultColWidth  := 64;
    DefaultDrawing   := True;
    DefaultRowHeight := 24;
    DragCursor       := crDrag;
    DragMode         := dmManual;
    Enabled          := True;
    FixedColor       := clBtnFace;
    FixedCols        := 1;
    FixedRows        := 1;
  //Font.Charset     := DEFAULT_CHARSET;
    Font.Color       := clBlue;
    Font.Height      := -11;
    Font.Name        := 'avantgarde';
    Font.Pitch       := fpDefault;
    Font.Size        := 10;
    Font.Style       := DrawGrid1.Font.Style+[fsItalic];
    GridLineWidth    := 1;
    Height           := 196;
  //HelpContext      := 0;
    Hint             := 'DrawGrid1';
    Left             := 10;
    Name             := 'DrawGrid1';
  //Options
    ParentColor      := False;
  //ParentCtl3D      := True;
    ParentFont       := True;
    ParentShowHint   := True;
    PopupMenu        := PopupMenu1;
    RowCount         := 5;
    ScrollBars       := ssBoth;
    ShowHint         := True;
    TabOrder         := 0;
    TabStop          := False;
    Tag              := 9;
    Top              := 10;
    Visible          := True;
    Width            := 300;
  end;}
//+++++++++++++++++++++++++++++++++++++ DriveComboBox1 +++++++++++++++++++++++++++++++
{DriveComboBox1 := TDriveComboBox.Create(Self);
  with DriveComboBox1 do
  begin
    Parent         := BenchForm[13];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    OnChange       := @EventOnChange;

  //BiDiMode 	   := bdLeftToRight;
    Color 	   := clWindow;
  //Ctl3D          := True;
  //Constraints    :=
    Cursor 	   := crDefault;
  //DirList        :=
    DragCursor 	   := crDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
  //Font.Charset   := DEFAULT_CHARSET;
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := DriveComboBox1.Font.Style+[fsItalic];
    Height 	   := 32;
  //HelpContext	   := 0;
    Hint 	   := 'DriveComboBox1';
  //ImeMode	   := imDontCare;
  //ImeName
    Left 	   := 10;
    Name 	   := 'DriveComboBox1';
  //ParentBiDiMode := True;
    ParentColor	   := False;
  //ParenCtl3D     := True;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    TextCase       := tcLowerCase;
    Top 	   := 10;
    Visible	   := True;
    Width 	   := 160;
  end;}
//+++++++++++++++++++++++++++++++++++++ Edit1 +++++++++++++++++++++++++++++++++++++++
Edit1 := TEdit.Create(Self);
  With Edit1 do
  begin
    Parent         := BenchForm[14];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    //OnMouseMove    := @EventOnMouseMove;
    OnKeyDown      := @EventOnKeyDown;

  //AutoSelect     := True;
    AutoSize       := True;
    BorderStyle    := bsSingle;
  //CharCase       := ecNormal;
    Color          := clRed;
  //Ctl3D          := True;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET;
    Font.Color     := clBlue;
    Font.Height    := -11;
    Font.Name      := 'avantgarde';
    Font.Pitch     := fpDefault;
    Font.Size      := 10;
    Font.Style     := Edit1.Font.Style+[fsItalic];
    Height         := 21;
  //HelpContext	   := 0;
    Hint 	   := 'Edit1';
  //ImeMode        := imDontCare;
  //ImeName
    Left           := 10;
  //MaxLength      := 0;
    Name           := 'Edit1';
  //OEMConvert     := False;
    ParentColor    := False;
    ParentFont     := True;
    ParentShowHint := True;
  //PasswordChar   := #0;
    PopupMenu      := PopupMenu1;
    ReadOnly       := False;
    ShowHint       := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Top 	   := 10;
    Visible	   := True;
    Width 	   := 150;
  end;
//+++++++++++++++++++++++++++++++++++++ FileListBox1 ++++++++++++++++++++++++++++++++
{FileListBox1 := TFileListBox.Create(Self);
  With FileListBox1 do
  begin
    Parent         := BenchForm[15];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;

    Align          := alNone;
    Anchors        := FileListBox1.Anchors + [akTop,akLeft];
    BorderStyle    := bsSingle;
    Color          := clWindow;
  //Ctl3D          := True;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
    ExtendedSelect := True;
    FileEdit       := Edit1;
    FileType       := FileListBox1.FileType + [ftNormal];
    Font.Charset   := DEFAULT_CHARSET;
    Font.Color     := clBlue;
    Font.Height    := -11;
    Font.Name      := 'avantgarde';
    Font.Pitch     := fpDefault;
    Font.Size      := 10;
    Font.Style     := FileListBox1.Font.Style+[fsItalic];
    Height         := 196;
    HelpContext    := 0;
    Hint           := 'FileListBox1';
    ImeMode        := imDontCare;
    ImeName
    IntegralHeight := True;
    ItemHeight     := 13;
    Left           := 10;
    Mask           := '*.*';
    MultiSelect    := True;
    Name           := 'FileListBox1';
    ParentColor	   := False;
  //ParentCtl3D    := True;
    ParentFont     := True;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowGlyphs     := True;
    ShowHint       := True;
    TabOrder       := 0;
    TabStop        := False;
    Tag            := 9;
    Top            := 10;
    Visible        := True;
    Width          := 150;
  end;}
//+++++++++++++++++++++++++++++++++++++ FilterComboBox1 +++++++++++++++++++++++++++++
{FilterComboBox1 := TFilterComboBox.Create(Self);
  with FilterComboBox1 do
  begin
    Parent         := BenchForm[16];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    OnChange       := @EventOnChange;

    Color 	   := clWindow;
  //Ctl3D          := True;
    Cursor 	   := crDefault;
    DragCursor 	   := crDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
    FileList       := FileListBox1;
    Filter         := 'All Files(*.*)|*.*';
    Font.Charset   := DEFAULT_CHARSET;
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := FilterComboBox1.Font.Style+[fsItalic];
    Height 	   := 32;
    HelpContext	   := 0;
    Hint 	   := 'FilterDriveComboBox1';
    ImeMode	   := imDontCare;
    ImeName
    Left 	   := 10;
    Name 	   := 'FilterComboBox1';
    ParentColor	   := False;
  //ParenCtl3D     := True;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    TextCase       := tcLowerCase;
    Top 	   := 10;
    Visible	   := True;
    Width 	   := 160;
  end;}
//+++++++++++++++++++++++++++++++++++++ FindDialog1 +++++++++++++++++++++++++++++++++
//FindDialog1 := TFindDialog.Create(Self); //BenchForm[17]
//+++++++++++++++++++++++++++++++++++++ FontDialog1 +++++++++++++++++++++++++++++++++
FontDialog1 := TFontDialog.Create(Self);  //BenchForm[18]
//+++++++++++++++++++++++++++++++++++++ Form1 +++++++++++++++++++++++++++++++++++++++
// Allready created !! BenchForm[19]
//+++++++++++++++++++++++++++++++++++++ GroupBox1 +++++++++++++++++++++++++++++++++++
GroupBox1 := TGroupBox.Create(Self);
  with GroupBox1 do
  begin
    Parent := BenchForm[20];
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    Left := 10;
    Top := 10;
    Width := 300;
    Height := 150;
  //Ctl3D := False;
    Caption := 'GroupBox1';
    Visible := True;
  end;
//++++++++++++++++++++++++++++++++++++++ Image1 +++++++++++++++++++++++++++++++++++++
{Image1 := TImage.Create(Self);
  With Image1 do
  begin
    Parent             := BenchForm[21];
    OnClick            := @EventOnClick;
    OnMouseDown        := @EventOnMouseDown;
    OnMouseUp          := @EventOnMouseUp;
    OnMouseMove        := @EventOnMouseMove;

    Align              := alNone;
    Anchors            := Image1.Anchors + [akTop,akLeft];
    AutoSize           := True;
    Center             := False;    
    Cursor             := crDefault;
    DragCursor         := crDrag;
    DragMode           := dmManual;
    Enabled            := True;
    Height             := 196;
    Hint               := 'Image1';
    IncrementalDisplay := True;
    Left               := 10;
    Name               := 'Image1';
    ParentShowHint     := True;
    Picture            := '../images/penguin.xpm';
    PopupMenu          := PopupMenu1;
    ShowHint           := True;
    Stretch            := False;
    Tag                := 21;
    Top                := 10;
    Transparent        := False;
    Visible            := True;
    Width              := 150;
  end;}
//++++++++++++++++++++++++++++++++++++++ Inifile1 +++++++++++++++++++++++++++++++++++
IniFile1 := TIniFile.Create('./Settings.laz');  //BenchForm[22]
//++++++++++++++++++++++++++++++++++++++ Label1 +++++++++++++++++++++++++++++++++++++
Label1 := TLabel.Create(Self);
  With Label1 do
  begin
    Parent         := BenchForm[23];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    Top            := 10;
    Left           := 10;

    Align          := alNone;
    Alignment      := taLeftJustify;
    AutoSize       := False;
    Caption        := 'Label1';
    Color          := clBtnFace;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //FocusControl   := nil;
  //Font.Charset   := DEFAULT_CHARSET;
    Font.Color     := clBlue;
    Font.Height    := -11;
    Font.Name      := 'avantgarde';
    Font.Pitch     := fpDefault;
    Font.Size      := 10;
    Font.Style     := Label1.Font.Style+[fsItalic];
    Height         := 100;
    Hint 	   := 'Label1';
    Layout         := tlTop;
  //Left           := 10;  //Has to be set before Layout
    Name           := 'Label1';
    ParentColor    := False;
    ParentFont     := True;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
  //ShowAccelChar  := True;
    ShowHint       := True;
    Tag            := 0;
  //Top            := 10;  //Has to be set before Layout
  //Transparent    := False;
    Visible	   := True;
    Width          := 150;
    WordWrap       := False;
  end;
//++++++++++++++++++++++++++++++++++++++ ListBox1, ListBox2 +++++++++++++++++++++++++
ListBox1 := TListBox.Create(Self);
  With ListBox1 do
  begin
    Parent         := BenchForm[24];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;

    Align          := alNone;
    Anchors 	   := ListBox1.Anchors + [akTop,akLeft];
  //BiDiMode 	   := bdLeftToRight;	//Identifier no found
    BorderStyle    := bsSingle;
    Color 	   := clRed;
  //Columns 	   := 0;		//Identifier no found
  //Ctl3D          := False;
  //Constraints    :=			//Identifier no found
    Cursor 	   := crDefault;	//Only crDefault
    DragCursor 	   := crDrag;
    DragKind	   := dkDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
    ExtendedSelect := True;
  //Font.Charset   := DEFAULT_CHARSET;	//Identifier not found
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';	//No function
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := ListBox1.Font.Style+[fsItalic];
    Height 	   := 148;
  //HelpContext	   := 0;		//Identifier not found
    Hint 	   := 'Components';	//Don't work...
  //ImeMode	   := imDontCare;
  //ImeName
  //IntegralHeight := True;
  //ItemHeight	   := 13;
    Items.Add('BitBtn');
    Items.Add('Button');
    Items.Add('CheckBox');
    Items.Add('ColorDialog');
    Items.Add('ComboBox');
    Items.Add('Edit');
    Items.Add('Form');
    Items.Add('FontDialog');
    Items.Add('IniFile');
    Items.Add('Label');
    Items.Add('ListBox');
    Items.Add('MainMenu');
    Items.Add('Memo');
    Items.Add('NoteBook');
    Items.Add('OpenDialog');
    Items.Add('ProgressBar');
    Items.Add('RadioButton');
    Items.Add('RadioGroup');
    Items.Add('SaveDialog');
    Items.Add('SpeedButton');
    Left 	   := 10;
    MultiSelect	   := True;
    Name 	   := 'ListBox1';
  //ParentBiDiMode := True;		//Identifier not found
    ParentColor	   := False;
  //ParentCtl3D    := True;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;		//Don't work...
    Sorted	   := False;		//If true Access violation
    Style	   := lbStandard;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Top 	   := 35;
    Visible	   := True;
    Width 	   := 150;
  end;

ListBox2 := TListBox.Create(Self);
  With ListBox2 do
  begin
    Parent 	   := BenchForm[24];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    //OnMouseUp	   := @MUp;

    Anchors 	   := ListBox2.Anchors + [akTop,akLeft];
    BorderStyle    := bsSingle;
    Color 	   := clRed;
    Cursor 	   := crDefault;	//Only crDefault
    DragCursor 	   := crDrag;
    DragKind	   := dkDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
    ExtendedSelect := True;
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';	//No function
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style 	   := ListBox2.Font.Style+[fsItalic];
    Height 	   := 148;
    Hint 	   := 'Components';	//Don't work...
    Left 	   := 235;
    MultiSelect	   := True;
    Name 	   := 'ListBox2';
    ParentColor	   := False;
    ParentFont	   := True;
    ParentShowHint := True;
    PopupMenu 	   := PopupMenu1;
    ShowHint 	   := True;		//Don't work...
    Sorted	   := False;		//If true Access violation
    Style	   := lbStandard;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Top 	   := 35;
    Visible	   := True;
    Width 	   := 150;
  end;
//+++++++++++++++++++++++++++++++++++++ MainMenu1 +++++++++++++++++++++++++++++++++++
// Created in own procedure
//+++++++++++++++++++++++++++++++++++++ MaskEdit1 +++++++++++++++++++++++++++++++++++
{MaskEdit1 := TMaskEdit.Create(Self);
  With MaskEdit1 do
  begin
    Parent         := BenchForm[25];
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;

    AutoSelect     := True;
    AutoSize       := True;
    BorderStyle    := bsSingle;
    CharCase       := ecNormal;
    Color          := clWindow;
  //Ctl3D          := True;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    EditMask
    Enabled        := True;
    Font.Charset   := DEFAULT_CHARSET;
    Font.Color     := clBlue;
    Font.Height    := -11;
    Font.Name      := 'avantgarde';
    Font.Pitch     := fpDefault;
    Font.Size      := 10;
    Font.Style     := MaskEdit1.Font.Style+[fsItalic];
    Height         := 21;
    HelpContext	   := 0;
    Hint 	   := 'MaskEdit1';
    ImeMode        := imDontCare;
    ImeName
    Left           := 10;
    MaxLength      := 0;
    Name           := 'MasEdit1';
    ParentColor    := False;
  //ParentCtl3D    := True;
    ParentFont     := True;
    ParentShowHint := True;
    PasswordChar   := #0;
    PopupMenu      := PopupMenu1;
    ReadOnly       := False;
    ShowHint       := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 9;
    Text           := 'MaskEdit1';
    Top 	   := 10;
    Visible	   := True;
    Width 	   := 150;
  end;}
//+++++++++++++++++++++++++++++++++++++ MediaPlayer1 ++++++++++++++++++++++++++++++++
{MediaPlayer1 := TMediaPlayer.Create(Self);
  With MediaPlayer1 do
  begin
    Parent         := BenchForm[26];

    AutoEnable     := True;
    AutoOpen       := False;
    AutoRewind     := True;
    ColoredButtons := ColoredButtons + [btPlay, btStop];
    Cursor         := crDefault;
    DeviceType     := dtAutoSelect;
    Display        := MediaPlayer1;
    Enabled        := True;
    EnabledButtons := EnabledButtons + [btPlay, btStop];
    FileName       := '';
    Height         := 30;
    HelpContext    := 0;
    Hint           := 'MediaPlayer1';
    Left           := 10;
    Name           := 'MediaPlayer1';
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    Shareable      := False;
    ShowHint       := True;
    TabOrder       := 0;
    TabStop        := False;
    Tag            := 0;
    Top            := 10;
    Visible        := True;
    VisibleButtons := VisibleButtons + [btPlay, btStop];
    Width          := 57;
  end;}
//+++++++++++++++++++++++++++++++++++++ Memo1 +++++++++++++++++++++++++++++++++++++++
Memo1 := TMemo.Create(Self);
  With Memo1 do
  begin
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnChange       := @EventOnChange;
    Parent 	   := Self;

    Align 	   := alNone;
    Anchors	   := Memo1.Anchors + [akLeft,akTop];
  //Alignment	   := taLeftJustify;
    BorderStyle	   := bsSingle;
    Color	   := clYellow;
  //Ctl3D	   := False;
    Cursor	   := crDefault;
    DragCursor	   := crDrag;
    DragMode	   := dmManual;
    Enabled	   := True;
    Font.Color 	   := clBlue;
    Font.Height    := -11;
    Font.Name 	   := 'avantgarde';	//No function
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 10;
    Font.Style     := Memo1.Font.Style+[fsBold];
    Height 	   := 200;
  //HelpContext	   := 0;
  //HideSelection  := True;
    Hint 	   := 'Memo Component';
  //ImeMode	   := imDontCare;
  //ImeName
    Left 	   := 10;
    Lines.Text     := #13#10+'                                     WELCOME'+#13#10+
                      'You will find the component you want to test in the MainMenu'+
                      ' "Components". 60 components are mentioned, but all do not exist yet.'+
                      ' I hope this application will be useful...'+#13#10+'Chris';
  //MaxLength	   := 0;
    Name	   := 'Memo1';
  //OEMConvert	   := False;
    ParentColor	   := False;
  //ParentCtl3D	   := False;
    ParentFont	   := False;
    ParentShowHint := True;
    PopupMenu	   := PopupMenu1;
    ReadOnly	   := False;
    ScrollBars	   := ssBoth;
    ShowHint 	   := True;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 0;    
    Top 	   := 53;
    Visible	   := True;
  //WantReturns	   := True;
  //WantTabs	   := False;
    Width 	   := 380;
    WordWrap 	   := True;
  end;
//++++++++++++++++++++++++++++++++++++ MessageDialog1 +++++++++++++++++++++++++++++++
//MessageDialog := TMessageDialog.Create(Self);
//++++++++++++++++++++++++++++++++++++ TNoteBook ++++++++++++++++++++++++++++++++++++
NoteBook1 := TNoteBook.Create(Self); // TODO : Add all properties
  With NoteBook1 do
  begin
    OnClick        := @EventOnClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;
    Parent := BenchForm[29];
    Height := 190;
    Left := 10;
    Top := 10;
    Width := 377;
    Name := 'NoteBook1';
    Align := alNone;
    if PageCount>0 then
      Pages.Strings[0] := 'Page0'     //Page 0
    else
      Pages.Add('Page0');
    Pages.Add('Page1');
    Pages.Add('Page2');
    Pages.Add('Page3');
    PageIndex := 0;
    PopupMenu := PopupMenu1;
    Hint := 'NoteBook1';
    ShowHint := True;
    Show;
  end;
//++++++++++++++++++++++++++++++++++++ OpenDialog1 ++++++++++++++++++++++++++++++++++
OpenDialog1 := TOpenDialog.Create(Self);
//++++++++++++++++++++++++++++++++++++ PageControl ++++++++++++++++++++++++++++++++++
{PageControl1 := TPageControl.Create(Self);
  with PageControl1 do
  begin
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    Parent         := BenchForm[31];

    ActivePage     := TabSheet1
    Align          := alNone;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := PageControl1.Font.Style+[fsBold];
    Height         := 200;
    HelpContext    := 0;
    Hint           := 'PageControl1';
    HotTrack       := False;
    Left           := 10;
    MultiLine      := False;
    Name           := 'PageControl1';
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ScrollOpposite := False;
    ShowHint       := True;
    TabHeight      := 0;
    TabOrder       := 0;
    TabPosition    := tpTop;
    TabStop        := True;
    TabWidth       := 0;
    Tag            := 0;   
    Top            := 10;
    Visible        := True;    
    Width          := 300;
  end;
TabSheet1 := TTabSheet.Create(Self);
TabSheet1.Caption := 'TabSheet1';
TabSheet1.PageControl := PageControl1;

TabSheet2 := TTabSheet.Create(Self);
TabSheet2.Caption := 'TabSheet2';
TabSheet2.PageControl := PageControl1;

TabSheet3 := TTabSheet.Create(Self);
TabSheet3.Caption := 'TabSheet3';
TabSheet3.PageControl := PageControl1;}
//++++++++++++++++++++++++++++++++++++ PaintBox1 ++++++++++++++++++++++++++++++++++++
PaintBox1 := TPaintBox.Create(Self); //Gives Access violation !!!
  with PaintBox1 do
  begin
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    Parent         := BenchForm[32];

    Align          := alNone;
    Color          := clBlue;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := PaintBox1.Font.Style+[fsBold];
    Height         := 100;
    Hint           := 'PaintBox1';
    Left           := 10;
    Name           := 'PaintBox1';
    ParentColor    := True;
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowHint       := True;
    Tag            := 0;   
    Top            := 10;
    Visible        := True;    
    Width          := 100;
  end;
//++++++++++++++++++++++++++++++++++++ Panel1 +++++++++++++++++++++++++++++++++++++++
benchform[33].width := 550;
Panel1 := TPanel.Create (Form1);
  with Panel1 do
  begin
    Parent         := BenchForm[33];
    PanelNormClick (self);
  end;
Panel2 := TPanel.Create (Form1);
  with Panel2 do
  begin
    Parent         := BenchForm[33];
    Align          := alBottom;
    Height := 150;
  //Ctl3D := False;
    Caption := 'Panel1';
    Visible := True;
  end;
//++++++++++++++++++++++++++++++++++++ PopupMenu1 +++++++++++++++++++++++++++++++++++
Hello := TMenuItem.Create(PopupMenu1);
Hello.Caption := 'Hello';
Doctor := TMenuItem.Create(Hello);
Doctor.Caption := 'Doctor';
Hello.Add(Doctor);

PopupMenu1 := TPopupMenu.Create(Form1);
  with PopupMenu1 do
  begin
    Parent := Form1;
  //Alignment := paLeft;
    AutoPopup := True;
  //HelpContext := 0;
    Items.Add(Hello);
    Name := 'PopupMenu1';
    Tag := 0;
  end;
//++++++++++++++++++++++++++++++++++++ PrintDialog1 +++++++++++++++++++++++++++++++++
//PrintDialog1 := TPrintDialog.Create(Self); //BenchForm[35]
//++++++++++++++++++++++++++++++++++++ PrinterSetupDialog1 ++++++++++++++++++++++++++
//PrinterSetupDialog1 := TPrinterSetupDialog.Create(Self); //BenchForm[36]
//++++++++++++++++++++++++++++++++++++ ProgressBar1 +++++++++++++++++++++++++++++++++
ProgressBar1 := TProgressBar.Create(Self);
   with ProgressBar1 do
   begin
     Parent := BenchForm[37];
     OnClick        := @EventOnClick;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;
     OnMouseMove    := @EventOnMouseMove;
     BarShowText := False;
     Top := 28;
     Left := 10;
     Height := 100;
     Width := 200;
     Min := 0;
     Max := 100;
     Name := 'ProgressBar1';
     Smooth := True;
     Orientation := pbHorizontal;
     Position := 0;
     PopupMenu := PopupMenu1;
     Enabled := True;
     Hint := 'ProgressBar1';
     ShowHint := True;
     Visible := True;
   end;
//++++++++++++++++++++++++++++++++++++ RadioButton1 +++++++++++++++++++++++++++++++++
RadioButton1 := TRadioButton.Create(Self);
  with RadioButton1 do
  begin
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    Parent         := BenchForm[38];

  //Alignment      := taRigthJustify;  //Identifier no found
    AllowGrayed    := True;
    Anchors        := RadioButton1.Anchors + [akTop, akLeft];
  //BiDiMode       := bdLeftToRight;
    Caption        := 'RadioButton1';
    Checked        := False;
    Color          := clBlue;
  //Constraints
    Cursor         := crDefault;  //Error if not crDefault
    DragCursor     := crDrag;
    DragKind       := dkDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := RadioButton1.Font.Style+[fsBold];
    Height         := 20;
  //HelpContext    := 0;
    Hint           := 'RadioButton1';
    Left           := 10;
    Name           := 'RadioButton1';
  //ParentBiDiMode := False;
    ParentColor    := True;
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowHint       := True;
    State          := cbUnchecked;
    TabOrder       := 10;
    Tag            := 0;   
    Top            := 10;
    Visible        := True;    
    Width          := 100;
  end;
//++++++++++++++++++++++++++++++++++++ RadioGroup1 ++++++++++++++++++++++++++++++++++
RadioGroup1 := TRadioGroup.Create(Self);
   with RadioGroup1 do
   begin
     Parent := BenchForm[39];
     OnClick        := @EventOnClick;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;
     OnMouseMove    := @EventOnMouseMove;
     top     := 20;
     left    := 20;
     Height  := 200;
     Width   := 140;
     Columns := 3;
     Items.Add ('0');
     Items.Add ('1');
     Items.Add ('2');
     Items.Add ('3');
     Items.Add ('4');
     Items.Add ('5');
     Items.Add ('6');
     Items.Add ('7');
     Items.Add ('8');
     ItemIndex := 0;
     Show;
     Caption := ' Radio '+IntToStr(ItemIndex)+' Checked';
   end;
//++++++++++++++++++++++++++++++++++++ ReplaceDialog1 +++++++++++++++++++++++++++++++
//ReplaceDialog1 := TReplaceDialog.Create(Self); //BenchForm[40]
//++++++++++++++++++++++++++++++++++++ RichEdit1 ++++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ SaveDialog1 ++++++++++++++++++++++++++++++++++
SaveDialog1 := TSaveDialog.Create(Self); //BenchForm[42]
//++++++++++++++++++++++++++++++++++++ Screen +++++++++++++++++++++++++++++++++++++++
// Created in TForm...
//++++++++++++++++++++++++++++++++++++ ScrollBar1 +++++++++++++++++++++++++++++++++++
ScrollBar1 := TScrollBar.Create(Self);
  with ScrollBar1 do
  begin
    Parent         := BenchForm[44];
    //OnMouseDown	   := @MDown;
    //OnMouseMove	   := @MMove;
    OnMouseMove    := @EventOnMouseMove;

  //Ctl3D          := False;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := true;
    Height         := 16;
  //HelpContext    := 0;
    Hint	   := 'ScrollBar1';
  //Kind	   := sbHorizontal;
  //LargeChange	   := 1;
    Left 	   := 10;
  //Max		   := 100;
  //Min		   := 0;
    Name	   := 'ScrollBar1';
  //ParentCtl3D	   := True;
    ParentShowHint := True;
    PopupMenu	   := PopupMenu1;
  //Position	   := 0;
  //ScrollStyle    := ssHorizontal;
    ShowHint	   := True;
  //SmallChange	   := 1;
    TabOrder	   := 0;
    TabStop	   := False;
    Tag		   := 0;
    Top 	   := 28;
    Visible	   := True;
    width 	   := 150;
  end;
//++++++++++++++++++++++++++++++++++++ Shape1 +++++++++++++++++++++++++++++++++++++++
{Shape1 := TShape.Create(Self);
  with Shape1 do
  begin
    Parent := BenchForm[46];
    Pen.Color := clBlue;
    Left := 10;
    Top := 10;
    Visible := True;
  end;}
//++++++++++++++++++++++++++++++++++++ SpeedButton1..4 ++++++++++++++++++++++++++++++
SpeedButton1 := TSpeedButton.Create(Self);
   With SpeedButton1 do
   begin
     OnClick := @OpenMemo;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;

     Parent := Self;
     Enabled := True;
     Cursor := crDefault;
     Left := 10;
     Top := 28;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     LoadGlyphFromLazarusResource('laz_open');
     Visible := True;
  end;


SpeedButton2 := TSpeedButton.Create(Self);
   With SpeedButton2 do
   begin
     OnClick := @SaveMemoAs;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;

     Parent := Self;
     Left := 35;
     Top := 28;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     LoadGlyphFromLazarusResource('laz_save');
     Enabled := True;
     Visible := True;
  end;

SpeedButton3 := TSpeedButton.Create(Self);
   With SpeedButton3 do
   begin
     OnClick := @SelectFont;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;

     Parent := Self;
     Left := 60;
     Top := 28;
     Flat := False;
     Hint := 'SpeedButton1';
     Color := clBtnFace;
     ShowHint := True;
     LoadGlyphFromLazarusResource('item_font');
     Enabled := True;
     Visible := True;
  end;

SpeedButton4 := TSpeedButton.Create(Self);
   With SpeedButton4 do
   begin
     OnClick := @ColorSelect;
     OnMouseDown    := @EventOnMouseDown;
     OnMouseUp      := @EventOnMouseUp;

     Parent := Self;
     Left := 85;
     Top := 28;
     Flat := False;
     Hint := 'SpeedButton4';
     Color := clBtnFace;
     ShowHint := True;
     LoadGlyphFromLazarusResource('tcolordialog');
     Enabled := True;
     Visible := True;
  end;
  
//++++++++++++++++++++++++++++++++++++ SpinButton1 ++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ SpinEdit1 ++++++++++++++++++++++++++++++++++++
SpinEdit1 := TSpinEdit.Create(Self);
  with SpinEdit1 do
  begin
    Parent := BenchForm[49];
    //OnChange       := @EventOnChange;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    //MaxValue := 10;
    //MinValue := 1;
    Top := 30;
    Left := 10;
    Width := 50;
    Height := 20;
    Value := 50;
    Visible := True;
  end;
//++++++++++++++++++++++++++++++++++++ StatusBar1 +++++++++++++++++++++++++++++++++++
StatusBar1 := TStatusBar.Create(Self);
  with StatusBar1 do
  begin
    Parent         := BenchForm[50];
    OnClick        := @EventOnClick;
   
    Align          := alBottom;
    Cursor         := crDefault;
    DragCursor     := crDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := StatusBar1.Font.Style+[fsBold];
    Height         := 19;
  //HelpContext    := 0;
    Hint           := 'You !';
    Left           := 0;
    Name           := 'StatusBar1';
  //Panels
  //ParentBiDiMode := False;
    ParentColor    := True;
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowHint       := True;
  //SimpelPanel    := True;
    SimpleText     := 'Hello';
  //SizeGrip       := False;
    Tag            := 0;
    Top            := 254;
    Visible        := True;
    Width          := 392;
  end; 
//++++++++++++++++++++++++++++++++++++ StringGrid1 ++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ TabbedNoteBook1 ++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ TabControl1 ++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ Thread1 ++++++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ Timer1 +++++++++++++++++++++++++++++++++++++++
Timer1 := TTimer.Create(Self);
  with Timer1 do
  begin
    OnTimer  := @Timer1OnTimer;
    Enabled  := False;
    Interval := 100;
  end;
//++++++++++++++++++++++++++++++++++++ ToggleBox1 +++++++++++++++++++++++++++++++++++
ToggleBox1 := TToggleBox.Create(Self);
  with ToggleBox1 do
  begin
    Parent         := BenchForm[56];
    OnClick        := @EventOnClick;
    OnDblClick     := @EventOnDblClick;
    OnMouseDown    := @EventOnMouseDown;
    OnMouseUp      := @EventOnMouseUp;
    OnMouseMove    := @EventOnMouseMove;

  //Action
  //Alignment      := taRigthJustify;
    AllowGrayed    := True;
    Anchors        := ToggleBox1.Anchors + [akTop, akLeft];
  //BiDiMode       := bdLeftToRight;
    Caption        := 'Hello';
    Checked        := False;
    Color          := clBlue;
  //Constraints
    Cursor         := crDefault;  //Error if not crDefault
    DragCursor     := crDrag;
    DragKind       := dkDrag;
    DragMode       := dmManual;
    Enabled        := True;
  //Font.Charset   := DEFAULT_CHARSET	//Identifier not found;
    Font.Color     := clRed;
    Font.Height    := -11;
    Font.Name	   := 'adventure';
    Font.Pitch 	   := fpDefault;
    Font.Size 	   := 6;
    Font.Style 	   := ToggleBox1.Font.Style+[fsBold];
    Height         := 35;
  //HelpContext    := 0;
    Hint           := 'Maybe this one';
    Left           := 10;
    Name           := 'ToggleBox1';
  //ParentBiDiMode := False;
    ParentColor    := True;
    ParentFont     := False;
    ParentShowHint := True;
    PopupMenu      := PopupMenu1;
    ShowHint       := True;
    State          := cbUnchecked;
    TabOrder       := 0;
    Tag            := 0;   
    Top            := 10;
    Visible        := True;    
    Width          := 50;
  end;
//++++++++++++++++++++++++++++++++++++ ToolBar1/ToolButton1..2 ++++++++++++++++++++++
ToolBar1 := TToolBar.Create(Self);
  with ToolBar1 do
  begin
    Parent := BenchForm[57];
    Show;
  end;
if Assigned(Toolbar1) then
begin
{ToolButton1 := TToolButton.Create(ToolBar1);
  with ToolButton1 do
  begin
    parent := ToolBar1;
    OnClick        := @EventOnClick;
    Caption := '1';
    Hint := 'ToolButton1';
    ShowHint := True;
    Show;
  end;}
//ToolButton2 := TToolButton.Create(ToolBar1);
  //with ToolButton2 do
  //begin
     // Assert(False, 'Trace:SETTING PARENT');
   // parent := ToolBar1;
   // OnClick        := @EventOnClick;
    //Show;
  //end;
end;
//++++++++++++++++++++++++++++++++++++ TrackBar1..2 +++++++++++++++++++++++++++++++++
TrackBar1 := TTrackBar.Create(Self);
   with TrackBar1 do
   begin
     //OnChange := @TrackBarChange;
     //OnClick := @WhenClick;
     //OnMouseDown := @MDown;
     //OnMouseUp := @MUp;
     Parent := BenchForm[58];
     Top := 10;
     Left := 20;
     Height := 220;
     Orientation := trVertical;
     Position := 1;
     Max := 204;
     Min := 1;
     Width := 15;
     Name := 'TrackBar1';
     PopupMenu := PopupMenu1;
     ScalePos := trTop;
     TickMarks := tmBoth;
     TickStyle := tsAuto;
     Visible := True;
  end;
TrackBar2 := TTrackBar.Create(Self);
   with TrackBar2 do
   begin
     OnChange := @EventOnChange;
     //OnClick := @WhenClick;
     //OnMouseDown := @MDown;
     //OnMouseUp := @MUp;
     Parent := BenchForm[58];
     Top := 239;
     Left := 50;
     Height := 15;
     Orientation := trHorizontal;
     Position := 1;
     Max := 194;
     Min := 1;
     Width := 220;
     Name := 'TrackBar2';
     PopupMenu := PopupMenu1;
     ScalePos := trTop;
     TickMarks := tmBoth;
     TickStyle := tsAuto;
     Visible := True;
  end;
//++++++++++++++++++++++++++++++++++++ UpDown1 ++++++++++++++++++++++++++++++++++++++

//++++++++++++++++++++++++++++++++++++ THE END ++++++++++++++++++++++++++++++++++++++
END;

initialization
   {$I ../images/laz_images.lrs}
   {$I ../images/components_images.lrs}
END.

