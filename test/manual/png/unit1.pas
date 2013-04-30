(*
  PNG test suite images from http://www.schaik.com/pngsuite2011/pngsuite.html
*)
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, fpreadgif, fpwritejpeg, fpreadpng, fpimage, fpwritebmp, LCLIntf;

type

  { TfrmPNGTestSuite }

  TfrmPNGTestSuite = class(TForm)
    imgCheckboard: TImage;
    imgCheckboardExpected: TImage;
    imgTestSuite: TImage;
    imgExpected: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblFileName: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    PanelExpected: TPanel;
    rbRefGIF: TRadioButton;
    rbConvGIF: TRadioButton;
    rbConvPNG: TRadioButton;
    RadioGroup1: TRadioGroup;
    procedure FormDestroy(Sender: TObject);
    procedure imgExpectedResize(Sender: TObject);
    procedure imgTestSuiteResize(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure Panel1Resize(Sender: TObject);
    procedure PanelExpectedResize(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure rbRefGIFClick(Sender: TObject);
  private
    { private declarations }
    FTestFiles: TStringList;
    procedure LoadTest(const aTestName: ansistring);
  public
    { public declarations }
  end; 

var
  frmPNGTestSuite: TfrmPNGTestSuite;

implementation

{$R *.lfm}

{ TfrmPNGTestSuite }

procedure TfrmPNGTestSuite.FormDestroy(Sender: TObject);
begin
  FTestFiles.Free;
end;

procedure TfrmPNGTestSuite.imgExpectedResize(Sender: TObject);
var
  img: TImage absolute Sender;
  r: TRect;
begin
  r:=img.BoundsRect;
  inflaterect(r,2,2);
  PanelExpected.Width:=r.Right-r.Left+2;
  PanelExpected.Height:=r.Bottom-r.top+2;
end;

procedure TfrmPNGTestSuite.imgTestSuiteResize(Sender: TObject);
var
  img: TImage absolute Sender;
  r: TRect;
begin
  r:=img.BoundsRect;
  inflaterect(r,2,2);
  Panel1.Width:=r.Right-r.Left+2;
  Panel1.Height:=r.Bottom-r.top+2;
end;

procedure TfrmPNGTestSuite.ListBox1SelectionChange(Sender: TObject; User: boolean);
var
  TestFile: ansistring;
  AsItShouldLook: ansistring;
begin
  TestFile:=ListBox1.Items[ListBox1.ItemIndex];
  lblFileName.Caption:=TestFile;
  TestFile:=Copy(TestFile,1,8);
  if rbRefGIF.Checked then begin
    AsItShouldLook:=format('%s%s%s.gif',['testsuite_check',PathDelim,TestFile]);
  end else if rbConvGIF.Checked then begin
    AsItShouldLook:=format('%s%s%s.png.gif',['testsuite_check',PathDelim,TestFile]);
  end else if rbConvPNG.Checked then begin
    AsItShouldLook:=format('%s%s%s.png',['testsuite_check',PathDelim,TestFile]);
  end else begin
    exit;
  end;
  if FileExists(AsItShouldLook) then begin
    imgExpected.Picture.LoadFromFile(AsItShouldLook);
  end else begin
    imgExpected.Picture.Clear;
  end;
  TestFile:=format('%s%s%s.png',['testsuite',PathDelim,TestFile]);
  try
    imgTestSuite.Picture.LoadFromFile(TestFile);
  except
    imgTestSuite.Picture.Clear;
  end;
end;

procedure TfrmPNGTestSuite.Panel1Resize(Sender: TObject);
var
  pnl: TPanel absolute Sender;
begin
  imgCheckboard.BoundsRect:=pnl.ClientRect;
end;

procedure TfrmPNGTestSuite.PanelExpectedResize(Sender: TObject);
var
  pnl: TPanel absolute Sender;
begin
  imgCheckboardExpected.BoundsRect:=pnl.ClientRect;
end;

procedure TfrmPNGTestSuite.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: LoadTest('basic');
    1: LoadTest('interlacing');
    2: LoadTest('oddsizes');
    3: LoadTest('backgroundcolors');
    4: LoadTest('transparency');
    5: LoadTest('gamma');
    6: LoadTest('imagefiltering');
    7: LoadTest('additionalpalettes');
    8: LoadTest('ancillarychunks');
    9: LoadTest('chunkordering');
    10: LoadTest('zlibcompression');
    11: LoadTest('corruptedfiles');
  end;
end;

procedure TfrmPNGTestSuite.rbRefGIFClick(Sender: TObject);
begin
  if ListBox1.ItemIndex<>-1 then begin
    ListBox1.OnSelectionChange(ListBox1,false);
  end;
end;

procedure TfrmPNGTestSuite.LoadTest(const aTestName: ansistring);
begin
  ListBox1.Clear;
  ListBox1.Items.LoadFromFile('testsuite'+PathDelim+aTestName+'.files.txt');
end;

end.

