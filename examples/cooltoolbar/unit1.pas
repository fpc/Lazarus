unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls,
  Menus, ActnList, ExtCtrls, StdCtrls, Spin, Toolwin, IniFiles;

type
  { TCoolBarDemo }
  TCoolBarDemo = class(TForm)
    AcTBLeft: TAction;
    AcTBTop: TAction;
    AcTBRight: TAction;
    AcTB1Visi: TAction;
    AcTB2Visi: TAction;
    AcTB3Visi: TAction;
    AcTB4Visi: TAction;
    AcTB5Visi: TAction;
    AcToolBarSmall: TAction;
    AcToolBarMedium: TAction;
    AcToolBarLarge: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ComboBox1: TComboBox;
    CoolBar1: TCoolBar;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MIShowTB1: TMenuItem;
    MIShowTB2: TMenuItem;
    MIShowTB3: TMenuItem;
    MIShowTB4: TMenuItem;
    MIShowTB5: TMenuItem;
    MITBLeft: TMenuItem;
    MITBTop: TMenuItem;
    MITBRight: TMenuItem;
    MITBMedium: TMenuItem;
    MITBLarge: TMenuItem;
    MITBSmall: TMenuItem;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    SpinEdit1: TSpinEdit;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar3: TToolBar;
    ToolBar4: TToolBar;
    ToolBar5: TToolBar;
    TBtn21: TToolButton;
    TBtn22: TToolButton;
    TBtn23: TToolButton;
    TBtn24: TToolButton;
    TBtn11: TToolButton;
    TBtn12: TToolButton;
    TBtn13: TToolButton;
    TBtn41: TToolButton;
    TBtn42: TToolButton;
    TBtn43: TToolButton;
    TBtn31: TToolButton;
    TBtn32: TToolButton;
    TBtn33: TToolButton;
    TBtn34: TToolButton;
    TBtn35: TToolButton;
    TBtn51: TToolButton;
    TBtn52: TToolButton;
    TBtn53: TToolButton;
    TBtn54: TToolButton;
    TBtn55: TToolButton;
    TBtn56: TToolButton;
    procedure AcTB1VisiExecute(Sender: TObject);
    procedure AcTB2VisiExecute(Sender: TObject);
    procedure AcTB3VisiExecute(Sender: TObject);
    procedure AcTB4VisiExecute(Sender: TObject);
    procedure AcTB5VisiExecute(Sender: TObject);
    procedure AcTBLeftExecute(Sender: TObject);
    procedure AcTBRightExecute(Sender: TObject);
    procedure AcTBTopExecute(Sender: TObject);
    procedure AcToolBarLargeExecute(Sender: TObject);
    procedure AcToolBarMediumExecute(Sender: TObject);
    procedure AcToolBarSmallExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { private declarations }
    IniFileName: string;
    const
      cTBSmall = 0;
      cTBMedium = 1;
      cTBLarge = 2;
      cTBTop = 0;
      cTBLeft = 1;
      cTBRight = 2;
      cCoolBarSettings = 'CoolBarSettings';
      cButtonSize = 'ButtonSize';
      cCoolBarPos = 'CoolBarPos';
      cBandBreak = 'CoolBandBreak';
      cBandPos = 'CoolBandPos';
      cBandVisi = 'CoolBandVisi';
    procedure LoadFromIni;
    procedure SaveToIni;
    procedure SetButtonSize(ASize: Integer);
    procedure SetCoolBarAlign(AAlign: TAlign; AVertical: Boolean);
  public
    { public declarations }
  end;

var
  CoolBarDemo: TCoolBarDemo;

implementation

{$R *.lfm}

{ TCoolBarDemo }

procedure TCoolBarDemo.RadioGroup1Click(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: CoolBar1.BiDiMode:=bdLeftToRight;
    1: CoolBar1.BiDiMode:=bdRightToLeft;
  end;
end;

procedure TCoolBarDemo.SpinEdit1Change(Sender: TObject);
begin
  CoolBar1.GrabWidth:=TSpinEdit(Sender).Value;
end;

procedure TCoolBarDemo.LoadFromIni;
var INI:TINIFile;
    i, j, aPos: Integer;
begin
  INI:=TIniFile.Create(IniFileName);
  try
    i:=INI.ReadInteger(cCoolBarSettings, cButtonSize, cTBSmall);
    if i=cTBSmall
      then AcToolBarSmall.Execute
      else if i=cTBMedium
        then AcToolBarMedium.Execute
        else AcToolBarLarge.Execute;
    i:=INI.ReadInteger(cCoolBarSettings, cCoolBarPos, cTBTop);
    if i=cTBTop
      then AcTBTop.Execute
      else if i=cTBLeft
        then AcTBLeft.Execute
        else AcTBRight.Execute;
    CoolBar1.DisableAutoSizing;
    for i:=0 to CoolBar1.Bands.Count-1 do
      begin
        aPos:=INI.ReadInteger(cCoolBarSettings, cBandPos+inttostr(i), i);
        if aPos<>CoolBar1.Bands[i].ID then
          for j:=i+1 to CoolBar1.Bands.Count-1 do
            if CoolBar1.Bands[j].ID=aPos then
              begin
                CoolBar1.Bands[j].Index:=i;
                break;
              end;
      end;
    for i:=0 to CoolBar1.Bands.Count-1 do
      begin
        CoolBar1.Bands[i].Break:=INI.ReadBool(cCoolBarSettings, cBandBreak+inttostr(i), True);
        CoolBar1.Bands[i].Visible:=INI.ReadBool(cCoolBarSettings, cBandVisi+inttostr(i), True);
      end;
    CoolBar1.EnableAutoSizing;
    AcTB1Visi.Checked:=TCoolBand(CoolBar1.Bands.FindItemID(0)).Visible;
    AcTB2Visi.Checked:=TCoolBand(CoolBar1.Bands.FindItemID(1)).Visible;
    AcTB3Visi.Checked:=TCoolBand(CoolBar1.Bands.FindItemID(2)).Visible;
    AcTB4Visi.Checked:=TCoolBand(CoolBar1.Bands.FindItemID(3)).Visible;
    AcTB5Visi.Checked:=TCoolBand(CoolBar1.Bands.FindItemID(4)).Visible;
  finally
    INI.Free;
  end;
end;

procedure TCoolBarDemo.SaveToIni;
var INI:TINIFile;
    i: Integer;
begin
  INI:=TIniFile.Create(IniFileName);
  try
    if AcToolBarLarge.Checked
      then i:=cTBLarge
      else if AcToolBarMedium.Checked
        then i:=cTBMedium
        else i:=cTBSmall;
    INI.WriteInteger(cCoolBarSettings, cButtonSize, i);
    if AcTBLeft.Checked
      then i:=cTBLeft
      else if AcTBRight.Checked
        then i:=cTBRight
        else i:=cTBTop;
    INI.WriteInteger(cCoolBarSettings, cCoolBarPos, i);
    for i:=0 to CoolBar1.Bands.Count-1 do
      begin
        INI.WriteInteger(cCoolBarSettings, cBandPos+inttostr(i), CoolBar1.Bands[i].ID);
        INI.WriteBool(cCoolBarSettings, cBandBreak+inttostr(i), CoolBar1.Bands[i].Break);
        INI.WriteBool(cCoolBarSettings, cBandVisi+inttostr(i), CoolBar1.Bands[i].Visible);
      end;
  finally
    INI.Free;
  end;
end;

procedure TCoolBarDemo.SetButtonSize(ASize: Integer);
begin
  ToolBar1.ButtonWidth:=ASize;
  ToolBar1.ButtonHeight:=ASize;
  ToolBar2.ButtonWidth:=ASize;
  ToolBar2.ButtonHeight:=ASize;
  ToolBar3.ButtonWidth:=ASize;
  ToolBar3.ButtonHeight:=ASize;
  ToolBar4.ButtonWidth:=ASize;
  ToolBar4.ButtonHeight:=ASize;
  ToolBar5.ButtonWidth:=ASize;
  ToolBar5.ButtonHeight:=ASize;
  CoolBar1.AutosizeBands;
end;

procedure TCoolBarDemo.SetCoolBarAlign(AAlign: TAlign; AVertical: Boolean);
var w, h: Integer;
    bTurn: Boolean;
begin
  BeginFormUpdate;
  bTurn:= (AVertical<>CoolBar1.Vertical);
  CoolBar1.Vertical:=AVertical;
  case AAlign of
    alLeft:
      begin
        CoolBar1.Anchors:=[akLeft, akTop, akBottom];
        CoolBar1.AnchorParallel(akLeft, 0, self);
        CoolBar1.AnchorParallel(akTop, 0, self);
        CoolBar1.AnchorParallel(akBottom, 0, self);
        CoolBar1.EdgeBorders:=[ebRight];
      end;
    alTop:
      begin
        CoolBar1.Anchors:=[akLeft, akTop, akRight];
        CoolBar1.AnchorParallel(akLeft, 0, self);
        CoolBar1.AnchorParallel(akTop, 0, self);
        CoolBar1.AnchorParallel(akRight, 0, self);
        CoolBar1.EdgeBorders:=[ebBottom];
      end;
    alRight:
      begin
        CoolBar1.Anchors:=[akRight, akTop, akBottom];
        CoolBar1.AnchorParallel(akRight, 0, self);
        CoolBar1.AnchorParallel(akTop, 0, self);
        CoolBar1.AnchorParallel(akBottom, 0, self);
        CoolBar1.EdgeBorders:=[ebLeft];
      end;
  end;
  EndFormUpdate;
  if bTurn then
    begin
      if not AVertical then
        begin
          w:=100;
          h:=25;
        end else
        begin
          w:=25;
          h:=100;
        end;
      ToolBar1.AutoSize:=False;
      ToolBar2.AutoSize:=False;
      ToolBar3.AutoSize:=False;
      ToolBar4.AutoSize:=False;
      ToolBar5.AutoSize:=False;
      ToolBar1.Width:=w;
      ToolBar1.Height:=h;
      ToolBar2.Width:=w;
      ToolBar2.Height:=h;
      ToolBar3.Width:=w;
      ToolBar3.Height:=h;
      ToolBar4.Width:=w;
      ToolBar4.Height:=h;
      ToolBar5.Width:=w;
      ToolBar5.Height:=h;
      ToolBar1.AutoSize:=True;
      ToolBar2.AutoSize:=True;
      ToolBar3.AutoSize:=True;
      ToolBar4.AutoSize:=True;
      ToolBar5.AutoSize:=True;
    end;
  CoolBar1.AutosizeBands;
end;

procedure TCoolBarDemo.ComboBox1Change(Sender: TObject);
begin
  CoolBar1.GrabStyle:=TGrabStyle(TComboBox(Sender).ItemIndex);
end;

procedure TCoolBarDemo.FormActivate(Sender: TObject);
begin
  LoadFromIni;
  CoolBarDemo.OnActivate:=nil;
end;

procedure TCoolBarDemo.FormCreate(Sender: TObject);
var
  ConfDir: String;
begin
  ConfDir:=GetAppConfigDir(False);
  ForceDirectories(ConfDir);
  IniFileName:=ConfDir+'cooltoolbar.ini';
  writeln(IniFileName);
end;

procedure TCoolBarDemo.FormDestroy(Sender: TObject);
begin
  SaveToIni;
end;

procedure TCoolBarDemo.AcToolBarLargeExecute(Sender: TObject);
begin
  SetButtonSize(38);
end;

procedure TCoolBarDemo.AcTBLeftExecute(Sender: TObject);
begin
  SetCoolBarAlign(alLeft, True);
end;

procedure TCoolBarDemo.AcTB1VisiExecute(Sender: TObject);
begin
  TCoolBand(CoolBar1.Bands.FindItemID(0)).Visible:=TAction(Sender).Checked;
end;

procedure TCoolBarDemo.AcTB2VisiExecute(Sender: TObject);
begin
  TCoolBand(CoolBar1.Bands.FindItemID(1)).Visible:=TAction(Sender).Checked;
end;

procedure TCoolBarDemo.AcTB3VisiExecute(Sender: TObject);
begin
  TCoolBand(CoolBar1.Bands.FindItemID(2)).Visible:=TAction(Sender).Checked;
end;

procedure TCoolBarDemo.AcTB4VisiExecute(Sender: TObject);
begin
  TCoolBand(CoolBar1.Bands.FindItemID(3)).Visible:=TAction(Sender).Checked;
end;

procedure TCoolBarDemo.AcTB5VisiExecute(Sender: TObject);
begin
  TCoolBand(CoolBar1.Bands.FindItemID(4)).Visible:=TAction(Sender).Checked;
end;

procedure TCoolBarDemo.AcTBRightExecute(Sender: TObject);
begin
  SetCoolBarAlign(alRight, True);
end;

procedure TCoolBarDemo.AcTBTopExecute(Sender: TObject);
begin
  SetCoolBarAlign(alTop, False);
end;

procedure TCoolBarDemo.AcToolBarMediumExecute(Sender: TObject);
begin
  SetButtonSize(30);
end;

procedure TCoolBarDemo.AcToolBarSmallExecute(Sender: TObject);
begin
  SetButtonSize(22);
end;

procedure TCoolBarDemo.Button1Click(Sender: TObject);
begin
  CoolBar1.AutosizeBands;
end;

end.

