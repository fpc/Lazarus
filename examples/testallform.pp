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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit TestAllForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, StdCtrls, LclProc, LCLType, ContNrs,

  Arrow, ButtonPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnClear: TBitBtn;
    BottomPnl: TPanel;
    EventsLB: TListBox;
    MainMenu1: TMainMenu;
    mnuCompTZ: TMenuItem;
    mnuCompQS: TMenuItem;
    mnuCompNP: TMenuItem;
    mnuCompGM: TMenuItem;
    mnuCompDF: TMenuItem;
    mnuCompAC: TMenuItem;
    mnuComponents: TMenuItem;
    mnuAbout: TMenuItem;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuDialogs: TMenuItem;
    EventsPnl: TPanel;
    TestPnl: TPanel;
    procedure BtnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
  private
    { private declarations }
    FObjList: TFPObjectList;
    procedure Clear;

    procedure CompMenuClick(Sender: TObject);
    procedure DlgMenuClick(Sender: TObject);
    procedure GenClick(Sender: TObject);
    procedure GenDblClick(Sender: TObject);
    procedure GenKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GenKeyPress(Sender: TObject; var Key: char);
    procedure GenKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GenMouseEnter(Sender: TObject);
    procedure GenMouseLeave(Sender: TObject);
    procedure GenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShowEventFmt(const Fmt: String; const Args: array of const);

    procedure ConnectStandardEvents(AControl: TControl);
    procedure CreateMainMenu;

    // Components tests
    procedure TestArrow;
    procedure TestBitBtn;
    procedure TestButton;
    procedure TestButtonpanel;
    procedure TestCalcedit;
    procedure TestCalendar;
    procedure TestCheckbox;
    procedure TestCheckcombobox;
    procedure TestCheckgroup;
    procedure TestChecklistbox;
    procedure TestColorbox;
    procedure TestColorbutton;
    procedure TestColorlistbox;
    procedure TestCombobox;
    procedure TestComboboxex;
    procedure TestControlbar;
    procedure TestCoolbar;
    procedure TestDateedit;
    procedure TestDirectoryedit;
    procedure TestEdit;
    procedure TestEditbutton;
    procedure TestFilelistbox;
    procedure TestFilenameedit;
    procedure TestFiltercombobox;
    procedure TestFloatspinedit;
    procedure TestGroupbox;
    procedure TestHeadercontrol;
    procedure TestImage;
    procedure TestLabel;
    procedure TestLabelededit;
    procedure TestListbox;
    procedure TestListview;
    procedure TestMaskedit;
    procedure TestMemo;
    procedure TestNotebook;
    procedure TestPagecontrol;
    procedure TestPaintbox;
    procedure TestPanel;
    procedure TestProgressbar;
    procedure TestRadiobutton;
    procedure TestRadiogroup;
    procedure TestShape;
    procedure TestShelllistview;
    procedure TestShelltreeview;
    procedure TestSpeedbutton;
    procedure TestSpinedit;
    procedure TestSplitter;
    procedure TestStacTictext;
    procedure TestStatusbar;
    procedure TestStringgrid;
    procedure TestTabcontrol;
    procedure TestTimeedit;
    procedure TestTimer;
    procedure TestTogglebox;
    procedure TestToolbar;
    procedure TestTrackbar;
    procedure TestTreeview;
    procedure TestUpdown;
    procedure TestValuelisteditor;

    // Dialog tests
    procedure TestMessageDialog;
    procedure TestCalculatorDialog;
    procedure TestCalendarDialog;
    procedure TestColorDialog;
    procedure TestFindDialog;
    procedure TestFontDialog;
    procedure TestOpenDialog;
    procedure TestOpenpictureDialog;
    procedure TestReplaceDialog;
    procedure TestSaveDialog;
    procedure TestSavepictureDialog;
    procedure TestSelectdirectoryDialog;
    procedure TestQuestionDialog;

  public
    { public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}
{$R ../images/laz_images.res}
{$R ../images/components_images.res}
{$i testallform_include.inc}

{ TForm1 }

procedure TForm1.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Clear;
begin
  FObjList.Clear;
  EventsLB.Clear;
end;

procedure TForm1.CompMenuClick(Sender: TObject);
var
  mi: TMenuItem;
  tg: PtrInt;
  TagValid: Boolean;
begin
  debugln('TForm1.CompMenuClick A');
  mi := Sender as TMenuItem;
  tg := mi.Tag and not tagCompStart;
  TagValid := ((mi.Tag and tagCompStart) = tagCompStart) and
              (tg >= Ord(Low(taComponents))) and
              (tg <= Ord(High(taComponents)));
  if not TagValid then
  begin
    DebugLn(['TForm1.CompMenuClick: Unexpected Tag from TMenuItem: [',mi.Name,']']);
    Exit;
  end;
  case taComponents(tg) of
    tacTarrow: TestArrow;
    tacTbitbtn: TestBitBtn;
    tacTButton: TestButton;
    tacTButtonpanel: TestButtonpanel;
    tacTCalcedit: TestCalcedit;
    tacTCalendar: TestCalendar;
    tacTCheckbox: TestCheckbox;
    tacTCheckcombobox: TestCheckcombobox;
    tacTCheckgroup: TestCheckgroup;
    tacTChecklistbox: TestChecklistbox;
    tacTColorbox: TestColorbox;
    tacTColorbutton: TestColorbutton;
    tacTColorlistbox: TestColorlistbox;
    tacTCombobox: TestCombobox;
    tacTComboboxex: TestComboboxex;
    tacTControlbar: TestControlbar;
    tacTCoolbar: TestCoolbar;
    tacTDateedit: TestDateedit;
    tacTDirectoryedit: TestDirectoryedit;
    tacTEdit: TestEdit;
    tacTEditbutton: TestEditbutton;
    tacTFilelistbox: TestFilelistbox;
    tacTFilenameedit: TestFilenameedit;
    tacTFiltercombobox: TestFiltercombobox;
    tacTFloatspinedit: TestFloatspinedit;
    tacTGroupbox: TestGroupbox;
    tacTHeadercontrol: TestHeadercontrol;
    tacTImage: TestImage;
    tacTLabel: TestLabel;
    tacTLabelededit: TestLabelededit;
    tacTListbox: TestListbox;
    tacTListview: TestListview;
    tacTMaskedit: TestMaskedit;
    tacTMemo: TestMemo;
    tacTNotebook: TestNotebook;
    tacTPagecontrol: TestPagecontrol;
    tacTPaintbox: TestPaintbox;
    tacTPanel: TestPanel;
    tacTProgressbar: TestProgressbar;
    tacTRadiobutton: TestRadiobutton;
    tacTRadiogroup: TestRadiogroup;
    tacTShape: TestShape;
    tacTShelllistview: TestShelllistview;
    tacTShelltreeview: TestShelltreeview;
    tacTSpeedbutton: TestSpeedbutton;
    tacTSpinedit: TestSpinedit;
    tacTSplitter: TestSplitter;
    tacTStacTictext: TestStacTictext;
    tacTStatusbar: TestStatusbar;
    tacTStringgrid: TestStringgrid;
    tacTTabcontrol: TestTabcontrol;
    tacTTimeedit: TestTimeedit;
    tacTTimer: TestTimer;
    tacTTogglebox: TestTogglebox;
    tacTToolbar: TestToolbar;
    tacTTrackbar: TestTrackbar;
    tacTTreeview: TestTreeview;
    tacTUpdown: TestUpdown;
    tacTValuelisteditor: TestValuelisteditor;
  end;
  debugln('TForm1.CompMenuClick End');
end;

procedure TForm1.DlgMenuClick(Sender: TObject);
var
  mi: TMenuItem;
  tg: PtrInt;
  TagValid: Boolean;
begin
  debugln('TForm1.DlgMenuClick A');
  mi := Sender as TMenuItem;
  tg := mi.Tag and not tagDlgStart;
  TagValid := ((mi.Tag and tagDlgStart) = tagDlgStart) and
              (tg >= Ord(Low(taComponents))) and
              (tg <= Ord(High(taComponents)));
  if not TagValid then
  begin
    DebugLn(['TForm1.DlgMenuClick: Unexpected Tag from TMenuItem: [',mi.Name,']']);
    Exit;
  end;
  case taDialogs(tg) of
    tadTCalculatorDialog: TestCalculatorDialog;
    tadTCalendarDialog: TestCalendarDialog;
    tadTColorDialog: TestColorDialog;
    tadTFindDialog: TestFindDialog;
    tadTFontDialog: TestFontDialog;
    tadTMessageDialog: TestMessageDialog;
    tadTOpenDialog: TestOpenDialog;
    tadTOpenpictureDialog: TestOpenpictureDialog;
    tadTReplaceDialog: TestReplaceDialog;
    tadTSaveDialog: TestSaveDialog;
    tadTSavepictureDialog: TestSavepictureDialog;
    tadTSelectdirectoryDialog: TestSelectdirectoryDialog;
    tadTQuestionDialog: TestQuestionDialog;
  end;
  debugln('TForm1.DlgMenuClick End');
end;

procedure TForm1.GenClick(Sender: TObject);
begin
  ShowEventFmt('%s.OnClick',[Sender.ClassName]);
end;

procedure TForm1.GenDblClick(Sender: TObject);
begin
  ShowEventFmt('%s.OnDblClick',[Sender.ClassName]);
end;

procedure TForm1.GenKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ShowEventFmt('%s.OnKeyDown [%s]',[Sender.ClassName,VkToString(Key)]);
end;

procedure TForm1.GenKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#33..#127] then
    ShowEventFmt('%s.OnKeyPress [%s]',[Sender.ClassName, Key])
  else
    ShowEventFmt('%s.OnKeyPress [#%d]',[Sender.ClassName, Ord(Key)])
end;

procedure TForm1.GenKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  ShowEventFmt('%s.OnKeyUp [%s]',[Sender.ClassName,VkToString(Key)]);
end;

procedure TForm1.GenMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowEventFmt('%s.OnMouseDown at (%d,%d)',[Sender.ClassName,X,Y]);
end;

procedure TForm1.GenMouseEnter(Sender: TObject);
begin
  ShowEventFmt('%s.OnMouseEnter',[Sender.ClassName]);
end;

procedure TForm1.GenMouseLeave(Sender: TObject);
begin
  ShowEventFmt('%s.OnMouseLeave',[Sender.ClassName]);
end;

procedure TForm1.GenMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowEventFmt('%s.OnMouseUp at (%d,%d)',[Sender.ClassName,X,Y]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FObjList := TFPObjectList.Create(True);
  CreateMainMenu;
  Randomize;
end;

procedure TForm1.BtnClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FObjList.Clear;
  FObjList.Free;
end;

procedure TForm1.mnuAboutClick(Sender: TObject);
begin
  MessageDlg(AppTitle, AboutMsg, mtInformation, [mbOK], 0);
end;

procedure TForm1.ShowEventFmt(const Fmt: String; const Args: array of const);
begin
  EventsLB.Items.Add(Fmt, Args);
  EventsLB.ItemIndex := EventsLB.Count - 1;
end;

type
  THackControl = class(TControl);

procedure TForm1.ConnectStandardEvents(AControl: TControl);
begin
  if not Assigned(AControl) then Exit;
  THackControl(AControl).OnMouseDown := @GenMouseDown;
  THackControl(AControl).OnMouseUp := @GenMouseUp;
  THackControl(AControl).OnMouseEnter := @GenMouseEnter;
  THackControl(AControl).OnMouseLeave := @GenMouseLeave;
  AControl.OnClick := @GenClick;
  THackControl(AControl).OnDblClick := @GenDblClick;
  if (AControl is TWinControl) then TWinControl(AControl).OnKeyDown := @GenKeyDown;
  if (AControl is TWinControl) then TWinControl(AControl).OnKeyUp := @GenKeyUp;
  if (AControl is TCustomEdit) then TCustomEdit(AControl).OnKeyPress := @GenKeyPress;
end;


procedure TForm1.CreateMainMenu;
var
  tac: taComponents;
  mi: TMenuItem;
  aCaption: String;
  tad: taDialogs;
begin
  for tac := Low(taComponents) to High(taComponents) do
  begin
    mi := TMenuItem.Create(Self);
    aCaption := taCompNames[tac];
    if (Upcase(aCaption[1]) = 'T') then aCaption[2] := UpCase(aCaption[2]);
    mi.Caption := aCaption;
    mi.Enabled := taCompImplemented[tac];
    mi.Name := 'mnu' + aCaption;
    mi.Tag := tagCompStart or Ord(tac);
    mi.OnClick := @CompMenuClick;
    //mnuComponents.Add(mi);
    case UpCase(aCaption[2]) of
      'A'..'C': mnuCompAC.Add(mi);
      'D'..'F': mnuCompDF.Add(mi);
      'G'..'M': mnuCompGM.Add(mi);
      'N'..'P': mnuCompNP.Add(mi);
      'Q'..'S': mnuCompQS.Add(mi);
      'T'..'Z': mnuCompTZ.Add(mi);
    end;
  end;
  for tad := Low(taDialogs) to High(taDialogs) do
  begin
    mi := TMenuItem.Create(Self);
    aCaption := taDlgNames[tad];
    //if (Upcase(aCaption[1]) = 'T') then aCaption[2] := UpCase(aCaption[2]);
    mi.Caption := aCaption;
    mi.Enabled := taDlgImplemented[tad];
    mi.Name := 'mnu' + aCaption;
    mi.Tag := tagDlgStart or Ord(tad);
    mi.OnClick := @DlgMenuClick;
    mnuDialogs.Add(mi);
  end;
end;

{$I testtools.inc}
end.

