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

unit TestAllForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, Buttons, StdCtrls, LclProc, LCLType, ContNrs,

  Arrow;

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
    function VkToString(Key: Word): String;
    procedure CreateMainMenu;
    // Components tests
    procedure TestArrow;
    procedure TestBitBtn;

  public
    { public declarations }
  end;

var
  Form1: TForm1;

{$i testallform_include.inc}

implementation

{$R *.lfm}
{$R ../images/laz_images.res}
{$R ../images/components_images.res}

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
  end;
  debugln('TForm1.CompMenuClick End');
end;

procedure TForm1.DlgMenuClick(Sender: TObject);
begin

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
  ShowEventFmt('%s.OnKeyDown [$%s]',[Sender.ClassName,VkToString(Key)]);
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
  ShowEventFmt('%s.OnKeyUp [$%s]',[Sender.ClassName,VkToString(Key)]);
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

function TForm1.VkToString(Key: Word): String;
begin
  Result := DbgsVKCode(Key);
  if (Pos('(', Result) > 0) and (Pos(')', Result) > 0) then
    Result := '$' + IntToHex(Key,4);
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

