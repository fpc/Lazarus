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
unit JumpHistoryView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, Menus,
  // LazUtils
  LazUTF8, LazStringUtils,
  // CodeTools
  CodeToolManager, CodeCache,
  // IDE
  IDEOptionDefs, EnvironmentOpts, LazarusIDEStrConsts, Project, ProjectDefs;

type

  { TJumpHistoryViewWin }

  TJumpHistoryViewWin = class(TForm)
    listHistory : TListBox;
    procedure FormCreate(Sender : TObject);
    procedure listHistoryClick(Sender : TObject);
    procedure listHistoryDblClick(Sender : TObject);
    procedure listHistoryKeyPress(Sender: TObject; var Key: char);
    procedure OnIdle(Sender : TObject; var {%H-}Done: Boolean);
  private
    { private declarations }
    fOnSelectionChanged : TNotifyEvent;
    fProjectChangeStamp: integer;
    function GetSelectedIndex : Integer;
    function BeautifyLine(const Filename: string; X, Y: integer;
      const Line: string): string;
    procedure InitDisplay;
  protected
    procedure IndexChanged(Sender: TObject; {%H-}Index: Integer);
    procedure ListChanged(Sender: TObject; {%H-}Index: Integer);
  public
    { public declarations }
    property SelectedIndex : Integer read GetSelectedIndex;
    property OnSelectionChanged: TNotifyEvent read fOnSelectionChanged
                                              write fOnSelectionChanged;
  end;

var
  JumpHistoryViewWin : TJumpHistoryViewWin = nil;

implementation

{$R *.lfm}

const
  MaxTextLen = 80;

{ TJumpHistoryViewWin }

procedure TJumpHistoryViewWin.FormCreate(Sender : TObject);
begin
  Caption := lisJHJumpHistory;
  Name := NonModalIDEWindowNames[nmiwJumpHistory];
  InitDisplay;
  Application.AddOnIdleHandler(@OnIdle);
end;

procedure TJumpHistoryViewWin.listHistoryClick(Sender : TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then exit;
  if Assigned(fOnSelectionChanged) then fOnSelectionChanged(self);
end;

procedure TJumpHistoryViewWin.listHistoryDblClick(Sender : TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then exit;
  if Assigned(fOnSelectionChanged) then fOnSelectionChanged(self);
end;

procedure TJumpHistoryViewWin.listHistoryKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then begin
    if Assigned(fOnSelectionChanged) then fOnSelectionChanged(self);
  end;
end;

procedure TJumpHistoryViewWin.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if (Project1<>nil)
  and (Project1.JumpHistory.ChangeStamp<>fProjectChangeStamp) then
    InitDisplay;
end;

function TJumpHistoryViewWin.GetSelectedIndex : Integer;
begin
  Result := listHistory.ItemIndex;
end;

function TJumpHistoryViewWin.BeautifyLine(const Filename : string; X, Y : integer;
  const Line : string) : string;
begin
  Result:=SpecialCharsToHex(Line);
  if UTF8Length(Result)>MaxTextLen then
    Result:=UTF8Copy(Result,1,MaxTextLen)+'...';
  Result:=Filename
          +' ('+IntToStr(Y)
          +','+IntToStr(X)+')'
          +' '+Result;
end;

procedure TJumpHistoryViewWin.InitDisplay;
var
  i : integer;
  jh_item : TProjectJumpHistoryPosition;
  SrcLine: String;
  CodeBuf: TCodeBuffer;
  Filename: String;
begin
  if (Project1<>nil)
  and (fProjectChangeStamp=Project1.JumpHistory.ChangeStamp) then exit;
  listHistory.Items.BeginUpdate;
  listHistory.Clear;
  if (Project1<>nil) then begin
    fProjectChangeStamp:=Project1.JumpHistory.ChangeStamp;
    for i := 0 to Project1.JumpHistory.Count -1 do begin
      jh_item := Project1.JumpHistory.Items[i];
      SrcLine:='';
      CodeBuf:=CodeToolBoss.LoadFile(jh_item.Filename,true,false);
      if CodeBuf<>nil then
        SrcLine:=CodeBuf.GetLine(jh_item.CaretXY.Y-1,false);
      Filename:=jh_item.Filename;
      if Project1<>nil then
        Filename:=Project1.GetShortFilename(Filename,true);
      listHistory.Items.Append
        (BeautifyLine(Filename,
                      jh_item.CaretXY.X,
                      jh_item.CaretXY.Y,
                      SrcLine
                     )
        );
    end;
    //DebugLn(['TJumpHistoryViewWin.InitDisplay Project1.JumpHistory.HistoryIndex=',Project1.JumpHistory.HistoryIndex]);
    listHistory.ItemIndex := Project1.JumpHistory.HistoryIndex;
  end;
  listHistory.Items.EndUpdate;
end;

procedure TJumpHistoryViewWin.IndexChanged(Sender : TObject; Index : Integer);
begin
  listHistory.ItemIndex := Project1.JumpHistory.HistoryIndex;
end;

procedure TJumpHistoryViewWin.ListChanged(Sender : TObject; Index : Integer);
begin
  InitDisplay;
end;

end.
