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
 Dialog box used during compile or build

 $Author$    Guadagnini David
 $Date$      Feb-28-2008
 $Revision$  2.1
}
unit InfoBuild;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  LCLProc, ExtCtrls, StdCtrls, ExtDlgs,
  LazIDEIntf,
  LazarusIDEStrConsts;

type

  { TFInfoCompile }

  TFInfoCompile = class ( TForm )
    BClose : TBitBtn;
    LInfoError : TLabel;
    LInfoNote : TLabel;
    LInfoHint : TLabel;
    LInfoWarning : TLabel;
    LInfoLines : TLabel;
    LNError : TLabel;
    LNNote : TLabel;
    LNLines : TLabel;
    LNHint : TLabel;
    LNWarning : TLabel;
    Panel2 : TPanel;
    Panel4 : TPanel;
    PCurrentStatus : TLabel;
    Panel1 : TPanel;
    PInfo : TPanel;
    procedure BCloseClick ( Sender : TObject ) ;
    procedure FormCreate ( Sender : TObject ) ;
  private
    NHints    : Integer;
    NWarnings : Integer;
    NErrors   : Integer;
    NNotes    : Integer;
    NLines    : Integer;
    ToAbort   : Boolean;
  public
    Procedure SetProjectName(Const Sname : String);
    Procedure SetStatus(Const SStatus : String);
    Procedure SetCanClose;
    Procedure MakeBold;
  end;

var
  FInfoCompile : TFInfoCompile;
  ShowCompileDialog: Boolean = false;

Procedure CreateInfoBuilder(Owner: TComponent);
Procedure DestroyInfoBuilder;
Procedure PutInfoBuilderStatus(Const Info : String);
Procedure PutInfoBuilderProject(Const Info : String);
Procedure AbleInfoBuilderExit;
Procedure PutExitInfoBuilder(Const Info : String);

implementation

{ TFInfoCompile }

procedure TFInfoCompile.BCloseClick ( Sender : TObject ) ;
begin
  If ToAbort then
  begin
    LazarusIDE.AbortBuild;
    SetStatus('Aborted...!');
    MakeBold;
    SetCanClose;
  end
    else
      Close;
end;

procedure TFInfoCompile.FormCreate ( Sender : TObject ) ;
begin
  NHints    := 0;
  NWarnings := 0;
  NErrors   := 0;
  NNotes    := 0;
  NLines    := 0;
  
  LInfoLines.Caption   := lisInfoBuildLines;
  LInfoError.Caption   := lisInfoBuildErrors;
  LInfoHint.Caption    := lisInfoBuildHint;
  LInfoWarning.Caption := lisInfoBuildWarning;
  LInfoNote.Caption    := lisInfoBuildNote;
  BClose.Kind          := bkNoToAll;
  BClose.Caption       := lisInfoBuildMakeAbort;
  Caption              := lisInfoBuildCaption;

  ToAbort              := True;
  BClose.LoadGlyphFromLazarusResource('btn_ok');

  SetProjectName('');
  SetStatus('');
end;


Procedure TFInfoCompile.SetProjectName(Const Sname : String);
begin
  PInfo.Caption := lisInfoBuildBuild + ' '+Sname;
end;

Procedure TFInfoCompile.SetStatus(Const SStatus : String);
Var
  S  : String;
  Ok : Boolean;
  NL : Integer;
begin
  S  := LowerCase(SStatus);

  if (pos('warning(s)', s) > 0) or
     (pos('hint(s)',    s) > 0) or
     (pos('note(s)',    s) > 0) then exit;

  Ok := True;

  if (Pos('hint:', s) > 0) then
  begin
    Inc(NHints);
    LNHint.Caption := IntToStr(NHints);
    Ok             := False;
  end;

  If Pos('warning:', s) > 0 then
  begin
    Inc(NWarnings);
    LNWarning.Caption := IntToStr(NWarnings);
    Ok                := False;
  end;

  If (Pos('fatal:', s) > 0) Or (Pos('error:', s) > 0) then
  begin
    Inc(NErrors);
    LNError.Caption := IntToStr(NErrors);
    Ok              := False;
  end;

  If (Pos('note:', s) > 0) then
  begin
    Inc(NNotes);
    LNNote.Caption := IntToStr(NNotes);
    Ok             := False;
  end;

  If (Pos('lines compiled', s) > 0) then
  begin
    s := copy(s, 1, pos('lines compiled', s)-1);
    while copy(s, 1, 1) = ' ' do delete(s, 1, 1);
    while copy(s, length(s), 1) = ' ' do delete(s, length(s), 1);
    
    try
      NL := strtoint(s)
    except
      NL := 0;
    end;
    
    NLines := NLines + NL;
    LNLines.Caption := IntToStr(NLines);
    Ok              := false;
  end;

  If Ok then PCurrentStatus.Caption := SStatus;
end;

Procedure TFInfoCompile.SetCanClose;
begin
  ToAbort        := False;
  BClose.Kind    := bkOk;
  BClose.Caption := lisMenuClose;
end;

Procedure TFInfoCompile.MakeBold;
begin
  PCurrentStatus.Font.Style:= PCurrentStatus.Font.Style + [fsBold];
end;

Procedure PutInfoBuilderStatus(Const Info : String);
begin
  if Assigned(FInfoCompile) then FInfoCompile.SetStatus(Info);
end;

Procedure DestroyInfoBuilder;
begin
  if Assigned(FInfoCompile) then
  begin
    FInfoCompile.Free;
    FinfoCompile := Nil;
  end;
end;

Procedure CreateInfoBuilder(Owner: TComponent);
begin
  //DebugLn(['CreateInfoBuilder ',ShowCompileDialog]);
  DestroyInfoBuilder;
  if ShowCompileDialog then
  begin
    FInfoCompile := TFInfoCompile.Create(Owner);
    FInfoCompile.Show;
  end;
end;

Procedure PutInfoBuilderProject(Const Info : String);
begin
  if Assigned(FInfoCompile) then FInfoCompile.SetProjectName(Info);
end;

Procedure AbleInfoBuilderExit;
begin
  if Assigned(FInfoCompile) then FInfoCompile.SetCanClose;
end;

Procedure PutExitInfoBuilder(Const Info : String);
begin
  PutInfoBuilderStatus(Info);
  if Assigned(FInfoCompile) then FInfoCompile.MakeBold;
  AbleInfoBuilderExit;
end;

initialization
  {$I infobuild.lrs}

end.


