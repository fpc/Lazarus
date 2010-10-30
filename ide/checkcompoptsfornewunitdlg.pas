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

  Author: Mattias: Gaertner

  Abstract:
    When a new unit is created check if compiler options in lpi and main source
    differ. This is a common mistake when upgrading old projects.
}
unit CheckCompOptsForNewUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  CodeToolManager, BasicCodeTools, DefineTemplates,
  ProjectIntf, IDEDialogs,
  InputHistory, TransferMacros, Project, LazarusIDEStrConsts;

type

  { TCheckCompOptsForNewUnitDialog }

  TCheckCompOptsForNewUnitDialog = class(TForm)
    AnsistringCheckBox: TCheckBox;
    DoNotWarnCheckBox: TCheckBox;
    OkButton: TButton;
    ModeComboBox: TComboBox;
    ModeLabel: TLabel;
    NoteLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    FMainAnsistring: char;
    FMainMode: string;
  public
    CompOpts: TLazCompilerOptions;
    procedure UpdateOptions;
    property MainMode: string read FMainMode write FMainMode;
    property MainAnsistring: char read FMainAnsistring write FMainAnsistring;
  end;

procedure CheckCompOptsAndMainSrcForNewUnit(CompOpts: TLazCompilerOptions);
function GetIgnorePathForCompOptsAndMainSrcDiffer(CompOpts: TLazCompilerOptions): string;

implementation

procedure CheckCompOptsAndMainSrcForNewUnit(CompOpts: TLazCompilerOptions);
var
  ProjCompOpts: TProjectCompilerOptions;
  MainUnit: TUnitInfo;
  Src: String;
  StartPos: Integer;
  p: PChar;
  Mode: String;
  AnsistringMode: Char;
  NestedComments: Boolean;
  Dlg: TCheckCompOptsForNewUnitDialog;
  IgnoreIdentifier: String;
begin
  if CompOpts is TProjectCompilerOptions then
  begin
    ProjCompOpts:=TProjectCompilerOptions(CompOpts);
    if (ProjCompOpts.LazProject=nil) then exit;
    MainUnit:=ProjCompOpts.LazProject.MainUnitInfo;
    if (MainUnit=nil) or (MainUnit.Source=nil) then exit;

    // check if this question should be ignored
    IgnoreIdentifier:=GetIgnorePathForCompOptsAndMainSrcDiffer(CompOpts);
    if (IgnoreIdentifier<>'')
    and (InputHistories.Ignores.Find(IgnoreIdentifier)<>nil) then
      exit;

    Src:=MainUnit.Source.Source;
    Mode:='';
    AnsistringMode:=#0;
    StartPos:=1;
    NestedComments:=false;
    repeat
      StartPos:=FindNextCompilerDirective(Src,StartPos,NestedComments);
      if StartPos>length(Src) then break;
      p:=@Src[StartPos];
      StartPos:=FindCommentEnd(Src,StartPos,NestedComments);
      if p^<>'{' then continue;
      inc(p);
      if p^<>'$' then continue;
      inc(p);
      if (Mode='') and (CompareIdentifiers(p,'mode')=0) then begin
        // mode directive
        inc(p,4);
        while p^ in [' ',#9] do inc(p);
        Mode:=GetIdentifier(p);
      end
      else if (AnsistringMode=#0) and (p^='H') and (p[1] in ['+','-']) then begin
        // ansistring directive
        AnsistringMode:=p[1];
      end;
    until false;
    //debugln(['CheckCompOptsAndMainSrcForNewUnit Mode=',Mode,' ProjMode=',ProjCompOpts.SyntaxMode,' Str=',AnsistringMode='+',' ProjStr=',ProjCompOpts.UseAnsiStrings]);
    if ((Mode<>'') and (SysUtils.CompareText(Mode,ProjCompOpts.SyntaxMode)<>0))
    or ((AnsistringMode<>#0) and ((AnsistringMode='+')<>ProjCompOpts.UseAnsiStrings))
    then begin
      Dlg:=TCheckCompOptsForNewUnitDialog.Create(nil);
      try
        Dlg.CompOpts:=CompOpts;
        Dlg.MainMode:=Mode;
        Dlg.MainAnsistring:=AnsistringMode;
        Dlg.UpdateOptions;
        Dlg.ShowModal;
      finally
        Dlg.Free;
      end;
    end;
  end;
end;

function GetIgnorePathForCompOptsAndMainSrcDiffer(CompOpts: TLazCompilerOptions
  ): string;
var
  ProjCompOpts: TProjectCompilerOptions;
begin
  Result:='';
  if (CompOpts is TProjectCompilerOptions) then
  begin
    ProjCompOpts:=TProjectCompilerOptions(CompOpts);
    if ProjCompOpts.LazProject<>nil then
      Result:='NewUnitProjOptsAndMainSrcDiffer/'+ProjCompOpts.LazProject.ProjectInfoFile;
  end;
end;

{$R *.lfm}

{ TCheckCompOptsForNewUnitDialog }

procedure TCheckCompOptsForNewUnitDialog.FormCreate(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
  Caption:=lisDirectivesForNewUnit;
  OkButton.Caption:=lisContinue;

  ModeLabel.Caption:=lisSyntaxMode;
  sl:=TStringList.Create;
  for i:=low(FPCSyntaxModes) to high(FPCSyntaxModes) do
    sl.Add(FPCSyntaxModes[i]);
  ModeComboBox.Items.Assign(sl);
  sl.Free;
  AnsistringCheckBox.Caption:=lisUseAnsistrings;
  DoNotWarnCheckBox.Caption:=lisDoNotShowThisDialogForThisProject;
end;

procedure TCheckCompOptsForNewUnitDialog.OkButtonClick(Sender: TObject);
var
  NewMode: String;
  i: Integer;
  IgnoreIdentifier: String;
begin
  NewMode:=ModeComboBox.Text;
  if SysUtils.CompareText(CompOpts.SyntaxMode,NewMode)<>0 then
  begin
    i:=low(FPCSyntaxModes);
    while (i<=High(FPCSyntaxModes))
    and (SysUtils.CompareText(FPCSyntaxModes[i],NewMode)<>0) do
      inc(i);
    if i>High(FPCSyntaxModes) then
    begin
      MessageDlg(lisCCOErrorCaption, Format(lisInvalidMode, [NewMode]),
        mtError, [mbCancel
        ], 0);
      exit;
    end;
  end;

  if (CompOpts.UseAnsiStrings<>AnsistringCheckBox.Checked)
  or (CompOpts.SyntaxMode<>NewMode) then
  begin
    CompOpts.UseAnsiStrings:=AnsistringCheckBox.Checked;
    CompOpts.SyntaxMode:=NewMode;
    IncreaseCompilerParseStamp;
  end;

  if DoNotWarnCheckBox.Checked then
  begin
    IgnoreIdentifier:=GetIgnorePathForCompOptsAndMainSrcDiffer(CompOpts);
    if IgnoreIdentifier<>'' then;
      InputHistories.Ignores.Add(IgnoreIdentifier,iiidForever);
  end;

  ModalResult:=mrOk;
end;

procedure TCheckCompOptsForNewUnitDialog.UpdateOptions;
begin
  NoteLabel.Caption:=lisTheProjectCompilerOptionsAndTheDirectivesInTheMain;
  AnsistringCheckBox.Checked:=CompOpts.UseAnsiStrings;
  ModeComboBox.Text:=CompOpts.SyntaxMode;
end;

end.

