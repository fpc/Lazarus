{  $Id$  }
{
 /***************************************************************************
                    condef.pas  -  Conditional Defines
                    ----------------------------------

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
unit ConDef;

{$mode objfpc}{$H+}

interface

(* Utility to assist in inserting conditional defines. For example, to convert
    OnCreate := @CreateHandler
  to:
    OnCreate := {$IFDEF FPC} @ {$ENDIF} CreateHandler
  select @ and then use Edit, Insert $IFDEF (default shortcut Ctrl+Shift+D),
  select "FPC,NONE" and hit return. If you select one or more complete lines
  then the conditional defines are put on sepearate lines as in:
  {$IFDEF DEBUG}
  Writeln('State= ', State)
  {$ENDIF}
  The choices are listed in abbreviated form so:
    MSWINDOWS,UNIX => {$IFDEF MSWINDOWS} ... {$ENDIF} {$IFDEF UNIX} ... {$ENDIF}
    FPC,ELSE       => {$IFDEF FPC} ... {$ELSE} ... {$ENDIF}
    DEBUG,NONE     => {$IFDEF DEBUG} ... {$ENDIF}
  This tool is most useful when you need to put several identical conditionals
  in a file, You can add to the possible conditionals by selecting or typing
  the required symbols in "First test" and /or "Second test" and using the
  Add button. Your additons are saved in the condef.xml file in the lazarus
  configuration directory.
*)

uses
  Classes, SysUtils, Messages, Graphics, Controls, Forms, LCLProc, Dialogs,
  StdCtrls, Buttons, FileUtil,
  Laz_XMLCfg,
  LazarusIDEStrConsts, IDEContextHelpEdit, IDEProcs, ButtonPanel;

type

  { TCondForm }

  TCondForm = class(TForm)
    AddBtn: TBitBtn;
    AddInverse: TButton;
    ButtonPanel: TButtonPanel;
    FirstLabel: TLabel;
    FirstTest: TComboBox;
    ListBox: TListBox;
    NewTestGroupBox: TGroupBox;
    RemoveBtn: TBitBtn;
    SecondLabel: TLabel;
    SecondTest: TComboBox;
    procedure AddBtnClick(Sender: TObject);
    procedure AddInverseCLICK(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CondFormCLOSE(Sender: TObject; var CloseAction: TCloseAction);
    procedure CondFormCREATE(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  public
    Choice, First, Second, FS: string;
    procedure DeleteSelected;
    procedure SaveChoices;
    function CreateXMLConfig: TXMLConfig;
  end;


function ShowConDefDlg: string;
function AddConditional(Text: string; IsPascal: Boolean):string;

implementation

{$R *.lfm}

uses
  LCLType, LazConf;

function ShowConDefDlg: string;
var
  DialogResult: Integer;
  CondForm: TCondForm;
begin
  Result := '';
  CondForm := TCondForm.Create(nil);
  try
    CondForm.ActiveControl := CondForm.ListBox;
    DialogResult := CondForm.ShowModal;
    if DialogResult <> mrOK then
      Result := ''
    else
      Result := CondForm.FS;
  finally
    CondForm.Free;
  end
end;

procedure TCondForm.AddBtnClick(Sender: TObject);
begin
  ListBox.Items.Add(FirstTest.Text+','+SecondTest.Text);
end;

procedure TCondForm.AddInverseCLICK(Sender: TObject);
begin
  ListBox.Items.Add('!'+FirstTest.Text+','+SecondTest.Text);
end;

procedure TCondForm.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TCondForm.CondFormCLOSE(Sender: TObject; var CloseAction: TCloseAction);
var
  SChanged: Boolean;
  i: Integer;
  procedure SUpdate(var s: string; n: string);
  begin
    if s <> n then begin
      SChanged := True;
      s := n;
    end;
  end;
begin
  SChanged := False;
  with ListBox do begin
    SUpdate(Choice,Items.CommaText);
    if ItemIndex >= 0 then begin
      FS := Items[ItemIndex];
      i := Pos(',', FS);
      if i > 0 then begin
        SUpdate(First, Copy(FS, 1, i-1));
        SUpdate(Second, Copy(FS, i+1, Length(FS)));
      end
    end;
  end;
  if SChanged then
    SaveChoices;
end;

procedure TCondForm.CondFormCREATE(Sender: TObject);
var
  i: Integer;
  XMLConfig: TXMLConfig;
begin
  NewTestGroupBox.Caption := rsCreateNewDefine;
  Caption := rsConditionalDefines;
  AddBtn.Caption := liswlAdd;
  AddInverse.Caption := rsAddInverse;
  RemoveBtn.Caption := rsRemove;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
  try
    XMLConfig:=CreateXMLConfig;
    try
      Choice := XMLConfig.GetValue('condef/Choice', '"MSWINDOWS,UNIX","MSWINDOWS,ELSE","FPC,NONE","FPC,ELSE","DEBUG,NONE"');
      First := XMLConfig.GetValue('condef/First', 'MSWINDOWS');
      Second := XMLConfig.GetValue('condef/Second', 'UNIX');
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      debugln('TCondForm.CondFormCREATE ',E.Message);
    end;
  end;
  with ListBox do begin
    Items.CommaText := Choice;
    i := Items.IndexOf(First+','+Second);
    if i < 0 then begin
      Items.Add(First+','+Second);
      ItemIndex := 0;
    end else
      ItemIndex := i;
  end;
end;

procedure TCondForm.ListBoxDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TCondForm.RemoveBtnClick(Sender: TObject);
begin
  DeleteSelected;
end;

procedure TCondForm.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then begin
    DeleteSelected;
    Key := 0;
  end;
end;

procedure TCondForm.FormShow(Sender: TObject);
begin
  if SecondTest.Items.Count < 10 then
    SecondTest.Items.AddStrings(FirstTest.Items);
end;

procedure TCondForm.DeleteSelected;
var
  i: Integer;
begin
  with ListBox.Items do
    for i := Count-1 downto 0 do
      if ListBox.Selected[i] then
        Delete(i);
end;

procedure TCondForm.SaveChoices;
var
  XMLConfig: TXMLConfig;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=CreateXMLConfig;
    try
      XMLConfig.SetValue('condef/Choice', Choice);
      XMLConfig.SetValue('condef/First', First);
      XMLConfig.SetValue('condef/Second', Second);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      debugln('TCondForm.SaveChoices ',E.Message);
    end;
  end;
end;

function TCondForm.CreateXMLConfig: TXMLConfig;
var
  ConfFileName: String;
begin
  Result:=nil;
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/condef.xml');
  try
    if (not FileExistsUTF8(ConfFileName)) then
      Result:=TXMLConfig.CreateClean(ConfFileName)
    else
      Result:=TXMLConfig.Create(ConfFileName);
  except
    on E: Exception do begin
      debugln('TCondForm.CreateXMLConfig ',E.Message);
    end;
  end;
end;

function AddConditional(Text: string; IsPascal: Boolean):string;
var
  cond, s, f: string;
  p, p1: Integer;
  IsElse, IsTwo, HasNewline: Boolean;
  Tail, Indent: string;
  function ifdef(s:string):string;
  begin
    if (s <>'') and (s[1] = '!') then begin
      if IsPascal then
        Result := 'N'
      else
        Result := 'n';
      s := Copy(s,2,Length(s)-1);
    end;
    if IsPascal then
      Result := '{$IF' + Result + 'DEF ' + s + '}'
    else
      Result := '#if' + Result + 'def ' + s;
  end;
begin
  Result := Text;
  cond := ShowConDefDlg;
  p := Pos(',',cond);
  if p <= 0 then Exit;
  f := Copy(Cond, 1, p-1);
  s := Copy(Cond, p+1, Length(Cond));
  IsElse := CompareText(s, 'ELSE') = 0;
  IsTwo := CompareText(s, 'NONE') <> 0;
  HasNewline := Pos(#10, Text) > 0;
  if HasNewline then begin
    p := 1;
    { leave leading newlines unchanged (outside $IFDEF) }
    while (p <= Length(Text)) and (Text[p] in [#10,#13]) do Inc(p);
    Result := Copy(Text,1,p-1);
    p1 := p;
    { Work out current indentation, to line up $IFDEFS }
    while (p <= Length(Text)) and (Text[p] in [#9,' ']) do Inc(p);
    Indent := Copy(Text, p1, p-p1);
    Text := Copy(Text,p,Length(Text));
    p := Length(Text);
    { Tailing whitespace is left outside $IFDEF }
    while (p>0) and (Text[p] in [' ',#9,#10,#13]) do Dec(p);
    Tail := Copy(Text, p+1, Length(Text));
    SetLength(Text,p);
  end else begin
    Result := '';
    Tail := '';
    Indent := '';
  end;
  if IsPascal then begin
    f := ifdef(f);
    s := ifdef(s);
    if HasNewline then begin
      Result := Result + Indent + f + LineEnding + Indent + Text + LineEnding;
      if IsElse then
        Result := Result + Indent + '{$ELSE}' + LineEnding
      else begin
        Result := Result + Indent + '{$ENDIF}';
        if IsTwo then
          Result := Result + LineEnding + Indent + s + LineEnding;
      end;
      if IsTwo then
        Result := Result + Indent + Text + LineEnding + Indent + '{$ENDIF}';
      Result := Result + Tail;
    end else begin
      Result := Result + f + ' ' + Text;
      if IsElse then
        Result := Result + ' {$ELSE} '
      else begin
        Result := Result + ' {$ENDIF}';
        if IsTwo then
          Result := Result + ' ' + s + ' ';
      end;
      if IsTwo then
        Result := Result + Text + ' {$ENDIF}';
    end;
  end else begin
    Result := Result + ifdef(f) + LineEnding + indent + Text + LineEnding;
    if IsElse then
      Result := Result + '#else' + LineEnding
    else begin
      Result := Result + '#endif /* ' + f + ' */' + LineEnding;
      if IsTwo then
        Result := Result + ifdef(s) + LineEnding;
    end;
    if IsTwo then begin
      Result := Result + indent + Text + LineEnding + '#endif /* ';
      if IsElse then
        Result := Result + f
      else
        Result := Result + s;
      Result := Result + ' */' + LineEnding;
    end;
    Result := Result + Tail;
  end;
end;

end.
