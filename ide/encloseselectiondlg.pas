{
/***************************************************************************
                           encloseselectiondlg.pas
                           -----------------------

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

  Author: Mattias Gaertner
  
  Abstract: Dialog to setup parameters of the enclose selection function
}
unit EncloseSelectionDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, BasicCodeTools, CodeToolManager, SourceChanger,
  LazarusIDEStrConsts, LazConf, IDEProcs;

type
  TEncloseSelectionType = (
    estTryFinally,
    estTryExcept,
    estBeginEnd,
    estForBeginEnd,
    estWhileDoBeginEnd,
    estRepeatUntil,
    estPascalComment
    );
    
  TEncloseSelectionDialog = class(TForm)
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    TypeRadiogroup: TRADIOGROUP;
    procedure EncloseSelectionDialogCREATE(Sender: TObject);
  private
  public
    function GetEncloseType: TEncloseSelectionType;
  end;
  
var
  EncloseSelectionDialog: TEncloseSelectionDialog;
  
function EncloseSelectionTypeDescription(TheType: TEncloseSelectionType
  ): string;
function ShowEncloseSelectionDialog(var TheType: TEncloseSelectionType
  ): TModalResult;
procedure GetEncloseSelectionParams(TheType: TEncloseSelectionType;
  var Template: string);
procedure EncloseTextSelection(const Template: string; Source: TStrings;
  SelectionStart, SelectionEnd: TPoint;
  Indent: integer;
  var NewSelection: string; var NewCursor: TPoint);

implementation

function EncloseSelectionTypeDescription(TheType: TEncloseSelectionType
  ): string;
begin
  case TheType of
  estTryFinally: Result:='Try..Finally';
  estTryExcept: Result:='Try..Except';
  estBeginEnd: Result:='Begin..End';
  estForBeginEnd: Result:='For | do begin..end';
  estWhileDoBeginEnd: Result:='While | do begin..end';
  estRepeatUntil: Result:='Repeat..Until |';
  estPascalComment: Result:='{..}';
  else
    RaiseException('EncloseSelectionTypeDescription');
  end;
end;

function ShowEncloseSelectionDialog(var TheType: TEncloseSelectionType
  ): TModalResult;
var
  TheDialog: TEncloseSelectionDialog;
begin
  TheDialog:=TEncloseSelectionDialog.Create(Application);
  Result:=TheDialog.ShowModal;
  if Result=mrOk then
    TheType:=TheDialog.GetEncloseType;
  TheDialog.Free;
end;

procedure GetEncloseSelectionParams(TheType: TEncloseSelectionType;
  var Template: string);
begin
  case TheType of
  estTryFinally:
    Template:='try'+LineBreak
             +'  <selection>'+LineBreak
             +'finally'+LineBreak
             +'  |'+LineBreak
             +'end;';

  estTryExcept:
    Template:='try'+LineBreak
             +'  <selection>'+LineBreak
             +'except'+LineBreak
             +'  |'+LineBreak
             +'end;';

  estBeginEnd:
    Template:='begin'+LineBreak
             +'  |<selection>'+LineBreak
             +'end;';

  estForBeginEnd:
    Template:='for | do begin'+LineBreak
             +'  <selection>'+LineBreak
             +'end;';

  estWhileDoBeginEnd:
    Template:='while | do begin'+LineBreak
             +'  <selection>'+LineBreak
             +'end;';

  estRepeatUntil:
    Template:='repeat'+LineBreak
             +'  <selection>'+LineBreak
             +'until |;';

  estPascalComment:
    Template:='{'+LineBreak
             +'  |<selection>'+LineBreak
             +'}';

  else
    RaiseException('GetEnclosedSelectionParams');
  end;
end;

procedure EncloseTextSelection(const Template: string; Source: TStrings;
  SelectionStart, SelectionEnd: TPoint;
  Indent: integer;
  var NewSelection: string; var NewCursor: TPoint);
const
  TemplateIndent = 2;
var
  TemplateLen: Integer;
  TemplatePos: Integer;
  LastWrittenTemplatePos: Integer;
  NewSelect: TMemoryStream;
  Y: Integer;
  X: Integer;
  ExtraIndent: Integer;

  procedure AddBeautified(const s: string);
  var
    NewStr: String;
    LengthOfLastLine: integer;
    LineEndCnt: Integer;
  begin
    if s='' then exit;
    NewStr:=s;
    writeln('AddBeautified A X=',X,' Y=',Y,' ExtraIndent=',ExtraIndent,' NewSTr="',NewSTr,'"');
    NewStr:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyStatement(
                NewStr,ExtraIndent,
                [bcfIndentExistingLineBreaks,bcfDoNotIndentFirstLine]);
    LineEndCnt:=LineEndCount(NewStr,LengthOfLastLine);
    inc(Y,LineEndCnt);
    if LineEndCnt=0 then
      inc(X,LengthOfLastLine)
    else begin
      X:=LengthOfLastLine+1;
      ExtraIndent:=GetLineIndent(NewStr,length(NewStr)+1);
    end;
    writeln('AddBeautified B X=',X,' Y=',Y,' ExtraIndent=',ExtraIndent,' NewSTr="',NewSTr,'"');
    NewSelect.Write(NewStr[1],length(NewStr));
  end;
  
  procedure FlushTemplate;
  var
    FromPos: Integer;
    ToPos: Integer;
  begin
    FromPos:=LastWrittenTemplatePos+1;
    ToPos:=TemplatePos-1;
    if ToPos>TemplateLen then ToPos:=TemplateLen;
    if FromPos<=ToPos then
      AddBeautified(copy(Template,FromPos,ToPos-FromPos+1));
    LastWrittenTemplatePos:=ToPos;
  end;
  
  procedure CalculateCursorPos;
  begin
    NewCursor:=Point(X,Y);
  end;
  
  procedure InsertSelection;
  var
    CurY: Integer;
    CurLine: string;
    IndentStr: String;
    MinX: Integer;
    MaxX: Integer;
  begin
    IndentStr:=GetIndentStr(ExtraIndent);
    for CurY:=SelectionStart.Y to SelectionEnd.Y do begin
      CurLine:=Source[CurY-1];
      MinX:=1;
      MaxX:=length(CurLine);
      if (CurY=SelectionStart.Y) then begin
        MinX:=SelectionStart.X;
        if MinX>MaxX then
          MinX:=MaxX;
      end;
      if (CurY=SelectionEnd.Y) and (MaxX>SelectionEnd.X) then
        MaxX:=SelectionEnd.X;
      // write indent
      if (IndentStr<>'') and (CurY<>SelectionStart.Y) then
        NewSelect.Write(IndentStr[1],length(IndentStr));
      // write line
      if MaxX>MinX then
        NewSelect.Write(CurLine[MinX],MaxX-MinX+1);
    end;
  end;
  
  procedure ParseMacro;
  var
    MacroNameStart: Integer;
    MacroNameEnd: Integer;
    
    function MacroNameIs(const Name: string): boolean;
    begin
      Result:=CompareText(@Template[MacroNameStart],MacroNameEnd-MacroNameStart,
                          @Name[1],length(Name),false)=0;
    end;
    
  begin
    FlushTemplate;
    inc(TemplatePos);
    MacroNameStart:=TemplatePos;
    while (TemplatePos<=TemplateLen)
    and (Template[TemplatePos] in ['a'..'z','A'..'Z','_','0'..'9']) do
      inc(TemplatePos);
    MacroNameEnd:=TemplatePos;
    if (TemplatePos<=TemplateLen) and (Template[TemplatePos]='>') then begin
      LastWrittenTemplatePos:=TemplatePos;
      inc(TemplatePos);
      if MacroNameIs('Selection') then begin
        InsertSelection;
      end;
    end;
  end;
  
begin
  writeln('EncloseTextSelection A ',SelectionStart.X,',',SelectionStart.Y,'-',SelectionEnd.X,',',SelectionEnd.Y,
    ' indent=',Indent,' Template="',Template,'"');
  NewSelect:=TMemoryStream.Create;
  NewCursor:=SelectionStart;
  X:=NewCursor.X;
  Y:=NewCursor.Y;
  ExtraIndent:=0;
  if Y<Source.Count then
    ExtraIndent:=GetLineIndent(Source[Y-1],X);
  writeln('AAA1 ',X,',',Y,' ',ExtraIndent,' "',Source[Y-1],'"');
  try
    TemplateLen:=length(Template);
    TemplatePos:=1;
    LastWrittenTemplatePos:=TemplatePos-1;
    while TemplatePos<=TemplateLen do begin
      case Template[TemplatePos] of
        '\':
          begin
            FlushTemplate;
            LastWrittenTemplatePos:=TemplatePos;
            inc(TemplatePos,2);
          end;
          
        '|':
          begin
            FlushTemplate;
            CalculateCursorPos;
            LastWrittenTemplatePos:=TemplatePos;
            inc(TemplatePos);
          end;
          
        '<':
          ParseMacro;

      else
        inc(TemplatePos);
      end;
    end;
    FlushTemplate;
  finally
    SetLength(NewSelection,NewSelect.Size);
    if NewSelection<>'' then begin
      NewSelect.Position:=0;
      NewSelect.Read(NewSelection[1],length(NewSelection));
    end;
    NewSelect.Free;
  end;
end;

{ TEncloseSelectionDialog }

procedure TEncloseSelectionDialog.EncloseSelectionDialogCREATE(Sender: TObject);
var
  t: TEncloseSelectionType;
begin
  with TypeRadiogroup.Items do begin
    BeginUpdate;
    for t:=Low(TEncloseSelectionType) to High(TEncloseSelectionType) do
      Add(EncloseSelectionTypeDescription(t));
    EndUpdate;
  end;
  TypeRadiogroup.ItemIndex:=0;
end;

function TEncloseSelectionDialog.GetEncloseType: TEncloseSelectionType;
var
  i: Integer;
begin
  i:=TypeRadiogroup.ItemIndex;
  for Result:=Low(TEncloseSelectionType) to High(TEncloseSelectionType) do
    if AnsiCompareText(TypeRadiogroup.Items[i],
                       EncloseSelectionTypeDescription(Result))=0
    then
      exit;
  RaiseException('TEncloseSelectionDialog.GetEncloseType');
end;

initialization
  {$I encloseselectiondlg.lrs}

end.

