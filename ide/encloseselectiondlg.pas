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
  ExtCtrls, LazarusIDEStrConsts, LazConf, IDEProcs;

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
             +'end;'+LineBreak;

  estTryExcept:
    Template:='try'+LineBreak
             +'  <selection>'+LineBreak
             +'except'+LineBreak
             +'  |'+LineBreak
             +'end;'+LineBreak;

  estBeginEnd:
    Template:='begin'+LineBreak
             +'  |<selection>'+LineBreak
             +'end;'+LineBreak;

  estForBeginEnd:
    Template:='for | do begin'+LineBreak
             +'  <selection>'+LineBreak
             +'end;'+LineBreak;

  estWhileDoBeginEnd:
    Template:='while | do begin'+LineBreak
             +'  <selection>'+LineBreak
             +'end;'+LineBreak;

  estRepeatUntil:
    Template:='repeat'+LineBreak
             +'  <selection>'+LineBreak
             +'until |;'+LineBreak;

  estPascalComment:
    Template:='{'+LineBreak
             +'  |<selection>'+LineBreak
             +'}'+LineBreak;

  else
    RaiseException('GetEnclosedSelectionParams');
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

