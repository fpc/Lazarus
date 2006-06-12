{ $Id$ }
{               ----------------------------------------------
                 evaluatedlg.pp  -  Evaluate and Modify
                ----------------------------------------------

 @created(Mon Nov 22st WET 2004)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@freepascal.org>)

 This unit contains the evaluate and modify dialog.


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

unit EvaluateDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLType, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, DebuggerDlg, BaseDebugManager;

type

  { TEvaluateDlg }

  TEvaluateDlg = class(TDebuggerDlg)
    cmbExpression: TComboBox;
    cmbNewValue: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    lblNewValue: TLabel;
    txtResult: TMemo;
    ToolBar1: TToolBar;
    tbInspect: TToolButton;
    tbWatch: TToolButton;
    tbModify: TToolButton;
    tbEvaluate: TToolButton;
    procedure FormShow(Sender: TObject);
    procedure cmbExpressionChange(Sender: TObject);
    procedure cmbExpressionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbEvaluateClick(Sender: TObject);
    procedure tbWatchClick(Sender: TObject);
    
  private
    function GetFindText: string;
    procedure SetFindText(const NewFindText: string);

  public
    property FindText: string read GetFindText write SetFindText;

  end;

implementation

{ TEvaluateDlg }

procedure TEvaluateDlg.cmbExpressionChange(Sender: TObject);
var
  HasExpression: Boolean;
begin
  HasExpression := Trim(cmbExpression.Text) <> '';
  tbEvaluate.Enabled := HasExpression;
  tbModify.Enabled := False;
  tbWatch.Enabled := HasExpression;
//  tbInspect.Enabled := HasExpression;
end;

procedure TEvaluateDlg.cmbExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and tbEvaluate.Enabled
  then begin
    tbEvaluate.Click;
    Key := 0;
  end;
end;

procedure TEvaluateDlg.SetFindText(const NewFindText: string);
begin
  if NewFindText<>'' then begin
    cmbExpression.Text := NewFindText;
    cmbExpression.SelectAll;
    tbEvaluate.Click;
  end;
  ActiveControl := cmbExpression;
end;

function TEvaluateDlg.GetFindText: string;
begin
  Result := cmbExpression.Text;
end;


procedure TEvaluateDlg.FormShow(Sender: TObject);
begin
  cmbExpression.SetFocus;
end;

procedure TEvaluateDlg.tbEvaluateClick(Sender: TObject);
var
  S, R: String;
begin
  S := cmbExpression.Text;
  if DebugBoss.Evaluate(S, R)
  then begin
    if cmbExpression.Items.IndexOf(S) = -1
    then cmbExpression.Items.Insert(0, S);
//    tbModify.Enabled := True;
  end;
  txtResult.Lines.Text := R;
end;

procedure TEvaluateDlg.tbWatchClick(Sender: TObject);
var
  S: String;
begin
  S := cmbExpression.Text;
  if DebugBoss.Watches.Find(S) = nil
  then DebugBoss.Watches.Add(S);
end;

initialization
  {$I evaluatedlg.lrs}

end.


