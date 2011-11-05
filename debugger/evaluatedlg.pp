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
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs,
  IDEWindowIntf, IDEOptionDefs,
  ComCtrls, StdCtrls, DebuggerDlg, BaseDebugManager,
  InputHistory, Debugger;

type

  { TEvaluateDlg }

  TEvaluateDlg = class(TDebuggerDlg)
    cmbExpression: TComboBox;
    cmbNewValue: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    lblNewValue: TLabel;
    txtResult: TMemo;
    ToolBar1: TToolBar;
    tbInspect: TToolButton;
    tbWatch: TToolButton;
    tbModify: TToolButton;
    tbEvaluate: TToolButton;
    procedure cmbNewValueKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure cmbExpressionChange(Sender: TObject);
    procedure cmbExpressionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tbEvaluateClick(Sender: TObject);
    procedure tbInspectClick(Sender: TObject);
    procedure tbModifyClick(Sender: TObject);
    procedure tbWatchClick(Sender: TObject);
    
  private
    function GetFindText: string;
    procedure SetFindText(const NewFindText: string);
    procedure Evaluate;
    procedure Modify;
  public
    constructor Create(TheOwner: TComponent); override;
    property FindText: string read GetFindText write SetFindText;
  end;

implementation

{$R *.lfm}

uses
  IDEImagesIntf, LazarusIDEStrConsts;

var
  EvaluateDlgWindowCreator: TIDEWindowCreator;

{ TEvaluateDlg }

constructor TEvaluateDlg.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner);

  Caption := lisKMEvaluateModify;
  cmbExpression.Items.Assign(InputHistories.HistoryLists.GetList(ClassName, True));

  tbEvaluate.Caption := lisEvaluate;
  tbModify.Caption := lisModify;
  tbWatch.Caption := lisWatch;
  tbInspect.Caption := lisInspect;

  Label1.Caption := lisDBGEMExpression;
  Label2.Caption := lisDBGEMResult;
  lblNewValue.Caption := lisDBGEMNewValue;

  ToolBar1.Images := IDEImages.Images_16;
  tbInspect.ImageIndex := IDEImages.LoadImage(16, 'debugger_inspect');
  tbWatch.ImageIndex := IDEImages.LoadImage(16, 'debugger_watches');
  tbModify.ImageIndex := IDEImages.LoadImage(16, 'debugger_modify');
  tbEvaluate.ImageIndex := IDEImages.LoadImage(16, 'debugger_evaluate');
end;

procedure TEvaluateDlg.Evaluate;
var
  S, R: String;
  DBGType: TDBGType;
begin
  S := cmbExpression.Text;
  InputHistories.HistoryLists.Add(ClassName, S);
  DBGType:=nil;
  if DebugBoss.Evaluate(S, R, DBGType)
  then begin
    if cmbExpression.Items.IndexOf(S) = -1
    then cmbExpression.Items.Insert(0, S);
    tbModify.Enabled := True;
  end
  else
    tbModify.Enabled := False;
  FreeAndNil(DBGType);
  txtResult.Lines.Text := R;
end;

procedure TEvaluateDlg.cmbExpressionChange(Sender: TObject);
var
  HasExpression: Boolean;
begin
  HasExpression := Trim(cmbExpression.Text) <> '';
  tbEvaluate.Enabled := HasExpression;
  tbModify.Enabled := HasExpression;
  tbWatch.Enabled := HasExpression;
  tbInspect.Enabled := HasExpression;
end;

procedure TEvaluateDlg.cmbExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and tbEvaluate.Enabled
  then begin
    Evaluate;
    Key := 0;
  end;
end;

procedure TEvaluateDlg.SetFindText(const NewFindText: string);
begin
  if NewFindText<>'' then
  begin
    cmbExpression.Text := NewFindText;
    cmbExpressionChange(nil);
    cmbExpression.SelectAll;
    tbEvaluate.Click;
  end;
  ActiveControl := cmbExpression;
end;

function TEvaluateDlg.GetFindText: string;
begin
  Result := cmbExpression.Text;
end;

procedure TEvaluateDlg.Modify;
var
  S, V, R: String;
  DBGType: TDBGType;
begin
  S := Trim(cmbExpression.Text);
  if S = '' then Exit;
  V := cmbNewValue.Text;
  if not DebugBoss.Modify(S, V) then Exit;

  if cmbNewValue.Items.IndexOf(V) = -1
  then cmbNewValue.Items.Insert(0, V);

  DBGType:=nil;
  if not DebugBoss.Evaluate(S, R, DBGType) then Exit;
  FreeAndNil(DBGType);
  txtResult.Lines.Text := R;
end;

procedure TEvaluateDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TEvaluateDlg.cmbNewValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (tbModify.Enabled)
  then begin
    Modify;
    Key := 0;
  end;
end;

procedure TEvaluateDlg.FormShow(Sender: TObject);
begin
  cmbExpression.SetFocus;
end;

procedure TEvaluateDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close
  else
    inherited;;
end;

procedure TEvaluateDlg.tbEvaluateClick(Sender: TObject);
begin
  Evaluate;
end;

procedure TEvaluateDlg.tbInspectClick(Sender: TObject);
begin
  DebugBoss.Inspect(cmbExpression.Text);
end;

procedure TEvaluateDlg.tbModifyClick(Sender: TObject);
begin
  Modify;
end;

procedure TEvaluateDlg.tbWatchClick(Sender: TObject);
var
  S: String;
  Watch: TCurrentWatch;
begin
  S := cmbExpression.Text;
  if DebugBoss.Watches.CurrentWatches.Find(S) = nil
  then begin
    Watch := DebugBoss.Watches.CurrentWatches.Add(S);
    Watch.Enabled := True;
  end;
  DebugBoss.ViewDebugDialog(ddtWatches);
end;

initialization

  EvaluateDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtEvaluate]);
  EvaluateDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  EvaluateDlgWindowCreator.CreateSimpleLayout;

end.


