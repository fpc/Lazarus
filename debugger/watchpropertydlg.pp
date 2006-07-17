{ $Id$ }
{               ----------------------------------------------
                 watchproperydlg.pp  -  property editor for 
                                        watches
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the watch property dialog.


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

unit WatchPropertyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,
  Buttons, Extctrls, Menus,
  IDEContextHelpEdit, Debugger, BaseDebugManager;

type

  { TWatchPropertyDlg }

  TWatchPropertyDlg = class(TForm)
    lblExpression: TLabel;
    lblRepCount: TLabel;
    lblDigits: TLabel;
    chkEnabled: TCHeckbox;
    chkAllowFunc: TCheckbox;
    rgStyle: TRadioGroup;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    txtExpression: TEdit;
    txtRepCount: TEdit;
    txtDigits: TEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FWatch: TIDEWatch;
  public
    constructor Create(AOWner: TComponent; const AWatch: TIDEWatch); overload;
    destructor Destroy; override;
  end;
  
implementation

{ TWatchPropertyDlg }

procedure TWatchPropertyDlg.btnOKClick(Sender: TObject);
begin
  if FWatch = nil
  then begin
    FWatch := DebugBoss.Watches.Add(txtExpression.Text);
  end
  else begin
    FWatch.Expression := txtExpression.Text;
  end;
  
  FWatch.Enabled := chkEnabled.Checked;
end;

procedure TWatchPropertyDlg.btnHelpClick(Sender: TObject);
begin
  ShowContextHelpForIDE(btnHelp);
end;

constructor TWatchPropertyDlg.Create(AOwner: TComponent; const AWatch: TIDEWatch);
begin
  FWatch := AWatch;
  inherited Create(AOwner);
  if FWatch = nil
  then begin 
    chkEnabled.Checked := True;
  end
  else begin
    txtExpression.Text := FWatch.Expression;
    chkEnabled.Checked := FWatch.Enabled;
  end;
  
  lblRepCount.Enabled := False;
  txtRepCount.Enabled := False;
  lblDigits.Enabled := False;
  txtDigits.Enabled := False;
  chkAllowFunc.Enabled := False;
  rgStyle.Enabled := False;
end;

destructor TWatchPropertyDlg.destroy;
begin
  inherited;
end;

initialization
{$I watchpropertydlg.lrs}



end.

