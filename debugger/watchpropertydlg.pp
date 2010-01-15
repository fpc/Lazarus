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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Extctrls, Menus,
  IDEContextHelpEdit, Debugger, BaseDebugManager, ButtonPanel;

type

  { TWatchPropertyDlg }

  TWatchPropertyDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    lblExpression: TLabel;
    lblRepCount: TLabel;
    lblDigits: TLabel;
    chkEnabled: TCHeckbox;
    chkAllowFunc: TCheckbox;
    rgStyle: TRadioGroup;
    txtExpression: TEdit;
    txtRepCount: TEdit;
    txtDigits: TEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FWatch: TIDEWatch;
  public
    constructor Create(AOWner: TComponent; const AWatch: TIDEWatch; const AWatchExpression: String = ''); overload;
    destructor Destroy; override;
  end;
  
implementation

{$R *.lfm}

uses
  LazarusIDEStrConsts;
  
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
  ShowContextHelpForIDE(Self);
end;

constructor TWatchPropertyDlg.Create(AOwner: TComponent; const AWatch: TIDEWatch;
  const AWatchExpression: String = '');
begin
  FWatch := AWatch;
  inherited Create(AOwner);
  if FWatch = nil
  then begin 
    chkEnabled.Checked := True;
    txtExpression.Text := AWatchExpression;
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
  
  Caption:= lisWatchPropert;
  lblExpression.Caption:= lisExpression;
  lblRepCount.Caption:= lisRepeatCount;
  lblDigits.Caption:= lisDigits;
  chkEnabled.Caption:= lisEnabled;
  chkAllowFunc.Caption:= lisAllowFunctio;
  rgStyle.Caption:= lisStyle;
  rgStyle.Items[0]:= lisCharacter;
  rgStyle.Items[1]:= lisString;
  rgStyle.Items[2]:= lisDecimal;
  rgStyle.Items[3]:= lisHexadecimal;
  rgStyle.Items[4]:= lisFloatingPoin;
  rgStyle.Items[5]:= lisPointer;
  rgStyle.Items[6]:= lisRecordStruct;
  rgStyle.Items[7]:= dlgAssemblerDefault;
  rgStyle.Items[8]:= lisMemoryDump;

  ButtonPanel.OKButton.OnClick := @btnOKClick;
  ButtonPanel.HelpButton.OnClick := @btnHelpClick;
end;

destructor TWatchPropertyDlg.destroy;
begin
  inherited;
end;

end.

