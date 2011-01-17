{ Debug server options form

  Copyright (C) 2009 Michael Van Canneyt (michael@freepascal.org)

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit frmOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBNewVisible: TCheckBox;
    CBCleanLogOnNewProcess: TCheckBox;
    CBShowOnStartUp: TCheckBox;
    CBShowOnMessage: TCheckBox;
    CBNewAtBottom: TCheckBox;
    GBWindow: TGroupBox;
    GBMessages: TGroupBox;
  private
    function GetB(AIndex: integer): Boolean;
    function GetCB(AIndex: Integer): TCheckBox;
    procedure SetB(AIndex: integer; const AValue: Boolean);
    { private declarations }
  public
    { public declarations }
    Property ShowOnStartup : Boolean Index 0 Read GetB Write SetB;
    Property ShowOnMessage : Boolean Index 1 Read GetB Write SetB;
    Property NewMessageAtBottom : Boolean Index 2 Read GetB Write SetB;
    Property NewMessageVisible: Boolean Index 3 Read GetB Write SetB;
    Property CleanLogOnNewProcess: Boolean Index 4 Read GetB Write SetB;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }

function TOptionsForm.GetCB(AIndex : Integer) : TCheckBox;

begin
  Case AIndex of
    0 : Result:=CBShowOnStartUp;
    1 : Result:=CBShowOnMessage;
    2 : Result:=CBNewAtBottom;
    3 : Result:=CBNewVisible;
    4 : Result:=CBCleanLogOnNewProcess;
  end;
end;

function TOptionsForm.GetB(AIndex: integer): Boolean;
begin
  Result:=GetCb(AIndex).Checked;
end;

procedure TOptionsForm.SetB(AIndex: integer; const AValue: Boolean);
begin
  GetCb(AIndex).Checked:=AValue;
end;

end.

