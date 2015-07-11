{
 **********************************************************************
  This file is part of the Free Pascal run time library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

 DataExport progress bar form.

 Copyright (c) 2007 by Michael Van Canneyt, member of the Free Pascal development team
}
unit frmexportprogress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, sdb_consts;

type

  { TExportProgressForm }

  TExportProgressForm = class(TForm)
    BCancel: TButton;
    LProgress: TLabel;
    PBExport: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FCount : Integer;
    function GetOnCancel: TNotifyEvent;
    procedure SetOnCancel(const AValue: TNotifyEvent);
  public
    { public declarations }
    Procedure StepIt;
    Property OnCancel : TNotifyEvent Read GetOnCancel Write SetOnCancel;
  end;

var
  ExportProgressForm: TExportProgressForm;

implementation

{$R *.lfm}

{ TExportProgressForm }

procedure TExportProgressForm.FormCreate(Sender: TObject);
begin
  //
  Caption := sdb_Exportprogress;
  LProgress.Caption:= Format(SProgress,[0]);
  BCancel.Caption:= sdb_Cancel;
  //
end;

function TExportProgressForm.GetOnCancel: TNotifyEvent;
begin
  Result:=BCancel.OnClick;
end;

procedure TExportProgressForm.SetOnCancel(const AValue: TNotifyEvent);
begin
  BCancel.OnClick:=AValue;
end;

procedure TExportProgressForm.StepIt;
begin
  Inc(FCount);
  LProgress.Caption:=Format(SProgress,[FCount]);
  If (PBExport.Position>=PBExport.Max) then
    PBExport.Position:=0;
  PBExport.StepIt;
  Application.ProcessMessages;
end;

end.

