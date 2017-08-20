{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Form to select a report data loop.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmideselectreportdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
  ButtonPanel, StdCtrls;

type

  { TSelectReportDataSourcesForm }

  TSelectReportDataSourcesForm = class(TForm)
    BPSelectall: TButtonPanel;
    CBSelect: TCheckBox;
    CLBItems: TCheckListBox;
    procedure CBSelectChange(Sender: TObject);
  private
    FAllData: TFPList;
    procedure SetAllData(AValue: TFPList);
  public
    procedure ShowData;
    Procedure GetSelected(aSelection : TFPList);
    Property AllData : TFPList Read FAllData Write SetAllData;
  end;

var
  SelectReportDataSourcesForm: TSelectReportDataSourcesForm;

implementation

{$R *.lfm}

{ TSelectReportDataSourcesForm }


procedure TSelectReportDataSourcesForm.CBSelectChange(Sender: TObject);
Var
  I : Integer;

begin
  For I:=0 to CLBItems.Items.Count-1 do
    CLBItems.Checked[i]:=True;
end;

procedure TSelectReportDataSourcesForm.SetAllData(AValue: TFPList);
begin
  if FAllData=AValue then Exit;
  FAllData:=AValue;
  ShowData;
end;

procedure TSelectReportDataSourcesForm.ShowData;

Var
  I,J : integer;
  C : TComponent;

begin
  CLBItems.Items.BeginUpdate;
  try
    CLBItems.Items.Clear;
    For I:=0 to FallData.Count-1 do
      begin
      C:=TComponent(FallData[I]);
      J:=CLBItems.Items.AddObject(C.Name,C);
      CLBItems.Checked[J]:=True;
      end;
    CLBItems.Sorted:=True;
  finally
    CLBItems.Items.EndUpdate;
  end;
end;

procedure TSelectReportDataSourcesForm.GetSelected(aSelection: TFPList);

Var
  I : Integer;

begin
  For I:=0 to CLBItems.Items.Count-1 do
    if CLBItems.Checked[I] then
      aSelection.Add(CLBItems.Items.Objects[i]);
end;

end.

