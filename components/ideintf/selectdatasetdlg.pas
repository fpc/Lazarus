unit selectdatasetdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls;

type

  { TSelectDatasetForm }

  TSelectDatasetForm = class(TForm)
    BPDatasets: TButtonPanel;
    lblCaption: TLabel;
    lbDatasets: TListBox;
  private
    function GetDatasets: TStrings;
    function GetSelected: String;
    procedure SetDatasets(AValue: TStrings);

  public
    Property Datasets : TStrings Read GetDatasets Write SetDatasets;
    Property Selected : String Read GetSelected;
  end;

Function SelectDataset(aList : TStrings; aTitle : String = '') : String;

var
  SelectDatasetForm: TSelectDatasetForm;

implementation

{$R *.lfm}

Function SelectDataset(aList : TStrings; aTitle : String = '') : String;

begin
  Result:='';
  With TSelectDatasetForm.Create(Application) do
    try
      Datasets:=aList;
      if aTitle<>'' then
        Caption:=aTitle;
      If ShowModal=mrOK then
        Result:=Selected;
    finally
      Free;
    end;
end;


{ TSelectDatasetForm }

function TSelectDatasetForm.GetDatasets: TStrings;

begin
  Result:=lbDatasets.Items;
end;

function TSelectDatasetForm.GetSelected: String;

begin
  With LbDatasets do
    if ItemIndex=-1 then
      Result:=''
    else
      Result:=Items[ItemIndex];
end;

procedure TSelectDatasetForm.SetDatasets(AValue: TStrings);

begin
  lbDatasets.Items:=aValue;
end;

end.

