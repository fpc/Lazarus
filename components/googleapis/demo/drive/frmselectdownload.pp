unit frmselectdownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls;

type

  { TSelectDownloadForm }

  TSelectDownloadForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    RGFormat: TRadioGroup;
  private
    function GetFormats: TStrings;
    function GetSelected: String;
    { private declarations }
  public
    { public declarations }
    Property Formats : TStrings Read GetFormats;
    Property Selected : String Read GetSelected;
  end;

var
  SelectDownloadForm: TSelectDownloadForm;

implementation

{$R *.lfm}

{ TSelectDownloadForm }

function TSelectDownloadForm.GetFormats: TStrings;
begin
  Result:=RGFormat.Items;
end;

function TSelectDownloadForm.GetSelected: String;
begin
  Result:=RGFormat.Items[RGFormat.ItemIndex];
end;

end.

