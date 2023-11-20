{ IDE options frame for pas2js options

  Author: Mattias Gaertner
}
unit fraTestInsightOpts;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Dialogs, Spin,
  // LazUtils
  LazFileCache, LazFileUtils, LazStringUtils, FileUtil,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEUtils, IDEDialogs,
  // FPCUnit
  TestInsightController, strtestcaseopts;

Type
  { TTestInsightOptionsFrame }

  TTestInsightOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbServerBasePath: TComboBox;
    cbAutoFetch: TCheckBox;
    lblBaseURL: TLabel;
    lblServerPort: TLabel;
    seServerPort: TSpinEdit;
  private
    FDialog: TAbstractOptionsEditorDialog;
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TTestInsightOptionsFrame }


function TTestInsightOptionsFrame.GetTitle: String;
begin
  Result := rsTestInsightTitle;
end;

procedure TTestInsightOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);

begin
  FDialog:=aDialog;
  seServerPort.Value:=DefaultPort;
  cbServerBasePath.Items.Add(DefaultBasePath);
  cbServerBasePath.ItemIndex:=0;

  lblServerPort.Caption:=rsServerPort;
  lblBaseURL.Caption:=rsServerPath;
  cbAutoFetch.Caption:=rsAutomaticallyFetchTestListOnOpen;
end;

procedure TTestInsightOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);

var
  O : TTestInsightOptions;

begin
  O:=gTestInsightController.Options;
  SetComboBoxText(cbServerBasePath,O.BasePath,cstCaseInsensitive);
  seServerPort.Value:=O.Port;
  cbAutoFetch.Checked:=O.AutoFetchTests;
end;

procedure TTestInsightOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);

var
  O : TTestInsightOptions;
  T : String;

begin
  O:=gTestInsightController.Options;
  T:=cbServerBasePath.Text;
  If (T='') then
    T:=DefaultBasePath
  else if T[1]<>'/' then
    T:='/'+T;
  O.BasePath:=T;
  O.Port:=seServerPort.Value;
  O.AutoFetchTests:=cbAutoFetch.Checked;
  gTestInsightController.Options.SaveToFile(TestInsightConfig);
end;

class function TTestInsightOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

