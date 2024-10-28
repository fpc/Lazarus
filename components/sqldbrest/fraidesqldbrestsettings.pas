unit fraidesqldbrestsettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ColorBox, Dialogs, SpinEx,
  IDEOptionsIntf, IDEOptEditorIntf;

type

  { TSQLDBRestConfigFrame }

  TSQLDBRestConfigFrame = class(TAbstractIDEOptionsEditor)
    cbSkipMysqlVersionCheck: TCheckBox;
  private

  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  end;

implementation

uses schemaeditorconf;

{$R *.lfm}


{ TSQLDBRestConfigFrame }

function TSQLDBRestConfigFrame.GetTitle: String;
begin
  Result:='SQLDBRest Editor'
end;

procedure TSQLDBRestConfigFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin

end;

procedure TSQLDBRestConfigFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  cbSkipMysqlVersionCheck.Checked:=SchemaSettings.DisableMySQLVersionCheck;
end;

procedure TSQLDBRestConfigFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  SchemaSettings.DisableMySQLVersionCheck:=cbSkipMysqlVersionCheck.Checked;
  SchemaSettings.SaveToFile(SchemaSettings.CurrentFile);
end;

class function TSQLDBRestConfigFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

