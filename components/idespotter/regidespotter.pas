unit RegIDESpotter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Procedure Register;

implementation

uses forms,lcltype,idecommands,toolbarintf,menuintf, frmspotter;

Var
  SpotterFrm : TSpotterForm;
  ShowCmdCategory : Boolean = True;

Procedure IdeMenuClicked(Sender : TObject);

begin
  if SpotterFrm=Nil then
    begin
    SpotterFrm:=TSpotterForm.Create(Application);
    SpotterFrm.ShowCategory:=ShowCmdCategory;
    end;
  SpotterFrm.Show;
end;

Procedure Register;

Const
{$IFDEF DARWIN}
  ShiftKeys = [ssMeta,ssShift];
{$ELSE}
  ShiftKeys = [ssAlt,ssShift];
{$ENDIF}


var
  IDEShortCutX: TIDEShortCut;
  IDECommandCategory: TIDECommandCategory;
  IDECommand: TIDECommand;
begin
  IDEShortCutX := IDEShortCut(VK_P, ShiftKeys, VK_UNKNOWN, []);
  IDECommandCategory := IDECommandList.FindCategoryByName(CommandCategoryViewName);
  if IDECommandCategory <> nil then
  begin
    IDECommand := RegisterIDECommand(IDECommandCategory, 'Spotter', 'Open Spotter',
      IDEShortCutX, nil, @IDEMenuClicked);
    if IDECommand <> nil then
      RegisterIDEButtonCommand(IDECommand);
  end;

  RegisterIDEMenuCommand(itmViewIDEInternalsWindows, 'Spotter', 'Spotter', nil,
    @IDEMenuClicked,IDECommand);
end;

end.

