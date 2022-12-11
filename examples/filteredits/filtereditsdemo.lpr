program FilterEditsDemo;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, LazControls, MainUnit, ListViewFilterUnit, ListFilterUnit,
TreeFilterUnit;

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TListFilterForm, ListFilterForm);
    Application.CreateForm(TListViewFilterForm, ListViewFilterForm);
    Application.CreateForm(TTreeFilterForm, TreeFilterForm);
    Application.Run;
end.

