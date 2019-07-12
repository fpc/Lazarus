unit PrjGrpInfoFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ProjectGroup, LazStringUtils, ProjectGroupStrConst;

type

  { TPrjGrpInfoForm }

  TPrjGrpInfoForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
  private
  public
  end;

procedure ShowPrgGrpInfo(Target: TIDECompileTarget);

implementation

procedure ShowPrgGrpInfo(Target: TIDECompileTarget);
var
  PrjGrpInfoForm: TPrjGrpInfoForm;
  sl: TStringList;
  s: String;
begin
  sl:=TStringList.Create;
  PrjGrpInfoForm:=TPrjGrpInfoForm.Create(nil);
  try
    if Target<>nil then
      ;// ToDo: show only SrcPath for this target
    PrjGrpInfoForm.Caption:=lisInfo;
    sl.Add(lisSourceDirectoriesOfProjectGroup);
    s:=IDEProjectGroupManager.GetSrcPaths;
    SplitString(s,';',sl,false);
    PrjGrpInfoForm.Memo1.Lines.Assign(sl);
    PrjGrpInfoForm.ShowModal;
  finally
    PrjGrpInfoForm.Free;
    sl.Free;
  end;
end;

{$R *.lfm}

end.

