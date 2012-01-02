unit uCmdLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

function CmdToPrj(const AFileName: string): boolean;

implementation

uses
  uManager, umakeskel;

(* Create an project file from an FPDoc commandline
*)
function CmdToPrj(const AFileName: string): boolean;
var
  lst: TStringList;
  l, w: string;
  prj: TFPDocMaker;
begin
(* Need a temporary project, that only includes the given files etc.
*)
  Result := False; //in case of errors
  lst := TStringList.Create;
  prj := TFPDocMaker.Create(nil);
  //prj.OnLog := @prj.LogToStdOut; -->ShowMsg???
  try
    lst.LoadFromFile(AFileName);
    l := lst[0];
    w := GetNextWord(l);
    if w <> 'fpdoc' then
      exit; //expected fpdoc command
    while l <> '' do begin
      w := GetNextWord(l);
      prj.ParseFPDocOption(w);
    end;
    w := prj.SelectedPackage.Name;
    if w = '' then
      exit; //no project name???
    l := ChangeFileExt(AFileName, '_prj.xml'); //same directory!!!
    Result := prj.CreateProject(l, prj.SelectedPackage);
  //now load the project into the manager
    if Result then
    //add package/project to the manager
      Manager.AddProject(w, l, True); //.Packages.Add(w + '=' + l);
  finally
    prj.Free;
    lst.Free;
  end;
end;

end.

