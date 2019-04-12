unit FppkgWorkerThread;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LCLIntf,
  pkghandler,
  pkgoptions,
  pkgglobals,
  pkgFppkg,
  IDEOptionsIntf,
  pkgmessages,
  laz_pkgrepos;

type

  { TFppkgWorkerThread }

  TFppkgWorkerThread = class(TThread)
  private
    FParaPackages: TStrings;
    FParaAction: string;
    FReturnHandle: THandle;

    FFPpkg: TpkgFPpkg;
  protected
    procedure Execute; override;
  public
    constructor Create(ParaAction: string; ParaPackages: TStrings; Description: string; ReturnHandle: THandle);
    destructor Destroy; override;
  end;


implementation

{ TFppkgWorkerThread }

procedure TFppkgWorkerThread.Execute;
var
  OldCurrDir: string;
  i: integer;
  s: string;
begin
  pkghandler.ClearExecutedAction;

  FFPpkg := TpkgFPpkg.Create(Nil);
  FFPpkg.InitializeGlobalOptions(IDEEnvironmentOptions.GetParsedFppkgConfig);
  FFPpkg.Options.GlobalSection.Downloader := 'FPC';
  FFPpkg.InitializeCompilerOptions;

  FFPpkg.CompilerOptions.InitCompilerDefaults;
  FFPpkg.FpmakeCompilerOptions.InitCompilerDefaults;
  FFPpkg.CompilerOptions.CheckCompilerValues;
  FFPpkg.FpmakeCompilerOptions.CheckCompilerValues;
  FFPpkg.LoadLocalAvailableMirrors;

  FFPpkg.ScanAvailablePackages;
  FFPpkg.ScanPackages;

  if (FParaAction='install') or (FParaAction='uninstall') or (FParaAction='fixbroken') then
    FFPpkg.ScanInstalledPackagesForAvailablePackages;

  OldCurrDir := GetCurrentDir;
  try
    if FParaPackages.Count = 0 then
    begin
      pkghandler.ExecuteAction(CurrentDirPackageName, FParaAction, FFPpkg);
    end
    else
    begin
      // Process packages
      for i := 0 to FParaPackages.Count - 1 do
      begin
        pkgglobals.Log(llDebug, SLogCommandLineAction,['[' + FParaPackages[i] + ']', FParaAction]);
        pkghandler.ExecuteAction(FParaPackages[i], FParaAction, FFPpkg);
      end;
    end;
  except
    On E: Exception do
    begin
      Error(SErrException + LineEnding + E.Message);
    end;
  end;
  SetCurrentDir(OldCurrDir);
  PostMessage(FReturnHandle, WM_WorkerThreadDone, 0, 0);
end;

constructor TFppkgWorkerThread.Create(ParaAction: string; ParaPackages: TStrings;
  Description: string; ReturnHandle: THandle);
begin
  FParaPackages := TStringList.Create;
  FParaPackages.Assign(ParaPackages);
  FParaAction :=  ParaAction;
  FReturnHandle := ReturnHandle;
  inherited Create(False);
end;

destructor TFppkgWorkerThread.Destroy;
begin
  FParaPackages.Free;
  FFPpkg.Free;
  inherited Destroy;
end;

end.

