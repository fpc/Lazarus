unit IdeTransferMacros;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // BuildIntf
  MacroDefIntf,
  // IdeConfig
  EnvironmentOpts, LazConf,
  // IDE
  LazarusIDEStrConsts, TransferMacros, IDETranslations;

type

  { TIdeTransferMarcros }

  TIdeTransferMarcros = class
  private
    // System
    class function MacroFuncExeExt(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    // Conf
    class function MacroFuncConfDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
    // EnvironmentOpts
    class function MacroFuncFPCSrcDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                                var {%H-}Abort: boolean): string;
    class function MacroFuncLazarusDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    class function MacroFuncLanguageID(const {%H-}s:string; const {%H-}Data: PtrInt;
                                 var {%H-}Abort: boolean): string;
    class function MacroFuncLanguageName(const {%H-}s:string; const {%H-}Data: PtrInt;
                                   var {%H-}Abort: boolean): string;
    class function MacroFuncTestDir(const {%H-}s:string; const {%H-}Data: PtrInt;
                              var {%H-}Abort: boolean): string;
  public
    class procedure InitMacros(AMacroList: TTransferMacroList);
  end;

implementation

{ TIdeTransferMarcros }

class function TIdeTransferMarcros.MacroFuncConfDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetPrimaryConfigPath;
end;

class function TIdeTransferMarcros.MacroFuncExeExt(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetExecutableExt;
end;

class function TIdeTransferMarcros.MacroFuncFPCSrcDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedFPCSourceDirectory;
end;

class function TIdeTransferMarcros.MacroFuncLazarusDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedLazarusDirectory;
end;

class function TIdeTransferMarcros.MacroFuncLanguageID(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.LanguageID;
end;

class function TIdeTransferMarcros.MacroFuncLanguageName(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=GetLazarusLanguageLocalizedName(EnvironmentOptions.LanguageID);
end;

class function TIdeTransferMarcros.MacroFuncTestDir(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=EnvironmentOptions.GetParsedTestBuildDirectory;
end;

class procedure TIdeTransferMarcros.InitMacros(AMacroList: TTransferMacroList);
begin
  // System
  AMacroList.Add(TTransferMacro.Create('Home',GetUserDir,
                 lisUserSHomeDirectory, nil, []));
  AMacroList.Add(TTransferMacro.Create('ExeExt','',
                 lisFileExtensionOfPrograms, @MacroFuncExeExt, []));

  // Conf
  AMacroList.Add(TTransferMacro.Create('ConfDir','',
                 lisConfigDirectory,@MacroFuncConfDir,[]));

  // EnvironmentOpts
  AMacroList.Add(TTransferMacro.Create('FPCSrcDir','',
                 lisFreePascalSourceDirectory,@MacroFuncFPCSrcDir,[]));
  AMacroList.Add(TTransferMacro.Create('LazarusDir','',
                 lisLazarusDirectory,@MacroFuncLazarusDir,[]));
  AMacroList.Add(TTransferMacro.Create('LanguageID','',
                 lisLazarusLanguageID,@MacroFuncLanguageID,[]));
  AMacroList.Add(TTransferMacro.Create('LanguageName','',
                 lisLazarusLanguageName,@MacroFuncLanguageName,[]));
  AMacroList.Add(TTransferMacro.Create('TestDir','',
                 lisTestDirectory,@MacroFuncTestDir,[]));
end;

end.

