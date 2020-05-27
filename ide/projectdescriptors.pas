unit ProjectDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls, Forms,
  // Codetools
  FileProcs,
  // LazUtils
  LazFileUtils, LazUTF8,
  // IdeIntf
  CompOptsIntf, ProjectIntf, LazIDEIntf,
  // IDE
  frmCustomApplicationOptions, LazarusIDEStrConsts, Project, W32Manifest;

type

  //----------------------------------------------------------------------------

  { TProjectApplicationDescriptor }

  TProjectApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

  { TProjectSimpleProgramDescriptor }

  TProjectSimpleProgramDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectProgramDescriptor }

  TProjectProgramDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectConsoleApplicationDescriptor }

  TProjectConsoleApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectLibraryDescriptor }

  TProjectLibraryDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TProjectManualProgramDescriptor }

  TProjectManualProgramDescriptor = class(TProjectDescriptor)
  private
    FAddMainSource: boolean;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
    property AddMainSource: boolean read FAddMainSource write FAddMainSource;
  end;

  { TProjectEmptyProgramDescriptor }

  TProjectEmptyProgramDescriptor = class(TProjectManualProgramDescriptor)
  public
    constructor Create; override;
  end;


implementation

{ TProjectApplicationDescriptor }

constructor TProjectApplicationDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameApplication;
  Flags:=Flags+[pfUseDefaultCompilerOptions];
end;

function TProjectApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=dlgPOApplication;
end;

function TProjectApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisApplicationProgramDescriptor;
end;

function TProjectApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.UseAppBundle:=true;
  AProject.UseManifest:=true;
  AProject.Scaled:=true;
  (AProject as TProject).ProjResources.XPManifest.DpiAware := xmdaTrue;
  AProject.LoadDefaultIcon;

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}{$H+}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  {$IFDEF UNIX}'+LineEnding
    +'  cthreads,'+LineEnding
    +'  {$ENDIF}'+LineEnding
    +'  {$IFDEF HASAMIGA}'+LineEnding
    +'  athreads,'+LineEnding
    +'  {$ENDIF}'+LineEnding
    +'  Interfaces, // this includes the LCL widgetset'+LineEnding
    +'  Forms'+LineEnding
    +'  { you can add units after this };'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'  RequireDerivedFormResource:=True;'+LineEnding
    +'  Application.Scaled:=True;'+LineEnding
    +'  Application.Initialize;'+LineEnding
    +'  Application.Run;'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  // add lcl pp/pas dirs to source search path
  AProject.AddPackageDependency('LCL');
  AProject.LazCompilerOptions.Win32GraphicApp:=true;
  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
end;

function TProjectApplicationDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  Result:=LazarusIDE.DoNewEditorFile(FileDescriptorForm,'','',
                         [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
end;

{ TProjectSimpleProgramDescriptor }

constructor TProjectSimpleProgramDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameSimpleProgram;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement,pfMainUnitHasScaledStatement]
              +[pfUseDefaultCompilerOptions];
end;

function TProjectSimpleProgramDescriptor.GetLocalizedName: string;
begin
  Result:=lisSimpleProgram;
end;

function TProjectSimpleProgramDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisSimpleProgramProgramDescriptor;
end;

function TProjectSimpleProgramDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
end;

function TProjectSimpleProgramDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TProjectProgramDescriptor }

constructor TProjectProgramDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameProgram;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement,pfMainUnitHasScaledStatement]
              +[pfUseDefaultCompilerOptions];
end;

function TProjectProgramDescriptor.GetLocalizedName: string;
begin
  Result:=lisProgram;
end;

function TProjectProgramDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisProgramProgramDescriptor;
end;

function TProjectProgramDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  NewSource:='program Project1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}{$H+}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  {$IFDEF UNIX}'+LineEnding
    +'  cthreads,'+LineEnding
    +'  {$ENDIF}'+LineEnding
    +'  Classes'+LineEnding
    +'  { you can add units after this };'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
end;

function TProjectProgramDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TProjectManualProgramDescriptor }

constructor TProjectManualProgramDescriptor.Create;
begin
  inherited Create;
  VisibleInNewDialog:=false;
  Name:=ProjDescNameCustomProgram;
  Flags:=Flags-[pfMainUnitHasUsesSectionForAllUnits,
                pfMainUnitHasCreateFormStatements,
                pfMainUnitHasTitleStatement,
                pfMainUnitHasScaledStatement]
              +[pfUseDefaultCompilerOptions];
  FAddMainSource:=true;
end;

function TProjectManualProgramDescriptor.GetLocalizedName: string;
begin
  Result:=lisCustomProgram;
end;

function TProjectManualProgramDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisCustomProgramProgramDescriptor;
end;

function TProjectManualProgramDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  if AddMainSource then begin
    MainFile:=AProject.CreateProjectFile('project1.pas');
    MainFile.IsPartOfProject:=true;
    AProject.AddFile(MainFile,false);
    AProject.MainFileID:=0;

    // create program source
    NewSource:='program Project1;'+LineEnding
      +LineEnding
      +'{$mode objfpc}{$H+}'+LineEnding
      +LineEnding
      +'uses'+LineEnding
      +'  Classes, SysUtils'+LineEnding
      +'  { you can add units after this };'+LineEnding
      +LineEnding
      +'begin'+LineEnding
      +'end.'+LineEnding
      +LineEnding;
    AProject.MainFile.SetSourceText(NewSource,true);
    AProject.LazCompilerOptions.Win32GraphicApp:=false;
  end;
end;

function TProjectManualProgramDescriptor.CreateStartFiles(AProject: TLazProject
  ): TModalResult;
begin
  if AProject.MainFile<>nil then
    Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                        [ofProjectLoading,ofRegularFile])
  else
    Result:=mrCancel;
end;

{ TProjectEmptyProgramDescriptor }

constructor TProjectEmptyProgramDescriptor.Create;
begin
  inherited Create;
  FAddMainSource:=false;
end;

{ TProjectConsoleApplicationDescriptor }

constructor TProjectConsoleApplicationDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameConsoleApplication;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement,pfMainUnitHasScaledStatement]
              +[pfUseDefaultCompilerOptions];
end;

function TProjectConsoleApplicationDescriptor.GetLocalizedName: string;
begin
  Result:=lisConsoleApplication;
end;

function TProjectConsoleApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisConsoleApplicationProgramDescriptor;
end;

function TProjectConsoleApplicationDescriptor.InitProject(AProject: TLazProject
  ): TModalResult;
var
  NewSource: TStringList;
  MainFile: TLazProjectFile;
  C, T : String;
  CC,CD,CU,CS, CO : Boolean;

begin
  Result:=inherited InitProject(AProject);
  If Result<>mrOk then
    Exit;
  With TCustomApplicationOptionsForm.Create(Application) do
    try
      Result:=ShowModal;
      If Result<>mrOk then
        Exit;
      C:=Trim(AppClassName);
      T:=StringReplace(Title,'''','''''',[rfReplaceAll]);
      CC:=CodeConstructor;
      CD:=CodeDestructor;
      CU:=CodeUsage;
      CS:=CodeStopOnError;
      CO:=CodeCheckOptions;
    finally
      Free;
    end;
  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
  AProject.LazCompilerOptions.Win32GraphicApp:=false;

  // create program source
  NewSource:=TStringList.Create;
  NewSource.Add('program Project1;');
  NewSource.Add('');
  NewSource.Add('{$mode objfpc}{$H+}');
  NewSource.Add('');
  NewSource.Add('uses');
  NewSource.Add('  {$IFDEF UNIX}');
  NewSource.Add('  cthreads,');
  NewSource.Add('  {$ENDIF}');
  NewSource.Add('  Classes, SysUtils, CustApp');
  NewSource.Add('  { you can add units after this };');
  NewSource.Add('');
  NewSource.Add('type');
  NewSource.Add('');
  NewSource.Add('  { '+C+' }');
  NewSource.Add('');
  NewSource.Add('  '+C+' = class(TCustomApplication)');
  NewSource.Add('  protected');
  NewSource.Add('    procedure DoRun; override;');
  NewSource.Add('  public');
  If CC or CS then
    NewSource.Add('    constructor Create(TheOwner: TComponent); override;');
  if CD then
    NewSource.Add('    destructor Destroy; override;');
  if CU then
    NewSource.Add('    procedure WriteHelp; virtual;');
  NewSource.Add('  end;');
  NewSource.Add('');
  NewSource.Add('{ '+C+' }');
  NewSource.Add('');
  NewSource.Add('procedure '+C+'.DoRun;');
  NewSource.Add('var');
  NewSource.Add('  ErrorMsg: String;');
  NewSource.Add('begin');
  if CO then
    begin
    NewSource.Add('  // quick check parameters');
    NewSource.Add('  ErrorMsg:=CheckOptions(''h'',''help'');');
    NewSource.Add('  if ErrorMsg<>'''' then begin');
    NewSource.Add('    ShowException(Exception.Create(ErrorMsg));');
    NewSource.Add('    Terminate;');
    NewSource.Add('    Exit;');
    NewSource.Add('  end;');
    NewSource.Add('');
    end;
  If CU then
    begin
    NewSource.Add('  // parse parameters');
    NewSource.Add('  if HasOption(''h'',''help'') then begin');
    NewSource.Add('    WriteHelp;');
    NewSource.Add('    Terminate;');
    NewSource.Add('    Exit;');
    NewSource.Add('  end;');
    end;
  NewSource.Add('');
  NewSource.Add('  { add your program here }');
  NewSource.Add('');
  NewSource.Add('  // stop program loop');
  NewSource.Add('  Terminate;');
  NewSource.Add('end;');
  NewSource.Add('');
  If CC or CS then
    begin
    NewSource.Add('constructor '+C+'.Create(TheOwner: TComponent);');
    NewSource.Add('begin');
    NewSource.Add('  inherited Create(TheOwner);');
    If CS then
    NewSource.Add('  StopOnException:=True;');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  If CD then
    begin
    NewSource.Add('destructor '+C+'.Destroy;');
    NewSource.Add('begin');
    NewSource.Add('  inherited Destroy;');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  If CU then
    begin
    NewSource.Add('procedure '+C+'.WriteHelp;');
    NewSource.Add('begin');
    NewSource.Add('  { add your help code here }');
    NewSource.Add('  writeln(''Usage: '',ExeName,'' -h'');');
    NewSource.Add('end;');
    NewSource.Add('');
    end;
  NewSource.Add('var');
  NewSource.Add('  Application: '+C+';');
  NewSource.Add('begin');
  NewSource.Add('  Application:='+C+'.Create(nil);');
  If (T<>'') then
    begin
    AProject.Flags:=AProject.Flags+[pfMainUnitHasTitleStatement];
    AProject.Title:=T;
    NewSource.Add('  Application.Title:='''+T+''';');
    end;
  NewSource.Add('  Application.Run;');
  NewSource.Add('  Application.Free;');
  NewSource.Add('end.');
  NewSource.Add('');
  AProject.MainFile.SetSourceText(NewSource.Text,true);
  NewSource.Free;
end;

function TProjectConsoleApplicationDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

{ TProjectLibraryDescriptor }

constructor TProjectLibraryDescriptor.Create;
begin
  inherited Create;
  Name:=ProjDescNameLibrary;
  Flags:=Flags-[pfMainUnitHasCreateFormStatements,pfMainUnitHasTitleStatement,pfMainUnitHasScaledStatement]
              +[pfUseDefaultCompilerOptions];
end;

function TProjectLibraryDescriptor.GetLocalizedName: string;
begin
  Result:=lisPckOptsLibrary;
end;

function TProjectLibraryDescriptor.GetLocalizedDescription: string;
begin
  Result:=lisLibraryProgramDescriptor;
end;

function TProjectLibraryDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  Result:=inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('project1.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;
  AProject.LazCompilerOptions.ExecutableType:=cetLibrary;

  // create program source
  NewSource:='library Project1;'+LineEnding
    +LineEnding
    +'{$mode objfpc}{$H+}'+LineEnding
    +LineEnding
    +'uses'+LineEnding
    +'  Classes'+LineEnding
    +'  { you can add units after this };'+LineEnding
    +LineEnding
    +'begin'+LineEnding
    +'end.'+LineEnding
    +LineEnding;
  AProject.MainFile.SetSourceText(NewSource,true);

  AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  AProject.LazCompilerOptions.TargetFilename:='project1';
  AProject.LazCompilerOptions.Win32GraphicApp:=false;
  AProject.LazCompilerOptions.RelocatableUnit:=true;
end;

function TProjectLibraryDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  Result:=LazarusIDE.DoOpenEditorFile(AProject.MainFile.Filename,-1,-1,
                                      [ofProjectLoading,ofRegularFile]);
end;

end.

