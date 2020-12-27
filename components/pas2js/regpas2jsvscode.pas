unit regpas2jsvscode;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ProjectIntf, BaseIDEIntf, LazIDEIntf, LazFileUtils;

type

  { TVSCodeExtensionProjectDescriptor }

  TVSCodeExtensionProjectDescriptor = class(TProjectDescriptor)
  Private
    FPackagePublisher : string;
    FPackageClassName : String;
    FPackageDir,
    FPackageLicense,
    FPackageName,
    FPackageDescription : String;
    FKeyWords,
    FCommands : TStrings;
    FContributesCommands : TStrings;
    FFiles : TStrings;
    procedure AddFileToProject(const aFileName: string);
    procedure AddGlueFile(aProject: TLazProject);
    procedure AddLaunchFile(aProject: TLazProject);
    procedure AddPackageJSONFile(aProject: TLazProject);
    procedure AddProjectFile(AProject: TLazProject);
    procedure AddTasksFile(aProject: TLazProject);
    procedure CreateProjectDirs;
    procedure CreateProjectSource(Src: TStrings);
    procedure DoDefaultReplaceMents(Src: TStrings);
    procedure InitVars;
    procedure InsertHandlerDefinitions(Src: TStrings; aIndex, aIndent: Integer);
    procedure InsertHandlerImplementations(Src: TStrings; aIndex: Integer);
    procedure InsertHandlerRegistrations(Src: TStrings; aIndex,aIndent: Integer);
    function LoadDefault(Src: TStrings; aFileName: string): boolean;
    Function ShowOptionsDialog : TModalResult;
  public
    constructor Create(); override;
    destructor destroy; override;
    Function DoInitDescriptor : TModalResult; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject) : TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject) : TModalResult; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
  fpjson,frmpas2jsvscodeextensionsettings,  CompOptsIntf,
  NewItemIntf, MenuIntf, pjsprojectoptions, pjsdsgnoptions;

Resourcestring
  SNewVSCodeExtension = 'Pas2js VS Code extension';
  SNewVSCodeExtensionDescr = 'Create a new pas2js VS Code extension';

Var
  VSCodeProjDesc:TVSCodeExtensionProjectDescriptor;

procedure Register;

begin
  VSCodeProjDesc:=TVSCodeExtensionProjectDescriptor.Create();
  RegisterProjectDescriptor(VSCodeProjDesc);
end;


{ TVSCodeExtensionProjectDescriptor }

function TVSCodeExtensionProjectDescriptor.ShowOptionsDialog : TModalResult;

begin
  With TVSCodeExtensionSettingsForm.Create(Application) do
    try
      PkgDescription:=FPackageDescription;
      PkgName:=FPackageName;
      PkgPublisher:=FPackagePublisher;
      PkgClassName:=FPackageClassName;
      PkgCommands:=FCommands;
      PkgContributesCommands:=FContributesCommands;
      PkgLicense:=FPackageLicense;
      PkgKeyWords:=FkeyWords.CommaText;
      PkgDir:=FPackageDir;
      Result:=ShowModal;
      if (Result=mrOK) then
        begin
        FPackageDescription:=PkgDescription;
        FPackageName:=PkgName;
        FPackageClassName:=PkgClassName;
        FPackagePublisher:=PkgPublisher;
        FCommands.Assign(PkgCommands);
        FContributesCommands.Assign(PkgContributesCommands);
        FPackageLicense:=PkgLicense;
        FKeyWords.CommaText:=PkgKeyWords;
        FPackageDir:=IncludeTrailingPathDelimiter(PkgDir);
        end;
    finally
      Free;
    end;
end;


constructor TVSCodeExtensionProjectDescriptor.Create;
begin
  inherited Create;
  FKeyWords:=TStringList.Create;
  FCommands:=TStringList.Create;
  FContributesCommands:=TStringList.Create;
  FFiles:=TStringList.Create;
  InitVars;
  Name:='pas2jsvscodeextension';
end;

destructor TVSCodeExtensionProjectDescriptor.destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FCommands);
  FreeAndNil(FKeywords);
  FreeAndNil(FContributesCommands);
  Inherited;
end;


function TVSCodeExtensionProjectDescriptor.GetLocalizedName: string;
begin
  Result:=SNewVSCodeExtension;
end;

function TVSCodeExtensionProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:=SNewVSCodeExtensionDescr;
end;

Procedure TVSCodeExtensionProjectDescriptor.InitVars;

begin
  FPackageDir:=GetUserDir+'myvscodeextension';
  FPackageClassName:='TMyVSCodeExtensionApplication';

  FPackageName:='myvscodeextension';
  FPackageDescription:='My VS Code Extension';
  FPackagePublisher:='me';
  FPackageLicense:='MIT';
  FKeyWords.Clear;
  FCommands.Clear;
  FContributesCommands.Clear;
end;

function TVSCodeExtensionProjectDescriptor.DoInitDescriptor: TModalResult;

begin
  initVars;
  Result:=ShowOptionsDialog;

end;

Procedure TVSCodeExtensionProjectDescriptor.CreateProjectDirs;

Const
  DirCount = 3;
  DefDirs : Array [1..DirCount] of string =
      ('','.vscode','js');

Var
  S : String;

begin
  FPackageDir:=IncludeLeadingPathDelimiter(FPackageDir);
  For S in DefDirs do
    If not ForceDirectories(FPackageDir+S) then
      ShowMessage('Failed to create directory '+FPackageDir+S);
end;

{$I vscodedefaults.inc}

Procedure TVSCodeExtensionProjectDescriptor.InsertHandlerDefinitions(Src : TStrings; aIndex,aIndent : Integer);

Var
  I,Cnt : Integer;
  Prefix,N,V : String;

  procedure AddLn(aLine : String);

  begin
    Src.Insert(aIndex+Cnt,aLine);
    inc(cnt);
  end;


begin
  cnt:=0;
  Src.Delete(aIndex);
  Prefix:=StringOfChar(' ',aIndent-2); // approximate
  For I:=0 to FCommands.Count-1 do
    begin
    FCommands.GetNameValue(I,N,V);
    AddLn(Prefix+'function '+V+'(args : TJSValueDynArray) : JSValue;');
    end;
end;

Procedure TVSCodeExtensionProjectDescriptor.InsertHandlerImplementations(Src : TStrings; aIndex : Integer);

Var
  cnt : Integer;

  procedure AddLn(aLine : String);

  begin
    Src.Insert(aIndex+Cnt,aLine);
    inc(cnt);
  end;
Var
  I : Integer;
  N,V : String;

begin
  cnt:=0;
  Src.Delete(aIndex);
  For I:=0 to FCommands.Count-1 do
    begin
    AddLn('');
    FCommands.GetNameValue(I,N,V);
    AddLn('function '+FPackageClassName+'.'+V+'(args : TJSValueDynArray) : JSValue;');
    AddLn('');
    AddLn('begin');
    AddLn('  Result:=null;');
    AddLn('end;');
    AddLn('');
    end;
end;

Procedure TVSCodeExtensionProjectDescriptor.InsertHandlerRegistrations(Src : TStrings; aIndex,aIndent : Integer);

Var
  cnt : Integer;

  procedure AddLn(aLine : String);

  begin
    Src.Insert(aIndex+Cnt,'  '+aLine);
    inc(cnt);
  end;
Var
  I : Integer;
  N,V : String;

begin
  Src.Delete(aIndex);
  if FCommands.Count=0 then exit;
  For I:=0 to FCommands.Count-1 do
    begin
    FCommands.GetNameValue(I,N,V);
    AddLn('disp:=VSCode.commands.registerCommand('''+N+''', @'+V+');');
    AddLn('TJSArray(ExtensionContext.subscriptions).push(disp);');
    end;
end;

Procedure TVSCodeExtensionProjectDescriptor.DoDefaultReplacements(Src : TStrings);

Var
  I,P : Integer;

begin
  For I:=Src.Count-1 downto 0 do
    begin
    Src[i]:=StringReplace(Src[I],'%PACKAGENAME%',FPackageName,[rfReplaceALl]);
    Src[i]:=StringReplace(Src[I],'%PACKAGEPROJECTNAME%',StripNonIdentifierChars(FPackageName),[rfReplaceALl]);
    Src[i]:=StringReplace(Src[I],'%CLASSNAME%',FPackageClassName,[rfReplaceALl]);
    P:=pos('%PACKAGEHANDLERINTFS%',Src[i]);
    if P>0 then
      InsertHandlerDefinitions(Src,I,P);
    P:=pos('%PACKAGEHANDLERIMPLS%',Src[i]);
    if P>0 then
      InsertHandlerImplementations(Src,I);
    P:=pos('%PACKAGEHANDLERREGS%',Src[i]);
    if P>0 then
      InsertHandlerRegistrations(Src,I,P);
    end;
end;

function TVSCodeExtensionProjectDescriptor.LoadDefault(Src : TStrings; aFileName : string) : boolean;

Var
  FN : String;

begin
  Result:=(PJSOptions.AtomTemplateDir<>'');
  if Result then
    begin
    FN:=IncludeTrailingPathDelimiter(PJSOptions.AtomTemplateDir)+aFileName;
    Result:=FileExists(FN);
    if Result then
      Src.LoadFromFile(FN);
    end;
end;


Procedure TVSCodeExtensionProjectDescriptor.AddGlueFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'js'+PathDelim+'packageglue.js';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'glue.js') then
      GetDefaultGlueFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(FN);
end;

Procedure TVSCodeExtensionProjectDescriptor.AddTasksFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'.vscode'+PathDelim+'tasks.json';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'tasks.json') then
      GetDefaultTasksFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(FN);
end;

Procedure TVSCodeExtensionProjectDescriptor.AddLaunchFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'.vscode'+PathDelim+'launch.json';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'launch.json') then
      GetDefaultLaunchFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(FN);
end;



Procedure TVSCodeExtensionProjectDescriptor.AddPackageJSONFile(aProject : TLazProject);

Var
  b,aJSON,Contribs,Cmd : TJSONObject;
  cmds, keys : TJSONArray;
  S,N,V : String;
  I : Integer;
  aStream : TStringStream;

begin
  aJSON:=TJSONObject.Create([
   'name',FPackagename,
   'main','js/packageglue.js',
   'version','0.0.1',
   'description',FPackageDescription,
   'license',FPackageLicense
  ]);
  try
    Keys:=TJSONArray.Create;
    aJSON.add('keywords',keys);
    For S in FKeyWords do
      Keys.Add(S);
    Contribs:=TJSONObject.Create;
    aJSON.Add('contributes',Contribs);
    cmds:=TJSONArray.Create;
    Contribs.Add('commands',cmds);
    For I:=0 to FContributesCommands.Count-1 do
      begin
      FContributesCommands.GetNameValue(I,N,V);
      cmd:=TJSONObject.Create(['command',N,'title',v]);
      cmds.Add(cmd);
      end;
    cmds:=TJSONArray.Create;
    aJSON.Add('activationEvents',cmds);
    for I:=0 to FCommands.Count-1 do
      begin
      FContributesCommands.GetNameValue(I,N,V);
      cmds.Add('onCommand:'+N);
      end;
    b:=TJSONObject.Create(['vscode','^1.32.0']);
    aJSON.Add('engines',b);
    b:=TJSONObject.Create(['@types/vscode', '^1.32.0']);
    aJSON.Add('devDependencies',b);
    aStream:=TStringStream.Create(aJSON.FormatJSON);
    aStream.SaveToFile(FPackageDir+'package.json');
  finally
    aJSON.Free;
    aStream.Free;
  end;
end;

procedure TVSCodeExtensionProjectDescriptor.CreateProjectSource(Src : TStrings);

begin
  if not LoadDefault(Src,'project.lpr') then
    GetDefaultProjectFile(Src);
  DoDefaultReplaceMents(Src);

end;

procedure TVSCodeExtensionProjectDescriptor.AddProjectFile(AProject: TLazProject);

Var
  aFile : TLazProjectFile;
  FN : String;
  Src : TStrings;

begin
  FN:=FPackageDir+StripNonIdentifierChars(FPackageName)+'.lpr';
  aFile:=aProject.CreateProjectFile(FN);
  AFile.IsPartOfProject:=true;
  AProject.AddFile(AFile,False);
  AProject.MainFileID:=0;
  Src:=TStringList.Create;
  try
    CreateProjectSource(Src);
    Src.SaveToFile(FN);
    AProject.MainFile.SetSourceText(src.Text,true);
  Finally
    Src.Free;
  end;
end;

function TVSCodeExtensionProjectDescriptor.InitProject(AProject: TLazProject) : TModalResult;

Var
  CompOpts : TLazCompilerOptions;
begin
  AProject.Title:=FPackageName;
  AProject.ProjectInfoFile:=FPackageDir+StripNonIdentifierChars(FPackageName)+'.lpi';
  CreateProjectDirs;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:='lib/'+StripNonIdentifierChars(FPackageName)+'.js';
  CompOpts.CustomOptions:='-Jivscodeimports.js -Jirtl.js -Jc '+CompOpts.CustomOptions+' -Javscodeexports.js';
  SetDefaultNodeRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AddProjectFile(aProject);
  Result:=mrOK;
end;

Procedure TVSCodeExtensionProjectDescriptor.AddFileToProject(Const aFileName : string);

begin
  FFiles.Add(aFileName);
end;



Function TVSCodeExtensionProjectDescriptor.CreateStartFiles(AProject: TLazProject) : TModalresult;

var
  aFileName : String;

begin
  FFiles.Clear;
  AddGlueFile(aProject);
  AddTasksFile(aProject);
  AddLaunchFile(aProject);
  AddPackageJSONFile(aProject);
  For aFileName in FFiles do
    LazarusIDE.DoOpenEditorFile(aFileName, -1, -1, [ofProjectLoading,ofQuiet,ofAddToProject]);
  Result:=mrOK;
end;


end.
