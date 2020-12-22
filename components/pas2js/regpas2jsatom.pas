unit regpas2jsatom;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ProjectIntf, BaseIDEIntf, LazIDEIntf, LazFileUtils;

type

  { TAtomPackageProjectDescriptor }

  TAtomPackageProjectDescriptor = class(TProjectDescriptor)
  Private
    FPackageClassName,
    FPackageDir,
    FPackageLicense,
    FPackageName,
    FPackageDescription : String;
    FlinkPackage : Boolean;
    FKeyWords,
    FCommands : TStrings;
    FActivationCommands : TStrings;
    procedure AddCSSFile(aProject: TLazProject);
    procedure AddFileToProject(aProject: TLazProject; const aFileName: string);
    procedure AddGlueFile(aProject: TLazProject);
    procedure AddKeyMapFile(aProject: TLazProject);
    procedure AddMenuFile(aProject: TLazProject);
    procedure AddPackageJSONFile(aProject: TLazProject);
    procedure AddProjectFile(AProject: TLazProject);
    procedure CreateProjectDirs;
    procedure CreateProjectSource(Src: TStrings);
    procedure DoDefaultReplacements(Src: TStrings);
    procedure InsertHandlerDefinitions(Src: TStrings; aIndex, aIndent: Integer);
    procedure InsertHandlerImplementations(Src: TStrings; aIndex: Integer);
    procedure InsertHandlerRegistrations(Src: TStrings; aIndex: Integer);
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
  {$IFDEF UNIX}
  baseunix,
  {$ENDIF}

  fpjson,frmpas2jsatompackagesettings,  CompOptsIntf,
  NewItemIntf, MenuIntf, pjsprojectoptions, pjsdsgnoptions;

Resourcestring
  SNewAtomPackage = 'Pas2js Atom package';
  SNewAtomPackageDescr = 'Create a new pas2js Atom package';

Var
  AtomProjDesc:TAtomPackageProjectDescriptor;


procedure Register;

begin
  //RegisterIdeMenuCommand(itmOptionsDialogs,STemplateSettings,SProjectTemplateSettings,nil,@ChangeSettings);
  AtomProjDesc:=TAtomPackageProjectDescriptor.Create();
  RegisterProjectDescriptor(AtomProjDesc);
end;


{ TAtomPackageProjectDescriptor }

function TAtomPackageProjectDescriptor.ShowOptionsDialog : TModalResult;

begin
  With TAtomPackageSettingsForm.Create(Application) do
    try
      PkgDescription:=FPackageDescription;
      PkgName:=FPackageName;
      pkgClassName:= FPackageClassName;
      pkgLink:=FlinkPackage;
      PkgCommands:=FCommands;
      PkgActivationCommands:=FActivationCommands;
      PkgLicense:=FPackageLicense;
      PkgKeyWords:=FkeyWords.CommaText;
      PkgDir:=FPackageDir;
      Result:=ShowModal;
      if (Result=mrOK) then
        begin
        FPackageDescription:=PkgDescription;
        FPackageName:=PkgName;
        FlinkPackage:=pkgLink;
        FCommands.Assign(PkgCommands);
        FActivationCommands.Assign(PkgActivationCommands);
        FPackageLicense:=PkgLicense;
        FKeyWords.CommaText:=PkgKeyWords;
        FPackageDir:=IncludeTrailingPathDelimiter(PkgDir);
        FPackageClassName:=pkgClassName;
        end;
    finally
      Free;
    end;
end;


constructor TAtomPackageProjectDescriptor.Create;
begin
  inherited Create;
  FPackageName:='my-atom-package';
  FPackageDescription:='My atom package';
  FPackageClassName:='TMyAtomPackageApplication';
  FKeyWords:=TStringList.Create;
  FPackageLicense:='MIT';
  FLinkPackage:=True;
  FCommands:=TStringList.Create;
  FActivationCommands:=TStringList.Create;
  Name:='pas2jsatompackage';
end;

destructor TAtomPackageProjectDescriptor.destroy;
begin
  FreeAndNil(FCommands);
  FreeAndNil(FKeywords);
  FreeAndNil(FActivationCommands);
  Inherited;
end;


function TAtomPackageProjectDescriptor.GetLocalizedName: string;
begin
  Result:=SNewAtomPackage;
end;

function TAtomPackageProjectDescriptor.GetLocalizedDescription: string;
begin
  Result:=SNewAtomPackageDescr;
end;


function TAtomPackageProjectDescriptor.DoInitDescriptor: TModalResult;

begin
  FPackageDir:=GetUserDir+'github'+pathdelim+'myatompackage';
  FPackageName:='my-atom-package';
  FPackageDescription:='My atom package';
  FPackageClassName:='TMyAtomPackageApplication';
  FKeyWords:=TStringList.Create;
  FPackageLicense:='MIT';
{$IFDEF UNIX}
  FLinkPackage:=True;
{$ELSE}
  FLinkPackage:=False;
{$ENDIF}
  FCommands.Clear;
  FActivationCommands.Clear;
  Result:=ShowOptionsDialog;
end;

Procedure TAtomPackageProjectDescriptor.CreateProjectDirs;

Const
  DirCount = 5;
  DefDirs : Array [1..DirCount] of string =
      ('','lib','keymaps','menus','styles');

Var
  S : String;
  L : String;

begin
  FPackageDir:=IncludeLeadingPathDelimiter(FPackageDir);
  For S in DefDirs do
    If not ForceDirectories(FPackageDir+S) then
      ShowMessage('Failed to create directory '+FPackageDir+S);
{$IFDEF UNIX}
  if FLinkPackage then
    begin
    L:=Sysutils.GetUserDir+'.atom'+PathDelim+'packages/'+FPackageName;
    fpSymlink(PChar(FPackageDir),PChar(L));
    end;
{$ENDIF}
end;

{$I defaults.inc}


Procedure TAtomPackageProjectDescriptor.InsertHandlerDefinitions(Src : TStrings; aIndex,aIndent : Integer);

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
    AddLn(Prefix+'Procedure '+V+';');
    end;
end;

Procedure TAtomPackageProjectDescriptor.InsertHandlerImplementations(Src : TStrings; aIndex : Integer);

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
    AddLn('Procedure '+FPackageClassName+'.'+V+';');
    AddLn('');
    AddLn('begin');
    AddLn('end;');
    AddLn('');
    end;
end;

Procedure TAtomPackageProjectDescriptor.InsertHandlerRegistrations(Src : TStrings; aIndex : Integer);

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
  AddLn('cmds:=TJSObject.New;');
  For I:=0 to FCommands.Count-1 do
    begin
    FCommands.GetNameValue(I,N,V);
    AddLn('cmds['''+N+''']:=@'+V+';');
    end;
  AddLn('subscriptions.add(atom.commands.add(''workspace'', cmds));');
end;

Procedure TAtomPackageProjectDescriptor.DoDefaultReplacements(Src : TStrings);

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
      InsertHandlerRegistrations(Src,I);
    end;
end;

function TAtomPackageProjectDescriptor.LoadDefault(Src : TStrings; aFileName : string) : boolean;

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

Procedure TAtomPackageProjectDescriptor.AddGlueFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'lib'+PathDelim+'packageglue.js';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'glue.js') then
      GetDefaultGlueFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(aProject,FN);
end;

Procedure TAtomPackageProjectDescriptor.AddCSSFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'styles'+PathDelim+'package.less';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'package.less') then
      GetDefaultCSSFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(aProject,FN);

end;

Procedure TAtomPackageProjectDescriptor.AddKeyMapFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'keymaps'+PathDelim+'keymaps.json';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'keymaps.json') then
      GetDefaultKeyMapFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(aProject,FN);
end;

Procedure TAtomPackageProjectDescriptor.AddMenuFile(aProject : TLazProject);

Var
  Src : TStrings;
  FN : String;

begin
  FN:=FPackageDir+'menus'+PathDelim+'menu.json';
  Src:=TStringList.Create;
  try
    if not LoadDefault(Src,'menu.json') then
      GetDefaultMenuFile(Src);
    DoDefaultReplaceMents(Src);
    Src.SaveToFile(FN);
  finally
    Src.Free;
  end;
  AddFileToProject(aProject,FN);
end;

Procedure TAtomPackageProjectDescriptor.AddPackageJSONFile(aProject : TLazProject);

Var
  aJSON,B : TJSONObject;
  keys : TJSONArray;
  S,N,V : String;
  JS : TJSONStringType;
  I : Integer;
  aStream : TStringStream;

begin
  aJSON:=TJSONObject.Create([
   'name',FPackagename,
   'main','lib/packageglue',
   'version','0.0.1',
   'description',FPackageDescription,
   'license',FPackageLicense
  ]);
  try
    Keys:=TJSONArray.Create;
    aJSON.add('keywords',keys);
    For S in FKeyWords do
      Keys.Add(S);
    b:=TJSONObject.Create;
    aJSON.Add('activationCommands',b);
    For I:=0 to FActivationCommands.Count-1 do
      begin
      FActivationCommands.GetNameValue(I,N,V);
      b.Add(V,FPackageName+':'+N);
      end;
    b:=TJSONObject.Create(['atom','>=1.0.0 <2.0.0']);
    aJSON.Add('engines',b);
    b:=TJSONObject.Create([]);
    aJSON.Add('dependencies',b);
    JS:=aJSON.FormatJSON;
    aStream:=TStringStream.Create(JS);
    aStream.SaveToFile(FPackageDir+'package.json');
  finally
    aJSON.Free;
  end;
end;

procedure TAtomPackageProjectDescriptor.CreateProjectSource(Src : TStrings);

begin
  if not LoadDefault(Src,'project.lpr') then
    GetDefaultProjectFile(Src);
  DoDefaultReplaceMents(Src);
end;

procedure TAtomPackageProjectDescriptor.AddProjectFile(AProject: TLazProject);

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

function TAtomPackageProjectDescriptor.InitProject(AProject: TLazProject) : TModalResult;


Var
  CompOpts : TLazCompilerOptions;
begin
  AProject.Title:=FPackageName;
  AProject.ProjectInfoFile:=FPackageDir+StripNonIdentifierChars(FPackageName)+'.lpi';
  CreateProjectDirs;
  CompOpts:=AProject.LazCompilerOptions;
  SetDefaultNodeJSCompileOptions(CompOpts);
  CompOpts.TargetFilename:='lib/'+StripNonIdentifierChars(FPackageName)+'.js';
  CompOpts.CustomOptions:='-Jiatomimports.js -Jirtl.js -Jc '+CompOpts.CustomOptions+' -Jiatomexports.js';
  SetDefaultNodeRunParams(AProject.RunParameters.GetOrCreate('Default'));
  AddProjectFile(aProject);
  Result:=mrOK;
end;

Procedure TAtomPackageProjectDescriptor.AddFileToProject(aProject : TLazProject; Const aFileName : string);

begin
  LazarusIDE.DoOpenEditorFile(aFileName, -1, -1, [ofProjectLoading,ofQuiet,ofAddToProject]);
end;

Function TAtomPackageProjectDescriptor.CreateStartFiles(AProject: TLazProject) : TModalresult;

begin
  AddGlueFile(aProject);
  AddCSSFile(aProject);
  AddKeyMapFile(aProject);
  AddMenuFile(aProject);
  AddPackageJSONFile(aProject);
  Result:=mrOK;
end;


end.
