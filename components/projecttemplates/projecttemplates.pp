unit ProjectTemplates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TProjectTemplates }
  TProjectTemplate = Class(TCollectionItem)
  private
    FAuthor: String;
    FDescription: String;
    FDirectory: String;
    FExclude: String;
    FName: String;
    FRecurse: Boolean;
    FFiles : TStrings;
    FVariables: TStrings;
    function GetFileCount: Integer;
    function GetFileName(FileIndex : Integer): String;
    procedure SetVariables(const AValue: TStrings);
    procedure GetFileList(Const Dir : String);
  Protected
    procedure InitFromDir(Const DirName : String);
    procedure CopyAndSubstituteDir(Const SrcDir,DestDir : String; Values : TStrings);
    procedure CopyAndSubstituteFile(Const SrcFN,DestFN : String; Values : Tstrings);
  Public
    constructor Create(ACollection: TCollection); override;
    Destructor Destroy; override;
    Procedure CreateProject(Const ProjectDir : String; Values : TStrings);
    Procedure CreateFile(FileIndex : Integer; Source,Values : TStrings);
    Procedure CreateFile(Const FileName: String; Source,Values : TStrings);
    Procedure CreateProjectDirs(Const BaseDir : String; Values : TStrings);
    Function TargetFileName(FN : String; Values : TStrings) : String;
    Function TargetFileName(I : Integer; Values : TStrings) : String;
    Property FileCount : Integer read GetFileCount;
    Property FileNames[FileIndex : Integer] : String Read GetFileName;
  published
    Property Name : String Read FName;
    Property Directory : String Read FDirectory;
    Property Description : String Read FDescription Write FDescription;
    Property Variables : TStrings Read FVariables Write SetVariables;
    Property Author : String Read FAuthor;
    Property Recurse : Boolean Read FRecurse;
    Property Exclude : String Read FExclude;
  end;

  { TProjectTemplates }

  TProjectTemplates = class(TCollection)
  private
    FTemplateDir: String;
    function GetTemplate(Index : Integer): TProjectTemplate;
    function GetTemplateName(Index : Integer): String;
    procedure SetTemplate(Index : Integer; const AValue: TProjectTemplate);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    Constructor Create(Const ATemplateDir : String);
    Procedure Initialize(Const ATemplateDir : String);
    Procedure CreateProject(Const ProjectName, ProjectDir : String; Variables : TStrings);
    Function IndexOfProject(Const ProjectName : String) : Integer;
    Function ProjectTemplateByName(Const ProjectName : String) : TProjectTemplate;
    Property TemplateDir : String Read FTemplateDir;
    Property Names [Index : Integer] : String  Read GetTemplateName;
    Property Templates[Index : Integer] : TProjectTemplate Read GetTemplate Write SetTemplate;default;
  end;

  ETemplateError=Class(Exception);

Const
  // Section & Key names for ini file.
  SProject       = 'Project';
  SVariables     = 'Variables';
  KeyName        = 'Name';
  KeyAuthor      = 'Author';
  KeyDescription = 'Description';
  KeyRecurse     = 'Recurse';
  KeyExclude     = 'Exclude';

Function SubstituteString(Const S : String; Variables : TStrings): String;
Function SimpleFileCopy(Const Source,Dest : String) : Boolean;

implementation

resourcestring
  SErrNoSuchTemplate = '"%s": No such template.';
  SErrCouldNotCreateDir = 'Could not create directory "%s"';
  SErrFailedToCopyFile = 'Failed to copy file "%s" to "%s"';

{ Auxiliary function }


Function SubstituteString(Const S : String; Variables : TStrings): String;

Var
  T : String;
  P : Integer;

begin
  T:=S;
  Result:='';
  Repeat
    P:=Pos('$(',T);
    If (P=0) then
      begin
      Result:=Result+T;
      T:='';
      end
    else
      begin
      Result:=Result+Copy(T,1,P-1);
      Delete(T,1,P+1);
      P:=Pos(')',T);
      If (P=0) then
        begin
        Result:=Result+'$('+T;
        T:='';
        end
      else
        begin
        Result:=Result+Variables.Values[Copy(T,1,P-1)];
        Delete(T,1,P);
        end;
      end;
  until (T='');
end;

Function SimpleFileCopy(Const Source,Dest : String) : Boolean;

begin
  Result:=True;
end;


{ TProjectTemplates }

function TProjectTemplates.GetTemplateName(Index : Integer): String;
begin
  Result:=GetTemplate(Index).Name;
end;


function TProjectTemplates.GetTemplate(Index : Integer): TProjectTemplate;
begin
  Result:=Items[Index] as TProjectTemplate
end;


procedure TProjectTemplates.SetTemplate(Index : Integer;
  const AValue: TProjectTemplate);
begin
  Items[Index]:=AValue;
end;


constructor TProjectTemplates.Create(const ATemplateDir: String);
begin
  Inherited Create(TProjectTemplate);
  Initialize(ATemplateDir);
end;


function TProjectTemplates.IndexOfProject(const ProjectName: String): Integer;

begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetTemplate(Result).Name,ProjectName)<>0) do
    Dec(Result)
end;


function TProjectTemplates.ProjectTemplateByName(const ProjectName: String): TProjectTemplate;

Var
  Index : Integer;

begin
  Index:=IndexOfProject(ProjectName);
  If (Index=-1) then
    Raise ETemplateError.CreateFmt(SErrNoSuchTemplate,[ProjectName]);
  Result:=GetTemplate(Index);
end;


procedure TProjectTemplates.Initialize(const ATemplateDir: String);

Var
  Info : TSearchRec;
  D : String;

begin
  Clear;
  FTemplateDir:=IncludeTrailingPathDelimiter(ATemplateDir);
  D:=FTemplateDir;
  try
    If FindFirst(D+'*',faDirectory,Info)=0 then
      Repeat
        If ((Info.Attr and faDirectory)<>0)
           and not ((Info.Name='.') or (Info.Name='..') or (Info.Name='')) then
          With Add as TProjectTemplate do
            InitFromDir(D+Info.Name);
      Until FindNext(Info)<>0;
  finally
    FindClose(Info);
  end;
end;


procedure TProjectTemplates.CreateProject(const ProjectName, ProjectDir: String; Variables : Tstrings);

Var
  T : TProjectTemplate;
  
begin
  T:=ProjectTemplateByName(ProjectName);
  T.CreateProject(ProjectDir,Variables);
end;

{ TProjectTemplate }

constructor TProjectTemplate.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVariables:=TStringList.Create;
  FFiles:=TStringList.Create;
end;


destructor TProjectTemplate.Destroy;
begin
  FreeAndNil(FVariables);
  FreeAndNil(FFiles);
  inherited Destroy;
end;


procedure TProjectTemplate.SetVariables(const AValue: TStrings);
begin
  FVariables.Assign(AValue);
end;

function TProjectTemplate.GetFileName(FileIndex : Integer): String;
begin
  Result:=FFiles[FileIndex];
end;

function TProjectTemplate.GetFileCount: Integer;
begin
  Result:=FFiles.Count;
end;


procedure TProjectTemplate.InitFromDir(const DirName: String);

Var
  L : TStringList;
  FN : String;
  
begin
  FDirectory:=IncludeTrailingPathDelimiter(DirName);
  L:=TStringList.Create;
  Try
    FN:=FDirectory+'project.ini';
    If FileExists(FN) then
      begin
      With TMemInifile.Create(FN) do
        try
          FName:=ReadString(SProject,KeyName,DirName);
          FAuthor:=ReadString(SProject,KeyAuthor,'');
          FDescription:=ReadString(SProject,KeyDescription,'');
          FRecurse:=ReadBool(SProject,KeyRecurse,False);
          FExclude:=ReadString(SProject,KeyExclude,'');
          If (FExclude<>'') then
            FExclude:=FExclude+',';
          ReadSectionValues(SVariables,FVariables);
        Finally
          Free;
        end;
      end;
    FN:=Directory+'description.txt';
    If FileExists(FN) then
      begin
      L.LoadFromFile(FN);
      FDescription:=L.Text;
      end;
    GetFileList(FDirectory);
  Finally
    L.Free;
  end;
end;


procedure TProjectTemplate.CreateFile(FileIndex: Integer; Source, Values: TStrings);
begin
  CreateFile(FileNames[FileIndex],Source,Values);
end;

procedure TProjectTemplate.CreateFile(const FileName: String; Source,
  Values: TStrings);
  
Var
  F : Text;
  Line : String;
  
begin
  AssignFile(F,FileName);
  Reset(F);
  Try
    While not EOF(F) do
      begin
      ReadLn(F,Line);
      Source.Add(SubstituteString(Line,Values));
      end;
  Finally
    CloseFile(F);
  end;
end;

procedure TProjectTemplate.CreateProjectDirs(const BaseDir: String; Values : TStrings);

Var
  RFN : String;
  I   : Integer;
  
begin
  If not ForceDirectories(BaseDir) then
    Raise ETemplateError.CreateFmt(SErrCouldNotCreateDir,[BaseDir]);
  For I:=0 to FileCount-1 do
    begin
    RFN:=ExtractRelativePath(Directory,FileNames[i]);
    RFN:=SubstituteString(ExtractFilePath(RFN),Values);
    If (RFN<>'') Then
      If not ForceDirectories(BaseDir+RFN) then
        Raise ETemplateError.CreateFmt(SErrCouldNotCreateDir,[BaseDir+RFN]);
    end;
end;

function TProjectTemplate.TargetFileName(FN: String; Values: TStrings): String;

Var
  RFN : String;

begin
  Result:=ExtractRelativePath(Directory,FN);
  Result:=SubstituteString(Result,Values);
end;

function TProjectTemplate.TargetFileName(I: Integer; Values: TStrings): String;
begin
  Result:=TargetFileName(FileNames[I],Values);
end;

procedure TProjectTemplate.CopyAndSubstituteFile(Const SrcFN,DestFN : String; Values : Tstrings);

Var
  L : TStrings;
  
begin
  If pos(ExtractFileExt(SrcFN)+',',Exclude)<>0 then
    begin
    If not SimpleFileCopy(SrcFN,DestFN) then
      Raise ETemplateError.CreateFmt(SErrFailedToCopyFile,[SrcFN,DestFN]);
    end
  else
    begin
    L:=TstringList.Create;
    try
      CreateFile(SrcFN,L,Values);
      L.SaveToFile(DestFN);
    Finally
      L.Free;
    end;
    end;
end;

procedure TProjectTemplate.GetFileList(Const Dir : String);

Var
  Info : TSearchRec;
  
begin
  If FindFirst(Dir+'*',0,Info)=0 then
    try
      repeat
        if (info.name<>'description.txt') and (info.name<>'project.ini') then
          FFiles.Add(Dir+Info.Name);
       Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
  if Recurse then
    If (FindFirst(Dir+'*',0,Info)<>0) then
      try
        repeat
          if ((Info.attr and faDirectory)<>0) and
            (Info.Name<>'.') and (info.Name<>'..') and (Info.Name<>'') then
         GetFileList(Dir+Info.Name+PathSeparator);
        until FindNext(Info)<>0;
      finally
        FindClose(Info);
      end;
end;


procedure TProjectTemplate.CopyAndSubstituteDir(Const SrcDir,DestDir : String; Values: Tstrings);

Var
  D1,D2 : String;
  Info : TSearchRec;

begin
  D1:=IncludeTrailingPathDelimiter(SrcDir);
  D2:=IncludeTrailingPathDelimiter(DestDir);
  If not ForceDirectories(D2) then
    Raise ETemplateError.CreateFmt(SErrCouldNotCreateDir,[D2]);
  If FindFirst(D1+'*',0,Info)=0 then
    try
      repeat
        if (info.name<>'description.txt')
         and (info.name<>'project.ini') then
        CopyAndSubstituteFile(D1+Info.Name,D2+SubstituteString(Info.Name,Values),Values);
       Until (FindNext(Info)<>0);
    finally
      FindClose(Info);
    end;
  if Recurse then
    If (FindFirst(D1+'*',0,Info)<>0) then
      try
        repeat
          if ((Info.attr and faDirectory)<>0) and
            (Info.Name<>'.') and (info.Name<>'..') and (Info.Name<>'')
          then
            CopyAndSubstituteDir(D1+Info.Name,D2+SubstituteString(Info.Name,Values),Values);
        until FindNext(Info)<>0;
      finally
        FindClose(Info);
      end;
end;

procedure TProjectTemplate.CreateProject(const ProjectDir: String;
  Values: TStrings);
begin
  CopyAndSubstituteDir(Directory,ProjectDir,Values);
end;



end.
