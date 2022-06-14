unit idehtmltools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, idehtml2class, ProjectIntf;

Const
  // Options for HTML -> class generation
  SHTML2FormOptions = 'HTML2FormOptions';
  // HTML file associated with a project file. This is a convention
  SDesignHTMLFile = 'DesignHTMLFile';

Type

  { TIDEHTMLTools }
  TComponentHTMLFileNameHandler = procedure (Sender : TObject; aComponent : TComponent; var aHTMLFileName : string) of object;

  TIDEHTMLTools = class(TPersistent)
  Private
    Type
      { TTagCacheItem }

      TTagCacheItem = class
        FFilename : String;
        FTimeStamp : TDateTime;
        FTags : TStringArray;
        Constructor Create(Const aFilename : String; aTimeStamp : TDateTime; aTags: TStringArray);
        function IsValid : Boolean;
      end;

      { THandler }

      THandler = Class(TObject)
      Private
        FClass : TComponentClass;
        FHandler : TComponentHTMLFileNameHandler;
      Public
        Constructor Create(aClass : TComponentClass; aHandler: TComponentHTMLFileNameHandler);
        Function Matches(aClass : TComponentClass; aHandler: TComponentHTMLFileNameHandler) : Boolean;
        Function MatchesClass(aClass : TComponentClass) : Boolean;
        Property ComponentClass : TComponentClass Read FClass;
        Property Handler : TComponentHTMLFileNameHandler Read FHandler;
      end;

    function GetNameFromComponent(aComponent: TComponent): string;
  Private
    Class Var
      _ComponentHandlers  : TFPObjectList;
  Private
    FTagCache : TFPObjectHashTable;
    function HasCached(const aFileName: string; aList: TStrings): Boolean;
  Public
    class function GetDefaultHTMLDesignFile(aFile: TLazProjectFile): String;
    class function GetDefaultHTML2ClassFile(aFile: TLazProjectFile): String;
  Public
    Constructor Create;
    Class Constructor Init;
    Class Destructor Done;
    Class Procedure RegisterComponent2HTMLFileHandler(aClass : TComponentClass; aHandler : TComponentHTMLFileNameHandler);
    Class Procedure UnRegisterComponent2HTMLFileHandler(aClass : TComponentClass; aHandler : TComponentHTMLFileNameHandler);
    Procedure GetTagIDs(Const aFileName : string; aList : TStrings; aOptions : TExtractOptions = []);
    Procedure GetTagIDs(Const aFileName : string; aList : TTagInfoList; aOptions : TExtractOptions = []);
    Function GetTagIDs(Const aFileName : string; aOptions : TExtractOptions = []) : TStringArray;
    class function TagToIdentifier(aTag: String): String;
    function GetHTMLFileForProjectFile(aFile: TLazProjectFile): String;
    Function GetHTMLFileForComponent(aComponent : TComponent) : String;
    class function GetProjectHTMLFile : String;
    Procedure ClearCache;
  end;

Var
  HTMLTools : TIDEHTMLTools;

implementation

uses LazIDEIntf, forms, pjscontroller;

{ TIDEHTMLTools.THandler }

constructor TIDEHTMLTools.THandler.Create(aClass: TComponentClass;
  aHandler: TComponentHTMLFileNameHandler);
begin
  FClass:=aClass;
  FHandler:=aHandler;
end;

function TIDEHTMLTools.THandler.Matches(aClass: TComponentClass;
  aHandler: TComponentHTMLFileNameHandler): Boolean;
begin
  Result:=(aHandler=FHandler) and MatchesClass(aClass);
end;

function TIDEHTMLTools.THandler.MatchesClass(aClass: TComponentClass): Boolean;
begin
  Result:=aClass.InheritsFrom(FClass);
end;

{ TIDEHTMLTools.TTagCacheItem }

constructor TIDEHTMLTools.TTagCacheItem.Create(const aFilename: String;
  aTimeStamp: TDateTime; aTags: TStringArray);
begin
  FTimeStamp:=aTimeStamp;
  FFilename:=aFileName;
  FTags:=aTags;
end;

function TIDEHTMLTools.TTagCacheItem.IsValid: Boolean;

Var
  aDateTime : TDateTime;

begin
  Result:=FileAge(FFileName,aDateTime) and (aDateTime<=FTimeStamp);
end;

{ TIDEHTMLTools }

constructor TIDEHTMLTools.Create;
begin
  FTagCache:=TFPObjectHashTable.Create(True);

end;

class constructor TIDEHTMLTools.Init;
begin
  _ComponentHandlers:=TFPObjectList.Create(True);
end;

class destructor TIDEHTMLTools.Done;
begin
  FreeAndNil(_ComponentHandlers);
end;

class procedure TIDEHTMLTools.RegisterComponent2HTMLFileHandler(
  aClass: TComponentClass; aHandler: TComponentHTMLFileNameHandler);
begin
  _ComponentHandlers.Add(THandler.Create(aClass,aHandler));
end;

class procedure TIDEHTMLTools.UnRegisterComponent2HTMLFileHandler(
  aClass: TComponentClass; aHandler: TComponentHTMLFileNameHandler);

Var
  Idx : Integer;

begin
  Idx:=_ComponentHandlers.Count-1;
  While (Idx>=0) and not THandler(_ComponentHandlers[Idx]).Matches(aClass,aHandler) do
    Dec(Idx);
  if Idx>=0 then
    _ComponentHandlers.Delete(Idx);
end;

class function TIDEHTMLTools.TagToIdentifier(aTag: String): String;

Var
  C : Char;

begin
  Result:='';
  for C in aTag do
    if C in ['_','a'..'z','A'..'Z','0'..'9'] then
      Result:=Result+C
    else
      Result:=Result+'_';
end;

function TIDEHTMLTools.HasCached(const aFileName: string; aList: TStrings
  ): Boolean;

Var
  Itm : TTagCacheItem;

begin
  Itm:=TTagCacheItem(FTagCache.Items[aFileName]);
  Result:=Assigned(Itm);
  if Result then
    begin
    Result:=Itm.IsValid;
    if Result then
      aList.AddStrings(Itm.FTags,True)
    else
      FTagCache.Delete(aFileName);
    end;
end;


procedure TIDEHTMLTools.GetTagIDs(const aFileName: string; aList: TStrings; aOptions : TExtractOptions = []);

Var
  Itm : TTagCacheItem;

begin
  If Not HasCached(aFileName,aList) then
    with THTMLExtractIDS.Create(Nil) do
      try
        Options:=aOptions;
        ExtractIDS(aFileName,aList);
        Itm:=TTagCacheItem.Create(aFileName,Now,aList.ToStringArray);
        FTagCache.Add(aFileName,Itm);
      finally
        Free;
      end;
end;

procedure TIDEHTMLTools.GetTagIDs(const aFileName: string; aList: TTagInfoList;
  aOptions: TExtractOptions);
begin
  // Todo : cache
  with THTMLExtractIDS.Create(Nil) do
    try
      Options:=aOptions;
      ExtractIDS(aFileName,aList);
    finally
      Free;
    end;
end;

function TIDEHTMLTools.GetTagIDs(const aFileName: string; aOptions : TExtractOptions = []): TStringArray;

Var
  aList : TStrings;

begin
  aList:=TStringList.Create;
  try
    GetTagIDS(aFileName,aList,aOptions);
    Result:=aList.ToStringArray;
  finally
    aList.Free;
  end;
end;

class function TIDEHTMLTools.GetDefaultHTMLDesignFile(aFile: TLazProjectFile
  ): String;

begin
  if assigned(aFile) and assigned(aFile.CustomData) then
    Result:=aFile.CustomData.Values[SDesignHTMLFile];
end;

class function TIDEHTMLTools.GetDefaultHTML2ClassFile(aFile : TLazProjectFile) : String;

Var
  aOptions : THTML2ClassOptions;
  S : String;

begin
  Result:='';
  S:=aFile.CustomData.Values[SHTML2FormOptions];
  if (S<>'') then
    begin
    aOptions:=THTML2ClassOptions.Create;
    try
      aOptions.FromJSON(S);
      Result:=aOptions.HTMLFileName;
    finally
      aOptions.Free;
    end;
    end;
end;

class function TIDEHTMLTools.GetProjectHTMLFile: String;

Var
  Prj : TLazProject;
  aFile : TLazProjectFile;
  I : Integer;

begin
  Result:='';
  Prj:=LazarusIDE.ActiveProject;
  if (Prj=Nil) then
    exit;
  I:=0;
  While (Result='') and (I<Prj.FileCount) do
    begin
    aFile:=Prj.Files[I];
    Writeln('Checking ',aFile.ClassName,', FileName: ',aFile.FileName,', FullFileName: ',aFile.GetFullFilename,', Custom Data: ',aFile.CustomData[PJSIsProjectHTMLFile]);
    if aFile.CustomData[PJSIsProjectHTMLFile]<>'' then
      Result:=aFile.Filename;
    Inc(I);
    end;
  if Result='' then
    begin
    Result:=TPJSController.GetProjectHTMLFilename(Prj);
    if not FileExists(Result) then
       Result:='';
    end;
end;

function TIDEHTMLTools.GetHTMLFileForProjectFile(aFile : TLazProjectFile): String;


begin
  // We should really have a pluggable mechanism.
  Result:=GetDefaultHTMLDesignFile(aFile);
  if Result='' then
    Result:=GetDefaultHTML2ClassFile(aFile);
  if Result='' then
    Result:=GetProjectHTMLFile;
end;

function TIDEHTMLTools.GetNameFromComponent(aComponent : TComponent) : string;

Var
  Idx,aCount : Integer;
  aClass : TComponentClass;

begin
  Result:='';
  aCount:=_ComponentHandlers.Count;
  aClass:=TComponentClass(aComponent.ClassType);
  Idx:=0;
  While (Result='') and (Idx<aCount) do
    With THandler(_ComponentHandlers[Idx]) do
      begin
      // Writeln('Checking class ',aClass);
      if MatchesClass(aClass) then
        Handler(Self,aComponent,Result);
      Inc(Idx);
      end;
end;

function TIDEHTMLTools.GetHTMLFileForComponent(aComponent: TComponent): String;

Var
  aFile : TLazProjectFile;
begin
  // See if a handler is registered for the component.
  Result:=GetNameFromComponent(aComponent);
  if Result='' then
    // try a handler for the form on which the component is dropped
    Result:=GetNameFromComponent(aComponent.Owner);
  if Result='' then
    begin
    // Now try settings stored in project
    aFile:=LazarusIDE.GetProjectFileWithRootComponent(aComponent.Owner);
    if aFile=Nil then
      aFile:=LazarusIDE.GetProjectFileWithRootComponent(aComponent);
    if Assigned(aFile) then
      Result:=GetHTMLFileForProjectFile(aFile);
    end;
end;

procedure TIDEHTMLTools.ClearCache;
begin
  FTagCache.Clear;
end;

Initialization
  HTMLTools:=TIDEHTMLTools.Create;

Finalization
  HTMLtools.Free;
end.

