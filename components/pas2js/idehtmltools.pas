unit idehtmltools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, ProjectIntf;

Const
  // Options for HTML -> class generation
  SHTML2FormOptions = 'HTML2FormOptions';
  // HTML file associated with a project file. This is a convention
  SDesignHTMLFile = 'DesignHTMLFile';

Type

  { TIDEHTMLTools }

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
    function HasCached(const aFileName: string; aList: TStrings): Boolean;
  Private
    FTagCache : TFPObjectHashTable;
  Public
    class function GetDefaultHTMLDesignFile(aFile: TLazProjectFile): String;
    class function GetDefaultHTML2ClassFile(aFile: TLazProjectFile): String;
    class function GetProjectHTMLFile : String;
  Public
    Constructor Create;
    Procedure GetTagIDs(Const aFileName : string; aList : TStrings);
    Function GetTagIDs(Const aFileName : string) : TStringArray;
    class function TagToIdentifier(aTag: String): String;
    function GetHTMLFileForProjectFile(aFile: TLazProjectFile): String;
    Function GetHTMLFileForComponent(aComponent : TComponent) : String;

    Procedure ClearCache;
  end;

Var
  HTMLTools : TIDEHTMLTools;

implementation

uses LazIDEIntf, forms, idehtml2class, pjscontroller;

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

Class Function TIDEHTMLTools.TagToIdentifier(aTag : String) : String;

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


procedure TIDEHTMLTools.GetTagIDs(const aFileName: string; aList: TStrings);

Var
  Itm : TTagCacheItem;

begin
  If Not HasCached(aFileName,aList) then
    with THTMLExtractIDS.Create(Nil) do
      try
        ExtractIDS(aFileName,aList);
        Itm:=TTagCacheItem.Create(aFileName,Now,aList.ToStringArray);
        FTagCache.Add(aFileName,Itm);
      finally
        Free;
      end;
end;

function TIDEHTMLTools.GetTagIDs(const aFileName: string): TStringArray;

Var
  aList : TStrings;

begin
  aList:=TStringList.Create;
  try
    GetTagIDS(aFileName,aList);
    Result:=aList.ToStringArray;
  finally
    aList.Free;
  end;
end;

class function TIDEHTMLTools.GetDefaultHTMLDesignFile(aFile: TLazProjectFile
  ): String;

begin
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
    Result:=Prj.CustomData.Values[PJSProjectHTMLFile];
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

function TIDEHTMLTools.GetHTMLFileForComponent(aComponent: TComponent): String;

Var
  aFile : TLazProjectFile;
begin
  aFile:=LazarusIDE.GetProjectFileWithRootComponent(aComponent.Owner);
  Result:=GetHTMLFileForProjectFile(aFile);
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

