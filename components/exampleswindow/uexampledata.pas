unit uexampledata;

{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit is the backend that provides an List that contains details of Lazarus
Example Projects. It might get its data from one of three different places,

* The LazarusDir and the LazarusConfigDir.
* A locally cached master meta file          Disabled as of Feb 2022
* A remote gitlab repository (ie, if the above is not present),  Disabled as of Feb 2022

This list can be used to populate the Lazarus Examples Window or used during the
markup of existing Lazarus Projects. The unit is used by the Lazarus Package and
a simple tool used to manage the meta data files.

-- PATHS --

This only really applies in the Out of Lazarus Package usage. David Bannon, Feb 2022

Data is inserted into the list from different sources and might refer to
content stored in different places.

So, wrt FFname in the list, a path starting with a slash, / or \, is an absolute
local path. OTOH, without a slash, its remote, eg, gitlab and relative to the
top of the repository.

Special case is when we are reading the local git repository, we are doing this
to make a file to upload to the gilab repo that is an index of the remote repository,
so, no leading slash and all paths are relative to the top of the local git repo.

This unit does not interact directly with user but it does (hopefully not often)
generate some error messages that may need i18n.  Only network errors have been done.


WARNING - This unit includes code to download (and even upload) from a gitlab
repo. At present its not being used and should get stripped out during linking.
If it appears, long term, we are never to use the online approach, remove it !
Code would be greatly simplified if we were not trying to also support OnLine.
}

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, fpjson, jsonparser ;

const
    MetaFileExt = '.ex-meta';              // Extension of meta files.

type TExampleDataSource = ( FromGitlabTree,    // Read all remote project meta files
                            FromLocalTree,     // Read all local Git project meta files
                            FromCacheFile,     // Load data from Local Cache File
                            FromLazSrcTree);   // Searches the Lazarus Src Tree, eg ~/examples; ~/components

type
    PExRec=^TExRec;
    TExRec = record
          EName    : string;      // CamelCase version of the example name, filenameonly of metadata file.
          Category : string;      // eg Beginner, NoDesign (read from remote data)
          Keywords : TStringList; // a list of (possibly multi-word) words, nil acceptable
          FFName   : string;      // Path and filename of meta file. Maybe absolute or relative, no extension
          Desc     : string;      // 1..many lines of description
    end;

type
    { TExampleList }
    TExampleList = class(TFPList)
        private

            procedure DumpList(wherefrom: string; ShowDesc: boolean = false);
            function Get(Index: integer): PExRec;

         public
             constructor Create();
             destructor Destroy; override;
                                         // Public - Puts new entry in List, Keys may be Nil
             function InsertData(Cat, Desc, FFName, AName: string; Keys: TStringList): boolean;
             function Find(const FFname: string): PExRec;
             function AsJSON(Index: integer): string;
             // Ret T if St is in Keywords at AnIndex, not necessarily equal to.
             function IsInKeywords(St : string; AnIndex : integer) : boolean;
             property Items[Index: integer]: PExRec read Get; default;


end;

{ Note - the above list is used to generate a master.ex-meta file that might be added
the the gitlab repo. So, dir seperators MUST be /. On Windows, they will be read
from a local tree as \ and a local master.ex-meta file will need to be converted.
I think we will declare they are always /, when reading local filesystems on
Windows, must convert during the insert into list stage.   }


type

    { TExampleData }

    TExampleData = class
        private
            ErrorString : String;
            ExList : TExampleList;
            GetListDataIndex : integer;

                                // Gets a Full URL and returns with St containing content, usually as JSON
            function Downloader(URL: string; out SomeString: String): boolean;
                                // Does a binary safe download of a file, URL will get repositary info prepended
                                // and file ends up in FullDest which should be a full path and filename.
            function DownLoadFile(const URL, FullDest: string): boolean;
                                //function EscJSON(InStr: string): string;
            function ExtractArrayFromJSON(const Field: string; jItem: TJSONData; STL: TStringList): boolean;
                                // Passed a json block, returns the indicated field, cannot handle arrays.
                                // Don't rely on its base64 decoding a binary file, see DownLoadFile() instead.
            function ExtractFromJSON(const Field, data: string; Base64: boolean=false) : string;
            function ExtractFromJSON(const Field: string; const jItem: TJSONData; out
                                        Res: string; Base64: boolean = false): boolean;
            function GetLazDir: string;

                                // Receives a pretested JSON (not just a field) containing metadata of an Example
                                // Returns false if data missing, drops msg to console about bad field.
                                // Path may be relative or absolute (ie starting with '/' or '\'). Ones without
                                // a leading slash are remote, ie gitlab. Ones with a slash should be resolvable
                                // locally. Note when indexing a local git tree, relative must be used, ie top of
                                // git tree. In this mode of course, the entry will not be resolvable locally.
            function InsertJSONData(jItem: TJSONData; FFName: string; AName: string = '' ): boolean;
            function LoadCacheFile(FFName: string): boolean;
            function ReadMasterJSON(FileContent: TStringList): boolean;
            function ReadRemoteMetaFile(URL: string): boolean;   // download and read meta file
                                // Gets passed a block of json, wrapped in {} containing several fields relating
                                // one example project. Path is ready to use in the List. Not suited to json
                                // With an internal Path field (ie master.ex-meta)
            function ReadSingleJSON(FileContent: TStringList; PathToStore: string = ''): boolean;
            function ScanLocalTree(Path: string; PathAbs: boolean): boolean;
                                // Will either scan and add what it finds to the List (if STL is nil) or it
                                // will add each full URL to the StringList if its valid and created.
            function ScanRemoteTree(Path: string; STL: TstringList = nil): boolean;
            function ScanOneTree(Path: string; out St: string): boolean;
            procedure fSetErrorString(Er : string);

        public
            LazConfigDir : string; // Where Lazarus keeps it config.
            RemoteRepo : string; // eg  https://gitlab.com/api/v4/projects/32480729/repository/

            ExamplesHome : string; // dir above examples_working_dir where we copy examples to, set by uintf.pas, usually <lazConf>/
            LazSrcDir    : string; // Laz dir where, eg ~/examples lives
            GitDir     : string; // where we look for a local git repo containg examples
            KeyFilter : string;  // A list of words, possibly grouped by " to filter Keywords
            CatFilter  : string; // A string that may contain 0 to n words, each word being a category as filtered by GetListData()
                                 // A service function, tests passed St to ensure its
                                 // a valid lump of Example Meta Data.
            function TestJSON(const J: string; out Error, Cat: string): boolean;
                                // Returns a path (with trailing delim) to where we will putting our downloaded
                                // or copied Example Projects. It includes the working dir. Usually something
                                // like <lazConfig>/examples_work_dir/ but is user configurable via Laz Settings.
            function ExampleWorkingDir: string;
                                // Public, returns with next set of data, false if no more available.
                                // Filters using CatFilter if CatFilter is not empty.
                                // If passed KeyList is not nil, filters keywords against KeyList.
            function GetListData(out Proj, Cat, Path, Keys: string; GetFirst: boolean;
                KeyList: TStringList = nil): boolean;
                                // Passed a created TStrings that it clears and fills in with all know categories
            function getCategoryData(const CatList : TStrings) : boolean;
                                // Pass the relative path and fileNameOnly of metafile, no extension (?)
            function GetDesc(const FFname: string): string;
            constructor Create;
            procedure LoadExData(DataSource: TExampleDataSource);
            destructor Destroy; override;
            procedure DumpExData();
                                // A service method, called by the GUI to download a project/
                                // Pass it a full example remote dir (eg Beginner/Laz_Hello/).
            function DownLoadDir(const FExampDir: string): boolean;
            function Count : integer;
            function ExtractFieldsFromJSON(const JStr: string; out EName, Cat, Keys, Desc,
                Error: string): boolean;
                                // Rets T if passed name is already in list as a project name
            function DoesNameExist(AName : string) : boolean;
            property ErrorMsg : string read ErrorString write FSetErrorString;
            class function EscJSON(InStr: string): string;
    end;


implementation


uses LCLProc,
    uConst,
    httpprotocol,  // for http encoding
    fphttpclient,  // determines a dependency on FPC 3.2.0 or later. Must for https downloads
    opensslsockets,
    ssockets, fpopenssl,
    lazfileutils, fileutil,
    jsonscanner,                        // these are the FPC JSON tools
    base64,
    laz2_DOM, laz2_XMLRead;            // just to get LazarusDirectory, remove if we find a better way !

const
    LastUpDate = 'LastUpDate';              // Name of JSON item were we store last update date


{ A URL starts with eg 'https://gitlab.com/api/v4/projects/32480729/repository/'
It contains a multidigit number that identifies the gitlab project. The number is a
combination of Owner (account, group..) and repository name. Its identified in Gitlab
web pages as "Project ID", group id will not work. A full URL might look like this -
https://gitlab.com/api/v4/projects/32866275/repository/files/Utility%2FExScanner%2Fproject1.ico?ref=main
}


// =============================================================================
//                T   E X A M P L E    L I S T
//==============================================================================

function TExampleList.Get(Index: integer): PExRec;
begin
    Result := PExRec(inherited get(Index));
end;

function TExampleList.InsertData(Cat, Desc, FFName, AName : string; Keys: TStringList): boolean;
var
    ExRecP : PExRec;
begin
    ExRecP := find(FFName);
    new(ExRecP);
    ExRecP^.Category := Cat;
    ExRecP^.KeyWords := Keys;      // Nil is acceptable
    ExRecP^.Desc := Desc;
    ExRecP^.FFName := FFName;
    ExRecP^.EName := AName;
    result := (inherited Add(ExRecP) > -1);
end;

                    // Returns an unquoted string being one JSON Escaped record from list.
function TExampleList.AsJSON(Index : integer) : string;                         // Not used, maybe remove ?   Or Add in EName
begin
    Result := '';
    Result := Result + 'Category : ' + Items[Index]^.Category + #10;
    Result := Result + 'Keywords : ' + Items[Index]^.Keywords.Text + #10#10;
    Result := Result + Items[Index]^.Desc;
    Result := Result.Replace('\', '\\', [rfReplaceAll] );
    Result := Result.Replace('"', '\"', [rfReplaceAll] );
end;

function TExampleList.IsInKeywords(St: string; AnIndex: integer): boolean;
    var KeyWord : String;
begin
    result := false;
    if pos(lowercase(St), lowercase(Items[AnIndex]^.EName)) > 0 then exit(true);
    for KeyWord in Items[AnIndex]^.Keywords do begin
        if pos(lowercase(St), lowercase(Keyword)) > 0 then exit(True);
    end;
end;


procedure TExampleList.DumpList(wherefrom: string; ShowDesc : boolean = false);    // ToDo : remove this, its just a debug method
var
    i : integer = 0;
begin
    DebugLn('-------- ExampleData Examples List ' + Wherefrom + '----------');
    while i < count do begin
        DebugLn('<<<< List - FFName=[' + Items[i]^.FFName +'] Cat=[' + Items[i]^.Category
                + '] EName=' + Items[i]^.EName
                + '] Key=[' + Items[i]^.Keywords.Text + ']');
        if ShowDesc then
            DebugLn(Items[i]^.Desc);
        inc(i);
    end;
end;

constructor TExampleList.Create();
begin
     inherited Create;
end;

destructor TExampleList.Destroy;
var
    i : integer;
begin
    for I := 0 to Count-1 do begin
        if Items[i]^.Keywords <> nil then
            Items[i]^.Keywords.free;
        dispose(Items[i]);
    end;
    inherited Destroy;
end;

function TExampleList.Find(const FFname: string): PExRec;
var
    i : integer = 0;
begin
    while i < count do begin
        if Items[i]^.FFname = FFname then
            exit(Items[i]);
        inc(i);
    end;
    Result := nil;
end;

// =============================================================================
//                     T     E X A M P L E   D A T A
// =============================================================================

// Address of this function is passed to a list sort call. We sort on category, Beginners at top
function CategorySorter( Item1: Pointer;   Item2: Pointer) : Integer;
begin
    result := CompareStr(PExRec(Item1)^.Category, PExRec(Item2)^.Category);
end;

function TExampleData.Count: integer;
begin
    result := ExList.Count;
end;

procedure TExampleData.fSetErrorString(Er : string);
begin
    ErrorString := Er;
    Debugln(ErrorString);
end;

function TExampleData.ExampleWorkingDir() : string;
begin
    result := AppendPathDelim(ExamplesHome) + cExamplesDir + PathDelim ;
end;



function TExampleData.ExtractFieldsFromJSON(const JStr: string; out EName, Cat,
                                                        Keys, Desc, Error: string): boolean;
var
    jData, jItem : TJSONData;
    STL : TStringList;
    St : string;
begin
    Error := '';
    result := TestJSON(JStr, Error, Cat);
    if Not Result then exit(False);               // some basic tests done, so
    jData := GetJSON(JStr);                       // we know these 2 lines are safe.
    jItem := jData.Items[0];
    STL := TStringList.Create;
    Result := False;
    try
        if not ExtractFromJSON('Description', jItem, Desc) then begin
            Desc := '';
        end;
        Keys := '';
        if ExtractArrayFromJSON('Keywords', JItem, StL) then begin
            for St in STL do
                Keys := Keys + '"' + ST + '",';
            if Keys.length > 1 then
                delete(Keys, Keys.Length, 1);
        end;
        EName := TJSONObject(jData).Names[0];
        Result := True;
    finally
        STL.Free;
        JData.Free;
    end;
end;

function TExampleData.DoesNameExist(AName: string): boolean;
var
    P : PExRec;
begin
    for P in ExList do
        if lowercase(AName) = lowercase(P^.EName) then
            exit(True);
    result := False;
end;

function TExampleData.TestJSON(const J : string; out Error, Cat : string) : boolean;
var
    jData, jItem : TJSONData;
begin
    Result := true;
    if (J.Length = 0) or (J[1] <> '{') then begin        // Ignore obvious non JSON
        Error := 'Empty text or does not start with {';
        exit(False)
    end;
    try
        try
            jData := GetJSON(J);                         // Is it valid JSON ?
            jItem := jData.Items[0];
        except
            on E: EJSONParser do begin
                Error := 'ERROR Parsing- invalid JSON ' + E.Message;
                jData := Nil;                            // Appears nothing is allocated on error  ?
                exit(false);
            end;
            on E: EScannerError do begin
                Error := 'ERROR Scanning- invalid JSON ' + E.Message;
                jData := Nil;                            // Appears nothing is allocated on error  ?
                exit(false);
            end;
        end;

        if  TJSONObject(jItem).Count = 0 then begin
            Error := 'WARNING - file does not contain suitable JSON : ';
            exit(false);
        end;
        if not ExtractFromJSON('Category', jItem, Cat) then begin
            Error := 'WARNING - Category Not Set ';
            exit(false);
        end;
    finally
        jData.free;
    end;
end;

// jItem never contains Project Path, its either found in json Name (master)
// or derived from where we found the project (individual). So, always passed here.
function TExampleData.InsertJSONData(jItem : TJSONData; FFName : string; AName : string = ''): boolean;
var
    Cat, Desc, AnotherName : String;
    // index : integer;
    KeyWords : TStringList;
begin
    Result := False;
    ExtractFromJSON('Category', jItem, Cat);        // An empty Cat is acceptable but undesirable.
    if not ExtractFromJSON('Description', jItem, Desc) then exit(False);
    KeyWords := TStringList.Create;
    ExtractArrayFromJSON('Keywords', jItem, Keywords);
    if AName <> '' then
        AnotherName := AName
    else
        if not ExtractFromJSON('Name', jItem, AnotherName) then
            AnotherName := '';
    if DoesNameExist(AnotherName) then begin
        debugln('TExampleData.InsertJSONData - WARNING duplicate Example Name found = '
            + AnotherName + ' ' + FFName);
    end
    else Result := ExList.InsertData(Cat, Desc, FFName, AnotherName, KeyWords);
    if not Result then KeyWords.Free;              // false means its not gone into list so our responsibility go free
end;

// Scans local tree below 'Path' looking for any likely Example Metadata files.
// For each, it loads content into a StringList and passes it to an Insert method.
// If AddPath, the full path is inserted, not just the relative one, eg extra dirs
function TExampleData.ScanLocalTree(Path : string; PathAbs : boolean) : boolean;
var
   STL : TStringList = nil;
   FileContent : TStringList;
   St : string;
begin
    STL := FindAllFiles(Path, '*' + MetaFileExt, True);
    try
        for St in STL do begin
            if pos('master' + MetaFileExt, St) > 0 then continue;    // don't do master if you stumble across one
            if pos(cExamplesDir, St) > 0 then continue;               // thats our downloaded location
            FileContent := TStringList.Create;
            FileContent.LoadFromFile(St);                            // Thats contents of one individual metadata file
            try
                if PathAbs then
                    Result := ReadSingleJSON(FileContent, St)
                else  Result := ReadSingleJSON(FileContent, copy(St, Path.Length+1, 1000));
                if not Result then begin
                    debugln('Offending file is ' + St);
                    debugln(ErrorMsg);
                    exit(False);
                end;
            finally
                FileContent.Free;
            end;
        end;
    finally
        STL.Free;
    end;
end;


function TExampleData.ReadSingleJSON(FileContent : TStringList; PathToStore : string = '') : boolean;
var
    jData, jItem : TJSONData;
begin
    Result := true;
    if (FileContent.Count > 0) and (FileContent[0][1] = '{') then begin     // Ignore obvious non JSON
        try
            try
                jData := GetJSON(FileContent.Text);                         // Is it valid JSON ?
                jItem := jData.Items[0];
            except
                on E: EJSONParser do begin
                    ErrorMsg := 'ERROR EJSONParser- invalid JSON ' + E.Message;
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
                on E: EScannerError do begin                                // Thats in jsonscanner unit, Must doc on Wiki !!!
                    ErrorMsg := 'ERROR EScanner- invalid JSON ' + E.Message;          // this is typically a single \
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
            end;
            if  TJSONObject(jItem).Count = 0 then begin
                debugln('WARNING - file does not contain suitable JSON : ');
                exit(false);
            end;
            InsertJSONData(jItem, PathToStore, TJSONObject(jData).Names[0]);
        finally
            jData.free;
        end;
    end;
end;

destructor TExampleData.Destroy;
begin
    ExList.free;
    inherited Destroy;
end;

procedure TExampleData.DumpExData;                   // ToDo : remove this, just a debug thingo
begin
    ExList.DumpList('TExampleData.Dump', True);
end;

constructor TExampleData.Create();
begin
    ExList := TExampleList.Create;
end;

procedure TExampleData.LoadExData(DataSource: TExampleDataSource);
begin
    // If we are loading the data from either the remote gitlab tree or a local
    // git tree, we save the master file.
    if not DirectoryExists(ExampleWorkingDir()) then
        if not ForceDirectory(ExampleWorkingDir()) then exit;
    case DataSource of
        FromGitLabTree : begin                           // too slow to be useful
                            ScanRemoteTree('');
                         end;
        FromLocalTree  : begin                           // not used in Lazarus Package
                            if ScanLocalTree(GitDir, False) then        // This should leave relative paths, suitable to upload to gitlab
                         end;
        FromLazSrcTree : begin
                               ScanLocalTree(GetLazDir(), True);                 // Scan the Lazarus SRC tree
                               ScanLocalTree(ExamplesHome, True);                // Get, eg, any OPM Examples
                               // in the above line, we assume if user has moved Examples, then they will have OPM there too.
                         end;
        FromCacheFile  : begin
                            if not LoadCacheFile(ExampleWorkingDir()+ 'master' + MetaFileExt) then begin
                                DownLoadFile('master' + MetaFileExt, ExampleWorkingDir()+ 'master' + MetaFileExt);
                                LoadCacheFile(ExampleWorkingDir()+ 'master' + MetaFileExt);                  // ToDo : Test that worked
                            end;
                            ScanLocalTree(ExamplesHome, True);                // Get, eg, any OPM Examples
                         end;
    end;
    ExList.Sort(@CategorySorter);
//    if ExList.Count = 0 then begin
//        debugln('TExampleData.LoadExData - found examples = ' + inttostr(ExList.Count));
//        debugln('Lazarus Dir (ie source tree) = ' + GetLazDir());
//        debugln('Lazarus Config Dir = ' + LazConfigDir);
//        debugln('Examples Home Dir  = ' + ExamplesHome);
//    end;
end;


// ****************** Local master meta File methods ***************************

function TExampleData.ReadMasterJSON(FileContent : TStringList) : boolean;
var
    jData, jItem : TJSONData;
    i : integer;
begin
    Result := true;

    if (FileContent.Count > 0) and (FileContent[0][1] = '{') then begin     // Ignore obvious non JSON
        try
            try
                jData := GetJSON(FileContent.Text);                         // Is it valid JSON ?
            except
                on E: EJSONParser do begin
                    ErrorMsg := 'ERROR EJSONParser - invalid JSON ' + E.Message;
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
                on E: EScannerError do begin
                    ErrorMsg := 'ERROR EScannerError - invalid JSON ' + E.Message;
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
            end;
            for i := 0 to jData.Count-1 do begin                            // check its real JSON, not just a field.
                jItem := jData.Items[i];                                    // do not free.
                if  TJSONObject(jItem).Count > 0 then begin                 // might be ...
                    InsertJSONData(jItem, TJSONObject(jData).Names[i]);
                end;
            end;
        finally
            freeandnil(jData);
        end;
    end else result := False;
end;

function TExampleData.LoadCacheFile(FFName : string) : boolean;
var
    FileContent : TStringList;
begin
    if not FileExists(FFName) then exit(False);
    FileContent := TStringList.Create;
    try
        FileContent.LoadFromFile(FFname);
        Result := ReadMasterJSON(FileContent);
        if not Result then
            debugln('Offending file is ' + FFName);
    finally
        FileContent.Free;
    end;
    Result := true;
end;

{   environmentoptions.xml
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <EnvironmentOptions>
    ...
    <LazarusDirectory Value="/home/dbannon/bin/Lazarus/lazarus-main">  .... }


function TExampleData.GetLazDir() : string;                          // Todo : make direct call
var
    Doc : TXMLDocument;
    Node, Node1 : TDOMNode;
begin
    Result := '';
    ReadXMLFile(Doc, LazConfigDir + 'environmentoptions.xml');       // even in EXTESTMODE LazConfigDir should be valid
    Node1 := Doc.DocumentElement.FindNode('EnvironmentOptions');
    if Node1 <> nil then begin
       Node := Node1.FindNode('LazarusDirectory');
        if Node <> nil then
           Result := AppendPathDelim(Resolvedots(Node.Attributes.GetNamedItem('Value').NodeValue));
            // Apparently sometimes Lazarus puts a relative path in envopts.xml - danger here is
            // that if Lazarus now has a working dir different from when the path was written, it
            // will be wrong anyway. Further research is indicated.
    end;
    Doc.free;
    // debugln('TExampleData.GetLazDir = ' + Result);
end;

class function TExampleData.EscJSON(InStr : string) : string;
begin
    Result := InStr.Replace('\', '\\', [rfReplaceAll]);
    Result := Result.Replace('"', '\"', [rfReplaceAll]);
    Result := Result.Replace(#10, '\n', [rfReplaceAll] );   // LF
    Result := Result.Replace(#13, '', [rfReplaceAll] );     // CR
    Result := Result.Replace(#09, '', [rfReplaceAll] );     // tab
end;


// ********************  Methods relating to using the data  *******************

function TExampleData.GetListData(out Proj, Cat, Path, Keys : string;
                        GetFirst: boolean; KeyList : TStringList = nil): boolean;
var
   St : string;
   DoContinue : boolean = false;
begin
    //KeyList may be Nil, if so, ignore
    Result := True;
    if CatFilter = '' then exit(False);
    if GetFirst then
        GetListDataIndex := 0;
    while True do begin
        if GetListDataIndex >= ExList.Count then exit(False);
        if CatFilter <> '' then begin               // Find an entry in one of the categories
            // orig a while instead of if, needed to use DoContinue ... Why ?
            if pos(ExList.Items[GetListDataIndex]^.Category, CatFilter) < 1 then begin
                inc(GetListDataIndex);
                continue;
            end;
        end;
        if KeyList <> Nil then begin
            for St in KeyList do
                // IndexOf requires a 1:1 match, we want to know if St is in the keyword.
                //if ExList.Items[GetListDataIndex]^.Keywords.IndexOf(St) = -1 then begin
                if not ExList.IsInKeywords(St, GetListDataIndex) then begin
                    inc(GetListDataIndex);
                    DoContinue := True;
                    Break;
                end;
            if DoContinue then begin          // Hmm, a GoTo would be easier ......
                DoContinue := False;
                Continue;
            end;
        end;
        break;
    end;
    Proj := ExList.Items[GetListDataIndex]^.EName;
    Cat := ExList.Items[GetListDataIndex]^.Category;
    Path := ExtractFilePath(ExList.Items[GetListDataIndex]^.FFname);
    Keys := '';
    for St in ExList.Items[GetListDataIndex]^.Keywords do
        Keys := Keys + St + ' ';
    inc(GetListDataIndex);
end;

function TExampleData.getCategoryData(const CatList: TStrings): boolean;
var
   P : PExRec;
begin
    if CatList = nil then exit(false);
    CatList.Clear;
    for P in ExList do begin
        if CatList.Indexof(P^.Category) < 0 then
            CatList.Add(P^.Category);
    end;
    Result := True;
end;

// Passed the FFName, a combination of Path and Proj including '.ex-meta'.
function TExampleData.GetDesc(const FFname: string): string;
var
   P : PExRec;
begin
    Result := '';
    for P in ExList do begin
        if (lowercase(P^.FFname) = lowercase(FFname)+MetaFileExt) then begin     // extension must remain lower case
            exit(P^.Desc);
        end;
    end;
    debugln('TExampleData.GetDesc - ERROR did not find Desc for ' + FFname);
    //ExList.DumpList('TExampleData.GetDesc', True);
end;


//  *************   Methods relating to getting REMOTE data  *******************

function TExampleData.DownLoadDir(const FExampDir : string): boolean;
var
   St : string;
   STL : TStringlist;
begin
    STL := TStringList.Create;
    try
        result := ScanRemoteTree(FExampDir, STL);
        for St in STL do begin
            if not DirectoryExistsUTF8(ExampleWorkingDir() + ExtractFileDir(St)) then
                ForceDirectory(ExampleWorkingDir() + ExtractFileDir(St));          // ToDo : but that might fail
            DownLoadFile(St, ExampleWorkingDir() + St);
        end;
    finally
        STL.Free;
    end;
end;


function TExampleData.DownLoadFile(const URL, FullDest : string) : boolean;
var
    St, S : string;
    MemBuffer      : TMemoryStream;
    DecodedStream  : TMemoryStream;
    Decoder        : TBase64DecodingStream;
begin
    if not Downloader(RemoteRepo + 'files/' + HTTPEncode(URL) + '?ref=main', St) then begin
        ErrorMsg := 'TExampleData.ReadMetaFile - download FAILED ' + URL;
        exit(false);
    end;
    S := ExtractFromJSON('content', St, False);               // Bring it back still base64 encoded
    MemBuffer := TMemoryStream.Create;                        // Speedups possible here. BuffStream ?
    try
        MemBuffer.Write(S[1], S.length);
        membuffer.Position := 0;
        DecodedStream := TMemoryStream.Create;
        Decoder       := TBase64DecodingStream.Create(MemBuffer);
        try
            DecodedStream.CopyFrom(Decoder, Decoder.Size);
            DecodedStream.SaveToFile(FullDest);              // Does not appear to benifit from TBufferedFileStream
        except on E: EStreamError do
            ErrorMsg := 'TExampleData.DownLoadFile - Error decoding ' + URL + ' ' + E.Message;
        end;
    finally
        MemBuffer.Free;
        DecodedStream.Free;
        Decoder.Free;
    end;
    result := fileexists(FullDest);
end;


// Passed some json, returns the indicated field IFF its an arrays. The TStringList
// must have been created before being passed.
function TExampleData.ExtractArrayFromJSON(const Field : string; jItem : TJSONData; STL : TStringList) : boolean;
// ToDo : better to handle this with a set or array ?  Once populated, it does not change
var
    JObject : TJSONObject;
    jArray : TJSONArray;
    i : integer;
begin
    result := true;
    try
        JObject := TJSONObject(jItem);                  // does not require a free
        if jObject.Find(Field, JArray) then
            for i := 0 to JArray.count -1 do
                STL.Add(JArray.Items[i].asstring);
    except
        on E:Exception do begin
            Result := False;               // Invalid JSON or content not present
            ErrorMsg := 'Exception while decoding JSON looking for ' + Field;
        end;
    end;
end;

function TExampleData.ExtractFromJSON(const Field, data : string; Base64 : boolean=false) : string;
var
    JData : TJSONData;
    JObject : TJSONObject;
    jStr : TJSONString;
begin
    result := '';
    try
        try
            JData := GetJSON(Data);                         // requires a free
            JObject := TJSONObject(jData);                  // does not require a free
            if jObject.Find(Field, Jstr) then begin
                if Base64 then
                    Result := DecodeStringBase64(jStr.AsString)
                else Result := jStr.AsString;
            end else ErrorMsg := 'Response has no ' + Field + ' field';
        except
            on E:Exception do begin
                        Result := '';               // Invalid JSON or content not present
                        ErrorMsg := 'Exception while decoding JSON looking for ' + Field;
            end;
        end;
    finally
        JData.Free;
    end;
    if Result = '' then debugln('ERROR, we did not find content in ' + Field);
end;

// Returns false if cannot parse passed jItem, thats not necessarily an error,
// Path will not be here if reading individual metadata files.
// If it is an error, ErrorString is set.
function TExampleData.ExtractFromJSON(const Field : string; const jItem : TJSONData;
                                out Res : string; Base64 : boolean=false) : boolean;
var
    JObject : TJSONObject;
    jStr : TJSONString;
begin
    res := '';
    try
        JObject := TJSONObject(jItem);                  // does not require a free
        if jObject.Find(Field, Jstr) then begin
            if Base64 then
                Res := DecodeStringBase64(jStr.AsString)
            else Res := jStr.AsString;
        end else if Field <> 'Path' then begin
            ErrorMsg := 'Response has no ' + Field + ' field';
        end;
    except
        on E:Exception do       // Invalid JSON or content not present
            ErrorMsg := 'Exception while decoding JSON looking for ' + Field;
    end;
    Result := (Res <> '');
end;


// Gets passed the RHS of URL of a metadata file, adds that content to list.
// eg Beginner/Laz_Hello/Laz_Hello.ex-meta
function TExampleData.ReadRemoteMetaFile(URL : string): boolean;
var
    St : string;
    StL : TStringList;
begin
    if not Downloader(RemoteRepo + 'files/' + HTTPEncode(URL) + '?ref=main', St) then begin
        ErrorMsg := 'TExampleData.ReadMetaFile - download FAILED';
        exit(false);
    end;
    StL := TStringList.Create;
    try
        STL.Text := ExtractFromJSON('content', St, True);                    // get 'content' and decode base64
        result := ReadSingleJSON(STL, URL);
        if not Result then
            debugln('Offending remote file is ' + URL);
    finally
        STL.Free;
    end;
end;

//           https://gitlab.com/api/v4/projects/32866275/repository/files/Utility/ExScanner/project1.ico?ref=main
//     curl "https://gitlab.com/api/v4/projects/32866275/repository/files/Utility%2FExScanner%2Fproject1.ico?ref=main"

function TExampleData.ScanRemoteTree(Path : string; STL : TstringList = nil) : boolean;
// warning - recursive function.
var
   St : string;
   jData : TJSONData;
   jObject : TJSONObject;
   jArray : TJSONArray;
   i : integer;
begin
   ScanOneTree(Path, St);
   jData := GetJSON(St);
   jArray:=TJSONArray(jData);
   for i:=0 to jArray.Count-1 do begin
        jObject:= TJSONObject(jArray[i]);
        if jObject.Find('type').AsString = 'tree' then                     // tree and blob are gitlab defines, in the download
            ScanRemoteTree(jObject.Find('path').AsString, STL);
        if (jObject.Find('type').AsString = 'blob') then begin             // A blob is a usable file
            if STL <> nil then
                 STL.add(jObject.Find('path').AsString)
            else                                                           // OK, fill in List mode.
                if (pos(MetaFileExt, jObject.Find('path').AsString) > 0) then begin
                    if pos('master' + MetaFileExt, jObject.Find('path').AsString) < 1 then  // don't do master meta file
                        if STL = Nil then
                            ReadRemoteMetaFile(jObject.Find('path').AsString );
            end;
        end;
   end;
   jArray.Free;
   Result := true;
end;

function TExampleData.ScanOneTree(Path : string; out St : string) : boolean;      // needed
var
   URL : string;
begin
    if Path <> '' then
        URL := RemoteRepo + 'tree?path=' + Path
    else URL := RemoteRepo + 'tree';
    Result := Downloader(URL, St);
end;


function TExampleData.Downloader(URL: string; out SomeString: String): boolean;
var
    Client: TFPHTTPClient;
begin
    // This is a dumb downloader, if you need auth then maybe look at transgithub in tomboy-ng
   // Further, gitlab API seems quite slow, up to a second for an 80K icon file ??
   // curl "https://gitlab.com/api/v4/projects/32866275/repository/files/Utility%2FExScanner%2Fproject1.ico?ref=main"
   // curl does the same thing in a bit over half that time. Hmm....
    Client := TFPHttpClient.Create(nil);
    Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
    Client.AddHeader('Content-Type','application/json; charset=UTF-8');
    Client.AllowRedirect := true;
    SomeString := '';
    try
        try
            SomeString := Client.Get(URL);
        except
            on E: ESocketError do begin
                ErrorMsg := rsExNetWorkError + ' ' + E.Message;
                exit(false);
                end;
            on E: EInOutError do begin
                ErrorMsg := rsExNetWorkError + ' InOut ' + E.Message;
                exit(False);
                end;
            on E: ESSL do begin
                ErrorMsg := rsExNetWorkError + ' SSL ' + E.Message;
                exit(False);
                end;
            on E: Exception do begin        // Following don't need i18n, we check they are there !
                case Client.ResponseStatusCode of
                    401 : ErrorMsg := 'GitHub.Downloader Exception ' + E.Message
                            + ' downloading ' + URL
                            + ' 401 Maybe your Token has expired or password is invalid ??';
                    404 : ErrorMsg := 'GitHub.Downloader Exception ' + E.Message
                            + ' downloading ' + URL + ' 404 File not found ' + URL;
                end;
                exit(false);
                end;
        end;
    finally
        Client.Free;
    end;
    result := true;
end;

end.

