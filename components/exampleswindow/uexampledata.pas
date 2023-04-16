unit uExampleData;

{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit is the backend that provides a List that contains details of Lazarus
Example Projects. It might get its data from one of two different places,

* The LazarusDir, thats the SRC dir, examples shipped with Lazarus.
* Any Packages installed in Lazarus, looks in <pcp>staticpackages.inc and in
  <pcp>packagefiles.xml. staticpackages.inc tells us its currently installed
  but need to check in packagefiles.xml to find if its (a) a User install and
  (b) if it has an example directory declared <ExamplesDirectory Value="../demo"/>

This list can be used to populate the Lazarus Examples Window or used during the
markup of existing Lazarus Projects. The unit is used by the Lazarus Package and
a simple tool used to manage the meta data files.

As of April 12, 2023, this unit no longer includes code to get and manage example
project in a remote git repo. As we now do cover third party project, a remote
"lazarus src only" example repo sounds out of scope.

}

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

{X$define SHOW_DEBUG}      // ToDo : remove this

interface

uses
    Classes, SysUtils, fpjson, jsonparser, jsonscanner, // these are the FPC JSON tools
    httpprotocol,  // for http encoding
    base64,
    Laz2_XMLRead, Laz2_DOM, LazFileUtils, FileUtil, LazLoggerBase
    {$ifndef EXTESTMODE}
    , IDEOptionsIntf
    {$endif};

const
    MetaFileExt = '.ex-meta';              // Extension of meta files.

type
    TExampleDataSource = (FromGitlabTree,  // Read all remote project meta files                   not used
                          FromLocalTree,   // Read all local Git project meta files                not used
                          FromThirdParty,  // Packages listed in first block of packagefiles.xml
                          FromCacheFile,   // Load data from Local Cache File                      not used
                          FromLazSrcTree); // Searches the Lazarus Src Tree, eg ~/examples; ~/components

    PExRec=^TExRec;
    TExRec = record
          EName    : string;      // CamelCase version of the example name, filenameonly of metadata file.
          Category : string;      // eg Beginner, General, ThirdParty (read from remote data)
          Keywords : TStringList; // a list of (possibly multi-word) words, nil acceptable
          FFName   : string;      // An Absolute Path and filename of meta file in its original position, not copy.
          Desc     : string;      // 1..many lines of description
          ThirdParty : boolean;   // False if examples are shipped in Lazarus Src.
    end;

    { TExampleList }

    TExampleList = class(TFPList)
        private

            procedure DumpList(wherefrom: string; ShowDesc: boolean = false);
            function Get(Index: integer): PExRec;

         public
             constructor Create();
             destructor Destroy; override;
                                         // Public - Puts new entry in List, Keys may be Nil
             function InsertData(Cat, Desc, FFName, AName: string; Keys: TStringList; IsTP: boolean=true): boolean;
             function Find(const FFname: string): PExRec;
             function AsJSON(Index: integer): string;
                                        // Ret T if St is in Keywords at AnIndex, not necessarily equal to.
             function IsInKeywords(St : string; AnIndex : integer) : boolean;
             property Items[Index: integer]: PExRec read Get; default;

    end;


    { TExampleData }

    TExampleData = class
        private
            ErrorString : String;
            GetListDataIndex : integer;

                                // Passed full file name of the packagesfiles.xml file in PCP, returns
                                // with the list filled with paths to some directory above the package
                                // lpk file being a suitable place to start searching for Examples.
            procedure CollectThirdPartyPackages(PkgFilesXML: String; AList, SList: TStrings);
            function GetTheRecord(const FFname: string): PExRec;
                                // Returns true if it has altered FullPkgFileName to where we can expect to find Examples
            function GetThirdPartyDir(var FullPkgFileName: string): boolean;
            procedure ScanLazarusSrc;
                                // Triggers a search of installed Third Party packages. Iterates over packagefiles.xml
                                // and puts any potential paths to example directories in a list. Then iterates over
                                // that list scanning blow each path looking for example directories (ie ones with a
                                // ex_meta file). Any it finds are added to ExList.
            procedure ScanThirdPartyPkg;
                                //function EscJSON(InStr: string): string;
            function ExtractArrayFromJSON(const Field: string; jItem: TJSONData; STL: TStringList): boolean;
                                // Passed a json block, returns the indicated field, cannot handle arrays.
            function ExtractFromJSON(const Field: string; const jItem: TJSONData; out Res: string; Base64: boolean = false): boolean;
                                // Receives a pretested JSON (not just a field) containing metadata of an Example
                                // Returns false if data missing, drops msg to console about bad field.
                                // Path may be relative or absolute (ie starting with '/' or '\'). Ones without
                                // a leading slash are remote, ie gitlab. Ones with a slash should be resolvable
                                // locally. Note when indexing a local git tree, relative must be used, ie top of
                                // git tree. In this mode of course, the entry will not be resolvable locally.
            function InsertJSONData(jItem: TJSONData; FFName: string; IsTP : boolean; AName: string = ''): boolean;
                                // Gets passed a block of json, wrapped in {} containing several fields relating
                                // one example project. Path is ready to use in the List. Not suited to json
                                // With an internal Path field (ie master.ex-meta)
            function ReadSingleJSON(FileContent: TStringList; IsTP : boolean; PathToStore: string = ''): boolean;
                                // Scans local tree below 'Path' looking for any likely Example Metadata files.
                                // For each, it loads content into a StringList and passes it to an Insert method.
                                // Path should be absolute and points to an 'Examples' or 'Demo' dir in a Third Party
                                // project (where it may find several project directories below).
            function ScanLocalTree(Path: string): boolean;
            procedure fSetErrorString(Er : string);
                                // Passed a full path to a metadata file, will open and process it.
            function UseMetaDataFile(FFName: string; IsThirdParty : Boolean): boolean;
            function DoesNameExist(AName : string) : boolean;

        public
            ExList : TExampleList;
            CatList : TStringList;      // A list of the categories we found in our examples, used by GUI.
            LazConfigDir : string;      // Where Lazarus keeps it config. Comes from uLaz_Examples, uIntf, LazarusIDE.GetPrimaryConfigPath
            ExamplesHome : string;      // dir above examples_working_dir where we copy examples to, set by uintf.pas, usually <lazConf>/
            LazSrcDir    : string;      // Laz dir where, eg ~/examples lives
            KeyFilter : string;         // A list of words, possibly grouped by " to filter Keywords
            CatFilter  : string;        // A string that may contain 0 to n words, each word being a category as filtered by GetListData()

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
            function GetListData(out Proj, Cat, Path, Keys: string; out Index: integer;
              GetFirst: boolean; KeyList: TStringList=nil): boolean;
                                // Passed a created TStrings that it clears and fills in with all know categories
            function getCategoryData(const ACatList : TStrings) : boolean;
            constructor Create;
                                // This is the main "do it" call for this unit. It populates the list from the
                                // indicated source and sorts it on a pre determined category.
            procedure LoadExData(DataSource: TExampleDataSource);
                                // Passed a index to the ExList.
                                // Returns a FullFilename to a lpi file of an Example, it might be the original one
                                // in a ThirdParty Package or the one copied to the Example Working Area.
                                // Ret '' if the lpi file is not found (because the project has not been copied or
                                // because it somehow lacks an lpi file).
            function GetProjectFile(ExIndex: integer): string;
                                // Returns true if the item refered to has an .lpi file in either its original
                                // directory (ThirdParty) or in the copy in ExampleWorkArea (Lazarus SRC).
            function IsValidProject(ExIndex: integer): boolean;
            destructor Destroy; override;
            procedure DumpExData();
            function Count : integer;
            property ErrorMsg : string read ErrorString write FSetErrorString;
            class function EscJSON(InStr: string): string;
    end;


implementation

uses
    uConst {$ifdef EXTESTMODE}, Main_Examples{$endif} ;


// =============================================================================
//                T   E X A M P L E    L I S T
//==============================================================================

function TExampleList.Get(Index: integer): PExRec;
begin
    Result := PExRec(inherited get(Index));
end;

function TExampleList.InsertData(Cat, Desc, FFName, AName : string; Keys: TStringList; IsTP : boolean = true): boolean;
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
    ExRecP^.ThirdParty := IsTP;
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
        DebugLn('----- List - FFName=[' + Items[i]^.FFName +'] Cat=[' + Items[i]^.Category
                + '] EName=' + Items[i]^.EName
                + '] ThirdParty=' + booltostr(Items[i]^.ThirdParty, True));
//                + '] Key=[' + Items[i]^.Keywords.Text + ']');
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


procedure TExampleData.CollectThirdPartyPackages(PkgFilesXML: String; AList, SList: TStrings);
var
    doc: TXMLDocument;
    userPkgLinks, pkgNode: TDOMNode;
    NameNode, FileNameNode: TDOMNode;
    FileNameAttr, NameAttr : TDOMNode;
    St : String;
begin
    if not FileExists(PkgFilesXML) then
        exit;
    ReadXMLFile(doc, PkgFilesXML);
    try
        userPkgLinks := doc.DocumentElement.FindNode('UserPkgLinks');
        if userPkgLinks = nil then
            exit;
        pkgNode := userPkgLinks.FirstChild;
        while pkgNode <> nil do begin
            NameNode := pkgNode.FindNode('Name');
            FileNameNode  := pkgNode.FindNode('Filename');
            if  not ((NameNode = nil) or (FileNameNode = nil)) then begin
                FileNameAttr := FileNameNode.Attributes.GetNamedItem('Value');
                NameAttr := NameNode.Attributes.GetNamedItem('Value');
                if not ((FileNameAttr = nil) or (NameAttr = nil)) then begin
                    St := NameAttr.Nodevalue;
                    if SList.IndexOf(St) > -1 then begin
                        St := filenameAttr.Nodevalue;
                        ForcePathDelims(St);            // ExtractFileDir has problems with unexpected pathdelim....
                        if GetThirdPartyDir(St) then begin
                            {$ifdef SHOW_DEBUG}debugln('CollectThirdPartyPackages adding St [' + St + ']');{$endif}
                            AList.Add(St);
                        end;
                    end;
                end;
            end;
            pkgNode := pkgNode.NextSibling;
        end;
    finally
        doc.Free;
    end;
end;


{ We look for a tag like <ExampleDirectory="../."/> just below <Package....
  If we find it, we use that, relative to the actual path of the LPK file to
  determine where we should, later, look for Examples.}

function TExampleData.GetThirdPartyDir(var FullPkgFileName: string): boolean;
var
    doc: TXMLDocument;
    NodeA, NodeB: TDOMNode;
    ADir : string = 'INVALID';
begin
    Result := true;
    {$ifdef SHOW_DEBUG}debugln('TExampleData.GetThirdParty - looking at [' + FullPkgFileName + ']');{$endif}
    if not FileExists(FullPkgFileName) then
        exit(false);                                        // only real error return code
    try
        ReadXMLFile(doc, FullPkgFileName);
    except on E: Exception do begin
            debugln('Warning :  [TExampleData.GetThirdPartyDir] XML Error : ' + E.Message);
            if assigned(doc) then
                doc.free;
            exit(false);
        end;
    end;
    try
        FullPkgFileName := ExtractFileDir(FullPkgFileName); // Remove the LPK name, might be best we can do.
        NodeB := doc.DocumentElement.FindNode('Package');
        if NodeB = nil then exit;
        NodeA := NodeB.FindNode('ExamplesDirectory');
        if NodeA <> nil then begin
            {$ifdef SHOW_DEBUG} debugln('ExampleDir Mode');{$endif}
            NodeB := NodeA.Attributes.GetNamedItem('Value');
            if NodeB <> nil then                            // Leave existing path in FullPkgFileName, ie assumes LPK file is level or above examples
                ADir := NodeB.NodeValue;                    // maybe something like eg ../../Examples
        end;
        {$ifdef SHOW_DEBUG}
        debugln('TExampleData.GetThirdParty - ADir=[' + ADir + '] and FullPkgFileName=[' + FullPkgFileName +']');
        {$endif}
        if ADir = 'INVALID' then
            exit(False)
        else FullPkgFileName := ExpandFileName(appendPathDelim(FullPkgFileName) + ADir);
        {$ifdef SHOW_DEBUG}
        debugln('GetThirdParty - FullPkgFileName=[' + FullPkgFileName +']');
        {$endif}
    finally
        doc.free;
    end;
end;

(*       An LPK file might look like this -
<?xml version="1.0" encoding="UTF-8"?>
<CONFIG>
  <Package Version="4">
    <PathDelim Value="\"/>
    <Name Value="my_great_package"/>
    <Type Value="RunAndDesignTime"/>
    <Author Value="David Bannon"/>
    <ExampleDirectory Value="../Examples/">     // Maybe not there ....
    .....
*)

procedure TExampleData.ScanThirdPartyPkg();
var
    STL : TStringList;      // The list we collect potential example directories in.
    SSlist : TStringList;   // The list of installed packages from staticpackages.inc
    i : integer;
    St : string;
begin
    if not FileExists(LazConfigDir + 'staticpackages.inc') then
        exit;               // No third party packages installed yet, that was easy !
    SSList := TStringList.Create;
//    SSList.Sorted := true;              // Don't sort 'cos we need edit each line below :-)
    SSList.Duplicates := dupIgnore;
    SSlist.LoadFromFile(LazConfigDir + 'staticpackages.inc');
    if SSList.Count < 1 then begin               // an empty file, unlikely
        SSList.Free;
        exit;
    end;
    for i := 0 to SSList.Count -1 do begin
        if SSList[i].EndsWith(',') then begin
            St := SSList[i];
            delete(St, length(St), 1);
            SSList[i] := St;
        end;
    end;
    STL := TStringList.Create;
    STL.Sorted := true;
    STL.Duplicates := dupIgnore;
    try
        CollectThirdPartyPackages(LazConfigDir + 'packagefiles.xml', STL, SSList);
        for i := 0 to Stl.Count -1 do begin
            ScanLocalTree(STL[i]);
            {$ifdef SHOW_DEBUG}
            debugln('ScanThirdPartyPkg - Scanning ' + STL[i]);
            {$endif}
        end;
    finally
        STL.Free;
        SSList.Free;
    end;
    //ExList.DumpList('After ScanThirdPartyPkg');
end;


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
    Debugln('Warning : [TExampleData]' + ErrorString);
end;

function TExampleData.ExampleWorkingDir() : string;
begin
    result := AppendPathDelim(ExamplesHome) + cExamplesDir + PathDelim ;
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
function TExampleData.InsertJSONData(jItem : TJSONData; FFName : string; IsTP : boolean; AName : string = ''): boolean;
var
    Cat, Desc, AnotherName : String;
    // index : integer;
    KeyWords : TStringList;
begin
    Result := False;
    if not ExtractFromJSON('Category', jItem, Cat) then        // An empty field here is acceptable but undesirable.
        debugln('Hint: (Lazarus) [TExampleData.InsertJSONData] Metadata file has no category : ' + FFName);
    if not ExtractFromJSON('Description', jItem, Desc) then
        debugln('Hint: (Lazarus) [TExampleData.InsertJSONData] Metadata file has no description : ' + FFName);
    {$ifdef WINDOWS}
    Desc := Desc.Replace(#10, #13#10, [rfReplaceAll]);
    {$endif}
    KeyWords := TStringList.Create;
    ExtractArrayFromJSON('Keywords', jItem, Keywords);
    if AName <> '' then
        AnotherName := AName
    else
        if not ExtractFromJSON('Name', jItem, AnotherName) then
            AnotherName := '';
    if DoesNameExist(AnotherName) then
        debugln('Warning: [TExampleData.InsertJSONData] duplicate Example Name found = '
            + AnotherName + ' ' + FFName)
    else begin
        Result := ExList.InsertData(Cat, Desc, FFName, AnotherName, KeyWords, IsTP);
        if Result then
            if CatList.Indexof(Cat) < 0 then
                CatList.Add(Cat);
    end;
    if not Result then KeyWords.Free;              // false means its not gone into list so our responsibility to free
end;

// Opens the examples.txt file in Examples dir of Lazarus Src, reads each line
// as a ex-meta file, adds that example to List.
procedure TExampleData.ScanLazarusSrc();
var
    LazExList : TStringList;
    FFName, St : string;
begin
    FFName := LazSrcDir + 'examples' + PathDelim + 'examples.txt';
    if not fileexists(FFName) then begin
        debugln('Warning [TExampleData.ScanLazarusSrc] : ' + FFName + ' does not exist');
        exit;
    end;
    LazExList := TStringList. Create;
    LazExList.LoadFromFile(FFName);
    for St in LazExList do
        UseMetaDataFile(ExpandFileName(SetDirSeparators(LazSrcDir + St)), False);
    LazExList.Free;
end;

function TExampleData.UseMetaDataFile(FFName : string; IsThirdParty : Boolean) : boolean;
var
    FileContent : TStringList;
begin
    FileContent := TStringList.Create;
    try try
        FileContent.LoadFromFile(FFName);                   // That is contents of one individual metadata file
        except on E: Exception do
               debugln('Warning : [TExampleData.UseMetaDataFile] ' + E.message);
        end;
        Result := ReadSingleJSON(FileContent, IsThirdParty, FFName);      // Calls InsertJSONData() if successful
        if not Result then begin
            debugln('Warning : [TExampleData.UseMetaDataFile] Bad Example Meta File : ' + FFName);
            debugln(ErrorMsg);
            exit;
        end;
    finally
        FileContent.Free;
    end;
end;

function TExampleData.ScanLocalTree(Path : string) : boolean;
var
   STL : TStringList = nil;
   St : string;
begin
    STL := FindAllFiles(Path, '*' + MetaFileExt, True);
    try
        for St in STL do begin
            if St.EndsWith(MetaFileExt) then
                UseMetaDataFile(ExpandFileName(SetDirSeparators(St)), True);
        end;
    finally
        STL.Free;
    end;
end;


function TExampleData.ReadSingleJSON(FileContent : TStringList; IsTP : boolean; PathToStore : string = '') : boolean;
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
                    ErrorMsg := 'Error in EJSONParser- invalid JSON in ' + PathToStore
                                                 + ' ' + E.Message;
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
                on E: EScannerError do begin                                // Thats in jsonscanner unit, Must doc on Wiki !!!
                    ErrorMsg := 'Error in EScanner- invalid JSON in ' + PathToStore     // this is typically a single \
                                                 + ' ' + E.Message;
                    jData := Nil;                                           // Appears nothing is allocated if error  ?
                    exit(false);
                end;
            end;
            if  TJSONObject(jItem).Count = 0 then begin
                debugln('WARNING : [TExampleData.ReadSingleJSON] - file ' + PathToStore + ' does not contain suitable JSON : ');
                exit(false);
            end;
            InsertJSONData(jItem, PathToStore, IsTP, TJSONObject(jData).Names[0]);
        finally
            jData.free;
        end;
    end;
end;

destructor TExampleData.Destroy;
begin
    CatList.Free;
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
    CatList  := TStringList.Create;
    LazSrcDir := IDEEnvironmentOptions.GetParsedLazarusDirectory;
end;


procedure TExampleData.LoadExData(DataSource: TExampleDataSource);
begin
    // If we are loading the data from either the remote gitlab tree or a local
    // git tree, we save the master file.
    if not DirectoryExists(ExampleWorkingDir()) then
        if not ForceDirectory(ExampleWorkingDir()) then exit;
    case DataSource of
        FromLazSrcTree : ScanLazarusSrc();          // get 'built in' examples from Lazarus
        FromThirdParty : ScanThirdPartyPkg();       // Get, eg, any OPM Examples or ones manually installed by user.
    end;
    ExList.Sort(@CategorySorter);
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

function TExampleData.GetListData(out Proj, Cat, Path, Keys : string; out Index : integer;
                        GetFirst: boolean; KeyList : TStringList = nil): boolean;
// ToDo : this would be a lot better just returning with the Index and letting calling process use Ex.ExList[i]^.xxxx
var
   St : string;
   DoContinue : boolean = false;
begin
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
        Assert(Assigned(KeyList), 'TExampleData.GetListData: KeyList=Nil');
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
        break;
    end;
    Proj := ExList.Items[GetListDataIndex]^.EName;
    Cat := ExList.Items[GetListDataIndex]^.Category;
    Path := ExtractFilePath(ExList.Items[GetListDataIndex]^.FFname);
    Index := GetListDataIndex;
    Keys := '';
    for St in ExList.Items[GetListDataIndex]^.Keywords do
        Keys := Keys + St + ' ';
    inc(GetListDataIndex);
end;

function TExampleData.getCategoryData(const ACatList: TStrings): boolean;
var
   P : PExRec;
begin
    if ACatList = nil then exit(false);
    ACatList.Clear;
    for P in ExList do begin
        if ACatList.Indexof(P^.Category) < 0 then
            ACatList.Add(P^.Category);
    end;
    Result := True;
end;

function TExampleData.IsValidProject(ExIndex : integer) : boolean;
var
    CheckPath : string;
begin
     CheckPath :=  GetProjectFile(ExIndex);
     result := CheckPath <> '';
end;

function TExampleData.GetProjectFile(ExIndex : integer) : string;
var
    CheckPath : string;
    Info : TSearchRec;
begin
    Result := '';
    if not ExList[ExIndex]^.ThirdParty then
        CheckPath := ExampleWorkingDir + lowercase(ExList[ExIndex]^.EName) + PathDelim
    else
        CheckPath := ExtractFilePath(ExList[ExIndex]^.FFName);        // Remove metadata file name
    {$ifdef SHOW_DEBUG} debugln('TExampleData.GetProjectFile Checking ' + CheckPath + ' for lpi file');{$endif}
    if FindFirst(CheckPath + '*.lpi', faAnyFile, Info) = 0 then begin
        Result := CheckPath + Info.Name;
    end;
    if Result = '' then
        debugln('Hint : [TExampleData.GetProjectFile] - ' + CheckPath + ' does not contain an LPI file');
    FindClose(Info);
end;


function TExampleData.GetTheRecord(const FFname: string) : PExRec;
begin
    for Result in ExList do begin
        if (lowercase(Result^.FFname) = lowercase(FFname)+MetaFileExt) then begin     // extension must remain lower case
            exit;
        end;
    end;
    Result := Nil;
end;


//  *************   Methods relating to getting REMOTE data  *******************

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


end.
