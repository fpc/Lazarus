{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Help database for FPDoc.
    
  ToDo:
    - localization. Example: german, LANG=de, uses path unitname/de/ instead
      of unitname/
}
unit HelpFPDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils,
  LazFileUtils,
  // LCL
  LCLProc, HelpIntfs, LazHelpIntf, LazHelpHTML,
  // BuildIntf
  MacroIntf;

type
  { TFPDocHTMLHelpDatabase }

  TFPDocHTMLHelpDatabase = class(THTMLHelpDatabase)
  public
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;
  
function RegisterFPDocHTMLHelpForPackage(const DBName, DBTitle, BaseURL,
  PackageName: string; AdditionalDirectories: string = ''
  ): TFPDocHTMLHelpDatabase;

implementation

function RegisterFPDocHTMLHelpForPackage(const DBName, DBTitle, BaseURL,
  PackageName: string; AdditionalDirectories: string
  ): TFPDocHTMLHelpDatabase;
{ It registers help for all *.pas and *.pp files of the package source
  directory (where the .lpk file is) and all sub directories.

  DBName:      the database ID. The IDE will change it if there is already a DB
               with this name. Use the package name.
  DBTitle:     the localized title shown in IDE dialogs.
  BaseURL:     all paths are relative to this URL.
  PackageName: the name of the package.
  AdditionalDirectories: additional source directories separated by semicolon.
                         Paths must be relative to the package source directory.
}
var
  FPDocNode: THelpNode;
  p: LongInt;
  Dir: String;
  NewDirHelp: THelpDBISourceDirectory;
begin
  // create help database
  Result:=TFPDocHTMLHelpDatabase(
          HelpDatabases.CreateHelpDatabase(DBName,TFPDocHTMLHelpDatabase,true));
  Result.DefaultBaseURL:=BaseURL;
  // register TOC (table of contents)
  FPDocNode:=THelpNode.CreateURL(Result,DBTitle,'file://index.html');
  Result.TOCNode:=THelpNode.Create(Result,FPDocNode);
  // register fpdoc item
  Result.RegisterItem(THelpDBISourceDirectory.Create(FPDocNode,
                      '$PkgDir('+PackageName+')',
                      '*.pp;*.pas', // this entry is for pascal files
                      true // for this source directory and all sub directories.
                      ));
  // register additional source directories
  while AdditionalDirectories<>'' do begin
    p:=System.Pos(';',AdditionalDirectories);
    if p<1 then p:=length(AdditionalDirectories)+1;
    Dir:=Trim(copy(AdditionalDirectories,1,p-1));
    if Dir<>'' then begin
      FPDocNode:=THelpNode.CreateURL(Result,DBTitle+' '+Dir,'file://index.html');
      NewDirHelp:=THelpDBISourceDirectory.Create(FPDocNode,
                             '$PkgDir('+PackageName+')'+Dir,'*.pp;*.pas',false);
      Result.RegisterItem(NewDirHelp);
      //DebugLn(['RegisterFPDocHTMLHelpForPackage NewDirHelp.Filename="',NewDirHelp.Filename,'"']);
    end;
    System.Delete(AdditionalDirectories,1,p);
  end;
end;

{ TFPDocHTMLHelpDatabase }

function TFPDocHTMLHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem;
  var ErrMsg: string): TShowHelpResult;
var
  ContextList: TPascalHelpContextList;
  AUnitName: String;
  URL: String;
  TheBaseURL: String;
  Filename: String;
  i: Integer;
  Context: String;
  p: LongInt;
begin
  if (Query is THelpQueryPascalContexts)
  and (QueryItem is TPascalHelpContextList) then begin
    // a Pascal context query
    ContextList:=TPascalHelpContextList(QueryItem);
    if (ContextList.Count>0) and (ContextList.List[0].Descriptor=pihcFilename)
    then begin
      // extract unit filename
      AUnitName:=lowercase(ExtractFileNameOnly(ContextList.List[0].Context));
      DebugLn('TFPDocHTMLHelpDatabase.ShowHelp A Unitname=',AUnitname,' NewNode.HelpType=',dbgs(ord(NewNode.HelpType)),' NewNode.Title=',NewNode.Title,' NewNode.URL=',NewNode.URL);
      if AUnitName<>'' then begin

        // create FPDoc context
        Filename:='';
        for i:=0 to ContextList.Count-1 do begin
          Context:=lowercase(ContextList.List[i].Context);
          case ContextList.List[i].Descriptor of
          
          pihcProperty,pihcVariable,pihcType,pihcConst:
            Filename:=Filename+Context+'.';
            
          pihcProcedure: begin
              // chomp parameters  ToDo: overloaded procs
              p:=System.Pos('(',Context);
              if p>0 then
                Context:=copy(Context,1,p-1);
              Filename:=Filename+Context+'.';
            end;

          end;
        end;
        
        // default is index.html
        if Filename='' then Filename:='index.';
        DebugLn('TFPDocHTMLHelpDatabase.ShowHelp Filename="',Filename,'" UnitName="',AUnitName,'"');

        // FPDoc Html always has .html as extension
        Filename:=AUnitName+'/'+Filename+'html';

        TheBaseURL:='';
        if NewNode.URLValid then begin
          // the node has an URL => use only the path
          TheBaseURL:=NewNode.URL;
          //debugln('A TheBaseURL=',TheBaseURL);
          if (IDEMacros<>nil) then
            IDEMacros.SubstituteMacros(TheBaseURL);
          //debugln('B TheBaseURL=',TheBaseURL);
          TheBaseURL:=ExtractURLDirectory(TheBaseURL);
          //debugln('C TheBaseURL=',TheBaseURL);
          DebugLn('TFPDocHTMLHelpDatabase.ShowHelp BaseURL of Node.URL="',TheBaseURL,'"');
        end;

        if TheBaseURL='' then begin
          TheBaseURL:=GetEffectiveBaseURL;
          DebugLn('TFPDocHTMLHelpDatabase.ShowHelp GetEffectiveBaseURL="',TheBaseURL,'"');
        end;

        // show URL
        if TheBaseURL<>'' then
          URL:=TheBaseURL+Filename
        else
          URL:=FilenameToURL(Filename);
        Result:=ShowURL(TrimUrl(URL),NewNode.Title,ErrMsg);
        exit;
      end;
    end;
  end;
  // otherwise use default
  Result:=inherited ShowHelp(Query, BaseNode, NewNode, QueryItem, ErrMsg);
end;

end.

