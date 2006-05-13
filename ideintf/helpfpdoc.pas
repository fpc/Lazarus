{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
  Classes, SysUtils, LCLProc, FileUtil, MacroIntf, HelpIntfs, LazHelpIntf,
  LazHelpHTML;

type
  { TFPDocHTMLHelpDatabase }

  TFPDocHTMLHelpDatabase = class(THTMLHelpDatabase)
  public
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;
  

implementation

{ TFPDocHTMLHelpDatabase }

function TFPDocHTMLHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem;
  var ErrMsg: string): TShowHelpResult;
var
  ContextList: TPascalHelpContextList;
  UnitName: String;
  URL: String;
  TheBaseURL: String;
  Filename: String;
  i: Integer;
  Context: String;
  p: LongInt;
begin
  if (Query is THelpQueryPascalContexts)
  and (QueryItem is TPascalHelpContextList) then begin
    // a pascal context query
    ContextList:=TPascalHelpContextList(QueryItem);
    if (ContextList.Count>0) and (ContextList.List[0].Descriptor=pihcFilename)
    then begin
      // extract unit filename
      UnitName:=lowercase(ExtractFileNameOnly(ContextList.List[0].Context));
      DebugLn('TFPDocHTMLHelpDatabase.ShowHelp A Unitname=',Unitname,' NewNode.HelpType=',dbgs(ord(NewNode.HelpType)),' NewNode.Title=',NewNode.Title,' NewNode.URL=',NewNode.URL);
      if UnitName<>'' then begin

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
        DebugLn('TFPDocHTMLHelpDatabase.ShowHelp Filename="',Filename,'" UnitName="',UnitName,'"');

        // FPDoc Html always has .html as extension
        Filename:=UnitName+'/'+Filename+'html';

        TheBaseURL:='';
        if NewNode.URLValid then begin
          // the node has an URL => use only the path
          TheBaseURL:=NewNode.URL;
          //debugln('A TheBaseURL=',TheBaseURL);
          if (HelpDatabases<>nil) then
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

