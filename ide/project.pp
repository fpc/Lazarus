{
 /***************************************************************************
                         project.pp  -  project utility class file
                             -------------------
                   TProject is responsible for managing a complete project.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
unit project;

{$mode objfpc}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  Classes, SysUtils, xmlcfg, lazconf, compileroptions, filectrl;

type
  TUnitInfo = class(TObject)
  private
    { Variables }
    fAutoCreate: Boolean;
    fBookmarks: TList;
    fBreakpoints: TList;
    fCursorPos: LongInt;
    fFilename: String;
    fLoaded:  Boolean;
    fReadOnly:  Boolean;
    fSource: TStrings;
    fSyntaxHighlighter: String;
    fUnitName: String;
  public
    constructor Create;
    destructor Destroy; override;

    function ReadUnit: Boolean;
    function WriteUnit: Boolean;
    procedure Clear;

    { Properties }
    property AutoCreate: Boolean read fAutoCreate write fAutoCreate;
    property Bookmarks: TList read fBookmarks write fBookmarks;
    property Breakpoints: TList read fBreakpoints write fBreakpoints;
    property CursorPos: LongInt read fCursorPos write fCursorPos;
    property Filename: String read fFilename write fFilename;
    property Loaded: Boolean read fLoaded write fLoaded;
    property ReadOnly: Boolean read fReadOnly write fReadOnly;
    property Source: TStrings read fSource write fSource;
    property SyntaxHighlighter: String read fSyntaxHighlighter write fSyntaxHighlighter;
    property UnitName: String read fUnitName write fUnitName;
  end;

  TProject = class(TObject)
  private
    xmlcfg: TXMLConfig;

    { Variables }
    fAliases: String;
    fCompilerOptions: TCompilerOptions;
    fIconPath: String;
    fLoaded:  Boolean;
    fMainUnit: String;
    fOutputDirectory: String;
    fProjectFile: String;
    fProjectInfoFile: String;
    fTargetFileExt: String;
    fSource: TStrings;
    fTitle: String;
    fUnitList: TList;
    fUnitNameList: String;
    fUnitOutputDirectory: String;

    { Functions }
    function GetUnitList: TList;
    function GetXMLConfigPath: String;

    { Procedures }
    procedure SetUnitList(AList: TList);
  public
    constructor Create;
    destructor Destroy; override;

    { Functions }
    function ReadProject: Boolean;
    function WriteProject: Boolean;

    { Procedures }
    procedure AddUnit(AUnit: TUnitInfo);
    procedure RemoveUnit(AUnitName: String);
    procedure Clear;

    { Properties }
    property Aliases: String read fAliases write fAliases;
    property CompilerOptions: TCompilerOptions read fCompilerOptions write fCompilerOptions;
    property IconPath: String read fIconPath write fIconPath;
    property Loaded: Boolean read fLoaded write fLoaded;
    property MainUnit: String read fMainUnit write fMainUnit;
    property OutputDirectory: String read fOutputDirectory write fOutputDirectory;
    property ProjectFile: String read fProjectFile write fProjectFile;
    property ProjectInfoFile: String read fProjectInfoFile write fProjectInfoFile;
    property Source: TStrings read fSource write fSource;
    property TargetFileExt: String read fTargetFileExt write fTargetFileExt;
    property Title: String read fTitle write fTitle;
    property UnitList: TList read GetUnitList write SetUnitList;
    property UnitNameList: String read fUnitNameList write fUnitNameList;
    property UnitOutputDirectory: String read fUnitOutputDirectory write fUnitOutputDirectory;
  end;

var
  Project1: TProject;
  
implementation


{------------------------------------------------------------------------------
                              TUnitInfo Class
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  TUnitInfo Constructor
 ------------------------------------------------------------------------------}
constructor TUnitInfo.Create;
begin
  inherited Create;
  Assert(False, 'Project Unit Info Class Created');

  fAutoCreate := true;
  fBookmarks := TList.Create;
  fBreakpoints := TList.Create;
  fCursorPos := 0;
  fFilename := '';
  fLoaded := false;
  fReadOnly :=  false;
  fSource := TStringList.Create;
  fSyntaxHighlighter := 'freepascal';
  fUnitName := '';
end;

{------------------------------------------------------------------------------
  TUnitInfo Destructor
 ------------------------------------------------------------------------------}
destructor TUnitInfo.Destroy;
begin
  fBookmarks.Free;
  fBreakpoints.Free;
  fSource.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TUnitInfo WriteUnit
 ------------------------------------------------------------------------------}
function TUnitInfo.WriteUnit: Boolean;
begin
  Result := true;

  try
    // Save unit source code
    Source.SaveToFile(Filename);
  except
    Result := false;
  end;
end;

{------------------------------------------------------------------------------
  TUnitInfo ReadUnit
 ------------------------------------------------------------------------------}
function TUnitInfo.ReadUnit: Boolean;
begin
  Result := true;

  try
    // Load unit source code
    Source.LoadFromFile(Filename);
    Loaded := true;
  except
    Result := false;
  end;
end;

{------------------------------------------------------------------------------
  TUnitInfo Clear
 ------------------------------------------------------------------------------}
procedure TUnitInfo.Clear;
begin
  fAutoCreate := true;
  fBookmarks.Clear;
  fBreakpoints.Clear;
  fCursorPos := 0;
  fFilename := '';
  fLoaded := false;
  fReadOnly :=  false;
  fSource.Clear;
  fSyntaxHighlighter := 'freepascal';
  fUnitName := '';
end;


{------------------------------------------------------------------------------
                              TProject Class
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  TProject Constructor
 ------------------------------------------------------------------------------}
constructor TProject.Create;
begin
  inherited Create;
  Assert(False, 'Trace:Project Class Created');

  xmlcfg := nil;

  fAliases := '';
  fCompilerOptions := TCompilerOptions.Create;
  fIconPath := '';
  fMainUnit := '';
  fOutputDirectory := '.';
  fProjectFile := '';
  fProjectInfoFile := '';
  fSource := TStringList.Create;
  fTargetFileExt := '';
  fTitle := '';
  fUnitList := TList.Create;
  fUnitNameList := '';
  fUnitOutputDirectory := '.';
end;

{------------------------------------------------------------------------------
  TProject Destructor
 ------------------------------------------------------------------------------}
destructor TProject.Destroy;
begin
  if (xmlcfg <> nil) then xmlcfg.Free;
  fSource.Free;
  fUnitList.Free;
  fCompilerOptions.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TProject WriteProject
 ------------------------------------------------------------------------------}
function TProject.WriteProject: Boolean;
var
  confPath: String;
  i: Integer;
begin
  Result := false;

  confPath := GetXMLConfigPath;
  if (confPath = '') then exit;

  xmlcfg := TXMLConfig.Create(SetDirSeparators(confPath));

  try
    xmlcfg.SetValue('ProjectOptions/General/ProjectFile/Value', ProjectFile);
    xmlcfg.SetValue('ProjectOptions/General/MainUnit/Value', MainUnit);
    xmlcfg.SetValue('ProjectOptions/General/Aliases/Value', Aliases);
    xmlcfg.SetValue('ProjectOptions/General/IconPath/Value', IconPath);
    xmlcfg.SetValue('ProjectOptions/General/TargetFileExt/Value', TargetFileExt);
    xmlcfg.SetValue('ProjectOptions/General/Title/Value', Title);
    xmlcfg.SetValue('ProjectOptions/General/OutputDirectory/Value', OutputDirectory);
    xmlcfg.SetValue('ProjectOptions/General/UnitOutputDirectory/Value', UnitOutputDirectory);

    // Save project source code
    {TODO:
       Check OutputDirectory to see if last character is /. If not, add one. If so, leave it as is
       Add code to check to ensure we have a good ProjectFile (not nil)
       Add code to split filename and put output directory into OutputDirectory property and 
            filename into ProjectFile property
    }
    Source.SaveToFile(ProjectFile);

    // Set options for each Unit
    if (UnitList <> nil) then
    begin
      for i := 0 to UnitList.Count - 1 do
      begin
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/UnitName/Value', TUnitInfo(UnitList.Items[i]).UnitName);
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/Filename/Value', TUnitInfo(UnitList.Items[i]).Filename);
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/CursorPos/Value', TUnitInfo(UnitList.Items[i]).CursorPos);
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/AutoCreate/Value', TUnitInfo(UnitList.Items[i]).AutoCreate);
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/ReadOnly/Value', TUnitInfo(UnitList.Items[i]).ReadOnly);
        xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/SyntaxHighlighter/Value', TUnitInfo(UnitList.Items[i]).SyntaxHighlighter);

        { TODO:
            Depending on how Bookmarks and Breakpoints work, save them out. They are setup as a TList.
            This may change if they are done in some other manner. Need to uncomment them and implement
            them once this it is known how they are going to work (depends on editor being used).
        }
        // xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/Bookmarks/Value', TUnitInfo(UnitList.Items[i]).Bookmarks);
        // xmlcfg.SetValue('ProjectOptions/UnitInfo/' + TUnitInfo(UnitList.Items[i]).UnitName + '/Breakpoints/Value', TUnitInfo(UnitList.Items[i]).Breakpoints);


          // Save the source code
          TUnitInfo(UnitList.Items[i]).Source.SaveToFile(TUnitInfo(UnitList.Items[i]).Filename);
      end;
    end;

    // Save the compiler options
    CompilerOptions.XMLConfigFile := xmlcfg;
    CompilerOptions.ProjectFile := confPath;
    CompilerOptions.SaveCompilerOptions(true);

    xmlcfg.Flush;
  finally
    xmlcfg.Free;
  end;
  Result := true;
end;

{------------------------------------------------------------------------------
  TProject ReadProject
 ------------------------------------------------------------------------------}
function TProject.ReadProject: Boolean;
var
  confPath: String;
  curUnitName: String;
  workUnitList: String;
  pui: TUnitInfo;
begin
  Result := false;

  confPath := GetXMLConfigPath;
  if (confPath = '') then exit;

  xmlcfg := TXMLConfig.Create(SetDirSeparators(confPath));

  try
    ProjectFile := xmlcfg.GetValue('ProjectOptions/General/ProjectFile/Value', '');
    MainUnit := xmlcfg.GetValue('ProjectOptions/General/MainUnit/Value', '');
    UnitNameList := xmlcfg.GetValue('ProjectOptions/General/UnitNameList/Value', '');
    Aliases := xmlcfg.GetValue('ProjectOptions/General/Aliases/Value', '');
    IconPath := xmlcfg.GetValue('ProjectOptions/General/IconPath/Value', './');
    TargetFileExt := xmlcfg.GetValue('ProjectOptions/General/TargetFileExt/Value', '');
    Title := xmlcfg.GetValue('ProjectOptions/General/Title/Value', '');
    OutputDirectory := xmlcfg.GetValue('ProjectOptions/General/OutputDirectory/Value', '.');
    UnitOutputDirectory := xmlcfg.GetValue('ProjectOptions/General/UnitOutputDirectory/Value', '.');

    // Save project source code
    Source.LoadFromFile(ProjectFile);

    pui := TUnitInfo.Create;
    try
      // Get first name
      curUnitName := Copy(UnitNameList, 0, Pos(UnitNameList, '|') - 1);
      workUnitList := Copy(UnitNameList, Pos(UnitNameList, '|') - 1, Length(UnitNameList));

      // Make sure we have a starting unit
      if (curUnitName <> '') then
      begin
        // Get all the info for each unit
        while (curUnitName <> '') do
        begin
          pui.UnitName := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/UnitName/Value', '');
          pui.Filename := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/Filename/Value', '');
          pui.CursorPos := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/CursorPos/Value', 0);
          pui.AutoCreate := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/AutoCreate/Value', true);
          pui.ReadOnly := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/ReadOnly/Value', false);
          pui.SyntaxHighlighter := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/SyntaxHighlighter/Value', 'freepascal');

          { TODO:
              Depending on how Bookmarks and Breakpoints work, save them out. They are setup as a TList.
              This may change if they are done in some other manner. Need to uncomment them and implement
              them once this it is known how they are going to work (depends on editor being used).
          }
          // pui.Bookmarks := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/Bookmarks/Value', '');
          // pui.Breakpoints := xmlcfg.GetValue('ProjectOptions/UnitInfo/' + curUnitName + '/Breakpoints/Value', '');

          // Load the source code
          pui.Source.LoadFromFile(pui.Filename);

          // Add the unit to the project
          AddUnit(pui);

          // Get the next unit name
          curUnitName := Copy(workUnitList, 0, Pos(workUnitList, '|') - 1);
          workUnitList := Copy(workUnitList, Pos(workUnitList, '|') - 1, Length(workUnitList));
        end;
      end;
    finally
      pui.free;
    end;

    // Load the compiler options
    CompilerOptions.XMLConfigFile := xmlcfg;
    CompilerOptions.ProjectFile := confPath;
    CompilerOptions.LoadCompilerOptions(true);
  finally
    xmlcfg.Free;
  end;

  Result := true;
end;

{------------------------------------------------------------------------------
  TProject AddUnit
 ------------------------------------------------------------------------------}
procedure TProject.AddUnit(AUnit: TUnitInfo);
begin
  if (AUnit <> nil) then UnitList.Add(AUnit);

  { TODO:
      Add the unit to the .lpr file.
      Add an AutoCreate method call to the .lpr file for the unit.
  }

end;

{------------------------------------------------------------------------------
  TProject RemoveUnit
 ------------------------------------------------------------------------------}
procedure TProject.RemoveUnit(AUnitName: String);
var
  i: Integer;
begin
  if (AUnitName <> '') then 
  begin
    for i := 0 to fUnitList.Count - 1 do
    begin
      if (TUnitInfo(UnitList.Items[i]).UnitName = AUnitName) then
        UnitList.Remove(UnitList.Items[i]);
    end;
  end;

  { TODO:
      Remove the unit from the .lpr file.
      Remove the AutoCreate method call from the .lpr file for the unit.
  }
end;

{------------------------------------------------------------------------------
  TProject Clear
 ------------------------------------------------------------------------------}
procedure TProject.Clear;
begin
  xmlcfg := nil;

  fAliases := '';
  fCompilerOptions.Clear;
  fIconPath := '';
  fMainUnit := '';
  fOutputDirectory := '.';
  fProjectFile := '';
  fProjectInfoFile := '';
  fSource.Clear;
  fTargetFileExt := '';
  fTitle := '';
  fUnitList.Clear;
  fUnitNameList := '';
  fUnitOutputDirectory := '.';
end;

{------------------------------------------------------------------------------
  TProject GetUnitList
 ------------------------------------------------------------------------------}
function TProject.GetUnitList: TList;
begin
  Result := fUnitList;
end;

{------------------------------------------------------------------------------
  TProject SetUnitList
 ------------------------------------------------------------------------------}
procedure TProject.SetUnitList(AList: TList);
begin
  fUnitList := AList;
end;

{------------------------------------------------------------------------------
  TProject GetXMLConfigPath
 ------------------------------------------------------------------------------}
function TProject.GetXMLConfigPath: String;
var
  confPath: String;
begin
  Result := '';

  if (ProjectInfoFile = '') then exit;

  confPath := GetPrimaryConfigPath + '/' + ProjectInfoFile;
  Writeln('[TPRoject] confPath = '+ConfPath);
  // See if config path exists and if not create it
  if (not DirectoryExists(GetPrimaryConfigPath)) then
  begin
     try
        // Create the directory
        CreatePrimaryConfigPath;

        { TODO:
            Try to read the configuration from the current path
            If successful, then read it in and write it to the primary path
            If unsuccessful, then just use defaults
        }
     except
       Assert(False, 'Trace:There was a problem creating the config directory. Using defaults.');
       Assert(False, 'Trace:File = ' + GetPrimaryConfigPath);
       confPath := './' + ProjectInfoFile;
       Result := confPath;
     end;
  end;

  Result := confPath;
end;


end.
{
  $Log$
  Revision 1.8  2001/02/22 17:04:57  lazarus
  added environment options + killed ide unit circles

  Revision 1.7  2001/02/08 06:08:13  lazarus
  Began adding code to save project to the output directory. Added TODO
  comments and cleaned up some of the code.                            CAW

  Revision 1.6  2001/01/31 13:03:33  lazarus
  Commitng source with new editor.
  Shane

  Revision 1.5  2001/01/31 06:28:41  lazarus
  Removed global unit.
  Renamed TProjectUnitInfo to TUnitInfo.
  Added Source property to both TUnitInfo and TProject to hold source code
    for units and project.
  Added functions to load and save units to TUnitInfo.
  Added code to save and load units when a project is saved and loaded.  CAW

  Revision 1.4  2001/01/29 05:42:41  lazarus
  Created new TProjectUnitInfo class.
  Created new TProject class. Saves to XML config file.
  Moved compiler options to write to the project file.            CAW

  Revision 1.3  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.2  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

  Revision 1.14  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.13  2000/05/10 02:34:43  lazarus
  Changed writelns to Asserts except for ERROR and WARNING messages.   CAW

  Revision 1.12  2000/04/18 20:06:39  lazarus
  Added some functions to Compiler.pp

  Revision 1.11  2000/04/17 19:50:05  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.10  2000/03/07 16:52:58  lazarus
  Fixxed a problem with the main.pp unit determining a new files FORM name.
  Shane

  Revision 1.9  2000/03/03 20:22:02  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.8  2000/03/01 21:54:05  lazarus
  90% finished with SAVE PROJECT and OPEN PROJECT
  Shane

  Revision 1.6  1999/05/14 18:44:17  lazarus
  *** empty log message ***

  Revision 1.5  1999/05/14 14:53:10  michael
  + Removed objpas from uses clause

  Revision 1.4  1999/05/14 14:39:44  michael
  All file stuff now uses sysutils. Win32 compiles

  Revision 1.3  1999/05/07 05:46:54  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/28 05:29:37  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/27 05:08:28  lazarus
  *** empty log message ***

  Revision 1.3  1999/04/20 02:56:42  lazarus
  *** empty log message ***

  Revision 1.2  1999/04/18 05:42:05  lazarus
  *** empty log message ***

  Revision 1.1  1999/04/14 07:31:44  michael
  + Initial implementation

}
