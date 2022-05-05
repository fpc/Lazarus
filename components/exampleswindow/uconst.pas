unit uConst;
{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit provides the Example Projects package with a few constants and
a number of string literals that will i18n translation.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cRemoteRepository = 'https://gitlab.com/api/v4/projects/32866275/repository/';
                                         // Immediate Local dir name under which we copy or
  cExamplesDir = 'examples_work_dir';    // download examples to. Carefull about simplifying it
  cConfigFileName = 'exampleprojectscfg.xml';
  BaseURL = 'https://gitlab.com/dbannon/laz_examples/-/tree/main/';   // Online Examples, there for testing for now...


resourcestring

    // --------- Multiple units
    rsExampleProjects = 'Example Projects';

    // ---------- uLaz_Examples
    rsExSearchPrompt = 'Search Here';
    rsExNoProjectFile = 'Project file not found.';
    rsFoundExampleProjects = 'Found %d Example Projects';
    rsRefreshExistingExample =  'Refresh Existing Example?';
    rsExDownloadingProject = 'Downloading Project ...';
    rsExCopyingProject = 'Copying Project ...';
    rsExProjectDownloadedTo = 'Project Downloaded to';      // followed by a full path name
    rsExProjectCopiedTo = 'Project Copied to';              // followed by a full path name
    rsExampleName = 'Name';                                 // Column title
    rsExamplePath = 'Path';                                 //  "
    rsExampleKeyWords = 'Keywords';                         //  "
    rsExSearchingForExamples = 'Searching for Examples ...';
    rsFailedToCopyFilesTo = 'Failed to copy files to';        // Followed by a dir were we, apparently, cannot write
    rsGroupHint = 'Double click inverts selection';

    // These are ObjectInspector set but I believe I cannot get OI literals i18n in a Package ??
    rsExampleOpen = 'Open';                                 // Button Caption
    rsExampleDownload = 'Download';                         //  "
    rsExampleClose = 'Close';                               //  "
    rsExampleCategory = 'Category';                         //  "
    rsExampleCopy = 'Copy to work area';                    //  "
    rsExampleView = 'View in Browser';                      //  "

    // Settings Frame
    rsDirWhereExamplesGo = 'Directory where Examples go';

    // ------- rsExampleData
    // Most literals in uExampleData are for debugging only and very unlikely to be
    // seen by the end user. But a couple of network related ones may need i18n -
    rsExNetWorkError = 'GitLab Network Error';              // Followed by system error msg


implementation

end.

