unit lhelpstrconsts;

{$mode objfpc}{$H+}

interface

resourcestring

  // GUI
  slhelp_About = 'About';
  slhelp_LHelpCHMFileViewerVersionCopyrightCAndrewHainesLaz = 'LHelp (CHM file viewer)%sVersion %s%sCopyright (C) Andrew Haines, %sLazarus contributors';
  slhelp_Ok = 'Ok';
  slhelp_PleaseEnterAURL = 'Please enter a URL';
  slhelp_SupportedURLTypeS = 'Supported URL type(s): (%s)';
  slhelp_File = '&File';
  slhelp_Open = '&Open ...';
  slhelp_OpenRecent = 'Open Recent';
  slhelp_OpenURL = 'Open &URL ...';
  slhelp_Close = '&Close';
  slhelp_EXit = 'E&xit';
  slhelp_View = '&View';
  slhelp_ShowContents = 'Show contents';
  slhelp_OpenNewTabWithStatusBar = 'Open new tab with status bar';
  slhelp_OpenNewFileInSeparateTab = 'Open new file in separate tab';
  slhelp_Help = '&Help';
  slhelp_About2 = '&About ...';
  slhelp_OpenExistingFile = 'Open existing file';
  slhelp_HelpFilesChmChmAllFiles = 'Help files (*.chm)|*.chm|All files (*.*)|*';
  slhelp_LHelp = 'LHelp';
  slhelp_LHelp2 = 'LHelp - %s';
  slhelp_CannotHandleThisTypeOfContentForUrl = 'Cannot handle this type of content. "%s" for url:%s%s';
  slhelp_CannotHandleThisTypeOfSubcontentForUrl = 'Cannot handle this type of subcontent. "%s" for url:%s%s';
  slhelp_Loading = 'Loading: %s';
  slhelp_NotFound = '%s not found!';
  slhelp_LoadedInMs = 'Loaded: %s in %sms';
  slhelp_TableOfContentsLoadingPleaseWait = 'Table of Contents Loading. Please Wait ...';
  slhelp_TableOfContentsLoading = 'Table of Contents Loading ...';
  slhelp_IndexLoading = 'Index Loading ...';
  slhelp_Untitled = 'untitled';
  slhelp_NoResults = 'No Results';
  slhelp_Contents = 'Contents';
  slhelp_Index = 'Index';
  slhelp_Search = 'Search';
  slhelp_Keyword = 'Keyword:';
  slhelp_Find = 'Find';
  slhelp_SearchResults = 'Search Results:';
  slhelp_Copy = 'Copy';
  slhelp_PageCannotBeFound = 'Page cannot be found!';

  // --help
  slhelp_LHelpOptions = '  LHelp options:';
  slhelp_UsageLhelpFilenameContextIdHideIpcnameLhelpMyapp = '    Usage: lhelp [[filename] [--context id] [--hide] [--ipcname lhelp-myapp]]';
  slhelp_HelpShowThisInformation = '    --help     :  Show this information';
  slhelp_HideStartHiddenButAcceptCommunicationsViaIPC =       '    --hide     :  Start hidden but accept communications via IPC';
  slhelp_ContextShowTheHelpInformationRelatedToThisContext =  '    --context  :  Show the help information related to this context';
  slhelp_IpcnameTheNameOfTheIPCServerToListenOnForProgramsW = '    --ipcname  :  The name of the IPC server to listen on for%s              programs who wish to control the viewer';

implementation

end.

