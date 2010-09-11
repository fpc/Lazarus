unit FileFind;
{
     This component performs file search with or without recursing subfolders,
     with events generated on file match and on change scanning folder.
     Written in Delphi3, but I suppose it will work on Delphi1 and Delphi2 also
     Version 1.0

     // following 3 Additions by David R Hazlehurst 4/6/98 (drarh@tcp.co.uk)
     TStringList to store list of files found. (called "FilesFound")
     Support for multiple wildcards in the one search. (SearchFile property)
     Added flag to indicate if searching already or not.

     Properties:
         property Stop : boolean - Set to true if you want to cancel searching.
	 property SearchFile : shortstring - Set starting path and file mask for searching (e.g. "c:\*.doc").
         property RecurseSubFolders : boolean - Do recursing or not.
         property FilesFound : TStringList - drh : list of files found (read only)
         property Searching : boolean - drh : Indicates if a search is in progress (read only)
     Event handlers:
         property OnFileFind : TFindFileEvent - On match found
         property OnChangeFolder : TChangeFolderEvent - On change folder
	 property OnFinish : TNotifyEvent - On end of searching
     Methods:
         procedure Start; - Start searching.

     This component is freeware.
     I guarantee you nothing concering this code, but you can use it as you wish.

     Happy coding

     This component is dedicated to the girl I love... B.

     Jem Naadi Ahmed
     Bulsoft
     Bulgaria
     25 May, 1998

     jemna@yahoo.com
     Any comments will be welcome.

     /////////////////////////////////////////////////////////////////////////////
     // Following changes by David R Hazlehurst (drarh@tcp.co.uk) 4th June 1998
     /////////////////////////////////////////////////////////////////////////////
     Reformatted some of the layout to suit my style (indents, tabs, spacing, etc).

     Create contructor moved to "public" block of type def.

     Added property to store list of files found ("FilesFound").
     This makes it easier for the user of the control to get the list info.

     Added code to create string list in contructor
     Added destructor event to handle destruction of string list created within the create event

     Added a public "Searching" flag to indicate if search is already underway.  This will
     not prevent you from calling "start" again, but does provide a means of checking to see
     if the user already started searching.  You can then decide if you want to start a new
     search.  Another reason for making this optional rather than stopping you from searching
     outright, is in case an error occurs while searching and you may not get past the search
     block.  Of course, you can add a block to the code if you really want it.

     Improved the "start" procedure as follows:
              the "SearchFile" parameter can now accept multiple wildcards to search
              for more than one file type.  Each wildcard is sepearted by ";" as per
              the shell.  Each type is searched in order given.  So, for example:

                  SearchFile := 'c:\*.bat;*.sys'

              could return "c:\autoexec.bat" and "c:\config.sys", as well as others.


     end of my list.

     Im not claiming ideal solutions here, they were just the first things that
     occurred to me (from my requirements), and hey, they work.  By all means, improve
     on my "improvements" if you wish.  I would also be interested in the results.
     /////////////////////////////////////////////////////////////////////////////

     29.11.2004 - ported to FPC/lazarus by barko, OPINFOS d.o.o.

}

interface

uses LResources,
     SysUtils, Classes, FileUtil, Graphics, Controls, Forms, Dialogs;

type
    TFindFileEvent = procedure(fullpath:string;info:TSearchRec)of object;
    TChangeFolderEvent = procedure(fullpath:string;info:TSearchRec)of object;

    TFileSearch = class(TComponent)
    private
	{ Private declarations }
	fRec : boolean;
	fStop : boolean;
        fSearching : boolean;
        fFilesFound : TStringList;
	fFileFindEvent : TFindFileEvent;
	fChangeFolderEvent : TChangeFolderEvent;
	fFinishEvent : TNotifyEvent;
	fdirName : shortstring;
    protected
	{ Protected declarations }
	procedure ScanDir(ffdirName:string;attr:word);

    public
    	{ Public declarations }
        constructor Create(aOwner:TComponent); override;
        destructor Destroy; override;

	procedure Start;

        property Searching : boolean read fSearching;

    published
	{ Published declarations }
	property Stop : boolean read fStop write fStop default false;
	property SearchFile : shortstring read fdirName write fdirName;
        property FilesFound : TStringList read fFilesFound;
	property RecurseSubFolders : boolean read fRec write fRec default true;
	property OnFileFind : TFindFileEvent read fFileFindEvent write fFileFindEvent;
	property OnChangeFolder : TChangeFolderEvent read fChangeFolderEvent write fChangeFolderEvent;
	property OnFinish : TNotifyEvent read fFinishEvent write fFinishEvent;
    end;

const
    {$IFNDEF LINUX} // barko
    delimeter = '\';
    {$ELSE}
    delimeter = '/';
    {$ENDIF} // barko


procedure Register;

implementation

procedure Register;
begin
     RegisterComponents('Samples', [TFileSearch]);
end;

constructor TFileSearch.Create(aOwner:TComponent);
begin
      inherited create(aOwner);

      fFilesFound := TStringList.Create;     // drh 4/6/98: Create results list
      fRec := true;
      fSearching := false;                   // drh 4/6/98: Initialise "Searching" flag
      fStop := false;
end;

// drh 4/6/98: Added destructor handler
destructor TFileSearch.Destroy;
begin
     fFilesFound.Free;
     inherited Destroy;
end;

procedure TFileSearch.Start;
var
   i, newWildCard : Integer;
   curSearchPath, wildCards : String;
   srchPaths : TStringList;
begin
     fStop := false;

     fSearching := True;   // drh 4/6/98: flag to indicate we are searching
     fFilesFound.Clear;    // drh 4/6/98: new search, so no files should be listed

     // Look for ";" wildcard seperators.
     // loop through replacing the "filename" with each wildcard...
     newWildCard := Pos( ';', fDirName);

     if newWildCard > 0 then
     begin
          curSearchPath := Copy( fdirName, 1, newWildCard-1);
          wildCards := Copy( fdirName, newWildCard+1, length(fDirName) );

          srchPaths := TStringList.Create;
          srchPaths.Add( curSearchPath );

          // Build up a list of search paths by looping through each wildcard
          while length(wildCards) > 0 do
          begin
               curSearchPath := ExtractFilePath( curSearchPath );
               newWildCard := Pos( ';', wildCards );

               if newWildCard > 0 then
               begin
                    curSearchPath := curSearchPath + Copy(wildCards, 1, newWildCard-1);
                    wildCards := Copy(wildCards, newWildCard+1, length(wildCards) );
               end
               else
               begin
                    curSearchPath := curSearchPath + wildCards;
                    wildCards := '';
               end;

               srchPaths.Add( curSearchPath );
          end;

          // Well, we got the paths, lets start searching them shall we?
          for i := 0 to srchPaths.Count - 1 do
              ScanDir(srchPaths.Strings[i], faAnyFile);

          // get rid of search paths.
          srchPaths.Free;
     end
     else // no other wildcards to search, just a single file def
         ScanDir(fdirName, faAnyFile);

     // drh 4/6/98:
     // the following was moved here from end of "ScanDir", just in
     // case multiple searches are being carried out (do not want
     // multiple end of search events being fired).
     if Assigned( fFinishEvent ) then
        fFinishEvent( Self );         // notify user that searching is complete.

     fSearching := False; // drh 4/6/98: flag indicates we arnt searching any more
end;

procedure TFileSearch.ScanDir(ffdirName:string; attr:word);
const
    {$IFNDEF LINUX} // barko
     fi : string = '*.*';
    {$ELSE}
     fi : string = '*';
    {$ENDIF} // barko
     p : string = '.';
     pp : string = '..' ;
var
   path : string;
   doserror : integer;
   sfi : string;

   procedure showq(fullpath:string; FolderInfo:TSearchRec);
   var
      dirq : TSearchRec;
   begin
        if assigned(fChangeFolderEvent) then
           fChangeFolderEvent(fullpath,FolderInfo);

        doserror := FindFirstUTF8(fullpath+sfi,attr,dirq);

        while (doserror = 0)and(not fstop) do
        begin
             if (dirq.name<>p) and (dirq.name<>pp) and (assigned(fFileFindEvent)) then
             begin
                fFileFindEvent( fullpath, dirq );
                fFilesFound.Add( fullpath + dirq.Name ); // drh 4/6/98: Add filename to list of those found thus far
             end;

             doserror := FindNextUTF8(dirq);
             application.processMessages;
        end;
        FindCloseUTF8(dirq);// barko

   end; // showq

   procedure ScanLDir(fffdirName:string; fInfo:TSearchRec);
   var
      dirinfo : TSearchRec;
   begin
        showq(fffDirName, fInfo);
        dosError := FindFirstUTF8(fffDirName+fi, faAnyfile, dirInfo);

        while (doserror = 0) and (not fstop) do
        begin
             application.ProcessMessages;

             if (dirInfo.name<>p) and (dirInfo.name<>pp) then
                if (dirInfo.attr and faDirectory <> 0) and (frec) then
                   ScanLDir(fffdirName+dirinfo.name+delimeter, dirInfo);

             dosError := FindNextUTF8(dirInfo);
             application.ProcessMessages;
        end;
        FindCloseUTF8(dirInfo); // barko

   end; // ScanLDir

var
   fInfo : TSearchRec;
   fPath : string;
begin    // ScanDir

     path := ExtractFilePath( ffDirName );
     sfi := ExtractFileName( ffDirName );

     fPath := Copy(path, 1, length(Path) - 1 );

     FindFirstUTF8(fPath, faAnyfile, fInfo);
     ScanLDir(Path, fInfo);
     FindCloseUTF8(fInfo); // barko
end; // ScanDir


initialization
  {$I filefind.lrs}
end.
