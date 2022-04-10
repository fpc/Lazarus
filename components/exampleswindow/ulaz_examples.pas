unit uLaz_Examples;
{
 **********************************************************************
  This file is part of a Lazarus Package, Examples Window.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 **********************************************************************

This unit displays all the examples that it can find metadata for. At present it
looks in the LazarusDir and then the LazConfigDir (but can be made to look online).

It scans the examples and makes Catagory Checkboxes for all the Categories it finds.

In OnLine mode, will look for a master meta file in LazConfigDir/examples
If its not there, it will try to download one from Remote.
In either case will scan the LazConfigDir (excluding Examples ???) looking for
potential 'other' example projects, recognisable by a valid json file with an
extension of ex-meta.

Notes -
    We have a search field across the top, its requires user to press enter,
    performance notwithstanding, it could be converted to update with every key press.

    David Bannon, Feb 2022
}
{$mode objfpc}{$H+}
{x$define EXTESTMODE}

{X$define ONLINE_EXAMPLES}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
    ExtCtrls, Interfaces, uexampledata, uConst
    {$ifndef EXTESTMODE}
    , IDEWindowIntf
    {$endif}
    ;


type

    { TFormLazExam }

    TFormLazExam = class(TForm)
        ButtonView: TButton;
        ButtonDownload: TButton;
        ButtonClose: TButton;
        ButtonOpen: TButton;
        CheckGroupCategory: TCheckGroup;
        EditSearch: TEdit;
        ListView1: TListView;
        Memo1: TMemo;
        Splitter2: TSplitter;
        StatusBar1: TStatusBar;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonDownloadClick(Sender: TObject);
        procedure ButtonOpenClick(Sender: TObject);
        procedure ButtonViewClick(Sender: TObject);
        procedure CheckGroupCategoryDblClick(Sender: TObject);
        procedure CheckGroupCategoryItemClick(Sender: TObject; Index: integer);
        procedure EditSearchExit(Sender: TObject);
        procedure EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListView1Click(Sender: TObject);
        procedure ListView1DblClick(Sender: TObject);
        procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    private
        procedure BuildSearchList(SL: TStringList; const Term: AnsiString);
                        // Copies the passed ex dir to a dir named for the Proj.
                        // SrcDir includes name of actual dir, DestDir does not.
        function CopyFiles(const Proj, SrcDir, DestDir: string): boolean;
                        // Checks for existance of passed path, the last element of which is case Insensitive.
                        // Returns with the actual name of the full path if successful.
        function DirExistsCaseInSense(const APath: string; out ActualFullDir: string) : boolean;
                        // Passed the Full Path (with or without trailing delim) to a Project Dir, rets F if not
                        // present, T if Dir exists. If it finds an lpi file, rets with FFilename, else empty string.
                        // WriteProjectToOpen will cause a download / copy of the files.
                        // Sets the Regional Var, ProjectToOpen if WriteProjectToOpen is true.
                        // Thats triggers a Lazarus Open when this window closes.
        function GetProjectFile(const APath: string; WriteProjectToOpen: boolean = false): boolean;
        procedure KeyWordSearch;
        function NewLVItem(const LView: TListView; const Proj, Path, KeyWords,
            Cat: string): TListItem;
                        // Displays the current content of Examples List in the listview and
                        // populates the Category checkboxes.
        procedure LoadUpListView();
        procedure PrimeCatFilter;
    public
        GitDir   : string;          // Not needed in Lazarus Package, used in dev's tool emt
        LazConfigDir : string;      // We will look for Laz config here.
        ExamplesHome : string;      // Defaults to LazConfig but user settable
        RemoteRepo : string;        // This is the full gitlab URL
        ProjectToOpen : string;     // If not empty after close, open the project named.
    end;

var
    FormLazExam: TFormLazExam;
    Ex : TExampleData;

implementation

uses LazFileUtils, LCLType, fileutil, LazLogger, LCLIntf;

{$R *.lfm}

{ TFormLazExam }


// ------------------------ L I S T   V I E W ----------------------------------

function TFormLazExam.NewLVItem(const LView : TListView; const Proj, Path, KeyWords, Cat : string): TListItem;
var
    TheItem : TListItem;
begin
    TheItem := LView.Items.Add;
    TheItem.Caption := Proj;
    TheItem.SubItems.Add(KeyWords);
    TheItem.SubItems.Add(Path);
    TheItem.SubItems.Add(Cat);
    Result := TheItem;
end;

procedure TFormLazExam.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
    ListView1Click(Sender);
end;

procedure TFormLazExam.LoadUpListView();
var
    Proj, Cat, Path, KeyW : string;
    Cnt : integer = 0;
    KeyList : TStringList = nil;
begin
    Screen.Cursor := crHourGlass;
    if EditSearch.text <> rsExSearchPrompt then begin
        KeyList := TStringList.Create;
        BuildSearchList(KeyList, EditSearch.Text);
    end;
    try
        if Ex.GetListData(Proj, Cat, Path, KeyW, True, KeyList) then begin
            NewLVItem(ListView1, Proj, Path, KeyW, Cat);
            inc(Cnt);
        end;
        while Ex.GetListData(Proj, Cat, Path, KeyW, False, KeyList) do begin
            NewLVItem(ListView1, Proj, Path, KeyW, Cat);
            inc(Cnt);
        end;
    finally
        if KeyList <> Nil then KeyList.Free;
        Screen.Cursor := crDefault;
    end;
    ButtonOpen.Enabled := false;
    ButtonDownLoad.enabled := false;
    ButtonView.enabled := false;
    Memo1.append(format(rsFoundExampleProjects, [Cnt]));
    StatusBar1.SimpleText := format(rsFoundExampleProjects, [Cnt]);
end;

procedure TFormLazExam.ListView1Click(Sender: TObject);
begin
    if  ListView1.Selected = nil then exit;         // White space below entries ....
    Memo1.Clear;
    Memo1.append(ListView1.Selected.SubItems[1]);
    Memo1.append('');
    Memo1.Append(Ex.GetDesc(ListView1.Selected.SubItems[1] + ListView1.Selected.Caption));
    // ListView1.Selected.Caption may be CamelCase from JSON.Name rather than path where we found it.
    ButtonDownLoad.enabled := true;
    ButtonView.enabled := true;
    //ButtonOpen.Enabled := GetProjectFile(ListView1.Selected.SubItems[1]);
    ButtonOpen.Enabled := GetProjectFile(Ex.ExampleWorkingDir() + ListView1.Selected.Caption);
end;


procedure TFormLazExam.ListView1DblClick(Sender: TObject);
begin
    ButtonDownloadClick(self);
    ButtonOpenClick(self);
end;

// --------------------- B U T T O N S -----------------------------------------

procedure TFormLazExam.ButtonOpenClick(Sender: TObject);
begin
    if GetProjectFile(Ex.ExampleWorkingDir() + ListView1.Selected.Caption, True)     // Sets ProjectToOpen on success
        and ProjectToOpen.IsEmpty then
            showmessage(rsExNoProjectFile)
        else
            close;
end;

procedure TFormLazExam.ButtonDownloadClick(Sender: TObject);
begin
    if  ListView1.Selected = nil then exit;         // White space below entries ....
    if GetProjectFile(Ex.ExampleWorkingDir() + ListView1.Selected.Caption) then begin
        if Application.MessageBox(pchar(rsRefreshExistingExample)
                        , pchar(ListView1.Selected.Caption)
                        , MB_ICONQUESTION + MB_YESNO) <> IDYES then exit;
        // OK - we overwrite. Any other files user has added are not removed
    end;
    Screen.Cursor := crHourGlass;
    try
        if Ex <> nil then begin
            {$ifdef ONLINE_EXAMPLES}
            StatusBar1.SimpleText := rsExDownloadingProject;
            Application.ProcessMessages;
                EX.DownLoadDir(ListView1.Selected.SubItems[1]);
            StatusBar1.SimpleText := rsExProjectDownloadedTo + ' ' + Ex.MasterMeta(True)
                                                       + ListView1.Selected.Caption;
            {$else}
            StatusBar1.SimpleText := rsExCopyingProject;
            Application.ProcessMessages;
            if copyFiles(  ListView1.Selected.Caption,
                        ListView1.Selected.SubItems[1], Ex.ExampleWorkingDir()) then
                StatusBar1.SimpleText := rsExProjectCopiedTo + ' ' + Ex.ExampleWorkingDir()
                            + ListView1.Selected.Caption
            else StatusBar1.SimpleText := rsFailedToCopyFilesTo + ' ' + Ex.ExampleWorkingDir();
            {$endif}
        end;
    finally
        Screen.Cursor := crDefault;
    end;
    ButtonOpen.Enabled := GetProjectFile(Ex.ExampleWorkingDir() + ListView1.Selected.Caption);

end;

procedure TFormLazExam.ButtonViewClick(Sender: TObject);
begin
    OpenURL(BaseURL + ListView1.Selected.SubItems[2] + '/' + ListView1.Selected.Caption);
end;

procedure TFormLazExam.ButtonCloseClick(Sender: TObject);
begin
    Close;
end;

function TFormLazExam.CopyFiles(const Proj, SrcDir, DestDir : string) : boolean;
var
    STL : TStringList;
    St  : string;
    ChopOff : integer;
begin
    ChopOff := length(AppendPathDelim(SrcDir));
    if not ForceDirectoriesUTF8(DestDir + Proj) then exit(False);
    STL := FindAllDirectories(SrcDir, True);
    for St in STL do
        // note the copy process leaves a leading Pathdelim, good, I think...
        if not ForceDirectoriesUTF8(DestDir + Proj + copy(St, ChopOff, 1000)) then exit(False);
    STL.Free;
    STL := FindAllFiles(SrcDir, AllFilesMask, True, faAnyFile);
    for St in STL do begin
        if not copyfile(St, DestDir + Proj + copy(St, ChopOff, 1000)) then exit(False);
        //debugln('TFormLazExam.CopyFiles Copy ' + ST + #10 + ' to ' + DestDir + Proj + copy(St, ChopOff, 1000));  // DRB
    end;
    STL.Free;
end;

// ----------------------- Check Boxes -----------------------------------------

procedure TFormLazExam.CheckGroupCategoryDblClick(Sender: TObject);
var
    i : integer;
begin
    for i := 0 to CheckGroupCategory.Items.Count -1 do
        CheckGroupCategory.Checked[i] := not CheckGroupCategory.Checked[i];
end;

procedure TFormLazExam.CheckGroupCategoryItemClick(Sender: TObject; Index: integer);
begin
    if Ex = Nil then exit;
    Memo1.clear;
    ListView1.Clear;
    PrimeCatFilter();
    LoadUpListView();
end;

// ---------------------- Setting Project to Open ------------------------------

function TFormLazExam.GetProjectFile(const APath : string; WriteProjectToOpen : boolean = false) : boolean;
var
    Info : TSearchRec;
    RealDir : string;
    // The project dir name may not be a case match for the Project Name.
    // We are looking here at dir under example_work_area so some match is expected
begin
    Result := DirExistsCaseInSense(APath, RealDir);
    if not (Result and WriteProjectToOpen) then exit;
    if FindFirst(RealDir + '*.lpi', faAnyFile, Info) = 0 then
        ProjectToOpen := RealDir + Info.Name
    else ProjectToOpen := '';
    FindClose(Info);
end;

function TFormLazExam.DirExistsCaseInSense(const APath : string; out ActualFullDir : string) : boolean;
var
    Info : TSearchRec;
    FName : string;
begin
    FName := lowercase(extractFileName(ChompPathDelim(APath)));
    if FindFirst(extractFileDir(ChompPathDelim(APath))+PathDelim + '*',faDirectory, Info) = 0 then begin
        try
            repeat
                if (Info.Attr and faDirectory) = faDirectory then
                    if lowercase(Info.Name) = FName then begin
                        ActualFullDir := extractFileDir(ChompPathDelim(APath))
                                            +PathDelim + Info.Name + PathDelim;
                        exit(True);
                    end;
            until FindNext(Info) <> 0;
        finally
            FindClose(Info);
        end;
    end;
    Result := False;
end;


// ---------------------- S E A R C H   R E L A T E D --------------------------

// Build a StringList of the terms user has typed in, words or "groups of words"
procedure TFormLazExam.BuildSearchList(SL : TStringList; const Term : AnsiString);
var
    I : integer = 1;
    AWord : string = '';
    InCommas : boolean = false;
begin
    while I <= length(trim(Term)) do begin
        if Term[i] = '"' then begin
            if InCommas then begin
                SL.add(AWord);
                AWord := '';
                InCommas := False;
            end else begin
                InCommas := true;
            end;
            inc(I);
            continue;
        end;
        if Term[i] = ' ' then begin
            if InCommas then
                AWord := AWord + Term[i]
            else begin
                if AWord <> '' then begin
                    SL.Add(AWord);
                    AWord := '';
                end;
            end;
            inc(I);
            continue;
        end;
        AWord := AWord + Term[i];
        inc(i);
        continue;
    end;
    if AWord <> '' then
        SL.Add(AWord);
end;

procedure TFormLazExam.EditSearchExit(Sender: TObject);
begin
    if EditSearch.Text = '' then begin
        EditSearch.Hint:= rsExSearchPrompt;
        EditSearch.Text := rsExSearchPrompt;
        EditSearch.SelStart := 1;
        EditSearch.SelLength := length(EditSearch.Text);
    end;
end;

procedure TFormLazExam.KeyWordSearch();
begin
      Memo1.clear;
      ListView1.Clear;
      Ex.KeyFilter := EditSearch.Text;
      LoadUpListView();
end;

procedure TFormLazExam.EditSearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    // Must do this here to stop LCL from selecting the text on VK_RETURN
    if Key = VK_RETURN then begin
      Key := 0;
      KeyWordSearch();
    end;
end;

procedure TFormLazExam.PrimeCatFilter();
var
    i : integer;
    St : string = '';
begin
    for i := 0 to CheckGroupCategory.Items.Count -1 do begin
        if  CheckGroupCategory.Checked[i] then
            St := St + CheckGroupCategory.Items[i] + ' ';
    end;
    Ex.CatFilter := St;
end;


// -------------------- H O U S E   K E E P I N G ------------------------------

procedure TFormLazExam.FormCreate(Sender: TObject);
begin
    Caption := rsExampleProjects;
    ListView1.Column[0].Caption := rsExampleName;
    ListView1.Column[1].Caption := rsExampleKeyWords;
    ListView1.Column[2].Caption := rsExamplePath;
    ListView1.AutoSortIndicator := True;
    ListView1.Column[0].SortIndicator := siDescending;
    ListView1.AutoSort := True;
    ListView1.SortDirection:= sdDescending;
    ListView1.AutoWidthLastColumn:= True;
    ListView1.ViewStyle:= vsReport;
    ListView1.Column[0].AutoSize := true;
    ListView1.Column[1].AutoSize := true;
    ListView1.Column[2].Visible := false;
    ListView1.ReadOnly := True;
    EditSearch.text := rsExSearchPrompt;
    Ex := nil;
    // These are ObjectInspector set but I believe I cannot get OI literals set in a Package ??
    ButtonClose.Caption := rsExampleClose;
    {$ifdef ONLINE_EXAMPLES}
    ButtonDownload.Caption := rsExampleDownLoad;
    {$else}
    ButtonDownload.Caption := rsExampleCopy;
    {$endif}
    ButtonView.Caption := rsExampleView;
    ButtonOpen.Caption := rsExampleOpen;
    CheckGroupCategory.Caption := rsExampleCategory;
    {$ifndef EXTESTMODE}
    IDEDialogLayoutList.ApplyLayout(Self);
    {$endif}
end;

procedure TFormLazExam.FormDestroy(Sender: TObject);
begin
    if Ex <> nil then Ex.Free;
end;

procedure TFormLazExam.FormShow(Sender: TObject);
var
    i : integer;
begin
    Memo1.clear;
    if Ex <> Nil then Ex.Free;
    StatusBar1.SimpleText := rsExSearchingForExamples;
    Ex := TExampleData.Create();
    Ex.GitDir     := GitDir;
    Ex.ExamplesHome := ExamplesHome;
    Ex.RemoteRepo := RemoteRepo;
    EX.LazConfigDir := LazConfigDir;
    {$ifdef ONLINE_EXAMPLES}
    Ex.LoadExData(FromCacheFile);
    {$else}
    Ex.LoadExData(FromLazSrcTree);
    {$endif}
    if Ex.ErrorMsg <> '' then
        Showmessage(Ex.ErrorMsg)
    else begin
        ex.getCategoryData(CheckGroupCategory.Items);       // This sets the name of all categories in the CheckGroup
        for i := 0 to CheckGroupCategory.items.Count-1 do   // check all the categories we found.
            CheckGroupCategory.Checked[i] := true;
        ListView1.Clear;
        PrimeCatFilter();
        LoadUpListView();
    end;
    if EditSearch.Text <> rsExSearchPrompt then
        KeyWordSearch()
    else EditSearch.SetFocus;


end;

{   Must add a FormClose event 
	IDEDialogLayoutList.SaveLayout(Self);
}


end.

