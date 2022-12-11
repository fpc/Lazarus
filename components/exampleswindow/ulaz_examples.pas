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

    David Bannon, Dec 2022
}
{$mode objfpc}{$H+}
{$define EXTESTMODE}

{X$define ONLINE_EXAMPLES}

interface

uses
    Classes, SysUtils,
    // LazUtils
    LazFileUtils, fileutil, LazLoggerBase,
    // LCL
    LCLType, LCLIntf, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
    ExtCtrls, Buttons,
    {$ifndef EXTESTMODE}
    IDEWindowIntf,
    {$endif}
    uexampledata, uConst;

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
        ClearSearchButton: TSpeedButton;
        Splitter2: TSplitter;
        StatusBar1: TStatusBar;
        procedure ButtonCloseClick(Sender: TObject);
        procedure ButtonDownloadClick(Sender: TObject);
        procedure ButtonOpenClick(Sender: TObject);
        procedure ButtonViewClick(Sender: TObject);
        procedure CheckGroupCategoryDblClick(Sender: TObject);
        procedure CheckGroupCategoryItemClick(Sender: TObject; {%H-}Index: integer);
        procedure ClearSearchButtonClick(Sender: TObject);
        procedure EditSearchChange(Sender: TObject);
        procedure EditSearchKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure ListView1Click(Sender: TObject);
        procedure ListView1DblClick(Sender: TObject);
        procedure ListView1Enter(Sender: TObject);
        procedure ListView1Exit(Sender: TObject);
        procedure ListView1KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
        procedure ListView1SelectItem(Sender: TObject; {%H-}Item: TListItem; {%H-}Selected: Boolean);
    private
        LastListViewIndex : integer;    // If 0 or greater, its an index to ListView
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
        procedure NewLVItem(const Proj, Path, KeyWords, Cat: string);
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

{$R *.lfm}

{ TFormLazExam }


// ------------------------ L I S T   V I E W ----------------------------------

procedure TFormLazExam.NewLVItem(const Proj, Path, KeyWords, Cat : string);
var
    TheItem : TListItem;
begin
    TheItem := ListView1.Items.Add;
    TheItem.Caption := Proj;
    TheItem.SubItems.Add(KeyWords);
    TheItem.SubItems.Add(Path);
    TheItem.SubItems.Add(Cat);
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
    KeyList := TStringList.Create;
    ListView1.BeginUpdate;
    try
        BuildSearchList(KeyList, EditSearch.Text);
        if Ex.GetListData(Proj, Cat, Path, KeyW, True, KeyList) then begin
            NewLVItem(Proj, Path, KeyW, Cat);
            inc(Cnt);
        end;
        while Ex.GetListData(Proj, Cat, Path, KeyW, False, KeyList) do begin
            NewLVItem(Proj, Path, KeyW, Cat);
            inc(Cnt);
        end;
    finally
        KeyList.Free;
        Screen.Cursor := crDefault;
        ListView1.EndUpdate;
    end;
    ButtonOpen.Enabled := false;
    ButtonDownLoad.enabled := false;
    ButtonView.enabled := false;
    Memo1.append(format(rsFoundExampleProjects, [Cnt]));
    StatusBar1.SimpleText := format(rsFoundExampleProjects, [Cnt]);
    LastListViewIndex := -1;        // start afresh
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
// A doubleclick will select that row, but it happens after OnEnter.
begin
    if ListView1.Selected = Nil then exit
    else
        LastListViewIndex := ListView1.ItemIndex;   // So other methods can find user choice
    ButtonDownloadClick(self);
    ButtonOpenClick(self);
end;

procedure TFormLazExam.ListView1Enter(Sender: TObject);
begin
    ListView1.ItemIndex := LastListViewIndex;    // possibly -1, half highlight item 0
end;

procedure TFormLazExam.ListView1Exit(Sender: TObject);
begin
    LastListViewIndex := ListView1.ItemIndex;        // save it before we leave, we'll be back
    ListView1.ClearSelection;
    ListView1.ItemIndex := -1;
end;

procedure TFormLazExam.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    if Key = VK_RETURN then begin
        Key := 0;
        // Its possible we tabbed into ListView without "selecting" a row.
        if ListView1.ItemIndex < 0 then        // I don't think this can happen anymore ?
            if ListView1.Items.count > 0 then
                ListView1.ItemIndex := 0       // Force select first item, its half highlite ??
            else
                Exit;
        ListView1DblClick(Sender);
    end
    else if not (Key in [VK_TAB, VK_ESCAPE, VK_PRIOR, VK_NEXT, VK_END, VK_HOME,
                         VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN]) then
        EditSearch.SetFocus;
end;

// --------------------- B U T T O N S -----------------------------------------

procedure TFormLazExam.ButtonOpenClick(Sender: TObject);
begin
    if LastListViewIndex < 0 then exit;
    ListView1.ItemIndex:= LastListViewIndex;
    if GetProjectFile(Ex.ExampleWorkingDir() + ListView1.Selected.Caption, True)     // Sets ProjectToOpen on success
        and ProjectToOpen.IsEmpty then
            showmessage(rsExNoProjectFile)
        else
            close;
end;

procedure TFormLazExam.ButtonDownloadClick(Sender: TObject);
begin
    if LastListViewIndex < 0 then exit;         // Can that happen ?
    ListView1.ItemIndex:= LastListViewIndex;

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
    ListView1.ItemIndex := -1;      // Unselect again for the Tabbers of this world.
end;

procedure TFormLazExam.ButtonViewClick(Sender: TObject);
begin
    // When we get here, we will have left the ListView and therefore triggered its onExit
    // Must restore its selected before we access it !
    if LastListViewIndex < 0 then exit;         // lets not be silly
    ListView1.ItemIndex:= LastListViewIndex;
    OpenURL(BaseURL + ListView1.Selected.SubItems[2] + '/' + ListView1.Selected.Caption);
    ListView1.ItemIndex := -1;
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
    CheckGroupCategoryItemClick(Sender, 0);
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

procedure TFormLazExam.KeyWordSearch();
begin
    Memo1.Clear;
    ListView1.Clear;
    Ex.KeyFilter := EditSearch.Text;
    LoadUpListView();
end;

procedure TFormLazExam.EditSearchChange(Sender: TObject);
begin
    ClearSearchButton.Enabled := EditSearch.Text <> '' ;
    if visible then
        KeyWordSearch();
end;

procedure TFormLazExam.EditSearchKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if Key in [VK_RETURN, VK_DOWN] then begin
        Key := 0;
        if ListView1.items.Count > 0 then begin
            ListView1.SetFocus;
            if Key = VK_DOWN then begin       // Is this logic for VK_DOWN good?
                ListView1.Selected := ListView1.Items[0];
                ListView1.ItemFocused := ListView1.Items[0];
            end;
        end;
    end;
end;

procedure TFormLazExam.ClearSearchButtonClick(Sender: TObject);
begin
    if EditSearch.Text = '' then Exit;
    EditSearch.Text := '';
    KeyWordSearch();
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
    LastListViewIndex := -1;        // Used to record ListView1.ItemIndex before Tabbing away

    EditSearch.TextHint := rsExSearchPrompt;
    // Does not work. Resource 'btnfiltercancel' not found.
    //ClearSearchButton.LoadGlyphFromLazarusResource('btnfiltercancel');
    ClearSearchButton.Enabled := False;
    CheckGroupCategory.Hint := rsGroupHint;
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
    Ex.Free;
end;

procedure TFormLazExam.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if Key = VK_ESCAPE then
        ModalResult := mrClose;
end;

procedure TFormLazExam.FormShow(Sender: TObject);
var
    i : integer;
begin
    Memo1.clear;
    EditSearch.text := '';              // or should we resume previous search ?
    Top := Screen.Height div 10;
    Height := Screen.Height * 7 div 10;
    ListView1.Height:= Screen.Height * 3 div 10;
    Ex.Free;
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
        Showmessage(Ex.ErrorMsg);                       // Note : previously, we treated this as fatal ?
    ex.getCategoryData(CheckGroupCategory.Items);       // This sets the name of all categories in the CheckGroup
    for i := 0 to CheckGroupCategory.items.Count-1 do   // check all the categories we found.
        CheckGroupCategory.Checked[i] := true;
    ListView1.Clear;
    PrimeCatFilter();
    LoadUpListView();
    ListView1.SetFocus;
end;

{   Must add a FormClose event 
	IDEDialogLayoutList.SaveLayout(Self);
}


end.

