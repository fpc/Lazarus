{ $Id$}
{
 *****************************************************************************
 *                              GtkWSDialogs.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit GtkWSDialogs;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef gtk2}
  Gtk2, Glib2, gdk2, pango,
  {$else}
  Gtk, gdk, Glib,
  {$endif}
  SysUtils, Classes, Controls, Graphics, Dialogs, ExtDlgs, LCLType, LMessages,
  InterfaceBase, LCLStrConsts, LCLProc, FileUtil, LazFileUtils, WSDialogs,
  WSLCLClasses, GtkInt, GtkProc, GtkWSControls, GtkExtra, GtkDef, GtkGlobals;

type

  { TGtkWSCommonDialog }

  TGtkWSCommonDialog = class(TWSCommonDialog)
  public
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetSizes(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TGtkWSFileDialog }

  TGtkWSFileDialog = class(TWSFileDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtkWSOpenDialog }

  TGtkWSOpenDialog = class(TWSOpenDialog)
  protected
    class function CreateOpenDialogFilter(OpenDialog: TOpenDialog; SelWidget: PGtkWidget): string; virtual;
    class procedure CreateOpenDialogHistory(OpenDialog: TOpenDialog; SelWidget: PGtkWidget); virtual;
    class procedure CreatePreviewDialogControl(PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtkWSSaveDialog }

  TGtkWSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TGtkWSSelectDirectoryDialog }

  TGtkWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
  end;

  { TGtkWSColorDialog }

  TGtkWSColorDialog = class(TWSColorDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtkWSColorButton }

  TGtkWSColorButton = class(TWSColorButton)
  published
  end;

  { TGtkWSFontDialog }

  TGtkWSFontDialog = class(TWSFontDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

// forward declarations

procedure UpdateDetailView(OpenDialog: TOpenDialog);
function gtkDialogHelpclickedCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;

implementation

{-------------------------------------------------------------------------------
  procedure UpdateDetailView
  Params: OpenDialog: TOpenDialog
  Result: none

  Shows some OS dependent information about the current file
-------------------------------------------------------------------------------}
procedure UpdateDetailView(OpenDialog: TOpenDialog);
var
  FileDetailLabel: PGtkWidget;
  Filename, OldFilename, Details: String;
  Widget: PGtkWidget;
begin
  //DebugLn(['UpdateDetailView ']);
  Widget := PGtkWidget(OpenDialog.Handle);
  {$IfDef GTK2}
  FileName := gtk_file_chooser_get_filename(PGtkFileChooser(Widget));
  {$ELSE}
  FileName := gtk_file_selection_get_filename(PGtkFileSelection(Widget));
  {$ENDIF}
  OldFilename := OpenDialog.Filename;
  if Filename = OldFilename then
    Exit;
  OpenDialog.Filename := Filename;
  // tell application, that selection has changed
  OpenDialog.DoSelectionChange;
  if (OpenDialog.OnFolderChange <> nil) and
     (ExtractFilePath(Filename) <> ExtractFilePath(OldFilename)) then
    OpenDialog.DoFolderChange;
  // show some information
  FileDetailLabel := gtk_object_get_data(PGtkObject(OpenDialog.Handle), 'FileDetailLabel');
  if FileDetailLabel = nil then
    Exit;
  if FileExistsUTF8(Filename) then
    Details := GetFileDescription(Filename)
  else
    Details := Format(rsFileInfoFileNotFound, [Filename]);
  gtk_label_set_text(PGtkLabel(FileDetailLabel), PChar(Details));
end;

// ------------------------ Signals --------------------------------------------

{-------------------------------------------------------------------------------
  function GTKDialogSelectRowCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever a row is selected in a commondialog
-------------------------------------------------------------------------------}
function gtkDialogSelectRowCB(widget: PGtkWidget; Row, Column: gInt;
  bevent: pgdkEventButton; data: gPointer): GBoolean; cdecl;
var
  theDialog: TCommonDialog;
  MenuWidget: PGtkWidget;
  AFilterEntry: TFileSelFilterEntry;
  FileSelWidget: PGtkFileSelection;
  ShiftState: TShiftState;
  loop : gint;
  startRow : gint;
  endRow : gint;
begin
  //debugln('GTKDialogSelectRowCB A ');
  Result:=CallBackDefaultReturn;
  if (Data=nil) or (BEvent=nil) or (Column=0) or (Row=0) then ;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    // only process the callback if there is event data. If there isn't any
    // event data that means it was called due to a direct function call of the
    // widget and not an actual mouse click on the widget.
    FileSelWidget:=PGtkFileSelection(theDialog.Handle);
    if (bevent <> nil) and (gdk_event_get_type(bevent) = GDK_2BUTTON_PRESS)
    and (FileSelWidget^.dir_list = widget) then begin
      MenuWidget := gtk_object_get_data(PGtkObject(FileSelWidget),
                                        'LCLFilterMenu');
      if MenuWidget <> nil then begin
        AFilterEntry := TFileSelFilterEntry(gtk_object_get_data(PGtkObject(
            gtk_menu_get_active(PGtkMenu(MenuWidget))), 'LCLIsFilterMenuItem'));
        if (AFilterEntry<>nil) and (AFilterEntry.Mask<>nil) then
          PopulateFileAndDirectoryLists(FileSelWidget,AFilterEntry.Mask);
      end;
    end
    else if (bevent <> nil)
    and (ofAllowMultiSelect in TOpenDialog(theDialog).Options)
    and (FileSelWidget^.file_list=widget) then begin
      // multi selection
      ShiftState := GTKEventStateToShiftState(BEvent^.State);
      if ssShift in ShiftState then begin
        if LastFileSelectRow <> -1 then begin
          startRow := LastFileSelectRow;
          endRow := row;
          if LastFileSelectRow > row then begin
            startRow := row;
            endRow := LastFileSelectRow;
          end;
          for loop := startRow to endRow do begin
            gtk_clist_select_row(PGtkCList(widget), loop, column);
          end;
        end;
      end
      else if not (ssCtrl in ShiftState) then begin
        gtk_clist_unselect_all(PGtkCList(widget));
        gtk_clist_select_row(PGtkCList(widget), row, column);
      end;
      LastFileSelectRow := row;
    end;
    UpdateDetailView(TOpenDialog(theDialog));
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogHelpclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever the user clicks the help button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogHelpclickedCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
var
  theDialog : TCommonDialog;
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  if theDialog is TOpenDialog then begin
    if TOpenDialog(theDialog).OnHelpClicked<>nil then
      TOpenDialog(theDialog).OnHelpClicked(theDialog);
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogApplyclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever the user clicks the Apply button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogApplyclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
  FontName: string;
  ALogFont: TLogFont;
  {$ifdef GTK2}
  FontDesc: PPangoFontDescription;
  {$endif}
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  if (theDialog is TFontDialog)
  and (fdApplyButton in TFontDialog(theDialog).Options)
  and (Assigned(TFontDialog(theDialog).OnApplyClicked)) then begin
    FontName := gtk_font_selection_dialog_get_font_name(
                                    pgtkfontselectiondialog(theDialog.Handle));
    if IsFontNameXLogicalFontDesc(FontName) then begin
      // extract basic font attributes from the font name in XLFD format
      ALogFont:=XLFDNameToLogFont(FontName);
      TFontDialog(theDialog).Font.Assign(ALogFont);
      // set the font name in XLFD format
      // a font name in XLFD format overrides in the gtk interface all other font
      // settings.
      TFontDialog(theDialog).Font.Name := FontName;
    end else begin
      {$ifdef GTK2}
      FontDesc := pango_font_description_from_string(PChar(FontName));
      with TFontDialog(theDialog).Font do
      begin
        BeginUpdate;
        Size := pango_font_description_get_size(FontDesc) div PANGO_SCALE;
        if pango_font_description_get_weight(FontDesc) >= PANGO_WEIGHT_BOLD then
          Style := Style + [fsBold]
        else
          Style := Style - [fsBold];
        if pango_font_description_get_style(FontDesc) > PANGO_STYLE_NORMAL then
          Style := Style + [fsItalic]
        else
          Style := Style - [fsItalic];
        Name := pango_font_description_get_family(FontDesc);
        EndUpdate;
      end;
      pango_font_description_free(FontDesc);
      {$else}
      debugln('Gtk1 should have valid XLogicalFontDesc!');
      {$endif}
    end;
    TFontDialog(theDialog).OnApplyClicked(theDialog);
  end;
end;

function gtkDialogOKclickedCB( widget: PGtkWidget; data: gPointer) : GBoolean; cdecl;
var
  theDialog : TCommonDialog;
  Fpointer : Pointer;
  // colordialog
  colorsel : PGtkColorSelection;
  newColor : TGdkColor;
  // fontdialog
  FontName : String;
  ALogFont  : TLogFont;
  // filedialog
  rowNum   : gint;
  fileInfo : PGChar;
  {$IfDef GTK2}
  fileList : PPgchar;
  FontDesc: PPangoFontDescription;
  {$else}
  cListRow : PGList;
  fileList : PGTKCList;
  {$EndIf}
  DirName  : string;
  FileName : string;
  Files: TStringList;
  CurFilename: string;
  //SelectedFont: PGdkFont;

  function CheckOpenedFilename(var AFilename: string): boolean;
  {$IFDEF GTK1}
  var
    MenuWidget: PGtkWidget;
    AFilterEntry: TFileSelFilterEntry;
  {$ENDIF}
  begin
    Result:=true;
    {$IFDEF GTK1}
    // check if entered file are a real directory
    if DirectoryExistsUTF8(AFileName) then
    begin
      // if only interested in directories, then it's done
      if TheDialog is TSelectDirectoryDialog then
      begin
        result := True;
        exit;
      end;
      // if allowed then jump to that directory
      if not (ofNoChangeDir in TOpenDialog(TheDialog).Options) then
      begin
        // change dir
        gtk_file_selection_set_filename(PGtkFileSelection(FPointer),
                                        PChar(AppendPathDelim(AFileName)));

        // populate file list
        MenuWidget := gtk_object_get_data(PGtkObject(PGtkFileSelection(FPointer)), 'LCLFilterMenu');
        if (MenuWidget <> nil) then begin
          AFilterEntry := TFileSelFilterEntry(gtk_object_get_data(PGtkObject(gtk_menu_get_active(
                                              PGtkMenu(MenuWidget))), 'LCLIsFilterMenuItem'));
          if ((AFilterEntry<>nil) and (AFilterEntry.Mask<>nil)) then
            PopulateFileAndDirectoryLists(PGtkFileSelection(FPointer), AFilterEntry.Mask);
        end;
      end;
      // wait for correct input
      result:=False;
      Exit;
    end;

    // maybe user entered nonexistent dir
    if ((AFileName<>'') and (IsPathDelimiter(AFileName, Length(AFileName)))) then
    begin
      // can not jump to nonexistent dir
      if not (ofNoChangeDir in TOpenDialog(TheDialog).Options) then
        MessageDlg(rsfdDirectoryMustExist, Format(rsfdDirectoryNotExist,[AFileName]),
                   mtError, [mbCancel], 0); // GTK2 shows "The folder contents could not be displayed"
      // wait for correct input
      result:=False;
      Exit;
    end;

    // check if user selected a file while requesting a directory
    if (AFileName<>'') and (TheDialog is TSelectDirectoryDialog) then begin
      if DirectoryExistsUTF8(ExtractFilePath(AFileName)) then begin
        AFileName:=ExtractFilePath(AFileName);
        result:=true;
        exit;
      end;
    end;

    {$ENDIF}
    // maybe file already exists
    if (ofOverwritePrompt in TOpenDialog(theDialog).Options)
    and FileExistsUTF8(AFilename) then
    begin
      Result := MessageDlg(rsfdOverwriteFile,
                         Format(rsfdFileAlreadyExists,[AFileName]),
                         mtConfirmation,[mbOk,mbCancel],0)=mrOk;
      if not Result then exit;
    end;
  end;

  procedure AddFile(List: TStrings; const NewFile: string);
  var
    i: Integer;
  begin
    for i:=0 to List.Count-1 do
      if List[i]=NewFile then exit;
    List.Add(NewFile);
  end;

begin
  Result := True;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  FPointer := Pointer(theDialog.Handle);

  if theDialog is TFileDialog then
  begin
    {$IfDef GTK2}
    FileName:=gtk_file_chooser_get_filename(PGtkFileChooser(FPointer));
    {$ELSE}
    FileName:=gtk_file_selection_get_filename(
                                PGtkFileSelection(FPointer));
    {$ENDIF}
    if theDialog is TOpenDialog then
    begin
      // check extra options
      if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
      begin
        DirName:=ExtractFilePath(FileName);
        TFileDialog(data).FileName := '';
        Files:=TStringList(TFileDialog(theDialog).Files);
        Files.Clear;
        if (Filename<>'') then begin
          Result:=CheckOpenedFilename(Filename);
          if not Result then exit;
          AddFile(Files,FileName);
        end;
        {$IfDef GTK2}
        fileList := gtk_file_selection_get_selections(PGtkFileSelection(FPointer));
        rowNum := 0;
        While FileList^ <> nil do
        begin
          fileInfo := FileList^;
          CurFilename:=fileInfo; // convert PChar to AnsiString (not typecast)
          if (CurFilename<>'') and (Files.IndexOf(CurFilename)<0) then begin
            CurFilename:=DirName+fileInfo;
            Result:=CheckOpenedFilename(CurFilename);
            if not Result then exit;
            Files.Add(CurFilename);
          end;
          inc(FileList);
          inc(rowNum);
        end;
        Dec(FileList, rowNum);
        g_strfreev(fileList);
        {$Else}
        fileList := PGtkCList(PGtkFileSelection(FPointer)^.file_list);
        rowNum := 0;
        cListRow := fileList^.row_list;
        while cListRow <> nil do
        begin
          if PGtkCListRow(cListRow^.data)^.state = GTK_STATE_SELECTED then
          begin
            if gtk_clist_get_cell_type(fileList, rowNum, 0) = GTK_CELL_TEXT
            then begin
              gtk_clist_get_text(fileList, rowNum, 0, @fileInfo);
              CurFilename:=DirName+fileInfo;
              Result:=CheckOpenedFilename(CurFilename);
              if not Result then exit;
              AddFile(Files,CurFilename);
            end;
          end;
          // get next row from list
          rowNum := rowNum + 1;
          cListRow := g_list_next(cListRow);
        end;
        {$EndIf}
      end else begin
        Result:=CheckOpenedFilename(Filename);
        if not Result then exit;
        TFileDialog(data).FileName := Filename;
      end;
    end
    else
    begin
      TFileDialog(data).FileName := Filename;
    end;
  end
  else if theDialog is TColorDialog then
  begin
    colorSel := PGtkColorSelection(PGtkColorSelectionDialog(FPointer)^.colorsel);
    gtk_color_selection_get_current_color(colorsel, @newColor);
    TColorDialog(theDialog).Color := TGDKColorToTColor(newcolor);
    {$IFDEF VerboseColorDialog}
    DebugLn('gtkDialogOKclickedCB ',DbgS(TColorDialog(theDialog).Color));
    {$ENDIF}
  end
  else if theDialog is TFontDialog then
  begin
    //DebugLn('Trace:Pressed OK in FontDialog');
    FontName := gtk_font_selection_dialog_get_font_name(
                                             pgtkfontselectiondialog(FPointer));
    //debugln('gtkDialogOKclickedCB FontName=',FontName);
    //SelectedFont:=gdk_font_load(PChar(FontName));
    //debugln('gtkDialogOKclickedCB ',dbgs(SelectedFont));

    if IsFontNameXLogicalFontDesc(FontName) then begin
      // extract basic font attributes from the font name in XLFD format
      ALogFont:=XLFDNameToLogFont(FontName);
      TFontDialog(theDialog).Font.Assign(ALogFont);
      // set the font name in XLFD format
      // a font name in XLFD format overrides in the gtk interface all other font
      // settings.
      TFontDialog(theDialog).Font.Name := FontName;
    end else begin
      {$ifdef GTK2}
      FontDesc := pango_font_description_from_string(PChar(FontName));
      with TFontDialog(theDialog).Font do
      begin
        BeginUpdate;
        Size := pango_font_description_get_size(FontDesc) div PANGO_SCALE;
        if pango_font_description_get_weight(FontDesc) >= PANGO_WEIGHT_BOLD then
          Style := Style + [fsBold]
        else
          Style := Style - [fsBold];
        if pango_font_description_get_style(FontDesc) > PANGO_STYLE_NORMAL then
          Style := Style + [fsItalic]
        else
          Style := Style - [fsItalic];
        Name := pango_font_description_get_family(FontDesc);
        EndUpdate;
      end;
      pango_font_description_free(FontDesc);
      {$else}
      debugln('Gtk1 should have valid XLogicalFontDesc!');
      {$endif}
    end;

    //DebugLn('Trace:-----'+TFontDialog(theDialog).Font.Name+'----');
  end;

  StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrOK;
end;

{-------------------------------------------------------------------------------
  function gtkDialogCancelclickedCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever the user clicks the cancel button in a
  commondialog
-------------------------------------------------------------------------------}
function gtkDialogCancelclickedCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
begin
  Result := CallBackDefaultReturn;
  if (Widget=nil) then ;
  theDialog := TCommonDialog(data);
  if theDialog is TFileDialog then
  begin
    TFileDialog(data).FileName := '';
  end;
  StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrCancel;
end;

{-------------------------------------------------------------------------------
  function GTKDialogRealizeCB
  Params: Widget: PGtkWidget; Data: Pointer
  Result: GBoolean

  This function is called, whenever a commondialog window is realized
-------------------------------------------------------------------------------}
function GTKDialogRealizeCB(Widget: PGtkWidget; Data: Pointer): GBoolean; cdecl;
var
  LCLComponent: TObject;
begin
  if (Data=nil) then ;
  gdk_window_set_events(GetControlWindow(Widget),
    gdk_window_get_events(GetControlWindow(Widget))
      or GDK_KEY_RELEASE_MASK or GDK_KEY_PRESS_MASK);
  LCLComponent:=GetLCLObject(Widget);
  if LCLComponent is TCommonDialog then
    TCommonDialog(LCLComponent).DoShow;
  Result:=true;
end;

{-------------------------------------------------------------------------------
  function gtkDialogCloseQueryCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, before a commondialog is destroyed
-------------------------------------------------------------------------------}
function gtkDialogCloseQueryCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog : TCommonDialog;
  CanClose: boolean;
begin
  Result := False; // true = do nothing, false = destroy or hide window
  if (Data=nil) then ;
  // data is not the commondialog. Get it manually.
  theDialog := TCommonDialog(GetLCLObject(Widget));
  if theDialog=nil then exit;
  if theDialog.OnCanClose<>nil then begin
    CanClose:=True;
    theDialog.OnCanClose(theDialog,CanClose);
    Result:=not CanClose;
  end;
  if not Result then begin
    StoreCommonDialogSetup(theDialog);
    DestroyCommonDialogAddOns(theDialog);
  end;
end;

{-------------------------------------------------------------------------------
  function gtkDialogDestroyCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, when a commondialog is destroyed
-------------------------------------------------------------------------------}
function gtkDialogDestroyCB(widget: PGtkWidget; data: gPointer): GBoolean; cdecl;
begin
  Result := True;
  if (Widget=nil) then ;
  TCommonDialog(data).UserChoice := mrAbort;
  TCommonDialog(data).Close;
end;

{-------------------------------------------------------------------------------
  function GTKDialogKeyUpDownCB
  Params: Widget: PGtkWidget; Event : pgdkeventkey; Data: gPointer
  Result: GBoolean

  This function is called, whenever a key is pressed or released in a common
  dialog window
-------------------------------------------------------------------------------}
function GTKDialogKeyUpDownCB(Widget: PGtkWidget; Event : pgdkeventkey;
  Data: gPointer) : GBoolean; cdecl;
begin
  //debugln('GTKDialogKeyUpDownCB A ');
  Result:=CallBackDefaultReturn;

  if (Widget=nil) then ;
  case gdk_event_get_type(Event) of

  GDK_KEY_RELEASE, GDK_KEY_PRESS:
    begin
      if Event^.KeyVal = GDK_KEY_Escape
      then begin
        StoreCommonDialogSetup(TCommonDialog(data));
        TCommonDialog(data).UserChoice:=mrCancel;
      end;
      if (TCommonDialog(data) is TOpenDialog) then begin
        UpdateDetailView(TOpenDialog(data));
      end;
    end;

  end;
end;

{-------------------------------------------------------------------------------
  function GTKDialogFocusInCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, when a widget of a commondialog gets focus
-------------------------------------------------------------------------------}
function GTKDialogFocusInCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog: TCommonDialog;
begin
  //debugln('GTKDialogFocusInCB A ');
  Result:=CallBackDefaultReturn;
  if (Data=nil) then ;
  theDialog:=TCommonDialog(GetLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    UpdateDetailView(TOpenDialog(theDialog));
  end;
end;

{-------------------------------------------------------------------------------
  function GTKDialogMenuActivateCB
  Params: widget: PGtkWidget; data: gPointer
  Result: GBoolean

  This function is called, whenever a menu of a commondialog is activated
-------------------------------------------------------------------------------}
function GTKDialogMenuActivateCB(widget: PGtkWidget; data: gPointer): GBoolean;
  cdecl;
var
  theDialog: TCommonDialog;

  procedure CheckFilterActivated(FilterWidget: PGtkWidget);
  var
    AFilterEntry: TFileSelFilterEntry;
  begin
    if FilterWidget=nil then exit;
    AFilterEntry:=TFileSelFilterEntry(gtk_object_get_data(PGtkObject(FilterWidget),
                                      'LCLIsFilterMenuItem'));
    if (AFilterEntry<>nil) and (AFilterEntry.Mask<>nil) then
    begin
      PopulateFileAndDirectoryLists(PGtkFileSelection(theDialog.Handle),
        AFilterEntry.Mask);
      TFileDialog(TheDialog).IntfFileTypeChanged(AFilterEntry.FilterIndex + 1);
      UpdateDetailView(TOpenDialog(theDialog));
    end;
  end;

var
  AHistoryEntry: PFileSelHistoryEntry;
  {$IFDEF Gtk1}
  FilterMenu, ActiveFilterMenuItem: PGtkWidget;
  {$ENDIF}
begin
  Result:=false;
  if (Data=nil) then ;
  theDialog:=TCommonDialog(GetNearestLCLObject(Widget));
  if (theDialog is TOpenDialog) then begin
    // check if history activated
    AHistoryEntry:=gtk_object_get_data(PGtkObject(Widget),
                                       'LCLIsHistoryMenuItem');
    if (AHistoryEntry<>nil) and (AHistoryEntry^.Filename<>nil) then begin
      // user has choosen a history file
      // -> select it in the filedialog
      {$IFDEF GTK1}
      gtk_file_selection_complete(PGtkFileSelection(theDialog.Handle),
        AHistoryEntry^.Filename);
      // restore filter
      if DirPathExists(AHistoryEntry^.Filename) then begin
        FilterMenu:=gtk_object_get_data(PGtkObject(theDialog.Handle),
                                        'LCLFilterMenu');
        if FilterMenu<>nil then begin
          ActiveFilterMenuItem:=gtk_menu_get_active(GTK_MENU(FilterMenu));
          CheckFilterActivated(ActiveFilterMenuItem);
        end;
      end;
      {$ELSE}
      gtk_file_chooser_set_current_folder(PGtkFileChooser(theDialog.Handle),AHistoryEntry^.Filename);
      {$ENDIF}
      UpdateDetailView(TOpenDialog(theDialog));
    end;
    {$IFDEF GTK1}
    // check if filter activated
    CheckFilterActivated(Widget);
    {$ENDIF}
  end;
end;

// -------------------- END OF Signals -----------------------------------------

{------------------------------------------------------------------------------
  Method: SetColorDialogColor
  Params:  ColorSelection : a gtk color selection dialog;
           Color          : the color to select
  Returns: nothing

  Set the color of the color selection dialog
 ------------------------------------------------------------------------------}
procedure SetColorDialogColor(ColorSelection: PGtkColorSelection; aColor: TColor);
var
  SelectionColor: TGDKColor;
  colorSel : PGTKCOLORSELECTION;
  ColorRef: TColorRef;
begin
  ColorRef:=ColorToRGB(aColor);
  {$IFDEF VerboseColorDialog}
  DebugLn('TGtkWidgetSet.SetColorDialogColor Start aColor=',DbgS(aColor));
  DebugLn('TGtkWidgetSet.SetColorDialogColor Converted aColor=',DbgS(ColorRef));
  {$ENDIF}
  SelectionColor.Pixel := 0;
  SelectionColor.Red :=  Red(ColorRef) shl 8;
  SelectionColor.Green:= Green(ColorRef) shl 8;
  SelectionColor.Blue:= Blue(ColorRef) shl 8;
  colorSel := PGTKCOLORSELECTION((PGTKCOLORSELECTIONDIALOG(ColorSelection))^.colorsel);
  gtk_color_selection_set_current_color(colorSel,@SelectionColor);
end;

class procedure TGtkWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  GtkWindow: PGtkWindow;
  {$IFDEF Gtk1}
  {$IFDEF UseXinerama}
  Requisition: TGtkRequisition;
  {$ENDIF}
  {$ENDIF}
begin
  ReleaseMouseCapture;
  GtkWindow:=PGtkWindow(ACommonDialog.Handle);
  gtk_window_set_title(GtkWindow,PChar(ACommonDialog.Title));
  if ACommonDialog is TColorDialog then
    SetColorDialogColor(PGtkColorSelection(GtkWindow),
                        TColorDialog(ACommonDialog).Color);
  {$IFDEF Gtk1}
  {$IFDEF UseXinerama}
  if GetFirstScreen then begin
    { Fix multi screen problems, at least partially by forcing dialog to centre of first screen }
    gtk_widget_size_request(PGtkWidget(GtkWindow), @Requisition);
    gtk_widget_set_uposition(PGtkWidget(GtkWindow), (FirstScreen.x - Requisition.width) div 2,
                                                    (FirstScreen.y - Requisition.height) div 2);
  end else
  {$ENDIF}
  {$ENDIF}
  gtk_window_set_position(GtkWindow, GTK_WIN_POS_CENTER);
  GtkWindowShowModal(GtkWindow);
end;

class procedure TGtkWSCommonDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  g_signal_connect(PGtkObject(AGtkWidget),
    'destroy', gtk_Signal_Func(@gtkDialogDestroyCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(AGtkWidget),
    'delete-event', gtk_Signal_Func(@gtkDialogCloseQueryCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(AGtkWidget),
    'key-press-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(AGtkWidget),
    'key-release-event', gtk_Signal_Func(@GTKDialogKeyUpDownCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(AGtkWidget),
    'realize', gtk_Signal_Func(@GTKDialogRealizeCB), AWidgetInfo^.LCLObject);
end;

class procedure TGtkWSCommonDialog.SetSizes(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
var
  NewWidth, NewHeight: integer;
begin
  // set default size
  NewWidth := TCommonDialog(AWidgetInfo^.LCLObject).Width;
  if NewWidth <= 0 then
    NewWidth := -2; // -2 = let the window manager decide
  NewHeight := TCommonDialog(AWidgetInfo^.LCLObject).Height;
  if NewHeight<=0 then
    NewHeight := -2; // -2 = let the window manager decide
  if (NewWidth > 0) or (NewHeight > 0) then
    gtk_window_set_default_size(PGtkWindow(AGtkWidget), NewWidth, NewHeight);
end;

class function TGtkWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

class procedure TGtkWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  { TODO: cleanup }
  TGtkWidgetSet(WidgetSet).DestroyLCLComponent(ACommonDialog);
end;

{ TGtkWSColorDialog }

class procedure TGtkWSColorDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  g_signal_connect(PGtkObject(PGtkColorSelectionDialog(AGtkWidget)^.ok_button),
    'clicked', gtk_signal_func(@gtkDialogOKclickedCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(PGtkColorSelectionDialog(AGtkWidget)^.cancel_button),
    'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), AWidgetInfo^.LCLObject);
end;

class function TGtkWSColorDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_color_selection_dialog_new(PChar(ACommonDialog.Title));
  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtkWSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

{ TGtkWSFileDialog }

class procedure TGtkWSFileDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  {****This is a major hack put by Cliff Baeseman to solve
   a gtk win32 dll implementation problem where the headers implementation
   does not match the linux version**** }
  {$IFNDEF WIN32}
    g_signal_connect(gtk_object(PGtkFileSelection(AGtkWidget)^.ok_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), AWidgetInfo^.LCLObject);
    g_signal_connect(gtk_object(PGtkFileSelection(AGtkWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), AWidgetInfo^.LCLObject);
  {$ELSE}
    g_signal_connect(gtk_object(PGtkFileSelection(AGtkWidget)^.cancel_button),
      'clicked', gtk_signal_func(@gtkDialogOKclickedCB), AWidgetInfo^.LCLObject);
    g_signal_connect(gtk_object(PGtkFileSelection(AGtkWidget)^.help_button),
      'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), AWidgetInfo^.LCLObject);
  {$ENDIF}
end;

class function TGtkWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // for: csFileDialog, csOpenFileDialog, csSaveFileDialog, csSelectDirectoryDialog, csPreviewFileDialog
  Widget := gtk_file_selection_new(PChar(ACommonDialog.Title));

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtkWSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

{ TGtkWSOpenDialog }

{------------------------------------------------------------------------------
  Function: TGtkWidgetSet.CreateOpenDialogFilter
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a Filter pulldown to a gtk file selection dialog. Returns the
  inital filter mask.
 ------------------------------------------------------------------------------}
class function TGtkWSOpenDialog.CreateOpenDialogFilter(OpenDialog: TOpenDialog;
  SelWidget: PGtkWidget): string;
var
  FilterList: TFPList;
  HBox, LabelWidget, FilterPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
  i, j, CurMask: integer;
  s: String;
begin
  ExtractFilterList(OpenDialog.Filter,FilterList,false);
  if FilterList.Count>0 then begin

    // create a HBox so that the filter pulldown is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterHBox', HBox);
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);

    // create the label 'Filter:'
    s:=rsgtkFilter;
    LabelWidget:=gtk_label_new(PChar(s));
    gtk_box_pack_start(GTK_BOX(HBox),LabelWidget,false,false,5);
    gtk_widget_show(LabelWidget);

    // create the pull down
    FilterPullDownWidget:=gtk_option_menu_new;
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterPullDown',
      FilterPullDownWidget);
    gtk_box_pack_start(GTK_BOX(HBox),FilterPullDownWidget,false,false,5);
    gtk_widget_show(FilterPullDownWidget);
    gtk_widget_show(HBox);

    // create the menu (the content of the pull down)
    MenuWidget:=gtk_menu_new;
    SetLCLObject(MenuWidget,OpenDialog);
    for i:=0 to FilterList.Count-1 do begin
      // create the menu items in the filter menu
      MenuItemWidget:=gtk_menu_item_new_with_label(
                               TFileSelFilterEntry(FilterList[i]).Description);
      // connect the new MenuItem to the FilterList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsFilterMenuItem',
        FilterList[i]);
      // add activation signal and add to menu
      g_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(MenuWidget, MenuItemWidget);
      gtk_widget_show(MenuItemWidget);
    end;
    gtk_widget_show(MenuWidget);
    gtk_option_menu_set_menu(GTK_OPTION_MENU(FilterPullDownWidget),
                             MenuWidget);
  end else begin
    MenuWidget:=nil;
  end;
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterMenu', MenuWidget);
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterList', FilterList);

  // set the initial filter
  Result := 'none'; { Don't use '' as null return as this is used for *.* }
  if FilterList.Count>0 then begin
    i:=0;
    j:=OpenDialog.FilterIndex - 1; // FilterIndex is 1 based
    if j<0 then j:=0;
    CurMask:=0;
    while (i<FilterList.Count) do begin
      if TFileSelFilterEntry(FilterList[i]).FilterIndex=j
      then begin
        CurMask:=i;
        break;
      end;
      inc(i);
    end;
    Result := TFileSelFilterEntry(FilterList[CurMask]).Mask;
    gtk_option_menu_set_history(GTK_OPTION_MENU(FilterPullDownWidget), CurMask);
  end;
end;

{------------------------------------------------------------------------------
  Function: TGtkWSOpenDialog.CreatePreviewDialogControl
  Params: PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a preview control to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
class procedure TGtkWSOpenDialog.CreatePreviewDialogControl(
  PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget);
var
  PreviewWidget: PGtkWidget;
  list_hbox: PGtkWidget;
  DirListWidget: PGtkWidget;
  ScrolledWin: PGtkWidget;
  AControl: TPreviewFileControl;
begin
  AControl:=PreviewDialog.PreviewFileControl;
  if AControl=nil then exit;
  // find the hbox widget of the file and directory dialog
  DirListWidget:=PGtkFileSelection(SelWidget)^.dir_list;
  ScrolledWin:=DirListWidget^.parent;
  if not GtkWidgetIsA(ScrolledWin, GTK_TYPE_SCROLLED_WINDOW) then
  begin
    DebugLn('NOTE: CreatePreviewDialogControl ',
      'parent widget of dir_list widget is not a scrolled window');
    exit;
  end;
  list_hbox:=ScrolledWin^.parent;
  if not GtkWidgetIsA(list_hbox,GTK_TYPE_HBOX) then begin
    DebugLn('NOTE: CreatePreviewDialogControl ',
      'parent widget of scrolled window is not a hbox');
    exit;
  end;
  // create the preview widget
  PreviewWidget:=PGtkWidget(AControl.Handle);
  gtk_object_set_data(PGtkObject(PreviewWidget),'LCLPreviewFixed',
                      PreviewWidget);
  gtk_widget_set_usize(PreviewWidget,AControl.Width,AControl.Height);
  gtk_box_pack_start(GTK_BOX(list_hbox),PreviewWidget,true,true,0);
  gtk_widget_show(PreviewWidget);
end;

{------------------------------------------------------------------------------
  Function: CreateOpenDialogHistory
  Params: OpenDialog: TOpenDialog; SelWidget: PGtkWidget
  Returns: -

  Adds a History pulldown to a gtk file selection dialog.
 ------------------------------------------------------------------------------}
class procedure TGtkWSOpenDialog.CreateOpenDialogHistory(OpenDialog: TOpenDialog; SelWidget: PGtkWidget);
var
  HistoryList: TFPList; // list of THistoryListEntry
  AHistoryEntry: PFileSelHistoryEntry;
  i: integer;
  s: string;
  HBox, LabelWidget, HistoryPullDownWidget,
  MenuWidget, MenuItemWidget: PGtkWidget;
begin
  if OpenDialog.HistoryList.Count>0 then begin

    // create the HistoryList where the current state of the history is stored
    HistoryList:=TFPList.Create;
    for i:=0 to OpenDialog.HistoryList.Count-1 do begin
      s:=OpenDialog.HistoryList[i];
      if s<>'' then begin
        New(AHistoryEntry);
        HistoryList.Add(AHistoryEntry);
        AHistoryEntry^.Filename := StrAlloc(length(s)+1);
        StrPCopy(AHistoryEntry^.Filename, s);
        AHistoryEntry^.MenuItem:=nil;
      end;
    end;

    // create a HBox so that the history is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryHBox', HBox);
    {$IFDEF GTK1}
    gtk_box_pack_start(GTK_BOX(GTK_FILE_SELECTION(SelWidget)^.main_vbox),
                       HBox,false,false,0);
    {$ELSE}
    gtk_file_chooser_set_extra_widget(PGtkDialog(SelWidget),HBox);
    {$ENDIF}

    // create the label 'History:'
    s:=rsgtkHistory;
    LabelWidget:=gtk_label_new(PChar(s));
    gtk_box_pack_start(GTK_BOX(HBox),LabelWidget,false,false,5);
    gtk_widget_show(LabelWidget);

    // create the pull down
    HistoryPullDownWidget:=gtk_option_menu_new;
    gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryPullDown',
      HistoryPullDownWidget);
    gtk_box_pack_start(GTK_BOX(HBox),HistoryPullDownWidget,false,false,5);
    gtk_widget_show(HistoryPullDownWidget);
    gtk_widget_show_all(HBox);

    // create the menu (the content of the pull down)
    MenuWidget:=gtk_menu_new;
    SetLCLObject(MenuWidget,OpenDialog);
    for i:=0 to HistoryList.Count-1 do begin
      // create the menu items in the history menu
      MenuItemWidget:=gtk_menu_item_new_with_label(
                                PFileSelHistoryEntry(HistoryList[i])^.Filename);
      // connect the new MenuItem to the HistoryList entry
      gtk_object_set_data(PGtkObject(MenuItemWidget), 'LCLIsHistoryMenuItem',
        HistoryList[i]);
      // add activation signal and add to menu
      g_signal_connect(GTK_OBJECT(MenuItemWidget), 'activate',
                         gtk_signal_func(@GTKDialogMenuActivateCB),
                         OpenDialog);
      gtk_menu_append(MenuWidget, MenuItemWidget);
      gtk_widget_show(MenuItemWidget);
    end;
    gtk_widget_show(MenuWidget);
    gtk_option_menu_set_menu(GTK_OPTION_MENU(HistoryPullDownWidget),
                             MenuWidget);
  end else begin
    MenuWidget:=nil;
    HistoryList:=nil
  end;
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryMenu', MenuWidget);
  gtk_object_set_data(PGtkObject(SelWidget), 'LCLHistoryList', HistoryList);
end;

class function TGtkWSOpenDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  SelWidget: PGtkWidget;
  OpenDialog: TOpenDialog absolute ACommonDialog;
  
  FileDetailLabel, HBox, FrameWidget: PGtkWidget;
  FileSelWidget: PGtkFileSelection;
  InitialFilter: string;
begin
  Result := TGtkWSFileDialog.CreateHandle(ACommonDialog);
  SelWidget := PGtkWidget(Result);
  
  //DebugLn(['TGtkWidgetSet.InitializeOpenDialog ']);
  FileSelWidget := GTK_FILE_SELECTION(SelWidget);

  // Help button
  if (ofShowHelp in OpenDialog.Options)
  and (FileSelWidget^.Help_Button<>nil) then begin
    gtk_widget_show(FileSelWidget^.Help_Button);
    g_signal_connect( gtk_object(FileSelWidget^.help_button),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  // connect selection entry (edit field for filename)
  if (FileSelWidget^.selection_entry<>nil) then begin
    SetLCLObject(FileSelWidget^.selection_entry,OpenDialog);
    g_signal_connect(
      gtk_object(FileSelWidget^.selection_entry),
      'key-press-event', gtk_signal_func(@GTKDialogKeyUpDownCB),
      OpenDialog);
    g_signal_connect(
      gtk_object(FileSelWidget^.selection_entry),
      'focus-in-event', gtk_signal_func(@GTKDialogFocusInCB), OpenDialog);
  end;

  // connect dir list (list of directories)
  if (FileSelWidget^.dir_list<>nil) then begin
    SetLCLObject(FileSelWidget^.dir_list,OpenDialog);
    g_signal_connect(gtk_object(FileSelWidget^.dir_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
  end;

  // connect file list (list of files in current directory)
  if (FileSelWidget^.file_list<>nil) then begin
    LastFileSelectRow := -1;
    SetLCLObject(FileSelWidget^.file_list,OpenDialog);
    g_signal_connect(gtk_object(FileSelWidget^.file_list),
      'select-row', gtk_signal_func(@GTKDialogSelectRowCB), OpenDialog);
    if ofAllowMultiSelect in OpenDialog.Options then
      gtk_clist_set_selection_mode(
        PGtkCList(FileSelWidget^.file_list),GTK_SELECTION_MULTIPLE);
  end;

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog, SelWidget);

  // Filter - a frame with an option menu
  InitialFilter := CreateOpenDialogFilter(OpenDialog,SelWidget);

  // Details - a frame with a label
  if (ofViewDetail in OpenDialog.Options) then begin
    // create the frame around the information
    FrameWidget:=gtk_frame_new(PChar(rsFileInformation));
    gtk_box_pack_start(GTK_BOX(FileSelWidget^.main_vbox),
                       FrameWidget,false,false,0);
    gtk_widget_show(FrameWidget);
    // create a HBox, so that the information is left justified
    HBox:=gtk_hbox_new(false,0);
    gtk_container_add(GTK_CONTAINER(FrameWidget), HBox);
    // create the label for the file information
    FileDetailLabel:=gtk_label_new(PChar(rsDefaultFileInfoValue));
    gtk_box_pack_start(GTK_BOX(HBox),FileDetailLabel,false,false,5);
    gtk_widget_show_all(HBox);
  end else
    FileDetailLabel:=nil;
  gtk_object_set_data(PGtkObject(SelWidget), 'FileDetailLabel',
                      FileDetailLabel);

  // preview
  if (OpenDialog is TPreviewFileDialog) then
    CreatePreviewDialogControl(TPreviewFileDialog(OpenDialog),SelWidget);

  // set initial filename
  if OpenDialog.Filename<>'' then
    gtk_file_selection_set_filename(FileSelWidget,PChar(OpenDialog.Filename));

  if InitialFilter <> 'none' then
    PopulateFileAndDirectoryLists(FileSelWidget, InitialFilter);
end;

{ TGtkWSFontDialog }

class procedure TGtkWSFontDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  // connect Ok, Cancel and Apply Button
  g_signal_connect(
    PGtkObject(PGtkFontSelectionDialog(AGtkWidget)^.ok_button),
    'clicked', gtk_signal_func(@gtkDialogOKclickedCB), AWidgetInfo^.LCLObject);
  g_signal_connect(
    PGtkObject(PGtkFontSelectionDialog(AGtkWidget)^.cancel_button),
    'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), AWidgetInfo^.LCLObject);
  g_signal_connect(
    PGtkObject(PGtkFontSelectionDialog(AGtkWidget)^.apply_button),
    'clicked', gtk_signal_func(@gtkDialogApplyclickedCB), AWidgetInfo^.LCLObject);
end;

class function TGtkWSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
{$IFDEF GTK1}
const
  FixedFilter: array [0..2] of PChar = ( 'c', 'm', nil );
{$ENDIF}

var
{$IFDEF GTK1}
  SpacingFilter: PPgchar;
  FontType: TGtkFontType;
{$ELSE}
  FontDesc: PPangoFontDescription;
  TmpStr: pChar;
{$ENDIF}
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  FontDialog: TFontDialog absolute ACommonDialog;
begin
  Widget := gtk_font_selection_dialog_new(PChar(ACommonDialog.Title));

  if fdApplyButton in FontDialog.Options then
    gtk_widget_show(PGtkFontSelectionDialog(Widget)^.apply_button);
  // set preview text
  if FontDialog.PreviewText <> '' then
    gtk_font_selection_dialog_set_preview_text(PGtkFontSelectionDialog(Widget),
      PChar(FontDialog.PreviewText));

  // set font name in XLFD format
  if IsFontNameXLogicalFontDesc(FontDialog.Font.Name) then
    gtk_font_selection_dialog_set_font_name(PGtkFontSelectionDialog(Widget),
      PChar(FontDialog.Font.Name))
  else
  begin
    {$IFDEF GTK1}
    DebugLn('Gtk1 should have IsFontNameXLogicalFontDesc true!');
    {$ELSE}
    FontDesc := pango_font_description_new;
    with FontDialog.Font do
    begin
      pango_font_description_set_size(FontDesc, Size * PANGO_SCALE);

      if fsBold in Style then
        pango_font_description_set_weight(FontDesc, PANGO_WEIGHT_BOLD)
      else
        pango_font_description_set_weight(FontDesc, PANGO_WEIGHT_NORMAL);

      if fsItalic in Style then
        pango_font_description_set_style(FontDesc, PANGO_STYLE_ITALIC)
      else
        pango_font_description_set_style(FontDesc, PANGO_STYLE_NORMAL);

      pango_font_description_set_family(FontDesc, PChar(Name));
    end;
    TmpStr := pango_font_description_to_string(FontDesc);
    gtk_font_selection_dialog_set_font_name(PGtkFontSelectionDialog(Widget), TmpStr);
    g_free(TmpStr);
    pango_font_description_free(FontDesc);
    {$ENDIF}
  end;

  {$IFDEF GTK1}
  { This functionality does not seem to be available in GTK2 }
  // Honor selected TFontDialogOption flags
  SpacingFilter := nil;
  if fdFixedPitchOnly in FontDialog.Options then
    SpacingFilter := @FixedFilter[0];
  FontType := GTK_FONT_ALL;
  if fdScalableOnly in FontDialog.Options then
    FontType := GTK_FONT_SCALABLE;
  gtk_font_selection_dialog_set_filter(PGtkFontSelectionDialog(Widget),
                                       GTK_FONT_FILTER_BASE, FontType,
                                       nil, nil, nil, nil, SpacingFilter, nil);
  {$ENDIF}

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtkWSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

end.
