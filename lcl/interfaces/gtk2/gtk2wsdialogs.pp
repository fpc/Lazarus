{ $Id$}
{
 *****************************************************************************
 *                             Gtk2WSDialogs.pp                              * 
 *                             ----------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Gtk2WSDialogs;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  Gtk2, Glib2, gdk2, pango, Gtk2Extra,
  // RTL, FCL and LCL
  SysUtils, Classes, Graphics, Controls, Dialogs, ExtDlgs, LCLType,
  FileUtil, LCLStrConsts, LCLProc, InterfaceBase,
  // Widgetset
  Gtk2Int, Gtk2WSControls, Gtk2Globals, Gtk2Def, Gtk2Proc,
  WSDialogs;
  
type
  { TGtk2WSCommonDialog }

  TGtk2WSCommonDialog = class(TWSCommonDialog)
  private
    class procedure SetColorDialogColor(ColorSelection: PGtkColorSelectionDialog; Color: TColor);
    class procedure SetColorDialogPalette(ColorSelection: PGtkColorSelectionDialog; Palette: TStrings);
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
    class procedure SetSizes(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TGtk2WSFileDialog }

  TGtk2WSFileDialog = class(TWSFileDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtk2WSOpenDialog }

  TGtk2WSOpenDialog = class(TWSOpenDialog)
  protected
    class function CreateOpenDialogFilter(OpenDialog: TOpenDialog; SelWidget: PGtkWidget): string; virtual;
    class procedure CreateOpenDialogHistory(OpenDialog: TOpenDialog; SelWidget: PGtkWidget); virtual;
    class procedure CreatePreviewDialogControl(PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtk2WSSaveDialog }

  TGtk2WSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TGtk2WSSelectDirectoryDialog }

  TGtk2WSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
  end;

  { TGtk2WSColorDialog }

  TGtk2WSColorDialog = class(TWSColorDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TGtk2WSColorButton }

  TGtk2WSColorButton = class(TWSColorButton)
  published
  end;

  { TGtk2WSFontDialog }

  TGtk2WSFontDialog = class(TWSFontDialog)
  protected
    class procedure SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo); virtual;
  published
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

// forward declarations

procedure UpdateDetailView(OpenDialog: TOpenDialog);

implementation

{$I gtk2defines.inc}

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
  FileName := gtk_file_chooser_get_filename(PGtkFileChooser(Widget));

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

// ---------------------- signals ----------------------------------------------

procedure gtkFileChooserSelectionChangedCB(Chooser: PGtkFileChooser;
  Data: Pointer); cdecl;
var
  cFilename: Pgchar;
  theDialog: TFileDialog;
begin
  //DebugLn(['gtkFileChooserSelectionChangedCB ']);
  cFilename := gtk_file_chooser_get_filename(Chooser);
  theDialog:=TFileDialog(Data);
  try
    if theDialog is TOpenDialog then
      UpdateDetailView(TOpenDialog(theDialog));
  finally
    if Assigned(cFilename) then
      g_free(cFilename);
  end;
end;

procedure Gtk2FileChooserResponseCB(widget: PGtkFileChooser; arg1: gint;
  data: gpointer); cdecl;

  procedure AddFile(List: TStrings; const NewFile: string);
  var
    i: Integer;
  begin
    for i := 0 to List.Count-1 do
      if List[i] = NewFile then
        Exit;
    List.Add(NewFile);
  end;

var
  TheDialog: TFileDialog;
  cFilename: PChar;
  cFilenames: PGSList;
  cFilenames1: PGSList;
  Files: TStringList;
begin
  //DebugLn(['Gtk2FileChooserResponseCB ']);
  theDialog := TFileDialog(data);

  if arg1 = GTK_RESPONSE_CANCEL then
  begin
    TheDialog.UserChoice := mrCancel;
    Exit;
  end;

  if theDialog is TOpenDialog then
  begin
    if ofAllowMultiSelect in TOpenDialog(theDialog).Options then
    begin
      TheDialog.FileName := '';
      Files := TStringList(TheDialog.Files);
      Files.Clear;
      cFilenames := gtk_file_chooser_get_filenames(widget);
      if Assigned(cFilenames) then
      begin
        cFilenames1 := cFilenames;
        while Assigned(cFilenames1) do
        begin
          cFilename := PChar(cFilenames1^.data);
          if Assigned(cFilename) then
          begin
            AddFile(Files, cFilename);
            g_free(cFilename);
          end;
          cFilenames1 := cFilenames1^.next;
        end;
        g_slist_free(cFilenames);
      end;
    end;
  end;

  cFilename := gtk_file_chooser_get_filename(widget);
  if Assigned(cFilename) then
  begin
    TheDialog.FileName := cFilename;
    g_free(cFilename);
  end;

  //?? StoreCommonDialogSetup(theDialog);
  theDialog.UserChoice := mrOK;
end;

procedure Gtk2FileChooserNotifyCB(dialog: PGObject; pspec: PGParamSpec;
  user_data: gpointer); cdecl;
var
  TheDialog: TFileDialog;
  GtkFilter: PGtkFileFilter;
  GtkFilterList: PGSList;
  NewFilterIndex: Integer;
begin
  //DebugLn(['Gtk2FileChooserNotifyCB ']);
  if pspec^.name = 'filter' then
  begin // filter changed
    theDialog := TFileDialog(user_data);
    GtkFilter := gtk_file_chooser_get_filter(dialog);
    GtkFilterList := gtk_file_chooser_list_filters(dialog);
    if (GtkFilter = nil) and (theDialog.Filter <> '') then
    begin
      // Either we don't have filter or gtk reset it.
      // Gtk resets filter if we set both filename and filter but filename
      // does not fit into filter. Gtk comparision has bug - it compares only by
      // mime-type, not by pattern. LCL set all filters by pattern.
      GtkFilter := g_slist_nth_data(GtkFilterList, theDialog.FilterIndex - 1);
      gtk_file_chooser_set_filter(dialog, GtkFilter);
    end
    else
    begin
      NewFilterIndex := g_slist_index(GtkFilterList, GtkFilter);
      theDialog.IntfFileTypeChanged(NewFilterIndex + 1);
    end;
    g_slist_free(GtkFilterList);
  end;
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

  FontDesc: PPangoFontDescription;
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

  fileList : PPgchar;
  FontDesc: PPangoFontDescription;

  DirName  : string;
  FileName : string;
  Files: TStringList;
  CurFilename: string;
  //SelectedFont: PGdkFont;

  function CheckOpenedFilename(var AFilename: string): boolean;
  begin
    Result:=true;

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
    FileName:=gtk_file_chooser_get_filename(PGtkFileChooser(FPointer));

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
      end
      else
      begin
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
    Assert(False, 'Trace:Pressed OK in FontDialog');
    FontName := gtk_font_selection_dialog_get_font_name(
                                             pgtkfontselectiondialog(FPointer));
    //debugln('gtkDialogOKclickedCB FontName=',FontName);
    //SelectedFont:=gdk_font_load(PChar(FontName));
    //debugln('gtkDialogOKclickedCB ',dbgs(SelectedFont));

    if IsFontNameXLogicalFontDesc(FontName) then
    begin
      // extract basic font attributes from the font name in XLFD format
      ALogFont:=XLFDNameToLogFont(FontName);
      TFontDialog(theDialog).Font.Assign(ALogFont);
      // set the font name in XLFD format
      // a font name in XLFD format overrides in the gtk interface all other font
      // settings.
      TFontDialog(theDialog).Font.Name := FontName;
    end
    else
    begin
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
    end;

    Assert(False, 'Trace:-----'+TFontDialog(theDialog).Font.Name+'----');
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
      gtk_file_chooser_set_current_folder(PGtkFileChooser(theDialog.Handle),AHistoryEntry^.Filename);

      UpdateDetailView(TOpenDialog(theDialog));
    end;
  end;
end;

// ---------------------- END OF signals ---------------------------------------

{ TGtk2WSOpenDialog }

class function TGtk2WSOpenDialog.CreateOpenDialogFilter(
  OpenDialog: TOpenDialog; SelWidget: PGtkWidget): string;
var
  ListOfFileSelFilterEntry: TFPList;
  i, j, k: integer;
  GtkFilter, GtkSelFilter: PGtkFileFilter;
  MaskList: TStringList;
  FilterEntry: TFileSelFilterEntry;
  FilterIndex: Integer;
begin
  FilterIndex := OpenDialog.FilterIndex;
  ExtractFilterList(OpenDialog.Filter, ListOfFileSelFilterEntry, false);
  GtkSelFilter := nil;
  if ListOfFileSelFilterEntry.Count > 0 then
  begin
    j := 1;
    MaskList := TStringList.Create;
    MaskList.Delimiter := ';';
    for i := 0 to ListOfFileSelFilterEntry.Count-1 do
    begin
      GtkFilter := gtk_file_filter_new();

      FilterEntry := TFileSelFilterEntry(ListOfFileSelFilterEntry[i]);
      MaskList.DelimitedText := FilterEntry.Mask;

      for k := 0 to MaskList.Count - 1 do
        gtk_file_filter_add_pattern(GtkFilter, PChar(MaskList.Strings[k]));

      gtk_file_filter_set_name(GtkFilter, FilterEntry.Description);

      gtk_file_chooser_add_filter(SelWidget, GtkFilter);

      if j = FilterIndex then
        GtkSelFilter := GtkFilter;

      Inc(j);
      GtkFilter := nil;
    end;
    MaskList.Free;
  end;

  FreeListOfFileSelFilterEntry(ListOfFileSelFilterEntry);
  //gtk_object_set_data(PGtkObject(SelWidget), 'LCLFilterList', ListOfFileSelFilterEntry);

  if GtkSelFilter <> nil then
    gtk_file_chooser_set_filter(SelWidget, GtkSelFilter);

  Result := 'hm'; { Don't use '' as null return as this is used for *.* }
end;

class procedure TGtk2WSOpenDialog.CreateOpenDialogHistory(
  OpenDialog: TOpenDialog; SelWidget: PGtkWidget);
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
    gtk_file_chooser_set_extra_widget(PGtkDialog(SelWidget),HBox);

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

class procedure TGtk2WSOpenDialog.CreatePreviewDialogControl(
  PreviewDialog: TPreviewFileDialog; SelWidget: PGtkWidget);
var
  PreviewWidget: PGtkWidget;
  AControl: TPreviewFileControl;
  FileChooser: PGtkFileChooser;
begin
  AControl := PreviewDialog.PreviewFileControl;
  if AControl = nil then Exit;

  FileChooser := PGtkFileChooser(SelWidget);

  PreviewWidget := PGtkWidget(AControl.Handle);

  gtk_object_set_data(PGtkObject(PreviewWidget),'LCLPreviewFixed',
                      PreviewWidget);
  gtk_widget_set_size_request(PreviewWidget,AControl.Width,AControl.Height);

  gtk_file_chooser_set_preview_widget(FileChooser, PreviewWidget);

  gtk_widget_show(PreviewWidget);
end;

{
  Adds some functionality to a gtk file selection dialog.
  - multiselection
  - range selection
  - close on escape
  - file information
  - history pulldown
  - filter pulldown
  - preview control

  requires: gtk+ 2.6
}
class function TGtk2WSOpenDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileSelWidget: PGtkFileChooser;
  OpenDialog: TOpenDialog absolute ACommonDialog;
  HelpButton: PGtkWidget;
  InitialFilename: String;
  //FrameWidget: PGtkWidget;
  //HBox: PGtkWidget;
  //FileDetailLabel: PGtkWidget;
begin
  Result := TGtk2WSFileDialog.CreateHandle(ACommonDialog);
  FileSelWidget := PGtkFileChooser(Result);

  if OpenDialog.InheritsFrom(TSaveDialog) then
  begin
    if OpenDialog.InitialDir <> '' then
      gtk_file_chooser_set_current_folder(FileSelWidget, Pgchar(OpenDialog.InitialDir));
  end;
  
  // Help button
  if (ofShowHelp in OpenDialog.Options) then
  begin
    HelpButton := gtk_dialog_add_button(FileSelWidget, GTK_STOCK_HELP, GTK_RESPONSE_NONE);

    g_signal_connect(PGtkObject(HelpButton),
      'clicked', gtk_signal_func(@gtkDialogHelpclickedCB), OpenDialog);
  end;

  if ofAllowMultiSelect in OpenDialog.Options then
    gtk_file_chooser_set_select_multiple(FileSelWidget, gboolean(gtrue));

  // History List - a frame with an option menu
  CreateOpenDialogHistory(OpenDialog, FileSelWidget);

  // Filter
  CreateOpenDialogFilter(OpenDialog, FileSelWidget);

  // connect change event
  g_signal_connect(PGtkObject(FileSelWidget),
    'selection-changed', gtk_signal_func(@gtkFileChooserSelectionChangedCB),
    OpenDialog);

  // Sets the dialog options

  // ofForceShowHidden
  if (ofForceShowHidden in OpenDialog.Options) then
    gtk_file_chooser_set_show_hidden(FileSelWidget, True);

  (*  TODO
  // Details - a frame with a label
  if (ofViewDetail in OpenDialog.Options) then begin

    // create the frame around the information
    FrameWidget:=gtk_frame_new(PChar(rsFileInformation));
    //gtk_box_pack_start(GTK_BOX(FileSelWidget^.main_vbox),
    //                   FrameWidget,false,false,0);
    gtk_box_pack_start(GTK_BOX(gtk_file_chooser_get_extra_widget(
             PGtkFileChooser(SelWidget))), FrameWidget,false,false,0);
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
  *)
  // preview
  if (OpenDialog is TPreviewFileDialog) then
    CreatePreviewDialogControl(TPreviewFileDialog(OpenDialog), PGtkWidget(FileSelWidget));

  // set initial filename (gtk expects an absolute filename)
  InitialFilename := TrimFilename(OpenDialog.FileName);
  if InitialFilename <> '' then
  begin
    if not FilenameIsAbsolute(InitialFilename) and (OpenDialog.InitialDir <> '') then
      InitialFilename := TrimFilename(OpenDialog.InitialDir + PathDelim + InitialFilename);
    if not FilenameIsAbsolute(InitialFilename) then
      InitialFilename := CleanAndExpandFilename(InitialFilename);
    gtk_file_chooser_set_filename(FileSelWidget, PChar(InitialFilename));
  end;

  //if InitialFilter <> 'none' then
  //  PopulateFileAndDirectoryLists(FileSelWidget, InitialFilter);
end;

{ TGtk2WSFileDialog }

class procedure TGtk2WSFileDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  g_signal_connect(AGtkWidget, 'response', gtk_signal_func(@Gtk2FileChooserResponseCB), AWidgetInfo^.LCLObject);
  g_signal_connect(AGtkWidget, 'notify', gtk_signal_func(@Gtk2FileChooserNotifyCB), AWidgetInfo^.LCLObject);
end;

{
  Creates a new TFile/Open/SaveDialog
  requires: gtk+ 2.6
}
class function TGtk2WSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog
  ): THandle;
var
  FileDialog: TFileDialog absolute ACommonDialog;
  Action: TGtkFileChooserAction;
  Button1: String;
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  // Defines an action for the dialog and creates it
  Action := GTK_FILE_CHOOSER_ACTION_OPEN;
  Button1 := GTK_STOCK_OPEN;

  if (FileDialog is TSaveDialog) or (FileDialog is TSavePictureDialog) then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SAVE;
    Button1 := GTK_STOCK_SAVE;
  end
  else
  if FileDialog is TSelectDirectoryDialog then
  begin
    Action := GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER;
    Button1 := GTK_STOCK_OPEN;
  end;

  Widget := gtk_file_chooser_dialog_new(PChar(FileDialog.Title), nil, Action,
    PChar(GTK_STOCK_CANCEL), [GTK_RESPONSE_CANCEL, PChar(Button1), GTK_RESPONSE_OK, nil]);

  {$ifdef GTK_2_8}
  if FileDialog is TSaveDialog then
  begin
    gtk_file_chooser_set_do_overwrite_confirmation(Widget,
      ofOverwritePrompt in TOpenDialog(FileDialog).Options);
  end;
  {$endif}

  if FileDialog.InitialDir <> '' then
    gtk_file_chooser_set_current_folder(Widget, Pgchar(FileDialog.InitialDir));

  if gtk_file_chooser_get_action(Widget) in
    [GTK_FILE_CHOOSER_ACTION_SAVE, GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER]
  then
    gtk_file_chooser_set_current_name(Widget, Pgchar(FileDialog.FileName));

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtk2WSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

{ TGtk2WSCommonDialog }

{------------------------------------------------------------------------------
  Method: SetColorDialogColor
  Params:  ColorSelection : a gtk color selection dialog;
           Color          : the color to select
  Returns: nothing

  Set the color of the color selection dialog
 ------------------------------------------------------------------------------}
class procedure TGtk2WSCommonDialog.SetColorDialogColor(ColorSelection: PGtkColorSelectionDialog;
  Color: TColor);
var
  SelectionColor: TGDKColor;
  colorSel: PGtkColorSelection;
begin
  Color := ColorToRGB(Color);
  SelectionColor := TColortoTGDKColor(Color);
  colorSel := PGtkColorSelection(ColorSelection^.colorsel);
  gtk_color_selection_set_current_color(colorSel, @SelectionColor);
  gtk_color_selection_set_previous_color(colorSel, @SelectionColor);
end;

class procedure TGtk2WSCommonDialog.SetColorDialogPalette(
  ColorSelection: PGtkColorSelectionDialog; Palette: TStrings);
const
  PaletteSetting = 'gtk-color-palette';
var
  colorSel: PGtkColorSelection;
  settings: PGtkSettings;
  new_palette: Pgchar;
  colors: PGdkColor;
  colors_len: gint;

  procedure FillCustomColors;
  var
    i, AIndex: integer;
    AColor: TColor;
  begin
    for i := 0 to Palette.Count - 1 do
      if ExtractColorIndexAndColor(Palette, i, AIndex, AColor) then
        if AIndex < colors_len then
          colors[AIndex] := TColortoTGDKColor(AColor);
  end;

begin
  colorSel := PGtkColorSelection(ColorSelection^.colorsel);
  // show palette
  gtk_color_selection_set_has_palette(colorSel, True);

  // replace palette. it is stored in 'gtk-color-palette' settings
  // 1. get original palette => we will know colors and replace only part of it
  settings := gtk_widget_get_settings(PGtkWidget(colorSel));
  new_palette := nil;
  g_object_get(settings, PaletteSetting, [@new_palette, nil]);
  gtk_color_selection_palette_from_string(new_palette, colors, @colors_len);
  g_free(new_palette);

  // 2. fill original palette with our custom colors
  FillCustomColors;

  // 3. set new palette back to settings
  new_palette := gtk_color_selection_palette_to_string(colors, colors_len);
  g_free(colors);
  gtk_settings_set_string_property(settings, PaletteSetting, new_palette, 'gtk_color_selection_palette_to_string');
  g_free(new_palette);
end;

class procedure TGtk2WSCommonDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
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

class procedure TGtk2WSCommonDialog.SetSizes(const AGtkWidget: PGtkWidget;
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

class function TGtk2WSCommonDialog.CreateHandle(
  const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
  DebugLn('TGtkWSCommonDialog.CreateHandle is generic dialog handle constructor => implement CreateHandle for: ', dbgsName(ACommonDialog))
end;

class procedure TGtk2WSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  GtkWindow: PGtkWindow;
begin
  ReleaseMouseCapture;
  GtkWindow:=PGtkWindow(ACommonDialog.Handle);
  gtk_window_set_title(GtkWindow,PChar(ACommonDialog.Title));
  if ACommonDialog is TColorDialog then
  begin
    SetColorDialogColor(PGtkColorSelectionDialog(GtkWindow),
                        TColorDialog(ACommonDialog).Color);
    SetColorDialogPalette(PGtkColorSelectionDialog(GtkWindow),
      TColorDialog(ACommonDialog).CustomColors);
  end;

  gtk_window_set_position(GtkWindow, GTK_WIN_POS_CENTER);
  GtkWindowShowModal(GtkWindow);
end;

class procedure TGtk2WSCommonDialog.DestroyHandle(
  const ACommonDialog: TCommonDialog);
begin
  { TODO: cleanup }
  TGtk2WidgetSet(WidgetSet).DestroyLCLComponent(ACommonDialog);
end;

{ TGtk2WSColorDialog }

class procedure TGtk2WSColorDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
  g_signal_connect(PGtkObject(PGtkColorSelectionDialog(AGtkWidget)^.ok_button),
    'clicked', gtk_signal_func(@gtkDialogOKclickedCB), AWidgetInfo^.LCLObject);
  g_signal_connect(PGtkObject(PGtkColorSelectionDialog(AGtkWidget)^.cancel_button),
    'clicked', gtk_signal_func(@gtkDialogCancelclickedCB), AWidgetInfo^.LCLObject);
end;

class function TGtk2WSColorDialog.CreateHandle(
  const ACommonDialog: TCommonDialog): THandle;
var
  Widget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
begin
  Widget := gtk_color_selection_dialog_new(PChar(ACommonDialog.Title));

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtk2WSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

{ TGtk2WSFontDialog }

class procedure TGtk2WSFontDialog.SetCallbacks(const AGtkWidget: PGtkWidget;
  const AWidgetInfo: PWidgetInfo);
begin
  TGtk2WSCommonDialog.SetCallbacks(AGtkWidget, AWidgetInfo);
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

class function TGtk2WSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FontDesc: PPangoFontDescription;
  TmpStr: pChar;

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
  end;

  { This functionality does not seem to be available in GTK2 }
  // Honor selected TFontDialogOption flags

  Result := THandle(PtrUInt(Widget));
  WidgetInfo := CreateWidgetInfo(Widget);
  WidgetInfo^.LCLObject := ACommonDialog;
  TGtk2WSCommonDialog.SetSizes(Widget, WidgetInfo);
  SetCallbacks(Widget, WidgetInfo);
end;

end.
