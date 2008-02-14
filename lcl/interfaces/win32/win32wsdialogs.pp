{ $Id$}
{
 *****************************************************************************
 *                             Win32WSDialogs.pp                             * 
 *                             -----------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Win32WSDialogs;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
// rtl
  Windows, ShellApi, ActiveX, SysUtils, Classes,
{$IFNDEF VER2_2}
  commdlg,
{$ENDIF}
// lcl
  LCLProc, LCLType, Dialogs, Controls, Graphics, Forms,
// ws
  WSDialogs, WSLCLClasses, Win32Extra, Win32Int, InterfaceBase,
  Win32Proc;

type

  { TWin32WSCommonDialog }

  TWin32WSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
  end;

  { TWin32WSFileDialog }

  TWin32WSFileDialog = class(TWSFileDialog)
  private
  protected
  public
  end;

  { TWin32WSOpenDialog }

  TWin32WSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TWin32WSSaveDialog }

  TWin32WSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TWin32WSSelectDirectoryDialog }

  TWin32WSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TWin32WSColorDialog }

  TWin32WSColorDialog = class(TWSColorDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;

  { TWin32WSColorButton }

  TWin32WSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TWin32WSFontDialog }

  TWin32WSFontDialog = class(TWSFontDialog)
  private
  protected
  public
    class function  CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
  end;


implementation

type
  TOpenFileDialogRec = record
    Dialog: TFileDialog;
    FileNames: String;
  end;
  POpenFileDialogRec = ^TOpenFileDialogRec;

// The size of the OPENFILENAME record depends on the windows version
// In the initialization section the correct size is determined.
var
  OpenFileNameSize: integer = 0;

{------------------------------------------------------------------------------
  Method: GetOwnerHandle
  Params:  ADialog - dialog to get 'guiding parent' window handle for
  Returns: A window handle

  Returns window handle to be used as 'owner handle', ie. so that the user must
  finish the dialog before continuing
 ------------------------------------------------------------------------------}
function GetOwnerHandle(ADialog : TCommonDialog): HWND;
begin
  with ADialog do
  begin
    if Owner is TWinControl then
      Result := TWinControl(Owner).Handle
    else
      Result := TWin32WidgetSet(WidgetSet).AppHandle;
  end;
end;

procedure SetDialogResult(const ACommonDialog: TCommonDialog; Ret: WINBOOL);
begin
  if Ret then
    ACommonDialog.UserChoice := mrOK
  else
    ACommonDialog.UserChoice := mrCancel;
end;

{ TWin32WSColorDialog }

class function TWin32WSColorDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
const
  { 16 basic RGB colors; names listed in comments for debugging }
  CustomColors: array[1..16] of dword = (
  0, //Black
  $C0C0C0, //Silver
  $808080, //Gray
  $FFFFFF, //White
  $000080, //Maroon
  $0000FF, //Red
  $800080, //Purple
  $FF00FF, //Fuchsia
  $008000, //Green
  $00FF00, //Lime
  $008080, //Olive
  $00FFFF, //Yellow
  $800000, //Navy
  $FF0000, //Blue
  $808000, //Teal
  $FFFF00 //Aqua
  );
var
  CC: TChooseColor;
  UserResult: WINBOOL;
begin
  ZeroMemory(@CC, sizeof(TChooseColor));
  with CC Do
  begin
    LStructSize := sizeof(TChooseColor);
    HWndOwner := GetOwnerHandle(ACommonDialog);
    RGBResult := ColorToRGB(TColorDialog(ACommonDialog).Color);
    LPCustColors := @CustomColors[1];
    Flags := CC_FULLOPEN or CC_RGBINIT;
  end;
  UserResult := ChooseColor(@CC);
  SetDialogResult(ACommonDialog, UserResult);
  if UserResult then
    TColorDialog(ACommonDialog).Color := CC.RGBResult;

  Result := 0;
end;

{Common code for OpenDialog and SaveDialog}

{The API of the multiselect open file dialog is a bit problematic.
 Before calling the OpenFile function you must create a buffer (lpStrFile) to
 hold the selected files.

 With a multiselect dialog there is no way to create a buffer with correct size:
 * either it is too small (for example 1 KB), if a lot a files are selected
 * or it wastes a lot of memory (for example 1 MB), and even than you have no
   guarantee, that is big enough.

 The OpenFile API call returns false, if an error has occurred or the user has
 pressed cancel. If there was an error CommDlgExtendedError returns
 FNERR_BUFFERTOOSMALL. But enlarging the buffer at that time is not usefull
 anymore, unless you show the dialog again with a bigger buffer (Sorry, the
 buffer was too small, please select the files again). This is not acceptable.

 It is possible to hook the filedialog, so you get messages, when the selection
 changes. A naive aproach would be to see, if the buffer would be big enough for
 the selected files and create or enlarge the buffer (as described in KB131462).
 Unfortunately, this only works with win9x and the unicode versions of later
 windows versions.

 Therefore in the hook function, if the size of the initial buffer (lpStrFile)
 is not large enough, the selected files are copied into a string. A pointer to
 this string is kept in the lCustData field of the the OpenFileName struct.
 When dialog is closed with a FNERR_BUFFERTOOSMALL error, this string is used to
 get the selected files. If this error did not occur, the normal way of
 retrieving the files is used.
}

type
  TWinFileDialogFunc = function(OpenFile: Windows.LPOPENFILENAME): WINBOOL; stdcall;

function OpenFileDialogCallBack(hWnd: Handle; uMsg: UINT; wParam: WPARAM;
  lParam: LPARAM): UINT; stdcall;
  
  procedure Reposition(ADialogWnd: Handle);
  var
    Left, Top: Integer;
    DialogRect: TRect;
  begin
    // Btw, setting width and height of dialog doesnot reposition child controls :(
    // So no way to set another height and width at least here
    
    // do reposition only if dialog has no parent form
    if (GetParent(ADialogWnd) = Win32WidgetSet.AppHandle) then
    begin
      GetWindowRect(ADialogWnd, @DialogRect);

      Left := (GetSystemMetrics(SM_CXSCREEN) - DialogRect.Right + DialogRect.Left) div 2;
      Top := (GetSystemMetrics(SM_CYSCREEN) - DialogRect.Bottom + DialogRect.Top) div 2;
      SetWindowPos(ADialogWnd, HWND_TOP, Left, Top, 0, 0, SWP_NOSIZE);
    end;
  end;
  
var
  OpenFileNotify: LPOFNOTIFY;
  OpenFileName: Windows.POPENFILENAME;
  NeededSize: SizeInt;
  DialogRec: POpenFileDialogRec;
begin
  if uMsg = WM_INITDIALOG then
  begin
    // Windows asks us to initialize dialog. At this moment controls are not
    // arranged and this is that moment when we should set bounds of our dialog
    Reposition(GetParent(hWnd));
  end
  else
  if uMsg = WM_NOTIFY then
  begin
    OpenFileNotify := LPOFNOTIFY(lParam);
    if OpenFileNotify <> nil then
    begin
      OpenFileName := OpenFileNotify^.lpOFN;
      DialogRec := POpenFileDialogRec(OpenFileName^.lCustData);
    end
    else
    begin
      OpenFileName := nil;
      DialogRec := nil;
    end;

    case OpenFileNotify^.hdr.code of
      CDN_SELCHANGE:
        begin
          // NeededSize is the size that the lpStrFile buffer must have.
          // the lpstrFile buffer contains the directory and a list of files
          // for example 'c:\winnt'#0'file1.txt'#0'file2.txt'#0#0.
          // GetFolderPath returns upper limit for the path, GetSpec for the files.
          // This is not exact because the GetSpec returns the size for
          // '"file1.txt" "file2.txt"', so that size will be two bytes per filename
          // more than needed in thlengthe lpStrFile buffer.
          NeededSize := CommDlg_OpenSave_GetFolderPath(GetParent(hwnd), nil, 0) +
                          CommDlg_OpenSave_GetSpec(GetParent(hwnd), nil, 0);
          // test if we need to use our own storage
          if (SizeInt(OpenFileName^.nMaxFile) < NeededSize) and (OpenFileName^.lCustData <> 0) then
          begin
            if length(DialogRec^.FileNames) < NeededSize then
              SetLength(DialogRec^.FileNames, NeededSize*2);
            CommDlg_OpenSave_GetSpec(GetParent(hwnd),
                                       PChar(DialogRec^.FileNames), Length(DialogRec^.FileNames));
          end;
        end;
      CDN_TYPECHANGE:
        begin
          DialogRec^.Dialog.IntfFileTypeChanged(OpenFileNotify^.lpOFN^.nFilterIndex);
        end;
    end;
  end;
  Result := 0;
end;

function CreateFileDialogHandle(AOpenDialog: TOpenDialog): THandle;
  function GetFlagsFromOptions(Options: TOpenOptions): DWord;
  begin
    Result := OFN_ENABLEHOOK;
    if ofAllowMultiSelect in Options then Result := Result or OFN_ALLOWMULTISELECT;
    if ofCreatePrompt in Options then Result := Result or OFN_CREATEPROMPT;
    if not (ofOldStyleDialog in Options) then Result := Result or OFN_EXPLORER;
    if ofExtensionDifferent in Options then Result := Result or OFN_EXTENSIONDIFFERENT;
    if ofFileMustExist in Options then Result := Result or OFN_FILEMUSTEXIST;
    if ofHideReadOnly in Options then Result := Result or OFN_HIDEREADONLY;
    if ofNoChangeDir in Options then Result := Result or OFN_NOCHANGEDIR;
    if ofNoDereferenceLinks in Options then Result := Result or OFN_NODEREFERENCELINKS;
    if ofEnableSizing in Options then Result := Result or OFN_ENABLESIZING;
    if ofNoLongNames in Options then  Result := Result or OFN_NOLONGNAMES;
    if ofNoNetworkButton in Options then Result := Result or OFN_NONETWORKBUTTON;
    if ofNoReadOnlyReturn in  Options then Result := Result or OFN_NOREADONLYRETURN;
    if ofNoTestFileCreate in Options then Result := Result or OFN_NOTESTFILECREATE;
    if ofNoValidate in Options then Result := Result or OFN_NOVALIDATE;
    if ofOverwritePrompt in Options then Result := Result or OFN_OVERWRITEPROMPT;
    if ofPathMustExist in Options then Result := Result or OFN_PATHMUSTEXIST;
    if ofReadOnly in Options then Result := Result or OFN_READONLY;
    if ofShareAware in Options then Result := Result or OFN_SHAREAWARE;
    if ofShowHelp in Options then Result := Result or OFN_SHOWHELP;
  end;

  procedure ReplacePipe(var AFilter:string);
  var i:integer;
  begin
    for i := 1 to length(AFilter) do
      if AFilter[i] = '|' then AFilter[i]:=#0;
    AFilter:=AFilter + #0#0;
  end;
const
  FileNameBufferLen = 1000;
var
  DialogRec: POpenFileDialogRec;
  OpenFile: LPOPENFILENAME;
  Filter: string;
  FileName: string;
  InitialDir: String;
  FileNameBuffer: PChar;
{$ifdef WindowsUnicodeSupport}
  FileNameWide: WideString;
  FileNameWideBuffer: PWideChar;
  FileNameBufferSize: Integer;
  FilterBuffer: WideString;
  TitleBuffer: WideString;
{$endif WindowsUnicodeSupport}
begin
{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
    FileNameWideBuffer := AllocMem(FileNameBufferLen * 2 + 2)
  else
    FileNameBuffer := AllocMem(FileNameBufferLen + 1);
{$else}
  FileNameBuffer := AllocMem(FileNameBufferLen + 1);
{$endif}

  FileName := AOpenDialog.FileName;
  InitialDir := AOpenDialog.InitialDir;
  if (FileName<>'') and (FileName[length(FileName)]=PathDelim) then
  begin
    // if the filename contains a directory, set the initial directory
    // and clear the filename
    InitialDir := Copy(FileName,1, Length(FileName)-1);
    FileName := '';
  end;

  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      FileNameWide := UTF8Decode(FileName);

      FillChar(FileNameWideBuffer^, FileNameBufferLen * 2 + 2, #0);

      if Length(FileNameWide) > FileNameBufferLen then
        FileNameBufferSize := FileNameBufferLen
      else
        FileNameBufferSize := Length(FileNameWide);

      Move(FileNameWide[1], FileNameWideBuffer^, FileNameBufferSize * 2);
    end
    else
      StrLCopy(FileNameBuffer, PChar(UTF8ToAnsi(FileName)), FileNameBufferLen);
  {$else}
    StrLCopy(FileNameBuffer, PChar(FileName), FileNameBufferLen);
  {$endif}

  if AOpenDialog.Filter <> '' then
  begin
    Filter := AOpenDialog.Filter;
    ReplacePipe(Filter);
  end
  else
    Filter:='All File Types(*.*)'+#0+'*.*'+#0#0; // Default -> avoid empty combobox

  OpenFile := AllocMem(SizeOf(OpenFileName));
  with OpenFile^ Do
  begin
    lStructSize := OpenFileNameSize;
    hWndOwner := GetOwnerHandle(AOpenDialog);
    hInstance := System.hInstance;

    nFilterIndex := AOpenDialog.FilterIndex;

  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      lpStrFile := PChar(FileNameWideBuffer);

      FilterBuffer := Utf8Decode(Filter);
      lpStrFilter := GetMem(Length(FilterBuffer) * 2 + 2);
      Move(FilterBuffer[1], lpStrFilter^, Length(FilterBuffer) * 2 + 2);

      TitleBuffer := Utf8Decode(AOpenDialog.Title);
      {$note AllocMem is used a workaround for a possible bug in Utf8Decode, it doesn't seem to null terminate the widestring}
      lpStrTitle := AllocMem(Length(TitleBuffer) * 2 + 2);
      Move(TitleBuffer[1], lpStrTitle^, Length(TitleBuffer) * 2);
    end
    else
    begin
      lpStrFile := FileNameBuffer;

      lpStrFilter := StrAlloc(Length(Filter)+1);
      StrPCopy(lpStrFilter, Utf8ToAnsi(Filter));

      lpStrTitle := GetMem(Length(AOpenDialog.Title)+1);
      StrPCopy(lpStrTitle, Utf8ToAnsi(AOpenDialog.Title));
    end;
  {$else}
    lpStrFile := FileNameBuffer;

    lpStrFilter := StrAlloc(Length(Filter)+1);
    StrPCopy(lpStrFilter, Filter);

    lpStrTitle := GetMem(Length(AOpenDialog.Title)+1);
    StrPCopy(lpStrTitle, AOpenDialog.Title);
  {$endif}

    lpStrInitialDir := PChar(InitialDir);

    nMaxFile := FileNameBufferLen + 1; // Size in TCHARs
    lpfnHook := @OpenFileDialogCallBack;
    Flags := GetFlagsFromOptions(AOpenDialog.Options);
    New(DialogRec);
    DialogRec^.Dialog := AOpenDialog;
    DialogRec^.FileNames := '';
    lCustData := LParam(DialogRec);
  end;
  Result := THandle(OpenFile);
end;

procedure ProcessFileDialogResult(AOpenDialog: TOpenDialog; UserResult: WordBool);
var
  DialogRec: POpenFileDialogRec;
  OpenFile: LPOPENFILENAME;

  procedure SetFilesProperty(AFiles:TStrings);
  var
    I: integer;
    pName: PChar;
  {$ifdef WindowsUnicodeSupport}
    PWideName: PWideChar;
  {$endif WindowsUnicodeSupport}
  begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      PWideName := PWideChar(OpenFile^.lpStrFile);
      I:=Length(PWideName);
      if I < OpenFile^.nFileOffset then
      begin
        Inc(PWideName, Succ(I));
        I:=Length(PWideName);
        while I > 0 do
        begin
          AFiles.Add(ExpandFileName(Utf8Encode(PWideName)));
          Inc(PWideName,Succ(I));
          I:=Length(PWideName);
        end;
      end
      else
        AFiles.Add(Utf8Encode(PWideName));
    end
    else
    begin
      pName := OpenFile^.lpStrFile;
      I:=Length(pName);
      if I < OpenFile^.nFileOffset then
      begin
        Inc(pName,Succ(I));
        I:=Length(pName);
        while I > 0 do
        begin
          AFiles.Add(ExpandFileName(StrPas(pName)));
          Inc(pName,Succ(I));
          I:=Length(pName);
        end;
      end
      else
        AFiles.Add(StrPas(pName));
    end;
  {$else}
    pName := OpenFile^.lpStrFile;
    I:=Length(pName);
    if I < OpenFile^.nFileOffset then
    begin
      Inc(pName,Succ(I));
      I:=Length(pName);
      while I > 0 do
      begin
        AFiles.Add(ExpandFileName(StrPas(pName)));
        Inc(pName,Succ(I));
        I:=Length(pName);
      end;
    end
    else
      AFiles.Add(StrPas(pName));
  {$endif}
  end;

  procedure SetFilesPropertyCustomFiles(AFiles:TStrings);
  var
    i, Start: integer;
    FileNames: String;
  begin
    FileNames := DialogRec^.FileNames;
    if (FileNames[1] = '"') then
    begin
      Start := 1; // first quote is on pos 1
      while FileNames[Start] <> #0 do
      begin
        i := Start + 1;
        while FileNames[i] <> '"' do
          inc(i);
        AFiles.Add(ExpandFileName(Copy(FileNames, Start + 1, I - Start - 1)));
        start := i+1;
        while (FileNames[Start] <> #0) and (FileNames[start] <> '"') do
          inc(Start);
      end;
    end;
  end;

  procedure SetFilesPropertyForOldStyle(AFiles:TStrings);
  var
    SelectedStr: string;
    I,Start: integer;
  begin
    SelectedStr:=StrPas(OpenFile^.lpStrFile);
    I:=Pos(' ',SelectedStr);
    if I = 0 then
      AFiles.Add(SelectedStr)
    else begin
      Delete(SelectedStr,1,I);
      SelectedStr:=SelectedStr+' ';
      Start:=1;
      for I:= 1 to Length(SelectedStr) do
        if SelectedStr[I] =  ' ' then
        begin
          AFiles.Add(ExpandFileName(Copy(SelectedStr,Start,I - Start)));
          Start:=Succ(I);
        end;
    end;
  end;
  
var
  BufferTooSmall: boolean;
begin
  OPENFILE := LPOPENFILENAME(AOpenDialog.Handle);
  DialogRec := POpenFileDialogRec(OPENFILE^.lCustData);
  BufferTooSmall := not UserResult and (CommDlgExtendedError=FNERR_BUFFERTOOSMALL);
  if BufferTooSmall then
    UserResult := true;
  SetDialogResult(AOpenDialog, UserResult);

  AOpenDialog.Files.Clear;
  if UserResult then
  begin
    AOpenDialog.FilterIndex := OpenFile^.nFilterIndex;
    if (ofOldStyleDialog in AOpenDialog.Options) then
      SetFilesPropertyForOldStyle(AOpenDialog.Files)
    else if BufferTooSmall then
      SetFilesPropertyCustomFiles(AOpenDialog.Files)
    else
      SetFilesProperty(AOpenDialog.Files);
    AOpenDialog.FileName := AOpenDialog.Files[0];
  end else
    AOpenDialog.FileName := '';
end;

{ TWin32WSSaveDialog }

class function TWin32WSSaveDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := CreateFileDialogHandle(TOpenDialog(ACommonDialog));
end;

class procedure TWin32WSSaveDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
var
  OpenFile: LPOPENFILENAME;
begin
  if ACommonDialog.Handle <> 0 then
  begin
    OpenFile := LPOPENFILENAME(ACommonDialog.Handle);
    if OpenFile^.lCustData <> 0 then
      Dispose(POpenFileDialogRec(OPENFILE^.lCustData));

  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      FreeMem(OpenFile^.lpStrFilter)
    else
      StrDispose(OpenFile^.lpStrFilter);
  {$else}
    StrDispose(OpenFile^.lpStrFilter);
  {$endif}

    FreeMem(OpenFile^.lpStrFile);
    FreeMem(OpenFile^.lpStrTitle);
    FreeMem(OpenFile);
  end;
end;

class procedure TWin32WSSaveDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
  if ACommonDialog.Handle <> 0 then
  begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      ProcessFileDialogResult(TOpenDialog(ACommonDialog),
        GetSaveFileNameW(LPOPENFILENAME(ACommonDialog.Handle)))
    else
      ProcessFileDialogResult(TOpenDialog(ACommonDialog),
        GetSaveFileName(LPOPENFILENAME(ACommonDialog.Handle)));
  {$else}
    ProcessFileDialogResult(TOpenDialog(ACommonDialog),
      GetSaveFileName(LPOPENFILENAME(ACommonDialog.Handle)));
  {$endif}
  end;
end;

{ TWin32WSOpenDialog }

class function TWin32WSOpenDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := CreateFileDialogHandle(TOpenDialog(ACommonDialog));
end;

class procedure TWin32WSOpenDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
var
  OPENFILE: LPOPENFILENAME;
begin
  if ACommonDialog.Handle <> 0 then
  begin
    OPENFILE := LPOPENFILENAME(ACommonDialog.Handle);
    if OPENFILE^.lCustData <> 0 then
      Dispose(POpenFileDialogRec(OPENFILE^.lCustData));

  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      FreeMem(OpenFile^.lpStrFilter)
    else
      StrDispose(OpenFile^.lpStrFilter);
  {$else}
    StrDispose(OpenFile^.lpStrFilter);
  {$endif}

    FreeMem(OpenFile^.lpStrFile);
    FreeMem(OpenFile^.lpStrTitle);
    FreeMem(OpenFile);
  end;
end;

class procedure TWin32WSOpenDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
  if ACommonDialog.Handle <> 0 then
  begin
  {$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
      ProcessFileDialogResult(TOpenDialog(ACommonDialog),
        GetOpenFileNameW(LPOPENFILENAME(ACommonDialog.Handle)))
    else
      ProcessFileDialogResult(TOpenDialog(ACommonDialog),
        GetOpenFileName(LPOPENFILENAME(ACommonDialog.Handle)));
  {$else}
    ProcessFileDialogResult(TOpenDialog(ACommonDialog),
      GetOpenFileName(LPOPENFILENAME(ACommonDialog.Handle)));
  {$endif}
  end;
end;

{ TWin32WSFontDialog }

class function TWin32WSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;

  function GetFlagsFromOptions(Options : TFontDialogOptions): dword;
  begin
    Result := 0;
    if fdAnsiOnly in Options then Result := Result or CF_ANSIONLY;
    if fdTrueTypeOnly in Options then Result := Result or CF_TTONLY;
    if fdEffects in Options then Result := Result or CF_EFFECTS;
    if fdFixedPitchOnly in Options then Result := Result or CF_FIXEDPITCHONLY;
    if fdForceFontExist in Options then Result := Result or CF_FORCEFONTEXIST;
    if fdNoFaceSel in Options then Result := Result or CF_NOFACESEL;
    if fdNoOEMFonts in Options then Result := Result or CF_NOOEMFONTS;
    if fdNoSimulations in Options then Result := Result or CF_NOSIMULATIONS;
    if fdNoSizeSel in Options then Result := Result or CF_NOSIZESEL;
    if fdNoStyleSel in Options then Result := Result or CF_NOSTYLESEL;
    if fdNoVectorFonts in Options then Result := Result or CF_NOVECTORFONTS;
    if fdShowHelp in Options then Result := Result or CF_SHOWHELP;
    if fdWysiwyg in Options then Result := Result or CF_WYSIWYG;
    if fdLimitSize in Options then Result := Result or CF_LIMITSIZE;
    if fdScalableOnly in Options then Result := Result or CF_SCALABLEONLY;
    if fdApplyButton in Options then Result := Result or CF_APPLY;
  end;

var
  CF: TChooseFont;
  LF: LCLType.LOGFONT;
  UserResult: WINBOOL;
begin
  with TFontDialog(ACommonDialog) do
  begin
    ZeroMemory(@CF, sizeof(TChooseFont));
    ZeroMemory(@LF, sizeof(LogFont));
    with LF do
    begin
      LFHeight := Font.Height;
      LFFaceName := TFontDataName(Font.Name);
      if (fsBold in Font.Style) then LFWeight:= FW_BOLD;
      LFItalic := byte(fsItalic in Font.Style);
      LFStrikeOut := byte(fsStrikeOut in Font.Style);
      LFUnderline := byte(fsUnderline in Font.Style);
      LFCharSet := Font.CharSet;
    end;
    with CF do
    begin
      LStructSize := sizeof(TChooseFont);
      HWndOwner := GetOwnerHandle(ACommonDialog);
{$ifndef ver2_2}
      LPLogFont := commdlg.PLOGFONT(@LF);
{$else}
      LPLogFont := windows.PLOGFONT(@LF);
{$endif}
      Flags := GetFlagsFromOptions(Options);
      Flags := Flags or CF_INITTOLOGFONTSTRUCT or CF_BOTH;
      RGBColors := Font.Color;
    end;
  end;

  UserResult := ChooseFont(@CF);
  SetDialogResult(ACommonDialog, UserResult);
  if UserResult then
  begin
    with TFontDialog(ACommonDialog).Font do
    begin
      Assign(LF);
      Color := CF.RGBColors;
    end;
  end;

  Result := 0;
end;

{ TWin32WSCommonDialog }

class function TWin32WSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  DebugLn('TWin32WSCommonDialog.CreateHandle: unhandled dialog!');
  Result := 0;
end;

class procedure TWin32WSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  DestroyWindow(ACommonDialog.Handle);
end;

{ TWin32WSSelectDirectoryDialog }

{------------------------------------------------------------------------------
 Function: BrowseForFolderCallback
 Params: Window_hwnd - The window that receives a message for the window
         Msg         - The message received
         LParam      - Long-integer parameter
         lpData      - Data parameter, contains initial path.
  Returns: non-zero long-integer

  Handles the messages sent to the toolbar button by Windows
 ------------------------------------------------------------------------------}
Function BrowseForFolderCallback(hwnd : Handle; uMsg : UINT;
  lParam, lpData : LPARAM) : Integer; stdcall;
begin
  case uMsg of
    BFFM_INITIALIZED:
        // Setting root dir
        SendMessage(hwnd, BFFM_SETSELECTION, ULONG(True), lpData);
    //BFFM_SELCHANGED
    //  : begin
    //    if Assigned(FOnSelectionChange) then .....
    //    end;
  end;
  Result := 0;
end;

class function TWin32WSSelectDirectoryDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  bi : TBrowseInfo;
  Options: TOpenOptions;
  Buffer : PChar;
  iidl : PItemIDList;
  InitialDir: string;
begin
  Buffer := CoTaskMemAlloc(MAX_PATH);
  InitialDir := TSelectDirectoryDialog(ACommonDialog).FileName;
  
  Options := TSelectDirectoryDialog(ACommonDialog).Options;
  
  if length(InitialDir)=0 then
    InitialDir := TSelectDirectoryDialog(ACommonDialog).InitialDir;
  if length(InitialDir)>0 then begin
    // remove the \ at the end.
    if Copy(InitialDir,length(InitialDir),1)=PathDelim then
      InitialDir := copy(InitialDir,1, length(InitialDir)-1);
    // if it is a rootdirectory, then the InitialDir must have a \ at the end.
    if Copy(InitialDir,length(InitialDir),1)=DriveDelim then
      InitialDir := InitialDir + PathDelim;
  end;
  with bi do
  begin
    hwndOwner := GetOwnerHandle(ACommonDialog);
    pidlRoot := nil;
    pszDisplayName := Buffer;
    lpszTitle := PChar(ACommonDialog.Title);
    ulFlags := BIF_RETURNONLYFSDIRS;
    if not (ofOldStyleDialog in Options) then
       ulFlags := ulFlags + BIF_NEWDIALOGSTYLE;
    lpfn := @BrowseForFolderCallback;
    // this value will be passed to callback proc as lpData
    lParam := LclType.LParam(PChar(InitialDir));
  end;

  iidl := SHBrowseForFolder(@bi);

  if Assigned(iidl) then
  begin
    SHGetPathFromIDList(iidl, Buffer);
    CoTaskMemFree(iidl);
    TSelectDirectoryDialog(ACommonDialog).FileName := Buffer;
  end;

  SetDialogResult(ACommonDialog, assigned(iidl));

  CoTaskMemFree(Buffer);

  Result := 0;
end;

initialization

  if (Win32MajorVersion=4) then
    OpenFileNameSize := sizeof(OPENFILENAME_NT4)
  else
    OpenFileNameSize:=sizeof(OPENFILENAME);

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCommonDialog, TWin32WSCommonDialog);
//  RegisterWSComponent(TFileDialog, TWin32WSFileDialog);
  RegisterWSComponent(TOpenDialog, TWin32WSOpenDialog);
  RegisterWSComponent(TSaveDialog, TWin32WSSaveDialog);
  RegisterWSComponent(TSelectDirectoryDialog, TWin32WSSelectDirectoryDialog);
  RegisterWSComponent(TColorDialog, TWin32WSColorDialog);
//  RegisterWSComponent(TColorButton, TWin32WSColorButton);
  RegisterWSComponent(TFontDialog, TWin32WSFontDialog);
////////////////////////////////////////////////////

end.
