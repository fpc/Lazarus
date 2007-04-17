{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSDialogs.pp                               * 
 *                              --------------                               * 
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
unit CarbonWSDialogs;

{$mode objfpc}{$H+}

interface

// debugging defines
{$I carbondebug.inc}

uses
  // libs
  FPCMacOSAll,
  // LCL
  SysUtils, Controls, Dialogs, LCLType, LCLProc,
  // widgetset
  WSLCLClasses, WSProc, WSDialogs,
  // LCL Carbon
  CarbonDef, CarbonPrivate;
  
type

  { TCarbonWSCommonDialog }

  TCarbonWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  public
  end;

  { TCarbonWSFileDialog }

  TCarbonWSFileDialog = class(TWSFileDialog)
  private
  protected
  public
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TCarbonWSOpenDialog }

  TCarbonWSOpenDialog = class(TWSOpenDialog)
  private
  protected
  public
  end;

  { TCarbonWSSaveDialog }

  TCarbonWSSaveDialog = class(TWSSaveDialog)
  private
  protected
  public
  end;

  { TCarbonWSSelectDirectoryDialog }

  TCarbonWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  private
  protected
  public
  end;

  { TCarbonWSColorDialog }

  TCarbonWSColorDialog = class(TWSColorDialog)
  private
  protected
  public
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TCarbonWSColorButton }

  TCarbonWSColorButton = class(TWSColorButton)
  private
  protected
  public
  end;

  { TCarbonWSFontDialog }

  TCarbonWSFontDialog = class(TWSFontDialog)
  private
  protected
  public
  end;


implementation

uses
  CarbonProc, CarbonDbgConsts;

{ TCarbonWSFileDialog }

{------------------------------------------------------------------------------
  Method:  TCarbonWSFileDialog.ShowModal
  Params:  ACommonDialog - LCL common dialog
  
  Shows a file dialog (open, save, slect directory) in Carbon interface. Sets
  ACommonDialog.UserChoice to mrOK or mrCancel. If mrOK, also sets
  ACommonDialog.FileName to first file selected and adds file(s) selected to
  ACommonDialog.Files.
 ------------------------------------------------------------------------------}
class procedure TCarbonWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);

 {
  Called by Execute method of TOpenDialog, TSaveDialog and TSelectDirectoryDialog.
   TODO: Figure out how to use dialog's InitialDir property.
   TODO: Figure out how to pass UPP of a custom filter callback function as 
         inFilterProc to NavCreateGetFileDialog and pass FileDialog as
         inClientData so callback function can access dialog's Filter property.
 }
var
  FileDialog: TFileDialog;
  CreationOptions: NavDialogCreationOptions;
  DialogRef: NavDialogRef;
  DialogReply: NavReplyRecord;
  FileCount: Integer;
  FileIdx: Integer;
  Keyword: AEKeyword;
  FileDesc: AEDesc;
  FileRef: FSRef;
  FileURL: CFURLRef;
  FileCFStr: CFStringRef;
begin
  {$IFDEF VerboseWSClass}
    DebugLn('TCarbonWSFileDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}

  FileDialog := ACommonDialog as TFileDialog;

  // Initialize record to default values
  if OSError(NavGetDefaultDialogCreationOptions(CreationOptions),
    Self, SShowModal, 'NavGetDefaultDialogCreationOptions') then Exit;

  if FileDialog.Title <> '' then  // Override dialog's default title?
    CreateCFString(FileDialog.Title, CreationOptions.windowTitle);

  FileDialog.UserChoice := mrCancel; // Return this if user cancels or we need to exit

  try
    if FileDialog is TSaveDialog then
    begin  // Checking for TSaveDialog first since it's descendent of TOpenDialog
      CreateCFString(ExtractFileName(FileDialog.FileName),
        CreationOptions.saveFileName);  // Note doesn't like path
        
      if ofOverwritePrompt in TOpenDialog(FileDialog).Options then
        CreationOptions.optionFlags :=
           CreationOptions.optionFlags xor kNavDontConfirmReplacement
      else
        CreationOptions.optionFlags :=
          CreationOptions.optionFlags or kNavDontConfirmReplacement;
         
      // Create Save dialog
      if OSError(
        NavCreatePutFileDialog(@CreationOptions, 0, 0, nil, nil, DialogRef),
        Self, SShowModal, 'NavCreatePutFileDialog') then Exit;
    end
    else
      if FileDialog is TSelectDirectoryDialog then // Create Choose folder dialog
      begin
        if OSError(
          NavCreateChooseFolderDialog(@CreationOptions, nil, nil, nil, DialogRef),
          Self, SShowModal, 'NavCreateChooseFolderDialog') then Exit;
      end
      else
        if FileDialog is TOpenDialog then
        begin
          if not (ofAllowMultiSelect in TOpenDialog(FileDialog).Options) then
            CreationOptions.optionFlags :=
              CreationOptions.optionFlags xor kNavAllowMultipleFiles
          else
            CreationOptions.optionFlags :=
              CreationOptions.optionFlags or kNavAllowMultipleFiles;

          // Create Open dialog
          if OSError(
            NavCreateGetFileDialog(@CreationOptions, nil, nil, nil, nil, nil, DialogRef),
            Self, SShowModal, 'NavCreateGetFileDialog') then Exit;
        end;

    try
      // Display dialog
      if OSError(NavDialogRun(DialogRef), Self, SShowModal, 'NavDialogRun') then Exit;
      
      if NavDialogGetUserAction(DialogRef) <> kNavUserActionCancel then // User OK?
      begin
        if OSError(NavDialogGetReply(DialogRef, DialogReply), Self, SShowModal,
          'NavDialogGetReply') then Exit;  // Get user's selection
          
        if OSError(AECountItems(DialogReply.Selection, FileCount), Self,
          SShowModal, 'AECountItems') then Exit;
          
        for FileIdx := 1 to FileCount do
        begin
          if OSError(AEGetNthDesc(DialogReply.Selection, FileIdx, typeFSRef,
            @Keyword, FileDesc), Self, SShowModal, 'AEGetNthDesc') then Exit;
          // Get file reference
          if OSError(AEGetDescData(FileDesc, @FileRef, SizeOf(FSRef)), Self,
            SShowModal, 'AEGetDescData') then Exit;
            
          if OSError(AEDisposeDesc(FileDesc), Self, SShowModal,
            'AEDisposeDesc') then Exit;
          
          FileURL := CFURLCreateFromFSRef(kCFAllocatorDefault, FileRef); // Get URL
          FileCFStr := CFURLCopyFileSystemPath(FileURL, kCFURLPOSIXPathStyle); // Get path
          
          FileDialog.Files.Add(CFStringToStr(FileCFStr));

          FreeCFString(FileURL);
          FreeCFString(FileCFStr);
          // Note: Previous 5 lines replace next 2 lines and eliminate need
          //   to decide what size to make FileBuf array.
          //   See http://developer.apple.com/technotes/tn2002/tn2078.html
          //  FSRefMakePath(FileRef, @FileBuf, SizeOf(FileBuf));  {Get file path}
          //  FileDialog.Files.Add(string(FileBuf));  //FileBuf contains UTF8 C string
        end;
          
        FileDialog.FileName := FileDialog.Files.Strings[0];

        if FileDialog is TSaveDialog then
          FileDialog.FileName := FileDialog.FileName + PathDelim +
            CFStringToStr(NavDialogGetSaveFileName(DialogRef));
            {Note: Not at all clear from Apple docs that NavReplyRecord.Selection
              returns only path to file's folder with Save dialog. Also, what they
              mean by the "full file name" returned by NavDialogGetSaveFileName
              must mean extension and not path to file's folder.}
              
        // Dispose of data that record points to (?)
        if OSError(NavDisposeReply(DialogReply), Self, SShowModal,
          'NavDisposeReply') then Exit;
          
        FileDialog.UserChoice := mrOK;
      end;
    finally
      NavDialogDispose(DialogRef);  // Dispose of dialog
    end;

  finally
    FreeCFString(CreationOptions.windowTitle);
    FreeCFString(CreationOptions.saveFileName);
  end;
end;  {TCarbonWSFileDialog.ShowModal}

{ TCarbonWSColorDialog }

{------------------------------------------------------------------------------
  Method:  TCarbonWSColorDialog.ShowModal
  Params:  ACommonDialog - LCL color dialog

  Shows Carbon interface color picker
 ------------------------------------------------------------------------------}
class procedure TCarbonWSColorDialog.ShowModal(
  const ACommonDialog: TCommonDialog);
var
  ColorInfo: ColorPickerInfo;
  ColorDialog: TColorDialog;
begin
  {$IFDEF VerboseWSClass}
    DebugLn('TCarbonWSColorDialog.ShowModal for ' + ACommonDialog.Name);
  {$ENDIF}
  
  ACommonDialog.UserChoice := mrCancel;
  ColorDialog := ACommonDialog as TColorDialog;
  
  FillChar(ColorInfo, SizeOf(ColorPickerInfo), 0);
  ColorInfo.theColor.color.rgb := CMRGBColor(ColorToRGBColor(ColorDialog.Color));
  ColorInfo.theColor.profile := nil;
  ColorInfo.dstProfile := nil;
  ColorInfo.flags := kColorPickerDialogIsModal or kColorPickerDialogIsMoveable or
    kColorPickerInPickerDialog;
  ColorInfo.placeWhere :=  kCenterOnMainScreen;
  ColorInfo.pickerType := 0; // use last picker subtype
  ColorInfo.eventProc := nil;
  ColorInfo.colorProc := nil;
  // ColorDialog.Title is ignored, ColorInfo.prompt is not shown anywhere
  
  if OSError(PickColor(ColorInfo), Self, SShowModal, 'PickColor') then Exit;
  
  if ColorInfo.newColorChosen then
  begin
    ColorDialog.Color := RGBColorToColor(RGBColor(ColorInfo.theColor.color.rgb));
    ACommonDialog.UserChoice := mrOK;
  end;
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCommonDialog, TCarbonWSCommonDialog);
  RegisterWSComponent(TFileDialog, TCarbonWSFileDialog);
//  RegisterWSComponent(TOpenDialog, TCarbonWSOpenDialog);
//  RegisterWSComponent(TSaveDialog, TCarbonWSSaveDialog);
//  RegisterWSComponent(TSelectDirectoryDialog, TCarbonWSSelectDirectoryDialog);
  RegisterWSComponent(TColorDialog, TCarbonWSColorDialog);
//  RegisterWSComponent(TColorButton, TCarbonWSColorButton);
//  RegisterWSComponent(TFontDialog, TCarbonWSFontDialog);
////////////////////////////////////////////////////
end.
