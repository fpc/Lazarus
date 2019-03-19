{
 *****************************************************************************
 *                              MuiWSDialogs.pp                              *
 *                              ---------------                              *
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
unit MuiWSDialogs;

{$mode objfpc}{$H+}

interface

{$if defined(AROS) and defined(VER3_0)}
  {$define USE_OLD_ASL}
{$endif}
{$if defined(MorphOS) or defined(Amiga68k)}
  {$define USE_OLD_ASL}
{$endif}

uses
  // RTL + LCL
  AGraphics, SysUtils, Classes, LCLType, LCLProc, Dialogs, Controls, Forms, Graphics,
  exec, asl, utility, tagsparamshelper, mui, intuition, MuibaseUnit, MUIformsunit,
  AmigaDos, Math,
  {$ifndef MorphOS}
  workbench,
  {$endif}
  {$if defined(MorphOS) or defined(Amiga68K)}
  amigalib,
  {$endif}
  muiglobal,
  // Widgetset
  WSDialogs, WSLCLClasses;

type

  { TMuiWSCommonDialog }

  TMuiWSCommonDialog = class(TWSCommonDialog)
  private
  protected
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure DestroyHandle(const ACommonDialog: TCommonDialog); override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSFileDialog }

  TMuiWSFileDialog = class(TWSFileDialog)
  private
  protected
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSOpenDialog }

  TMuiWSOpenDialog = class(TWSOpenDialog)
  published
  end;

  { TMuiWSSaveDialog }

  TMuiWSSaveDialog = class(TWSSaveDialog)
  published
  end;

  { TMuiWSSelectDirectoryDialog }

  TMuiWSSelectDirectoryDialog = class(TWSSelectDirectoryDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSColorDialog }

  TMuiWSColorDialog = class(TWSColorDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;

  { TMuiWSColorButton }

  TMuiWSColorButton = class(TWSColorButton)
  published
  end;

  { TMuiWSFontDialog }

  TMuiWSFontDialog = class(TWSFontDialog)
  published
    class function CreateHandle(const ACommonDialog: TCommonDialog): THandle; override;
    class procedure ShowModal(const ACommonDialog: TCommonDialog); override;
  end;


implementation


{ TMuiWSCommonDialog }

{------------------------------------------------------------------------------
  Function: TMuiWSCommonDialog.CreateHandle
  Params:  None
  Returns: Nothing

  Dummy handle creator. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class function TMuiWSCommonDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSCommonDialog.DestroyHandle
  Params:  None
  Returns: Nothing

  Dummy handle destructor. On Qt we don´t need a Handle for common dialogs
 ------------------------------------------------------------------------------}
class procedure TMuiWSCommonDialog.DestroyHandle(const ACommonDialog: TCommonDialog);
begin
  if (ACommonDialog.HandleAllocated) then
    FreeAslRequest(Pointer(ACommonDialog.Handle));
end;

class procedure TMuiWSCommonDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin

end;

{ TMuiWSFileDialog }

class function TMuiWSFileDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
var
  FileDialog: PFileRequester;
begin
  FileDialog := PFileRequester(AllocFileRequest());
  Result := THandle(FileDialog);
end;

//procedure IntuiMsgFunc(iMsg: PIntuiMessage; Req: PFileRequester); cdecl;
//begin
  //writeln('test');
//  DoMethod(MUIApp.obj, MUIM_Application_CheckRefresh, []);
//end;
{$ifdef MorphOS}
type
  PWBArg = ^TWBArg;
  TWBArg = record
    wa_Lock: BPTR;      { a lock descriptor }
    wa_Name: STRPTR;       { a string relative to that lock }
  end;

  WBArgList = Array [1..100] of tWBArg; { Only 1..smNumArgs are valid }
  PWBArgList = ^WBArgList;
{$endif}

{------------------------------------------------------------------------------
  Function: TMuiWSFileDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSFileDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  FileDialog: TFileDialog;
  MuiDialog: PFileRequester;
  TagsList: TATagList;
  MultiSelect: Boolean;
  i: LongInt;

  function GetFilename(FDir, FName: string): string;
  begin
    FDir := Trim(FDir);
    if Length(FDir) = 0 then
      Result := FName
    else
    begin
      if (FDir[Length(FDir)] = DIRECTORYSEPARATOR) or (FDir[Length(FDir)] = ':') then
        Result := FDir + FName
      else
        Result := FDir + DIRECTORYSEPARATOR + FName;
    end;
  end;

begin
  {------------------------------------------------------------------------------
    Initialization of variables
   ------------------------------------------------------------------------------}
  MultiSelect:= False;
  TagsList.Clear;
  FileDialog := TFileDialog(ACommonDialog);
  MuiDialog := PFileRequester(FileDialog.Handle);

  //Win := 0;
  //GetAttr(MUIA_Window_Window, MUIApp.MainWin, @Win);


  TagsList.AddTags([
    //ASLFR_Window, Win,
    ASLFR_TitleText, NativeUInt(PChar(ACommonDialog.Title)),
    ASLFR_InitialDrawer, NativeUInt(PChar(TFileDialog(ACommonDialog).InitialDir)),
    ASLFR_InitialFile, NativeUInt(PChar(TFileDialog(ACommonDialog).FileName))
  ]);

  If FileDialog.Filter <> '' then
  begin
    //writeln(FileDialog.Filter);
    TagsList.AddTags([
      ASLFR_InitialPattern, NativeUInt(PChar(FileDialog.Filter)),
      ASLFR_DoPatterns, TagTrue
      ]);
  end;

  if ACommonDialog is TSaveDialog then
  begin
    TagsList.AddTags([ASLFR_DoSaveMode, TagTrue]);
  end else
  begin
    if (ofAllowMultiSelect in TOpenDialog(FileDialog).Options) then
    begin
      MultiSelect:= True;
      TagsList.AddTags([ASLFR_DoMultiSelect, TagTrue]);
    end;
  end;
  if ACommonDialog is TSelectDirectoryDialog then
  begin
    TagsList.AddTags([ASLFR_DrawersOnly, TagTrue]);
  end;
  //
  //Hook.h_Entry := NativeUInt(@IntuiMsgFunc);
  //Hook.h_SubEntry := 0;
  //Hook.h_Data := MuiDialog;
  //TagsList.AddTags([ASLFR_UserData, NativeUInt(MUIApp), ASLFR_IntuiMsgFunc, NativeUInt(@Hook)]);//}
{$ifndef USE_OLD_ASL}
  {$ifdef Amiga}
  if AslRequest(MuiDialog, TagsList) then
  {$else}
  if MUI_AslRequest(MuiDialog, TagsList) then
  {$endif}
  begin
    FileDialog.FileName := GetFilename(string(MuiDialog^.fr_Drawer), string(MuiDialog^.fr_file));
    if MultiSelect then
    begin
      FileDialog.Files.Clear;
      for i := 1 to  MuiDialog^.fr_NumArgs do
      begin
        FileDialog.Files.Add(GetFilename(string(MuiDialog^.fr_Drawer), string(PWBArgList(MuiDialog^.fr_ArgList)^[i].wa_Name)));
      end;
    end;
    FileDialog.UserChoice := mrOK;
  end else
{$else}
  {$ifdef Amiga}
  if Boolean(AslRequest(MuiDialog, TagsList)) then
  {$else}
  if MUI_AslRequest(MuiDialog, TagsList) then
  {$endif}
  begin
    FileDialog.FileName := GetFilename(string(MuiDialog^.rf_Dir), string(MuiDialog^.rf_file));
    if MultiSelect then
    begin
      FileDialog.Files.Clear;
      for i := 1 to  MuiDialog^.rf_NumArgs do
      begin
        FileDialog.Files.Add(GetFilename(string(MuiDialog^.rf_Dir), string(PWBArgList(MuiDialog^.rf_ArgList)^[i].wa_Name)));
      end;
    end;
    FileDialog.UserChoice := mrOK;
  end else
{$endif}
    FileDialog.UserChoice := mrCancel;
end;

{ TMuiWSSelectDirectoryDialog }

class function TMuiWSSelectDirectoryDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSSelectDirectoryDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSSelectDirectoryDialog.ShowModal(const ACommonDialog: TCommonDialog);
begin
end;

{ TMuiWSColorDialog }

class function TMuiWSColorDialog.CreateHandle(const ACommonDialog: TCommonDialog): THandle;
begin
  Result := 1;
end;

{------------------------------------------------------------------------------
  Function: TMuiWSColorDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}

function ColLongWord(c: Byte): LongWord;
begin
  Result := c or (c shl 8) or (c shl 16) or (c shl 24);
end;

class procedure TMuiWSColorDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  ColorDialog: TColorDialog absolute ACommonDialog;
  AppTags: TATagList;
  GrpTags: TATagList;
  BGrpTags: TATagList;
  WinTags: TATagList;
  PalTags: TATagList;
  SetTags: TATagList;
  LocalApp: PObject_;
  Win: PObject_;
  Palette: PObject_;
  Group: PObject_;
  BGroup: PObject_;
  but1, but2: PObject_;
  sigs: LongWord;
  Res: Integer;
  r,g,b: NativeUInt;
  DefWidth, DefHeight: Integer;
begin
  R := ColLongWord(Red(ColorDialog.Color));
  G := ColLongWord(Green(ColorDialog.Color));
  B := ColLongWord(Blue(ColorDialog.Color));
  //
  PalTags.AddTags([
    MUIA_Coloradjust_Red, R,
    MUIA_Coloradjust_Green, G,
    MUIA_Coloradjust_Blue, B
  ]);
  Palette := MUI_NewObjectA(MUIC_ColorAdjust, PalTags);

  but1 := MUI_MakeObject(MUIO_Button, [PtrUInt(PChar('OK'))]);
  but2 := MUI_MakeObject(MUIO_Button, [PTrUInt(PChar('Cancel'))]);

  BGrpTags.AddTags([
    MUIA_Group_Child, NativeUInt(but1),
    MUIA_Group_Child, NativeUInt(but2),
    MUIA_Group_HorizSpacing, 20,
    MUIA_Group_Horiz, TagTrue]);
  BGroup := MUI_NewObjectA(MUIC_Group, BGrpTags);

  GrpTags.AddTags([
    MUIA_Group_Child, NativeUInt(Palette),
    MUIA_Group_Child, NativeUInt(BGroup),
    MUIA_Group_Horiz, TagFalse]);

  Group := MUI_NewObjectA(MUIC_Group, GrpTags);

  DefWidth := 300;
  DefHeight := 300;

  if ColorDialog.Width > 0 then
    DefWidth := ColorDialog.Width;
  if ColorDialog.Height > 0 then
    DefHeight := ColorDialog.Height;

  WinTags.AddTags([
    MUIA_Window_Title, NativeUInt(PChar(ColorDialog.Title)),
    MUIA_Window_RootObject, NativeUInt(Group),
    MUIA_Window_Width, DefWidth,
    MUIA_Window_Height, DefHeight]);
  Win := MUI_NewObjectA(MUIC_Window, WinTags);

  AppTags.AddTags([MUIA_Application_Window, NativeUInt(Win)]);
  LocalApp := MUI_NewObjectA(MUIC_Application, AppTags);

  DoMethod(Win, [MUIM_Notify,
    PtrUInt(MUIA_Window_CloseRequest), TagTrue,
    PtrUInt(LocalApp), 2, PtrUInt(MUIM_Application_ReturnID), PtrUInt(MUIV_Application_ReturnID_Quit)]);

  DoMethod(but2, [MUIM_Notify,
    PtrUInt(MUIA_Pressed), TagTrue,
    PtrUInt(LocalApp), 2, PtrUInt(MUIM_Application_ReturnID), PtrUInt(MUIV_Application_ReturnID_Quit)]);

  DoMethod(but1, [MUIM_Notify,
    PtrUInt(MUIA_Pressed), TagTrue,
    PtrUInt(LocalApp), 2, PtrUInt(MUIM_Application_ReturnID), 42]);

  SetTags.Clear;
  SetTags.AddTag(MUIA_Window_Open, TagTrue);
  SetAttrsA(Win, SetTags);
  Res := -1;
  while true  do
  begin
    Res := Integer(DoMethod(LocalApp, [MUIM_Application_NewInput, PtrInt(@sigs)]));
    case Res of
      MUIV_Application_ReturnID_Quit: begin
        ACommonDialog.UserChoice := mrCancel;
        Break;
      end;
      42: begin
        ACommonDialog.UserChoice := mrOK;
        Break;
      end;
    end;
    if sigs <> 0 then
    begin
      sigs := Wait(sigs or SIGBREAKF_CTRL_C);
      if (Sigs and SIGBREAKF_CTRL_C) <> 0 then
        Break;
    end;
  end;
  MUI_DisposeObject(LocalApp);

  GetAttr(MUIA_Coloradjust_Red, Palette, @R);
  GetAttr(MUIA_Coloradjust_Green, Palette, @G);
  GetAttr(MUIA_Coloradjust_Blue, Palette, @B);

  ColorDialog.Color := RGBToColor((R shr 24) and $FF, (G shr 24) and $FF, (B shr 24) and $FF);
end;

{ TMuiWSFontDialog }

class function TMuiWSFontDialog.CreateHandle(const ACommonDialog: TCommonDialog
  ): THandle;
var
  MuiDialog: PFontRequester;
begin
  MuiDialog := PFontRequester(AllocAslRequest(ASL_FontRequest, nil));
  Result := THandle(MuiDialog);
end;

{------------------------------------------------------------------------------
  Function: TMuiWSFontDialog.ShowModal
  Params:  None
  Returns: Nothing
 ------------------------------------------------------------------------------}
class procedure TMuiWSFontDialog.ShowModal(const ACommonDialog: TCommonDialog);
var
  FDialog: TFontDialog absolute ACommonDialog;
  MuiDialog: PFontRequester;
  TagsList: TATagList;
  PText: string;
  TitleText: string;
  FontName: string;
  Style: LongWord;
begin
  MuiDialog := PFontRequester(FDialog.Handle);
  //
  PText := Trim(FDialog.PreviewText);
  TitleText := Trim(FDialog.Title);
  FontName := Trim(FDialog.Font.Name);

  if PText <> '' then
    TagsList.AddTags([ASLFO_SampleText, NativeUInt(PChar(PText))]);
  if TitleText <> '' then
    TagsList.AddTags([ASLFO_TitleText, NativeUInt(PChar(TitleText))]);
  if FDialog.MinFontSize > 0 then
    TagsList.AddTags([ASLFO_MinHeight, FDialog.MinFontSize]);
  if FDialog.MaxFontSize > 0 then
    TagsList.AddTags([ASLFO_MaxHeight, FDialog.MaxFontSize]);

  // Style Dialog
  TagsList.AddTags([ASLFO_DoStyle, IfThen(not (fdNoStyleSel in FDialog.Options), TagTrue, TagFalse)]);
  // Fixed Width
  TagsList.AddTags([ASLFO_FixedWidthOnly, IfThen(fdFixedPitchOnly in FDialog.Options, TagTrue, TagFalse)]);

  // Initial Things
  if FontName <> '' then
    TagsList.AddTags([ASLFO_InitialName, NativeUInt(PChar(FontName))]);
  if FDialog.Font.Size > 0 then
    TagsList.AddTags([ASLFO_InitialSize, FDialog.Font.Size]);
  // Styles
  Style := FS_NORMAL;
  if fsBold in FDialog.Font.Style then
    Style := Style or FSF_BOLD;
  if fsItalic in FDialog.Font.Style then
    Style := Style or FSF_ITALIC;
  if fsUnderline in FDialog.Font.Style then
    Style := Style or FSF_UNDERLINED;
  TagsList.AddTags([
    ASLFO_InitialStyle, Style,
    ASLFO_DoFrontPen, TagFalse
    ]);
  //
  {$ifdef Amiga}
  if Boolean(AslRequest(MuiDialog, TagsList)) then
  {$else}
  if MUI_AslRequest(MuiDialog, TagsList) then
  {$endif}
  begin
    FontName := string(MuiDialog^.fo_Attr.ta_Name);
    FDialog.Font.Name := stringreplace(Fontname, '.font', '', [rfIgnoreCase, rfReplaceAll]);
    if not (fdNoSizeSel in FDialog.Options) then
      FDialog.Font.Size := MUIDialog^.fo_Attr.ta_YSize;
    if not (fdNoStyleSel in FDialog.Options) then
    begin
      FDialog.Font.Style := [];
      Style := MUIDialog^.fo_Attr.ta_Style;
      if (Style and FSF_BOLD) <> 0 then
        FDialog.Font.Style := FDialog.Font.Style + [fsBold];
      if (Style and FSF_ITALIC) <> 0 then
        FDialog.Font.Style := FDialog.Font.Style + [fsItalic];
      if (Style and FSF_UNDERLINED) <> 0 then
        FDialog.Font.Style := FDialog.Font.Style + [fsUnderline];
    end;
    if (MuiDialog^.fo_Attr.ta_Flags and FPF_PROPORTIONAL) <> 0 then
      FDialog.Font.Pitch := fpDefault
    else
      FDialog.Font.Pitch := fpFixed;
    ACommonDialog.UserChoice := mrOk;
  end else
  begin
    ACommonDialog.UserChoice := mrCancel;
  end;

end;

end.
