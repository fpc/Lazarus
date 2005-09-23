{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Abstract:
    Defines interface to source editors.
}
unit SrcEditorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls;
  
type
  TSrcEditSearchOption = (sesoMatchCase, sesoWholeWord, sesoBackwards,
    sesoEntireScope, sesoSelectedOnly, sesoReplace, sesoReplaceAll, sesoPrompt,
    sesoRegExpr, sesoRegExprMultiLine);
  TSrcEditSearchOptions = set of TSrcEditSearchOption;


  { TSourceEditorInterface }

  TSourceEditorInterface = class
  protected
    function GetLineText: string; virtual; abstract;
    procedure SetLineText(const AValue: string); virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    procedure SetReadOnly(const AValue: Boolean); virtual; abstract;
    function GetSourceText: string; virtual; abstract;
    procedure SetSourceText(const AValue: string); virtual; abstract;
    function GetTopLine: Integer; virtual; abstract;
    procedure SetTopLine(const AValue: Integer); virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetEditorControl: TWinControl; virtual; abstract;
    function GetCodeToolsBuffer: TObject; virtual; abstract;
    function GetCursorScreenXY: TPoint; virtual; abstract;
    function GetCursorTextXY: TPoint; virtual; abstract;
    procedure SetCursorScreenXY(const AValue: TPoint); virtual; abstract;
    procedure SetCursorTextXY(const AValue: TPoint); virtual; abstract;
    function GetBlockBegin: TPoint; virtual; abstract;
    function GetBlockEnd: TPoint; virtual; abstract;
    procedure SetBlockBegin(const AValue: TPoint); virtual; abstract;
    procedure SetBlockEnd(const AValue: TPoint); virtual; abstract;
    function GetSelEnd: Integer; virtual; abstract;
    function GetSelStart: Integer; virtual; abstract;
    procedure SetSelEnd(const AValue: Integer); virtual; abstract;
    procedure SetSelStart(const AValue: Integer); virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    procedure SetLines(const AValue: TStrings); virtual; abstract;
  public
    // selections
    function SelectionAvailable: boolean; virtual; abstract;
    function GetText(OnlySelection: boolean): string; virtual; abstract;
    procedure SelectText(LineNum, CharStart, LineNum2, CharEnd: Integer); virtual; abstract;
    procedure ReplaceLines(StartLine, EndLine: integer; const NewText: string); virtual; abstract;
    procedure CopyToClipboard; virtual; abstract;
    procedure CutToClipboard; virtual; abstract;

    // screen and text position mapping
    function LineCount: Integer; virtual; abstract;
    function TextToScreenPosition(const Position: TPoint): TPoint; virtual; abstract;
    function ScreenToTextPosition(const Position: TPoint): TPoint; virtual; abstract;

    // characters and pixels
    function WidthInChars: Integer; virtual; abstract;
    function HeightInLines: Integer; virtual; abstract;
    function CharWidth: integer; virtual; abstract;
    function CursorInPixel: TPoint; virtual; abstract;

    // update
    procedure BeginUndoBlock; virtual; abstract;
    procedure EndUndoBlock; virtual; abstract;
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    procedure IncreaseIgnoreCodeBufferLock; virtual; abstract;
    procedure DecreaseIgnoreCodeBufferLock; virtual; abstract;
    procedure UpdateCodeBuffer; virtual; abstract;// copy the source from EditorComponent

    // search and replace
    function SearchReplace(const ASearch, AReplace: string;
                           SearchOptions: TSrcEditSearchOption): integer; virtual; abstract;
  public
    property FileName: string read GetFileName;
    property EditorControl: TWinControl read GetEditorControl;// normally TSynEdit
    property CodeToolsBuffer: TObject read GetCodeToolsBuffer;
    property CursorScreenXY: TPoint read GetCursorScreenXY write SetCursorScreenXY;
    property CursorTextXY: TPoint read GetCursorTextXY write SetCursorTextXY;
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property TopLine: Integer read GetTopLine write SetTopLine;
    property SourceText: string read GetSourceText write SetSourceText;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property LineText: string read GetLineText write SetLineText;
    property Lines: TStrings read GetLines write SetLines;
  end;

  { TSourceEditorWindowInterface }

  TSourceEditorWindowInterface = class(TForm)
  protected
    function GetActiveEditor: TSourceEditorInterface; virtual; abstract;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); virtual; abstract;
  public
    property ActiveEditor: TSourceEditorInterface read GetActiveEditor
                                                  write SetActiveEditor;
  end;
  
var
  SourceEditorWindow: TSourceEditorWindowInterface = nil;// set by the IDE

implementation

end.

