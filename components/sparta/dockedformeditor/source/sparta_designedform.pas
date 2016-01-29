{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_DesignedForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, SrcEditorIntf, sparta_InterfacesMDI;

type
  IDesignedFormIDE = interface(IDesignedForm)
  ['{DFA6C1D8-FA74-443D-B702-82E447F9A111}']
    // for last active window
    function GetLastActiveSourceWindow: TSourceEditorWindowInterface;
    procedure SetLastActiveSourceWindow(AValue: TSourceEditorWindowInterface);
    property LastActiveSourceWindow: TSourceEditorWindowInterface read GetLastActiveSourceWindow write SetLastActiveSourceWindow;
  end;

  IDesignedFakeControl = interface
  ['{31708772-D9FF-42D8-88AD-D27663393177}']
  end;

  IDesignedFakeForm = interface
  ['{A887F50D-13A3-4048-AFFD-F07816FDD08A}']
    // other hacked values
    procedure SetFormBorderStyle(ANewStyle: TFormBorderStyle);
    procedure SetBorderIcons(AVal: TBorderIcons);
    procedure SetFormStyle(AValue : TFormStyle);
    procedure SetCaption(const AValue: string);
    function GetBorderStyle: TFormBorderStyle;
    function GetBorderIcons: TBorderIcons;
    function GetFormStyle: TFormStyle;
    function GetCaption: string;

    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons;
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetFormBorderStyle;
    property FormStyle: TFormStyle read GetFormStyle write SetFormStyle;
    property Caption: string read GetCaption write SetCaption;
  end;

implementation

end.

