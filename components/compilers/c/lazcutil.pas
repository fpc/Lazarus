{ Various Lazarus IDE extensions for C sources and compilers.

  Copyright (C) 2008 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit LazCUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, IDEMsgIntf, LazCStrConsts;
  

type

  { TGCCMessageScanner }

  TGCCMessageScanner = class(TIDEMsgScanner)
  public
    function ParseLine(MsgLine: TIDEMessageLine; var Show: boolean): boolean; override;// true if line was handled
  end;
  
  { TGCCMsgScannerType }

  TGCCMsgScannerType = class(TIDEMsgScannerType)
  public
    function ShortDescription: string; override;
    function Description: string; override;
    function StartScan(Lines: TIDEMessageLineList): TIDEMsgScanner; override;
  end;
  
procedure Register;

implementation

procedure Register;
var
  Scanner: TGCCMsgScannerType;
begin
  Scanner:=TGCCMsgScannerType.Create(nil);
  Scanner.Name:='GCC';
  IDEMsgScanners.RegisterType(Scanner);
end;

{ TGCCMessageScanner }

function TGCCMessageScanner.ParseLine(MsgLine: TIDEMessageLine;
  var Show: boolean): boolean;
begin
  DebugLn(['TGCCMessageScanner.ParseLine "',MsgLine.Msg,'"']);
  Result:=false;
end;

{ TGCCMsgScannerType }

function TGCCMsgScannerType.ShortDescription: string;
begin
  Result:=lcGNUProjectCAndCCompiler;
end;

function TGCCMsgScannerType.Description: string;
begin
  Result:=ShortDescription;
end;

function TGCCMsgScannerType.StartScan(Lines: TIDEMessageLineList
  ): TIDEMsgScanner;
begin
  Result:=TGCCMessageScanner.Create(Self,Lines);
end;

end.

