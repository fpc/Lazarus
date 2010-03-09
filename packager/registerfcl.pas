{  $Id$  }
{
 /***************************************************************************
                            registerlcl.pas
                            ---------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Registration of the FCL components.
}
unit RegisterFCL;

{$mode objfpc}{$H+}

interface

uses
  LazarusPackageIntf,
  Classes, SysUtils, Process, DB, BufDataset, SimpleIPC, XMLCfg, EventLog;

procedure Register;

implementation

procedure RegisterProcess;
begin
  RegisterComponents('System',[TProcess]);
end;

procedure RegisterDB;
begin
  RegisterComponents('Data Access',[TDatasource
// Only for fpc version 2.4.1 and above
{$IFNDEF VER2_4_0}
                                    ,TBufDataset
{$ENDIF}
                                    ]);
end;

procedure RegisterSimpleIPC;
begin
  RegisterComponents('System',[TSimpleIPCClient, TSimpleIPCServer]);
end;

procedure RegisterXMLCfg;
begin
  RegisterComponents('System',[TXMLConfig]);
end;

procedure RegisterEventLog;
begin
  RegisterComponents('System',[TEventLog]);
end;

procedure Register;
begin
  RegisterUnit('DB',@RegisterDB);
  RegisterUnit('Process',@RegisterProcess);
  RegisterUnit('SimpleIPC', @RegisterSimpleIPC);
  RegisterUnit('XMLCfg',@RegisterXMLCfg);
  RegisterUnit('EventLog',@RegisterEventLog);
end;

end.

