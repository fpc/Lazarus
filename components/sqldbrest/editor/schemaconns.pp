unit schemaconns;

// Connections to be included

{$DEFINE HASIBCONNECTION}
{$DEFINE HASMYSQL50CONNECTION}
{$DEFINE HASMYSQL55CONNECTION}
{$DEFINE HASMYSQL4CONNECTION}
{$DEFINE HASPQCONNECTION}
{$DEFINE HASSQLITE3CONNECTION}

{$IF (FPC_FULLVERSION>30302) or not defined(win64)}
 {$DEFINE HASORACLECONNECTION}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20601}
  // MS SQL Server and Sybase ASE connectors were introduced in the FPC 2.7 development branch,
  //  and backported to 2.6.1. Operating systems should match FPC packages\fcl-db\fpmake.pp
  {$IF DEFINED(BEOS) OR DEFINED(HAIKU) OR DEFINED(LINUX) OR DEFINED(FREEBSD) OR DEFINED (NETBSD) OR DEFINED(OPENBSD) OR DEFINED(WIN32) OR DEFINED(WIN64)}
    {$DEFINE HASMSSQLCONNECTION}
    {$DEFINE HASSYBASECONNECTION}
  {$ENDIF}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20603}
  {$DEFINE HASMYSQL56CONNECTION}
{$ENDIF}
{$IF FPC_FULLVERSION >= 20701}
  {$DEFINE HASMYSQL57CONNECTION}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF HASIBCONNECTION}
  ibconnection,
  {$ENDIF}
  {$IFDEF HASMSSQLCONNECTION}
  // mssqlconn provide both MS SQL Server and Sybase ASE connectors.
  mssqlconn,
  {$ENDIF}
  odbcconn,
  {$IFDEF HASPQCONNECTION}
  pqconnection,
  {$ENDIF}
  {$IFDEF HASORACLECONNECTION}
  oracleconnection,
  {$ENDIF}
  {$IFDEF HASMYSQL4CONNECTION}
  mysql40conn, mysql41conn,
  {$ENDIF}
  {$IFDEF HASMYSQL50CONNECTION}
  mysql50conn,
  mysql51conn,
  {$ENDIF}
  {$IFDEF HASMYSQL55CONNECTION}
   mysql55conn,
  {$ENDIF}
  {$IFDEF HASMYSQL56CONNECTION}
  mysql56conn,
  {$ENDIF}
  {$IFDEF HASMYSQL57CONNECTION}
  mysql57conn,
  {$ENDIF}
  {$IFDEF HASSQLITE3CONNECTION}
  sqlite3conn,
  {$ENDIF}
  sqldb;

implementation

end.

