unit MissingH2PasDirectives;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$ifndef MPI_INCLUDED}
{$define MPI_INCLUDED}

  { was #define dname def_expr }
  function MPI_CHAR : MPI_Datatype;
  { was #define dname def_expr }
  function MPI_SIGNED_CHAR : MPI_Datatype;

{$ENDIF}

implementation

  { was #define dname def_expr }
  function MPI_CHAR : MPI_Datatype;
      begin
         MPI_CHAR:=MPI_Datatype($4c000101);
      end;

  { was #define dname def_expr }
  function MPI_UNSIGNED_CHAR : MPI_Datatype;
      begin
         MPI_UNSIGNED_CHAR:=MPI_Datatype($4c000102);
      end;

end.

