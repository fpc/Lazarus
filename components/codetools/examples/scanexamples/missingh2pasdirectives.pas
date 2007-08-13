unit MissingH2PasDirectives;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$ifndef MPIO_INCLUDE}
  {$ifndef HAVE_MPI_DARRAY_SUBARRAY}
    {$undef HAVE_MPI_DARRAY_SUBARRAY}
    {$define HAVE_MPI_DARRAY_SUBARRAY}
  {$endif}
  {$ifndef HAVE_PRAGMA_HP_SEC_DEF}
    {$ifndef HAVE_MPI_DARRAY_SUBARRAY}
    type c = char;
    {$endif}
  {$endif}
{$endif}

{$ifndef MPI_INCLUDED}
{$define MPI_INCLUDED}

  { was #define dname def_expr }
  function MPI_CHAR : MPI_Datatype;
  { was #define dname def_expr }
  function MPI_SIGNED_CHAR : MPI_Datatype;

{$ENDIF}

{$ifndef MPI_INCLUDED2}

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
  function MPI_SIGNED_CHAR : MPI_Datatype;
      begin
         MPI_SIGNED_CHAR:=MPI_Datatype($4c000118);
      end;

  { was #define dname def_expr }
  function MPI_CHAR : MPI_Datatype;
      begin
         MPI_CHAR:=MPI_Datatype($4c000101);
      end;

  { was #define dname def_expr }
  function MPI_SIGNED_CHAR : MPI_Datatype;
      begin
         MPI_SIGNED_CHAR:=MPI_Datatype($4c000118);
      end;

end.

