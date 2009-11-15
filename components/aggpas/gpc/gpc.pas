(*
===========================================================================

Project:   Generic Polygon Clipper

           A new algorithm for calculating the difference, intersection,
           exclusive-or or union of arbitrary polygon sets.

File:      gpc.pas
Author:    Alan Murta (gpc@cs.man.ac.uk)
Version:   2.30
Date:      17th October 1998

Pascal port by: Stefan Schedel (Stefan.Schedel@loewe.de)

Copyright: (C) 1997, Advanced Interfaces Group, University of Manchester.
	     All rights reserved.

           This software may be freely copied, modified, and redistributed
           provided that this copyright notice is preserved on all copies.
           The intellectual property rights of the algorithms used reside
           with the University of Manchester Advanced Interfaces Group.

           You may not distribute this software, in whole or in part, as
           part of any commercial product without the express consent of
           the author.

           There is no warranty or other guarantee of fitness of this
           software for any purpose. It is provided solely "as is".

===========================================================================
*)

unit GPC;

interface
uses
  Windows;
//===========================================================================
//                               Constants
//===========================================================================

const
  Version      = 'GPC_VERSION "2.30"';
  GPC_EPSILON  : double =  2.2204460492503131E-16;  { from float.h }

//===========================================================================
//                           Public Data Types
//===========================================================================

type

  Tgpc_op =                                 { Set operation type                }
  (
    GPC_DIFF,                               { Difference                        }
    GPC_INT,                                { Intersection                      }
    GPC_XOR,                                { Exclusive or                      }
    GPC_UNION                               { Union                             }
  );

  Tgpc_vertex = record                      { Polygon vertex structure          }
    x : double;                             { Vertex x component                }
    y : double;                             { vertex y component                }
  end;

  Pgpc_vertex_array = ^Tgpc_vertex_array;   { Helper Type for indexing          }
  Tgpc_vertex_array = array[0.. MaxInt div sizeof(Tgpc_vertex) - 1] of Tgpc_vertex;

  Pgpc_vertex_list = ^Tgpc_vertex_list;     { Vertex list structure             }
  Tgpc_vertex_list = record
    num_vertices : integer;                 { Number of vertices in list        }
    vertex       : Pgpc_vertex_array;       { Vertex array pointer              }
  end;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..MaxInt div sizeof(Integer) - 1] of Integer;

  Pgpc_vertex_list_array = ^Tgpc_vertex_list_array; { Helper Type for indexing  }
  Tgpc_vertex_list_array = array[0.. MaxInt div sizeof(Tgpc_vertex) - 1] of Tgpc_vertex_list;

  Pgpc_polygon = ^Tgpc_polygon;
  Tgpc_polygon = record                     { Polygon set structure             }
    num_contours : integer;                 { Number of contours in polygon     }
    hole         : PIntegerArray;           { Hole / external contour flags     }
    contour      : Pgpc_vertex_list_array;  { Contour array pointer             }
  end;

  Pgpc_tristrip = ^Tgpc_tristrip;           { Tristrip set structure            }
  Tgpc_tristrip = record
    num_strips  : integer;                  { Number of tristrips               }
    strip       : Pgpc_vertex_list_array;   { Tristrip array pointer            }
  end;



//===========================================================================
//                       Public Function Prototypes
//===========================================================================


procedure gpc_read_polygon        (var f : text; p : Pgpc_polygon);

procedure gpc_write_polygon       (var f : text; p : Pgpc_polygon);

procedure gpc_add_contour         (polygon : Pgpc_polygon;
                                   contour : Pgpc_vertex_list;
                                   hole    : integer);

procedure gpc_polygon_clip        (set_operation   : Tgpc_op;
                                   subject_polygon : Pgpc_polygon;
                                   clip_polygon    : Pgpc_polygon;
                                   result_polygon  : Pgpc_polygon);

procedure gpc_free_polygon        (polygon         : Pgpc_polygon);

procedure gpc_free_tristrip       (tristrip        : Pgpc_tristrip);




implementation

uses
  SysUtils,
  Math;



//===========================================================================
//                                Constants
//===========================================================================

const
  DBL_MAX : double = MaxDouble;

  DBL_DIG  = 15;

  FFALSE = 0;
  FTRUE  = 1;

  LEFT   = 0;
  RIGHT  = 1;

  ABOVE  = 0;
  BELOW  = 1;

  CLIP   = 0;
  SUBJ   = 1;

  INVERT_TRISTRIPS  = FALSE;



//===========================================================================
//                            Private Data Types
//===========================================================================

type
  Tvertex_type =
  (                                         { Edge intersection classes         }
   NUL,                                     { Empty non-intersection            }
   EMX,                                     { External maximum                  }
   ELI,                                     { External left intermediate        }
   TED,                                     { Top edge                          }
   ERI,                                     { External right intermediate       }
   RED,                                     { Right edge                        }
   IMM,                                     { Internal maximum and minimum      }
   IMN,                                     { Internal minimum                  }
   EMN,                                     { External minimum                  }
   EMM,                                     { External maximum and minimum      }
   LED,                                     { Left edge                         }
   ILI,                                     { Internal left intermediate        }
   BED,                                     { Bottom edge                       }
   IRI,                                     { Internal right intermediate       }
   IMX,                                     { Internal maximum                  }
   FUL                                      { Full non-intersection             }
  );

Th_state =                                  { Horizontal edge states            }
  (
   NH,                                      { No horizontal edge                }
   BH,                                      { Bottom horizontal edge            }
   TH                                       { Top horizontal edge               }
  );

Tbundle_state =
  (
   UNBUNDLED,
   BUNDLE_HEAD,
   BUNDLE_TAIL
  );

  PPvertex_node = ^Pvertex_node;
  Pvertex_node = ^Tvertex_node;             { Internal vertex list datatype     }
  Tvertex_node = record
    x          : double;                    { X coordinate component            }
    y          : double;                    { Y coordinate component            }
    next       : Pvertex_node;              { Pointer to next vertex in list    }
  end;

  Pvertex_node_array = ^Tvertex_node_array; { Helper type for indexing          }
  Tvertex_node_array = array[0..1] of Pvertex_node;


  PPpolygon_node = ^Ppolygon_node;
  Ppolygon_node = ^Tpolygon_node;
  Tpolygon_node = record
    active     : integer;
    hole       : integer;
    v          : array[0..1] of Pvertex_node;
    Next       : Ppolygon_node;
    proxy      : Ppolygon_node;
  end;

  PPedge_node = ^Pedge_node;
  Pedge_node = ^Tedge_node;
  Tedge_node = record
    vertex     : Tgpc_vertex;               { Piggy-backed contour vertex data  }
    bot        : Tgpc_vertex;               { Edge lower (x, y) coordinate      }
    top        : Tgpc_vertex;               { Edge upper (x, y) coordinate      }
    xb         : double;                    { Scanbeam bottom x coordinate      }
    xt         : double;                    { Scanbeam top x coordinate         }
    dx         : double;                    { Change in x for a unit y increase }
    typ        : integer;                   { Clip / subject edge flag          }
    bundle     : array[0..1, 0..1] of integer;{ Bundle edge flags               }
    bside      : array[0..1] of integer;    { Bundle left / right indicators    }
    bstate     : array[0..1] of Tbundle_state;{ Edge bundle state                }
    outp       : array[0..1] of Ppolygon_node;{ Output polygon / tristrip pointer }
    prev       : Pedge_node;                { Previous edge in the AET          }
    next       : Pedge_node;                { Next edge in the AET              }
    pred       : Pedge_node;                { Edge connected at the lower end   }
    succ       : Pedge_node;                { Edge connected at the upper end   }
    next_bound : Pedge_node;                { Pointer to next bound in LMT      }
  end;

  PPedge_node_array = ^Pedge_node_array;
  Pedge_node_array = ^Tedge_node_array;
  Tedge_node_array = array[0..MaxInt div sizeof(Tedge_node) - 1] of Tedge_node;

  PPlmt_node = ^Plmt_node;
  Plmt_node = ^Tlmt_node;
  Tlmt_node = record                        { Local minima table                }
    y          : double;                    { Y coordinate at local minimum     }
    first_bound: Pedge_node;                { Pointer to bound list             }
    next       : Plmt_node;                 { Pointer to next local minimum     }
  end;

  PPsb_tree = ^Psb_tree;
  Psb_tree = ^Tsb_tree;
  Tsb_tree = record                         { Scanbeam tree                     }
    y          : double;                    { Scanbeam node y value             }
    less       : Psb_tree;                  { Pointer to nodes with lower y     }
    more       : Psb_tree;                  { Pointer to nodes with higher y    }
  end;


  PPit_node = ^Pit_node;
  Pit_node = ^Tit_node;                     { Intersection table                }
  Tit_node = record
    ie          : array[0..1] of Pedge_node;{ Intersecting edge (bundle) pair   }
    point       : Tgpc_vertex;              { Point of intersection             }
    next        : Pit_node;                 { The next intersection table node  }
  end;

  PPst_node = ^Pst_node;
  Pst_node = ^Tst_node;                     { Sorted edge table                 }
  Tst_node = record
    edge        : Pedge_node;               { Pointer to AET edge               }
    xb          : double;                   { Scanbeam bottom x coordinate      }
    xt          : double;                   { Scanbeam top x coordinate         }
    dx          : double;                   { Change in x for a unit y increase }
    prev        : Pst_node;                 { Previous edge in sorted list      }
  end;

  Pbbox = ^Tbbox;
  Tbbox = record                            { Contour axis-aligned bounding box }
    xmin        : double;                   { Minimum x coordinate              }
    ymin        : double;                   { Minimum y coordinate              }
    xmax        : double;                   { Maximum x coordinate              }
    ymax        : double;                   { Maximum y coordinate              }
  end;

  PbboxArray = ^TbboxArray;
  TbboxArray = array[0..MaxInt div sizeof(Tbbox) - 1] of Tbbox;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array[0..MaxInt div sizeof(double) - 1] of double;



//===========================================================================
//               C Macros, defined as function for PASCAL
//===========================================================================

function EQ(a, b : double) : boolean; begin EQ := abs(a - b) <= gpc_epsilon end;
function NE(a, b : double) : boolean; begin NE := abs(a - b) > gpc_epsilon end;
function GT(a, b : double) : boolean; begin GT := (a - b) > gpc_epsilon end;
function LT(a, b : double) : boolean; begin LT := (b - a) > gpc_epsilon end;
function GE(a, b : double) : boolean; begin GE := not LT(a, b) end;
function LE(a, b : double) : boolean; begin LE := not GT(a, b) end;

function PREV_INDEX(i, n : integer) : integer; begin PREV_INDEX := ((i - 1 + n) mod n); end;
function NEXT_INDEX(i, n : integer) : integer; begin NEXT_INDEX := ((i + 1) mod n); end;
function OPTIMAL(v : Pgpc_vertex_array; i, n : integer) : boolean;
begin
  OPTIMAL := NE(v[PREV_INDEX(i, n)].y, v[i].y) or NE(v[NEXT_INDEX(i, n)].y, v[i].y);
end;

function FWD_MIN(v : Pedge_node_array; i, n : integer) : boolean;
begin
  FWD_MIN := GE(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y) and GT(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
end;

function NOT_FMAX(v : Pedge_node_array; i, n : integer) : boolean;
begin
  NOT_FMAX := GT(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
end;

function REV_MIN(v : Pedge_node_array; i, n : integer) : boolean;
begin
  REV_MIN := GT(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y) and GE(v[NEXT_INDEX(i, n)].vertex.y, v[i].vertex.y);
end;


function NOT_RMAX(v : Pedge_node_array; i, n : integer) : boolean;
begin
  NOT_RMAX := GT(v[PREV_INDEX(i, n)].vertex.y, v[i].vertex.y);
end;


procedure MALLOC(var p : pointer; b : integer; s : string);
begin
  GetMem(p, b); if (p = nil) and (b <> 0) then raise Exception.Create(s);
end;


procedure add_vertex(var p : Pvertex_node; x, y : double);
begin
  if p = nil then
    begin
      MALLOC(pointer(p), sizeof(Tvertex_node), 'tristrip vertex creation');
      p.x := x;
      p.y := y;
      p.next := nil;
    end
  else
    { Head further down the list }
    add_vertex(p.next, x, y);
end;


procedure VERTEX(var e : Pedge_node; p, s : integer; var x, y : double);
begin
  add_vertex(e.outp[p].v[s], x, y);
  Inc(e.outp[p].active);
end;


procedure P_EDGE(var d, e : Pedge_node; p : integer; var i, j : double);
begin
  d := e;
  repeat d := d.prev until d.outp[p] = nil;
  i := d.bot.x + d.dx * (j - d.bot.y);
end;

procedure N_EDGE(var d, e : Pedge_node; p : integer; var i, j : double);
begin
  d := e;
  repeat d := d.next; until d.outp[p] = nil;
  i := d.bot.x + d.dx * (j - d.bot.y);
end;


procedure Free(var p : pointer);
begin
  FreeMem(p); p := nil;
end;


procedure CFree(var p : pointer);
begin
  if p <> nil then Free(p);
end;



//===========================================================================
//                               Global Data
//===========================================================================



{ Horizontal edge state transitions within scanbeam boundary }
const
  next_h_state : array[0..2, 0..5] of Th_state =
  {        ABOVE     BELOW     CROSS }
  {        L   R     L   R     L   R }
  { NH } ((BH, TH,   TH, BH,   NH, NH),
  { BH }  (NH, NH,   NH, NH,   TH, TH),
  { TH }  (NH, NH,   NH, NH,   BH, BH));




//===========================================================================
//                             Private Functions
//===========================================================================


procedure reset_it(var it : Pit_node);
var
  itn : Pit_node;
begin
  while (it <> nil) do
  begin
    itn := it.next;
    Free(pointer(it));
    it := itn;
  end;
end;


procedure reset_lmt(var lmt : Plmt_node);
var
  lmtn : Plmt_node;
begin
  while lmt <> nil do
  begin
    lmtn := lmt^.next;
    Free(pointer(lmt));
    lmt := lmtn;
  end;
end;


procedure insert_bound(b : PPedge_node_array; e : Pedge_node_array);
var
  existing_bound : pointer;
begin
  if b^ = nil then
    begin
      { Link node e to the tail of the list }
      b^ := e;
    end
  else
    begin
      { Do primary sort on the x field }
      if (LT(e[0].bot.x, b^[0].bot.x)) then
        begin
          { Insert a new node mid-list }
          existing_bound := b^;
          b^ := e;
          b^[0].next_bound := existing_bound;
        end
      else
        begin
          if (EQ(e[0].bot.x, b^[0].bot.x)) then
            begin
              { Do secondary sort on the dx field }
              if (LT(e[0].dx, b^[0].dx)) then
                begin
                  { Insert a new node mid-list }
                  existing_bound := b^;
                  b^ := e;
                  b^[0].next_bound := existing_bound;
                end
              else
                begin
                  { Head further down the list }
                  insert_bound(@(b^[0].next_bound), e);
                end;
            end
          else
            begin
              { Head further down the list }
              insert_bound(@(b^[0].next_bound), e);
            end;
        end;
    end;
end;


function bound_list(var lmt : Plmt_node; y : double) : PPedge_node_array;
var
  existing_node : Plmt_node;
begin
  if lmt = nil then
    begin
      { Add node onto the tail end of the LMT }
      MALLOC(pointer(lmt), sizeof(Tlmt_node), 'LMT insertion');
      lmt.y := y;
      lmt.first_bound := nil;
      lmt.next := nil;
      result := @lmt.first_bound;
    end
  else
    if LT(y, lmt.y) then
      begin
        { Insert a new LMT node before the current node }
        existing_node := lmt;
        MALLOC(pointer(lmt), sizeof(Tlmt_node), 'LMT insertion');
        lmt.y := y;
        lmt.first_bound := nil;
        lmt.next := existing_node;
        result := @lmt.first_bound;
      end
    else
      if EQ(y, lmt.y) then
        { Use this existing LMT node }
        Result := @lmt.first_bound
      else
        { Head further up the LMT }
        Result := bound_list(lmt.next, y);
end;


procedure add_to_sbtree(var entries : integer; var sbtree : Psb_tree; var y : double);
begin
  if sbtree = nil then
    begin
      { Add a new tree node here }
      MALLOC(pointer(sbtree), sizeof(Tsb_tree), 'scanbeam tree insertion');
      sbtree.y := y;
      sbtree.less := nil;
      sbtree.more := nil;
      Inc(entries);
    end
  else
    begin
      if GT(sbtree.y, y) then
        begin
          { Head into the 'less' sub-tree }
          add_to_sbtree(entries, sbtree.less, y);
        end
      else
        begin
          if LT(sbtree.y, y) then
          begin
            { Head into the 'more' sub-tree }
            add_to_sbtree(entries, sbtree.more, y);
          end;
        end;
    end;
end;


procedure build_sbt(var entries : integer; var sbt : TDoubleArray; sbtree : Psb_tree);
begin
  if sbtree.less <> nil then
    build_sbt(entries, sbt, sbtree.less);
  sbt[entries] := sbtree.y;
  Inc(entries);
  if sbtree.more <> nil then
    build_sbt(entries, sbt, sbtree.more);
end;


procedure free_sbtree(var sbtree : Psb_tree);
begin
  if sbtree <> nil then
  begin
    free_sbtree(sbtree.less);
    free_sbtree(sbtree.more);
    Free(pointer(sbtree));
  end;
end;


function count_optimal_vertices(c : Tgpc_vertex_list) : integer;
var
  i  : integer;
begin
  Result := 0;

  { Ignore non-contributing contours }
  if c.num_vertices > 0 then
  begin
    for i := 0 to c.num_vertices - 1 do
      { Ignore superfluous vertices embedded in horizontal edges }
      if OPTIMAL(c.vertex, i, c.num_vertices) then Inc(Result);
  end;
end;


function build_lmt(var lmt : Plmt_node; var sbtree : Psb_tree; var sbt_entries : integer;
                    p : Pgpc_polygon; typ : integer; op : Tgpc_op) : Pedge_node_array;

var
  c, i, min, max, num_edges, v, num_vertices : integer;
  total_vertices, e_index                    : integer;
  e, edge_table                              : Pedge_node_array;
begin
  total_vertices := 0; e_index := 0;

  for c := 0 to p.num_contours - 1 do
    Inc(total_vertices, count_optimal_vertices(p.contour[c]));

  { Create the entire input polygon edge table in one go }
  MALLOC(pointer(edge_table), total_vertices * sizeof(Tedge_node),
         'edge table creation');

  for c := 0 to p.num_contours - 1 do
  begin
    if p.contour[c].num_vertices < 0 then
      begin
        { Ignore the non-contributing contour and repair the vertex count }
        p.contour[c].num_vertices := -p.contour[c].num_vertices;
      end
    else
      begin
        { Perform contour optimisation }
        num_vertices := 0;
        for i := 0 to p.contour[c].num_vertices - 1 do
          if (OPTIMAL(p.contour[c].vertex, i, p.contour[c].num_vertices)) then
          begin
            edge_table[num_vertices].vertex.x := p.contour[c].vertex[i].x;
            edge_table[num_vertices].vertex.y := p.contour[c].vertex[i].y;

            { Record vertex in the scanbeam table }
            add_to_sbtree(sbt_entries, sbtree, edge_table[num_vertices].vertex.y);

            Inc(num_vertices);
          end;

        { Do the contour forward pass }
        for min := 0 to num_vertices - 1 do
        begin
          { If a forward local minimum... }
          if FWD_MIN(edge_table, min, num_vertices) then
          begin
            { Search for the next local maximum... }
            num_edges := 1;
            max := NEXT_INDEX(min, num_vertices);
            while (NOT_FMAX(edge_table, max, num_vertices)) do
            begin
              Inc(num_edges);
              max := NEXT_INDEX(max, num_vertices);
            end;

            { Build the next edge list }
            e := @edge_table[e_index];
            Inc(e_index, num_edges);
            v := min;
            e[0].bstate[BELOW] := UNBUNDLED;
            e[0].bundle[BELOW][CLIP] := FFALSE;
            e[0].bundle[BELOW][SUBJ] := FFALSE;
            for i := 0 to num_edges - 1 do
            begin
              e[i].xb := edge_table[v].vertex.x;
              e[i].bot.x := edge_table[v].vertex.x;
              e[i].bot.y := edge_table[v].vertex.y;

              v := NEXT_INDEX(v, num_vertices);

              e[i].top.x := edge_table[v].vertex.x;
              e[i].top.y := edge_table[v].vertex.y;
              e[i].dx := (edge_table[v].vertex.x - e[i].bot.x) /
                         (e[i].top.y - e[i].bot.y);
              e[i].typ := typ;
              e[i].outp[ABOVE] := nil;
              e[i].outp[BELOW] := nil;
              e[i].next := nil;
              e[i].prev := nil;
              if (num_edges > 1) and (i < (num_edges - 1)) then e[i].succ := @e[i + 1] else
                            e[i].succ :=  nil;
              if (num_edges > 1) and (i > 0) then e[i].pred := @e[i - 1] else e[i].pred := nil;
              e[i].next_bound := nil;
              if op = GPC_DIFF then e[i].bside[CLIP] := RIGHT else e[i].bside[CLIP] := LEFT;
              e[i].bside[SUBJ] := LEFT;
            end;
            insert_bound(bound_list(lmt, edge_table[min].vertex.y), e);
          end;
        end;

        { Do the contour reverse pass }
        for min := 0 to num_vertices - 1 do
        begin
          { If a reverse local minimum... }
          if REV_MIN(edge_table, min, num_vertices) then
          begin
            { Search for the previous local maximum... }
            num_edges := 1;
            max := PREV_INDEX(min, num_vertices);
            while NOT_RMAX(edge_table, max, num_vertices) do
            begin
              Inc(num_edges);
              max := PREV_INDEX(max, num_vertices);
            end;

            { Build the previous edge list }
            e := @edge_table[e_index];
            Inc(e_index, num_edges);
            v := min;
            e[0].bstate[BELOW] := UNBUNDLED;
            e[0].bundle[BELOW][CLIP] := FFALSE;
            e[0].bundle[BELOW][SUBJ] := FFALSE;
            for i := 0 to num_edges - 1 do
            begin
              e[i].xb := edge_table[v].vertex.x;
              e[i].bot.x := edge_table[v].vertex.x;
              e[i].bot.y := edge_table[v].vertex.y;

              v := PREV_INDEX(v, num_vertices);

              e[i].top.x := edge_table[v].vertex.x;
              e[i].top.y := edge_table[v].vertex.y;
              e[i].dx := (edge_table[v].vertex.x - e[i].bot.x) /
                         (e[i].top.y - e[i].bot.y);
              e[i].typ := typ;
              e[i].outp[ABOVE] := nil;
              e[i].outp[BELOW] := nil;
              e[i].next := nil;
              e[i].prev := nil;
              if (num_edges > 1) and (i < (num_edges - 1)) then e[i].succ := @e[i + 1] else
                            e[i].succ := nil;
              if (num_edges > 1) and (i > 0) then e[i].pred := @e[i - 1] else e[i].pred := nil;
              e[i].next_bound := nil;
              if op = GPC_DIFF then e[i].bside[CLIP] := RIGHT else e[i].bside[CLIP] := LEFT;
              e[i].bside[SUBJ] := LEFT;
            end;
            insert_bound(bound_list(lmt, edge_table[min].vertex.y), e);
          end;
        end;
      end;
  end;
  Result := edge_table;
end;



procedure add_edge_to_aet(var aet : Pedge_node; edge : Pedge_node; prev : Pedge_node);
begin
  if aet = nil then
    begin
      { Append edge onto the tail end of the AET }
      aet := edge;
      edge.prev := prev;
      edge.next := nil;
    end
  else
    begin
      { Do primary sort on the xb field }
      if LT(edge.xb, aet.xb) then
        begin
          { Insert edge here (before the AET edge) }
          edge.prev := prev;
          edge.next := aet;
          aet.prev := edge;
          aet := edge;
        end
      else
        begin
          if EQ(edge.xb, aet.xb) then
            begin
              { Do secondary sort on the dx field }
              if LT(edge.dx, aet.dx) then
                begin
                  { Insert edge here (before the AET edge) }
                  edge.prev := prev;
                  edge.next := aet;
                  aet.prev := edge;
                  aet := edge;
                end
              else
                begin
                  { Head further into the AET }
                  add_edge_to_aet(aet.next, edge, aet);
                end;
            end
          else
            begin
              { Head further into the AET }
              add_edge_to_aet(aet.next, edge, aet);
            end;
        end;
    end;
end;



procedure add_intersection(var it : Pit_node; edge0, edge1 : Pedge_node; x, y : double);
var
  existing_node : Pit_node;
begin

  if it = nil then
    begin
      { Append a new node to the tail of the list }
      MALLOC(pointer(it), sizeof(Tit_node), 'IT insertion');
      it.ie[0] := edge0;
      it.ie[1] := edge1;
      it.point.x := x;
      it.point.y := y;
      it.next := nil;
    end
  else
    begin
      if GT(it.point.y, y) then
        begin
          { Insert a new node mid-list }
          existing_node := it;
          MALLOC(pointer(it), sizeof(Tit_node), 'IT insertion');
          it.ie[0] := edge0;
          it.ie[1] := edge1;
          it.point.x := x;
          it.point.y := y;
          it.next := existing_node;
        end
      else
        { Head further down the list }
        add_intersection(it.next, edge0, edge1, x, y);
    end;
end;



procedure add_st_edge(var st : Pst_node; var it : Pit_node; edge : Pedge_node; dy : double);
var
  existing_node : Pst_node;
  den, x, y, r  : double;
begin
  if st = nil then
    begin
      { Append edge onto the tail end of the ST }
      MALLOC(pointer(st), sizeof(Tst_node), 'ST insertion');
      st.edge := edge;
      st.xb := edge.xb;
      st.xt := edge.xt;
      st.dx := edge.dx;
      st.prev := nil;
    end
  else
    begin
      den := (st.xt - st.xb) - (edge.xt - edge.xb);

      { If new edge and ST edge don't cross }
      if (GE(edge.xt, st.xt) or EQ(edge.dx, st.dx) or EQ(den, 0.0)) then
        begin
          { No intersection - insert edge here (before the ST edge) }
          existing_node := st;
          MALLOC(pointer(st), sizeof(Tst_node), 'ST insertion');
          st.edge := edge;
          st.xb := edge.xb;
          st.xt := edge.xt;
          st.dx := edge.dx;
          st.prev := existing_node;
        end
      else
        begin
          { Compute intersection between new edge and ST edge }
          r := (edge.xb - st.xb) / den;
          x := st.xb + r * (st.xt - st.xb);
          y := r * dy;

          { Insert the edge pointers and the intersection point in the IT }
          add_intersection(it, st.edge, edge, x, y);

          { Head further into the ST }
          add_st_edge(st.prev, it, edge, dy);

        end;
    end;
end;



procedure build_intersection_table(var it : Pit_node; aet : Pedge_node; dy : double);
var
  st, stp : Pst_node;
  edge    : Pedge_node;
begin

  { Build intersection table for the current scanbeam }
  reset_it(it);
  st := nil;

  { Process each AET edge }
  edge := aet;
  while edge <> nil do
  begin
    if (edge.bstate[ABOVE] = BUNDLE_HEAD) or
       (edge.bundle[ABOVE][CLIP] <> 0) or (edge.bundle[ABOVE][SUBJ] <> 0) then
          add_st_edge(st, it, edge, dy);
    edge := edge.next;
  end;

  { Free the sorted edge table }
  while st <> nil do
  begin
    stp := st.prev;
    Free(pointer(st));
    st := stp;
  end;
end;



function count_contours(polygon : Ppolygon_node) : integer;
var
  nv       : integer;
  v, nextv : Pvertex_node;
begin

  Result := 0;
  while polygon <> nil do
  begin
    if polygon.active <> 0 then
    begin
      { Count the vertices in the current contour }
      nv := 0;
      v := polygon.proxy.v[LEFT];
      while v <> nil do begin Inc(nv); v := v.next; end;

      { Record valid vertex counts in the active field }
      if (nv > 2) then
        begin
          polygon.active := nv;
          Inc(Result);
        end
      else
        begin
          { Invalid contour: just free the heap }
          v := polygon.proxy.v[LEFT];
          while v <> nil do begin nextv := v.next; FREE(pointer(v)); v := nextv; end;
          polygon.active := 0;
        end;
    end;

    polygon := polygon.next;
  end;
end;


procedure add_left(p : Ppolygon_node; x, y : double);
var
  nv : Pvertex_node;
begin
  { Create a new vertex node and set its fields }
  MALLOC(pointer(nv), sizeof(Tvertex_node), 'vertex node creation');
  nv.x := x;
  nv.y := y;

  { Add vertex nv to the left end of the polygon's vertex list }
  nv.next := P.proxy.v[LEFT];

  { Update proxy[LEFT] to point to nv }
  P.proxy.v[LEFT] := nv;
end;


procedure merge_left(P : Ppolygon_node; Q :Ppolygon_node; list : Ppolygon_node);
var
  target : Ppolygon_node;
begin
  { Label contour as a hole }
  q.proxy.hole := FTRUE;

  if P.proxy <> Q.proxy then
  begin
    { Assign P's vertex list to the left end of Q's list }
    P.proxy.v[RIGHT].next := Q.proxy.v[LEFT];
    Q.proxy.v[LEFT] := P.proxy.v[LEFT];

    { Redirect any P->proxy references to Q->proxy }
    target := P.proxy;
    while list <> nil do
    begin
      if list.proxy = target then
      begin
        list.active := FFALSE;
        list.proxy := Q.proxy;
      end;
      list := list.next;
    end;
  end;
end;


procedure add_right(P : Ppolygon_node; x, y : double);
var
  nv : Pvertex_node;
begin

  { Create a new vertex node and set its fields }
  MALLOC(pointer(nv), sizeof(Tvertex_node), 'vertex node creation');
  nv.x := x;
  nv.y := y;
  nv.next := nil;

  { Add vertex nv to the right end of the polygon's vertex list }
  P.proxy.v[RIGHT].next := nv;

  { Update proxy.v[RIGHT] to point to nv }
  P.proxy.v[RIGHT] := nv;
end;


procedure merge_right(P : Ppolygon_node; Q : Ppolygon_node; list : Ppolygon_node);
var
  target : PPolygon_node;
begin
  { Label contour as external }
  Q.proxy.hole := FFALSE;

  if P.proxy <> Q.proxy then
  begin
    { Assign P's vertex list to the right end of Q's list }
    Q.proxy.v[RIGHT].next := P.proxy.v[LEFT];
    Q.proxy.v[RIGHT] := P.proxy.v[RIGHT];

    { Redirect any P->proxy references to Q->proxy }
    target := P.proxy;
    while list <> nil do
    begin
      if list.proxy = target then
      begin
        list.active := FFALSE;
        list.proxy := Q.proxy;
      end;
      list := list.next;
    end;
  end;
end;


procedure add_local_min(P : PPpolygon_node; edge : Pedge_node; x, y : double);
var
  nv           : Pvertex_node;
  existing_min : Ppolygon_node;
begin
  existing_min := p^;

  MALLOC(pointer(P^), sizeof(Tpolygon_node), 'polygon node creation');

  { Create a new vertex node and set its fields }
  MALLOC(pointer(nv), sizeof(Tvertex_node), 'vertex node creation');
  nv.x := x;
  nv.y := y;
  nv.next := nil;

  { Initialise proxy to point to p itself }
  p^.proxy := P^;
  p^.active := FTRUE;
  p^.next := existing_min;

  { Make v[LEFT] and v[RIGHT] point to new vertex nv }
  p^.v[LEFT] := nv;
  p^.v[RIGHT] := nv;

  { Assign polygon p to the edge }
  edge.outp[ABOVE] := p^;
end;


function count_tristrips(tn : Ppolygon_node) : integer;
begin
  Result := 0;

  while tn <> nil do
  begin
    if tn.active > 2 then Inc(Result);
    tn := tn.next;
  end;
end;

(*
procedure add_vertex(t : PPvertex_node; x, y : double)
begin
  if t^ <> nil then
    begin
      MALLOC(t^, sizeof(Tvertex_node), ttristrip vertex creationt');
      t^.x := x;
      t^.y := y;
      t^.next := nil;
    end
  else
    { Head further down the list }
    add_vertex(@t^.next, x, y);
end;
*)

procedure new_tristrip(var tn : Ppolygon_node; edge : Pedge_node; x, y : double);
begin
    if tn = nil then
    begin
      MALLOC(pointer(tn), sizeof(Tpolygon_node), 'tristrip node creation');
      tn.next := nil;
      tn.v[LEFT] := nil;
      tn.v[RIGHT] := nil;
      tn.active := 1;
      add_vertex(tn.v[LEFT], x, y);
      edge.outp[ABOVE] := tn;
    end
  else
    { Head further down the list }
    new_tristrip(tn.next, edge, x, y);
end;


function create_contour_bboxes(p : Pgpc_polygon) : PbboxArray;
var
  c, v : integer;
begin
  MALLOC(pointer(Result), p.num_contours * sizeof(Tbbox), 'Bounding box creation');

  { Construct contour bounding boxes }
  for c := 0 to p.num_contours - 1 do
  begin
    { Initialise bounding box extent }
    Result[c].xmin := DBL_MAX;
    Result[c].ymin := DBL_MAX;
    Result[c].xmax := -DBL_MAX;
    Result[c].ymax := -DBL_MAX;

    for v := 0 to p.contour[c].num_vertices - 1 do
    begin
      { Adjust bounding Result }
      if (p.contour[c].vertex[v].x < Result[c].xmin) then
        Result[c].xmin := p.contour[c].vertex[v].x;
      if (p.contour[c].vertex[v].y < Result[c].ymin) then
        Result[c].ymin := p.contour[c].vertex[v].y;
      if (p.contour[c].vertex[v].x > Result[c].xmax) then
        Result[c].xmax := p.contour[c].vertex[v].x;
      if (p.contour[c].vertex[v].y > Result[c].ymax) then
          Result[c].ymax := p.contour[c].vertex[v].y;
    end;
  end;
end;


procedure minimax_test(subj : Pgpc_polygon; clip : Pgpc_polygon; op : Tgpc_op);
var
  s_bbox, c_bbox : PbboxArray;
  s, c           : integer;
  o_table        : PIntegerArray;
  overlap        : integer;
begin
  s_bbox := create_contour_bboxes(subj);
  c_bbox := create_contour_bboxes(clip);

  MALLOC(pointer(o_table), subj.num_contours * clip.num_contours * sizeof(Integer),
         'overlap table creation');

  { Check all subject contour bounding boxes against clip boxes }
  for s := 0 to subj.num_contours - 1 do
    for c := 0 to clip.num_contours - 1 do
      o_table[c * subj.num_contours + s] := integer(
             (not(LT(s_bbox[s].xmax, c_bbox[c].xmin) or
                GT(s_bbox[s].xmin, c_bbox[c].xmax))) and
             (not(LT(s_bbox[s].ymax, c_bbox[c].ymin) or
                GT(s_bbox[s].ymin, c_bbox[c].ymax))));

  { For each clip contour, search for any subject contour overlaps }
  for c := 0 to clip.num_contours - 1 do
  begin
    overlap := 0; s := 0;
    while (overlap = 0) and (s < subj.num_contours) do
    begin
      overlap := o_table[c * subj.num_contours + s];
      Inc(s);
    end;

    if overlap = 0 then
      { Flag non contributing status by negating vertex count }
      clip.contour[c].num_vertices := -clip.contour[c].num_vertices;
  end;

  if (op = GPC_INT) then
  begin
    { For each subject contour, search for any clip contour overlaps }
    for s := 0 to subj.num_contours - 1 do
    begin
      overlap := 0; c := 0;
      while (overlap = 0) and (c < clip.num_contours) do
      begin
        overlap := o_table[c * subj.num_contours + s];
        Inc(c);
      end;

      if overlap = 0 then
        { Flag non contributing status by negating vertex count }
        subj.contour[s].num_vertices := -subj.contour[s].num_vertices;
    end;
  end;

  FREE(pointer(s_bbox));
  FREE(pointer(c_bbox));
  FREE(pointer(o_table));
end;


//===========================================================================
//                             Public Functions
//===========================================================================


procedure gpc_free_polygon(Polygon : Pgpc_polygon);
var
  c : integer;
begin
  for c := 0 to Polygon.num_contours - 1 do
    CFree(pointer(Polygon.contour[c].vertex));

  CFree(pointer(Polygon.hole));
  CFree(pointer(Polygon.contour));
  Polygon.num_contours := 0;
end;


procedure gpc_read_polygon(var f : text; p : Pgpc_polygon);
var
  c, v : integer;
begin
  readln(f, p.num_contours);
  MALLOC(pointer(p.contour), p.num_contours * sizeof(Tgpc_vertex_list), 'contour creation');
  for c := 0 to p.num_contours - 1 do
  begin
    readln(f, p.contour[c].num_vertices);
    MALLOC(pointer(p.contour[c].vertex), p.contour[c].num_vertices * sizeof(Tgpc_vertex), 'vertex creation');
    for v := 0 to p.contour[c].num_vertices - 1 do
    begin
      read(f, p.contour[c].vertex[v].x);
      readln(f, p.contour[c].vertex[v].y);
    end;
  end;
end;


procedure gpc_write_polygon(var f : text; p : Pgpc_polygon);
var
  c, v : integer;
begin
  writeln(f, p.num_contours);
  for c := 0 to p.num_contours - 1 do
  begin
    writeln(f, p.contour[c].num_vertices);
    for v := 0 to p.contour[c].num_vertices - 1 do
      writeln(f, p.contour[c].vertex[v].x:20:DBL_DIG , ' ' , p.contour[c].vertex[v].y:20:DBL_DIG);
  end;
end;


procedure gpc_add_contour(polygon : Pgpc_polygon; contour : Pgpc_vertex_list; hole : integer);
var
  c, v             : integer;
  extended_hole    : PIntegerArray;
  extended_contour : Pgpc_vertex_list_array;
begin

  { Create an extended hole array }
  MALLOC(pointer(extended_hole), (polygon.num_contours + 1)
         * sizeof(integer), 'contour hole addition');

  { Create an extended contour array }
  MALLOC(pointer(extended_contour), (polygon.num_contours + 1)
         * sizeof(Tgpc_vertex_list), 'contour addition');

  { Copy the old contour into the extended contour array }
  for c := 0 to polygon.num_contours - 1 do
  begin
    extended_hole[c] := polygon.hole[c];
    extended_contour[c] := polygon.contour[c];
  end;

  { Copy the new contour onto the end of the extended contour array }
  c := polygon.num_contours;
  extended_hole[c] := hole;
  extended_contour[c].num_vertices := contour.num_vertices;
  MALLOC(pointer(extended_contour[c].vertex), contour.num_vertices
         * sizeof(Tgpc_vertex), 'contour addition');
  for v := 0 to contour.num_vertices - 1 do
    extended_contour[c].vertex[v] := contour.vertex[v];

  { Dispose of the old contour }
  CFREE(pointer(polygon.contour));
  CFREE(pointer(polygon.hole));

  { Update the polygon information }
  Inc(polygon.num_contours);
  polygon.hole := extended_hole;
  polygon.contour := extended_contour;
end;


procedure gpc_polygon_clip(set_operation   : Tgpc_op; subject_polygon : Pgpc_polygon;
                           clip_polygon    : Pgpc_polygon; result_polygon  : Pgpc_polygon);

var
  sbtree                                 : Psb_tree;
  it, intersect                          : Pit_node;
  edge, prev_edge, next_edge, succ_edge  : Pedge_node;
  e0, e1                                 : Pedge_node;
  aet                                    : Pedge_node;
  c_heap, s_heap                         : Pedge_node_array;
  lmt, local_min                         : Plmt_node;
  out_poly, P, Q, poly, npoly, cf        : Ppolygon_node;
  vtx, nv                                : Pvertex_node;
  horiz                                  : array[0..1] of Th_state;
  inn, exists, parity                    : array[0..1] of integer;
  c, v, contributing, search, scanbeam   : integer;
  sbt_entries, _class, bl, br, tl, tr    : integer;
  sbt                                    : PDoubleArray;
  xb, px, yb, yt, dy, ix, iy             : double;
begin
  edge := nil; 
  sbtree := nil; it := nil; aet := nil; lmt := nil;
  out_poly := nil; cf := nil;
  inn[0] := LEFT; inn[1] := LEFT;
  exists[0] := LEFT; exists[1] := LEFT;
  parity[0] := LEFT; parity[1] := LEFT;
  scanbeam := 0; sbt_entries := 0;
  sbt := nil;

  { Test for trivial NULL result cases }
  if ((subject_polygon.num_contours = 0) and (clip_polygon.num_contours = 0))
    or ((subject_polygon.num_contours = 0) and ((set_operation = GPC_INT) or (set_operation = GPC_DIFF)))
    or ((clip_polygon.num_contours = 0) and (set_operation = GPC_INT)) then
  begin
    result_polygon.num_contours := 0;
    result_polygon.hole := nil;
    result_polygon.contour := nil;
    exit;
  end;

  { Identify potentialy contributing contours }
  if (((set_operation = GPC_INT) or (set_operation = GPC_DIFF))
   and (subject_polygon.num_contours > 0) and (clip_polygon.num_contours > 0)) then
    minimax_test(subject_polygon, clip_polygon, set_operation);

  { Build LMT }
  if subject_polygon.num_contours > 0 then
    s_heap := build_lmt(lmt, sbtree, sbt_entries, subject_polygon, SUBJ, set_operation);
  if clip_polygon.num_contours > 0 then
    c_heap := build_lmt(lmt, sbtree, sbt_entries, clip_polygon, CLIP, set_operation);

  { Return a NULL result if no contours contribute }
  if lmt = nil then
  begin
    result_polygon.num_contours := 0;
    result_polygon.hole := nil;
    result_polygon.contour := nil;
    reset_lmt(lmt);
    FREE(pointer(s_heap));
    FREE(pointer(c_heap));
    exit;
  end;

  { Build scanbeam table from scanbeam tree }
  MALLOC(pointer(sbt), sbt_entries * sizeof(double), 'sbt creation');
  build_sbt(scanbeam, sbt^, sbtree);
  scanbeam := 0;
  free_sbtree(sbtree);

  { Allow pointer re-use without causing memory leak }
  if subject_polygon = result_polygon then
    gpc_free_polygon(subject_polygon);
  if clip_polygon = result_polygon then
    gpc_free_polygon(clip_polygon);

  { Invert clip polygon for difference operation }
  if set_operation = GPC_DIFF then
    parity[CLIP] := RIGHT;

  local_min := lmt;

  { Process each scanbeam }
  while (scanbeam < sbt_entries) do
  begin
    { Set yb and yt to the bottom and top of the scanbeam }
    yb := sbt[scanbeam]; Inc(scanbeam);
    if scanbeam < sbt_entries then
    begin
      yt := sbt[scanbeam];
      dy := yt - yb;
    end;

    { === SCANBEAM BOUNDARY PROCESSING ================================ }

    { If LMT node corresponding to yb exists }
    if local_min <> nil then
    begin
      if EQ(local_min.y, yb) then
      begin
        { Add edges starting at this local minimum to the AET }
        edge := local_min.first_bound;
        while edge <> nil do
        begin
          add_edge_to_aet(aet, edge, nil);
          edge := edge.next_bound;
        end;
        local_min := local_min.next;
      end;
    end;

    { Set dummy previous x value }
    px := -DBL_MAX;

    { Create bundles within AET }
    e0 := aet;
    e1 := aet;

    { Set up bundle fields of first edge }
    aet.bundle[ABOVE][integer(aet.typ <> 0)] := integer(NE(aet.top.y, yb));
    aet.bundle[ABOVE][integer(aet.typ = 0)] := FFALSE;
    aet.bstate[ABOVE] := UNBUNDLED;

    next_edge := aet.next;

    while next_edge <> nil do
    begin
      { Set up bundle fields of next edge }
      next_edge.bundle[ABOVE][next_edge.typ] := integer(NE(next_edge.top.y, yb));
      next_edge.bundle[ABOVE][integer(next_edge.typ = 0)] := FFALSE;
      next_edge.bstate[ABOVE] := UNBUNDLED;

      { Bundle edges above the scanbeam boundary if they coincide }
      if next_edge.bundle[ABOVE][next_edge.typ] <> 0 then
      begin
        if (EQ(e0.xb, next_edge.xb) and EQ(e0.dx, next_edge.dx)
	 and NE(e0.top.y, yb)) then
        begin
          next_edge.bundle[ABOVE][next_edge.typ] := next_edge.bundle[ABOVE][next_edge.typ] xor
            e0.bundle[ABOVE][ next_edge.typ];
          next_edge.bundle[ABOVE][integer(next_edge.typ = 0)] :=
            e0.bundle[ABOVE][integer(next_edge.typ = 0)];
          next_edge.bstate[ABOVE] := BUNDLE_HEAD;
          e0.bundle[ABOVE][CLIP] := FFALSE;
          e0.bundle[ABOVE][SUBJ] := FFALSE;
          e0.bstate[ABOVE] := BUNDLE_TAIL;
        end;
        e0 := next_edge;
      end;
      next_edge := next_edge.next;
    end;

    horiz[CLIP] := NH;
    horiz[SUBJ] := NH;

    { Process each edge at this scanbeam boundary }
    edge := aet;
    while edge <> nil do
    begin
      exists[CLIP] := edge.bundle[ABOVE][CLIP] +
                   (edge.bundle[BELOW][CLIP] shl 1);
      exists[SUBJ] := edge.bundle[ABOVE][SUBJ] +
                   (edge.bundle[BELOW][SUBJ] shl 1);

      if (exists[CLIP] <> 0) or (exists[SUBJ] <> 0) then
      begin
        { Set bundle side }
        edge.bside[CLIP] := parity[CLIP];
        edge.bside[SUBJ] := parity[SUBJ];

        { Determine contributing status and quadrant occupancies }
        case set_operation of
          GPC_DIFF,
          GPC_INT: begin
            contributing := integer(   ((exists[CLIP] <> 0) and ((parity[SUBJ] <> 0) or (horiz[SUBJ] <> NH)))
                                    or ((exists[SUBJ] <> 0) and ((parity[CLIP] <> 0) or (horiz[CLIP] <> NH)))
                                    or ((exists[CLIP] <> 0) and (exists[SUBJ] <> 0) and (parity[CLIP] = parity[SUBJ])));
            br := integer((parity[CLIP] <> 0) and (parity[SUBJ] <> 0));
            bl := integer(     ((parity[CLIP] xor edge.bundle[ABOVE][CLIP]) <> 0)
                           and ((parity[SUBJ] xor edge.bundle[ABOVE][SUBJ]) <> 0));
            tr := integer(     ((parity[CLIP] xor integer(horiz[CLIP] <> NH)) <> 0)
                           and ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH)) <> 0));
            tl := integer(     ((parity[CLIP] xor integer(horiz[CLIP] <> NH) xor edge.bundle[BELOW][CLIP]) <> 0)
                           and ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH) xor edge.bundle[BELOW][SUBJ]) <> 0));
            end;

          GPC_XOR: begin
            contributing := integer((exists[CLIP] <> 0) or (exists[SUBJ] <> 0));
            br := integer(parity[CLIP] xor parity[SUBJ]);
            bl := integer(     ((parity[CLIP] xor edge.bundle[ABOVE][CLIP]) <> 0)
                           xor ((parity[SUBJ] xor edge.bundle[ABOVE][SUBJ]) <> 0));
            tr := integer(     ((parity[CLIP] xor integer(horiz[CLIP] <> NH)) <> 0)
                           xor ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH)) <> 0));
            tl := integer(     ((parity[CLIP] xor integer(horiz[CLIP] <> NH) xor edge.bundle[BELOW][CLIP]) <> 0)
                           xor ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH) xor edge.bundle[BELOW][SUBJ]) <> 0));
            end;

          GPC_UNION: begin
            contributing := integer(   ((exists[CLIP] <> 0) and ((parity[SUBJ] = 0) or (horiz[SUBJ] <> NH)))
                                    or ((exists[SUBJ] <> 0) and ((parity[CLIP] = 0) or (horiz[CLIP] <> NH)))
                                    or ((exists[CLIP] <> 0) and (exists[SUBJ] <> 0) and (parity[CLIP] = parity[SUBJ])));

            br := integer((parity[CLIP] <> 0) or (parity[SUBJ] <> 0));
            bl := integer(   ((parity[CLIP] xor edge.bundle[ABOVE][CLIP]) <> 0)
                          or ((parity[SUBJ] xor edge.bundle[ABOVE][SUBJ]) <> 0));
            tr := integer(   ((parity[CLIP] xor integer(horiz[CLIP] <> NH)) <> 0)
                          or ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH)) <> 0));
            tl := integer(   ((parity[CLIP] xor integer(horiz[CLIP] <> NH) xor edge.bundle[BELOW][CLIP]) <> 0)
                          or ((parity[SUBJ] xor integer(horiz[SUBJ] <> NH) xor edge.bundle[BELOW][SUBJ]) <> 0));
            end;
        end;  { case }

        { Update parity }
(*        parity[CLIP] := integer((parity[CLIP] <> 0) xor (edge.bundle[ABOVE][CLIP] <> 0));
        parity[SUBJ] := integer((parity[SUBJ] <> 0) xor (edge.bundle[ABOVE][SUBJ] <> 0));
  *)
        parity[CLIP] := parity[CLIP] xor edge.bundle[ABOVE][CLIP];
        parity[SUBJ] := parity[SUBJ] xor edge.bundle[ABOVE][SUBJ];

        { Update horizontal state }
        if exists[CLIP] <> 0 then
          horiz[CLIP] :=
            next_h_state[integer(horiz[CLIP])]
                        [((exists[CLIP] - 1) shl 1) + parity[CLIP]];
        if exists[SUBJ] <> 0 then
          horiz[SUBJ] :=
            next_h_state[integer(horiz[SUBJ])]
                        [((exists[SUBJ] - 1) shl 1) + parity[SUBJ]];

        _class := tr + (tl shl 1) + (br shl 2) + (bl shl 3);

        if contributing <> 0 then
        begin
          xb := edge.xb;

          case Tvertex_type(_class) of
          EMN,
          IMN: begin
            add_local_min(@out_poly, edge, xb, yb);
            px := xb;
            cf := edge.outp[ABOVE];
            end;
          ERI: begin
            if NE(xb, px) then
            begin
              add_right(cf, xb, yb);
              px := xb;
            end;
            edge.outp[ABOVE] := cf;
            cf := nil;
            end;
          ELI: begin
            add_left(edge.outp[BELOW], xb, yb);
            px := xb;
            cf := edge.outp[BELOW];
            end;
          EMX: begin
            if NE(xb, px) then
            begin
              add_left(cf, xb, yb);
              px := xb;
            end;
            merge_right(cf, edge.outp[BELOW], out_poly);
            cf := nil;
            end;
          ILI: begin
            if NE(xb, px) then
            begin
              add_left(cf, xb, yb);
              px := xb;
            end;
            edge.outp[ABOVE] := cf;
            cf := nil;
            end;
          IRI: begin
            add_right(edge.outp[BELOW], xb, yb);
            px := xb;
            cf := edge.outp[BELOW];
            edge.outp[BELOW] := nil;
            end;
          IMX: begin
            if NE(xb, px) then
            begin
              add_right(cf, xb, yb);
              px := xb;
            end;
            merge_left(cf, edge.outp[BELOW], out_poly);
            cf := nil;
            edge.outp[BELOW] := nil;
            end;
          IMM: begin
            if NE(xb, px) then
            begin
              add_right(cf, xb, yb);
              px := xb;
            end;
            merge_left(cf, edge.outp[BELOW], out_poly);
            edge.outp[BELOW] := nil;
            add_local_min(@out_poly, edge, xb, yb);
            cf := edge.outp[ABOVE];
            end;
          EMM: begin
            if NE(xb, px) then
            begin
              add_left(cf, xb, yb);
              px := xb;
            end;
            merge_right(cf, edge.outp[BELOW], out_poly);
            edge.outp[BELOW] := nil;
            add_local_min(@out_poly, edge, xb, yb);
            cf := edge.outp[ABOVE];
            end;
          LED: begin
            if EQ(edge.bot.y, yb) then
              add_left(edge.outp[BELOW], xb, yb);
            edge.outp[ABOVE] := edge.outp[BELOW];
            px := xb;
            end;
          RED: begin
            if EQ(edge.bot.y, yb) then
              add_right(edge.outp[BELOW], xb, yb);
            edge.outp[ABOVE] := edge.outp[BELOW];
            px := xb;
            end;
          else
          end; { End of case }
        end; { End of contributing conditional }
      end; { End of edge exists conditional }
      edge := edge.next;
    end; { End of AET loop }

    { Delete terminating edges from the AET, otherwise compute xt }
    edge := aet;
    while edge <> nil do
    begin
      if EQ(edge.top.y, yb) then
        begin
          prev_edge := edge.prev;
          next_edge := edge.next;
          if prev_edge <> nil then
            prev_edge.next := next_edge
          else
            aet := next_edge;
          if next_edge <> nil then
            next_edge.prev := prev_edge;

          { Copy bundle head state to the adjacent tail edge if required }
          if (edge.bstate[BELOW] = BUNDLE_HEAD) and (prev_edge <> nil) then
          begin
            if prev_edge.bstate[BELOW] = BUNDLE_TAIL then
            begin
              prev_edge.outp[BELOW] := edge.outp[BELOW];
              prev_edge.bstate[BELOW] := UNBUNDLED;
              if prev_edge.prev <> nil then
                if prev_edge.prev.bstate[BELOW] = BUNDLE_TAIL then
                  prev_edge.bstate[BELOW] := BUNDLE_HEAD;
            end;
          end;
        end
      else
        begin
          if EQ(edge.top.y, yt) then
            edge.xt := edge.top.x
          else
            edge.xt := edge.bot.x + edge.dx * (yt - edge.bot.y);
        end;

      edge := edge.next;
    end;

    if scanbeam < sbt_entries then
    begin
      { === SCANBEAM INTERIOR PROCESSING ============================== }

      build_intersection_table(it, aet, dy);

      { Process each node in the intersection table }
      intersect := it;
      while intersect <> nil do
      begin
        e0 := intersect.ie[0];
        e1 := intersect.ie[1];

        { Only generate output for contributing intersections }
        if      ((e0.bundle[ABOVE][CLIP] <> 0) or (e0.bundle[ABOVE][SUBJ] <> 0))
            and ((e1.bundle[ABOVE][CLIP] <> 0) or (e1.bundle[ABOVE][SUBJ] <> 0)) then
	begin
          P := e0.outp[ABOVE];
          Q := e1.outp[ABOVE];
          ix := intersect.point.x;
          iy := intersect.point.y + yb;

          inn[CLIP] := integer(    ((e0.bundle[ABOVE][CLIP] <> 0) and (e0.bside[CLIP] = 0))
                                or ((e1.bundle[ABOVE][CLIP] <> 0) and (e1.bside[CLIP] <> 0))
                                or ((e0.bundle[ABOVE][CLIP] = 0) and (e1.bundle[ABOVE][CLIP] = 0)
                                    and (e0.bside[CLIP] <> 0) and (e1.bside[CLIP] <> 0)));

          inn[SUBJ] := integer(    ((e0.bundle[ABOVE][SUBJ] <> 0) and (e0.bside[SUBJ] = 0))
                                or ((e1.bundle[ABOVE][SUBJ] <> 0) and (e1.bside[SUBJ] <> 0))
                                or ((e0.bundle[ABOVE][SUBJ] = 0) and (e1.bundle[ABOVE][SUBJ] = 0)
                                    and (e0.bside[SUBJ] <> 0) and (e1.bside[SUBJ] <> 0)));

          { Determine quadrant occupancies }
          case set_operation of

            GPC_DIFF,
            GPC_INT: begin
              tr := integer((inn[CLIP] <> 0) and (inn[SUBJ] <> 0));
              tl := integer(    ((inn[CLIP] xor e1.bundle[ABOVE][CLIP]) <> 0)
                            and ((inn[SUBJ] xor e1.bundle[ABOVE][SUBJ]) <> 0));
              br := integer(    ((inn[CLIP] xor e0.bundle[ABOVE][CLIP]) <> 0)
                            and ((inn[SUBJ] xor e0.bundle[ABOVE][SUBJ]) <> 0));
              bl := integer(    ((inn[CLIP] xor e1.bundle[ABOVE][CLIP] xor e0.bundle[ABOVE][CLIP]) <> 0)
                            and ((inn[SUBJ] xor e1.bundle[ABOVE][SUBJ] xor e0.bundle[ABOVE][SUBJ]) <> 0));
              end;

            GPC_XOR: begin
              tr := integer((inn[CLIP] <> 0) xor (inn[SUBJ] <> 0));
              tl := integer(    (inn[CLIP] xor e1.bundle[ABOVE][CLIP])
                            xor (inn[SUBJ] xor e1.bundle[ABOVE][SUBJ]));
              br := integer(    (inn[CLIP] xor e0.bundle[ABOVE][CLIP])
                            xor (inn[SUBJ] xor e0.bundle[ABOVE][SUBJ]));
              bl := integer(    (inn[CLIP] xor e1.bundle[ABOVE][CLIP] xor e0.bundle[ABOVE][CLIP])
                            xor (inn[SUBJ] xor e1.bundle[ABOVE][SUBJ] xor e0.bundle[ABOVE][SUBJ]));
              end;

            GPC_UNION: begin
              tr := integer((inn[CLIP] <> 0) or (inn[SUBJ] <> 0));
              tl := integer(    ((inn[CLIP] xor e1.bundle[ABOVE][CLIP]) <> 0)
                             or ((inn[SUBJ] xor e1.bundle[ABOVE][SUBJ]) <> 0));
              br := integer(    ((inn[CLIP] xor e0.bundle[ABOVE][CLIP]) <> 0)
                             or ((inn[SUBJ] xor e0.bundle[ABOVE][SUBJ]) <> 0));
              bl := integer(    ((inn[CLIP] xor e1.bundle[ABOVE][CLIP] xor e0.bundle[ABOVE][CLIP]) <> 0)
                             or ((inn[SUBJ] xor e1.bundle[ABOVE][SUBJ] xor e0.bundle[ABOVE][SUBJ]) <> 0));
              end;
          end;  { case }

          _class := tr + (tl shl 1) + (br shl 2) + (bl shl 3);

          case Tvertex_type(_class) of
            EMN: begin
                add_local_min(@out_poly, e0, ix, iy);
                e1.outp[ABOVE] := e0.outp[ABOVE];
              end;
            ERI: begin
                if P <> nil then
                begin
                  add_right(P, ix, iy);
                  e1.outp[ABOVE] := P;
                  e0.outp[ABOVE] := nil;
                end;
              end;
            ELI: begin
                if Q <> nil then
                begin
                  add_left(Q, ix, iy);
                  e0.outp[ABOVE] := Q;
                  e1.outp[ABOVE] := nil;
                end;
              end;
            EMX: begin
                if (P <> nil) and (Q <> nil) then
                begin
                  add_left(P, ix, iy);
                  merge_right(P, Q, out_poly);
                  e0.outp[ABOVE] := nil;
                  e1.outp[ABOVE] := nil;
                end;
              end;
            IMN: begin
                add_local_min(@out_poly, e0, ix, iy);
                e1.outp[ABOVE] := e0.outp[ABOVE];
                end;
            ILI: begin
                if P <> nil then
                begin
                  add_left(P, ix, iy);
                  e1.outp[ABOVE] := P;
                  e0.outp[ABOVE] := nil;
                end;
              end;
            IRI: begin
                if Q <> nil then
                begin
                  add_right(Q, ix, iy);
                  e0.outp[ABOVE] := Q;
                  e1.outp[ABOVE] := nil;
                end;
              end;
            IMX: begin
                if (P <> nil) and (Q <> nil) then
                begin
                  add_right(P, ix, iy);
                  merge_left(P, Q, out_poly);
                  e0.outp[ABOVE] := nil;
                  e1.outp[ABOVE] := nil;
                end;
              end;
            IMM: begin
                if (P <> nil) and (Q <> nil) then
                begin
                  add_right(P, ix, iy);
                  merge_left(P, Q, out_poly);
                  add_local_min(@out_poly, e0, ix, iy);
                  e1.outp[ABOVE] := e0.outp[ABOVE];
                end;
              end;
            EMM: begin
                if (P <> nil) and (Q <> nil) then
                begin
                  add_left(P, ix, iy);
                  merge_right(P, Q, out_poly);
                  add_local_min(@out_poly, e0, ix, iy);
                  e1.outp[ABOVE] := e0.outp[ABOVE];
                end;
              end;
            else
          end; { End of case }
	end; { End of contributing intersection conditional }

        { Swap bundle sides in response to edge crossing }
        if (e0.bundle[ABOVE][CLIP] <> 0) then
          e1.bside[CLIP] := integer(e1.bside[CLIP] = 0);
        if (e1.bundle[ABOVE][CLIP] <> 0) then
          e0.bside[CLIP] := integer(e0.bside[CLIP] = 0);
        if (e0.bundle[ABOVE][SUBJ] <> 0) then
          e1.bside[SUBJ] := integer(e1.bside[SUBJ] = 0);
        if (e1.bundle[ABOVE][SUBJ] <> 0) then
          e0.bside[SUBJ] := integer(e0.bside[SUBJ] = 0);

        { Swap e0 and e1 bundles in the AET }
        prev_edge := e0.prev;
        next_edge := e1.next;
        if next_edge <> nil then
          next_edge.prev := e0;

        if e0.bstate[ABOVE] = BUNDLE_HEAD then
        begin
          search := FTRUE;
          while search <> 0 do
          begin
            prev_edge := prev_edge.prev;
            if prev_edge <> nil then
              begin
                if prev_edge.bstate[ABOVE] <> BUNDLE_TAIL then
                  search := FFALSE;
              end
            else
              search := FFALSE;
          end;
        end;
        if prev_edge = nil then
          begin
            aet.prev := e1;
            e1.next := aet;
            aet := e0.next;
          end
        else
          begin
            prev_edge.next.prev := e1;
            e1.next := prev_edge.next;
            prev_edge.next := e0.next;
          end;
        e0.next.prev := prev_edge;
        e1.next.prev := e1;
        e0.next := next_edge;

        intersect := intersect.next;
      end; { End of IT loop}

      { Prepare for next scanbeam }
      edge := aet;
      while edge <> nil do
      begin
        next_edge := edge.next;
        succ_edge := edge.succ;

        if EQ(edge.top.y, yt) and (succ_edge <> nil) then
          begin
            { Replace AET edge by its successor }
            succ_edge.outp[BELOW] := edge.outp[ABOVE];
            succ_edge.bstate[BELOW] := edge.bstate[ABOVE];
            succ_edge.bundle[BELOW][CLIP] := edge.bundle[ABOVE][CLIP];
            succ_edge.bundle[BELOW][SUBJ] := edge.bundle[ABOVE][SUBJ];
            prev_edge := edge.prev;
            if prev_edge <> nil then
              prev_edge.next := succ_edge
            else
              aet := succ_edge;
            if next_edge <> nil then
              next_edge.prev := succ_edge;
            succ_edge.prev := prev_edge;
            succ_edge.next := next_edge;
          end
        else
          begin
            { Update this edge }
            edge.outp[BELOW] := edge.outp[ABOVE];
            edge.bstate[BELOW] := edge.bstate[ABOVE];
            edge.bundle[BELOW][CLIP] := edge.bundle[ABOVE][CLIP];
            edge.bundle[BELOW][SUBJ] := edge.bundle[ABOVE][SUBJ];
            edge.xb := edge.xt;
          end;
        edge.outp[ABOVE] := nil;
        edge := next_edge;
      end;
    end;
  end; { === END OF SCANBEAM PROCESSING ================================== }

  { Generate result polygon from out_poly }
  result_polygon.contour := nil;
  result_polygon.num_contours := count_contours(out_poly);
  if result_polygon.num_contours > 0 then
  begin
    MALLOC(pointer(result_polygon.hole), result_polygon.num_contours
           * sizeof(Integer), 'hole flag table creation');
    MALLOC(pointer(result_polygon.contour), result_polygon.num_contours
           * sizeof(Tgpc_vertex_list), 'contour creation');
    poly := out_poly;
    c := 0;

    while poly <> nil do
    begin
      npoly := poly.next;
      if poly.active <> 0 then
      begin
        result_polygon.hole[c] := poly.proxy.hole;
        result_polygon.contour[c].num_vertices := poly.active;
        MALLOC(pointer(result_polygon.contour[c].vertex),
          result_polygon.contour[c].num_vertices * sizeof(Tgpc_vertex),
          'vertex creation');

        v := result_polygon.contour[c].num_vertices - 1;
        vtx := poly.proxy.v[LEFT];
        while vtx <> nil do
        begin
          nv := vtx.next;
          result_polygon.contour[c].vertex[v].x := vtx.x;
          result_polygon.contour[c].vertex[v].y := vtx.y;
          FREE(pointer(vtx));
          Dec(v);
          vtx := nv;
        end;
        Inc(c);
      end;
      FREE(pointer(poly));
      poly := npoly;
    end;
  end;

  { Tidy up }
  reset_it(it);
  reset_lmt(lmt);
  FREE(pointer(c_heap));
  FREE(pointer(s_heap));
  FREE(pointer(sbt));
end;


procedure gpc_free_tristrip(tristrip : Pgpc_tristrip);
var
  s : integer;
begin
  for s := 0 to tristrip.num_strips - 1 do
    CFREE(pointer(tristrip.strip[s].vertex));
  CFREE(pointer(tristrip.strip));
  tristrip.num_strips := 0;
end;







//===========================================================================
//                           End of file: gpc.pas
//===========================================================================


end.

