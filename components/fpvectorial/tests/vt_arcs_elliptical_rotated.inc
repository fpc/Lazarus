{ Include file for "Arcs" > "Elliptical rotated" tests. Used by vtmain.pas. }

node1 := Tree.Items.AddChild(mainnode, 'elliptical, rotated by 30deg around center');

node2 := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1_rot30ccw.png', $00012200));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1.png', $2200));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1_rot30cw.png', $00022200));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12_rot30ccw.png', $00012201));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12.png', $2201));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12_rot30cw.png', $00022201));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2_rot30ccw.png', $00012202));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2.png', $2202));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2_rot30cw.png', $00022202));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23_rot30ccw.png', $00012203));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23.png', $2203));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23_rot30cw.png', $00022203));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3_rot30ccw.png', $00012204));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3.png', $2204));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3_rot30cw.png', $00022204));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34_rot30ccw.png', $00012205));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34.png', $2205));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34_rot30cw.png', $00022205));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4_rot30ccw.png', $00012206));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4.png', $2206));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4_rot30cw.png', $00022206));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41_rot30ccw.png', $00012207));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41.png', $2207));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41_rot30cw.png', $00022207));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1_rot30ccw.png', $00012300));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1.png', $2300));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q1_rot30cw.png', $00022300));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12_rot30ccw.png', $00012301));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12.png', $2301));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q12_rot30cw.png', $00022301));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2_rot30ccw.png', $00012302));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2.png', $2302));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q2_rot30cw.png', $00022302));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23_rot30ccw.png', $00012303));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23.png', $2303));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q23_rot30cw.png', $00022303));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3_rot30ccw.png', $00012304));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3.png', $2304));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q3_rot30cw.png', $00022304));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34_rot30ccw.png', $00012305));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34.png', $2305));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q34_rot30cw.png', $00022305));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4_rot30ccw.png', $00012306));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4.png', $2306));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q4_rot30cw.png', $00022306));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41_rot30ccw.png', $00021307));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41.png', $2307));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_cw_q41_rot30cw.png', $00022307));

// -----------------------------------------------------------------------------

node2 := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1_rot30ccw.png', $00012000));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1.png', $2000));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1_rot30cw.png', $00022000));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12_rot30ccw.png', $00012001));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12.png', $2001));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12_rot30cw.png', $00022001));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2_rot30ccw.png', $00012002));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2.png', $2002));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2_rot30cw.png', $00022002));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23_rot30ccw.png', $00012003));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23.png', $2003));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23_rot30cw.png', $00022003));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3_rot30ccw.png', $00012004));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3.png', $2004));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3_rot30cw.png', $00022004));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34_rot30ccw.png', $00012005));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34.png', $2005));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34_rot30cw.png', $00022005));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4_rot30ccw.png', $00012006));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4.png', $2006));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4_rot30cw.png', $00022006));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41_rot30ccw.png', $00012007));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41.png', $2007));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41_rot30cw.png', $00022007));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1_rot30ccw.png', $00012100));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1.png', $2100));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q1_rot30cw.png', $00022100));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12_rot30ccw.png', $00012101));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12.png', $2101));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q12_rot30cw.png', $00022101));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2_rot30ccw.png', $00012102));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2.png', $2102));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q2_rot30cw.png', $00022102));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23_rot30ccw.png', $00012103));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23.png', $2103));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q23_rot30cw.png', $00022103));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3_rot30ccw.png', $00012104));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3.png', $2104));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q3_rot30cw.png', $00022104));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34_rot30ccw.png', $00012105));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34.png', $2105));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q34_rot30cw.png', $00022105));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4_rot30ccw.png', $00012106));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4.png', $2106));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q4_rot30cw.png', $000212106));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41_rot30ccw.png', $00012107));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41.png', $2107));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ellrot_ccw_q41_rot30cw.png', $00022107));


