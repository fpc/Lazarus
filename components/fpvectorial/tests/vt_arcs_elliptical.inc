{ Include file for "Arcs" > "Elliptical" tests. Used by vtmain.pas. }

node1 := Tree.Items.AddChild(mainnode, 'elliptical');

node2 := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1_rot30ccw.png', $00011200));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1.png', $1200));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1_rot30cw.png', $00021200));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12_rot30ccw.png', $00011201));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12.png', $1201));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12_rot30cw.png', $00021201));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2_rot30ccw.png', $00011202));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2.png', $1202));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2_rot30cw.png', $00021202));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23_rot30ccw.png', $00011203));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23.png', $1203));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23_rot30cw.png', $00021203));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3_rot30ccw.png', $00011204));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3.png', $1204));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3_rot30cw.png', $00021204));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34_rot30ccw.png', $00011205));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34.png', $1205));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34_rot30cw.png', $00021205));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4_rot30ccw.png', $00011206));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4.png', $1206));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4_rot30cw.png', $00021206));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41_rot30ccw.png', $00011207));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41.png', $1207));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41_rot30cw.png', $00021207));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1_rot30ccw.png', $00011300));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1.png', $1300));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q1_rot30cw.png', $00021300));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12_rot30ccw.png', $00011301));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12.png', $1301));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q12_rot30cw.png', $00021301));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2_rot30ccw.png', $00011302));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2.png', $1302));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q2_rot30cw.png', $00021302));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23_rot30ccw.png', $00011303));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23.png', $1303));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q23_rot30cw.png', $00021303));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3_rot30ccw.png', $00011304));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3.png', $1304));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q3_rot30cw.png', $00021304));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34_rot30ccw.png', $00011305));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34.png', $1305));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q34_rot30cw.png', $00021305));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4_rot30ccw.png', $00011306));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4.png', $1306));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q4_rot30cw.png', $00021306));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41_rot30ccw.png', $00011307));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41.png', $1307));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_cw_q41_rot30cw.png', $00021307));

// -----------------------------------------------------------------------------

node2 := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1_rot30ccw.png', $00011000));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1.png', $1000));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1_rot30cw.png', $00021000));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12_rot30ccw.png', $00011001));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12.png', $1001));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12_rot30cw.png', $00021001));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2_rot30ccw.png', $00011002));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2.png', $1002));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2_rot30cw.png', $00021002));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23_rot30ccw.png', $00011003));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23.png', $1003));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23_rot30cw.png', $00021003));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3_rot30ccw.png', $00011004));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3.png', $1004));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3_rot30cw.png', $00021004));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34_rot30ccw.png', $00011005));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34.png', $1005));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34_rot30cw.png', $00021005));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4_rot30ccw.png', $00011006));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4.png', $1006));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4_rot30cw.png', $00021006));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41_rot30ccw.png', $00011007));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41.png', $1007));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41_rot30cw.png', $00021007));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1_rot30ccw.png', $00011100));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1.png', $1100));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q1_rot30cw.png', $00021100));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12_rot30ccw.png', $00011101));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12.png', $1101));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q12_rot30cw.png', $00021101));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2_rot30ccw.png', $00011102));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2.png', $1102));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q2_rot30cw.png', $00021102));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23_rot30ccw.png', $00011103));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23.png', $1103));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q23_rot30cw.png', $00021103));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3_rot30ccw.png', $00011104));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3.png', $1104));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q3_rot30cw.png', $00021104));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34_rot30ccw.png', $00011105));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34.png', $1105));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q34_rot30cw.png', $00021105));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4_rot30ccw.png', $00011106));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4.png', $1106));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q4_rot30cw.png', $00021106));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41_rot30ccw.png', $00011107));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41.png', $1107));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30° CW',
    TRenderParams.Create(@Render_Arc, 'arc_ell_ccw_q41_rot30cw.png', $00021107));

