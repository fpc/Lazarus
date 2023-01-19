{ Include file for "Arcs" > "Circular" tests. Used by vtmain.pas. }

node1 := Tree.Items.AddChild(mainnode, 'circular');

node2 := Tree.Items.AddChild(node1, 'clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1_rot30ccw.png', $00010200));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1.png', $0200));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1_rot30cw.png', $00020200));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12_rot30ccw.png', $00010201));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12.png', $0201));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12_rot30cw.png', $00020201));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2_rot30ccw.png', $00010202));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2.png', $0202));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2_rot30cw.png', $00020202));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23_rot30ccw.png', $00010203));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23.png', $0203));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23_rot30cw.png', $00020203));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3_rot30ccw.png', $00010204));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3.png', $0204));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3_rot30cw.png', $00020204));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34_rot30ccw.png', $00010205));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34.png', $0205));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34_rot30cw.png', $00020205));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4_rot30ccw.png', $00010206));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4.png', $0206));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4_rot30cw.png', $00020206));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41_rot30ccw.png', $00010207));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41.png', $0207));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41_rot30cw.png', $00020207));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1_rot30ccw.png', $00010300));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1.png', $0300));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q1_rot30cw.png', $00020300));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12_rot30ccw.png', $00010301));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12.png', $0301));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q12_rot30cw.png', $00020301));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2_rot30ccw.png', $00010302));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2.png', $0302));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q2_rot30cw.png', $00020302));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23_rot30ccw.png', $00010303));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23.png', $0303));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q23_rot30cw.png', $00020303));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3_rot30ccw.png', $00010304));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3.png', $0304));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q3_rot30cw.png', $00020304));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34_rot30ccw.png', $00010305));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34.png', $0305));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q34_rot30cw.png', $00020305));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4_rot30ccw.png', $00010306));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4.png', $0306));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q4_rot30cw.png', $00020306));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41_rot30ccw.png', $00010307));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41.png', $0307));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_cw_q41_rot30cw.png', $00020307));

// -----------------------------------------------------------------------------

node2 := Tree.Items.AddChild(node1, 'counter-clockwise from point 1 to point 2');

  node := Tree.Items.AddChild(node2, 'Quadrant I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1_rot30ccw.png', $00010000));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1.png', $0000));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1_rot30cw.png', $00020000));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12_rot30ccw.png', $00010001));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12.png', $0001));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12_rot30cw.png', $00020001));

  node := Tree.Items.AddChild(node2, 'Quadrant II');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2_rot30ccw.png', $00010002));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2.png', $0002));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2_rot30cw.png', $00020002));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23_rot30ccw.png', $00010003));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23.png', $0003));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23_rot30cw.png', $00020003));

  node := Tree.Items.AddChild(node2, 'Quadrant III');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3_rot30ccw.png', $00010004));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3.png', $0004));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3_rot30cw.png', $00020004));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34_rot30ccw.png', $00010005));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34.png', $0005));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34_rot30cw.png', $00020005));

  node := Tree.Items.AddChild(node2, 'Quadrant IV');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4_rot30ccw.png', $00010006));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4.png', $0006));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4_rot30cw.png', $0002006));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41_rot30ccw.png', $0001007));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41.png', $0007));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41_rot30cw.png', $00020007));

  node := Tree.Items.AddChild(node2, 'Quadrant I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1_rot30ccw.png', $00010100));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1.png', $0100));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q1_rot30cw.png', $00020100));

  node := Tree.Items.AddChild(node2, 'Quadrant I+II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12_rot30ccw.png', $00010101));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12.png', $0101));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q12_rot30cw.png', $00020101));

  node := Tree.Items.AddChild(node2, 'Quadrant II, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2_rot30ccw.png', $00010102));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2.png', $0102));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q2_rot30cw.png', $00020102));

  node := Tree.Items.AddChild(node2, 'Quadrant II+III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23_rot30ccw.png', $00010103));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23.png', $0103));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q23_rot30cw.png', $00020103));

  node := Tree.Items.AddChild(node2, 'Quadrant III, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3_rot30ccw.png', $00010304));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3.png', $0104));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q3_rot30cw.png', $00020104));

  node := Tree.Items.AddChild(node2, 'Quadrant III+IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34_rot30ccw.png', $00010105));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34.png', $0105));
  Tree.Items.AddChildObject(node, 'rotated around(10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q34_rot30cw.png', $00020105));

  node := Tree.Items.AddChild(node2, 'Quadrant IV, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4_rot30ccw.png', $00010106));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4.png', $0106));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q4_rot30cw.png', $00020106));

  node := Tree.Items.AddChild(node2, 'Quadrant IV+I, reverse');
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CCW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41_rot30ccw.png', $00010107));
  Tree.Items.AddChildObject(node, 'normal',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41.png', $0107));
  Tree.Items.AddChildObject(node, 'rotated around (10,10) by 30deg CW',
    TRenderParams.Create(@Render_Arc, 'arc_ccw_q41_rot30cw.png', $00020107));

