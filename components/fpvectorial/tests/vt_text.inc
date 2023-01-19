{ Include file for "Text" tests. Used by vtmain.pas. }

// single line
node1 := Tree.Items.AddChild(mainnode, 'single line');

node := Tree.Items.AddChild(node1, 'horizontal');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_Text, 'text_left.png', $0000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_Text, 'text_center.png', $0001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_Text, 'text_right.png', $0002));

node := Tree.Items.AddChild(node1, 'rotated by 30 deg');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_Text, 'text_left_30.png', $1000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_Text, 'text_center_30.png', $1001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_Text, 'text_right_30.png', $1002));

node := Tree.Items.AddChild(node1, 'vertical (90 deg, upward)');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_Text, 'text_left_90.png', $2000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_Text, 'text_center_90.png', $2001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_Text, 'text_right_90.png', $2002));

node := Tree.Items.AddChild(node1, 'vertical (-90 deg, downward)');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_Text, 'text_left_m90.png', $3000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_Text, 'text_center_m90.png', $3001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_Text, 'text_right_m90.png', $3002));

// two lines
node1 := Tree.Items.AddChild(mainnode, 'two lines');

node := Tree.Items.AddChild(node1, 'horizontal');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_left.png', $0000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_2Lines, 'text2_center.png', $0001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_right.png', $0002));

node := Tree.Items.AddChild(node1, 'rotated by 30 deg');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_left_30.png', $1000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_2Lines, 'text2_center_30.png', $1001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_right_30.png', $1002));

node := Tree.Items.AddChild(node1, 'vertical (90 deg, upward)');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_left_90.png', $2000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_2Lines, 'text2_center_90.png', $2001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_right_90.png', $2002));

node := Tree.Items.AddChild(node1, 'vertical (-90 deg, downward)');
Tree.Items.AddChildObject(node, 'left aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_left_m90.png', $3000));
Tree.Items.AddChildObject(node, 'centered',
  TRenderParams.Create(@Render_2Lines, 'text2_center_m90.png', $3001));
Tree.Items.AddChildObject(node, 'right aligned',
  TRenderParams.Create(@Render_2Lines, 'text2_right_m90.png', $3002));

// Fonts
node := Tree.Items.AddChild(mainnode, 'Fonts');

Tree.Items.AddChildObject(node, 'Times New Roman + Courier New',
  TRenderParams.Create(@Render_Text_Fonts, 'text_fonts.png'));

// Text colors
Tree.Items.AddChildObject(mainnode, 'Text colors',
  TRenderParams.Create(@Render_Text_Colors, 'text_colors.png'));


