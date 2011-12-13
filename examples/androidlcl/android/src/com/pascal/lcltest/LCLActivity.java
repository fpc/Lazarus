package com.pascal.lcltest;

import android.app.*;
import android.content.*;
import android.os.*;
import android.widget.*;
import android.util.*;
import android.graphics.*;
import android.view.*;

public class LCLActivity extends Activity 
{
  // -------------------------------------------
  // Our drawing surface
  // -------------------------------------------
  private class LCLSurface extends SurfaceView
  {
    public LCLSurface(Context context)
    {
      super(context);
      // Allows View.postInvalidate() to work
      setWillNotDraw(false);
      // We already double buffer, so no need for a second one
      setWillNotCacheDrawing(true);
    }

    @Override protected void onDraw(Canvas canvas)
    {
      int lWidth = getWidth();
      int lHeight = getHeight();

      //Log.v("lclproject", "LCLSurface.onDraw width=" + Integer.toString(lWidth)
      //  + " height=" + Integer.toString(lHeight));
 
      Bitmap localbitmap = Bitmap.createBitmap(lWidth, lHeight, Bitmap.Config.ARGB_8888);
      LCLDrawToBitmap(lWidth, lHeight, localbitmap);
      canvas.drawBitmap(localbitmap, 0, 0, null);
    }


    @Override public boolean onTouchEvent (MotionEvent event)
    {
      int eventResult = LCLOnTouch(event.getX(), event.getY(), event.getAction());
      if ((eventResult | 1) != 0) postInvalidate();
      return true;
    }
  }    

  // -------------------------------------------
  // Activity Events
  // -------------------------------------------

  /** Called when the activity is first created. */
  @Override
  public void onCreate(Bundle savedInstanceState) 
  {
    super.onCreate(savedInstanceState);
          
    LCLSurface lclsurface = new LCLSurface(this);
    setContentView(lclsurface);
    lclsurface.postInvalidate();
    // Tell the LCL that an OnCreate has happened and what is our instance
    LCLOnCreate(this);
  }
  
  // -------------------------------------------
  // JNI table of Pascal functions
  // -------------------------------------------
  public native int LCLDrawToBitmap(int width, int height, Bitmap bitmap);
  public native int LCLOnTouch(float x, float y, int action);
  public native int LCLOnCreate(LCLActivity lclactivity);
  public native int LCLOnMessageBoxFinished(int Result);

  // -------------------------------------------
  // Functions exported to the Pascal side
  // -------------------------------------------

  // input: String lcltext, int lcltextsize
  // output: int lclwidth, int lclheight, int lclascent, etc
  public void LCLDoGetTextBounds()
  {
    Paint localpaint = new Paint();
    Rect localbounds = new Rect();
    localpaint.setTextSize(lcltextsize);
    localpaint.getTextBounds(lcltext, 0, lcltext.length(), localbounds);
    lclwidth = localbounds.width();
    // Don't use just localbounds.height() from the source text
    // because it will calculate the minimum necessary height,
    // but we can't easily use that to draw text because it draws relative to the baseline
    localpaint.getTextBounds("Íqg", 0, 3, localbounds);
    lclheight = localbounds.height();
    // Also get some measures
    lcltextascent = (int) localpaint.getFontMetrics().ascent;
    lcltextbottom = (int) localpaint.getFontMetrics().bottom;
    lcltextdescent = (int) localpaint.getFontMetrics().descent;
    lcltextleading = (int) localpaint.getFontMetrics().leading;
    lcltexttop = (int) localpaint.getFontMetrics().top;
  }

  // input: String lcltext, int lclwidth, int lclheight
  // output: lclbitmap
  public void LCLDoDrawText()
  {
    lclbitmap = Bitmap.createBitmap(lclwidth, lclheight, Bitmap.Config.ARGB_8888);
    Canvas localcanvas = new Canvas(lclbitmap);
    Paint localpaint = new Paint();
    localpaint.setColor(Color.BLACK);
    localpaint.setTextSize(lcltextsize);
    localpaint.setFlags(Paint.ANTI_ALIAS_FLAG);
    localcanvas.drawColor(Color.TRANSPARENT); // TRANSPARENT
    // The Y coordinate is the lower baseline of letters like "abc"
    // see http://code.google.com/p/android/issues/detail?id=393
    localcanvas.drawText(lcltext, 0, lclheight - lcltextbottom, localpaint);
  }

  // LCLType definitions

  private final int idButtonBase = 0x00000000;
  private final int idButtonOk = 0x00000001;
  private final int idButtonCancel = 0x00000002;
  private final int idButtonHelp = 0x00000003;
  private final int idButtonYes = 0x00000004;
  private final int idButtonNo = 0x00000005;
  private final int idButtonClose = 0x00000006;
  private final int idButtonAbort = 0x00000007;
  private final int idButtonRetry = 0x00000008;
  private final int idButtonIgnore = 0x00000009;
  private final int idButtonAll = 0x0000000A;
  private final int idButtonYesToAll = 0x0000000B;
  private final int idButtonNoToAll = 0x0000000C;
  private final int idButtonOpen = 0x0000000D;
  private final int idButtonSave = 0x0000000E;
  private final int idButtonShield = 0x0000000F;

  // input: String lcltext, String lcltitle, int lclconfig (buttons)
  // output: nothing, but calles LCLOnMessageBoxFinished
  public void LCLDoShowMessageBox()
  {
    DialogInterface.OnClickListener dialogClickListener = new DialogInterface.OnClickListener()
    {
      @Override
      public void onClick(DialogInterface dialog, int which)
      {
        switch (which)
        {
        case DialogInterface.BUTTON_POSITIVE:
          LCLOnMessageBoxFinished(lclbutton1);
          break;
        case DialogInterface.BUTTON_NEUTRAL:
          LCLOnMessageBoxFinished(lclbutton2);
          break;
        case DialogInterface.BUTTON_NEGATIVE:
          LCLOnMessageBoxFinished(lclbutton3);
          break;
        };
      }
    };

    DialogInterface.OnCancelListener dialogCancelListener = new DialogInterface.OnCancelListener()
    {
      @Override
      public void onCancel(DialogInterface dialog)
      {
        // The Cancel button number matches for LCLIntf.MessageBox and LCLIntf.PromptDialog
        LCLOnMessageBoxFinished(idButtonCancel);
      }
    };

    AlertDialog.Builder builder = new AlertDialog.Builder(this);
    builder.setMessage(lcltext);
    builder.setTitle(lcltitle);
    if (lclbutton1 >= 0) builder.setPositiveButton(lclbutton1str, dialogClickListener);
    if (lclbutton2 >= 0) builder.setNeutralButton(lclbutton2str, dialogClickListener);
    if (lclbutton3 >= 0) builder.setNegativeButton(lclbutton3str, dialogClickListener);
    builder.show().setOnCancelListener(dialogCancelListener);
  }

  // -------------------------------------------
  // Fields exported to the Pascal side for easier data communication
  // -------------------------------------------
  public String lcltext;
  public String lcltitle;
  public String lclbutton1str;
  public String lclbutton2str;
  public String lclbutton3str;
  public int lclwidth;
  public int lclheight;
  public int lclbutton1;
  public int lclbutton2;
  public int lclbutton3;
  public Bitmap lclbitmap;
  public int lcltextsize;
  public int lcltextascent;
  public int lcltextbottom;
  public int lcltextdescent;
  public int lcltextleading;
  public int lcltexttop;

  static
  {
    try 
    {
      Log.i("lclapp", "Trying to load liblclapp.so");
      System.loadLibrary("lclapp");
    } 
    catch(UnsatisfiedLinkError ule) 
    {
      Log.e("lclapp", "WARNING: Could not load liblclapp.so");
      ule.printStackTrace();
    }
  }
}
