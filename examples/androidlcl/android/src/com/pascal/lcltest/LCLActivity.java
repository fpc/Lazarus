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
  // Our drawing surface
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
 
      Bitmap lclbitmap = Bitmap.createBitmap(lWidth, lHeight, Bitmap.Config.ARGB_8888);
      LCLDrawToBitmap(lWidth, lHeight, lclbitmap);
      canvas.drawBitmap(lclbitmap, 0, 0, null);
    }


    @Override public boolean onTouchEvent (MotionEvent event)
    {
      int eventResult = LCLOnTouch(event.getX(), event.getY(), event.getAction());
      if ((eventResult | 1) != 0) postInvalidate();
      return true;
    }
  }    

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
  
  // JNI table of Pascal functions
  public native int LCLDrawToBitmap(int width, int height, Bitmap bitmap);
  public native int LCLOnTouch(float x, float y, int action);
  public native int LCLOnCreate(LCLActivity lclactivity);

  // Functions exported to the Pascal side

  // input: String lcltext
  // output: int lclwidth, int lclheight
  public void LCLDoGetTextBounds()
  {
    Paint paint = new Paint();
    Rect bounds = new Rect();
    paint.getTextBounds(lcltext, 0, lcltext.length(), bounds);
    lclwidth = bounds.width();
    lclheight = bounds.height();
  }

  // Fields exported to the Pascal side for easier data communication
  public String lcltext;
  public int lclwidth;
  public int lclheight;

  static
  {
    try 
    {
      Log.i("lclproject", "Trying to load liblclapp.so");
      System.loadLibrary("lclapp");
    } 
    catch(UnsatisfiedLinkError ule) 
    {
      Log.e("lclproject", "WARNING: Could not load liblclapp.so");
      ule.printStackTrace();
    }
  }
}
