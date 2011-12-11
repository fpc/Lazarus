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

  // input: String lcltext
  // output: int lclwidth, int lclheight
  public void LCLDoGetTextBounds()
  {
    Paint localpaint = new Paint();
    Rect localbounds = new Rect();
    localpaint.getTextBounds(lcltext, 0, lcltext.length(), localbounds);
    lclwidth = localbounds.width();
    lclheight = localbounds.height();
  }

  // input: String lcltext, int lclwidth, int lclheight
  // output: lclbitmap
  public void LCLDoDrawText()
  {
    lclbitmap = Bitmap.createBitmap(lclwidth, lclheight, Bitmap.Config.ARGB_8888);
    Canvas localcanvas = new Canvas(lclbitmap);
    Paint localpaint = new Paint();
    localcanvas.drawText(lcltext, 0, 0, localpaint);
  }

  // LCLType definitions

  private final int MB_OK = 0x00000000;
  private final int MB_OKCANCEL = 0x00000001;
  private final int MB_ABORTRETRYIGNORE = 0x00000002;
  private final int MB_YESNOCANCEL = 0x00000003;
  private final int MB_YESNO = 0x00000004;
  private final int MB_RETRYCANCEL = 0x00000005;
  private final int MB_ICONHAND = 0x00000010;
  private final int MB_ICONQUESTION = 0x00000020;
  private final int MB_ICONEXCLAMATION = 0x00000030;
  private final int MB_ICONASTERICK = 0x00000040;
  //MB_ICONWARNING = MB_ICONEXCLAMATION;
  //MB_ICONERROR = MB_ICONHAND;
  //MB_ICONSTOP = MB_ICONHAND;
  //MB_ICONINFORMATION = MB_ICONASTERICK;

  private final int IDOK = 1;     //ID_OK = IDOK;
  private final int IDCANCEL = 2; //ID_CANCEL = IDCANCEL;
  private final int IDABORT = 3;  //ID_ABORT = IDABORT;
  private final int IDRETRY = 4;  //ID_RETRY = IDRETRY;
  private final int IDIGNORE = 5; //ID_IGNORE = IDIGNORE;
  private final int IDYES = 6;    //ID_YES = IDYES;
  private final int IDNO = 7;     //ID_NO = IDNO;
  private final int IDCLOSE = 8;  //ID_CLOSE = IDCLOSE;
  private final int IDHELP = 9;   //ID_HELP = IDHELP;

  private int LCLMessageBoxType; // Stores the type of the last message box

  // input: String lcltext, String lcltitle, int lclconfig (buttons)
  // output: nothing, but calles LCLOnMessageBoxFinished
  public void LCLDoShowMessageBox()
  {
    DialogInterface.OnClickListener dialogClickListener = new DialogInterface.OnClickListener()
    {
      @Override
      public void onClick(DialogInterface dialog, int which)
      {
        switch (LCLMessageBoxType)
        {
        case MB_OK:
          LCLOnMessageBoxFinished(IDOK);
          break;
        case MB_OKCANCEL:
          if (which == DialogInterface.BUTTON_POSITIVE) LCLOnMessageBoxFinished(IDOK);
          else LCLOnMessageBoxFinished(IDCANCEL);
          break;
        case MB_ABORTRETRYIGNORE:
          if (which == DialogInterface.BUTTON_POSITIVE) LCLOnMessageBoxFinished(IDRETRY);
          else if (which == DialogInterface.BUTTON_NEGATIVE) LCLOnMessageBoxFinished(IDABORT);
          else LCLOnMessageBoxFinished(IDIGNORE);
          break;
        case MB_YESNOCANCEL:
          if (which == DialogInterface.BUTTON_POSITIVE) LCLOnMessageBoxFinished(IDYES);
          else if (which == DialogInterface.BUTTON_NEGATIVE) LCLOnMessageBoxFinished(IDNO);
          else LCLOnMessageBoxFinished(IDCANCEL);
          break;
        case MB_YESNO:
          if (which == DialogInterface.BUTTON_POSITIVE) LCLOnMessageBoxFinished(IDYES);
          else LCLOnMessageBoxFinished(IDNO);
          break;
        case MB_RETRYCANCEL:
          if (which == DialogInterface.BUTTON_POSITIVE) LCLOnMessageBoxFinished(IDRETRY);
          else LCLOnMessageBoxFinished(IDCANCEL);
          break;
        };
      }
    };

    DialogInterface.OnCancelListener dialogCancelListener = new DialogInterface.OnCancelListener()
    {
      @Override
      public void onCancel(DialogInterface dialog)
      {
        LCLOnMessageBoxFinished(IDCANCEL);
      }
    };

    AlertDialog.Builder builder = new AlertDialog.Builder(this);
    builder.setMessage(lcltext);
    builder.setTitle(lcltitle);
    LCLMessageBoxType = lclconfig;
    switch (lclconfig)
    {
    case MB_OK:
      builder.setPositiveButton("OK", dialogClickListener);
      break;
    case MB_OKCANCEL:
      builder.setPositiveButton("OK", dialogClickListener);
      builder.setNegativeButton("Cancel", dialogClickListener);
      break;
    case MB_ABORTRETRYIGNORE:
      builder.setNegativeButton("Abort", dialogClickListener);
      builder.setPositiveButton("Retry", dialogClickListener);
      builder.setNeutralButton("Ignore", dialogClickListener);
      break;
    case MB_YESNOCANCEL:
      builder.setPositiveButton("Yes", dialogClickListener);
      builder.setNegativeButton("No", dialogClickListener);
      builder.setNeutralButton("Cancel", dialogClickListener);
      break;
    case MB_YESNO:
      builder.setPositiveButton("Yes", dialogClickListener);
      builder.setNegativeButton("No", dialogClickListener);
      break;
    case MB_RETRYCANCEL:
      builder.setPositiveButton("Retry", dialogClickListener);
      builder.setNegativeButton("Cancel", dialogClickListener);
      break;
    };
    builder.show().setOnCancelListener(dialogCancelListener);
  }

  // -------------------------------------------
  // Fields exported to the Pascal side for easier data communication
  // -------------------------------------------
  public String lcltext;
  public String lcltitle;
  public String lclpositivebutton;
  public String lclnegativebutton;
  public String lclneutralbutton;
  public int lclwidth;
  public int lclheight;
  public int lclconfig;
  public Bitmap lclbitmap;

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
