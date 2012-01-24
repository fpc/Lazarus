package com.pascal.lcltest;

import android.app.*;
import android.content.*;
import android.os.*;
import android.widget.*;
import android.util.*;
import android.graphics.*;
import android.text.*;
import android.view.*;
import android.view.inputmethod.*;
import android.content.res.Configuration;
import android.content.Intent;
import android.hardware.Sensor;
import android.hardware.SensorEvent;
import android.hardware.SensorEventListener;
import android.hardware.SensorManager;
import android.telephony.SmsManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import java.util.*;

public class LCLActivity extends Activity implements SensorEventListener, LocationListener
{
  // -------------------------------------------
  // Input connection to get character events
  // -------------------------------------------
  private class LCLInputConnection extends BaseInputConnection
  {
    private SpannableStringBuilder _editable;
    View _lclView;

    public LCLInputConnection(View targetView, boolean fullEditor)
    {
      super(targetView, fullEditor);
      _lclView = (View) targetView;
    }

/*    public Editable getEditable()
    {
      if (_editable == null)
      {
        _editable = (SpannableStringBuilder) Editable.Factory.getInstance()
        .newEditable("Placeholder");
      }
      return _editable;
    } This crashes in HTC */

    // This method sends a text to be added at the current cursor position
    @Override public boolean commitText(CharSequence text, int newCursorPosition)
    {
      //if (_editable != null) _editable.append(text);
      Log.v("lclproject", "LCLInputConnection.commitText =" + text + " newCursorPosition=" + Integer.toString(newCursorPosition));

      // Send each character of the string
      int eventResult, i;
      for (i = 0; i<text.length(); i++)
      {
        eventResult = LCLOnKey(-1, 0, null, (int) text.charAt(i));
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }
      return true;
    }

    @Override public boolean deleteSurroundingText(int leftLength, int rightLength)
    {
      Log.v("lclproject", "LCLInputConnection.deleteSurroundingText left=" + Integer.toString(leftLength) + " right=" + Integer.toString(rightLength));

      // For each left surrounding text deletion, send a backspace key
      int eventResult, i;
      for (i = 0; i<leftLength; i++)
      {
        eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
        eventResult = LCLOnKey(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }

      // For each right surrounding text deletion, send a del key
      // KEYCODE_FORWARD_DEL Since: API Level 11
      /*for (i = 0; i<leftLength; i++)
      {
        eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_FORWARD_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
        eventResult = LCLOnKey(KeyEvent.ACTION_UP, KeyEvent.KEYCODE_FORWARD_DEL, null, (char) 0);
        if ((eventResult & 1) != 0) lclsurface.postInvalidate();
      }*/

      return super.deleteSurroundingText(leftLength, rightLength);
    }
  }

  // -------------------------------------------
  // Our drawing surface
  // -------------------------------------------
  private class LCLSurface extends View
  {
    public LCLSurface(Context context)
    {
      super(context);
      // Allows View.postInvalidate() to work
      setWillNotDraw(false);
      // We already double buffer, so no need for a second one
      setWillNotCacheDrawing(true);
      // Set focus on us to get keyboard events
      requestFocus();
      setFocusableInTouchMode(true);
    }

    @Override protected void onDraw(Canvas canvas)
    {
      //Log.i("lclapp", "onDraw started");
      int lWidth = getWidth();
      int lHeight = getHeight();
      int oldlclformwidth = lclformwidth;

      lclformwidth = lWidth;
      lclformheight = lHeight;
      lclscreenwidth = lclformwidth;
      lclscreenheight = lclformheight;

      // Check if we rotated in the draw event, OnConfigurationChanged can't return the new form width =(
      // see http://stackoverflow.com/questions/2524683/how-to-get-new-width-height-of-root-layout-in-onconfigurationchanged
      if (lWidth != oldlclformwidth) LCLOnConfigurationChanged(lclxdpi, lWidth); // we send xdpi because thats what the LCL uses for Screen.PixelsPerInch

      //Log.v("lclproject", "LCLSurface.onDraw width=" + Integer.toString(lWidth)
      //  + " height=" + Integer.toString(lHeight));
 
      Bitmap localbitmap = Bitmap.createBitmap(lWidth, lHeight, Bitmap.Config.ARGB_8888);
      LCLDrawToBitmap(lWidth, lHeight, localbitmap);
      canvas.drawBitmap(localbitmap, 0, 0, null);
      //Log.i("lclapp", "onDraw finished");
    }

    @Override public boolean onKeyDown (int keyCode, KeyEvent event)
    {
      //Log.v("lclproject", "LCLSurface.onKeyDown");
      super.onKeyDown(keyCode, event);
      int eventResult = LCLOnKey(KeyEvent.ACTION_DOWN, keyCode, event, (char) 0);
      if ((eventResult & 1) != 0) postInvalidate();
      return true;
    }

    @Override public boolean onKeyUp (int keyCode, KeyEvent event)
    {
      // First handle the KeyUp event
      int eventResult = LCLOnKey(KeyEvent.ACTION_UP, keyCode, event, event.getUnicodeChar());
      if ((eventResult & 1) != 0) postInvalidate();

      // Handling of the Back hardware key
      super.onKeyUp(keyCode, event);
      if ((eventResult & 2) != 0)
      {
        //Log.v("lclproject", "BackKey going to home");
        finish();
        return false; // From the docs it seams that only returning false should do it, but calling finish() is really necessary
      }
      else
      {
        //Log.v("lclproject", "BackKey not going to home");
        return true;
      }
    }

    @Override public boolean onTouchEvent (MotionEvent event)
    {
      int eventResult = LCLOnTouch(event.getX(), event.getY(), event.getAction());
      if ((eventResult | 1) != 0) postInvalidate();
      return true;
    }

    @Override public InputConnection onCreateInputConnection(EditorInfo outAttrs)
    {
      outAttrs.actionLabel = null;
      outAttrs.label = "Test text";
      outAttrs.inputType = InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS;
      outAttrs.imeOptions = EditorInfo.IME_ACTION_DONE;
      return new LCLInputConnection(this, true);
    }

    @Override public boolean onCheckIsTextEditor()
    {
      return true;
    }
  }

  // Global objects
  LCLSurface lclsurface;
  SensorManager localSensorManager;

  // Utility routines
  public static double[] convertFloatsToDoubles(float[] input)
  {
    if (input == null) return null;
    double[] output = new double[input.length];
    for (int i = 0; i < input.length; i++)
    {  output[i] = input[i]; }
    return output;
  }

  public void ProcessEventResult(int eventResult)
  {
    if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
    //if ((eventResult | 2) != 0) reserved for BACK key handling
  }

  // -------------------------------------------
  // Activity Events
  // -------------------------------------------

  /** Called when the activity is first created. */
  @Override public void onCreate(Bundle savedInstanceState)
  {
    super.onCreate(savedInstanceState);
          
    lclsurface = new LCLSurface(this);
    setContentView(lclsurface);
    lclsurface.postInvalidate();

    // Tell the LCL that an OnCreate has happened and what is our instance
    lclformwidth = lclsurface.getWidth();
    lclformheight = lclsurface.getHeight();
    lclscreenwidth = lclformwidth;
    lclscreenheight = lclformheight;
    DisplayMetrics metrics = new DisplayMetrics();
    getWindowManager().getDefaultDisplay().getMetrics(metrics);
    lclxdpi = (int) metrics.xdpi;
    lclydpi = (int) metrics.ydpi;
    LCLOnCreate(this);
  }

  @Override public void onConfigurationChanged (Configuration newConfig)
  {
    super.onConfigurationChanged(newConfig);

    lclformwidth = lclsurface.getWidth();
    lclformheight = lclsurface.getHeight();
    lclscreenwidth = lclformwidth;
    lclscreenheight = lclformheight;
    DisplayMetrics metrics = new DisplayMetrics();
    getWindowManager().getDefaultDisplay().getMetrics(metrics);
    lclxdpi = (int) metrics.xdpi;
    lclydpi = (int) metrics.ydpi;
    // Don't call LCLOnConfigurationChanged, wait for a onDraw instead
    //lclsurface.postInvalidate();
    //Log.i("lclapp", "onConfigurationChanged finished");
  }

  // -------------------------------------------
  // JNI table of Pascal functions
  // -------------------------------------------
  public native int LCLDrawToBitmap(int width, int height, Bitmap bitmap);
  public native int LCLOnTouch(float x, float y, int action);
  public native int LCLOnCreate(LCLActivity lclactivity);
  public native int LCLOnMessageBoxFinished(int Result);
  public native int LCLOnKey(int kind, int keyCode, KeyEvent event, int AChar);
  public native int LCLOnTimer(Runnable timerid);
  public native int LCLOnConfigurationChanged(int ANewDPI, int ANewWidth);
  public native int LCLOnSensorChanged(int ASensorKind, double[] AValues);

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
    // Painter.getTextBounds consistently gives us a too small size, so work around that
    lclwidth = lclwidth + (3 * lcltextsize) / 16;
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

  // input: String lcltext, int lclmaxwidth, int lcltextsize
  // output: int lclmaxcount
  public void LCLDoGetTextPartialWidths()
  {
    Paint localpaint = new Paint();
    Rect localbounds = new Rect();
    localpaint.setTextSize(lcltextsize);

    float localmaxwidth = (float) lclmaxwidth;
    //Log.i("lclapp", "[LCLDoGetTextPartialWidths] lcltext="+lcltext+" localmaxwidth="+Float.toString(localmaxwidth));
    lclmaxcount = localpaint.breakText(lcltext, true, localmaxwidth, lclpartialwidths);
  }

  // input: String lcltext, int lclwidth, int lclheight
  // output: lclbitmap
  public void LCLDoDrawText(int ATextColor)
  {
    lclbitmap = Bitmap.createBitmap(lclwidth, lclheight, Bitmap.Config.ARGB_8888);
    Canvas localcanvas = new Canvas(lclbitmap);
    Paint localpaint = new Paint();
    localpaint.setColor(ATextColor);
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
  };

  private Handler LocalHandler = new Handler();

  private class LCLRunnable implements Runnable
  {
    public boolean Destroyed = false;

    public void run()
    {
      int eventResult = LCLOnTimer(this);
      ProcessEventResult(eventResult);
      if (this.Destroyed == false) LocalHandler.postDelayed(this, lcltimerinterval);
    }
  };

  // input:  int lcltimerinterval in milliseconds
  // output:  Runnable lcltimerid
  public void LCLDoCreateTimer()
  {
    lcltimerid = new LCLRunnable();

    LocalHandler.removeCallbacks(lcltimerid);
    LocalHandler.postDelayed(lcltimerid, lcltimerinterval);
  };

  // input: Runnable lcltimerid
  public void LCLDoDestroyTimer()
  {
    LocalHandler.removeCallbacks(lcltimerid);
    ((LCLRunnable) lcltimerid).Destroyed = true;
  };

  public void LCLDoHideVirtualKeyboard()
  {
    InputMethodManager localInputManager = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
    localInputManager.hideSoftInputFromWindow(lclsurface.getWindowToken(), 0);
  };

  public void LCLDoShowVirtualKeyboard()
  {
    InputMethodManager localInputManager = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
    localInputManager.showSoftInput(lclsurface, 0);
  };

  // SensorEventListener overrides

  @Override public void onSensorChanged(SensorEvent event)
  {
    double[] eventValues = convertFloatsToDoubles(event.values);
    int eventKind = event.sensor.getType();
    int eventResult = LCLOnSensorChanged(eventKind, eventValues);
    if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
  }

  @Override public void onAccuracyChanged(Sensor sensor, int accuracy)
  {
  }

  public void LCLDoStartReadingAccelerometer()
  {
    localSensorManager = (SensorManager) getSystemService(SENSOR_SERVICE);
    localSensorManager.registerListener(this,
      localSensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER),
      SensorManager.SENSOR_DELAY_NORMAL);
  };

  public void LCLDoStopReadingAccelerometer()
  {
    localSensorManager.unregisterListener(this);
  };

  // input: String lcldestination, String lcltext (Body)
  public void LCLDoSendMessage()
  {
    if (lclkind == 1)
    {
      PendingIntent sentPI = PendingIntent.getBroadcast(this, 0,
        new Intent("SMS_SENT"), 0);

      PendingIntent deliveredPI = PendingIntent.getBroadcast(this, 0,
        new Intent("SMS_DELIVERED"), 0);

      //---when the SMS has been sent---
      registerReceiver(new BroadcastReceiver()
      {
        @Override public void onReceive(Context arg0, Intent arg1)
        {
          double[] statusArray = new double[1];
          int eventResult = 0;
          switch (getResultCode())
          {
            case Activity.RESULT_OK:
              statusArray[0] = 1.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_GENERIC_FAILURE:
              statusArray[0] = 2.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_NO_SERVICE:
              statusArray[0] = 3.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_NULL_PDU:
              statusArray[0] = 2.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case SmsManager.RESULT_ERROR_RADIO_OFF:
              statusArray[0] = 5.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
          }
          ProcessEventResult(eventResult);
        }
      }, new IntentFilter("SMS_SENT"));

      //---when the SMS has been delivered---
      registerReceiver(new BroadcastReceiver()
      {
        @Override public void onReceive(Context arg0, Intent arg1)
        {
          double[] statusArray = new double[1];
          int eventResult = 0;
          switch (getResultCode())
          {
            case Activity.RESULT_OK:
              statusArray[0] = 10.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
            case Activity.RESULT_CANCELED:
              statusArray[0] = 11.0;
              eventResult = LCLOnSensorChanged(-11, statusArray);
              break;
          }
          ProcessEventResult(eventResult);
        }
      }, new IntentFilter("SMS_DELIVERED"));

      // SMS sending seams to cause an awful lot of exceptions
      // See: http://stackoverflow.com/questions/4580952/why-do-i-get-nullpointerexception-when-sending-an-sms-on-an-htc-desire-or-what
      // See: http://code.google.com/p/android/issues/detail?id=3718
      try
      {
        SmsManager sms = SmsManager.getDefault();
        Log.i("lclapp", "[LCLDoSendMessage] lcldestination="+lcldestination+" lcltext="+lcltext);
        ArrayList<String> parts = sms.divideMessage(lcltext);
        //sms.sendMultipartTextMessage(lcldestination, null, parts, sentPI, deliveredPI);
        sms.sendTextMessage(lcldestination, null, lcltext, sentPI, deliveredPI);
      }
      catch (Exception e)
      {
      }
    }
  };

  // LocationListener overrides

  @Override public void onLocationChanged(Location loc)
  {
    if (loc != null)
    {
      double[] positionArray = new double[6];
      positionArray[0] = loc.getLatitude();
      positionArray[1] = loc.getLongitude();
      positionArray[2] = loc.getAltitude();
      positionArray[3] = (double)loc.getAccuracy();
      positionArray[4] = (double)loc.getSpeed();
      positionArray[5] = (double)loc.getTime();
      int eventResult = LCLOnSensorChanged(-10, positionArray);
      if (((eventResult | 1) != 0) && (lclsurface != null)) lclsurface.postInvalidate();
    }
  }

  @Override public void onProviderDisabled(String provider)
  {
  }

  @Override public void onProviderEnabled(String provider)
  {
  }

  @Override public void onStatusChanged(String provider, int status, Bundle extras)
  {
  }

  // input:  int lclkind
  public void LCLDoRequestPositionInfo()
  {
    LocationManager mlocManager = (LocationManager)getSystemService(Context.LOCATION_SERVICE);
    switch (lclkind)
    {
      case 1: mlocManager.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);
      case 2: mlocManager.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 0, 0, this);
      default: Log.i("lclapp", "[LCLDoRequestPositionInfo] Wrong lclkind parameter");
    }
  }

  // -------------------------------------------
  // Fields exported to the Pascal side for easier data communication
  // -------------------------------------------
  public String lcltext;
  public String lcltitle;
  public String lclbutton1str;
  public String lclbutton2str;
  public String lclbutton3str;
  //
  public int lclwidth;
  public int lclheight;
  public int lclbutton1;
  public int lclbutton2;
  public int lclbutton3;
  public Bitmap lclbitmap;
  //
  public int lcltextsize;
  public int lcltextascent;
  public int lcltextbottom;
  public int lcltextdescent;
  public int lcltextleading;
  public int lcltexttop;
  public int lclmaxwidth;
  public int lclmaxcount;
  public float[] lclpartialwidths;
  //
  public int lcltimerinterval;
  public Runnable lcltimerid;
  //
  public int lclxdpi;
  public int lclydpi;
  public int lclformwidth;
  public int lclformheight;
  public int lclscreenwidth;
  public int lclscreenheight;
  // for LazDeviceAPIs
  public String lcldestination;
  public int lclkind;

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
