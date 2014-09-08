package com.geishatokyo.scalappybird

import android.content.Context
import android.content.res.Resources
import android.view.SurfaceHolder
import android.graphics.{Canvas, Paint, Rect, Path, Bitmap, BitmapFactory}

object Util {
  val deviceWidth = 800
  val startTime = System.currentTimeMillis()
  def getTime() = {
    (System.currentTimeMillis() - startTime)/1000.0
  }
}

case class Point(var x: Int, var y: Int)

trait GameObject {
  def initialize(c: Context)
  def update() 
  def draw(g: Canvas)
  def touchEvent(x: Int, y: Int) = {}
}

object GameObject {
  var canvasWidth: Int = _
  var canvasHeight: Int = _  
}

class Bird(p: Point) extends GameObject {

  var img : Bitmap = null;
  var point: Point = p
  var speed: Int = 10
  var startTime: Double = 0
  val upOffset: Int = -200
  var enable: Boolean = true

  def initialize(c: Context) = {
    val res:Resources = c.getResources
    img = BitmapFactory.decodeResource(res, R.drawable.bird)
  }

  def update() {
    
    // falling
    if (enable){
      // point.y = point.y + speed
      if(startTime != 0)
      point.y = point.y - (25 * (1 - (System.currentTimeMillis() - startTime)/1000)).toInt
    }

    // dead or alive
    if(point.y >= GameObject.canvasHeight) {
      enable = false
    }
  }

  def draw(g: Canvas) {
    val p = new Paint
    if (enable){
     g drawBitmap(img, point.x, point.y, p)
    } else {
      val gamePaint = new Paint
      gamePaint.setARGB(255, 255, 0, 0)
      gamePaint.setTextSize(64)
      g drawText ("game over", 250, GameObject.canvasHeight/2-32, gamePaint)     
    }
  }

  override def touchEvent(x: Int, y: Int) = {
    if (enable){
      startTime = System.currentTimeMillis()
      // point.y = point.y + upOffset
    }    
  }
}

trait StaticGameObject extends GameObject {
  
  def point: Point
  def resourceId: Int
  var img: Bitmap = null
  lazy val width: Int = img.getWidth
  lazy val height: Int = img.getHeight
  
  def initialize(c: Context) = {
    val res:Resources = c.getResources
    img = BitmapFactory.decodeResource(res, resourceId)
  }
  def update() {}

  def draw(g: Canvas) {
    val p = new Paint
    g drawBitmap(img, point.x, point.y, p)
  }
}

class ScoreCount(p: Point) extends StaticGameObject {
  val point: Point = p
  val resourceId: Int = R.drawable.pipeup
  val startTime = System.currentTimeMillis
  override def draw(g: Canvas) {
    val t = new Paint
    t.setARGB(255, 0, 0, 0)
    t.setTextSize(64)
    g drawText (Util.getTime.toInt.toString, p.x, p.y, t)         
  }
  
}

class PipeUp(p: Point) extends StaticGameObject {
  val startTime = System.currentTimeMillis
  val startPointx = p.x
  val point: Point = p
  val resourceId: Int = R.drawable.pipeup
  override def update() {
    point.y = GameObject.canvasHeight - height
    point.x = point.x -  10
    if((startPointx - point.x).abs > GameObject.canvasWidth) point.x += GameObject.canvasWidth 
  }
}

class PipeDown(p: Point) extends StaticGameObject {
  val startTime = System.currentTimeMillis
  val startPointx = p.x
  val point: Point = p
  val resourceId: Int = R.drawable.pipedown  
  override def update() {
    point.x = point.x - 10
    if((startPointx - point.x).abs > GameObject.canvasWidth) point.x += GameObject.canvasWidth 
  }
}

trait PaddingWidthStaticGameObject extends StaticGameObject {
  override def draw(g: Canvas) {
    val p = new Paint
    (0 to GameObject.canvasWidth / width) foreach {i => 
      g drawBitmap(img, point.x + i * width, point.y, p)
    }
  }
}

class Land(p: Point) extends PaddingWidthStaticGameObject {
  val startTime = System.currentTimeMillis
  val point: Point = p
  val startPointx = p.x
  val resourceId: Int = R.drawable.land
  override def update() {
    point.y = GameObject.canvasHeight - height
    point.x = point.x - 10
    if((startPointx - point.x).abs > GameObject.canvasWidth) point.x += GameObject.canvasWidth 
  }
}

class Sky(p: Point) extends PaddingWidthStaticGameObject {
  val startTime = System.currentTimeMillis
  val startPointx = p.x
  val point: Point = p
  val resourceId: Int = R.drawable.sky
  override def update() {
    point.y = GameObject.canvasHeight - height - 112 /* Land height */
    point.x = point.x - 10
    if((startPointx - point.x).abs > GameObject.canvasWidth) point.x += GameObject.canvasWidth 
  }  
}

class MainThread(holder: SurfaceHolder, context: Context) extends Thread {
  val deviceWidth = Util.deviceWidth
  // initialize
  var gameObjects : List[GameObject] = {
    List(new Sky(Point(0,900-109)),
         new Land(Point(0,900)),
         new PipeUp(Point(500,760)),
         new PipeDown(Point(300,0)),
         new Sky(Point(deviceWidth,900-109)),
         new Land(Point(deviceWidth,900)),
         new PipeUp(Point(500+deviceWidth,760)),
         new PipeDown(Point(300+deviceWidth,0)),
         new ScoreCount(Point(600,80)),
         new Bird(Point(100,0)))
  }
  gameObjects.foreach(_.initialize(context))

  val bluishWhite = new Paint
  bluishWhite.setARGB(255, 255, 255, 255)
  val bluishBlack = new Paint
  bluishBlack.setARGB(255, 0, 0, 0)

  override def run {
    val quantum = 100
    var isRunning: Boolean = true
    while (isRunning) {
      val t0 = System.currentTimeMillis
      game()
      val t1 = System.currentTimeMillis
      if (t1 - t0 < quantum) Thread.sleep(quantum - (t1 - t0))
    }
  }

  def game() {
      update()
      draw()
  }

  def draw() {
    withCanvas { g =>
      g drawRect (0, 0, GameObject.canvasWidth, GameObject.canvasHeight, bluishWhite)
      gameObjects.foreach(_.draw(g))
    }
  }

  def setCanvasSize(w: Int, h: Int) {
    GameObject.canvasWidth = w
    GameObject.canvasHeight = h
  }

  def addTouch(x: Int, y: Int) {
    gameObjects.foreach(_.touchEvent(x,y))
  } 
  def update() {
    gameObjects.foreach(_.update())
  } 

  def withCanvas(f: Canvas => Unit) {
    val canvas = holder.lockCanvas(null)
    try {
      f(canvas)
    } finally {
      holder.unlockCanvasAndPost(canvas)
    }
  }
}
