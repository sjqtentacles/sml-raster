(* sml-raster demo: composes a small scene that exercises every primitive
   (gradient background, filled/outline circles, lines, filled triangles,
   a filled polygon, and a blit) and writes it to assets/shapes.png. *)

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

val width = 480
val height = 320

fun clampi v = if v < 0 then 0 else if v > 255 then 255 else v
fun lerpI (a, b, t) = Real.round (real a + (real b - real a) * t)

(* Vertical sky gradient, built straight into the pixel buffer: doing this with
   functional setPixel would copy the whole image once per pixel. *)
val bgData = Word8Vector.tabulate (4 * width * height, fn i =>
  let
    val px = i div 4
    val ch = i mod 4
    val y = px div width
    val t = real y / real (height - 1)
  in
    case ch of
        0 => Word8.fromInt (clampi (lerpI (38, 12, t)))
      | 1 => Word8.fromInt (clampi (lerpI (54, 18, t)))
      | 2 => Word8.fromInt (clampi (lerpI (92, 40, t)))
      | _ => 0w255
  end)
val canvas : Image.image = { width = width, height = height, data = bgData }

(* A small checkerboard tile to show off blit. *)
val tile : Image.image =
  let
    val ts = 48
    val data = Word8Vector.tabulate (4 * ts * ts, fn i =>
      let
        val px = i div 4
        val ch = i mod 4
        val x = px mod ts
        val y = px div ts
        val on = ((x div 8) + (y div 8)) mod 2 = 0
        val v = if on then 235 else 90
      in
        if ch = 3 then 0w255 else Word8.fromInt v
      end)
  in { width = ts, height = ts, data = data } end

val img =
  let
    val c = canvas
    val c = Raster.fillCircle c { cx = 380, cy = 84, r = 38 } (rgba (255, 214, 120, 255))
    val c = Raster.circle     c { cx = 380, cy = 84, r = 38 } (rgba (255, 240, 200, 255))
    fun ray (c, dx, dy) =
      Raster.line c { x0 = 380, y0 = 84, x1 = 380 + dx, y1 = 84 + dy }
                  (rgba (255, 220, 150, 255))
    val c = ray (c, 78, 0)
    val c = ray (c, 64, 44)
    val c = ray (c, 40, 66)
    val c = ray (c, ~78, 0)
    val c = Raster.fillCircle c { cx = 90,  cy = 50, r = 2 } (rgba (255, 255, 255, 255))
    val c = Raster.fillCircle c { cx = 150, cy = 92, r = 2 } (rgba (235, 235, 255, 255))
    val c = Raster.fillCircle c { cx = 245, cy = 60, r = 2 } (rgba (255, 255, 235, 255))
    val c = Raster.fillTriangle c ((20, 300), (150, 150), (280, 300)) (rgba (60, 80, 110, 255))
    val c = Raster.fillTriangle c ((180, 300), (320, 120), (470, 300)) (rgba (50, 66, 96, 255))
    val c = Raster.fillPolygon c
              [ (0, 300), (90, 232), (190, 270), (300, 222)
              , (420, 280), (479, 250), (479, 319), (0, 319) ]
              (rgba (34, 96, 70, 255))
    val c = Raster.blit c { dst = (16, 232), src = tile }
    val c = Raster.rect c { x = 16, y = 232, w = 48, h = 48 } (rgba (255, 255, 255, 200))
    val c = Raster.rect c { x = 0, y = 0, w = width, h = height } (rgba (255, 255, 255, 255))
  in
    c
  end

val () =
  let
    val os = BinIO.openOut "assets/shapes.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/shapes.png\n"
  end
