(* test.sml -- TDD spec for sml-raster.

   Tests use small blank images and assert EXACT pixel values via
   Image.getPixel at known coordinates. *)

structure Tests =
struct
  open Harness

  structure I = Image
  structure R = Raster

  fun b i = Word8.fromInt i

  (* color constructors *)
  fun rgba (r, g, bl, a) : I.rgba8 = { r = b r, g = b g, b = b bl, a = b a }
  val transparent = rgba (0, 0, 0, 0)
  val black  = rgba (0, 0, 0, 255)
  val white  = rgba (255, 255, 255, 255)
  val red    = rgba (255, 0, 0, 255)
  val green  = rgba (0, 255, 0, 255)
  val blue   = rgba (0, 0, 255, 255)

  (* readable pixel comparison (r,g,b,a as ints) *)
  fun pxStr ({ r, g, b = bl, a } : I.rgba8) =
    "(" ^ Int.toString (Word8.toInt r) ^ ","
        ^ Int.toString (Word8.toInt g) ^ ","
        ^ Int.toString (Word8.toInt bl) ^ ","
        ^ Int.toString (Word8.toInt a) ^ ")"

  fun eqPx (p : I.rgba8, q : I.rgba8) =
    #r p = #r q andalso #g p = #g q andalso #b p = #b q andalso #a p = #a q

  fun checkPixel name img (x, y) expected =
    let val actual = I.getPixel img (x, y)
    in if eqPx (expected, actual) then pass name
       else fail name ("at (" ^ Int.toString x ^ "," ^ Int.toString y ^ ") expected "
                       ^ pxStr expected ^ " got " ^ pxStr actual)
    end
  and pass name = check name true
  and fail name detail = check (name ^ " [" ^ detail ^ "]") false

  fun mk (w, h) = R.blank (w, h) transparent

  fun runAll () =
  let
  in
    section "blank";
    let val img = R.blank (8, 8) transparent
    in checkInt "blank width" (8, #width img);
       checkInt "blank height" (8, #height img);
       checkPixel "blank is transparent" img (0,0) transparent;
       checkPixel "blank is transparent (corner)" img (7,7) transparent
    end;

    section "setPixel";
    let val img = R.setPixel (mk (8,8)) (3,4) red
    in checkPixel "setPixel target" img (3,4) red;
       checkPixel "setPixel left neighbor untouched" img (2,4) transparent;
       checkPixel "setPixel right neighbor untouched" img (4,4) transparent;
       checkPixel "setPixel above untouched" img (3,3) transparent;
       checkPixel "setPixel below untouched" img (3,5) transparent
    end;

    section "setPixel clipping";
    let val base = mk (8,8)
        val img = R.setPixel base (~1, 4) red
        val img2 = R.setPixel img (8, 4) green
        val img3 = R.setPixel img2 (4, 99) blue
    in check "setPixel oob does not raise" true;
       checkPixel "setPixel oob no-op left" img3 (0,4) transparent;
       checkPixel "setPixel oob no-op corner" img3 (7,7) transparent
    end;

    section "line horizontal";
    let val img = R.line (mk (8,8)) { x0 = 1, y0 = 2, x1 = 5, y1 = 2 } white
    in checkPixel "hline start" img (1,2) white;
       checkPixel "hline mid"   img (3,2) white;
       checkPixel "hline end"   img (5,2) white;
       checkPixel "hline before start unset" img (0,2) transparent;
       checkPixel "hline after end unset" img (6,2) transparent;
       checkPixel "hline other row unset" img (3,3) transparent
    end;

    section "line vertical";
    let val img = R.line (mk (8,8)) { x0 = 4, y0 = 1, x1 = 4, y1 = 6 } white
    in checkPixel "vline start" img (4,1) white;
       checkPixel "vline mid"   img (4,3) white;
       checkPixel "vline end"   img (4,6) white;
       checkPixel "vline before unset" img (4,0) transparent;
       checkPixel "vline after unset" img (4,7) transparent
    end;

    section "line diagonal";
    let val img = R.line (mk (8,8)) { x0 = 0, y0 = 0, x1 = 4, y1 = 4 } white
    in checkPixel "diag (0,0)" img (0,0) white;
       checkPixel "diag (1,1)" img (1,1) white;
       checkPixel "diag (2,2)" img (2,2) white;
       checkPixel "diag (4,4)" img (4,4) white;
       checkPixel "diag off-line unset" img (0,4) transparent
    end;

    section "fillRect";
    let val img = R.fillRect (mk (8,8)) { x = 2, y = 2, w = 3, h = 2 } green
    in checkPixel "fillRect TL" img (2,2) green;
       checkPixel "fillRect TR" img (4,2) green;
       checkPixel "fillRect BL" img (2,3) green;
       checkPixel "fillRect BR" img (4,3) green;
       checkPixel "fillRect outside left" img (1,2) transparent;
       checkPixel "fillRect outside right" img (5,2) transparent;
       checkPixel "fillRect outside below" img (2,4) transparent
    end;

    section "rect outline";
    let val img = R.rect (mk (8,8)) { x = 1, y = 1, w = 4, h = 4 } blue
    in checkPixel "rect top-left corner" img (1,1) blue;
       checkPixel "rect top-right corner" img (4,1) blue;
       checkPixel "rect bottom-left corner" img (1,4) blue;
       checkPixel "rect bottom-right corner" img (4,4) blue;
       checkPixel "rect top edge" img (2,1) blue;
       checkPixel "rect left edge" img (1,2) blue;
       checkPixel "rect interior unset" img (2,2) transparent;
       checkPixel "rect interior unset 2" img (3,3) transparent
    end;

    section "fillCircle / circle";
    let val img = R.fillCircle (mk (16,16)) { cx = 8, cy = 8, r = 4 } red
    in checkPixel "fillCircle center" img (8,8) red;
       checkPixel "fillCircle near center" img (9,8) red;
       checkPixel "fillCircle inside radius" img (8,11) red;
       checkPixel "fillCircle far corner unset" img (0,0) transparent;
       checkPixel "fillCircle outside radius unset" img (8,14) transparent
    end;
    let val img = R.circle (mk (16,16)) { cx = 8, cy = 8, r = 4 } red
    in checkPixel "circle east point" img (12,8) red;
       checkPixel "circle west point" img (4,8) red;
       checkPixel "circle north point" img (8,4) red;
       checkPixel "circle south point" img (8,12) red;
       checkPixel "circle center unset (outline only)" img (8,8) transparent
    end;

    section "ellipse outline";
    let
      val cx = 12 and cy = 12 and rx = 8 and ry = 5
      val img = R.ellipse (mk (25,25)) { cx = cx, cy = cy, rx = rx, ry = ry } red
      (* symmetry about x = cx and y = cy across the whole framebuffer *)
      fun symmetric () =
        let
          fun loopY y =
            if y >= 25 then true
            else
              let
                fun loopX x =
                  if x >= 25 then true
                  else
                    eqPx (I.getPixel img (x, y), I.getPixel img (2*cx - x, y))
                    andalso eqPx (I.getPixel img (x, y), I.getPixel img (x, 2*cy - y))
                    andalso loopX (x + 1)
              in loopX 0 andalso loopY (y + 1) end
        in loopY 0 end
    in
      checkPixel "ellipse east extreme" img (cx+rx, cy) red;
      checkPixel "ellipse west extreme" img (cx-rx, cy) red;
      checkPixel "ellipse north extreme" img (cx, cy-ry) red;
      checkPixel "ellipse south extreme" img (cx, cy+ry) red;
      checkPixel "ellipse center unset (outline only)" img (cx, cy) transparent;
      checkPixel "ellipse far corner unset" img (0,0) transparent;
      check "ellipse is symmetric about both axes" (symmetric ())
    end;

    section "fillEllipse";
    let
      val cx = 12 and cy = 12 and rx = 8 and ry = 5
      val img = R.fillEllipse (mk (25,25)) { cx = cx, cy = cy, rx = rx, ry = ry } green
    in
      checkPixel "fillEllipse center" img (cx, cy) green;
      checkPixel "fillEllipse interior" img (cx+4, cy+1) green;
      checkPixel "fillEllipse east extreme" img (cx+rx, cy) green;
      checkPixel "fillEllipse north extreme" img (cx, cy-ry) green;
      checkPixel "fillEllipse just past east extreme unset" img (cx+rx+1, cy) transparent;
      checkPixel "fillEllipse far corner unset" img (0,0) transparent
    end;

    section "arc";
    let
      val cx = 12 and cy = 12 and r = 8
      val circ = R.circle (mk (25,25)) { cx = cx, cy = cy, r = r } blue
      val full = R.arc (mk (25,25))
                   { cx = cx, cy = cy, r = r, startAngle = 0.0, endAngle = 2.0 * Math.pi } blue
      val half = R.arc (mk (25,25))
                   { cx = cx, cy = cy, r = r, startAngle = 0.0, endAngle = Math.pi } blue
    in
      check "full-turn arc equals circle primitive" (#data full = #data circ);
      checkPixel "half arc covers +y semicircle" half (cx, cy+r) blue;
      checkPixel "half arc excludes -y semicircle" half (cx, cy-r) transparent;
      checkPixel "half arc start endpoint set" half (cx+r, cy) blue;
      check "arc off-screen center does not raise"
        (let val _ = R.arc (mk (4,4))
                        { cx = ~5, cy = ~5, r = 3, startAngle = 0.0, endAngle = 1.0 } blue
         in true end)
    end;

    section "triangle outline";
    let val img = R.triangle (mk (8,8)) ((0,0),(6,0),(0,6)) white
    in checkPixel "triangle vertex A" img (0,0) white;
       checkPixel "triangle vertex B" img (6,0) white;
       checkPixel "triangle vertex C" img (0,6) white;
       checkPixel "triangle top edge mid" img (3,0) white;
       checkPixel "triangle left edge mid" img (0,3) white;
       checkPixel "triangle interior unset" img (1,1) transparent
    end;

    section "fillTriangle";
    let val img = R.fillTriangle (mk (8,8)) ((1,1),(6,1),(1,6)) green
    in checkPixel "fillTriangle interior" img (2,2) green;
       checkPixel "fillTriangle near right-angle" img (2,3) green;
       checkPixel "fillTriangle vertex" img (1,1) green;
       checkPixel "fillTriangle exterior (above)" img (4,0) transparent;
       checkPixel "fillTriangle exterior (beyond hypotenuse)" img (6,6) transparent
    end;

    section "polyline";
    let val img = R.polyline (mk (8,8)) [(0,0),(0,3),(3,3)] white
    in checkPixel "polyline p0" img (0,0) white;
       checkPixel "polyline corner" img (0,3) white;
       checkPixel "polyline p2" img (3,3) white;
       checkPixel "polyline mid seg1" img (0,1) white;
       checkPixel "polyline mid seg2" img (2,3) white;
       checkPixel "polyline off-path unset" img (3,0) transparent
    end;

    section "fillPolygon";
    let val img = R.fillPolygon (mk (8,8)) [(1,1),(6,1),(6,6),(1,6)] blue
    in checkPixel "fillPolygon interior" img (3,3) blue;
       checkPixel "fillPolygon edge area" img (1,1) blue;
       checkPixel "fillPolygon outside" img (0,0) transparent;
       checkPixel "fillPolygon outside 2" img (7,7) transparent
    end;

    section "blendPixel";
    (* background opaque red, blend half-alpha white over it.
       a = 128. out = (src*128 + dst*(255-128) + 127) div 255
       R: (255*128 + 255*127 + 127) div 255 = (32640+32385+127)/255 = 65152/255 = 255
       G: (255*128 + 0*127 + 127) div 255 = (32640+127)/255 = 32767/255 = 128
       B: same as G = 128
       A: (128*128 + 255*127 + 127) div 255 = (16384+32385+127)/255 = 48896/255 = 191 *)
    let val bg = R.fillRect (mk (4,4)) { x=0,y=0,w=4,h=4 } red
        val img = R.blendPixel bg (1,1) (rgba (255,255,255,128))
    in checkPixel "blendPixel half white over red" img (1,1) (rgba (255,128,128,191));
       checkPixel "blendPixel neighbor unchanged" img (0,1) red
    end;
    (* opaque blend (a=255) overwrites *)
    let val bg = R.fillRect (mk (4,4)) { x=0,y=0,w=4,h=4 } red
        val img = R.blendPixel bg (2,2) green
    in checkPixel "blendPixel opaque overwrites" img (2,2) green
    end;
    (* fully transparent blend (a=0) leaves dst *)
    let val bg = R.fillRect (mk (4,4)) { x=0,y=0,w=4,h=4 } red
        val img = R.blendPixel bg (2,2) (rgba (0,255,0,0))
    in checkPixel "blendPixel transparent no change" img (2,2) red
    end;

    section "blit";
    let val src = R.fillRect (mk (3,3)) { x=0,y=0,w=3,h=3 } green
        val dst = mk (8,8)
        val img = R.blit dst { dst = (2,2), src = src }
    in checkPixel "blit TL" img (2,2) green;
       checkPixel "blit BR" img (4,4) green;
       checkPixel "blit outside before" img (1,2) transparent;
       checkPixel "blit outside after" img (5,5) transparent
    end;
    (* blit clipped at edge *)
    let val src = R.fillRect (mk (3,3)) { x=0,y=0,w=3,h=3 } green
        val dst = mk (8,8)
        val img = R.blit dst { dst = (6,6), src = src }
    in check "blit at edge does not raise" true;
       checkPixel "blit clipped visible" img (6,6) green;
       checkPixel "blit clipped visible corner" img (7,7) green
    end;
    (* blit at negative offset clips *)
    let val src = R.fillRect (mk (3,3)) { x=0,y=0,w=3,h=3 } green
        val dst = mk (8,8)
        val img = R.blit dst { dst = (~1,~1), src = src }
    in check "blit neg offset does not raise" true;
       checkPixel "blit neg offset visible part" img (0,0) green;
       checkPixel "blit neg offset visible part 2" img (1,1) green
    end;

    ()
  end

  fun run () = (reset (); runAll (); Harness.run ())
end
