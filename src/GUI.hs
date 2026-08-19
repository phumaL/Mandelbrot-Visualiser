{-# LANGUAGE OverloadedStrings #-}


-- | GTK3 GUI: all IO lives here; pure modules are called as functions.
module GUI (runGUI) where

import Control.Monad      (void, when)
import Data.IORef
import Data.Text          (Text)
import qualified Data.Text as T

import qualified GI.Gtk                    as Gtk
import qualified GI.Gdk                    as Gdk
import qualified GI.GdkPixbuf              as PB
import qualified GI.GLib                   as GLib
import qualified GI.Cairo.Render           as Cairo
import           GI.Cairo.Render.Connector (renderWithContext)
import qualified Data.ByteString           as BS
import           Control.Parallel.Strategies (parListChunk, rdeepseq, using)


import Fractal  (FractalConfig(..), FractalType(..), defaultConfig, escapeTime, newtonTime)
import Grid     (Viewport(..), generateGrid)
import Colour   (Colour(..), Greyscale(..), Fire(..), Ocean(..), newtonColour)
import Image    (applyColourScheme, savePPM)

-- ---------------------------------------------------------------------------
-- Top-level entry point (called from Main.hs)
-- ---------------------------------------------------------------------------

runGUI :: IO ()
runGUI = do
  _ <- Gtk.init Nothing

  -- Mutable state: the last rendered colour grid (Nothing until first render).
  lastGridRef  <- newIORef (Nothing :: Maybe [[Colour]])
  -- Store last render dimensions for Save PPM.
  lastDimsRef   <- newIORef (0 :: Int, 0 :: Int)
  -- Cached pixbuf for fast repaints (Nothing until first render).
  lastPixbufRef <- newIORef (Nothing :: Maybe PB.Pixbuf)
  -- Rubber-band state: (x0,y0) at press and (x1,y1) at current cursor position.
  rubberBandRef <- newIORef (Nothing :: Maybe (Double, Double, Double, Double))
  -- -------------------------------------------------------------------------
  -- Window
  -- -------------------------------------------------------------------------
  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win "Mandelbrot & Julia Renderer"
  Gtk.windowSetDefaultSize win 1100 720
  void $ Gtk.onWidgetDestroy win Gtk.mainQuit

  -- -------------------------------------------------------------------------
  -- Top-level layout: controls on the left, canvas on the right
  -- -------------------------------------------------------------------------
  mainBox <- Gtk.boxNew Gtk.OrientationHorizontal 6
  Gtk.containerAdd win mainBox

  -- ── Left panel ─────────────────────────────────────────────────────────
  ctrlFrame <- Gtk.frameNew (Just "Controls")
  Gtk.widgetSetMarginStart  ctrlFrame 8
  Gtk.widgetSetMarginEnd    ctrlFrame 4
  Gtk.widgetSetMarginTop    ctrlFrame 8
  Gtk.widgetSetMarginBottom ctrlFrame 8
  Gtk.boxPackStart mainBox ctrlFrame False False 0

  ctrlBox <- Gtk.boxNew Gtk.OrientationVertical 6
  Gtk.widgetSetMarginStart  ctrlBox 8
  Gtk.widgetSetMarginEnd    ctrlBox 8
  Gtk.widgetSetMarginTop    ctrlBox 8
  Gtk.widgetSetMarginBottom ctrlBox 8
  Gtk.containerAdd ctrlFrame ctrlBox

  -- ── Right panel: drawing area ───────────────────────────────────────────
  canvas <- Gtk.drawingAreaNew
  Gtk.widgetSetSizeRequest canvas 800 600
  Gtk.widgetAddEvents canvas
    [ Gdk.EventMaskButtonPressMask
    , Gdk.EventMaskButtonReleaseMask
    , Gdk.EventMaskButton1MotionMask
    ]
  Gtk.boxPackStart mainBox canvas True True 0

  -- -------------------------------------------------------------------------
  -- Helper: add a label + widget row to ctrlBox
  -- -------------------------------------------------------------------------
  let addRow :: Text -> Gtk.Widget -> IO ()
      addRow lbl w = do
        row <- Gtk.boxNew Gtk.OrientationHorizontal 4
        l   <- Gtk.labelNew (Just lbl)
        Gtk.widgetSetSizeRequest l 130 (-1)
        Gtk.labelSetXalign l 1.0
        Gtk.boxPackStart row l   False False 0
        Gtk.boxPackStart row w   True  True  0
        Gtk.boxPackStart ctrlBox row False False 0

  -- -------------------------------------------------------------------------
  -- Controls
  -- -------------------------------------------------------------------------

  -- Fractal type
  typeCombo <- Gtk.comboBoxTextNew
  Gtk.comboBoxTextAppendText typeCombo "Mandelbrot"
  Gtk.comboBoxTextAppendText typeCombo "Julia"
  Gtk.comboBoxTextAppendText typeCombo "Newton"
  Gtk.comboBoxSetActive typeCombo 0
  addRow "Fractal type:" =<< Gtk.toWidget typeCombo

  -- Width
  widthAdj <- Gtk.adjustmentNew 800 100 2000 1 10 0
  widthSpin <- Gtk.spinButtonNew (Just widthAdj) 1 0
  addRow "Width (px):" =<< Gtk.toWidget widthSpin

  -- Height
  heightAdj <- Gtk.adjustmentNew 600 100 2000 1 10 0
  heightSpin <- Gtk.spinButtonNew (Just heightAdj) 1 0
  addRow "Height (px):" =<< Gtk.toWidget heightSpin

  -- Max iterations
  iterAdj <- Gtk.adjustmentNew 256 8 2048 1 16 0
  iterSpin <- Gtk.spinButtonNew (Just iterAdj) 1 0
  addRow "Max iterations:" =<< Gtk.toWidget iterSpin

  -- Zoom
  zoomAdj <- Gtk.adjustmentNew 1.0 0.1 1.0e12 0.1 1.0 0
  zoomSpin <- Gtk.spinButtonNew (Just zoomAdj) 0.1 6
  addRow "Zoom:" =<< Gtk.toWidget zoomSpin

  -- Center X
  centerXEntry <- Gtk.entryNew
  Gtk.entrySetText centerXEntry "0.0"
  addRow "Center X:" =<< Gtk.toWidget centerXEntry

  -- Center Y
  centerYEntry <- Gtk.entryNew
  Gtk.entrySetText centerYEntry "0.0"
  addRow "Center Y:" =<< Gtk.toWidget centerYEntry

  -- Julia C real (shown only when Julia selected)
  juliaCRealEntry <- Gtk.entryNew
  Gtk.entrySetText juliaCRealEntry "-0.7"
  juliaCRealRow <- Gtk.boxNew Gtk.OrientationHorizontal 4
  juliaCRealLbl <- Gtk.labelNew (Just "Julia C real:")
  Gtk.widgetSetSizeRequest juliaCRealLbl 130 (-1)
  Gtk.labelSetXalign juliaCRealLbl 1.0
  Gtk.boxPackStart juliaCRealRow juliaCRealLbl False False 0
  juliaCRealWidget <- Gtk.toWidget juliaCRealEntry
  Gtk.boxPackStart juliaCRealRow juliaCRealWidget True True 0
  Gtk.boxPackStart ctrlBox juliaCRealRow False False 0

  -- Julia C imag
  juliaCImagEntry <- Gtk.entryNew
  Gtk.entrySetText juliaCImagEntry "0.27"
  juliaCImagRow <- Gtk.boxNew Gtk.OrientationHorizontal 4
  juliaCImagLbl <- Gtk.labelNew (Just "Julia C imag:")
  Gtk.widgetSetSizeRequest juliaCImagLbl 130 (-1)
  Gtk.labelSetXalign juliaCImagLbl 1.0
  Gtk.boxPackStart juliaCImagRow juliaCImagLbl False False 0
  juliaCImagWidget <- Gtk.toWidget juliaCImagEntry
  Gtk.boxPackStart juliaCImagRow juliaCImagWidget True True 0
  Gtk.boxPackStart ctrlBox juliaCImagRow False False 0

  -- Hide Julia C rows initially (Mandelbrot is default)
  Gtk.widgetHide juliaCRealRow
  Gtk.widgetHide juliaCImagRow

  -- Start Z real (shown only when Mandelbrot selected)
  startZRealScale <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal (-2.0) 2.0 0.01
  Gtk.scaleSetDrawValue startZRealScale True
  Gtk.scaleSetDigits startZRealScale 2
  Gtk.rangeSetValue startZRealScale 0.0
  startZRealRow <- Gtk.boxNew Gtk.OrientationVertical 2
  startZRealLbl <- Gtk.labelNew (Just "Start Z (re):")
  Gtk.labelSetXalign startZRealLbl 0.0
  Gtk.boxPackStart startZRealRow startZRealLbl False False 0
  startZRealWidget <- Gtk.toWidget startZRealScale
  Gtk.boxPackStart startZRealRow startZRealWidget False False 0
  Gtk.boxPackStart ctrlBox startZRealRow False False 0

  -- Start Z imag
  startZImagScale <- Gtk.scaleNewWithRange Gtk.OrientationHorizontal (-2.0) 2.0 0.01
  Gtk.scaleSetDrawValue startZImagScale True
  Gtk.scaleSetDigits startZImagScale 2
  Gtk.rangeSetValue startZImagScale 0.0
  startZImagRow <- Gtk.boxNew Gtk.OrientationVertical 2
  startZImagLbl <- Gtk.labelNew (Just "Start Z (im):")
  Gtk.labelSetXalign startZImagLbl 0.0
  Gtk.boxPackStart startZImagRow startZImagLbl False False 0
  startZImagWidget <- Gtk.toWidget startZImagScale
  Gtk.boxPackStart startZImagRow startZImagWidget False False 0
  Gtk.boxPackStart ctrlBox startZImagRow False False 0

  -- Colour scheme
  schemeCombo <- Gtk.comboBoxTextNew
  Gtk.comboBoxTextAppendText schemeCombo "Greyscale"
  Gtk.comboBoxTextAppendText schemeCombo "Fire"
  Gtk.comboBoxTextAppendText schemeCombo "Ocean"
  Gtk.comboBoxSetActive schemeCombo 0
  addRow "Colour scheme:" =<< Gtk.toWidget schemeCombo

  -- Separator
  sep <- Gtk.separatorNew Gtk.OrientationHorizontal
  Gtk.boxPackStart ctrlBox sep False False 4

  -- Render button
  renderBtn <- Gtk.buttonNewWithLabel "Render"
  Gtk.boxPackStart ctrlBox renderBtn False False 0

  -- Save PPM button
  saveBtn <- Gtk.buttonNewWithLabel "Save PPM"
  Gtk.boxPackStart ctrlBox saveBtn False False 0

  -- Reset button
  resetBtn <- Gtk.buttonNewWithLabel "Reset"
  Gtk.boxPackStart ctrlBox resetBtn False False 0

  -- Status label
  statusLbl <- Gtk.labelNew (Just "Ready.")
  Gtk.labelSetXalign statusLbl 0.0
  Gtk.boxPackStart ctrlBox statusLbl False False 4

  -- -------------------------------------------------------------------------
  -- Show/hide Julia C rows when fractal type changes
  -- -------------------------------------------------------------------------
  void $ Gtk.onComboBoxChanged typeCombo $ do
    idx <- Gtk.comboBoxGetActive typeCombo
    if idx == 1  -- Julia
      then do Gtk.widgetShow juliaCRealRow
              Gtk.widgetShow juliaCImagRow
      else do Gtk.widgetHide juliaCRealRow
              Gtk.widgetHide juliaCImagRow
    if idx == 0  -- Mandelbrot
      then do Gtk.widgetShow startZRealRow
              Gtk.widgetShow startZImagRow
      else do Gtk.widgetHide startZRealRow
              Gtk.widgetHide startZImagRow

  -- -------------------------------------------------------------------------
  -- Draw signal: reads last grid from IORef and paints with Cairo
  -- -------------------------------------------------------------------------
  void $ Gtk.onWidgetDraw canvas $ \cairoCtx -> do
    mPixbuf <- readIORef lastPixbufRef
    mBand   <- readIORef rubberBandRef
    case mPixbuf of
      Nothing     -> return ()
      Just pixbuf -> do
        Gdk.cairoSetSourcePixbuf cairoCtx pixbuf 0 0
        renderWithContext Cairo.paint cairoCtx
    renderWithContext (
      case mBand of
        Nothing             -> return ()
        Just (x0, y0, x1, y1) ->
          drawRubberBand (min x0 x1) (min y0 y1) (abs (x1-x0)) (abs (y1-y0))
      ) cairoCtx
    return True

  -- -------------------------------------------------------------------------
  -- Rubber-band select-to-zoom (press → drag → release draws a box;
  -- release zooms so that box fills the canvas).
  --
  -- Math: let unit = 4/(zoom*h).  The complex center of the selection is
  --   newCx = cx + unit*((x0+x1)/2 - w/2)
  --   newCy = cy - unit*((y0+y1)/2 - h/2)
  -- The new zoom scales so the shorter dimension of the box fills the canvas:
  --   newZoom = zoom * min(w/bw, h/bh)
  -- -------------------------------------------------------------------------
  void $ Gtk.onWidgetButtonPressEvent canvas $ \ev -> do
    btn <- Gdk.getEventButtonButton ev
    when (btn == 1) $ do
      x <- Gdk.getEventButtonX ev
      y <- Gdk.getEventButtonY ev
      writeIORef rubberBandRef (Just (x, y, x, y))
    return True

  void $ Gtk.onWidgetMotionNotifyEvent canvas $ \ev -> do
    x     <- Gdk.getEventMotionX ev
    mBand <- readIORef rubberBandRef
    case mBand of
      Nothing          -> return False
      Just (x0,y0,_,_) -> do
        -- Constrain the box to the canvas aspect ratio so the selection
        -- maps perfectly onto the canvas with no extra border.
        cw <- Gtk.spinButtonGetValueAsInt widthSpin
        ch <- Gtk.spinButtonGetValueAsInt heightSpin
        let ar = fromIntegral cw / fromIntegral ch :: Double
            y1 = y0 + (x - x0) / ar
        writeIORef rubberBandRef (Just (x0, y0, x, y1))
        Gtk.widgetQueueDraw canvas
        return True

  void $ Gtk.onWidgetButtonReleaseEvent canvas $ \ev -> do
    btn   <- Gdk.getEventButtonButton ev
    mBand <- readIORef rubberBandRef
    writeIORef rubberBandRef Nothing
    when (btn == 1) $
      case mBand of
        Nothing -> return ()
        Just (x0, y0, x1, y1) -> do
          let bw = abs (x1 - x0)
              bh = abs (y1 - y0)
          when (bw > 4 && bh > 4) $ do
            w     <- Gtk.spinButtonGetValueAsInt widthSpin
            h     <- Gtk.spinButtonGetValueAsInt heightSpin
            z     <- Gtk.spinButtonGetValue zoomSpin
            cxTxt <- Gtk.entryGetText centerXEntry
            cyTxt <- Gtk.entryGetText centerYEntry
            case (reads (T.unpack cxTxt), reads (T.unpack cyTxt)) of
              ([(cx, "")], [(cy, "")]) -> do
                let unit    = 4.0 / (z * fromIntegral h)
                    newCx   = cx + unit * ((x0+x1)/2 - fromIntegral w / 2)
                    newCy   = cy - unit * ((y0+y1)/2 - fromIntegral h / 2)
                    newZoom = min 1.0e12 $
                                z * min (fromIntegral w / bw) (fromIntegral h / bh)
                Gtk.spinButtonSetValue zoomSpin newZoom
                Gtk.entrySetText centerXEntry (T.pack $ show newCx)
                Gtk.entrySetText centerYEntry (T.pack $ show newCy)
                Gtk.buttonClicked renderBtn
              _ -> return ()
    return True

  -- -------------------------------------------------------------------------
  -- Render button: pure pipeline → store result → queue redraw
  -- -------------------------------------------------------------------------
  void $ Gtk.onButtonClicked renderBtn $ do
    -- Read control values
    eCfg <- readConfig typeCombo widthSpin heightSpin iterSpin zoomSpin
                       centerXEntry centerYEntry
                       juliaCRealEntry juliaCImagEntry
                       startZRealScale startZImagScale schemeCombo
    case eCfg of
      Left err -> Gtk.labelSetText statusLbl (T.pack $ "Error: " ++ err)
      Right (cfg, vp, schemeIdx) -> do
        Gtk.labelSetText statusLbl "Rendering…"
        -- Flush pending GTK events so the label update is visible.
        void $ Gtk.mainIterationDo False

        -- ── Pure computation pipeline (rows evaluated in parallel) ────────
        let grid       = generateGrid vp
            colourGrid = case fractalType cfg of
              Newton ->
                let times = map (map (newtonTime  cfg)) grid
                              `using` parListChunk 32 rdeepseq
                in  map (map (\rt -> newtonColour rt (maxIter cfg))) times
              _      ->
                let times = map (map (escapeTime  cfg)) grid
                              `using` parListChunk 32 rdeepseq
                in  applyScheme schemeIdx (maxIter cfg) times
        -- ── End pure pipeline ──────────────────────────────────────────────

        -- Convert to a GdkPixbuf for single-call canvas painting.
        pixbuf <- colourGridToPixbuf colourGrid
        -- Store results: grid for Save PPM, pixbuf for draw handler.
        writeIORef lastGridRef   (Just colourGrid)
        writeIORef lastPixbufRef (Just pixbuf)
        writeIORef lastDimsRef   (vpWidth vp, vpHeight vp)

        -- Resize canvas to match render dimensions and trigger redraw.
        Gtk.widgetSetSizeRequest canvas
          (fromIntegral $ vpWidth vp) (fromIntegral $ vpHeight vp)
        Gtk.widgetQueueDraw canvas
        Gtk.labelSetText statusLbl "Done."

  -- -------------------------------------------------------------------------
  -- Save PPM button
  -- -------------------------------------------------------------------------
  void $ Gtk.onButtonClicked saveBtn $ do
    mGrid <- readIORef lastGridRef
    case mGrid of
      Nothing -> Gtk.labelSetText statusLbl "Render first."
      Just colourGrid -> do
        (w, h) <- readIORef lastDimsRef
        -- FileChooserNative avoids the varargs binding issue of fileChooserDialogNew.
        fcn <- Gtk.fileChooserNativeNew
                 (Just "Save PPM")
                 (Just win)
                 Gtk.FileChooserActionSave
                 (Just "_Save")
                 (Just "_Cancel")
        Gtk.fileChooserSetCurrentName fcn "output.ppm"
        resp <- Gtk.nativeDialogRun fcn
        when (resp == fromIntegral (fromEnum Gtk.ResponseTypeAccept)) $ do
          mPath <- Gtk.fileChooserGetFilename fcn
          case mPath of
            Nothing   -> Gtk.labelSetText statusLbl "No file selected."
            Just path -> do
              savePPM path w h colourGrid
              Gtk.labelSetText statusLbl $ T.pack $ "Saved to " ++ path

  -- -------------------------------------------------------------------------
  -- Reset button: restore all controls to defaults and re-render
  -- -------------------------------------------------------------------------
  void $ Gtk.onButtonClicked resetBtn $ do
    Gtk.comboBoxSetActive typeCombo   0
    Gtk.spinButtonSetValue widthSpin  800
    Gtk.spinButtonSetValue heightSpin 600
    Gtk.spinButtonSetValue iterSpin   256
    Gtk.spinButtonSetValue zoomSpin   1.0
    Gtk.entrySetText centerXEntry     "0.0"
    Gtk.entrySetText centerYEntry     "0.0"
    Gtk.entrySetText juliaCRealEntry  "-0.7"
    Gtk.entrySetText juliaCImagEntry  "0.27"
    Gtk.rangeSetValue startZRealScale 0.0
    Gtk.rangeSetValue startZImagScale 0.0
    Gtk.comboBoxSetActive schemeCombo 0
    Gtk.widgetHide juliaCRealRow
    Gtk.widgetHide juliaCImagRow
    Gtk.widgetShow startZRealRow
    Gtk.widgetShow startZImagRow
    Gtk.buttonClicked renderBtn

  -- -------------------------------------------------------------------------
  -- Show window and enter the GTK event loop
  -- -------------------------------------------------------------------------
  Gtk.widgetShowAll win
  -- Re-hide Julia C rows (widgetShowAll would have shown them).
  Gtk.widgetHide juliaCRealRow
  Gtk.widgetHide juliaCImagRow
  Gtk.main

-- ---------------------------------------------------------------------------
-- Pixbuf + Cairo rendering helpers
-- ---------------------------------------------------------------------------

-- | Pack a colour grid into a GdkPixbuf (RGB, 8 bits/sample, no alpha).
--   The pixbuf is used for a single @cairoSetSourcePixbuf@ + @paint@ call,
--   replacing the old per-pixel Cairo rectangle approach (~480 000 operations
--   for 800×600 reduced to 1).
colourGridToPixbuf :: [[Colour]] -> IO PB.Pixbuf
colourGridToPixbuf rows = do
  let h  = length rows
      w  = if null rows then 0 else length (head rows)
      bs = BS.pack $ concatMap (concatMap (\(Colour r g b) -> [r, g, b])) rows
  gb <- GLib.bytesNew (Just bs)
  PB.pixbufNewFromBytes gb PB.ColorspaceRgb False 8
    (fromIntegral w) (fromIntegral h) (fromIntegral (w * 3))

-- | Draw the rubber-band selection rectangle as a white stroked outline.
drawRubberBand :: Double -> Double -> Double -> Double -> Cairo.Render ()
drawRubberBand rx ry rw rh = do
  Cairo.setSourceRGBA 1.0 1.0 1.0 0.9
  Cairo.setLineWidth 1.5
  Cairo.rectangle rx ry rw rh
  Cairo.stroke

-- ---------------------------------------------------------------------------
-- Config reading helpers (pure where possible, IO only for widget queries)
-- ---------------------------------------------------------------------------

-- | Read all control values and validate them.
--   Returns @Left errMsg@ on bad input, or @Right (cfg, vp, schemeIdx)@.
readConfig
  :: Gtk.ComboBoxText -> Gtk.SpinButton -> Gtk.SpinButton
  -> Gtk.SpinButton -> Gtk.SpinButton
  -> Gtk.Entry -> Gtk.Entry -> Gtk.Entry -> Gtk.Entry
  -> Gtk.Scale -> Gtk.Scale
  -> Gtk.ComboBoxText
  -> IO (Either String (FractalConfig, Viewport, Int))
readConfig typeCombo widthSpin heightSpin iterSpin zoomSpin
           cxEntry cyEntry crEntry ciEntry
           szrScale sziScale schemeCombo = do
  typeIdx   <- Gtk.comboBoxGetActive typeCombo
  w         <- Gtk.spinButtonGetValueAsInt widthSpin
  h         <- Gtk.spinButtonGetValueAsInt heightSpin
  iters     <- Gtk.spinButtonGetValueAsInt iterSpin
  z         <- Gtk.spinButtonGetValue     zoomSpin
  cxTxt     <- Gtk.entryGetText cxEntry
  cyTxt     <- Gtk.entryGetText cyEntry
  crTxt     <- Gtk.entryGetText crEntry
  ciTxt     <- Gtk.entryGetText ciEntry
  szr       <- Gtk.rangeGetValue szrScale
  szi       <- Gtk.rangeGetValue sziScale
  schemeIdx <- Gtk.comboBoxGetActive schemeCombo

  -- Parse text entries using Either for safe failure.
  return $ do
    cx  <- parseDouble "Center X"    cxTxt
    cy  <- parseDouble "Center Y"    cyTxt
    cr  <- parseDouble "Julia C re"  crTxt
    ci  <- parseDouble "Julia C im"  ciTxt
    let ftype = case typeIdx of
                  1 -> Julia
                  2 -> Newton
                  _ -> Mandelbrot
        cfg = defaultConfig
                { fractalType = ftype
                , center      = (cx, cy)
                , zoom        = z
                , maxIter     = fromIntegral iters
                , juliaC      = (cr, ci)
                , startZ      = (szr, szi)
                }
        vp  = Viewport
                { vpWidth  = fromIntegral w
                , vpHeight = fromIntegral h
                , vpCenter = (cx, cy)
                , vpZoom   = z
                }
    Right (cfg, vp, fromIntegral schemeIdx)

-- | Parse a @Text@ value as a @Double@, labelling errors with the field name.
parseDouble :: String -> Text -> Either String Double
parseDouble field txt =
  case reads (T.unpack txt) of
    [(d, "")] -> Right d
    _         -> Left $ "Invalid number for " ++ field ++ ": " ++ T.unpack txt

-- | Dispatch to the correct colour scheme instance by index.
--   Uses pattern matching on an Int rather than existential types, satisfying
--   the requirement that pattern matching appears in non-trivial contexts.
applyScheme :: Int -> Int -> [[Int]] -> [[Colour]]
applyScheme schemeIdx maxI escapeTimes =
  case schemeIdx of
    1    -> applyColourScheme Fire      maxI escapeTimes
    2    -> applyColourScheme Ocean     maxI escapeTimes
    _    -> applyColourScheme Greyscale maxI escapeTimes
