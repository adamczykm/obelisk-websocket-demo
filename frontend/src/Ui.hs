{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Ui where

import Data.Align
import Data.Functor (($>))
import Data.Maybe (fromJust, isJust)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Bootstrap4 as B4
import Common.Utils (Email, Password, parseEmail, parsePassword)
import Control.Monad (zipWithM)
import Reflex.Dom.Core
import Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Fix (MonadFix)
import Control.Lens (over, _Left, _2)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Array (listArray, (!), Array)
import TextShow
import Data.Default (Default)
import Obelisk.Generated.Static
import Control.Lens.TH
import Data.Bool (bool)
import DomUtils
import JSDOM.Types (MonadJSM, uncheckedCastTo)
import qualified JSDOM.EventM as JSDOM
import qualified JSDOM.Generated.EventTarget as JSDOM

-- this is all very provisional
type InputVerificator a = Text -> Either Text a

data InputType
  = InputText
  | InputEmail
  | InputPassword
  | InputCheck
  | InputRadio
  deriving(Eq, Show)

data FormSpec a where
  FormSpecInput ::
    { _inputType :: InputType
    , _labelText :: Text
    , _placeHolder :: Maybe Text
    , _description :: Maybe Text
    , _verificator :: InputVerificator a
    } -> FormSpec a
  FormSpecAppend ::  FormSpec a -> FormSpec b -> FormSpec (a,b)

(+:) :: FormSpec a -> FormSpec b -> FormSpec (a,b)
(+:) = FormSpecAppend
infixr 4 +:

-- form = buildForm (defEmil <> defPassword <. defRemember)
emailForm_ :: Text -> FormSpec Email
emailForm_ lbl = FormSpecInput InputEmail lbl Nothing Nothing parseEmail

passwordForm_ :: Text -> FormSpec Password
passwordForm_ lbl = FormSpecInput InputPassword lbl Nothing Nothing parsePassword

-- TODO change first parameter to config

buildForm_ :: forall t m a.
  (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m)
  => Text -> FormSpec a -> m (Event t a)
buildForm_ formId fs = elAttr "form" ("id" =: formId) $ do
    formInputChanged <- buildFormInputs 0 fs
    divClass "text-center" $ switchDyn <$> widgetHold disabledBtn (submitBtn <$> updated formInputChanged)
  
  where
    submitBtn :: forall b. Either b a -> m (Event t a)
    submitBtn = \case
      Left _  -> disabledBtn
      Right a -> (a <$) <$> enabledBtn

    disabledBtn = never <$ B4.buttonClass [B4.btnDark, "disabled"] "Login"
    enabledBtn = B4.buttonClass [B4.btnDark] "Login"

    buildFormInputs :: forall a'. Int -> FormSpec a' -> m (Dynamic t (Either [Text] a'))
    buildFormInputs ix FormSpecInput {..} = elAttr "div" ("class" =: "form-group" <> "style" =: "margin-bottom:0.75rem") $ do
      rec
        tn <- inputElement $ def
          & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ initialInputAttrs
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ modifyAttrsEv

        let verifyInputEv = _verificator <$> _inputElement_input tn
            modifyAttrsEv = ffor verifyInputEv $ \case
              Left _ -> incorrectInputAttrs
              Right _ -> correctInputAttrs

      widgetHold_  emptyFeedback (feedbackWidget <$> verifyInputEv)
      return $ over _Left (:[]) . _verificator <$> _inputElement_value tn
      where
        emptyFeedback = elAttr "div" ("class" =: "invalid-feedback" <> "style" =: "visibility: hidden; display: block") $ text "hidden"
        feedbackWidget :: forall b'. Either Text b' -> m ()
        feedbackWidget = \case
          Right _ -> emptyFeedback
          Left txt -> elAttr "div" ("class" =: "invalid-feedback" <> "style" =: "visibility: visible") $ text txt

        initialInputAttrs =
          "id" =: inputName <>
          "type" =: getInputType _inputType <>
          "placeholder" =: _labelText <>
          "class" =: "form-control"

        correctInputAttrs =
          "id" =: Just inputName <>
          "type" =: Just (getInputType _inputType) <>
          "placeholder" =: Just _labelText <>
          "class" =: Just "form-control is-valid"

        incorrectInputAttrs =
          "id" =: Just inputName <>
          "type" =: Just (getInputType _inputType) <>
          "placeHolder" =: Just _labelText <>
          "class" =: Just "form-control is-invalid"

        inputName = T.intercalate "-" [formId, T.pack (show ix ++ show _inputType)]
        getInputType = \case
          InputEmail -> "email"
          InputText -> "text"
          InputPassword -> "password"
          InputCheck -> "checkbox"
          InputRadio -> "radio"

    buildFormInputs ix (FormSpecAppend aInp bInp) = do
      aDyn <- buildFormInputs ix aInp
      bDyn <- buildFormInputs (ix+1) bInp
      return $ ffor2 aDyn bDyn $ \aE bE -> case (aE, bE) of
        (Right a, Right b) -> Right (a, b)
        (Left t1, Left t2) -> Left (t1 ++ t2)
        (Left t1, _) -> Left t1
        (_, Left t2) -> Left t2

-- =================================== SORTTABLE =============================

data SortKey row = NoSort | forall k. Ord k => SortKey (row -> k)

isNoSort :: SortKey row -> Bool
isNoSort NoSort = True
isNoSort _ = False

data SortOrder = SortAsc | SortDesc
  deriving(Eq,Show)

-- | Internal data structure for representing status of sorting in a SortTable
data SortStatus row = SortStatus
  { _headerIx :: Maybe Int -- ^ Index of current "sorting header" if any is active.
  , _ascending :: Bool -- ^ Order of sorting
  , _key :: SortKey row -- ^ Key by which rows will be sorted.
  }


data SortTableConfig t row = SortTableConfig
  { _sortTableConfig_initialSortStatus :: SortStatus row
  , _sortTableConfig_tableDynAttrs :: Dynamic t (Map Text Text)
  , _sortTableConfig_theadDynAttrs :: Dynamic t (Map Text Text)
  , _sortTableConfig_tbodydDynAttrs :: Dynamic t (Map Text Text)
  }
makeLenses ''SortTableConfig

instance Reflex t => Default (SortTableConfig t row) where
  def = SortTableConfig (SortStatus Nothing False NoSort) (constDyn table) (constDyn mempty) (constDyn mempty)
    where
      table = "class" =: "table table-sortable"




--- TODO if sorttable is released as standalone component, host css somewhere
linkSortTableCss :: DomBuilder t m => m ()
linkSortTableCss = elAttr "link" attrs (return ())
  where
    attrs = Map.fromList
      [ ("rel", "stylesheet")
      , ("type","text/css")
      , ("crossorigin","anonymous")
      , ("href", static @"sort_table.css")
      ]

linkReflexUiCss :: DomBuilder t m => m ()
linkReflexUiCss = elAttr "link" attrs (return ())
  where
    attrs = Map.fromList
      [ ("rel", "stylesheet")
      , ("type","text/css")
      , ("crossorigin","anonymous")
      , ("href", static @"ui.css")
      ]

sortTable :: forall t m row a. (MonadIO m, MonadIO (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => SortTableConfig t row -- ^ Configuration of SortTable widget
  -> [(m (), SortKey row)] -- ^ List of header widgets paired with sortkey selection (usually just text)
  -> Dynamic t [row] -- ^ Dynamic list of rows
  -> (Dynamic t row -> m a) -- ^ Function creating <tr> element from row
  -> m (Dynamic t [a])
sortTable cfg headers dynRows rowWidget = do

  -- Define new event so we don't have to create a big rec block
  (headerClicked, headerClicked_IOAction) <- newTriggerEvent

  let
    getSortKey :: Array Int (SortKey row)
    getSortKey =
      let sk = snd <$> headers
      in listArray (0, length sk - 1) sk

    getCurrentSort :: m (Dynamic t (SortStatus row))
    getCurrentSort = foldDyn newSort (_sortTableConfig_initialSortStatus cfg) headerClicked
      where
        newSort newIx (SortStatus hdrIx isAsc _) =
          let newIsAsc = if Just newIx == hdrIx then not isAsc else True
          in SortStatus (Just newIx) newIsAsc (getSortKey ! newIx)

  -- state of table sorting
  currentSort <- getCurrentSort

  let
    -- dyn of sorted rows (actual source of table rows)
    sortedRowsDyn :: Dynamic t [row]
    sortedRowsDyn = ffor2 currentSort dynRows $ \(SortStatus _ isAsc sortKey) rows ->
      case sortKey of
        NoSort -> rows
        SortKey rk ->
          let ordering r1 r2 = if isAsc
                then compare (rk r1) (rk r2)
                else compare (rk r2) (rk r1)
          in sortBy ordering rows

    makeHeaderWidget :: Dynamic t (SortStatus row) -> Int -> m () -> m (Event t Int)
    makeHeaderWidget sortStatusDyn ix w = do
      (e,_) <- elAttr' "th" ("scope" =: "col") $ elAttr "span" ("style" =: "white-space:nowrap") $ do
        w
        elDynAttr "span" sortIndicatorDynAttr $ return ()

      if isNoSort (getSortKey ! ix)
        then return never
        else return $ domEvent Click e $> ix

        where
          sortIndicatorDynAttr = ffor sortStatusDyn $ \(SortStatus hdrIx isAsc _) ->
            case getSortKey ! ix of
              NoSort -> mempty
              _ -> "class" =: (if Just ix /= hdrIx
                               then "can-sort"
                               else if isAsc then "asc" else "desc")


  -- creating table
  elDynAttr "table" (_sortTableConfig_tableDynAttrs cfg) $ do
    headerClicks <- elDynAttr "thead" (_sortTableConfig_theadDynAttrs cfg) $ el "tr" $
      zipWithM (makeHeaderWidget currentSort) [0..] (fst <$> headers)

    -- Perform defined headerClickedEv
    _ <- performEvent $ liftIO . headerClicked_IOAction <$> leftmost headerClicks

    elDynAttr "tbody" (_sortTableConfig_tbodydDynAttrs cfg) $
      simpleList sortedRowsDyn rowWidget


testSortTable :: forall t m. (MonadIO m, MonadIO (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m)
  => m (Dynamic t [()])
testSortTable = sortTable
  (def & sortTableConfig_theadDynAttrs .~ constDyn ("class" =: B4.theadDark))
  [(text "Fst", SortKey fst), (text "Snd", SortKey snd)]
  (constDyn [(1,"jeden") :: (Int,Text),(2,"dwa"),(3,"trzy")])
  (\d -> el "tr" $ do
      el "td" $ dynText $ showt . fst <$> d
      el "td" $ dynText $ showt . snd <$> d )


-- =============================== select ==================================


selectListWithDummy_ :: forall t m a v. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m () -> Dynamic t (NonEmpty v) -> (Dynamic t v -> m a) -> m (Dynamic t (Maybe v))
selectListWithDummy_ dummyWidget options optionWidget = snd <$> selectListWithDummy dummyWidget options optionWidget


selectListWithDummy :: forall t m a v. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => m () -> Dynamic t (NonEmpty v) -> (Dynamic t v -> m a) -> m (Event t (Event t (Maybe a)), Dynamic t (Maybe v))
selectListWithDummy dummyWidget options optionWidget =
  let
    optionMap = Map.fromList . zip [0..] . toList <$> options
  in
    selectMapWithDummy (-1 :: Int) dummyWidget optionMap (const optionWidget)


selectList_ :: forall t m a v. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => Dynamic t (NonEmpty v) -> (Dynamic t v -> m a) -> m (Dynamic t v)
selectList_ options optionWidget = snd <$> selectList options optionWidget

selectList :: forall t m a v. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
  => Dynamic t (NonEmpty v) -> (Dynamic t v -> m a) -> m (Event t a, Dynamic t v)
selectList options optionWidget =
  let
    optionMap = Map.fromList . zip [0..] . toList <$> options
  in
    selectMap (0 :: Int) optionMap (const optionWidget)

selectMapWithDummy_ :: forall t m a v k. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, Ord k)
  => k -> m () -> Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t (Maybe v))
selectMapWithDummy_ dummyKey dummyWidget options optionWidget = snd <$> selectMapWithDummy dummyKey dummyWidget options optionWidget

selectMapWithDummy :: forall t m a v k. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, Ord k)
  => k -> m () -> Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Event t (Event t (Maybe a)), Dynamic t (Maybe v))
selectMapWithDummy dummyKey dummyWidget options optionWidget =
  let
    optionWidget' :: k -> Dynamic t (Maybe v) -> m (Event t (Maybe a))
    optionWidget' k optionDyn = dyn $ ffor optionDyn $ \case
      Nothing -> Nothing <$ dummyWidget
      _ -> Just <$> optionWidget k (fromJust <$> optionDyn)
    options' :: Dynamic t (Map k (Maybe v))
    options' = ((<> (dummyKey =: Nothing)) . fmap Just <$> options)
  in
    selectMap dummyKey options' optionWidget'


selectMap_ :: forall t m a v k. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, Ord k)
  => k -> Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Dynamic t v)
selectMap_ initialSelectedKey options optionWidget = snd <$> selectMap initialSelectedKey options optionWidget

selectMap :: forall t m a v k. (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m, Ord k)
  => k -> Dynamic t (Map k v) -> (k -> Dynamic t v -> m a) -> m (Event t a, Dynamic t v)
selectMap initialSelectedKey options optionWidget = do
  (e, retWidgMapDyn) <- elAttr' "div" ("class" =: "reflex-select border rounded") $ listWithKey options
      (\k -> divClass "reflex-select-item border-top" . optionWidget k)

  let retWidgDyn = fromJust . Map.lookup initialSelectedKey <$> retWidgMapDyn
      retDyn = fromJust . Map.lookup initialSelectedKey <$> options
      ev = leftmost [domEvent Change e, domEvent Click e]

  return (tag (current retWidgDyn) ev, retDyn)





-- ============================== MODAL =====================================

data ModalPosition = ModalPositionTop | ModalPositionCenter

modalButton_ :: DomBuilder t m => Text -> m a -> m (Event t a)
modalButton_ modalId = modalButton modalId B4.btnDark ""

modalButton :: DomBuilder t m => Text -> Text -> Text -> m a -> m (Event t a)
modalButton modalId kls style w = do
  (e, a) <- elAttr' "button" attrs w
  return $ domEvent Click e $> a
  where
    attrs =
      "class" =: ("btn " <> kls) <>
      "style" =: style <>
      "type" =: "button" <>
      "data-toggle" =: "modal" <>
      "data-target" =: ("#" <> modalId)

modalHeader :: DomBuilder t m => Text -> Text -> Text -> Text -> Text -> m (Event t ())
modalHeader elName kls style title id' = do
  elAttr elName ("class" =: ("modal-title " <> kls) <> "style" =: style <> "id" =: id') $ text title
  (e, _) <- elAttr' "button" buttonAttrs $ elAttr "span" ("aria-hidden" =: "True") $ text "×"
  return $ domEvent Click e $> ()
    where
      buttonAttrs = "type" =: "button" <> "class" =: "close" <> "data-dismiss" =: "modal"


modalOneButtonFooter :: DomBuilder t m => Text -> m (Event t ())
modalOneButtonFooter title = do
  (e,_) <- elAttr' "button" attrs (text title)
  return $ domEvent Click e $> ()
    where
      attrs = "type" =: "button" <> "class" =: "btn btn-dark" <> "data-dismiss" =: "modal"

modalTwoButtonFooter :: DomBuilder t m => Text -> Text -> Bool -> m (Event t Bool)
modalTwoButtonFooter cancelTitle okTitle doesOkDismiss = do
  (e1, _) <- elAttr' "button"
    ("type" =: "button" <>
     "class" =: "btn btn-secondary" <>
     "data-dismiss" =: "modal") $
    text cancelTitle
  (e2, _) <- elAttr' "button"
     ("type" =: "button" <>
      "class" =: "btn btn-dark" <>
      if doesOkDismiss then "data-dismiss" =: "modal" else mempty) $
     text okTitle
  return $ leftmost [domEvent Click e1 $> False, domEvent Click e2 $> True]

modalSimple :: forall t m a. DomBuilder t m
 => Text
 -> ModalPosition
 -> Text
 -> Text
 -> Text
 -> Bool
 -> m a
 -> m (Event t a)
modalSimple id' position title cancelTitle okTitle doesOkDismiss bodyWidget = do
  (_,b,f) <- modalCustom id' position
              (modalHeader "h5" "" "" title)
              bodyWidget
              (modalTwoButtonFooter cancelTitle okTitle doesOkDismiss)
  return $ fforMaybe f (bool Nothing (Just b))


modalCustom :: forall t m a b c. DomBuilder t m
  => Text
  -> ModalPosition
  -> (Text -> m a)
  -> m b
  -> m c
  -> m (a,b,c)
modalCustom modalId position modalHeader' modalBody modalFooter = elAttr "div" mainDivAttrs $ elAttr "div" dialogDivAttrs $ divClass "modal-content" $ do
  a <- divClass "modal-header" (modalHeader' modalTitle)
  b <- divClass "modal-body" modalBody
  c <- divClass "modal-footer" modalFooter
  return (a,b,c)
    where
      modalTitle = modalId <> "Title"
      positionClass = case position of
        ModalPositionTop -> ""
        ModalPositionCenter -> "modal-dialog-centered"
      dialogDivAttrs = "class" =: ("modal-dialog " <> positionClass) <> "role" =: "document"
      mainDivAttrs =
        "class" =: "modal fade" <>
        "id" =: modalId <>
        "tabIndex" =: "-1" <>
        "role" =: "dialog" <>
        "aria-labelledby" =: modalTitle <>
        "aria-hidden" =: "true"

  -- <!-- Modal -->
-- <div class="modal fade" id="exampleModalCenter" tabindex="-1" role="dialog" aria-labelledby="exampleModalCenterTitle" aria-hidden="true">
--   <div class="modal-dialog modal-dialog-centered" role="document">
--     <div class="modal-content">
--       <div class="modal-header">
--         <h5 class="modal-title" id="exampleModalCenterTitle">Modal title</h5>
--         <button type="button" class="close" data-dismiss="modal" aria-label="Close">
--           <span aria-hidden="true">&times;</span>
--         </button>
--       </div>
--       <div class="modal-body">
--         ...
--       </div>
--       <div class="modal-footer">
--         <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>
--         <button type="button" class="btn btn-primary">Save changes</button>
--       </div>
--     </div>
--   </div>
-- </div>

-- | Create a dynamically-changing set of widgets, one of which is
-- can selected at any time.
selectViewListWithKeyMaybe
  :: forall t m k v a
   . (Adjustable t m, Ord k, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Maybe k)
  -- ^ Current selection key
  -> Dynamic t (Map k v)
  -- ^ Dynamic key/value map
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -- ^ Function to create a widget for a given key from Dynamic value
  -- and Dynamic Bool indicating if this widget is currently selected
  -> m (Event t (k, a))
  -- ^ Event that fires when any child's return Event fires.  Contains
  -- key of an arbitrary firing widget.
selectViewListWithKeyMaybe selection vals mkChild = do
  -- For good performance, this value must be shared across all children
  let selectionDemux = demux selection
  selectChild <- listWithKey vals $ \k v -> do
    let selected = demuxed selectionDemux (Just k)
    selectSelf <- mkChild k v selected
    return $ fmap ((,) k) selectSelf
  return $ switchPromptlyDyn $ leftmost . Map.elems <$> selectChild

selectMapWithKeyPrompt :: forall t m v k. (MonadJSM m, MonadIO (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m, Ord k)
 => m () -> (k -> Dynamic t v -> m ()) -> Dynamic t (Map k v) -> (k -> Dynamic t v -> m ())
 -> m (Dynamic t (Maybe v))
selectMapWithKeyPrompt prompt selectedWidget options optionWidget = do

  -- =============== WIDGET'S STATE EVENTS & DYNAMICs =================

  -- new events for avoiding cycles and more cleanliness
  (optionSelected, optionSelected_IOAction) <- newTriggerEvent
  (optionActive, optionActive_IOAction) <- newTriggerEvent
  (selectToggled, selectToggled_IOAction) <- newTriggerEvent
  (selectDeactivated, selectDeactivated_IOAction) <- newTriggerEvent

  -- currently selected item, if any
  selectedOption <- holdDyn Nothing optionSelected
  -- item currently active - will be selected on enter

  -- is select widget during selection process
  let computeActive tgl curr = case (curr, tgl) of
        (c, True) -> not c
        (_, False) -> False

  widgetActive <- foldDyn computeActive False (leftmost [False <$ selectDeactivated, True <$ selectToggled])

  activeOption <- holdDyn Nothing (leftmost [optionActive, ffilter id (updated widgetActive) $> Nothing])

  -- =============== DYNAMIC ATTRIBUTES =================

      -- make given attrs dynamic by adding class based on dynamic bool
  let addActiveClass activeDyn attrs = ffor activeDyn $ \case
        False -> attrs
        True -> Map.insertWith (<>) "class" "active " attrs

      widgetAttrs b = addActiveClass b ("class" =: "reflex-ui-select")
      promptAttrs b = addActiveClass b ("class" =: "reflex-ui-selected-item container justify-content-between border rounded")
      optionListAttrs b = addActiveClass b ("class" =: "reflex-ui-select-option-list shadow border rounded")
      optionAttrs b = addActiveClass b ("class" =: "reflex-ui-select-option")


  -- ========== WIDGET'S DOM STRUCTURE ROOT ===========

  -- == Widget dom root
  divDyn (widgetAttrs widgetActive) $ do

    -- == Selected item / prompt widget
    (promptItemEl,_) <- divDyn' (promptAttrs widgetActive)  $ do
      dyn_ $ ffor2 widgetActive selectedOption $ \active selected -> case (active, selected) of
        (True, _) -> prompt
        (False, Nothing) -> prompt
        (False, Just (k, vDyn)) -> selectedWidget k (constDyn vDyn)
      elClass "div" "arrow" $ return ()

    let promptClick = traceEventWith (const "promptclick") $ domEvent Click promptItemEl

    -- Perform toggle active on selected-item/prompt click
    performEvent_ $ liftIO . selectToggled_IOAction <$> promptClick
    -- and allow to deselect item when select widget is active
    performEvent_ $ liftIO . optionSelected_IOAction <$> gate (current widgetActive) (promptClick $> Nothing)

    -- == Selection option list & items

    -- Build wrapper for optionWidget that will respond to click and mouseover and return selected value
    -- second event argument will be True when option was actually Clicked
    let wrappedOptionWidget :: k -> Dynamic t v -> Dynamic t Bool ->  m (Event t (Maybe v, Bool))
        wrappedOptionWidget k valDyn isActive = do

          widg <- dyn $ ffor valDyn $ \v -> do
            (e, _) <- divDyn' (optionAttrs isActive) (optionWidget k valDyn)
            return $ leftmost [domEvent Click e $> (Just v, True), domEvent Mouseover e $> (Just v, False)]

          switchHold ((,False) . fmap snd <$> optionSelected) widg


    -- == Option list (with options)
    optionItemEvent <- divDyn (optionListAttrs widgetActive) $
      selectViewListWithKeyMaybe (fmap fst <$> activeOption) options wrappedOptionWidget

    let optionItemClicked = ffilter (snd . snd) optionItemEvent

    mouseClickEv <- gate ((&&) <$> current widgetActive <*> (isJust <$> current activeOption)) <$> wrappedBodyEvent Click (return ())


    -- make option active on both click and mouse over event
    performEvent_ $ liftIO . optionActive_IOAction . moveMaybe . over _2 fst <$> optionItemEvent
    -- select new option on mouse click
    performEvent_ $ liftIO . optionSelected_IOAction . moveMaybe . over _2 fst <$> optionItemClicked
    -- desactivate widget if there was a mouse click
    performEvent_ $ liftIO . selectDeactivated_IOAction <$> (mouseClickEv $> ())
    return ()

  -- =============== KEYBOARD CONTROL =================
  keyDownEv <- gate (current widgetActive) <$> wrappedBodyEvent Keydown (do
    ret <- JSDOM.uiKeyCode
    JSDOM.preventDefault
    return ret)

  let keyDownEnterEv = ffilter ((==) Enter . keyCodeLookup . fromIntegral) keyDownEv
      keyDownEscapeEnterEv = ffilter ((==) Escape . keyCodeLookup . fromIntegral) keyDownEv
      arrowKeyDown = ffilter (\k -> k == ArrowUp || k == ArrowDown) $ keyCodeLookup . fromIntegral <$> keyDownEv

  -- confirm active option on enter
  performEvent_ $ liftIO . optionSelected_IOAction <$> tag (current activeOption) keyDownEnterEv
  performEvent_ $ liftIO . selectDeactivated_IOAction <$> leftmost [keyDownEscapeEnterEv $> (), keyDownEnterEv $> ()]

  -- create event containing adjusted active item index
  let
    currentStatus = (,) <$> current options <*> current activeOption
    activeItemKeyboardAdjustedEv = ffor (attach currentStatus arrowKeyDown) $
        \((opts, active), key) -> case (active,key) of
          (Nothing, ArrowDown) -> -- "select first index"
            Just (head $ Map.toList opts)
          (curr@(Just (k,_)), ArrowDown) -> -- "select next index"
            let nextIndex = (Map.findIndex k opts + 1)
            in if nextIndex >= Map.size opts then curr
               else Just (Map.elemAt nextIndex opts)
          (Just (k,_), ArrowUp) -> -- "select previous index"
              let nextIndex = (Map.findIndex k opts - 1)
              in if nextIndex < 0 then Nothing
                 else Just (Map.elemAt nextIndex opts)
          (curr, _) -> curr

  -- change active item with arrows
  performEvent_ $ liftIO . optionActive_IOAction <$> activeItemKeyboardAdjustedEv

  return (fmap snd <$> selectedOption)

    where
      divDyn = elDynAttr "div"
      divDyn' = elDynAttr' "div"

      moveMaybe :: forall a b. (a, Maybe b) -> Maybe (a, b)
      moveMaybe (_, Nothing) = Nothing
      moveMaybe (a, Just b) = Just (a,b)


selectMapWithKeyPromptSimple :: forall t m v k. (MonadJSM m, MonadIO (Performable m), MonadFix m, TriggerEvent t m, PerformEvent t m, MonadHold t m, DomBuilder t m, PostBuild t m, Ord k)
 => Text -> (k -> Dynamic t v -> m ()) -> Dynamic t (Map k v) -> m (Dynamic t (Maybe v))
selectMapWithKeyPromptSimple promptText optionWidget options = selectMapWithKeyPrompt
  (elAttr "div" ("style" =: "margin: auto 0") $ text promptText)
  optionWidget
  options
  optionWidget
