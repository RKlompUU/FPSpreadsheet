{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|	Module      :  Menu
	Copyright   :  (c) Daan Leijen 2003
	               (c) Shelarcy (shelarcy@gmail.com) 2006
	License     :  wxWindows

	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable

Defines Menus, toolbars, and statusbars.
    
The function 'menuPane' is used to create a menu
that can contain 'menuItem's. Menu items can contain event handlers
using ('on' 'command'), but they can also be set, using the 'menu'
function, on a frame or (mdi) window so that the menu command is handled
in the context of the active window instead of the context of the
entire application. 

> do frame  <- frame    [text := "Demo"]
>    file   <- menuPane [text := "&File"]
>    mclose <- menuItem file [text := "&Close\tCtrl+C", help := "Close the document"] 
>    set frame [menuBar          := [file] 
>              ,on (menu mclose) := ...] 

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Menu
    ( -- * Menu
      -- ** Menu containers
      MenuBar, Menu, menuBar, menuPopup, menuPane, menuHelp
    , menuRes, menuBarLoadRes
    -- ** Menu events
    , menu, menuId
      -- ** Menu items
    , MenuItem, menuItem, menuQuit, menuAbout, menuItemEx
    , menuItemOnCommandRes, menuLine, menuSub, menuRadioItem
    -- * Tool bar
    , ToolBar, toolBar, toolBarEx
    , ToolBarItem, toolMenu, toolMenuFromBitmap, toolItem, toolControl, tool
    -- * Status bar
    , StatusField, statusBar, statusField, statusWidth
    -- * Deprecated
    , menuList, menubar, statusbar
    ) where

import Data.Char( toUpper )
import Data.List( partition, intersperse )
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr( nullPtr )
import Graphics.UI.WXCore hiding (Event)

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events


{--------------------------------------------------------------------------------
  Menubar
--------------------------------------------------------------------------------}
{-# DEPRECATED menubar "Use menuBar instead" #-}
-- | /Deprecated/: use 'menuBar'.
menubar :: WriteAttr (Frame a) [Menu ()]
menubar
  = menuBar

-- | Set the menu bar of a frame.
menuBar :: WriteAttr (Frame a) [Menu ()]
menuBar
  = writeAttr "menubar" setter
  where
    setter frame menus
      = do mb <- menuBarCreate wxMB_DOCKABLE
           mapM_ (append mb) menus
           frameSetMenuBar frame mb
           -- set delayed menu handlers on the frame
           mapM_ (evtHandlerSetAndResetMenuCommands frame) menus
           -- work around menu bug in wxMac 2.5.1
           vis <- windowIsShown frame
           if (vis && wxToolkit == WxMac && (div wxVersion 100) >= 25)
            then do windowHide frame
                    windowShow frame
                    return ()
            else return ()

    append mb menu
      = do title <- menuGetTitle menu
           menuSetTitle menu ""
           menuBarAppend mb menu title

-- | Retrieve a menu bar instance which has been constructed by loading
--   a resource file for a given top level window.
menuBarLoadRes :: Window a -> FilePath -> String -> IO (MenuBar ())
menuBarLoadRes parent rc name =
    do
      res <- xmlResourceCreateFromFile rc wxXRC_USE_LOCALE
      m   <- xmlResourceLoadMenuBar res parent name
      return m

-- | Show a popup menu for a certain window.
menuPopup :: Menu b -> Point -> Window a -> IO ()
menuPopup menu pt parent
  = do windowPopupMenu parent menu pt
       return ()

{--------------------------------------------------------------------------------
  Menu
--------------------------------------------------------------------------------}
{-# DEPRECATED menuList "Use menuPane instead" #-}
-- | /Deprecated/: use 'menuPane'.
menuList :: [Prop (Menu ())] -> IO (Menu ())
menuList 
  = menuPane 

-- | Create a new menu with a certain title (corresponds with 'text' attribute).
--
-- * Instances: 'Textual'
--
menuPane :: [Prop (Menu ())] -> IO (Menu ())
menuPane props
  = do m <- menuCreate "" wxMENU_TEAROFF
       set m props
       return m

-- | Append a /help/ menu item (@"&Help"@). On some platforms,
-- the /help/ menu is handled specially
menuHelp :: [Prop (Menu ())] -> IO (Menu ())
menuHelp props
  = menuPane ([text := "&Help"] ++ props)

-- | Complete the construction of a menu which has been loaded
--   from a resource file.
-- | Get a menu by name from a menu loaded from a resource file, 
--   given the frame which owns the menu. You
--   can directly set properties on the item as part of the call, which
--   enables simple connection of event handlers (e.g. on command).
menuRes :: Window a -> String -> [Prop (Menu ())] -> IO (Menu ())
menuRes parent menu_name props =
    do
      menu <- xmlResourceGetMenu parent menu_name
      set menu props
      return menu

instance Textual (Menu a) where
  text
    = newAttr "text" menuGetTitle menuSetTitle

{--------------------------------------------------------------------------------
  Menu items
--------------------------------------------------------------------------------}
-- | Create a submenu item.
menuSub :: Menu b -> Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuSub parent menu props
  = do id <- idCreate
       label <- case (findProperty text "" props) of 
                  Just (txt,_) -> return txt
                  Nothing      -> do title <- menuGetTitle menu
                                     if (null title) 
                                      then return "<empty>"
                                      else return title                  
       menuSetTitle menu ""           -- remove title on submenus
       menuAppendSub parent id label menu ""
       menuPropagateEvtHandlers menu  -- move the evtHandlers to the parent
       item <- menuFindItem parent id
       set item props
       return item

-- | Add a menu seperator.
menuLine :: Menu a -> IO ()
menuLine menu
  = menuAppendSeparator menu


-- | Append a menu item. The label can contain
-- menu accellerators by using an ampersand. It can also contain keyboard accellerators
-- after a tab (@'\\t'@) character.
--
-- > menuItem menu [text := "&Open\tCtrl+O", help := "Opens an existing document"] 
--
-- You can create a checkable menu item by setting 'checkable' to 'True' in the
-- properties of a menu item.
--
-- Note: on GTK, it is required to set the 'text' attribute immediately at creation time.
--
-- * Events: 'menu', 'menuId'
--
-- * Instances: 'Textual', 'Able', 'Help', 'Checkable', 'Identity', 'Commanding'.
--
menuItem :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItem menu props
  = do let kind = case (findProperty checkable False props) of
                    Just (True,_) -> wxITEM_CHECK
                    _             -> wxITEM_NORMAL
       menuItemKind menu kind props                     

-- | Append a radio menu item. These items are 'checkable' by default.
-- A sequence of radio menu items form automatically a group. 
-- A different kind of menu item, like  a 'menuLine', terminates the group.
-- Note: one sometimes has to set the first selected radio item 
-- specifically after setting the "menubar" property, or otherwise the
-- radio item bullet is not displayed on windows.
-- See 'menuItem' for other properties of menu radio items.
menuRadioItem :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuRadioItem menu props
  = menuItemKind menu wxITEM_RADIO ([checked := True] ++ props)

menuItemKind menu kind props
  = do id <- idCreate
       let label = case (findProperty text "" props) of 
                     Nothing      -> "<empty>"
                     Just (txt,_) -> txt
       menuItemEx menu id label kind props
       


-- | Append an /about/ menu item (@"&About..."@). On some platforms,
-- the /about/ menu is handled specially.
--
-- * Events: 'menu', 'menuId'
--
-- * Instances: 'Textual', 'Able', 'Help', 'Checkable', 'Identity', 'Commanding'.
--
menuAbout :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuAbout menu props
  = menuItemId menu wxID_ABOUT "&About..." props

-- | Append an /quit/ menu item (@"&Quit\tCtrl+Q"@). On some platforms,
-- the /quit/ menu is handled specially
--
-- * Events: 'menu', 'menuId'
--
-- * Instances: 'Textual', 'Able', 'Help', 'Checkable', 'Identity', 'Commanding'.
--
menuQuit :: Menu a -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuQuit menu props
  = menuItemId menu wxID_EXIT "&Quit\tCtrl+Q" props

-- | Append a menu item with a specific id and label.
--
-- * Events: 'menu', 'menuId'
--
-- * Instances: 'Textual', 'Able', 'Help', 'Checkable', 'Identity', 'Commanding'.
--
menuItemId :: Menu a -> Id -> String -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItemId menu id label props
  = menuItemEx menu id label wxITEM_NORMAL props

-- | Append a menu item with a specific id, label, and kind (like 'wxITEM_CHECK').
--
-- * Events: 'menu', 'menuId'
--
-- * Instances: 'Textual', 'Able', 'Help', 'Checkable', 'Identity', 'Commanding'.
--
menuItemEx :: Menu a -> Id -> String -> Int -> [Prop (MenuItem ())] -> IO (MenuItem ())
menuItemEx menu id label kind props
  = do if (kind == wxITEM_RADIO)
        then menuAppendRadioItem menu id label ""
        else menuAppend menu id label "" (kind == wxITEM_CHECK)
       item <- menuFindItem menu id
       set item props
       return item

instance Able (MenuItem a) where
  enabled = newAttr "enabled" menuItemIsEnabled menuItemEnable

instance Textual (MenuItem a) where
  text
    = reflectiveAttr "text" menuItemGetItemLabel menuItemSetItemLabel

instance Help (MenuItem a) where
  help  = newAttr "help" menuItemGetHelp menuItemSetHelp

instance Checkable (MenuItem a) where
  checkable = reflectiveAttr "checkable" menuItemIsCheckable (\m c -> menuItemSetCheckable m c)
  checked   = newAttr "checked"   menuItemIsChecked menuItemCheck

instance Identity (MenuItem a) where
  identity  = newAttr "identity" menuItemGetId menuItemSetId

{--------------------------------------------------------------------------------
  Events
--------------------------------------------------------------------------------}
-- | React to menu events.
menu :: MenuItem a -> Event (Window w) (IO ())
menu item
  = let id = unsafePerformIO (get item identity)
    in  menuId id

-- | React to a menu event based on identity.
menuId :: Id -> Event (Window w) (IO ())
menuId id
  = newEvent "menu" (\w -> evtHandlerGetOnMenuCommand w id) (\w h -> evtHandlerOnMenuCommand w id h)
              
{--------------------------------------------------------------------------------
  Menu commands:

  Ok, we would like to set command handlers in two ways:
  1) As an "on command" on the menu item itself. 
  2) With an "on (menu xxx)" on a window. 

  Unfortunately, wxWidgets does not support method (1) for menus that are
  part of a menubar and assumes they are set on using (2) on the associated
  frame. We can't tell whether a menu is part of a menubar or popup menu until
  the user sets it. Thus we both set the event handlers always directly on the
  top level menu (this is good enough for popup menus) and we maintain 
  a list of menu item id's and associated event handler as client data on the
  top level menu. When the menu is set as part of a menu bar, we install the
  handlers on the associated frame.
--------------------------------------------------------------------------------}
instance Commanding (MenuItem a) where
  command
    = newEvent "command" menuItemGetOnCommand menuItemOnCommand

menuItemGetOnCommand :: MenuItem a -> IO (IO ())
menuItemGetOnCommand item 
  = do id      <- get item identity
       topmenu <- menuItemGetTopMenu item
       evtHandlerGetOnMenuCommand topmenu id

menuItemOnCommand :: MenuItem a -> IO () -> IO ()
menuItemOnCommand item io
  = do id      <- get item identity
       topmenu <- menuItemGetTopMenu item
       -- always set it on the menu itself (has only effect on popup menus)
       evtHandlerOnMenuCommand topmenu id io
       -- update the Haskell event handler list for delayed frame installation
       menuUpdateEvtHandlers topmenu (insert id io)
       -- and set it directly on the frame if already instantiated. 
       frame   <- menuGetFrame topmenu
       when (not (objectIsNull frame)) (evtHandlerOnMenuCommand frame id io)
  where
    insert key val []         = [(key,val)]
    insert key val ((k,v):xs) | key == k  = (key,val):xs
                              | otherwise = (k,v):insert key val xs

-- | When setting event handlers on menu items which have been loaded from
--   XRC resource files, properties cannot be used as the menu item
--   instances are opaque to wxHaskell.
--
--   This function offers a convenient way to attach menu item event
--   handlers, given the identity of the window which owns the menu containing
--   the menu item, and the name of the menu item

menuItemOnCommandRes :: Window a -> String -> IO () -> IO ()
menuItemOnCommandRes win item_name handler =
    do
      res     <- xmlResourceGet
      item_id <- xmlResourceGetXRCID res item_name
      evtHandlerOnMenuCommand win item_id handler

-- Propagate the (delayed) event handlers of a submenu to the parent menu.
-- This is necessary for event handlers set on menu items in a submenu that
-- was not yet assigned to a parent menu.
menuPropagateEvtHandlers :: Menu a -> IO ()
menuPropagateEvtHandlers menu
  = do parent   <- menuGetTopMenu menu
       handlers <- menuGetEvtHandlers menu
       menuSetEvtHandlers menu []
       menuSetEvtHandlers parent handlers

-- Get associated frame of a menu in a menubar. Returns NULL for popup and sub menus.
menuGetFrame :: Menu a -> IO (Frame ())
menuGetFrame menu
  = do menubar <- menuGetMenuBar menu
       if (objectIsNull menubar) 
        then return objectNull
        else menuBarGetFrame menubar

-- Get top level menu of a menu item (never null)
menuItemGetTopMenu :: MenuItem a -> IO (Menu ())
menuItemGetTopMenu item
  = do menu <- menuItemGetMenu item
       menuGetTopMenu menu

-- Get the top level menu of a possible sub menu 
menuGetTopMenu :: Menu a -> IO (Menu ())
menuGetTopMenu menu
  = do parent <- menuGetParent menu
       if (objectIsNull parent)
        then return (downcastMenu menu)
        else menuGetTopMenu parent

-- Set all menu event handlers on a certain window (EvtHandler)
evtHandlerSetAndResetMenuCommands :: EvtHandler a -> Menu b -> IO ()
evtHandlerSetAndResetMenuCommands evtHandler menu
  = do handlers <- menuGetEvtHandlers menu
       menuSetEvtHandlers menu []
       mapM_ (\(id,io) -> evtHandlerOnMenuCommand evtHandler id io) handlers

-- Update the menu event handler list.
menuUpdateEvtHandlers menu f
  = do hs <- menuGetEvtHandlers menu
       menuSetEvtHandlers menu (f hs)

menuGetEvtHandlers :: Menu a -> IO [(Id,IO ())]
menuGetEvtHandlers menu 
  = do mbHandlers <- unsafeEvtHandlerGetClientData menu
       case mbHandlers of
         Nothing -> return []
         Just hs -> return hs

menuSetEvtHandlers :: Menu a -> [(Id,IO ())] -> IO ()
menuSetEvtHandlers menu hs
  = evtHandlerSetClientData menu (return ()) hs 


{--------------------------------------------------------------------------------
  Toolbar
--------------------------------------------------------------------------------}
-- | Create a toolbar window with a divider and text labels.
-- Normally, you can use 'toolMenu' to add tools in the toolbar
-- that behave like normal menu items.
--
-- >  tbar   <- toolBar f []
-- >  toolMenu tbar open  "Open"  "open.png"  []
-- >  toolMenu tbar about "About" "about.png" []
--
toolBar :: Frame a -> [Prop (ToolBar ())] -> IO (ToolBar ())
toolBar parent props
  = toolBarEx parent True True props

-- | Create a toolbar window. The second argument specifies whether text labels
-- should be shown, and the third argument whether a divider line is present
-- above the toolbar.
toolBarEx :: Frame a -> Bool -> Bool -> [Prop (ToolBar ())] -> IO (ToolBar ())
toolBarEx parent showText showDivider props
  = do let style = ( wxTB_DOCKABLE .+. wxTB_FLAT
                   .+. (if showText then wxTB_TEXT else 0)
                   .+. (if showDivider then 0 else wxTB_NODIVIDER)
                   )
       t <- toolBarCreate parent idAny rectNull style
       frameSetToolBar parent t
       {-
       t <- frameCreateToolBar parent style 
       -}
       set t props
       return t

-- | A tool in a toolbar.
--
-- * Events: 'tool'
--
-- * Instances: 'Able', 'Help', 'Tipped', 'Checkable', 'Identity', 'Commanding'.
--
data ToolBarItem  = ToolBarItem (ToolBar ()) Id Bool

instance Able ToolBarItem  where
  enabled 
    = newAttr "enabled" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolEnabled toolbar id

      setter (ToolBarItem toolbar id isToggle) enable
        = toolBarEnableTool toolbar id enable
         

instance Tipped ToolBarItem where
  tooltip 
    = newAttr "tooltip" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolShortHelp toolbar id

      setter (ToolBarItem toolbar id isToggle) txt
        = toolBarSetToolShortHelp toolbar id txt
         
instance Help ToolBarItem  where
  help  
    = newAttr "help" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolLongHelp toolbar id

      setter (ToolBarItem toolbar id isToggle) txt
        = toolBarSetToolLongHelp toolbar id txt
         

instance Checkable ToolBarItem where
  checkable 
    = readAttr "checkable" getter
    where
      getter (ToolBarItem toolbar id isToggle)
        = return isToggle

  checked   
    = newAttr "checked"  getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = toolBarGetToolState toolbar id

      setter (ToolBarItem toolbar id isToggle) toggle
        = toolBarToggleTool toolbar id toggle
         

instance Identity ToolBarItem where
  identity  
    = readAttr "identity" getter
    where
      getter (ToolBarItem toolbar id isToggle)
        = return id


instance Commanding ToolBarItem where
  command
    = newEvent "command" getter setter
    where
      getter (ToolBarItem toolbar id isToggle)
        = evtHandlerGetOnMenuCommand toolbar id

      setter (ToolBarItem toolbar id isToggle) io
        = evtHandlerOnMenuCommand toolbar id io

-- | React on tool event. (normally handled by 'menu' though, so only use this
-- for orphan toolbar items).
tool :: ToolBarItem -> Event (Window w) (IO ())
tool (ToolBarItem toolbar id isToggle)
  = newEvent "tool" getter setter
  where
    getter w
      = evtHandlerGetOnMenuCommand w id
    setter w io
      = evtHandlerOnMenuCommand w id io

-- | Create a tool bar item based on a menu. Takes a a relevant menu
-- item, a label and an image file (bmp, png, gif, ico, etc.) as arguments. The image
-- file is normally 16x15 pixels.
-- The toolbar item will fire the relevant menu items just as if the menu has been selected.
-- Checkable menus will give a checkable toolbar item. Beware though that checkable tools
-- normally require a specific @on command@ handler to keep them synchronised with the 
-- corresponding menu item.
toolMenu :: ToolBar a -> MenuItem a -> String -> FilePath -> [Prop ToolBarItem] -> IO ToolBarItem
toolMenu toolbar menuitem label bitmapPath props
  = withBitmapFromFile bitmapPath $ \bitmap ->
      toolMenuFromBitmap toolbar menuitem label bitmap props

-- | This is a generalized version of 'toolMenu' function. You can specify 'Bitmap' that is
-- loaded from any other place instead of using 'FilePath' directly.
toolMenuFromBitmap :: ToolBar a -> MenuItem a -> String -> Bitmap b -> [Prop ToolBarItem] -> IO ToolBarItem
toolMenuFromBitmap toolbar menuitem label bitmap props
  = do isToggle <- get menuitem checkable
       id       <- get menuitem identity
       lhelp    <- get menuitem help
       shelp    <- get menuitem help
       toolBarAddTool2 toolbar id label bitmap nullBitmap 
                       (if isToggle then wxITEM_CHECK else wxITEM_NORMAL)
                       shelp lhelp
       let t = ToolBarItem (downcastToolBar toolbar) id isToggle
       set t props
       toolBarRealize toolbar
       return t

-- | Create an /orphan/ toolbar item that is unassociated with a menu. Takes a 
-- label, a flag that is 'True' when the item is 'checkable' and a path to an image
-- (bmp, png, gif, ico, etc.) as arguments.
toolItem :: ToolBar a -> String -> Bool -> FilePath -> [Prop ToolBarItem] -> IO ToolBarItem
toolItem toolbar label isCheckable bitmapPath props
  = withBitmapFromFile bitmapPath $ \bitmap ->
    do id <- idCreate
       toolBarAddTool2 toolbar id label bitmap nullBitmap 
                            (if isCheckable then wxITEM_CHECK else wxITEM_NORMAL)
                            "" ""
       let t = ToolBarItem (downcastToolBar toolbar) id isCheckable
       set t props
       toolBarRealize toolbar
       return t

-- | Add an arbitrary control to a toolbar (typically a 'ComboBox'). The control
-- must be created with the toolbar as the parent.
toolControl :: ToolBar a -> Control b -> IO ()
toolControl toolbar control
  = do toolBarAddControl toolbar control
       return ()
   

{--------------------------------------------------------------------------------
  Statusbar
--------------------------------------------------------------------------------}
-- | A field in a status bar.
--
-- * Instances: 'Textual'
-- 
data StatusField  = SF (Var Int) (Var (StatusBar ())) (Var Int) (Var String)

-- | The status width attribute determines the width of a status bar field.
-- A negative width makes the field stretchable. The width than determines
-- the amount of stretch in relation to other fields. The default
-- status width is @-1@, ie. all fields stretch evenly.
--
-- Here is an example of a status bar
-- with three fields, where the last field is 50 pixels wide, the first takes
-- 66% of the remaining space and the second field 33%.
--
-- > field1 <- statusField [statusWidth := -2]
-- > field2 <- statusField [text := "hi"]
-- > field3 <- statusField [statusWidth := 50]
-- > set frame [statusBar := [field1,field2,field3]] 
--
statusWidth :: Attr StatusField Int
statusWidth 
  = newAttr "statusWidth" getter setter
  where
    getter (SF vwidth _ _ _)
      = varGet vwidth

    setter (SF vwidth _ _ _) w
      = varSet vwidth w

-- | Create a status field.
statusField :: [Prop StatusField] -> IO StatusField
statusField props
  = do vwidth<- varCreate (-1)
       vsbar <- varCreate objectNull
       vidx  <- varCreate (-1)
       vtext <- varCreate ""
       let sf = SF vwidth vsbar vidx vtext
       set sf props
       return sf


instance Textual StatusField where
  text
    = newAttr "text" get set
    where
      get (SF _ vsbar vidx vtext)
        = varGet vtext

      set (SF _ vsbar vidx vtext)  text
        = do varSet vtext text
             idx <- varGet vidx
             if (idx >= 0)
              then do sbar <- varGet vsbar
                      statusBarSetStatusText sbar text idx
              else return ()



{-# DEPRECATED statusbar "Use statusBar instead" #-}
-- | /Deprecated/: use 'statusBar'. 
statusbar :: WriteAttr (Frame a) [StatusField]
statusbar
  = statusBar

-- | Specify the statusbar of a frame.
statusBar :: WriteAttr (Frame a) [StatusField]
statusBar
  = writeAttr "statusbar" set
  where
    set f fields
      = do ws <- mapM (\field -> get field statusWidth) fields
           sb <- statusBarCreateFields f ws
           mapM_ (setsb sb) (zip [0..] fields )

    setsb sb (idx,SF _ vsbar vidx vtext)
      = do varSet vsbar sb
           varSet vidx idx
           text <- varGet vtext
           statusBarSetStatusText sb text idx -- initialize
