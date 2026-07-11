module AgentDaemon.TmuxCloseSpec
    ( spec
    ) where

{- |
Module      : AgentDaemon.TmuxCloseSpec
Description : Live tmux proof for current-context close boundaries
Copyright   : (c) Paolo Veronelli, 2026
License     : MIT

Exercises close preparation and execution against uniquely named,
disposable tmux sessions.
-}

import AgentDaemon.Close
    ( CloseConsequence (..)
    , CloseScope (..)
    )
import AgentDaemon.Tmux
    ( PreparedTmuxClose
    , TmuxCloseFailure (..)
    , TmuxCloseResult (..)
    , executeCurrentClose
    , prepareCurrentClose
    )
import Control.Exception (bracket)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Unique (hashUnique, newUnique)
import System.Exit (ExitCode (..))
import System.Process
    ( callProcess
    , readProcess
    , readProcessWithExitCode
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )

spec :: Spec
spec = describe "current-only tmux close boundary" $ do
    it "removes only the prepared active pane and preserves its window" $
        withSession $ \sessionName -> do
            tmux ["split-window", "-d", "-t", target sessionName]
            preparedPane <- currentPane sessionName
            preparedWindow <- currentWindow sessionName
            beforeWindows <- windowIds sessionName
            prepared <- expectPrepared sessionName CloseCurrentPane
            result <- executeCurrentClose sessionName prepared
            afterPanes <- paneIds sessionName
            windowIds sessionName `shouldReturnSorted` beforeWindows
            afterPanes `shouldExclude` preparedPane
            currentWindow sessionName `shouldReturnText` preparedWindow
            result `shouldBe` Right (TmuxCloseResult PaneRemoved False)

    it "removes the current last-pane window and preserves a survivor" $
        withSession $ \sessionName -> do
            tmux ["new-window", "-d", "-t", target sessionName]
            preparedWindow <- currentWindow sessionName
            preparedPane <- currentPane sessionName
            prepared <- expectPrepared sessionName CloseCurrentPane
            result <- executeCurrentClose sessionName prepared
            windowIds sessionName >>= (`shouldExclude` preparedWindow)
            paneIds sessionName >>= (`shouldExclude` preparedPane)
            sessionExists sessionName >>= (`shouldBe` True)
            result `shouldBe` Right (TmuxCloseResult PaneAndWindowRemoved False)

    it "ends the session when closing its only pane" $
        withSession $ \sessionName -> do
            prepared <- expectPrepared sessionName CloseCurrentPane
            result <- executeCurrentClose sessionName prepared
            sessionExists sessionName >>= (`shouldBe` False)
            result `shouldBe` Right (TmuxCloseResult SessionEnded True)

    it "removes only the prepared current window and all of its panes" $
        withSession $ \sessionName -> do
            tmux ["split-window", "-d", "-t", target sessionName]
            preparedWindow <- currentWindow sessionName
            preparedPanes <- paneIds sessionName
            tmux ["new-window", "-d", "-t", target sessionName]
            prepared <- expectPrepared sessionName CloseCurrentWindow
            result <- executeCurrentClose sessionName prepared
            survivingWindows <- windowIds sessionName
            survivingPanes <- paneIds sessionName
            survivingWindows `shouldExclude` preparedWindow
            mapM_ (survivingPanes `shouldExclude`) preparedPanes
            sessionExists sessionName >>= (`shouldBe` True)
            result `shouldBe` Right (TmuxCloseResult WindowRemoved False)

    it "ends the session when closing its only window" $
        withSession $ \sessionName -> do
            prepared <- expectPrepared sessionName CloseCurrentWindow
            result <- executeCurrentClose sessionName prepared
            sessionExists sessionName >>= (`shouldBe` False)
            result `shouldBe` Right (TmuxCloseResult SessionEnded True)

    it "rejects a changed active pane and preserves every pane" $
        withSession $ \sessionName -> do
            tmux ["split-window", "-d", "-t", target sessionName]
            before <- paneIds sessionName
            prepared <- expectPrepared sessionName CloseCurrentPane
            otherPane <- inactivePane sessionName
            tmux ["select-pane", "-t", T.unpack otherPane]
            result <- executeCurrentClose sessionName prepared
            paneIds sessionName >>= (`shouldBe` before)
            result `shouldBe` Left TmuxCloseStaleCurrentContext

    it "rejects a changed active window and preserves every context" $
        withSession $ \sessionName -> do
            tmux ["new-window", "-d", "-t", target sessionName]
            beforeWindows <- windowIds sessionName
            beforePanes <- paneIds sessionName
            prepared <- expectPrepared sessionName CloseCurrentWindow
            otherWindow <- inactiveWindow sessionName
            tmux ["select-window", "-t", T.unpack otherWindow]
            result <- executeCurrentClose sessionName prepared
            windowIds sessionName >>= (`shouldBe` beforeWindows)
            paneIds sessionName >>= (`shouldBe` beforePanes)
            result `shouldBe` Left TmuxCloseStaleCurrentContext

    it "rejects a changed current-window pane count and closes nothing" $
        withSession $ \sessionName -> do
            prepared <- expectPrepared sessionName CloseCurrentPane
            tmux ["split-window", "-d", "-t", target sessionName]
            before <- paneIds sessionName
            result <- executeCurrentClose sessionName prepared
            paneIds sessionName >>= (`shouldBe` before)
            result `shouldBe` Left TmuxCloseStaleCurrentContext

    it "rejects a changed session window count and closes nothing" $
        withSession $ \sessionName -> do
            prepared <- expectPrepared sessionName CloseCurrentWindow
            tmux ["new-window", "-d", "-t", target sessionName]
            before <- windowIds sessionName
            result <- executeCurrentClose sessionName prepared
            windowIds sessionName >>= (`shouldBe` before)
            result `shouldBe` Left TmuxCloseStaleCurrentContext

    it "propagates the conditional false-branch sentinel exit" $
        withSession $ \sessionName -> do
            (falseExit, _, _) <-
                tmuxResult
                    [ "if-shell"
                    , "-F"
                    , "-t"
                    , target sessionName
                    , "0"
                    , "display-message -p boundary-true"
                    , "run-shell 'exit 71'"
                    ]
            (trueExit, _, _) <-
                tmuxResult
                    [ "if-shell"
                    , "-F"
                    , "-t"
                    , target sessionName
                    , "1"
                    , "display-message -p boundary-true"
                    , "run-shell 'exit 71'"
                    ]
            falseExit `shouldBe` ExitFailure 71
            trueExit `shouldBe` ExitSuccess

withSession :: (Text -> IO a) -> IO a
withSession action = do
    unique <- hashUnique <$> newUnique
    let sessionName = "agent-daemon-close-" <> T.pack (show unique)
    bracket
        (createDisposableSession sessionName)
        cleanupSession
        (const $ action sessionName)

createDisposableSession :: Text -> IO Text
createDisposableSession sessionName = do
    tmux
        [ "new-session"
        , "-d"
        , "-s"
        , T.unpack sessionName
        , "-n"
        , "prepared"
        , "sleep 60"
        ]
    pure sessionName

cleanupSession :: Text -> IO ()
cleanupSession sessionName = do
    _ <- tmuxResult ["kill-session", "-t", target sessionName]
    pure ()

expectPrepared :: Text -> CloseScope -> IO PreparedTmuxClose
expectPrepared sessionName scope = do
    result <- prepareCurrentClose sessionName scope
    case result of
        Right prepared -> pure prepared
        Left failure -> expectationFailure (show failure) >> error "unreachable"

currentPane :: Text -> IO Text
currentPane sessionName =
    tmuxText ["display-message", "-p", "-t", target sessionName, "#{pane_id}"]

currentWindow :: Text -> IO Text
currentWindow sessionName =
    tmuxText ["display-message", "-p", "-t", target sessionName, "#{window_id}"]

inactivePane :: Text -> IO Text
inactivePane sessionName =
    tmuxText ["list-panes", "-t", target sessionName, "-f", "#{==:#{pane_active},0}", "-F", "#{pane_id}"]

inactiveWindow :: Text -> IO Text
inactiveWindow sessionName =
    tmuxText ["list-windows", "-t", target sessionName, "-f", "#{==:#{window_active},0}", "-F", "#{window_id}"]

paneIds :: Text -> IO [Text]
paneIds sessionName =
    sort . T.lines <$> tmuxText ["list-panes", "-s", "-t", target sessionName, "-F", "#{pane_id}"]

windowIds :: Text -> IO [Text]
windowIds sessionName =
    sort . T.lines <$> tmuxText ["list-windows", "-t", target sessionName, "-F", "#{window_id}"]

sessionExists :: Text -> IO Bool
sessionExists sessionName = do
    (exitCode, _, _) <- tmuxResult ["has-session", "-t", target sessionName]
    pure $ exitCode == ExitSuccess

tmux :: [String] -> IO ()
tmux = callProcess "tmux"

tmuxText :: [String] -> IO Text
tmuxText args = T.strip . T.pack <$> readProcess "tmux" args ""

tmuxResult :: [String] -> IO (ExitCode, String, String)
tmuxResult args = readProcessWithExitCode "tmux" args ""

target :: Text -> String
target = T.unpack

shouldExclude :: (Eq a) => [a] -> a -> IO ()
values `shouldExclude` value = (value `elem` values) `shouldBe` False

shouldReturnSorted :: IO [Text] -> [Text] -> IO ()
action `shouldReturnSorted` expected = action >>= (`shouldBe` sort expected)

shouldReturnText :: IO Text -> Text -> IO ()
action `shouldReturnText` expected = action >>= (`shouldBe` expected)
