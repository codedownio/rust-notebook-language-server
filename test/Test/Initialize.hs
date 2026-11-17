{-# LANGUAGE TypeOperators #-}

module Test.Initialize (tests, main) where

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Language.LSP.Notebook
import Language.LSP.NotebookLanguageServer.Config
import Language.LSP.NotebookLanguageServer.Transform.Util
import Language.LSP.Protocol.Types
import Test.Sandwich
import Transform.Config


tests :: TopSpec
tests = describe "Initialize" $ do
  it "points the workspace folders at the shadow crate" $ do
    transformed <- runInitialize (initializeParams (Just (InL [WorkspaceFolder (filePathToUri "/home/notebooks") "notebooks"])))
    transformed `shouldBe` initializeParams (Just (InL [WorkspaceFolder (filePathToUri shadowDir) "shadow"]))

  it "leaves out workspace folders the client didn't send" $ do
    runInitialize (initializeParams Nothing) >>= (`shouldBe` initializeParams Nothing)

shadowDir :: FilePath
shadowDir = "/tmp/rnls/shadow"

runInitialize :: MonadIO m => InitializeParams -> m InitializeParams
runInitialize params = liftIO $ do
  state :: TransformerState RustNotebookTransformer <- newTransformerState
  runNoLoggingT $ runReaderT (notebookConfigTransformInitialize (rustNotebookConfig shadowDir 0) params) state

initializeParams :: Maybe ([WorkspaceFolder] |? Null) -> InitializeParams
initializeParams folders = InitializeParams {
  _workDoneToken = Nothing
  , _processId = InR Null
  , _clientInfo = Nothing
  , _locale = Nothing
  , _rootPath = Just (InL (T.pack shadowDir))
  , _rootUri = InL (filePathToUri shadowDir)
  , _capabilities = ClientCapabilities Nothing Nothing Nothing Nothing Nothing Nothing
  , _initializationOptions = Nothing
  , _trace = Nothing
  , _workspaceFolders = folders
  }

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions tests
