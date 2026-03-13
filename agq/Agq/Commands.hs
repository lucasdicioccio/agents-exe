module Agq.Commands (
    cmdInit,
    cmdInitQueue,
    cmdInitGithub,
    cmdAdd,
    cmdPull,
    cmdPromote,
    cmdStatus,
    cmdNext,
    cmdProcess,
    cmdExec,
    cmdMergePRs,
    cmdClean,
    cmdRecover,
    cmdRetry,
) where

import Agq.Config (AgqConfig (..), AgqLabels (..), defaultConfig)
import Agq.DB
import Agq.Run
import Agq.Schedule

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, guard, unless, void, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as Text
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process

-- ---------------------------------------------------------------------------
-- cmdInit  (bootstrap — no config or DB required)
-- ---------------------------------------------------------------------------

{- | Write a template agq.json to the given path using the built-in defaults.
Does nothing if the file already exists.
-}
cmdInit :: FilePath -> IO ()
cmdInit cfgPath = do
    exists <- doesFileExist cfgPath
    if exists
        then putStrLn $ cfgPath <> " already exists — not overwriting."
        else do
            LBS.writeFile cfgPath (AesonPretty.encodePretty defaultConfig)
            putStrLn $ "Created " <> cfgPath
            putStrLn "Edit it to suit your project, then run:"
            putStrLn "  agq init-queue    # create directories and SQLite DB"
            putStrLn "  agq init-github   # create GitHub labels"

-- ---------------------------------------------------------------------------
-- cmdInitQueue
-- ---------------------------------------------------------------------------

cmdInitQueue :: AgqConfig -> Connection -> IO ()
cmdInitQueue cfg conn = do
    createDirectoryIfMissing True (taskDir cfg)
    createDirectoryIfMissing True (sessionsDir cfg)
    initDB conn
    ensureGitignore
    putStrLn $ "Queue initialised at " <> queueDb cfg

{- | Ensure .gitignore contains the noisy agent log patterns so that
'git add -A' inside a worktree never commits them.
-}
ensureGitignore :: IO ()
ensureGitignore = do
    let path = ".gitignore"
        required = ["agents-logfile", "logs.json", "conv.*.json"]
    exists <- doesFileExist path
    current <- if exists then Text.readFile path else return ""
    let missing = filter (\p -> not (p `elem` Text.lines current)) required
    unless (null missing) $ do
        let additions =
                "\n# agents-exe verbose logs (added by agq init-queue)\n"
                    <> Text.unlines missing
        Text.appendFile path additions
        putStrLn $
            ".gitignore: added "
                <> show (length missing)
                <> " missing pattern(s): "
                <> Text.unpack (Text.intercalate ", " missing)

-- ---------------------------------------------------------------------------
-- cmdAdd
-- ---------------------------------------------------------------------------

cmdAdd :: AgqConfig -> Connection -> Text -> Text -> [Text] -> [Text] -> Int -> IO ()
cmdAdd cfg conn label name deps extraTags tries = do
    createDirectoryIfMissing True (taskDir cfg)
    let fname = taskDir cfg </> Text.unpack name <> ".md"
    exists <- doesFileExist fname
    unless exists $ writeFile fname ""
    content <- readFile fname
    when (null content) $ do
        let editor = "vim"
        callProcess editor [fname]
    base <- detectBaseBranch cfg
    let task =
            Task
                { taskId = 0
                , taskName = name
                , taskLabel = label
                , taskSource = SourceLocal
                , taskStatus = Pending
                , taskInstructionFile = fname
                , taskBaseBranch = base
                , taskIsFinal = False
                , taskTriesRemaining = tries
                }
    insertTask conn task deps (label : extraTags)
    putStrLn $ "Added task: " <> Text.unpack name <> " (label=" <> Text.unpack label <> ", tries=" <> show tries <> ")"

-- ---------------------------------------------------------------------------
-- cmdPull
-- ---------------------------------------------------------------------------

cmdPull :: AgqConfig -> Connection -> IO ()
cmdPull cfg conn = do
    createDirectoryIfMissing True (taskDir cfg)
    (ec, out) <-
        runGh
            [ "issue"
            , "list"
            , "--label"
            , Text.unpack (labelToBeTaken (labels cfg))
            , "--author"
            , Text.unpack (githubUsername cfg)
            , "--json"
            , "number,labels"
            ]
    when (ec /= ExitSuccess) $ putStrLn "Warning: gh issue list failed."
    let issues = case Aeson.decode (lbsFromText out) of
            Just (Aeson.Array arr) -> reverse (foldr (:) [] arr)
            _ -> []
    when (null issues) $ putStrLn "No tasks found in GitHub."
    forM_ issues $ \issueVal -> do
        let num = extractInt issueVal "number"
            ghLabels = extractLabels issueVal
            hasTbt = labelToBeTaken (labels cfg) `elem` ghLabels
            label = findProjectLabel (Map.keys (projects cfg)) ghLabels
        case num of
            Nothing -> putStrLn "Skipping malformed issue."
            Just n
                | not hasTbt ->
                    putStrLn $ "Skipping issue #" <> show n <> ": missing label '" <> Text.unpack (labelToBeTaken (labels cfg)) <> "'."
                | Just lbl <- label -> importGhIssue cfg conn n lbl
                | otherwise ->
                    putStrLn $ "Skipping issue #" <> show n <> ": no project label."

importGhIssue :: AgqConfig -> Connection -> Int -> Text -> IO ()
importGhIssue cfg conn n lbl = do
    let name = "gh-" <> Text.pack (show n)
        fname = taskDir cfg </> Text.unpack name <> ".md"
    exists <- doesFileExist fname
    unless exists $ writeFile fname ""
    content <- readFile fname
    when (null content) $ do
        (_, body) <-
            runGh
                [ "issue"
                , "view"
                , show n
                , "--json"
                , "title,body"
                , "--jq"
                , "\"# \" + .title + \"\\n\\n\" + .body"
                ]
        Text.writeFile fname (Text.strip body <> "\n\n---\nFrom #" <> Text.pack (show n) <> "\n")
    freshContent <- Text.readFile fname
    detectedBase <- detectBaseBranch cfg
    let base = fromMaybe detectedBase (parseHeader "Base-branch:" freshContent)
        isFin = parseHeader "Final:" freshContent == Just "true"
        tries = maybe (defaultTries cfg) (read . Text.unpack) (parseHeader "Tries:" freshContent)
        task =
            Task
                { taskId = 0
                , taskName = name
                , taskLabel = lbl
                , taskSource = SourceGithub n
                , taskStatus = Pending
                , taskInstructionFile = fname
                , taskBaseBranch = base
                , taskIsFinal = isFin
                , taskTriesRemaining = tries
                }
        deps = parseDeps freshContent
    insertTask conn task deps [lbl]
    void $ runGh ["issue", "edit", show n, "--remove-label", Text.unpack (labelToBeTaken (labels cfg)), "--add-label", Text.unpack (labelTaken (labels cfg))]
    putStrLn $ "Enqueued GitHub issue #" <> show n <> " as " <> Text.unpack lbl

-- ---------------------------------------------------------------------------
-- cmdPromote
-- ---------------------------------------------------------------------------

cmdPromote :: AgqConfig -> IO ()
cmdPromote cfg = do
    (ec, out) <-
        runGh
            [ "issue"
            , "list"
            , "--label"
            , Text.unpack (labelWait (labels cfg))
            , "--author"
            , Text.unpack (githubUsername cfg)
            , "--json"
            , "number,title"
            ]
    when (ec /= ExitSuccess) $ putStrLn "Warning: gh issue list failed."
    let issues = case Aeson.decode (lbsFromText out) of
            Just (Aeson.Array arr) -> foldr (:) [] arr
            _ -> []
    when (null issues) $ putStrLn $ "No issues in '" <> Text.unpack (labelWait (labels cfg)) <> "'."
    forM_ issues $ \issueVal ->
        case extractInt issueVal "number" of
            Nothing -> return ()
            Just n -> do
                (_, body) <- runGh ["issue", "view", show n, "--json", "body", "--jq", ".body"]
                let deps = parseDeps body
                allSat <- checkDepsSatisfied cfg deps
                if allSat
                    then do
                        putStrLn $ "Promoting issue #" <> show n <> " to " <> Text.unpack (labelToBeTaken (labels cfg))
                        void $ runGh ["issue", "edit", show n, "--remove-label", Text.unpack (labelWait (labels cfg)), "--add-label", Text.unpack (labelToBeTaken (labels cfg))]
                    else
                        putStrLn $ "Issue #" <> show n <> " still waiting on deps."

checkDepsSatisfied :: AgqConfig -> [Text] -> IO Bool
checkDepsSatisfied cfg deps = do
    results <- mapM checkDep deps
    return (and results)
  where
    -- Block promotion only when a dep is explicitly OPEN and not labelled done-in-branch.
    -- Unknown state (not found, error, deleted, transferred) is treated as done.
    checkDep dep = do
        let depStr = Text.unpack dep
        (ec1, out1) <- runGh ["issue", "view", depStr, "--json", "state,labels"]
        if ec1 /= ExitSuccess
            then return True -- not found / error: treat as done
            else do
                let issueVal = fromMaybe Aeson.Null (Aeson.decode (lbsFromText out1))
                    state = maybe "" Text.strip (extractText issueVal "state")
                    lbls = extractLabels issueVal
                if state /= "OPEN"
                    then return True -- closed
                    else
                        if labelDoneInBranch (labels cfg) `elem` lbls
                            then return True -- done in a feature branch
                            else return False -- still open, blocks promotion

-- ---------------------------------------------------------------------------
-- cmdStatus
-- ---------------------------------------------------------------------------

cmdStatus :: AgqConfig -> Connection -> IO ()
cmdStatus _cfg conn = do
    tasks <- listTasks conn
    putStrLn $ padR 6 "ID" <> padR 30 "NAME" <> padR 12 "LABEL" <> padR 10 "STATUS" <> padR 6 "TRIES" <> padR 20 "DEPS" <> "TAGS"
    putStrLn (replicate 96 '-')
    forM_ tasks $ \(t, deps, tags) ->
        putStrLn $
            padR 6 (show (taskId t))
                <> padR 30 (Text.unpack (taskName t))
                <> padR 12 (Text.unpack (taskLabel t))
                <> padR 10 (Text.unpack (taskStatusText (taskStatus t)))
                <> padR 6 (show (taskTriesRemaining t))
                <> padR 20 (Text.unpack (Text.intercalate "," deps))
                <> Text.unpack (Text.intercalate "," tags)
    putStrLn ""
    locks <- query_ conn "SELECT tag, task_name, acquired_at FROM locks" :: IO [(Text, Text, Int)]
    if null locks
        then putStrLn "No active locks."
        else do
            putStrLn "Active locks:"
            forM_ locks $ \(tag, tname, at) ->
                putStrLn $ "  [" <> Text.unpack tag <> "] held by " <> Text.unpack tname <> " since " <> show at
  where
    padR n s = let s' = take n s in s' <> replicate (n - length s') ' '

-- ---------------------------------------------------------------------------
-- cmdNext
-- ---------------------------------------------------------------------------

{- | Show which tasks are ready to be taken and explain why blocked tasks
are not ready. Excludes "done" tasks from the blocked list.
-}
cmdNext :: AgqConfig -> Connection -> IO ()
cmdNext _cfg conn = do
    tasks <- listTasksWithDetails conn
    let (ready, blocked) = partitionTasks tasks
    if null ready
        then putStrLn "No tasks are ready to be taken."
        else do
            putStrLn "Tasks ready to be taken (would be started by 'agq process'):"
            putStrLn ""
            forM_ ready $ \(t, deps, tags, _incompleteDeps, _blockingLocks) -> do
                putStrLn $ "  " <> Text.unpack (taskName t)
                putStrLn $ "    Label: " <> Text.unpack (taskLabel t)
                putStrLn $ "    Tags:  " <> Text.unpack (Text.intercalate ", " tags)
                unless (null deps) $ do
                    putStrLn $ "    Deps:  " <> Text.unpack (Text.intercalate ", " deps) <> " (all satisfied)"
                putStrLn $ "    Tries remaining: " <> show (taskTriesRemaining t)
                putStrLn ""
    unless (null blocked) $ do
        putStrLn "Tasks that cannot be taken:"
        putStrLn ""
        forM_ blocked $ \(t, deps, tags, incompleteDeps, blockingLocks) -> do
            let reason = explainBlockage t incompleteDeps blockingLocks
            putStrLn $ "  " <> Text.unpack (taskName t) <> " — " <> reason
            putStrLn $ "    Label: " <> Text.unpack (taskLabel t)
            putStrLn $ "    Tags:  " <> Text.unpack (Text.intercalate ", " tags)
            unless (null deps) $ do
                let depStatus = map (\d -> if d `elem` incompleteDeps then d <> " (pending)" else d <> " (done)") deps
                putStrLn $ "    Deps:  " <> Text.unpack (Text.intercalate ", " depStatus)
            putStrLn $ "    Tries remaining: " <> show (taskTriesRemaining t)
            putStrLn ""
  where
    -- Partition tasks into ready and blocked (excluding done tasks)
    partitionTasks tasks = foldr partition ([], []) tasks
      where
        partition item@(t, _deps, _tags, incompleteDeps, blockingLocks) (ready, blocked) =
            case taskStatus t of
                Done -> (ready, blocked) -- Exclude done tasks from both lists
                _ ->
                    if isReady t incompleteDeps blockingLocks
                        then (item : ready, blocked)
                        else (ready, item : blocked)

    -- A task is ready if it's pending, has tries remaining, no incomplete deps, and no blocking locks
    isReady t incompleteDeps blockingLocks =
        taskStatus t == Pending
            && taskTriesRemaining t > 0
            && null incompleteDeps
            && null blockingLocks

    -- Explain why a task is blocked
    explainBlockage t incompleteDeps blockingLocks =
        case taskStatus t of
            Running -> "Already running"
            Failed -> "Failed (use 'agq retry' to reset)"
            Done -> "Done" -- Should not appear in blocked list
            Pending ->
                if taskTriesRemaining t <= 0
                    then "No tries remaining (use 'agq retry' to restore)"
                    else
                        if not (null blockingLocks)
                            then "Blocked by lock(s) on tag(s): " <> Text.unpack (Text.intercalate ", " blockingLocks)
                            else
                                if not (null incompleteDeps)
                                    then "Waiting on incomplete dependency: " <> Text.unpack (Text.intercalate ", " incompleteDeps)
                                    else "Unknown reason"

-- ---------------------------------------------------------------------------
-- cmdProcess
-- ---------------------------------------------------------------------------

cmdProcess :: AgqConfig -> Connection -> Bool -> Bool -> IO ()
cmdProcess cfg conn parallel loop = do
    n <- recoverStaleLocks conn (lockStaleSeconds cfg)
    when (n > 0) $ putStrLn $ "Recovered " <> show n <> " stale task(s)."
    go
  where
    go = do
        mname <- claimNextTask conn
        case mname of
            Nothing -> do
                cnt <- countPendingRunning conn
                if cnt == 0 && not loop
                    then putStrLn "Queue empty. Exiting."
                    else do
                        putStrLn $ "Waiting (" <> show cnt <> " task(s) pending/running)..."
                        threadDelay (pollSeconds cfg * 1000000)
                        go
            Just name ->
                if parallel
                    then do
                        void $ forkIO (cmdExec cfg conn name)
                        go
                    else do
                        cmdExec cfg conn name
                        go

-- ---------------------------------------------------------------------------
-- cmdExec
-- ---------------------------------------------------------------------------

cmdExec :: AgqConfig -> Connection -> Text -> IO ()
cmdExec cfg conn name = do
    mt <- getTaskByName conn name
    case mt of
        Nothing -> putStrLn $ "Task not found: " <> Text.unpack name
        Just t -> execTask cfg conn t

execTask :: AgqConfig -> Connection -> Task -> IO ()
execTask cfg conn t = do
    let nameStr = Text.unpack (taskName t)
        base = Text.unpack (taskBaseBranch t)
        lbl = taskLabel t
        agentCfg =
            Text.unpack $
                fromMaybe
                    (fromMaybe "task-agent.json" (Map.lookup "default" (agents cfg)))
                    (Map.lookup lbl (agents cfg))
        projDir = Text.unpack $ fromMaybe "." (Map.lookup lbl (projects cfg))
        mHook =
            Text.unpack
                <$> ( case Map.lookup lbl (hooks cfg) of
                        Just h -> Just h
                        Nothing -> Map.lookup "default" (hooks cfg)
                    )
        target = if taskIsFinal t then "main" else base

    -- instrFile must be absolute: it is stored relative to the repo root but
    -- agents-exe runs from inside the worktree subdirectory.
    repoRoot <- getCurrentDirectory
    let instrFile = repoRoot </> taskInstructionFile t

    -- 1. Fetch base branch, creating it on origin from the default branch if absent
    ecFetch <- runGit ["fetch", "origin", base]
    when (ecFetch /= ExitSuccess) $ do
        putStrLn $ "Base branch '" <> base <> "' not found on origin, creating it..."
        defaultBranch <- detectBaseBranch cfg
        void $ runGit ["fetch", "origin", Text.unpack defaultBranch]
        void $ runGit ["push", "origin", "refs/remotes/origin/" <> Text.unpack defaultBranch <> ":refs/heads/" <> base]
        void $ runGit ["fetch", "origin", base]

    -- 2. Set up worktree (remove old one first if present)
    void $ runGit ["worktree", "remove", "--force", nameStr]
    void $ runGit ["worktree", "prune"]
    ecWt <- runGit ["worktree", "add", nameStr, "origin/" <> base]
    when (ecWt /= ExitSuccess) $ do
        releaseLock conn (taskName t) Failed (Just "worktree creation failed")
        fail $ "Failed to create worktree " <> nameStr

    let worktreeProj = nameStr </> projDir
        -- Session files live inside the worktree so that git add -A commits them
        -- as part of the work item, mirroring sqq-agent.sh's behaviour.
        sessDir = repoRoot </> worktreeProj </> sessionsDir cfg
        sessFile = sessDir </> nameStr <> ".session.json"
        sessMd = sessDir </> nameStr <> ".session.md"

    createDirectoryIfMissing True sessDir

    -- 3. Optional prepare hook
    mHookAbs <- case mHook of
        Nothing -> return Nothing
        Just h -> do
            let absH = worktreeProj </> h
            exists <- doesFileExist absH
            -- return the relative hook location as it will run in the worktreeProj
            return (if exists then Just h else Nothing)
    case mHookAbs of
        Nothing -> return ()
        Just h -> void $ runWithCwd worktreeProj h ["prepare", Text.unpack lbl, nameStr, instrFile]

    -- 4. Run agent with inner attempt loop.
    -- Each attempt calls agents-exe with the same session file so it resumes
    -- where the previous attempt left off.  The number of attempts is bounded
    -- by agentAttempts from the config (default 1).
    putStrLn $ "[agq] Running agent for task '" <> nameStr <> "'"
    putStrLn $ "[agq]   agent    : " <> agentCfg
    putStrLn $ "[agq]   instr    : " <> instrFile
    putStrLn $ "[agq]   session  : " <> sessFile
    putStrLn $ "[agq]   attempts : " <> show (agentAttempts cfg)

    let agentArgs =
            [ "--agent-file"
            , agentCfg
            , "run"
            , "--session-file"
            , sessFile
            , "-f"
            , instrFile
            ]
        runAttempt attempt = do
            putStrLn $
                "[agq] Attempt "
                    <> show attempt
                    <> "/"
                    <> show (agentAttempts cfg)
                    <> " for task '"
                    <> nameStr
                    <> "'"
            runWithCwdBoth worktreeProj "agents-exe" agentArgs

        attemptLoop attempt = do
            (ec, out, err) <- runAttempt attempt
            if ec == ExitSuccess
                then return (ec, out, err)
                else
                    if attempt < agentAttempts cfg
                        then do
                            putStrLn $ "[agq] Attempt " <> show attempt <> " failed, retrying..."
                            attemptLoop (attempt + 1)
                        else return (ec, out, err)

    (ecAgent, commitMsg, agentErr) <- attemptLoop 1
    putStrLn $
        "[agq] Agent finished for task '"
            <> nameStr
            <> "' — "
            <> if ecAgent == ExitSuccess then "success" else "FAILED"

    -- Bail out when all attempts are exhausted
    when (ecAgent /= ExitSuccess) $ do
        -- Write full stderr to disk next to the session files
        let errFile = sessDir </> nameStr <> ".err"
        Text.writeFile errFile agentErr
        putStrLn $ "[agq] Stderr log written to " <> errFile
        let errSnippet = Text.unlines . reverse . take 100 . reverse . Text.lines $ agentErr
            errMsg = "agents-exe failed for task `" <> Text.pack nameStr <> "`\n\n```\n" <> errSnippet <> "```"
        -- For GitHub-sourced tasks, post the failure snippet as an issue comment
        case taskSource t of
            SourceGithub n -> void $ runGh ["issue", "comment", show n, "--body", Text.unpack errMsg]
            SourceLocal -> return ()
        releaseLock conn (taskName t) Failed (Just "agents-exe returned non-zero")

    -- Only continue if agent succeeded
    when (ecAgent == ExitSuccess) $ do
        let commit =
                if Text.null (Text.strip commitMsg)
                    then "Update via automation (" <> nameStr <> ")"
                    else Text.unpack (Text.strip commitMsg)

        -- 5. session-print → write sibling .md so it gets committed with the work
        putStrLn $ "[agq] Printing session for task '" <> nameStr <> "' -> " <> sessMd
        (_, sessionMd) <- captureCmd "agents-exe" ["session-print", sessFile]
        Text.writeFile sessMd sessionMd

        -- 6. Commit and push
        -- Branch name includes a short SHA and the tries_remaining counter so that
        -- successive retry attempts never collide with each other on the remote.
        -- e.g. gh-132-c7823.2  (tries_remaining=2 after this attempt was claimed)
        (_, shaOut) <- captureCmd "git" ["-C", nameStr, "rev-parse", "--short", "HEAD"]
        let shortSha = Text.unpack (Text.strip shaOut)
            branchName = nameStr <> "-" <> shortSha <> "." <> show (taskTriesRemaining t)
        void $ runGit ["-C", nameStr, "checkout", "-b", branchName]
        (_, statusOut) <- captureCmd "git" ["-C", nameStr, "status", "--porcelain"]
        unless (Text.null (Text.strip statusOut)) $ do
            void $ runGit ["-C", nameStr, "add", "-A"]
            void $ runGit ["-C", nameStr, "commit", "--no-verify", "-m", commit]

            -- static-checks hook: runs after agent commit, before push
            staticCheckOutput <- case mHookAbs of
                Nothing -> return ""
                Just h -> do
                    putStrLn $ "[agq] Running static-checks for task '" <> nameStr <> "'"
                    (_, scOut, scErr) <- runWithCwdBoth worktreeProj h ["static-check", Text.unpack lbl, nameStr, instrFile]
                    (_, scStatus) <- captureCmd "git" ["-C", nameStr, "status", "--porcelain"]
                    unless (Text.null (Text.strip scStatus)) $ do
                        putStrLn $ "[agq] static-checks modified files, committing..."
                        void $ runGit ["-C", nameStr, "add", "-A"]
                        void $ runGit ["-C", nameStr, "commit", "--no-verify", "-m", "Run static-checks for " <> nameStr]
                    return (scOut <> scErr)

            void $ runGit ["-C", nameStr, "push", "-u", "origin", branchName]

            let prTitle = takeWhile (/= '\n') commit
                closesLine = case taskSource t of
                    SourceGithub n -> "\n\nCloses #" <> show n <> "."
                    SourceLocal -> ""
                staticChecksSection =
                    let trimmed = Text.strip staticCheckOutput
                     in if Text.null trimmed
                            then ""
                            else "\n\n## Static checks\n\n```\n" <> Text.unpack trimmed <> "\n```"
                prBody = commit <> closesLine <> staticChecksSection
            (_, prUrl, _) <-
                captureCmdBoth
                    "gh"
                    [ "pr"
                    , "create"
                    , "--base"
                    , target
                    , "--head"
                    , branchName
                    , "--title"
                    , prTitle
                    , "--body"
                    , prBody
                    , "--label"
                    , Text.unpack (labelAgentPr (labels cfg))
                    ]

            case mHookAbs of
                Nothing -> return ()
                Just h -> do
                    (_, checkOut, checkErr) <- runWithCwdBoth worktreeProj h ["check", Text.unpack lbl, nameStr, instrFile]
                    let checkOutput = Text.strip (checkOut <> checkErr)
                        prUrlStr = Text.unpack (Text.strip prUrl)
                    unless (Text.null checkOutput || null prUrlStr) $
                        void $
                            runCmd "gh" ["pr", "comment", prUrlStr, "--body", Text.unpack checkOutput]

        releaseLock conn (taskName t) Done Nothing

-- ---------------------------------------------------------------------------
-- cmdMergePRs
-- ---------------------------------------------------------------------------

cmdMergePRs :: AgqConfig -> IO ()
cmdMergePRs cfg = do
    (_, defOut) <- runGh ["repo", "view", "--json", "defaultBranchRef", "--jq", ".defaultBranchRef.name"]
    let def = Text.strip defOut
    (_, out) <- runGh ["pr", "list", "--label", Text.unpack (labelAgentPr (labels cfg)), "--json", "number,baseRefName,title"]
    let prs = case Aeson.decode (lbsFromText out) of
            Just (Aeson.Array arr) -> foldr (:) [] arr
            _ -> []
    when (null prs) $ putStrLn $ "No PRs with label " <> Text.unpack (labelAgentPr (labels cfg)) <> "."
    forM_ prs $ \prVal ->
        case (extractInt prVal "number", extractText prVal "baseRefName") of
            (Just n, Just b) ->
                if b /= def
                    then do
                        putStrLn $ "Merging PR #" <> show n <> " (base=" <> Text.unpack b <> ")"
                        void $ runCmd "gh" ["pr", "merge", show n, "--merge", "--auto"]
                        labelClosedIssues cfg n
                    else putStrLn $ "Skipping PR #" <> show n <> ": targets default branch."
            _ -> return ()

{- | Fetch the PR body, parse "Closes/Fixes/Resolves #N" references, and add
labelDoneInBranch to each referenced issue so that promote() treats them as
satisfied even though GitHub won't auto-close them (PR targets a non-default branch).
-}
labelClosedIssues :: AgqConfig -> Int -> IO ()
labelClosedIssues cfg prNum = do
    (ec, body) <- runGh ["pr", "view", show prNum, "--json", "body", "--jq", ".body"]
    when (ec == ExitSuccess) $ do
        let closed = parseClosesRefs body
        forM_ closed $ \issueNum -> do
            putStrLn $ "  Labelling issue #" <> show issueNum <> " as " <> Text.unpack (labelDoneInBranch (labels cfg))
            void $ runGh ["issue", "edit", show issueNum, "--add-label", Text.unpack (labelDoneInBranch (labels cfg))]

-- | Extract issue numbers from GitHub closing keywords: Closes/Fixes/Resolves #N
parseClosesRefs :: Text -> [Int]
parseClosesRefs body = mapMaybe extractNum candidates
  where
    candidates = do
        line <- Text.lines body
        let low = Text.toLower line
        guard $ any (`Text.isInfixOf` low) ["closes", "fixes", "resolves"]
        word <- Text.words line
        guard $ "#" `Text.isPrefixOf` word
        return word
    extractNum w =
        let digits = Text.takeWhile isDigit (Text.drop 1 w)
         in if Text.null digits
                then Nothing
                else Just (read (Text.unpack digits))

-- ---------------------------------------------------------------------------
-- cmdClean
-- ---------------------------------------------------------------------------

cmdClean :: AgqConfig -> Bool -> Bool -> IO ()
cmdClean cfg doIt force = do
    (_, wtOut) <- captureCmd "git" ["worktree", "list", "--porcelain"]
    (_, rootOut) <- captureCmd "git" ["rev-parse", "--show-toplevel"]
    let root = Text.strip rootOut
        wts = parseWorktrees wtOut
        nonMain = filter (\p -> Text.strip p /= root) wts
    toClean <-
        myFilterM
            ( \wt -> do
                let nm = takeBaseName (Text.unpack wt)
                    md = sessionsDir cfg </> nm <> ".session.md"
                doesFileExist md
            )
            nonMain
    if null toClean
        then putStrLn "No worktrees with completed sessions."
        else
            if not doIt
                then do
                    putStrLn "=== PREVIEW (use --do-it to execute) ==="
                    mapM_ (putStrLn . ("  " <>) . Text.unpack) toClean
                else do
                    putStrLn "=== EXECUTING CLEANUP ==="
                    forM_ toClean $ \wt -> do
                        let nm = takeBaseName (Text.unpack wt)
                        putStrLn $ "Removing worktree: " <> nm
                        if force
                            then void $ runGit ["worktree", "remove", "--force", nm]
                            else void $ runGit ["worktree", "remove", nm]
                    void $ runGit ["worktree", "prune"]
                    forM_ toClean $ \wt -> do
                        let nm = takeBaseName (Text.unpack wt)
                        void $ runGit ["branch", "-D", nm]

parseWorktrees :: Text -> [Text]
parseWorktrees txt =
    mapMaybe parseLine (Text.lines txt)
  where
    parseLine l
        | "worktree " `Text.isPrefixOf` l = Just (Text.drop (Text.length "worktree ") l)
        | otherwise = Nothing

myFilterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
myFilterM _ [] = return []
myFilterM p (x : xs) = do
    b <- p x
    rest <- myFilterM p xs
    return (if b then x : rest else rest)

-- ---------------------------------------------------------------------------
-- cmdRecover
-- ---------------------------------------------------------------------------

cmdRecover :: AgqConfig -> Connection -> IO ()
cmdRecover cfg conn = do
    n <- recoverStaleLocks conn (lockStaleSeconds cfg)
    putStrLn $ "Recovered " <> show n <> " stale task(s)."

-- ---------------------------------------------------------------------------
-- cmdRetry
-- ---------------------------------------------------------------------------

{- | Reset a failed (or stuck running) task back to pending so it will be
picked up again by the next 'process' cycle. The attempt counter is NOT
reset — it continues to accumulate across retries.
-}
cmdRetry :: AgqConfig -> Connection -> Text -> Int -> IO ()
cmdRetry cfg conn name tries = do
    let t = if tries <= 0 then defaultTries cfg else tries
    ok <- retryTask conn name t
    if ok
        then putStrLn $ "Task '" <> Text.unpack name <> "' reset to pending (tries_remaining=" <> show t <> ")."
        else putStrLn $ "Task '" <> Text.unpack name <> "' not found or not in a retryable state (must be failed or running)."

-- ---------------------------------------------------------------------------
-- cmdInitLabels
-- ---------------------------------------------------------------------------

-- Each label has a fixed colour and description; --force makes gh idempotent
-- (creates the label if absent, updates it if already present).
workflowLabelDefs :: AgqLabels -> [(Text, String, String)]
workflowLabelDefs lbls =
    [ (labelToBeTaken lbls, "0075ca", "Task is ready to be picked up by an agent")
    , (labelTaken lbls, "e4e669", "Task has been claimed and is being worked on")
    , (labelWait lbls, "d93f0b", "Task is waiting for its dependencies to complete")
    , (labelAgentPr lbls, "6f42c1", "Pull request was created automatically by an agent")
    , (labelDoneInBranch lbls, "1d76db", "Issue was addressed by a PR merged into a feature branch")
    ]

createLabel :: Text -> String -> String -> IO ()
createLabel name color desc = do
    (ec, _) <-
        runGh
            [ "label"
            , "create"
            , Text.unpack name
            , "--color"
            , color
            , "--description"
            , desc
            , "--force"
            ]
    if ec == ExitSuccess
        then putStrLn $ "  ok  " <> Text.unpack name
        else putStrLn $ "  ERR " <> Text.unpack name

cmdInitGithub :: AgqConfig -> IO ()
cmdInitGithub cfg = do
    putStrLn "Workflow labels:"
    forM_ (workflowLabelDefs (labels cfg)) $ \(name, color, desc) ->
        createLabel name color desc
    putStrLn "Project labels:"
    forM_ (Map.keys (projects cfg)) $ \proj ->
        createLabel proj "bfd4f2" ("Project: " <> Text.unpack proj)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lbsFromText :: Text -> LBS.ByteString
lbsFromText = LBS.fromStrict . TE.encodeUtf8

extractInt :: Aeson.Value -> Text -> Maybe Int
extractInt val key = case val of
    Aeson.Object obj ->
        case KeyMap.lookup (Key.fromText key) obj of
            Just (Aeson.Number n) ->
                case (floatingOrInteger n :: Either Double Int) of
                    Right i -> Just i
                    Left d -> Just (round d)
            _ -> Nothing
    _ -> Nothing

extractText :: Aeson.Value -> Text -> Maybe Text
extractText val key = case val of
    Aeson.Object obj ->
        case KeyMap.lookup (Key.fromText key) obj of
            Just (Aeson.String s) -> Just s
            _ -> Nothing
    _ -> Nothing

extractLabels :: Aeson.Value -> [Text]
extractLabels val = case val of
    Aeson.Object obj ->
        case KeyMap.lookup (Key.fromText "labels") obj of
            Just (Aeson.Array arr) ->
                mapMaybe (\v -> extractText v "name") (foldr (:) [] arr)
            _ -> []
    _ -> []

findProjectLabel :: [Text] -> [Text] -> Maybe Text
findProjectLabel keys ghLabels =
    case filter (`elem` ghLabels) keys of
        (k : _) -> Just k
        [] -> Nothing

parseHeader :: Text -> Text -> Maybe Text
parseHeader header content =
    case filter (Text.isPrefixOf (Text.toLower header) . Text.toLower) (Text.lines content) of
        [] -> Nothing
        (l : _) -> Just (Text.strip (Text.drop (Text.length header) l))

parseDeps :: Text -> [Text]
parseDeps content =
    case parseHeader "Depends-on:" content of
        Nothing -> []
        Just val ->
            map (Text.replace "#" "" . Text.strip) $
                Text.splitOn "," val

{- | Detect the remote default branch via git, falling back to the configured value.
Runs: git symbolic-ref refs/remotes/origin/HEAD --short
which returns e.g. "origin/main" or "origin/master"; we strip the "origin/" prefix.
-}
detectBaseBranch :: AgqConfig -> IO Text
detectBaseBranch cfg = do
    (ec, out) <- captureCmd "git" ["symbolic-ref", "refs/remotes/origin/HEAD", "--short"]
    if ec == ExitSuccess
        then
            let raw = Text.strip out
             in return $
                    if "origin/" `Text.isPrefixOf` raw
                        then Text.drop (Text.length "origin/") raw
                        else raw
        else return (baseBranch cfg)

