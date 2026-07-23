{- | Reconcile the @icarium.toml@ category vocabulary against the DB.

Above the repo layer (it reads a 'CategoriesConfig') and below the command
layer (it neither parses flags nor prints): `category sync` and `init` both
drive it and render the report their own way.
-}
module Icarium.Categories (
    SyncReport (..),
    syncCategories,
) where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)

import Icarium.Config (CategoriesConfig (..))
import Icarium.Repo.Category qualified as RC
import Icarium.Types (Category (..), CategoryAxis (..))

data SyncReport = SyncReport
    { srInserted :: [(CategoryAxis, Text)]
    , srOrphans :: [Category]
    , srPruned :: [Category]
    , srBlocking :: [(Category, [Text])]
    }
    deriving (Show)

{- | Reconcile toml categories into the DB.

Inserts categories present in toml but absent from DB.
prune=False: records DB-only categories as orphans (caller should exit non-zero).
prune=True: deletes all DB-only categories when none are in use; if any are
in use, records them as blocking and performs no deletions.
-}
syncCategories :: Connection -> CategoriesConfig -> Bool -> IO SyncReport
syncCategories conn cfg prune = do
    dbCats <- RC.listCategories conn Nothing
    let tomlCats = tomlCategoryList cfg
        toIns = toInsertList dbCats tomlCats
        orphans = dbOnlyList dbCats tomlCats
    forM_ toIns $ uncurry (RC.insertCategory conn)
    (pruned, blocking, remaining) <-
        if not prune
            then pure ([], [], orphans)
            else do
                results <- forM orphans $ \cat -> do
                    usages <- RC.categoryNodeUsages conn (categoryId cat)
                    pure (cat, usages)
                let blockingRs = [(cat, ids) | (cat, ids) <- results, not (null ids)]
                    unusedCats = [cat | (cat, []) <- results]
                if null blockingRs
                    then do
                        forM_ unusedCats $ \cat ->
                            RC.deleteCategory conn (categoryAxis cat) (categoryName cat)
                        pure (unusedCats, [], [])
                    else pure ([], blockingRs, [])
    pure
        SyncReport
            { srInserted = toIns
            , srOrphans = remaining
            , srPruned = pruned
            , srBlocking = blocking
            }

tomlCategoryList :: CategoriesConfig -> [(CategoryAxis, Text)]
tomlCategoryList cfg =
    [(Domain, n) | n <- catDomains cfg]
        ++ [(Discipline, n) | n <- catDisciplines cfg]
        ++ [(Kind, n) | n <- catKinds cfg]

toInsertList :: [Category] -> [(CategoryAxis, Text)] -> [(CategoryAxis, Text)]
toInsertList dbCats =
    filter (\(ax, n) -> not (any (\c -> categoryAxis c == ax && categoryName c == n) dbCats))

dbOnlyList :: [Category] -> [(CategoryAxis, Text)] -> [Category]
dbOnlyList dbCats tomlCats =
    filter (\c -> (categoryAxis c, categoryName c) `notElem` tomlCats) dbCats
