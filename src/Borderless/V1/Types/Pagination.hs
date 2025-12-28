module Borderless.V1.Types.Pagination
    ( -- * Pagination Types
      PageResponse(..)
    , emptyPage
    ) where

import Borderless.Prelude

-- | Paginated response from the API
data PageResponse a = PageResponse
    { _pageResponseHasMore :: Bool
    , _pageResponseData_   :: Vector a
    } deriving stock (Eq, Generic, Show)

instance FromJSON a => FromJSON (PageResponse a) where
    parseJSON = withObject "PageResponse" $ \v -> PageResponse
        <$> v .: "hasMore"
        <*> v .: "data"

instance ToJSON a => ToJSON (PageResponse a) where
    toJSON PageResponse{..} = object
        [ "hasMore" .= _pageResponseHasMore
        , "data"    .= _pageResponseData_
        ]

-- | Empty page response
emptyPage :: PageResponse a
emptyPage = PageResponse
    { _pageResponseHasMore = False
    , _pageResponseData_   = mempty
    }
