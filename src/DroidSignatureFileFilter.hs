module DroidSignatureFileFilter
( FilterOption(..)
, filterSigFile
, listFileFormats
) where

import Text.XML.Light
import Data.List ((\\), nub, intercalate)
import Data.Maybe (fromMaybe)

-- | Configuration options for the 'filterSigFile' function.
data FilterOption
    -- | Include file formats that are supertypes of the selected formats.
    = WithSupertypes
    -- | Include file formats that are subtypes of the selected formats.
    | WithSubtypes
    deriving (Eq)

-- | Filter an XML string representing a DROID signature file based on a list
-- of PUIDs. Only those entries (file format and internal signature
-- descriptions) that are necessary to identify files with one of the given
-- PUIDs will occur in the resulting XML string. With an empty list of PUIDs,
-- the input XML string is returned unmodified.
filterSigFile :: [FilterOption] -- ^ Configuration options.
              -> [String]       -- ^ PUIDs.
              -> String         -- ^ Signature file XML content.
              -> String         -- ^ Filtered signature file XML content.
filterSigFile _    []    xml = xml
filterSigFile _    _     ""  = ""
filterSigFile opts puids xml = case parseXMLDoc xml of
    Nothing -> error "Failed to parse signature file."
    Just e  -> showTopElement $ replaceChildren e [isc', ffc']
        where
            isc  = head $ findChildren (mkNm "InternalSignatureCollection") e
            ffc  = head $ findChildren (mkNm "FileFormatCollection") e
            ffs  = filterChildrenByAttr "PUID" puids ffc
            ffc' = replaceChildren ffc $ nub $ ffs
                   ++ (if WithSupertypes `elem` opts
                        then supertypes ffc ffs
                        else [])
                   ++ (if WithSubtypes `elem` opts
                        then subtypes ffc ffs
                        else [])
            ids  = concatMap sigIDs $ findChildren (mkNm "FileFormat") ffc'
            isc' = replaceChildren isc $ filterChildrenByAttr "ID" ids isc

-- | List the file formats that occur in an XML string representing a DROID
-- signature file. Each file format is represented by a string of this form:
--
-- > PUID <tab> name <tab> version
listFileFormats :: String   -- ^ Signature file XML content.
                -> [String] -- ^ File formats.
listFileFormats ""  = []
listFileFormats xml = case parseXMLDoc xml of
    Nothing -> error "Failed to parse signature file."
    Just e  -> map showFileFormat ffs
        where
            ffc = head $ findChildren (mkNm "FileFormatCollection") e
            ffs = findChildren (mkNm "FileFormat") ffc
            showFileFormat ff = intercalate "\t"
                [ fromMaybe "" $ findAttr (unqual "PUID") ff
                , fromMaybe "" $ findAttr (unqual "Name") ff
                , fromMaybe "" $ findAttr (unqual "Version") ff
                ]

-- | Replace all content of an element by appending a list of elements as
-- children.
replaceChildren :: Element -> [Element] -> Element
replaceChildren e cs = e { elContent = map Elem cs }

-- | Filter the children of an element based on an attribute name and a list
-- of attribute values. Only those children that have an attribute with the
-- specified name and a value from the specified list will occur in the
-- resulting element list.
filterChildrenByAttr :: String    -- ^ Attribute name.
                     -> [String]  -- ^ Attribute values.
                     -> Element   -- ^ Parent element.
                     -> [Element] -- ^ Filtered child elements.
filterChildrenByAttr a vs = filterChildren (f . findAttr (unqual a))
    where
        f (Just v) = v `elem` vs
        f Nothing  = False

-- | Find the file formats in a given file format collection that are (direct
-- or indirect) supertypes of one of the given elements.
supertypes :: Element   -- ^ FileFormatCollection element.
           -> [Element] -- ^ FileFormat elements.
           -> [Element] -- ^ FileFormat elements (supertypes).
supertypes = related isSupertypeOf

-- | Find the file formats in a given file format collection that are (direct
-- or indirect) subtypes of one of the given elements.
subtypes :: Element   -- ^ FileFormatCollection element.
         -> [Element] -- ^ FileFormat elements.
         -> [Element] -- ^ FileFormat elements (subtypes).
subtypes = related isSubtypeOf

-- | Find the file formats in a given file format collection that are
-- (directly or indirectly) related to one of the given file formats with
-- respect to a given predicate.
related :: (Element -> Element -> Bool) -- ^ Predicate that determines whether
                                        -- two elements are related.
        -> Element                      -- ^ FileFormatCollection element.
        -> [Element]                    -- ^ FileFormat elements.
        -> [Element]                    -- ^ FileFormat elements.
related pred ffc es = related' pred ffc es []
    where
        related' _    _   [] acc = acc
        related' pred ffc es acc = related' pred ffc es' (acc ++ es')
            where es' = filterChildren (\ff -> any (pred ff) es) ffc \\ acc

-- | Find the file format ID of a given element. If the element has no ID
-- attribute the empty string is returned.
fmtID :: Element -- ^ FileFormat element.
      -> String  -- ^ File format ID attribute value.
fmtID = fromMaybe "" . findAttr (unqual "ID")

-- | Find all internal signature IDs that are referenced by a given element.
sigIDs :: Element  -- ^ FileFormat element.
       -> [String] -- ^ Internal signature IDs.
sigIDs = map strContent . findChildren (mkNm "InternalSignatureID")

-- | Find the file format IDs of all file formats that are immediate
-- supertypes of a given element.
supFmtIDs :: Element  -- ^ FileFormat element.
          -> [String] -- ^ Supertype format IDs.
supFmtIDs = map strContent . findChildren (mkNm "HasPriorityOverFileFormatID")

-- | Check if a file format x is a supertype of another file format y, i.e.,
-- if the relation y HasPriorityOverFileFormatID x holds.
isSupertypeOf :: Element -- ^ FileFormat element x.
              -> Element -- ^ FileFormat element y.
              -> Bool
x `isSupertypeOf` y = fmtID x `elem` supFmtIDs y

-- | Check if a file format x is a subtype of another file format y, i.e., if
-- the relation x HasPriorityOverFileFormatID y holds.
isSubtypeOf :: Element -- ^ FileFormat element x.
            -> Element -- ^ FileFormat element y.
            -> Bool
isSubtypeOf = flip isSupertypeOf

-- | The DROID signature file namespace string.
sfNamespace = "http://www.nationalarchives.gov.uk/pronom/SignatureFile"

-- | Create a QName for a given element name based on the DROID signature file
-- namespace.
mkNm :: String -> QName
mkNm n = QName n (Just sfNamespace) Nothing

-- | Test two elements for equality. N.B., in this context two elements are
-- considered equal if their ID attributes have the same value. If one or both
-- (!) elements do not have an ID attribute they are not considered equal.
instance Eq Element where
    x == y =
        case map (findAttr (unqual "ID")) [x,y] of
            [Nothing, Nothing] -> False
            [xID,     yID    ] -> xID == yID

