module DroidSignatureFileFilter
( FilterOption(..)
, filterSigFile
) where

import Text.XML.Light
import Data.List (nub)

-- | Configuration options for the `filterSigFile` function.
data FilterOption = WithSupertypes deriving (Eq)

-- | Filter an XML string representing a DROID signature file based on a list
-- of PUIDs. Only those entries (file format and internal signature
-- descriptions) that are necessary to identify files with one of the given
-- PUIDs will occur in the resulting XML string. With an empty list of PUIDs,
-- the input XML string is returned unmodified.
filterSigFile :: [FilterOption] -> [String] -> String -> String
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
                   ++ if WithSupertypes `elem` opts
                        then supertypes ffc ffs
                        else []
            ids  = concatMap sigIDs $ findChildren (mkNm "FileFormat") ffc'
            isc' = replaceChildren isc $ filterChildrenByAttr "ID" ids isc

-- | Replace all content of an element by appending a list of elements as
-- children.
replaceChildren :: Element -> [Element] -> Element
replaceChildren e cs = e { elContent = map Elem cs }

-- | Filter the children of an element based on an attribute name and a list
-- of attribute values. Only those children that have an attribute with the
-- specified name and a value from the specified list will occur in the
-- resulting element list.
filterChildrenByAttr :: String -> [String] -> Element -> [Element]
filterChildrenByAttr a vs = filterChildren (f . findAttr (unqual a))
    where
        f (Just v) = v `elem` vs
        f Nothing  = False

-- | Find the file format IDs of all file formats that are immediate
-- supertypes of a given element. The element should be of type `FileFormat`.
supFmtIDs :: Element -> [String]
supFmtIDs = map strContent . findChildren (mkNm "HasPriorityOverFileFormatID")

-- | Find the file formats in a given file format collection that are (direct
-- or indirect) supertypes of one of the given elements. The file format
-- collection should be of type `FileFormatCollection`, the elements should be
-- of type `FileFormat`.
supertypes :: Element -> [Element] -> [Element]
supertypes ffc es = supertypes' ffc es []

supertypes' :: Element -> [Element] -> [Element] -> [Element]
supertypes' _   [] acc = acc
supertypes' ffc es acc = supertypes' ffc es' (acc ++ es')
    where es' = filterChildrenByAttr "ID" (concatMap supFmtIDs es) ffc

-- | Find all internal signature IDs that are referenced by a given element.
-- The element should be of type `FileFormat`.
sigIDs :: Element -> [String]
sigIDs = map strContent . findChildren (mkNm "InternalSignatureID")

-- | The DROID signature file namespace string.
sfNamespace = "http://www.nationalarchives.gov.uk/pronom/SignatureFile"

-- | Create a QName for a given element name based on the DROID signature file
-- namespace.
mkNm :: String -> QName
mkNm n = QName n (Just sfNamespace) Nothing

-- | Test two elements for equality. N.B., in this context two elements are
-- considered equal if their `ID` attributes have the same value. If one or
-- both (!) elements do not have an `ID` attribute they are not considered
-- equal.
instance Eq Element where
    x == y =
        case map (findAttr (unqual "ID")) [x,y] of
            [Nothing, Nothing] -> False
            [xID,     yID    ] -> xID == yID

