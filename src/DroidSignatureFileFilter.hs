module DroidSignatureFileFilter (filterSigFile) where

import Text.XML.Light

-- | Filter an XML string representing a DROID signature file based on a list
-- of PUIDs. Only those entries (file format and internal signature
-- descriptions) that are necessary to identify files with one of the given
-- PUIDs will occur in the resulting XML string.
filterSigFile :: [String] -> String -> String
filterSigFile puids xml = case parseXMLDoc xml of
    Nothing -> error "Failed to parse signature file."
    Just e  -> showTopElement $ replaceChildren e [isc', ffc']
        where
            isc  = head $ findChildren (mkNm "InternalSignatureCollection") e
            ffc  = head $ findChildren (mkNm "FileFormatCollection") e
            isc' = replaceChildren isc $ filterChildrenByAttr "ID" ids isc
            ffc' = replaceChildren ffc $ filterChildrenByAttr "PUID" puids ffc
            -- TODO Add `Set.toList . Set.fromList`?
            ids  = concatMap sigIDs $ findChildren (mkNm "FileFormat") ffc'

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

