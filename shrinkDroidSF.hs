import Text.XML.Light

-- TODO Getopt!
signatureFile = "DROID_SignatureFile_V84.xml"
puids = ["fmt/354", "x-fmt/111"]

main = do
    content <- readFile signatureFile
    case parseXMLDoc content of
        Nothing  -> error "Failed to parse signature file."
        Just xml -> putStrLn . showTopElement $ filterSigFile puids xml

filterSigFile :: [String] -> Element -> Element
filterSigFile ps e = replaceChildren e [isc', ffc']
    where
        isc  = head $ findChildren (sfQName "InternalSignatureCollection") e
        ffc  = head $ findChildren (sfQName "FileFormatCollection") e
        isc' = replaceChildren isc $ filterChildrenByAttr "ID" ids isc
        ffc' = replaceChildren ffc $ filterChildrenByAttr "PUID" ps ffc
        -- TODO Add `Set.toList . Set.fromList`?
        ids  = concatMap sigIDs $ findChildren (sfQName "FileFormat") ffc'

replaceChildren :: Element -> [Element] -> Element
replaceChildren e cs = e { elContent = map Elem cs }

filterChildrenByAttr :: String -> [String] -> Element -> [Element]
filterChildrenByAttr a vs = filterChildren (f . findAttr (unqual a))
    where
        f (Just v) = v `elem` vs
        f Nothing  = False

sigIDs :: Element -> [String]
sigIDs = map strContent . findChildren (sfQName "InternalSignatureID")

sfNamespace = "http://www.nationalarchives.gov.uk/pronom/SignatureFile"

sfQName :: String -> QName
sfQName n = QName n (Just sfNamespace) Nothing

