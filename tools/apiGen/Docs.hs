module Docs (
  ApiDoc,
  ModuleDoc(..),
  noModuleDoc,
  DocSection(..),
  Since,
  FuncDoc(..),
  ParamDoc(..),
  PropDoc(..),
  SignalDoc(..),
  DocPara(..),
  DocParaSpan(..),
  extractDocumentation
  ) where

import qualified Text.XML.HaXml as Xml

import Char (isUpper)
import List (partition)

-------------------------------------------------------------------------------
-- Types representing the content of the documentation XML file
-------------------------------------------------------------------------------
type ApiDoc = [ModuleDoc]

data ModuleDoc = ModuleDoc {
    moduledoc_name :: String,              -- these docs apply to this object
    moduledoc_altname :: String,           -- sometimes a better index entry
    moduledoc_summary :: [DocPara],        -- usually a one line summary
    moduledoc_description :: [DocPara],    -- the main description
    moduledoc_sections :: [DocSection],    -- any additional titled subsections
    moduledoc_hierarchy :: [DocParaSpan],  -- a tree of parent objects (as text)
    moduledoc_functions :: [FuncDoc],      -- documentation for each function
    moduledoc_callbacks :: [FuncDoc],      -- documentation for callback types
    moduledoc_properties :: [PropDoc],     -- documentation for each property
    moduledoc_childprops :: [PropDoc],     -- documentation for each child property
    moduledoc_signals :: [SignalDoc],      -- documentation for each signal
    moduledoc_since :: String              -- which version of the api the
  }  					   -- module is available from, eg "2.4"

noModuleDoc = ModuleDoc {
    moduledoc_name = "",
    moduledoc_altname = "",
    moduledoc_summary = [],
    moduledoc_description = [],
    moduledoc_sections = [],
    moduledoc_hierarchy = [],
    moduledoc_functions = [],
    moduledoc_callbacks = [],
    moduledoc_properties = [],
    moduledoc_childprops = [],
    moduledoc_signals = [],
    moduledoc_since = ""
  }

data DocSection = DocSection {
    section_title :: String,
    section_paras :: [DocPara]
  }

type Since = String

data FuncDoc = FuncDoc {
    funcdoc_name :: String,		-- C function name
    funcdoc_paragraphs :: [DocPara],	-- documentation markup
    funcdoc_params :: [ParamDoc],	-- parameter documentation
    funcdoc_since :: Since		-- which version of the api the
  }					-- function is available from, eg "2.4"

data ParamDoc = ParamDoc {
    paramdoc_name :: String,            -- parameter name or "Returns"
    paramdoc_paragraph :: [DocParaSpan] -- a simple paragraph
  }

data PropDoc = PropDoc {
    propdoc_name :: String,		-- property name
    propdoc_paragraphs :: [DocPara],	-- documentation markup
    propdoc_since :: Since		-- which version of the api the
  }					-- function is available from, eg "2.4"

data SignalDoc = SignalDoc {
    signaldoc_name :: String,		-- C signal name
    signaldoc_paragraphs :: [DocPara],	-- documentation markup
    signaldoc_params :: [ParamDoc],	-- parameter documentation
    signaldoc_since :: Since		-- which version of the api the
  }					-- function is available from, eg "2.4"

data DocPara =
    DocParaText [DocParaSpan]           -- an ordinary word-wrapped paragraph
  | DocParaProgram String               -- a verbatum section
  | DocParaTitle String                 -- a title to a subsection eg an example
  | DocParaDefItem [DocParaSpan] [DocParaSpan] -- a definition list item
  | DocParaListItem [DocParaSpan]       -- a itemisted list item

data DocParaSpan = DocText String       -- just simple text
                 | DocFuncXRef String   -- cross reference to a function name
                 | DocTypeXRef String   -- cross reference to a type name
                 | DocOtherXRef String  -- xref format not directly supported
                 | DocEmphasis String   -- emphasised text, usually italic
		 | DocLiteral String    -- some literal like numbers
		 | DocArg  String       -- function argument names


-------------------------------------------------------------------------------
-- extract functions to convert the doc xml file to the internal representation
-------------------------------------------------------------------------------
extractDocumentation :: Xml.Document -> ApiDoc
extractDocumentation (Xml.Document _ _ (Xml.Elem "apidoc" [] modules)) =
  map extractDocModule modules

extractDocModule :: Xml.Content -> ModuleDoc
extractDocModule (Xml.CElem (Xml.Elem "module" [] (moduleinfo:rest))) =
  let functions = [ e | e@(Xml.CElem (Xml.Elem "function" _ _)) <- rest ]
      properties = [ e | e@(Xml.CElem (Xml.Elem "property" _ _)) <- rest ]
      childprops = [ e | e@(Xml.CElem (Xml.Elem "childprop" _ _)) <- rest ]
      signals = [ e | e@(Xml.CElem (Xml.Elem "signal" _ _)) <- rest ]
      (callbacks, functions') = partition (isUpper.head.funcdoc_name)
                                         (map extractDocFunc functions)
  in (extractDocModuleinfo moduleinfo) {
    moduledoc_functions = functions',
    moduledoc_callbacks = callbacks,
    moduledoc_properties = map extractDocProp properties,
    moduledoc_childprops = map extractDocChildProp childprops,
    moduledoc_signals = map extractDocSignal signals
  }

extractDocModuleinfo :: Xml.Content -> ModuleDoc
extractDocModuleinfo 
  (Xml.CElem (Xml.Elem "module-info" []
    [Xml.CElem (Xml.Elem "name" [] name)
    ,Xml.CElem (Xml.Elem "altname" [] altname)
    ,Xml.CElem (Xml.Elem "summary" [] summary)
    ,Xml.CElem (Xml.Elem "description" [] parasAndSections)
    ,Xml.CElem (Xml.Elem "object-hierarchy" [] objHierSpans)]
  )) = 
  let (paras, sections) = span (\elem ->
        case elem of
          Xml.CElem (Xml.Elem "section" _ _) -> False
          _ -> True) parasAndSections
   in ModuleDoc {
    moduledoc_name = Xml.verbatim name,
    moduledoc_altname = Xml.verbatim altname,
    moduledoc_summary = [DocParaText (map extractDocParaSpan summary)],
    moduledoc_description = concatMap extractDocPara paras,
    moduledoc_sections = map extractDocSection sections,
    moduledoc_hierarchy = map extractDocParaSpan objHierSpans,
    moduledoc_functions = undefined,
    moduledoc_callbacks = undefined,
    moduledoc_properties = undefined,
    moduledoc_childprops = undefined,
    moduledoc_signals = undefined,
    moduledoc_since = ""
  }

extractDocSection :: Xml.Content -> DocSection
extractDocSection
  (Xml.CElem (Xml.Elem "section" []
    (Xml.CElem (Xml.Elem "title" [] [Xml.CString _ title])
    :paras))) =
  DocSection {
    section_title = title,
    section_paras = concatMap extractDocPara paras
  }
extractDocSection other = error $ "extractDocSection: " ++ Xml.verbatim other

extractDocFunc :: Xml.Content -> FuncDoc
extractDocFunc
  (Xml.CElem (Xml.Elem "function" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)
    ,Xml.CElem (Xml.Elem "params" [] params)]
  )) =
  let since = case since' of
                [] -> ""
		[Xml.CString _ since] | last since == '.' -> init since
                                      | otherwise         -> since
   in FuncDoc {
        funcdoc_name = name,
	funcdoc_paragraphs = concatMap extractDocPara paras,
        funcdoc_params = map extractParamDoc params,
	funcdoc_since = since
      }

extractParamDoc :: Xml.Content -> ParamDoc
extractParamDoc 
  (Xml.CElem (Xml.Elem "param" []
    (Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    :spans))) =
  ParamDoc {
    paramdoc_name = name,
    paramdoc_paragraph = map extractDocParaSpan spans
  }

extractDocProp :: Xml.Content -> PropDoc
extractDocProp
  (Xml.CElem (Xml.Elem "property" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)]
  )) =
  let since = case since' of
                [] -> ""
		[Xml.CString _ since] -> since
   in PropDoc {
        propdoc_name = name,
	propdoc_paragraphs = concatMap extractDocPara paras,
	propdoc_since = since
      }

extractDocChildProp :: Xml.Content -> PropDoc
extractDocChildProp
  (Xml.CElem (Xml.Elem "childprop" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)]
  )) =
  let since = case since' of
                [] -> ""
		[Xml.CString _ since] -> since
   in PropDoc {
        propdoc_name = name,
	propdoc_paragraphs = concatMap extractDocPara paras,
	propdoc_since = since
      }

extractDocSignal :: Xml.Content -> SignalDoc
extractDocSignal
  (Xml.CElem (Xml.Elem "signal" []
    [Xml.CElem (Xml.Elem "name" [] [Xml.CString _ name])
    ,Xml.CElem (Xml.Elem "since" [] since')
    ,Xml.CElem (Xml.Elem "doc" [] paras)
    ,Xml.CElem (Xml.Elem "params" [] params)]
  )) =
  let since = case since' of
                [] -> ""
		[Xml.CString _ since] -> since
   in SignalDoc {
        signaldoc_name = name,
	signaldoc_paragraphs = concatMap extractDocPara paras,
        signaldoc_params = map extractParamDoc params,
	signaldoc_since = since
      }

extractDocPara :: Xml.Content -> [DocPara]
extractDocPara (Xml.CElem elem@(Xml.Elem "para" [] _)) =
  case Xml.xmlUnEscape Xml.stdXmlEscaper elem of
    (Xml.Elem _ [] spans) -> extractDocPara' spans
extractDocPara (Xml.CElem (Xml.Elem "programlisting" _ content)) =
  let listing = concat [ str | (Xml.CString _ str) <- content ] in
  [DocParaProgram listing]
extractDocPara (Xml.CElem (Xml.Elem "example" _
                 (Xml.CElem (Xml.Elem "title" [] [Xml.CString _ title])
                 :content) )) =
  [DocParaTitle title] ++ concatMap extractDocPara content 
extractDocPara other = error $ "extractDocPara: " ++ Xml.verbatim other

extractDocPara' :: [Xml.Content] -> [DocPara]
extractDocPara' = reconstructParas [] . map extractDocParaOrSpan
  where reconstructParas :: [DocParaSpan] -> [Either DocParaSpan [DocPara]] -> [DocPara]
        reconstructParas []    [] = []
        reconstructParas spans [] = [DocParaText (reverse spans)]
        reconstructParas spans (Left  span:rest) = reconstructParas (span:spans) rest
        reconstructParas []    (Right paras:rest) = paras ++ reconstructParas [] rest
        reconstructParas spans (Right paras:rest) = DocParaText (reverse spans)
                                                 : paras ++ reconstructParas [] rest

extractDocParaOrSpan :: Xml.Content -> Either DocParaSpan [DocPara]
extractDocParaOrSpan (Xml.CElem (Xml.Elem "listitem" [] content)) =
  Right [DocParaListItem (map extractDocParaSpan content)]
extractDocParaOrSpan (Xml.CElem (Xml.Elem "definition" []
                       (Xml.CElem (Xml.Elem "term" [] term)
                       :content))) =
  Right [DocParaDefItem (map extractDocParaSpan term) (map extractDocParaSpan content)]
extractDocParaOrSpan (Xml.CElem (Xml.Elem "programlisting" _ content)) =
  let listing = concat [ str | (Xml.CString _ str) <- content ] in
  Right [DocParaProgram listing]
extractDocParaOrSpan para@(Xml.CElem (Xml.Elem "para" _ _)) = Right (extractDocPara para)
extractDocParaOrSpan content@(Xml.CElem   _  ) = Left $ extractDocParaSpan content
extractDocParaOrSpan content@(Xml.CString _ _) = Left $ extractDocParaSpan content
extractDocParaOrSpan other = error $ "extractDocParaOrSpan: " ++ Xml.verbatim other

extractDocParaSpan :: Xml.Content -> DocParaSpan
extractDocParaSpan (Xml.CString _ text) = DocText text
extractDocParaSpan (Xml.CElem (Xml.Elem tag [] content)) =
  let text = concat [ str | (Xml.CString _ str) <- content ] in
  case tag of
    "xref-func"  -> DocFuncXRef text
    "xref-type"  -> DocTypeXRef text
    "xref-other" -> DocOtherXRef text
    "emphasis"   -> DocEmphasis text
    "literal"    -> DocLiteral text
    "arg"        -> DocArg text
    other -> error $ "extractDocParaSpan: other tag " ++ tag

extractDocParaSpan other@(Xml.CRef (Xml.RefEntity entity)) = DocText (Xml.verbatim other)
extractDocParaSpan other = error $ "extractDocParaSpan: " ++ Xml.verbatim other
