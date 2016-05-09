module Stylesheet (..) where

import Css.File exposing (CssFileStructure)
import Styles

port files : CssFileStructure
port files = Css.File.toFileStructure [("public/stylesheet.css", Css.File.compile Styles.css)]
