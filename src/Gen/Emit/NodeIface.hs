-----------------------------------------------------------------------------
-- |
-- Module      :  NodeIface
-- Copyright   :  Copyright (c) 2007 Igor Boehm - Bytelabs.org. All rights reserved.
-- License     :  BSD-style (see the file LICENSE) 
-- Author      :  Igor Boehm  <igor@bytelabs.org>
--
--
-- Depending on the tree pattern matching grammar specification, the
-- correct AST node Java interface is emitted by this module.
-----------------------------------------------------------------------------

module Gen.Emit.NodeIface (
        -- * Functions
        genNodeInterface,
    ) where

import Control.Monad.State

import qualified Gen.Ident as I (Ident, pkgId, nN, nTy, ntTy, rTy, eTy, cTy)
import Gen.Emit.Class (JavaClass(..))
import Gen.Emit.Java.Class (Java, java)
import Gen.Emit.Java.Modifier (Modifier(..))
import qualified Gen.Emit.Java.Method as Method (Method, new)
import qualified Gen.Emit.Java.Comment as Comment (Comment, new)
import qualified Gen.Emit.Java.Parameter as Parameter (newFromList)
-----------------------------------------------------------------------------

type Children = Int
type Link = Bool
type KindReturn = String
type Package = String

-- | Generates Java Node Interface class.
genNodeInterface :: I.Ident -> Children -> Link -> KindReturn -> Java
genNodeInterface ids children hasLnk retTy
    = evalState
        (do
            clazz <- get
            put (setModifier clazz Public)
            clazz <- get
            put (setIface clazz True)
            clazz <- get
            put (setComments clazz $ genComments ids)
            clazz <- get
            put (setMethods
                    clazz $ (genChildMethods ids children)
                            ++ genKindMethod retTy
                            ++ genEntryMethods ids
                            ++ (if (hasLnk) then genLinkMethod ids else []))
            get)
        (java (I.pkgId ids) (I.nN ids))

-- | Generate Child Node access methods.
genChildMethods :: I.Ident -> Children -> [Method.Method]
genChildMethods ids children
    = map
        (\arity ->
            Method.new Public False (I.nTy ids) ("child"++ show arity) [] "")
        ([1 .. children])

-- | genLinkMethod.
genLinkMethod :: I.Ident -> [Method.Method]
genLinkMethod ids = [Method.new Public False (I.nTy ids) "link" [] ""]

-- | Generate link node access method interface
genKindMethod :: KindReturn -> [Method.Method]
genKindMethod ty = [Method.new Public False ty "kind" [] ""]

-- | Generate Java Entry manipulation method interfaces
genEntryMethods :: I.Ident -> [Method.Method]
genEntryMethods ids
    = [Method.new Public False "boolean"  "is"   (Parameter.newFromList [(I.ntTy ids,"nt")]) ""] ++
    [Method.new Public False (I.eTy ids)  "put"  (Parameter.newFromList [(I.ntTy ids,"nt"),(I.eTy ids,"entry")]) ""] ++
    [Method.new Public False (I.eTy ids)  "get"  (Parameter.newFromList [(I.ntTy ids,"nt")]) ""] ++
    [Method.new Public False (I.cTy ids)  "cost" (Parameter.newFromList [(I.ntTy ids,"nt")]) ""] ++
    [Method.new Public False (I.rTy ids)  "rule" (Parameter.newFromList [(I.ntTy ids,"nt")]) ""]

genComments :: I.Ident -> Comment.Comment
genComments ids
    = Comment.new [
    "Node Interface Implementation:",
    "",
    "\t- Create the following instance Variable:",
    "\t  // Map from Key:"++I.ntTy ids++" -> Value:("++I.rTy ids++": rule, int: cost)",
    "\t  private EnumMap<"++I.ntTy ids++", Entry> table = new EnumMap<"++I.ntTy ids++", Entry>("++I.ntTy ids++".class);",
    "",
    "\t- The is(), put(), get(), cost(), and rule() methods have the following implementations:",
    "\t\tis():\n\t\tpublic boolean is("++I.ntTy ids++" nt) {\n\t\t\treturn this.table.containsKey(nt);\n\t\t}",
    "",
    "\t\tput():\n\t\tpublic "++I.eTy ids++" put("++I.ntTy ids++" nt, "++I.eTy ids++" entry) {\n\t\t\treturn this.table.put(nt, entry);\n\t\t}",
    "",
    "\t\tget():\n\t\tpublic "++I.eTy ids++" get("++I.ntTy ids++" nt) {\n\t\t\treturn this.table.get(nt);\n\t\t}",
    "",
    "\t\tcost():\n\t\tpublic "++I.cTy ids++" cost("++I.ntTy ids++" nt) {\n\t\t\tEntry e = this.table.get(nt);\n\t\t\treturn (e != null) ? e.cost : Integer.MAX_VALUE;\n\t\t}",
    "",
    "\t\trule():\n\t\tpublic "++I.rTy ids++" rule("++I.ntTy ids++" nt) {\n\t\t\treturn (this.table.get(nt)).rule;\n\t\t}"
    ]