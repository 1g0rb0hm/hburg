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
genNodeInterface :: Package -> Children -> Link -> KindReturn -> Java
genNodeInterface pkg children hasLnk retTy
    = let j0 = jSetModifier (java pkg "Node") Public in
    let methods
            = genChildMethods children ++ 
                genKindMethod retTy ++ 
                genMapEntryMethods ++
                (if (hasLnk) then genLinkMethod else [])
        in
    jSetIface (jSetMethods (jSetComments j0 genComments) methods) True

-- | Generate Child Node access methods.
genChildMethods :: Children -> [Method.Method]
genChildMethods children
    = map
        (\arity ->
            Method.new Public False "Node" ("child" ++ show arity) [] "")
        ([1 .. children])

-- | genLinkMethod.
genLinkMethod :: [Method.Method]
genLinkMethod = [Method.new Public False "Node" "link" [] ""]

-- | Generate link node access method interface
genKindMethod :: KindReturn -> [Method.Method]
genKindMethod ty = [Method.new Public False ty "kind" [] ""]

-- | Generate Java MapEntry manipulation method interfaces
genMapEntryMethods :: [Method.Method]
genMapEntryMethods
    = [Method.new Public False "boolean"  "is"   (Parameter.newFromList [("NT","nt")]) ""] ++
    [Method.new Public False "MapEntry" "put"  (Parameter.newFromList [("NT","nt"),("MapEntry","entry")]) ""] ++
    [Method.new Public False "MapEntry" "get"  (Parameter.newFromList [("NT","nt")]) ""] ++
    [Method.new Public False "int"      "cost" (Parameter.newFromList [("NT","nt")]) ""] ++
    [Method.new Public False "RuleEnum" "rule" (Parameter.newFromList [("NT","nt")]) ""]

genComments :: Comment.Comment
genComments
    = Comment.new [
    "Node Interface Implementation:",
    "",
    "\t- Create the following instance Variable:",
    "\t  // Map from Key:Nt -> Value:(RuleEnum: rule, int: cost)",
    "\t  private EnumMap<NT, MapEntry> table = new EnumMap<NT, MapEntry>(NT.class);",
    "",
    "\t- The is(), put(), get(), cost(), and rule() methods have the following implementations:",
    "\t\tis():\n\t\tpublic boolean is(NT nt) {\n\t\t\treturn this.table.containsKey(nt);\n\t\t}",
    "",
    "\t\tput():\n\t\tpublic MapEntry put(NT nt, MapEntry entry) {\n\t\t\treturn this.table.put(nt, entry);\n\t\t}",
    "",
    "\t\tget():\n\t\tpublic MapEntry get(NT nt) {\n\t\t\treturn this.table.get(nt);\n\t\t}",
    "",
    "\t\tcost():\n\t\tpublic int cost(NT nt) {\n\t\t\tMapEntry e = this.table.get(nt);\n\t\t\treturn (e != null) ? e.cost : Integer.MAX_VALUE;\n\t\t}",
    "",
    "\t\trule():\n\t\tpublic RuleEnum rule(NT nt) {\n\t\t\treturn (this.table.get(nt)).rule;\n\t\t}"
    ]